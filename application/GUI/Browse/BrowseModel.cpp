/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <Browse/BrowseModel.h>
using namespace GUI;

#include <QPixmap>
#include <QFileInfo>
#include <IconProvider.h>

#include <Common/Global.h>

#include <Log.h>

/**
  * @class GUI::BrowseModel
  *
  * The model of a distant peer file system. The directory content is lazy loaded, see the method 'loadChildren()'.
  * Used by 'WidgetBrowse'.
  */

BrowseModel::BrowseModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const DirListModel& sharedDirsModel, const Common::Hash& peerID, bool loadRoots) :
    coreConnection(coreConnection), sharedDirsModel(sharedDirsModel), peerID(peerID), root(new Node())
{
   if (loadRoots && !peerID.isNull())
      this->browse(this->peerID);
}

BrowseModel::~BrowseModel()
{
   if (!this->browseResult.isNull())
      this->browseResult->disconnect(this);

   delete this->root;
}

QModelIndex BrowseModel::index(int row, int column, const QModelIndex &parent) const
{
   if (!this->hasIndex(row, column, parent))
       return QModelIndex();

   const Node* parentNode;

    if (!parent.isValid())
       parentNode = this->root;
   else
       parentNode = static_cast<Node*>(parent.internalPointer());

   Node* childNode = parentNode->getChild(row);

   if (childNode)
       return this->createIndex(row, column, childNode);
   else if (parentNode->hasUnloadedChildren()) // The view want some not yet loaded children.. so we will load them.
      const_cast<BrowseModel*>(this)->loadChildren(parent);

   return QModelIndex();
}

QModelIndex BrowseModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
       return QModelIndex();

   Node* node = static_cast<Node*>(index.internalPointer());
   Node* parentItem = node->getParent();

   if (!parentItem || parentItem == this->root)
       return QModelIndex();

   return this->createIndex(parentItem->getRow(), 0, parentItem);
}

int BrowseModel::rowCount(const QModelIndex& parent) const
{
   const Node* parentNode;
   if (parent.column() > 0)
       return 0;

   if (!parent.isValid())
       parentNode = this->root;
   else
       parentNode = static_cast<Node*>(parent.internalPointer());

   int nbLoadedChildren = parentNode->getNbChildren();
   if (nbLoadedChildren > 0)
      return nbLoadedChildren;

   if (parentNode->hasUnloadedChildren())
      return 1; // We lie and tell there is a child.
   else
      return 0;
}

int BrowseModel::columnCount(const QModelIndex& parent) const
{
   return 2;
}

QVariant BrowseModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      {
         Node* node = static_cast<Node*>(index.internalPointer());
         return node->getData(index.column());
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            Node* node = static_cast<Node*>(index.internalPointer());
            return IconProvider::getIcon(node->getEntry());
         }
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return static_cast<int>((index.column() < this->columnCount() - 1 ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default:
      return QVariant();
   }
}

Protos::Common::Entry BrowseModel::getEntry(const QModelIndex& index) const
{
   Node* node = static_cast<Node*>(index.internalPointer());
   return node->getEntry();
}

bool BrowseModel::isDir(const QModelIndex& index) const
{
   return this->getEntry(index).type() == Protos::Common::Entry_Type_DIR;
}

QString BrowseModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   const Protos::Common::Entry entry = this->getEntry(index);
   const Common::SharedDir sharedDir = this->sharedDirsModel.getDir(entry.shared_dir().id().hash());
   if (sharedDir.isNull())
      return QString();

   QString path = sharedDir.path;
   return path.append(Common::ProtoHelper::getRelativePath(entry, appendFilename));
}

void BrowseModel::refresh()
{
   if (!this->browseResult.isNull())
      return;

   Protos::Common::Entries entries;

   Node::NodeBreadthIterator i(this->root);
   Node* currentNode;
   while (currentNode = i.next())
      if (currentNode->getNbChildren() > 0)
         entries.add_entry()->CopyFrom(currentNode->getEntry());

   this->browseResult = this->coreConnection->browse(this->peerID, entries, true);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();
}

QModelIndex BrowseModel::searchChild(const QString name, const QModelIndex& parent)
{
   Node* node = parent.isValid() ? static_cast<Node*>(parent.internalPointer()) : this->root;

   for (int i = 0; i < node->getNbChildren(); i++)
   {
      if (Common::ProtoHelper::getStr(node->getChild(i)->getEntry(), &Protos::Common::Entry::name) == name)
      {
         return this->index(i, 0, parent);
      }
   }
   return QModelIndex();
}

bool BrowseModel::isWaitingResult() const
{
   return !this->browseResult.isNull();
}

void BrowseModel::resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   QList<Node*> nodesToDelete;

   // Synchronize the content of all directories.
   Node::NodeBreadthIterator i(this->root);
   int j = -1;
   Node* currentNode;
   while (currentNode = i.next())
      if (currentNode->getNbChildren() > 0)
      {
         if (++j >= entries.size() - 1)
            break;
         nodesToDelete << this->synchronize(currentNode, entries.Get(j));
      }

   // Synchronize the root.
   for (QListIterator<Node*> i(this->synchronizeRoot(entries.Get(entries.size() - 1))); i.hasNext();)
      nodesToDelete.prepend(i.next());

   QListIterator<Node*> k(nodesToDelete);
   k.toBack();
   while (k.hasPrevious())
   {
      Node* node = k.previous();
      this->beginRemoveRows(node->getParent() == this->root ? QModelIndex() : this->createIndex(node->getParent()->getRow(), 0, node->getParent()), node->getRow(), node->getRow()); // Root cannot be deleted, so the node must have a parent.
      delete node;
      this->endRemoveRows();
   }
   this->browseResult.clear();
   emit loadingResultFinished();
}

void BrowseModel::result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   if (entries.size() > 0 && entries.Get(0).entry_size() > 0)
   {
      this->beginInsertRows(this->currentBrowseIndex, 0, entries.Get(0).entry_size() - 1);

      if (this->currentBrowseIndex.internalPointer())
      {
         Node* node = static_cast<Node*>(this->currentBrowseIndex.internalPointer());
         node->insertChildren(entries.Get(0));
      }
      else
         this->root->insertChildren(entries.Get(0));

      this->endInsertRows();
   }

   this->currentBrowseIndex = QModelIndex();
   this->browseResult.clear();
   emit loadingResultFinished();
}

void BrowseModel::resultTimeout()
{
   this->browseResult.clear();
   emit loadingResultFinished();
}

void BrowseModel::browse(const Common::Hash& peerID, Node* node)
{
   this->browseResult = node ? this->coreConnection->browse(this->peerID, node->getEntry()) : this->coreConnection->browse(this->peerID);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();
}

void BrowseModel::loadChildren(const QPersistentModelIndex &index)
{
   this->currentBrowseIndex = index;
   this->browse(this->peerID, static_cast<Node*>(index.internalPointer()));
}

/**
  * Add the new elements in 'entries' and return the entries in 'node->children' which dont exist in 'entries'.
  * 'entries' and 'node->children' must be sorted by their name.
  */
QList<BrowseModel::Node*> BrowseModel::synchronize(BrowseModel::Node* node, const Protos::Common::Entries& entries)
{
   QList<Node*> nodesToDelete;

   if (!node->getParent())
      return nodesToDelete;

   QModelIndex parentIndex = this->createIndex(node->getRow(), 0, node);

   int i = 0; // Children of 'node'.
   int j = 0; // Entries.

   while (i < node->getNbChildren() || j < entries.entry_size())
   {
      if (i >= node->getNbChildren() || j < entries.entry_size() && node->getChild(i)->getEntry() > entries.entry(j)) // New entry.
      {
         this->beginInsertRows(parentIndex, i, i);
         node->insertChild(entries.entry(j++), i++);
         this->endInsertRows();
      }
      else if (j >= entries.entry_size() || node->getChild(i)->getEntry() < entries.entry(j)) // Entry deleted.
      {
         nodesToDelete << node->getChild(i++);
      }
      else // Entry paths are equal.
      {
         if (node->getChild(i)->getEntry() != entries.entry(j))
         {
            node->getChild(i)->setEntry(entries.entry(j));
            emit dataChanged(parentIndex.child(i, 0), parentIndex.child(i, 1));
         }

         i++;
         j++;
      }
   }

   return nodesToDelete;
}

/**
  * Special case for the shared directories (roots). They may not be sorted in a alphabetic way. They are identified by their ID.
  */
QList<BrowseModel::Node*> BrowseModel::synchronizeRoot(const Protos::Common::Entries& entries)
{
   QModelIndex parentIndex = QModelIndex();

   int j = 0; // Root's children.
   for (int i = 0 ; i < entries.entry_size(); i++)
   {
      // We've searching if the entry alredy exists.
      for (int j2 = j; j2 < this->root->getNbChildren(); j2++)
      {
         if (entries.entry(i).shared_dir().id().hash() == this->root->getChild(j2)->getEntry().shared_dir().id().hash()) // ID's are equal -> same entry.
         {
            if (entries.entry(i) != this->root->getChild(j2)->getEntry()) // The entry data may have changed.
            {
               this->root->getChild(j2)->setEntry(entries.entry(i));
               emit dataChanged(this->index(j2, 0), this->index(j2, this->columnCount() - 1));
            }

            if (j2 != j) // 'beginMoveRows(..)' crashes if j2 == j.
            {
               this->beginMoveRows(parentIndex, j2, j2, parentIndex, j);
               this->root->moveChild(j2, j);
               this->endMoveRows();
            }
            j++;
            goto nextEntry;
         }
      }
      // The entry doesn't exist, we create it.
      this->beginInsertRows(parentIndex, j, j);
      this->root->insertChild(entries.entry(i), j++);
      this->endInsertRows();
      nextEntry:;
   }

   QList<Node*> nodesToDelete;
   while (j < this->root->getNbChildren())
      nodesToDelete << this->root->getChild(j++);
   return nodesToDelete;
}

/**
  * @class GUI::NodeBreadthIterator
  *
  * To iterate into a node structure.
  */

BrowseModel::Node::NodeBreadthIterator::NodeBreadthIterator(BrowseModel::Node* node)
{
   this->readChildren(node);
}

BrowseModel::Node* BrowseModel::Node::NodeBreadthIterator::next()
{
   if (this->nextNodes.isEmpty())
      return 0;

   BrowseModel::Node* node = this->nextNodes.takeFirst();
   this->readChildren(node);
   return node;
}

void BrowseModel::Node::NodeBreadthIterator::readChildren(Node* parentNode)
{
   for (QListIterator<BrowseModel::Node*> i(parentNode->children); i.hasNext();)
   {
      BrowseModel::Node* child = i.next();
      if (child->entry.type() == Protos::Common::Entry_Type_DIR)
         this->nextNodes << child;
   }
}

/**
  * @class GUI::Node
  *
  * Either a file or a directory in the tree view structure.
  */

BrowseModel::Node::Node() :
   parent(0)
{
   this->entry.set_type(Protos::Common::Entry_Type_DIR);
}

BrowseModel::Node::Node(const Protos::Common::Entry& entry, Node* parent) :
   entry(entry), parent(parent)
{
   this->copySharedDirFromParent();
   if (!this->entry.shared_dir().has_shared_name())
      this->entry.mutable_shared_dir()->set_shared_name(this->entry.name()); // For the root.
}

BrowseModel::Node::~Node()
{
   for (QListIterator<Node*> i(this->children); i.hasNext();)
      delete i.next();

   if (this->parent)
   {
      this->parent->children.removeOne(this);
      if (this->parent->children.isEmpty())
         this->parent->entry.set_is_empty(true);
   }
}

BrowseModel::Node* BrowseModel::Node::getParent()
{
   return this->parent;
}

int BrowseModel::Node::getNbChildren() const
{
   return this->children.size();
}

BrowseModel::Node* BrowseModel::Node::getChild(int row) const
{
   if (row >= this->children.size())
      return 0;
   return this->children[row];
}

void BrowseModel::Node::moveChild(int from, int to)
{
   if (from >= this->children.size() || to >= this->children.size())
      return;
   this->children.move(from, to);
}

void BrowseModel::Node::insertChildren(const Protos::Common::Entries& entries)
{
   for (int i = 0; i < entries.entry_size(); i++)
      this->newNode(entries.entry(i));
}

void BrowseModel::Node::insertChild(const Protos::Common::Entry& entry, int pos)
{
   this->newNode(entry, pos);
}

bool BrowseModel::Node::hasUnloadedChildren() const
{
   return this->entry.type() == Protos::Common::Entry_Type_DIR && this->children.isEmpty() && !this->entry.is_empty();
}

/**
  * O(n).
  */
int BrowseModel::Node::getRow() const
{
   if (this->parent)
      return this->parent->children.indexOf(const_cast<Node*>(this));

   return 0;
}

QVariant BrowseModel::Node::getData(int column) const
{
   switch (column)
   {
   case 0: return Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name);
   case 1: return Common::Global::formatByteSize(this->entry.size());
   default: return QVariant();
   }
}

const Protos::Common::Entry& BrowseModel::Node::getEntry() const
{
   return this->entry;
}

void BrowseModel::Node::setEntry(const Protos::Common::Entry& entry)
{
   this->entry.CopyFrom(entry);
   this->copySharedDirFromParent();
}

BrowseModel::Node* BrowseModel::Node::newNode(const Protos::Common::Entry& entry)
{
   this->children << new Node(entry, this);
   return this->children.last();
}

BrowseModel::Node* BrowseModel::Node::newNode(const Protos::Common::Entry& entry, int pos)
{
   if (pos > this->children.size())
      pos = this->children.size();

   Node* newNode = new Node(entry, this);
   this->children.insert(pos, newNode);
   return newNode;
}

void BrowseModel::Node::copySharedDirFromParent()
{
   // Copy the shared directory ID from the parent.
   if (!this->entry.has_shared_dir() && this->parent && this->parent->parent)
      this->entry.mutable_shared_dir()->CopyFrom(this->parent->entry.shared_dir());
}

bool GUI::operator>(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type() || e1.size() != e2.size() || e1.is_empty() != e2.is_empty())
      return false;

   return Common::ProtoHelper::getStr(e1, &Protos::Common::Entry::name) > Common::ProtoHelper::getStr(e2, &Protos::Common::Entry::name);
}

bool GUI::operator<(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type() || e1.size() != e2.size() || e1.is_empty() != e2.is_empty())
      return false;

   return Common::ProtoHelper::getStr(e1, &Protos::Common::Entry::name) < Common::ProtoHelper::getStr(e2, &Protos::Common::Entry::name);
}

bool GUI::operator==(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   return Common::ProtoHelper::getStr(e1, &Protos::Common::Entry::name) == Common::ProtoHelper::getStr(e2, &Protos::Common::Entry::name) &&
      e1.type() == e2.type() &&
      e1.size() == e2.size() &&
      e1.is_empty() == e2.is_empty();
}

bool GUI::operator!=(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   return !(e1 == e2);
}
