/**
  * Aybabtu - A decentralized LAN file sharing software.
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

#include <Common/Global.h>

/**
  * @class BrowseModel
  * The model of a distant peer file system. The directory content is lazy loaded, see the method 'loadChildren()'.
  * Used by 'WidgetBrowse'.
  */

BrowseModel::BrowseModel(CoreConnection& coreConnection, const Common::Hash& peerID) :
    coreConnection(coreConnection), peerID(peerID), root(new Node())
{
   if (!peerID.isNull())
      this->browse(this->peerID);
}

BrowseModel::~BrowseModel()
{
   if (!this->browseResult.isNull())
      disconnect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));

   delete this->root;
}

QModelIndex BrowseModel::index(int row, int column, const QModelIndex &parent) const
{
   if (!this->hasIndex(row, column, parent))
       return QModelIndex();

   Node* parentNode;

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

   if (!parentItem)
       return QModelIndex();

   return this->createIndex(parentItem->getRow(), 0, parentItem);
}

int BrowseModel::rowCount(const QModelIndex& parent) const
{
   Node* parentNode;
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
      break;

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            Node* node = static_cast<Node*>(index.internalPointer());
            if (node->getEntry().type() == Protos::Common::Entry_Type_DIR)
               return QPixmap(":/icons/ressources/folder.png");
            else
               return QPixmap(":/icons/ressources/file.png");
         }
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return static_cast<int>((index.column() < this->columnCount(QModelIndex()) - 1 ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default: return QVariant();
   }
}

Protos::Common::Entry BrowseModel::getEntry(const QModelIndex& index)
{
   Node* node = static_cast<Node*>(index.internalPointer());
   return node->getEntry();
}

void BrowseModel::refresh()
{
   Protos::Common::Entries entries;

   Node::NodeBreadthIterator i(this->root);
   Node* currentNode;
   while ((currentNode = i.next()) && currentNode->getNbChildren() > 0)
      entries.add_entry()->CopyFrom(currentNode->getEntry());

   this->browseResult = this->coreConnection.browse(this->peerID, entries, true);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();

   /*QList<Node*> nodesToVisit;
   nodesToVisit << this->root;

   while (!nodesToVisit.isEmpty())
   {
      Node* currentNode = nodesToVisit.takeFirst();
      for (QListIterator<Node*> i(currentNode->children); i.hasNext();)
      {
         Node* child = i.next();
         if (child->entry.type() == Protos::Common::Entry_Type_DIR)
            nodesToVisit << child;
      }
      currentNode
   }*/

   /*this->browseResult = node ? this->coreConnection.browse(this->peerID, node->getEntry()) : this->coreConnection.browse(this->peerID);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();*/
}

void BrowseModel::resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   if (entries.size() == 0)
      return;

   QList<Node*> nodesToDelete;

   // Synchronize the root.
   nodesToDelete << this->synchronize(this->root, entries.Get(entries.size() - 1));

   // Synchronize the content of all directories.
   Node::NodeBreadthIterator i(this->root);
   int j = -1;
   Node* currentNode;
   while ((currentNode = i.next()) && currentNode->getNbChildren() > 0 && ++j < entries.size() - 1)
      nodesToDelete << this->synchronize(currentNode, entries.Get(j));

   QListIterator<Node*> k(nodesToDelete);
   k.toBack();
   while (k.hasPrevious())
   {
      delete k.previous();
   }
}

void BrowseModel::result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   if (entries.size() == 0)
      return;

   if (entries.Get(0).entry_size() > 0)
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
}

void BrowseModel::resultTimeout()
{
   this->browseResult.clear();
}

void BrowseModel::browse(const Common::Hash& peerID, Node* node)
{
   this->browseResult = node ? this->coreConnection.browse(this->peerID, node->getEntry()) : this->coreConnection.browse(this->peerID);
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
  * 'entries' and 'node->children' must be sorted in the same way.
  */
QList<BrowseModel::Node*> BrowseModel::synchronize(BrowseModel::Node* node, const Protos::Common::Entries& entries)
{
   QModelIndex parentIndex = this->createIndex(node->getRow(), 0, node);

   QList<Node*> nodesToDelete;

   int i = 0;
   int j = 0;

   while (i < node->getNbChildren() || j < entries.entry_size())
   {
      if (i >= node->getNbChildren() || node->getChild(i)->getEntry() > entries.entry(j)) // New entry.
      {
         this->beginInsertRows(parentIndex, j, j);
         node->insertChild(entries.entry(j++), ++i);
         this->endInsertRows();
      }
      else if (j >= entries.entry_size() || node->getChild(i)->getEntry() < entries.entry(j)) // Entry deleted.
      {
         nodesToDelete << node->getChild(i++);
      }
      else // Entries are equal.
      {
         i++;
         j++;
      }
   }

   return nodesToDelete;
}

/**
  * @class NodeBreadthIterator
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
  * @class Node
  * Either a file or a directory in the tree view structure.
  */

BrowseModel::Node::Node()
   : parent(0)
{
   this->entry.set_type(Protos::Common::Entry_Type_DIR);
}

BrowseModel::Node::Node(const Protos::Common::Entry& entry, Node* parent)
   : entry(entry), parent(parent)
{
   // Copy the shared directory ID from the parent.
   if (!this->entry.has_shared_dir())
   {
      if (this->parent->parent)
         this->entry.mutable_shared_dir()->CopyFrom(this->parent->entry.shared_dir());
   }
   if (!this->entry.shared_dir().has_shared_name())
      this->entry.mutable_shared_dir()->set_shared_name(this->entry.name()); // For the root.
}

BrowseModel::Node::~Node()
{
   for (QListIterator<Node*> i(this->children); i.hasNext();)
      delete i.next();
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

void BrowseModel::Node::insertChildren(const Protos::Common::Entries& entries)
{
   for (int i = 0; i < entries.entry_size(); i++)
      this->newNode(entries.entry(i));
}

void BrowseModel::Node::insertChild(const Protos::Common::Entry& entry, int pos)
{
   this->newNode(entry, pos);
}

bool BrowseModel::Node::hasUnloadedChildren()
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

bool GUI::operator>(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type() || e1.size() != e2.size())
      return false;

   return Common::ProtoHelper::getStr(e1, &Protos::Common::Entry::name) > Common::ProtoHelper::getStr(e2, &Protos::Common::Entry::name);
}

bool GUI::operator<(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type() || e1.size() != e2.size())
      return false;

   return Common::ProtoHelper::getStr(e1, &Protos::Common::Entry::name) < Common::ProtoHelper::getStr(e2, &Protos::Common::Entry::name);
}

