/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
    coreConnection(coreConnection), sharedDirsModel(sharedDirsModel), peerID(peerID), root(new Tree())
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

QModelIndex BrowseModel::index(int row, int column, const QModelIndex& parent) const
{
   if (!this->hasIndex(row, column, parent))
       return QModelIndex();

   const Tree* parentTree;

   if (!parent.isValid())
       parentTree = this->root;
   else
       parentTree = static_cast<Tree*>(parent.internalPointer());

   Tree* childTree = parentTree->getChild(row);

   if (childTree)
       return this->createIndex(row, column, childTree);
   else if (parentTree->hasUnloadedChildren()) // The view want some not yet loaded children.. so we will load them.
      const_cast<BrowseModel*>(this)->loadChildren(parent);

   return QModelIndex();
}

QModelIndex BrowseModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
       return QModelIndex();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   Tree* parentItem = tree->getParent();

   if (!parentItem || parentItem == this->root)
       return QModelIndex();

   return this->createIndex(parentItem->getOwnPosition(), 0, parentItem);
}

int BrowseModel::rowCount(const QModelIndex& parent) const
{
   const Tree* parentTree;
   if (parent.column() > 0)
       return 0;

   if (!parent.isValid())
       parentTree = this->root;
   else
       parentTree = static_cast<Tree*>(parent.internalPointer());

   int nbLoadedChildren = parentTree->getNbChildren();
   if (nbLoadedChildren > 0)
      return nbLoadedChildren;

   if (parentTree->hasUnloadedChildren())
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

   switch (role)
   {
   case Qt::DisplayRole:
      {
         Tree* tree = static_cast<Tree*>(index.internalPointer());
         return tree->data(index.column());
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            Tree* tree = static_cast<Tree*>(index.internalPointer());
            return IconProvider::getIcon(tree->getItem());
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
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   return tree->getItem();
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

   TreeBreadthIterator i(this->root);
   Tree* currentTree;
   while (currentTree = i.next())
      if (currentTree->getNbChildren() > 0)
         entries.add_entry()->CopyFrom(currentTree->getItem());

   this->browseResult = this->coreConnection->browse(this->peerID, entries, true);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();
}

QModelIndex BrowseModel::searchChild(const QString name, const QModelIndex& parent)
{
   Tree* tree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;

   for (int i = 0; i < tree->getNbChildren(); i++)
   {
      if (Common::ProtoHelper::getStr(tree->getChild(i)->getItem(), &Protos::Common::Entry::name) == name)
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

int BrowseModel::nbSharedDirs() const
{
   return this->sharedDirsModel.getDirs().size();
}

void BrowseModel::resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   QList<Tree*> TreesToDelete;

   // Synchronize the content of all directories.
   TreeBreadthIterator i(this->root);
   int j = -1;
   Tree* currentTree;
   while (currentTree = i.next())
      if (currentTree->getNbChildren() > 0)
      {
         if (++j >= entries.size() - 1)
            break;
         TreesToDelete << this->synchronize(currentTree, entries.Get(j));
      }

   // Synchronize the root.
   for (QListIterator<Tree*> i(this->synchronizeRoot(entries.Get(entries.size() - 1))); i.hasNext();)
      TreesToDelete.prepend(i.next());

   QListIterator<Tree*> k(TreesToDelete);
   k.toBack();
   while (k.hasPrevious())
   {
      Tree* tree = k.previous();
      this->beginRemoveRows(tree->getParent() == this->root ? QModelIndex() : this->createIndex(tree->getParent()->getOwnPosition(), 0, tree->getParent()), tree->getOwnPosition(), tree->getOwnPosition()); // Root cannot be deleted, so the Tree must have a parent.
      delete tree;
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
         Tree* tree = static_cast<Tree*>(this->currentBrowseIndex.internalPointer());
         tree->insertChildren(entries.Get(0));
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
   L_WARN("Asking for entries message timedout");
   this->browseResult.clear();
   emit loadingResultFinished();
}

void BrowseModel::browse(const Common::Hash& peerID, Tree* tree)
{
   this->browseResult = tree ? this->coreConnection->browse(this->peerID, tree->getItem()) : this->coreConnection->browse(this->peerID);
   connect(this->browseResult.data(), SIGNAL(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)), this, SLOT(result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&)));
   connect(this->browseResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->browseResult->start();
}

void BrowseModel::loadChildren(const QPersistentModelIndex &index)
{
   this->currentBrowseIndex = index;
   this->browse(this->peerID, static_cast<Tree*>(index.internalPointer()));
}

/**
  * Add the new elements in 'entries' and return the entries in 'Tree->children' which dont exist in 'entries'.
  * 'entries' and 'Tree->children' must be sorted by their name.
  */
QList<BrowseModel::Tree*> BrowseModel::synchronize(BrowseModel::Tree* tree, const Protos::Common::Entries& entries)
{
   QList<Tree*> TreesToDelete;

   if (!tree->getParent())
      return TreesToDelete;

   QModelIndex parentIndex = this->createIndex(tree->getOwnPosition(), 0, tree);

   int i = 0; // Children of 'Tree'.
   int j = 0; // Entries.

   while (i < tree->getNbChildren() || j < entries.entry_size())
   {
      if (i >= tree->getNbChildren() || j < entries.entry_size() && tree->getChild(i)->getItem() > entries.entry(j)) // New entry.
      {
         this->beginInsertRows(parentIndex, i, i);
         tree->insertChild(entries.entry(j++), i++);
         this->endInsertRows();
      }
      else if (j >= entries.entry_size() || tree->getChild(i)->getItem() < entries.entry(j)) // Entry deleted.
      {
         TreesToDelete << tree->getChild(i++);
      }
      else // Entry paths are equal.
      {
         if (tree->getChild(i)->getItem() != entries.entry(j))
         {
            tree->getChild(i)->setItem(entries.entry(j));
            emit dataChanged(this->createIndex(i, 0, tree->getChild(i)), this->createIndex(i, 1, tree->getChild(i)));
         }

         i++;
         j++;
      }
   }

   return TreesToDelete;
}

/**
  * Special case for the shared directories (roots). They may not be sorted in a alphabetic way. They are identified by their ID.
  */
QList<BrowseModel::Tree*> BrowseModel::synchronizeRoot(const Protos::Common::Entries& entries)
{
   QModelIndex parentIndex = QModelIndex();

   int j = 0; // Root's children.
   for (int i = 0 ; i < entries.entry_size(); i++)
   {
      // We've searching if the entry alredy exists.
      for (int j2 = j; j2 < this->root->getNbChildren(); j2++)
      {
         if (entries.entry(i).shared_dir().id().hash() == this->root->getChild(j2)->getItem().shared_dir().id().hash()) // ID's are equal -> same entry.
         {
            if (entries.entry(i) != this->root->getChild(j2)->getItem()) // The entry data may have changed.
            {
               this->root->getChild(j2)->setItem(entries.entry(i));
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

   QList<Tree*> TreesToDelete;
   while (j < this->root->getNbChildren())
      TreesToDelete << this->root->getChild(j++);
   return TreesToDelete;
}

void BrowseModel::reset()
{
   this->beginResetModel();
   delete this->root;
   this->root = new Tree();
   this->endResetModel();
}


/**
  * @class GUI::Tree
  *
  * Either a file or a directory in the tree view structure.
  */

BrowseModel::Tree::Tree()
{
   this->getItem().set_type(Protos::Common::Entry_Type_DIR);
}

BrowseModel::Tree::Tree(const Protos::Common::Entry& entry, Tree* parent) :
   Common::Tree<Protos::Common::Entry, BrowseModel::Tree>(entry, parent)
{
   this->copySharedDirFromParent();
   if (!this->getItem().shared_dir().has_shared_name())
      this->getItem().mutable_shared_dir()->set_shared_name(this->getItem().name()); // For the root.
}

BrowseModel::Tree::~Tree()
{
   if (this->getParent())
   {
      if (this->getParent()->getNbChildren() == 0)
         this->getParent()->getItem().set_is_empty(true);
   }
}

void BrowseModel::Tree::insertChildren(const Protos::Common::Entries& entries)
{
   for (int i = 0; i < entries.entry_size(); i++)
      this->insertChild(entries.entry(i));
}

void BrowseModel::Tree::setItem(const Protos::Common::Entry& entry)
{
   Common::Tree<Protos::Common::Entry, BrowseModel::Tree>::setItem(entry);
   this->copySharedDirFromParent();
}

bool BrowseModel::Tree::hasUnloadedChildren() const
{
   return this->getItem().type() == Protos::Common::Entry_Type_DIR && this->getNbChildren() == 0 && !this->getItem().is_empty();
}

QVariant BrowseModel::Tree::data(int column) const
{
   switch (column)
   {
   case 0: return Common::ProtoHelper::getStr(this->getItem(), &Protos::Common::Entry::name);
   case 1: return Common::Global::formatByteSize(this->getItem().size());
   default: return QVariant();
   }
}

void BrowseModel::Tree::copySharedDirFromParent()
{
   // Copy the shared directory ID from the parent.
   if (!this->getItem().has_shared_dir() && this->getParent() && this->getParent()->getParent())
      this->getItem().mutable_shared_dir()->CopyFrom(this->getParent()->getItem().shared_dir());
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
