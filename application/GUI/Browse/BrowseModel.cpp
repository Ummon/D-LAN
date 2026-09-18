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
#include <QHash>
#include <vector>
#include <IconProvider.h>

#include <Common/Global.h>

#include <Log.h>

/**
  * @class GUI::BrowseModel
  *
  * The model of a distant peer file system. The directory content is lazy loaded, see the method 'loadChildren()'.
  * Used by 'WidgetBrowse'.
  */

BrowseModel::BrowseModel(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const SharedEntryListModel& sharedEntryListModel,
   const Common::Hash& peerID,
   bool loadRoots
) :
   coreConnection(coreConnection), sharedEntryListModel(sharedEntryListModel), peerID(peerID), root(new Tree())
{
   if (loadRoots && !this->peerID.isNull())
      this->browse();
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

   const Tree* childTree = parentTree->getChild(row);

   if (childTree)
      return this->createIndex(row, column, childTree);
   else if (parentTree->hasUnloadedChildren()) // The view want some not yet loaded children . . . so we will load them.
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
   if (parent.column() > 0)
      return 0;

   const Tree* parentTree;

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

int BrowseModel::columnCount(const QModelIndex&) const
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
      return QVariant((index.column() < this->columnCount() - 1 ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

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

/**
  * Returns the local path of the entry at the given index.
  */
QString BrowseModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   const Protos::Common::Entry entry = this->getEntry(index);
   const Common::SharedEntry sharedEntry = this->sharedEntryListModel.getSharedEntry(entry.shared_entry().id().hash());

   if (sharedEntry.isNull())
      return QString();

   if (sharedEntry.path.isFile())
   {
      return sharedEntry.path.toString(appendFilename);
   }
   else
   {
      QString path = sharedEntry.path.toString();
      QString relativePath = Common::ProtoHelper::getPath(entry).toString(appendFilename);

      if (relativePath == '/')
         return path;
      else
         return path.append(relativePath);
   }
}

void BrowseModel::refresh()
{
   if (!this->browseResult.isNull())
      return;

   Protos::Common::Entries entries;

   this->root->mapPostOrder(
      [&entries](Tree* tree)
      {
         if (tree->getNbChildren() > 0)
            entries.add_entries()->CopyFrom(tree->getItem());
         return true;
      }
   );

   this->browseResult = this->coreConnection->browse(this->peerID, entries, true);

   connect(this->browseResult.data(), &RCC::IBrowseResult::result, this, &BrowseModel::resultRefresh);
   connect(this->browseResult.data(), &Common::Timeoutable::timeout, this, &BrowseModel::resultTimeout);
   this->browseResult->start();
}

QModelIndex BrowseModel::searchChild(const QString& name, const QModelIndex& parent)
{
   Tree* tree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;

   for (int i = 0; i < tree->getNbChildren(); i++)
   {
      if (QString::fromStdString(tree->getChild(i)->getItem().name()) == name)
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
   return this->sharedEntryListModel.getSharedDirectories().size();
}

void BrowseModel::resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   if (entries.size() == 0)
   {
      this->reset();
   }
   else
   {
      // Synchronize the content of all directories.
      int j = -1;
      this->root->mapPostOrder([&](Tree* tree) {
         if (tree->getNbChildren() > 0)
         {
            if (++j >= entries.size() - 1)
               return false;
            this->synchronize(tree, entries.Get(j));
         }
         return true;
      });

      // Synchronize the root.
      this->synchronizeRoot(entries.Get(entries.size() - 1));
   }

   this->browseResult.clear();

   emit loadingResultFinished();
}

void BrowseModel::result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries)
{
   if (entries.size() > 0 && entries.Get(0).entries_size() > 0)
   {
      this->beginInsertRows(this->currentBrowseIndex, 0, entries.Get(0).entries_size() - 1);

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
   L_WARN("Asking for entries message timed out");
   this->currentBrowseIndex = QModelIndex();
   this->browseResult.clear();
   emit loadingResultFinished();
}

void BrowseModel::browse(Tree* tree)
{
   if (!this->browseResult.isNull())
      return;

   this->browseResult = tree ?
        this->coreConnection->browse(this->peerID, tree->getItem())
      : this->coreConnection->browse(this->peerID);
   connect(this->browseResult.data(), &RCC::IBrowseResult::result, this, &BrowseModel::result);
   connect(this->browseResult.data(), &Common::Timeoutable::timeout, this, &BrowseModel::resultTimeout);
   this->browseResult->start();
}

void BrowseModel::loadChildren(const QPersistentModelIndex &index)
{
   // Keep the destination of the pending request: browse() refuses another request
   // while it is running, so changing the index here would redirect its response.
   if (!this->browseResult.isNull() || index == this->currentBrowseIndex)
      return;

   this->currentBrowseIndex = index;
   this->browse(static_cast<Tree*>(index.internalPointer()));
}

/**
  * Synchronize the 'tree->children' with the given state.
  * 'entries' and 'tree->children' must be sorted by their name.
  */
void BrowseModel::synchronize(BrowseModel::Tree* tree, const Protos::Common::Entries& entries)
{
   if (!tree->getParent())
      return;

   QModelIndex parentIndex = this->createIndex(tree->getOwnPosition(), 0, tree);

   int i = 0; // Children of 'Tree'.
   int j = 0; // Entries.
   int firstModified = -1;
   auto flushModified = [&]()
   {
      if (firstModified >= 0)
      {
         emit dataChanged(this->createIndex(firstModified, 0, tree->getChild(firstModified)),
            this->createIndex(i - 1, this->columnCount() - 1, tree->getChild(i - 1)));
         firstModified = -1;
      }
   };

   while (i < tree->getNbChildren() || j < entries.entries_size())
   {
      // New entry.
      if (i >= tree->getNbChildren() || j < entries.entries_size() && tree->getChild(i)->getItem() > entries.entries(j))
      {
         flushModified();
         const int first = j++;
         while (j < entries.entries_size() &&
            (i >= tree->getNbChildren() || entries.entries(j) < tree->getChild(i)->getItem()))
            j++;
         const int count = j - first;
         this->beginInsertRows(parentIndex, i, i + count - 1);
         tree->insertChildren(entries, first, count, i);
         this->endInsertRows();
         i += count;
      }
      else if (j >= entries.entries_size() || tree->getChild(i)->getItem() < entries.entries(j)) // Entry deleted.
      {
         flushModified();
         int last = i;
         while (last + 1 < tree->getNbChildren() &&
            (j >= entries.entries_size() || tree->getChild(last + 1)->getItem() < entries.entries(j)))
            last++;
         this->beginRemoveRows(parentIndex, i, last);
         tree->removeChildren(i, last - i + 1);
         this->endRemoveRows();
      }
      else // Entry paths are equal.
      {
         if (!sameDisplayedContent(tree->getChild(i)->getItem(), entries.entries(j)))
         {
            tree->getChild(i)->setItem(entries.entries(j));
            if (firstModified < 0)
               firstModified = i;
         }
         else
            flushModified();
         i++;
         j++;
      }
   }
   flushModified();
}

/**
  * Special case for the shared directories (roots). They may not be sorted in a alphabetic way.
  * They are identified by their ID.
  */
void BrowseModel::synchronizeRoot(const Protos::Common::Entries& entries)
{
   const QModelIndex parentIndex;
   QHash<QByteArray, Tree*> existing;
   existing.reserve(this->root->getNbChildren());
   auto id = [](const Protos::Common::Entry& entry) { return QByteArray::fromStdString(entry.shared_entry().id().hash()); };
   for (int row = 0; row < this->root->getNbChildren(); ++row)
   {
      auto* child = this->root->getChild(row);
      existing.insert(id(child->getItem()), child);
   }

   int row = 0;
   while (row < entries.entries_size())
   {
      auto* child = existing.value(id(entries.entries(row)), nullptr);
      if (child)
      {
         // Avoid a position scan for roots that are already in the right place.
         if (this->root->getChild(row) != child)
         {
            const int previousRow = child->getOwnPosition();
            this->beginMoveRows(parentIndex, previousRow, previousRow, parentIndex, row);
            this->root->moveChild(previousRow, row);
            this->endMoveRows();
         }
         if (entries.entries(row) != child->getItem() ||
            entries.entries(row).shared_entry().shared_name() != child->getItem().shared_entry().shared_name())
         {
            child->setItem(entries.entries(row));
            emit dataChanged(this->index(row, 0), this->index(row, this->columnCount() - 1));
         }
         ++row;
      }
      else
      {
         const int first = row++;
         while (row < entries.entries_size() && !existing.contains(id(entries.entries(row))))
            ++row;
         this->beginInsertRows(parentIndex, first, row - 1);
         this->root->insertChildren(entries, first, row - first, first);
         this->endInsertRows();
      }
   }

   if (row < this->root->getNbChildren())
   {
      this->beginRemoveRows(parentIndex, row, this->root->getNbChildren() - 1);
      this->root->removeChildren(row, this->root->getNbChildren() - row);
      this->endRemoveRows();
   }
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
   if (this->getItem().shared_entry().shared_name().size() == 0)
      this->getItem().mutable_shared_entry()->set_shared_name(this->getItem().name()); // For the root.
}

/**
  * When the last child of a directory is removed the directory becomes known to be empty, otherwise
  * 'hasUnloadedChildren()' would keep returning 'true' and the view would show a child which never comes,
  * asking to browse the directory again each time it's expanded.
  */
BrowseModel::Tree::~Tree()
{
   // 1 and not 0: it's the base 'Common::Tree::~Tree()', which runs after this destructor, that removes this
   // node from its parent. This node is therefore still counted here and 1 means it's the last one.
   if (this->getParent() && this->getParent()->getNbChildren() == 1)
      this->getParent()->getItem().set_is_empty(true);
}

void BrowseModel::Tree::insertChildren(const Protos::Common::Entries& entries)
{
   this->insertChildren(entries, 0, entries.entries_size(), this->getNbChildren());
}

void BrowseModel::Tree::insertChildren(const Protos::Common::Entries& entries, int first, int count, int position)
{
   std::vector<std::unique_ptr<Tree>> nodes;
   nodes.reserve(count);
   for (int i = 0; i < count; ++i)
      nodes.emplace_back(this->newTree(entries.entries(first + i)));

   this->children.insert(position, count, nullptr);
   for (int i = 0; i < count; ++i)
      this->children[position + i] = nodes[i].release();
   if (count > 0)
      this->getItem().set_is_empty(false);
}

void BrowseModel::Tree::removeChildren(int first, int count)
{
   const auto removed = this->children.sliced(first, count);
   this->children.remove(first, count);
   // Detach before deletion to avoid a parent-list scan and shift per node.
   for (auto* child : removed)
   {
      child->parent = nullptr;
      delete child;
   }
   if (this->children.isEmpty())
      this->getItem().set_is_empty(true);
}

void BrowseModel::Tree::setItem(const Protos::Common::Entry& entry)
{
   Common::Tree<Protos::Common::Entry, BrowseModel::Tree>::setItem(entry);
   this->copySharedDirFromParent();
}

bool BrowseModel::Tree::hasUnloadedChildren() const
{
   return
      this->getItem().type() == Protos::Common::Entry_Type_DIR &&
      this->getNbChildren() == 0 &&
      !this->getItem().is_empty();
}

QVariant BrowseModel::Tree::data(int column) const
{
   switch (column)
   {
   case NAME:
      {
         const QString& name = Common::ProtoHelper::getName(this->getItem());
         if (!name.isEmpty())
            return name;
         else
            return Common::Path(QString::fromStdString(this->getItem().shared_entry().path())).getLastElement();
      }
   case SIZE: return Common::Global::formatByteSize(this->getItem().size());
   default: return QVariant();
   }
}

void BrowseModel::Tree::copySharedDirFromParent()
{
   // Copy the shared directory ID from the parent.
   if (!this->getItem().has_shared_entry() && this->getParent() && this->getParent()->getParent())
      this->getItem().mutable_shared_entry()->CopyFrom(this->getParent()->getItem().shared_entry());
}

bool GUI::operator>(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type())
      return e1.type() == Protos::Common::Entry::FILE;

   return e1.name() > e2.name();
}

bool GUI::operator<(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   if (e1.type() != e2.type())
      return e1.type() == Protos::Common::Entry::DIR;

   return e1.name() < e2.name();
}

bool GUI::sameDisplayedContent(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   return
      e1.name() == e2.name() &&
      e1.type() == e2.type() &&
      e1.size() == e2.size() &&
      e1.is_empty() == e2.is_empty();
}
