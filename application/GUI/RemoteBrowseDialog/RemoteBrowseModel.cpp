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

#include <RemoteBrowseDialog/RemoteBrowseModel.h>
using namespace GUI;

#include <algorithm>

#include <QPixmap>
#include <QFileInfo>

#include <IconProvider.h>

#include <Common/Global.h>

#include <Log.h>

namespace
{
   bool entryLess(const Protos::GUI::LocalBrowseResult::Entry& left, const Protos::GUI::LocalBrowseResult::Entry& right)
   {
      if (left.type() != right.type())
         return left.type() == Protos::GUI::LocalBrowseResult::DIR;
      return left.name() < right.name();
   }

   /**
     * The paths with a drive letter and the network ones are those of Windows: their case doesn't matter.
     */
   Qt::CaseSensitivity caseSensitivity(const QString& path)
   {
      return (path.size() >= 2 && path[1] == ':') || path.startsWith("//") ? Qt::CaseInsensitive : Qt::CaseSensitive;
   }
}

/**
  * @class GUI::BrowseModel
  *
  * The model of a distant peer file system. The directory content is lazy loaded, see the method 'loadChildren()'.
  * Used by 'WidgetBrowse'.
  */

RemoteBrowseModel::RemoteBrowseModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection), filters(FILE | DIR), root(new Tree())
{
   this->browse(this->root);
}

RemoteBrowseModel::~RemoteBrowseModel()
{
   if (!this->localBrowseResult.isNull())
      this->localBrowseResult->disconnect(this);

   delete this->root;
}

bool RemoteBrowseModel::isLocal() const
{
   return this->coreConnection->isLocal();
}

QModelIndex RemoteBrowseModel::index(int row, int column, const QModelIndex& parent) const
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

   return QModelIndex();
}

QModelIndex RemoteBrowseModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
      return QModelIndex();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   Tree* parentItem = tree->getParent();

   if (!parentItem || parentItem == this->root)
      return QModelIndex();

   return this->createIndex(parentItem->getOwnPosition(), 0, parentItem);
}

int RemoteBrowseModel::rowCount(const QModelIndex& parent) const
{
   if (parent.column() > 0)
      return 0;

   const Tree* parentTree;

   if (!parent.isValid())
      parentTree = this->root;
   else
      parentTree = static_cast<Tree*>(parent.internalPointer());

   return parentTree->getNbChildren();
}

bool RemoteBrowseModel::hasChildren(const QModelIndex& parent) const
{
   if (parent.column() > 0)
      return false;
   const Tree* tree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;
   return tree->getNbChildren() > 0 || tree->hasUnloadedChildren();
}

bool RemoteBrowseModel::canFetchMore(const QModelIndex& parent) const
{
   if (parent.column() > 0)
      return false;
   const Tree* tree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;
   return tree->hasUnloadedChildren() &&
      (this->localBrowseResult.isNull() || parent != this->currentBrowseIndex) &&
      !this->pendingBrowseIndexes.contains(parent) && !this->pendingRefreshIndexes.contains(parent);
}

void RemoteBrowseModel::fetchMore(const QModelIndex& parent)
{
   if (this->canFetchMore(parent))
      this->loadChildren(parent);
}

int RemoteBrowseModel::columnCount(const QModelIndex&) const
{
   return 3;
}

QVariant RemoteBrowseModel::data(const QModelIndex& index, int role) const
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
            const auto& item = tree->getItem();
            QString name = QString::fromStdString(item.name());
            if (item.type() == Protos::GUI::LocalBrowseResult::DIR)
               name += "/";
            return IconProvider::getIcon(Common::Path(name));
         }
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return QVariant((index.column() == NAME ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default:
      return QVariant();
   }
}

void RemoteBrowseModel::setFilters(Filters filters)
{
   this->filters = filters;
}

/**
  * Shows or hides the hidden entries of the directories already loaded, the core isn't asked.
  */
void RemoteBrowseModel::setShowHidden(bool show)
{
   if (this->showHidden == show)
      return;
   this->showHidden = show;

   const bool browsingDirectory = this->currentBrowseIndex.isValid();

   QList<Tree*> trees { this->root };
   while (!trees.isEmpty())
   {
      Tree* tree = trees.takeLast();
      this->redisplay(tree);
      for (int i = 0; i < tree->getNbChildren(); ++i)
         trees.append(tree->getChild(i));
   }

   // The directory being browsed isn't displayed anymore, its entries must not be given to the root.
   if (browsingDirectory && !this->currentBrowseIndex.isValid() && !this->localBrowseResult.isNull())
   {
      this->localBrowseResult->disconnect(this);
      this->localBrowseResult.clear();
   }

   this->exploreDirectories();
   this->exploreCompletions();
   this->loadPendingChildren();
}

/**
  * The given entry and its parents remain displayed even if they are hidden.
  */
void RemoteBrowseModel::keepDisplayed(const QModelIndex& index)
{
   if (!index.isValid() || index.model() != this)
      return;

   for (Tree* tree = static_cast<Tree*>(index.internalPointer()); tree->getParent(); tree = tree->getParent())
      if (tree->getItem().hidden())
         tree->getParent()->revealedEntries.insert(QString::fromStdString(tree->getItem().name()));
}

/**
  * Returns the local path of the entry at the given index.
  */
QString RemoteBrowseModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   if (!index.isValid())
      return QString();
   if (!appendFilename && !this->isDirectory(index))
      return this->getPath(index.parent());
   return static_cast<Tree*>(index.internalPointer())->path();
}

bool RemoteBrowseModel::isDirectory(const QModelIndex& index) const
{
   return index.isValid() &&
      static_cast<Tree*>(index.internalPointer())->getItem().type() == Protos::GUI::LocalBrowseResult::DIR;
}

void RemoteBrowseModel::getIndexFromPath(const QString& path)
{
   this->exploring = false;
   // Accept either path separator, regardless of the core's OS or path prefix.
   QString normalized = path;
   normalized.replace('\\', '/');
   this->pathRequiresDirectory = normalized.endsWith('/');
   this->pathToExplore = QDir::cleanPath(normalized);
   if (!normalized.startsWith('/') && !(normalized.size() >= 3 && normalized[1] == ':' && normalized[2] == '/'))
   {
      emit indexFromPath(QModelIndex());
      return;
   }
   this->exploring = true;
   this->exploreDirectories();
}

void RemoteBrowseModel::getCompletions(const QString& path)
{
   this->pathToComplete = path;
   this->completing = true;
   this->exploreCompletions();
}

void RemoteBrowseModel::cancelPathLookup()
{
   this->exploring = false;
   this->completing = false;
}

void RemoteBrowseModel::refresh(const QModelIndexList& folders)
{
   if (this->refreshing)
      return;

   for (const auto& index : folders)
   {
      if (index.model() != this || !this->isDirectory(index))
         continue;
      const auto folder = index.siblingAtColumn(0);
      this->pendingBrowseIndexes.removeAll(folder);
      if ((this->localBrowseResult.isNull() || folder != this->currentBrowseIndex) &&
          !this->pendingRefreshIndexes.contains(folder))
         this->pendingRefreshIndexes.append(folder);
   }
   this->refreshing = true;
   emit refreshingChanged(true);
   this->loadPendingChildren();
}

void RemoteBrowseModel::result(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry> filteredEntries;
   for (const auto& entry : entries)
   {
      if (
         entry.type() == Protos::GUI::LocalBrowseResult::DIR && this->filters.testAnyFlag(DIR) ||
         entry.type() == Protos::GUI::LocalBrowseResult::FILE && this->filters.testAnyFlag(FILE)
      )
         filteredEntries.Add()->CopyFrom(entry);
   }

   Tree* tree = this->currentBrowseIndex.isValid() ? static_cast<Tree*>(this->currentBrowseIndex.internalPointer()) : this->root;
   tree->childrenLoaded = true;
   this->display(tree, filteredEntries);

   this->currentBrowseIndex = QModelIndex();
   this->localBrowseResult->disconnect(this);
   this->localBrowseResult.clear();

   this->exploreDirectories();
   this->exploreCompletions();
   this->loadPendingChildren();
}

void RemoteBrowseModel::resultTimeout()
{
   L_WARN("Asking for local entries message timed out");
   this->currentBrowseIndex = QModelIndex();
   this->localBrowseResult->disconnect(this);
   this->localBrowseResult.clear();
   const bool resolvingPath = this->exploring;
   this->cancelPathLookup();
   if (resolvingPath)
      emit indexFromPath(QModelIndex());
   this->loadPendingChildren();
}

void RemoteBrowseModel::browse(Tree* tree)
{
   this->localBrowseResult = this->coreConnection->localBrowse(tree->path(), !this->filters.testAnyFlag(FILE));
   connect(this->localBrowseResult.data(), &RCC::ILocalBrowseResult::result, this, &RemoteBrowseModel::result);
   connect(this->localBrowseResult.data(), &Common::Timeoutable::timeout, this, &RemoteBrowseModel::resultTimeout);
   this->localBrowseResult->start();
}

void RemoteBrowseModel::loadChildren(const QPersistentModelIndex &index)
{
   Tree* tree = index.isValid() ? static_cast<Tree*>(index.internalPointer()) : this->root;
   if (!tree->hasUnloadedChildren())
      return;
   if (this->pendingRefreshIndexes.contains(index))
      return;
   if (!this->localBrowseResult.isNull())
   {
      // Expanding the view must not cancel the request needed by a path lookup.
      if (index != this->currentBrowseIndex && !this->pendingBrowseIndexes.contains(index))
         this->pendingBrowseIndexes.append(index);
      return;
   }
   this->currentBrowseIndex = index;
   this->browse(tree);
}

void RemoteBrowseModel::loadPendingChildren()
{
   while (this->localBrowseResult.isNull() && !this->pendingRefreshIndexes.isEmpty())
   {
      const auto index = this->pendingRefreshIndexes.takeFirst();
      // A parent refresh may have removed this folder in the meantime.
      if (!index.isValid())
         continue;
      this->currentBrowseIndex = index;
      this->browse(static_cast<Tree*>(index.internalPointer()));
   }
   while (this->localBrowseResult.isNull() && !this->pendingBrowseIndexes.isEmpty())
   {
      const auto index = this->pendingBrowseIndexes.takeFirst();
      if (index.isValid())
         this->loadChildren(index);
   }
   if (this->refreshing && this->localBrowseResult.isNull())
   {
      this->refreshing = false;
      emit refreshingChanged(false);
   }
}

/**
  * Displays the given entries of a directory.
  * The hidden ones are kept aside unless they are shown or have been revealed.
  */
void RemoteBrowseModel::display(Tree* tree, const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry> displayedEntries;
   google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry> hiddenEntries;
   for (const auto& entry : entries)
   {
      if (entry.hidden() && !this->showHidden && !tree->revealedEntries.contains(QString::fromStdString(entry.name())))
         hiddenEntries.Add()->CopyFrom(entry);
      else
         displayedEntries.Add()->CopyFrom(entry);
   }

   std::sort(displayedEntries.begin(), displayedEntries.end(), entryLess);

   tree->hiddenEntries.Swap(&hiddenEntries);
   this->synchronize(tree, displayedEntries);
}

/**
  * Displays again the known entries of a directory, to be called when the entries to display have changed.
  */
void RemoteBrowseModel::redisplay(Tree* tree)
{
   if (tree->hiddenEntries.empty() && tree->getNbChildren() == 0)
      return;

   google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry> entries(tree->hiddenEntries);
   for (int i = 0; i < tree->getNbChildren(); ++i)
      entries.Add()->CopyFrom(tree->getChild(i)->getItem());
   this->display(tree, entries);
}

void RemoteBrowseModel::synchronize(Tree* tree, const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries)
{
   const auto parent = this->indexFromTree(tree);
   int row = 0;
   int entry = 0;
   while (row < tree->getNbChildren() || entry < entries.size())
   {
      if (row < tree->getNbChildren() &&
          (entry == entries.size() || entryLess(tree->getChild(row)->getItem(), entries.Get(entry))))
      {
         int count = 1;
         while (row + count < tree->getNbChildren() &&
                (entry == entries.size() || entryLess(tree->getChild(row + count)->getItem(), entries.Get(entry))))
            ++count;
         this->beginRemoveRows(parent, row, row + count - 1);
         for (int i = 0; i < count; ++i)
            delete tree->getChild(row);
         this->endRemoveRows();
      }
      else if (entry < entries.size() &&
               (row == tree->getNbChildren() || entryLess(entries.Get(entry), tree->getChild(row)->getItem())))
      {
         int count = 1;
         while (entry + count < entries.size() &&
                (row == tree->getNbChildren() || entryLess(entries.Get(entry + count), tree->getChild(row)->getItem())))
            ++count;
         this->beginInsertRows(parent, row, row + count - 1);
         for (int i = 0; i < count; ++i)
            tree->insertChild(entries.Get(entry + i), row + i);
         this->endInsertRows();
         row += count;
         entry += count;
      }
      else
      {
         auto* child = tree->getChild(row);
         const auto& updated = entries.Get(entry);
         if (child->getItem().SerializeAsString() != updated.SerializeAsString())
         {
            child->setItem(updated);
            emit dataChanged(this->index(row, 0, parent), this->index(row, this->columnCount() - 1, parent));
         }
         ++row;
         ++entry;
      }
   }
}

/**
  * Looks for the entry having the given cleaned path among the known ones, starting from the root.
  * Returns 'false' if the entries of a directory of the path aren't known yet: they are asked and the lookup has
  * to be done again when some entries are received, an entry may be deleted in the meantime.
  * Otherwise 'tree' is the entry, null if there is none.
  */
bool RemoteBrowseModel::find(const QString& path, Tree*& tree)
{
   const Qt::CaseSensitivity sensitivity = caseSensitivity(path);
   tree = this->root;
   while (tree)
   {
      if (tree != this->root && QDir::cleanPath(tree->path()).compare(path, sensitivity) == 0)
         return true;
      if (tree->hasUnloadedChildren())
      {
         this->loadChildren(this->indexFromTree(tree));
         return false;
      }
      const auto isOnPath = [&](const QString& entryPath, const Protos::GUI::LocalBrowseResult::Entry& entry) {
         const QString prefix = entryPath.endsWith('/') ? entryPath : entryPath + '/';
         return path.compare(entryPath, sensitivity) == 0 ||
            (entry.type() == Protos::GUI::LocalBrowseResult::DIR && path.startsWith(prefix, sensitivity));
      };
      Tree* match = nullptr;
      qsizetype longestMatch = -1;
      for (int i = 0; i < tree->getNbChildren(); ++i)
      {
         Tree* child = tree->getChild(i);
         const QString childPath = QDir::cleanPath(child->path());
         if (isOnPath(childPath, child->getItem()) && childPath.size() > longestMatch)
         {
            match = child;
            longestMatch = childPath.size();
         }
      }
      if (!match)
      {
         // A hidden entry can still be reached by its path, it's then displayed and found by the next iteration.
         const QString directoryPath = tree->path();
         bool revealed = false;
         for (const auto& entry : tree->hiddenEntries)
         {
            const QString name = QString::fromStdString(entry.name());
            if (isOnPath(QDir::cleanPath(directoryPath + name), entry))
            {
               tree->revealedEntries.insert(name);
               revealed = true;
               break;
            }
         }
         if (revealed)
         {
            this->redisplay(tree);
            continue;
         }
      }
      tree = match;
   }
   return true;
}

void RemoteBrowseModel::exploreDirectories()
{
   Tree* tree;
   if (!this->exploring || !this->find(this->pathToExplore, tree))
      return;

   const QModelIndex index = this->indexFromTree(tree);
   this->exploring = false;
   emit indexFromPath(!this->pathRequiresDirectory || this->isDirectory(index) ? index : QModelIndex());
}

/**
  * The completions of a path are the entries of its directory, which is thus looked for.
  */
void RemoteBrowseModel::exploreCompletions()
{
   if (!this->completing)
      return;

   QString normalized = this->pathToComplete;
   normalized.replace('\\', '/');
   const qsizetype nameStart = normalized.lastIndexOf('/') + 1;
   const QString name = normalized.mid(nameStart);

   // Without any separator the path is the beginning of a root entry: a drive.
   Tree* directory = this->root;
   if (nameStart > 0 && !this->find(QDir::cleanPath(normalized.left(nameStart)), directory))
      return;
   if (directory && directory->hasUnloadedChildren())
   {
      this->loadChildren(this->indexFromTree(directory));
      return;
   }

   QStringList paths;
   if (directory && directory->getItem().type() == Protos::GUI::LocalBrowseResult::DIR && !normalized.isEmpty())
   {
      // The case of a drive letter doesn't matter.
      const Qt::CaseSensitivity sensitivity =
         directory == this->root ? Qt::CaseInsensitive : caseSensitivity(normalized);
      QList<const Protos::GUI::LocalBrowseResult::Entry*> entries;
      const auto add = [&](const Protos::GUI::LocalBrowseResult::Entry& entry) {
         if (QString::fromStdString(entry.name()).startsWith(name, sensitivity))
            entries.append(&entry);
      };
      for (int i = 0; i < directory->getNbChildren(); ++i)
         add(directory->getChild(i)->getItem());
      // The entries which aren't displayed are proposed once the beginning of their name is typed.
      if (!name.isEmpty())
         for (const auto& entry : directory->hiddenEntries)
            add(entry);
      std::sort(entries.begin(), entries.end(), [](const auto* left, const auto* right) {
         return entryLess(*left, *right);
      });

      const QString directoryPath = directory->path();
      for (const auto* entry : entries)
      {
         QString path = directoryPath + QString::fromStdString(entry->name());
         if (entry->type() == Protos::GUI::LocalBrowseResult::DIR && !path.endsWith('/'))
            path += '/';
         paths << path;
      }
   }

   this->completing = false;
   emit completions(this->pathToComplete, paths);
}

/**
  * Returns the index corresponding to the given tree, the root has no index: QModelIndex().
  * The position is asked to the tree itself, it can't be cached in 'find(..)' because the lookup is
  * done again each time a browse result is received.
  */
QModelIndex RemoteBrowseModel::indexFromTree(Tree* tree) const
{
   if (!tree || tree == this->root)
      return QModelIndex();

   return this->createIndex(tree->getOwnPosition(), 0, tree);
}

/////

/**
  * @class GUI::Tree
  *
  * Either a file or a directory in the tree view structure.
  */

RemoteBrowseModel::Tree::Tree()
{
   this->getItem().set_type(Protos::GUI::LocalBrowseResult::DIR);
}

RemoteBrowseModel::Tree::Tree(const Protos::GUI::LocalBrowseResult::Entry& entry, Tree* parent) :
   Common::Tree<Protos::GUI::LocalBrowseResult::Entry, RemoteBrowseModel::Tree>(entry, parent)
{
}

RemoteBrowseModel::Tree::~Tree()
{
}

void RemoteBrowseModel::Tree::insertChildren(
   const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries
)
{
   for (int i = 0; i < entries.size(); i++)
      this->insertChild(entries.at(i));
}

bool RemoteBrowseModel::Tree::hasUnloadedChildren() const
{
   return
      this->getItem().type() == Protos::GUI::LocalBrowseResult::DIR  &&
      !this->childrenLoaded &&
      (!this->getParent() || this->getItem().size() > 0);
}

QVariant RemoteBrowseModel::Tree::data(int column) const
{
   switch (column)
   {
   case NAME:
      {
         const auto& item = this->getItem();
         if (item.volume_label().empty())
            return QString::fromStdString(item.name());
         else
         {
            QString name = QString::fromStdString(item.name());
            if (name.size() >= 3 && (name.endsWith('/') || name.endsWith('\\')))
               name.removeLast();
            return QString(QString::fromStdString(item.volume_label()) + " (" + name + ")");
         }
      }

   case DATE_MODIFIED:
      {
         const auto& item = this->getItem();
         if (item.volume_label().empty() && item.date_modified() != 0)
            return QDateTime::fromMSecsSinceEpoch(this->getItem().date_modified());
         else
            return QVariant();
      }

   case SIZE:
      {
         const auto& item = this->getItem();
         if (item.type() == Protos::GUI::LocalBrowseResult::FILE)
            return Common::Global::formatByteSize(this->getItem().size());
         else if (this->getItem().capacity() != 0)
            return
               QString(
                  Common::Global::formatByteSize(this->getItem().capacity() - this->getItem().size()) + " " +
                  tr("free")
               );
         else
            return QVariant();
      }
   default: return QVariant();
   }
}

QString RemoteBrowseModel::Tree::path() const
{
   QString path;

   const Tree* current = this;
   while (current->getParent()) {
      const auto& item = current->getItem();
      const bool isDir = item.type() == Protos::GUI::LocalBrowseResult::DIR;

      QString name = QString::fromStdString(item.name());

      if (isDir && !name.endsWith('/'))
         path.prepend('/');

      path.prepend(name);

      current = current->getParent();
   }

   return path;
}
