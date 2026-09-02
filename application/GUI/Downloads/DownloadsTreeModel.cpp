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

#include <Downloads/DownloadsTreeModel.h>
using namespace GUI;

#include <algorithm>

#include <QtGlobal>

#include <Common/ProtoHelper.h>
#include <Common/StringUtils.h>

#include <Log.h>

/**
  * @class DownloadsTreeModel
  *
  * This model shows the download queue as a tree and is designed to be used with a 'QTreeView'.
  * Each node is a directory and the leaves are the queued files or directories.
  * The method 'onNewState(..)' is automatically and periodically called by 'DownloadsModel' each time a
  * new state is sent by the core.
  */

DownloadsTreeModel::DownloadsTreeModel(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const PeerListModel& peerListModel,
   const SharedEntryListModel& sharedEntryListModel,
   const IFilter<DownloadFilterStatus>& filter
) :
   DownloadsModel(coreConnection, peerListModel, sharedEntryListModel, filter), root(new Tree())
{
}

DownloadsTreeModel::~DownloadsTreeModel()
{
   delete this->root;
}

/**
  * @return The IDs of all sub-directories and files together. It includes the top item at the given index.
  */
QList<quint64> DownloadsTreeModel::getDownloadIDs(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return QList<quint64>();

   return this->getDownloadIDs(tree);
}

/**
  * @return 'true' is all files in the tree corresponding to the given index are paused, the completed files are ignored.
  */
bool DownloadsTreeModel::isDownloadPaused(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return true;

   if (tree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
      return tree->getItem().status() == Protos::Common::DownloadStatus::PAUSED;

   for (Common::TreeBreadthFirstIterator<Tree> i(tree); i.hasNext();)
   {
      Tree* current = i.next();
      if (
         current->getItem().local_entry().type() == Protos::Common::Entry::FILE &&
         current->getItem().status() != Protos::Common::DownloadStatus::PAUSED &&
         current->getItem().status() != Protos::Common::DownloadStatus::COMPLETE
      )
         return false;
   }

   return true;
}

bool DownloadsTreeModel::isEntryLocationKnown(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   for (Common::TreeBreadthFirstIterator<Tree> i(tree, true); i.hasNext();)
   {
      Tree* current = i.next();
      if (current->getItem().local_entry().exists())
         return true;
   }

   return false;
}

bool DownloadsTreeModel::isFileComplete(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   if (tree->getItem().local_entry().type() == Protos::Common::Entry::DIR && tree->getItem().id() == 0)
   {
      for (Common::TreeBreadthFirstIterator<Tree> i(tree); i.hasNext();)
      {
         Tree* current = i.next();
         if (
            current->getItem().local_entry().type() == Protos::Common::Entry::FILE &&
            current->getItem().status() != Protos::Common::DownloadStatus::COMPLETE
         )
            return false;
      }
      return true;
   }

   return tree->getItem().status() == Protos::Common::DownloadStatus::COMPLETE;
}

bool DownloadsTreeModel::isSourceAlive(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   return tree->getItem().peer_ids_size() > 0 && !this->peerListModel.getNick(tree->getItem().peer_ids(0).hash()).isNull();
}

Protos::Common::Entry::Type DownloadsTreeModel::getType(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return Protos::Common::Entry::FILE;

   return tree->getItem().local_entry().type();
}

QString DownloadsTreeModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return QString();

   const Common::SharedEntry& sharedEntry =
      this->sharedEntryListModel.getSharedEntry(tree->getSharedEntryId());

   if (sharedEntry.isNull())
      return QString();

   if (sharedEntry.path.isFile())
   {      
      return sharedEntry.path.toString(appendFilename);
   }
   else
   {
      if (tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
      {
         QString path;
         Tree* superTree = tree;
         while (superTree != this->root)
         {
            path.prepend(QString::fromStdString(superTree->getItem().local_entry().name())).prepend('/');
            superTree = superTree->getParent();
         }
         const QString& sharedPath = sharedEntry.path.toString();
         path.prepend(sharedPath.left(sharedPath.size() - 1));
         return path;
      }
      else
      {
         return DownloadsModel::getExistingPathOrParentDirectory(
            sharedEntry.path.append(Common::ProtoHelper::getPath(tree->getItem().local_entry())),
            appendFilename
         );
      }
   }
}

bool DownloadsTreeModel::hasChildren(const QModelIndex& parent) const
{
   return this->rowCount(parent) > 0;
}

int DownloadsTreeModel::rowCount(const QModelIndex& parent) const
{
   if (parent.column() > 0)
      return 0;

   if (!parent.isValid())
      return this->root->getNbChildren();
   else
      return static_cast<Tree*>(parent.internalPointer())->getNbChildren();
}

QVariant DownloadsTreeModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid())
      return QVariant();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (tree)
      return DownloadsModel::getData(tree->getItem(), index, role);
   return QVariant();
}

QModelIndex DownloadsTreeModel::index(int row, int column, const QModelIndex& parent) const
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

   return QModelIndex();
}

QModelIndex DownloadsTreeModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
      return QModelIndex();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   Tree* parentItem = tree->getParent();

   if (!parentItem || parentItem == this->root)
      return QModelIndex();

   return this->createIndex(parentItem->getOwnPosition(), 0, parentItem);
}

/**
  * Must be overridden because 'QAbstractTableModel::sibling(..)' assumes a flat model and drops the
  * parent ('return index(row, column);'), which corrupts the selection ranges built by
  * 'QTreeViewPrivate::select(..)' (shift-click, rubber band, shift+arrows). Same bug family as
  * 'hasChildren(..)' and 'parent(..)'.
  */
QModelIndex DownloadsTreeModel::sibling(int row, int column, const QModelIndex& index) const
{
   if (!index.isValid())
      return QModelIndex();

   return this->index(row, column, this->parent(index));
}

Qt::DropActions DownloadsTreeModel::supportedDropActions() const
{
   return Qt::MoveAction;
}

Qt::ItemFlags DownloadsTreeModel::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid() && static_cast<Tree*>(index.internalPointer())->getParent() == this->root)
       return Qt::ItemIsDragEnabled | defaultFlags;
   else
       return Qt::ItemIsDropEnabled | defaultFlags;
}

bool DownloadsTreeModel::dropMimeData(
   const QMimeData* data,
   Qt::DropAction action,
   int where,
   int /*column*/,
   const QModelIndex& parent
)
{
   // We can only drop on the root.
   if (where == -1 || !data || action != Qt::MoveAction || where > this->root->getNbChildren() || parent.isValid())
       return false;

   QList<int> rows = this->getDraggedRows(data);
   if (rows.isEmpty())
      return false;

   std::sort(rows.begin(), rows.end()); // TODO: is 'getDraggedRows(..)' returns a sorted list?

   const int first = rows.first();
   const int last = rows.last();

   int begin = 0;
   int end = 0;
   if (abs(where - first) > abs(where - last))
   {
      begin = qMin(where, first);
      end = qMax(where, first);
   }
   else
   {
      begin = qMin(where, last);
      end = qMax(where, last);
   }

   Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE;
   if (where > (first + last + 1) / 2)
   {
      end--;
      position = Protos::GUI::MoveDownloads::AFTER;
   }

   QList<quint64> downloadIDsToMove;
   QList<quint64> downloadRefs;
   for (int i = begin; i <= end; i++)
   {
      if (!rows.empty() && rows.first() == i)
      {
         rows.removeFirst();
         downloadIDsToMove << this->getDownloadIDs(this->root->getChild(i));
      }
      else
      {
         downloadRefs << this->getDownloadIDs(this->root->getChild(i));
      }
   }

   if (downloadRefs.isEmpty())
   {
      if (last < this->root->getNbChildren() - 1 && (where > (first + last + 1) / 2 || first == 0))
      {

         downloadRefs << this->getDownloadIDs(this->root->getChild(last + 1));
         position = Protos::GUI::MoveDownloads::BEFORE;
      }
      else if (first > 0)
      {
         downloadRefs << this->getDownloadIDs(this->root->getChild(first - 1));
         position = Protos::GUI::MoveDownloads::AFTER;
      }
   }

   // Some rows to move may not have be processed by the last loop.
   for (QListIterator<int> i(rows); i.hasNext();)
      downloadIDsToMove << this->getDownloadIDs(this->root->getChild(i.next()));

   this->coreConnection->moveDownloads(downloadRefs, downloadIDsToMove, position);

   return true;
}

/**
  * Three passes are done:
  *  1) The existing entries are updated and marked as visited with their ancestors.
  *  2) The entries not visited are deleted, contiguous siblings are removed in one 'beginRemoveRows(..)' / 'endRemoveRows()'.
  *  3) The top level entries are reordered to follow the queue order and the new entries are created.
  * The deletion must happen before the reordering: when a block of entries is removed (for example the completed downloads)
  * every remaining top level entry would otherwise be moved above the removed ones, one 'beginMoveRows(..)' at a time, which
  * is very slow for the view.
  */
void DownloadsTreeModel::onNewState(const Protos::GUI::State& state)
{
   const QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   // All nodes set as unvisited.
   for (Common::TreeBreadthFirstIterator<Tree> i(this->root); i.hasNext();)
      i.next()->visited = false;

   // First pass: update the existing entries and mark them and their ancestors as visited.
   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.downloads(activeDownloadIndices[i]);

      if (Tree* itemTree = this->indexedEntries.value(download.id(), 0))
      {
         for (Tree* tree = itemTree; tree; tree = tree->getParent())
            tree->visited = true;

         this->update(itemTree, download);
      }
   }

   // Second pass: delete the entries which no longer exist.
   // We can't use the iterator 'TreeBreadthIterator' because the structure is altered during the loop.
   QList<Tree*> trees;
   trees << this->root;
   while (!trees.isEmpty())
   {
      Tree* currentTree = trees.takeLast();
      for (int i = 0; i < currentTree->getNbChildren(); i++)
      {
         if (currentTree->getChild(i)->visited)
         {
            if (currentTree->getChild(i)->getNbChildren() > 0)
               trees << currentTree->getChild(i);
            continue;
         }

         // Contiguous unvisited siblings are removed together.
         int last = i;
         while (last + 1 < currentTree->getNbChildren() && !currentTree->getChild(last + 1)->visited)
            last++;

         for (int j = i; j <= last; j++)
            this->updateDirectoriesEntryDeleted(currentTree->getChild(j));

         this->beginRemoveRows(
            currentTree == this->root ?
                 QModelIndex()
               : this->createIndex(currentTree->getOwnPosition(), 0, currentTree),
            i,
            last
         );
         for (int j = i; j <= last; j++)
         {
            Tree* treeToDelete = currentTree->getChild(i); // Always 'i': a deleted tree removes itself from its parent.
            for (Common::TreeBreadthFirstIterator<Tree> k(treeToDelete, true); k.hasNext();)
            {
               Tree* treeChild = k.next();
               if (treeChild->getItem().id() != 0)
                  this->indexedEntries.remove(treeChild->getItem().id());
            }
            delete treeToDelete;
         }
         this->endRemoveRows();
         i--; // The next child to examine is now at the position 'i'.
      }
   }

   // Third pass: the top level entries are reordered and the new entries are created.
   // 'visited' is reused here to know which top level entries have already been processed, see 'moveUp(..)' and 'insert(..)'.
   for (int i = 0; i < this->root->getNbChildren(); i++)
      this->root->getChild(i)->visited = false;

   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.downloads(activeDownloadIndices[i]);

      if (Tree* itemTree = this->indexedEntries.value(download.id(), 0)) // The item already exists, its top level ancestor may have to be moved.
      {
         Tree* topTree = itemTree;
         while (topTree->getParent() != this->root)
            topTree = topTree->getParent();

         topTree->visited = true;
         this->moveUp(topTree);
      }
      else // We have to create a new entry in the tree.
      {
         const QStringList& path =
            QString::fromStdString(download.local_entry().path()).split('/', Qt::SkipEmptyParts);

         // A node is created for each directory.
         Tree* currentTree = this->root;
         for (QStringListIterator i(path); i.hasNext();)
            currentTree = this->updateDirectoryFromPath(
               currentTree,
               i.next(),
               QString::fromStdString(download.peer_source_nick()),
               download.peer_ids_size() == 0 ? Common::Hash() : Common::Hash(download.peer_ids(0).hash()),
               download.local_entry().shared_entry().id().hash()
            );

         this->insert(currentTree, download);
      }
   }
}

QList<quint64> DownloadsTreeModel::getDownloadIDs(Tree* tree) const
{
   int id;
   QList<quint64> IDs;

   if ((id = tree->getItem().id()) != 0)
      IDs << id;

   // We have to send all sub item IDs.
   for (Common::TreeBreadthFirstIterator<Tree> i(tree); i.hasNext();)
   {
      const Tree* current = i.next();

      if ((id = current->getItem().id()) != 0)
         IDs << id;
   }
   return IDs;
}

/**
  * Insert (or update if it already exists) the given directory ('dir') as a sub-node of the given tree ('parentTree').
  * The id of all new directories equals 0.
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoryFromPath(
   Tree* parentTree,
   const QString& dir,
   const QString& peerSourceNick,
   const Common::Hash& peerSourceID,
   const Common::Hash& sharedDirID
)
{
   Protos::GUI::State::Download download;
   download.mutable_local_entry()->set_name(dir.toStdString());
   download.set_peer_source_nick(peerSourceNick.toStdString());
   download.add_peer_ids()->set_hash(peerSourceID.getData(), Common::Hash::HASH_SIZE);
   download.mutable_local_entry()->mutable_shared_entry()->mutable_id()->set_hash(sharedDirID.getData(), Common::Hash::HASH_SIZE);
   download.mutable_local_entry()->set_type(Protos::Common::Entry::DIR);

   // If the directory already exist, we just update it.
   for (int i = 0; i < parentTree->getNbChildren(); i++)
      // Top entries may have the same name, we can't use the shared id as he commented code below because it may be defined only for downloading and finished files.
      //if ((parentTree != this->root || download.local_entry().shared_dir().id().hash() == parentTree->getChild(i)->getItem().local_entry().shared_dir().id().hash()) && download.local_entry().name() == parentTree->getChild(i)->getItem().local_entry().name())
      if (download.local_entry().name() == parentTree->getChild(i)->getItem().local_entry().name())
         return this->update(parentTree->getChild(i), download);

   return this->insert(parentTree, download);
}

/**
  * Insert a new download into the given tree. It can be a file or a directory.
  * @param tree
  * @param download
  * @return
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::insert(Tree* entry, const Protos::GUI::State::Download& download)
{
   const int nbChildren = entry->getNbChildren();

   // Special case, the children of the root aren't sorted in an alphabetic way.
   if (entry == this->root)
   {
      int i = nbChildren;
      while (i >= 1 && !this->root->getChild(i-1)->visited)
         i--;
      return this->createEntry(QModelIndex(), i, download);
   }

   // We find a place to create the new entry and to keep the children in alphabetic order.
   for (int i = 0; i <= nbChildren; i++)
   {
      if (i == nbChildren || (entry != this->root && download < entry->getChild(i)->getItem())) // The root elements aren't sorted.
      {
         QModelIndex parentIndex = entry == this->root ? QModelIndex() : this->createIndex(entry->getOwnPosition(), 0, entry);
         return this->createEntry(parentIndex, i, download);
      }
   }
   return 0;
}

DownloadsTreeModel::Tree* DownloadsTreeModel::createEntry(
   const QModelIndex& parent,
   int position,
   const Protos::GUI::State::Download& download
)
{
   this->beginInsertRows(parent, position, position);
   Tree* parentTree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;
   Tree* newTree = parentTree->insertChild(download, position);
   if (newTree->getItem().id() != 0)
      this->indexedEntries.insert(download.id(), newTree);
   this->endInsertRows();

   // If the created entry is a leaf then its ancestor directories are updated.
   if (download.id() != 0)
      this->updateDirectoriesNewEntry(newTree);

   return newTree;
}

/**
  * Move the given tree right after the last visited tree ('Tree::visited' == true).
  * The tree must be a direct child of the root.
  * Used when reordering the top items against the download file list.
  * @param tree
  * @return the given tree
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::moveUp(DownloadsTreeModel::Tree* tree)
{
   Q_ASSERT(tree);
   Q_ASSERT(tree->getParent() == this->root);

   const int ownPosition = tree->getOwnPosition();
   int i = ownPosition;
   while (i >= 1 && !this->root->getChild(i-1)->visited)
      i--;

   if (ownPosition > i)
   {
      this->beginMoveRows(QModelIndex(), ownPosition, ownPosition, QModelIndex(), i);
      this->root->moveChild(ownPosition, i);
      this->endMoveRows();
   }

   return tree;
}

/**
  * Update the given entry. Then recursively update all parent directories.
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::update(Tree* entry, const Protos::GUI::State::Download& download)
{
   Q_ASSERT(entry);

   entry->visited = true;

   if (entry->getItem().id() != 0 && entry->getItem() != download)
   {
      if (download.id() == 0)
         this->indexedEntries.remove(entry->getItem().id());

      const Protos::GUI::State::Download oldDownload = entry->getItem();
      entry->setItem(download);
      this->updateDirectoriesEntryModified(entry, oldDownload);
      const int treePosition = entry->getOwnPosition();
      emit dataChanged(this->createIndex(treePosition, 0, entry), this->createIndex(treePosition, this->columnCount() - 1, entry));
   }

   return entry;
}

bool DownloadsTreeModel::isErroneous(Protos::Common::DownloadStatus status)
{
   return status >= Protos::Common::DownloadStatus::UNKNOWN_PEER_SOURCE;
}

/**
  * The counters for a single entry (a file or a directory from the core) which has the given status.
  */
DownloadsTreeModel::StatusCounters DownloadsTreeModel::countersOf(Protos::Common::DownloadStatus status)
{
   return {
      isErroneous(status) ? 1 : 0,
      status == Protos::Common::DownloadStatus::PAUSED ? 1 : 0,
      status == Protos::Common::DownloadStatus::DOWNLOADING ? 1 : 0
   };
}

/**
  * The given entry is about to be deleted with all its content: the sizes and the counters of its ancestors are decreased.
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesEntryDeleted(Tree* entry)
{
   const qint64 size = entry->getItem().local_entry().size();
   const qint64 downloadedBytes = entry->getItem().downloaded_bytes();

   // A directory node (id == 0) aggregates the counters of all the files it contains, an entry from the core counts for itself.
   const StatusCounters counters =
      entry->getItem().id() == 0 ?
           StatusCounters { entry->nbErrorFiles, entry->nbPausedFiles, entry->nbDownloadingFiles }
         : countersOf(entry->getItem().status());

   return this->updateDirectories(
      entry,
      -size,
      -downloadedBytes,
      { -counters.nbErrorFiles, -counters.nbPausedFiles, -counters.nbDownloadingFiles }
   );
}

DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesNewEntry(Tree* entry)
{
   const qint64 size = entry->getItem().local_entry().size();
   const qint64 downloadedBytes = entry->getItem().downloaded_bytes();
   const Protos::Common::DownloadStatus status = entry->getItem().status();

   return this->updateDirectories(entry, size, downloadedBytes, countersOf(status), status);
}

DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesEntryModified(
   Tree* entry,
   const Protos::GUI::State::Download& oldDownload
)
{
   const qint64 itemSizeDelta =
      static_cast<qint64>(entry->getItem().local_entry().size()) - static_cast<qint64>(oldDownload.local_entry().size());

   const qint64 itemDownloadedBytesDelta =
      static_cast<qint64>(entry->getItem().downloaded_bytes()) - static_cast<qint64>(oldDownload.downloaded_bytes());

   const Protos::Common::DownloadStatus newStatus = entry->getItem().status();
   const StatusCounters oldCounters = countersOf(oldDownload.status());
   const StatusCounters newCounters = countersOf(newStatus);

   return this->updateDirectories(
      entry,
      itemSizeDelta,
      itemDownloadedBytesDelta,
      {
         newCounters.nbErrorFiles - oldCounters.nbErrorFiles,
         newCounters.nbPausedFiles - oldCounters.nbPausedFiles,
         newCounters.nbDownloadingFiles - oldCounters.nbDownloadingFiles
      },
      newStatus
   );
}

/**
  * Update all parent directories of the given entry.
  * The following data are updated:
  *  - Size
  *  - Bytes downloaded
  *  - Number of erroneous, paused and downloading files
  *  - Status
  * @param countersDelta Applied to the counters of each ancestor.
  * @param errorStatus The new status of the entry when it's erroneous, it becomes the status of the ancestors
  *        when they contain only this erroneous file. Give a non-erroneous status (default) if the entry isn't erroneous
  *        or if it's unknown (deletion).
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectories(
   Tree* entry,
   qint64 entrySizeDelta,
   qint64 entryDownloadedBytesDelta,
   const StatusCounters& countersDelta,
   Protos::Common::DownloadStatus errorStatus
)
{
   if (
      entrySizeDelta == 0 &&
      entryDownloadedBytesDelta == 0 &&
      countersDelta.nbErrorFiles == 0 &&
      countersDelta.nbPausedFiles == 0 &&
      countersDelta.nbDownloadingFiles == 0 &&
      !isErroneous(errorStatus) // An erroneous entry may have switched to another kind of error.
   )
      return entry;

   Tree* currentDirectory = entry->getParent();
   while (currentDirectory != this->root)
   {
      currentDirectory->getItem().mutable_local_entry()->set_size(
         static_cast<qint64>(currentDirectory->getItem().local_entry().size()) + entrySizeDelta
      );

      currentDirectory->getItem().set_downloaded_bytes(
         static_cast<qint64>(currentDirectory->getItem().downloaded_bytes()) + entryDownloadedBytesDelta
      );

      currentDirectory->nbErrorFiles += countersDelta.nbErrorFiles;
      currentDirectory->nbPausedFiles += countersDelta.nbPausedFiles;
      currentDirectory->nbDownloadingFiles += countersDelta.nbDownloadingFiles;

      if (currentDirectory->getItem().local_entry().size() == currentDirectory->getItem().downloaded_bytes())
         currentDirectory->getItem().set_status(Protos::Common::DownloadStatus::COMPLETE);
      else if (currentDirectory->nbErrorFiles > 0)
      {
         // The status of the erroneous entry is shown if it's the only one or if the directory doesn't show an error yet.
         // Otherwise the error currently shown is kept: we don't know the status of the other erroneous files.
         if (isErroneous(errorStatus) && (currentDirectory->nbErrorFiles == 1 || !isErroneous(currentDirectory->getItem().status())))
            currentDirectory->getItem().set_status(errorStatus);
      }
      else if (currentDirectory->nbPausedFiles > 0)
         currentDirectory->getItem().set_status(Protos::Common::DownloadStatus::PAUSED);
      else if (currentDirectory->nbDownloadingFiles > 0)
         currentDirectory->getItem().set_status(Protos::Common::DownloadStatus::DOWNLOADING);
      else
         currentDirectory->getItem().set_status(Protos::Common::DownloadStatus::QUEUED);

      const int currentDirectoryPosition = currentDirectory->getOwnPosition();
      emit dataChanged(
         this->createIndex(currentDirectoryPosition, 0, currentDirectory),
         this->createIndex(currentDirectoryPosition, this->columnCount() - 1, currentDirectory)
      );

      currentDirectory = currentDirectory->getParent();
   }

   return entry;
}

/////

DownloadsTreeModel::Tree::Tree() :
   visited(true), nbPausedFiles(0), nbErrorFiles(0), nbDownloadingFiles(0)
{
   this->getItem().set_status(Protos::Common::DownloadStatus::QUEUED);
}

DownloadsTreeModel::Tree::Tree(const Protos::GUI::State::Download& download, Tree* parent) :
   Common::Tree<Protos::GUI::State::Download, Tree>(download, parent),
   visited(true),
   nbPausedFiles(0),
   nbErrorFiles(0),
   nbDownloadingFiles(0)
{
}

/**
  * Search for a shared entry id among the child files if the current entry doesn't have one.
  */
Common::Hash DownloadsTreeModel::Tree::getSharedEntryId() const
{
   const Tree* current = this;

   forever
   {
      const Common::Hash id = Common::Hash(current->getItem().local_entry().shared_entry().id().hash());
      if (!id.isNull())
         return id;

      for (auto type : QList<Protos::Common::Entry::Type> { Protos::Common::Entry::FILE, Protos::Common::Entry::DIR })
         for (int i = 0; i < current->getNbChildren(); ++i)
            if (current->getChild(i)->getItem().local_entry().type() == type)
            {
               current = current->getChild(i);
               goto next;
            }

      next:;
   }

   return Common::Hash();
}

/////

bool GUI::operator>(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() < d2.local_entry().type();

   // We don't use the 'QString' class because of performance issue of the conversion.
   return Common::StringUtils::strcmpi(d1.local_entry().name(), d2.local_entry().name()) == 1;
}

bool GUI::operator<(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() > d2.local_entry().type();

   // We don't use the 'QString' class because of performance issue of the conversion.
   return Common::StringUtils::strcmpi(d1.local_entry().name(), d2.local_entry().name()) == -1;
}
