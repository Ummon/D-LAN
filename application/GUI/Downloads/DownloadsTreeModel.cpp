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

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <QtGlobal>

/**
  * @class DownloadsTreeModel
  *
  * This model shows the download queue as a tree and is designed to be used with a 'QTreeView'. Each node is a directory and the leaves are the queued files.
  * The method 'onNewState(..)' is automatically and periodically called by 'DownloadsModel' each time a new state is sent by the core.
  */

DownloadsTreeModel::DownloadsTreeModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   DownloadsModel(coreConnection, peerListModel, sharedDirsModel, filter), root(new Tree())
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
      return tree->getItem().status() == Protos::GUI::State::Download::PAUSED;

   for (Common::TreeBreadthIterator<Tree> i(tree); i.hasNext();)
   {
      Tree* current = i.next();
      if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE && current->getItem().status() != Protos::GUI::State::Download::PAUSED && current->getItem().status() != Protos::GUI::State::Download::COMPLETE)
         return false;
   }

   return true;
}

bool DownloadsTreeModel::isFileLocationKnown(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   for (Common::TreeBreadthIterator<Tree> i(tree, true); i.hasNext();)
   {
      Tree* current = i.next();
      if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE && current->getItem().local_entry().exists())
         return true;
   }

   return false;
}

bool DownloadsTreeModel::isFileComplete(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   if (tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
   {
      for (Common::TreeBreadthIterator<Tree> i(tree); i.hasNext();)
      {
         Tree* current = i.next();
         if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE && current->getItem().status() != Protos::GUI::State::Download::COMPLETE)
            return false;
      }
      return true;
   }

   return tree->getItem().status() == Protos::GUI::State::Download::COMPLETE;
}

bool DownloadsTreeModel::isSourceAlive(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

   return tree->getItem().peer_id_size() > 0 && !this->peerListModel.getNick(tree->getItem().peer_id(0).hash()).isNull();
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

   const Common::SharedDir sharedDir = this->sharedDirsModel.getDir(tree->getItem().local_entry().shared_dir().id().hash());
   if (sharedDir.isNull())
      return QString();

   QString path;
   if (tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
   {
      Tree* superTree = tree;
      while (superTree != this->root)
      {
         path.prepend(Common::ProtoHelper::getStr(superTree->getItem().local_entry(), &Protos::Common::Entry::name)).prepend('/');
         superTree = superTree->getParent();
      }
      path.prepend(sharedDir.path.left(sharedDir.path.count() - 1));
   }
   else
   {
      path = sharedDir.path.left(sharedDir.path.count() - 1);
      path.append(Common::ProtoHelper::getRelativePath(tree->getItem().local_entry(), appendFilename));
   }
   return path;
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

bool DownloadsTreeModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int where, int /*column*/, const QModelIndex& parent)
{
   if (where == -1 || !data || action != Qt::MoveAction || where > this->root->getNbChildren() || parent.isValid()) // We can only drop on the root.
       return false;

   QList<int> rows = this->getDraggedRows(data);
   if (rows.isEmpty())
      return false;

   qSort(rows); // TODO: is 'getDraggedRows(..)' returns a sorted list?

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

   // Some rows to move may not have be processed by the last loop.
   for (QListIterator<int> i(rows); i.hasNext();)
      downloadIDsToMove << this->getDownloadIDs(this->root->getChild(i.next()));

   this->coreConnection->moveDownloads(downloadRefs, downloadIDsToMove, position);

   return true;
}

void DownloadsTreeModel::onNewState(const Protos::GUI::State& state)
{
   QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   for (Common::TreeBreadthIterator<Tree> i(this->root); i.hasNext();)
      i.next()->visited = false;

   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.download(activeDownloadIndices[i]);

      if (download.local_entry().type() == Protos::Common::Entry::DIR)
         continue;

      Tree* fileTree = this->indexedFiles.value(download.id(), 0);
      if (fileTree) // The file already exists in the tree, we will update its information and all parent directories.
      {
         Tree* parentTree = fileTree;
         do
         {
            parentTree->visited = true;
            if (parentTree->getParent() == this->root)
               this->moveUp(parentTree);
         } while (parentTree = parentTree->getParent());

         this->update(fileTree, download);
      }
      else
      {
         QStringList path = ProtoHelper::getStr(download.local_entry(), &Protos::Common::Entry::path).split('/', QString::SkipEmptyParts);

         // A node is created for each directory.
         Tree* currentTree = this->root;
         for (QStringListIterator i(path); i.hasNext();)
            currentTree = this->insertDirectory(
               currentTree,
               i.next(),
               ProtoHelper::getStr(download, &Protos::GUI::State::Download::peer_source_nick),
               download.peer_id_size() == 0 ? Common::Hash() : Common::Hash(download.peer_id(0).hash()),
               download.local_entry().shared_dir().id().hash()
            );

         this->insert(currentTree, download);
      }
   }

   // Delete unknown items, we can't use the iterator 'TreeBreadthIterator' because the structure is altered during the loop.
   QList<Tree*> trees;
   trees << this->root;
   while (!trees.isEmpty())
   {
      Tree* currentTree = trees.takeLast();
      for (int i = 0; i < currentTree->getNbChildren(); i++)
      {
         Tree* currentChildTree = currentTree->getChild(i);
         if (!currentChildTree->visited)
         {
            this->updateDirectoriesEntryDeleted(currentChildTree, currentChildTree->getItem());

            this->beginRemoveRows(currentTree == this->root ? QModelIndex() : this->createIndex(currentTree->getOwnPosition(), 0, currentTree), i, i);
            for (Common::TreeBreadthIterator<Tree> j(currentChildTree, true); j.hasNext();)
            {
               Tree* treeChild = j.next();
               if (treeChild->getItem().local_entry().type() == Protos::Common::Entry::FILE)
                  this->indexedFiles.remove(treeChild->getItem().id());
            }
            delete currentChildTree;
            this->endRemoveRows();
            i--;
         }
         else if(currentChildTree->getNbChildren() > 0)
         {
            trees << currentChildTree;
         }
      }
   }
}

QList<quint64> DownloadsTreeModel::getDownloadIDs(Tree* tree) const
{
   if (tree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
      return QList<quint64>() << tree->getItem().id();

   // We have to send all sub file ids, a directory doesn't have an id.
   QList<quint64> IDs;
   for (Common::TreeBreadthIterator<Tree> i(tree); i.hasNext();)
   {
      const Tree* current = i.next();
      if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE)
         IDs << current->getItem().id();
   }
   return IDs;
}

DownloadsTreeModel::Tree* DownloadsTreeModel::insertDirectory(Tree* parentTree, const QString& dir, const QString& peerSourceNick, const Common::Hash& peerSourceID, const Common::Hash& sharedDirID)
{
   Protos::GUI::State::Download download;
   ProtoHelper::setStr(*download.mutable_local_entry(), &Protos::Common::Entry::set_name, dir);
   ProtoHelper::setStr(download, &Protos::GUI::State::Download::set_peer_source_nick, peerSourceNick);
   download.add_peer_id()->set_hash(peerSourceID.getData(), Common::Hash::HASH_SIZE);
   download.mutable_local_entry()->mutable_shared_dir()->mutable_id()->set_hash(sharedDirID.getData(), Common::Hash::HASH_SIZE);
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
DownloadsTreeModel::Tree* DownloadsTreeModel::insert(Tree* tree, const Protos::GUI::State::Download& download)
{
   const int nbChildren = tree->getNbChildren();

   // Special case, the children of the root aren't sorted in an alphabetic way.
   if (tree == this->root)
   {
      int i = nbChildren;
      while (i >= 1 && !this->root->getChild(i-1)->visited)
         i--;
      return this->createEntry(QModelIndex(), i, download);
   }

   // We find a place to create the new entry and to keep the children in alphabetic order.
   for (int i = 0; i <= nbChildren; i++)
   {
      if (i == nbChildren || (tree != this->root && download < tree->getChild(i)->getItem())) // The root elements aren't sorted.
      {
         QModelIndex parentIndex = tree == this->root ? QModelIndex() : this->createIndex(tree->getOwnPosition(), 0, tree);
         return this->createEntry(parentIndex, i, download);
      }
   }
   return 0;
}

DownloadsTreeModel::Tree* DownloadsTreeModel::createEntry(const QModelIndex& parent, int position, const Protos::GUI::State::Download& download)
{
   this->beginInsertRows(parent, position, position);
   Tree* parentTree = parent.isValid() ? static_cast<Tree*>(parent.internalPointer()) : this->root;
   Tree* newTree = parentTree->insertChild(download, position);
   if (newTree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
      this->indexedFiles.insert(download.id(), newTree);
   this->endInsertRows();

   if (download.local_entry().type() == Protos::Common::Entry::FILE)
      this->updateDirectoriesNewFile(newTree);

   return newTree;
}

/**
  * Move the given tree right after the last visited tree (Tree::visited = true).
  * The tree must be a direct child of the root.
  * Used when reordering the top items against the download file list.
  * @param tree
  * @return the given tree
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::moveUp(DownloadsTreeModel::Tree* tree)
{
   Q_ASSERT(tree && tree->getParent() == this->root);

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
  * Update the given file. Then recursively update all parent directories.
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::update(Tree* tree, const Protos::GUI::State::Download& download)
{
   Q_ASSERT(tree);

   tree->visited = true;
   if (download.local_entry().type() == Protos::Common::Entry::FILE && tree->getItem() != download)
   {
      const Protos::GUI::State::Download oldDownload = tree->getItem();
      tree->setItem(download);
      this->updateDirectoriesFileModified(tree, oldDownload);
      const int treePosition = tree->getOwnPosition();
      emit dataChanged(this->createIndex(treePosition, 0, tree), this->createIndex(treePosition, 3, tree));
   }
   return tree;
}

DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesEntryDeleted(Tree* entry, const Protos::GUI::State::Download& oldDownload)
{
   const quint64 size = -entry->getItem().local_entry().size();
   const quint64 downloadedBytes = -entry->getItem().downloaded_bytes();

   return this->updateDirectories(entry, size, downloadedBytes, oldDownload.status());
}

DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesNewFile(Tree* file)
{
   const quint64 fileSize = file->getItem().local_entry().size();
   const quint64 fileDownloadedBytes = file->getItem().downloaded_bytes();

   return this->updateDirectories(file, fileSize, fileDownloadedBytes);
}

DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectoriesFileModified(Tree* file, const Protos::GUI::State::Download& oldDownload)
{
   const quint64 fileSizeDelta = file->getItem().local_entry().size() - oldDownload.local_entry().size();
   const quint64 fileDownloadedBytesDelta = file->getItem().downloaded_bytes() - oldDownload.downloaded_bytes();

   return this->updateDirectories(file, fileSizeDelta, fileDownloadedBytesDelta, oldDownload.status());
}

/**
  * Update all parent directories of the given file.
  * The following data are updated:
  *  - Size
  *  - Bytes downloaded
  *  - Status
  */
DownloadsTreeModel::Tree* DownloadsTreeModel::updateDirectories(Tree* file, quint64 fileSizeDelta, quint64 fileDownloadedBytesDelta, Protos::GUI::State::Download::Status oldStatus)
{
   Protos::GUI::State::Download::Status newStatus = file->getItem().status();

   if (fileSizeDelta == 0 && fileDownloadedBytesDelta == 0 && newStatus == oldStatus)
      return file;

   Tree* currentDirectory = file->getParent();
   while (currentDirectory != this->root)
   {
      currentDirectory->getItem().mutable_local_entry()->set_size(currentDirectory->getItem().local_entry().size() + fileSizeDelta);
      currentDirectory->getItem().set_downloaded_bytes(currentDirectory->getItem().downloaded_bytes() + fileDownloadedBytesDelta);

      currentDirectory->nbErrorFiles += oldStatus >= Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE && newStatus < Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE ? -1 : (oldStatus < Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE && newStatus >= Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE ? 1 : 0);
      currentDirectory->nbPausedFiles += oldStatus == Protos::GUI::State::Download::PAUSED && newStatus != Protos::GUI::State::Download::PAUSED ? -1 : (oldStatus != Protos::GUI::State::Download::PAUSED && newStatus == Protos::GUI::State::Download::PAUSED ? 1 : 0);
      currentDirectory->nbDownloadingFiles += oldStatus == Protos::GUI::State::Download::DOWNLOADING && newStatus != Protos::GUI::State::Download::DOWNLOADING ? -1 : (oldStatus != Protos::GUI::State::Download::DOWNLOADING && newStatus == Protos::GUI::State::Download::DOWNLOADING ? 1 : 0);

      if (currentDirectory->getItem().local_entry().size() == currentDirectory->getItem().downloaded_bytes())
         currentDirectory->getItem().set_status(Protos::GUI::State::Download::COMPLETE);
      else if (currentDirectory->nbErrorFiles == 1)
         currentDirectory->getItem().set_status(newStatus);
      else if (currentDirectory->nbErrorFiles == 0 && currentDirectory->nbPausedFiles > 0)
         currentDirectory->getItem().set_status(Protos::GUI::State::Download::PAUSED);
      else if (currentDirectory->nbErrorFiles == 0 && currentDirectory->nbDownloadingFiles > 0)
         currentDirectory->getItem().set_status(Protos::GUI::State::Download::DOWNLOADING);
      else if (currentDirectory->nbErrorFiles == 0)
         currentDirectory->getItem().set_status(Protos::GUI::State::Download::QUEUED);

      const int currentDirectoryPosition = currentDirectory->getOwnPosition();
      emit dataChanged(this->createIndex(currentDirectoryPosition, 0, currentDirectory), this->createIndex(currentDirectoryPosition, 3, currentDirectory));

      currentDirectory = currentDirectory->getParent();
   }

   return file;
}

/////

DownloadsTreeModel::Tree::Tree() :
   visited(true), nbPausedFiles(0), nbErrorFiles(0), nbDownloadingFiles(0)
{
   this->getItem().set_status(Protos::GUI::State::Download::QUEUED);
}

DownloadsTreeModel::Tree::Tree(const Protos::GUI::State::Download& download, Tree* parent) :
   Common::Tree<Protos::GUI::State::Download, Tree>(download, parent), visited(true), nbPausedFiles(0), nbErrorFiles(0), nbDownloadingFiles(0)
{
}

/////

bool GUI::operator>(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() < d2.local_entry().type();

   // We don't use the 'QString' class because of performance issue of the conversion.
   return Common::Global::strcmpi(d1.local_entry().name(), d2.local_entry().name()) == 1;
}

bool GUI::operator<(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() > d2.local_entry().type();

   // We don't use the 'QString' class because of performance issue of the conversion.
   return Common::Global::strcmpi(d1.local_entry().name(), d2.local_entry().name()) == -1;
}
