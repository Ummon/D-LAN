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

DownloadsTreeModel::DownloadsTreeModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   DownloadsModel(coreConnection, peerListModel, sharedDirsModel, filter), root(new Tree())
{
}

DownloadsTreeModel::~DownloadsTreeModel()
{
   delete this->root;
}

QList<quint64> DownloadsTreeModel::getDownloadIDs(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return QList<quint64>();

   if (tree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
      return QList<quint64>() << tree->getItem().id();

   // We have to send all sub file ids, a directory doesn't have an id.
   QList<quint64> IDs;
   for (TreeBreadthIterator<Protos::GUI::State::Download> i(tree); i.hasNext();)
   {
      Tree* current = dynamic_cast<Tree*>(i.next());
      if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE)
         IDs << current->getItem().id();
   }
   return IDs;
}

bool DownloadsTreeModel::isDownloadPaused(const QModelIndex& index) const
{   
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return true;

   if (tree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
      return tree->getItem().status() == Protos::GUI::State::Download::PAUSED;

   for (TreeBreadthIterator<Protos::GUI::State::Download> i(tree); i.hasNext();)
   {
      Tree* current = dynamic_cast<Tree*>(i.next());
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

   for (TreeBreadthIterator<Protos::GUI::State::Download> i(tree, true); i.hasNext();)
   {
      Tree* current = dynamic_cast<Tree*>(i.next());
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
      for (TreeBreadthIterator<Protos::GUI::State::Download> i(tree); i.hasNext();)
      {
         Tree* current = dynamic_cast<Tree*>(i.next());
         if (current->getItem().local_entry().type() == Protos::Common::Entry::FILE && current->getItem().status() != Protos::GUI::State_Download_Status_COMPLETE)
            return false;
      }
      return true;
   }

   return tree->getItem().status() == Protos::GUI::State_Download_Status_COMPLETE;
}

QString DownloadsTreeModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());
   if (!tree)
      return false;

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
         superTree = dynamic_cast<Tree*>(superTree->getParent());
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

int DownloadsTreeModel::columnCount(const QModelIndex& parent) const
{
   return 4;
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

   Tree* childTree = dynamic_cast<Tree*>(parentTree->getChild(row));

   if (childTree)
       return this->createIndex(row, column, childTree);

   return QModelIndex();
}

QModelIndex DownloadsTreeModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
       return QModelIndex();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   Tree* parentItem = dynamic_cast<Tree*>(tree->getParent());

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

bool DownloadsTreeModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex& parent)
{
   // TODO
   return false;
}

void DownloadsTreeModel::onNewState(const Protos::GUI::State& state)
{
   QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   for (Common::TreeBreadthIterator<Protos::GUI::State::Download> i(this->root); i.hasNext();)
      dynamic_cast<Tree*>(i.next())->toDelete = true;

   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.download(activeDownloadIndices[i]);

      if (download.local_entry().type() == Protos::Common::Entry::DIR)
         continue;

      Tree* fileTree = this->indexedFiles.value(download.id(), 0);
      if (fileTree)
      {
         Tree* parentTree = fileTree;
         while (parentTree = dynamic_cast<Tree*>(parentTree->getParent()))
            parentTree->toDelete = false;

         this->update(fileTree, download);
      }
      else
      {
         QStringList path = ProtoHelper::getStr(download.local_entry(), &Protos::Common::Entry::path).split('/', QString::SkipEmptyParts);

         // A node is created for each directory.
         Tree* currentTree = this->root;
         for (QStringListIterator i(path); i.hasNext();)
            currentTree = this->insertDirectory(currentTree, i.next(), ProtoHelper::getStr(download, &Protos::GUI::State::Download::peer_source_nick), download.local_entry().shared_dir().id().hash());

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
         Tree* currentChildTree = dynamic_cast<Tree*>(currentTree->getChild(i));
         if (currentChildTree->toDelete)
         {
            this->updateDirectoriesEntryDeleted(currentChildTree, currentChildTree->getItem());

            this->beginRemoveRows(currentTree == this->root ? QModelIndex() : this->createIndex(currentTree->getOwnPosition(), 0, currentTree), i, i);
            for (Common::TreeBreadthIterator<Protos::GUI::State::Download> j(currentTree, true); j.hasNext();)
            {
               Tree* treeChild = dynamic_cast<Tree*>(j.next());
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

DownloadsTreeModel::Tree* DownloadsTreeModel::insertDirectory(Tree* tree, const QString& dir, const QString& peerSourceNick, const Common::Hash& sharedDirID)
{
   Protos::GUI::State::Download download;
   ProtoHelper::setStr(*download.mutable_local_entry(), &Protos::Common::Entry::set_name, dir);
   ProtoHelper::setStr(download, &Protos::GUI::State::Download::set_peer_source_nick, peerSourceNick);
   download.mutable_local_entry()->mutable_shared_dir()->mutable_id()->set_hash(sharedDirID.getData(), Common::Hash::HASH_SIZE);
   download.mutable_local_entry()->set_type(Protos::Common::Entry::DIR);

   return this->insert(tree, download);
}

DownloadsTreeModel::Tree* DownloadsTreeModel::insert(Tree* tree, const Protos::GUI::State::Download& download)
{
   const int nbChildren = tree->getNbChildren();

   for (int i = 0; i < nbChildren; i++)
   {
      if (download.local_entry().type() == Protos::Common::Entry::FILE && download.id() == tree->getChild(i)->getItem().id() ||
          download.local_entry().name() == tree->getChild(i)->getItem().local_entry().name())
      {
         return this->update(dynamic_cast<Tree*>(tree->getChild(i)), download);
      }
   }

   // TODO: A dichotomic search may be a good idea here instead of a simple loop.
   for (int i = 0; i <= nbChildren; i++)
   {
      if (i ==  nbChildren || (tree != this->root && download < tree->getChild(i)->getItem())) // The root elements aren't sorted.
      {
         QModelIndex parentIndex = tree == this->root ? QModelIndex() : this->createIndex(tree->getOwnPosition(), 0, tree);

         this->beginInsertRows(parentIndex, i, i);
         Tree* newTree = dynamic_cast<Tree*>(tree->insertChild(download, i));
         if (newTree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
            this->indexedFiles.insert(download.id(), newTree);
         this->endInsertRows();

         if (download.local_entry().type() == Protos::Common::Entry::FILE)
            this->updateDirectoriesNewFile(newTree);

         return newTree;
      }
   }
   return 0;
}

DownloadsTreeModel::Tree* DownloadsTreeModel::update(Tree* tree, const Protos::GUI::State::Download& download)
{
   tree->toDelete = false;
   if (download.local_entry().type() == Protos::Common::Entry::FILE && tree->getItem() != download)
   {
      const Protos::GUI::State::Download oldDownload = tree->getItem();
      tree->setItem(download);
      this->updateDirectoriesFileModified(tree, oldDownload);
      const int treePosition = tree->getOwnPosition();
      emit dataChanged(this->createIndex(treePosition, 1, tree), this->createIndex(treePosition, 3, tree));
   }
   return tree;
}

DownloadsTreeModel::Tree*  DownloadsTreeModel::updateDirectoriesEntryDeleted(Tree* entry, const Protos::GUI::State::Download& oldDownload)
{
   const quint64 size = -entry->getItem().local_entry().size();
   const quint64 downloadedBytes = -entry->getItem().downloaded_bytes();

   return this->updateDirectories(entry, size, downloadedBytes, oldDownload.status());
}

DownloadsTreeModel::Tree*  DownloadsTreeModel::updateDirectoriesNewFile(Tree* file)
{
   const quint64 fileSize = file->getItem().local_entry().size();
   const quint64 fileDownloadedBytes = file->getItem().downloaded_bytes();

   return this->updateDirectories(file, fileSize, fileDownloadedBytes);
}

DownloadsTreeModel::Tree*  DownloadsTreeModel::updateDirectoriesFileModified(Tree* file, const Protos::GUI::State::Download& oldDownload)
{
   const quint64 fileSizeDelta = file->getItem().local_entry().size() - oldDownload.local_entry().size();
   const quint64 fileDownloadedBytesDelta = file->getItem().downloaded_bytes() - oldDownload.downloaded_bytes();

   return this->updateDirectories(file, fileSizeDelta, fileDownloadedBytesDelta, oldDownload.status());
}

DownloadsTreeModel::Tree*  DownloadsTreeModel::updateDirectories(Tree* file, quint64 fileSizeDelta, quint64 fileDownloadedBytesDelta, Protos::GUI::State::Download::Status oldStatus)
{
   Protos::GUI::State::Download::Status newStatus = file->getItem().status();

   if (fileSizeDelta == 0 && fileDownloadedBytesDelta == 0 && newStatus == oldStatus)
      return file;

   Tree* currentDirectory = dynamic_cast<Tree*>(file->getParent());
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
      emit dataChanged(this->createIndex(currentDirectoryPosition, 1, currentDirectory), this->createIndex(currentDirectoryPosition, 3, currentDirectory));

      currentDirectory = dynamic_cast<Tree*>(currentDirectory->getParent());
   }

   return file;
}

/////

DownloadsTreeModel::Tree::Tree() :
   toDelete(false), nbPausedFiles(0), nbErrorFiles(0), nbDownloadingFiles(0)
{
   this->getItem().set_status(Protos::GUI::State::Download::QUEUED);
}

DownloadsTreeModel::Tree::Tree(const Protos::GUI::State::Download& download, Tree* parent) :
   Common::Tree<Protos::GUI::State::Download>(download, parent), toDelete(false), nbPausedFiles(0), nbErrorFiles(0), nbDownloadingFiles(0)
{
}

Common::Tree<Protos::GUI::State::Download>* DownloadsTreeModel::Tree::newTree(const Protos::GUI::State::Download& download)
{
   return new Tree(download, this);
}

/////

bool GUI::operator>(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() < d2.local_entry().type();
   return d1.local_entry().name() > d2.local_entry().name();
}

bool GUI::operator<(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() > d2.local_entry().type();
   return d1.local_entry().name() < d2.local_entry().name();
}
