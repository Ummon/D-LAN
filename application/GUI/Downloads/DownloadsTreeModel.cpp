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

#include <IconProvider.h>

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

   if (!tree || tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
      return false;

   return tree->getItem().status() == Protos::GUI::State::Download::PAUSED;
}

bool DownloadsTreeModel::isFileLocationKnown(const QModelIndex& index) const
{
   Tree* tree = static_cast<Tree*>(index.internalPointer());

   if (!tree || tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
      return false;

   return tree->getItem().status() == Protos::GUI::State::Download::PAUSED;
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

   QString path;
   if (tree->getItem().local_entry().type() == Protos::Common::Entry::DIR)
   {
      // TODO
      /*Tree* superTree = tree;
      while (superTree != this->root)
      {
         path.prepend(Common::ProtoHelper::getStr(superTree->getItem().local_entry(), &Protos::Common::Entry::name)).prepend('/');
         superTree = superTree->getParent();
      }*/
      return path;
   }
   else
   {
      const Common::SharedDir sharedDir = this->sharedDirsModel.getDir(tree->getItem().local_entry().shared_dir().id().hash());
      if (sharedDir.isNull())
         return QString();

      QString path = sharedDir.path;
      return path.append(Common::ProtoHelper::getRelativePath(tree->getItem().local_entry(), appendFilename));
   }
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

   switch (role)
   {
   case Qt::DisplayRole:
      {
         Tree* tree = static_cast<Tree*>(index.internalPointer());
         const Protos::GUI::State::Download& currentDownload = tree->getItem();
         switch (index.column())
         {
         case 0: return Common::ProtoHelper::getStr(currentDownload.local_entry(), &Protos::Common::Entry::name);
         case 1: return Common::Global::formatByteSize(currentDownload.local_entry().size());
         case 2: return QVariant::fromValue(Progress(currentDownload.local_entry().size() == 0 ? 0 : 10000 * currentDownload.downloaded_bytes() / currentDownload.local_entry().size(), currentDownload.status()));
         default: return QVariant();
         }
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            Tree* tree = static_cast<Tree*>(index.internalPointer());
            return IconProvider::getIcon(tree->getItem().local_entry());
         }
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return static_cast<int>(index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft) | Qt::AlignVCenter;

   default: return QVariant();
   }
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

   if (index.isValid())
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

   Common::TreeBreadthIterator<Protos::GUI::State::Download> i(this->root);
   Tree* currentTree;
   while (currentTree = dynamic_cast<Tree*>(i.next()))
      currentTree->setToDelete();

   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.download(activeDownloadIndices[i]);

      if (download.local_entry().type() == Protos::Common::Entry::DIR)
         continue;

      QStringList path = ProtoHelper::getStr(download.local_entry(), &Protos::Common::Entry::path).split('/', QString::SkipEmptyParts);

      // A node is created for each directory.
      Tree* currentTree = this->root;
      for (QStringListIterator i(path); i.hasNext();)
         currentTree = this->insert(currentTree, i.next());

      this->insert(currentTree, download);
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
         if (currentChildTree->isToDelete())
         {
            this->beginRemoveRows(currentTree == this->root ? QModelIndex() : this->createIndex(currentTree->getOwnPosition(), 0, currentTree), i, i);
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

DownloadsTreeModel::Tree* DownloadsTreeModel::insert(Tree* tree, const QString& dir)
{
   Protos::GUI::State::Download download;
   ProtoHelper::setStr(*download.mutable_local_entry(), &Protos::Common::Entry::set_name, dir);
   download.mutable_local_entry()->set_type(Protos::Common::Entry::DIR);

   return this->insert(tree, download);
}

DownloadsTreeModel::Tree* DownloadsTreeModel::insert(Tree* tree, const Protos::GUI::State::Download& download)
{
   for (int i = 0; i <= tree->getNbChildren(); i++)
   {
      if (i == tree->getNbChildren() || (tree != this->root && download < tree->getChild(i)->getItem())) // The root elements aren't sorted.
      {
         QModelIndex parentIndex = tree == this->root ? QModelIndex() : this->createIndex(tree->getOwnPosition(), 0, tree);
         this->beginInsertRows(parentIndex, i, i);
         Tree* newTree = dynamic_cast<Tree*>(tree->insertChild(download, i));
         this->endInsertRows();

         /*if (newTree->getItem().local_entry().type() == Protos::Common::Entry::FILE)
            this->updateDirectories(newTree);*/

         return newTree;
      }

      if (ProtoHelper::getStr(download.local_entry(), &Protos::Common::Entry::name) == ProtoHelper::getStr(tree->getChild(i)->getItem().local_entry(), &Protos::Common::Entry::name))
      {
         Tree* existingTree = dynamic_cast<Tree*>(tree->getChild(i));
         existingTree->setToDelete(false);
         if (existingTree->getItem() != download)
         {
            existingTree->setItem(download);
            emit dataChanged(this->createIndex(i, 0, existingTree), this->createIndex(i, 4, existingTree));
         }
         return dynamic_cast<Tree*>(tree->getChild(i));
      }
   }
   return 0;
}

/////

DownloadsTreeModel::Tree::Tree() :
   toDelete(false)
{
}

DownloadsTreeModel::Tree::Tree(const Protos::GUI::State::Download& download, Tree* parent) :
   Common::Tree<Protos::GUI::State::Download>(download, parent), toDelete(false)
{
}

DownloadsTreeModel::Tree::~Tree()
{
}

void DownloadsTreeModel::Tree::setToDelete(bool toDelete)
{
   this->toDelete = toDelete;
}

bool DownloadsTreeModel::Tree::isToDelete() const
{
   return this->toDelete;
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
   return Common::ProtoHelper::getStr(d1.local_entry(), &Protos::Common::Entry::name) > Common::ProtoHelper::getStr(d2.local_entry(), &Protos::Common::Entry::name);
}

bool GUI::operator<(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (d1.local_entry().type() != d2.local_entry().type())
      return d1.local_entry().type() > d2.local_entry().type();
   return Common::ProtoHelper::getStr(d1.local_entry(), &Protos::Common::Entry::name) < Common::ProtoHelper::getStr(d2.local_entry(), &Protos::Common::Entry::name);
}

/*bool GUI::operator==(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   return ProtoHelper::getStr(d1.local_entry(), &Protos::Common::Entry::name) == ProtoHelper::getStr(d2.local_entry(), &Protos::Common::Entry::name);
}

bool GUI::operator!=(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   return !(d1 == d2);
}*/
