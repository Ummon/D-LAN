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
  
#include <Search/SearchModel.h>
using namespace GUI;

#include <algorithm>
#include <functional>
#include <string>

#include <QtAlgorithms>

#include <Common/Settings.h>
#include <Common/Global.h>

const int SearchModel::NB_SIGNAL_PROGRESS(50);

SearchColumn SearchModel::toSearchColumn(int number)
{
   return static_cast<SearchColumn>(number);
}

int SearchModel::toColumnNumber(SearchColumn column)
{
   return static_cast<int>(column);
}

/**
  * @class GUI::SearchModel
  *
  * The result of a search. A search can only be performed once and for a limited period of time, see the setting "search_time".
  * The directories from the result can be browsed, thus this model inherits from the 'BrowseModel'.
  */

SearchModel::SearchModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel) :
   BrowseModel(coreConnection, sharedDirsModel, Common::Hash()), peerListModel(peerListModel), maxLevel(0), nbFolders(0), nbFiles(0), currentProgress(0)
{
   delete this->root;
   this->root = new SearchTree();

   this->timerProgress.setInterval(SETTINGS.get<quint32>("search_time") / NB_SIGNAL_PROGRESS);
   connect(&this->timerProgress, SIGNAL(timeout()), this, SLOT(sendNextProgress()));

   this->timerTimeout.setInterval(SETTINGS.get<quint32>("search_time"));
   this->timerTimeout.setSingleShot(true);
   connect(&this->timerTimeout, SIGNAL(timeout()), this, SLOT(stopSearching()));
}

SearchModel::~SearchModel()
{
   if (!this->searchResult.isNull())
      this->searchResult->disconnect(this);
}

Common::Hash SearchModel::getPeerID(const QModelIndex& index) const
{
   SearchTree* tree = static_cast<SearchTree*>(index.internalPointer());
   return tree->getPeerID();
}

void SearchModel::search(const Protos::Common::FindPattern& findPattern, bool local)
{
   // Unable to perform more than one search.
   if (!this->searchResult.isNull())
      return;

   this->searchResult = this->coreConnection->search(findPattern, local);
   connect(this->searchResult.data(), SIGNAL(result(const Protos::Common::FindResult&)), this, SLOT(result(const Protos::Common::FindResult&)));
   // We don't use the 'timout' signal from 'ISearchResult', not useful.
   this->searchResult->start();

   this->timerProgress.start();
   this->timerTimeout.start();
}

QVariant SearchModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid())
      return QVariant();

   if (index.column() == 2) // Match rate in percent. Cannot be 0%.
   {
      switch (role)
      {
      case Qt::DisplayRole:
         {
            SearchTree* tree = static_cast<SearchTree*>(index.internalPointer());

            if (tree->getParent()->getParent() != 0 && tree->getParent()->getItem().type() == Protos::Common::Entry_Type_DIR)
               return QVariant();

            int percentMatch = 100 - 100 * tree->getLevel() / (this->maxLevel + 1);

            return percentMatch > 100 ? 100 : percentMatch;
         }

      default:
         return QVariant();
      }
   }
   else
   {
      return BrowseModel::data(index, role);
   }
}

QVariant SearchModel::headerData(int section, Qt::Orientation orientation, int role) const
{
   if (orientation == Qt::Vertical)
      return QAbstractItemModel::headerData(section, orientation, role);

   switch (role)
   {
   case Qt::DisplayRole:
      switch (section)
      {
      case 0: return tr("Name");
      case 1: return tr("Directory");
      case 2: return tr("Relevance");
      case 3: return tr("Peer");
      case 4: return tr("Size");
      default: return QAbstractItemModel::headerData(section, orientation, role);
      }

   case Qt::TextAlignmentRole:
      switch (section)
      {
      case 4: return Qt::AlignRight;
      default: return QAbstractItemModel::headerData(section, orientation, role);
      }
   }

   return QAbstractItemModel::headerData(section, orientation, role);
}

int SearchModel::columnCount(const QModelIndex& parent) const
{
   return 5;
}
int SearchModel::getNbFolders() const
{
   return this->nbFolders;
}

int SearchModel::getNbFiles() const
{
   return this->nbFiles;
}

/**
  * Return true for files with multiple similar files.
  */
bool SearchModel::isNonTerminalFile(const QModelIndex& index)
{
   SearchTree* tree = static_cast<SearchTree*>(index.internalPointer());
   return tree->getItem().type() == Protos::Common::Entry::FILE && tree->getNbChildren() > 0;
}

void SearchModel::loadChildren(const QPersistentModelIndex &index)
{
   this->peerID = static_cast<const SearchTree*>(index.internalPointer())->getPeerID();
   BrowseModel::loadChildren(index);
}

bool entryLessThan(const Protos::Common::Entry& e1, int level1, const QString& peerNick1, const Protos::Common::Entry& e2, int level2, const QString& peerNick2, SearchColumn column, Qt::SortOrder order)
{
   const QString& path1 = Common::ProtoHelper::getRelativePath(e1, Common::EntriesToAppend::NONE, !Common::ProtoHelper::isRoot(e1)).toLower();
   const QString& path2 = Common::ProtoHelper::getRelativePath(e2, Common::EntriesToAppend::NONE, !Common::ProtoHelper::isRoot(e2)).toLower();

   // It's not necessary to transform them to 'QString'.
   std::string name1 = e1.name();
   std::transform(name1.begin(), name1.end(), name1.begin(), tolower);
   std::string name2 = e2.name();
   std::transform(name2.begin(), name2.end(), name2.begin(), tolower);

   switch (column)
   {
      case SearchColumn::NAME:
         if (name1 != name2)
            return order == Qt::AscendingOrder ? name1 < name2 : name1 > name2;
         break;

      case SearchColumn::DIRECTORY:
         if (path1 != path2)
            return order == Qt::AscendingOrder ? path1 < path2 : path1 > path2;
         break;

      case SearchColumn::RELEVANCE:
         if (level1 != level2)
            return order == Qt::AscendingOrder ? level1 < level2 : level1 > level2;
         break;

      case SearchColumn::PEER:
         if (peerNick1 != peerNick2)
            return order == Qt::AscendingOrder ? peerNick1 < peerNick2 : peerNick1 > peerNick2;
         break;

      case SearchColumn::SIZE:
         if (e1.size() != e2.size())
            return order == Qt::AscendingOrder ? e1.size() < e2.size() : e1.size() > e2.size();
   }

   if (column != SearchColumn::RELEVANCE && level1 != level2)
      return order == Qt::AscendingOrder ? level1 < level2 : level1 > level2;

   if (column != SearchColumn::DIRECTORY && path1 != path2)
      return order == Qt::AscendingOrder ? path1 < path2 : path1 > path2;

   if (column != SearchColumn::NAME && name1 != name2)
      return order == Qt::AscendingOrder ? name1 < name2 : name1 > name2;

   if (column != SearchColumn::PEER && peerNick1 != peerNick2)
      return order == Qt::AscendingOrder ? peerNick1 < peerNick2 : peerNick1 > peerNick2;

   return order == Qt::AscendingOrder ? e1.size() < e2.size() : e1.size() > e2.size();
}

/**
  * Sort the current data and save column and order if more date
  */
void SearchModel::sort(int column, Qt::SortOrder order)
{
   this->currentSortedColumn = column == -1 ? SearchColumn::RELEVANCE : toSearchColumn(column);
   this->currentSortOrder = order;

   emit layoutAboutToBeChanged(QList<QPersistentModelIndex> { QPersistentModelIndex(QModelIndex()) }, QAbstractItemModel::VerticalSortHint);

   QHash<Tree*, QModelIndex> unsortedChildren;
   for (int i = 0; i < this->root->getNbChildren(); i++)
      unsortedChildren.insert(this->root->getChild(i), this->index(i, 0));

   this->root->sort([&](const Tree* t1, const Tree* t2)
   {
      return entryLessThan(
         t1->getItem(),
         dynamic_cast<const SearchTree*>(t1)->getLevel(),
         dynamic_cast<const SearchTree*>(t1)->getPeerNick(),
         t2->getItem(),
         dynamic_cast<const SearchTree*>(t2)->getLevel(),
         dynamic_cast<const SearchTree*>(t2)->getPeerNick(),
         this->currentSortedColumn,
         this->currentSortOrder
      );
   });

   // As the documentation says we have to tell the views which index goes who... it's a bit complicated and CPU consuming, is there a simplest way?
   for (int i = 0; i < this->root->getNbChildren(); i++)
      this->changePersistentIndex(unsortedChildren.value(this->root->getChild(i)), this->index(i, 0));

   emit layoutChanged(QList<QPersistentModelIndex> { QPersistentModelIndex(QModelIndex()) }, QAbstractItemModel::VerticalSortHint);
}

/**
  * This method is called several times, one per received entries. The entries are inserted into the model.
  * The given entries are sorted by their level, we will keep the sort when inserting the entry but with some modifications:
  *  - All entries with the same level are sorted first by their path (prefixed with the shared directory name) and then by their name.
  *  - All file entries with the same chunks (identical data) are grouped. They can be owned by different peer.
  */
void SearchModel::result(const Protos::Common::FindResult& findResult)
{
   if (findResult.entry_size() == 0)
      return;

   QList<const Protos::Common::FindResult_EntryLevel*> sortedEntries;
   for (int i = 0; i < findResult.entry_size(); i++)
      sortedEntries << &findResult.entry(i);

   std::sort(
      sortedEntries.begin(),
      sortedEntries.end(),
      [&](const Protos::Common::FindResult_EntryLevel* e1, const Protos::Common::FindResult_EntryLevel* e2)
      {
         // The peer nick isn't necessary because the results are from the same peer.
         return entryLessThan(e1->entry(), e1->level(), QString(), e2->entry(), e2->level(), QString(), this->currentSortedColumn, this->currentSortOrder);
      }
   );

   int currentIndex = 0;
   bool maxLevelChange = false;

   for (QListIterator<const Protos::Common::FindResult_EntryLevel*> i(sortedEntries); i.hasNext();)
   {
      const Protos::Common::FindResult_EntryLevel* entry = i.next();
      if (this->setMaxLevel(entry->level()))
         maxLevelChange = true;

      // Search if a similar entry already exists. If so then insert the new tree as child.
      if (entry->entry().type() == Protos::Common::Entry_Type_FILE && entry->entry().chunk_size() > 0)
      {
         Common::Hash firstChunk = entry->entry().chunk(0).hash();
         SearchTree* similarTree = nullptr;
         if ((similarTree = this->indexedFile.value(firstChunk)) && similarTree->isSameAs(entry->entry()))
         {
            if (similarTree->getNbChildren() == 0)
            {
               this->beginInsertRows(this->createIndex(0, 0, similarTree), 0, 0);
               similarTree->insertChild(similarTree);
               this->endInsertRows();
            }

            // Search the better name (tree with the lowest level) to display it on the top.
            for (int i = 0; i <= similarTree->getNbChildren(); i++)
            {
               if (i == similarTree->getNbChildren() || static_cast<SearchTree*>(similarTree->getChild(i))->getLevel() > static_cast<int>(entry->level()))
               {
                  this->beginInsertRows(this->createIndex(0, 0, similarTree), i, i);
                  Common::Hash peerID = findResult.peer_id().hash();
                  SearchTree* newTree = similarTree->insertChild(i, *entry, peerID, this->peerListModel.getNick(peerID, tr("<unknown>")));
                  this->endInsertRows();

                  if (static_cast<int>(entry->level()) < similarTree->getLevel())
                  {
                     const int row = similarTree->getOwnPosition();
                     similarTree->copyFrom(newTree);
                     emit dataChanged(this->createIndex(row, 0, similarTree), this->createIndex(row, 3, similarTree));
                  }

                  break;
               }
            }

            continue;
         }
      }

      currentIndex = this->insertTree(*entry, findResult.peer_id().hash(), currentIndex);
   }

   if (maxLevelChange && this->rowCount() > 0)
      emit dataChanged(this->createIndex(0, 2), this->createIndex(this->rowCount() - 1, 2));
}

void SearchModel::sendNextProgress()
{
   this->currentProgress += 100 / NB_SIGNAL_PROGRESS;
   emit progress(this->currentProgress > 100 ? 100 : this->currentProgress);
}

void SearchModel::stopSearching()
{
   this->searchResult.clear();
   this->timerProgress.stop();
   emit progress(100);
}

SearchModel::SearchTree* SearchModel::getRoot()
{
   return static_cast<SearchTree*>(this->root);
}

/**
  * Create a new tree, it can be a directory or a file. It will be inserted in the structure depending its level and its path+name.
  * Return the index of the first entry of the same level.
  */
int SearchModel::insertTree(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, int currentIndex)
{
   if (entry.entry().type() == Protos::Common::Entry_Type_FILE)
      this->nbFiles++;
   else
      this->nbFolders++;

   const QString peerNick = this->peerListModel.getNick(peerID, tr("<unknown>"));

   SearchTree* root = this->getRoot();

   // Search a place to insert the new entry, order (level > path > name) must be kept.
   while (currentIndex < root->getNbChildren() &&
          entryLessThan(
             root->getChild(currentIndex)->getItem(),
             static_cast<SearchTree*>(root->getChild(currentIndex))->getLevel(),
             static_cast<SearchTree*>(root->getChild(currentIndex))->getPeerNick(),
             entry.entry(),
             static_cast<int>(entry.level()),
             peerNick,
             this->currentSortedColumn,
             this->currentSortOrder)
          )
      currentIndex++;

   this->beginInsertRows(QModelIndex(), currentIndex, currentIndex);
   SearchTree* newTree = root->insertChild(currentIndex++, entry, peerID, peerNick);
   this->endInsertRows();

   if (newTree->getItem().type() == Protos::Common::Entry_Type_FILE && newTree->getItem().chunk_size() > 0)
      this->indexedFile.insert(newTree->getItem().chunk(0).hash(), newTree);

   return currentIndex;
}

/**
  * The relevance is a percentage of the current level against the max level, thus if the max level change
  * we have to recompute all percentage values.
  */
bool SearchModel::setMaxLevel(int newLevel)
{
   if (newLevel > this->maxLevel)
   {
      this->maxLevel = newLevel;
      return true;
   }
   return false;
}

/////

/**
  * Will append the shared directory name to the relative path.
  * Obsolete, 'Common::ProtoHelper::getRelativePath(..)' is used instead.
  */
/*QString SearchModel::SearchTree::entryPath(const Protos::Common::Entry& entry)
{
   const QString& path = Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path);

   QString completePath;
   if (path.isEmpty())
      completePath.append("/");
   else
   {
      const QString& sharedName = Common::ProtoHelper::getStr(entry.shared_dir(), &Protos::Common::SharedDir::shared_name);
      if (!Common::Global::isWindowsPath(sharedName))
         completePath.append("/");
      completePath.append(sharedName).append(path);
   }

   return completePath;
}*/

SearchModel::SearchTree::SearchTree() :
   level(0)
{
}

SearchModel::SearchTree::SearchTree(const Protos::Common::Entry& entry, int level, const Common::Hash& peerID, const QString& peerNick, SearchTree* parent) :
   Tree(entry, parent), level(level), peerID(peerID), peerNick(peerNick)
{
}

SearchModel::SearchTree::SearchTree(const Protos::Common::Entry& entry, const Common::Hash& peerID, SearchTree* parent) :
   Tree(entry, parent), level(0), peerID(peerID)
{
}

SearchModel::SearchTree* SearchModel::SearchTree::insertChild(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchTree* searchTree = new SearchTree(entry.entry(), entry.level(), peerID, peerNick, this);
   this->children << searchTree;
   return searchTree;
}

SearchModel::SearchTree* SearchModel::SearchTree::insertChild(int index, const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchTree* searchTree = new SearchTree(entry.entry(), entry.level(), peerID, peerNick, this);
   this->children.insert(index, searchTree);
   return searchTree;
}

SearchModel::SearchTree* SearchModel::SearchTree::insertChild(SearchModel::SearchTree* tree)
{
   SearchTree* searchTree = new SearchTree(tree->getItem(), tree->getLevel(), tree->getPeerID(), tree->peerNick, this);
   this->children << searchTree;
   return searchTree;
}

int SearchModel::SearchTree::getLevel() const
{
   return this->level;
}

Common::Hash SearchModel::SearchTree::getPeerID() const
{
   return this->peerID;
}

const QString& SearchModel::SearchTree::getPeerNick() const
{
   return this->peerNick;
}

QVariant SearchModel::SearchTree::data(int column) const
{
   switch (column)
   {
   case 0:
      return Common::ProtoHelper::getStr(this->getItem(), &Protos::Common::Entry::name);

   case 1:
      if (this->parent->getParent() != nullptr && this->parent->getItem().type() == Protos::Common::Entry_Type_DIR || this->getItem().type() == Protos::Common::Entry_Type_FILE && this->getNbChildren() > 0)
         return QVariant();

      return Common::ProtoHelper::getRelativePath(this->getItem(), Common::EntriesToAppend::NONE, !Common::ProtoHelper::isRoot(this->getItem()));

   // case 2: // See 'SearchModel::data(..)'.

   case 3:
      if (this->getItem().type() == Protos::Common::Entry_Type_FILE && this->getNbChildren() > 0)
      {
         for (int i = 0; i < this->getNbChildren(); i++)
            if (this->peerNick != static_cast<const SearchTree*>(this->getChild(i))->peerNick)
               return QVariant();
      }
      return this->peerNick;

   case 4:
      return Common::Global::formatByteSize(this->getItem().size());

   default:
      return QVariant();
   }
}

SearchModel::SearchTree* SearchModel::SearchTree::newTree(const Protos::Common::Entry& entry)
{
   return new SearchTree(entry, this->peerID, this);
}

void SearchModel::SearchTree::copyFrom(const SearchModel::SearchTree* otherTree)
{
   this->getItem().CopyFrom(otherTree->getItem());
   this->level = otherTree->getLevel();
   this->peerID = otherTree->getPeerID();
   this->peerNick = otherTree->peerNick;
}

bool SearchModel::SearchTree::isSameAs(const Protos::Common::Entry& otherEntry) const
{
   // Special case with empty files.
   if (this->getItem().size() == 0 && otherEntry.size() == 0)
      return true;

   if (otherEntry.chunk_size() == 0 || otherEntry.chunk_size() != this->getItem().chunk_size())
      return false;

   for (int i = 0; i < otherEntry.chunk_size(); i++)
   {
      Common::Hash otherHash{otherEntry.chunk(i).hash()};
      if (otherHash.isNull() || otherHash != Common::Hash(this->getItem().chunk(i).hash()))
         return false;
   }

   return true;
}



