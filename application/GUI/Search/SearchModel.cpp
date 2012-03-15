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
#include <string>

#include <QtAlgorithms>

#include <Common/Settings.h>
#include <Common/Global.h>

const int SearchModel::NB_SIGNAL_PROGRESS(50);

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
   this->root = new SearchNode();

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
   SearchNode* node = static_cast<SearchNode*>(index.internalPointer());
   return node->getPeerID();
}

void SearchModel::search(const QString& terms)
{
   // Unable to perform more than one search.
   if (!this->searchResult.isNull())
      return;

   this->searchResult = this->coreConnection->search(terms);
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
            SearchNode* node = static_cast<SearchNode*>(index.internalPointer());

            if (node->getParent()->getParent() != 0 && node->getParent()->getEntry().type() == Protos::Common::Entry_Type_DIR)
               return QVariant();

            int percentMatch = 100 - 100 * node->getLevel() / (this->maxLevel + 1);

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
      case 0: return tr("Filename");
      case 1: return tr("Folder");
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

void SearchModel::loadChildren(const QPersistentModelIndex &index)
{
   this->peerID = static_cast<const SearchNode*>(index.internalPointer())->getPeerID();
   BrowseModel::loadChildren(index);
}

bool entryLessThan(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2)
{
   std::string s1;
   std::string s2;

   if (e1.has_shared_dir() && e2.has_shared_dir())
   {
      s1 = e1.shared_dir().shared_name();
      s2 = e2.shared_dir().shared_name();
      std::transform(s1.begin(), s1.end(), s1.begin(), tolower);
      std::transform(s2.begin(), s2.end(), s2.begin(), tolower);
   }

   if (s1 == s2)
   {
      std::string p1 = e1.path();
      std::string p2 = e2.path();
      std::transform(p1.begin(), p1.end(), p1.begin(), tolower);
      std::transform(p2.begin(), p2.end(), p2.begin(), tolower);

      if (p1 == p2)
      {
         std::string n1 = e1.name();
         std::string n2 = e2.name();
         std::transform(n1.begin(), n1.end(), n1.begin(), tolower);
         std::transform(n2.begin(), n2.end(), n2.begin(), tolower);
         return n1 < n2;
      }
      else
         return p1 < p2;
   }
   else
      return s1 < s2;
}

bool findEntryLessThan(const Protos::Common::FindResult_EntryLevel* e1, const Protos::Common::FindResult_EntryLevel* e2)
{
   if (e1->entry().type() == e2->entry().type())
   {
      if (e1->level() == e2->level())
      {
         return entryLessThan(e1->entry(), e2->entry());
      }
      else
         return e1->level() < e2->level();
   }
   else
      return e1->entry().type() >= e2->entry().type();
}

/**
  * This method is called several times, one by received entries. The entries are inserted into the model.
  * The given entries are sorted by their level, we will keep the sort when inserting the entry but with some modifications :
  * - The directories are put first.
  * - All entries with the same level are sorted first by their path (prefixed with the shared directory name) and then by their name.
  * - All file entries with the same chunks (identical data) are grouped. They can be owned by different peer.
  */
void SearchModel::result(const Protos::Common::FindResult& findResult)
{
   if (findResult.entry_size() == 0)
      return;

   QList<const Protos::Common::FindResult_EntryLevel*> sortedEntries;
   for (int i = 0; i < findResult.entry_size(); i++)
      sortedEntries << &findResult.entry(i);
   qSort(sortedEntries.begin(), sortedEntries.end(), &findEntryLessThan);

   int currentIndex = 0;

   bool maxLevelChange = false;

   for (QListIterator<const Protos::Common::FindResult_EntryLevel*> i(sortedEntries); i.hasNext();)
   {
      const Protos::Common::FindResult_EntryLevel* entry = i.next();
      if (this->setMaxLevel(entry->level()))
         maxLevelChange = true;

      // Search if a similar entry already exists. If so then insert the new node as child.
      if (entry->entry().type() == Protos::Common::Entry_Type_FILE && entry->entry().chunk_size() > 0)
      {
         Common::Hash firstChunk = entry->entry().chunk(0).hash();
         SearchNode* similarNode = 0;
         if ((similarNode = this->indexedFile.value(firstChunk)) && similarNode->isSameAs(entry->entry()))
         {
            if (similarNode->getNbChildren() == 0)
            {
               this->beginInsertRows(this->createIndex(0, 0, similarNode), 0, 0);
               similarNode->insertChild(similarNode);
               this->endInsertRows();
            }

            // Search the better name (node with the lowest level) to display it on the top.
            for (int i = 0; i <= similarNode->getNbChildren(); i++)
            {
               if (i == similarNode->getNbChildren() || static_cast<SearchNode*>(similarNode->getChild(i))->getLevel() > static_cast<int>(entry->level()))
               {
                  this->beginInsertRows(this->createIndex(0, 0, similarNode), i, i);
                  Common::Hash peerID = findResult.peer_id().hash();
                  SearchNode* newNode = similarNode->insertChild(i, *entry, peerID, this->peerListModel.getNick(peerID));
                  this->endInsertRows();

                  if (static_cast<int>(entry->level()) < similarNode->getLevel())
                  {
                     const int row = similarNode->getRow();
                     similarNode->copyFrom(newNode);
                     emit dataChanged(this->createIndex(row, 0, similarNode), this->createIndex(row, 3, similarNode));
                  }

                  break;
               }
            }

            continue;
         }
      }

      currentIndex = this->insertNode(*entry, findResult.peer_id().hash(), currentIndex);
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

SearchModel::SearchNode* SearchModel::getRoot()
{
   return static_cast<SearchNode*>(this->root);
}

/**
  * Create a new node, it can be a directory or a file. It will be inserted in the structure depending its level and its path+name.
  * Return the index of the first entry of the same level.
  */
int SearchModel::insertNode(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, int currentIndex)
{
   if (entry.entry().type() == Protos::Common::Entry_Type_FILE)
      this->nbFiles++;
   else
      this->nbFolders++;

   SearchNode* root = this->getRoot();

   // Search a place to insert the new entry, order (type > level > path > name) must be kept.
   while (currentIndex < root->getNbChildren() && (
      root->getChild(currentIndex)->getEntry().type() > entry.entry().type() ||
      root->getChild(currentIndex)->getEntry().type() == entry.entry().type() && static_cast<SearchNode*>(root->getChild(currentIndex))->getLevel() < static_cast<int>(entry.level()) ||
      root->getChild(currentIndex)->getEntry().type() == entry.entry().type() && static_cast<SearchNode*>(root->getChild(currentIndex))->getLevel() == static_cast<int>(entry.level()) && entryLessThan(root->getChild(currentIndex)->getEntry(), entry.entry())
   ))
      currentIndex++;

   this->beginInsertRows(QModelIndex(), currentIndex, currentIndex);
   SearchNode* newNode = root->insertChild(currentIndex++, entry, peerID, this->peerListModel.getNick(peerID));
   this->endInsertRows();

   if (newNode->getEntry().type() == Protos::Common::Entry_Type_FILE && newNode->getEntry().chunk_size() > 0)
      this->indexedFile.insert(newNode->getEntry().chunk(0).hash(), newNode);

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
  */
QString SearchModel::SearchNode::entryPath(const Protos::Common::Entry& entry)
{
   const QString path = Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path);

   QString completePath;
   if (path.isEmpty())
      completePath.append("/");
   else
   {
      const QString sharedName = Common::ProtoHelper::getStr(entry.shared_dir(), &Protos::Common::SharedDir::shared_name);
      completePath.append("/").append(sharedName).append(path);
   }

   return completePath;
}

SearchModel::SearchNode::SearchNode() :
   level(0)
{
}

SearchModel::SearchNode::SearchNode(const Protos::Common::Entry& entry, int level, const Common::Hash& peerID, const QString& peerNick, Node* parent) :
   Node(entry, parent), level(level), peerID(peerID), peerNick(peerNick)
{
}

SearchModel::SearchNode::SearchNode(const Protos::Common::Entry& entry, const Common::Hash& peerID,  Node* parent) :
   Node(entry, parent), level(0), peerID(peerID)
{
}

SearchModel::SearchNode* SearchModel::SearchNode::insertChild(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchNode* searchNode = new SearchNode(entry.entry(), entry.level(), peerID, peerNick, this);
   this->children << searchNode;
   return searchNode;
}

SearchModel::SearchNode* SearchModel::SearchNode::insertChild(int index, const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchNode* searchNode = new SearchNode(entry.entry(), entry.level(), peerID, peerNick, this);
   this->children.insert(index, searchNode);
   return searchNode;
}

SearchModel::SearchNode* SearchModel::SearchNode::insertChild(SearchModel::SearchNode* node)
{
   SearchNode* searchNode = new SearchNode(node->getEntry(), node->getLevel(), node->getPeerID(), node->peerNick, this);
   this->children << searchNode;
   return searchNode;
}

int SearchModel::SearchNode::getLevel() const
{
   return this->level;
}

Common::Hash SearchModel::SearchNode::getPeerID() const
{
   return this->peerID;
}

QVariant SearchModel::SearchNode::getData(int column) const
{
   switch (column)
   {
   case 0: return Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name);
   case 1:
      if (this->parent->getParent() != 0 && this->parent->getEntry().type() == Protos::Common::Entry_Type_DIR)
         return QVariant();

      if (this->entry.type() == Protos::Common::Entry_Type_FILE && this->getNbChildren() > 0)
         return QVariant();

      return entryPath(this->entry);

   // case 2: // See SearchModel::data(..).

   case 3:
      if (this->entry.type() == Protos::Common::Entry_Type_FILE && this->getNbChildren() > 0)
      {
         for (int i = 0; i < this->getNbChildren(); i++)
            if (this->peerNick != static_cast<const SearchNode*>(this->getChild(i))->peerNick)
               return QVariant();
      }
      return this->peerNick;

   case 4: return Common::Global::formatByteSize(this->entry.size());
   default: return QVariant();
   }
}

SearchModel::Node* SearchModel::SearchNode::newNode(const Protos::Common::Entry& entry)
{
   this->children << new SearchNode(entry, this->peerID, this);
   return this->children.last();
}

void SearchModel::SearchNode::copyFrom(const SearchModel::SearchNode* otherNode)
{
   this->entry.CopyFrom(otherNode->getEntry());
   this->level = otherNode->getLevel();
   this->peerID = otherNode->getPeerID();
   this->peerNick = otherNode->peerNick;
}

bool SearchModel::SearchNode::isSameAs(const Protos::Common::Entry& otherEntry) const
{
   if (otherEntry.chunk_size() != this->entry.chunk_size())
      return false;

   for (int i = 0; i < otherEntry.chunk_size(); i++)
      if (Common::Hash(otherEntry.chunk(i).hash()) != Common::Hash(this->entry.chunk(i).hash()))
         return false;

   return true;
}



