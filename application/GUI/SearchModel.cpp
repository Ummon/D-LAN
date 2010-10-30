#include <SearchModel.h>
using namespace GUI;

#include <Common/Settings.h>
#include <Common/Global.h>

const int SearchModel::NB_SIGNAL_PROGRESS(50);

SearchModel::SearchModel(CoreConnection& coreConnection, PeerListModel& peerListModel)
   : BrowseModel(coreConnection, Common::Hash()), peerListModel(peerListModel), currentProgress(0)
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
}

void SearchModel::search(const QString& terms)
{
   // Unable to perform more than one search. (TODO)

   this->searchResult = this->coreConnection.search(terms);
   connect(this->searchResult.data(), SIGNAL(result(const Protos::Common::FindResult&)), this, SLOT(result(const Protos::Common::FindResult&)));
   this->searchResult->start();

   this->timerProgress.start();
   this->timerTimeout.start();
}

int SearchModel::columnCount(const QModelIndex& parent) const
{
   return 4;
}

void SearchModel::loadChildren(const QPersistentModelIndex &index)
{
   this->peerID = static_cast<const SearchNode*>(index.internalPointer())->getPeerID();
   BrowseModel::loadChildren(index);
}

void SearchModel::result(const Protos::Common::FindResult& findResult)
{
   SearchNode* root = this->getRoot();

   int currentDirNode = -1; // Will point to the first directory of a group of same level directory.
   if (root->getNbChildren() != 0 && root->getChild(0)->getEntry().type() == Protos::Common::Entry_Type_DIR)
      currentDirNode = 0;

   for (int i = 0; i < findResult.entry_size(); i++)
   {
      const Protos::Common::FindResult_EntryLevel entry = findResult.entry(i);

      if (entry.entry().type() == Protos::Common::Entry_Type_DIR)
      {
         if (currentDirNode == -1)
         {
            this->beginInsertRows(QModelIndex(), 0, 0);
            const Common::Hash peerID = findResult.peer_id().hash().data();
            this->getRoot()->insertChild(entry, peerID, this->peerListModel.getNick(peerID));
            this->endInsertRows();
            currentDirNode = 0;
         }
         else
         {
            // Search the first directory with the same level or, at least, a greater level.
            while (dynamic_cast<SearchNode*>(root->getChild(currentDirNode))->getLevel() < entry.level())
            {
               currentDirNode += 1;
               if (currentDirNode == root->getNbChildren() || root->getChild(currentDirNode)->getEntry().type() != Protos::Common::Entry_Type_DIR)
                  break;
            }

            // Search a place to insert the new directory node among nodes of the same level, the alphabetic order must be kept.
            int nodeToInsertBefore = currentDirNode;
            while (
               Common::ProtoHelper::getStr(entry.entry(), &Protos::Common::Entry::path) <= Common::ProtoHelper::getStr(root->getChild(nodeToInsertBefore)->getEntry(), &Protos::Common::Entry::path) &&
               Common::ProtoHelper::getStr(entry.entry(), &Protos::Common::Entry::name) <= Common::ProtoHelper::getStr(root->getChild(nodeToInsertBefore)->getEntry(), &Protos::Common::Entry::name)
            )
            {
               nodeToInsertBefore += 1;
               if (
                  nodeToInsertBefore == root->getNbChildren() ||
                  root->getChild(nodeToInsertBefore)->getEntry().type() != Protos::Common::Entry_Type_DIR ||
                  dynamic_cast<SearchNode*>(root->getChild(nodeToInsertBefore))->getLevel() > entry.level()
               )
                  break;
            }

            this->beginInsertRows(QModelIndex(), nodeToInsertBefore, nodeToInsertBefore);
            const Common::Hash peerID = findResult.peer_id().hash().data();
            root->insertChild(nodeToInsertBefore, entry, peerID, this->peerListModel.getNick(peerID));
            this->endInsertRows();
         }
      }
      else // It's a file.
      {
         // TODO
      }
   }
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
   return dynamic_cast<SearchNode*>(this->root);
}

/////

SearchModel::SearchNode::SearchNode()
   : level(0)
{
}

SearchModel::SearchNode::SearchNode(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick, Node* parent)
   : Node(entry.entry(), parent), level(entry.level()), peerID(peerID), peerNick(peerNick)
{
}

SearchModel::SearchNode::SearchNode(const Protos::Common::Entry& entry, const Common::Hash& peerID,  Node* parent)
   : Node(entry, parent), level(0), peerID(peerID)
{
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
   switch(column)
   {
   case 0: return this->peerNick;
   case 1: return Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::path);
   case 2: return Common::ProtoHelper::getStr(this->entry, &Protos::Common::Entry::name);
   case 3: return Common::Global::formatByteSize(this->entry.size());
   default: return QVariant();
   }
}

SearchModel::Node* SearchModel::SearchNode::newNode(const Protos::Common::Entry& entry)
{
   this->children << new SearchNode(entry, this->peerID, this);
   return this->children.last();
}

SearchModel::SearchNode* SearchModel::SearchNode::insertChild(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchNode* searchNode = new SearchNode(entry, peerID, peerNick, this);
   this->children << searchNode;
   return searchNode;
}

SearchModel::SearchNode* SearchModel::SearchNode::insertChild(int index, const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick)
{
   SearchNode* searchNode = new SearchNode(entry, peerID, peerNick, this);
   this->children.insert(index, searchNode);
   return searchNode;
}



