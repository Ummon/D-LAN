#ifndef GUI_SEARCHMODEL_H
#define GUI_SEARCHMODEL_H

#include <QAbstractItemModel>
#include <QTimer>
#include <QHash>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

#include <CoreConnection.h>
#include <BrowseModel.h>
#include <PeerListModel.h>

namespace GUI
{
   class SearchModel : public BrowseModel
   {
      class SearchNode;
      static const int NB_SIGNAL_PROGRESS; // The number of signal progress sent during a search.
      Q_OBJECT
   public:
      SearchModel(CoreConnection& coreConnection, PeerListModel& peerListModel);
      ~SearchModel();

      Common::Hash getPeerID(const QModelIndex& index);

      void search(const QString& terms);

      int columnCount(const QModelIndex& parent = QModelIndex()) const;

   signals:
      /**
        * 0 to 100;
        */
      void progress(int);

   protected:
      void loadChildren(const QPersistentModelIndex &index);

   protected slots:
      void result(const Protos::Common::FindResult& findResult);
      void sendNextProgress();
      void stopSearching();

   private:
      SearchNode* getRoot();
      int insertNode(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, int currentIndex);

      PeerListModel& peerListModel;

      QSharedPointer<ISearchResult> searchResult;

      QTimer timerProgress;
      QTimer timerTimeout;

      int currentProgress;

      QHash<Common::Hash, SearchNode*> indexedFile;

      class SearchNode : public Node
      {
      public:
         static QString entryPath(const Protos::Common::Entry& entry);

         SearchNode();
         SearchNode(const Protos::Common::Entry& entry, int level, const Common::Hash& peerID, const QString& peerNick, Node* parent);
         SearchNode(const Protos::Common::Entry& entry, const Common::Hash& peerID,  Node* parent);

         SearchNode* insertChild(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);
         SearchNode* insertChild(int index, const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);
         SearchNode* insertChild(SearchNode* node);

         int getLevel() const;
         Common::Hash getPeerID() const;
         QVariant getData(int column) const;

         void copyFrom(const SearchNode* otherNode);
         bool isSameAs(const Protos::Common::Entry& otherEntry) const;

      protected:
         Node* newNode(const Protos::Common::Entry& entry);

      private:
         int level;
         Common::Hash peerID;
         QString peerNick;
      };
   };
}

#endif
