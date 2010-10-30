#ifndef GUI_SEARCHMODEL_H
#define GUI_SEARCHMODEL_H

#include <QAbstractItemModel>
#include <QTimer>

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

      void search(const QString& terms);

      //QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
      //QModelIndex parent(const QModelIndex& child) const;
      //int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      //QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

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

      PeerListModel& peerListModel;

      QSharedPointer<ISearchResult> searchResult;

      QTimer timerProgress;
      QTimer timerTimeout;

      int currentProgress;

      class SearchNode : public Node
      {
      public:
         SearchNode();
         SearchNode(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick, Node* parent);
         SearchNode(const Protos::Common::Entry& entry, const Common::Hash& peerID,  Node* parent);
         int getLevel() const;
         Common::Hash getPeerID() const;
         SearchNode* insertChild(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);
         SearchNode* insertChild(int index, const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);

         QVariant getData(int column) const;

      protected:
         Node* newNode(const Protos::Common::Entry& entry);

      private:
         const int level;
         const Common::Hash peerID;
         const QString peerNick;
      };
   };
}

#endif
