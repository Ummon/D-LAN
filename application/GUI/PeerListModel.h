#ifndef GUI_PEERLISTMODEL_H
#define GUI_PEERLISTMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <CoreConnection.h>

namespace GUI
{
   class PeerListModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      PeerListModel(CoreConnection& coreConnection);
      QString getNick(const Common::Hash& peerID);
      bool isOurself(int rowNum) const;
      Common::Hash getPeerID(int rowNum) const;
      void clear();

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   signals:
      /**
        * To remove peer files browse window. (Not used).
        */
      void peersRemoved(QList<Common::Hash> peerIDs);

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      void setPeers(const google::protobuf::RepeatedPtrField<Protos::GUI::Peer>& peers);
      void sort();

      CoreConnection& coreConnection;

      struct Peer
      {
         Peer(const Common::Hash& peerID, const QString& nick, quint64 sharingAmount)
            : peerID(peerID), nick(nick), sharingAmount(sharingAmount) {}

         bool operator==(const Peer& p) const { return this->peerID == p.peerID; }
         bool operator!=(const Peer& p) const { return this->peerID != p.peerID; }
         bool operator<(const Peer& p) const { return this->sharingAmount < p.sharingAmount; }
         bool operator>(const Peer& p) const { return this->sharingAmount > p.sharingAmount; }

         Common::Hash peerID;
         QString nick;
         quint64 sharingAmount;
      };
      QList<Peer> peers;
   };

}

#endif
