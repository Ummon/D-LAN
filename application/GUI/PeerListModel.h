#ifndef GUI_PEERLISTMODEL_H
#define GUI_PEERLISTMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

namespace GUI
{
   class PeerListModel : public QAbstractTableModel
   {
   public:
      PeerListModel();
      void setPeers(const google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer>& peers);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private:
      void sort();

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
