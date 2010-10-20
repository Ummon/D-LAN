#ifndef GUI_CHATMODEL_H
#define GUI_CHATMODEL_H

#include <QAbstractTableModel>
#include <QString>
#include <QDateTime>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <CoreConnection.h>
#include <PeerListModel.h>

namespace GUI
{
   class ChatModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      ChatModel(CoreConnection& coreConnection, PeerListModel& peerListModel);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   public slots:
      void newChatMessage(const Common::Hash& peerID, const QString& message);

   private:
      CoreConnection& coreConnection;
      PeerListModel& peerListModel;

      struct Message
      {
         Message(const Common::Hash& peerID, const QString& nick, const QDateTime& dateTime, const QString& message)
            : peerID(peerID), nick(nick), dateTime(dateTime), message(message) {}

         Common::Hash peerID;
         QString nick;
         QDateTime dateTime;
         QString message;
      };

      QList<Message> messages;
   };

}

#endif
