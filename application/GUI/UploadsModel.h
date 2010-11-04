#ifndef GUI_UPLOADSMODEL_H
#define GUI_UPLOADSMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <CoreConnection.h>
#include <PeerListModel.h>

namespace GUI
{
   class UploadsModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      explicit UploadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      CoreConnection& coreConnection;
      PeerListModel& peerListModel;

      QList<Protos::GUI::State_Upload> uploads;
   };

   bool operator==(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2);
   bool operator!=(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2);
}

#endif
