#ifndef GUI_DOWNLOADSMODEL_H
#define GUI_DOWNLOADSMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <CoreConnection.h>
#include <PeerListModel.h>

namespace GUI
{
   class DownloadsModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      explicit DownloadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      quint64 getDownloadID(int row) const;
      QList<quint64> getCompletedDownloadIDs() const;

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      CoreConnection& coreConnection;
      PeerListModel& peerListModel;

      QList<Protos::GUI::State_Download> downloads;
   };

   struct Progress
   {
      Progress() : progress(0), status(Protos::GUI::State_Download_Status_QUEUED) {}
      Progress(quint32 progress, Protos::GUI::State_Download_Status status) : progress(progress), status(status) {}

      quint32 progress;
      Protos::GUI::State_Download_Status status;
   };

   bool operator==(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2);
   bool operator!=(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2);
}
Q_DECLARE_METATYPE(GUI::Progress)

#endif
