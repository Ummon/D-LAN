#ifndef GUI_DOWNLOADSMODEL_H
#define GUI_DOWNLOADSMODEL_H

#include <QAbstractTableModel>
#include <QDragEnterEvent>

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

      quint64 getDownloadID(int row) const;
      QList<quint64> getCompletedDownloadIDs() const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      Qt::DropActions supportedDropActions() const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

  protected:
      bool dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex & parent);

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

   QDataStream& operator<<(QDataStream& out, const Progress& progress);
   QDataStream& operator>>(QDataStream& in, Progress& progress);

   bool operator==(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2);
   bool operator!=(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2);
}
Q_DECLARE_METATYPE(GUI::Progress)

#endif
