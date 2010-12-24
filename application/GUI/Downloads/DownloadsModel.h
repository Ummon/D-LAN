/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef GUI_DOWNLOADSMODEL_H
#define GUI_DOWNLOADSMODEL_H

#include <QAbstractTableModel>
#include <QDragEnterEvent>

#include <Protos/gui_protocol.pb.h>

#include <IFilter.h>
#include <CoreConnection/CoreConnection.h>
#include <PeerList/PeerListModel.h>
#include <Downloads/DownloadFilterStatus.h>

namespace GUI
{
   class DownloadsModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      explicit DownloadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel, const IFilter<DownloadFilterStatus>& filter);

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
      const IFilter<DownloadFilterStatus>& filter;

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
