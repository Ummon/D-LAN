/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <IFilter.h>
#include <PeerList/PeerListModel.h>
#include <Downloads/DownloadFilterStatus.h>
#include <Settings/DirListModel.h>

namespace GUI
{
   class DownloadsModel : public QAbstractTableModel
   {
      Q_OBJECT

      static const int WEIGHT_LAST_ETA = 5; // Used in the weighted mean computation.

   public:
      explicit DownloadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter);

      quint64 getTotalBytesInQueue() const;
      quint64 getTotalBytesDownloadedInQueue() const;
      quint64 getEta() const;

      quint64 getDownloadID(int row) const;

      bool isDownloadPaused(int row) const;
      bool isFileLocationKnown(int row) const;
      bool isFileComplete(int row) const;

      QString getPath(int row, bool appendFilename = true) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      Qt::DropActions supportedDropActions() const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

   signals:
      void globalProgressChanged();

   protected:
      bool dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex & parent);

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const PeerListModel& peerListModel;
      const DirListModel& sharedDirsModel;
      const IFilter<DownloadFilterStatus>& filter;

      quint64 totalBytesInQueue;
      quint64 totalBytesDownloadedInQueue;
      quint64 eta;

      QList<Protos::GUI::State::Download> downloads;
   };

   struct Progress
   {
      Progress() : progress(0), status(Protos::GUI::State::Download::QUEUED) {}
      Progress(quint32 progress, Protos::GUI::State::Download::Status status) : progress(progress), status(status) {}

      quint32 progress;
      Protos::GUI::State::Download::Status status;
   };

   QDataStream& operator<<(QDataStream& out, const Progress& progress);
   QDataStream& operator>>(QDataStream& in, Progress& progress);

   bool operator==(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
   bool operator!=(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
}

Q_DECLARE_METATYPE(GUI::Progress)

#endif
