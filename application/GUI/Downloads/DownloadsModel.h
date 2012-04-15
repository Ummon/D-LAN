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
#include <QList>

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
   public:
      DownloadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter);
      virtual ~DownloadsModel() {}

      virtual QList<quint64> getDownloadIDs(const QModelIndex& index) const = 0;

      virtual bool isDownloadPaused(const QModelIndex& index) const = 0;
      virtual bool isFileLocationKnown(const QModelIndex& index) const = 0;
      virtual bool isFileComplete(const QModelIndex& index) const = 0;
      virtual bool isSourceAlive(const QModelIndex& index) const = 0;
      virtual Protos::Common::Entry::Type getType(const QModelIndex& index) const = 0;

      /**
        * @remarks appendFilename is only valid if the index is a file (not a directory).
        */
      virtual QString getPath(const QModelIndex& index, bool appendFilename = true) const = 0;

      int columnCount(const QModelIndex& parent = QModelIndex()) const;

   protected slots:
      virtual void onNewState(const Protos::GUI::State& state) = 0;

   protected:
      QVariant getData(const Protos::GUI::State::Download& download, const QModelIndex& index, int role) const;
      QList<int> getNonFilteredDownloadIndices(const Protos::GUI::State& state) const;
      QList<int> getDraggedRows(const QMimeData* data);

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const PeerListModel& peerListModel;
      const DirListModel& sharedDirsModel;
      const IFilter<DownloadFilterStatus>& filter;
   };

   struct Progress
   {
      Progress() : progress(0), status(Protos::GUI::State::Download::QUEUED), type(Protos::Common::Entry::FILE) {}
      Progress(quint32 progress, Protos::GUI::State::Download::Status status, Protos::Common::Entry::Type type) : progress(progress), status(status), type(type) {}

      quint32 progress; // 0 to 10'000.
      Protos::GUI::State::Download::Status status;
      Protos::Common::Entry::Type type;
   };

   QDataStream& operator<<(QDataStream& out, const Progress& progress);
   QDataStream& operator>>(QDataStream& in, Progress& progress);

   bool operator==(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
   bool operator!=(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
}

Q_DECLARE_METATYPE(GUI::Progress)

#endif
