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
  
#ifndef GUI_FLATDOWNLOADSMODEL_H
#define GUI_FLATDOWNLOADSMODEL_H

#include <QDragEnterEvent>

#include <Downloads/DownloadsModel.h>

namespace GUI
{
   class DownloadsFlatModel : public DownloadsModel
   {
      Q_OBJECT
      static const int WEIGHT_LAST_ETA = 3; // Used in the weighted mean computation.

   public:
      DownloadsFlatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter);

      quint64 getTotalBytesInQueue() const;
      quint64 getTotalBytesDownloadedInQueue() const;
      quint64 getEta() const;

      QList<quint64> getDownloadIDs(const QModelIndex& index) const;

      bool isDownloadPaused(const QModelIndex& index) const;
      bool isFileLocationKnown(const QModelIndex& index) const;
      bool isFileComplete(const QModelIndex& index) const;
      bool isSourceAlive(const QModelIndex& index) const;
      Protos::Common::Entry::Type getType(const QModelIndex& index) const;

      QString getPath(const QModelIndex& index, bool appendFilename = true) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      Qt::DropActions supportedDropActions() const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

   signals:
      void globalProgressChanged();

   protected:
      bool dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex & parent);

   protected slots:
      void onNewState(const Protos::GUI::State& state);

   private:
      quint64 totalBytesInQueue;
      quint64 totalBytesDownloadedInQueue;
      quint64 eta;

      QList<Protos::GUI::State::Download> downloads;
   };
}

#endif
