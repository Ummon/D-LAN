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

#pragma once

#include <QDragEnterEvent>

#include <Downloads/DownloadsModel.h>

class TestsDownloadsFlatModel;

namespace GUI
{
   class DownloadsFlatModel : public DownloadsModel
   {
      Q_OBJECT
      friend class ::TestsDownloadsFlatModel; // Check retained storage after removing rows.

   public:
      DownloadsFlatModel(
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         const PeerListModel& peerListModel,
         const SharedEntryListModel& sharedEntryListModel,
         const IFilter<DownloadFilterStatus>& filter
      );

      quint64 getTotalBytesInQueue() const;
      quint64 getTotalBytesDownloadedInQueue() const;
      quint64 getEta() const;

      void updateDownloads(const Protos::GUI::State& state) override;
      // One rate sample per incoming state, independent of visible rows and filters.
      void updateProgress(const Protos::GUI::State& state);

      QList<quint64> getDownloadIDs(const QModelIndex& index) const override;

      bool isDownloadPaused(const QModelIndex& index) const override;
      bool isEntryLocationKnown(const QModelIndex& index) const override;
      bool isFileComplete(const QModelIndex& index) const override;
      bool isSourceAlive(const QModelIndex& index) const override;
      Protos::Common::Entry::Type getType(const QModelIndex& index) const override;

      QString getPath(const QModelIndex& index, bool appendFilename = true) const override;

      int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
      Qt::DropActions supportedDropActions() const override;
      Qt::ItemFlags flags(const QModelIndex& index) const override;

   signals:
      void globalProgressChanged();

   protected:
      bool dropMimeData(
         const QMimeData* data,
         Qt::DropAction action,
         int row,
         int column,
         const QModelIndex & parent
      ) override;

   private:
      quint64 totalBytesInQueue;
      quint64 totalBytesDownloadedInQueue;
      quint64 eta;

      static constexpr int NB_OF_DL_RATE_VALUES = 10;
      int nbOfNonZeroDlRateValues;
      quint64 sumDlRateValues;
      quint32 dlRateValues[NB_OF_DL_RATE_VALUES]{};
      quint32 currentDlRateValueIndex;

      QList<Protos::GUI::State::Download> downloads;
   };
}
