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
  
#include <Downloads/DownloadsModel.h>
using namespace GUI;

DownloadsModel::DownloadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   sharedDirsModel(sharedDirsModel),
   filter(filter)
{
   qRegisterMetaTypeStreamOperators<Progress>("Progress"); // Don't know where to put this call..
   connect(this->coreConnection.data(), SIGNAL(newState(Protos::GUI::State)), this, SLOT(onNewState(Protos::GUI::State)));
}

QList<int> DownloadsModel::getNonFilteredDownloadIndices(const Protos::GUI::State& state) const
{
   int statusToFilter = 0;
   const QList<DownloadFilterStatus>& filterStatus = this->filter.getFilteredValues();
   for (int i = 0; i < filterStatus.size(); i++)
      statusToFilter |= filterStatus[i];

   QList<int> indices;
   for (int i = 0; i < state.download_size(); i++)
   {
      switch (state.download(i).status())
      {
      case Protos::GUI::State::Download::QUEUED:
      case Protos::GUI::State::Download::PAUSED:
         if (!(statusToFilter & STATUS_QUEUED))
            indices << i;
         break;

      case Protos::GUI::State::Download::GETTING_THE_HASHES:
      case Protos::GUI::State::Download::DOWNLOADING:
         if (!(statusToFilter & STATUS_DOWNLOADING))
            indices << i;
         break;

      case Protos::GUI::State::Download::COMPLETE:
         if (!(statusToFilter & STATUS_COMPLETE))
            indices << i;
         break;

      case Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE:
      case Protos::GUI::State::Download::ENTRY_NOT_FOUND:
      case Protos::GUI::State::Download::NO_SOURCE:
      case Protos::GUI::State::Download::NO_SHARED_DIRECTORY_TO_WRITE:
      case Protos::GUI::State::Download::NO_ENOUGH_FREE_SPACE:
      case Protos::GUI::State::Download::UNABLE_TO_CREATE_THE_FILE:
      case Protos::GUI::State::Download::UNABLE_TO_RETRIEVE_THE_HASHES:
      case Protos::GUI::State::Download::TRANSFERT_ERROR:
         if (!(statusToFilter & STATUS_ERROR))
            indices << i;
         break;

      case Protos::GUI::State_Download_Status_DELETED:; // We don't care about deleted entries.
      }
   }

   return indices;
}


/**
  * Used when drag'n dropping some downloads.
  */
QDataStream& GUI::operator<<(QDataStream& out, const Progress& progress)
{
   out << progress.progress;
   out << static_cast<int>(progress.status);
   return out;
}

/**
  * Used when drag'n dropping some downloads.
  */
QDataStream& GUI::operator>>(QDataStream& in, Progress& progress)
{
   in >> progress.progress;
   int status;
   in >> status;
   progress.status = static_cast<Protos::GUI::State_Download_Status>(status);
   return in;
}

bool GUI::operator==(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   if (
      d1.id() != d2.id() ||
      d1.local_entry().type() != d2.local_entry().type() ||
      d1.local_entry().path() != d2.local_entry().path() ||
      d1.local_entry().name() != d2.local_entry().name() ||
      d1.local_entry().size() != d2.local_entry().size() ||
      d1.status() != d2.status() ||
      d1.downloaded_bytes() != d2.downloaded_bytes() ||
      d1.peer_id_size() != d2.peer_id_size()
   )
      return false;

   for (int i = 0; i < d1.peer_id_size(); i++)
      if (d1.peer_id(i).hash() != d2.peer_id(i).hash())
         return false;

   return true;
}

bool GUI::operator!=(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2)
{
   return !(d1 == d2);
}
