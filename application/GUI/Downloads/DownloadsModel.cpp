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

#include <QPixmap>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <IconProvider.h>

DownloadsModel::DownloadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   sharedDirsModel(sharedDirsModel),
   filter(filter)
{
   qRegisterMetaTypeStreamOperators<Progress>("Progress"); // Don't know where to put this call..
   connect(this->coreConnection.data(), SIGNAL(newState(Protos::GUI::State)), this, SLOT(onNewState(Protos::GUI::State)));
}

int DownloadsModel::columnCount(const QModelIndex& /*parent*/) const
{
   return 5;
}

QVariant DownloadsModel::getData(const Protos::GUI::State::Download& download, const QModelIndex& index, int role) const
{
   switch (role)
   {
   case Qt::DisplayRole:
      switch (index.column())
      {
      case 0: return Common::ProtoHelper::getStr(download.local_entry(), &Protos::Common::Entry::name);
      case 1: return Common::Global::formatByteSize(download.local_entry().size());
      case 2:
         return QVariant::fromValue(Progress(
            download.local_entry().size() == 0 ? 0 : 10000 * download.downloaded_bytes() / download.local_entry().size(),
            download.status(),
            download.local_entry().type()
         ));
      case 3:
         if (download.has_peer_source_nick())
            return Common::ProtoHelper::getStr(download, &Protos::GUI::State::Download::peer_source_nick);
         return QString();

      case 4:
         if (download.peer_id_size() > 1)
            return QString("+").append(QString::number(download.peer_id_size() - 1));
         return QString();

      default: return QVariant();
      }

   case Qt::DecorationRole:
      if (index.column() == 0)
      {
         if (download.status() >= Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE)
            return QPixmap(":/icons/ressources/error.png");
         else
            return IconProvider::getIcon(download.local_entry());
      }
      return QVariant();

   case Qt::ToolTipRole:
      if (index.column() == 4)
      {
         QString peersStr;
         for (int i = 1; i < download.peer_id_size(); i++)
         {
            Common::Hash peerID(download.peer_id(i).hash());
            const QString nick = this->peerListModel.getNick(peerID);
            if (nick.isNull())
               continue;
            if (!peersStr.isEmpty())
               peersStr.append("\n");
            peersStr += nick;
         }
         return peersStr;
      }
      else
      {
         QString toolTip;
         switch (download.status())
         {
         case Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE:
            toolTip += tr("Source peer offline (%1)").arg(Common::ProtoHelper::getStr(download, &Protos::GUI::State::Download::peer_source_nick));
            break;
         case Protos::GUI::State::Download::ENTRY_NOT_FOUND:
            toolTip += tr("The source peer doesn't have the entry");
            break;
         case Protos::GUI::State::Download::NO_SOURCE:
            toolTip += tr("There is no source to download from");
            break;
         case Protos::GUI::State::Download::NO_SHARED_DIRECTORY_TO_WRITE:
            toolTip += tr("No incoming directory");
            break;

         case Protos::GUI::State::Download::NO_ENOUGH_FREE_SPACE:
            toolTip += tr("Not enough free space left");
            break;
         case Protos::GUI::State::Download::UNABLE_TO_CREATE_THE_FILE:
            toolTip += tr("Unable to create the file");
            break;
         case Protos::GUI::State::Download::UNABLE_TO_RETRIEVE_THE_HASHES:
            toolTip += tr("Unable to retrieve the hashes");
            break;

         case Protos::GUI::State::Download::TRANSFERT_ERROR:
            toolTip += tr("Transfert error");
            break;
         case Protos::GUI::State::Download::UNABLE_TO_OPEN_THE_FILE:
            toolTip += tr("Unable to open the file");
            break;
         case Protos::GUI::State::Download::FILE_IO_ERROR:
            toolTip += tr("Unable to write the file");
            break;
         case Protos::GUI::State::Download::FILE_NON_EXISTENT:
            toolTip += tr("The local file has been deleted");
            break;
         case Protos::GUI::State::Download::GOT_TOO_MUCH_DATA:
            toolTip += tr("Too much data received");
            break;
         case Protos::GUI::State::Download::HASH_MISSMATCH:
            toolTip += tr("Data received do not match the hash");
            break;
         default:;
         }
         const QString& path = this->getPath(index);
         if (!path.isEmpty())
         {
            if (!toolTip.isEmpty())
               toolTip += " - ";
            toolTip += this->getPath(index);
         }
         return toolTip;
      }

   case Qt::TextAlignmentRole:
      return static_cast<int>(index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft) | Qt::AlignVCenter;

   default: return QVariant();
   }
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

      case Protos::GUI::State::Download::PAUSED:
      case Protos::GUI::State::Download::UNKNOWN_PEER_SOURCE:
      case Protos::GUI::State::Download::ENTRY_NOT_FOUND:
      case Protos::GUI::State::Download::NO_SOURCE:
      case Protos::GUI::State::Download::NO_SHARED_DIRECTORY_TO_WRITE:
      case Protos::GUI::State::Download::NO_ENOUGH_FREE_SPACE:
      case Protos::GUI::State::Download::UNABLE_TO_CREATE_THE_FILE:
      case Protos::GUI::State::Download::UNABLE_TO_RETRIEVE_THE_HASHES:
      case Protos::GUI::State::Download::TRANSFERT_ERROR:
      case Protos::GUI::State::Download::UNABLE_TO_OPEN_THE_FILE:
      case Protos::GUI::State::Download::FILE_IO_ERROR:
      case Protos::GUI::State::Download::FILE_NON_EXISTENT:
      case Protos::GUI::State::Download::GOT_TOO_MUCH_DATA:
      case Protos::GUI::State::Download::HASH_MISSMATCH:
         if (!(statusToFilter & STATUS_INACTIVE))
            indices << i;
         break;

      case Protos::GUI::State_Download_Status_DELETED:; // We don't care about deleted entries.
      }
   }

   return indices;
}

QList<int> DownloadsModel::getDraggedRows(const QMimeData* data)
{
   if (!data)
      return QList<int>();

   QStringList types = this->mimeTypes();
   if (types.isEmpty())
       return QList<int>();

   QString format = types.at(0);
   if (!data->hasFormat(format))
      return QList<int>();

   QByteArray encoded = data->data(format);
   QDataStream stream(&encoded, QIODevice::ReadOnly);
   QList<int> rows;

   int previousRow = -1;
   while (!stream.atEnd())
   {
       int currentRow;
       int currentCol;
       QMap<int, QVariant> value;
       stream >> currentRow >> currentCol >> value;

       if (currentRow != previousRow)
       {
          previousRow = currentRow;
          if (currentRow >= 0)
             rows << currentRow;
       }
   }

   return rows;
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
