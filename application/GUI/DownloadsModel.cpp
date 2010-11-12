#include <DownloadsModel.h>
using namespace GUI;

#include <QPixmap>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

DownloadsModel::DownloadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel)
   : coreConnection(coreConnection), peerListModel(peerListModel)
{
   connect(&this->coreConnection, SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
}

int DownloadsModel::rowCount(const QModelIndex& parent) const
{
   return this->downloads.size();
}

int DownloadsModel::columnCount(const QModelIndex& parent) const
{
   return 4;
}

QVariant DownloadsModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->downloads.size())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      {
         const Protos::GUI::State_Download& currentDownload = this->downloads[index.row()];
         switch (index.column())
         {
         case 0: return Common::ProtoHelper::getStr(currentDownload.entry(), &Protos::Common::Entry::name);
         case 1: return Common::Global::formatByteSize(currentDownload.entry().size());
         case 2: return QVariant::fromValue(Progress(currentDownload.progress(), currentDownload.status()));
         case 3:
            {
               QString peersStr;
               // O(n^2).
               for (int i = 0; i < currentDownload.peer_id_size(); i++)
               {
                  if (i != 0)
                     peersStr.append(" ");
                  peersStr.append(this->peerListModel.getNick(currentDownload.peer_id(i).hash().data()));
               }
               return peersStr;
            }
         default: return QVariant();
         }
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            if (this->downloads[index.row()].status() >= Protos::GUI::State_Download_Status_UNKNOWN_PEER)
               return QPixmap(":/icons/ressources/error.png");
            else if (this->downloads[index.row()].entry().type() == Protos::Common::Entry_Type_DIR)
               return QPixmap(":/icons/ressources/folder.png");
            else
               return QPixmap(":/icons/ressources/file.png");
         }
         return QVariant();
      }

   case Qt::ToolTipRole:
      switch(this->downloads[index.row()].status())
      {
      case Protos::GUI::State_Download_Status_UNKNOWN_PEER:
         return "Unknown source peer";
      case Protos::GUI::State_Download_Status_ENTRY_NOT_FOUND:
         return "The source peer doesn't have the entry";
      case Protos::GUI::State_Download_Status_NO_SOURCE:
         return "There is no source to download from";
      case Protos::GUI::State_Download_Status_NO_SHARED_DIRECTORY_TO_WRITE:
         return "No incoming folder";
      case Protos::GUI::State_Download_Status_NO_ENOUGH_FREE_SPACE:
         return "Not enough free space left";
      case Protos::GUI::State_Download_Status_THE_FILE_ALREADY_EXISTS:
         return "The file alreasy exists";
      case Protos::GUI::State_Download_Status_UNABLE_TO_CREATE_THE_FILE:
         return "Unable to create the file";
      default:
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft;

   default: return QVariant();
   }
}

quint64 DownloadsModel::getDownloadID(int row) const
{
   if (row >= this->downloads.size())
      return 0;
   return this->downloads[row].id();
}

QList<quint64> DownloadsModel::getCompletedDownloadIDs() const
{
   QList<quint64> IDs;
   for (int i = 0; i < this->downloads.size(); i++)
      if (this->downloads[i].status() == Protos::GUI::State_Download_Status_COMPLETE)
         IDs << this->downloads[i].id();
   return IDs;
}

void DownloadsModel::newState(const Protos::GUI::State& state)
{
   int i = 0;
   for (; i < state.download_size() && i < this->downloads.size(); i++)
   {
      if (state.download(i) != this->downloads[i])
      {
         this->downloads[i].CopyFrom(state.download(i));
         emit dataChanged(this->createIndex(i, 0), this->createIndex(i, 3));
      }
   }

   // Insert new elements.
   if (i < state.download_size())
   {
      this->beginInsertRows(QModelIndex(), i, state.download_size() - 1);
      while (i < state.download_size())
      {
         const Protos::GUI::State_Download& download = state.download(i++);
         this->downloads << download;
      }
      this->endInsertRows();
   }

   // Delete some elements.
   if (i < this->downloads.size())
   {
      this->beginRemoveRows(QModelIndex(), i, this->downloads.size() - 1);
      const int nbDownloads = this->downloads.size();
      while (i++ < nbDownloads)
         this->downloads.removeLast();
      this->endRemoveRows();
   }
}

bool GUI::operator==(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2)
{
   if (
      d1.id() != d2.id() ||
      d1.entry().type() != d2.entry().type() ||
      d1.entry().path() != d2.entry().path() ||
      d1.entry().name() != d2.entry().name() ||
      d1.entry().size() != d2.entry().size() ||
      d1.status() != d2.status() ||
      d1.progress() != d2.progress() ||
      d1.peer_id_size() != d2.peer_id_size()
   )
      return false;

   for (int i = 0; i < d1.peer_id_size(); i++)
      if (d1.peer_id(i).hash() != d2.peer_id(i).hash())
         return false;

   return true;
}

bool GUI::operator!=(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2)
{
   return !(d1 == d2);
}
