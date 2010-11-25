#include <DownloadsModel.h>
using namespace GUI;

#include <QPixmap>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <Log.h>

DownloadsModel::DownloadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel)
   : coreConnection(coreConnection), peerListModel(peerListModel)
{
   qRegisterMetaTypeStreamOperators<Progress>("Progress"); // Don't know where to put this call..
   connect(&this->coreConnection, SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
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

Qt::DropActions DownloadsModel::supportedDropActions() const
{
   return Qt::MoveAction;
}

Qt::ItemFlags DownloadsModel::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid())
       return Qt::ItemIsDragEnabled | defaultFlags;
   else
       return Qt::ItemIsDropEnabled | defaultFlags;

   return defaultFlags;
}

bool DownloadsModel::removeRows ( int row, int count, const QModelIndex & parent) const
{
   L_DEBU("OMG 4");
   return false;
}

bool DownloadsModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex & parent)
{
   //return QAbstractTableModel::dropMimeData(data, action, row, column, parent);

   if (!data || action != Qt::MoveAction)
       return false;

   QStringList types = this->mimeTypes();
   if (types.isEmpty())
       return false;
   QString format = types.at(0);
   if (!data->hasFormat(format))
       return false;

   QByteArray encoded = data->data(format);
   QDataStream stream(&encoded, QIODevice::ReadOnly);


   QList<quint64> downloadIDs;
   int top = INT_MAX;

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
          top = qMin(currentRow, top);
          if (currentRow >= 0 && currentRow < this->downloads.size())
             downloadIDs << this->downloads[currentRow].id();
       }
   }

   int offset = row - top;
   if (offset > 0)
      offset -= 1;

   if (offset == 0)
      return false;

   // TODO : send a message to the core to move the downloads.
   L_DEBU(QString("Offset = %1, nb = %2").arg(offset).arg(downloadIDs.size()));

   return false;
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
