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

#include <Utils.h>
using namespace DM;

#include <QRegularExpression>

QString Utils::sharedName(QString userName)
{
   // Shared names are labels, not filesystem paths. Use portable filename rules.
   for (qsizetype i = 0; i < userName.size(); ++i)
      if (userName.at(i).unicode() < 32 || QStringLiteral("<>:\"/\\|?*").contains(userName.at(i)))
         userName[i] = '_';

   // Bound both UTF-8 bytes and UTF-16 units without splitting a surrogate pair.
   while (userName.toUtf8().size() > 240)
   {
      const bool pair = userName.back().isLowSurrogate() && userName.size() > 1 && userName.at(userName.size() - 2).isHighSurrogate();
      userName.chop(pair ? 2 : 1);
   }
   while (userName.endsWith(' ') || userName.endsWith('.'))
      userName.chop(1);
   if (userName.isEmpty())
      return QStringLiteral("Shared directory");

   // Windows reserves device names even when followed by a file extension.
   static const QRegularExpression reserved(
      QStringLiteral("^(CON|PRN|AUX|NUL|CONIN\\$|CONOUT\\$|COM[1-9\u00b9\u00b2\u00b3]|LPT[1-9\u00b9\u00b2\u00b3])$"),
      QRegularExpression::CaseInsensitiveOption);
   if (reserved.match(userName.section('.', 0, 0).trimmed()).hasMatch())
      userName.prepend('_');
   return userName;
}

#ifdef DEBUG
   QString Utils::getStatusStr(Protos::Common::DownloadStatus status)
   {
      switch (status)
      {
      case Protos::Common::DownloadStatus::QUEUED: return "QUEUED";
      case Protos::Common::DownloadStatus::GETTING_THE_HASHES: return "GETTING_THE_HASHES";
      case Protos::Common::DownloadStatus::DOWNLOADING: return "DOWNLOADING";
      case Protos::Common::DownloadStatus::COMPLETE: return "COMPLETE";
      case Protos::Common::DownloadStatus::PAUSED: return "PAUSED";
      case Protos::Common::DownloadStatus::DELETED: return "DELETED";

      case Protos::Common::DownloadStatus::UNKNOWN_PEER_SOURCE: return "UNKNOWN_PEER_SOURCE";
      case Protos::Common::DownloadStatus::ENTRY_NOT_FOUND: return "ENTRY_NOT_FOUND";
      case Protos::Common::DownloadStatus::NO_SOURCE: return "NO_SOURCE";

      case Protos::Common::DownloadStatus::NO_SHARED_DIRECTORY_TO_WRITE: return "NO_SHARED_DIRECTORY_TO_WRITE";
      case Protos::Common::DownloadStatus::NO_ENOUGH_FREE_SPACE: return "NO_ENOUGH_FREE_SPACE";
      case Protos::Common::DownloadStatus::UNABLE_TO_CREATE_THE_FILE: return "UNABLE_TO_CREATE_THE_FILE";
      case Protos::Common::DownloadStatus::UNABLE_TO_CREATE_THE_DIRECTORY: return "UNABLE_TO_CREATE_THE_DIRECTORY";
      case Protos::Common::DownloadStatus::UNABLE_TO_RETRIEVE_THE_HASHES: return "UNABLE_TO_RETRIEVE_THE_HASHES";

      case Protos::Common::DownloadStatus::TRANSFER_ERROR: return "TRANSFER_ERROR";
      case Protos::Common::DownloadStatus::UNABLE_TO_OPEN_THE_FILE: return "UNABLE_TO_OPEN_THE_FILE";
      case Protos::Common::DownloadStatus::FILE_IO_ERROR: return "FILE_IO_ERROR";
      case Protos::Common::DownloadStatus::FILE_NON_EXISTENT: return "FILE_NON_EXISTENT";
      case Protos::Common::DownloadStatus::GOT_TOO_MUCH_DATA: return "GOT_TOO_MUCH_DATA";
      case Protos::Common::DownloadStatus::HASH_MISMATCH: return "HASH_MiSMATCH";

      case Protos::Common::DownloadStatus::REMOTE_SCANNING_IN_PROGRESS: return "REMOTE_SCANNING_IN_PROGRESS";
      case Protos::Common::DownloadStatus::LOCAL_SCANNING_IN_PROGRESS: return "LOCAL_SCANNING_IN_PROGRESS";
      case Protos::Common::DownloadStatus::UNABLE_TO_GET_ENTRIES: return "UNABLE_TO_GET_ENTRIES";

      default: return "<UNKOWN STATUS>";
      }
      return QString();
   }
#endif
