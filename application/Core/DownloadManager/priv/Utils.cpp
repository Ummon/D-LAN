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

#ifdef DEBUG
   QString Utils::getStatusStr(Status status)
   {
      switch (status)
      {
      case QUEUED: return "QUEUED";
      case GETTING_THE_HASHES: return "GETTING_THE_HASHES";
      case DOWNLOADING: return "DOWNLOADING";
      case COMPLETE: return "COMPLETE";
      case PAUSED: return "PAUSED";
      case DELETED: return "DELETED";

      case UNKNOWN_PEER_SOURCE: return "UNKNOWN_PEER_SOURCE";
      case ENTRY_NOT_FOUND: return "ENTRY_NOT_FOUND";
      case NO_SOURCE: return "NO_SOURCE";

      case NO_SHARED_DIRECTORY_TO_WRITE: return "NO_SHARED_DIRECTORY_TO_WRITE";
      case NO_ENOUGH_FREE_SPACE: return "NO_ENOUGH_FREE_SPACE";
      case UNABLE_TO_CREATE_THE_FILE: return "UNABLE_TO_CREATE_THE_FILE";
      case UNABLE_TO_CREATE_THE_DIRECTORY: return "UNABLE_TO_CREATE_THE_DIRECTORY";
      case UNABLE_TO_RETRIEVE_THE_HASHES: return "UNABLE_TO_RETRIEVE_THE_HASHES";

      case TRANSFER_ERROR: return "TRANSFER_ERROR";
      case UNABLE_TO_OPEN_THE_FILE: return "UNABLE_TO_OPEN_THE_FILE";
      case FILE_IO_ERROR: return "FILE_IO_ERROR";
      case FILE_NON_EXISTENT: return "FILE_NON_EXISTENT";
      case GOT_TOO_MUCH_DATA: return "GOT_TOO_MUCH_DATA";
      case HASH_MISMATCH: return "HASH_MiSMATCH";

      case REMOTE_SCANNING_IN_PROGRESS: return "REMOTE_SCANNING_IN_PROGRESS";
      case LOCAL_SCANNING_IN_PROGRESS: return "LOCAL_SCANNING_IN_PROGRESS";
      case UNABLE_TO_GET_ENTRIES: return "UNABLE_TO_GET_ENTRIES";
      }
      return QString();
   }
#endif
