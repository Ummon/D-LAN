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
      case UNABLE_TO_RETRIEVE_THE_HASHES: return "UNABLE_TO_RETRIEVE_THE_HASHES";
      case TRANSFERT_ERROR: return "TRANSFERT_ERROR";
      case UNABLE_TO_OPEN_THE_FILE: return "UNABLE_TO_OPEN_THE_FILE";
      case FILE_IO_ERROR: return "FILE_IO_ERROR";
      case FILE_NON_EXISTENT: return "FILE_NON_EXISTENT";
      case GOT_TOO_MUCH_DATA: return "GOT_TOO_MUCH_DATA";
      case HASH_MISSMATCH: return "HASH_MISSMATCH";
      }
      return QString();
   }
#endif
