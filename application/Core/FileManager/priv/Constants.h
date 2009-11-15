#ifndef FILEMANAGER_CONSTANTS_H
#define FILEMANAGER_CONSTANTS_H

#include <QString>

namespace FM
{
   const int BUFFER_SIZE = 524288; ///< (512kB) Buffer used when reading a file (hashing).
   const int CHUNK_SIZE = 33554432; ///< (32 MB).

   const int  MAX_WORD_LENGTH = 3; ///< Using when filtering the search pattern, words smallers than this value will be dropped.
   const QString FILE_CACHE("file_cache.bin"); ///< The name of the file cache saved in the home directory.

   const int MINIMUM_DURATION_WHEN_HASHING = 30; ///< In seconds.

   const int TIME_BETWEEN_RESCAN = 30; ///< Only for unwatchable directories. In seconds
}

#endif
