#ifndef FILEMANAGER_CONSTANTS_H
#define FILEMANAGER_CONSTANTS_H

#include <QString>

namespace FM
{
   const int CHUNK_SIZE = 33554432; ///< (32 MB).
   const int  MAX_WORD_LENGTH = 3; ///< Using when filtering the search pattern, words smallers than this value will be dropped.
}

#endif
