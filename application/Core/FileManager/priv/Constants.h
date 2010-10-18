#ifndef FILEMANAGER_CONSTANTS_H
#define FILEMANAGER_CONSTANTS_H

#include <QString>

namespace FM
{
   const int MAX_WORD_LENGTH = 3; ///< Using when filtering the search pattern, words smallers than this value will be dropped.
   const int FILE_CACHE_VERSION = 1;
}

#endif
