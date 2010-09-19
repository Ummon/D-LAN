#ifndef COMMON_CONSTANTS_H
#define COMMON_CONSTANTS_H

#include <QDir>

namespace Common
{
   const QString APPLICATION_FOLDER_NAME(".aybabtu");
   const QString APPLICATION_FOLDER_PATH(QDir::homePath() + '/' + APPLICATION_FOLDER_NAME);
   const QString LOG_FOLDER_NAME("log");
   const QString FILE_CACHE("cache.bin"); ///< The name of the file cache saved in the home directory.

   const int BASE_PORT = 59487;
}

#endif
