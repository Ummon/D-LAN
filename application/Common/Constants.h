#ifndef COMMON_CONSTANTS_H
#define COMMON_CONSTANTS_H

#include <QDir>

namespace Common
{
   const QString APPLICATION_FOLDER_NAME(".aybabtu");
   const QString APPLICATION_FOLDER_PATH(QDir::homePath() + '/' + APPLICATION_FOLDER_NAME);
}

#endif
