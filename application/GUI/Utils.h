#ifndef GUI_UTILS_H
#define GUI_UTILS_H

#include <QSharedPointer>
#include <QStringList>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class Utils
   {
   public:
      static QStringList askForDirectories(QSharedPointer<RCC::ICoreConnection> coreConnection, const QString& message = QString());
      static QStringList askForDirectoriesToDownloadTo(QSharedPointer<RCC::ICoreConnection> coreConnection);
   };
}

#endif
