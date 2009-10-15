#ifndef COMMON_PERSISTANTDATA_H
#define COMMON_PERSISTANTDATA_H

#include <QString>
#include <QByteArray>

#include "Common_global.h"

namespace Common
{
   class COMMON_EXPORT PersistantData
   {
   public:
      static void setValue(const QString& name, const QByteArray& data);
      static QByteArray getValue(const QString& name);

   private:
      static bool createApplicationFolder();

      static const QString applicationFolderName;
      static const QString applicationFolderPath;
      static const QString tempPostfixTerm;
   };
}
#endif
