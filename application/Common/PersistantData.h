#ifndef COMMON_PERSISTANTDATA_H
#define COMMON_PERSISTANTDATA_H

#include <exception>
using namespace std;

#include <QString>
#include <QByteArray>

#include "Common_global.h"

namespace Common
{
   class UnknownValueException : public exception {};

   /**
     * Some little functions to persist data and retrieve it.
     * The data are persisted in the user directory.
     * Theses functions can be used for the application settings.
     */
   class COMMON_EXPORT PersistantData
   {
   public:

      /**
        * Define a value associated to a name.
        * You may refer to the name policy of the platform. Try to avoir special characters or space.
        * You can use an extension in the name like "settings.conf".
        * @param name The name of the data
        * @param data The data to persist
        */
      static void setValue(const QString& name, const QByteArray& data);

      /**
        * Retrieve the data associated to a given name.
        * @param name The name of the data
        * @return the associated value
        * @exception UnknownValueException Throwed if the value doesn't exist
        */
      static QByteArray getValue(const QString& name);

      /**
        * Remove a data.
        * @return Return false if the data didn't exist.
        */
      static bool rmValue(const QString& name);

   private:
      static bool createApplicationFolder();

      static const QString applicationFolderName;
      static const QString applicationFolderPath;
      static const QString tempPostfixTerm;
   };
}
#endif
