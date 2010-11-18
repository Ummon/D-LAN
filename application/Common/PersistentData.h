#ifndef COMMON_PERSISTANTDATA_H
#define COMMON_PERSISTANTDATA_H

#include <google/protobuf/message.h>

#include <QString>
#include <QByteArray>

namespace Common
{
   class UnknownValueException {};
   class PersistentDataIOException
   {
   public:
      PersistentDataIOException(const QString message) : message(message) {}
      const QString message;
   };

   class PersistentData
   {
      static const QString TEMP_SUFFIX_TERM;
   public:

      static void setValue(const QString& name, const google::protobuf::Message& data, bool humanReadable = false);
      static void getValue(const QString& name, google::protobuf::Message& data, bool humanReadable = false);
      static bool rmValue(const QString& name);
   };
}
#endif
