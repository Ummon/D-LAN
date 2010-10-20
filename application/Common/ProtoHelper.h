#ifndef COMMON_PROTOHELPER_H
#define COMMON_PROTOHELPER_H

#include <QString>

#include <google/protobuf/message.h>

namespace Common
{
   /**
     * The ugliest class ever!
     */
   class ProtoHelper
   {
   public:
      template <typename T>
      static void setStr(T& mess, void (T::*setter)(const char*), const QString& str);

      template <typename T>
      static QString getStr(const T& mess, const std::string& (T::*getter)() const);

      template <typename T>
      static QString getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i);

      static QString getDebugStr(const google::protobuf::Message& mess);
   };
}

/***** Definitions *****/
using namespace Common;

template <typename T>
void ProtoHelper::setStr(T& mess, void (T::*setter)(const char*), const QString& str)
{
   QByteArray array = str.toUtf8();
   (mess.*setter)(array.data());
}

template <typename T>
QString ProtoHelper::getStr(const T& mess, const std::string& (T::*getter)() const)
{
   return QString::fromUtf8((mess.*getter)().data());
}

template <typename T>
QString ProtoHelper::getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i)
{
   return QString::fromUtf8((mess.*getter)(i).data());
}

#endif
