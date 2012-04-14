/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#ifndef COMMON_PROTOHELPER_H
#define COMMON_PROTOHELPER_H

#include <QString>
#include <QLocale>
#include <QHostAddress>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>

namespace Common
{
   /**
     * The ugliest class ever!
     * Has some methods to read and write string field from Protocol Buffer objects.
     */
   class ProtoHelper
   {
   public:
      template <typename T>
      static void setStr(T& mess, void (T::*setter)(const char*), const QString& str);

      template <typename T>
      static QString getStr(const T& mess, const std::string& (T::*getter)() const);

      template <typename T>
      static void addRepeatedStr(T& mess, void (T::*adder)(const char*), const QString& str);

      template <typename T>
      static QString getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i);

      static void setLang(Protos::Common::Language& langMess, const QLocale& locale);
      static QLocale getLang(const Protos::Common::Language& langMess);

      static void setIP(Protos::Common::IP& ipMess, const QHostAddress& address);
      static QHostAddress getIP(const Protos::Common::IP& ipMess);

      /**
        * Return the relative path of an entry, for exemple:
        *  - entry is a root: "/".
        *  - entry is a directory: "/abc/xyz/".
        *  - entry is a file: "/abc/xyz/file.txt" (with 'appendFilename' == true).
        *  - entry is a file: "/abc/xyz/" (with 'appendFilename' == false).
        */
      static QString getRelativePath(const Protos::Common::Entry& entry, bool appendFilename = true);

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
void ProtoHelper::addRepeatedStr(T& mess, void (T::*adder)(const char*), const QString& str)
{
   QByteArray array = str.toUtf8();
   (mess.*adder)(array.data());
}

template <typename T>
QString ProtoHelper::getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i)
{
   return QString::fromUtf8((mess.*getter)(i).data());
}

#endif
