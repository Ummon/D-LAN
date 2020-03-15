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
  
#pragma once

#include <QString>
#include <QLocale>
#include <QHostAddress>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>

namespace Common
{
   enum class EntriesToAppend
   {
      NONE = 0,
      FILE = 1,
      DIR = 2,
   };

   constexpr EntriesToAppend operator|(EntriesToAppend e1, EntriesToAppend e2) { return static_cast<EntriesToAppend>(static_cast<int>(e1) | static_cast<int>(e2)); }
   constexpr EntriesToAppend operator&(EntriesToAppend e1, EntriesToAppend e2) { return static_cast<EntriesToAppend>(static_cast<int>(e1) & static_cast<int>(e2)); }
   constexpr bool contains(EntriesToAppend e1, EntriesToAppend e2) { return (e1 & e2) == e2; }

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
        *  - entry is a root: "/" (with 'prependSharedDir == false')
        *  - entry is a root: "/root_dir/" ('prependSharedDir == true')
        *  - entry is a directory: "/abc/xyz/" (with 'entriesToAppend == DIR' and 'prependSharedDir == false').
        *  - entry is a file: "/abc/xyz/file.txt" (with 'entriesToAppend == DIR' and 'prependSharedDir == false').
        *  - entry is a file: "/root_dir/abc/xyz/file.txt" (with 'entriesToAppend == DIR' and 'prependSharedDir == true').
        *  - entry is a file: "/abc/xyz/" (with 'entriesToAppend != FILE' and 'prependSharedDir == false').
        */
      static QString getRelativePath(const Protos::Common::Entry& entry, EntriesToAppend entriesToAppend = EntriesToAppend::FILE | EntriesToAppend::DIR, bool prependSharedDir = false);

      static bool isRoot(const Protos::Common::Entry& entry);

      static QString getDebugStr(const google::protobuf::Message& mess);
   };
}

template <typename T>
void Common::ProtoHelper::setStr(T& mess, void (T::*setter)(const char*), const QString& str)
{
   const QByteArray& array = str.toUtf8();
   (mess.*setter)(array.constData());
}

template <typename T>
QString Common::ProtoHelper::getStr(const T& mess, const std::string& (T::*getter)() const)
{
   const std::string& str = (mess.*getter)();
   return QString::fromUtf8(str.data(), str.length());
}

template <typename T>
void Common::ProtoHelper::addRepeatedStr(T& mess, void (T::*adder)(const char*), const QString& str)
{
   const QByteArray& array = str.toUtf8();
   (mess.*adder)(array.constData());
}

template <typename T>
QString Common::ProtoHelper::getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i)
{
   const std::string& str = (mess.*getter)(i);
   return QString::fromUtf8(str.data(), str.length());
}
