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

#include <QHash>
#include <QString>
#include <QLocale>
#include <QHostAddress>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Common/Path.h>

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

      /*
      Obsolete, see the commented implementation below.
      template <typename T>
      static void setStr(T& mess, void (T::*setter)(const char*), const QString& str);

      template <typename T>
      static QString getStr(const T& mess, const std::string& (T::*getter)() const);

      template <typename T>
      static void addRepeatedStr(T& mess, void (T::*adder)(const char*), const QString& str);

      template <typename T>
      static QString getRepeatedStr(const T& mess, const std::string& (T::*getter)(int) const, int i);
      */

      static void setLang(Protos::Common::Language& langMess, const QLocale& locale);
      static QLocale getLang(const Protos::Common::Language& langMess);

      static void setIP(Protos::Common::IP& ipMess, const QHostAddress& address);
      static QHostAddress getIP(const Protos::Common::IP& ipMess);

      /**
        * Returns the entry name or the shared entry name is entry name is empty.
        */
      static QString getName(const Protos::Common::Entry& entry);

      /**
        * Return the path of an entry, for exemple:
        *  - entry is a root: "entry" (with 'absolutePath == false')
        *  - entry is a root: "/root_dir/entry" ('absolutePath == true')
        */
      static Path getPath(const Protos::Common::Entry& entry, bool absolutePath = false);

      static bool isRoot(const Protos::Common::Entry& entry);

      static QString getDebugStr(const google::protobuf::Message& mess);

      template<typename T>
      static T readUInt(const quint8*& p);

      static QString readString(const quint8*& p);

   private:
      static void readUInt(const quint8*& p, quint32 res, quint32& result);
      static void readUInt(const quint8*& p, quint32 res, quint64& result);
   };
}

namespace Protos::Common
{
   /**
     * Two entries are considered equal if they denote the same file or directory.
     * The chunks and the volatile flags ('exists', 'is_empty', ...) are not taken into account.
     */
   inline bool operator==(const Entry& e1, const Entry& e2)
   {
      return
         e1.type() == e2.type() &&
         e1.name() == e2.name() &&
         e1.path() == e2.path() &&
         e1.size() == e2.size() &&
         e1.shared_entry().id().hash() == e2.shared_entry().id().hash();
   }

   /**
     * Must be defined in the namespace of 'Entry' to be found by ADL from 'QHash'/'QSet'.
     * Must hash exactly the same fields as 'operator=='.
     */
   inline size_t qHash(const Entry& entry, size_t seed = 0)
   {
      const std::string& name = entry.name();
      const std::string& path = entry.path();
      const std::string& sharedEntryID = entry.shared_entry().id().hash();

      return qHashMulti(
         seed,
         static_cast<int>(entry.type()),
         QByteArrayView(name.data(), static_cast<qsizetype>(name.size())),
         QByteArrayView(path.data(), static_cast<qsizetype>(path.size())),
         static_cast<quint64>(entry.size()),
         QByteArrayView(sharedEntryID.data(), static_cast<qsizetype>(sharedEntryID.size()))
      );
   }
}

/* Obsolete, we just use 'QString::toStdString' and 'QString::fromStd
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
*/

template<typename T>
T Common::ProtoHelper::readUInt(const quint8*& p)
{
   p += 1; // Skip the first byte (type + field n°)

   quint32 res = p[0];
   if (!(res & 0x80))
   {
      p += 1;
      return res;
   }

   quint32 byte = p[1];
   res += (byte - 1) << 7;
   if (!(byte & 0x80))
   {
      p += 2;
      return res;
   }

   T result;
   readUInt(p, res, result);
   return result;
}
