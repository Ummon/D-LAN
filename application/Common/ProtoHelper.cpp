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

#include <ProtoHelper.h>
using namespace Common;

#include <QRegExp>
#include <QStringList>

#include <Hash.h>
#include <Global.h>

void ProtoHelper::setLang(Protos::Common::Language& langMess, const QLocale& locale)
{
   const QStringList& langCountry = locale.name().split('_');
   if (langCountry.length() == 2)
   {
      ProtoHelper::setStr(langMess, &Protos::Common::Language::set_lang, langCountry[0]);
      ProtoHelper::setStr(langMess, &Protos::Common::Language::set_country, langCountry[1]);
   }
   else
   {
      ProtoHelper::setStr(langMess, &Protos::Common::Language::set_lang, "en");
      ProtoHelper::setStr(langMess, &Protos::Common::Language::set_country, "US");
   }
}

QLocale ProtoHelper::getLang(const Protos::Common::Language& langMess)
{
   QString langStr = ProtoHelper::getStr(langMess, &Protos::Common::Language::lang);
   if (ProtoHelper::getStr(langMess, &Protos::Common::Language::country).isEmpty())
      langStr.append("_").append(ProtoHelper::getStr(langMess, &Protos::Common::Language::country));

   return QLocale(langStr);
}

void ProtoHelper::setIP(Protos::Common::IP& ipMess, const QHostAddress& address)
{
   switch (address.protocol())
   {
   case QAbstractSocket::IPv4Protocol:
      {
         quint32 ipInt = address.toIPv4Address();
         char ip[4];
         ip[0] = (ipInt & 0xFF000000) >> 24;
         ip[1] = (ipInt & 0x00FF0000) >> 16;
         ip[2] = (ipInt & 0x0000FF00) >> 8;
         ip[3] = ipInt & 0x000000FF;

         ipMess.set_type(Protos::Common::IP::IPv4);
         ipMess.set_ip(ip, sizeof(ip));
      }
      break;
   case QAbstractSocket::IPv6Protocol:
      {
         // We can use 'reinterpret_cast<>()' on the Qt type but I guess it's not a good idea.
         Q_IPV6ADDR qipv6addr = address.toIPv6Address();
         char ip[16];
         for (int i = 0; i < 16; i++)
            ip[i] = qipv6addr[i];

         ipMess.set_type(Protos::Common::IP::IPv6);
         ipMess.set_ip(ip, sizeof(ip));
      }
      break;
   default:;
   }
}

QHostAddress ProtoHelper::getIP(const Protos::Common::IP& ipMess)
{
   switch (ipMess.type())
   {
   case Protos::Common::IP::IPv4:
      {
         const char* ip = ipMess.ip().data();
         quint32 ipInt =
            static_cast<quint32>(ip[0]) << 24 & 0xFF000000 |
            static_cast<quint32>(ip[1]) << 16 & 0x00FF0000 |
            static_cast<quint32>(ip[2]) << 8 & 0x0000FF00 |
            static_cast<quint32>(ip[3]) & 0x000000FF;
         return QHostAddress(ipInt);
      }
      break;
   case Protos::Common::IP::IPv6:
      {
         const char* ip = ipMess.ip().data();
         Q_IPV6ADDR qipv6addr;
         for (int i = 0; i < 16; i++)
            qipv6addr[i] = ip[i];
         return QHostAddress(qipv6addr);
      }
      break;
   default:;
      return QHostAddress();
   }
}

QString ProtoHelper::getRelativePath(const Protos::Common::Entry& entry, EntriesToAppend entriesToAppend, bool prependSharedDir)
{
   QString path = Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path);
   QString sharedName = Common::ProtoHelper::getStr(entry.shared_dir(), &Protos::Common::SharedDir::shared_name);
   if (!Common::Global::isWindowsPath(sharedName))
      sharedName.prepend("/");

   // Empty relative path means the directory is a shared directory (root), see "application/Protos/common.proto" for more information.
   if (path.isEmpty())
   {
      if (prependSharedDir)
         path.append(sharedName).append("/");
      else
         path = "/";
   }
   else
   {
      if (prependSharedDir)
         path.prepend(sharedName);

      if (contains(entriesToAppend, EntriesToAppend::FILE) && entry.type() == Protos::Common::Entry_Type_FILE || contains(entriesToAppend, EntriesToAppend::DIR) && entry.type() == Protos::Common::Entry_Type_DIR)
      {
         path.append(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name));
         if (entry.type() == Protos::Common::Entry_Type_DIR)
            path.append("/");
      }
   }

   return path;
}

bool ProtoHelper::isRoot(const Protos::Common::Entry& entry)
{
   return entry.path().empty();
}

QString ProtoHelper::getDebugStr(const google::protobuf::Message& mess)
{
   std::string debugString = mess.DebugString();
   QString str = QString::fromUtf8(debugString.data());

   // Very dirty : substitute the bytes representation (ascii + escaped octal number) with a hexadecimal representation.
   // hash: "ID\214\351\t\003\312w\213u\320\236@0o\032\220\"(\033"

   const QString prefix("hash: \"");

   int pos = 0;
   while ((pos = str.indexOf(prefix, pos)) != -1)
   {
      pos += prefix.size();
      const int initialPos = pos;
      QString hashHex;
      hashHex.reserve(2 * Hash::HASH_SIZE);
      while (str[pos] != '"')
      {
         if (str[pos] != '\\')
         {
            hashHex.append(QString::number(str[pos].toLatin1(), 16));
            pos++;
         }
         else
         {
            switch (str[pos+1].toLatin1())
            {
            case 'r':
               hashHex.append("0d");
               pos += 2;
               break;
            case 'n':
               hashHex.append("0a");
               pos += 2;
               break;
            case 't':
               hashHex.append("09");
               pos += 2;
               break;
            case '"':
            case '\'':
            case '\\':
               hashHex.append(QString::number(str[pos+1].toLatin1(), 16));
               pos += 2;
               break;
            default: // It's an octal number, for example : "\123"
               bool ok;
               hashHex.append(QString("%1").arg(str.mid(pos+1, 3).toInt(&ok, 8), 2, 16, QLatin1Char('0')));
               pos += 4;
            }
         }
      }
      /* Used during debugging :
      const int length = hashHex.size();
      const QString hash = str.mid(initialPos, pos - initialPos);*/

      str.replace(initialPos, pos - initialPos, hashHex);
      pos = initialPos + 2 * Hash::HASH_SIZE;
  }

   return str;
}

void ProtoHelper::readUInt(const quint8*& p, quint32 res, quint32& result)
{
   for (quint32 i = 2; i < 5; i++)
   {
      quint32 byte = static_cast<quint8>(p[i]);
      res += (byte - 1) << (7 * i);
      if (byte < 128)
      {
         p += (size_t)i + 1;
         result = res;
         return;
      }
   }

   for (quint32 i = 5; i < 10; i++)
   {
      quint32 byte = static_cast<quint8>(p[i]);
      if (byte < 128)
      {
         p += (size_t)i + 1;
         result = res;
         return;
      }
   }

   result = res;
}

void ProtoHelper::readUInt(const quint8*& p, quint32 res32, quint64& result)
{
   quint64 res = res32;
   for (quint32 i = 2; i < 10; i++)
   {
      quint64 byte = static_cast<quint8>(p[i]);
      res += (byte - 1) << (7 * i);
      if (byte < 128)
      {
         p += (size_t)i + 1;
         result = res;
         return;
      }
   }

   result = 0;
}

QString ProtoHelper::readString(const quint8*& p)
{
   quint32 length = ProtoHelper::readUInt<quint32>(p);
   QString str = QString::fromUtf8(reinterpret_cast<const char*>(p), length);
   p += length;
   return str;
}
