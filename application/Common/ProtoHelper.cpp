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

QString ProtoHelper::getRelativePath(const Protos::Common::Entry& entry, bool appendFilename)
{
   QString path = Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path);

   // Empty relative path means the directory is a shared directory (root), see "application/Protos/common.proto" for more information.
   if (path.isEmpty())
      path = "/";
   else if (appendFilename || entry.type() == Protos::Common::Entry_Type_DIR)
      path.append(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name));
   return path;
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
      hashHex.reserve(40);
      while (str[pos] != '"')
      {
         if (str[pos] != '\\')
         {
            hashHex.append(QString::number(str[pos].toAscii(), 16));
            pos++;
         }
         else
         {
            switch (str[pos+1].toAscii())
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
               hashHex.append(QString::number(str[pos+1].toAscii(), 16));
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
  }

   return str;
}
