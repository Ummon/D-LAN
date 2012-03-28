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
  
#ifndef COMMON_PERSISTANTDATA_H
#define COMMON_PERSISTANTDATA_H

#include <google/protobuf/message.h>

#include <QString>
#include <QByteArray>

#include <Common/Global.h>

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
      static void setValue(const QString& name, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
      static void setValue(const QString& directory, const QString& name, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
      static void getValue(const QString& name, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
      static void getValue(const QString& directory, const QString& name, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
      static bool rmValue(const QString& name, Global::DataFolderType dataFolderType);

   private:
      static void setValueFilepath(const QString& filepath, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
      static void getValueFilepath(const QString& filepath, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable = false);
   };
}
#endif
