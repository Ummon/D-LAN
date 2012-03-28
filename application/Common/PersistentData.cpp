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
  
#include <Common/PersistentData.h>
using namespace Common;

#include <QFile>
#include <QDir>
#include <QtDebug>

#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>

#include <Constants.h>
#include <Global.h>

/**
  * @class Common::PersistentData
  *
  * Some little functions to persist data and retrieve it.
  * The data are persisted in the user directory (roaming or local, see Common::Global::getDataFolder(..) method for more information).
  * The data are described by a Protocol Buffer message.
  * Theses functions can be used for the application settings.
  */

const QString PersistentData::TEMP_SUFFIX_TERM(".temp");

/**
  * Define a value associated to a name.
  * You may refer to the name policy of the platform. Try to avoid special characters or space.
  * You can use an extension in the name like "settings.conf".
  * @exception PersistentDataIOException if the value can't be persisted.
  */
void PersistentData::setValue(const QString& name, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
{
   Q_ASSERT(!name.isEmpty());
   PersistentData::setValueFilepath(Global::getDataFolder(dataFolderType) + '/' + name, data, dataFolderType, humanReadable);
}

void PersistentData::setValue(const QString& directory, const QString& name, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
{
   Q_ASSERT(!name.isEmpty());
   PersistentData::setValueFilepath(directory + '/' + name, data, dataFolderType, humanReadable);
}

/**
  * Retrieve the data associated to a given name.
  * @exception PersistentDataIOException
  * @exception UnknownValueException Throwed if the value doesn't exist
  */
void PersistentData::getValue(const QString& name, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
{
   Q_ASSERT(!name.isEmpty());
   PersistentData::getValueFilepath(Global::getDataFolder(dataFolderType) + '/' + name, data, dataFolderType, humanReadable);
}

void PersistentData::getValue(const QString& name, const QString& directory, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
{
   Q_ASSERT(!name.isEmpty());
   PersistentData::getValueFilepath(directory + '/' + name, data, dataFolderType, humanReadable);
}

/**
  * Remove a data.
  * @exception PersistentDataIOException
  * @return Return false if the data didn't exist.
  */
bool PersistentData::rmValue(const QString& name, Global::DataFolderType dataFolderType)
try
{
   Q_ASSERT(!name.isEmpty());
   return QFile::remove(Global::getDataFolder(dataFolderType) + '/' + name);
}
catch(Global::UnableToGetFolder& e)
{
   throw PersistentDataIOException(e.errorMessage);
}


void PersistentData::setValueFilepath(const QString& filepath, const google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
try
{
   const QString TEMP_FILEPATH(filepath + TEMP_SUFFIX_TERM);

   // To avoid ::Print(..) to crash, see defect #153.
   if (Global::availableDiskSpace(Global::getDataFolder(dataFolderType)) < 20 * 1024 * 1024)
      return;

   {
      QFile file(TEMP_FILEPATH);
      if (!file.open(QIODevice::WriteOnly))
         throw PersistentDataIOException(QString("Unable to open the file in write mode : %1, error : %2").arg(TEMP_FILEPATH).arg(file.errorString()));

#if !DEBUG
      if (humanReadable)
      {
#endif
         google::protobuf::io::FileOutputStream fileStream(file.handle());
         google::protobuf::TextFormat::Printer printer;
         printer.SetUseShortRepeatedPrimitives(true);
         printer.Print(data, &fileStream);
#if !DEBUG
      }
      else
      {
         data.SerializeToFileDescriptor(file.handle());
      }
#endif
   }

   Global::rename(TEMP_FILEPATH, filepath);
}
catch(Global::UnableToGetFolder& e)
{
   throw PersistentDataIOException(e.errorMessage);
}

void PersistentData::getValueFilepath(const QString& filepath, google::protobuf::Message& data, Global::DataFolderType dataFolderType, bool humanReadable)
try
{
   QFile file(filepath);
   if (!file.open(QIODevice::ReadOnly))
      throw UnknownValueException();

#if !DEBUG
   if (humanReadable)
   {
#endif
      google::protobuf::io::FileInputStream fileStream(file.handle());
      google::protobuf::TextFormat::Parse(&fileStream, &data);
#if !DEBUG
   }
   else
   {
      data.ParsePartialFromFileDescriptor(file.handle());
   }
#endif
}
catch(Global::UnableToGetFolder& e)
{
   throw PersistentDataIOException(e.errorMessage);
}
