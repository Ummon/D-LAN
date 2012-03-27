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
  
#ifndef COMMON_CONSTANTS_H
#define COMMON_CONSTANTS_H

#include <QDir>

namespace Common
{
   class Constants
   {
   public:
      static const QString APPLICATION_FOLDER_NAME;

      static const QString FILE_EXTENSION;

      static const QString FILE_CACHE;
      static const QString FILE_QUEUE;

      static const QString CORE_SETTINGS_FILENAME;
      static const QString GUI_SETTINGS_FILENAME;

      static const QString LANGUAGE_DIRECTORY;

      static const QString SERVICE_NAME;

      static const int PROTOBUF_STREAMING_BUFFER_SIZE;

      static const QString BINARY_PREFIXS[];
   };
}

#endif
