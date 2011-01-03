/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
   const QString LOG_FOLDER_NAME("log");

// Some files are saved as text format in debug and as binary in release.
#ifdef DEBUG
   const QString FILE_EXTENSION("txt");
#else
   const QString FILE_EXTENSION("bin");
#endif

   const QString FILE_CACHE("cache." + FILE_EXTENSION); ///< The name of the file cache saved in the local data directory.
   const QString FILE_QUEUE("queue." + FILE_EXTENSION); ///< This file contains the current downloads.

   const QString CORE_SETTINGS_FILENAME("core_settings.txt");
   const QString GUI_SETTINGS_FILENAME("gui_settings.txt");

   const QString SERVICE_NAME("AybabtuCore");

   const int PROTOBUF_STREAMING_BUFFER_SIZE(4 * 1024); ///< 4kB.

   const QString BINARY_PREFIXS[] = {"B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB"};
}

#endif
