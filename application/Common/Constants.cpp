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
  
#include <Common/Constants.h>
using namespace Common;

#ifdef Q_OS_WIN32
   const QString Constants::APPLICATION_FOLDER_NAME("D-LAN");
#else
   const QString Constants::APPLICATION_FOLDER_NAME(".d-lan");
#endif

// Some files are saved as text format in debug and as binary in release.
#ifdef DEBUG
   const QString Constants::FILE_EXTENSION("txt");
#else
   const QString Constants::FILE_EXTENSION("bin");
#endif

const QString Constants::FILE_CACHE("cache." + FILE_EXTENSION); ///< The name of the file cache saved in the local data directory.
const QString Constants::FILE_QUEUE("queue." + FILE_EXTENSION); ///< This file contains the current downloads.

const QString Constants::CORE_SETTINGS_FILENAME("core_settings.txt");
const QString Constants::GUI_SETTINGS_FILENAME("gui_settings.txt");

const QString Constants::LANGUAGE_DIRECTORY("languages");

const QString Constants::STYLE_DIRECTORY("styles");
const QString Constants::STYLE_FILE_NAME("style.css");

const QString Constants::SERVICE_NAME("D-LAN Core");

const int Constants::PROTOBUF_STREAMING_BUFFER_SIZE(4 * 1024); ///< 4kB.

const QString Constants::BINARY_PREFIXS[] = {"B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB"};
