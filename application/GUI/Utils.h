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
  
#ifndef GUI_UTILS_H
#define GUI_UTILS_H

#include <QSharedPointer>
#include <QStringList>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class Utils
   {
   public:
      static QStringList askForDirectories(QSharedPointer<RCC::ICoreConnection> coreConnection, const QString& message = QString());
      static QStringList askForDirectoriesToDownloadTo(QSharedPointer<RCC::ICoreConnection> coreConnection);

      static QString emoticonsDirectoryPath();

      static void openLocations(const QStringList& paths);
      static void openLocation(const QString& path);
      static void openFile(const QString& path);
   };
}

#endif
