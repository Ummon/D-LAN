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

#include <QIcon>
#include <QMap>
#include <QFileIconProvider>
#include <Protos/common.pb.h>

namespace GUI
{
   class IconProvider
   {
   public:
      /**
        * Returns an icon associated to a provided entry. The icon may depends of the entry type (file or directory) and of the file extension.
        * @param entry The entry
        * @param withWarning If 'true' a little warning image is added to the returned icon
        * @return The corresponding icon
        */
      static QIcon getIcon(const Protos::Common::Entry& entry, bool withWarning = false);

   private:
      static QIcon getIconCache(const QString& icon, bool withWarning);
      static QIcon getIconNative(const QString& icon);

      static QIcon drawWarning(const QIcon& icon);

      static QFileIconProvider iconProvider;      
      static QMap<QString, QIcon> cachedIcons;
      static QMap<QString, QIcon> cachedIconsWithWarning;
      static QIcon fileIconWithWarning;
      static QIcon folderIconWithWarning;
   };
}
