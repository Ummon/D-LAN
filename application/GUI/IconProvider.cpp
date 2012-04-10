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
  
#include <IconProvider.h>
using namespace GUI;

#include <Common/ProtoHelper.h>

#if defined(Q_OS_WIN32)
   #include <shlobj.h>
   #include <shellapi.h>
#elif defined(Q_OS_LINUX)
   // Nothing.
#else
   // Nothing.
#endif

/**
  * @class IconProvider
  *
  * @author Yann Diorcet
  * @author Greg Burri
  */

QIcon IconProvider::getIcon(const Protos::Common::Entry& entry)
{
   if (entry.type() == Protos::Common::Entry_Type_DIR)
   {
     return IconProvider::qtIconProvider.icon(QFileIconProvider::Folder);
   }
   else
   {
      QString name = Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name);
      const int index = name.lastIndexOf(".");
      if (index != -1)
         return IconProvider::getIconCache(name.mid(index));
      else
         return IconProvider::qtIconProvider.icon(QFileIconProvider::File);
   }
}

QIcon IconProvider::getIconCache(const QString& extension)
{
   QIcon icon = cachedIcons.value(extension);
   if (icon.isNull())
   {
      icon = IconProvider::getIconNative(extension);
      cachedIcons.insert(extension, icon);
   }
   return icon;
}

QIcon IconProvider::getIconNative(const QString& extension)
{
   QIcon icon;
#if defined(Q_OS_WIN32)
   SHFILEINFO psfi;
   SHGetFileInfo(extension.toStdWString().c_str(), FILE_ATTRIBUTE_NORMAL, &psfi, sizeof(psfi),
                 SHGFI_ICON | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES);
   if (psfi.hIcon != NULL)
   {
      icon = QIcon(QPixmap::fromWinHICON(psfi.hIcon));
   }
#else
   icon = IconProvider::qtIconProvider.icon(QFileIconProvider::File);
#endif
   return icon;
}

QFileIconProvider IconProvider::qtIconProvider;
QMap<QString, QIcon> IconProvider::cachedIcons;
