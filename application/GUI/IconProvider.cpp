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

#include <QPainter>

#include <Common/ProtoHelper.h>

#include <Log.h>

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

QIcon IconProvider::getIcon(const Protos::Common::Entry& entry, bool withWarning)
{
   if (entry.type() == Protos::Common::Entry_Type_DIR)
   {
      if (withWarning)
      {
         if (IconProvider::folderIconWithWarning.isNull())
            IconProvider::folderIconWithWarning =
               IconProvider::drawWarning(IconProvider::iconProvider.icon(QFileIconProvider::Folder));
         return IconProvider::folderIconWithWarning;
      }
      else
         return IconProvider::iconProvider.icon(QFileIconProvider::Folder);
   }
   else
   {
      const QString& name = QString::fromStdString(entry.name());
      return IconProvider::getIconCache(name, withWarning);
   }
}

QIcon IconProvider::getIcon(const Common::Path& path)
{
   if (path.isFile())
      return IconProvider::getIconCache(path.getFilename(), false);
   else
      return IconProvider::iconProvider.icon(QFileIconProvider::Folder);
}

QIcon IconProvider::getDirectoryIcon()
{
   return IconProvider::iconProvider.icon(QFileIconProvider::Folder);
}

QIcon IconProvider::getIconCache(const QString& filename, bool withWarning)
{
   const int index = filename.lastIndexOf(".");
   if (index != -1)
   {
      return IconProvider::getIconCacheByExtension(filename.mid(index), withWarning);
   }
   else
   {
      if (withWarning)
      {
         if (IconProvider::fileIconWithWarning.isNull())
            IconProvider::fileIconWithWarning =
               IconProvider::drawWarning(IconProvider::iconProvider.icon(QFileIconProvider::File));
         return IconProvider::fileIconWithWarning;
      }
      else
         return IconProvider::iconProvider.icon(QFileIconProvider::File);
   }
}

QIcon IconProvider::getIconCacheByExtension(const QString& extension, bool withWarning)
{
   QMap<QString, QIcon>& cache = withWarning ? IconProvider::cachedIconsWithWarning : IconProvider::cachedIcons;

   // 'getIconNative(..)' may legitimately return a null icon and the
   // result has to be recognised as cached, otherwise the native lookup is redone at each call.
   const auto i = cache.constFind(extension);
   if (i != cache.constEnd())
      return *i;

   const QIcon icon =
      withWarning ?
           IconProvider::drawWarning(IconProvider::getIconNative(extension))
         : IconProvider::getIconNative(extension);

   cache.insert(extension, icon);
   return icon;
}

/**
  * No specific implementation for Linux.
  */
QIcon IconProvider::getIconNative(const QString& extension)
{
   QIcon icon;
#if defined(Q_OS_WIN32)
   SHFILEINFO psfi;
   SHGetFileInfo(
      extension.toStdWString().c_str(),
      FILE_ATTRIBUTE_NORMAL,
      &psfi,
      sizeof(psfi),
      SHGFI_ICON | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES
   );
   if (psfi.hIcon != NULL)
   {
      icon = QIcon(QPixmap::fromImage(QImage::fromHICON(psfi.hIcon)));
      DestroyIcon(psfi.hIcon);
   }
#else
   icon = IconProvider::iconProvider.icon(QFileIconProvider::File);
#endif
   return icon;
}

QIcon IconProvider::drawWarning(const QIcon& icon)
{
   QPixmap miniError(":/icons/resources/error.svg");
   QIcon result;
   foreach (auto size, icon.availableSizes())
   {
      QPixmap pixmap = icon.pixmap(size);
      if (pixmap.width() >= miniError.width() && pixmap.height() >= miniError.height() + 1)
      {
         QPainter painter(&pixmap);
         painter.drawPixmap(
            pixmap.width() - miniError.width(),
            pixmap.height() - miniError.height() - 1,
            miniError.width(),
            miniError.height(),
            miniError
         );
      }
      result.addPixmap(pixmap);
   }
   return result;
}

QFileIconProvider IconProvider::iconProvider;
QMap<QString, QIcon> IconProvider::cachedIcons;
QMap<QString, QIcon> IconProvider::cachedIconsWithWarning;
QIcon IconProvider::fileIconWithWarning;
QIcon IconProvider::folderIconWithWarning;
