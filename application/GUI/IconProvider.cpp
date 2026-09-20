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

#if defined(Q_OS_WIN32)
   #include <shlobj.h>
   #include <shellapi.h>
#elif defined(Q_OS_LINUX)
   #include <QMimeDatabase>
   #include <QMimeType>
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
      const QString& name = Common::ProtoHelper::getName(entry);
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
#ifdef Q_OS_LINUX
   // Use the whole filename: MIME globs also recognise Makefile and *.tar.gz.
   // MatchExtension works for remote files without reading their contents.
   const QMimeType mime = QMimeDatabase().mimeTypeForFile(filename, QMimeDatabase::MatchExtension);
   return IconProvider::getIconCacheByType(mime.name(), withWarning);
#else
   const int index = filename.lastIndexOf(".");
   if (index != -1)
   {
      return IconProvider::getIconCacheByType(filename.mid(index), withWarning);
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
#endif
}

QIcon IconProvider::getIconCacheByType(const QString& type, bool withWarning)
{
   auto& cache = withWarning ? IconProvider::cachedIconsWithWarning : IconProvider::cachedIcons;
#if defined(Q_OS_WIN32) || defined(Q_OS_MACOS)
   const QString key = type.toLower(); // File-type associations are case-insensitive.
#else
   const QString& key = type; // Linux keys are MIME types, not filename extensions.
#endif

   // 'getIconNative(..)' may legitimately return a null icon and the
   // result has to be recognised as cached, otherwise the native lookup is redone at each call.
   if (const auto* icon = cache.object(key))
      return *icon;

   const QIcon icon =
      withWarning ?
           IconProvider::drawWarning(IconProvider::getIconNative(key))
         : IconProvider::getIconNative(key);

   cache.insert(key, new QIcon(icon));
   return icon;
}

/**
  * 'type' is a MIME type on Linux and a file extension on Windows/macOS.
  * The macOS implementation is in IconProviderMac.mm.
  */
#ifndef Q_OS_MACOS
QIcon IconProvider::getIconNative(const QString& type)
{
   QIcon icon;
#if defined(Q_OS_WIN32)
   SHFILEINFO psfi;
   SHGetFileInfo(
      type.toStdWString().c_str(),
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
#elif defined(Q_OS_LINUX)
   const QMimeType mime = QMimeDatabase().mimeTypeForName(type);
   icon = QIcon::fromTheme(mime.iconName());
   if (icon.isNull())
      icon = QIcon::fromTheme(mime.genericIconName());
   if (icon.isNull())
      icon = IconProvider::iconProvider.icon(QFileIconProvider::File);
#else
   icon = IconProvider::iconProvider.icon(QFileIconProvider::File);
#endif
   return icon;
}
#endif

QIcon IconProvider::drawWarning(const QIcon& icon)
{
   if (icon.isNull())
      return icon;
   const QIcon warning(":/icons/resources/error.svg");
   QIcon result;
   auto sizes = icon.availableSizes();
   // Scalable icon engines may not advertise any fixed sizes.
   if (sizes.isEmpty())
      sizes = {QSize(16, 16), QSize(22, 22), QSize(32, 32), QSize(48, 48), QSize(64, 64)};
   for (const auto& size : sizes)
   {
      QPixmap pixmap = icon.pixmap(size);
      if (!pixmap.isNull())
      {
         QPainter painter(&pixmap);
         const QSize logicalSize = pixmap.deviceIndependentSize().toSize();
         const int badgeSize = qMax(1, qMin(logicalSize.width(), logicalSize.height()) / 2);
         warning.paint(&painter, logicalSize.width() - badgeSize, logicalSize.height() - badgeSize, badgeSize, badgeSize);
      }
      result.addPixmap(pixmap);
   }
   return result;
}

QFileIconProvider IconProvider::iconProvider;
QCache<QString, QIcon> IconProvider::cachedIcons { MAX_CACHED_ICONS };
QCache<QString, QIcon> IconProvider::cachedIconsWithWarning { MAX_CACHED_ICONS };
QIcon IconProvider::fileIconWithWarning;
QIcon IconProvider::folderIconWithWarning;
