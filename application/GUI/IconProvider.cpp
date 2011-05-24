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
      {
         // Get extension.
         name = name.mid(index);
         return IconProvider::getIconCache(name);
      }
      else
      {
         return IconProvider::qtIconProvider.icon(QFileIconProvider::File);
      }
   }
}

QIcon IconProvider::getIconCache(const QString& extension)
{
   QIcon icon = iconMap.value(extension);
   if (icon.isNull())
   {
      icon = IconProvider::getIconNative(extension);
      iconMap.insert(extension, icon);
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
QMap<QString, QIcon> IconProvider::iconMap;
