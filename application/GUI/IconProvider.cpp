#include "iconprovider.h"

#if defined(Q_OS_WIN32)
#include <shlobj.h>
#include <shellapi.h>
#elif defined(Q_OS_LINUX)

#else

#endif

QFileIconProvider IconProvider::qtIconProvider;
QMap<QString, QIcon> IconProvider::iconMap;

IconProvider::IconProvider()
{
}

QIcon IconProvider::getIcon(Protos::Common::Entry entry)
{
    if (entry.type() == Protos::Common::Entry_Type_DIR)
        return IconProvider::qtIconProvider.icon(QFileIconProvider::Folder);
    else {
        QString name = QString::fromStdString(entry.name());
        int index = name.lastIndexOf(".");
        if(index != -1) {
            // Get extension
            name = name.mid(index);
            return IconProvider::getIconCache(name);
        }else
            return IconProvider::qtIconProvider.icon(QFileIconProvider::File);
    }
}

QIcon IconProvider::getIconCache(QString extension)
{
    if(iconMap.contains(extension))
    {
        return iconMap.value(extension);
    }
    else
    {
        const QIcon &icon = IconProvider::getIconNative(extension);
        iconMap.insert(extension, icon);
        return icon;
    }
}

QIcon IconProvider::getIconNative(QString extension){
    QIcon icon;
#if defined(Q_OS_WIN32)
    SHFILEINFO psfi;
    SHGetFileInfo(extension.toStdWString().c_str(), FILE_ATTRIBUTE_NORMAL, &psfi, sizeof(psfi),
                  SHGFI_ICON | SHGFI_SMALLICON | SHGFI_USEFILEATTRIBUTES);
    if(psfi.hIcon != NULL)
    {
        icon = QIcon(QPixmap::fromWinHICON(psfi.hIcon));
    }
#endif
    return icon;
}
