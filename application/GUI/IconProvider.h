#ifndef ICONPROVIDER_H
#define ICONPROVIDER_H

#include <QIcon>
#include <QMap>
#include <QFileIconProvider>
#include <Protos/common.pb.h>

class IconProvider
{
private:
    static QFileIconProvider qtIconProvider;
    static QMap<QString, QIcon> iconMap;

    static QIcon getIconNative(QString icon);
    static QIcon getIconCache(QString icon);
public:
    IconProvider();

    static QIcon getIcon(Protos::Common::Entry entry);
};

#endif // ICONPROVIDER_H
