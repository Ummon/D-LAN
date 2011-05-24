#ifndef GUI_ICONPROVIDER_H
#define GUI_ICONPROVIDER_H

#include <QIcon>
#include <QMap>
#include <QFileIconProvider>
#include <Protos/common.pb.h>

namespace GUI
{
   class IconProvider
   {
   public:
      static QIcon getIcon(const Protos::Common::Entry& entry);

   private:
      static QIcon getIconCache(const QString& icon);
      static QIcon getIconNative(const QString& icon);

      static QFileIconProvider qtIconProvider;
      static QMap<QString, QIcon> iconMap;
   };
}

#endif
