#ifndef COMMON_KNOWNEXTENSIONS_H
#define COMMON_KNOWNEXTENSIONS_H

#include <QString>
#include <QList>
#include <QSet>

namespace Common
{
   enum ExtensionCategory
   {
      AUDIO = 0,
      VIDEO = 1,
      COMPRESSED = 2,
      DOCUMENT = 3,
      PICTURE = 4,
      SUBTITLE = 5,
      EXECUTABLE = 6,
      MEDIA_ARCHIVE = 7
   };

   class KnownExtensions
   {
   public:
      static bool exists(const QString& extension);
      static int nbCategory();
      static QList<QString> getExtension(ExtensionCategory cat);

      /**
        * Returns '-1' if there is no extension.
        * For example: "abc.zip" may return 4.
        */
      static int getBeginingExtension(const QString& filename);
      static QString removeExtension(const QString& filename);
      static QString getExtension(const QString& filename);

   private:
      static void add(ExtensionCategory cat, const QString& extension);
      static QHash<QString, ExtensionCategory> extensions;
      static QList<QList<QString>> extensionsByCategory;

      static struct Init { Init(); } initializer;
   };
}

#endif
