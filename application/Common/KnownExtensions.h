#pragma once

#include <QString>
#include <QList>
#include <QSet>

namespace Common
{
   enum class ExtensionCategory
   {
      AUDIO = 1,
      VIDEO = 2,
      COMPRESSED = 3,
      DOCUMENT = 4,
      PICTURE = 5,
      SUBTITLE = 6,
      EXECUTABLE = 7,
      MEDIA_ARCHIVE = 8
   };

   class CategoryNotFoundException {};

   class KnownExtensions
   {
   public:
      static bool exists(const QString& extension);
      static int nbCategory();
      static QList<QString> getExtensions(ExtensionCategory cat);
      static ExtensionCategory getCategoryFrom(const QString& extension);

      /**
        * Returns '-1' if there is no extension.
        * For example: "abc.zip" may return 4.
        */
      static int getBeginningExtension(const QString& filename);
      static QString removeExtension(const QString& filename);
      static QString getExtension(const QString& filename);

   private:
      static void add(ExtensionCategory cat, const QString& extension);
      static QHash<QString, ExtensionCategory> extensions;
      static QList<QList<QString>> extensionsByCategory;

      static struct Init { Init(); } initializer;
   };
}
