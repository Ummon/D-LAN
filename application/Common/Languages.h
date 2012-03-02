#ifndef COMMON_LANGUAGES_H
#define COMMON_LANGUAGES_H

#include <QString>
#include <QList>
#include <QLocale>

namespace Common
{
   struct Language
   {
      Language(QString filename, QLocale locale) : filename(filename), locale(locale) {}

      QString filename;
      QLocale locale;
   };

   class Languages
   {
   public:
      Languages(const QString& path);

      QList<Language> getAvailableLanguages();

   private:
      QString path;
   };
}

#endif
