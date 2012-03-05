#ifndef COMMON_LANGUAGES_H
#define COMMON_LANGUAGES_H

#include <QString>
#include <QList>
#include <QLocale>

namespace Common
{
   struct Language
   {
      Language() {}
      Language(QString filename, QLocale locale) : filename(filename), locale(locale) {}

      QString filename;
      QLocale locale;
   };

   class Languages
   {
   public:
      enum ExeType { CORE, GUI };

      Languages(const QString& path);

      QList<Language> getAvailableLanguages(ExeType exeType);
      Language getBestMatchLanguage(ExeType exeType, QLocale locale);

   private:
      QString path;
   };
}

Q_DECLARE_METATYPE(Common::Language)

#endif
