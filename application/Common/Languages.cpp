#include <Common/Languages.h>
using namespace Common;

#include <QDir>
#include <QRegExp>

/**
  * @class Common::Languages
  *
  * Using to get the available languages in a given directory.
  */

Languages::Languages(const QString& path) :
   path(path)
{
}

QList<Language> Languages::getAvailableLanguages()
{
   QList<Language> languages;
   QDir dir(this->path);
   for (QStringListIterator i(dir.entryList(QStringList() << "*.qm", QDir::Files, QDir::Name)); i.hasNext();)
   {
      QString filename(i.next());
      QRegExp reg("\\w+\\.(\\w+)\\.qm");
      if (reg.exactMatch(filename))
      {
         QLocale locale(reg.capturedTexts()[1]);
         if (locale.language() != QLocale::C)
            languages << Language(filename, locale);
      }
   }
   return languages;
}
