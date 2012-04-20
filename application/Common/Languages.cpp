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

QList<Language> Languages::getAvailableLanguages(ExeType exeType)
{
   QList<Language> languages;
   QDir dir(this->path);
   for (QStringListIterator i(dir.entryList(QStringList() << "*.qm", QDir::Files, QDir::Name)); i.hasNext();)
   {
      QString filename(i.next());
      QRegExp reg(QString("d_lan_").append(exeType == ExeType::GUI ? "gui" : "core").append("\\.(\\w+)\\.qm"));
      if (reg.exactMatch(filename))
      {
         QLocale locale(reg.capturedTexts()[1]);
         if (locale.language() != QLocale::C)
            languages << Language { filename, locale };
      }
   }
   return languages;
}

Language Languages::getBestMatchLanguage(ExeType exeType, QLocale locale)
{
   QList<Language> languages = this->getAvailableLanguages(exeType);
   if (languages.isEmpty())
      return Language();

   Language bestCurrentLanguage;
   for (QListIterator<Language> i(languages); i.hasNext();)
   {
      Language currentLanguage = i.next();
      if (currentLanguage.locale.language() == locale.language())
      {
         if (currentLanguage.locale.country() == locale.country()) // Perfect match.
            return currentLanguage;
         if (bestCurrentLanguage.filename.isEmpty())
            bestCurrentLanguage = currentLanguage;
      }
   }
   return bestCurrentLanguage;
}
