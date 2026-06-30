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
  
#include <Emoticons/Emoticons.h>
using namespace GUI;

#include <QGuiApplication>
#include <QScreen>
#include <QDir>
#include <QDomDocument>
#include <QStringBuilder>

#include <Log.h>

/**
  * @class GUI::Emoticons
  *
  * Can manage different emoticon themes, each themes is a directory which contains
  * Some emoticon images and a meta-file called "Emoticons.plist" defining the name of
  * each emoticon and the associated symbols.
  *
  * At the moment, the name of emoticons corresponds to its filename.
  */

const QString Emoticons::DEFAULT_THEME_NAME(cleanName("Default"));
const QString Emoticons::META_FILE_NAME("Emoticons.plist");

Emoticons::Emoticons(const QString& directory, const QString& defaultTheme)
   : directory(directory)
{
   this->loadThemes();

   if (!defaultTheme.isEmpty())
      this->setDefaultTheme(defaultTheme);
}

QStringList Emoticons::getThemes() const
{
   return this->smiles.keys();
}

void Emoticons::setDefaultTheme(const QString& name)
{
   if (this->smiles.contains(name))
      this->defaultTheme = name;
   else if (this->smiles.contains(DEFAULT_THEME_NAME))
      this->defaultTheme = DEFAULT_THEME_NAME;
   else if (!this->smiles.empty())
      this->defaultTheme = this->smiles.constBegin().key();
   else
      this->defaultTheme = "";
}

QString Emoticons::getDefaultTheme() const
{
   return this->defaultTheme;
}

QStringList Emoticons::getSmileNames(const QString& theme) const
{
   return this->smiles.value(theme).keys();
}

QPixmap Emoticons::getSmileImage(const QString& theme, const QString& name) const
{
   return this->smiles.value(theme).value(name).image;
}

QStringList Emoticons::getSmileSymbols(const QString& theme, const QString& name) const
{
   return this->smiles.value(theme).value(name).symbols;
}

/**
  * Returns the (theme, smile name) from the given symbol searching first in the current default theme then the other ones.
  * Returns empty strings if not found.
  */
std::pair<QString, QString> Emoticons::getSmileName(const QString& symbol) const
{
   const QString name = this->getSmileName(symbol, this->defaultTheme);
   if (!name.isEmpty())
      return std::make_pair(this->defaultTheme, name);

   for (const QString& theme : this->smiles.keys())
   {
      if (theme != this->defaultTheme)
      {
         const QString name = this->getSmileName(symbol, theme);
         if (!name.isEmpty())
            return std::make_pair(theme, name);
      }
   }

   return std::make_pair(QString(), QString());
}

QString Emoticons::getSmileName(const QString& symbol, const QString& theme) const
{
   const QMap<QString, Smile>& smilesDefaultTheme = this->smiles.value(theme);
   for (QMapIterator<QString, Smile> i(smilesDefaultTheme); i.hasNext();)
   {
      i.next();
      for (const QString& s : i.value().symbols)
      {
         if (symbol == s)
            return i.key();
      }
   }

   return QString();
}

void Emoticons::loadThemes()
{
   this->smiles.clear();

   foreach (QString themeDir, QDir(this->directory).entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name))
   {
      QString themePath = this->directory % '/' % themeDir;
      QString metaFilePath = themePath % '/' % META_FILE_NAME;
      QFile metaFile(metaFilePath);
      if (!metaFile.open(QIODevice::ReadOnly))
      {
         L_WARN(QString("Unable to open the emoticon meta file: %1").arg(metaFilePath));
         continue;
      }

      QDomDocument doc;

      auto parseResult = doc.setContent(&metaFile);
      if (!parseResult)
      {
         L_WARN(
            QString("Unable to parse the meta file: %1, error: %2, line: %3")
               .arg(metaFilePath, parseResult.errorMessage)
               .arg(parseResult.errorLine)
         );
         continue;
      }

      QDomElement root = doc.documentElement();
      QDomElement dictRoot = root.firstChildElement("dict");
      QDomElement dictEmoticons = XmlNextValue(dictRoot.firstChildElement(), "Emoticons");

      QPair<QString, QDomElement> currentSmile = XmlNextValue(dictEmoticons.firstChildElement());

      while (!currentSmile.second.isNull())
      {
         QString imagePath = themePath % '/' % currentSmile.first;
         Smile smile { QPixmap(imagePath), QStringList() };
         smile.image.setDevicePixelRatio(QGuiApplication::primaryScreen()->devicePixelRatio());

         if (!smile.image.isNull())
         {
            QDomNodeList symbols = XmlNextValue(currentSmile.second.firstChildElement(), "Equivalents").childNodes();
            for (int i = 0; i < symbols.count(); i++)
            {
               QDomElement symbol = symbols.at(i).toElement();
               if (!symbol.isNull() && symbol.tagName() == "string")
                  smile.symbols << symbol.text();
            }

            this->smiles[cleanName(themeDir)][cleanName(currentSmile.first)] = smile;
         }
         else
            L_WARN(QString("Unable to load the following emoticon: %1").arg(imagePath));

         currentSmile = XmlNextValue(currentSmile.second.nextSiblingElement());
      }
   }
}

QDomElement Emoticons::XmlNextValue(const QDomElement& domElement, const QString& key)
{
   QDomElement currentElement = domElement;

   do
   {
      if (currentElement.tagName() == "key" && currentElement.text() == key)
         return currentElement.nextSiblingElement();

      currentElement = currentElement.nextSiblingElement();
   } while (!currentElement.isNull());

   return QDomElement();
}

QPair<QString, QDomElement> Emoticons::XmlNextValue(const QDomElement& domElement)
{
   QDomElement currentElement = domElement;

   do
   {
      if (currentElement.tagName() == "key")
         return qMakePair(currentElement.text(), currentElement.nextSiblingElement());

      currentElement = currentElement.nextSiblingElement();
   } while (!currentElement.isNull());

   return qMakePair(QString(), QDomElement());
}

QString Emoticons::cleanName(const QString& name)
{
   QString result = name.toLower();
   result.replace(' ', "");
   return result;
}
