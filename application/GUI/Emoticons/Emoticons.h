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
  
#ifndef GUI_EMOTICONS_H
#define GUI_EMOTICONS_H

#include <QString>
#include <QStringList>
#include <QList>
#include <QMap>
#include <QPixmap>
#include <QPair>
#include <QDomElement>

namespace GUI
{
   class Emoticons
   {
      static const QString DEFAULT_THEME_NAME;
      static const QString META_FILE_NAME;

   public:
      Emoticons(const QString& directory, const QString& defaultTheme = "");

      QStringList getThemes() const;

      void setDefaultTheme(const QString& name);
      QString getDefaultTheme() const;

      QStringList getSmileNames(const QString& theme) const;

      QPixmap getSmileImage(const QString& theme, const QString& name) const;

      QStringList getSmileSymbols(const QString& theme, const QString& name) const;

      QString getSmileName(const QString& symbol) const;

   private:
      void loadThemes();

      static QDomElement XmlNextValue(const QDomElement& domElement, const QString& key);
      static QPair<QString, QDomElement> XmlNextValue(const QDomElement& domElement);
      static QString cleanName(const QString& name);

      QString defaultTheme;

      struct Smile
      {
         QPixmap image;
         QStringList symbols;
      };

      const QString directory;

      // Theme directory -> smile name -> (image, symbols).
      QMap<QString, QMap<QString, Smile>> smiles;
   };
}

#endif
