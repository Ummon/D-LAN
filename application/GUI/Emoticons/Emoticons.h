#ifndef GUI_EMOTICONS_H
#define GUI_EMOTICONS_H

#include <QString>
#include <QStringList>
#include <QList>
#include <QMap>
#include <QImage>
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

      QImage getSmileImage(const QString& theme, const QString& name) const;

      QStringList getSmileSymbols(const QString& theme, const QString& name) const;

      QString getSmileName(const QString& symbol) const;

   private:
      void loadThemes();

      static QDomElement XmlNextValue(const QDomElement& domElement, const QString& key);
      static QPair<QString, QDomElement> XmlNextValue(const QDomElement& domElement);

      QString defaultTheme;

      struct Smile
      {
         QImage image;
         QStringList symbols;
      };

      const QString directory;

      // Theme directory -> smile name -> (image, symbols).
      QMap<QString, QMap<QString, Smile>> smiles;
   };
}

#endif
