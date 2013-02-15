#include <Emoticons.h>
using namespace GUI;

#include <QDir>

const QString Emoticons::DEFAULT_THEME_NAME("Default");

Emoticons::Emoticons(const QString& directory, const QString& defaultTheme) :
   directory(directory)
{
   if (!defaultTheme.isEmpty())
      this->setDefaultTheme(defaultTheme);


}

QStringList Emoticons::getThemes() const
{
   //QStringList result = QDir(this->directory).entryList(QDir::Dirs, QDir::Name);
   return this->smiles.keys();
   //return result;
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

QImage Emoticons::getSmileImage(const QString& theme, const QString& name) const
{
   return this->smiles.value(theme).value(name).image;
}

QStringList Emoticons::getSmileSymbols(const QString& theme, const QString& name) const
{
   return this->smiles.value(theme).value(name).symbols;
}

QString Emoticons::getSmileName(const QString& symbol) const
{
   const QMap<QString, Smile>& smilesDefaultTheme = this->smiles.value(this->defaultTheme);
   for (QMapIterator<QString, Smile> i(smilesDefaultTheme); i.hasNext(); i.next())
   {
      for (QStringListIterator j(i.value().symbols); j.hasNext();)
      {
         const QString& s = j.next();
         if (symbol == s)
            return i.key();
      }
   }

   return QString();
}
