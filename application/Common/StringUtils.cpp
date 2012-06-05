#include <Common/StringUtils.h>
using namespace Common;

#include <QRegExp>

QString StringUtils::toLowerAndRemoveAccents(const QString& str)
{
   QString strLower = str.toLower(); // It depends of the current locale.

   for (int i = 0; i < strLower.size(); i++)
      switch (strLower[i].unicode())
      {
      case 0x00E0: // à .
      case 0x00E1: // á.
      case 0x00E2: // â.
      case 0x00E3: // ã.
      case 0x00E4: // ä.
      case 0x00E5: // å.
      case 0x0101: // ā.
      case 0x0103: // ă.
      case 0x0105: // ą.
         strLower[i] = 'a';
         break;
      case 0x00E7: // ç.
      case 0x0107: // ć.
      case 0x0109: // ĉ.
      case 0x010B: // ċ.
      case 0x010D: // č.
         strLower[i] = 'c';
         break;
      case 0x010F: // ď.
      case 0x0111: // đ.
         strLower[i] = 'd';
         break;
      case 0x00E8: // è.
      case 0x00E9: // é.
      case 0x00EA: // ê.
      case 0x00EB: // ë.
      case 0x0113: // ē.
      case 0x0115: // ĕ.
      case 0x0117: // ė.
      case 0x0119: // ę.
      case 0x011B: // ě.
         strLower[i] = 'e';
         break;
      case 0x011D: // ĝ.
      case 0x011F: // ğ.
      case 0x0121: // ġ.
      case 0x0123: // ģ.
         strLower[i] = 'g';
         break;
      case 0x0125: // ĥ.
      case 0x0127: // ħ.
         strLower[i] = 'h';
         break;
      case 0x00EC: // ì.
      case 0x00ED: // í.
      case 0x00EE: // î.
      case 0x00EF: // ï.
      case 0x0129: // ĩ.
      case 0x012B: // ī.
      case 0x012D: // ĭ.
      case 0x012F: // į.
      case 0x0131: // ı.
         strLower[i] = 'i';
         break;
      case 0x00F1: // ñ.
         strLower[i] = 'n';
         break;
      case 0x00F2: // ò.
      case 0x00F3: // ó.
      case 0x00F4: // ô.
      case 0x00F5: // õ.
      case 0x00F6: // ö.
         strLower[i] = 'o';
         break;
      case 0x00F9: // ù.
      case 0x00FA: // ú.
      case 0x00FB: // û.
      case 0x00FC: // ü.
         strLower[i] = 'u';
         break;
      case 0x00FD: // ý.
      case 0x00FE: // ÿ.
         strLower[i] = 'y';
         break;
      }

   return strLower;
}

/**
  * Take raw terms in a string and split, trim and filter to
  * return a list of keyword.
  * Some character or word can be removed.
  * Maybe a class 'WordSplitter' should be created.
  * @example " The little  DUCK " => ["the", "little", "duck"].
  */
QStringList StringUtils::splitInWords(const QString& words)
{
   static const QRegExp regExp("(\\W+|_)");
   return StringUtils::toLowerAndRemoveAccents(words).split(regExp, QString::SkipEmptyParts);
}

/**
  * http://www.tamasoft.co.jp/en/general-info/unicode.html
  * http://en.wikipedia.org/wiki/Hangul
  */
bool StringUtils::isKorean(const QString& str)
{
   for (int i = 0; i < str.size(); ++i)
   {
      const ushort& code = str[i].unicode();
      if (
          code >= 0x1100 && code <= 0x11FF ||
          code >= 0x3130 && code <= 0x318F ||
          code >= 0x3200 && code <= 0x32FF ||
          code >= 0xA960 && code <= 0xA97F ||
          code >= 0xAC00 && code <= 0xD7AF ||
          code >= 0xD7B0 && code <= 0xD7FF ||
          code >= 0xFF00 && code <= 0xFFEF
       )
         return true;
   }
   return false;
}

/**
  * Compare two std::string without case sensitive.
  * @return 0 if equal, 1 if s1 > s2, -1 if s1 < s2.
  */
int StringUtils::strcmpi(const std::string& s1, const std::string& s2)
{
   for (unsigned int i = 0; i < s1.length() && i < s2.length(); i++)
   {
      const int c1 = tolower(s1[i]);
      const int c2 = tolower(s2[i]);
      if (c1 > c2) return 1;
      else if (c1 < c2) return -1;
   }
   if (s1.length() > s2.length())
      return 1;
   else if (s1.length() < s2.length())
      return -1;
   return 0;
}

/**
  * If more speedup is needed, it may be replaced by the FNV hash function: http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
  */
quint32 StringUtils::hashStringToInt(const QString& str)
{
   QByteArray data = str.toLocal8Bit();
   if (data.size() <= 1)
      return qChecksum(data.constData(), data.size());

   const int s = data.size();
   const quint32 part1 = qChecksum(data.constData(), s / 2);
   const quint32 part2 = qChecksum(data.constData() + s / 2, s / 2 + (s % 2 == 0 ? 0 : 1));
   return part1 | part2 << 16;
}
