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

#include <Common/StringUtils.h>
using namespace Common;

#include <QRegularExpression>

QString StringUtils::toLowerAndRemoveAccents(const QString& str)
{
   const QString decomposed = str.toLower().normalized(QString::NormalizationForm_KD);
   QString result;
   result.reserve(decomposed.size());
   for (const QChar c : decomposed)
      if (c.category() != QChar::Mark_NonSpacing && c.category() != QChar::Mark_SpacingCombining)
         result.append(c);
   return result;
}

/**
  * Take raw terms in a string and split, trim and filter to
  * return a list of lower case keywords without accents.
  * Some character or word can be removed.
  * @example " The little  DUCK " => ["the", "little", "duck"].
  */
QStringList StringUtils::splitInWords(const QString& words)
{
   static const QRegularExpression regExp("(\\W+|_)");
   return StringUtils::toLowerAndRemoveAccents(words).split(regExp, Qt::SkipEmptyParts);
}

/**
 * Take a string (like a command line) and split it in trimmed arguments.
 * Arguments can be quoted, for instance :
 *    abc "def ghi" => ["abc", "def ghi"]
 */
QStringList StringUtils::splitArguments(const QString& str)
{
   QStringList args;
   QString currentArg;
   bool inQuotes = false;
   bool quoted = false; // 'true' if the current argument has a quoted part, it's kept even if empty.

   for (int i = 0; i < str.length(); i++)
   {
      const QChar c = str[i];

      if (c == '"')
      {
         inQuotes = !inQuotes;
         quoted = true;
      }
      else if (c.isSpace() && !inQuotes)
      {
         if (quoted || !currentArg.isEmpty())
            args << currentArg;
         currentArg.clear();
         quoted = false;
      }
      else
      {
         currentArg.append(c);
      }
   }

   if (quoted || !currentArg.isEmpty())
      args << currentArg;

   return args;
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
   const QByteArray data = str.toUtf8();
   const QByteArrayView view = QByteArrayView(data);
   if (data.size() <= 1)
      return qChecksum(view);

   auto s = data.length();

   const quint32 part1 = qChecksum(view.sliced(0, s / 2));
   const quint32 part2 = qChecksum(view.sliced(s / 2, s / 2 + (s % 2 == 0 ? 0 : 1)));
   return part1 | part2 << 16;
}

#ifdef Q_OS_WIN32
QList<wchar_t> StringUtils::towcharList(const QString& str)
{
   QList<wchar_t> str_wchar(str.size() + 1);
   str.toWCharArray(str_wchar.data());
   str_wchar[str_wchar.size() - 1] = 0;
   return str_wchar;
}
#endif
