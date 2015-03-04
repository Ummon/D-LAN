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
  
#ifndef COMMON_STRINGUTILS_H
#define COMMON_STRINGUTILS_H

#include <string>

#include <QString>
#include <QStringList>

namespace Common
{
   class StringUtils
   {
   public:
      static QString toLowerAndRemoveAccents(const QString& str);
      static QStringList splitInWords(const QString& words);
      static QStringList splitArguments(const QString& str);

      static bool isKorean(const QString& str);

      static int strcmpi(const std::string& s1, const std::string& s2);

      static inline int commonPrefix(const QString& s1, const QString& s2) { return StringUtils::commonPrefix(&s1, &s2); }
      static inline int commonPrefix(const QStringRef& s1, const QStringRef& s2);

      static quint32 hashStringToInt(const QString& str);
   };
}

inline int Common::StringUtils::commonPrefix(const QStringRef& s1, const QStringRef& s2)
{
   int i = 0;
   while (i < s1.size() && i < s2.size())
   {
      if (s1.at(i) != s2.at(i))
         return i;
      ++i;
   }
   return i;
}

#endif
