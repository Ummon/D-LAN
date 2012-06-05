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
