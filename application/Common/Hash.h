#ifndef COMMON_HASH_H
#define COMMON_HASH_H

#include <QByteArray>

namespace Common
{
   class Hash : public QByteArray
   {
   public:
      Hash();
      Hash(const char* str);
      Hash(const QByteArray& bytes);

      QString toStr() const;
      static Hash rand();
   };
}
#endif
