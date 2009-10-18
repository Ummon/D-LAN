#ifndef COMMON_HASH_H
#define COMMON_HASH_H

#include <QByteArray>

#include "Common_global.h"

namespace Common
{
   class COMMON_EXPORT Hash : public QByteArray
   {
   public:
      Hash();
      Hash(const char* str);
      Hash(const QByteArray& bytes);

      const QString toStr() const;
      static Hash rand();
   };
}
#endif
