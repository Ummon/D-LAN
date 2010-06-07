#ifndef COMMON_HASH_H
#define COMMON_HASH_H

#define WITH_MUTEX false

#include <QString>
#include <QByteArray>

#if WITH_MUTEX
#  include <QMutex>
#endif

namespace Common
{
   class Hash
   {
   public:
      static const int HASH_SIZE = 20; ///< 20 bytes.

      Hash();
      Hash(const Hash& h);
      Hash(const char* h);
      Hash(const QByteArray& a);

      ~Hash();

      Hash& operator=(const Hash&);
      const char* getData() const;
      QString toStr() const;
      bool isNull() const;

      static Hash rand();
      static Hash fromStr(const QString& str);

   private:
      inline void dereference();
      inline void newData();

      struct SharedData
      {
#if WITH_MUTEX
         QMutex mutex;
#endif
         int nbRef;
         char hash[HASH_SIZE];
      };

      SharedData* data;
   };

   inline bool operator==(const Hash& h1, const Hash& h2)
   {
      return memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) == 0;
   }

   /**
     * Used by QHash.
     */
   inline uint qHash(const Hash& h)
   {
      return *(const uint*)(h.getData());
   }
}
#endif
