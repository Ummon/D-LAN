#ifndef COMMON_HASH_H
#define COMMON_HASH_H

#include <QString>
#include <QByteArray>

namespace Common
{
   /**
     * An uber-optimized SHA-1 hash.
     * see : http://fr.wikipedia.org/wiki/SHA-1
     */
   class Hash
   {
   public:
      static const int HASH_SIZE = 20; ///< 20 bytes.

      /**
        * Build a new empty hash, its value is set to 0.
        */
      Hash();
      ~Hash();
      Hash(const Hash& h);
      Hash(const char* h);
      Hash(const QByteArray& a);
      Hash& operator=(const Hash&);

      const char* getData() const;
      QString toStr() const;

      static Hash rand();

   private:
      /**
        * Dereference the pointed data.
        * return true if the data has been deleted.
        */
      inline bool dereference();
      inline void newData();

      char* data; ///< Point to an array of HASH_SIZE+1 bytes. The first byte is the number of hash which point this array.
   };

   inline bool operator==(const Hash& h1, const Hash& h2)
   {
      return memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) == 0;
   }

   inline uint qHash(const Hash& h)
   {
      return (uint)(h.getData() + 1);
   }
}
#endif
