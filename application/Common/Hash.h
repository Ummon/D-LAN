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

      /**
        * Build a new hash from an existing one.
        * The data are shared between both.
        */
      Hash(const Hash& h);

      /**
        * Build a new hash from a char*.
        * 'h' must have a length equal or bigger to HASH_SIZE!
        * The data are copied, no pointer is keept to 'h'.
        */
      Hash(const char* h);

      /**
        * Build a new hash from a QByteArray.
        * 'a' must have a length equal or bigger to HASH_SIZE!
        * The data are copied, no pointer is keept to 'a'.
        */
      Hash(const QByteArray& a);

      /**
        * Remove its reference to the shared data, if its the last
        * then delete the data.
        */
      ~Hash();

      /**
        * Assign the data of a another hash.
        * The data are shared.
        */
      Hash& operator=(const Hash&);

      /**
        * Return a pointer to its internal data.
        * The length of the returned value is exactly HASH_SIZE.
        */
      const char* getData() const;

      /**
        * Return a human readable string.
        * For example : 16bd4b1e656129eb9ddaa2ce0f0705f1cc161f77.
        */
      QString toStr() const;

      bool isNull() const;

      /**
        * Return a new rand hash.
        */
      static Hash rand();

   private:
      /**
        * Dereference the pointed data.
        */
      inline void dereference();

      /**
        * Allocated new shared data.
        */
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
      return (uint)h.getData();
   }
}
#endif
