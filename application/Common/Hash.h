#ifndef COMMON_HASH_H
#define COMMON_HASH_H

#define WITH_MUTEX false

#include "Uncopyable.h"

#include <QString>
#include <QByteArray>
#include <QDataStream>

namespace Blake
{
   extern "C" {
      #include <Libs/blake/blake_opt.h>
   }
}

#include <Libs/MersenneTwister.h>

#if WITH_MUTEX
#  include <QMutex>
#endif

namespace Common
{
   class Hasher;
   class Hash
   {
   private:
      static MTRand mtrand;

   public:
      static const int HASH_SIZE = 28; ///< 28 bytes.

      Hash();
      Hash(const Hash& h);
      Hash(const char* h);
      Hash(const QByteArray& a);

      ~Hash();

      Hash& operator=(const Hash&);

      /**
        * Return a pointer to its internal data.
        * The length of the returned value is exactly HASH_SIZE.
        */
      inline const char* getData() const { return this->data->hash; }

      QString toStr() const;
      bool isNull() const;

      static Hash rand();
      static Hash fromStr(const QString& str);

   private:
      inline void dereference();
      inline void newData()
      {
         this->data = new SharedData;
         this->data->nbRef = 1;
      }

      friend QDataStream& operator>>(QDataStream&, Hash&);
      friend QDataStream& operator<<(QDataStream& stream, const Hash& hash);
      friend class Hasher;

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

   /**
     * It will read an hash from a data stream and modify the given hash.
     */
   inline QDataStream& operator>>(QDataStream& stream, Hash& hash)
   {
      char data[Hash::HASH_SIZE];
      int length = stream.readRawData(data, Hash::HASH_SIZE);
      if (length != Hash::HASH_SIZE)
         return stream;

      if (qstrncmp(hash.data->hash, data, Hash::HASH_SIZE) != 0)
      {
         hash.newData();
         memcpy(hash.data->hash, data, Hash::HASH_SIZE);
      }

      return stream;
   }

   /**
     * It will write an hash to a data stream.
     */
   inline QDataStream& operator<<(QDataStream& stream, const Hash& hash)
   {
      if (!hash.isNull())
         stream.writeRawData(hash.data->hash, Hash::HASH_SIZE);

      return stream;
   }

   inline bool operator==(const Hash& h1, const Hash& h2)
   {
      return memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) == 0;
   }

   inline bool operator!=(const Hash& h1, const Hash& h2)
   {
      return !(h1 == h2);
   }

   /**
     * Used by QHash.
     */
   inline uint qHash(const Hash& h)
   {
      return *(const uint*)(h.getData());
   }

   class Hasher : Uncopyable
   {
   public:
      Hasher();
      void addData(const char*, int size);
      Hash getResult();
      void reset();

   private:
      Blake::hashState state;
   };
}
#endif
