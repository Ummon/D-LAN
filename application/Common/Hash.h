/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
      // HASH_SIZE can be 28, 32, 48 or 64 bytes.
      static const int HASH_SIZE = 28;

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
      QString toStrCArray() const;
      bool isNull() const;

      static Hash rand();
      static Hash fromStr(const QString& str);
      static const Hash null;

   private:
      inline void dereference();
      inline void newData();

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
      void addSalt(const char* salt);
      void addPredefinedSalt();
      void addData(const char*, int size);
      Hash getResult();
      void reset();

      static Common::Hash hash(const QString& str);
      static Common::Hash hashWithSalt(const QString& str);

   private:
      Blake::hashState state;
   };
}

using namespace Common;

inline void Hash::dereference()
{
#if WITH_MUTEX
   QMutexLocker locker(&this->data->mutex);
#endif
   this->data->nbRef -= 1;
   if (this->data->nbRef == 0)
      delete this->data;
}

inline void Hash::newData()
{
   this->data = new SharedData;
   this->data->nbRef = 1;
}

#endif

