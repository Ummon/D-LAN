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

#ifndef COMMON_HASH_NO_SHARE_H
#define COMMON_HASH_NO_SHARE_H

#include <string>

#include <QString>
#include <QByteArray>
#include <QDataStream>
#include <QCryptographicHash>

#include <Libs/MersenneTwister.h>

#include <Common/Uncopyable.h>

namespace Common
{
   class Hasher;
   class Hash
   {
      static MTRand mtrand;

   public:
      static const int HASH_SIZE = 20;

   private:
      static const char NULL_HASH[HASH_SIZE];

   public:
      Hash() noexcept;
      Hash(const Hash&) = default;

      explicit Hash(const char* h); // It's too dangerous to construct an implicit Hash from a const char*.
      Hash(const std::string& str);
      Hash(const QByteArray& a);

      Hash(Hash&&) = default;
      Hash& operator=(const Hash&) = default;

      ~Hash() {}

      /**
        * Return a pointer to its internal data.
        * The length of the returned value is exactly HASH_SIZE.
        */
      inline const char* getData() const noexcept { return this->data; }
      inline QByteArray getByteArray() const { return QByteArray(this->data, HASH_SIZE); }

      QString toStr() const;
      QString toStrCArray() const;
      bool isNull() const noexcept;

      static Hash rand();
      static Hash rand(quint32 seed);
      static void setRandSeed(quint32 seed);
      static Hash fromStr(const QString& str);

   private:
      friend QDataStream& operator>>(QDataStream&, Hash&);
      friend QDataStream& operator<<(QDataStream& stream, const Hash& hash);
      friend bool operator==(const Hash& h1, const Hash& h2);
      friend class Hasher;

      char data[HASH_SIZE];
   };

   /**
     * It will read an hash from a data stream and modify the given hash.
     */
   inline QDataStream& operator>>(QDataStream& stream, Hash& hash)
   {
      char data[Hash::HASH_SIZE];
      if (stream.readRawData(data, Hash::HASH_SIZE) != Hash::HASH_SIZE)
         return stream;

      memcpy(hash.data, data, Hash::HASH_SIZE);

      return stream;
   }

   /**
     * It will write an hash to a data stream.
     */
   inline QDataStream& operator<<(QDataStream& stream, const Hash& hash)
   {
      stream.writeRawData(hash.data, Hash::HASH_SIZE);

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

   inline bool operator<(const Hash& h1, const Hash& h2)
   {
      return memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) < 0;
   }

   /**
     * Used by QHash.
     */
   inline uint qHash(const Hash& h)
   {
      // Take the first sizeof(uint) bytes of the hash data.
      return *(const uint*)(h.getData());
   }

   class Hasher : Uncopyable
   {
      static MTRand mtrand;

   public:
      Hasher();
      void addSalt(quint64 salt);
      void addData(const char*, int size);
      Hash getResult();
      void reset();

      static Common::Hash hash(const QString& str);
      static Common::Hash hash(const Common::Hash& hash);
      static Common::Hash hashWithSalt(const QString& str, quint64 salt);
      static Common::Hash hashWithSalt(const Common::Hash& hash, quint64 salt);
      static Common::Hash hashWithRandomSalt(const QString& str, quint64& salt);
      static Common::Hash hashWithRandomSalt(const Common::Hash& hash, quint64& salt);

   private:
      QCryptographicHash cryptographicHash;
   };
}

#endif
