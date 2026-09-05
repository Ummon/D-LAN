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

#pragma once

#include <cstring>
#include <optional>
#include <span>
#include <string>

#include <QHash>
#include <QString>
#include <QByteArray>
#include <QDataStream>

#include <blake3.h>

#include <Common/Uncopyable.h>

namespace Common
{
   class Hasher;

   /**
     * The alternative implementation of 'Hash', selected with 'SHARED_DATA' in "Hash.h".
     * The hash data is shared between the copies instead of being embedded in each object.
     *
     * @remarks The reference counter isn't atomic: unlike 'Hash_noShare', an object of this implementation
     *          must not be copied from two threads at the same time.
     * @remarks Its observable behaviour must stay identical to the one of 'Hash_noShare', both are the same
     *          type for the rest of the application. In particular the hash values must be the same, thus
     *          the same hash function is used.
     */
   class Hash
   {
   public:
      static const int HASH_SIZE = 28;
      static const int NB_BYTES_SHORT_STR = 3;

   private:
      static const char NULL_HASH[HASH_SIZE];

   public:
      Hash() noexcept;
      Hash(const Hash& h) noexcept;
      Hash(Hash&& h) noexcept;

      explicit Hash(const char* h); // It's too dangerous to construct an implicit Hash from a const char*.
      Hash(const std::string& str);
      Hash(const QByteArray& a);

      ~Hash();

      Hash& operator=(const Hash& h);
      Hash& operator=(Hash&& h) noexcept;

      /**
        * Return a pointer to its internal data.
        * The length of the returned value is exactly HASH_SIZE.
        */
      inline const char* getData() const noexcept { return this->data ? this->data->hash : NULL_HASH; }
      inline QByteArray getByteArray() const { return QByteArray(this->data ? this->data->hash : NULL_HASH, HASH_SIZE); }

      QString toStr() const;
      QString toStrShort() const;
      QString toStrCArray() const;
      bool isNull() const noexcept;

      static Hash rand();
      static Hash rand(quint32 seed);
      // Accepts exactly 2 * HASH_SIZE ASCII hexadecimal characters (either case).
      // Returns std::nullopt for invalid input; an all-zero hash is a valid result.
      [[nodiscard]] static std::optional<Hash> fromStr(const QString& str);

   private:
      inline void dereference();
      inline void newData();

      /**
        * A hash whose bytes are all zero is a null hash, it must be represented by an absence of data,
        * otherwise 'isNull()' wouldn't agree with the other implementation.
        */
      inline void releaseDataIfNull();

      friend QDataStream& operator>>(QDataStream&, Hash&);
      friend QDataStream& operator<<(QDataStream& stream, const Hash& hash);
      friend bool operator==(const Hash& h1, const Hash& h2) noexcept;
      friend class Hasher;

      struct SharedData
      {
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
      if (stream.readRawData(data, Hash::HASH_SIZE) != Hash::HASH_SIZE)
         return stream;

      if (memcmp(Hash::NULL_HASH, data, Hash::HASH_SIZE) == 0)
      {
         if (hash.data)
         {
            hash.dereference();
            hash.data = nullptr;
         }
      }
      else if (!hash.data || memcmp(hash.data->hash, data, Hash::HASH_SIZE) != 0)
      {
         hash.dereference();
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
      if (hash.data)
         stream.writeRawData(hash.data->hash, Hash::HASH_SIZE);
      else
         stream.writeRawData(Hash::NULL_HASH, Hash::HASH_SIZE);

      return stream;
   }

   inline bool operator==(const Hash& h1, const Hash& h2) noexcept
   {
      return h1.data == h2.data || memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) == 0;
   }

   inline bool operator!=(const Hash& h1, const Hash& h2) noexcept
   {
      return !(h1 == h2);
   }

   inline bool operator<(const Hash& h1, const Hash& h2) noexcept
   {
      return memcmp(h1.getData(), h2.getData(), Hash::HASH_SIZE) < 0;
   }

   /**
     * Used by QHash.
     */
   inline size_t qHash(const Hash& h, size_t seed = 0)
   {
      return qHashBits(h.getData(), Hash::HASH_SIZE, seed);
   }

   class Hasher : Uncopyable
   {
   public:
      Hasher();
      void addSalt(quint64 salt);
      // Hashes a valid byte span. Empty spans are a no-op.
      void addData(std::span<const char> data);
      Hash getResult();
      void reset();

      static Common::Hash hash(const QString& str);
      static Common::Hash hash(const Common::Hash& hash);
      static Common::Hash hashWithSalt(const QString& str, quint64 salt);
      static Common::Hash hashWithSalt(const Common::Hash& hash, quint64 salt);
      static Common::Hash hashWithRandomSalt(const QString& str, quint64& salt);
      static Common::Hash hashWithRandomSalt(const Common::Hash& hash, quint64& salt);

   private:
      blake3_hasher hasher;
   };
}

/**
  * Removes the reference to the pointed data if it exists.
  */
inline void Common::Hash::dereference()
{
   if (this->data)
   {
      this->data->nbRef -= 1;
      if (this->data->nbRef == 0)
         delete this->data;
   }
}

inline void Common::Hash::newData()
{
   this->data = new SharedData;
   this->data->nbRef = 1;
}

inline void Common::Hash::releaseDataIfNull()
{
   if (this->data && memcmp(this->data->hash, NULL_HASH, HASH_SIZE) == 0)
   {
      this->dereference();
      this->data = nullptr;
   }
}
