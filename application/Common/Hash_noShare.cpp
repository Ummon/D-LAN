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

#include <Common/Hash.h>

#if !SHARED_DATA

#include <Common/Hash_noShare.h>
using namespace Common;

#include <QtGlobal>
#include <QTime>
#include <QRandomGenerator64>

const char Hash::NULL_HASH[HASH_SIZE] {};

/**
  * Build a null hash, its value is set to 0.
  */
Hash::Hash() noexcept :
   data{}
{
}

/**
  * Build a new hash from a char*, 'h' is not a readable string, @see fromStr.
  * 'h' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is kept to 'h'.
  * 'h' can be a null pointer, in this case a null hash will be built.
  */
Hash::Hash(const char* h)
{
   memcpy(this->data, h == nullptr ? NULL_HASH : h, HASH_SIZE);
}

/**
  * Build a new hash from a string.
  *
  * This two following lines are nearly the same except the second will checks the size of the
  * string and set the hash to null if not equals to 'HASH_SIZE'.
  *
  * // 'str' is a std:string.
  * Hash h(str.data()); // Use the constructor 'Hash::Hash(const char* h)'.
  * Hash h(str); // Use this constructor, safer.
  */
Hash::Hash(const std::string& str)
{
   memcpy(this->data, static_cast<int>(str.size()) != HASH_SIZE ? NULL_HASH : str.data(), HASH_SIZE);
}

/**
  * Build a new hash from a QByteArray.
  * 'a' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is kept to 'a'.
  */
Hash::Hash(const QByteArray& a)
{
   Q_ASSERT_X(a.size() == HASH_SIZE, "Hash::Hash", QString("The given QByteArray must have a size of %1").arg(HASH_SIZE).toUtf8().constData());

   memcpy(this->data, a.isNull() || a.size() != HASH_SIZE ? NULL_HASH : a.constData(), HASH_SIZE);
}

/**
  * Return a human readable string.
  * For example : "7b6f7f3309179b97b88de3c178274b7e38343267bcdfe653c819593e".
  * @see fromStr to decode a such string.
  */
QString Hash::toStr() const
{
   QString ret(2 * HASH_SIZE, QChar());

   for (int i = 0; i < HASH_SIZE; i++)
   {
      char p1 = (this->data[i] & 0xF0) >> 4;
      char p2 = this->data[i] & 0x0F;
      ret[i*2] = p1 <= 9 ? char('0' + p1) : char('a' + (p1-10));
      ret[i*2 + 1] = p2 <= 9 ? char('0' + p2) : char('a' + (p2-10));
   }
   return ret;
}

/**
  * Return a human readable partial string, only the first 3 bytes.
  * For example : "#16bd4b".
  */
QString Hash::toStrShort() const
{
   QString ret(2 * NB_BYTES_SHORT_STR, QChar());

   for (int i = 0; i < NB_BYTES_SHORT_STR; ++i)
   {
      char p1 = (this->data[i] & 0xF0) >> 4;
      char p2 = this->data[i] & 0x0F;
      ret[i*2] = p1 <= 9 ? char('0' + p1) : char('a' + (p1-10));
      ret[i*2 + 1] = p2 <= 9 ? char('0' + p2) : char('a' + (p2-10));
   }
   return "#" % ret;
}

/**
  * Return a C Array, for example :
  * "{
  * 0x4f, 0xb9, 0x6c, 0x68,
  * 0xa4, 0xe8, 0xcd, 0x5b,
  * 0x6e, 0xb0, 0xb7, 0x44,
  * 0x36, 0x77, 0x2a, 0x6a,
  * 0x09, 0x4c, 0xa5, 0xfc,
  * 0xfc, 0x46, 0x33, 0x3a,
  * 0x30, 0xa4, 0xc1, 0x12,
  * }"
  */
QString Hash::toStrCArray() const
{
   QString str("{");
   for (int i = 0; i < HASH_SIZE; i++)
   {
      if (i % 4 == 0)
         str += "\n";
      str += QString("0x%1, ").arg((unsigned char)this->data[i], 2, 16, QLatin1Char('0'));
   }
   str += "\n}";
   return str;
}

/**
  * The special hash value with all bytes to 0 is defined as a null value.
  */
bool Hash::isNull() const noexcept
{
   return memcmp(this->data, NULL_HASH, HASH_SIZE) == 0;
}

/**
  * Return a new rand hash.
  */
Hash Hash::rand()
{
   Hash hash;
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data[i] = static_cast<char>(QRandomGenerator64::global()->bounded(256));
   return hash;
}

Hash Hash::rand(quint32 seed)
{
   QRandomGenerator64 rng(seed);
   Hash hash;
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data[i] = static_cast<char>(rng.bounded(256));
   return hash;
}

std::optional<Hash> Hash::fromStr(const QString& str)
{
   if (str.size() != 2 * HASH_SIZE)
      return std::nullopt;

   const auto hexValue = [](QChar c) -> int
   {
      const auto value = c.unicode();
      if (value >= '0' && value <= '9')
         return value - '0';
      if (value >= 'a' && value <= 'f')
         return value - 'a' + 10;
      if (value >= 'A' && value <= 'F')
         return value - 'A' + 10;
      return -1;
   };

   char bytes[HASH_SIZE];
   for (int i = 0; i < HASH_SIZE; ++i)
   {
      const int high = hexValue(str[2 * i]);
      const int low = hexValue(str[2 * i + 1]);
      if (high < 0 || low < 0)
         return std::nullopt;
      bytes[i] = static_cast<char>((high << 4) | low);
   }

   return Hash(bytes);
}

/////

/**
  * @class Common::Hasher
  *
  * To create hash from row data.
  */

Hasher::Hasher()
{
   blake3_hasher_init(&this->hasher);
}

void Hasher::addSalt(quint64 salt)
{
   QByteArray saltArray(8, 0);
   for (int i = 0; i < 8; i++)
      saltArray[i] = salt >> (8*i) & 0xFF;
   blake3_hasher_update(&this->hasher, saltArray.constData(), saltArray.length());
}

void Hasher::addData(std::span<const char> data)
{
   if (data.empty())
      return;

   blake3_hasher_update(&this->hasher, data.data(), data.size());
}

Hash Hasher::getResult()
{
   Hash result;
   blake3_hasher_finalize(&this->hasher, (uint8_t*)result.data, Hash::HASH_SIZE);
   return result;
}

void Hasher::reset()
{
   blake3_hasher_reset(&this->hasher);
}

Common::Hash Hasher::hash(const QString& str)
{
   const QByteArray data = str.toUtf8();

   Hasher hasher;
   hasher.addData(data);
   return hasher.getResult();
}

Common::Hash Hasher::hash(const Common::Hash& hash)
{
   Hasher hasher;
   hasher.addData(std::span<const char>(hash.getData(), Hash::HASH_SIZE));
   return hasher.getResult();
}

/**
  * Returns hash(str) + salt.
  */
Common::Hash Hasher::hashWithSalt(const QString& str, quint64 salt)
{
   const QByteArray data = str.toUtf8();
   Hasher hasher;
   hasher.addData(data);
   hasher.addSalt(salt);
   return hasher.getResult();
}

Common::Hash Hasher::hashWithSalt(const Common::Hash& hash, quint64 salt)
{
   Hasher hasher;
   hasher.addData(std::span<const char>(hash.getData(), Hash::HASH_SIZE));
   hasher.addSalt(salt);
   return hasher.getResult();
}

Hash Hasher::hashWithRandomSalt(const QString& str, quint64& salt)
{
   salt = QRandomGenerator64::global()->generate64();
   return Hasher::hashWithSalt(str, salt);
}

Hash Hasher::hashWithRandomSalt(const Common::Hash& hash, quint64& salt)
{
   salt = QRandomGenerator64::global()->generate64();
   return Hasher::hashWithSalt(hash, salt);
}

#endif
