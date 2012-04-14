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
using namespace Common;

#include <QtGlobal>
#include <QTime>

/**
  * @class Common::Hash
  *
  * An Über-optimized hash.
  */

MTRand Hash::mtrand;

/**
  * Build a new empty hash, its value is set to 0.
  */
Hash::Hash()
{
   this->newData();
   memset(this->data->hash, 0, HASH_SIZE);
}

/**
  * Build a new hash from an existing one.
  * The data are shared between both.
  */
Hash::Hash(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker locker(&h.data->mutex);
#endif
   this->data = h.data;
   this->data->nbRef += 1;
}

/**
  * Build a new hash from a char*, 'h' is not a readable string, @see fromStr.
  * 'h' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is keept to 'h'.
  */
Hash::Hash(const char* h)
{
   Q_ASSERT(h);

   this->newData();
   memcpy(this->data->hash, h, HASH_SIZE);
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
   this->newData();
   if (static_cast<int>(str.size()) != HASH_SIZE)
      memset(this->data->hash, 0, HASH_SIZE);
   else
      memcpy(this->data->hash, str.data(), HASH_SIZE);
}

/**
  * Build a new hash from a QByteArray.
  * 'a' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is keept to 'a'.
  */
Hash::Hash(const QByteArray& a)
{   
   Q_ASSERT_X(a.size() == HASH_SIZE, "Hash::Hash", QString("The given QByteArray must have a size of %1").arg(HASH_SIZE).toUtf8().constData());

   this->newData();
   if (a.size() != HASH_SIZE)
      memset(this->data->hash, 0, HASH_SIZE);
   else
      memcpy(this->data->hash, a.constData(), HASH_SIZE);
}

/**
  * Remove its reference to the shared data, if its the last
  * then delete the data.
  */
Hash::~Hash()
{
   this->dereference();
}

/**
  * Assign the data of a another hash.
  * The data are shared.
  */
Hash& Hash::operator=(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker locker(&h.data->mutex);
#endif
   if (&h != this)
   {
      this->dereference();
      this->data = h.data;
      this->data->nbRef += 1;
   }
   return *this;
}

/**
  * Return a human readable string.
  * For example : 16bd4b1e656129eb9ddaa2ce0f0705f1cc161f77.
  * @see fromStr to decode a such string.
  */
QString Hash::toStr() const
{
   QString ret(2 * HASH_SIZE);
   for (int i = 0; i < HASH_SIZE; i++)
   {
      char p1 = (this->data->hash[i] & 0xF0) >> 4;
      char p2 = this->data->hash[i] & 0x0F;
      ret[i*2] = p1 <= 9 ? '0' + p1 : 'a' + (p1-10);
      ret[i*2 + 1] = p2 <= 9 ? '0' + p2 : 'a' + (p2-10);
   }
   return ret;
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
      str += QString("0x%1, ").arg((unsigned char)this->data->hash[i], 2, 16, QLatin1Char('0'));
   }
   str += "\n}";
   return str;
}

/**
  * The special hash value with all bytes to 0 is defined as a null value.
  */
bool Hash::isNull() const
{
   for (int i = 0; i < HASH_SIZE; i++)
      if (this->data->hash[i] != 0)
         return false;
   return true;
}

/**
  * Return a new rand hash.
  */
Hash Hash::rand()
{
   Hash hash;
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data->hash[i] = static_cast<char>(Hash::mtrand.randInt(255));
   return hash;
}

Hash Hash::rand(quint32 seed)
{
   MTRand mtrand(seed);
   Hash hash;
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data->hash[i] = static_cast<char>(mtrand.randInt(255));
   return hash;
}

Hash Hash::fromStr(const QString& str)
{
   Q_ASSERT_X(str.size() == 2 * HASH_SIZE, "Hash::fromStr", "The string representation of an hash must have twice as character as the size (in byte) of the hash.");

   Hash hash;
   QString strLower = str.toLower();

   for (int i = 0; i < HASH_SIZE && 2*i + 1 < strLower.size(); i++)
   {
      char c1 = strLower[2*i].toAscii();
      char c2 = strLower[2*i + 1].toAscii();

      char p1 = c1 <= '9' ? c1 - '0' : c1 - 'a' + 10;
      char p2 = c2 <= '9' ? c2 - '0' : c2 - 'a' + 10;

      hash.data->hash[i] = (p1 << 4 & 0xF0) | (p2 & 0x0F);
   }

   return hash;
}

/////

/**
  * @class Common::Hasher
  *
  * To create hash from row data.
  */

MTRand Hasher::mtrand;

Hasher::Hasher() :
   cryptographicHash(QCryptographicHash::Sha1)
{
   this->reset();
}

/**
  * Deprecated, it's useless to have a hardcoded salt.
  *
  * May be called right after the constructor or the 'reset()' method.
  * @param salt Must be Hash::HASH_SIZE bytes length.
  */
/*void Hasher::addPredefinedSalt()
{
   static const char salt[] = {
      -0x46, -0x1B,  0x4D, -0x0E,
      -0x55, -0x7C, -0x4B, -0x32,
      -0x07, -0x66,  0x6C,  0x78,
      -0x7a,  0x7f,  0x6d,  0x7B,
      -0x35, -0x24, -0x3F,  0x6A,
      -0x1A,  0x03, -0x66,  0x3c,
       0x75, -0x36,  0x4d, -0x24,
      -0x70,  0x6A,  0x10,  0x11
   };

   this->cryptographicHash.addData(salt, sizeof(salt));
}*/

void Hasher::addSalt(quint64 salt)
{
   QByteArray saltArray(8, 0);
   for (int i = 0; i < 8; i++)
      saltArray[i] = salt >> (8*i) & 0xFF;
   this->cryptographicHash.addData(saltArray);
}

/**
  * @param size In bytes.
  */
void Hasher::addData(const char* data, int size)
{
   Q_ASSERT(data);
   Q_ASSERT(size >= 0);

   this->cryptographicHash.addData(data, size);
}

Hash Hasher::getResult()
{
   Hash result;
   memcpy(result.data->hash, this->cryptographicHash.result().constData(), Hash::HASH_SIZE);
   return result;
}

void Hasher::reset()
{
   this->cryptographicHash.reset();
}

Common::Hash Hasher::hash(const QString& str)
{
   const QByteArray data = str.toUtf8();

   Hasher hasher;
   hasher.addData(data.constData(), data.size());
   return hasher.getResult();
}

Common::Hash Hasher::hash(const Common::Hash& hash)
{
   Hasher hasher;
   hasher.addData(hash.getData(), Hash::HASH_SIZE);
   return hasher.getResult();
}

/**
 * Returns hash(str) + salt.
 */
Common::Hash Hasher::hashWithSalt(const QString& str, quint64 salt)
{
   const QByteArray data = str.toUtf8();
   Hasher hasher;
   hasher.addData(data.constData(), data.size());
   hasher.addSalt(salt);
   return hasher.getResult();
}

Common::Hash Hasher::hashWithSalt(const Common::Hash& hash, quint64 salt)
{
   Hasher hasher;
   hasher.addData(hash.getData(), Hash::HASH_SIZE);
   hasher.addSalt(salt);
   return hasher.getResult();
}

Hash Hasher::hashWithRandomSalt(const QString& str, quint64& salt)
{
   salt = static_cast<quint64>(Hash::mtrand.randInt()) << 32 | Hash::mtrand.randInt();
   return Hasher::hashWithSalt(str, salt);
}

Hash Hasher::hashWithRandomSalt(const Common::Hash& hash, quint64& salt)
{
   salt = static_cast<quint64>(Hash::mtrand.randInt()) << 32 | Hash::mtrand.randInt();
   return Hasher::hashWithSalt(hash, salt);
}
