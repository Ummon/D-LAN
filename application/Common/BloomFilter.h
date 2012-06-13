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
  
#ifndef COMMON_BLOOMFILTER_H
#define COMMON_BLOOMFILTER_H

#include <qmath.h>

#include <Common/Hash.h>

/**
  * @class Common::BloomFilter
  * A very simple bloom filter implementation for the class 'Common::Hash'.
  * Calibrated by default for n = 100'000 hashes (~6.1 TiB of chunks) and p = 0.006 (probability of false positive).
  *
  * We don't use any hash functions to compute the positions, instead we use part of the hash. See the 'position(..)' function.
  *
  * More information: http://en.wikipedia.org/wiki/Bloom_filter
  *
  * Description of the parameters:
  *  - 'w' Is the number of bit allocated to compute a position in the filter. Thus the filter size (in bits) is 2^w. The default size (w = 20) takes 128 KiB of memory.
  *  - w * k must be lower or equal than 8 * HASH_SIZE because we extract each 'k' positions from the hash data.
  *  - 'w' must be lower or equal than 32 bits,
  *
  * @remarks (x >> 3) is used as a division by 8.
  */

namespace Common
{
   class BloomFilter
   {
   public:
      BloomFilter(int w = 20, int n = 100000) :
         w(w), m(qPow(2, w)), k(qLn(2) * m / n), wMask((1 << this->w) - 1)
      {
         this->bitArray = new uchar[this->m >> 3];
         this->reset();
      }

      ~BloomFilter()
      {
         delete[] this->bitArray;
      }

      inline void add(const Hash& hash);
      inline bool test(const Hash& hash) const;
      inline void reset();

   private:
      /**
        * Returns the position value of 'i',  0 <= i <= k.
        */
      inline quint32 position(const Hash& hash, int i) const;

      const int w;
      const int m; // Total number of positions.
      const int k; // Number of positions set per hash: ln(2) * m / n.

      const quint32 wMask;

      uchar* bitArray;
   };
}

inline void Common::BloomFilter::add(const Hash& hash)
{
   for (int i = 0; i < k; i++)
   {
      const quint32 p = position(hash, i);
      this->bitArray[p >> 3] |= 1 << (p % 8);
   }
}

/**
  * Returns 'true' if the hash may exist in the set and 'false' if the hash doesn't exist in the set.
  */
inline bool Common::BloomFilter::test(const Hash& hash) const
{
   for (int i = 0; i < k; i++)
   {
      const quint32 p = position(hash, i);
      if ((this->bitArray[p >> 3] & 1 << (p % 8)) == 0)
         return false;
   }
   return true;
}

inline void Common::BloomFilter::reset()
{
   memset(this->bitArray, 0, sizeof(uchar) * (this->m >> 3));
}

inline quint32 Common::BloomFilter::position(const Hash& hash, int i) const
{
   const char* hashData = hash.getData();
   const quint32 pPos = i * this->w; // Position in 'hashData' of 'p'.
   const quint32* p = reinterpret_cast<const quint32*>(hashData + (pPos >> 3));   
   return *p >> (32 - this->w - pPos % 8) & this->wMask;
}

#endif
