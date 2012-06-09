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

#include <Common/Hash.h>

/**
  * @class Common::BloomFilter
  * A very simple bloom filter implementation for the class 'Common::Hash'.
  * Calibrated for n = 100'000 hashes (~6.1 TiB) and p = 0.006 (probability of false positive).
  *
  * We don't use any hash functions to compute the positions, instead we use part of the hash. See the 'position(..)' function.
  *
  * More information: http://en.wikipedia.org/wiki/Bloom_filter
  *
  * @remarks (x >> 3) is used as a division by 8.
  */

namespace Common
{
   class BloomFilter
   {
      static const int w = 20; // Size of the integer used to find a position.
      static const int m = 1048576; // Total number of positions: 2^20. It corresponds to a 128 KiB array.
      static const int k = 7; // Number of positions set per hash.

   public:
      BloomFilter() { this->reset(); }

      inline void add(const Hash& hash);
      inline bool test(const Hash& hash) const;
      inline void reset();

   private:
      /**
        * Returns the position value of 'n',  0 <= n <= k.
        */
      inline static quint32 position(const Hash& hash, int n);

      uchar bitArray[m >> 3];
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
   memset(this->bitArray, 0, sizeof(this->bitArray));
}

inline quint32 Common::BloomFilter::position(const Hash& hash, int n)
{
   const char* hashData = hash.getData();
   const quint32 pPos = n * w; // Position in 'hashData' of 'p'.
   const quint32* p = reinterpret_cast<const quint32*>(hashData + (pPos >> 3));
   static const quint32 wMask = (1 << w) - 1;
   return *p >> (32 - w - pPos % 8) & wMask;
}

#endif
