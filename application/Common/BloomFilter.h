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
  * Calibrated by default for n = 100'000 hashes (~6.1 TiB) and p = 0.006 (probability of false positive).
  *
  * We don't use any hash functions to compute the positions, instead we use part of the hash. See the 'position(..)' function.
  *
  * More information: http://en.wikipedia.org/wiki/Bloom_filter
  *
  * Description of the template parameters:
  *  - 'w' Is the number of bit allocated to compute a position in the filter. Thus the filter size (in bits) is 2^w. The default size (w = 20) takes 128 KiB of memory.
  *  - w * k must be lower or equal than 8 * HASH_SIZE because we extract each 'k' positions from the hash data.
  *  - 'w' must be lower or equal than 32 bits,
  *
  * @remarks (x >> 3) is used as a division by 8.
  */

namespace Common
{
   template<int v, int p>
   struct Power {
      static const int value = v * Power<v, p - 1>::value;
   };
   template<int v>
   struct Power<v, 1> {
      static const int value = v;
   };

   template<int w = 20, int n = 100000>
   class BloomFilter
   {
      static const int m =  Power<2, w>::value; // Total number of positions.
      static const int k = double(m) * 0.69314718 / double(n); // Number of positions set per hash: ln(2) * m / n.

   public:
      BloomFilter() { this->reset(); }

      inline void add(const Hash& hash);
      inline bool test(const Hash& hash) const;
      inline void reset();

   private:
      /**
        * Returns the position value of 'n',  0 <= i <= k.
        */
      inline static quint32 position(const Hash& hash, int i);

      uchar bitArray[m >> 3];
   };
}

template<int w, int n>
inline void Common::BloomFilter<w, n>::add(const Hash& hash)
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
template<int w, int n>
inline bool Common::BloomFilter<w, n>::test(const Hash& hash) const
{
   for (int i = 0; i < k; i++)
   {
      const quint32 p = position(hash, i);
      if ((this->bitArray[p >> 3] & 1 << (p % 8)) == 0)
         return false;
   }
   return true;
}

template<int w, int n>
inline void Common::BloomFilter<w, n>::reset()
{
   memset(this->bitArray, 0, sizeof(this->bitArray));
}

template<int w, int n>
inline quint32 Common::BloomFilter<w, n>::position(const Hash& hash, int i)
{
   const char* hashData = hash.getData();
   const quint32 pPos = i * w; // Position in 'hashData' of 'p'.
   const quint32* p = reinterpret_cast<const quint32*>(hashData + (pPos >> 3));
   static const quint32 wMask = (1 << w) - 1;
   return *p >> (32 - w - pPos % 8) & wMask;
}

#endif
