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

#include <QBitArray>

#include <Common/Hash.h>

namespace Common
{
   class BloomFilter
   {
      static const int w = 18; // Size of the integer used to find a position.
      static const int m = 262144; // Total number of positions: 2^18. It corresponds to a 32 KiB array.
      static const int k = 7; // Number of positions set per hash.

   public:
      BloomFilter();

      void add(const Hash& hash);
      bool test(const Hash& hash) const;
      void reset();

   private:
      /**
        * Returns the position value of 'n',  0 <= n <= k.
        */
      inline static quint32 position(const Hash& hash, int n)
      {
         const char* hashData = hash.getData();
         const int pPos = n * w; // Position in 'hashData' of 'p'.
         const quint32* p = reinterpret_cast<const quint32*>(hashData + pPos / 8);
         return *p >> (32 - w - pPos % 8) & ((1 << w) - 1);
      }

      QBitArray bitArray;
   };
}

#endif
