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
  
#include <Common/BloomFilter.h>
using namespace Common;

/**
  * @class Common::BloomFilter
  * A very simple bloom filter implementation for the class 'Common::Hash'.
  * Calibrated for n = 30'000 hashes (~1.8 TiB) and p = 0.015 (probability).
  *
  * We don't use any hash functions to compute the positions, instead we use part of the hash. See the 'position(..)' function.
  *
  * More information: http://en.wikipedia.org/wiki/Bloom_filter
  *
  * Little performance measurement on an i7 @ 2.8 GHz (compiled with -02):
  *  - Time of 30'000 * 'add(..)': 1.3 ms
  *  - Time of 100'000 * 'test(..)' (with 30'000 hashes): 0.22 ms
  */

BloomFilter::BloomFilter() :
   bitArray(m)
{
}

void BloomFilter::add(const Hash& hash)
{
   for (int i = 0; i < k; i++)
      this->bitArray.setBit(position(hash, i));
}

/**
  * Returns 'true' if the hash may exist in the set and 'false' if the hash doesn't exist in the set.
  */
bool BloomFilter::test(const Hash& hash) const
{
   for (int i = 0; i < k; i++)
      if (!this->bitArray.at(position(hash, i)))
         return false;
   return true;
}

void BloomFilter::reset()
{
   this->bitArray.fill(false);
}
