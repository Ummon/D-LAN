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
  
#ifndef FILEMANAGER_CONSTANTS_H
#define FILEMANAGER_CONSTANTS_H

#include <QString>

namespace FM
{
   // 2 -> 3 : BLAKE -> Sha-1
   const int FILE_CACHE_VERSION = 3;

   // When searching we don't want to send all the hashes of entries
   // because it may take a lot of memory (UDP datagram are very small).
   const int NB_MAX_HASHES_PER_ENTRY_SEARCH = 8;
}

#endif
