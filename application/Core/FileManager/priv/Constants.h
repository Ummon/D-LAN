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

#include <QString>

namespace FM
{
   // When searching we don't want to send all the hashes of entries
   // because it may take a lot of memory (UDP datagram are very small).
   const int NB_MAX_HASHES_PER_ENTRY_SEARCH = 8;

   // A file being downloaded is flushed to the disk each time this amount of data has been written into one of its chunks,
   // see 'Chunk::write(..)'. A flush suspends the writes into the file: this amount takes a few milliseconds, the data
   // received meanwhile fits in the buffer of the socket and the peer doesn't have to suspend its upload. Flushing a
   // whole chunk at once takes about ten times longer than the socket can absorb.
   const int FLUSH_PERIOD = 4 * 1024 * 1024; // [byte].
}
