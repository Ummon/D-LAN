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
  
#include <priv/OccupiedPeers.h>
using namespace DM;

#include <QMutexLocker>

OccupiedPeers::OccupiedPeers()
{
}

bool OccupiedPeers::isPeerFree(PM::IPeer* peer) const
{
   QMutexLocker locker(&this->mutex);
   return !this->occupiedPeers.contains(peer);
}

bool OccupiedPeers::setPeerAsOccupied(PM::IPeer* peer)
{
   if (!peer)
      return false;

   QMutexLocker locker(&this->mutex);
   if (this->occupiedPeers.contains(peer))
      return false;

   this->occupiedPeers.insert(peer);
   return true;
}

void OccupiedPeers::setPeerAsFree(PM::IPeer* peer)
{
   if (!peer)
      return;

   {
      QMutexLocker locker(&this->mutex);
      this->occupiedPeers.remove(peer);
   }
   emit newFreePeer(peer);
}

void OccupiedPeers::newPeer(PM::IPeer* peer)
{
   if (!peer)
      return;

   this->mutex.lock();
   if (!this->occupiedPeers.contains(peer))
   {
      this->mutex.unlock();
      emit newFreePeer(peer);
   }
   else
      this->mutex.unlock();
}

int OccupiedPeers::nbOccupiedPeers() const
{
   QMutexLocker locker(&this->mutex);
   return this->occupiedPeers.size();
}

const QSet<PM::IPeer*>& OccupiedPeers::getOccupiedPeers() const
{
   QMutexLocker locker(&this->mutex);
   return this->occupiedPeers;
}

