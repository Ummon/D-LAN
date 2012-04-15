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
  
#ifndef DOWNLOADMANAGER_OCCUPIEDPEERS_H
#define DOWNLOADMANAGER_OCCUPIEDPEERS_H

#include <QObject>
#include <QSet>
#include <QMutex>

#include <Common/Uncopyable.h>

namespace PM
{
   class IPeer;
}

namespace DM
{
   class OccupiedPeers : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      OccupiedPeers();

      bool isPeerFree(PM::IPeer* peer) const;
      bool setPeerAsOccupied(PM::IPeer* peer);
      void setPeerAsFree(PM::IPeer* peer);
      void newPeer(PM::IPeer* peer);
      int nbOccupiedPeers() const;
      const QSet<PM::IPeer*>& getOccupiedPeers() const;

   signals:
      void newFreePeer(PM::IPeer*);

   private:
      QSet<PM::IPeer*> occupiedPeers; // Peers currently occupied.
      mutable QMutex mutex;
   };
}

#endif
