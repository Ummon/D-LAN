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
  
#include <HashesReceiver.h>

#include <QtDebug>
#include <QMutexLocker>
#include <QElapsedTimer>
#include <QTest>

/**
  * @class HashesReceiver
  *
  * The objects from this class will be able to receive the signals from
  * the class 'FM::IGetHashesResult' and print them.
  */

HashesReceiver::HashesReceiver(int numberChunksToReceive) :
   numberChunksToReceive(numberChunksToReceive),
   num(0)
{
}

/**
  * @return false if timeouted or if a hash is incorrect.
  * @param timeout [ms]
  */
bool HashesReceiver::waitToReceive(QList<Common::Hash>& expectedHashes, int timeout)
{
   // We don't use a QWaitCondition to avoid blocking the Qt main loop.
   QElapsedTimer timer;
   timer.start();

   forever
   {
      {
         QMutexLocker locker(&this->mutex);
         if (this->receivedHashes.size() == expectedHashes.size())
         {
            QListIterator<Common::Hash> i(this->receivedHashes);
            QListIterator<Common::Hash> j(expectedHashes);

            while (i.hasNext() && j.hasNext())
            {
               Common::Hash h1 = i.next();
               Common::Hash h2 = j.next();
               if (h1 != h2)
               {
                  qDebug() << "Error : " << h1.toStr() << " != " << h2.toStr();
                  return false;
               }
            }

            return true;
         }
      }

      QTest::qWait(50);

      if (timer.elapsed() > timeout)
         return false;
   }
}

void HashesReceiver::nextHash(Protos::Core::HashResult hashResult)
{
   QMutexLocker locker(&this->mutex);
   Common::Hash hash { hashResult.hash().hash() };
   qDebug() << "Hash received, chunk num:" << this->num << " hash:" << hash.toStr();
   this->receivedHashes << hash;

   this->num++;
}

