#include <HashesReceiver.h>

#include <QtDebug>
#include <QElapsedTimer>
#include <QTest>

/**
  * @class HashesReceiver
  * The objects from this class will be able to receive the signals from
  * the class 'FM::IGetHashesResult' and print them.
  */

HashesReceiver::HashesReceiver()
   : num(0)
{
}

/**
  * @return false if timeouted or if a hash is incorrect.
  * @param timeout [ms]
  */
bool HashesReceiver::waitToReceive(QList<Common::Hash>& hashes, int timeout)
{
   QListIterator<Common::Hash> i(this->receivedHashes);
   QListIterator<Common::Hash> j(hashes);


   QElapsedTimer timer;
   timer.start();

   forever
   {
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
      if (this->receivedHashes.size() == hashes.size())
         return true;

      QTest::qWait(50);

      if (timer.elapsed() > timeout)
         return false;
   }
}

void HashesReceiver::nextHash(Common::Hash hash)
{
   qDebug() << this->num << " : " << hash.toStr();
   this->receivedHashes << hash;
   this->num++;
}

