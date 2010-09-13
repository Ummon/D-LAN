#include <HashesReceiver.h>

#include <QtDebug>

/**
  * @class HashesReceiver
  * The objects from this class will be able to receive the signals from
  * the class 'FM::IGetHashesResult' and print them.
  */

HashesReceiver::HashesReceiver()
   : num(0)
{
}

void HashesReceiver::nextHash(Common::Hash hash)
{
   qDebug() << this->num << " : " << hash.toStr();
   this->num++;
}

