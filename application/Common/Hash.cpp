#include "Hash.h"
using namespace Common;

#include <cstdlib>

#include <QtGlobal>
#include <QTime>

Hash::Hash()
{}

Hash::Hash(const char* str)
   : QByteArray(str)
{}

Hash::Hash(const QByteArray& bytes)
   : QByteArray(bytes)
{}

const QString Hash::toStr() const
{
   return QString(this->toHex().data());
}

Hash Hash::rand()
{
   Hash hash;
   hash.resize(20);
   qsrand(QTime(0,0,0).secsTo(QTime::currentTime()));
   for (int i = 0; i < 20; i++)
   {
      hash[i] = (char)(qrand() % 256);
   }
   return hash;
}
