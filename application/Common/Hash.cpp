#include <Hash.h>
using namespace Common;

#include <QtGlobal>
#include <QTime>

Hash::Hash()
{
   this->newData();
   memset(this->data, 0, HASH_SIZE);
}

Hash::~Hash()
{
   this->dereference();
}

Hash::Hash(const Hash& h)
{
   this->data = h.data;
   this->data[0] += 1;
}

Hash::Hash(const char* h)
{
   this->newData();
   memcpy(this->data + 1, h, HASH_SIZE);
}

Hash::Hash(const QByteArray& a)
{
   if (a.size() != HASH_SIZE)
      throw QString("The given QByteArray must have a size of %1").arg(HASH_SIZE);

   this->newData();
   memcpy(this->data + 1, a.constData(), HASH_SIZE);
}

Hash& Hash::operator=(const Hash& h)
{
   this->dereference();
   this->data = h.data;
   this->data[0] += 1;
   return *this;
}

const char* Hash::getData() const
{
   return this->data+1;
}

QString Hash::toStr() const
{
   QString ret(40);
   for (int i = 0; i < HASH_SIZE; i++)
   {
      char p1 = (this->data[i+1] & 0xF0) >> 4;
      char p2 = this->data[i+1] & 0x0F;
      ret[i*2] = p1 <= 9 ? '0' + p1 : 'a' + (p1-10);
      ret[i*2 + 1] = p2 <= 9 ? '0' + p2 : 'a' + (p2-10);
   }
   return ret;
}

Hash Hash::rand()
{
   Hash hash;
   qsrand(QTime(0,0,0).secsTo(QTime::currentTime()));
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data[i+1] = (char)(qrand() % 256);

   return hash;
}

bool Hash::dereference()
{
   this->data[0] -= 1;
   if (this->data[0] == 0)
   {
      delete[] this->data;
      return true;
   }
   return false;
}

void Hash::newData()
{
   this->data = new char[HASH_SIZE + 1];
   this->data[0] = 1;
}
