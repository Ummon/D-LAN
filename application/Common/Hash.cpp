#include <Hash.h>
using namespace Common;

#include <QtGlobal>
#include <QTime>

Hash::Hash()
{
   this->newData();
   memset(this->data->hash, 0, HASH_SIZE);
}

Hash::Hash(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker(&h.data->mutex);
#endif
   this->data = h.data;
   this->data->nbRef += 1;
}

Hash::Hash(const char* h)
{
   this->newData();
   memcpy(this->data->hash, h, HASH_SIZE);
}

Hash::Hash(const QByteArray& a)
{
   if (a.size() != HASH_SIZE)
      throw QString("The given QByteArray must have a size of %1").arg(HASH_SIZE);

   this->newData();
   memcpy(this->data->hash, a.constData(), HASH_SIZE);
}

Hash::~Hash()
{
   this->dereference();
}

Hash& Hash::operator=(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker(&h.data->mutex);
#endif
   this->dereference();
   this->data = h.data;
   this->data->nbRef += 1;
   return *this;
}

const char* Hash::getData() const
{
   return this->data->hash;
}

QString Hash::toStr() const
{
   QString ret(40);
   for (int i = 0; i < HASH_SIZE; i++)
   {
      char p1 = (this->data->hash[i] & 0xF0) >> 4;
      char p2 = this->data->hash[i] & 0x0F;
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
      hash.data->hash[i] = (char)(qrand() % 256);

   return hash;
}

void Hash::dereference()
{
#if WITH_MUTEX
   QMutexLocker(&this->data->mutex);
#endif
   this->data->nbRef -= 1;
   if (this->data->nbRef == 0)
      delete this->data;
}

void Hash::newData()
{
   this->data = new SharedData;
   this->data->nbRef = 1;
}
