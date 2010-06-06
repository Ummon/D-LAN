#include <Hash.h>
using namespace Common;

#include <QtGlobal>
#include <QTime>

/**
  * @class Hash
  * An uber-optimized SHA-1 hash.
  * see : http://fr.wikipedia.org/wiki/SHA-1
  */

/**
  * Build a new empty hash, its value is set to 0.
  */
Hash::Hash()
{
   this->newData();
   memset(this->data->hash, 0, HASH_SIZE);
}

/**
  * Build a new hash from an existing one.
  * The data are shared between both.
  */
Hash::Hash(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker locker(&h.data->mutex);
#endif
   this->data = h.data;
   this->data->nbRef += 1;
}

/**
  * Build a new hash from a char*, 'h' is not a readable string, @see fromStr.
  * 'h' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is keept to 'h'.
  */
Hash::Hash(const char* h)
{
   this->newData();
   memcpy(this->data->hash, h, HASH_SIZE);
}

/**
  * Build a new hash from a QByteArray.
  * 'a' must have a length equal or bigger to HASH_SIZE!
  * The data are copied, no pointer is keept to 'a'.
  */
Hash::Hash(const QByteArray& a)
{
   if (a.size() != HASH_SIZE)
      throw QString("The given QByteArray must have a size of %1").arg(HASH_SIZE);

   this->newData();
   memcpy(this->data->hash, a.constData(), HASH_SIZE);
}

/**
  * Remove its reference to the shared data, if its the last
  * then delete the data.
  */
Hash::~Hash()
{
   this->dereference();
}

/**
  * Assign the data of a another hash.
  * The data are shared.
  */
Hash& Hash::operator=(const Hash& h)
{
#if WITH_MUTEX
   QMutexLocker locker(&h.data->mutex);
#endif
   if (&h != this)
   {
      this->dereference();
      this->data = h.data;
      this->data->nbRef += 1;
   }
   return *this;
}

/**
  * Return a pointer to its internal data.
  * The length of the returned value is exactly HASH_SIZE.
  */
const char* Hash::getData() const
{
   return this->data->hash;
}

/**
  * Return a human readable string.
  * For example : 16bd4b1e656129eb9ddaa2ce0f0705f1cc161f77.
  * @see fromStr to decode a such string.
  */
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

bool Hash::isNull() const
{
   for (int i = 0; i < HASH_SIZE; i++)
      if (this->data->hash[i] != 0)
         return false;
   return true;
}

/**
  * Return a new rand hash.
  */
Hash Hash::rand()
{
   Hash hash;
   qsrand(QTime(0,0,0).msecsTo(QTime::currentTime()));
   for (int i = 0; i < HASH_SIZE; i++)
      hash.data->hash[i] = (char)(qrand() % 256);

   return hash;
}

Hash Hash::fromStr(const QString& str)
{
   Hash hash;
   QString strLower = str.toLower();

   for (int i = 0; i < HASH_SIZE; i++)
   {
      char c1 = strLower[i*2].toAscii();
      char c2 = strLower[i*2 + 1].toAscii();

      char p1 = c1 <= '9' ? c1 - '0' : c1 - 'a' + 10;
      char p2 = c2 <= '9' ? c2 - '0' : c2 - 'a' + 10;

      hash.data->hash[i] = (p1 << 4 & 0xF0) | (p2 & 0x0F);
   }

   return hash;
}

/**
  * Dereference the pointed data.
  */
void Hash::dereference()
{
#if WITH_MUTEX
   QMutexLocker locker(&this->data->mutex);
#endif
   this->data->nbRef -= 1;
   if (this->data->nbRef == 0)
      delete this->data;
}

/**
  * Allocated new shared data.
  */
void Hash::newData()
{
   this->data = new SharedData;
   this->data->nbRef = 1;
}
