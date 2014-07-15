#ifndef COMMON_SELFWEAKPOINTER_H
#define COMMON_SELFWEAKPOINTER_H

#include <QSharedPointer>
#include <QWeakPointer>

namespace Common
{
   template<typename T>
   class SelfWeakPointer
   {
   public:
      SelfWeakPointer();

      QSharedPointer<T> getStrongRef();
      QWeakPointer<T> getWeakRef();

   private:
      QWeakPointer<T> weakPointer;
   };
}

template <typename T>
Common::SelfWeakPointer<T>::SelfWeakPointer()
   : weakPointer(QSharedPointer<T>(reinterpret_cast<T*>(this)))
{
}

template <typename T>
QSharedPointer<T> Common::SelfWeakPointer<T>::getStrongRef()
{
   return this->weakPointer.toStrongRef();
}

template <typename T>
QWeakPointer<T> Common::SelfWeakPointer<T>::getWeakRef()
{
   return this->weakPointer;
}

#endif
