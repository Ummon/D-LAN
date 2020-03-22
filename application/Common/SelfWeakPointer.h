#pragma once

#include <QSharedPointer>
#include <QWeakPointer>

namespace Common
{
   template<typename T>
   class SelfWeakPointer
   {
   public:
      SelfWeakPointer();
      virtual ~SelfWeakPointer() {}

      /**
        * Get the strong ref, this method can be called only once! The next strong ref will be empty.
        */
      QSharedPointer<T> grabStrongRef();

      QWeakPointer<T> getWeakRef();

   private:
      QSharedPointer<T> strongPointer;
      QWeakPointer<T> weakPointer;
   };
}

template <typename T>
Common::SelfWeakPointer<T>::SelfWeakPointer()
   : strongPointer((T*)this), weakPointer(this->strongPointer)
{
}

template <typename T>
QSharedPointer<T> Common::SelfWeakPointer<T>::grabStrongRef()
{
   QSharedPointer<T> strongPointerCopy { this->strongPointer };
   this->strongPointer.clear();
   return strongPointerCopy;
}

template <typename T>
QWeakPointer<T> Common::SelfWeakPointer<T>::getWeakRef()
{
   return this->weakPointer;
}
