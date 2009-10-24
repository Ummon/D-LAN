#ifndef COMMON_DELETABLE_H
#define COMMON_DELETABLE_H

#include <exception>
using namespace std;

namespace Common
{
   class DeletedException : public exception {};

   class Deletable
   {
   public:
      Deletable() : deleted(false) {}
      virtual ~Deletable() {}

      virtual void setDeleted() { this->deleted = true; }
      virtual bool isDeleted() { return this->deleted; }

   protected:
      virtual void checkDeleted() { if (this->deleted) throw DeletedException(); }

   private:
      bool deleted;
   };
}

#endif
