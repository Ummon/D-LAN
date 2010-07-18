#ifndef UNCOPYABLE_H
#define UNCOPYABLE_H

namespace Common
{
   class Uncopyable
   {
   protected:
      Uncopyable() {}
      ~Uncopyable() {}

   private:
      Uncopyable(const Uncopyable&);
      Uncopyable& operator=(const Uncopyable&);
   };
}

#endif
