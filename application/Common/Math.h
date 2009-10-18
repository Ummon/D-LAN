#ifndef COMMON_MATH_H
#define COMMON_MATH_H

#include "Common_global.h"

namespace Common
{
   class COMMON_EXPORT Math
   {
   public:
      /**
        * The number of k-combinations (each of size k) from a set S with n elements (size n).
        * @link http://en.wikipedia.org/wiki/Combination
        */
      static int nCombinations(int n, int k);
   };
}
#endif
