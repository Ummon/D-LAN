#ifndef COMMON_MATH_H
#define COMMON_MATH_H

namespace Common
{
   class Math
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
