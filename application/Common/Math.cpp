#include <Math.h>
using namespace Common;

int Math::nCombinations(int n, int k)
{
   if (n < 0 || k < 0)
      return 0;

   int c = 1;
   for(int i = 1; i <= k; i++)
      c = c * (n - k + i) / i;
   return c;
}
