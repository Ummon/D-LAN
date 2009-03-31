#include "sha1_util.h"
#include <stdio.h>

#include <math.h>
#include <stdlib.h>

int main(int argc, char** argv) {
   if (argc < 2)
   {
      printf("Usage : %s <file>\n", argv[0]);
      return 0;
   }
   
   char (*hashes)[SHA1_DIGEST_SIZE];
   int n;
   
   // 2097152 = 2 Mo
   int res = all_hash_chunks(argv[1], 2097152, &hashes, &n);
   
   if (res)
   {
      printf("Error : %i\n", res);
      return res;
   }
   
   printf("nb part : %i\n", n);
   
   int i = 0;
   for (i = 0; i < n; i++)
   {
      int j = 0;
      for (j = 0; j < SHA1_DIGEST_SIZE; j++)
      {
         printf("%02x", hashes[i][j] & 0xff);
      }
      printf("\n");
   }
      
   return 0;
}