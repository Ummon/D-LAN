#include "sha1_util.h"
#include "sha1.h"

#include <stdlib.h>

#define BLOCKSIZE 65536 // 64 kB

extern int 
all_hash_chunks(
   const char* path,
   size_t chunk_size,
   char (**hash_chunks)[SHA1_DIGEST_SIZE],
   int* nb_chunks
) {
   div_t nb_block_per_chunk_div_t = div(chunk_size, BLOCKSIZE);
   if (nb_block_per_chunk_div_t.rem != 0)
      return 2;
   int nb_block_per_chunk = nb_block_per_chunk_div_t.quot;
   
   FILE* file = fopen(path, "rb");
   if (!file)
      return 1;
   
   // get the file size
   fseek(file, 0, SEEK_END);
   long filesize = ftell(file);
   fseek(file, 0, SEEK_SET);
   
   // allocate memory for the hashes
   ldiv_t nb_chunks_div_t = ldiv(filesize, chunk_size);
   (*nb_chunks) = nb_chunks_div_t.quot + (nb_chunks_div_t.rem != 0 ? 1 : 0); // we add a part for the reminder
   (*hash_chunks) = malloc((*nb_chunks) * sizeof(char*));
   int i;
   
   char buffer[BLOCKSIZE + 72];
   size_t sum = 0;
   struct sha1_ctx ctx;
   int nb_block_read;
   
   int chunk_number;
   for (chunk_number = 0; chunk_number < (*nb_chunks); chunk_number++)
   {
      sha1_init_ctx (&ctx);

      for (nb_block_read = 0; nb_block_read < nb_block_per_chunk; nb_block_read++) {
         size_t n;
         for(;;) {
            n = fread(buffer + sum, 1, BLOCKSIZE - sum, file);
            sum += n;
               
            if (sum == BLOCKSIZE)
               break;
              
            if (n == 0) {
               if (ferror(file))
                  return 1;
               goto process_partial_block;
            }
               
            if (feof(file))
               goto process_partial_block;
         }
         sha1_process_block(buffer, BLOCKSIZE, &ctx);
         sum = 0;
      }
      
      process_partial_block:;
      
      if (sum > 0)
         sha1_process_bytes(buffer, sum, &ctx);

      sha1_finish_ctx(&ctx, (*hash_chunks)[chunk_number]);
   }
   
   return 0;
}