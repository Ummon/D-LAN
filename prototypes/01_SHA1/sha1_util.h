#ifndef SHA1_UTIL_H
#define SHA1_UTIL_H

#include <stdio.h>
#include <stdint.h>

#include "sha1.h"

/**
  * Compute all hash of a given file.
  * path : a local file location.
  * chunk_size : the size of the chunks in byte. For example 16Mo = 16777216.
  *              must be : 2^x | x <= 8
  * hashChunk [out] : a pointer to an array of hash.
  * Return : error code :
  *  1: file doesn't exist or unreadable.
  *  2: bad size
  */
extern int all_hash_chunks(const char* path, size_t chunk_size, char (**hash_chunks)[SHA1_DIGEST_SIZE]);

/**
  * ! Not used for the moment.
  *
  * Compute just one hash chunk.
  * path : a local file location.
  * chunk_size : the size of the chunks in byte. For example 16Mo = 16777216.
  * pos : the position of the chunk, 0 is the first one then 1, 2, ..
  * hashChunk [out] : a pointer to the result hash chunk.
  * Return : error code :
  *  1: file doesn't exist or unreadable.
  *  2: file too short.
  */
// extern int hash_chunk(const char* path, size_t chunk_size, unsigned int pos, char *(hashChunks[SHA1_DIGEST_SIZE]));

#endif