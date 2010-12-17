
#define NB_ROUNDS32 10
#define NB_ROUNDS64 14

/*
  32-bit type (for most systems, including NIST's test machine)
*/
typedef unsigned int u32;

/*
  64-bit type (for most systems, including NIST's test machine)
*/
typedef unsigned long long u64;

/*
  type for raw data
*/
typedef unsigned char BitSequence; 

/* 
  64-bit word 
*/
typedef unsigned long long DataLength; 


/*
  byte-to-word conversion and vice-versa (little endian)  
*/
#define U8TO32_BE(p) \
  (((u32)((p)[0]) << 24) | \
   ((u32)((p)[1]) << 16) | \
   ((u32)((p)[2]) <<  8) | \
   ((u32)((p)[3])      ))

#define U8TO64_BE(p) \
  (((u64)U8TO32_BE(p) << 32) | (u64)U8TO32_BE((p) + 4))

#define U32TO8_BE(p, v) \
  do { \
    (p)[0] = (BitSequence)((v) >> 24);  \
    (p)[1] = (BitSequence)((v) >> 16); \
    (p)[2] = (BitSequence)((v) >>  8); \
    (p)[3] = (BitSequence)((v)      ); \
  } while (0)

#define U64TO8_BE(p, v) \
  do { \
    U32TO8_BE((p),     (u32)((v) >> 32));	\
    U32TO8_BE((p) + 4, (u32)((v)      ));	\
  } while (0)

/*
  error codes
*/
typedef enum { SUCCESS=0, FAIL=1, BAD_HASHBITLEN=2  } HashReturn;

/* 
   hash structure
*/
typedef struct  { 
  int hashbitlen;  /* length of the hash value (bits) */
  int datalen;     /* amount of remaining data to hash (bits) */
  int init;        /* set to 1 when initialized */
  int nullt;       /* Boolean value for special case \ell_i=0 */
  /*
    variables for the 32-bit version  
  */
  u32 h32[8];         /* current chain value (initialized to the IV) */
  u32 t32[2];         /* number of bits hashed so far */
  BitSequence data32[64];     /* remaining data to hash (less than a block) */
  u32 salt32[4];      /* salt (null by default) */
  /*
    variables for the 64-bit version  
  */
  u64 h64[8];      /* current chain value (initialized to the IV) */
  u64 t64[2];      /* number of bits hashed so far */
  BitSequence data64[128];  /* remaining data to hash (less than a block) */
  u64 salt64[4];   /* salt (null by default) */
} hashState;

/*
  load the hashSate structure (copy hashbitlen...)

  INPUT
  state: structure that holds the hashState information
  hashbitlen: length of the hash output

  OUTPUT
  SUCCESS on success
  BAD_HASHBITLEN if hashbitlen invalid
*/
HashReturn Init( hashState * state, int hashbitlen );

/*
  adds a salt to the hash function (OPTIONAL)
  should be called AFTER Init, and BEFORE Update

  INPUT
  state: hashSate structure
  salt: the salt, whose length is determined by hashbitlen

  OUTPUT
  SUCCESS on success
 */
HashReturn AddSalt( hashState * state, const BitSequence * salt );

/*
  update the state (chain value) with new data, storing overhead data if necessary

  INPUT
  state: hashState structure
  data: data to hash
  databitlen: bit length of the data (not bytes!)

  OUTPUT
  SUCCESS on success
*/
HashReturn Update( hashState * state, const BitSequence * data, DataLength databitlen );

/*
  finalize the hash, hashing remaining data and padding the message

  INPUT
  state: hashState structure
  hashval: storage for the hash value

  OUTPUT
  SUCCESS on success
*/
HashReturn Final( hashState * state, BitSequence * hashval );

/*
  all-in-once function

  INPUT
  cf. above functions

  OUTPUT
  SUCCESS on success
  FAIL if arbitrary failure
  BAD_HASHBITLEN if invalid hashbitlen
*/
HashReturn Hash( int hashbitlen, const BitSequence * data, DataLength databitlen, 
		 BitSequence * hashval );

/*
  the 10 permutations of {0,...15}
*/
static const unsigned char sigma[][16] = {
    {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 } ,
    { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 } ,
    { 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 } ,
    {  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 } ,
    {  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 } ,
    {  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 } ,
    { 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 } ,
    { 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 } ,
    {  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 } ,
    { 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 }, 
    {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 } ,
    { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 } ,
    { 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 } ,
    {  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 } ,
    {  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 } ,
    {  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 } ,
    { 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 } ,
    { 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 } ,
    {  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 } ,
    { 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 }  
  };

/*
  constants for BLAKE-32 and BLAKE-28
*/
static const u32 c32[16] = {
    0x243F6A88, 0x85A308D3,
    0x13198A2E, 0x03707344,
    0xA4093822, 0x299F31D0,
    0x082EFA98, 0xEC4E6C89,
    0x452821E6, 0x38D01377,
    0xBE5466CF, 0x34E90C6C,
    0xC0AC29B7, 0xC97C50DD,
    0x3F84D5B5, 0xB5470917 
};

/*
  constants for BLAKE-64 and BLAKE-48
*/
static const u64 c64[16] = {
  0x243F6A8885A308D3ULL,0x13198A2E03707344ULL,
  0xA4093822299F31D0ULL,0x082EFA98EC4E6C89ULL,
  0x452821E638D01377ULL,0xBE5466CF34E90C6CULL,
  0xC0AC29B7C97C50DDULL,0x3F84D5B5B5470917ULL,
  0x9216D5D98979FB1BULL,0xD1310BA698DFB5ACULL,
  0x2FFD72DBD01ADFB7ULL,0xB8E1AFED6A267E96ULL,
  0xBA7C9045F12C7F99ULL,0x24A19947B3916CF7ULL,
  0x0801F2E2858EFC16ULL,0x636920D871574E69ULL
};

/*
  padding data
*/
static const BitSequence padding[129] =
  {
    0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/*
  initial values ( IVx for BLAKE-x)
*/
static const u32 IV32[8]={
  0x6A09E667, 0xBB67AE85,
  0x3C6EF372, 0xA54FF53A,
  0x510E527F, 0x9B05688C,
  0x1F83D9AB, 0x5BE0CD19
};
static const u32 IV28[8]={
  0xC1059ED8, 0x367CD507,
  0x3070DD17, 0xF70E5939,
  0xFFC00B31, 0x68581511,
  0x64F98FA7, 0xBEFA4FA4
};
static const u64 IV48[8]={
  0xCBBB9D5DC1059ED8ULL, 0x629A292A367CD507ULL,
  0x9159015A3070DD17ULL, 0x152FECD8F70E5939ULL,
  0x67332667FFC00B31ULL, 0x8EB44A8768581511ULL,
  0xDB0C2E0D64F98FA7ULL, 0x47B5481DBEFA4FA4ULL
};
static const u64 IV64[8]={
  0x6A09E667F3BCC908ULL, 0xBB67AE8584CAA73BULL,
  0x3C6EF372FE94F82BULL, 0xA54FF53A5F1D36F1ULL,
  0x510E527FADE682D1ULL, 0x9B05688C2B3E6C1FULL,
  0x1F83D9ABFB41BD6BULL, 0x5BE0CD19137E2179ULL
};

