/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
//hash functions, independent of anything lisp
#ifndef __HASH_FN_H
#define __HASH_FN_H
#include <gmp.h>
#include <stdint.h>
#include <stdio.h>
uint32_t murmur_hash(const void* key,int keylen);
#define fnv_prime_32 16777619
#define fnv_prime_64 1099511628211UL
#define fnv_prime_128 309485009821345068724781371
#define fnv_prime_256 374144419156711147060143317175368453031918731002211
#define fnv_prime_512 "35835915874844867368919076489095108449946327955754392558399825615420669938882575126094039892345713852759"
#define fnv_prime_1024 "5016456510113118655434598811035278955030765345404790744303017523831112055108147451509157692220295382716162651878526895249385292291816524375083746691371804094271873160484737966720260389217684476157468082573"
#define offset_basis_32 2166136261
#define offset_basis_64 14695981039346656037UL
#define offset_basis_128 144066263297769815596495629667062367629
#define offset_basis_256 100029257958052580907070968620625704837092796014241193945225284501741471925557
#define offset_basis_512 "9659303129496669498009435400716310466090418745672637896108374329434462657994582932197716438449813051892206539805784495328239340083876191928701583869517785"
#define offset_basis_1024 "14197795064947621068722070641403218320880622795441933960878474914617582723252296732303717722150864096521202355549365628174669108571814760471015076148029755969804077320157692458563003215304957150157403644460363550505412711285966361610267868082893823963790439336411086884584107735010676915"
uint32_t fnv_hash32(const void* key, int keylen);
uint64_t fnv_hash(const void* key,int keylen);
mpz_t* fnv_hash_mpz(const void* key, int keylen,int keysize);
uint64_t bernstein_hash(const void* key,int keylen);
uint32_t oat_hash (const void *key, int len );
static const int PEARSON_LOOKUP[256] = {
  98,  6, 85,150, 36, 23,112,164,135,207,169,  5, 26, 64,165,219, //  1
  61, 20, 68, 89,130, 63, 52,102, 24,229,132,245, 80,216,195,115, //  2
  90,168,156,203,177,120,  2,190,188,  7,100,185,174,243,162, 10, //  3
  237, 18,253,225,  8,208,172,244,255,126,101, 79,145,235,228,121, //  4
  123,251, 67,250,161,  0,107, 97,241,111,181, 82,249, 33, 69, 55, //  5
  59,153, 29,  9,213,167, 84, 93, 30, 46, 94, 75,151,114, 73,222, //  6
  197, 96,210, 45, 16,227,248,202, 51,152,252,125, 81,206,215,186, //  7
  39,158,178,187,131,136,  1, 49, 50, 17,141, 91, 47,129, 60, 99, //  8
  154, 35, 86,171,105, 34, 38,200,147, 58, 77,118,173,246, 76,254, //  9
  133,232,196,144,198,124, 53,  4,108, 74,223,234,134,230,157,139, // 10
  189,205,199,128,176, 19,211,236,127,192,231, 70,233, 88,146, 44, // 11
  183,201, 22, 83, 13,214,116,109,159, 32, 95,226,140,220, 57, 12, // 12
  221, 31,209,182,143, 92,149,184,148, 62,113, 65, 37, 27,106,166, // 13
  3, 14,204, 72, 21, 41, 56, 66, 28,193, 40,217, 25, 54,179,117, // 14
  238, 87,240,155,180,170,242,212,191,163, 78,218,137,194,175,110, // 15
  43,119,224, 71,122,142, 42,160,104, 48,247,103, 15, 11,138,239  // 16
};
uint64_t Pearson_hash16(const void *key, int keylen);
#define pearson_format_str64 "%02X%02X%02X%02X%02X%02X%02X%02X"
#define pearson_arglist_64 hh[0],hh[1],hh[2],hh[3],hh[4],hh[5],hh[6],hh[7]
#define pearson_format_str128 pearson_format_str64 pearson_format_str64
#define pearson_arglist_128 hh[0],hh[1],hh[2],hh[3],hh[4],hh[5],hh[6],hh[7],hh[8],hh[9],hh[10],hh[11],hh[12],hh[13],hh[14],hh[15]
#define pearson_format_str256 pearson_format_str128 pearson_format_str128
#define pearson_arglist_256 hh[0],hh[1],hh[2],hh[3],hh[4],hh[5],hh[6],hh[7],hh[8],hh[9],hh[10],hh[11],hh[12],hh[13],hh[14],hh[15],hh[16],hh[17],hh[18],hh[19],hh[20],hh[21],hh[22],hh[23],hh[24],hh[25],hh[26],hh[27],hh[28],hh[29],hh[30],hh[31],hh[32],hh[33],hh[34],hh[35]
#define pearson_format_str512 pearson_format_str256 pearson_format_str256
#define pearson_arglist_512 hh[0],hh[1],hh[2],hh[3],hh[4],hh[5],hh[6],hh[7],hh[8],hh[9],hh[10],hh[11],hh[12],hh[13],hh[14],hh[15],hh[16],hh[17],hh[18],hh[19],hh[20],hh[21],hh[22],hh[23],hh[24],hh[25],hh[26],hh[27],hh[28],hh[29],hh[30],hh[31],hh[32],hh[33],hh[34],hh[35],hh[36],hh[37],hh[38],hh[39],hh[40],hh[41],hh[42],hh[43],hh[44],hh[45],hh[46],hh[47],hh[48],hh[49],hh[50],hh[51],hh[52],hh[53],hh[54],hh[55],hh[56],hh[57],hh[58],hh[59],hh[60],hh[61],hh[62],hh[63]
#define pearson_format_str1024 pearson_format_str512 pearson_format_str512
#define pearson_arglist_1024 hh[0],hh[1],hh[2],hh[3],hh[4],hh[5],hh[6],hh[7],hh[8],hh[9],hh[10],hh[11],hh[12],hh[13],hh[14],hh[15],hh[16],hh[17],hh[18],hh[19],hh[20],hh[21],hh[22],hh[23],hh[24],hh[25],hh[26],hh[27],hh[28],hh[29],hh[30],hh[31],hh[32],hh[33],hh[34],hh[35],hh[36],hh[37],hh[38],hh[39],hh[40],hh[41],hh[42],hh[43],hh[44],hh[45],hh[46],hh[47],hh[48],hh[49],hh[50],hh[51],hh[52],hh[53],hh[54],hh[55],hh[56],hh[57],hh[58],hh[59],hh[60],hh[61],hh[62],hh[63],hh[64],hh[65],hh[66],hh[67],hh[68],hh[69],hh[70],hh[71],hh[72],hh[73],hh[74],hh[75],hh[76],hh[77],hh[78],hh[79],hh[80],hh[81],hh[82],hh[83],hh[84],hh[85],hh[86],hh[87],hh[88],hh[89],hh[90],hh[91],hh[92],hh[93],hh[94],hh[95],hh[96],hh[97],hh[98],hh[99],hh[100],hh[101],hh[102],hh[103],hh[104],hh[105],hh[106],hh[107],hh[108],hh[109],hh[110],hh[111],hh[112],hh[113],hh[114],hh[115],hh[116],hh[117],hh[118],hh[119],hh[120],hh[121],hh[122],hh[123],hh[124],hh[125],hh[126],hh[127]
#define pearson_hash_n(hash_size)                               \
  mpz_t* Pearson_hash_##hash_size (const void *key, int keylen){        \
    int h, i, j, k;                                             \
    uint8_t *x=(uint8_t*)key,ch;                                \
    uint8_t hh[hash_size+1];                                    \
    ch=x[0];                                                    \
    for (j=0; j<hash_size; j++) {                               \
      h=0;                                                      \
      for (i=0; i<keylen; i++) {                                \
        k=h^x[i];                                               \
        h=PEARSON_LOOKUP[k];                                    \
      }                                                         \
      hh[j]=h;                                             \
      x[0]=x[0]+1;                                              \
    }                                                           \
    x[0]=ch;                                                    \
    snprintf(hh,hash_size+1,pearson_format_str##hash_size    \
             ,pearson_arglist_##hash_size);                     \
    mpz_t *retval;                                               \
    mpz_init_set_str(*retval,hh,0);                              \
    return retval;                                              \
  }
#endif
