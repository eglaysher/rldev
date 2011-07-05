
/*
   Kprl: RealLive compressor.
   Copyright (C) 2006 Haeleth
   Revised 2009-2011 by Richard 23

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
*/

#include "lzcomp.h"
#include "stdio.h"
extern "C" {
#include "rldev.h"
    
/* RealLive uses a rather basic XOR encryption scheme, to which this
 * is the key. */
static uchar xor_mask[] = {
    0x8b, 0xe5, 0x5d, 0xc3, 0xa1, 0xe0, 0x30, 0x44, 0x00, 0x85, 0xc0, 0x74,
    0x09, 0x5f, 0x5e, 0x33, 0xc0, 0x5b, 0x8b, 0xe5, 0x5d, 0xc3, 0x8b, 0x45,
    0x0c, 0x85, 0xc0, 0x75, 0x14, 0x8b, 0x55, 0xec, 0x83, 0xc2, 0x20, 0x52,
    0x6a, 0x00, 0xe8, 0xf5, 0x28, 0x01, 0x00, 0x83, 0xc4, 0x08, 0x89, 0x45,
    0x0c, 0x8b, 0x45, 0xe4, 0x6a, 0x00, 0x6a, 0x00, 0x50, 0x53, 0xff, 0x15,
    0x34, 0xb1, 0x43, 0x00, 0x8b, 0x45, 0x10, 0x85, 0xc0, 0x74, 0x05, 0x8b,
    0x4d, 0xec, 0x89, 0x08, 0x8a, 0x45, 0xf0, 0x84, 0xc0, 0x75, 0x78, 0xa1,
    0xe0, 0x30, 0x44, 0x00, 0x8b, 0x7d, 0xe8, 0x8b, 0x75, 0x0c, 0x85, 0xc0,
    0x75, 0x44, 0x8b, 0x1d, 0xd0, 0xb0, 0x43, 0x00, 0x85, 0xff, 0x76, 0x37,
    0x81, 0xff, 0x00, 0x00, 0x04, 0x00, 0x6a, 0x00, 0x76, 0x43, 0x8b, 0x45,
    0xf8, 0x8d, 0x55, 0xfc, 0x52, 0x68, 0x00, 0x00, 0x04, 0x00, 0x56, 0x50,
    0xff, 0x15, 0x2c, 0xb1, 0x43, 0x00, 0x6a, 0x05, 0xff, 0xd3, 0xa1, 0xe0,
    0x30, 0x44, 0x00, 0x81, 0xef, 0x00, 0x00, 0x04, 0x00, 0x81, 0xc6, 0x00,
    0x00, 0x04, 0x00, 0x85, 0xc0, 0x74, 0xc5, 0x8b, 0x5d, 0xf8, 0x53, 0xe8,
    0xf4, 0xfb, 0xff, 0xff, 0x8b, 0x45, 0x0c, 0x83, 0xc4, 0x04, 0x5f, 0x5e,
    0x5b, 0x8b, 0xe5, 0x5d, 0xc3, 0x8b, 0x55, 0xf8, 0x8d, 0x4d, 0xfc, 0x51,
    0x57, 0x56, 0x52, 0xff, 0x15, 0x2c, 0xb1, 0x43, 0x00, 0xeb, 0xd8, 0x8b,
    0x45, 0xe8, 0x83, 0xc0, 0x20, 0x50, 0x6a, 0x00, 0xe8, 0x47, 0x28, 0x01,
    0x00, 0x8b, 0x7d, 0xe8, 0x89, 0x45, 0xf4, 0x8b, 0xf0, 0xa1, 0xe0, 0x30,
    0x44, 0x00, 0x83, 0xc4, 0x08, 0x85, 0xc0, 0x75, 0x56, 0x8b, 0x1d, 0xd0,
    0xb0, 0x43, 0x00, 0x85, 0xff, 0x76, 0x49, 0x81, 0xff, 0x00, 0x00, 0x04,
    0x00, 0x6a, 0x00, 0x76
};

/* In some new titles, a second round of XORing is performed on a
 * block of uncompressed bytecode, using the following 16-byte key: */
/*static uchar xor_mask_2[] = {
    0xa8, 0x28, 0xfd, 0x66, 0xa0, 0x23, 0x77, 0x69, 0xf9, 0x45, 0xf8, 0x2c,
    0x7c, 0x00, 0xad, 0xf4
};*/
// static uchar xor_mask_2[] = {
    // 0xAF, 0x2F, 0xFB, 0x6B, 0xAF, 0x30, 0x77, 0x17, 0x87, 0x48, 0xFE, 0x2C,
    // 0x68, 0x1A, 0xB9, 0xF0
// };

struct Xor_Key {
  char key[16];
  int offset;
  int length;
};

// typedef Xor_Key Rl_Key[];

// Per game xor keys to be passed to decompress, terminated with -1 offset
// entries.

/* In some new titles, a second round of XORing is performed on a block of
 * uncompressed bytecode. The keys appear to be on a game-by-game basis. */

// Clannad Full Voice

Xor_Key CFV_key[] = {
    { { 0xaf, 0x2f, 0xfb, 0x6b, 0xaf, 0x30, 0x77, 0x17,
        0x87, 0x48, 0xfe, 0x2c, 0x68, 0x1a, 0xb9, 0xf0 }, 256, 257 },
    { { 0x00 }, 0, 0 }
};

// Little Busters!

Xor_Key LB_key[] = {
    { { 0xa8, 0x28, 0xfd, 0x66, 0xa0, 0x23, 0x77, 0x69,
        0xf9, 0x45, 0xf8, 0x2c, 0x7c, 0x00, 0xad, 0xf4 }, 256, 257 },
    { { 0x00 }, 0, 0 }
};

// Little Busters! EX and ME (Memorial Edition)

Xor_Key LBEX_key[] = {
    { { 0xa8, 0x28, 0xfd, 0x71, 0xb4, 0x23, 0x64, 0x15,
        0x96, 0x48, 0x8a, 0x43, 0x62, 0x0e, 0xad, 0xf0 }, 256, 128 },
    { { 0xde, 0xd9, 0x4a, 0x18, 0xaf, 0x23, 0x1d, 0x9a,
        0xac, 0x23, 0x25, 0x48, 0xd8, 0xd4, 0x8f, 0xa7 }, 384, 128 },
    { { 0xde, 0xf1, 0xb7, 0x69, 0x1b, 0x00, 0x79, 0x8f,
        0x3a, 0x6b, 0xaf, 0x0b, 0xba, 0xda, 0x22, 0x57 }, 512, 16 },
    { { 0x76, 0xf1, 0xb7, 0x69, 0x1b, 0x00, 0x79, 0x8f,
        0x3a, 0x6b, 0xaf, 0x0b, 0xba, 0xda, 0x22, 0x57 }, 528, 113 },
    { { 0x00 }, 0, 0 }
};


// Five by RAM

Xor_Key FIVE_key[] = {
    { { 0xe5, 0xe8, 0x20, 0xe8, 0x6e, 0x91, 0xb4, 0xb1,
        0x4b, 0xc5, 0x34, 0x9e, 0xad, 0x2c, 0x71, 0x32 }, 256, 128 },
    { { 0x4a, 0x05, 0xad, 0x8b, 0xa4, 0xa9, 0x89, 0x8d,
        0xd4, 0xe9, 0x87, 0xf8, 0xee, 0x2e, 0x99, 0x65 }, 384, 128 },
    { { 0x4a, 0xed, 0x8d, 0x63, 0xca, 0x38, 0x3d, 0x3c,
        0x9f, 0x2c, 0xb3, 0x66, 0x43, 0x02, 0xe8, 0x57 }, 512, 16 },
    { { 0xaf, 0xed, 0x8d, 0x63, 0xca, 0x38, 0x3d, 0x3c,
        0x9f, 0x2c, 0xb3, 0x66, 0x43, 0x02, 0xe8, 0x57 }, 528, 113 },
    { { 0x00 }, 0, 0 }
};

// Snow Standard Edition

Xor_Key SNOW_key[] = {
    { { 0xe4, 0xab, 0xa2, 0xc9, 0xec, 0x39, 0x36, 0x62,
        0xc9, 0x03, 0xba, 0x6d, 0x2e, 0x9c, 0xf2, 0x64 }, 256, 257 },
    { { 0x0 }, 0, 0 }
};

Xor_Key* keys[] = {
    CFV_key, LB_key, LBEX_key, FIVE_key, SNOW_key
};

/* Decrypt an "encrypted" file */
value rl_prim_apply_mask (value array, value origin)
{
    CAMLparam2(array, origin);
    uchar i = 0;
    uchar *start = Binarray_val(array) + Long_val(origin);
    uchar *end = Binarray_val(array) + Bigarray_val(array)->dim[0];

//    printf("rl_prim_apply_mask at %d\n", Long_val(origin));
    
    while (start < end) *start++ ^= xor_mask[i++];

//  while (start < end) *start++ = 0x11;
    
    CAMLreturn(Val_unit);
}

/* Decompress an archived file. */
/*
// value rl_prim_decompress (value src_in, value dst_in, value use_xor_2, value key)
// value rl_prim_decompress (value src_in, value dst_in, XorKey *key)
value rl_prim_decompress (value src_in, value dst_in, value key_id)
// value rl_prim_decompress (value src_in, value dst_in, value key_id, value xor_key)
{
//  CAMLparam4(src_in, dst_in, use_xor_2, key);
    CAMLparam3(src_in, dst_in, key_id);
//    CAMLparam4(src_in, dst_in, key_id, xor_key);
    
//    uchar *xor_mask_2 = Binarray_val(key);
    int bit = 1;
    uchar *src = Binarray_val(src_in);
    uchar *dststart = Binarray_val(dst_in);
    uchar *dst = dststart;
    uchar *dstend = dststart + Bigarray_val(dst_in)->dim[0];
    uchar *srcend = src + Bigarray_val(src_in)->dim[0];
    uchar flag;
    int keyid = Long_val(key_id);
    
    Xor_Key *key = 0;
    
//    int xor_len = 

//    if(Is_block(xor_key)) printf("is block\n");
    
//    if(keyid) {
//        key = keys[keyid - 1];
//    }

//    printf("keyid: %d\n", keyid);
    
    if(keyid > -1) {
        key = keys[keyid];
    }
    
//    key = LB_key;
    
    src += 8;
    flag = *src++;
    while (src < srcend && dst < dstend) {
        if (bit == 256) {
            bit = 1;
            flag = *src++;
        }
        if (flag & bit) {
            *dst++ = *src++;
        }
        else {
            int i, count;
            uchar *repeat;
            count = *src++;
            count += (*src++) << 8;
            repeat = dst - ((count >> 4) - 1) - 1;
            count = (count & 0x0f) + 2;
            if (repeat < dststart || repeat >= dst)
                failwith ("corrupt data");
            for (i = 0; i < count; i++) {
                *dst++ = *repeat++;
            }
        }
        bit <<= 1;
    }
    / *
    if (0 && Bool_val(use_xor_2)) {
        dst = dststart + 256;
        for (int i = 0; i < 257; ++i) {
            if(dst > dstend)
                break;
            *dst++ ^= xor_mask_2[i % 16];
        }
    }
    * /
/ *
     dst = dststart + 256;
    
    for(int j = 0; j < 257; ++j) {
        if(dst > dstend) break;
        *dst++ = 0xff;
    }
* /

//    if(keyid && key) {
    if(key) {
        for(; key->length; key++) {
        //    printf("key offset: %d\n", key->offset);
        //    printf("key length: %d\n", key->length);
            dst = dststart + key->offset;
            for (int i = 0; i < key->length && dst < dstend; ++i) {

                *dst++ ^= key->key[i % 16];

//                *dst++ = 0x22;
            }
        }
    }

    CAMLreturn(Val_unit);
}
*/


/* Decompress an archived file. */
value rl_prim_decompress (value src_in, value dst_in)
{
    CAMLparam2(src_in, dst_in);

    int bit = 1;
    uchar *src = Binarray_val(src_in);
    uchar *dststart = Binarray_val(dst_in);
    uchar *dst = dststart;
    uchar *dstend = dststart + Bigarray_val(dst_in)->dim[0];
    uchar *srcend = src + Bigarray_val(src_in)->dim[0];
    uchar flag;
        
    src += 8;
    flag = *src++;
    while (src < srcend && dst < dstend) {
        if (bit == 256) {
            bit = 1;
            flag = *src++;
        }
        if (flag & bit) {
            *dst++ = *src++;
        }
        else {
            int i, count;
            uchar *repeat;
            count = *src++;
            count += (*src++) << 8;
            repeat = dst - ((count >> 4) - 1) - 1;
            count = (count & 0x0f) + 2;
            if (repeat < dststart || repeat >= dst)
                failwith ("corrupt data");
            for (i = 0; i < count; i++) {
                *dst++ = *repeat++;
            }
        }
        bit <<= 1;
    }

    CAMLreturn(Val_unit);
}


/* Decrypt decompressed data. */

value rl_prim_decrypt (value data, value game_id) {
    CAMLparam2(data, game_id);

    uchar *dst = Binarray_val(data);
//    uchar *dststart = Binarray_val(data);
//    uchar *dst = dststart;
    uchar *dststart = dst;
    uchar *dstend = dststart + Bigarray_val(data)->dim[0];
//  uchar *srcend = src + Bigarray_val(src_in)->dim[0];
//  uchar flag;
    int gameid = Long_val(game_id);
    
    Xor_Key *key = 0;
    
//    if(keyid) {
//        key = keys[keyid - 1];
//    }

    if(gameid > -1 && (key = keys[gameid])) {
        for(; key->length; key++) {
            dst = dststart + key->offset;
            for (int i = 0; i < key->length && dst < dstend; ++i) {
                *dst++ ^= key->key[i % 16];
            }
        }
    }

    CAMLreturn(Val_unit);
}

value _rl_prim_decrypt (value data, value offset, value length, value key) {
    
    CAMLparam4(data, offset, length, key);

    uchar *dsts = Binarray_val(data) + Long_val(offset);
    uchar *dste = dsts + Long_val(length);
    uchar *dst = dsts;
//    uchar *dst = Binarray_val(data);// + Long_val(offset);
    uchar *xor_mask = Binarray_val(key);
    
    int i = 0;
    
//    *dst = 0xff;
//    dste = dsts + 128;
    
//    dst = dststart + 256;
    while(dst < dste) {
    //    i++;
        *dst++ ^= xor_mask[i % 16];
    //    *dst++ = xor_mask[i % 16];
    //    *dst ^= xor_mask[i % 16];
    //    *dst++ = 0;
        i++;
    }
    
/*
     dst = dsts;
    
    for(int j = 0; j < 257; ++j) {
        if(dst > dste) break;
        *dst++ = j;
    }
*/
/*
     for (int i = 0; i < 257; ++i) {
        if(dst > dstend)
            break;
        *dst++ ^= xor_mask[i % 16];
    }
*/

    CAMLreturn(Val_unit);
}


/*
value rl_prim_compress (value src_in, value use_xor_2, value key)
{
    using namespace AVG32Comp;
    CAMLparam3(src_in, use_xor_2, key);

    uchar *xor_mask_2 = Binarray_val(key);
    uchar *src = Binarray_val(src_in);
    uchar *srcstart = src;
    uchar *srcend = src + Bigarray_val(src_in)->dim[0];

    if (Bool_val(use_xor_2)) {
        src = srcstart + 256;
        for (int i = 0; i < 257; ++i) {
            if(src > srcend)
                break;
            *src++ ^= xor_mask_2[i % 16];
        }
    }

    Compress<CInfoRealLive, Container::RLDataContainer> cmp;
    cmp.WriteData ((char *)srcstart, Bigarray_val(src_in)->dim[0]);
    cmp.WriteDataEnd();
    cmp.Deflate();
    cmp.Flush();
    memmove (srcstart, cmp.Data(), cmp.Length());
    CAMLreturn(Val_long(cmp.Length()));
}
*/

/*
value rl_prim_compress (value src_in, value key_id)
{
    using namespace AVG32Comp;
    CAMLparam2(src_in, key_id);

    uchar *src = Binarray_val(src_in);
    uchar *srcstart = src;
    uchar *srcend = src + Bigarray_val(src_in)->dim[0];
    int keyid = Long_val(key_id);
    
    Xor_Key *key = 0;

    if(keyid > -1) {
        for(key = keys[keyid]; key->length; key++) {
            src = srcstart + key->offset;
            for (int i = 0; i < key->length && src < srcend; ++i) {
                *src++ ^= key->key[i % 16];
            }
        }
    }

/ *
    if (Bool_val(use_xor_2)) {
        src = srcstart + 256;
        for (int i = 0; i < 257; ++i) {
            if(src > srcend)
                break;
            *src++ ^= xor_mask_2[i % 16];
        }
    }
* /

    Compress<CInfoRealLive, Container::RLDataContainer> cmp;
    cmp.WriteData ((char *)srcstart, Bigarray_val(src_in)->dim[0]);
    cmp.WriteDataEnd();
    cmp.Deflate();
    cmp.Flush();
    memmove (srcstart, cmp.Data(), cmp.Length());
    CAMLreturn(Val_long(cmp.Length()));
}
*/

value rl_prim_compress (value src_in)
{
    using namespace AVG32Comp;
    CAMLparam1(src_in);

    uchar *src = Binarray_val(src_in);
    uchar *srcstart = src;
    uchar *srcend = src + Bigarray_val(src_in)->dim[0];

    Compress<CInfoRealLive, Container::RLDataContainer> cmp;
    cmp.WriteData ((char *)srcstart, Bigarray_val(src_in)->dim[0]);
    cmp.WriteDataEnd();
    cmp.Deflate();
    cmp.Flush();
    memmove (srcstart, cmp.Data(), cmp.Length());
    CAMLreturn(Val_long(cmp.Length()));
}

}
