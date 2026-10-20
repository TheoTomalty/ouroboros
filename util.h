#ifndef UTIL_H
#define UTIL_H

#include <stdint.h>
#include <stdbool.h>


typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

void util_decode_utf8(u8 *s, u32 *code, u32 *len);
void util_encode_utf8(u32 code, u8 *s, u32 *len);

u32 util_hash_uint32(u32 h1, u32 k1);
u32 util_hash_string(u32 seed, u32 len, u8 *s);
u32 util_hash_finalize(u32 h);


#endif /* UTIL_H */
