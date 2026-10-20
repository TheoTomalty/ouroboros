#include <stdlib.h>
#include "util.h"


void util_decode_utf8(u8 *s, u32 *code, u32 *len) {
	u8 c1 = s[0];
	*code = 0;
	if (c1 & (1 << 7)) {
		u32 n = 0;
		for (;;) {
			if (n > 3) abort();
			if (!(c1 & (1 << (6 - n)))) {
				break;
			}
			++n;
		}
		if (n == 0) abort();
		*code += (c1 % (1 << (6 - n))) << (6 * n);
		*len = 1 + n;
		for (u32 i = 0; i < n; ++i) {
			u8 ci = s[i + 1];
			if (!(ci & (1 << 7)) || (ci & (1 << 6))) abort();
			*code += (ci % (1 << 6)) << (6 * (n - (i + 1)));
		}
	}
	else {
		*code = c1;
		*len = 1;
	}
}

void util_encode_utf8(u32 code, u8 *s, u32 *len) {
	if (code < 0x80) {
		*len = 1;
		s[0] = (u8) code;
	}
	else if (0x80 <= code && code < 0x800) {
		*len = 2;
		s[0] = 0b11000000 | ((u8) ((code >> 6) % (1 << 5)));
		s[1] = 0b10000000 | ((u8) ((code >> 0) % (1 << 6)));
	}
	else if (0x800 <= code && code < 0x10000) {
		*len = 3;
		s[0] = 0b11100000 | ((u8) ((code >> 12) % (1 << 4)));
		s[1] = 0b10000000 | ((u8) ((code >> 6) % (1 << 6)));
		s[2] = 0b10000000 | ((u8) ((code >> 0) % (1 << 6)));
	}
	else if (0x10000 <= code && code < 0x110000) {
		*len = 4;
		s[0] = 0b11110000 | ((u8) ((code >> 18) % (1 << 3)));
		s[1] = 0b10000000 | ((u8) ((code >> 12) % (1 << 6)));
		s[2] = 0b10000000 | ((u8) ((code >> 6) % (1 << 6)));
		s[3] = 0b10000000 | ((u8) ((code >> 0) % (1 << 6)));
	}
	else {
		abort();
	}
}

/* These hashing functions are based on MurmurHash3 by Austin Appleby (public domain),
 * which can be found here: https://github.com/aappleby/smhasher (src/MurmurHash3.cpp)
 * They were copied with only a few changes, and adapted to fit the interface defined in
 * https://github.com/ocaml/ocaml (runtime/hash.c).  They must be the same as the ones
 * used in OCaml because otherwise the bootstrap and self-hosted outputs will differ.
 */

#define	ROTL32(x,y)	(((x) << (y)) | ((x) >> (32 - (y))))
#define C1 0xcc9e2d51
#define C2 0x1b873593

u32 util_hash_uint32(u32 h1, u32 k1) {
	k1 *= C1;
	k1 = ROTL32(k1,15);
	k1 *= C2;

	h1 ^= k1;
	h1 = ROTL32(h1,13);
	h1 = h1*5+0xe6546b64;

	return h1;
}

u32 util_hash_string(u32 seed, u32 len, u8 *s) {
	const u8 * data = (const u8*) s;
	const int nblocks = len / 4;

	u32 h1 = seed;

	//----------
	// body

	const u32 * blocks = (const u32 *)(data + nblocks*4);


	for(int i = -nblocks; i; i++) {
		h1 = util_hash_uint32(h1, blocks[i]);
	}

	//----------
	// tail

	const u8 * tail = (const u8*)(data + nblocks*4);

	u32 k1 = 0;

	switch(len & 3) {
	case 3: k1 ^= tail[2] << 16;
	case 2: k1 ^= tail[1] << 8;
	case 1: k1 ^= tail[0];
		k1 *= C1; k1 = ROTL32(k1,15); k1 *= C2; h1 ^= k1;
	};

	//----------
	// finalization

	h1 ^= len;

	return h1;
}

u32 util_hash_finalize(u32 h) {
	h ^= h >> 16;
	h *= 0x85ebca6b;
	h ^= h >> 13;
	h *= 0xc2b2ae35;
	h ^= h >> 16;

	return h;
}
