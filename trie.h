#ifndef TRIE_H
#define TRIE_H

#include <stdint.h>
#include <stdbool.h>


#define TRIE_SIZE 16

typedef struct edge {
	struct trie *target;
	int len;
	uint8_t label[];
} edge_t;

typedef struct trie {
	struct edge *edges[TRIE_SIZE];
	void *value;
	bool is_leaf;
} trie_t;

trie_t *trie_new(void);
void   *trie_lookup(trie_t *, uint8_t *, int);
trie_t *trie_insert(trie_t *, uint8_t *, int, void *);
trie_t *trie_insert_copy(trie_t *, uint8_t *, int, void *);
trie_t *trie_insert_mut(trie_t *, uint8_t *, int, void *);
void trie_debug_print(trie_t *);
void trie_iter(trie_t *, void (*)(int, uint8_t *, void *, void *), void *);


#endif /* TRIE_H */
