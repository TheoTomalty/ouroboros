#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "trie.h"


#define LOWER_NIBBLE  0x0f
#define UPPER_NIBBLE ~0x0f

void trie_delete(void *_trie) {
    trie_t *trie = _trie;
    for (int i = 0; i < TRIE_SIZE; ++i) {
        edge_t *edge = trie->edges[i];
        if (edge != NULL)
            rc_decr(edge);
    }
}

void edge_delete(void *_edge) {
    edge_t *edge = _edge;
    rc_decr(edge->target);
}

static inline int len_to_charlen(int len) {
    return (len / 2) + (len % 2);
}

int get_chunk(uint8_t *key, int i) {
    return (i % 2 == 0) ?
        (key[i/2] & LOWER_NIBBLE):
        (key[i/2] & UPPER_NIBBLE) >> 4;
}

void set_chunk(uint8_t *key, int i, int v) {
    int k = key[i/2];
    if (i % 2 == 0)
        key[i/2] = (k & UPPER_NIBBLE) | (v & LOWER_NIBBLE);
    else
        key[i/2] = (k & LOWER_NIBBLE) | (v << 4);
}

trie_t *trie_new() {
    trie_t *ret = (trie_t *) rc_alloc(sizeof(trie_t), &trie_delete);
    memset(ret, 0, sizeof(trie_t));
    return ret;
}

trie_t *maybe_copy_trie(trie_t *trie) {
    if (rc_count(trie) == 1)
        return trie;
    rc_decr(trie);
    trie_t *ret = (trie_t *) rc_alloc(sizeof(trie_t), &trie_delete);
    memcpy(ret, trie, sizeof(trie_t));
    for (int i = 0; i < TRIE_SIZE; ++i) {
        edge_t *edge = ret->edges[i];
        if (edge != NULL)
            rc_incr(edge);
    }
    return ret;
}

edge_t *new_edge(uint8_t *key, int offset, int len) {
    int size = sizeof(edge_t) + len_to_charlen(len);
    edge_t *ret = (edge_t *) rc_alloc(size, &edge_delete);
    memset(ret, 0, size);
    ret->len = len;
    for (int i = 0; i < len; ++i) {
        set_chunk(ret->label, i, get_chunk(key, i + offset));
    }
    return ret;
}

edge_t *maybe_copy_edge(edge_t *edge) {
    if (rc_count(edge) == 1)
        return edge;
    rc_decr(edge);
    int size = sizeof(edge_t) + len_to_charlen(edge->len);
    edge_t *ret = (edge_t *) rc_alloc(size, &edge_delete);
    memcpy(ret, edge, size);
    rc_incr(ret->target);
    return ret;
}

void *trie_lookup(trie_t *trie, uint8_t *key, int size) {
    size = size * 2;
    int i = 0;
    while (i < size) {
        edge_t *edge = trie->edges[get_chunk(key, i)];
        if (edge == NULL || edge->len > size - i) {
            return NULL;
        }
        for (int j = 1; j < edge->len; ++j) {
            if (get_chunk(edge->label, j) != get_chunk(key, i + j)) {
                return NULL;
            }
        }
        i = i + edge->len;
        trie = edge->target;
    }
    if (trie->is_leaf) {
        return trie->value;
    }
    else {
        return NULL;
    }
}

trie_t *trie_insert(trie_t *trie, uint8_t *key, int size, void *value) {
    size = size * 2;
    int i = 0;
    trie = maybe_copy_trie(trie);
    trie_t *root = trie;
    while (i < size) {
        int edge_id = get_chunk(key, i);
        edge_t *edge = trie->edges[edge_id];
        if (edge == NULL) {
            // Can add directly on the trie
            // size - i > 0
            trie_t *child = trie_new();
            child->is_leaf = true;
            child->value = value;
            edge = new_edge(key, i, size - i);
            edge->target = child;
            trie->edges[edge_id] = edge;
            return root;
        }
        for (int j = 1; j < edge->len; ++j) {
            bool b;
            int c2;
            int c1 = get_chunk(edge->label, j);
            if ((b = i + j >= size) || c1 != (c2 = get_chunk(key, i + j))) {
                // `edge' needs to be split
                // j > 0, edge->label - j > 0, edge->label[0..j] = key[i..(i + j)]
                edge_t *edge2 = new_edge(edge->label, j, (edge->len - j));
                edge2->target = edge->target;
                trie_t *intermediary = trie_new();
                intermediary->edges[c1] = edge2;
                edge_t *edge1 = new_edge(edge->label, 0, j);
                edge1->target = intermediary;
                trie->edges[edge_id] = edge1;
                rc_incr(edge->target);
                rc_decr(edge);
                if (b) {
                    // Value can go right on intermediary trie
                    intermediary->is_leaf = true;
                    intermediary->value = value;
                }
                else {
                    // size - (i + j) > 0
                    trie_t *child = trie_new();
                    child->is_leaf = true;
                    child->value = value;
                    edge_t *edge3 = new_edge(key, i + j, size - (i + j));
                    edge3->target = child;
                    intermediary->edges[c2] = edge3;
                }
                return root;
            }
        }
        i = i + edge->len;
        edge = maybe_copy_edge(edge);
        trie_t *child = maybe_copy_trie(edge->target);
        edge->target = child;
        trie->edges[edge_id] = edge;
        trie = child;

    }
    trie->is_leaf = true;
    trie->value = value;
    return root;
}

trie_t *trie_insert_copy(trie_t *trie, uint8_t *key, int size, void *value) {
    rc_incr(trie);
    return trie_insert(trie, key, size, value);
}

trie_t *trie_insert_mut(trie_t *trie, uint8_t *key, int size, void *value) {
    if (rc_count(trie) != 1) abort();
    return trie_insert(trie, key, size, value);
}

void trie_debug_print(trie_t *trie) {
    void indent(int n) {
        for (int i = 0; i < n; ++i) {
            putchar('\t');
        }
    }

    void aux(trie_t *trie, int n) {
        indent(n);

        if (trie->is_leaf) {
            printf("leaf(%p)\n", trie->value);
        }
        else {
            printf("trie:\n");
        }

        for (int j = 0; j < TRIE_SIZE; ++j) {
            edge_t *edge = trie->edges[j];
            if (edge != NULL) {
                indent(n + 1);
                for (int k = 0; k < edge->len; ++k) {
                    printf("%x ", get_chunk(edge->label, k));
                }
                printf("->\n");
                aux(edge->target, n + 2);
            }
        }

    }

    return aux(trie, 0);
}

void trie_iter(trie_t *trie, void (*cb)(int, uint8_t *, void *, void *), void *data) {
    void aux(trie_t *trie, int len, uint8_t *prefix, void (*cb)(int, uint8_t *, void *, void *), void *data) {
        if (trie->is_leaf) {
            if (len % 2 == 1) {
                set_chunk(prefix, len, 0);
            }
            cb(len_to_charlen(len), prefix, trie->value, data);
        }
        for (int j = 0; j < TRIE_SIZE; ++j) {
            edge_t *edge = trie->edges[j];
            if (edge != NULL) {
                int new_len = len + edge->len;
                uint8_t *new_prefix = (uint8_t *) malloc(len_to_charlen(new_len));
                if (prefix != NULL) {
                    memcpy(new_prefix, prefix, len_to_charlen(len));
                }
                for (int k = 0; k < edge->len; ++k) {
                    set_chunk(new_prefix, len + k, get_chunk(edge->label, k));
                }
                aux(edge->target, new_len, new_prefix, cb, data);
                free(new_prefix);
            }
        }
    }

    return aux(trie, 0, NULL, cb, data);
}
