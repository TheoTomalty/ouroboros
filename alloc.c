#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "alloc.h"


typedef union header {
    void (*delete)(void *);
    struct {
        uint16_t padding[3];
        uint16_t rc;
    };
} header_t;

void *rc_alloc(size_t size, void (*delete)(void *)) {
    header_t *ret = malloc(sizeof(header_t) + size);
    ret->delete = delete;
    ret->rc = 1;
    return &ret[1];
}

#define HEAD(ptr) (&((header_t *) ptr)[-1])

void rc_incr(void *ptr) {
    HEAD(ptr)->rc++;
}
void rc_decr(void *ptr) {
    header_t *header = HEAD(ptr);
    if (--(header->rc) == 0) {
        header->delete(ptr);
        free(header);
    }
}

int rc_count(void *ptr) {
    return HEAD(ptr)->rc;
}

void rc_noop(void *) {}
