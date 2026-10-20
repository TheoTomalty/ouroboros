#ifndef ALLOC_H
#define ALLOC_H

#include <stdlib.h>


void *rc_alloc(size_t size, void (*delete)(void *));
void rc_incr(void *ptr);
void rc_decr(void *ptr);
int  rc_count(void *ptr);

void rc_noop(void *);


#endif /* ALLOC_H */
