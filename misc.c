#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "util.h"


void putstr(i8 *s) {
    if (write(STDOUT_FILENO, s, strlen(s)) < 0) abort();
}

void putstrln(i8 *s) {
    if (write(STDOUT_FILENO, s, strlen(s)) < 0) abort();
    char ch = '\n';
    if (write(STDOUT_FILENO, &ch, 1) < 0) abort();
}

void eputstr(i8 *s) {
    if (write(STDERR_FILENO, s, strlen(s)) < 0) abort();
}

void eputstrln(i8 *s) {
    if (write(STDERR_FILENO, s, strlen(s)) < 0) abort();
    char ch = '\n';
    if (write(STDERR_FILENO, &ch, 1) < 0) abort();
}

void read_file(i8 *fname, i64 *len, i8 **s) {
    int fd;
    if ((fd = open(fname, O_RDONLY)) < 0) {
        perror(fname);
        exit(1);
    }
    int size = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    *len = size;
    *s = (i8 *) malloc((size + 1) * sizeof(i8));
    (*s)[size] = '\0';
    
    i8 *ptr = *s;
    int chunk = 0;
    while ((chunk = read(fd, ptr, size - (int) (ptr - *s))) > 0) {
        ptr += chunk;
    }
    close(fd);
}
