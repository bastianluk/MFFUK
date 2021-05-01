#include <errno.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

int main()
{
    return 1;
}

void *checked_malloc (size_t size) {
    void *addr = malloc (size);
    if (addr == NULL) {
        printf ("Failed to allocate %zi bytes.\n", size);
        exit (ENOMEM);
    }
    return (addr);
}
