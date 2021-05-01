#include <stdio.h>
int test (int depth) {
    printf ("%p\n", &depth);
    if (depth > 0) test (depth - 1);
    return (depth);
}
int main (void) {
    test (10);
    printf ("main: %p\n", main);
}

