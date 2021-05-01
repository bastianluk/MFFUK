#include <stdio.h>
int test (void) {
    int i = 1234;
    int j = 1234;
    printf ("test i: %p j: %p\n", &i, &j);
    return (i);
}
int main (void) {
    int i = 1234;
    test ();
    int j = 1234;
    printf ("main i: %p j: %p\n", &i, &j);
    return (i);
}

