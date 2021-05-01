#include <stdio.h>
int i = 1234;
int main (void) {
  printf ("main: %p i: %p\n", main, &i);
  return (i);
}
