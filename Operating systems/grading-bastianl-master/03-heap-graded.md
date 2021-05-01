# Process Memory Layout

This is a graded test, do not forget to commit your responses in time !


## Stack

Stack is a data structure intricately tied to the way the (procedure, function, method) calls work.
Besides control flow information (return addresses), stack is a handy place to store function
arguments and local variables, especially when these do not fit into the available registers,
and for general register spilling whenever register content needs to be temporarily preserved.

The following questions check whether you understand how stack works.

**Q1** The following program attempts to print the address of a local variable:

```c
#include <stdio.h>

int *test (void) {
    int i = 1234;
    return (&i);
}

int main (void) {
    int *addr = test ();
    printf ("%p\n", addr);
}
```

When compiling the program with GCC, the compiler emits the following warning:

```
main.c: In function ‘test’:
main.c:5:13: warning: function returns address of local variable [-Wreturn-local-addr]
    5 |     return (&i);
      |            ~^~~
```

Explain what is the problem with the code that the warning indicates.

**A1** The bebavior in this case is not specified by C language and left to compiler makers - in this case they decide to prevent the fact that you would be returning address from stack that would become invalid as the scope/lifetime of the local variable is bound to the lifetime of the function/"time spent" in the function. Returning the address would render it invalid/inaccessible hence it is pointless and rather prevented.


**Q2** Imagine you declare two integer variables, one global, one local.
Both are stored in memory, the local one on the stack.
Do you think there is a difference in speed when
accessing the content of these variables ?

Defend your answer (explain why you reached the conclusion you did).

**A2** The use of pointers to access variables on the heap makes it slower as one has to "jump" around the memory to access it while local variables stored on stack are close to the code instructions being executed at the time making it faster.


**Q3** Look at the code below.

```c
#include <stdio.h>

long *temp_one;
long *temp_two;

void three (long i) {
    printf ("function three (%li) entered\n", i);
    *(temp_two - 2) = *(temp_one - 2);
    printf ("function three (%li) leaving\n", i);
}

void two (long i) {
    printf ("function two (%li) entered\n", i);
    temp_two = &i;
    three (i + 1);
    printf ("function two (%li) leaving\n", i);
}

void one (long i) {
    printf ("function one (%li) entered\n", i);
    temp_one = &i;
    two (i + 1);
    printf ("function one (%li) leaving\n", i);
}

int main (void) {
    one (1);
    return (0);
}
```

The code is not very portable and would crash when run on many computers, however,
at least on the `lab` machine used in the course (and possibly other 64-bit Linux
computers), it displays this output:

```
function one (1) entered
function two (2) entered
function three (3) entered
function three (3) leaving
function one (2) leaving
function one (1) leaving
```

Note the fifth line, which says `function one (2) leaving` instead of `function two (2) leaving`.
Explain what is happening to the control flow of the program and why.

**A3** I think it is caused by a clever play with addresses and instructions: in `one` we assign `temp_one` giving us a reference point inside the one function, in `two` we do the same thing for inside the `two` function with `temp_two`. Finally, in `three` we use those references and we overwrite the instruction that would print out `function two (2) leaving` by the instruction from `one` in this call - `*(temp_two - 2) = *(temp_one - 2);`.


## Heap

The actual allocation strategy of the heap allocator can be quite complex, but sometimes,
on an empty heap, it is not entirely unreasonable to expect consecutive allocations
to be as close to each other as possible (heap allocator metadata permitting).
Hence, the following program can, perhaps, give some idea of space overhead
with heap allocations:

```c
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int main (void) {
    for (int i = 0 ; i < 256 ; i++) {
        void *p = malloc (i);
        void *q = malloc (i);
        int delta = (uintptr_t) q - (uintptr_t) p;
        printf ("allocating %i bytes consumes %i bytes\n", i, delta);
    }
    return (0);
}
```

To answer the following questions, preferably execute the code above on the `lab` computer.
If you use a different environment, please discuss the results so that we have
sufficient information to determine whether your answer is correct.

**Q4** Based on the output of the program above, what is the apparent size of the heap block headers,
that is, the metadata structures that the heap allocator places between consecutive blocks ?

**A4** It looks like 8 bytes as before jumping to a bigger block the smallest diff of size and block size is always 8.
```
allocating 24 bytes consumes 32 bytes
allocating 25 bytes consumes 48 bytes
allocating 40 bytes consumes 48 bytes
allocating 41 bytes consumes 64 bytes
allocating 56 bytes consumes 64 bytes
allocating 57 bytes consumes 80 bytes
...
```

**Q5** Based on the output of the program above, are the allocated block sizes somehow rounded,
and if so, to what multiple ?

**A5** It goes in multiples of 16 but starts with 32 (16 * 2). So `16 + k * 16` for natural k.
