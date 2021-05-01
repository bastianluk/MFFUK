# Process Memory Layout

This is the first of three self study modules dedicated to process memory layout,
dealing with those parts of the process memory that host code and stack.
Where the code is concerned, the goal is to explain the role and principal functionality of linking and relocation.
Where the stack is concerned, the goal is to outline the stack layout,
explain the use for local variables and return addresses,
and sketch the connection to the processor addressing modes from the last study module.

This module is timed together with the first assignment for the MIPS processor,
it is therefore shorter in content and gives more space for playing with the MIPS compiler and simulator.


## Process Memory Layout

A process executes in an address space of its own, that is, it is provided with the illusion of executing in a memory array that is (almost) entirely at its disposal. The (virtual) addresses used by the process are simply locations in this memory array. This illusion is created through virtual memory paging (details coming later).

The memory of a process typically hosts four types of content:

- The process _code_, that is, the machine instructions making up the entire program.
- The process _heap_, that is, the area from which dynamic memory allocation requests are satisfied.
  When a high level programming language uses constructs such as `new`, this is where the memory comes from.
- The process _stack_, that is, a runtime structure used to hold local variables and return addresses.
  When a high level programming language declares a local variable, it usually resides on the stack.
  Also, when procedures, functions or methods call each other, this is where the record of call nesting is kept.
- The _static_ process _data_, that is, data such as constant strings that are part of the program.

Consider the following program fragment:

```c
#include <errno.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

void *checked_malloc (size_t size) {
    void *addr = malloc (size);
    if (addr == NULL) {
        printf ("Failed to allocate %zi bytes.\n", size);
        exit (ENOMEM);
    }
    return (addr);
}
```

Most of the program text would be compiled into instructions making up the process code.
The local variable `addr` and the function argument `size` are (likely) on the stack,
as is the return address pointing to the code that called `checked_malloc`.
The call to `malloc` allocates data on the heap.
Finally, the string is static data.

Obviously, the code and static data need to be stored in the program executable,
in contrast the heap and the stack are created and managed during program execution.
See for example the `.text` and `.rodata` sections in an ELF executable built from the example above:

```shell
gcc -c checked_malloc.c
objdump -s -j .rodata malloc.o
objdump -d -j .text malloc.o
```


## Code and Linking

When a program uses library functions, such as the calls to `exit`, `malloc` and `printf` in the example above,
the code of the program needs to be connected to the code of the library functions used.
This process is called linking.

In more general terms, linking satisfies _external_ references with _exported_ symbols.
Symbols typically refer to functions, but can also be variables, such as `errno`,
or any other object with an address.

See how in the example above, the compiled object file declares external function dependency:

```shell
objdump -t checked.o
```

In the output of the command above, we see the exported `checked_malloc` (the flags denote a global symbol that is a function), and the external `malloc`, `printf` and `exit`:

```
0000000000000000 g     F .text  0000000000000049 checked_malloc
0000000000000000         *UND*  0000000000000000 malloc
0000000000000000         *UND*  0000000000000000 printf
0000000000000000         *UND*  0000000000000000 exit
```

Linking can be _static_ or _dynamic_. Static linking is done at compile time and merges the program code with the code from the libraries into a single executable. Dynamic linking is done by the program loader of the operating system at program start time, the program code and the libraries remain separate executables.
The `ldd` command can be used to see what libraries satisfy the external dependencies.

**Q1** Add a `main` function to the example above to make it into a complete executable.
Determine what library or libraries (full file name or names) provides the `malloc`, `printf` and `exit` functions.

**A1** EDIT POST deadline - "empty answer"


## Code and Relocation

In the previous module, you saw various ways of expressing addresses in the program code,
and you saw how the program code may assume that it resides at certain range of addresses.
Obviously, this assumption is embedded in the executable that carries the program code,
and the program loader of the operating system must use the correct range of addresses
for the code to function.

Using the correct range of addresses may not seem to be a problem for the program executable alone
(it should have an entire address space of its own so is free to use any range), however,
the same is not the case when dynamic libraries are involved.
In an arbitrary set of libraries where each library can request an arbitrary address range,
overlaps can naturally occur, and the program loader must resolve them.

When moving a program to a different address range, all addresses referring to the locations within program must be adjusted. This process is called _relocation_. Obviously, relocation needs to identify all addresses to adjust, this list would be difficult or impossible to reconstruct from code alone, hence relocatable executables carry the list of addresses that may need adjustment.

Another reason for relocation is security, certain types of security attacks are easier to carry out when the attacker knows what addresses the attacked program uses. Modern operating systems therefore support process memory layout randomization, where the exact addresses to use are decided at program start time.

Let us extend the example from the previous module with printing of data and code addresses:

```c
#include <stdio.h>
int i = 1234;
int main (void) { printf ("main: %p i: %p\n", main, &i); return (i); }
```

Compile the program as a position independent executable:

```shell
gcc -pie -fpie -o main main.c
```

**Q2** Run the example above a few times and see what addresses the `main` function and the `i` variable reside at. (If the addresses remain constant, try the `lab` computer, your system may have address randomization disabled.)

Is the program moved en bloc (the distance between `main` and `i` remains constant) or is each section moved to a random location independently ? What is the granularity of the randomization (what is the smallest address step that you think the randomization mechanism can use) ?

**A2** It is moved en block.


## Stack

In the past courses, you have learned the basis of how the program stack works. (If you need a refresher, you may want to check out https://www.ksi.mff.cuni.cz/teaching/nswi170-web/download/02-instructions_and_compilation.pdf for information on MIPS, a detailed description for Intel is for example at https://eli.thegreenplace.net/2011/02/04/where-the-top-of-the-stack-is-on-x86 and https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64.)

We can use the fact that local variables are stored on the stack to print the stack location:

```c
#include <stdio.h>
int main (void) { int i = 1234; printf ("%p\n", &i); return (i); }
```

Now, consider a more involved example of the same:

```c
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
```

**Q3** If you compile and run the code above, you should see that the pairs of `i` and `j` inside `test` and `i` and `j` inside `main` are stored next to each other (the two addresses are exactly a size of one integer apart), however, the variables inside `test` are not stored next to the variables inside `main`. Why ?

**A3** There is overhead to the allocation of each method. Not only its variables but arguments, return addresses, old stack pointer value... need to have space allocated on stack. The memory allocation is done in a way that it resembles layers depending on how long each thing needs to "live" and so the program can run in a linear fashion and not jump around the stack > each method should have most of what it needs together and not groupped by "type" (not all variables of the program together, then all the arguments together etc.) 

**Q4** In the example above, are the addresses of variables inside `test` higher or lower than the addresses of variables inside `main` ? Why ?

**A4** test is lower because main is allocated first and then the stack "grows" downwards (as weird as that sounds).

Consider another example, this time with limited recursion:

```c
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
```

**Q5** Looking at the addresses printed by the example with recursion, what can you tell about the memory overhead of each nested call (that is, how much memory does it consume) ? Why ? Could the stack eventually reach all the way to the code of function `main`, and what would happen if it did ?

**A5** EDIT POST deadline: 32 bytes (32/8 = 4 but the original value was already in bytes not bits) it seems like - the consumption per block/call. I thought it couldnt as the main should be allocated first, at the top, but from the prints I got that the main is lower and eventually, with enough calls, it could run out of memory causing stack overflow.

And finally, a mandatory summary question:

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study block (but not the same as the last time around :-).

**A0** More details on recursion and memory allocation VS "regular" non recursive allocation.