# Memory Models

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday November 19 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the last of three self study modules that will look at the synchronization issues
when using multiple threads for concurrent programming. The goal of this module is to hint
at the complexities associated with memory operation ordering, which were set aside until now.


## Memory Access Optimizations

One of the most expensive operations in programs are memory accesses.
That is why our compilers and processors try to eliminate
memory accesses whenever possible.
For example, consider this code:

```c
int find_non_zero (int *array) {
    int position = 0;
    int value;
    do {
        value = array [position];
        position ++;
    } while (value == 0);
    return (value);
}
```

We do not really expect the compiler to emit code that would fetch the values of `array` and `position`
from memory every time `array [position]` is computed, or that would update the value of `position`
in memory every time it is incremented (or indeed stored `position` in memory at all).
We are used to seeing code like this:

```
find_non_zero:
        movl    4(%esp), %eax
.L2:
        movl    (%eax), %edx
        addl    $4, %eax
        testl   %edx, %edx
        je      .L2
        movl    %edx, %eax
        ret
```

Without going into detail, we can see that the code fetches the value of `array` from memory to the `%eax`
register as the very first thing it does. Similarly, the `value` variable is kept in the `%edx` register
rather than in memory, and the `position` variable is even completely eliminated, instead the result
of computing `array [position]` is incrementally updated in `%eax`.

The compiler is not where the things stop though. To reduce latency, the processor will fetch memory content
in blocks of 64 bytes and store them in a local cache (these blocks are called cache lines). Since our program
iterates through the input array in steps of four bytes (integer size here), only the first access to every such
block will require a memory access, the next 64/4-1=15 accesses will be satisfied from the cache and therefore fast.

The processor does even more. In the code, moving the pointer in `%eax` to the next array element (the `addl $4, %eax` instruction)
happens after the current array element is read (the `movl (%eax), %edx` instruction), however, the read can take long and
the processor can compute the next pointer value even before the read completes. Also, the processor can easily detect
that the code accesses memory sequentially, it can therefore initiate the read of the next cache line even before
the code actually asks for it. In the worst case, such prefetch will be useless and waste some cache space
and some memory bandwidth, but ideally it will reduce latency when that cache line is needed.

Please take a look at the MESI protocol explanation and animation at https://www.scss.tcd.ie/Jeremy.Jones/VivioJS/caches/MESIHelp.htm
to see an example how the data is moved between memory and caches in a multiprocessor system and how cache coherency is maintained.
You may want to consult Wikipedia or other online resources for a quick explanation of the protocol.
Do not go into detail, the important points you should take away are:

- access to memory happens through cache lines,
- cache lines have states (modified, exclusive, shared, invalid),
- multiple readers can have multiple cache lines all in shared states,
- each writer requires exclusive cache line and invalidates other shared cache lines.

Please take a look into the Intel _Processor Manual Volume 3A Section 8.2 Memory Ordering_ for examples
of changes in memory access order that can appear in practice.


## Memory Access Ordering

As a direct implication of the optimizations mentioned above,
the actual manner in which memory is accessed by an executing
program can radically differ from what can be seen in the source
code or even in the machine code.

This poses an obvious problem for programming with threads and shared memory.
If synchronization is carried out through a carefully orchestrated set
of memory accesses that the individual threads carry out, how can we
be sure the program will still work after the optimizations ?

As one possibly troublesome example, consider the following code:

```c
volatile int data;
volatile bool signal = false;

void thread_one_function (void) {
    data = some_data_value ();
    signal = true;
}

void thread_two_function (void) {
    while (!signal) {
        // Busy wait for signal to indicate that data was set.
    }
    pritnf ("Data is %i\n", data);
}
```

The idea of the code is to have one thread produce `data` and then use the `signal` variable to tell the other thread that `data` is ready.
But if the compiler or the processor do not know that `data` and `signal` are related, one or both can change the memory access order
so that `data` is not set by the time `thread_two_function` tries to read it even though `signal` suggests otherwise.

This may look like a somewhat contrived example, however, consider this one:

```c
volatile singleton_t *singleton = NULL;
lock_t singleton_creation_lock;

singleton_t *get_singleton () {
    if (singleton == NULL) {
        singleton_creation_lock.lock ();
        if (singleton == NULL) {
            singleton = new singleton_t ();
        }
        singleton_creation_lock.unlock ();
    }
    return (singleton);
}
```

The code sketches an attempt at Double Checked Locking, a design pattern that provides a delayed creation of the singleton object.
The design pattern tries to prevent a race when multiple threads would query a non existent singleton at the same time,
but it also tries to avoid locking on the fast path once the singleton is created.
The code does not work as intended though - for example, there are
no guarantees the `singleton` variable will be set only after
the `singleton_t` instance is fully constructed.
A similarly incorrect version of the pattern was published for example in the famous
[POSA book](https://www.amazon.com/Pattern-Oriented-Software-Architecture-Concurrent-Networked/dp/0471606952)
and it took the developer public several years to realize [the pattern is broken](http://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html).


## Memory Models

To enable robust programming with threads and shared memory, programming languages have introduced memory models.
A memory model is a set of formal rules that describe guarantees on interaction through shared memory.
The memory model must reconcile two opposing concerns:

- the model must be simple enough and restrictive enough to permit comfortable use by software developers, and
- the model must be flexible enough to permit reasonable optimizations in both the compiler and the processor.

Please take a look at the Java memory model.
Probably the best document to use is [JSR133](http://www.cs.umd.edu/~pugh/java/memoryModel/jsr133.pdf),
it should be enough to read the informal part, roughly up to and including Section 6. There is also
[a starting page with a lot of additional material](http://www.cs.umd.edu/~pugh/java/memoryModel).
What you should take away is:

- what is a data race,
- what is a program order,
- what is a synchronization order,
- what is a sequentially consistent execution.


## Questions

Almost looked like we forgot :-) anyway, since there is a lot of reading, we only have three questions, plus the mandatory one at the end.


**Q1** Here is a slightly modified version of the example program that demonstrates data race on a shared counter:

```c
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int counters [256];

#define LOOPS 100000000

void *counter_thread_body (void *arguments) {
    int *counter = (int *) arguments;
    for (int i = 0 ; i < LOOPS ; i ++) {
        __atomic_fetch_add (counter, 1, __ATOMIC_SEQ_CST);
    }
    return (NULL);
}

int main (int argc, char *argv []) {

    // One counter index comes from command line.
    int index = atoi (argv [1]);

    pthread_t thread_one, thread_two;

    // Launch two threads that both execute the same body.
    // Each thread will increment a different counter.
    pthread_create (&thread_one, NULL, counter_thread_body, &counters [0]);
    pthread_create (&thread_two, NULL, counter_thread_body, &counters [index]);
    // Wait for the two threads to finish.
    pthread_join (thread_one, NULL);
    pthread_join (thread_two, NULL);

    return (0);
}
```

This program accepts one argument on the command line, a counter index.
It then launches two threads that each increment a different counter,
`thread_one` increments `counters [0]`, `thread_two` increments
`counters [index]` specified at the command line.

(When launched with index 0, the example becomes the same as those you already saw.)

Run the example with index 1 and with index 42 and time each run.
One of the two cases will most likely be much slower than the other.
Which one and why ?

**A1** ...


**Q2** Take a look into the _Processor Manual_ for the Intel processors and list
the memory ordering fences (sometimes also called memory barriers) that the processor provides.

**A2** ...


**Q3** Imagine two threads, one thread prints 'A' and then 'B', the other thread prints 'C' and then 'D'.
What outputs can you see if you execute these threads concurrently in a sequentially consistent manner ?

**A3** ...


And now the ever present mandatory summary question (note that you must come up with a question for the quiz to be accepted):

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** "A similarly incorrect version of the pattern was published for example in the famous
POSA book and it took the developer public several years to realize the pattern is broken." - how come it took so long to debunk this? Was it just that not that many people were involved at the time?
