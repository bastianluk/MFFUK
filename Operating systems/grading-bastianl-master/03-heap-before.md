# Process Memory Layout

This is the second of three self study modules dedicated to process memory layout,
dealing with those parts of the process memory that host heap.
The goal is to explain the basic features provided by non managed heap,
and give a reasonable impression of what overhead to expect when using heap.


## Heap

In most configurations, the implementation of the application heap is not really part of the operating system,
but rather part of the runtime library of whatever language is used to implement the particular application.
Reasons why this is done include:

- Efficiency. Modern programming languages use heap extensively (for example all object allocations
  may use the heap) and adding the overhead of calling the operating system with every heap
  allocation is not acceptable.

- Flexibility. Different programming languages have different requirements on their heap implementation
  (such as whether it uses garbage collection or not) and keeping the implementation associated with
  the particular language runtime makes it easier to reflect such requirements.

In the case of non managed heap, that is, heap that does not provide garbage collection,
the two basic functions are "allocate a block of given size" (this is what for example
`malloc` does) and "free previously allocated block" (this is what for example `free`
does). Other functions may include the ability to resize an allocated block, to
allocate with particular alignment restrictions, or to provide housekeeping
information about the heap (see the `SEE ALSO` section of `man malloc`).

The library implementation of the heap functions does need the operating system to provide
the memory that it further manages. Typically, the heap library requests relatively large
blocks of virtual memory from the operating system whenever it runs out of memory to
allocate, and then partitions these into relatively small blocks given out to the
application. (Where the kernel manages page aligned blocks with sizes easily in
megabytes, the heap library manages blocks with average sizes in tens of bytes.)

Read up on the basics of implementing a heap in _Arpaci-Dusseau Section 17 Free Space Management_.


**Q1** Make a guess on what is the average size of a block allocated on the heap in
a typical application (obviously, this can depend on many factors, just aim for an
average situation).

Type your response as a single number in bytes.

**A1** ...


**Q2** Look at the following program. What is wrong with it (from memory allocation perspective) ?

```c
#include <malloc.h>

int main (void) {
    int *p = (int *) malloc (sizeof (int) * 16);
    for (int i = 1 ; i <= 16 ; i ++) {
        p [i] = i;
    }
    free (p);
    return (0);
}
```

(If you do not see anything, try running the example with `valgrind ./main`, available on the `lab` machine.)

**A2** ...


**Q3** The chapter selected for self study mentions blocks containing "a magic number
to provide additional integrity checking". In what situation would such integrity
checking discover the issue from the previous question ?

**A3** ...


**Q4** Imagine you just wrote a program that measures the speed of selected file operations.
It creates a single large file on the disk, and, in a loop, always `seek`s to a random
position in that file and `read`s a single byte, measuring the time each read takes.

You will obviously need to store the time samples somewhere. Imagine they are simple
integers, so what you do is you take a linked list implementation (for example
the one you saw in the lab assignment) and use it to store the integers
(every measurement simply appends one integer to the list).

Why is that not a good idea ?

**A4** ...


**Q5** Assume you have an allocator that strictly follows the "first fit" strategy.
Propose a pattern of calls to `malloc` and `free` that would create a particularly
fragmented heap, that is, a heap where the total sum of free blocks is large but
the size of the largest available free block is small.

**A5** ..


And finally, a mandatory summary question:

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** Is there a "perfect" usecase for the different "fitting" approaches? Mainly interested in an example for the `worst fit` if there even is an example of it being useful.
