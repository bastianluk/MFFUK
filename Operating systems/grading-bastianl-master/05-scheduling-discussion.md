# Process Scheduling Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should have quite decent idea of what a running program looks like
    - The memory layout of code, heap and stack
    - What the machine code instructions look like
    - What the general purpose processor registers look like
    - The role of some special purpose registers like program counter and stack pointer


## Selected Left Over Discussion Bits


### Summary For L4 G1 and L4 G2
- questions were about memory consumed by heap allocation of linked list
- correct
    - each list element will have its own block
    - each block will have 8B header and 12B useful data for minimum total size of 20B
    - data alignment requires stretching the total size to multiple of 16B which is 32B
    - that means additional 12B padding needs to be added _after_ the end of each data
    - so 1000 list elements will occupy 32000B give or take the last padding
- incorrect
    - padding placed between the header and the data
        - remember that `free` gets pointer to data but needs to locate the header
        - this is only easy when the header is just before the data

### Summary For L4 G3
- question was about object reference counts in code
- correct
    - at location ONE objects ROOT and RIGHT are referenced once, object LEFT is not referenced
    - at location TWO no object is referenced because destroying ROOT released contained references
- discussion
    - in fact objects cease to exist when reference count reaches 0
      so correct answer could also say no reference count exists
      because objects do not exist either
    - no one was brave enough to reply with `40` (`101000` binary :-)
- incorrect
    - forgot that freeing an object must also release all references it holds

### Summary For L4 G4
- question was about garbage collection roots in code
- discussion
    - handling of variable `node` in `main`
        - "not a root because it does not point to anything" (but would be if it did)
        - "is a root even if it contains `null` simply because it is live"
        - accepted both if explanation given
    - handling of variable `i` in `main`
        - "could be a root if `int` were a reference type"
        - accepted either way if explanation given
    - are roots the references or the referenced objects ?
        - you can see both definitions
        - I prefer references (makes more sense)

### Summary For L4 G5
- question was about allocation speed for large array
- discussion
    - average vs best case
        - managed heap probably better than non managed heap in typical case
        - but probably worse in average case (re paper about overheads)
        - and not that much of a difference in the best case
        - accepted either way if explanation given
    - managed heap always has to initialize (not scalars)
    - such a large block would be outside standard heap in both heap cases
    - allocation cost vs initialization cost
        - remember that one cache line fetch equals hundreds of other instructions (that do not access memory)
        - initialization effort therefore equal to doing millions of other instructions (that do not access memory)


### Tracing Collectors

    - how does a collector look for roots ?
        - information from (often JIT) compiler
        - typically only in selected program locations (safepoints)



## Content For Today

From self study you know we want to:

- run more programs in parallel (processes), and
- run more parts of the same program in parallel (threads).

First a discussion whether and why this is (or is not) a good idea.


### Parallelism

    - history quiz (where was parallelization first tackled on big scale ?)
        - probably since ages (think large building projects)
        - in numerical mathematics at least since 18th century
            - cosmological computations but then pretty much everything
            - Wikipedia has some fascinating examples ("human computers")
        - first hardware probably in 1960s
        - but please spare me :-)

The reasons for parallelizing were always the same - speeding things up.
For computers a powerful impetus are physical limits on single core speed.

    - https://software.intel.com/content/www/us/en/develop/blogs/why-has-cpu-frequency-ceased-to-grow.html
    - http://cpudb.stanford.edu/visualize/clock_frequency

The downside ? Parallelism makes everything more complicated.
Probably the best lecture point for this are your teams :-).

    - could the operating system run on its own (dedicated) processor ?

    - how can we tell if more threads will improve performance ?
        - that really depends on what we do with the threads
        - https://en.wikipedia.org/wiki/Amdahl%27s_law


### Processes And Threads

From self study you have a rough idea of what processes and threads are:

- when no threads were around a process was simply a running program,
- with threads the picture is slightly more refined:
    - process refers to the whole program:
        - code and data and whatever is shared by threads,
        - also operating system allocates resources to process.
    - threads are the actually executing activities inside process,
    - so we say process owns its threads (and has at least one).

- what are "process control block" and "thread control block" ?

We also know that while hardware with multiple processors can run multiple threads by itself,
that parallelism is necessarily limited to the number of processors and with more threads
or processes than that, we have to multiplex (cooperatively or preemptively).

    - what is the point of threads with single processor ?
        - sometimes easier program organization
        - especially with blocking operations

    - standard (kernel) threads vs lightweight (green) threads ?

There was also this interesting question, which is obviously related to how a computer with multiple processors works:

    - how do processors communicate with each other ?
        - registers and some cache levels private
        - memory content shared
            - cache coherency
            - sharing works at physical memory level
            - does not care about (current) virtual address spaces
        - signalling via interrupts typically possible

Obviously each thread needs its own stack
(because stack records things tightly tied to control flow and each thread has its own control flow).

    - how does a thread know what stack pointer to use ?
        - stack pointer is simply a register that is set by the system on context switch
        - the system allocates area for thread stack on thread creation
        - so each thread simply uses what it is given
    - and can the thread stacks overwrite each other ?
        - the same answer as stack overflow discussion from before
    - and what happens to stack when thread terminates ?
        - that part of process address space is returned to system
    - and can we compress the stack when not used ?

    - what exactly is a kernel stack of a thread ?
        - thread can manipulate its stack pointer pretty much arbitrarily
            - after all it is just another register accessible in user mode
            - obviously this can make stack stop working (think stack pointer in non allocated address range)
        - kernel requires reliable stack
            - ability to recover from arbitrary stack related issues would make code very complex
            - crashing inside kernel can disrupt kernel data structures
            - so we simply switch to a separate thread when entering kernel code
            - but again we need to have as many stacks as there are threads
        - remember in your assignments there is no user mode (yet)
            - so all your threads always run in kernel
            - and all their stacks are kernel stacks

    - would it not be better it threads did not share memory (and thus avoided race conditions) ?

    - is there limit to number of threads in a process ?
        - man page says `ulimit -T` but in my shell it does not work
        - more practical are memory limits and scheduling issues
    - also how many threads (or processes) are worth it ?
        - think about true parallelism achieved
        - too much parallelism with I/O bound work can mean long queues
        - going beyond processor count with compute bound work does not help



## Context Switch

Understanding context switch requires having a good idea of what the thread (or process) state is.

Recall the example from the self study materials:

```c
#include <fcntl.h>
#include <unistd.h>

void main (void) {
    int file = open ("filename", 0);
    char data [1234];
    int size = read (file, data, sizeof (data));
    // Location X
    close (file);
}
```


### Summary For L5 Q1
- discussion
    - what is "operating system memory" or "operating system state" from the self study text ?
        - think any data structures operating system keeps on behalf of your process
            - handles and descriptors for open files and sockets
            - virtual address space maps and page tables
            - and so on


Context switching of registers seemed generally clear. Related is context switching of stack.


### Summary For L5 Q3
- the question was about context switching stacks
- correct
    - multiple thread stacks are simply multiple blocks of memory in the same address space
    - only stack pointer is switched when thread context is switched
- incorrect
    - with more threads each single stack is smaller
    - stack is saved to and loaded from heap
    - stack is placed in thread local storage
    - each thread has different stack on the same virtual address
    - stack resides in the operating system memory (rather than process memory)


Although we did not talk much about virtual memory yet, a question of context switching address spaces also popped up.

    - how is the address space changed ?
        - address space (typically) described in paging tables
            - give physical page address for each virtual page address
            - switching to different paging table changes address space
                - Intel: special register (CR3) points to current page table root
                - MIPS: special register (ASID) says which translation cache entries apply
        - so at context switch point it is typically just writing to a (privileged) register
        - but the total cost can be (much) larger (because many things are cached)
            - imagine two competing processes actively using 24MB L3 cache
            - filling single 64B cache line can take ~100ns
            - napkin computation says ~40ms refill

Finally, a few assorted questions:

    - does content of (memory) caches survive context switch ?
        - and is that a possible vulnerability ?

    - how can return from context switch (trap) fail ?
        - incorrect context to switch to (for example wrong stack pointer address)

    - what happens to the processor pipeline in context switch ?

    - how many registers do common processors have ?
        - https://en.wikipedia.org/wiki/X86#x86_registers
        - https://en.wikipedia.org/wiki/X86#/media/File:Table_of_x86_Registers_svg.svg


### Code Bits

In the self study materials, you also received a code fragment doing (thread) context switch on MIPS.


### Summary For L5 Q4
- the question was about context structure size
- incorrect
    - registers are 8 bytes each


Some questions revolved around the use of `cpu_context_switch`.
For now think about cooperative context switching.

    - the code of `cpu_context_switch` gets arguments in $a0 and $a1
        - this means they are trashed (used) before context switch can save them ?

    - where in memory does the context go to ?

There were also bits about assembly:

    - difference between "macro" and "function"
        - macros are expanded at compilation time
        - functions are called at runtime
        - many implications
    - and why do we have inline functions when we can use macros ?

And bits about hardware:

    - what are the $lo and $hi registers ?
        - multiply and divide operations have "too large" results
            - possibly twice as big as standard register in multiply
            - quotient and remainder in divide
        - opcodes designed with only one target register field
        - plus possibly other ad hoc design decisions
    - and why are they handled separately in context switch code ?
        - because we need to trash a general purpose register to do that

    - what are the $k0 and $k1 registers ?
        - difficult (impossible ?) to save registers without trashing any
        - having two registers reserved for these tight corner cases helps
    - also must interrupt handling preserve all general purpose registers ?

    - what are the practical differences between 32-bit and 64-bit modes with MIPS ?
        - the most important difference is `msim` only supports 32 bits :-)
        - anyone interested in extending it (as project or thesis) tell us

    - what about context switching the floating point registers ?
        - not used in assignments and not supported by `msim`

And bits about how we code:

    - why have `zero` field in `context_t` when it is not used ?
        - CDO ... among less organized people also referred to as OCD :-)

    - is it good practice to hardcode `context_t` structure offsets ?

    - could we write context switch in C instead of assembly ?
        - or just use something more readable than assembly ?
            - actually assembly _is_ our best shot at readable machine code :-)

    - why does `SAVE_REGISTERS` use opposite order from `LOAD_REGISTERS` ?


### Scheduling

The last remaining topic is scheduling, that is:

- when should we switch context ?
- how to decide what process or thread to run ?

    - do we schedule threads or processes ?
        - and do all threads of a process share one quantum ?

    - does the system prefer switching between threads to switching between processes (because it is faster) ?
        - too many aspects of "faster" for a simple answer
        - do we want multiple threads of the same process run on the same core ?


### Summary For L5 Q2
- discussion
    - deriving quantum from expected user reaction time or screen refresh frequency


    - what is the ideal quantum duration ?
        - think what are the pressures
            - good enough illusion of concurrent execution
            - relatively small context switch overhead
        - typically on the order of ~10 ms
        - but remember waiting means context switch anyway

    - how complex are the heuristics to find the right quantum for each process ?

    - would some programs benefit from disabling context switching (to save overhead) ?

    - can scheduler recognize hung program (infinite loop or deadlock ?)

    - are games and other soft real-time processes handled in a special manner ?


### Synchronization

Finally, a topic for the next lecture is synchronization.
This just collects the few questions you already had.

    - so why is using global shared variables bad ?
    - how exactly do atomic variables work ?
