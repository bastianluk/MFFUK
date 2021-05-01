# Memory Model Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should understand what the race conditions are
- You should feel relatively secure solving basic race conditions with tools like locks or condition variables
- You should also be able to implement other basic synchronization tasks such as bounded buffer or rendez vous
    - Bounded buffer
        - For example semaphores that track free and full space
        - Or condition variable that signals buffer insert and remove operations
    - Rendez vous
        - Trivial signalling with semaphore
        - Or again condition variable

- You should have a basic understanding of how cache coherency works with the MESI protocol
    - Cache split into lines
    - Each line has a state
        - Shared for data that can exist in another cache too
        - Exclusive for data that is only held locally
        - Modified for data that contains changes
    - Transition between states
        - Writing requires exclusive or modified local copy
        - Reading forces other copies to become shared

- And you probably feel that memory models are quite complex and not all that clear


### Locks

#### Summary for L7 G1
- question was about spin lock `while (!__atomic_fetch_and (lock_state, ~1)) { }`
- correct
    - unlocked value must be non zero but yield zero after lowest bit masked out
    - the only value that fits this is 1
- incorrect
    - any odd value can be used as unlocked value
        - the loop in `spin_lock` would not necessarily terminate
    - any non zero value can be used as unlocked value
        - the lock would not necessarily be locked after return from `spin_lock`

#### Summary for L7 G3
- question was about threads locking in different order
- most people spotted the deadlock variant
    - so think how that could be avoided in large and complex code

Some other questions popped up:

    - how would a read write lock be implemented ?
        - start with the basic lock with queues
        - imagine two `int` variables `readers` and `writers` instead of just `bool locked`
        - the waiting condition for read lock is simply `if (writers)`
        - the waiting condition for write lock is `if (readers || writers)`
        - the real complications come when considering what is correct dynamic behavior

    - are locks also memory barriers ?
        - at the very least `lock` does `acquire` and `unlock` does `release`

    - can we `goto` inside a lock or outside a lock ?
        - see "shoot yourself in foot" :-)


### Semaphores

#### Summary for L7 G4
- question was about a lock implemented using a semaphore
- correct
    - owner not considered (one thread locks, another unlocks)
    - unlock before lock possible (unlock, then two locks)
    - recursive locking with explanation also accepted
- incorrect
    - first thread locks, second thread locks which blocks, first thread unlocks, second thread wakes and unlocks
        - saw this in several answers but I could not figure out what was intended to be the difference here

There was just one other question:

    - can we up a semaphore above initial value ?
        - TL;DR yes
        - there is obviously also some maximum value
            - `man sem_post` reports `EOVERFLOW` when the maximum allowable value for a semaphore would be exceeded
            - Win32 API `CreateSemaphore` must specify user defined maximum count


### Condition Variables

#### Summary for L7 G5
- the question was a simple condition variable usage
- incorrect
    - forgot that `wait` temporarily unlocks the given mutex
        - and so suggested nothing will ever get printed
    - forgot that `wait` also locks the given mutes
        - and so suggested anything from `K` on can get printed
- discussion
    - should the variables be `volatile` ?
    - is this about the waiter thread being "fast enough" ?


### General Synchronization

Few questions concerned general synchronization issues:

    - when should we use lock free data structures ?

    - is there a formal discipline studying safe concurrency ?
        - sure, depends what side to approach the problem from
        - the problem is not only about "safe"
            - "efficient" to saturate current hardware
            - "intuitive" so humans can use it
            - ...

### Threads

And finally a few questions concerning threads.
Some suggested taking a look again at possible thread states:

- A _running_ thread is actually using the processor (the processor is executing its instructions)
- A _ready_ thread
    - is in the scheduler queue (or whatever structure the scheduler uses) but there is no processor that runs it
    - can become running at any time by the decision of the operating system scheduler
- A _waiting_ (or _sleeping_) thread
    - is not in the scheduler queue and scheduler will not wake it
    - typically in a queue for a synchronization primitive (lock or such) or for an I/O operation
    - will be moved to the ready state when the event it waits for occurs (lock unlocked or I/O operation completed or such)

- Calling `sleep` (as in `man 3 sleep`) will put the thread into _waiting_ state
- Calling `yield` (as in `man pthread_yield`) will put the thread into _ready_ state

Few other questions:

    - why are the thread arguments `void *` ?

    - can we assign threads to cores ?
        - `man sched_setaffinity`
        - cgroups CPU controller
        - NUMA policies
        - ...

    - what bus is used by the context switch ?


## Current Content

The first of the new topics for self study concerned memory accesses.

### Memory Access

The first point to take away is the existence of caches and how they impact memory accesses.

#### Summary for L8 A1
- the question was about false sharing
- discussion
    - quite vague phrasing in many explanations
        - look at the MESI animation for explanation
    - can we automate false sharing detection or avoidance ?
    - and will the compiler do it for us ?

Some more questions concerned the MESI protocol and general cache stuff:

    - what other protocols than MESI ?
        - modern variants with cache forwarding (MESIF, MOESI)
        - but reality always more complex !

    - when are dirty cache lines written back to memory ?
        - on explicit cache flush operations
        - when making space for other data in cache
        - sometimes when other processor requests the data
          but modern architectures can do cache to cache transfer

    - what is prefetching ?
        - software can give hints what data to fetch before it uses them
        - hardware can guess what data will be needed
            - accesses in streams with regular stride
            - coding patterns such as call and return
              but this is more under branch prediction than prefetch

    - what are the common cache line sizes ?
        - Intel 64B everywhere but prefetches adjacent line in pair so it sometimes looks like 128B
        - At least some ARM platforms have instruction cache line 32B and data cache line 64B
            - Processor reports this in Cache Size ID Register
        - Some POWER platforms have cache line 128B
        - So it really depends :-)
    - and why ?

    - can a memory operation use just part of cache line ?
        - critical word first fetch is common
        - non temporal stores bypass cache
        - but otherwise pretty much no

    - any difference between L1 and L2 and L3 except for sizes ?

From that we quickly got to the memory access ordering questions:

    - why is instruction reordering beneficial ?
    - so what are the fences (barriers) anyway ?
    - are memory fences an alternative to locks ?

    - what is the scope of the Intel processor memory fence operations ?

    - how to search for "fence" in the Intel processor manual without going mad ?
        - text search happens to work quite well in this case
        - there is also the index at the end of the manual
        - what do you learn from sarcastic questions ?

    - how much of the memory access ordering rules is managed by the operating system ?
    - and how much magic can hardware provide on the memory ordering side to help the software ?


### Memory Models

From the memory access ordering questions there is just one step to the memory models.
A quick summary of what the memory models are trying to do:

- The programmers want reasonably intuitive program semantics
    - Single threaded semantics well established
    - Multiple threads usually understood through interleaving
        - This is tricky anyway if atomic operations are just reads and writes
        - See "serializability" on reads and writes vs "linearizability" on objects
        - But feels comfortable enough to most people
- The compilers and the hardware want maximum (ordering) freedom for efficiency
- So we look for a compromise

One canonical example for interleaving semantics (assume `print` just prints and other obvious simplifications):

```c
volatile int a = 0;
volatile int b = 0;

void thread_one_function (void) {
    a = 1;
    print (b);
}

void thread_two_function (void) {
    b = 1;
    print (a);
}
```

Interpreting this through interleaving:

- either `thread_one_function` is faster and then output is A=1, B=0
- or `thread_two_function` is faster and then output is A=0, B=1
- or both are about equally fast and then output is A=1, B=1

But in practice often also A=0, B=0 is possible.

To see what guarantees are given, it is useful to understand these basic concepts:

- ordering
    - program order (anything a single thread prescribes)
    - synchronization order (anything synchronization prescribes)

- data race
    - any non ordered conflicting (at least one write) access to shared variables

- consistency models
    - sequential consistency (some interleaving that matches explicit ordering)
        - this is the guarantee we think programmers might like
        - in some models given to data-race-free programs
    - happens before consistency (any behavior that matches explicit ordering)
        - permits for example cyclic speculation dependencies
        - but (relatively) easy implementation with barriers

This roughly outlines the memory model of Java that was in the self study.
Obvious question was whether this is similar everywhere.

    - why did we read about Java ?
        - because it is possibly one of the most well described and reasonably practical memory models

    - how much do memory models differ across languages ?
        - Go https://golang.org/ref/mem
            - just use channels for communication (or `sync` package if you really need it)
            - "if you must read the rest of this document to understand the behavior of your program, you are being too clever"
        - Rust https://doc.rust-lang.org/reference/memory-model.html
            - no stable memory model (yet ?)
            - https://github.com/rust-lang/unsafe-code-guidelines
        - Python PEP 583 https://www.python.org/dev/peps/pep-0583
            - check this out for a lot of nice examples
            - but beware this was later withdrawn
        - .NET CLR defines permitted reorderings
            - https://docs.microsoft.com/en-us/archive/msdn-magazine/2012/december/csharp-the-csharp-memory-model-in-theory-and-practice
            - https://docs.microsoft.com/en-us/archive/msdn-magazine/2013/january/csharp-the-csharp-memory-model-in-theory-and-practice-part-2

    - so do we really have to study thousands of pages of processor documentation just to know if our program works ?

Many people were surprised when reading about the "out-of-thin-air" values.

> From http://www.cs.umd.edu/~pugh/java/memoryModel/unifiedProposal/testcases.html
>
>     Initially,  x = y = 0
>
>     Thread 1:
>         r3 = x
>         if (r3 != 42)
>             x = 42
>         r1 = x
>         y = r1
>
>     Thread 2:
>         r2 = y
>         x = r2
>
> Behavior in question: r1 == r2 == r3 == 42
>
> Decision: Allowed. A compiler could determine that at r1 = x in thread 1,
>   it must be legal for to read x and see the value 42. Changing r1 = x
>   to r1 = 42 would allow y = r1 to be transformed to y = 42 and performed
>   earlier, resulting in the behavior in question.

- see
    - https://doi.org/10.1145/2618128.2618134
    - https://doi.org/10.1145/3276506

- seeing through interactions between compiler optimizations and hardware behavior is difficult
- also the behavior does not have to happen in practice to be a problem
    - impossible to perform some verification tasks if out of thin air values permitted
    - future designs need to know the boundaries of permitted behavior

And then some questions that suggested memory models feel quite complex:

    - so how come I could live and code quite fine without knowing about memory models ?

    - good literature about memory models in Czech or Slovak ?
        - I really have no idea
        - think about how that could happen ?
            - the topic is old and stale enough that people had time to translate it
            - top tier researcher decides to publish in Czech or Slovak
            - ... ?
        - English but a decent read is also https://queue.acm.org/detail.cfm?id=2088916

    - did not get the double checked locking example
    - how come double checked locking took so long to fix ?
    - so does Double Checked Locking work or not ?
        - depends on what you mean by "work"
            - low bar: an implementation that is functionally correct with particular compiler on particular hardware
            - high bar: an implementation that is functionally correct and improves performance with any compiler on any hardware

And finally a collection of assorted questions:

    - who are the memory model rules for ?

    - will `volatile` variables have barriers around ?
        - yes (if needed) in Java
        - not in C++ (use atomics)
    - also, can a data race happen on a `volatile` variable ?
        - not in Java (by definition)
        - yes in C++ (again, use atomics)

    - is "data race" the same as "race condition" ?
        - data race is typically a precisely defined behavior in particular memory model
        - race condition is generally any (potentially undesirable) behavior that depends on scheduling
        - some people even distinguish "benign" and "critical" race conditions to indicate whether they lead to bugs

    - how much are data races really happening ?

    - what is the relationship between sequentially consistent execution and program and synchronization order ?
    - if there are no synchronization actions in code does the memory model still limit execution ?


## Miscellanea

    - timer interrupt in `msim`
        - please ask these types of questions in the mailing list
        - see the assignment for task 04 which has the details

    - felt like information for graded quiz was not provided
        - please be more specific, we can look into this

    - provide function signature or link to documentation so that we do not have to search for them
        - for what functions were you missing signatures ?
        - how far down a Google result list were they ?
