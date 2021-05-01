# Synchronization Tools Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Administration

- Thank you for the answers in the anonymous survey !
    - We will post a summary in the mailing list
    - Quite high participation
        - 92 students submitted responses
        - 94 students claimed points


## Summary So Far

- You should be able to describe what a race condition is
    - Any bug that only exhibits itself in particular concurrent execution scenarios
    - Definition of a race condition therefore requires definition of correct (expected) behavior !
        - For example two threads doing `I++` is given as basic race condition example
        - But there are programs that do this "approximate counting" on purpose,
          typically to avoid high costs of frequent atomic increments

- You should be able to explain what is meant by atomicity in this context
    - Race condition is exhibited with unfortunate interaction between critical sections
    - When critical sections execute without interaction (atomically) we have no race
    - At least for certain definition of critical sections (getting circular) :-)

- You should understand how locks help make critical sections mutually exclusive
- You should be able to implement your own lock
    - A spin lock with use of reasonable atomic operations
    - A lock with queue

- You should understand the semantics of semaphores and condition variables


## Atomic Operations

So far, we have simply accepted atomic operations as "something that the hardware provides".
This is in fact pretty much as far as we can go in this course,
so here are just a few points that react to your questions:

    - how are atomic instructions done ?
        - asked in the context of multiple processors and memory caches
        - from processor perspective often enough to get exclusive cache line
            - there are protocols for that and demo will be included for next class
            - the protocols require caches to cooperate across entire system
            - Intel sometimes calls this cache locking
        - some operations require atomic block of multiple bus operations
            - processor can temporarily request exclusive bus access
            - Intel calls this bus locking

    - why is synchronization said to be slow ?
        - one reason is overhead of atomic operations
            - particularly painful with many processors and contention
                - latency increase between cache and memory operations ~100x
                - exclusive bus access reduces concurrency across system
        - try the trivial race condition example with `__atomic_fetch_add`

    - is there a way to perform the atomic operations in a high level language ?
        - you have seen the built in functions in GCC
        - modern dialects of C and C++ have atomic types
        - more complex languages tend to prefer concurrent data structures
    - are the GCC atomic builtins standardized ? and is there something really portable ?
        - origin in Intel ICC compiler so in a way standardized :-)
        - C language standard has atomics since C11
        - C++ language standard has atomics since C++11
        - portability is a tricky question
    - and how can the compiler give me an atomic operation when it is really a hardware thing ?
        - fall back on library implementation

    - if each processor has a different set of atomics how is the operating system made portable ?
        - Linux essentially tries to provide common set of portable operations
            - https://www.kernel.org/doc/html/v4.10/core-api/atomic_ops.html
            - https://www.kernel.org/doc/html/v4.10/core-api/local_ops.html
        - but atomic alone is not enough, we need full memory model (next class)
            - https://www.kernel.org/doc/Documentation/memory-barriers.txt
        - maybe things will change at some point in the future
            - https://lwn.net/Articles/586838

    - what is better a counter with a lock or an atomic counter ?
        - lock implementation often contains atomic counter
        - but lock implementations can do fast path optimizations

Few other related questions:

    - where is the `atomic_reset` complement to `atomic_test_and_set` ?
        - this is simple atomic write
        - basic memory read and write operations often atomic by default
            - for reasonably sized types
            - must be aligned correctly

    - what good is `test-and-set` if we have `compare-and-swap` ?
        - available operations depend on processor and some may not be available
        - also see theoretical results on strength
            - Herlihy Consensus Number https://doi.org/10.1145/114005.102808
            - But also https://doi.org/10.1145/164051.164070

    - could we do entirely without atomics ?
        - some operations simply will be atomic
            - can you imagine hardware with non atomic single byte memory write ?
            - so typically we have atomic (small) reads and writes
        - more advanced things can be built from atomic reads and writes
            - think Dijkstra or Peterson algorithms
            - but there are limits (for example fair lock for N threads needs O(N) variables)

    - how is `test_and_set` done on MPIS ?
        - take a look at the `ll` and `sc` instructions
        - essentially a loop that tries the update until it succeeds


## Summary For L7 Q1
- question was about possible results for atomic test-and-set
- correct
    - possible results (true, false) and (false, true)
- discussion
    - the first thread will return `false` and the second will return `true`
        - good for our intuitive understanding of things
        - but the perceived order of threads can change
    - if a third thread did X then Y
        - introducing arbitrary extra actors into these questions makes any result possible
    - many answers started with "if the operation is _really_ atomic then ..."
        - why would you not believe that it is ? :-)
    - this is not necessarily a lock


## Summary For L7 Q2
- question was about possible results for atomic compare-and-swap
- correct
    - possible results (true, 0, false, 65) and (false, 66, true, 0)
- discussion
    - semantics of this particular `__atomic_compare_exchange` not clear
    - sometimes confused with the example in the book
        - the types were not really compatible
        - the question did not make sense then (why modify the expected value ?)


## Locks

Locks are everywhere, beware of terminology differences. Few keywords:

- A lock and a mutex are the same thing
    - Also sometimes "binary semaphores"
    - Windows calls some locks "critical sections"
- A spin lock is a lock that waits actively
    - Threads that cannot proceed consume processor time by repeatedly checking lock
    - Sometimes a combination of spinning for a while and then sleeping
- A ticket lock is a type of spin lock that enforces fairness
- A recursive lock is a lock that remembers how many times it was locked
    - Repeated locking only possible from the same thread
    - Must be unlocked as many times as was locked
    - Quite useful when nesting code with locks

It appears some students are still not quite sure about the lock with queue:

    - how exactly does the lock with queue implementation work ?


## Summary For L7 Q3
- question was about spin lock with atomic exchange
- correct
    - simple solution is `while (atomic_exchange (&l, true)) { }`
    - but equally working is for example `while (!atomic_exchange (&l, false)) { }`
    - or perhaps written more generally `while (atomic_exchange (&l, STATE_LOCKED) != STATE_FREE) { }`
- discussion
    - is this really used in practice ? or useful anywhere ?
        - this code is actually quite bad for caches
        - better is to spin with read and then try with atomic operation
    - confusion in terminology
        - the `test-and-set` example in the book really is an atomic exchange
        - the `test-and-set` example in the question (and elsewhere) is really different

    - would you accept `while (atomic_exchange (&l, 1) == l) { }` ?
    - and is it different from `while (l == atomic_exchange (&l, 1)) { }` ?

    - not all infinite waiting is deadlock


Few more questions concerned the cost of locking:

    - what is the overhead of locking a lock ?
        - important concept is fast path
            - most common case which is locking of non contested lock
            - many optimizations cater to fast path
                - biased locking https://blogs.oracle.com/dave/biased-locking-in-hotspot
                - futex https://lwn.net/Articles/360699
        - but this is also about scalability
            - contended locks are pretty much always bad
            - some ways to bypass this
                - optimistic synchronization
                - lock free data structures
                - reader writer locks

    - can we influence the order in which the waiting threads get the mutex ?
        - sometimes fairness (FCFS) or near fairnes is provided
        - but it is not always the best thing to have
        - with spin locks scheduler decides

    - any reason to lock in a single threaded process ?

    - is spinlock ever good (better than lock with sleeping) ?
    - why is yielding so much worse than sleeping in the lock implementation ?


And few other questions:

    - do locks and similar primitives need protection so that applications do not violate them ?
        - essential to say whether we are after threads or processes
            - examples so far were all between threads of the same process
            - and in general it makes little sense to try protecting threads from each other
        - but there are also locks and similar primitives between processes
            - need to invent naming because pointers typically do not work
            - association with protected content often just convention

    - what about using existence of files as a lock ?


## Semaphores

These are really not all that different from locks on the implementation side,
but questions about usage were quite persistent:

    - are the negative states really there ?

    - do we ever need a semafore with an initial value other than zero or one ?

    - about a million questions on the same note
        - should we ever use semaphores if we have locks and condition variables ?
        - when should we use condition variables in place of semaphores ?
        - should we use semaphores in place of locks ?

The answer to these usually boils down to few simple things:

    - sometimes you only have some tools available
        - Windows did not offer condition variables until after XP
        - Java only offered semaphores since 1.5

    - some tools are quite good fit for some problems
        - a simple not very hot critical section ?
            - just use a lock, that is what they are for
        - blocking producer on buffer full (or consumer on buffer empty ?)
            - semaphores set to buffer capacity work well with this
        - wait for multiple threads to finish work (or tell multiple threads to start) ?
            - semaphores or sometimes specialized barrier objects
            - but condition variables also work
        - wait for complex mix of conditions ?
            - condition variables usually the only choice
        - access to very hot shared data structure ?
            - get a specialized concurrent implementation
            - or refactor to avoid this :-)
        - and so on


## Summary For L7 Q5
- question was about use of semaphores
- correct
    - output either "1" or "2" and one thread always remains waiting
- discussion
    - why is buffering mentioned ?


## Condition Variables

Condition variables offer quite flexible synchronization mechanism with passive wait.
There seemed to be some confusion about correct usage, here in bullets:

- Typical scenario is waiting for variable(s) to have specific value(s)
- All access to those variables must be protected by a lock
- Whoever waits tests the value(s) in a `while` loop with `wait`
    - The `wait` call temporarily relinquishes the lock
    - On return the lock is again acquired
- Whoever modifies the value(s) must send a signal with `signal` or `broadcast`

As a result, we get mostly blocking wait for arbitrary conditions.

Selected questions:

    - how is use of condition variable different from spinning on a global variable ?
        - with condition variables the waiting is (mostly) passive

    - does `signal` put the caller to sleep ?
    - can we signal outside the mutex ?
        - because what would be the use of waking a thread that will wait on the mutex anyway ?
            - theorists have proposed different models
                - Mesa semantics
                - Hoare semantics
                - Brinch Hansen semantics
            - but practice is simpler :-)

    - how are condition variables implemented internally ?
        - https://sourceware.org/git?p=glibc.git;a=blob_plain;f=nptl/pthread_cond_wait.c;hb=HEAD

    - why is there spurious wakeup ?
        - https://stackoverflow.com/a/1051816

    - do we need the threads to be in a parent child relationship as suggested in self study link ?


## Summary For L7 Q4
- question was about use of condition variables
- correct
    - output either "1" and one thread remains waiting or "12"
    - obviously except for spurious wake up


## Assorted Synchronization Questions

There were a few questions related to implementation of threads or synchronization primitives:

    - how does a thread sleep ? and what if there is no ready thread ?
    - how does a thread wake another thread ?
    - can a thread wake itself ?

    - does the language implementation (of a synchronization primitive) always call the operating system implementation ?
    - are synchronization objects a library thing or a language thing or something else entirely ?
    - how portable are the synchronization primitives like locks ?
        - system API not at all
        - language API quite often

    - tried adding extra processor to `msim` and the basic kernel said `HHeelllloo  WWoorrlldd`, why ?

    - are there tools that look for synchronization issues ?
        - yes but there are limits to what can be done
        - see for example `valgrind` with `drd` or `helgrind` tools
        - goes back to the need to define what a synchronization issue is
            - some languages have a clear definition of a data race
            - some issues like deadlocks are always bad
            - also excess overhead can be detected

    - locks and semaphores are quite old but are in relatively new languages like C# so is there no progress ?


## Miscellanea

    - a good reference on makefiles ?
        - I would just say use documentation ?
        - https://www.gnu.org/software/make/manual
        - do not spend too much time on this beyond basic concepts and rules

    - information about concurrency on GPU ?
        - NPRG058 Advanced Programming in Parallel Environment

    - additional literature on parallel computing ?
        - more on memory models
            - C++ memory model stuff https://www.hboehm.info/c++mm
            - Java memory model FAQ https://www.cs.umd.edu/~pugh/java/memoryModel/jsr-133-faq.html
            - Intel memory ordering information in processor manuals currently Vol. 3A Section 8.2
            - Overview of data coherency issues https://doi.org/10.2200/S00962ED2V01Y201910CAC049
            - The kind of issues that are still tough http://dx.doi.org/10.1145/2676726.2676995
        - more on non blocking synchronization
            - Herlihy work on wait free synchronization https://doi.org/10.1145/114005.102808
            - Libraries such as Intel TBB https://software.intel.com/content/www/us/en/develop/tools/threading-building-blocks.html
        - more on alternative language constructs
            - Actor models http://hdl.handle.net/1721.1/6952
            - Erlang https://erlang.org/doc/getting_started/conc_prog.html
            - Rust ownership types https://doc.rust-lang.org/book/ch16-00-concurrency.html
        - more on concurrency and consistency in distributed systems
            - CAP theorem https://doi.org/10.1145/564585.564601
            - Conflict free data types https://link.springer.com/chapter/10.1007%2F978-3-642-24550-3_29
