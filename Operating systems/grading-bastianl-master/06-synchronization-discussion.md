# Process Synchronization Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should have quite decent idea of what are processes and threads
    - Process as a concept representing a running program (resources + activities)
    - Threads as the workers that perform individual process activities
- You should have quite decent idea of how processes and threads execute
    - How context switching between threads works
    - How some operating system resources are per process
- Some parts still not discussed
    - Details of how virtual address spaces work (paging)
    - Details of how the system decides who runs (scheduling)


## Selected Left Over Discussion Bits


### Summary For L5 G1
- question was about frames per second
- correct
    - the threads will form a "convoy" on the exclusive communication code
    - the exclusive communication code limits throughput to 10 frames per second max
    - with 10 threads and each taking 1.1 seconds per frame we have `max (10, 10*10/11) ~ 9.09` frames per second
    - with 20 threads and each taking 1.1 seconds per frame we have `max (10, 20*10/11) = 10` frames per second
- discussion
    - "I spent 2 hours on this question"


### Summary For L5 G2
- question was about file handles
- correct
    - file handle is scoped per process
        - think about file descriptor table as being indexed by `(handle, process)` rather than just `handle`
        - think why this is so (for example disclosing information to other processes)
    - even two threads could see the same numbers if they open and close the files in non overlapping time intervals
- incorrect
    - processes execute sequentially and therefore the file descriptor numbers can be recycled
    - each process has its own stack which is why it has its own file descriptor
    - file descriptors are pointers into operating system memory
    - file descriptor table is controlled by the processor
    - program prints addresses of variables


### Summary For L5 G3
- question was about space for stack
- discussion
    - for upper bound estimate we do not need the stacks to be empty
    - we also do not need the stacks to be next to each other


### Summary For L5 G4
- question was about not saving entire context
- correct
    - if the operating system is not sure about the program then it has to switch all
    - in this particular case not switching floating point registers would not break anything
- incorrect
    - we do not have to care because access to registers is fast
        - but remember that registers are saved to and loaded from memory
        - also how often do you think a context switch happens ?
            - quantum looks long
            - but also blocking
- discussion
    - how exactly is floating point operation defined ?
        - any access to floating point registers ?
        - an actual computation ?
    - the operating system cannot know what registers the program uses so it has to switch all
        - we cannot really trust the program to tell us
        - but sometimes we have hardware support
            - but there are known exploits
            - https://arxiv.org/abs/1806.07480
    - this optimization only makes sense with large blocks of registers


### Summary For L5 G5
- question was about broken context switch code
- correct
    - if the program does not carry over any value in that register over context switch
        - can happen by chance (even if unlikely)
        - but can be part of ABI (caller saved)
- incorrect
    - if the register is not used (as stand alone answer not specific enough)
    - if the register always contains zero (as stand alone answer not likely)
    - it must be `zero` or `k0` or `k1` (correct in MIPS context but not complete)
- discussion
    - "register not used" vs "register content not used"
    - how lucky does one have to be ?


## Context Switch

- where does the context switch example save the program counter ?
    - the example is to be used with _cooperative_ context switching
        - when `cpu_switch_context` is called the return address is saved by the processor
            - the MIPS calling conventions use the `ra` register for that
            - that register is saved as part of the context
            - C code often saves `ra` to stack on entry
    - with _preemptive_ context switching similar


## Threads

    - are threads on the same processor different from threads on multiple processors ?

    - how many threads run in the operating system ?
        - Linux as one possible example (do `ps --ppid 2`)
            - one thread per processor to handle soft IRQ routines
            - one thread per processor and work queue
                - kblockd work queue for managing block I/O
                - several event work queues
            - one thread per processor for thread migration
            - one thread per processor for processor hot plug support
            - several threads for memory management and compaction
            - several threads for file system activities
            - ...

    - if a language has threads are they always 1:1 mapped on operating system threads ?
        - quite often but not always
        - same problems as green threads

    - what use is the return value of a thread ?


## Content For Today

We should first take a quick look at scheduling.


### Summary For L5 Q2
- question was about quantum duration
- discussion
    - what is the ideal quantum duration ?
        - think what are the pressures
            - good enough illusion of concurrent execution
            - relatively small context switch overhead
        - typically on the order of ~10 ms
        - but remember waiting means context switch anyway
        - deriving quantum from expected user reaction time or screen refresh frequency
            - which is why Linux CFS scheduler specifies turn around period of run queue (`sched_latency_ns`)
            - try `perf sched record`, then `perf sched latency` and `perf sched timehist` to show wait and exec times and wake latencies

    - how complex are the heuristics to find the right quantum for each process ?
    - would some programs benefit from disabling context switching (to save overhead) ?
    - are games and other soft real-time processes handled in a special manner ?
    - can scheduler recognize hung program (infinite loop or deadlock ?)
    - why does process with high priority not run all the time ?

    - do we schedule threads or processes ?
        - and how are threads assigned to processors ?
        - and do all threads of a process share one quantum ?

    - does the system prefer switching between threads to switching between processes (because it is faster) ?
        - too many aspects of "faster" for a simple answer
        - do we want multiple threads of the same process run on the same core ?


### Synchronization

- You should understand how data race can happen with non atomic access to shared variables
    - You should have seen such data race happen on a small example code
    - You should appreciate how tricky the behavior can be


Some questions you had:

    - what was the most destructive bug you saw involving a race ?
        - let us take this into a public discussion during the lecture :-)

    - can we detect race conditions easily ?
        - first problem is how to define what is a race condition


    - when should `volatile` (not) be used ?
        - good for
            - making sure variables are fetched when asked for
            - and essentially nothing else !
        - not good for
            - guaranteeing atomic access
            - disabling assorted optimizations
        - what can be the cost of lost optimization opportunities ?


On the topic of atomic operations:

    - how are atomic instructions done ?
    - why is synchronization said to be slow ?
    - how can we be sure TestAndSet is really atomic ?
    - is there a way to perform the atomic operations in a high level language ?
    - if each processor has a different set of atomics how is the operating system made portable ?

    - what is better a counter with a lock or an atomic counter ?


Did not manage to process but will do later:

    - additional literature on parallel computing ?
    - what about concurrency on GPU ?


### Locks

- what is a mutex ?
- can a lock be stored in cache ?
- what about fences and barriers ?

- anything faster than a lock ?
- how big is the locking overhead ?
- any reason to lock in a single threaded process ?

- is spinlock ever good (better than lock with sleeping) ?
- how do we know who holds a lock and who is in the queue waiting ?

- how exactly does the lock with queue implementation work ?
- how is "infinite sleeping" better than "infinite spinning" ?
- why is yielding so much worse than sleeping in the lock implementation ?

- what about using existence of files as a lock ?


# Administration

- summarize expected reading at the top ?
