# Garbage Collection Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should know that a running program will have its
  (machine) code, (static) data, heap and stack placed
  somewhere in its virtual address space

- You should have a rough idea of what the code looks like
- You should have a rough idea of what the stack looks like

- You should have a rough idea of what the heap looks like
    - An area of memory used to satisfy dynamic allocations
        - Basic interface is `malloc` and `free`
        - Object allocation in higher level languages
    - Optimized for fast allocation of relatively small blocks
        - In many workloads typically tens of bytes
        - But even large blocks must be supported
    - Internal housekeeping structures also inside heap
        - Actual structure not standardized
        - Quite often lists of free blocks

- You should understand what a memory leak is
    - A failure to free an allocated block when no longer used
    - Accumulation of leaks can eventually exhaust memory


## Selected Left Over Discussion Bits


### Summary for L3 G2
- question was whether access to local or global variable differs in speed
- correct
    - no single big reason for difference
    - but stack may be more likely to be cached
    - in some cases address computation in one or the other case may be (slightly) longer
- incorrect
    - confusion in optimizations
        - wrong: _processor_ decides to put variable in register
        - wrong: _compiler_ decides to put variable in cache
    - accessing variables on the heap takes longer because "they have to be located" first
        - maybe when one is thinking about dynamic languages but that is moving us into way more complex territory
        - often when global scope is dynamic then so is local (but easier to assign offsets to locals than globals)
        - https://stackoverflow.com/a/46798876/345436
    - wrong: global variables must only be accessed for a short time because of multiple threads
    - wrong: stack is closer to code and therefore faster
- related
    - exact terminology global vs exported (did not reflect it in grading)
    - are global variables on the stack ? or in the code ? or where are they ?


### Stack

- what is a frame pointer
    - the data kept by a particular function on stack is said to form a _stack frame_
    - compiler may emit code that connects stack frames into a linked list
        - useful for debugging (inspecting stack content) and more
        - pointers used to create the linked list are called _frame pointers_


### Summary for L3 G3
- question was what happened to program control flow
- correct
    - return address on the stack was modified
    - works only because the functions involved have the same stack frame structure
- incorrect
    - wrong: the address of the (local) strings being printed was modified
    - wrong: the code that does the printing was modified


### Heap Structure

Essentially the questions revolved around what real life heap looks like.

- How is coalescing done if free lists are not sorted by address ?
    - Really depends on actual allocator implementation
    - For example `dlmalloc` and friends also use block footers
        - Next block easy to find (add size to header address)
        - Previous block footer just before this block header
    - Allocators like `tcmalloc` and friends do not coalesce
        - Entire pages for blocks of the same size
        - Removes need for individual headers (!)

- If heap is so complex why not use stack more ?
    - Stack can only allocate and free in LIFO order
    - Good for function call nesting but dangerous otherwise
    - Some environments can use stack instead of heap as optimization (escape analysis)
        - https://shipilev.net/jvm/anatomy-quarks/18-scalar-replacement

- How complex are real heap implementations ?


### Heap Overhead

- Space overheads (L3 G4 G5)
    - Why are blocks on the heap aligned ?
    - What is typical size of heap header ?
    - Can heap blocks be relocated (compacted) to get rid of fragmentation ?
- Time overheads
    - Computers are super fast so why do we care about heap speed so much ?
    - How much slower would it be to alloc all on heap (nothing on stack except control flow) ?

- Should I write my own `malloc` in my applications ?

- Real challenge comes with multicore efficiency
    - Patterns that cause cross core communication (producer consumer)
    - Spare free block balancing strategies (blowup)
    - False sharing everywhere

People interested in overhead numbers from practice may want to read:

- https://doi.org/10.1145/1297027.1297046 for space overheads
- https://doi.org/10.1145/378995.379232 for some time overheads


### Allocation Strategies

### Summary for L3 Q5
- Many quite complicated strategies invented :-)
    - Some incorrect solutions trying to free different size than allocated
    - Some naive recursive solutions assumed ability to allocate in certain part of heap
- But really just allocate lots of blocks and then free every second one
    - If one would be serious then one would think about where to keep all the pointers
    - That can be handled simply by building two alternating linked lists and then freeing one
    - Also note that this is bad for most strategies
- More interesting were thoughts about practical applications
    - Mixing allocations of short lived and long lived objects
    - Typical length of short lived objects would have to grow (!)

- Why look at the antipatterns ? And what are the real patterns ?
- What good is worst fit ? And what allocation strategy is really used ?


## Content For Today

- From self study you should have some idea of what garbage collection is
    - Reachability as a substitute for liveness
    - Basic ideas for reference counting and tracing
        - Counting requires action on all reference lifecycle operations
        - Tracing requires knowledge of roots and object structure


### General Garbage Collection

- how old is the idea ?
    - Lisp introduced GC around 1950s and 1960s
    - generational hypothesis by David Ungar from 1980s
    - Java usage surpassed C++ usage around 2000 (depending on usage definition this can be gamed arbitrarily :-)

- can we please get rid of garbage collection ?
    - at least in certain application domains
    - at least when we are really tough low level gurus

- what exactly is the mutator ?
- does the garbage collector run in a separate thread ?

- how does the garbage collector obtain access to heap memory ?
- how does the garbage collector know where the objects are ?

- can we combine garbage collector with explicit `malloc` and `free` ?
- can we manage memory across more processes, for example garbage collect other heap when we need memory ?

- anything other than garbage collection or explicit `malloc` and `free` ?
    - for example regions in https://www.aicas.com/cms/en/rtsj


### Summary for L4 Q1 and L4 Q2
- questions were about basic reachability
- essential
    - distinguish objects on heap and reference variables
        - names belong to (reference) variables and not objects
        - assignment between variables copies references
            - consider `aa = a` vs `aa = new A (a)`
- observation
    - everything in these simple functions could be optimized away
        - yes but that was somewhat besides the point


### Summary for L4 Q4
- question was about when to perform garbage collection
- correct
    - certainly when running out of memory
- (possibly) incorrect
    - when we do not need to access the heap for a while
    - at the end of each function scope (might be too often)
    - at the end of the `main` method (when code is done executing)
        - remember that this is about _reuse_ of own heap
        - when program terminates the whole address space is dropped
- discussion
    - internal (running out of memory) vs external (can afford delaying now) reasons
    - when the program is idle
    - when there is a lot of garbage on the heap
        - how do you tell what is garbage without running a garbage collector ?
    - what if the collector is running concurrently ?

- when and how often are collections happening ?
    - and how to decide whether to collect or ask kernel for more memory ?
        - https://docs.oracle.com/javase/8/docs/technotes/guides/vm/gctuning/ergonomics.html

- can a collection run be forced ?
    - and what good is it to force a collection run ?

- can garbage collection somehow fail ?
    - consuming too much time
    - waiting for safe points in all threads
    - running out of memory for temporary data


### Summary for L4 Q5
- question was about "stopping the world"
- incorrect
    - only stopping allocations
- discussion
    - is the tracing collection always "stop the world" ?
        - and why is "stopping the world" needed ?
        - and do we stop "just the mutator" ?


### Garbage Collection Requirements

Think about what language features really prevent garbage collection.


### Summary for L4 Q3
- is it still garbage collection if it breaks referential integrity ?

- why still no (widely used) garbage collection in C(++) ?
    - how does conservative garbage collection handle pointer arithmetic ?
    - and what language properties are needed for garbage collector ?
    - and does garbage collection in C++ make it slower ?

- is garbage collector also in the kernel ?
    - or even used anywhere in an operating system ?
    - and could the kernel garbage collect applications ?
    - https://www.usenix.org/conference/osdi18/presentation/cutler

- good material on advanced garbage collectors ?
    - for overview there is [the book](http://gchandbook.org)
    - for G1 collector https://doi.org/10.1145/1029873.1029879
    - for Azul C4 collector https://doi.org/10.1145/1993478.1993491
    - for Shennandoah collector https://doi.org/10.1145/2972206.2972210
    - ...


### Reference Counting

- why is reference counting not more common ?
    - https://doi.org/10.1145/2258996.2259008

- are smart pointers in C++ a form of reference counted garbage collection ?
    - and how come they can coexist with standard C pointers ?

- can we combine reference counting and tracing ?

- where would the reference counts be stored ?
    - garbage collector needs to know where references are in heap blocks
    - this usually translates to knowing what object type each heap block holds
    - which means objects have headers that tell their types and that is a good place for reference counts too

- how can we store extra bits in a pointer (for one-bit counting but also otherwise) ?


### Mark and Sweep

- are all objects always traversed ?
    - collecting generations
    - collecting regions
    - need remembered sets (cross generation or cross region pointers)

- are the roots that are not on heap (stack, registers) also marked and how ?


### Compaction

- graph traversal is good opportunity for patching references


### Garbage Collection Overhead

- what is the overhead compared to explicit memory management ?
    - if garbage collection wins is it because of poor programming with explicit memory management ?
    - https://doi.org/10.1145/1103845.1094836

- what should be the practical take away from garbage collection internals ?
- are there any applications where the overhead becomes prohibitive ?
    - think more about latency (pauses) than average slowdown
    - there are specialized pauseless garbage collectors


# Administration

- can you publish reference solutions to the assignments ?
- can you use C# instead of Java or was that BÚNO ?
