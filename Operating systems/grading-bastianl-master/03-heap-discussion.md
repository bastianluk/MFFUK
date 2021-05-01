# Process Heap Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should know that a running program will have its
  (machine) code, (static) data, heap and stack placed
  somewhere in its virtual address space

- You should have a rough idea of what the code looks like
    - At the lowest level simply numbers that the processor interprets as instructions
    - Some numbers tell addresses that the instructions use
        - These are important because they may need adjusting during loading
        - Compilers may produce position independent code which tries
          to limit the number of addresses that need adjusting
- You should have a rough idea of how the code is loaded
    - Executable files (applications and libraries) contain blocks of machine code
    - Operating system loader retrieves machine code of application and required libraries
        - Addresses in machine code may need to be adjusted depending on where it is loaded at
        - References to library symbols need to be connected to actual location of those symbols

- You should have a rough idea of what the stack looks like
    - Simply a continuous array in memory and a pointer (in dedicated processor register) to current top
        - Allocation quite fast (simply shifting pointer to top)
        - Allocation must strictly follow LIFO pattern
    - In typical calling conventions used for return addresses and function arguments and local variables


## Selected Left Over Discussion Bits

### Program Code

- What is the difference between `-fpie` and `-fpic` ?
    - http://www.openbsd.org/papers/nycbsdcon08-pie
    - Essentially few more optimizations possible in PIE than PIC because PIE does not support symbol interposition

- The _code_ size was smaller for position dependent code,
  but the _file_ size was smaller for position independent code (L2 G5) ?
    - Smaller procedure linkage table (PLT)
    - Position independent executable does not call local exported functions through PLT
    - Mostly template functions from standard C++ library (but I could be wrong here) ...

### Program Stack

Really just part of the virtual address space (simple array) where stack pointer points to.

- Can I save a variable at a specific stack address ?
    - Stack addresses are not different from any other memory addresses
        - So sure you can write to stack addresses
        - But typically not a good idea

- How is stack capacity determined ?
    - Really just guesswork because that would require knowing what the program will do
        - Large applications have surprisingly high levels of nesting
        - But real killer is naturally recursion
    - https://unix.stackexchange.com/questions/127602/default-stack-size-for-pthreads
- Can we prevent stack overflow ?
    - Detect but not really prevent (we cannot refuse program execution)
    - Virtual memory with bumper pages helps
    - Safe programming environments also do this
    - Also available for example with `gcc -fstack-check`

- Can I access values on stack below the stack pointer, for example
  those values left there after return from a function ?
    - Again, stack addresses are not different from any other memory addresses
    - So sure you can access the addresses, question is what would be the purpose
        - Sometimes used to detect debugging or otherwise obfuscate code
        - Or perhaps post mortem stack dump
    - See also red zone notes on https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64

- What is the speed difference between hardware and software stack ?
- Can the stack stretch from memory to disk ?
- Why do threads share heap but not stack ?

### Calling Conventions

- Asking for details of calling conventions
    - Calling conventions in C ?
    - Calling conventions on MIPS ?
    - Typically general idea suffices
        - Arguments passed (mostly) on stack stored right first (for vararg functions)
        - Return values in registers (large returns converted to caller allocated)
        - Optional standard stack frame structure created on function entry
        - Local variables allocated together with single stack pointer sub
        - Caller cleans stack frame (for vararg functions)
        - May have alignment restrictions (see C++ `max_align_t`)
    - For details look up the specific technical documents
        - https://wiki.osdev.org/System_V_ABI
        - Or ask Jan Hubicka when you meet him :-)

- When are local variables (or arguments) not on the stack ?
- How come operating system (Windows vs Linux on the same processor) can influence stack layout ?

- How will the stack and heap features in our assignment environment be limited ?

### Summary for L2 Q3 and L2 Q4
- incorrect
    - tying stack locations to order of things in source
        - order of functions
        - order of variables

### Summary for L2 Q5
- correct
    - in principle overwrite is possible and then the code would crash
    - but in 64-bit address spaces stack is quite far from code
    - and there will be non allocated memory in between
    - or there will be explicit stack guard page
    - or it will trash heap before code
    - and code is read only anyway
- incorrect
    - stack overflow detected magically
        - memory protection typically plays role

### Memory Management

Most questions will be satisfied in later lectures, just a few here.

- What is segmentation and is it related to segmentation fault ?
- How much should we know about virtual and physical addresses ?
- why not have code, data, heap, stack in separate address spaces ?
- How does the operating system find the usable addresses (gaps vs memory vs devices) ?


## Content For Today

- From self study you should have some idea of what heap allocator does
    - Program heap is really just a large array of bytes
    - The heap allocator keeps track of what bytes are in use
        - Interface is "give me X bytes for my use" (`malloc`)
          and "here you have those bytes back" (`free`)
    - So what are the difficult parts ?
        - Keep track of used memory _inside that same memory_
        - Doing this quickly for a variety of workloads
        - Doing this with low space overhead

### Heap Usage

First few points to clarify from your questions.

- Why not permit partial `free` of previously `malloc`ated block ?
    - Likely not useful in object oriented programs
    - See also `man realloc`

- Who gives us the address of the heap ? How is that memory allocated ?
- What does the operating system do when a program runs out of memory ?
    - Depends quite a lot on what memory you have in mind
    - All blocks on the heap are full
        - Program (heap library implementation) asks operating system for more memory to add to the heap
    - Available physical memory exhausted
        - Operating system will swap some physical pages to disk to free them for other use
    - Available virtual memory exhausted
        - No free physical pages and no place to swap to means something must be killed
    - Will talk about this more in lectures on virtual memory

- Does kernel have its own heap ?
    - And who does it ask for extra memory when heap is full ?

- How is it with heap and multiple threads, what about race conditions ?

- If we need an empty array is it faster to free and allocate new or fill with zeros ?

Now we should see what else can go wrong when using heap.

### Summary for L3 Q2
- Correct
    - Mostly everybody saw the write past end of array
    - Some people also noticed missing `null` check on `malloc` :-)
- Incorrect
    - Call to `free` only frees first array element
        - Most likely confusing this with `new[]` and `delete[]`
    - Iteration is over size of pointer not size of integer

### Summary for L3 Q3
- Not clear how the magic number works
    - Typically expecting more sophistication
    - Is the magic number really used in practice ?
    - How is magic chosen and is it different for each pointer ?
    - How robust is magic to abuse ?
        - Why not compute magic as checksum of header ?
        - Consider why you would attack your own heap (exploits)

- Detecting incorrect usage
    - Think about typical errors (buffer overflow, missing free, repeated free, use after freeing, ...)
    - Must be really quick and with low overhead (which `valgrind` is not)
    - How does `valgrind` work ?

- Does leaking matter to the operating system or will all the heap
  simply be gone after program runs out of memory and terminates ?

### Heap Structure

Essentially the questions revolved around what real life heap looks like.

- How is coalescing done if free lists are not sorted by address ?
    - Really depends on actual allocator implementation
    - For example `dlmalloc` and friends also use block footers
        - Next block easy to find (add size to header address)
        - Previous block footer just before this block header
    - Some allocators do not coalesce
        - For example entire pages for blocks of the same size
        - Removes need for individual headers (!)

- Why not use a bitmap of free chunks instead of a list ?

- If heap is so complex why not use stack more ?
    - Stack can only allocate and free in LIFO order
    - Good for function call nesting but dangerous otherwise
    - Some environments can use stack instead of heap as optimization (escape analysis)
        - https://shipilev.net/jvm/anatomy-quarks/18-scalar-replacement

- What is "top chunk in arena" and why is it special ?
    - Operating system can shrink virtual memory block given to heap
    - Shrinking obviously happens from the top chunk
    - But real allocators use more blocks anyway

- Why not leave (parts of) heap management to the user ?
- How complex are real heap implementations ?
- Is this the same in managed heaps ?

### Heap Overhead

Smart heap use requires understanding of overheads.

### Summary for L3 Q4
- Expected
    - Space overhead is too big
- Other notes
    - Time overhead of allocation is too big
        - Not likely when compared to disk access times
    - Linked list complexity is O(n)
        - But we are only inserting on one end so O(1)
    - Crazy experiment !
        - Yes :-)

More questions related to overhead.

- Space overheads
    - What is typical size of heap header ?
    - Is allocated size rounded up to some multiple ?
    - Can heap blocks be relocated (compacted) to get rid of fragmentation ?
- Time overheads
    - Computers are super fast so why do we care about heap speed so much ?
    - How much slower would it be to alloc all on heap (nothing on stack except control flow) ?

- Should I write my own `malloc` in my applications ?

People interested in overhead numbers from practice may want to read https://doi.org/10.1145/1297027.1297046.

### Allocation Strategies

### Summary for L3 Q5
- Many quite complicated strategies invented :-)
    - Some incorrect solutions trying to free different size than allocated
    - Some naive recursive solutions assumed ability to allocate in certain part of heap
- But really just allocate lots of blocks and then free odd ones or even ones
    - If one would be serious then one would think about where to keep all the pointers
    - That can be handled simply by building two alternating linked lists and then freeing one
    - Also note that this is bad for most strategies
- More interesting were thoughts about practical applications
    - Mixing allocations of short lived and long lived objects
    - Typical length of short lived objects would have to grow (!)
- Scary suggestion to "allocate the whole disk"

- What good is worst fit ?
- Why look at the antipatterns ? And what are the real patterns ?
- And what allocation strategy is really used ?


# Administration

- Took me about an hour to write the shell script
  for listing the average number of shared libraries
  and that was just about 1/10 of the work on the quizzes
    - Do not really code things for one time answer
    - Use better strategy for going through quizzes (shortest answers first)

- Provide reading list on top of the self study instructions ?

- Discuss correct answers to before class quiz ?
    - Note that correct answers are often mentioned in this document

- Put more links into discussion file
    - Trying to keep it reasonable :-)

- Provided chapters do not explain things enough for me (and it takes me too long to read them anyway)
    - The document for heap management was around 5500 words
        - Realistic college level reading speeds probably 300-400 words per minute
        - Difficult technical documents possibly as low as 150 words per minute
        - So the text should take anywhere between 15 and 45 minutes
    - If you are much slower than this
        - Language skills ?
            - We can try to provide references in Czech but there simply are not many
            - Feel free to use Czech in questions and labs
        - Technical background ?
            - This should gradually improve
            - Also ask for extra consultation during labs
            - Non specific statements are not helpful (we need to know what background is missing to help)
        - Other issues (possibly special needs) ?
            - Contact us privately and we will see what we can arrange
