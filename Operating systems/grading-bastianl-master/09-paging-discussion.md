# Paging Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Leftover Bits And Graded Quiz

### L8 G2
- was about fixing notification through shared variable
- correct
    - make the shared variable volatile
        - not strictly correct because reordering still possible
        - essentially the same issue as double checked locking
        - but accepted as close enough
    - make the shared variable atomic per C memory model
        - atomic variable accesses enforce ordering
        - this is not the same as volatile
    - add synchronization around the variable access
    - replace variable access with synchronization
- discussion
    - tempting to use a condition variable without a loop
        - one thread `wait`s
        - the other thread `signal`s
        - but remember spurious wakeup !

### L8 G3
- was about data races between two threads
- correct
    - no data races because all variable accesses ordered
- discussion
    - is this a race when there is no read ?
    - what about the `lock` and `semaphore` variables ?
        - obviously we cannot require synchronization to perform synchronization
        - but there is a distinction between synchronization object and reference to it

### L8 G4
- was about data races between two threads
- discussion
    - no data race does not mean known final variable values
        - variable `b` in the example was always accessed under lock
        - but the final value could still depend on scheduling

### L8 G5
- was about which set implementation is faster when
- discussion
    - what about prefetching ? branch prediction ? cache eviction policy ? ... ?
    - using Big O when we are really after exact operation count
    - what exactly is "consistently"

### Synchronization

In general the answers were mostly correct, there were very few questions about synchronization:

    - how do we tell the compiler and the processor about the memory barriers needed in synchronization objects ?
        - synchronization tools must be part of the language
        - see for example https://doi.org/10.1145/1065010.1065042
        - but there are usually low level constructs that permit implementing own synchronization

    - can memory model permit reordering across locks ?
        - usually locks have at least acquire-release semantics
            - no reads and writes can be reordered before acquire
            - no reads and writes can be reordered after release
        - that still permits some reordering
            - for example moving stuff inside a lock
            - useful for lock coarsening optimizations

    - if Java writes (32-bit) integers atomically, does that mean there are no data races on them ?
        - data races are not about atomicity but about ordering
        - Java also needs guarantees for programs with data races
            - in particular reference values cannot be invented
            - that means some assignments must always be atomic


## Summary So Far

- You should understand why we want address spaces per process
    - Isolates programs from each other
        - Impossible to overwrite memory that cannot be addressed
        - No (or very little) restrictions on address ranges used

- You should understand how paging creates address spaces
    - Addresses used by applications are always virtual addresses
        - Processor will perform translation transparently
        - Operating system configures different translation for different processes

- You should have a rough idea of all things provided through paging
    - Efficient switching between address spaces on context switch
        - Just select a different translation structure to use
        - Everything else can stay in place (in physical memory)
    - Handling of some memory fragmentation issues
        - Unused virtual address ranges do not have to have physical memory assigned
        - Contiguous virtual address ranges do not have to map to contiguous physical memory ranges
    - Handling of some memory capacity issues
        - Rarely used content does not need to be held in physical memory pages
            - Typically swapped to disk by the operating system
            - But other solutions such as compression or swapping to network exist
        - Content can be fetched on demand on first access
    - A few more advanced uses will be discussed later
        - Sharing pages between different address spaces
        - Using paging to reduce memory copying
        - Using paging to access files
        - ...


## General Address Translation

Few initial questions revolved around address translation in general:

    - why do we bother, what is the purpose ?
        - a reasonably well understood mechanism for process isolation and some other things
        - apparently easy enough to implement in hardware that processor manufactures do not mind

    - is there something better than paging ?
        - depends on better for what
            - for handling some memory issues segmentation might work better
                - transparent relocation of virtual address ranges
                - more fine grained logical protection
                - more flexible addressing
            - in some situations paging is not needed at all
                - small embedded systems
                - trusted code platforms

    - can processes configure their own address translation ?
        - typically not the essentials of physical address selection
            - that would possibly break protection between processes
            - and it is rarely needed anyway
        - but sometimes some things can be tweaked from process side
            - see `man madvise` for memory usage hints
            - see microkernels and pagers for more

It is also useful to point out certain basic things:

- address translation must be very efficient (because it is essentially done all the time)
- address translation structures must be reasonably small (because they are needed for each process)


## Virtual and Physical Addresses

First again a small refresher of what was in the self study:

- virtual addresses
    - these are all the addresses that programs see and use
        - numerical pointer values are virtual addresses
        - even in envirnments that do not reveal pointer values the addresses are there
        - registers such as stack pointer and program counter contain virtual addresses
    - any virtual address is transparently translated by the hardware to a physical address upon use
        - the processor does this while executing instructions
        - no extra program actions needed

- physical addresses
    - these are the addresses that are sent out to the bus between the processor and the memory (and other devices)
    - physical addresses are (almost) never useful to programs
        - so what are the exceptions ?
            - when virtual address translation is not configured yet
            - when talking to devices that need to know addresses
            - when configuring the address translation
            - when allocating physical memory
        - these tasks typically handled by kernel

### L9 Q4
- question about type of addresses in page table entries
- consider
    - to use a virtual address it must be translated
    - the translation requires traversing the page table
    - traversing the page table requires using addresses stored in it
    - where would you go from here if the addresses were virtual too ?

For some reason, disks have again crept into the picture in your questions:

    Physical pages are on disk, right ? So how do we access memory ? Or do we
    mean the disk when we talk about physical memory ?
    Do processors prefetch disk to memory ?

Few more questions concerned sizes of the addresses:

    - sizes of virtual vs physical address spaces ?
        - virtual addresses sized to fit software needs
            - currently 32 bits seem a bit on the small side but 64 bits a bit too much really
            - so many current 64 bit processors really use only some of the 64 bits (like 48)
            - but there were also research experiments with for example 128 bits
        - physical addresses sized to fit hardware capabilities
            - right now the high end seems to be in the terabyte range
            - so we are really talking 40-ish bits in physical addresses

    - why so odd numbers of bits ?
        - on physical side saving bits saves wires
        - on virtual side things are often rounded up


## General Paging Principles

There are only few major things to understand about paging:

- we do not really translate addresses of individual bytes, we translate addresses of larger blocks
    - necessary otherwise just remembering how to translate would eat up all memory
    - those larger blocks are called pages
        - sometimes a distinction between _pages_ (virtual memory or memory content) and _frames_ (physical memory)
        - pages exist simply because we decided to interpret memory as being cut into blocks
            - there is no "other memory outside pages"
            - there are no "headers for each page"
- a (virtual) page is either _mapped_ or not
    - mapped means the address translation structures tell us the corresponding physical address
    - not mapped means hardware cannot translate the address and will trigger an exception
        - that exception can mean many things
            - the page may not have been allocated and an attempt to use it is invalid
            - the page may have been allocated but not yet mapped
            - the page may have been allocated but swapped out
            - the page may have been access protected
            - ...
        - and hence the operating system handling can also vary


### L9 Q1
- question was about splitting virtual address into page number and offset
- incorrect
    - offset as start of page from start of address space
    - offset in high bits, page in low bits
    - page 12 bits, offset 20 bits
- discussion
    - if an offset tells you a location in a 4kB page, it should not be larger than 4095
    - how does hardware perform the computation ?
        - page number = floor (address / page size)
        - page offset = address % page size
    - division really not needed when page size is a power of two
- please please please just leave the answers in hexadecimal


There were also many additional questions:

    - How long was paging around ?
        - https://retrocomputing.stackexchange.com/questions/12697/was-there-a-clearly-identifiable-first-computer-to-use-or-demonstrate-the-use

    - Can we run out of physical memory ?

    - How can we have more virtual memory than physical memory ?

    - What about other things that are addressed but are not memory, like peripherals, are they also paged ?

    - What is the best page size ?
        - And is it the same for 32-bit and 64-bit systems ?
        - And is it related to how much physical memory we have ?
        - Do the page sizes fit the cache line sizes ?
        - Are these related to heap block sizes ?


    - Is paging slow ?
    - Is paging available everywhere ?
    - And are there operating systems that do not do paging ?
    - Could we bypass address translation for cases where we need speed ?


## Address Translation Structures

For the address translation structures we will use the two Figures 4-2 during the lecture so no content here except for your questions.

    - What does `VPN` and `PFN` stand for ?
        - Virtual Page Number
        - Physical Frame Number

    - What good are hardware supported paging tables anyway ?
    - And how come hardware copes when they are so complex ?
    - And are they still there or are they a relic ?

    - Does hierarchical paging table structure make sense ?
        - Do paging directories always pay off ?
        - How many levels are used in practice ?
        - How many levels of page tables are optimal ?

    - Can we print the content of page tables ?
        - https://stackoverflow.com/questions/20069620/print-kernels-page-table-entries

    - How are the protection bits used, does an exception happen on violation ?
    - Is the global bit meant for shared kernel memory ?
        - Yes and yes

Some questions were MIPS specific:

    - How to pass 40-bit addresses for translation on MIPS ?
    - What if we run out of ASID identifiers (on MIPS) ?

### L9 Q3
- correct
    - Figure 4-2 shows two page sizes but from the numbers it cannot be 16MB so it is 4kB
    - so VPN = 0x12345, PFN = 0xABCD5, Offset = 0x678
- incorrect
    - Offset is the entire part matching between VA and PA
        - could be if Figure 4-2 did not say choose either 4kB or 16MB pages
        - so in principle not really incorrect for MIPS in general
    - top three bits of PFN select segment
    - VPN and PFN with the same values
    - top eight bits of VPN are ASID

### L9 Q5
- incorrect
    - trying to consider the physical address too
        - the fields in question were indices into various page table levels
        - indices are derived from the virtual address only
        - physical address is assembled from table content
    - problems splitting numbers at given bits


## For Next Week

And then there is a stack of questions that we will address the next week so not sorted too much here.

### Operating System Implementation

    - How can an operating system be portable when paging structures are processor specific ?
    - How are pages for processes allocated and deallocated ?
    - And how are pages allocated in memory, do they have headers and such, or ?

    - Can we context switch without flushing TLB ?
    - Can we save TLB as part of context switch ?

    - Why kill process on invalid address access rather than simply map some memory to it ?

    - Can we combine multiple page sizes ?
        - For example in different address ranges ?

    - Who decides the replacement strategies, hardware or software ?

### Hardware Related Tidbits

    - If a device reads from memory we write to, is that a data race (for example the console device we use) ?
    - And what if two threads try to program a device, is that a data race ?

    - Why do memory accesses need to be aligned ?

    - What is CR3 ?
    - What are the MIPS segments ?
    - Is there a special register for current ASID ?
    - Why do MIPS TLB entries have pairs of physical addresses ?

    - How much slower would hardware without TLB be ?

    - Is sharing TLB with ASID a security issue ?
    - Is there some reserved space in TLB per ASID ?
    - Is software TLB or hardware TLB faster ?

    - Is the video RAM handled the same way ?

    - Is paging on ARM better than on Intel x86 ?
    - Is paging the same on Intel and AMD x86 ?

    - Can TLB overflow ?
    - Is cache computation taking processor time ?
    - Why does the `cpuid` command print so many cache and TLB sizes ?
    - Where are the (L1 L2 L3) caches ? Do they use virtual or physical addresses ?

    - Do different memories have different speeds ?
    - So why are caches faster than memory ?

### Swapping

    - How to decide the victim for eviction ?

### L9 Q2
- correct
    - obviously cache entries of different address spaces must not be mixed
        - throw away everything, or
        - remember address space identifier for each entry, and
        - nothing really needs to be done when switching between threads of the same process
- incorrect
    - a different (single) page is used after context switch
    - each process has its own TLB cache
- discussion
    - can we context switch that too ?


## Miscellaneous

- Why did we not start with paging ?
- Why does `kmalloc` not fail on out of memory ?
- Can you also give us the answers to questions in writing ?
    - I always try to do that here
    - If you miss an answer mail me
