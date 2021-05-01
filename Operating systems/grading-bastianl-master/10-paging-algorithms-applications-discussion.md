# Paging Algorithms and Applications Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Graded Quiz

### L9 G1
- was about maximum possible page size
- incorrect
    - looked at the same digits in hexadecimal rather than binary

### L9 G2
- was about maximum range addressed through single second level table
- discussion
    - when using an equivalent of thousands separator in hexadecimal,
      is it each three digits or each four digits ? :-)
      `0x400_000` or `0x40_0000` ?
      https://www.python.org/dev/peps/pep-0515

### L9 G3
- was about virtual address in four level paging scheme
- correct
    - binary 00000000-00000000-01000000-01000000-00110000-00000100
    - expressed in hexadecimal 40403004h
- incorrect but accepted
    - counting entries from one rather than zero
    - binary 00000000-10000000-10000000-01100000-01000000-00000101
    - expressed in hexadecimal 8080604005h
- discussion
    - why are so many people not sure whether first entry is index 0 ?
    - and if first entry is index 0 then why would first byte be offset 1 ?

### L9 G4
- was about some page table entries being the same among more processes
- correct
    - the content of some pages is shared
    - how does this happen ?
        - `fork` copies address space
        - shared memory communication
        - shared kernel address ranges
        - `mmap` of the same files (libraries)
- incorrect
    - the same page table entries in different processes use different physical addresses
    - the shared memory pages must use the same virtual addresses in all processes
    - the processes use different offsets in these pages to avoid collision
        - this is really not done and would not work well
    - these are the pages that are swapped when out of physical memory
        - these would not stay in the page tables
- discussion
    - using accurate phrasing
        - processes "share their memory"
        - some memory is "shared by all processes"
        - processes "share their physical addresses"

### L9 G5
- was about recognizing page table entries and TLB entries
- correct
    - page table entries do not store virtual address information
- incorrect
    - TLB would not have "rights" or "flags"
    - PTE would have "next" field


## Summary So Far

- You should understand how paging works at technical level
    - Translation through multiple levels of page tables
    - Translation caching
- You should understand what is done in hardware and what in software
    - Typical CISC processors
        - Page tables traversed by hardware
        - TLB filled and searched by hardware
    - Typical RISC processors
        - TLB searched by hardware
        - TLB filled by software
        - Page tables traversed by software


### Demand Loading

- You should understand how address spaces are populated with mapped blocks
    - Operating system remembers what data goes at what addresses
        - Data read from files written back to files
        - Data read from files written to swap
        - Data zero filled written to swap
    - Actual reading is done on first access attempt
        - Access attempt triggers page fault
        - Inside fault handler data loaded and mapped
        - The same access retried on return from fault handler

### L10 Q1
- question was about zeros in address space layout
- correct
    - everybody recognized the zeros are offsets
    - but fewer people pointed out the content is page aligned
    - and perhaps even more important the system works with whole pages
- incorrect
    - aligned to 4 bytes (or 8 bytes) (or 16 bytes)


### Mapped Files

- Simply exposes how demand paging works

### L10 Q2
- question was about benefits of memory mapping executables
- correct
    - selective loading of those pages that are needed (used)
    - with mapping pages can be shared
- incorrect
    - working with memory is faster than working with disk
    - access is sequential so we do not need seeking
    - no need to load data into physical memory
    - access is fater than read syscall
- discussion
    - what does this mean for the executable structure ?


### Swapping

- What to do when physical memory runs out
    - Free some pages for use
        - Non dirty content can be dropped and read again when needed
        - Modified content needs to be stored somewhere
            - Typically to file or swap
            - But other solutions exist

- How to make swapping faster ?
    - Maintain some minimum share of free pages
    - So victims chosen before memory actually runs out
    - Sometimes also choose victims that are easier to free

### L10 Q3
- question was about your swap size
- discussion
    - most people reported their swap being empty
    - specific number hard to tell with dynamic swap
    - "I have 128GB RAM"
    - why not have swap ?
        - with runaway process system stuck swapping until system kills it
    - why have swap ?
        - survive temporarily demanding memory applications
        - some rarely used content will be pushed out

From your other questions:

    - How are shared pages swapped out (when there are more page table entries) ?
        - Kernel data structures (must) permit locating all mappings of a page
        - See for example https://lwn.net/Articles/23732 and related discussion

    - Why is system swapping when memory is not yet full ?

    - What should the swap size be ?


### Copy On Write

### L10 Q5
- question was about uses of copy on write
- discussion
    - kernel same page merging
    - programming language COW
    - filesystem COW


### Victim Choice Policies

- A similar problem in all caches we saw so far (TLB, L1-L2-L3, swapping)
- Common goals
    - Maximize performance which means minimize cache misses
    - Perfect choice requires knowing future
    - So real strategies guess
- Differences
    - TLB
        - Strategy in hardware (CISC) or in software (RISC)
        - Limited information about entry usage and history
        - Handles one process at a time most often
        - Must be fast relative to instruction clock
    - L1-L2-L3
        - Strategy in hardware
        - Limited information about entry usage and history
        - Handles several processes together
        - Must be fast relative to instruction clock
    - Swapping
        - Strategy in software
        - Limited information about entry usage but more flexible history
            - Accessed and dirty bits in page table entries
            - Dirty bit in TLB entries
        - Handles processes and file cache together (page cache)
        - Must be fast relative to disk access

- When does the strategy really matter ?
    - TLB
        - When a program intensively accesses a wide range (hundreds or more) of page addresses
    - L1-L2-L3
        - When a program intensively accesses a large amount (tens of megabytes or more) of memory
    - Swapping
        - When all programs together access more memory than the system has available

- Actual strategies used ?
    - Various LRU approximations
      https://arxiv.org/abs/1904.06278
    - Simple strategies such as two hand clock
    - Adaptive strategies such as ARC for storage

### L10 Q4
- question was about locality of reference
- discussion
    - guarantees vs assumptions
    - processor vs process
    - memory vs disk
    - and does it hold really ?

From your other questions:

    - Who decides the replacement strategies, hardware or software ?


### Operating System Implementation

Questions related to system internals:

    - How can an operating system be portable when paging structures are processor specific ?
        - This is indeed quite difficult
        - Processor specific code separate (HAL)
        - Linux for example has page table abstraction for all processors
            - Intel page table definition https://elixir.bootlin.com/linux/latest/source/arch/x86/include/asm/pgtable_64_types.h
            - MIPS page table definition https://elixir.bootlin.com/linux/latest/source/arch/mips/include/asm/pgtable-32.h
            - Generic definitions in https://elixir.bootlin.com/linux/latest/source/include/linux/pgtable.h

    - How are pages for processes allocated and deallocated ?
        - Operating system has to keep track of all pages
        - The rest is essentially page cache victim policy
            - Free physical pages prepared on background
            - Physical pages assigned when handling faults or few other situations (`man mlock`)
            - Interestingly swap space not reserved on allocation (`man mmap` and see `MAP_NORESERVE`)

    - Can we combine multiple page sizes ?
    - For example in different address ranges ?
        - Explicit allocation see https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt
        - Automatic merging see https://www.kernel.org/doc/Documentation/vm/transhuge.txt
        - Physical memory fragmentation causes problems
            - Large pages must also start at aligned (physical) addresses (otherwise offset could not be simply copied)
            - That means either reserving some physical memory for large pages or page migration

    - Where does the system keep the page tables ?
        - And does it remember the physical addresses of the page tables ?
            - Yes of course :-)

### L9 Q2
- question was about TLB content on context switch
- correct
    - obviously cache entries of different address spaces must not be mixed
        - throw away everything, or
        - remember address space identifier for each entry, and
        - nothing really needs to be done when switching between threads of the same process
- incorrect
    - a different (single) page is used after context switch
    - each process has its own TLB cache
- discussion
    - can we context switch without flushing TLB ?
    - can we save TLB as part of context switch ?
        - possible for example on MIPS but I think not done
            - such TLB copies would have to be updated on address space changes
        - some systems maintain software extension of hardware TLB for fast fill

Questions related to system operation:

    - Why kill process on invalid address access rather than simply map some memory to it ?
        - Not always killed
            - COW implementation
            - Tricks such as bumper pages
        - So invalid is only access to memory that was not explicitly allocated
            - And that is usually in line with how programs are implemented
            - And it would be trivial to allocate entire memory first
                - But that is bound to run out of memory eventually


### Hardware Related Tidbits

Some questions related to concurrency:

    - If a device reads from memory we write to, is that a data race (for example the console device we use) ?
    - And what if two threads try to program a device, is that a data race ?

    - Why do memory accesses need to be aligned ?

More questions related to addressing:

    - What are the MIPS segments ?
    - What are the Intel segments ?

    - What is CR3 ?
    - Is there a special register for current ASID ?
        - MIPS CP0 EntryHi (see also `cp0_set_asid` in `cp0.h`)
        - Intel CR3 serves as an address space identifier too

    - How much slower would hardware without TLB be ?
    - Is software TLB or hardware TLB faster ?

    - Is there some reserved space in TLB per ASID ?
        - No but there are reserved TLB entries on MIPS
        - Does address of TLB miss handler needed translating ?

    - Is sharing TLB with ASID a security issue ?

    - Is paging on ARM better than on Intel x86 ?
    - Is paging the same on Intel and AMD x86 ?

    - Is the video RAM handled the same way ?

And questions related to cache internals:

    - Do different memories have different speeds ?
    - So why are caches faster than memory ?

    - Is cache computation taking processor time ?
    - Why does the `cpuid` command print so many cache and TLB sizes ?
    - Where are the (L1 L2 L3) caches ? Do they use virtual or physical addresses ?

    - Why do MIPS TLB entries have pairs of physical addresses ?
    - What is a conflict miss ?
    - Can TLB overflow ?
