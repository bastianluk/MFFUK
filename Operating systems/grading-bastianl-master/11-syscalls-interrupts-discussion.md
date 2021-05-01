# System Calls and Interrupts Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Graded Quiz

### L10 G1
- was about what memory blocks are backed by anonymous swap (rather than specific files)
- correct
    - most answers included stack and heap
    - many also named `vdso` and `vvar` and `vsyscall`
- discussion
    - are kernel pages also swapped out and in on demand ?
    - `vdso` and `vvar` for system call acceleration

### L10 G2
- was about two processes memory mapping the same file with a shared mapping
- discussion
    - will changes be seen with a delay or immediately ?
        - after the file is updated ?
        - after `msync` is called ?
    - POSIX standard vs Linux man page
        - memory mapped file vs shared memory object (`man shm_open`)
        - for whatever reason the POSIX standard is not explicit here
          https://pubs.opengroup.org/onlinepubs/9699919799
            - but supports for example placing semaphore in mapped shared _file_
    - think about implementation
        - sharing the same physical page
            - the same guarantees as threads of the same process on different processors
        - using different physical pages
            - what would be the semantics of writes ?
        - something else altogether ?

### L10 G3
- was about swapping out some parts of hierarchical page tables
- correct
    - the structures are designed to support this (even if practical utility is questionable)
- incorrect
    - the page table directory entry contains physical address so no virtualization
    - we would need a page table for a page table and that is a cyclic dependency
- discussion
    - top level obviously has to stay
    - does this happen in a real system ?
    - can an address mapped through a swapped out page table remain in TLB ?
    - how much do we save, really ?
        - `cat /proc/meminfo | grep PageTables` (only last level)
        - Intel one page table for 512 or 1024 pages so small gain ?
        - but what if the virtual address space is heavily fragmented ?

### L10 G4
- was about number of misses with LRU strategy
- correct
    - `X > C ? X*L : X`
    - corner case `L == 0` ignored :-)
- discussion
    - TLB vs L1-L2-L3 vs swapping
    - is the first access also counted as a miss ?
    - the `X*L` variant essentially means no caching !

### L10 G5
- was about speed differences between COW and private memory
- correct
    - first write to a page will be slower with COW
    - otherwise the timing should be the same
- incorrect
    - writes are slow because they go to disk
    - all reads or writes are faster or slower
    - writes are slow because they are copied to other processes
- discussion
    - resistance to noise when there is just one sample
    - what about TLB and L1-L2-L3 caches ?


## Left Over Bits

### Operating System Implementation

Questions related to system internals:

    - The `/proc/self/maps` file shows a mapping for what exactly ?
        - `self` means "this process"
        - use `/proc/<pid>/maps` for given PID
        - obviously requires appropriate permissions

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

    - When we access a page and it is not `present` how do we know where it is ?
        - Page table entries can sometimes hold this information (swap index and offset)
            - Imagine having room for 40 bits of physical address
            - That can be used to index one of 2^40 swap pages
            - 4 PB of swap should be enough for everybody :-)
        - Otherwise additional structures

    - Does everything need to be mapped to a file or an anonymous mapping ?
        - There may be regions that never get swapped out
        - Typically (some) kernel memory is a good bet
        - Also `man mlock`

    - At what size do memory mapped files start making sense ?
        - Does not fit very well into typical heap (especially managed)
        - Things smaller than a page obviously not very practical
        - Otherwise pretty much always

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

    - How does hyperthreading differ from multicore ?
        - https://www.howtogeek.com/194756/cpu-basics-multiple-cpus-cores-and-hyper-threading-explained

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

    - How do we know Belady was right about the optimum policy ?
        - https://doi.org/10.1145/321623.321632

    - Do different memories have different speeds ?
    - So why are caches faster than memory ?

    - Is cache computation taking processor time ?
    - Why does the `cpuid` command print so many cache and TLB sizes ?
    - Where are the (L1 L2 L3) caches ? Do they use virtual or physical addresses ?

    - Why do MIPS TLB entries have pairs of physical addresses ?
    - What is a conflict miss ?
    - Can TLB overflow ?


## Summary So Far

### System Calls

- You should have a reasonable idea of how system calls work
    - Library wrappers to handle impedance mismatch between function call and system call
    - Secure entry point that elevates privileges
    - Argument passing and checking

Collected questions:

    - why not use standard function calling conventions ?
    - how are the registers in the ABI chosen ?
    - how many registers can be used ?
        - some system call interfaces may require use of some registers
            - for example Intel SYSEXIT expects arguments in `RDX` and `ECX`
        - preparing arguments in registers may require using (other) registers
            - so using too many registers may actually complicate things
        - register use only makes sense if the system can use the values in registers immediately
            - otherwise it will have to store the arguments somewhere (stack) anyway

### L11 Q2
- was about `get_user` macro purpose and checks
- correct
    - serves to access user space pointers in portable manner
    - type checks (static so not so interesting)
    - pointer validity checks (address range)
    - checks if page faults are permitted
- discussion
    - how is `get_user` useful to us ?
        - purpose of the macro ?
        - purpose of the question ?
            - see a real example of syscall argument handling
            - learn to use open source code for information
            - get an impression of real code complexity
    - how can we tell what sources are for 80x86 when browsing Linux kernel ?
        - subdirectories under `arch`

### Interrupts

- You should have a good idea of how interrupts are used
    - Whenever asynchronous notification mechanism is required
        - Servicing devices
        - Inter processor interrupts
    - Quite similar to processor exceptions
- You should have some idea of how interrupts are handled
    - Some minimum hardware support required
        - Saving return address
        - Addressing nesting (disabling interrupts)
    - Software handling obvious
        - Save registers trashed by handler (similar to context switch)
        - Execute handler to service the device
        - Restore registers and return

Collected questions:

    - can we have nested interrupts ?
        - in most cases we try to avoid this
        - sometimes difficult
            - TLB miss inside other interrupt handler
            - NMI signal (usually fatal conditions anyway)

    - if interrupt is like a context switch could we try using fewer registers and thus reduce overhead ?
        - this would require writing handler in a language with control over registers
        - possibly done in some very special situations where the savings matter

    - what is the interrupt latency ?
        - definition
            - time from interrupt request to interrupt service
        - typical values
            - expect averages in small hundreds of instructions
            - but peak latencies are also important
                - hence disable interrupts for short time only
                - also means deferred processing (more later)
    - how can we provide service guarantees ?
        - if we really want hard guarantees then real time kernel needed
        - interrupt handlers can be subject to (real time) scheduling

    - are interrupts handled one by one ?
    - what if two interrupt requests arrive at the same time ?
        - interrupt controller usually defines priorities
        - handler can look for additional service requests on exit

    - how are interrupts delivered to cores ?
        - https://unix.stackexchange.com/questions/516115/whats-the-policy-determining-which-cpu-handles-which-interrupt-in-the-linux-ker

    - what happens to interrupts delivered while CPU has interrupts disabled ?
        - typically request persists until handled
        - device can give up (and usually signal lost interrupt)

    - can a device interrupt too often ?
        - and is that a security issue ?
        - and can we detect that ?

### L11 Q4
- was about accounting interrupt handler time
- sorry about deduced vs deducted
- discussion
    - being just ?
        - spread equally ?
        - card driver has own quantum ?
        - how to determine the responsible party ?
        - whoever performed the I/O is likely sleeping at time of interrupt ?
    - interaction with scheduler
        - why not adjust the timer so that the interrupt overhead is hidden ?
        - the timer interrupt register is saved as part of context ?
    - handler must context switch to whoever performed the I/O ?

### Direct Memory Access

- You should very roughly understand the purpose of direct memory access
    - Processor too expensive to serve as a device to copy data blocks
    - Copying through processor doubles bus traffic
- The intricacies of bus protocols involved were left aside

Collected questions:

    - is DMA obsolete ?
    - is DMA engine part of CPU ?
    - and why are smart controllers called bus masters ?
        - some literature uses relatively narrow definition of DMA
            - DMA controller separate from device controller
            - essentially from the days of PC ISA bus
        - bus master concept needed in modern bus arbitration protocols
        - so sometimes devices that can do DMA are called bus masters
          (because they can assume that role in the bus arbitration protocol)

    - do we have to explicitly program DMA or is it somehow used automatically ?
    - is memory-to-memory DMA always available (and does it happen automatically) ?

    - is the operating system in full control of the transfers ?
    - can device overwrite useful data in memory ?
    - does DMA always use physical addresses ?

    - how can we prevent DMA attacks through devices ?
        - https://www.kernel.org/doc/html/latest/x86/intel-iommu.html
        - I/O MMU is a good idea in principle but bugs do creep in :-(

    - what if DMA access memory cached by CPU ?
        - https://stackoverflow.com/questions/54263838/how-is-dma-cache-coherency-kept-on-intel-chipsets
        - but in non Intel world many other things exist (see for example ARM network accelerators)

    - how can memory serve the processor and the devices at the same time ?
        - or multiple processors for that matter ?

    - any downside to using DMA apart from overhead ?

    - is RDMA used ?

### L11 Q5
- was about where the address for DMA comes from
- discussion
    - too late or too complicated ?
        - card asks the processor for address
        - card asks the DMA controller for address
        - card reads the address from the CPU registers
        - the address is determined during interrupt handling
    - not flexible enough
        - each device has a hardcoded address range
        - configured at initialization (vs at start of each operation)
    - the memory can reside on the card itself

## Devices

### General

Collected questions:

    - should driver availability and quality determine our OS choice ?

    - is there a different driver for each device ?
        - and how are the drivers recognized and loaded ?
        - and how does the driver know device address ?
        - try `udevadm monitor --property` and insert an USB device
        - also try `systemd-hwdb query` with the `MODALIAS` value observed

    - how are the addresses assigned ?
    - how are device addresses distinguished from memory addresses ?
        - what is the advantage of having dedicated `IN` and `OUT` instructions ?
        - when should we use I/O addresses and when memory mapped I/O ?

    - is device data buffered somewhere ?
    - can we use some devices without interrupts ?

    - how does PCIe work ?
        - https://pcisig.com/specifications?field_technology_value[]=express
        - joking is nice but please ask questions that can be reasonably answered during online meeting

    - material about C++ I/O libraries ?
        - please ask David Bednarek

    - what are some unusual devices that exist in the system ?
        - just do `lspci` or `lsusb`
    - was there ever a (comically) useless register ?
        - there were certainly hardware design flaws
        - but not sure if this was what you meant

### Mouse

    - do interrupts happen continuously when I move the mouse ?

### Keyboard

### L11 Q3
- was about expected keyboard interface
- discussion
    - code mapping
    - multiple keys pressed
    - handling of ALT CTRL SHIFT
    - handling of sequences of keys (why ?)

### Display

    - how does the MSIM console device really light up pixels on the screen ?
    - can we have more MSIM consoles ? will it exhaust registers ?
    - how does the CPU access the RAM on the GPU ?

### Network

    - can a network card deliver to correct buffer depending on connection ?
    - where is the boundary between hardware and software processing ?
