# Paging Algorithms and Applications

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday December 10 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


## Demand Loading

**Q1** Name (at least) two blocks (by their content) in the virtual address space of a process that are typically
backed by anonymous swap rather than mapped from a file. (By blocks, this question means continuous
virtual address ranges such as those allocated by `mmap` and shown in `/proc/self/maps`.)

**A1** ...


## Mapped Files

**Q2** Imagine two processes that map the same file into their virtual address spaces,
the mapping is shared. (For a difference between a shared and a private mapping
you can see `man mmap`.) Can the writes to the mapped file by one process
be observed by the other process ?

**A2** ...


## Swapping

**Q3** If a system with paging runs low on physical memory, it will swap some pages of the running processes to disk.
Can it also swap some page tables of those processes to disk, or do these have to remain in memory
as long as the processes are running ?
(For an example page table structure consider the one from the Intel processor manual Volume 3A
Section 4.5 on Figure 4-2 "Linear-Address Translation to a 4-KByte Page using 32-Bit Paging".)

**A3** ...


## Victim Choice Policies

**Q4** Assume a (fully associative) cache organized into
cache lines of 64 bytes with strictly LRU victim choice policy.
Assume the cache is initially empty and can hold at most C cache lines.
Give a formula for the number of cache misses for a program that accesses
X addresses exactly 64 bytes apart in a loop that repeats L times.
Your answer should (obviously) be a function of C, X and L.

**A4** ...


## Copy On Write Memory Mapping

**Q5** Imagine some blocks in memory of a process are copy on write shared with another process,
some other blocks are private. Could you use access (read or write) timing to determine which
blocks are which, and if so, how ?

**A5** ...
