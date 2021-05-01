# Paging

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday November 26 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the first of two self study modules that will look at virtual memory and paging.
The goal of this module is to introduce the hardware details of how paging works on current processors.
It may look like there is quite a lot of reading for this module, however, much of what is presented was
already outlined to some degree in the Principles of Computers and Computer Systems lectures, feel free
to skip the content you are already familiar with.


## Address Spaces

Until now, we have been content with the assertion that each executing process has its own (virtual) address space,
that is, each process is free to use whatever addresses it (or the operating system) sees fit for its code and data.
Each pointer that a process uses contains an address that is local within the particular address space.
This provides the processes with not only freedom but also security - since any address a process
uses is always interpreted as local to the particular address space, there is simply no way
a process can attempt to access other address spaces (not without asking the operating
system, or using side channels that are out of scope for now, anyway).

The question is, how can we provide a distinct address space to each process
when our computer really only has one memory with one set of addresses ?
The answer is - by using address translation hardware.


## Address Translation

In a processor without address translation, whenever a program uses a memory address (for example
to read the next instruction to execute, or to read the data for such instruction), that same
address appears on the address bus of the processor and is seen by the memory chips that
provide the data.

In a processor with address translation, the addresses that programs (typically including the operating
system) use are called _virtual_ addresses, and when used, they are first translated by the address
translation hardware into _physical_ addresses. Only the translated addresses are sent out on the
address bus and seen by the memory chips.

To provide a distinct address space for each process, the operating system configures the address translation hardware
so that the virtual addresses of each address space get translated to different physical addresses. In other words,
each memory block in the virtual address space is actually some block in the physical memory that the given
virtual address range translates to, and since the translations are (mostly) distinct, then so are the
address spaces.

Two details that we set aside for now:

- What if we have more memory blocks in the virtual address spaces than we have physical memory ?
  In that case, the address translation hardware can make some virtual address ranges point "nowhere"
  and let the operating system decide what to do when a process accesses those ranges. The operating
  system can then for example juggle fewer physical memory blocks among a larger number of virtual
  memory blocks to provide the illusion of having more memory, in a way similar to how fewer
  processor cores are juggled between more threads to provide the illusion of concurrent
  execution.

- What if we actually want to share some memory between address spaces ?
  That is also possible, and in fact often done for example with the kernel memory
  (rather than having its own address space, the operating system kernel typically
  occupies certain range in the address spaces of all processes, this makes passing
  data between processes and the kernel easier). When needed, selected virtual
  addresses from different address spaces can point to the same physical
  addresses, effectively leading to sharing.


## Memory Pages

What should we imagine the address translation to be like ?
It must be a simple enough operation so that the hardware can perform it efficiently
(remember, caches excepting, the processor accesses memory pretty much all the time).
It must also be flexible enough to permit remapping of reasonably small blocks of memory.

This is where the pages come in.
When a virtual address is to be translated to a physical address, it is split into two parts
(just imagine splitting a number by drawing a line between certain digits, only done in binary).
The upper part of the address is said to refer to a particular _page_, and is translated.
The lower part of the address is said to be an _offset_ within a page, copied as is.
The translation itself is a simple array lookup, the page number is used
as an index to a paging table that contains the value to replace
the page number with.

By choosing where to make the split between the page number and the page offset
in the virtual address, we determine the size of the pages and the size of
the paging table needed to translate all pages in an address space.
A very common split is at 12 bits, leading to 2^12=4096 bytes
page size.

This particular address translation mechanism is still too simple to be used in practice,
but is good enough to get the basic idea. Please see _Arpaci-Dusseau Section 18 Paging Introduction_
for a more detailed description, or skip to the next content if you are already familiar with this.

**Q1** If we have 4kB pages, what is the page number and the offset for virtual address 0x12345678 ?

**A1** ...


## Translation Caching

Paging tables are stored in memory alongside other operating system and process data.
Since an address translation is required for each memory access,
we need to access the paging tables for each memory access too,
effectively multiplying the number of memory accesses performed.
Since memory is already quite often the performance bottleneck,
multiplying accesses is not really an option.
This is why address translations are cached.

Please see _Arpaci-Dusseau Section 19 Paging Faster Translation_ for details.
Again, if you are already familiar with this content, skip it.

For a practical example, see the MIPS _Processor Manual Section 4.1 Translation Lookaside Buffer and Section 4.2 Address Spaces_. To check if you understand the content, just see if you could explain what is on Figure 4-2.

**Q2** What happens to the address translation cache content when a context switch to a different process takes place ?

**A2** ...

**Q3** If MIPS were to translate virtual address 0x12345678 to physical address 0xABCD5678,
what would be the value of fields `VPN`, `PFN` and `Offset` from Figure 4-2 mentioned above ?

**A3** ...


## Hierarchical Tables

Flat page tables are not practical because they need to be present in memory in their entirety.
We therefore introduce hierarchical tables, where the virtual address is split not into
a page number and a page offset, but into multiple levels of paging entry numbers
and an offset. Each paging level is then resolved in a manner similar to the
flat page tables.

Please see _Arpaci-Dusseau Section 20 Paging Smaller Tables_ for details.
Ignore _20.2 (Paging plus Segmentation)_ and _20.4 (Inverted Page Tables)_.
Again, if you are already familiar with this content, skip it.

For a practical example, see the Intel _Processor Manual Volume 3A Section 4.2 Hierarchical Paging Structures and Section 4.3 32 Bit Paging_. To check if you understand the content, just see if you could explain what is on Figure 4-2.

**Q4** The interim page table entries contain addresses of the next page table levels.
Are these virtual addresses or physical addresses ?

**A4** ...

**Q5** If Intel were to translate virtual (linear) address 0x12345678 to physical address 0xABCD5678,
what would be the value of fields `Directory` and `Table` and `Offset` from Figure 4-2 mentioned above ?

**A5** ...

Closing with the mandatory summary question (note that you must come up with a question for the quiz to be accepted):

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** What was the first architecture (and rougly when) that had to deal with the problem of having the virtual address space larger than the physical one?