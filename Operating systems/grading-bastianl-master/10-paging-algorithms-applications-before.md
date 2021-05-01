# Paging Algorithms and Applications

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday December 3 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the second of two self study modules that will look at virtual memory and paging.
The goal of this module is to discuss how paging is used in the operating system,
beyond the obvious construction of virtual address spaces.


## Demand Loading

We already know that paging is used to construct the virtual address spaces of individual processes.
How are those address spaces populated with content ? Recall what the address spaces contain:

- The program code (and static program data). These are put in the address space by the operating system loader when starting the process.
- The initial program stack. Again, this is something the operating system loader has to allocate.
- Heap and other content. This is content that the process allocates as it executes.

You can see the occupied portions of the address space yourself.
On a Linux system, run the `cat /proc/self/maps` command.
You should see a list of blocks with addresses,
mapping flags (such as `r` for readable, `w` for writeable, `x` for executable),
and sometimes additional information, in particular what file the content was read from, if any.

**Q1** Why do all the block addresses in the listing above end with several zeros ?

**A1** ...

As an important point, the allocated portions of the address spaces are not really loaded immediately.
Instead, the operating system simply records in its internal structures that a particular block should contain particular content.
Only upon the first access is that content actually put into some physical memory frame and mapped to the appropriate virtual address.


## Memory Mapped Files

Keeping track of where the page content came from is useful when the system starts running out of physical memory.
Pages that were not modified since being loaded can simply be discarded because they can always be loaded again.
If we choose to do so, pages that were modified can be written back to disk, to the files where they came from.

This gives us an interesting interface to files at the application level.
Rather than repeatedly calling `read` to read file content to memory,
and possibly `write` to write any modifications to disk,
we can simply request that the file content is mapped to some address range.
Whenever a program access some page in that range for the first time,
the access attempt triggers an exception that will load the data.
And whenever a program modifies some page, the content can be
eventually written back to the file too.
This is called _Memory Mapped Files_.

See `man mmap` for an interface to this functionality.

It is worth noting that executables and libraries are mapped rather than loaded, too.

**Q2** There are both time and space savings related to mapping rather than loading executables. Can you briefly explain ?

**A2** ...


## Swapping

What if we are running out of physical memory ?
Pages that were mapped from files and not modified since being loaded can simply be discarded and the frames reused.
Pages that were mapped from files and modifed can be written back if appropriate.
What about pages that should not be written back (such as those of executables),
or pages that were not mapped from a file (such as those of heap) ?
The operating system sets aside some storage for those pages,
called _swap_, otherwise the mechanism is quite similar.

**Q3** Do you have swap in your system ? Can you tell what is its maximum capacity and how full is it right now ? Answer with numbers in megabytes.

**A3** ...


## Victim Choice Policies

At this point, we have already seen two places in the system memory management where caching plays a role.
One place is the TLB cache, which caches the results of recent address translations.
Another place are the L1-L2-L3 caches, which cache the content of physical memory.

In some sense, we can say that demand loading and swapping is also a form of caching,
one where the virtual memory content is cached in the physical memory.
This analogy helps us look at the victim choice policies together.

A victim selection policy is simply the algorithm that decides what cache entry to free when the cache is full.
This is needed in TLB when a new address translation is computed,
in L1-L2-L3 when a new cache line is read, and
in paging when a new page is mapped.

The policies typically have one goal and that is maximizing performance,
which usually translates into minimizing victim evictions across execution.
Please read the Arpaci-Dusseau Chapter 22 Policies to learn more about this topic.

You should read enough to understand:

- Optimum Replacement Policy
- Least Recently Used Policy
- Two Hand Clock Policy

**Q4** What is _locality of reference_ ?

**A4** ...


## Other Applications

There is just one other application we have space for now, and that is _Copy on Write_.
Please find the topic mentioned in the Arpaci-Dusseau Chapter 23 Complete Virtual Memory Systems.

**Q5** Can you find one practical example where copy on write is useful ?

**A5** ...


SURPRISE ! Since we have accumulated enough question last week,
there is no mandatory last question this week. But you can
ask if you want to, just add the **A0** header back :-)
