# Assignment 05: Virtual Memory Management

After a short break in the form of thread support, preemptive
scheduling, and synchronization primitives, this assignment returns to
memory management. The goal of this assignment is to extend your kernel
with basic support for virtual memory, which is needed for user-space
threads and processes.

Implementing support for virtual memory requires managing the address
translation hardware (often referred to as memory management unit --
MMU), and servicing the address translation exceptions. This in turn
requires the kernel to support the notion of a virtual address space
backed by physical memory. To implement memory mapping efficiently, the
MMU performs address translation with the granularity of virtual memory
pages mapped to (page-sized) physical memory frames. Consequently, your
kernel will need to implement a frame allocator to manage physical
memory.

---

## Address spaces

In general purpose operating systems, an address space describes a
collection of virtual memory areas (VMA) which represent ranges of valid
addresses that a program can use. The virtual memory areas typically
correspond to code, static data, heap, and stack, but there can be more.
The operating system is responsible for establishing the mapping of
(valid) virtual memory addresses to physical memory addresses (with the
granularity of pages).

Your assignment will be simpler, because we require your implementation
to only support a single virtual memory area. The size of the VMA will
be _fixed_ at creation time, and it will _always_ start at virtual
address `0`, even though the first page (i.e., addresses from `0` to
`PAGE_SIZE - 1`) **WILL NOT** be mapped to ensure that dereferencing a
`NULL` pointer triggers an exception.

Threads will be associated with address spaces. When creating a new
thread, the creator can choose whether to share its own address space
with the thread being created, or whether to create a thread with a new
address space of given size. The kernel should destroy the address space
when the _last_ thread using an address space terminates.

The functions in the `mm/as.h` header file represent the interface you
are required to implement to enable address space management. You are
expected to extend the `as_t` structure as necessary to suit the needs
of your implementation.

---

## Frame allocator

As mentioned earlier, address translation requires that virtual memory
pages be mapped to page-sized physical memory frames. Your kernel
therefore needs to be able to allocate the physical frames that will be
used to back the virtual pages, which is what frame allocator does.

In your kernel, physical memory is currently managed (as a single
contiguous block) by the kernel heap allocator. It allows allocating
blocks with a relatively fine granularity, both in terms of size and
alignment. However, the heap allocator is unsuitable for allocating
physical memory frames for address translation, because it stores
service information inside the allocated blocks and the `kmalloc()`
interface is not rich enough to specify alignment (both for size and
starting addresses).

For this reason, memory management is typically layered. The frame
allocator is used to manage physical memory with the granularity of
pages. To be able to provide whole frames, the allocator stores usage
elsewhere (i.e., not in or near the returned frames). The kernel heap
allocator is then built on top of the frame allocator, requesting new
frames when running out of free space. Such a heap allocator therefore
needs to support operating with non-contiguous ranges of physical
memory.

While this would be the preferred solution, an acceptable solution
(albeit one awarded less points) is to split the available physical
memory between the frame allocator and the kernel heap allocator at boot
time (without dynamic resizing).

Regardless of the implementation, the functions in the `mm/frame.h`
include file represent the frame allocator interface you are required to
implement. The functions are expected to work in a manner similar to
`kmalloc()` and `kfree()`, even though they have a slightly different
API.

Your implementation can assume that all physical memory will fit into
the 512MiB segment (`KSEG0`) directly accessible from the kernel.
Neither your frame allocator nor your memory probing code needs to
consider physical memory beyond this.

---

## TLB

The actual address translation on MIPS is done via software-managed TLB,
but only for address from certain ranges. So far your kernel code has
been executing (and accessing memory) at addresses in the `KSEG0` range
(2 - 2.5GiB), which is not mapped to physical addresses via TLB, but
simply by subtracting `0x80000000` (2GiB) from the address. Hence you
did not need to worry about setting up and managing the TLB. This is a
common processor feature, often referred to as _identity mapping_, which
simplifies kernel bootstrap.

However, for user-space virtual memory support, your kernel needs to
manage mapping for addresses in the `KUSEG` range (0 - 2GiB) which are
mapped using the TLB. Whenever a CPU accesses an address in this range
for which there is no translation entry in the TLB, it triggers a *TLB
exception* which the operating system kernel must handle.

Specifically, the kernel needs to use the information about the
exception (especially the contents of the *BadVAddr* CP0 register) to
determine whether the accessed address is valid or invalid for the
currently running thread. If the access is valid, the kernel must insert
a corresponding address translation entry into the TLB and restart the
offending instruction. If the access is invalid, the offending thread
must be terminated.

The processor distinguishes three types of TLB exceptions, but your
kernel should only need to handle the *TLB refill* exception in the
`handle_tlb_refill()` function in `src/mm/tlb.c`. Please consult the
section on TLB exceptions (starting on page 138/158) in the
[MIPS R4000 Microprocessor User's Manual](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/R4000_Users_Manual_2Ed.pdf)
for more details.


To make the address translation more efficient, the MMU of the MIPS
processor supports tagging TLB entries with an 8-bit address space
identifier (ASID) to disambiguate TLB entries with identical virtual
addresses.

The preferred implementation of address translation is to use the ASID
to allow translation entries for different address spaces to coexist in
the TLB. This requires allocating and assigning an unused ASID to an
address space when created, and reclaiming the ASID when an address
space is destroyed.

At the other end of the spectrum, the most basic acceptable solution
(albeit one awarded less points) is to implement address translation
without ASIDs, which just requires flushing the entire TLB on each
address space switch.

There are a few more accepted alternatives in between (and above), see
the [Grading](#grading) section for details.

---

## Thread API

Finally, we require your thread subsystem to implement one more
function, `thread_kill()`, which should forcefully terminate a thread.
This function can be used for example when a thread accesses memory
outside its virtual address space.

If a thread has been killed, the `thread_join()` function should return
`EKILLED` to the caller.

---

## Support code

This assignment again updates the kernel base code to provide the
necessary context and some convenience functions. We recommend to review
the update to get an idea what has changed and where.

### Exception handling

Compared to the previous assignment, there is another change to
exception handling. This time we have added the assembly language
portion of the TLB Refill exception handler. The code is mostly
identical to the general exception handling code, but it calls a
different C function (`handle_tlb_refill()` in `mm/tlb.c`) to handle the
exception, which is where you need to put your implementation of the TLB
Refill handler.

### TLB management

MIPS uses a software-managed TLB and provides special instructions
(`tlbwr`, `tlbwi`, and `tlbr`) for reading and writing the contents of
the TLB. We have made those instructions available to C code through the
`cp0_tlb_*` macros defined in the `drivers/cp0.h` header file.

As an example of how to use the macros, the following code adds a
mapping from virtual addresses `0x2000` and `0x3000` to physical frames
`0x17000` and `0x1B000` for ASID `0x15`.

```c
cp0_write_pagemask_4k();
cp0_write_entrylo0(0x17, true, true, false);
cp0_write_entrylo1(0x1B, true, true, false);
cp0_write_entryhi(0x1, 0x15);
cp0_tlb_write_random();
```

Please consult the
[MIPS R4000 Microprocessor User's Manual](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/R4000_Users_Manual_2Ed.pdf)
for more details on the software interface to memory management unit
(starting on page 84/114).

Note that you can use the `cpu0 tlbd` command of the MSIM simulator to
display the contents of the TLB when in interactive mode.

### Bitmaps

As a convenience, we provide a simple bitmap implementation in
`adt/bitmap.h`. You may want to use this in your frame allocator code,
but it is not mandated.

### Tests

This assignment again provides new tests to help you drive the
implementation. The tests can be split into three groups: tests for the
`thread_kill()` function, tests for the frame allocator, and tests for
the virtual memory mapping.

The `thread_kill()` tests are extremely simple and should primarily
serve as a reminder, so that you do not forget to implement this
function.

The frame allocator tests are similar to the heap allocator tests. Do
not forget to run those too to ensure that your frame allocator and heap
allocator can happily coexist.

The virtual memory mapping tests check that your threads can access
virtual addresses in the first few kilobytes of virtual memory and that
separate address spaces do not influence each other.

As usual, the tests should help you check that you have not forgotten
any major part of the implementation. It is possible to pass some tests
even with a solution that does not really work -- such solutions will
obviously be penalized later.

While passing the tests is required for this assignment, your
implementation will be reviewed, and must include a working frame
allocator and basic address space management functions to actually pass.

---

## Additional hints/suggestions

 * Allocate thread stacks from frame allocator directly.
   You will need a much smaller heap then.
 * TLB entries are actually pairs of entries.
 * What is PFN and VPN2?
 * How is `thread_kill()` different from `thread_finish()`?
 * Why the `TLBWR` and `TLBWI` instructions do not have any operands?
 * What happens when (timer) interrupt occurs in the middle of TLB
   exception handling?
 * What happens when `handle_tlb_refill()` returns?
 * What happens when entry in TLB is marked as invalid?

---

## Grading

The assignment will be graded, with respect to the state of the
assignment at submission deadline, as follows:

  - A fully working assignment that supports ASID recycling (i.e,
    there can be up to 255 _live_ address spaces at any time) will
    receive a baseline of **10 points**.

  - A working implementation that uses ASIDs but does not support ASID
    recycling (i.e., there can be at most 255 address spaces _ever_
    created) will receive a baseline of **8 points**.

  - A working implementation which does not use ASIDs and needs to
    flush the entire TLB on address space switch will receive a baseline
    of **6 points**.

The baseline will be adjusted based on specific properties of the
implementation as follows:

  - A solution with support for ASID stealing and selective TLB
    shootdown (i.e., there is no fixed limit on the number of _live_
    address spaces at any time, apart from available memory) will
    receive a **bonus of 2 points**.

  - A solution with support for allocating frames on demand in TLB
    Refill handler (killing the thread triggering the exception if out
    of memory) will receive a **bonus of 1 point**.

  - An implementation where the physical memory usage of the heap
    allocator can grow and shrink (frames allocated and returned on
    demand) will receive a **bonus of 1 point**.

  - A solution in which the heap allocator does not request memory
    from the frame allocator will receive a **penalty of 2 points**.

The baseline will be further adjusted based on additional criteria:

  - Bonus for relevant features that were not part of the assignment
    (such as more complex tests).

  - Penalty for bugs that were not discovered by the tests but are
    still important (in extreme cases where the code passes the tests
    more or less by chance the penalty can be severe).

  - Penalty for technically poor solution even if it works somehow.

  - Penalty for poor coding style (such as inconsistent or cryptic
    variable names, lack of comments, poor code structure, and so on).

Any single bonus or penalty will typically be restricted to 1-2 points,
however, exceptions in extreme cases are possible.
