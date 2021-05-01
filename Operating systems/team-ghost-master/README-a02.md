# Assignment 02: Kernel memory management

This is the first team assignment in which you will lay down the foundation of
your operating system kernel. The assignment has two parts. The first is just a
rehash the previous assignments to get your team work started. The second is
something new, and requires you to implement kernel heap memory management.

---

## Part 1: Formatted printing and debug functions

At this point, your team should have access to multiple implementations of the
`printk()` function (one from each team member) and perhaps even multiple
implementations of the functions for dumping function machine code or reading
the value of the stack pointer register. Obviously, there is little need for
multiple implementations of these functions in a single kernel, therefore your
first task as a team is to choose the one implementation that you want to be
using in subsequent development.

All the implementations should be functionally equivalent (if they passed the
tests), but there are other aspects that may make one implementation more
favorable over the others. Therefore you should jointly review and discuss the
available implementations, focusing on code quality, internal design and
extensibility, and make a decision. You may simply pick one that looks best and
integrate it, or you may try to cherry pick and fuse the best parts from
multiple implementations.

In any case, keep in mind that `printk()` is going to be one of your key 
debugging aids and you need to trust it. Make sure to support the features
required in assignment `a01`, and **as a new feature**, add support for
printing unsigned integers via the `%u` specifier (this can be used for
`size_t` and `uintptr_t` types too).

### Formatted printing

Your `printk()` **MUST** support printing the values of the following types:

- characters (via `%c`)
- signed integers in decimal (via `%d`)
- **unsigned integers in decimal (via `%u`)**
- unsigned integers in hexadecimal (via `%x` and `%X`)
- zero-terminated strings (via `%s`)
- pointers (via `%p`)

Optionally (highly recommended), your `printk()` should also support printing
the values of the following types:

- `list_t *` pointers (via `%pL`), including list size and links
- function pointers (via `%pF`), including machine code dump
- **pointers to memory blocks (via `%pA`)**, for part 2 of the assignment

### Debugging functions

Your kernel **MUST** provide the following debugging functions:

- reading the value of the stack pointer (via `debug_get_stack_pointer()`)
- dumping machine code at function address (via `debug_dump_function()`)

The provided code already contains the `assert` macro in `debug.h`
(note that it is only evaluated under `--debug` configuration) and also
`machine_enter_debugger()` to programmatically switch MSIM into interactive
mode from your C code
(recall the [kernel exercise](https://d3s.mff.cuni.cz/teaching/nswi004/exercises-kernel-intro/#entering-the-debugger)).

---

## Part 2: Kernel heap allocator

So far, your kernel code could not use dynamic memory allocation. This was not
really a problem for assignment `a01`, because you could tell how much memory
you will need at compilation time and this amount did not change at runtine.

However, without the ability to allocate memory dynamically, your kernel would
be limited to fixed amount of all kernel objects, forcing you to come up with
specific numbers for a specific machine configuration (in terms of available
memory). While such a restriction would be acceptable in certain domains, e.g.,
real-time kernels in safety- and mission-critical systems, it is not acceptable
in a kernel (albeit simple) for a general-purpose system.

Consequently, the goal of the second part of this assignment is to implement
support for **dynamic memory allocation** in your kernel.

## Basic functions

Your kernel heap implementation needs to provide three basic functions (see
the `mm/heap.h` header file for function signatures):

- `heap_init()`, which initializes the heap (and needs to be called from
   prior to making any calls to the following two functions).

- `kmalloc()`, which allocates a block of memory of given size.

- `kfree()`, which frees a (previously allocated) block of memory.


The entry point to your kernel is the `kernel_main()` function. The key
responsibility that function is to initialize various kernel subsystems
and start the thread scheduling loop.

In user-space, the runtime library executes various initializers before calling
your `main()` function. In the kernel, not only is the runtime library
unavailable, but you also need to keep the order of subsystem initialization
under control, because some subsystems depend on other subsystems. This is best
done by being explicit about the order of subsystem initialization, and the
`kernel_main()` function is a perfect place for such kind of work.

Currently, there is not much going on in the `kernel_main()` function, but this
will soon change, because this is where you need to put the call `heap_init()`
before you can start using the kernel heap.

Please keep the key responsibility of `kernel_main()` in mind and avoid using
the function for other than coordination purposes (especially avoid putting 
low-level code dealing with implementation details into that function).


## Bump-pointer heap

Managing heap can be relatively tricky, because you need to keep track of a
variable number of objects (allocated and free blocks) without using standard
dynamic memory allocation functions. To get a minimal working solution (so that
you can better understand the problem) it helps to think of a simple solution
first.

Imagine a user-space program that only calls `malloc()` (and never `free()`).
Such a program either will either complete its task with whatever memory was
available, or it will run out of memory. Implementing a heap allocator for such
a program would be very simple -- the allocator could simply keep a pointer to
the last free memory location and advance the pointer forward on each call to
`malloc()` and do nothing on a call to `free()`. This is sometimes called
*Bump Pointer Allocator*, because all it does is bumping a pointer forward.

To pass the assignment, you can implement the `kmalloc()` function using this
type of allocator, but keep in mind that you **WILL** need a working `kfree()`
later on, so expect to return to the allocator if you choose to go this way.

In fact, given its simplicity, we would generally recommend to implement the
bump-pointer allocator first so that you become more accustomed to working in
the kernel environment. Obviously, certain tests will fail without a working
`kfree()`, but you will get *something working* to boost your confidence and
relieve pressure. With a working bump-pointer allocator safely committed to
your repository, you can move on to implementation of a proper heap allocator.


## Memory detection

The amount of memory the simulated computer has is determined by the directives
in the `msim.conf` configuration file. However, this file **IS NOT** visible to
your kernel and there is no firmware (BIOS) that would provide the information
about available memory to the kernel.

Therefore the kernel needs to find this information on its own, and apart from
compile-time constants, the simplest way is determine the amount of available
memory is to probe the memory by writing to it and checking if the previously
written value can be read back.

Your implementation **MUST NOT** rely on the fact that MSIM returns a special
value when reading non-existent memory, nor can it rely on any compile-time
constants (such as those used by the tests) to find out how much memory your
system has.

Your implementation can assume that all memory is available in a single
contiguous block, starting with the kernel image. The `_kernel_end` symbol,
defined by the linker, represents the address of the first byte after the
kernel image. Note that it is safe to probe the memory in blocks of 4 KiB to
speed up the detection.


## Debugging extension for `printk()`

As a debugging aid, especially when working on a full-featured heap allocator,
we recommend extending `printk()` to support printing information about an
allocated memory block via the `%pA` specifier.

We do not prescribe any specific format, but depending on the nature of your
implementation, you should be able to print the size of the allocated block
and information about the state of the sibling blocks.

---

## Support code

At this point, you should be familiar with the general layout of the skeleton
code we provide together with the assignment. Compared to assignment `a01`,
this assignment also provides `src/mm/heap.c` which contains empty definitions
of the "public" functions that need to be implemented.

### Tests

Just like in previous assignments, the basic correctness of your implementation
will be primarily evaluated using automated tests. This assignment retains all
tests from the previous assignments and adds 4 new tests for the kernel heap
allocator in `kernel/tests/heap` directory.

The heap allocator tests are intentionally simple to accept the bump-pointer
heap allocator implementation, which is why you should consider adding your
own tests. As mentioned earlier -- implementing heap allocator can be a bit
tricky and error-prone, but it is a fundamental subsystem of the kernel and
you need to be able to trust it. If you cannot manage kernel heap properly,
you will run into problems implementing (and debugging) other parts of your
kernel.

---

## Grading

The first part of the assignment will not be subject to grading, because it was
already graded in the individual assignments. It is up to you and the members of
your team to ensure that you have a working `printk()` that you can trust,
along with the two debugging functions.

The second part of the assignment will be graded, with respect to the state of
the assignment at submission deadline, as follows:

- A fully working and documented solution, that is, with both `kmalloc()` and
 `kfree()` doing what they should, will receive a baseline of **10 points**.

- A working solution with support for bump-pointer heap allocation only, that
  is, with a working `kmalloc()` but empty `kfree()`, will receive a baseline
  of **6 points**.

The baseline will be further adjusted based on additional criteria:

- Bonus for relevant features that were not part of the assignment (such as
  more complex tests, support for debugging allocation errors, and so on).

- Penalty for missing documentation. Your `heap.c` must contain a high-level
  overview of your implementation (at least one paragraph of a reasonable
  description).

- Penalty for bugs that were not discovered by the tests but are still
  important (in extreme cases where the code passes the tests more or less by
  chance the penalty can be severe).

- Penalty for technically poor solution even if it works somehow.

- Penalty for poor coding style (such as inconsistent or cryptic variable
  names, lack of comments, poor code structure, and so on).

Any single bonus or penalty will typically be restricted to 1-2 points,
however, exceptions in extreme cases are possible.
