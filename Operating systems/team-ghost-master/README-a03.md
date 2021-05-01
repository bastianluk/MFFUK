# Assignment 03: Kernel Threads and Cooperative Scheduler

The goal of this assignment is to extend your kernel with support for
threads and very basic (cooperative) thread scheduling. Once
implemented, you will be able to create threads for different tasks, but
(for now) the threads will have to voluntarily give up (yield) the
processor to let the other threads run because of the cooperative nature
of thread scheduling.

Preemptive scheduling is the topic of the next assignment, but it
requires handling interrupts from a timer device, which is why it is
important to get the simple things working first. Similarly, before you
start working on this assignment, do make sure that you have a working
`printk()` and a kernel heap manager (at least the bump-pointer variant,
even though you will eventually need a full-fledged implementation).

The title suggests that the assignment has (again) two parts. This
reflects the logical distinction between thread management and thread
scheduling, even though both parts depend on each other and must work
together.

---

## Part 1: Kernel threads

To implement support for kernel threads, you need to introduce an object
that represents a thread, and functions to manipulate/manage thread
objects. To make your design testable using a common suite of tests, we
prescribe an API that you need to implement.

The required functions are listed in the `proc/thread.h` header file,
but you will be probably more interested in the `src/proc/thread.c`
file, which provides an empty skeleton of the required functions along
with a description.

The `proc/thread.h` header file also contains a skeleton of the
`thread_t` type which represents a thread object. At this point, the
thread object contains a name and a pointer to a function of type
`thread_entry_func_t`. The function represents the code to be executed
by a thread (more on this [later](#thread-entry-function)). You will
need to add other fields (as necessary) to the `thread_t` type.

In general, the subsystem should keep track of all threads and manage
the state of individual threads. Taking a closer look at the functions
that you need to implement, some deal with subsystem initialization and
thread creation, some can query information, and some need to cooperate
with the scheduler by calling the scheduler API.

One particular function, `thread_switch_to()`, is of special interest to
the scheduler, because it provides the ability to switch between
threads. To implement such a function, you will need to be able to
switch CPU context, which typically needs to be done in assembly code --
to keep tight control over the code being executed.

To this end, we provide the `cpu_switch_context()` function which does
the actual switching of CPU context -- you just need to use it properly
in your implementation.

The function is declared in the `proc/context.h` header file, along with
the `context_t` type which represents the CPU context (CPU registers
along with some CP0 registers). The function has two parameters,
`this_context` and `next_context`, both pointers to the `context_t`
type. When called, the function saves the current CPU context into
`this_context`, and loads another context (the one to switch to) from
`next_context`.

While not strictly necessary, we suggest that you review the
implementation of the `cpu_switch_context()` function in
`src/proc/context.S`. It is important to understand that because the
function changes the `sp` and `ra` registers, it returns to another
context.


### Thread entry function

We have mentioned earlier that a thread entry function (see the
`entry_func` field in the `thread_t` structure) represents the code to
be executed by a thread. The function expects to receive a single
argument and returns a single value. Both values are of type `void *`
and need to be cast to a proper type by the code using them.

This means that to start a thread, you need to prepare a CPU context in
which, when activated, causes the CPU to jump to that function as if an
actual function call was made. This in turn requires some understanding
of the function calling conventions, i.e., what the compiler-generated
code expects to see and where.

This is defined in the MIPS ABI and is normally handled by the compiler,
but it is our responsibility when we need to jump-start a thread. The
ABI specification is a quite long read, but we only need a few details
related to passing arguments to a function and stack layout
requirements.

The MIPS ABI specifies that the first 4 function arguments are passed in
registers `a0`..`a3` and that additional arguments are passed on the
stack. Luckily, our thread entry function only needs one argument, which
is passed in register `a0`, so there is no need to store arguments on
the stack.

However, regarding the stack layout, the ABI states two important
things. One is that **stack frames must be aligned to 8 bytes** (even
though the hardware only requires 4-byte alignment). The other is that
in non-leaf functions (i.e., functions that call other functions) the
caller stack frame must provide a function call argument area which is
as big as the maximum number of bytes used to call other functions, but
**at least 4 words** (16 bytes), even if the maximum number of arguments
to any called function is fewer than 4 words.

In other words, even though the first 4 arguments are passed in
registers, the caller **must always reserve space for 4 words on the
stack** which the callee can use to save the values of the arguments
passed in registers.

Your code therefore needs ensure that when you call the
compiler-generated code of the thread entry function, the arguments and
the stack layout satisfies these requirements. To this end, the
`proc/context.h` header file provides the `ABI_STACK_FRAME` constant
which you should use to reserve space on stack before calling the thread
entry function. The value was actually set rather defensively and is
higher than strictly required. Note that the actual use of this constant
(as well as proper stack frame alignment) **is your responsibility.**

For more details, we suggest to review the **Function Calling Sequence**
section of the
[System V Application Binary Interface, MIPS RISC Processor Supplement, 3rd Edition](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/ABI-PS-MIPS.pdf).


### Additional hints/suggestions

To better understand the problem, consider the following questions when
implementing the prescribed API functions:

 * For a kernel-mode thread, the CP0 Status register should be
   initialized to `0xff01` (see page 105/135 of the
   [MIPS R4000 Microprocessor User's Manual](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/R4000_Users_Manual_2Ed.pdf)
   for details on the layout of the register).
 * Who calls `thread_finish` if the `thread_entry_func_t` returns?
 * Who will be responsible for thread destruction?

Also, to simplify debugging, consider extending your `printk()`
implementation to support printing information about a given thread by
providing a `T` modifier to the `%p` format specifier (similar to `L`
for lists).

---

## Part 2: Cooperative scheduler

As mentioned above, scheduling is logically distinct from thread
management, even though both subsystems need to cooperate. A thread
subsystem provides the scheduler with the ability to switch between
threads, while the scheduler provides the thread subsystem with the
ability to trigger scheduling actions, e.g., when a thread wants to
yield the CPU to someone else, or when a thread needs to be suspended.
To give you an idea what services are needed from the scheduler, we
prescribe an API that you need to implement.

The required functions are listed in the `proc/scheduler.h` header file.
The description of each function, along with a skeleton that needs to be
filled in, can be found in the `src/proc/scheduler.c` source file.

In general, the scheduler needs to keep track of all threads that can be
scheduled so that when `scheduler_schedule_next()` is called, the
scheduler can switch to a thread that should be running.

As mentioned earlier, a cooperative scheduler requires individual
threads to yield the CPU to other threads. This can be done by calling
the `thread_yield()` function (which operates on the currently executing
thread), which should trigger a scheduling action.

---

## Support code

In addition to the files containing the thread and scheduler API, the
corresponding empty function definitions, and the `cpu_switch_context()`
function mentioned above, there are few other changes in the initial
code provided for this assignment.

First of all, the `kernel_main` function was changed to create a thread
for running the system (or the tests).

The `debug.h` header file provides the `panic()` and `panic_if()` macros
that can be used to halt the simulator if your code encounters an
unrecoverable error. A typical example can be found in the
`kernel_main()` function, where the kernel panics if creating the main
kernel thread fails. Unlike assertions, kernel panics are not removed
from production builds.

The `errno.h` header file provides the `errno_t` type for passing error
information around. Feel free to add your own error codes and do not
forget to extend the `errno_as_str()` function which provides a
human-readable description of the error.

The `ktest.h` header file contains a new macro, `ktest_assert_errno()`
to simplify testing error values for `EOK`.

### Tests

Just like in previous assignments, we provide a suite of tests to help
you stay on the right track. This assignment retains all tests from the
previous assignments and adds 9 new tests for kernel threads in the
`kernel/tests/thread` directory.

The tests primarily target the thread management API and only test the
working of the scheduler indirectly. Because the tests assume
cooperative scheduling, they are rather simple and mostly test basic
things -- they should help you check that you have not forgotten any
major part of the implementation, but it may be possible to pass some of
tests even with a solution that does work correctly.

While passing the tests is required for this assignment, your
implementation will be also checked manually, and must include working
scheduler and thread management functions to actually pass.

---

## Grading

The assignment will be graded, with respect to the state of the
assignment at submission deadline, as follows:

  - A fully working assignment will receive a baseline of **10 points**.

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
