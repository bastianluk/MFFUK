# Assignment 04: Preemptive Scheduler and Synchronization

The goal of this assignment is to extend your kernel with support for
preemptive thread scheduling and basic synchronization primitives. Once
implemented, threads will not need to explicitly *yield* the processor
to other threads to achieve multitasking -- the kernel will switch
between threads periodically in response to interrupts from a timer
device.

Whenever a thread accesses shared data, it needs to ensure that its
action does not conflict with the actions of other threads. When threads
are scheduled cooperatively, the running thread controls when other
threads can run, so it can simply avoid yielding the processor to other
threads while in the middle of a sensitive operation (critical section).

With preemptive scheduling, the running thread is no longer in control,
because it can be interrupted at **ANY** time, even in the middle of a
critical section. We therefore need to provide threads with other ways
to synchronize access to shared data, hence the need for synchronization
primitives.

Even though the assignment is split into two parts (to reflect the
logical distinction between scheduling and synchronization), preemptive
scheduling and synchronization really go hand-in-hand.

---

## Part 1: Preemptive Scheduler

To implement preemptive scheduling, your kernel needs to be able switch
between threads without waiting for a thread to yield the processor.
This is typically implemented with the help of a timer device, which
will periodically issue an interrupt request to the processor.

When interrupt processing is enabled on the CPU, a timer interrupt
causes the CPU to stop executing the code of the currently running
thread and jump into an interrupt handler code of the kernel. In the
timer interrupt handler code, the kernel can invoke the scheduler to
switch context to another thread.

The MIPS processor provides a simple timer that your scheduler code will
need to configure to control the interval of timer interrupts. We
provide a simple function `timer_interrupt_after()` in `drivers/timer.h`
for this purpose. See the section on [timer control](#timer-control)
below for more details.

Your kernel and scheduler will need to be changed to use the timer to
implement preemptive scheduling. Because we do not require your kernel
to provide generic timer support, it is possible to program the timer
device directly from the scheduler -- no other part of your kernel will
use it.

You will also need to trigger execution of scheduler code whenever the
timer interrupts the currently running thread. To this end, you will
need to modify the general exception handler to recognize and properly
handle the timer interrupt, and to trigger scheduler code. See the
section on [exception handling](#exception-handling) for more details.

### Configurable quantum

Preemptive scheduler operates with the concept of a *quantum*, which
determines the maximum time a thread is allowed to run before the
scheduler unconditionally switches to another thread. A shorter
*quantum* causes threads to be switched faster (making the system more
responsive) at the cost of higher overhead due to context switching.

More importantly though, the value of *quantum* influences the
interleaving of threads, which makes it useful for testing, because
different values can lead to different thread schedules and help expose
race conditions.

For this reason, your scheduler should use the
`KERNEL_SCHEDULER_QUANTUM` macro, which was added to `scheduler.c`, as
the value of a thread quantum. This will make it possible to test your
your code with different quanta, which is exactly what the tests
provided with this assignment do.

---

## Part 2: Synchronization primitives

There are many synchronization primitives, but you are only required to
implement two. The first is *mutex*, declared in the `proc/mutex.h`
header file, which allows implementing *mutual exclusion*, the simplest
of synchronization tasks.

The second is a *semaphore*, declared in the `proc/sem.h` header file,
which allows multiple threads to enter a critical section at the same
time. Semaphores are a bit more complex, and are used for
synchronization tasks involving a fixed number of resources, e.g.,
producer/consumer with a bounded buffer.

Just like in previous assignments, the header files prescribe an API
that you are required to implement. Both mutexes and semaphores should
use passive waiting. That, however, requires managing sleeping threads.
You may want to start with simpler, active-waiting variants, which do
not require thread management.

### Low-level mutual exclusion

To implement mutexes and semaphores with passive waiting, you will need
to synchronize access to the shared data structures of the
synchronization primitives. To handle this chicken-and-egg problem, you
will need to rely on more low-level primitives.

A busy-waiting loop using a *test-and-set* operation to implement mutual
exclusion would be one such a primitive (typically referred to as
*spin-lock*). However, because your kernel is only expected to run on a
single processor, you can use an even simpler approach, which relies on
temporarily disabling interrupts.

Disabling interrupts means that processor does not react to interrupt
requests, which allows you to execute a critical section of code without
being interrupted and rescheduled (recall that the scheduler relies on
timer interrupts to switch between threads unconditionally). When you
finish executing critical section code, interrupts need to be enabled
again to resume normal preemptive scheduling.

For this purpose, we provide the `interrupts_disable()` and
`interrupts_restore()` functions in the `exc.h` header file. The `bool
interrupts_disable()` function returns the CPU interrupt-processing
state prior to disabling interrupts, while the `void
interrupts_restore(bool)` function restores the interrupt-processing
state to the given value. This allows using the functions in nested
calls. See the section on [interrupt control](#interrupt-control) below
for more details.

### Thread safety

When you switch to preemptive scheduling in your kernel, you will also
need to make your kernel subsystems thread-safe. Any function that
accesses shared data and can be called concurrently from multiple
threads (e.g., `kmalloc()`, `kfree()`, as well as functions in the
thread subsystem and the scheduler) will need at least a rudimentary
form of locking, which can be (at least initially) implemented by
disabling interrupts.

To illustrate the required changes, consider the following code which
always updates one part of a shared data structure, but updates the
other only when `update_post` is `true`:

```c
void do_something_with_shared_data_cooperative(bool update_post) {
    shared_data.pre = shared_data.pre + 1;
    if (!update_post) {
        return;
    }
    shared_data.post = shared_data.post + 1;
}
```

We can protect the shared data structure by disabling interrupts as
follows:

```c
void do_something_with_shared_data_cooperative(bool update_post) {
    bool saved = interrupts_disable();

    shared_data.pre = shared_data.pre + 1;
    if (!update_post) {
        interrupts_restore(saved);
        return;
    }
    shared_data.post = shared_data.post + 1;

    interrupts_restore(saved);
}
```

Note the call to `interrupts_restore()` before the early return.

---

## Support code

Just like the previous assignments, this assignment updates the kernel
base code to provide the necessary context and some convenience
functions. We recommend to review the update to get an idea what has
changed and where.

### Exception handling

The most important change is in exception handling. Instead of dumping
CPU registers and switching the simulator to interactive mode, the code
at `exception_general` in `head.S` now jumps to an actual handler
defined defined in `src/exc/handlers.S`
(`handle_exception_general_asm`), which just saves exception context on
the stack of the interrupted thread and calls into a C function,
`handle_exception_general()` defined in `src/exc/exc.c`, which is
responsible for handling general exceptions, which also includes
interrupts.

The `handle_exception_general()` function receives a single argument,
`eh_context_t *`, which is a pointer to exception context containing the
values of CPU registers at the time of exception (interrupt) along with
several important CP0 registers that need to be preserved.

Specifically, you should read the value of the CP0 *Cause* register from
the exception context. Functions provided in `drivers/cp0.h` should help
you determine the cause of the exception and handle it appropriately.

To implement preemptive scheduling you should only need to handle
exceptions caused by interrupts. For any other causes you should dump
the CPU and CP0 registers and switch the simulator to interactive mode
to simplify debugging.

*Note: First-level TLB exceptions, which are currently triggered, e.g.,
by dereferencing a `NULL` pointer, still switch the simulator to
interactive mode, but nested TLB exceptions are handled by the general
exception handler, which is why you should report any unexpected general
exceptions.*

### Timer control

The MIPS processor provides a simple timer which can be configured
through the *Count* and *Compare* registers of *System Coprocessor 0*.

The *Count* register acts as a timer and is being incremented by the CPU
at a constant rate. The value of the *Compare* does not change on its
own, but whenever the *Count* register equals the *Compare* register,
the processor issues an interrupt request by setting the *Interrupt
Pending 7* (IP7) bit of the CP0 *Cause* register. This causes an
interrupt as soon as the interrupt is enabled, which causes the
processor to jump into the general exception handler.

Both registers can be read and written by kernel code, even though the
*Compare* register is intended to be write-only. Writing a value to the
*Compare* register clears the timer interrupt. For more details, see
page 103/133 of the
[MIPS R4000 Microprocessor User's Manual](https://d3s.mff.cuni.cz/files/teaching/nswi004/download/R4000_Users_Manual_2Ed.pdf).

The `drivers/timer.h` header file provides very basic support for the
MIPS timer. Because your scheduler should only need to set the interval
until the next timer interrupt, the provided `timer_interrupt_after()`
function does exactly that.

Specifically, the function sets the value of the CP0 *Compare* register
to the current value of the CP0 *Count* register to which it adds a
given number of CPU cycles. The number of cycles determines the interval
until next timer interrupt.

### Interrupt control

Interrupts on MIPS are controlled by bit 0 and bits 15-8 of the CP0
*Status* register. Bit 0 (*Interrupt Enable*) controls CPU
interrupt-processing state for all interrupts. Bits 15-8 (*Interrupt
Mask 7*, ..., *Interrupt Mask 0*) allow masking (disabling) individual
interrupts.

When the *Interrupt Enable* bit is cleared, the CPU ignores all
interrupts. When the bit is set, the CPU reacts to interrupts that are
enabled by the individual *Interrupt Mask* bits.

The `exc.h` header file provides two simple functions for coarse-grained
interrupt control, i.e., these functions only manipulate the state of
the *Interrupt Enable* bit of the CP0 *Status* register.

The `bool interrupts_disable()` function saves the value of the
*Interrupt Enable* bit prior to clearing it and returns the saved value.
The `interrupts_restore(bool)` function restores the value of the
*Interrupt Enable* bit to the given value.

### Debugging

We have extended the tester script to also check that code that should
panic the kernel actually panics. This is needed for example in the
`mutex_destroy` test, which checks that the kernel panics when trying to
destroy a locked mutex.

As usual, some of the tests use extended `%p` specifiers to print
details about certain kernel objects. We use `%pS` for semaphores and
`%pM` for mutexes (which is why we used `%pA` for allocated memory block
in the previous assignment). These `printk()` extensions are optional
and do not affect the test outcome.

### Tests

The tests provided in this assignment can be split into two groups:
tests for the synchronization primitives and extensions to the existing
tests to check that preemptive scheduling does not break your
implementation.

The synchronization tests check different aspects of the primitives and
should be easy to understand. The extended tests (for heap and threads)
check that the scheduler is really preemptive and that the internal data
structures are not corrupted by concurrent execution.

Because preemptive scheduling provides a lot of room for race conditions
which may only manifest under a specific thread schedule, we have added
a `KERNEL_SCHEDULER_QUANTUM` macro to `scheduler.c` which determines the
duration of the thread time quantum. Kernel tests can use an addition
parameter (`kqNUM`, see `suite_a04_fuzzy.txt`) to change this quantum
from `tools/tester.py`. This is extremely useful for flushing out race
conditions that are bound to appear.

Note that the provided fuzzy suite is meant to be run for every commit
and is relatively short. Consider creating a custom test suite with more
fine-grained steps (and for multiple tests) to properly stress-test your
solution. However, because such a test suite can run several hours,
please **DO NOT** integrate it into your CI configuration and only run
it locally.

As usual, the tests should help you check that you have not forgotten
any major part of the implementation. It is possible to pass some tests
even with a solution that does not really work.

While passing the tests is required for this assignment, your
implementation will also be checked manually, and must include working
scheduler and thread management functions to actually pass.

---

## Additional hints/suggestions

  * Interrupt from timer is number 7.
  * What happens when `handle_exception_general` returns?
  * Why do we need `interrupts_disable()` and `interrupts_restore()`
    instead of `interrupts_enable()`?

---

## Grading

The assignment will be graded, with respect to the state of the
assignment at submission deadline, as follows:

  - A fully working assignment will receive a baseline of **10 points**.

  - Working implementation with active waiting will receive a baseline
    of **6 points**.

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
