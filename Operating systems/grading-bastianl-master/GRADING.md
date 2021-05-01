# Grading for Lukáš Bastián

|                                                    | Grade | Points | Percent |
| -------------------------------------------------- | ----- | ------:| -------:|
| **Current grade**                                  | **2** |        |   73.5% |
| Knowledge Points                                   |    OK |     35 |   53.8% |
| Implementation Points                              |    OK |     40 |   66.7% |
| Activity Points                                    |    OK |     34 |  100.0% |


If you see an issue with the grading, please mail us as soon as possible
(via `teachers-nswi004@d3s.mff.cuni.cz`).


## Overview of Individual Assignments

| Topic                                              | Points |
| -------------------------------------------------- | ------:|
| **Knowledge Points**                               | **35** |
| 01 Architecture                                    |      5 |
| 02 Code Stack                                      |      4 |
| 03 Heap                                            |      3 |
| 04 Garbage                                         |      1 |
| 05 Scheduling                                      |      4 |
| 06 Synchronization                                 |      5 |
| 07 Synchronization Tools                           |      2 |
| 08 Memory Models                                   |      3 |
| 09 Paging                                          |      3 |
| 10 Paging Algorithms and Applications              |      0 |
| 11 System Calls and Interrupts                     |      0 |
| 12 Devices                                         |      5 |
| 13 File Systems                                    |      0 |
| **Implementation Points**                          | **40** |
| 00 Printf and lists (`b7de644da65787ff`)           |      - |
| 01 Introduction to kernel (`4be1137fab216d68`)     |     11 |
| 02 Kernel heap (`31b94c6d1a36e216`)                |      9 |
| 03 Threads and scheduler (`ae3f1452de8c7ef1`)      |     10 |
| 04 Synchronization (`bd45639381b65353`)            |     10 |
| 05 Virtual memory (`88fdffbd73baa5b1`)             |      0 |
| 06 Disk driver (`6d164549f7944327`)                |      0 |
| **Activity Points**                                | **34** |
| Commits                                            |     21 |
| Lines of code added                                |     13 |




## Comments for Individual Assignments

Comments are optional and usually not provided for correct answers.



### 01 Architecture (Knowledge Points)

Correctly answered: A1, A2, A3, A4, A5.

### 02 Code Stack (Knowledge Points)

Correctly answered: A1, A2, A3, A5.

Comment for A4:
Line 3 is ambiguous, but 7 is clearly relative.

### 03 Heap (Knowledge Points)

Correctly answered: A1, A4, A5.

Comment for A2:
Once the address of a variable is known, the access is the same for both the
heap and the stack. Also, addresses of the stack are not necessarily closer to
the code than addresses of the heap, and except for some very rare cases, it
would not impact performance even if they were.

Comment for A3:
Code is not touched (it would most likely turn out to be read only anyway). It
is the return addresses that are manipulated.

### 04 Garbage (Knowledge Points)

Correctly answered: A1.

Comment for A2:
Just one header will be needed for the new data.

Comment for A3:
When ROOT is freed, a reference to RIGHT is released, so count for RIGHT at TWO
is also 0.

Comment for A4:
You are not answering the question (which was to list garbage collection
roots).

Comment for A5:
On compacted heap the allocation is almost as cheap as on stack.

### 05 Scheduling (Knowledge Points)

Correctly answered: A2, A3, A4, A5.

Comment for A1:
Do not use a formula, simply try to imagine how the threads would run.

Comment for A4:
Nice discussion.

### 06 Synchronization (Knowledge Points)

Correctly answered: A0.

### 07 Synchronization Tools (Knowledge Points)

Correctly answered: A2, A3.

Comment for A1:
The loop spins until `lock_state` is observed to be non zero, and at the same
time sets `lock_state` to `lock_state & ~1`, which must be zero. The only value
that is non zero but turns to zero when the lowest bit is masked out is 1, so
that is representing the unlocked lock.

Comment for A5:
The 'K' answer is correct, but the program can also display no output because
`thread_two_function` can wake too late, when `x` is no longer 10.

### 08 Memory Models (Knowledge Points)

Correctly answered: A2, A3, A5.

Comment for A1:
The initialization does not play a role. The compiler reduced the repeated
testing of `go` to a single test because the variable is not marked as shared
between threads and therefore it assumes that after the variable is read once
(by the `if` clause), it cannot change anymore.

Comment for A4:
Actually, `b` is not in a data race even if we do not know what value it will
have at the end.

Comment for A5:
Accurate enough. If we assume the searches hit, then linear access would stop
on average halfway.

### 09 Paging (Knowledge Points)

Correctly answered: A3, A4, A5.

Comment for A1:
The question was about maximum page size possible with these addresses. To
answer, one must see how many consecutive bits in the two addresses, starting
from the lowest one, are equal.

Comment for A2:
With an address split of 10 bits first level table index, 10 bits second level
table index, and 12 bits offset, a ingle second level page table maps 2^10=1024
pages of 2^12=4096 bytes, the correct answer therefore is 4 megabytes, or
400000 hexadecimal.

Comment for A3:
Borderline accepted. You got all the levels right but messed up the fifth byte
on page, offset 4.

Comment for A4:
The processes do not "have" offsets, they are simply sharing pages. This
happens for example when loading libraries.

### 11 System Calls and Interrupts (Knowledge Points)

The previous assignment 11-syscalls-interrupts-before was either not submitted or lacked the mandatory part.

Correctly answered: A1, A2, A3, A4.

Comment for A1:
The main syscall is `write`, `brk` and `fstat` are related to buffering inside
the standard library.

Comment for A2:
Correct conclusion for blocking, the syscall you would see then is `futex`.

### 12 Devices (Knowledge Points)

Correctly answered: A1, A2, A3, A4, A5.

Comment for A5:
Your strife for an absolutely accurate answer would make your math teachers
proud :-)

### 00 Printf and lists (Implementation Points)

Function name `write` may collide with standard (POSIX) function.

Do not use magic numbers, `char` can be easily converted to `int` to get its
ASCII value.

It is generally better to stick to the same naming scheme, i.e. `snake_case` if
it is used in the rest of the code. In other words: make it easy for the
reader.

Type `char` can be safely compared to literal `0` instead of `'\0'`.

I like that you copy the string to your own buffer in `freq.c`.

Inconsistent formatting (block indenting and `{}` placement).

### 01 Introduction to kernel (Implementation Points)

+10 Assignment 01 passes all tests.

Reading register could use its alias, i.e. read from register `sp`.

+1 Support for `%pF` and/or `%u`.

Duplicate code when printing integer and hexadecimal number.

Do not use magic numbers, `char` can be easily converted to `int` to get its
ASCII value.

Nice and simple extension with `%x8`.

Register type can be directly set to `uintptr_t` (the sizes are set to be
correct for this).

### 02 Kernel heap (Implementation Points)

+10, passes basic and extended tests

-1, allocator overview is missing

+1, functionality (coalescing)

-1, minor code issues

#### Printing/debugging

(-) code/complexity: too much indirection for `char` -> `enum`

#### Heap allocator

(!) Allocator overview is missing.

(+) optional: coalescing support

(-) Code does not validate blocks/pointers in `kfree()`

(+) code/documentation: commented functions in `heap.c`

(-) code/abstraction: constant literals used in expressions/statements

(-) code/decomposition: alignment, free block search in `kmalloc()`

(-) code/decomposition: pointer conversion, block state change, coalescing in
`kfree()`

(-) code/clarity: lower-level function `memory_scanner()` changes module-level
variables, producing side-effects that are not visible at call site

### 03 Threads and scheduler (Implementation Points)

10 All tests passing.

Do not forget to merge the fuzzy testing for A02.

Extremely complicated code of the scheduler. The logic that could be
(unconditonally) in threads module is scattered across the whole scheduler.

Operation that cannot announce its failure shall not fail if possible.
Operating system shall be able to suspend/yield/etc. a thread even in low-
memory conditions. Adding `link_t sched_link` to `thread_t` would prevent this
and it is a clean solution (we know beforehand that threads will be scheduled
as otherwise they are of little use, hence it is okay to count with it directly
inside `thread_t`).

Please, re-think how the responsibilities of thread/scheduler are divided.
Scheduler has only a single job - keep list of runnable (ready) threads. List
of not running (suspended) threads is of no use to it as they are not runnable.

Note that with the right division of labour, `thread_suspend` is simply
`remove_thread` and `schedule_next` without any extra code.

Threads are never freed.

`charptr_to_chararr` is actually `strcpy`?

Overall the solution is rather unreadable as it is burried inside several
layers of indirection that add no abstraction to simplify the understanding.
Consider seriously refactoring the solution when time allows.

### 04 Synchronization (Implementation Points)

+10 All tests passing.

Heap shall use higher-level primitive such as mutex.

`kfree` can enter critical section after the initial checks and chunk
dereferencing.

Thread quantum shall be set by scheduler, not by exception handler.

Exception handler shall start by checking exception code.

Can `scheduler_remove_thread` reach end? Assertion or restoring interrupts is
missing.

The scheduler code is extremely complicated given the fact that it is a bare
round-robin scheduler without priorities. I suspect that adding the critical
section locking must have been a nightmare.

Your mutex is half-recursive as it is possible to call `lock` multiple times
and unlock it only once. Some old POSIX locks had this semantics and no one
ever used them ;-).

`mutex->thread` is `mutex->owner`.

It would be worth the effort to generalize the `block_current_thread` and use
it at multiple places.

Mutex and semaphore without any visible race conditions.

Overall quite nice and simple implementation. But do not be afraid to refactor
your code. Do not burden yourself with bad design for the rest of the semester.

### 05 Virtual memory (Implementation Points)

No commit tagged `a05`, evaluated commit `88fdffbd` from Dec 20, 2020.

The solution is incomplete, yielding a baseline of 0 points.

* (+) frame allocator (shown in online labs, probably working)

* (-) no frame allocation support in heap allocator

* (-) no address space management

* (-) no TLB management and exception handling

* (-) no thread AS support

* (+) has `thread_kill` (probably working)

### 06 Disk driver (Implementation Points)

0 Not submitted.

### Commits (Activity Points)

Found 97 commits on 21 different days.

The following identities were associated with your account:
`lukasbastian@localhost.localdomain`, `luk3bastian@gmail.com`,
`721-bastianl@users.noreply.localhost`.

The statistics were collected at 2021-01-20.

### Lines of code added (Activity Points)

Added 1333 lines total (excluding merge commits).

The statistics were collected at 2021-01-20.

