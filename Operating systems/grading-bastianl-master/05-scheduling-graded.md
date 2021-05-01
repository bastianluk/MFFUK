# Process Scheduling

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday November 5 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


## Parallelism

**Q1** Assume a program that processes a video stream.
In a single thread, it processes one frame per second.
If parallelized, each thread can ideally process one frame per second,
but then spends extra 1/10 of a second communicating the results,
holding an exclusive lock on communication for that time
(no other thread can communicate at the same time).

What will be the average (long term) processing speed, in frames per second, with 10 threads, and with 20 threads ?
Assume we have at least as many processors as we launch threads.

**A1** `((thCount * 1) / ((thCount * 1/10) + 1)) = newFps` >>> `10 / 2 = 5` fps (for 10 threads) and `20 / 3 = 6.66666...` fps (for 20 threads) NOTE - diminishing returns on more threads/processors (thCount doubled, FPS didnt)


## Processes And Threads

**Q2** Imagine two very similar programs. Each opens a file and prints the handle of that open file
(error checking omitted for brevity):

```c
// Program ONE

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main (void) {
    int file = open ("ONE", 0);
    printf ("%d\n", file);
    close (file);
    return (0);
}
```

```c
// Program TWO

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main (void) {
    int file = open ("TWO", 0);
    printf ("%d\n", file);
    close (file);
    return (0);
}
```

Assume further that the two files opened by the two programs exist.
When you run the two programs, you see that they print the same number, although each opens a different file.
Why is that, and would that change if those were two threads in the same program rather than two programs ?

**A2** The file descriptor table from which the ret value of `open` comes from is per process - that means that the number in both the programs (separate processes) can be the same as it starts populating the table similarly. This also means that more threads in a single program/process would change this as the files now would be stored in the same table and would have to be assigned different numbers.


**Q3** In the environment of your kernel programming assignments,
we took a simple program that launches two threads and
dumps the processor registers as the first thing
each thread does. This is the output:

```
== KERNEL TEST thread/fairness ==
<msim> Alert: XRD: Register dump
processor 0
   0                0   at                0   v0 ffffffff80001960   v1                0   a0                0
  a1                0   a2                0   a3                0   t0                0   t1                0
  t2                0   t3                0   t4                0   t5                0   t6                0
  t7                0   s0                0   s1                0   s2                0   s3                0
  s4                0   s5                0   s6                0   s7                0   t8                0
  t9                0   k0 ffffffff80003acc   k1             ff01   gp ffffffff80000000   sp ffffffff80004b34
  fp                0   ra ffffffff800017d8   pc ffffffff80001970   lo                0   hi                0
<msim> Alert: XRD: Register dump
processor 0
   0                0   at                0   v0 ffffffff80001900   v1                0   a0                0
  a1                0   a2                0   a3                0   t0                0   t1                0
  t2                0   t3                0   t4                0   t5                0   t6                0
  t7                0   s0                0   s1                0   s2                0   s3                0
  s4                0   s5                0   s6                0   s7                0   t8                0
  t9                0   k0 ffffffff80004bcc   k1             ff01   gp ffffffff80000000   sp ffffffff80005c34
  fp                0   ra ffffffff800017d8   pc ffffffff80001910   lo                0   hi                0


Test finished.
```

If you know that all our threads reserve a memory block of the same size for their stacks,
what would be the upper bound on that size (judging from the dump) ?

**A3** judging by the SP values (in hex 5c34 - 4b34 = 1100) it is 4352 B


## Context Switching

**Q4** Processors typically have separate registers for integer processing and for floating point computations.
For example, the MIPS processor has 32 general purpose registers (`r0` to `r31`) for integer processing
and 32 floating point general purpose registers (`fgr0` to `fgr31`) for floating point computations.
If you had a program you know does not use floating point computations at all, could you save
on the context switch time by not saving and loading the floating point register content
for that single program, or is it always necessary to save and load all registers ?

**A4** Generally, I think it is necessary to save and load only the registers that we need to retain the values in between switches - so if we know for certain that we dont need the fgr0-31 registers we dont have to save-load their values. To expand on the idea - as the information and processing of what to save/load and not save/load could slow down the switch which needs to be as fast as possible as it happens often, it should be whole groups (like here, all floating point registers being ommited) saved/loaded or not rather than individual registers having the freedom to be separately saved/loaded.


**Q5** Imagine you are writing the kernel for your assignments and a bug crept into the context switch code.
As a result, the content of one of the general purpose registers is replaced with zeros on every context switch.
Assume the context switch is cooperative, that is, your threads call the `cpu_switch_context` function from the past
quiz every time the context should be switched. In what conditions would the context switch bug have no effect
on the threads (that is, everything would run as if the context switch code were fine) ?

**A5** If the register that is being set to zeros was never used or if it was always first written into and then read from which would hide the initial incorrect value (the likelines is low for this to happen but it could). And of course if the zero value is what was stored there previously :)
