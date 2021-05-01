# System Calls and Interrupts

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday December 17 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


## System Calls

The `strace` utility (installed for example on the `lab` computer) can list the system calls performed by a process.
This can come in handy for the next three questions, which should all be answered for the `lab` computer
(or use your own Linux).

**Q1** Does the `printf` library function invoke any system call(s), and if so, which one(s) ?

**A1** (for all a1-3) I wote a small program that used the function in question and compared the difference of strace output with the function actually called with nonempty parameters vs with the function commented out using `strace -c -S name ./a.out` - `printf` uses the following syscalls: `brk`, `write`, `fstat`

**Q2** Does the `pthread_mutex_lock` library function invoke any system call(s), and if so, which one(s) ?

**A2** using the same procedure as above, didnt see any difference in the output so no syscalls but I suspect that is because there were no threads battling for the lock and once some waiting of threads needs to be handled, syscalls would be made, though I didnt find any in my way of testing.

**Q3** Does the `clock_gettime` library function invoke any system call(s), and if so, which one(s) ?

**A3** using the same procedure as above, didnt see any difference in the output - for some reason in the man page it talks about `These system calls first appeared in Linux 2.6.` it being a system call it self, but it doesnt appear in the output of strace.


## Interrupts

**Q4** What address does the MIPS processor jump to when a (hardware) interrupt request is received ?

**A4**  0x80000180

**Q5** Linux systems show the counts of various interrupts in the `/proc/interrupts` file.
Write a small program that forces the system to generate the TLB Shootdown interrupts
and include its source here. As a hint, first think when TLB Shootdown interrupts
are necessary (the interrupts are used by the operating system when one core
needs to tell another core to clear the TLB content).

Remember, please do not include blank lines in your response body.
Your answer should work on the `lab` computer.

**A5** ...
