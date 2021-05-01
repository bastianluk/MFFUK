# Process Scheduling

Please do not forget to commit your responses in time,
the deadline for this quiz is Thursday October 29 at 12:00 CET (noon).

Following the process memory layout content, this self study module will look
at the concept of threads and context switching as part of process scheduling.

(We will return to memory management once more with virtual memory and paging.)

The goal of this module is to explain how context switching works, and to
connect the general description to the context switching implementation
in the kernel you use in your lab assignments.


## Why ?

Essentially, our ultimate goal is running multiple programs concurrently.
This is done both for application specific purposes (for example, we want
a desktop computer to be able to both play music on the background and
run a text editing software on the foreground) and for efficiency
(for example, we want a server computer to continuously utilize
all available processors to provide services).

### With Multiple Processors

Certain types of computer hardware lend themselves to straightforward concurrent program execution.
These are especially the shared memory multiprocessor systems, where multiple processors are
connected to the same physical memory in a way that makes the individual processors
reasonably equivalent from the program execution perspective.

(We are generalizing here, to avoid touching on the nitty gritty details of specific hardware architectures.)

With this hardware, the operating system can simply load multiple programs into memory,
use virtual memory management to provide each program with separate address space,
and run each program on one of the available processors. The programs then
execute truly concurrently.

But what if we have more programs to run than processors to run them on ?

### With Context Switching

Context switching makes it possible to run multiple programs concurrently even
when we do not have multiple processors, or when we do not have as many processors
as we have programs that can run. The idea is quite simple - assuming we can save and load
the state of a running program, we can let the programs take turns on the processor, with each
program loading its last state, running for a while, then saving its state and letting another program run.

The act of saving the state of one program and loading the state of another is called _context switching_.

Context switching was described in _Arpaci-Dusseau Section 6 Limited Direct Execution_,
which was part of the self study for the very first lecture. You may want to refresh
your memory by looking at the subsection dedicated to switching between processes.


## How ?

To understand context switching in more detail, we will first look at what exactly
is the process context, or, the state of the program that is being switched.

### Process Context Elements

A running program essentially consists of the code and the state that the code manipulates.
Part of that state is in memory (all the variables on the stack and the heap and so on),
part is in the processor registers, and part can be in the operating system state or
in the devices (but we will assume that the operating system handles the devices
and therefore we do not care about the device state here).

In principle, switching from one running program to another would require switching
all three parts of the state (state in the program memory, state in the processor
registers, state in the operating system memory). Each of the three is handled
differently:

- State in the program memory can be quite large, therefore copying it would take
  too long and make context switching too slow. What is done instead is this part
  of the state is left as is, and virtual memory management is used to prevent other
  programs from interfering with it (usually the address mapping is configured
  so that the entire virtual address space of the process is replaced by
  another, pointing to different physical memory addresses).

- State in the processor registers is relatively small (typically few hundred bytes)
  and easily accessed. The context switching implementation can simply save all
  register values to memory.

- State in the operating system memory does not need to be switched because
  the operating system knows what process it is associated with.

**Q1** Consider the following program:

```c
#include <fcntl.h>
#include <unistd.h>

void main (void) {
    int file = open ("filename", 0);
    char data [1234];
    int size = read (file, data, sizeof (data));
    // Location X
    close (file);
}
```

Assume we need to perform context switch at location X. What would be the state in the program memory,
the state in the processor registers, and the state in the operating system memory, for this program ?
If you can think of more items, it is enough to just give one of each type.

**A1** ...

### Process Preemption

Often, there are many points in program execution where context switching will obviously happen.
This includes especially any blocking I/O operations - when the program asks the operating
system to perform an I/O operation and needs to wait for the result, the operating system
will simply context switch to another program that can run in the meantime.

Sometimes, however, the program might continue running without calling the operating system.
To preserve the illusion of concurrent execution, the operating system must _preempt_ the
program execution and context switch. Usually, this is done by defining a _quantum_ of
time that the program can run and programming the system hardware to generate
an interrupt once that quantum expires. The operating system will handle
the interrupt and perform a context switch as needed.

**Q2** Assume the system has a single processor and three processes that can
run for quite some time without requesting any blocking operation. How often
do you think should the operating system context switch between the programs ?
Give your answer as a suggested quantum length.

**A2** ...


## Threads

Until now, we were only concerned with running multiple isolated programs. But what if
we have multiple processors but only one program to run, are all but one processor
bound to stay idle ?

A possible answer to this situation is the introduction of _threads_. Threads can be
imagined as virtual processors executing (possibly different parts of) our program.
Each thread has its own stack, but shares the address space with the other threads
of the same process, making it (relatively) easy to distribute work over the
shared data to multiple processors.

Read about threads in _Arpaci-Dusseau Section 26 Concurrency Introduction_,

From the context switching perspective, threads bring an interesting simplification.
Since threads share the same address space, context switching between threads
belonging to the same process does not have to handle the state in the
program memory - it simply stays as is. Also, the operating system
resources are often associated with the entire process, rather
than individual threads, hence the state in the operating
system memory is also not an issue. All that remains
is saving and loading processor registers.

**Q3** We have suggested context switching between threads of the same process
does not care about the program memory. But the program memory contains the
stack, which obviously needs to be context switched. How is this apparent
contradiction resolved ?

**A3** ...


## Assignments

In the coming lab assignment, you will implement your own threads. Before you do so,
it is useful to understand how context switching in your kernel works. Below are
the relevant parts of the code - they are in assembly and you should not need
to modify them in any way, however, it helps to get a general idea of what
is happening. Please focus especially on the comments, it is not
necessary to understand every single instruction.

(You will also get the code in the source files of the next assignment.)

First, the context switch itself (the `SAVE_REGISTERS` and `LOAD_REGISTERS`
statements are macros whose implementation follows later):

```assembly
/*
 * void cpu_switch_context(
 *     context_t * a0_this_context,
 *     context_t * a1_next_context
 * )
 *
 * Switches processor to another context. The first argument points to
 * "this" context, i.e., the context_t structure where the current state
 * (general purpose registers, and some special register) of the CPU will
 * be stored.
 *
 * The second argument points to the "next" context, from which the
 * state of the CPU will be restored to resume execution.
 */

.globl cpu_switch_context
.ent   cpu_switch_context

cpu_switch_context:

    /*
     * Save the general-purpose registers and the $hi and $lo
     * registers into the context_t structure pointed to by $a0.
     */

    SAVE_REGISTERS $a0

    mfhi $t0
    mflo $t1
    sw $t0, CONTEXT_HI_OFFSET($a0)
    sw $t1, CONTEXT_LO_OFFSET($a0)

    /*
     * Save the CP0 Status register and disable interrupts (by
     * clearing the IE bit of the Status register), which will
     * allow us to use the $k0 and $k1 registers without having
     * to worry about being interrupted (and the content of the
     * $k0 and $k1 registers being destroyed) while restoring
     * the target context (pointed to by $a1).
     */

    mfc0 $t0, $cp0_status
    sw $t0, CONTEXT_CP0_STATUS_OFFSET($a0)
    la $t1, ~CP0_STATUS_IE_MASK
    and $t0, $t1
    mtc0 $t0, $cp0_status

    /*
     * Load the $hi and $lo registers and the general purpose registers
     * from the target context. This also switches to another stack!
     *
     * Note that we use $k0 as the base register when restoring the
     * general purpose registers, because the $a1 register will be
     * loaded from the target context (and stop pointing to it).
     */

    lw $t0, CONTEXT_HI_OFFSET($a1)
    lw $t1, CONTEXT_LO_OFFSET($a1)
    mthi $t0
    mtlo $t1

    move $k0, $a1
    LOAD_REGISTERS $k0

    /*
     * Prepare to load the CP0 Status register of the target context.
     * The Status register is actually loaded in the branch delay slot
     * of the jump that returns control to the new thread.
     *
     * Setting the Status register in the branch delay slot makes it
     * possible to return from kernel mode to user mode. Setting the
     * register sooner would mean switching from kernel mode to user
     * mode while executing in KSEG0, which is not allowed.
     *
     * A somewhat cleaner alternative to this particular method of
     * returning from kernel mode to user mode is the ERET instruction.
     *
     * Note that setting the Status register to the value from the
     * target context will enable interrupts (if they were enabled).
     */

    lw $k1, CONTEXT_CP0_STATUS_OFFSET($k0)
    j $ra
    mtc0 $k1, $cp0_status

.end cpu_switch_context
```

Now the definition of the macros and the data structure used to hold the state:

```assembly
#ifndef __ASSEMBLER__

#include <types.h>

/**
 * CPU context (registers).
 *
 * The general purpose registers are stored in slots corresponding
 * to their register number.
 *
 * Other registers are stored in subsequent slots.
 *
 * Note that for simplicity, we waste a bit of space for
 * registers that do not need saving ($zero, $k0, and $k1).
 */
typedef struct {
    // 0
    unative_t zero;

    // 1
    unative_t at;

    // 2..3
    unative_t v0;
    unative_t v1;

    // 4..7
    unative_t a0;
    unative_t a1;
    unative_t a2;
    unative_t a3;

    // 8..15
    unative_t t0;
    unative_t t1;
    unative_t t2;
    unative_t t3;
    unative_t t4;
    unative_t t5;
    unative_t t6;
    unative_t t7;

    // 16..23
    unative_t s0;
    unative_t s1;
    unative_t s2;
    unative_t s3;
    unative_t s4;
    unative_t s5;
    unative_t s6;
    unative_t s7;

    // 24..25
    unative_t t8;
    unative_t t9;

    // 26..27
    unative_t k0;
    unative_t k1;

    // 28..31
    unative_t gp;
    unative_t sp;
    unative_t fp;
    unative_t ra;

    // 32..33
    unative_t lo;
    unative_t hi;

    // 34
    unative_t status;

    // 35..36
    unative_t cause;
    unative_t epc;

    // 37..38
    unative_t badvaddr;
    unative_t entryhi;
} context_t;

// This function is implemented in assembler yet it can have normal C signature
void cpu_switch_context(context_t* this_context, context_t* next_context);

#endif

#ifdef __ASSEMBLER__

/*
 * The offsets for GP registers must match the context_t structure, the
 * layout is chosen so that the offsets correspond to register numbers.
 */

.set CONTEXT_ZERO_OFFSET, 0*4

.set CONTEXT_AT_OFFSET, 1*4

.set CONTEXT_V0_OFFSET, 2*4
.set CONTEXT_V1_OFFSET, 3*4

.set CONTEXT_A0_OFFSET, 4*4
.set CONTEXT_A1_OFFSET, 5*4
.set CONTEXT_A2_OFFSET, 6*4
.set CONTEXT_A3_OFFSET, 7*4

.set CONTEXT_T0_OFFSET, 8*4
.set CONTEXT_T1_OFFSET, 9*4
.set CONTEXT_T2_OFFSET, 10*4
.set CONTEXT_T3_OFFSET, 11*4
.set CONTEXT_T4_OFFSET, 12*4
.set CONTEXT_T5_OFFSET, 13*4
.set CONTEXT_T6_OFFSET, 14*4
.set CONTEXT_T7_OFFSET, 15*4

.set CONTEXT_S0_OFFSET, 16*4
.set CONTEXT_S1_OFFSET, 17*4
.set CONTEXT_S2_OFFSET, 18*4
.set CONTEXT_S3_OFFSET, 19*4
.set CONTEXT_S4_OFFSET, 20*4
.set CONTEXT_S5_OFFSET, 21*4
.set CONTEXT_S6_OFFSET, 22*4
.set CONTEXT_S7_OFFSET, 23*4

.set CONTEXT_T8_OFFSET, 24*4
.set CONTEXT_T9_OFFSET, 25*4

.set CONTEXT_K0_OFFSET, 26*4
.set CONTEXT_K1_OFFSET, 27*4

.set CONTEXT_GP_OFFSET, 28*4
.set CONTEXT_SP_OFFSET, 29*4
.set CONTEXT_FP_OFFSET, 30*4
.set CONTEXT_RA_OFFSET, 31*4

/*
 * The offsets for special registers must match the context_t structure.
 * In contrast to GP registers, the layout is irregular, because not all
 * special registers need saving so these are added as needed.
 */

.set CONTEXT_LO_OFFSET, 32*4
.set CONTEXT_HI_OFFSET, 33*4

.set CONTEXT_CP0_STATUS_OFFSET, 34*4

.set CONTEXT_CP0_CAUSE_OFFSET, 35*4
.set CONTEXT_CP0_EPC_OFFSET, 36*4

.set CONTEXT_CP0_BADVADDR_OFFSET, 37*4
.set CONTEXT_CP0_ENTRYHI_OFFSET, 38*4


/*
 * The SAVE_REGISTERS macro stores general purpose registers
 * into the context_t structure. The macro can be used with
 * any general purpose register (no registers are destroyed
 * when this code runs).
 */
.macro SAVE_REGISTERS base
    // No need to save $zero register

    sw $at, CONTEXT_AT_OFFSET(\base)

    sw $v0, CONTEXT_V0_OFFSET(\base)
    sw $v1, CONTEXT_V1_OFFSET(\base)

    sw $a0, CONTEXT_A0_OFFSET(\base)
    sw $a1, CONTEXT_A1_OFFSET(\base)
    sw $a2, CONTEXT_A2_OFFSET(\base)
    sw $a3, CONTEXT_A3_OFFSET(\base)

    sw $t0, CONTEXT_T0_OFFSET(\base)
    sw $t1, CONTEXT_T1_OFFSET(\base)
    sw $t2, CONTEXT_T2_OFFSET(\base)
    sw $t3, CONTEXT_T3_OFFSET(\base)
    sw $t4, CONTEXT_T4_OFFSET(\base)
    sw $t5, CONTEXT_T5_OFFSET(\base)
    sw $t6, CONTEXT_T6_OFFSET(\base)
    sw $t7, CONTEXT_T7_OFFSET(\base)
    sw $t8, CONTEXT_T8_OFFSET(\base)
    sw $t9, CONTEXT_T9_OFFSET(\base)

    sw $s0, CONTEXT_S0_OFFSET(\base)
    sw $s1, CONTEXT_S1_OFFSET(\base)
    sw $s2, CONTEXT_S2_OFFSET(\base)
    sw $s3, CONTEXT_S3_OFFSET(\base)
    sw $s4, CONTEXT_S4_OFFSET(\base)
    sw $s5, CONTEXT_S5_OFFSET(\base)
    sw $s6, CONTEXT_S6_OFFSET(\base)
    sw $s7, CONTEXT_S7_OFFSET(\base)

    // No need to save $k0 and $k1 registers

    sw $gp, CONTEXT_GP_OFFSET(\base)
    sw $sp, CONTEXT_SP_OFFSET(\base)
    sw $fp, CONTEXT_FP_OFFSET(\base)
    sw $ra, CONTEXT_RA_OFFSET(\base)
.endm SAVE_REGISTERS

/*
 * The LOAD_REGISTERS macro loads general purpose registers from
 * the context_t structure. The macro should ONLY be used with
 * registers $k0 or $k1 as the \base register, because other
 * registers will be overwritten during restoration.
 *
 * Also, precisely because we will be using the $k0 and $k1
 * registers, the interrupts MUST be disabled to ensure that the
 * code cannot be interrupted (and the contents of $k0 and $k1
 * destroyed).
 *
 * Note that the macro also restores the stack pointer, i.e,
 * it switches to another stack.
 */
.macro LOAD_REGISTERS base
    lw $ra, CONTEXT_RA_OFFSET(\base)
    lw $fp, CONTEXT_FP_OFFSET(\base)
    lw $sp, CONTEXT_SP_OFFSET(\base)
    lw $gp, CONTEXT_GP_OFFSET(\base)

    // Must not restore $k0 and $k1 registers

    lw $s7, CONTEXT_S7_OFFSET(\base)
    lw $s6, CONTEXT_S6_OFFSET(\base)
    lw $s5, CONTEXT_S5_OFFSET(\base)
    lw $s4, CONTEXT_S4_OFFSET(\base)
    lw $s3, CONTEXT_S3_OFFSET(\base)
    lw $s2, CONTEXT_S2_OFFSET(\base)
    lw $s1, CONTEXT_S1_OFFSET(\base)
    lw $s0, CONTEXT_S0_OFFSET(\base)

    lw $t9, CONTEXT_T9_OFFSET(\base)
    lw $t8, CONTEXT_T8_OFFSET(\base)
    lw $t7, CONTEXT_T7_OFFSET(\base)
    lw $t6, CONTEXT_T6_OFFSET(\base)
    lw $t5, CONTEXT_T5_OFFSET(\base)
    lw $t4, CONTEXT_T4_OFFSET(\base)
    lw $t3, CONTEXT_T3_OFFSET(\base)
    lw $t2, CONTEXT_T2_OFFSET(\base)
    lw $t1, CONTEXT_T1_OFFSET(\base)
    lw $t0, CONTEXT_T0_OFFSET(\base)

    lw $a3, CONTEXT_A3_OFFSET(\base)
    lw $a2, CONTEXT_A2_OFFSET(\base)
    lw $a1, CONTEXT_A1_OFFSET(\base)
    lw $a0, CONTEXT_A0_OFFSET(\base)

    lw $v1, CONTEXT_V1_OFFSET(\base)
    lw $v0, CONTEXT_V0_OFFSET(\base)

    lw $at, CONTEXT_AT_OFFSET(\base)

    // No need to restore $zero register
.endm LOAD_REGISTERS

#endif
```

If you have persisted until here, congratulations !
Do not worry too much if some details of the code
are not clear, a general impression is enough.

**Q4** The `cpu_switch_context` function takes two arguments.
One is the "output" data structure used to save the current context into,
the other is the "input" data structure used to load the new context from.
What is the size of these structures, in bytes ?

(Size is useful to give you some idea of how much work it is to context switch between threads.)

**A4** ...

**Q5** An earlier question asked how stack is handled when context switching between threads.
Can you identify the location in this context switch code that handles the stack ?
Give the text of the line(s) in question as your answer.

Please note that our quiz parser stops at empty lines, if you use empty lines
in your answer replace them with single dot or use some other visually similar solution.

**A5** ...


And finally, a mandatory summary question:

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** Parallelization is a big / important subject - when/where has it been first tackled on a big scale?
