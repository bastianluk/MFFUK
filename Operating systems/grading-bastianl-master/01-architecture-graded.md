# Operating System Architecture

This is a graded test, do not forget to commit and push your responses in time (deadline is noon October 8).


## Basic Computer Functionality

This question tests your awareness of basic computer functionality from past courses.
Think about the examples of processor instructions you saw,
and think about functions that you need libraries to implement.

**Q1** Consider the following general descriptions of operations.
In the description, `X` and `Y` are used in place of register names
(imagine for example `EAX` for Intel or `RA` for MIPS),
`F` is used in place of a file name
(imagine for example `README.md`),
`M` is used in place of a memory address
(imagine for example `0x12345678`),
`C` is used in place of an integer constant
(imagine for example `0xCAFE`).

1. add `C` to value in `X`
2. copy value from `X` to `Y`
3. read value from `M` and store it in `X`
4. read value from file `F` position `C` and store it in `X`

Which of the operations above are likely provided as actual processor instructions in a common processor ?

**A1** 1,2,3 - 4 is NOT as the concept of files is provided by the OS - for the CPU it is just memory.


## Program Execution

This pair of questions concerns the interaction between the executing program and the operating system, as described for example in _Arpaci-Dusseau Section 6 Limited Direct Execution_. Think about the reasons why the executing program, which is in control of the processor, would willingly cede this control to the operating system, or why the operating system would usurp the control without the cooperation of the executing program.

**Q2** Give an example of a situation where an executing application would cede control to the operating system, which would eventually return control back to the application.

**A2** The application wants to make an I/O operation - the OS has to take over, if CPU is running in Kernel mode, it should succeed granted the application is not buggy, and once done, the OS will give the control back.

**Q3** Give an example of a situation where an executing application would cede control to the operating system, which would react by terminating the application.

**A3** The application tries to access invalid memory - the application would be terminated.


## Privileged Operations

This pair of questions targets your understanding of privileged instructions, that is, processor instructions whose execution should be restricted to the privileged (kernel) mode of operation. The questions use specific instruction examples for the MIPS processor (which will be used in the lab assignments), you may want to consult the relevant sections of the MIPS processor manual.

**Q4** The `BNE` instruction (MIPS manual page A-39) is a conditional control transfer instruction. It has three operands, denoted as `rs`, `rt`, and `offset`. It compares the content of register `rs` with the content of register `rt` and if the two differ, it transfers control to an address that is `offset * 4` bytes away from the current program location.

Is this instruction privileged ? Defend your answer (that is, argue for why you answered the way you did).

**A4** NOT privileged - granted the `offset * 4` is a valid memory location that the program can access.

**Q5** The `MTC0` instruction (MIPS manual page A-114) is an instruction for moving data between general processor registers and registers dedicated to system control. It has two operands, denoted as `rt` and `rd`, and it simply moves the content of general processor register `rs` to the system control register `rt`. (For the list of general processor registers, see MIPS manual Figure 1-2, for the list of system control registers, see MIPS manual Table 1-19.)

Is this instruction privileged ? Defend your answer (that is, argue for why you answered the way you did).

**A5** IS privileged - it is trying to write to a system control register and interaction with those is a privileged operation.
