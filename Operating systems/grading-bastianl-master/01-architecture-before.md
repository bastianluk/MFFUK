# Operating System Architecture

The goal of this self study module is to refresh knowledge on basic computer functionality,
in particular on the representation of a program in the computer memory, and
to look at the basic architecture of the operating system and
the work an operating system has to do when loading programs.


## Basic Computer Functionality

The background for this lecture includes awareness of basic computer functionality, such as taught in
the [NSWI120 Principles of Computers](https://d3s.mff.cuni.cz/teaching/nswi120)
and [NSWI170 Computer Systems](https://www.ksi.mff.cuni.cz/teaching/nswi170-web)
courses. Please consult the course pages for refresher materials.

**Q1** Consider the following general descriptions of operations:

1. add constant 123 to value in register X
2. copy value from register X to register Y
3. compute sin of value in register X and store result in register Y
4. read value from memory address 123 and store it in register X
5. write value from register X to memory variable V
6. print character with code in register X to screen
7. copy memory block from address 123 of size 456 to address 789

Which of the operations above could be provided as actual processor instructions in a common processor ?

**A1** 1., 2., 4., 5., 7.


## Operating System Architecture

The role of the operating system is to manage shared resources and provide shared abstractions
for the applications executing on the computer. On the management part, the main goals are
fair (or otherwise controlled) resource allocation and protection. On the abstraction
part, the operating system provides concepts such as processes, sockets, files,
and so on.

Resource protection relies on introducing the concept of privileged operations,
which only the operating system is permitted to execute. The architecture
of the operating system typically reflects this concept by defining
the system kernel, which executes the privileged operations, and
possibly additional system services, which have no special
privileges compared to applications.

Read up on the concept of privileged operations in _Arpaci-Dusseau Section 6 Limited Direct Execution_.

**Q2** Give an example of a (high level) operating system function that needs to execute privileged operations.

**A2** Terminate process / end task

**Q3** Give an example of a (low level) program operation that would be considered privileged.

**A3** Related to the above - remove process from memory


## Machine Program Representation

The processor executes program represented as machine instructions. Machine instructions
are stored simply as numbers in memory, specific numbers denote specific instructions
and their operands. Encoding of instructions to numbers depend on the particular
processor type, examples can be found in the processor manuals.

Check out an example of instruction encoding for Intel in the _Processor Manual Volume 2 Appendix A Opcode Map_.
Check out an example of instruction encoding for MIPS in the _Processor Manual Section 2.1 CPU Instruction Formats_.

Human readable notation is used to represent machine instructions in disassembly.
Typically, a compiler can produce an assembly listing as well. Consider this
code and try the following on a Linux computer:

```c
int i = 1234;
int main (void) { return (i); }
```

With GCC:
```shell
gcc -S main.c
less main.s
```

With CLANG:
```shell
clang -S main.c
less main.s
```

Alternatively, we can disassemble the binary format produced by the compiler.

With GCC:
```shell
gcc -o main main.c
objdump -d main
```

With CLANG:
```shell
clang -o main main.c
objdump -d main
```

Obviously, the processor will attempt to interpret any memory content as code if told to do so.
This is just for fun:

```shell
echo 'Hello !' > code.bin
objdump -b binary -m i386:x86-64 -D code.bin
```

**Q4** On modern computers, the examples above would be compiled in 64-bit mode by default.
That mode produces somewhat more complicated code than the 32-bit mode.
Add the `-m32` compiler command line switch to the examples above
to get the 32-bit code rather than the 64-bit code.

In the generated code, look at the body of the `main` function, particularly how it accesses the `i` variable.
How is the address of the `i` variable specified in the assembly source and in the disassembly ?

**A4** EDIT POST deadline - "empty answer"

Processors support various addressing modes.
Read about Intel addressing modes in the _Processor Manual Volume 1 Section 3.7.5 Specifying an Offset_.
Read about MIPS addressing modes in the _Processor manual Appendix A Instruction LD_.

**Q5** Look at the code from the example above.
What memory addresses is it located at ?
What would need to be done if it were to be moved to a different range of addresses ?

**A5** EDIT POST deadline - "empty answer"

Finally, a mandatory summary question:

**Q0** Phrase one question you would like to ask related to the topics above.

**A0** EDIT POST deadline - "empty answer"
