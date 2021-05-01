# Operating System Architecture Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.

## Meta

- What was the point of these exercises ?
- What was the answer supposed to be ?
- How should we read large manuals ?
- Can we use Czech or is English mandatory ?
- What Linux distribution would you recommend ?

## Summary for Q1

## Processor

- privileges
    - how does the processor know if it is in kernel mode or user mode ?
        - Intel: segment model with CPL in CS
            - why are rings 1 and 2 not used much ?
        - MIPS: field in CP0 Status
- instructions
    - why the different nop instructions like nopl or nopw
    - why is 32bit assembly longer than 64bit assembly ?
    - would the example code be longer on RISC ?
    - how is sine computed if not supported by the processor ?
        - http://www.coranac.com/2009/07/sines
        - the big question is how does the processor do it :-)
- misconceptions
    - processors do not work with memory but instead with cache ?
    - would a 16bit processor align the program to multiples of 2 ?

## Operating Systems

- are smart phone systems the same as desktop systems ?
- what exactly does the trap table look like (syscall number, handler address) ?
    - Intel Vol. 3A 6.10
    - MIPS Ch. 5 Exception Vector Locations
    - note searching vs direct indexing !
- scheduling
    - how do interrupts work (how can the timer stop the executing program) ?
    - is the (scheduler) timer interrupt set by operating system or processor specific ?

## Summary for Q2

- correct
    - an I/O operation
    - context switching
- depends
    - memory allocation

## Summary for Q3

- my bad for not defining low level
- correct
    - syscall instruction
    - disabling interrupts
    - access to memory that should not be accessible
- incorrect
    - accessing memory

## Process Execution

- why not analyze code instead of designing hardware support for privileges and interrupts ?
- we have read only about processes but what about threads and who handles them ?
- how do we know what a process can do after switching to kernel mode ?
- how does the processor know what instructions are privileged ?
- who decides the address where the program is located ?

## Program Compilation

- why are there the runtime libraries ?
    - and why is the compiler adding so much junk ?
    - https://stackoverflow.com/questions/34966097/what-functions-does-gcc-add-to-the-linux-elf
- why did not the code simply return a constant ?
    - https://stackoverflow.com/questions/572547/what-does-static-mean-in-c

## Variable Initialization

- why are constructors not called ?

## Variable Addressing

- why compute offset at runtime ?
- what is the global offset table ?
- why is addressing different in 32bit and 64bit mode ?
- why use relative addressing for static global variable ? (64bit eip relative)
- why are sometimes address calculations apparently inefficient ? (32bit pic magic)
- is addressing different for code and data ?

## Reading Assembly Language

- http://patshaughnessy.net/2016/11/26/learning-to-read-x86-assembly-language
- how to read various addressing expressions ?
    - Intel Vol.1 3.7.5 Specifying an Offset
    - Intel syntax [base + index*scale + disp]
    - AT&T syntax disp (base, index, scale)
- how to locate specific instruction syntax ?
- can we do an assembly language refresher ?
    - https://godbolt.org
    - https://learnxinyminutes.com/docs/mips
    - shall we also write this or just read ?
- what are the directives in the sources ?
- where is the variable value ?
- what are the push and pop instructions doing in the code ?
- are there differences between compilers we should know about ?

## Summary for Q4

- correct
    - by name vs by address
    - by relative address vs by absolute address

## Relocation

- why would code need to be moved ?

## Summary for Q5

- correct
    - moving en bloc should work
    - addresses may need to be recomputed
    - moving typically means adding offset to addresses
- incorrect
    - code will be stored on some addresses on the disk
    - moving would require changing the base stored in rip
    - moving would require changing the base stored in ebp
    - relative jump targets would have to be recomputed
    - addresses are not known because we work with virtual memory
    - addresses would have to be mapped to virtual addresses
    - processor will add offset to every address used
    - syscall would give us the addresses of code and data

https://eli.thegreenplace.net/2012/01/03/understanding-the-x64-code-models
