# Process Memory Layout Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Summary So Far

- You should have rough idea of operating system architecture
  - Operating system is just another program on your computer
    - Most aspects not really different from other applications
    - But parts of it are given elevated privileges
      - User mode for applications (and libraries and services)
      - Kernel mode for operating system functions applications should not have
  - Operating system provides
    - Abstractions over hardware (like files or sockets)
    - Management of shared resources (the whole computer)
      - Direct access by applications typically not allowed
      - Aplications ask operating system through system calls

- You should have some idea of what program code looks like
  - Instructions operate at very low level of abstraction
  - Examples of how instructions are used from earlier courses


## Selected Left Over Discussion Bits

### Privileges

- poor examples of privileged operations (L1 G4)
    - possibility to jump to a bad address (kernel or not mapped)
    - screen output (system not necessarily involved in screen output)

- program will be terminated if trying to open a non existing file (L1 G3) ?
    - too harsh, see `man open` section `RETURN VALUE`

### Program Compilation

- why is C compiled into assembly first and not directly into machine code ?

- why are there the runtime libraries ?
    - and why is the compiler adding so much junk ?
    - why is libc.so linked even to program with empty main ?
    - https://stackoverflow.com/questions/34966097/what-functions-does-gcc-add-to-the-linux-elf

- why do some obvious optimizations not happen ?
    - why did not the code simply return a constant ?
        - https://stackoverflow.com/questions/572547/what-does-static-mean-in-c
    - why not inlining functions even at max optimization (L2 Q4) ?
        - https://stackoverflow.com/questions/40778557/c-compiler-optimisationgcc-automatically-inline-a-non-static-function-vs-stat
        - inlining heuristics quite complex
    - why not tail call optimize the example (L2 Q5) ?
        - taking address of stack variable forces compiler to materialize it
        - but you can force it to do inlining at least :-)

### Variable Addressing

- absolute vs relative addressing
    - why compute offset at runtime ?
    - what is the global offset table ?
    - why is addressing different in 32bit and 64bit mode ?
    - why use relative addressing for static global variable ? (64bit eip relative)

- how to read various addressing expressions ?
    - Intel Vol.1 3.7.5 Specifying an Offset
    - Intel syntax [base + index*scale + disp]
        - for example `mov eax, [rbx + rsi * 4 + 1234h]`
    - AT&T syntax disp (base, index, scale)
        - for example `movl 0x1234 (%rbx, %rsi, 4), %eax`

- what are FS and GS in address expressions ?
    - Intel specific segmentation
    - FS (often) used for TLS
    - https://unix.stackexchange.com/questions/209968/what-is-the-register-gs-used-for

- is addressing different for code and data ?

### Reading Assembly Language

- http://patshaughnessy.net/2016/11/26/learning-to-read-x86-assembly-language
- can we do an assembly language refresher ?
    - https://godbolt.org
    - https://learnxinyminutes.com/docs/mips
    - http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
    - http://flint.cs.yale.edu/cs421/papers/x86-asm/asm.html
    - but you should not need it for lab assignments

- what are the directives in the sources ?
    - we need to emit more than just code
    - output structured into sections
        - multiple .text sections in assembly ?

- gcc -g and objdump -S or gcc -fverbose-asm


## Content For Today

- We shall discuss what process memory layout looks like
  - Virtual address space
    - Simply a large array of bytes where your whole application resides
    - Only an illusion provided by paging but ignore that for now
    - Any address a program uses automatically
  - What we need to store in that address space
    - Our program (code) and all the libraries
    - Quite often the kernel is also there
    - Any data our program has
        - Static data (trivial)
        - Stack(s)
        - Heap


## Program Code

- We want to have a rough idea of how program code gets into memory
- What do executable files contain ?
    - Code (obviously)
    - But often much more
        - Linking and relocation information
        - Information for source level debugging

### Linking

- Like lego bricks (plug exports into externals)
- Static vs dynamic

### Summary for Q1

- you could dump the symbols in the library files for accurate answer
- correct
    - symbols are provided by the standard system library libc
- incorrect
    - library (so) vs header (h)
- miscellanea
    - what is VDSO ? https://en.wikipedia.org/wiki/VDSO


### Relocation

Essential for libraries, but also memory layout randomization.

- why do we have to relocate en bloc ?
- why not always compile as position independent executable ?
- why would one disable randomization ?
- is there performance impact of randomization ?
- why minimum randomization step of 4 kilobytes ?

- what is the randomization range ?
    - https://lwn.net/Articles/667790

### Summary for Q2
- incorrect
    - granularity is the size of program (so that two images do not overlap)
        - remember each process (program instance) has its own address space


## Program Stack

Really just part of the virtual address space (simple array) where stack pointer points to.

- why is stack growing downwards ?
- and can I change the stack direction ?

- can I save a variable at a specific stack address ?

- why is the stack over allocated in the examples ?
    - max_align_t

- how is stack capacity determined ?
    - https://unix.stackexchange.com/questions/127602/default-stack-size-for-pthreads
- can we prevent stack overflow ?
    - gcc -fstack-check
    - but what to do then ?
- can the stack stretch from memory to disk ?

- can I access values on stack below the stack pointer,
  for example those values left there after return from a function ?

- what is the speed difference between hardware and software stack ?

### Calling Conventions

- calling conventions in C ?
- calling conventions on MIPS ?
- when are local variables (or arguments) not on the stack ?
- how come operating system (Windows vs Linux on the same processor) can influence stack layout ?

- how will the stack and heap features in our assignment environment be limited ?

### Summary for Q3 and Q4
- incorrect
    - tying stack locations to order of things in source
        - order of functions
        - order of variables

### Summary for Q5
- correct
    - in principle overwrite is possible and then the code would crash
    - but in 64-bit address spaces stack is quite far from code
    - and there will be non allocated memory in between
    - or there will be explicit stack guard page
    - or it will trash heap before code
    - and code is read only anyway
- incorrect
    - stack overflow detected magically
        - memory protection typically plays role


# Administration

- overall course concept appears to work
    - quite insightful questions
    - high scores on quizzes
- but also some people report issues
    - not comfortable with self study style
    - worried about time requirements
    - missed information
    - totally lost

What we are doing about your concerns:

- self study style
    - including more connecting text in self study instructions
    - taking care to provide accurate and limited references
    - briefly summarizing content during lectures
        - also remember that lectures discuss questions you raised
        - so be specific in your questions and do not be afraid to ask
    - when English is an issue write responses in Czech or Slovak

- worried about time requirements
    - time spent on self study and quizzes
        - please be careful about strategy
            - go through the entire quiz first and look up details afterwards
            - stop (and send us your questions) if you go beyond roughly one hour
            - remember that grading is very liberal (50% of points already passes)
        - into how much detail should we go ?
            - tutorial on invoking gcc ?
            - how much are we supposed to know of objdump and assembly ?
            - can you provide better references so that we do not have to google everything ?
    - time spent on project assignments
        - we can only easily callibrate on past years
        - push frequently so that we see progress
        - do not waste time when stuck
            - ask in mailing list
            - ask during labs
            - but ask !
    - https://www.construx.com/blog/productivity-variations-among-software-developers-and-teams-the-origin-of-10x

- missed information
    - why is deadline for quizzes NOT ON WEBPAGE ?
    - trying to concentrate everything in one place
    - quite tolerant when you miss something (but giving signals)

- totally lost
    - ask for help during labs
        - more time for one-on-one interaction
        - fewer people so less pressure
    - but you have to give us some hint
