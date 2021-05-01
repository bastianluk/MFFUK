# Process Memory Layout

This is a graded test, do not forget to commit your responses in time !


## Code and Linking

When launching a program, the operating system must dynamically link it with the required shared libraries.

**Q1** You already saw how to use the `ldd` tool to display the list of shared libraries an executable requires.
Use the tool to compute the average number of shared libraries required by the executables
in the `/bin/` directory on the `lab.d3s.mff.cuni.cz` machine.

(See [this link](https://d3s.mff.cuni.cz/teaching/nswi004/qa/#logging-to-the-lab-machine) for login instructions.)

Besides giving the number, describe what you did to get it.

**A1** executables count = 1441, sum dep = 17545 >>> avg dep count = 12,1756 ... around 12 dependencies per executable. I ran ldd for each executable (used test to see if ldd can be run for each file i from /bin/*) in the folder storing the result executable per line in some file, then number of rows devided by the sum of values.

The following two questions concern an example compiled on the `lab.d3s.mff.cuni.cz` machine
in the `/srv/nswi004/examples/main-with-libs` directory, you will need to log in to that
machine to answer correctly.

The example consists of the application executable and three shared libraries,
compiled from `main.c`, `libone.c`, `libtwo.c` and `libthree.c` respectively.

```c
extern void lib_one (void);
extern void lib_two (void);
extern void lib_three (void);

int main (void) {

    lib_one ();
    lib_two ();
    lib_three ();

    return (0);
}
```

```c
#include <stdio.h>

void lib_one (void) {
    printf ("lib_one called\n");
}
```

The shared libraries were compiled using `gcc -shared -fpic -o libone.so libone.c` and so on.
When run, the `main` executable displays the expected output:

```
lib_one called
lib_two called
lib_three called
```

**Q2** It so happens that the example on the `lab.d3s.mff.cuni.cz` machine was not compiled correctly.
Only two out of the three libraries are linked dynamically, one is linked statically.
Find which one and give its name here.

**A2** libone

**Q3** There is another issue with the example on the `lab.d3s.mff.cuni.cz` machine.
One of the libraries was actually compiled from slightly different sources and
exports one extra symbol in addition to the `lib` function.
Type the name of the symbol here.

(Note that the symbols exported by the shared libraries for dynamic linking are not stored
together with standard symbols. If tools like `objdump` report an empty symbol table,
that is because you are dumping the standard symbols rather than the dynamic symbols.
See the man page for the right switch.)

**A3** yes_i_am_exported from libthree


## Code and Relocation

When launching a program, the operating system must make sure it is relocated appropriately,
that is, it must adjust all absolute addresses contained within the program to point to
the correct locations depending on where the program was actually loaded.

The program can also contain relative addresses, that is, addresses that keep pointing
to the same location if the relative distance between the reference and the target
stays the same (for example when the program is loaded in one piece).

The following question is about recognizing absolute and relative addresses.

**Q4** Here is a simple C loop that sums 1024 integers.

```c
int data [1024];
int sum = 0;

void summarize () {
    for (int i = 0 ; i < 1024 ; i ++) {
        sum += data [i];
    }
}
```

Below is one possible way the loop can be compiled into Intel x86 assembly - opcodes to the
left, instructions to the right. Can you, by looking at the opcodes, tell which
instructions use relative and which absolute addresses ?

To avoid confusion in terminology, just assume that absolute addresses are addresses that point directly to a memory location, while relative addresses reference a memory location by giving the distance from another absolute address.

```
0x8049166:   b8 00 00 00 00          mov    $0x0,%eax
0x804916b:   8b 15 24 c0 04 08       mov    0x804c024,%edx
0x8049171:   03 14 85 40 c0 04 08    add    0x804c040(%eax,4),%edx
0x8049178:   89 15 24 c0 04 08       mov    %edx,0x804c024
0x804917e:   83 c0 01                add    $0x1,%eax
0x8049181:   3d 00 04 00 00          cmp    $0x400,%eax
0x8049186:   75 e3                   jne    0x804916b

0x804c024:   00 00 00 00             ;int sum

0x804c040:   .. .. .. ..             ;int data [1024]
```

For those who are not familiar with Intel x86 assembly and prefer not to guess,
here is an explanation of what the individual instructions do.

```
mov    $0x0,%eax                     ;register eax = 0
mov    0x804c024,%edx                ;load value from memory address 0x804c024 to register edx
add    0x804c040(%eax,4),%edx        ;add value from memory address 0x804c040+eax*4 to register edx
mov    %edx,0x804c024                ;store value in register edx to memory address 0x804c024
add    $0x1,%eax                     ;add 1 to register eax
cmp    $0x400,%eax                   ;compare value in register eax to 0x400
jne    0x804916b                     ;if last compare was not equal continue executing from 0x804916b
```

Use line numbers (1 to 7) to refer to addresses in your answer.

**A4** 1 - none/register, 2 - absolute, 3 - relative offset from an address, 4 - absolute, 5 - none/register, 6 - none/register, 7 - absolute

**Q5** On the `lab.d3s.mff.cuni.cz` machine (for consistent results), download the [prime sieve example](https://raw.githubusercontent.com/d-iii-s/teaching-performance-evaluation/master/src/experiment-prime-sieve/basic.cc). Compile it using GCC into both a position-independent executable (`g++ -fpie -fpic ...`) and a position-dependent executable (`g++ -fno-pic ...`), and take a look at the size of the code in the executables (section `.text`).

Which code is bigger, the position-independent one or the position-dependent one ? By how many percent ?

**A5** Position independent is bigger (main went from 63 instructions to 69 >> 9.5% increaase in size) other than that it was pretty much similar and the whole output went from 223-is to 230-ish lines and the increase is less noticeable.
