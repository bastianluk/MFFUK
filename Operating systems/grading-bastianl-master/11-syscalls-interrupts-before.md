# System Calls and Interrupts

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday December 10 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the first of three self study modules that will look at devices and I/O.
The goal of this module is to refresh knowledge of basic device communication
mechanisms, in particular interrupts and direct memory access, and start
discussion of the intricacies of interrupt handling.


## System Calls

Why mention system calls ? On many processor architectures, the handling of system calls is
similar to handling of processor exceptions and handling of device interrupts. In all three
cases, the operating system code obtains control at a well defined system entry point
(system call handler, exception handler, interrupt handler), possibly saves some
context information (return address, register content that must not be trashed),
executes the appropriate handler and then returns from the kernel
back to the application that was interrupted.
So this is as good a place as any to fill in some remaining details on system calls.

Luckily, you already did some self study about system calls in _Arpaci-Dusseau Section 6 Limited Direct Execution_ so you do not have to do that again.

One thing that distinguishes system call handling from exception handling and interrupt handling
is the fact that system call usually receives parameters from the application invoking it.
Such parameter passing must follow some parameter passing conventions.
For examples of such conventions, look at `man syscall`.

**Q1** On a MIPS architecture in Linux, what is the register used to pass the system call number to the kernel ?

**A1** ...

System calls are also a security sensitive interface because they make it possible
for code with low privileges (applications) to request service
from code with high privileges (kernel).
The checks performed on the interface naturally depend on the particular system call.

**Q2** Using a Linux source code browser, such as https://elixir.bootlin.com, take a look at the `get_user` macro for the Intel 80x86 architecture.
(In the current Linux kernel version the link to the macro is https://elixir.bootlin.com/linux/latest/source/arch/x86/include/asm/uaccess.h#L172.)
Looking at the comments and the code (when following the links, make sure to stay in the Intel 80x86 sources),
can you tell what this macro is for and name at least one check it does ?

**A2** ...


## Devices

You should have some idea of how devices are connected to the processor from earlier courses.
Few major points to recall:

- Devices are connected through their _controllers_ (electronic circuitry designed to permit the processor to control the devices)
- Controllers expose _registers_ that the processor reads and writes to interact with the device
    - The name is really overloaded, controller registers and processor registers are different things
    - The controller registers have _addresses_ that are usually hardwired or configured at boot time
    - Processor accesses the controller registers simply by reading and writing the right addresses
        - On Intel processors special `in` and `out` instructions are used
        - On MIPS processors standard memory load and store operations work

- A device controller can (typically) ask for service by generating an _interrupt_ request
- A device controller can (typically) access memory directly when transferring data

If you want one example of a device you are already familiar with, look at the console device in your assignments.
The controller of this device only exposes one register,
configured in `msim.conf` to reside at physical address `0x10000000`,
and accessed in `printer.h` using kernel segment address `0x90000000`.
Writing to this register tells the device to display the character whose code is written.
This particular device never generates an interrupt request and never accesses memory directly.

If you think your recollection of devices from earlier courses is not sufficient, you can read _Arpaci-Dusseau Section 36 I/O Devices_.

**Q3** Imagine you have a keyboard device with a single controller register that can only be read.
What would you expect the register read operation to return ?

**A3** ...


## Interrupts

Devices may require operating system attention at arbitrary moments.
A network card controller may require service whenever a network packet arrives.
A keyboard controller may require service whenever a key is pressed or released.
We therefore need a way for the device to attract attention of the processor,
which would otherwise only execute whatever current program it runs.
This is done through interrupts.

Every time a processor is about to execute an instruction, it checks whether an interrupt is requested.
(You can imagine there is a wire leading into the processor, labeled IRQ for interrupt request,
and the processor checks whether there is an active logical signal on that wire.)
If it is, the processor will save the current program counter address
and fetch the next instruction from a predefined address.
Effectively, this means the processor calls an interrupt handler in response to the interrupt request.
This is all we need, for the interrupt handler can then talk to the device controllers and perform whatever service is requested.

If you want one example of interrupt handling you are already familiar with, look at the timer interrupt in your assignments.
Although in this case the interrupt is generated by circuitry internal to the processor,
you can imagine that the timer is an independent device that interrupts the processor.

If you think your recollection of interrupts from earlier courses is not sufficient, you can read _Arpaci-Dusseau Section 36 I/O Devices_.

**Q4** Imagine a system with multiple running processes.
One of those processes performs intensive network I/O,
with interrupts used to service the network device.
The other processes just compute with no I/O.

Assume further that this system has a simple scheduler that runs processes round robin for given time quanta.

The interrupt service code obviously needs some time to run. Whose quantum will this time be deducted from ?

**A4** ...


## Direct Memory Access

Some devices either produce or consume large amounts of data.
Rather than requiring the processor to read such data from the device and write it to memory, or vice versa,
it may be more efficient to let the device write the data to memory or read the data from memory directly.
Since the device controller can access the system bus, this is obviously possible in principle,
even if many technical details must be resolved.
This is called DMA or _direct memory access_.

In some systems, a special controller is dedicated to coordinating
data transfers between the device controller and memory.
Such controller is called _the DMA controller_.
On other systems, device controllers are smart enough
to perform direct memory access themselves,
such controllers act as _bus masters_.

If you think your recollection of direct memory access from earlier courses is not sufficient, you can read _Arpaci-Dusseau Section 36 I/O Devices_.

**Q5** Imagine a network card has just received data that should be stored in memory using direct memory access.
How does the network card know what address to store the data at ?

**A5** ...


Finally, a mandatory summary question (note that you must come up with a question for the quiz to be accepted):

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** ...
