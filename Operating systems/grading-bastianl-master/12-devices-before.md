# Devices

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday December 17 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the second of three self study modules that will look at devices and I/O.
The goal of this module is to discuss the typical device driver architecture
and to take a look at the interface common devices export.


## Device Driver Architecture

In _Arpaci-Dusseau Section 36 I/O Devices_, recommended for the last self study module,
you could see the code of a simple IDE (disk) driver with a single public facing
function, `ide_rw`, which puts a disk read or write request into a queue
that the driver maintains, and, if the disk is idle, tells the disk
controler to go to work. The driver also has a single interrupt
handling function, `ide_intr`, which is invoked when the disk
controller finishes work.

**Q1** The public facing function and the interrupt handler both access a shared queue, which is protected by a lock.
Think about the kernel implementation you are working on in your assignments.
What would happen if a thread were holding a lock
(such as is done in the `ide_rw` function above)
and an interrupt handler attempted to acquire it
(such as is done in the `ide_intr` function above) ?

**A1** ...


## Device Interfaces

In _Arpaci-Dusseau Section 36 I/O Devices_, recommended for the last self study module,
you could see an example protocol for communicating with an IDE (disk) controller.
The protocol went roughly as follows:

- The controller exposes multiple registers
    - Sector number and count
    - Command (for writing)
    - Status (for reading)
    - Data

- The main device functions are sector read and sector write

- To perform a sector read the driver has to
    - Wait until the status register says the controller is ready
    - Write the sector number and count to the controller registers
    - Write the sector read command code to the command register
    - Wait until the status register says the command is done
    - Read the sector data from the data register

- Writing is performed analogously

**Q2** Pick some device type (other than disk) and, at roughly the same level of detail as above,
describe how you think such device is controlled by the operating system - in particular,
list the main device functions and how the controller would be told to perform them.
There is no need to consider advanced functions, or describe existing hardware,
just describe how you think your chosen device type could work.

Please remember, no empty lines in the answer.

**A2** device - keyboard - in this case the relationship is kind of the other way - the keyboard is telling the OS that it should make time for its input rather than the keyboard being asked to record the key presses. The keyboard controller has the ability to tranfer the key presses-switch toggles to "some meaningful digital data" for the PC (signal that a key X, Y or whichever has been pressed) and also the ability to tell the system to "start listening" to what the keyboard input is. The main device functions are to pass the input to the system, the controller does them by transforming the physical input to digital and sending an interrupt to the system, once its listening also sending the data. If it is some special key, like mac command key and windows windows key there has to be a driver to translate the meaning of this to the particular system.


## Administration

As explained in the mail, you can complete the course when you have enough points for a grade, even when you did not submit all the assignments.
From the few discussions with students and the current point statistics, we believe most teams might decide to skip the last (sixth) assignment.
To help us with preparation, we need to know who, if anyone, would decide to implement the last (sixth) assignment.

**Q3** Will you, personally, work on the last (sixth) assignment ? A positive answer is binding, the default is "no".

**A3** no


No mandatory Q0, instead, please answer Q2 (in other words, Q2 is mandatory :-).
