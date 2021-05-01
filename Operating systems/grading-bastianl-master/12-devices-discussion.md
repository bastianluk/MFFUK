# Devices Discussion Points

These are the discussion points collected before the online meeting.
Written in considerable shorthand, use as you see fit.


## Graded Quiz

### L11 G1
- was about syscall(s) invoked by `printf`
- correct
    - write (obvious)
    - depending on circumstances also brk and fstat and ioctl
- discussion
    - buffering means no direct relationship between function calls and system calls

### L11 G2
- was about syscall(s) invoked by `pthread_mutex_lock`
- correct
    - futex (if waiting)
- discussion
    - does the function have to do a syscall for all lock types ?
        - there is also `pthread_spin_lock`
        - `pthread_mutex_lock` can "block"


### L11 G3
- was about syscall(s) invoked by `clock_gettime`
- correct
    - no syscall
    - or clock_gettime
- discussion
    - syscall overhead too high for reading time ?

### L11 G4
- was about MIPS interrupt service routine address
- correct
    - typically 0x80000180
    - some other addresses in exceptional circumstances
- discussion
    - NMI vs hardware interrupt
    - why MIPS manual sometimes contains 64 bit addresses when the processor is 32 bit ?
        - the (real) processor has both 32 bit and 64 bit modes
        - the manual sometimes uses shared description with sign extended values

### L11 G5
- was about causing TLB shootdown interrupts
- correct
    - must have multiple (running) threads
    - address space mapping must change
        - `mmap` and `munmap`
        - `mprotect`
        - `madvise`
        - but also first access to page
        - which may include thread stack
- discussion
    - please follow instructions ?


## Left Over Bits

### Paging

Collected questions:

    - if we had a present bit with the top level page table pointer, we could swap that too, right ?
        - yes, but we do not have that
        - really minimum gain

### Direct Memory Access

- You should very roughly understand the purpose of direct memory access
    - Processor too expensive to serve as a device to copy data blocks
    - Copying through processor doubles bus traffic
- The intricacies of bus protocols involved were left aside

Collected questions:

    - is DMA obsolete ?
    - is DMA engine part of CPU ?
    - and why are smart controllers called bus masters ?
        - some literature uses relatively narrow definition of DMA
            - DMA controller separate from device controller
            - essentially from the days of PC ISA bus
        - bus master concept needed in modern bus arbitration protocols
        - so sometimes devices that can do DMA are called bus masters
          (because they can assume that role in the bus arbitration protocol)

    - do we have to explicitly program DMA or is it somehow used automatically ?
    - is memory-to-memory DMA always available (and does it happen automatically) ?

    - is the operating system in full control of the transfers ?
    - can device overwrite useful data in memory ?
    - does DMA always use physical addresses ?

    - how can we prevent DMA attacks through devices ?
        - https://www.kernel.org/doc/html/latest/x86/intel-iommu.html
        - I/O MMU is a good idea in principle but bugs do creep in :-(

    - what if DMA access memory cached by CPU ?
        - https://stackoverflow.com/questions/54263838/how-is-dma-cache-coherency-kept-on-intel-chipsets
        - but in non Intel world many other things exist (see for example ARM network accelerators)

    - how can memory serve the processor and the devices at the same time ?
        - or multiple processors for that matter ?

    - any downside to using DMA apart from overhead ?

    - is RDMA used ?

### L11 Q5
- was about where the address for DMA comes from
- correct
    - the system has to set the address when programming the controller
    - usually done as part of programming the controller for receiving packets
- discussion
    - too late or too complicated ?
        - card asks the processor for address
        - card asks the DMA controller for address
        - card reads the address from the CPU registers
        - the address is determined during interrupt handling
    - not flexible enough
        - each device has a hardcoded address range
        - configured at initialization (vs at start of operation)
    - the memory can reside on the card itself


## Devices

- You should have a basic idea of what the interface between a device controller and the processor can look like
    - Device controllers typically expose control and status registers and data buffers
        - Writing numbers to control registers tells the device what to do
        - Reading numbers from status registers tells the system what is happening
        - Data buffers can be read and written to transport data from and to the device
    - These have addresses
        - Either the same as (physical) memory addresses (MIPS)
        - Or a separate physical address space of device addresses (Intel)
        - Some devices can mix both (registers in device address space but buffers in memory address space)
    - Device controllers can request service by raising interrupt requests
    - Device controllers can communicate (read and write) with main memory
    - Standardization of this interface minimal
        - Mostly for trivial devices (mouse)
        - Interface often not even documented

- You should understand the role of device drivers in an operating system
    - Provide abstraction layer on top of device controller interface
        - Driver interface typically the same per class of devices
        - Special functions through calls such as `ioctl`

- You should understand basic elements of typical device driver design
    - Request queues for devices where it makes sense (disk)
    - Handling of internal synchronization
        - Deferred interrupt processing
        - Threaded interrupt handling

### L12 Q1
- was about what happens when trying to lock inside an interrupt handler
- correct
    - in simplest case this leads to deadlock because interrupt handler steals processor from current thread
- discussion
    - many special mechanisms concocted over the years
        - Linux SoftIRQs and Tasklets and Work Queues
        - Windows Deferred Procedure Calls
        - Solaris Pinned Threads
    - perhaps easier to convert everything to threads ?
        - Linux threaded interrupt handling
        - but simplicity can be deceptive
    - high throughput I/O can be really complicated
        - http://kernel.dk/blk-mq.pdf
        - https://www.kernel.org/doc/html/latest/block/blk-mq.html

Collected questions:

    - should driver availability and quality determine our OS choice ?

    - is there a different driver for each device ?
        - and how are the drivers recognized and loaded ?
        - and how does the driver know device address ?
        - try `udevadm monitor --property` and insert an USB device
        - also try `systemd-hwdb query` with the `MODALIAS` value observed

    - how are the addresses assigned ?
    - how are device addresses distinguished from memory addresses ?
        - what is the advantage of having dedicated `IN` and `OUT` instructions ?
        - when should we use I/O addresses and when memory mapped I/O ?

    - is device data buffered somewhere ?
    - can we use some devices without interrupts ?
    - who sets the busy flag in the device status register ?

    - how does PCIe work ?
        - https://pcisig.com/specifications?field_technology_value[]=express
        - please ask questions that can be reasonably answered during online meeting

    - material about C++ I/O libraries ?
        - please ask David Bednarek

    - what are some unusual devices that exist in the system ?
        - just do `lspci` or `lsusb`
    - was there ever a (comically) useless register ?
        - there were certainly hardware design flaws
        - but not sure if this was what you meant

### Keyboard

- typical features
    - interrupt on key press and key release
    - provide information about keys in status registers
- discussion
    - https://stanislavs.org/helppc/8042.html
    - https://wiki.osdev.org/%228042%22_PS/2_Controller
    - can we do keyboard with polling rather than interrupts ?

### L11 Q3
- was about expected keyboard interface
- discussion
    - keycode mapping
    - multiple keys pressed
    - handling of ALT CTRL SHIFT
    - handling of sequences of keys (why ?)

### Mouse

- typical features
    - interrupt on position update
    - simple data packets with delta movement and button information
- discussion
    - does the driver display the cursor ?
    - does the driver know about the screen position ?
    - interface for telling the mouse to move and click ?
    - do interrupts happen continuously when I move the mouse ?
        - serial interfaces used to run at 1200 or 2400 bps with ~5B packets so ~50 interrupts per second

### Timers

- typical features
    - interrupt after programmable delay
    - interrupt with programmable period
    - provide clock counter value in status registers
    - provide real time clock value in status registers

### Bus Controllers

- typical features
    - enumerate devices present on the bus
        - `lspci` and `lspci -n` and `lspci -vv`
        - `lsusb` and `lsusb -t` and `lsusb -v`
    - configure devices (addresses, protocols)
    - notify about device insertions and removals (hotplug)
    - sometimes communicate with devices (when not mapped directly)
- discussion
    - does every bus need its own controller ?
        - depends on what exactly we mean by controller
            - no if we talk about "controller with features visible to software"
            - yes if we talk about "some hardware circuitry that permits the bus to function"

### Interrupt Controllers

- typical features
    - enable and disable interrupts on selected request lines (or vectors)
    - define interrupt handling details (edge vs level, routing, priorities)
    - generate interrupts to other processors
- discussion
    - often integrated in CPU
    - Intel processor manual Volume 3A Chapter 10 Advanced Programmable Interrupt Controller
    - https://software.intel.com/content/www/us/en/develop/download/intel-64-architecture-x2apic-specification.html

### Display

- typical features (basic)
    - rendering video RAM content to screen
        - can be very simple mapping (for example 3 bytes per pixel giving RGB)
        - think about size of video RAM content for typical display modes
            - full HD is 1920x1080 ~ 2M pixels
            - with 3B per pixel ~6MB RAM
            - at 25fps ~150MB/s
        - support for double buffering
    - configuring video mode (resolution, depth, rate)
    - communicating with display (see DDC and EDID)
    - acceleration
        - basic 2D such as block move (blitting)
        - advanced 3D (see NPRG019 https://cgg.mff.cuni.cz/~honza/hwgraf.html)
        - possibly other niceties such as (some standard) video stream decoding
        - GPGPU not really for displaying things anymore (see NPRG042 and NPRG058)
- discussion
    - how does the MSIM console device really light up pixels on the screen ?
    - can we have more MSIM consoles ? will it exhaust registers ?
    - is image sent from video RAM to screen explicitly ?
    - how does the CPU access the RAM on the GPU ?
    - command oriented vs bitmap ?
    - touch screen ?

### Network

- typical features (basic)
    - configure link layer (MAC address, receive filters)
    - interrupt on packets sent and received (possibly coalesced)
    - DMA for packet data transfer (possibly scatter-gather features)
    - acceleration
        - checksum computation and validation
        - TCP stream segmentation
        - multiple queues
        - `ethtool --show-offload`
        - https://is.cuni.cz/webapps/zzp/detail/200230
- discussion
    - can a network card deliver to correct buffer depending on connection ?
    - where is the boundary between hardware and software processing ?
    - port number as a register ?

## Sound

- typical features
    - configure mixer through control registers
    - stream audio data through DMA (often cyclic buffer)
    - interrupt on buffer close to full or close to empty
    - sometimes sound synthesis
    - sometimes MIDI interface
    - DRM
    - https://wiki.osdev.org/Intel_High_Definition_Audio
    - https://www.intel.com/content/dam/www/public/us/en/documents/product-specifications/high-definition-audio-specification.pdf
- discussion
    - timing and latencies on streaming path
        - sampling rate related to required audio range (often 44100Hz)
        - sample width 8 bit too small so 16 bit (sizes in between awkward for software)
        - with two channels this yields 44100*2*2 ~ 180kB/s constant stream speed
        - with 64kB buffer and interrupt at 90% empty we have ~35ms for refill
        - but ~355ms delay on audio path (humans can hear ~10ms delay)

## Printer

- typical features
    - command language to describe what to print
    - connected through standard port (USB, serial, parallel, network)

## Disk
## Lights
## Camera
## Sensors
## Bluetooth
