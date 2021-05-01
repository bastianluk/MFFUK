# Devices

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday January 7 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


# Device Drivers

**Q1** The Linux kernel network device interface is defined by the `net_device` structure in the `netdevice.h` header
(accessible for example at https://elixir.bootlin.com/linux/v5.10/source/include/linux/netdevice.h). The structure
includes function pointers to operations provided by the driver of that device. What is the signature of
the function used to set the MAC address of the network device ?

**A1** `int (*ndo_set_mac_address)(struct net_device *dev, void *addr);`

**Q2** One of the device driver source files in the Linux kernel is `pci.c` from the `nouveau` driver source tree
(accessible for example at https://elixir.bootlin.com/linux/v5.10/source/drivers/gpu/drm/nouveau/nvkm/engine/device/pci.c).
The driver contains lines similar to the following:

```c
static const struct nvkm_device_pci_vendor
nvkm_device_pci_10de_05e7[] = {
        { 0x10de, 0x0595, "Tesla T10 Processor" },
        { 0x10de, 0x068f, "Tesla T10 Processor" },
        { 0x10de, 0x0697, "Tesla M1060" },
        { 0x10de, 0x0714, "Tesla M1060" },
        { 0x10de, 0x0743, "Tesla M1060" },
        {}
};

```

What are the numbers in the first two columns of the source code fragment and why is the first one always `0x10de` ?

**A2** Those are used to identify PCI devices - first one is a vendor ID (assigned to the vendor by the PCI-SIG), second one is a device ID (assigned by the vendor); The first one being the same means the listed PCI devices come from the same vendor.


# Devices

The `lspci` command can help answer some of the following questions.

**Q3** On the `lab` computer, what is the device driver name, the range of I/O port addresses used, and the interrupt request vector number of the PCI network card device ?

**A3** driver name: virtio-pci; i/o port address range: from `c040` to `c05f` (output was: starting at `c040` with size 32); interrupt request vector number: 11 (output was: pin A routed to IRQ 11 - which checks out as the IRQ 11 is usually left as an open interrupt to peripherals if I recall correctly).

**Q4** On the `lab` computer, which of the PCI devices uses the largest memory address range and what is the range ?

**A4** name of the device is: VGA compatible controller: Cirrus Logic GD 5446 (prog-if 00 [VGA controller]); and the range: from `fc000000` to `fdffffff` (output was:starting at `fc000000` with size 32M - which I took as 2 to the power of 25 or `200000` in hexa as that is the closest power of 2 to the 32000000 (33554432).

**Q5** Assume a graphic card with a very simple video RAM layout.
The resolution is 256x256 pixels, a black and white image with
a single bit per pixel, pixels arranged in rows.
When you dump the video RAM content,
you see the following values:

```
0000: 00 00 00 00 00 00 00 00 7E 00 00 00 00 00 00 00
0010: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0020: 00 00 00 00 00 00 00 00 81 00 00 00 00 00 00 00
0030: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0040: 00 00 00 00 00 00 00 00 A5 00 00 00 00 00 00 00
0050: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0060: 00 00 00 00 00 00 00 00 81 00 00 00 00 00 00 00
0070: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0080: 00 00 00 00 00 00 00 00 A5 00 00 00 00 00 00 00
0090: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00A0: 00 00 00 00 00 00 00 00 99 00 00 00 00 00 00 00
00B0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00C0: 00 00 00 00 00 00 00 00 81 00 00 00 00 00 00 00
00D0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00E0: 00 00 00 00 00 00 00 00 7E 00 00 00 00 00 00 00
00F0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
....
```

What is displayed ?

**A5** if 0~black, 1~white: ☺; the other way around: ☻. To put it into words (and not just characters), it is a smiley face in the upper left hand corner of the image/frame (I put the image I based this answer on to [grading-bastianl/other/os-12-devices-graded-a5.png](https://gitlab.mff.cuni.cz/teaching/nswi004/2020/grading-bastianl/-/blob/master/other/os-12-devices-graded-a5.png) - the presumption is that the `....` mean that the there is nothing significant and just zeros in the following memory and not that the pattern repeats over and over - otherwise there would be a stripe of smiley face "emojis" goind down the left-ish side of the image/frame)
