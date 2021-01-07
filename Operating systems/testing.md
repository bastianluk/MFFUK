00:00.0 Host bridge: Intel Corporation 440FX - 82441FX PMC [Natoma] (rev 02)
        Subsystem: Red Hat, Inc. Qemu virtual machine
        Control: I/O+ Mem+ BusMaster- SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-

00:01.0 ISA bridge: Intel Corporation 82371SB PIIX3 ISA [Natoma/Triton II]
        Subsystem: Red Hat, Inc. Qemu virtual machine
        Control: I/O+ Mem+ BusMaster- SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=medium >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-

00:01.1 IDE interface: Intel Corporation 82371SB PIIX3 IDE [Natoma/Triton II] (prog-if 80 [ISA Compatibility mode-only controller, supports bus mastering])
        Subsystem: Red Hat, Inc. Qemu virtual machine
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B+ ParErr- DEVSEL=medium >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Region 0: Memory at 000001f0 (32-bit, non-prefetchable) [virtual] [size=8]
        Region 1: Memory at 000003f0 (type 3, non-prefetchable) [virtual]
        Region 2: Memory at 00000170 (32-bit, non-prefetchable) [virtual] [size=8]
        Region 3: Memory at 00000370 (type 3, non-prefetchable) [virtual]
        Region 4: I/O ports at c0e0 [virtual] [size=16]
        Kernel driver in use: ata_piix
        Kernel modules: pata_acpi, ata_generic

00:01.3 Bridge: Intel Corporation 82371AB/EB/MB PIIX4 ACPI (rev 03)
        Subsystem: Red Hat, Inc. Qemu virtual machine
        Control: I/O+ Mem+ BusMaster- SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B+ ParErr- DEVSEL=medium >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Interrupt: pin A routed to IRQ 9
        Kernel driver in use: piix4_smbus
        Kernel modules: i2c_piix4

00:02.0 VGA compatible controller: Cirrus Logic GD 5446 (prog-if 00 [VGA controller])
        Subsystem: Red Hat, Inc. QEMU Virtual Machine
        Control: I/O+ Mem+ BusMaster- SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Region 0: Memory at fc000000 (32-bit, prefetchable) [size=32M]
        Region 1: Memory at febd0000 (32-bit, non-prefetchable) [size=4K]
        Expansion ROM at 000c0000 [disabled] [size=128K]
        Kernel driver in use: cirrus
        Kernel modules: cirrus

00:03.0 Ethernet controller: Red Hat, Inc. Virtio network device
        Subsystem: Red Hat, Inc. Device 0001
        Physical Slot: 3
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx+
        Status: Cap+ 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin A routed to IRQ 11
        Region 0: I/O ports at c040 [size=32]
        Region 1: Memory at febd1000 (32-bit, non-prefetchable) [size=4K]
        Region 4: Memory at fe000000 (64-bit, prefetchable) [size=16K]
        Expansion ROM at feb80000 [disabled] [size=256K]
        Capabilities: <access denied>
        Kernel driver in use: virtio-pci

00:04.0 USB controller: Intel Corporation 82801I (ICH9 Family) USB UHCI Controller #1 (rev 03) (prog-if 00 [UHCI])
        Subsystem: Red Hat, Inc. QEMU Virtual Machine
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin A routed to IRQ 11
        Region 4: I/O ports at c060 [size=32]
        Kernel driver in use: uhci_hcd

00:04.1 USB controller: Intel Corporation 82801I (ICH9 Family) USB UHCI Controller #2 (rev 03) (prog-if 00 [UHCI])
        Subsystem: Red Hat, Inc. QEMU Virtual Machine
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin B routed to IRQ 10
        Region 4: I/O ports at c080 [size=32]
        Kernel driver in use: uhci_hcd

00:04.2 USB controller: Intel Corporation 82801I (ICH9 Family) USB UHCI Controller #3 (rev 03) (prog-if 00 [UHCI])
        Subsystem: Red Hat, Inc. QEMU Virtual Machine
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin C routed to IRQ 10
        Region 4: I/O ports at c0a0 [size=32]
        Kernel driver in use: uhci_hcd

00:04.7 USB controller: Intel Corporation 82801I (ICH9 Family) USB2 EHCI Controller #1 (rev 03) (prog-if 20 [EHCI])
        Subsystem: Red Hat, Inc. QEMU Virtual Machine
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap- 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0, Cache Line Size: 64 bytes
        Interrupt: pin D routed to IRQ 11
        Region 0: Memory at febd2000 (32-bit, non-prefetchable) [size=4K]
        Kernel driver in use: ehci-pci

00:05.0 SCSI storage controller: Red Hat, Inc. Virtio block device
        Subsystem: Red Hat, Inc. Device 0002
        Physical Slot: 5
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx+
        Status: Cap+ 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin A routed to IRQ 10
        Region 0: I/O ports at c000 [size=64]
        Region 1: Memory at febd3000 (32-bit, non-prefetchable) [size=4K]
        Region 4: Memory at fe004000 (64-bit, prefetchable) [size=16K]
        Capabilities: <access denied>
        Kernel driver in use: virtio-pci

00:06.0 Unclassified device [00ff]: Red Hat, Inc. Virtio memory balloon
        Subsystem: Red Hat, Inc. Device 0005
        Physical Slot: 6
        Control: I/O+ Mem+ BusMaster+ SpecCycle- MemWINV- VGASnoop- ParErr- Stepping- SERR+ FastB2B- DisINTx-
        Status: Cap+ 66MHz- UDF- FastB2B- ParErr- DEVSEL=fast >TAbort- <TAbort- <MAbort- >SERR- <PERR- INTx-
        Latency: 0
        Interrupt: pin A routed to IRQ 10
        Region 0: I/O ports at c0c0 [size=32]
        Region 4: Memory at fe008000 (64-bit, prefetchable) [size=16K]
        Capabilities: <access denied>
        Kernel driver in use: virtio-pci
