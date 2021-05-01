// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Kernel bootstrap code.
 *
 * This code will be loaded at (physical) address 0.
 */

/*
 * MSIM-specific macro for halting the simulator.
 */
.macro halt
    .insn
    .word 0x28

    /*
     * Loop forever in case the above instruction is not
     * properly recognized by the simulator.
     * As a matter of fact, if the instruction is not recognized,
     * exception should have been raised but we are a bit paranoid
     * here ;-).
     */

    0:
        j 0b
        nop

.endm halt

/*
 * MSIM-specific macro for entering interactive mode.
 */
.macro msim_stop
    .insn
    .word 0x29
.endm msim_stop

/*
 * For now, dump registers and enter interactive mode on any exception.
 */
.macro announce_exception
    .insn
    .word 0x37
#ifdef MSIM_HAS_XCRV
    .insn
    .word 0x0e
#endif
    msim_stop
.endm announce_exception

/*
 * Sets the CP0 Status register to a sane value, leaving the
 * processor in kernel mode with interrupts enabled.
 *
 * Among other things, by clearing bit 22 (BEV) of the Status register
 * we instruct the CPU to use normal exception vector base (0x80000000)
 * instead of bootstrap exception vector base (0xBFC00200), and by
 * setting bits 15:8 (IM7:IM0) and bit 0 (IE) we unmask all interrupt
 * sources and enable interrupts globally.
 *
 * Note: Status register is CP0 register number 12. Details on the
 * bit layout of the register can be found in the MIPS R4000
 * Microprocessor User's Manual (page 105/135).
 *
 * Note: This macro can be only used when it is safe to use the $k0
 * register, i.e., when the code cannot be interrupted. We use it
 * after CPU reset when bit 2 (ERL) of the Status register is set,
 * which prevents interrupts from occurring (hence it is safe).
 */
.macro set_status_register
    la $k0, 0xff01
    mtc0 $k0, $12
    nop
.endm set_status_register



/*
 * We know what we are doing here so do not let
 * the assembler change our code in any way.
 */
.set noreorder

/*
 * Put the hardwired code to a special input section ".excvec".
 * This section is then placed by the linker (as instructed by the kernel
 * linker script) at the very beginning of the kernel image (offset 0).
 */
.section .excvec, "ax"


/*
 * TLB Refill Exception handler entry point.
 *
 * This is a dedicated entry point for servicing the common-case
 * TLB Refill exceptions. Because the space for exception handling
 * code is limited here, we only jump to the actual handler located
 * elsewhere.
 *
 * Detailed description of the TLB Refill exception handling can be
 * found in the MIPS R4000 Microprocessor User's Manual (page 129/159).
 */

.globl handle_exception_general_asm

.org 0x000
.ent exception_tlb_refill
exception_tlb_refill:
    j handle_tlb_refill_asm
    nop
.end exception_tlb_refill


/*
 * Debugging-only exception handling.
 *
 * We just dump the CPU registers and enter the interactive
 * mode of the simulator.
 */

.org 0x100
.ent exception_cache_error
exception_cache_error:
    announce_exception
.end exception_cache_error


/*
 * General Exception handler entry point
 * (also called common interrupt vector).
 *
 * Most exceptions are serviced through this handler, including
 * interrupts, which have the lowest priority. Because the space
 * for exception handling code is limited here, we only jump to
 * the actual handler located elsewhere.
 *
 * Detailed description of exception handling can be found in the
 * MIPS R4000 Microprocessor User's Manual (page 119/149).
 */

.globl handle_exception_general_asm

.org 0x180
.ent exception_general
exception_general:
    j handle_exception_general_asm
    nop
.end exception_general


/*
 * Kernel entry point.
 *
 * The bootstrap loader (0x1FC00000) jumps here.
 * We only jump to the C code from here.
 */
.org 0x400
.ent start
start:
    la $sp, 0x80000400
    la $gp, 0x80000000
    set_status_register
    jal kernel_main
    nop

    /*
     * Again, paranoid style: this is unreachable anyway.
     */
    halt
.end start

/*
 * Rest of code.
 */
.text
