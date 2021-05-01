// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * System bootstrap code.
 *
 * When R4000 is started, it starts executing code at virtual address
 * 0xBFC00000 (maps to physical address 0x1FC00000).
 * Note that this code is in KSEG1 which allows the kernel to prepare
 * address translation etc. before it is needed.
 *
 * This code only jumps to kernel located at physical address 0x400
 * where rest of the code is stored.
 * Note that kernel code can actually be stored here but it is much simpler
 * to jump to fixed address lower in the memory and not worry whether we
 * do not use memory outside of KSEG1.
 */

.text

/*
 * We know what we are doing here so do not let
 * assembler change our code in any way.
 */
.set noreorder

.ent __start

__start:
    /*
     * We only jump to the real kernel code.
     *
     * Note that LA is a macro instruction provided by the assembler which
     * loads a 32-bit constant into a register using two instructions with
     * 16-bit immediate operands. This is because MIPS R4000 does not have
     * a separate instruction for loading a 32-bit constant (which comes
     * from having 32-bit fixed-size instruction words).
     *
     * Also note that we explicitly add NOP to the branch delay slot
     * to prevent any unexpected behaviour.
     */
    la $ra, 0x80000400
    j $ra
    nop

.end __start
