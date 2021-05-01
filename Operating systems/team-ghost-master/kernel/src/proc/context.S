// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <proc/context.h>

#define cp0_status 12

.set CP0_STATUS_IE_MASK, 0x00000001

.text
.set noreorder
.set nomacro

/*
 * void cpu_switch_context(
 *     context_t * a0_this_context,
 *     context_t * a1_next_context
 * )
 *
 * Switches processor to another context. The first argument points to
 * "this" context, i.e., the context_t structure where the current state
 * (general purpose registers, and some special register) of the CPU will
 * be stored.
 *
 * The second argument points to the "next" context, from which the
 * state of the CPU will be restored to resume execution.
 */

.globl cpu_switch_context
.ent cpu_switch_context

cpu_switch_context:

    /*
     * Save the general-purpose registers and the $hi and $lo
     * registers into the context_t structure pointed to by $a0.
     */

    SAVE_REGISTERS $a0

    mfhi $t0
    mflo $t1
    sw $t0, CONTEXT_HI_OFFSET($a0)
    sw $t1, CONTEXT_LO_OFFSET($a0)

    /*
     * Save the CP0 Status register and disable interrupts (by
     * clearing the IE bit of the Status register), which will
     * allow us to use the $k0 and $k1 registers without having
     * to worry about being interrupted (and the content of the
     * $k0 and $k1 registers being destroyed) while restoring
     * the target context (pointed to by $a1).
     */

    mfc0 $t0, $cp0_status
    sw $t0, CONTEXT_CP0_STATUS_OFFSET($a0)
    la $t1, ~CP0_STATUS_IE_MASK
    and $t0, $t1
    mtc0 $t0, $cp0_status

    /*
     * Load the $hi and $lo registers and the general purpose registers
     * from the target context. This also switches to another stack!
     *
     * Note that we use $k0 as the base register when restoring the
     * general purpose registers, because the $a1 register will be
     * loaded from the target context (and stop pointing to it).
     */

    lw $t0, CONTEXT_HI_OFFSET($a1)
    lw $t1, CONTEXT_LO_OFFSET($a1)
    mthi $t0
    mtlo $t1

    move $k0, $a1
    LOAD_REGISTERS $k0

    /*
     * Prepare to load the CP0 Status register of the target context.
     * The Status register is actually loaded in the branch delay slot
     * of the jump that returns control to the new thread.
     *
     * Setting the Status register in the branch delay slot makes it
     * possible to return from kernel mode to user mode. Setting the
     * register sooner would mean switching from kernel mode to user
     * mode while executing in KSEG0, which is not allowed.
     *
     * A somewhat cleaner alternative to this particular method of
     * returning from kernel mode to user mode is the ERET instruction.
     *
     * Note that setting the Status register to the value from the
     * target context will enable interrupts (if they were enabled).
     */

    lw $k1, CONTEXT_CP0_STATUS_OFFSET($k0)
    j $ra
    mtc0 $k1, $cp0_status

.end cpu_switch_context
