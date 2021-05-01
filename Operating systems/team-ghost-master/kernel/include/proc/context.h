// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _PROC_CONTEXT_H
#define _PROC_CONTEXT_H

/*
 * This file is shared between C and assembler sources, because we
 * want to keep information on low-level structures in the same file.
 * We use ifdefs to include the portions relevant to each context.
 */

/** Minimal stack frame size according to MIPS o32 ABI. */
#define ABI_STACK_FRAME 32

/** Size of the context_t structure below, needed in assembler code. */
#define CONTEXT_SIZE (38 * 4)

#ifndef __ASSEMBLER__

#include <types.h>

/**
 * CPU context (registers).
 *
 * The general purpose registers are stored in slots corresponding
 * to their register number.
 *
 * Other registers are stored in subsequent slots.
 *
 * Note that for simplicity, we waste a bit of space for
 * registers that do not need saving ($zero, $k0, and $k1).
 */
typedef struct {
    // 0
    unative_t zero;

    // 1
    unative_t at;

    // 2..3
    unative_t v0;
    unative_t v1;

    // 4..7
    unative_t a0;
    unative_t a1;
    unative_t a2;
    unative_t a3;

    // 8..15
    unative_t t0;
    unative_t t1;
    unative_t t2;
    unative_t t3;
    unative_t t4;
    unative_t t5;
    unative_t t6;
    unative_t t7;

    // 16..23
    unative_t s0;
    unative_t s1;
    unative_t s2;
    unative_t s3;
    unative_t s4;
    unative_t s5;
    unative_t s6;
    unative_t s7;

    // 24..25
    unative_t t8;
    unative_t t9;

    // 26..27
    unative_t k0;
    unative_t k1;

    // 28..31
    unative_t gp;
    unative_t sp;
    unative_t fp;
    unative_t ra;

    // 32..33
    unative_t lo;
    unative_t hi;

    // 34
    unative_t status;

    // 35..36
    unative_t cause;
    unative_t epc;

    // 37..38
    unative_t badvaddr;
    unative_t entryhi;
} context_t;

void cpu_switch_context(context_t* this_context, context_t* next_context);

#endif

#ifdef __ASSEMBLER__
// clang-format off

/*
 * The offsets for GP registers must match the context_t structure, the
 * layout is chosen so that the offsets correspond to register numbers.
 */

.set CONTEXT_ZERO_OFFSET, 0*4

.set CONTEXT_AT_OFFSET, 1*4

.set CONTEXT_V0_OFFSET, 2*4
.set CONTEXT_V1_OFFSET, 3*4

.set CONTEXT_A0_OFFSET, 4*4
.set CONTEXT_A1_OFFSET, 5*4
.set CONTEXT_A2_OFFSET, 6*4
.set CONTEXT_A3_OFFSET, 7*4

.set CONTEXT_T0_OFFSET, 8*4
.set CONTEXT_T1_OFFSET, 9*4
.set CONTEXT_T2_OFFSET, 10*4
.set CONTEXT_T3_OFFSET, 11*4
.set CONTEXT_T4_OFFSET, 12*4
.set CONTEXT_T5_OFFSET, 13*4
.set CONTEXT_T6_OFFSET, 14*4
.set CONTEXT_T7_OFFSET, 15*4

.set CONTEXT_S0_OFFSET, 16*4
.set CONTEXT_S1_OFFSET, 17*4
.set CONTEXT_S2_OFFSET, 18*4
.set CONTEXT_S3_OFFSET, 19*4
.set CONTEXT_S4_OFFSET, 20*4
.set CONTEXT_S5_OFFSET, 21*4
.set CONTEXT_S6_OFFSET, 22*4
.set CONTEXT_S7_OFFSET, 23*4

.set CONTEXT_T8_OFFSET, 24*4
.set CONTEXT_T9_OFFSET, 25*4

.set CONTEXT_K0_OFFSET, 26*4
.set CONTEXT_K1_OFFSET, 27*4

.set CONTEXT_GP_OFFSET, 28*4
.set CONTEXT_SP_OFFSET, 29*4
.set CONTEXT_FP_OFFSET, 30*4
.set CONTEXT_RA_OFFSET, 31*4

/*
 * The offsets for special registers must match the context_t structure.
 * In contrast to GP registers, the layout is irregular, because not all
 * special registers need saving so these are added as needed.
 */

.set CONTEXT_LO_OFFSET, 32*4
.set CONTEXT_HI_OFFSET, 33*4

.set CONTEXT_CP0_STATUS_OFFSET, 34*4

.set CONTEXT_CP0_CAUSE_OFFSET, 35*4
.set CONTEXT_CP0_EPC_OFFSET, 36*4

.set CONTEXT_CP0_BADVADDR_OFFSET, 37*4
.set CONTEXT_CP0_ENTRYHI_OFFSET, 38*4


/*
 * The SAVE_REGISTERS macro stores general purpose registers
 * into the context_t structure. The macro can be used with
 * any general purpose register (no registers are destroyed
 * when this code runs).
 *
 * The assembler directives in the macro turn off warnings
 * when using the $at register and also tell the assembler
 * to avoid macro instructions and instruction reordering.
 */
.macro SAVE_REGISTERS base
.set push
.set noat
.set noreorder
.set nomacro

    // No need to save $zero register

    sw $at, CONTEXT_AT_OFFSET(\base)

    sw $v0, CONTEXT_V0_OFFSET(\base)
    sw $v1, CONTEXT_V1_OFFSET(\base)

    sw $a0, CONTEXT_A0_OFFSET(\base)
    sw $a1, CONTEXT_A1_OFFSET(\base)
    sw $a2, CONTEXT_A2_OFFSET(\base)
    sw $a3, CONTEXT_A3_OFFSET(\base)

    sw $t0, CONTEXT_T0_OFFSET(\base)
    sw $t1, CONTEXT_T1_OFFSET(\base)
    sw $t2, CONTEXT_T2_OFFSET(\base)
    sw $t3, CONTEXT_T3_OFFSET(\base)
    sw $t4, CONTEXT_T4_OFFSET(\base)
    sw $t5, CONTEXT_T5_OFFSET(\base)
    sw $t6, CONTEXT_T6_OFFSET(\base)
    sw $t7, CONTEXT_T7_OFFSET(\base)
    sw $t8, CONTEXT_T8_OFFSET(\base)
    sw $t9, CONTEXT_T9_OFFSET(\base)

    sw $s0, CONTEXT_S0_OFFSET(\base)
    sw $s1, CONTEXT_S1_OFFSET(\base)
    sw $s2, CONTEXT_S2_OFFSET(\base)
    sw $s3, CONTEXT_S3_OFFSET(\base)
    sw $s4, CONTEXT_S4_OFFSET(\base)
    sw $s5, CONTEXT_S5_OFFSET(\base)
    sw $s6, CONTEXT_S6_OFFSET(\base)
    sw $s7, CONTEXT_S7_OFFSET(\base)

    // No need to save $k0 and $k1 registers

    sw $gp, CONTEXT_GP_OFFSET(\base)
    sw $sp, CONTEXT_SP_OFFSET(\base)
    sw $fp, CONTEXT_FP_OFFSET(\base)
    sw $ra, CONTEXT_RA_OFFSET(\base)

.set pop
.endm SAVE_REGISTERS

/*
 * The LOAD_REGISTERS macro loads general purpose registers from the
 * context_t structure. The macro should ONLY be used with the $k0,
 * $k1, or $sp registers as the \base parameter. Other registers will
 * be destroyed during restoration. The $sp register is an exception,
 * because it is restored last (on purpose).
 *
 * When using the $k0 and $k1 registers, the interrupts MUST be
 * disabled to ensure that the code cannot be interrupted (and the
 * contents of the $k0 and $k1 registers destroyed).
 *
 * Keep in mind that the macro restores the stack pointer, i.e., it
 * will switch to another stack.
 */
.macro LOAD_REGISTERS base
.set push
.set noat
.set noreorder
.set nomacro

    lw $ra, CONTEXT_RA_OFFSET(\base)
    lw $fp, CONTEXT_FP_OFFSET(\base)

    // Restore $sp as the very last register

    lw $gp, CONTEXT_GP_OFFSET(\base)

    // Must not restore $k0 and $k1 registers

    lw $s7, CONTEXT_S7_OFFSET(\base)
    lw $s6, CONTEXT_S6_OFFSET(\base)
    lw $s5, CONTEXT_S5_OFFSET(\base)
    lw $s4, CONTEXT_S4_OFFSET(\base)
    lw $s3, CONTEXT_S3_OFFSET(\base)
    lw $s2, CONTEXT_S2_OFFSET(\base)
    lw $s1, CONTEXT_S1_OFFSET(\base)
    lw $s0, CONTEXT_S0_OFFSET(\base)

    lw $t9, CONTEXT_T9_OFFSET(\base)
    lw $t8, CONTEXT_T8_OFFSET(\base)
    lw $t7, CONTEXT_T7_OFFSET(\base)
    lw $t6, CONTEXT_T6_OFFSET(\base)
    lw $t5, CONTEXT_T5_OFFSET(\base)
    lw $t4, CONTEXT_T4_OFFSET(\base)
    lw $t3, CONTEXT_T3_OFFSET(\base)
    lw $t2, CONTEXT_T2_OFFSET(\base)
    lw $t1, CONTEXT_T1_OFFSET(\base)
    lw $t0, CONTEXT_T0_OFFSET(\base)

    lw $a3, CONTEXT_A3_OFFSET(\base)
    lw $a2, CONTEXT_A2_OFFSET(\base)
    lw $a1, CONTEXT_A1_OFFSET(\base)
    lw $a0, CONTEXT_A0_OFFSET(\base)

    lw $v1, CONTEXT_V1_OFFSET(\base)
    lw $v0, CONTEXT_V0_OFFSET(\base)

    lw $at, CONTEXT_AT_OFFSET(\base)

    // No need to restore $zero register

    lw $sp, CONTEXT_SP_OFFSET(\base)

.set pop
.endm LOAD_REGISTERS

// clang-format on
#endif

#endif
