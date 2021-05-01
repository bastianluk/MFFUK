// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _EXC_H
#define _EXC_H

/*
 * This file is shared between C and assembler sources, because we
 * want to keep information on low-level structures in the same file.
 * We use ifdefs to include the portions relevant to each context.
 */

#include <proc/context.h>

/** Size of the eh_context_t structure below, needed in assembler code. */
#define EH_CONTEXT_SIZE CONTEXT_SIZE

#ifndef __ASSEMBLER__

/*
 * To keep things simple, the eh_context_t type is an alias for
 * context_t, but that does mean they can be used interchangeably.
 * Specifically, a pointer to eh_context_t SHOULD NOT be passed to
 * the cpu_switch_context() function.
 *
 * Even though the alias makes both types structurally equivalent,
 * it should be considered a mere coincidence and an implementation
 * detail. For example, the exception context could be made smaller,
 * because the exception handler could rely on the C compiler to
 * save the callee-saved registers.
 *
 * More importantly, eh_context_t is stored on the stack (which may
 * be in TLB-mapped memory and accessing it could trigger TLB-related
 * exceptions), while context_t should be part of a thread structure
 * and is expected to be stored in unmapped memory.
 */
typedef context_t eh_context_t;

void handle_exception_general(eh_context_t* eh_context);

bool interrupts_disable(void);
void interrupts_restore(bool enable);

#endif

#ifdef __ASSEMBLER__
// clang-format off

/*
 * Note that the code below currently relies on eh_context_t being an
 * alias for context_t. This is a trade-off between proper design and
 * simplicity, trying to avoid duplicated code.
 */

.macro EH_SAVE_REGISTERS base
    SAVE_REGISTERS \base
.endm EH_SAVE_REGISTERS

.macro EH_LOAD_REGISTERS base
    LOAD_REGISTERS \base
.endm EH_LOAD_REGISTERS


/*
 * The following macros save/restore critical CP0 registers to/from
 * a given memory location.
 *
 * The \scratch_base parameter is the name of a register holding the
 * address of a memory location reserved for the CP0 registers (large
 * enough to hold 4 register values).
 *
 * The \temp parameter is the name of a register to use for moving
 * data between CPU and CP0.
 */
.set SCRATCH_AREA_SIZE, 4*4

.set SCRATCH_CP0_STATUS_OFFSET, 0*4
.set SCRATCH_CP0_CAUSE_OFFSET, 1*4
.set SCRATCH_CP0_EPC_OFFSET, 2*4
.set SCRATCH_CP0_BADVADDR_OFFSET, 3*4


.macro EH_SAVE_CP0_REGISTERS scratch_base temp
.set push
.set nomacro

    mfc0 \temp, $REG_CP0_STATUS
    sw \temp, SCRATCH_CP0_STATUS_OFFSET(\scratch_base)

    mfc0 \temp, $REG_CP0_CAUSE
    sw \temp, SCRATCH_CP0_CAUSE_OFFSET(\scratch_base)

    mfc0 \temp, $REG_CP0_EPC
    sw \temp, SCRATCH_CP0_EPC_OFFSET(\scratch_base)

    mfc0 \temp, $REG_CP0_BADVADDR
    sw \temp, SCRATCH_CP0_BADVADDR_OFFSET(\scratch_base)

.set pop
.endm EH_SAVE_CP0_REGISTERS


.macro EH_LOAD_CP0_REGISTERS scratch_base temp
.set push
.set nomacro

    // No need to restore CP0 BadVAddr register

    lw \temp, SCRATCH_CP0_EPC_OFFSET(\scratch_base)
    mtc0 \temp, $REG_CP0_EPC

    // No need to restore CP0 Cause register

    lw \temp, SCRATCH_CP0_STATUS_OFFSET(\scratch_base)
    mtc0 \temp, $REG_CP0_STATUS

.set pop
.endm EH_LOAD_CP0_REGISTERS


/*
 * The following macros copy values between a memory area holding the
 * values of critical CP0 registers and the eh_context_t structure.
 *
 * The \scratch_base parameter is the name of the register holding
 * the address of the memory area with critical CP0 registers.
 *
 * The \eh_context_base parameter is the name of the register holding
 * the address of the eh_context_t structure.
 *
 * The \temp parameter is the name of the register for temporaries.
 */
.macro COPY_CP0_REGISTERS_TO_EH_CONTEXT scratch_base eh_context_base temp
.set push
.set nomacro

    lw \temp, SCRATCH_CP0_STATUS_OFFSET(\scratch_base)
    sw \temp, CONTEXT_CP0_STATUS_OFFSET(\eh_context_base)

    lw \temp, SCRATCH_CP0_CAUSE_OFFSET(\scratch_base)
    sw \temp, CONTEXT_CP0_CAUSE_OFFSET(\eh_context_base)

    lw \temp, SCRATCH_CP0_EPC_OFFSET(\scratch_base)
    sw \temp, CONTEXT_CP0_EPC_OFFSET(\eh_context_base)

    lw \temp, SCRATCH_CP0_BADVADDR_OFFSET(\scratch_base)
    sw \temp, CONTEXT_CP0_BADVADDR_OFFSET(\eh_context_base)

.set pop
.endm COPY_CP0_REGISTERS_TO_EH_CONTEXT


.macro COPY_CP0_REGISTERS_FROM_EH_CONTEXT eh_context_base scratch_base temp
.set push
.set nomacro

    lw \temp, CONTEXT_CP0_STATUS_OFFSET(\eh_context_base)
    sw \temp, SCRATCH_CP0_STATUS_OFFSET(\scratch_base)

    lw \temp, CONTEXT_CP0_CAUSE_OFFSET(\eh_context_base)
    sw \temp, SCRATCH_CP0_CAUSE_OFFSET(\scratch_base)

    lw \temp, CONTEXT_CP0_EPC_OFFSET(\eh_context_base)
    sw \temp, SCRATCH_CP0_EPC_OFFSET(\scratch_base)

    lw \temp, CONTEXT_CP0_BADVADDR_OFFSET(\eh_context_base)
    sw \temp, SCRATCH_CP0_BADVADDR_OFFSET(\scratch_base)

.set pop
.endm COPY_CP0_REGISTERS_FROM_EH_CONTEXT

// clang-format on
#endif

#endif
