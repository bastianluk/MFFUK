// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <proc/context.h>
#include <drivers/cp0.h>
#include <exc.h>

.bss

/*
 * Scratch area for CP0 registers that need to be saved on entry
 * to the general exception handler. These will be copied to the
 * eh_context_t structure allocated later on the stack.
 */
.align 4
geh_cp0_state:
    .space SCRATCH_AREA_SIZE

/*
 * Scratch area for CP0 registers that need to be saved on entry
 * to the TLB refill exception handler. These will be copied to
 * the eh_context_t structure allocated later on the stack.
 */
.align 4
teh_cp0_state:
    .space SCRATCH_AREA_SIZE


.text
.set noreorder

/*
 * General exception handler.
 *
 * When the CPU reaches this code, bit 1 (EXL) of the CP0 Status
 * register will be set, the CP0 Cause register (#13) will indicate
 * the cause of the exception, and the CP0 EPC register (#14) will
 * contain the value of the PC register at which execution should
 * resume (unless the exception occurred in a branch delay slot).
 *
 * Because EXL=1, the CPU is running in kernel mode with interrupts
 * disabled, and because this code is in unmapped memory (KSEG0), it
 * should not trigger TLB-related exceptions -- unless it touches
 * a TLB-mapped address (KUSEG and others).
 *
 * The exception handler needs to save the CPU state of the code
 * interrupted by the exception, and the simplest approach is to
 * save the CPU context on the stack of the currently executing
 * thread (even though it is not necessarily the best approach).
 * When storing CPU context on the stack, we need to take into
 * account the location of the stack in the address space.
 *
 * When the thread stack is unmapped memory (KSEG0), accessing it will
 * not trigger TLB-exceptions, but if we run out of stack space, we
 * will not be able to detect it (before we overwrite something).
 *
 * When the thread stack is located in mapped memory, we will be able
 * to detect running out of stack space, but we will also need to take
 * into account that accessing the stack will trigger TLB-related
 * exceptions that need to be handled.
 *
 * The following entry point code assumes that the stack may be placed
 * in mapped memory, which means that we cannot just start storing CPU
 * context on the thread stack. If we did and a TLB-related exception
 * occurred, the exception would overwrite critical CP0 registers that
 * identify the exception we were processing, along with the address
 * of the instruction we should resume execution after handling it.
 *
 * The same thing would happen if an interrupt occured before we saved
 * the critical CP0 registers. This particular situation is prevented
 * by the CPU -- when an exception occurs, the CPU sets the EXL bit in
 * the CP0 Status register, which disables interrupts (i.e., the EXL
 * bit overrides the global interrupt-enable state).
 *
 * Consequently, in the exception handler, we first save critical CP0
 * registers in a small, fixed-size block of unmapped memory. Then we
 * switch the CPU from exception level to plain kernel mode, and at
 * the same time, we explicitly disable interrupts (leaving exception
 * level processing would stop blocking them).
 *
 * Then we store CPU registers on the stack and copy the CP0 registers
 * from the scratch area to the stack as well. If a TLB-related
 * exception occurred while accessing the thread stack, it would be
 * handled by the TLB refill handler.
 *
 * After storing the CPU exception context on the stack, we jump into
 * C code implementing the actual exception handling. After handling
 * the exception, we repeat similar steps in reverse order and resume
 * execution of the interrupted code.
 */
.globl handle_exception_general_asm

.ent handle_exception_general_asm
handle_exception_general_asm:
    /*
     * We first save important CP0 registers ($epc, $cause, $badvaddr,
     * and $status) to a fixed location in unmapped memory (KSEG0) so
     * that accessing it cannot cause a TLB-related exception.
     */
    la $k1, geh_cp0_state
    EH_SAVE_CP0_REGISTERS $k1 $k0

    /*
     * Next we switch from exception level to plain kernel mode by
     * clearing bit 1 (EXL) and bits 4:3 (KSU) of the CP0 Status
     * register, while simultaneously disabling interrupts by
     * clearing bit 0 (IE).
     */
    la $k1, ~(CP0_STATUS_KSU_MASK | CP0_STATUS_EXL_BIT | CP0_STATUS_IE_BIT)
    mfc0 $k0, $REG_CP0_STATUS
    and $k0, $k1
    mtc0 $k0, $REG_CP0_STATUS

    /*
     * Next we allocate space for a CPU context structure on the stack,
     * and save the CPU registers into it. We also copy the saved CP0
     * registers from the fixed memory location to the stack.
     *
     * This code can trigger a TLB refill exception, hence the need to
     * save the critical CP0 registers elsewhere. We also need to stop
     * using the kernel scratch registers ($k0 and $k1).
     */
    subu $sp, EH_CONTEXT_SIZE
    EH_SAVE_REGISTERS $sp

    la $t1, geh_cp0_state
    COPY_CP0_REGISTERS_TO_EH_CONTEXT $t1 $sp $t0

    /*
     * Once the CPU context is stored on the stack, we could re-enable
     * interrupts to make the handler re-entrant. For simplicity, we
     * keep interrupts disabled (re-enabling them would also require
     * a bit more complex epilogue in this handler).
     *
     * We jump to C code to actually handle the exception.
     *
     * Because the C function expects a pointer to the exception CPU
     * context as a parameter, we pass it in register $a0.
     *
     * Note that the MIPS ABI requires the caller to reserve stack
     * space (to be used by the callee) for all parameters passed in
     * registers. For simplicity, here we reserve a fixed amount that
     * is sufficient for all situations (and clean up the stack after
     * the function returns).
     */
    move $a0, $sp
    jal handle_exception_general
    subu $sp, ABI_STACK_FRAME

    addiu $sp, ABI_STACK_FRAME

    /*
     * After the C code returns, we restore the registers from the
     * stack and resume execution of the interrupted code.
     *
     * Again, accessing the stack could cause a TLB refill exception,
     * hence a two-phase restore, similar to the entry to this handler.
     *
     * We first copy the CP0 registers to the fixed memory location and
     * then restore the CPU registers from the exception context. Note
     * that we cannot use the kernel scratch registers (yet) because a
     * TLB refill exception would destroy their content.
     *
     * No exception should occur when restoring the CP0 registers from
     * the fixed memory location, so we use the scratch registers to
     * restore the CP0 Status and EPC registers.
     *
     * Note that even though restoring the value of the Status register
     * could re-enable interrupts, recall that we are restoring a value
     * from the entry to this handler, at which point bit 1 (EXL) was
     * set, blocking interrupts from occurring.
     */
    la $t1, geh_cp0_state
    COPY_CP0_REGISTERS_FROM_EH_CONTEXT $sp $t1 $t0

    EH_LOAD_REGISTERS $sp
    addiu $sp, EH_CONTEXT_SIZE

    la $k1, geh_cp0_state
    EH_LOAD_CP0_REGISTERS $k1 $k0

    /*
     * Finally, we use the ERET exception to return from the exception
     * handler, which clears bit 1 (EXL) in the CP0 Status register
     * (stops blocking interrupts) and resume resumes execution from
     * the address in EPC.
     *
     * Note that the return can happen after a relatively long time,
     * because a timer interrupt may cause a thread to be rescheduled.
     * Hence the context could be switched several times before we
     * return here.
     */
    eret

.end handle_exception_general_asm


/*
 * TLB refill exception handler.
 *
 * This handler is the same as the general exception handler, but
 * it jumps into a different C function to handle the exception.
 *
 * Please note that this handler is only called for non-nested TLB
 * Refill exception, i.e., when the exception is triggered in normal
 * execution mode (EXL=0). Other TLB exceptions (and TLB Refill
 * triggered with EXL=1) go to the general exception handler.
 */
.globl handle_tlb_refill_asm

.ent handle_tlb_refill_asm
handle_tlb_refill_asm:
    /*
     * We first save important CP0 registers ($epc, $cause, $badvaddr,
     * and $status) to a fixed location in unmapped memory (KSEG0) so
     * that accessing it cannot cause a TLB-related exception.
     */
    la $k1, teh_cp0_state
    EH_SAVE_CP0_REGISTERS $k1 $k0

    /*
     * Next we switch from exception level to plain kernel mode by
     * clearing bit 1 (EXL) and bits 4:3 (KSU) of the CP0 Status
     * register, while simultaneously disabling interrupts by
     * clearing bit 0 (IE).
     */
    la $k1, ~(CP0_STATUS_KSU_MASK | CP0_STATUS_EXL_BIT | CP0_STATUS_IE_BIT)
    mfc0 $k0, $REG_CP0_STATUS
    and $k0, $k1
    mtc0 $k0, $REG_CP0_STATUS

    /*
     * Next we allocate space for a CPU context structure on the stack,
     * and save the CPU registers into it. We also copy the saved CP0
     * registers from the fixed memory location to the stack.
     *
     * This code can trigger a TLB refill exception, hence the need to
     * save the critical CP0 registers elsewhere. We also need to stop
     * using the kernel scratch registers ($k0 and $k1).
     */
    subu $sp, EH_CONTEXT_SIZE
    EH_SAVE_REGISTERS $sp

    la $t1, teh_cp0_state
    COPY_CP0_REGISTERS_TO_EH_CONTEXT $t1 $sp $t0

    /*
     * Once the CPU context is stored on the stack, we could re-enable
     * interrupts to make the handler re-entrant. For simplicity, we
     * keep interrupts disabled (re-enabling them would also require
     * a bit more complex epilogue in this handler).
     *
     * We jump to C code to actually handle the exception.
     *
     * Because the C function expects a pointer to the exception CPU
     * context as a parameter, we pass it in register $a0.
     *
     * Note that the MIPS ABI requires the caller to reserve stack
     * space (to be used by the callee) for all parameters passed in
     * registers. For simplicity, here we reserve a fixed amount that
     * is sufficient for all situations (and clean up the stack after
     * the function returns).
     */
    move $a0, $sp
    jal handle_tlb_refill
    subu $sp, ABI_STACK_FRAME

    addiu $sp, ABI_STACK_FRAME

    /*
     * After the C code returns, we restore the registers from the
     * stack and resume execution of the interrupted code.
     *
     * Again, accessing the stack could cause a TLB refill exception,
     * hence a two-phase restore, similar to the entry to this handler.
     *
     * We first copy the CP0 registers to the fixed memory location and
     * then restore the CPU registers from the exception context. Note
     * that we cannot use the kernel scratch registers (yet) because a
     * TLB refill exception would destroy their content.
     *
     * No exception should occur when restoring the CP0 registers from
     * the fixed memory location, so we use the scratch registers to
     * restore the CP0 Status and EPC registers.
     *
     * Note that even though restoring the value of the Status register
     * could re-enable interrupts, recall that we are restoring a value
     * from the entry to this handler, at which point bit 1 (EXL) was
     * set, blocking interrupts from occurring.
     */
    la $t1, teh_cp0_state
    COPY_CP0_REGISTERS_FROM_EH_CONTEXT $sp $t1 $t0

    EH_LOAD_REGISTERS $sp
    addiu $sp, EH_CONTEXT_SIZE

    la $k1, teh_cp0_state
    EH_LOAD_CP0_REGISTERS $k1 $k0

    /*
     * Finally, we use the ERET exception to return from the exception
     * handler, which clears bit 1 (EXL) in the CP0 Status register
     * (stops blocking interrupts) and resume resumes execution from
     * the address in EPC.
     */
    eret
.end handle_tlb_refill_asm
