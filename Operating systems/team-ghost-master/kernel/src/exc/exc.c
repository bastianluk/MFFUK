// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <exc.h>
#include <drivers/machine.h>
#include <drivers/timer.h>
#include <lib/print.h>
#include <proc/scheduler.h>
#include <proc/thread.h>

/** Handles general exception.
 *
 * The function receives a pointer to an exception context, which
 * represents a snapshot of CPU and (some) CP0 registers at the
 * time of the exception occurring.
 */
void handle_exception_general(eh_context_t* eh_context) {
    bool is_timer_interrupt = cp0_cause_is_interrupt_pending(eh_context->status, 7);
    if (is_timer_interrupt)
    {
        thread_yield();
        timer_interrupt_after(KERNEL_SCHEDULER_QUANTUM);
        return;
    }

    machine_dump_registers();
    machine_enter_debugger();
}

/** Disables interrupts (globally) and returns the previous state.
 *
 * Saves and returns the value of bit 0 (IE) of the CP0 Status
 * register prior to clearing it (and thus disabling interrupts).
 */
bool interrupts_disable(void) {
    unative_t status = cp0_read(REG_CP0_STATUS);
    cp0_write(REG_CP0_STATUS, status & ~CP0_STATUS_IE_BIT);
    return (status & CP0_STATUS_IE_BIT) > 0;
}

/** Restores the (global) interrupt-enable state.
 *
 * Sets bit 0 (IE) of the CP0 Status register to the given value.
 * Typically used with the return value of interrupts_disable()
 * to perform a complementary operation.
 */
void interrupts_restore(bool enable) {
    unative_t status = cp0_read(REG_CP0_STATUS);
    if (enable) {
        status = status | CP0_STATUS_IE_BIT;
    } else {
        status = status & ~CP0_STATUS_IE_BIT;
    }
    cp0_write(REG_CP0_STATUS, status);
}
