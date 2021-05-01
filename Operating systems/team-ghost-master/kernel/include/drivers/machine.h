// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _DRIVERS_MACHINE_H
#define _DRIVERS_MACHINE_H

/** Halts the (virtual) machine. */
static inline void machine_halt(void) {
    __asm__ volatile(".word 0x28\n");
}

/** Enter MSIM interactive mode. */
static inline void machine_enter_debugger(void) {
    __asm__ volatile(".word 0x29\n");
}

/** Dump content of registers to the screen. */
static inline void machine_dump_registers(void) {
    __asm__ volatile(".word 0x37\n");
}

#endif
