// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _DEBUG_CODE_H
#define _DEBUG_CODE_H

#include <lib/print.h>
#include <types.h>

/** Get current value of stack pointer. */
static inline uintptr_t debug_get_stack_pointer(void) {
    register uintptr_t sp __asm__("sp");
    return sp;
}

void debug_dump_function(const char* name, uintptr_t address, size_t instruction_count);

#endif
