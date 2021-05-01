// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _MAIN_H
#define _MAIN_H

#include <types.h>

void kernel_main(void);

/** Address at the kernel end
 *
 * The symbol is defined in the linker script, the actual value is provided during linking.
 * The type of the symbol (a zero-length array of bytes) is chosen to suggest that there
 * may be some bytes there but we do not know how many.
 */
extern uint8_t _kernel_end[0];

#endif
