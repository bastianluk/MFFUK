// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _DRIVERS_TIMER_H_
#define _DRIVERS_TIMER_H_

#include <drivers/cp0.h>
#include <types.h>

static inline void timer_interrupt_after(unative_t cycles) {
    cp0_write_compare(cp0_read_count() + cycles);
}

#endif
