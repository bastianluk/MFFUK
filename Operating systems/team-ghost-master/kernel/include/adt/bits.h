// SPDX-License-Identifier: Apache-2.0
// Copyright 2020 Charles University

#ifndef _ADT_BITS_H
#define _ADT_BITS_H

#include <types.h>

static inline bool is_power_of_two(unsigned long x) {
    /*
     * Using a trick here: if x is power of 2, then
     * it has no common bits with (x-1) [one bit set
     * vs. all lower bits set].
     *
     * And we consider 0 as a special case.
     */
    return (x != 0) && !(x & (x - 1));
}

#endif
