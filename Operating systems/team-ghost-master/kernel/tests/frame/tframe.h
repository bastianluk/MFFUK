// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _TESTS_FRAME_TFRAME_H
#define _TESTS_FRAME_TFRAME_H

#define ktest_check_frame_alloc_result(count, addr) \
    do { \
        uintptr_t __addr = (uintptr_t)addr; \
        unsigned long long __addr_end = __addr + (count)*FRAME_SIZE; \
        ktest_assert(__addr < 0x20000000, \
                #addr " (0x%x) not in [0x00000000, 0x20000000)", __addr); \
        ktest_assert(__addr_end < 0x20000000, \
                #addr " + " #count " (0x%x + %d frames) not in [0x00000000, 0x20000000)", __addr, (count)); \
        ktest_assert((__addr % FRAME_SIZE) == 0, "bad alignment on 0x%x", __addr); \
    } while (0)

#endif
