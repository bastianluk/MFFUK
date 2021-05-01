// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _TESTS_HEAP_THEAH_H
#define _TESTS_HEAP_THEAH_H

#include <main.h>

#define ktest_check_kmalloc_result(addr_ptr, size) \
    do { \
        uintptr_t __addr = (uintptr_t)addr_ptr; \
        uintptr_t __addr_end_of_kernel_image = (uintptr_t)&_kernel_end; \
        unsigned long long __addr_end = __addr + (size); \
        ktest_assert((__addr >= 0x80000000) && (__addr < 0xA0000000), \
                #addr_ptr " (0x%x) not in kernel segment [0x80000000, 0xA0000000)", __addr); \
        ktest_assert((__addr_end >= 0x80000000) && (__addr_end < 0xA0000000), \
                #addr_ptr " + " #size " (0x%x + %dB) not in kernel segment [0x80000000, 0xA0000000)", __addr, (size)); \
        ktest_assert((__addr % 4) == 0, "bad alignment on 0x%x", __addr); \
        ktest_assert(__addr >= __addr_end_of_kernel_image, \
                #addr_ptr " (0x%x) is inside kernel image (ends at 0x%x)", \
                __addr, __addr_end_of_kernel_image); \
    } while (0)

#define ktest_check_kmalloc_writable(addr_ptr) \
    do { \
        volatile uint8_t* __ptr = (uint8_t*)addr_ptr; \
        __ptr[0] = 0xAA; \
        __ptr[1] = 0x55; \
        ktest_assert(__ptr[0] == 0xAA, "expected 0xAA, got 0x%x", __ptr[0]); \
        ktest_assert(__ptr[1] == 0x55, "expected 0x55, got 0x%x", __ptr[1]); \
    } while (0)

#endif
