// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../tframe.h"
#include <ktest.h>
#include <mm/frame.h>
#include <types.h>

#define MIN_SIZE 1
#define START_SIZE (MIN_SIZE << 12)
#define MAX_ALLOCATIONS (1024 * 1024 * 1024 / START_SIZE) + (START_SIZE / MIN_SIZE)

/*
 * Tests that frame_alloc() eventually exhausts memory.
 */

void kernel_test(void) {
    ktest_start("frame/exhaust1");

    size_t allocation_count = 0;
    size_t allocation_size = START_SIZE;
    while (1) {
        uintptr_t phys;
        errno_t err = frame_alloc(allocation_size, &phys);
        if (err == ENOMEM) {
            dprintk("Failed to allocate %u frames...\n", allocation_size);
            allocation_size = allocation_size / 2;
            if (allocation_size < MIN_SIZE) {
                break;
            }
            continue;
        }
        ktest_assert_errno(err, "frame_alloc");
        ktest_check_frame_alloc_result(allocation_size, phys);

        printk("Allocated %u frames at 0x%x\n", allocation_size, phys);

        allocation_count++;
        ktest_assert(allocation_count < MAX_ALLOCATIONS,
                "too many (%u) allocations succeeded", allocation_count);
    }

    ktest_assert(allocation_count > 0, "at least one allocation has to succeed");

    ktest_passed();
}
