// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../theap.h"
#include <ktest.h>
#include <mm/heap.h>
#include <types.h>

#define MIN_SIZE 8
#define START_SIZE (MIN_SIZE << 12)
#define MAX_ALLOCATIONS (1024 * 1024 * 1024 / START_SIZE) + (START_SIZE / MIN_SIZE)

/*
 * Tests that kmalloc() eventually exhausts memory.
 */

void kernel_test(void) {
    ktest_start("heap/exhaust");

    size_t allocation_count = 0;
    size_t allocation_size = START_SIZE;
    while (1) {
        // Stress proper alignment a little bit ;-)
        void* ptr = kmalloc(allocation_size - 1);
        dprintk("kmalloc(%u) = %pA\n", allocation_size - 1, ptr);
        if (ptr == NULL) {
            allocation_size = allocation_size / 2;
            if (allocation_size < MIN_SIZE) {
                break;
            }
            continue;
        }

        ktest_check_kmalloc_result(ptr, allocation_size - 1);
        ktest_check_kmalloc_writable(ptr);

        allocation_count++;

        if (allocation_count > MAX_ALLOCATIONS) {
            printk("Too many (%d) allocations succeeded.\n", allocation_count);
            ktest_failed();
            // Unreachable
            return;
        }
    }

    ktest_assert(allocation_count > 0, "no memory on heap");

    ktest_passed();
}
