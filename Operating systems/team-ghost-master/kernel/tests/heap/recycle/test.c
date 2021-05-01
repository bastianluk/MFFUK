// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../theap.h"
#include <ktest.h>
#include <mm/heap.h>
#include <types.h>

#define MIN_SIZE 16
#define START_SIZE (MIN_SIZE << 12)
#define MAX_ALLOCATIONS (1024 * 1024 * 1024 / START_SIZE) + (START_SIZE / MIN_SIZE)
#define LOOPS 2

/*
 * Tests that freeing all allocated memory does not cause
 * fragmentation that would prevent subsequent allocation
 * of all memory.
 *
 * The test repeatedly allocates all available memory using
 * kmalloc() and then frees is all kfree(). A properly implemented
 * allocator must be able to repeat such an operation without
 * "losing" memory due to fragmentation.
 */

static size_t exhaust_and_free(void) {
    printk("Starting exhaustive allocation ...\n");

    uintptr_t previous_block = 0;

    size_t allocation_size = START_SIZE;
    size_t allocation_count = 0;
    while (1) {
        // Stress proper alignment a little bit ;-)
        uintptr_t* ptr = kmalloc(allocation_size - 1);
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
            return 0;
        }

        ptr[0] = previous_block;
        previous_block = (uintptr_t)ptr;
    }

    printk(" ... and returning everything back.\n", previous_block);

    // Free it back
    while (previous_block != 0) {
        uintptr_t* ptr = (uintptr_t*)previous_block;
        previous_block = ptr[0];
        kfree(ptr);
    }

    return allocation_count;
}

void kernel_test(void) {
    ktest_start("heap/recycle");

    size_t last_count = exhaust_and_free();
    ktest_assert(last_count > 0, "no allocation succeeded");
    printk("Allocated %d.\n", last_count);
    for (int i = 0; i < LOOPS; i++) {
        size_t count = exhaust_and_free();
        printk("Allocated %d blocks.\n", count);
        ktest_assert(count == last_count,
                "allocation counts differ (%u => %u)", last_count, count);
    }

    ktest_passed();
}
