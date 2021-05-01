// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../tframe.h"
#include <ktest.h>
#include <mm/frame.h>
#include <types.h>

#define MIN_SIZE 1
#define START_SIZE (MIN_SIZE << 12)
#define MAX_ALLOCATIONS (1024 * 1024 * 1024 / START_SIZE) + (START_SIZE / MIN_SIZE)
#define LOOPS 4

typedef struct block block_t;
struct block {
    size_t count;
    uintptr_t phys;
    block_t* previous;
};

/*
 * Tests that frame_alloc() eventually exhausts memory, frees it back to the
 * system and tries once again. Proper allocator must allow that such
 * operation can be tried several times.
 */

static size_t exhaust_and_free(void) {
    block_t* previous_block = NULL;

    size_t allocation_size = START_SIZE;
    size_t allocation_count = 0;
    size_t allocated_frames_total = 0;
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

        dprintk("Allocated %u frames at 0x%x\n", allocation_size, phys);

        allocated_frames_total += allocation_size;
        allocation_count++;
        ktest_assert(allocation_count < MAX_ALLOCATIONS,
                "too many (%u) allocations succeeded", allocation_count);

        block_t* block = (block_t*)(phys + 0x80000000);
        block->count = allocation_size;
        block->phys = phys;
        block->previous = previous_block;
        previous_block = block;
    }

    dprintk("Freeing it back (starting at %p)\n", previous_block);

    // Free it back
    while (previous_block != NULL) {
        block_t* temp = previous_block;
        previous_block = previous_block->previous;
        errno_t err = frame_free(temp->count, temp->phys);
        ktest_assert_errno(err, "frame_free");
    }

    return allocated_frames_total;
}

void kernel_test(void) {
    ktest_start("frame/exhaust2");

    size_t last_count = exhaust_and_free();
    ktest_assert(last_count > 0, "no allocation succeeded");
    printk("Initialization: allocated %u frames.\n", last_count);
    for (int i = 0; i < LOOPS; i++) {
        size_t count = exhaust_and_free();
        printk("Loop %d: allocated %u frames.\n", i, count);
        ktest_assert(count == last_count,
                "allocation counts differ (%u => %u)", last_count, count);
    }

    ktest_passed();
}
