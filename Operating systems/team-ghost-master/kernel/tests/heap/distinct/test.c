// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../theap.h"
#include <ktest.h>
#include <mm/heap.h>
#include <types.h>

/*
 * Tests that the allocator does not return overlapping blocks.
 *
 * The test is rather slow, which is why the suite only contains
 * test instances for small memory sizes. Feel free to run the
 * test with more memory during development.
 */

#define ALLOCATION_MAX_COUNT 256
#define MAX_ALLOCATION_FAILURES 8

// Static, automatically filled with zeros.
static uint32_t* allocations[ALLOCATION_MAX_COUNT];

static inline size_t get_elements_at_index(size_t elements) {
    static const size_t sizes[] = { 311, 412, 201, 197, 517 };
    static const size_t sizes_count = sizeof(sizes) / sizeof(sizes[0]);
    return sizes[elements % sizes_count];
}

static inline uint32_t get_pattern_at_index(size_t index) {
    // Do not return 0x00 or 0xFF
    uint8_t b = (index % 254) + 1;
    return (b << 24) | (b << 16) | (b << 8) | b;
}

static uint32_t* allocate_and_fill(size_t elements, uint32_t pattern) {
    size_t size = elements * sizeof(uint32_t);
    uint32_t* ptr = kmalloc(size);
    if (ptr == NULL) {
        return NULL;
    }
    ktest_check_kmalloc_result(ptr, size);
    for (size_t i = 0; i < elements; i++) {
        ptr[i] = pattern;
    }

    return ptr;
}

void kernel_test(void) {
    ktest_start("heap/distinct");

    size_t failed_allocations = 0;
    size_t attempted_allocations = 0;
    size_t index = 0;
    while (failed_allocations < MAX_ALLOCATION_FAILURES) {
        index = (index + 1) % ALLOCATION_MAX_COUNT;

        if (index == 0) {
            printk("Done next iteration ...\n");
        }

        attempted_allocations++;
        allocations[index] = allocate_and_fill(get_elements_at_index(index), get_pattern_at_index(index));
        if (allocations[index] == NULL) {
            failed_allocations++;
        }

        // Check that allocations array was not overwritten.
        for (size_t i = 0; i < ALLOCATION_MAX_COUNT; i++) {
            uint32_t* data = allocations[i];
            if (data == NULL) {
                continue;
            }
            size_t size = get_elements_at_index(i);
            size_t pattern = get_pattern_at_index(i);
            for (size_t j = 0; j < size; j++) {
                ktest_assert(data[j] == pattern, "block %u[%uB], index %u, expected 0x%x, got 0x%x",
                        i, size, j, pattern, data[j]);
            }
        }
    }

    ktest_assert(attempted_allocations > failed_allocations, "all attempted allocations failed");

    ktest_passed();
}
