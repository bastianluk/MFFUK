// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Tests that AS management and TLB refill works even on address spaces
 * that cannot fit into TLB at once.
 */

#include <ktest.h>
#include <mm/as.h>
#include <mm/tlb.h>
#include <proc/thread.h>

/*
 * Number of pages.
 *
 * Number of entries is multiplied by two because of odd/even
 * mappings, the second multiplication is to actually exceed the size.
 */
#define AS_SIZE_PAGES (TLB_ENTRY_COUNT * 2 * 2)

/*
 * Do not touch every byte to speed-up the test a bit.
 */
#define STEP 1001

#define DOT_STEP 16

static inline uint8_t get_value_at_addr(uintptr_t addr) {
    return ((addr / PAGE_SIZE) ^ (addr & 0xff)) & 0xff;
}

static inline void print_progress(size_t step) {
    if ((step % DOT_STEP) == 0) {
        printk(".");
    }
}

static void* as_worker(void* ignored) {
    /*
     * Need to skip adress 0x0000 that is
     * not mapped.
     */
    uint8_t* data = (uint8_t*)(PAGE_SIZE);
    uint8_t* data_end = (uint8_t*)(PAGE_SIZE * (AS_SIZE_PAGES - 1));

    printk("Writing values  ");

    for (uint8_t* it = data; it < data_end; it += STEP) {
        *it = get_value_at_addr((uintptr_t)it);
        print_progress((data - it) / STEP);
    }

    printk(" done.\nReading it back ");

    for (uint8_t* it = data; it < data_end; it += STEP) {
        uint8_t expected = get_value_at_addr((uintptr_t)it);
        uint8_t actual = *it;
        ktest_assert(expected == actual, "value mismatch");
        print_progress((data - it) / STEP);
    }

    printk(" ok.\n");

    return NULL;
}

void kernel_test(void) {
    ktest_start("as/large");

    thread_t* worker;
    errno_t err = thread_create_new_as(&worker, as_worker, NULL, 0, "test-as-large", AS_SIZE_PAGES * PAGE_SIZE);
    ktest_assert_errno(err, "thread_create");

    err = thread_join(worker, NULL);
    ktest_assert_errno(err, "thread_join");

    ktest_passed();
}
