// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic virtual memory mapping test.
 *
 * Create threads with new AS and increasing size, check that it is
 * possible to access all memory and that accessing beyond that memory
 * kills the thread.
 */

#include <ktest.h>
#include <mm/as.h>
#include <proc/thread.h>

/*
 * Try up to MAX_PAGES but fail if we cannot reach MIN_PAGES (i.e. it is
 * okay if we fail on ENOMEM on big address spaces).
 */
#define MAX_PAGES 50
#define MIN_PAGES 10

/*
 * Do not touch every byte to speed-up the test a bit.
 */
#define STEP 253

static volatile bool accessible_memory_ok;

static inline uint8_t get_value_at_addr(uintptr_t addr) {
    return ((addr / PAGE_SIZE) ^ (addr & 0xff)) & 0xff;
}

static void* as_worker(void* unused) {
    thread_t* self = thread_get_current();
    as_t* as = thread_get_as(self);

    size_t size = as_get_size(as);
    size_t size_pages = size / PAGE_SIZE;
    ktest_assert(size_pages > 0, "AS size must be > 0");

    /*
     * Need to skip adress 0x0000 that is
     * not mapped.
     */
    uint8_t* data = (uint8_t*)(PAGE_SIZE);
    uint8_t* data_end = (uint8_t*)(size);

    printk("%u pages: checking range [%p, %p)\n", size_pages, data, data_end);

    for (uint8_t* it = data; it < data_end; it += STEP) {
        *it = get_value_at_addr((uintptr_t)it);
    }

    for (uint8_t* it = data; it < data_end; it += STEP) {
        uint8_t expected = get_value_at_addr((uintptr_t)it);
        uint8_t actual = *it;
        ktest_assert(expected == actual, "value mismatch");
    }

    accessible_memory_ok = true;

    /*
     * Enforce that instructions will not be re-ordered across this place.
     */
    __asm__ volatile("");

    /*
     * Cause fault at next page.
     */
    *data_end = 0xff;

    printk("%u: Survived touching unmapped memory.\n", size_pages);
    ktest_failed();

    return NULL;
}

void kernel_test(void) {
    ktest_start("as/basic");

    for (size_t i = 2; i < MAX_PAGES; i++) {
        accessible_memory_ok = false;
        thread_t* worker;
        errno_t err = thread_create_new_as(&worker, as_worker, NULL, 0, "worker", i * PAGE_SIZE);
        if ((err == ENOMEM) && (i > MIN_PAGES)) {
            break;
        }
        ktest_assert_errno(err, "thread_create");
        err = thread_join(worker, NULL);
        ktest_assert(err == EKILLED, "thread_join should signal killed thread (got %s)", errno_as_str(err));

        ktest_assert(accessible_memory_ok, "thread killed when touching mapped memory");
    }

    ktest_passed();
}
