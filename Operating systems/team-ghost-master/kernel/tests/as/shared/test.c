// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Tests that multiple threads can share an address space.
 */

#include <debug/code.h>
#include <exc.h>
#include <ktest.h>
#include <ktest/thread.h>
#include <mm/as.h>
#include <proc/sem.h>
#include <proc/thread.h>

/*
 * We configure the tests in the following axes:
 *  - how many different AS we create
 *  - how many threads we launch per each AS
 *  - how big is the AS
 *  - how many times we check the mapping inside each thread
 *  - how many value we want to test inside AS (we use relatively
 *    high value of STEP to speed-up the test)
 */

#define TEST_AS_COUNT 5
#define TEST_AS_SIZE_PAGES 8
#define THREADS_PER_AS 5
#define STEP 1001
#define LOOPS 100

/** Information about thread that created the new AS. */
typedef struct {
    thread_t* thread;
    unative_t pattern;
} parent_info_t;

/** Children threads. */
typedef struct {
    bool reverse;
    parent_info_t* parent;
    thread_t* thread;
    size_t counter;
} child_info_t;

/*
 * Accounting of runnint threads (to display progress).
 */

static size_t running_threads = 0;

static void running_threads_inc(void) {
    bool ipl = interrupts_disable();
    running_threads++;
    interrupts_restore(ipl);
}

static void running_threads_dec(void) {
    bool ipl = interrupts_disable();
    running_threads--;
    interrupts_restore(ipl);
}

static size_t running_threads_get_count(void) {
    bool ipl = interrupts_disable();
    size_t val = running_threads;
    interrupts_restore(ipl);
    return val;
}

/*
 * Ranges of virtual memory we check and value at specific position.
 */

static uintptr_t get_data_start_addr(void) {
    return PAGE_SIZE;
}

static uintptr_t get_data_end_addr(void) {
    return PAGE_SIZE + STEP * (PAGE_SIZE * (TEST_AS_SIZE_PAGES - 1) / STEP);
}

static inline uint8_t get_value_at_addr(uintptr_t addr, unative_t pattern) {
    return (addr ^ pattern) & 0xff;
}

static void check_values(uint8_t* start, uint8_t* end, int step, unative_t pattern) {
    for (volatile uint8_t* it = start; it != end; it += step) {
        uint8_t expected = get_value_at_addr((uintptr_t)it, pattern);
        uint8_t actual = *it;
        ktest_assert(actual == expected,
                "%pT: value mismatch (at %p, pattern=%u (0x%x), actual=0x%x, expected=0x%x)",
                thread_get_current(), it, pattern, pattern, actual, expected);
        *it = expected;
    }
}

static void* worker_child(void* arg) {
    running_threads_inc();
    child_info_t* info = arg;

    dprintk("%pT [child %u/%u] running...\n", info->thread, info->parent->pattern, info->counter);

    uint8_t* data_start = (uint8_t*)get_data_start_addr();
    uint8_t* data_end = (uint8_t*)get_data_end_addr();

    for (size_t i = 0; i < LOOPS; i++) {
        ktest_assert(info != NULL, "");
        ktest_assert(info->parent != NULL, "");
        if (info->reverse) {
            check_values(data_end, data_start, -STEP, info->parent->pattern);
        } else {
            check_values(data_start, data_end, STEP, info->parent->pattern);
        }
        thread_yield();
    }

    running_threads_dec();
    return NULL;
}

static void* worker_parent(void* arg) {
    running_threads_inc();
    parent_info_t* info = arg;

    dprintk("%pT [parent %u] running at %p...\n", info->thread, info->pattern, debug_get_stack_pointer());

    uint8_t* data_start = (uint8_t*)get_data_start_addr();
    uint8_t* data_end = (uint8_t*)get_data_end_addr();

    for (volatile uint8_t* it = data_start; it <= data_end; it += STEP) {
        *it = get_value_at_addr((uintptr_t)it, info->pattern);
    }

    dprintk("%pT [parent %u] spawning children...\n", info->thread, info->pattern);

    child_info_t children[THREADS_PER_AS];
    for (size_t i = 0; i < THREADS_PER_AS; i++) {
        children[i].counter = i;
        children[i].parent = info;
        children[i].reverse = (i % 2) == 0;
        ktest_thread_create_checked(&children[i].thread, worker_child, &children[i], 0, "child");
    }

    for (size_t i = 0; i < LOOPS; i++) {
        check_values(data_start, data_end, STEP, info->pattern);
        thread_yield();
    }

    for (size_t i = 0; i < THREADS_PER_AS; i++) {
        ktest_thread_join_checked(children[i].thread, NULL);
    }

    running_threads_dec();
    return NULL;
}

void kernel_test(void) {
    ktest_start("as/shared");
    parent_info_t workers[TEST_AS_COUNT];

    printk("Will create %u different address spaces ...\n", TEST_AS_COUNT);
    for (size_t i = 0; i < TEST_AS_COUNT; i++) {
        workers[i].pattern = i;
        errno_t err = thread_create_new_as(&workers[i].thread, worker_parent, &workers[i], 0, "parent", TEST_AS_SIZE_PAGES * PAGE_SIZE);
        ktest_assert_errno(err, "thread_create");
    }

    thread_yield();

    while (true) {
        size_t running_count = running_threads_get_count();
        printk("There are still %u running worker threads (max at %u) ...\n",
                running_count, TEST_AS_COUNT * (THREADS_PER_AS + 1));
        if (running_count == 0) {
            break;
        }
        // Reduce when debugging - this keeps number of message relatively low
        for (size_t i = 0; i < 5; i++) {
            thread_yield();
        }
    }

    for (size_t i = 0; i < TEST_AS_COUNT; i++) {
        ktest_thread_join_checked(workers[i].thread, NULL);
    }

    ktest_passed();
}
