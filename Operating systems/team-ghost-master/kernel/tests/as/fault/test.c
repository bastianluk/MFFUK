// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Checks that virtual memory at address 0 is not mapped.
 */

#include <ktest.h>
#include <mm/as.h>
#include <proc/thread.h>

static volatile uint8_t blackhole;

static void* as_worker(void* arg) {
    /*
     * Using arg as the source of NULL and volatile to prevent any
     * optimizations from the compiler.
     */
    volatile uint8_t* arg_u8 = (uint8_t*)arg;

    __asm__ volatile("");

    *arg_u8 = 0xff;

    __asm__ volatile("");

    printk("Survived touching NULL.\n");
    blackhole = *arg_u8;
    ktest_failed();

    return NULL;
}

void kernel_test(void) {
    ktest_start("as/fault");

    thread_t* worker;
    errno_t err = thread_create_new_as(&worker, as_worker, NULL, 0, "worker", PAGE_SIZE * 4);
    ktest_assert_errno(err, "thread_create");
    err = thread_join(worker, NULL);
    ktest_assert(err == EKILLED, "thread_join should signal killed thread (got %s)", errno_as_str(err));

    ktest_passed();
}
