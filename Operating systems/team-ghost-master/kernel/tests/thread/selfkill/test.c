// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Test that thread can kill itself.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

static void* suicide_worker(void* ignored) {
    thread_kill(thread_get_current());

    printk("Thread survived call to thread_kill(thread_get_current()).\n");

    ktest_failed();

    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/basic");

    thread_t* worker;
    ktest_thread_create_checked(&worker, suicide_worker, NULL, 0, "suicide");

    errno_t err = thread_join(worker, NULL);
    ktest_assert(err == EKILLED, "thread_join should signal killed thread");

    ktest_passed();
}
