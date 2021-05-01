// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Test for thread_finish() function to check that this function really
 * never returns.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

#define LOOPS 5

static void* empty_worker(void* ignored) {
    thread_finish(NULL);

    ktest_assert(false, "thread survived thread_finish()");
}

void kernel_test(void) {
    ktest_start("thread/finish");

    thread_t* worker;
    ktest_thread_create_checked(&worker, empty_worker, NULL, 0, "test-worker");

    ktest_thread_join_checked(worker, NULL);

    ktest_passed();
}
