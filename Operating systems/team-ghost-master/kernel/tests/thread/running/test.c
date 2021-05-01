// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic test that thread code is actually executed.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

static volatile bool marker_one = false;
static volatile bool marker_two = false;
static volatile bool marker_three = false;

static void* worker_one(void* ignored) {
    marker_one = true;
    return NULL;
}

static void* worker_two(void* ignored) {
    marker_two = true;
    return NULL;
}

static void* worker_three(void* ignored) {
    marker_three = true;
    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/running");

    thread_t* one;
    ktest_thread_create_checked(&one, worker_one, NULL, 0, "test_one");

    thread_t* two;
    ktest_thread_create_checked(&two, worker_two, NULL, 0, "test_two");

    thread_t* three;
    ktest_thread_create_checked(&three, worker_three, NULL, 0, "test_three");

    ktest_thread_join_checked(one, NULL);
    ktest_thread_join_checked(two, NULL);
    ktest_thread_join_checked(three, NULL);

    // Check that the workers have actually run
    ktest_assert(marker_one, "worker one was not executed");
    ktest_assert(marker_two, "worker two was not executed");
    ktest_assert(marker_three, "worker three was not executed");

    ktest_passed();
}
