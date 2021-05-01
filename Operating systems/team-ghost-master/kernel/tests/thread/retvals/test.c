// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Test for checking that thread return values are correctly retrieved
 * when the thread is joined. Both "return" and thread_finish() is tested.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

static int ret_one, ret_two, ret_three;

static void* worker_one(void* ignored) {
    return &ret_one;
}

static void* worker_two(void* ignored) {
    return &ret_two;
}

static void* worker_three(void* ignored) {
    thread_finish(&ret_three);
}

void kernel_test(void) {
    ktest_start("thread/retvals");

    thread_t* one;
    ktest_thread_create_checked(&one, worker_one, NULL, 0, "test_one");

    thread_t* two;
    ktest_thread_create_checked(&two, worker_two, NULL, 0, "test_two");

    thread_t* three;
    ktest_thread_create_checked(&three, worker_three, NULL, 0, "test_three");

    void* retval;
    ktest_thread_join_checked(one, &retval);
    ktest_assert(retval == &ret_one, "worker_one returned invalid value");

    ktest_thread_join_checked(two, &retval);
    ktest_assert(retval == &ret_two, "worker_two returned invalid value");

    ktest_thread_join_checked(three, &retval);
    ktest_assert(retval == &ret_three, "worker_three returned invalid value");

    ktest_passed();
}
