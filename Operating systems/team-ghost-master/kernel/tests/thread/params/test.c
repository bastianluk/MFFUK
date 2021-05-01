// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Test for checking that thread parameters are passed correctly into
 * the threads.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

static int one, two, three;
static void* param_one = &one;
static void* param_two = &two;
static void* param_three = &three;

static void* worker_one(void* param) {
    ktest_assert(param == param_one, "worker_one parameter passed incorrectly");
    return NULL;
}

static void* worker_two(void* param) {
    ktest_assert(param == param_two, "worker_two parameter passed incorrectly");
    return NULL;
}

static void* worker_three(void* param) {
    ktest_assert(param == param_three, "worker_three parameter passed incorrectly");
    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/params");

    thread_t* one;
    ktest_thread_create_checked(&one, worker_one, param_one, 0, "test_one");

    thread_t* two;
    ktest_thread_create_checked(&two, worker_two, param_two, 0, "test_two");

    thread_t* three;
    ktest_thread_create_checked(&three, worker_three, param_three, 0, "test_three");

    ktest_thread_join_checked(one, NULL);
    ktest_thread_join_checked(two, NULL);
    ktest_thread_join_checked(three, NULL);

    ktest_passed();
}
