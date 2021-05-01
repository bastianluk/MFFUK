// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Trival test that the scheduler is fair. We run two threads where each
 * increments its own counter, yielding after each increment. We expect
 * that both counters would contain similar values.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

#define LOOPS 20

static volatile bool terminate_one = false;
static volatile bool terminate_two = false;
static volatile int counter_one = 0;
static volatile int counter_two = 0;

static void* worker_one(void* ignored) {
    while (!terminate_one) {
        counter_one++;
        thread_yield();
    }

    return NULL;
}

static void* worker_two(void* ignored) {
    while (!terminate_two) {
        counter_two++;
        thread_yield();
    }

    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/fairness");

    thread_t* thread_one;
    ktest_thread_create_checked(&thread_one, worker_one, NULL, 0, "test_one");

    thread_t* thread_two;
    ktest_thread_create_checked(&thread_two, worker_two, NULL, 0, "test_two");

    for (int i = 0; i < LOOPS; i++) {
        thread_yield();
    }

    terminate_one = true;
    terminate_two = true;

    ktest_thread_join_checked(thread_one, NULL);
    ktest_thread_join_checked(thread_two, NULL);

    ktest_assert_in_range("thread_one counter", counter_one, LOOPS / 2, LOOPS * 2);
    ktest_assert_in_range("thread_two counter", counter_two, LOOPS / 2, LOOPS * 2);

    ktest_passed();
}
