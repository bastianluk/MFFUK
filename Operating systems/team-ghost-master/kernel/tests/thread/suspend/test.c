// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic test that checks that thread_suspend() does not return immediatelly.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

#define SAFETY_LOOPS 10

static volatile bool marker_one = false;
static volatile bool marker_two = false;
static volatile bool marker_three = false;

static thread_t* suspended_thread = NULL;
static volatile bool marker = false;
static volatile bool terminate_idle = false;

static void* worker_suspend(void* ignored) {
    marker = true;
    thread_suspend();
    ktest_assert(!marker, "marker should be false (set from different thread)");
    return NULL;
}

static void* worker_wake_up(void* ignored) {
    while (!marker) {
        thread_yield();
    }

    /*
     * Now, worker_suspend should be already suspended.
     * But let's wait a bit to be sure.
     */
    for (int i = 0; i < SAFETY_LOOPS; i++) {
        thread_yield();
    }

    marker = false;
    errno_t err = thread_wakeup(suspended_thread);
    ktest_assert_errno(err, "thread_wakeup(suspended_thread)");

    return NULL;
}

static void* worker_idle(void* ignored) {
    while (!terminate_idle) {
        thread_yield();
    }
    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/suspend");

    ktest_thread_create_checked(&suspended_thread, worker_suspend, NULL, 0, "test_suspend");

    thread_t* wake_upper;
    ktest_thread_create_checked(&wake_upper, worker_wake_up, NULL, 0, "test_wake_up");

    thread_t* idle;
    ktest_thread_create_checked(&idle, worker_idle, NULL, 0, "test_idle");

    ktest_thread_join_checked(suspended_thread, NULL);
    ktest_thread_join_checked(wake_upper, NULL);

    terminate_idle = true;
    ktest_thread_join_checked(idle, NULL);

    ktest_passed();
}
