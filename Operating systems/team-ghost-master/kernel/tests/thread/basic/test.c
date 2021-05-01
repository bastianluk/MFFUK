// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic thread test. Creates one thread that calls yield() several times
 * and then terminates.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

#define LOOPS 5

static void* empty_worker(void* ignored) {
    for (int i = 0; i < LOOPS; i++) {
        thread_yield();
    }
    return NULL;
}

void kernel_test(void) {
    ktest_start("thread/basic");

    printk("Will create new thread ...\n");

    thread_t* worker;
    ktest_thread_create_checked(&worker, empty_worker, NULL, 0, "test-worker");

    printk("Thread %pT created, about to join it...\n", worker);

    ktest_thread_join_checked(worker, NULL);

    printk("Thread joined succesfully...\n");

    ktest_passed();
}
