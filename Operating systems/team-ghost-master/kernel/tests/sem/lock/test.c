// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic semaphore test where semaphore is used just like a mutex.
 * Repeatedly locks and unlocks a semaphore from different threads.
 * The semaphore protects a shared variable that should not be broken
 * by repeated updates.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/sem.h>
#include <proc/thread.h>

#define LOOPS 200
#define WAIT_LOOPS 5
#define THREAD_COUNT 20

static size_t thread_count;
static thread_t* threads[THREAD_COUNT];

static volatile size_t total_counter = 0;
static sem_t total_counter_guard;

static void* worker(void* ignored) {
    for (int i = 0; i < LOOPS; i++) {
        sem_wait(&total_counter_guard);
        int temp = total_counter;
        for (int j = 0; j < WAIT_LOOPS; j++) {
            thread_yield();
        }
        total_counter = temp + 1;
        sem_post(&total_counter_guard);
    }

    return NULL;
}

void kernel_test(void) {
    ktest_start("sem/lock");

    errno_t err = sem_init(&total_counter_guard, 1);
    ktest_assert_errno(err, "sem_init");

    for (thread_count = 0; thread_count < THREAD_COUNT; thread_count++) {
        err = thread_create(&threads[thread_count], worker, NULL, 0, "worker");
        if (err == ENOMEM) {
            break;
        }
        ktest_assert_errno(err, "thread_create");
    }

    for (size_t i = 0; i < thread_count; i++) {
        ktest_thread_join_checked(threads[i], NULL);
    }

    ktest_assert(total_counter == LOOPS * thread_count, "total_counter is broken");

    ktest_passed();
}
