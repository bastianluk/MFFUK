// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Try to destroy a semaphore with waiting threads.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/sem.h>
#include <proc/thread.h>

sem_t sem;
volatile bool worker_started = false;

static void* locking_worker(void* ignored) {
    worker_started = true;
    sem_wait(&sem);

    ktest_assert(false, "unreachable code");
    return NULL;
}

void kernel_test(void) {
    ktest_start("sem/bad_destroy");
    ktest_expect_panic();

    errno_t err = sem_init(&sem, 1);
    ktest_assert_errno(err, "sem_init");

    sem_wait(&sem);

    thread_t* worker;
    ktest_thread_create_checked(&worker, locking_worker, NULL, 0, "test-worker");

    while (!worker_started) {
        thread_yield();
    }

    dprintk("About to destroy %pS\n", &sem);
    sem_destroy(&sem);

    ktest_failed();
}
