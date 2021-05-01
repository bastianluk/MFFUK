// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Check that sem_trywait returns EBUSY when semaphore is locked.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/sem.h>
#include <proc/thread.h>

static sem_t sem;

static void* try_worker(void* ignored) {
    dprintk("Will trywait on %pS\n", &sem);
    errno_t err = sem_trywait(&sem);
    ktest_assert(err == EBUSY, "expected semaphore %pS to be busy, got %s", &sem, errno_as_str(err));

    return NULL;
}

void kernel_test(void) {
    ktest_start("sem/try_busy");

    errno_t err = sem_init(&sem, 1);
    ktest_assert_errno(err, "sem_init");

    sem_wait(&sem);

    thread_t* worker;
    ktest_thread_create_checked(&worker, try_worker, NULL, 0, "worker");

    ktest_thread_join_checked(worker, NULL);

    sem_post(&sem);
    sem_destroy(&sem);

    ktest_passed();
}
