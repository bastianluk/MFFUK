// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Check that mutex_trylock returns EBUSY when mutex is locked.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/mutex.h>
#include <proc/thread.h>

static mutex_t mutex;

static void* try_worker(void* ignored) {
    dprintk("Will trywait on %pM\n", &mutex);
    errno_t err = mutex_trylock(&mutex);
    ktest_assert(err == EBUSY, "expected mutex %pM to be busy, got %s", &mutex, errno_as_str(err));

    return NULL;
}

void kernel_test(void) {
    ktest_start("mutex/try_busy");

    errno_t err = mutex_init(&mutex);
    ktest_assert_errno(err, "mutex_init");

    mutex_lock(&mutex);

    thread_t* worker;
    ktest_thread_create_checked(&worker, try_worker, NULL, 0, "worker");

    ktest_thread_join_checked(worker, NULL);

    mutex_unlock(&mutex);
    mutex_destroy(&mutex);

    ktest_passed();
}
