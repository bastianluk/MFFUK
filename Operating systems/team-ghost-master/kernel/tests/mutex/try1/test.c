// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic mutex try-lock test. Repeatedly locks and unlocks a single mutex.
 */

#include <ktest.h>
#include <proc/mutex.h>

#define LOOPS 5000

void kernel_test(void) {
    ktest_start("mutex/try1");

    mutex_t mutex;
    errno_t err = mutex_init(&mutex);
    ktest_assert_errno(err, "mutex_init");
    dprintk("Initialized mutex %pM.\n", &mutex);

    for (int i = 0; i < LOOPS; i++) {
        err = mutex_trylock(&mutex);
        ktest_assert_errno(err, "mutex_try_lock");
        dprintk("Mutex %pM locked after trylock.\n", &mutex);
        mutex_unlock(&mutex);
    }

    mutex_destroy(&mutex);

    ktest_passed();
}
