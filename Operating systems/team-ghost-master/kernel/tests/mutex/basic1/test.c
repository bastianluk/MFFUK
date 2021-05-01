// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic mutex test. Repeatedly locks and unlocks a single mutex.
 */

#include <ktest.h>
#include <proc/mutex.h>

#define LOOPS 5000

void kernel_test(void) {
    ktest_start("mutex/basic1");

    mutex_t mutex;
    errno_t err = mutex_init(&mutex);
    ktest_assert_errno(err, "mutex_init");

    for (int i = 0; i < LOOPS; i++) {
        mutex_lock(&mutex);
        mutex_unlock(&mutex);
    }

    mutex_destroy(&mutex);

    ktest_passed();
}
