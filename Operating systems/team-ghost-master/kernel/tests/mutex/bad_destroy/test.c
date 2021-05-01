// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Try to destroy a locked mutex, expecting panic.
 */

#include <ktest.h>
#include <proc/mutex.h>

void kernel_test(void) {
    ktest_start("mutex/bad_destroy");
    ktest_expect_panic();

    mutex_t mutex;
    errno_t err = mutex_init(&mutex);
    ktest_assert_errno(err, "mutex_init");

    mutex_lock(&mutex);

    dprintk("About to destroy %pM\n", &mutex);
    mutex_destroy(&mutex);

    ktest_failed();
}
