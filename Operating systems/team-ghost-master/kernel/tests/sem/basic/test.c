// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Basic semaphore test. Repeatedly locks and unlocks a single semaphore.
 */

#include <ktest.h>
#include <proc/sem.h>

#define LOOPS 500
#define INIT_VALUE_MAX 5

void kernel_test(void) {
    ktest_start("sem/basic");

    for (int init_value = 1; init_value <= INIT_VALUE_MAX; init_value++) {
        printk("Testing semaphore with initial value %d ...\n", init_value);

        sem_t sem;
        errno_t err = sem_init(&sem, init_value);
        ktest_assert_errno(err, "sem_init");

        dprintk("Testing sem %pS with init_value of %d.\n", &sem, init_value);
        for (int i = 0; i < LOOPS; i++) {
            for (int j = 0; j < init_value; j++) {
                sem_wait(&sem);
            }
            for (int j = 0; j < init_value; j++) {
                sem_post(&sem);
            }
        }

        ktest_assert(sem_get_value(&sem) == init_value, "semaphore not in initial state");

        sem_destroy(&sem);
    }

    ktest_passed();
}
