// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Tests that different AS are separated from each other.
 */

#include <drivers/machine.h>
#include <exc.h>
#include <ktest.h>
#include <mm/as.h>
#include <proc/sem.h>
#include <proc/thread.h>

/*
 * Increase to 260 if you recycle ASIDs of active address
 * spaces ;-)
 */
#ifndef TEST_AS_COUNT
#define TEST_AS_COUNT 20
#endif

#define LOOPS 100

typedef struct {
    thread_t* thread;
    unative_t pattern;
} worker_info_t;

static worker_info_t worker_info[TEST_AS_COUNT];

// To monitor the progress, protected by disabled interrupts
static size_t global_counter = 0;

// Announce one sub-job was finished
static void progressbar_step(void) {
    //printk(".");

    bool ipl = interrupts_disable();
    global_counter++;
    interrupts_restore(ipl);
}

// Also returns whether all is done
static bool progressbar_print(void) {
    bool ipl = interrupts_disable();
    size_t counter = global_counter;
    interrupts_restore(ipl);

    printk("Waiting for workers to finish (%u (%d%%) done)...\n",
            counter, 100 * counter / (TEST_AS_COUNT * LOOPS));
    return counter == TEST_AS_COUNT * LOOPS;
}

// Lock to announce that worker has started
static sem_t started_worker_counter;
// Unlock to start all the workers at once
static sem_t worker_barrier;

static void* as_worker(void* info_arg) {
    worker_info_t* info = info_arg;
    dprintk("Worker %pT (%u) started...\n", thread_get_current(), info->pattern);

    sem_wait(&started_worker_counter);
    sem_wait(&worker_barrier);

    volatile unative_t* data = (unative_t*)(PAGE_SIZE);

    unative_t i = 0;
    do {
        unative_t expected_pattern = info->pattern ^ i;
        *data = expected_pattern;

        thread_yield();

        ktest_assert(*data == expected_pattern,
                "%pT: value mismatch (base_pattern=0x%x, i=0x%x, actual=0x%x, expected=0x%x)",
                info->thread, info->pattern, i, *data, expected_pattern);

        i++;
        progressbar_step();
    } while (i < LOOPS);

    return NULL;
}

void kernel_test(void) {
    ktest_start("as/asids");

    errno_t err = sem_init(&started_worker_counter, TEST_AS_COUNT);
    ktest_assert_errno(err, "sem_init");
    err = sem_init(&worker_barrier, 0);
    ktest_assert_errno(err, "sem_init");

    printk("Will create %u different address spaces ...\n", TEST_AS_COUNT);
    for (size_t i = 0; i < TEST_AS_COUNT; i++) {
        worker_info[i].pattern = i;
        err = thread_create_new_as(&worker_info[i].thread, as_worker, &worker_info[i], 0, "worker", PAGE_SIZE * 2);
        ktest_assert_errno(err, "thread_create");
    }

    printk("Waiting for workers to actually start ...\n");

    while (sem_trywait(&started_worker_counter) != EBUSY) {
        sem_post(&started_worker_counter);
    }

    for (size_t i = 0; i < TEST_AS_COUNT; i++) {
        sem_post(&worker_barrier);
    }

    while (true) {
        thread_yield();
        bool done = progressbar_print();
        if (done) {
            break;
        }
    }

    for (size_t i = 0; i < TEST_AS_COUNT; i++) {
        worker_info[i].pattern = i;
        err = thread_join(worker_info[i].thread, NULL);
        ktest_assert_errno(err, "thread_join");
    }

    ktest_passed();
}
