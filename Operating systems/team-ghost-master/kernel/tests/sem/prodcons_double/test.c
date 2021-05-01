// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Producer/consumer test. We spawn different numbers of producers and
 * consumers (they equal out at the end of the test, though) to test that
 * semaphores work. If all is well, the queue should be empty at the end
 * of the run and all threads should terminate.
 *
 * This is an extension of the prodcons test that it uses two queues
 * and trywait operation to insert into free queue first (when possible).
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/sem.h>
#include <proc/thread.h>

#define LOOPS 100
#define BASE_COUNT 15
#define QUEUE_SIZE (BASE_COUNT * 2)
#define TOTAL_THREAD_COUNT (6 * BASE_COUNT)

#define WAIT_LOOPS BASE_COUNT

static sem_t queue_one_empty;
static sem_t queue_one_full;
static sem_t queue_two_empty;
static sem_t queue_two_full;

static size_t producer_thread_count = 0;
static thread_t* producer_threads[TOTAL_THREAD_COUNT];

static size_t consumer_thread_count = 0;
static thread_t* consumer_threads[TOTAL_THREAD_COUNT];

static volatile size_t active_threads = 0;
static sem_t active_threads_guard;

static void register_thread(void) {
    sem_wait(&active_threads_guard);
    active_threads++;
    sem_post(&active_threads_guard);
}

static void unregister_thread(void) {
    sem_wait(&active_threads_guard);
    active_threads--;
    sem_post(&active_threads_guard);
}

/*
 * Smart wrapper around sem_wait and sem_trywait.
 *
 * When try_at_all is false, do not try any locking and return at once.
 * This check would have to be otherwise in the producer/consumer code to
 * prevent overflowing items on one queue.
 *
 * The block parameter then specifies whether it makes sense to try nonblock
 * call (i.e. if it makes sense to try the other lock next time).
 */
static errno_t wait_with_condition(sem_t* sem, bool try_at_all, bool block) {
    if (!try_at_all) {
        return EBUSY;
    }

    if (block) {
        sem_wait(sem);
        return EOK;
    } else {
        return sem_trywait(sem);
    }
}

static void* worker_producer(void* ignored) {
    register_thread();

    int items_one_to_produce = LOOPS;
    int items_two_to_produce = LOOPS;

    while ((items_one_to_produce > 0) || (items_two_to_produce > 0)) {
        errno_t err = wait_with_condition(&queue_one_empty, items_one_to_produce > 0, items_two_to_produce == 0);
        if (err == EOK) {
            // Here would be lock on the queue ONE
            // itself and adding to it.
            // printk("a");
            items_one_to_produce--;
            sem_post(&queue_one_full);
        }
        err = wait_with_condition(&queue_two_empty, items_two_to_produce > 0, items_one_to_produce == 0);
        if (err == EOK) {
            // Here would be lock on the queue TWO
            // itself and adding to it.
            // printk("b");
            items_two_to_produce--;
            sem_post(&queue_two_full);
        }
        thread_yield();
    }

    unregister_thread();
    return NULL;
}

static void* worker_consumer(void* ignored) {
    register_thread();

    int items_one_to_consume = LOOPS;
    int items_two_to_consume = LOOPS;

    while ((items_one_to_consume > 0) || (items_two_to_consume > 0)) {
        errno_t err = wait_with_condition(&queue_two_full, items_two_to_consume > 0, items_one_to_consume == 0);
        if (err == EOK) {
            // Here would be lock on the queue
            // itself and adding to it.
            // printk("B");
            items_two_to_consume--;
            sem_post(&queue_two_empty);
        }

        err = wait_with_condition(&queue_one_full, items_one_to_consume > 0, items_two_to_consume == 0);
        if (err == EOK) {
            // Here would be lock on the queue
            // itself and adding to it.
            // printk("A");
            items_one_to_consume--;
            sem_post(&queue_one_empty);
        }
        thread_yield();
    }

    unregister_thread();
    return NULL;
}

static void spawn_workers(size_t producers, size_t consumers) {
    printk("Will spawn %u producers and %u consumers ...\n", producers, consumers);
    while (producers > 0) {
        ktest_assert(producer_thread_count < TOTAL_THREAD_COUNT, "too many producers created");
        ktest_thread_create_checked(&producer_threads[producer_thread_count], worker_producer, NULL, 0, "producer");
        producers--;
        producer_thread_count++;
    }
    while (consumers > 0) {
        ktest_assert(consumer_thread_count < TOTAL_THREAD_COUNT, "too many consumers created");
        ktest_thread_create_checked(&consumer_threads[consumer_thread_count], worker_consumer, NULL, 0, "consumer");
        consumers--;
        consumer_thread_count++;
    }
}

static void let_them_work() {
    for (int i = 0; i < WAIT_LOOPS; i++) {
        thread_yield();
    }
}

void kernel_test(void) {
    ktest_start("sem/prodcons_double");

    errno_t err = sem_init(&active_threads_guard, 1);
    ktest_assert_errno(err, "sem_init(active_threads_guard)");

    err = sem_init(&queue_one_empty, QUEUE_SIZE);
    ktest_assert_errno(err, "sem_init(queue_one_empty)");
    err = sem_init(&queue_two_empty, QUEUE_SIZE);
    ktest_assert_errno(err, "sem_init(queue_two_empty)");
    err = sem_init(&queue_one_full, 0);
    ktest_assert_errno(err, "sem_init(queue_one_full)");
    err = sem_init(&queue_two_full, 0);
    ktest_assert_errno(err, "sem_init(queue_two_full)");

    spawn_workers(BASE_COUNT * 3, BASE_COUNT * 1);
    let_them_work();
    spawn_workers(BASE_COUNT * 2, BASE_COUNT * 2);
    let_them_work();
    spawn_workers(BASE_COUNT * 1, BASE_COUNT * 3);
    let_them_work();

    while (true) {
        dprintk("Q1: %pS %pS  Q2: %pS %pS\n", &queue_one_empty, &queue_one_full, &queue_two_empty, &queue_two_full);

        sem_wait(&active_threads_guard);
        size_t still_active = active_threads;
        sem_post(&active_threads_guard);

        printk("There are %u active threads ...\n", still_active);
        if (still_active == 0) {
            break;
        }

        let_them_work();
    }

    for (size_t i = 0; i < producer_thread_count; i++) {
        ktest_thread_join_checked(producer_threads[i], NULL);
    }
    for (size_t i = 0; i < consumer_thread_count; i++) {
        ktest_thread_join_checked(consumer_threads[i], NULL);
    }

    ktest_assert(sem_get_value(&queue_one_full) == 0, "queue ONE still contains items");
    ktest_assert(sem_get_value(&queue_two_full) == 0, "queue TWO still contains items");
    ktest_assert(sem_get_value(&queue_one_empty) == QUEUE_SIZE, "queue ONE is not empty");
    ktest_assert(sem_get_value(&queue_two_empty) == QUEUE_SIZE, "queue TWO is not empty");

    ktest_passed();
}
