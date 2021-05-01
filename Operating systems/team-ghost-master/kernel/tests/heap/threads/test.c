// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Stress test for scheduler and thread management. Create up to THREAD_COUNT
 * threads that repeatedly suspend or wake-up other threads. All threads
 * should eventually finish.
 */

#include <ktest.h>
#include <mm/heap.h>
#include <proc/mutex.h>
#include <proc/thread.h>

#define THREAD_COUNT 10
#define CHUNKS 8
#define CHUNK_SIZE 16

static size_t thread_count;
static thread_t* threads[THREAD_COUNT];
static volatile size_t thread_started_count = 0;
static mutex_t thread_started_count_guard;

static volatile bool all_started = false;
static volatile bool run_now = false;
static volatile bool stop_work = false;

static uintptr_t* malloc_and_mark(uintptr_t marker, size_t count) {
    uintptr_t* result = kmalloc(sizeof(uintptr_t) * count);
    if (result == NULL) {
        return NULL;
    }
    for (size_t i = 0; i < count; i++) {
        result[i] = marker;
    }
    return result;
}

static void free_and_check(uintptr_t marker, uintptr_t* data, size_t count) {
    if (data == NULL) {
        return;
    }
    for (size_t i = 0; i < count; i++) {
        ktest_assert(data[i] == marker, "%pT invalid marker %u (%x != %x)", thread_get_current(), i, data[i], marker);
    }
    kfree(data);
}

static void do_useful_work(uintptr_t marker, uintptr_t** chunks, size_t chunk_count) {
    for (int j = 0; j < CHUNKS; j++) {
        free_and_check(marker, chunks[j], CHUNK_SIZE);
        chunks[j] = malloc_and_mark(marker, CHUNK_SIZE);
    }
}

static void* worker(void* ignored) {
    mutex_lock(&thread_started_count_guard);
    thread_started_count++;
    mutex_unlock(&thread_started_count_guard);

    while (!run_now) {
    }
    uintptr_t* chunks[CHUNKS];
    uintptr_t my_mark = (uintptr_t)&chunks;
    for (int i = 0; i < CHUNKS; i++) {
        chunks[i] = NULL;
    }

    while (!stop_work) {
        do_useful_work(my_mark, chunks, CHUNKS);
    }

    return NULL;
}

void kernel_test(void) {
    ktest_start("heap/threads");

    errno_t err = mutex_init(&thread_started_count_guard);

    for (thread_count = 0; thread_count < THREAD_COUNT; thread_count++) {
        err = thread_create(&threads[thread_count], worker, NULL, 0, "worker");
        if (err == ENOMEM) {
            break;
        }
        ktest_assert_errno(err, "thread_create");
    }

    printk("Created %u threads...\n", thread_count);
    bool all_started = false;
    while (!all_started) {
        mutex_lock(&thread_started_count_guard);
        if (thread_started_count == thread_count) {
            all_started = true;
        }
        mutex_unlock(&thread_started_count_guard);
        thread_yield();
    }
    run_now = true;

    uintptr_t* chunks[CHUNKS];
    uintptr_t my_mark = (uintptr_t)&chunks;
    for (int i = 0; i < CHUNKS; i++) {
        chunks[i] = NULL;
    }
    for (int i = 0; i < THREAD_COUNT * 4; i++) {
        do_useful_work(my_mark, chunks, CHUNKS);
        printk("Finished loop %d ...\n", i);
    }

    stop_work = true;

    for (size_t i = 0; i < thread_count; i++) {
        err = thread_join(threads[i], NULL);
        ktest_assert_errno(err, "thread_join");
    }

    ktest_passed();
}
