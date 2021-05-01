// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Checks that thread scheduling works even in low-memory conditions.
 *
 * We create several threads, let them run for a while and then we
 * exhaust all available heap memory. The scheduling shall continue
 * without any issues as no memory allocation shall be needed.
 *
 * Test is concluded by releasing the allocated memory and joining
 * all threads.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <mm/heap.h>
#include <proc/thread.h>

#define THREAD_COUNT 50
#define MIN_ALLOCATION_SIZE 4
#define START_ALLOCATION_SIZE (MIN_ALLOCATION_SIZE << 12)

static volatile bool all_started = false;
static volatile bool shutdown = false;
static size_t thread_count;
static thread_t* threads[THREAD_COUNT];

static void* active_worker(void* ignored) {
    while (!all_started) {
        thread_yield();
    }

    while (!shutdown) {
        thread_yield();
    }

    return NULL;
}

static void* sleeping_worker(void* ignored) {
    while (!all_started) {
        thread_yield();
    }

    thread_suspend();

    while (!shutdown) {
        thread_yield();
    }

    return NULL;
}

static void let_scheduler_work(void) {
    printk("Letting threads run for a while ...\n");
    for (size_t i = 0; i < thread_count; i++) {
        thread_yield();
    }
}

static size_t get_running_count(void) {
    size_t result = 0;
    for (size_t i = 0; i < thread_count; i++) {
        if (!thread_has_finished(threads[i])) {
            result++;
        }
    }
    return result;
}

static uintptr_t exhaust_heap_memory(void) {
    printk("Starting exhaustive allocation ...\n");

    uintptr_t previous_block = 0;

    size_t allocation_size = START_ALLOCATION_SIZE;
    while (1) {
        uintptr_t* ptr = kmalloc(allocation_size);
        dprintk("kmalloc(%u) = %pA\n", allocation_size, ptr);
        if (ptr == NULL) {
            allocation_size = allocation_size / 2;
            if (allocation_size < MIN_ALLOCATION_SIZE) {
                break;
            }
            continue;
        }

        ptr[0] = previous_block;
        previous_block = (uintptr_t)ptr;
    }

    return previous_block;
}

static void return_memory_to_heap(uintptr_t previous_block) {
    printk("Returning the allocated memory back ...\n");
    while (previous_block != 0) {
        uintptr_t* ptr = (uintptr_t*)previous_block;
        previous_block = ptr[0];
        kfree(ptr);
    }
}

void kernel_test(void) {
    ktest_start("thread/exhaust");

    for (thread_count = 0; thread_count < THREAD_COUNT; thread_count++) {
        bool is_active_thread = thread_count % 2 == 0;
        errno_t err = thread_create(&threads[thread_count],
                is_active_thread ? active_worker : sleeping_worker,
                NULL, 0,
                is_active_thread ? "active_worker" : "sleeping_worker");
        if (err == ENOMEM) {
            break;
        }
        ktest_assert_errno(err, "thread_create");
        dprintk("Created thread #%u: %pT.\n", thread_count, threads[thread_count]);
        thread_yield();
    }

    printk("Created %u threads ...\n", thread_count);

    all_started = true;

    let_scheduler_work();

    printk("Half of the threads shall be blocked by now.\n");

    uintptr_t exhaust_ptr = exhaust_heap_memory();

    let_scheduler_work();

    printk("Waking up the suspended threads ...\n");
    for (size_t i = 0; i < thread_count; i++) {
        errno_t err = thread_wakeup(threads[i]);
        ktest_assert_errno(err, "thread_wakeup");
    }

    let_scheduler_work();

    return_memory_to_heap(exhaust_ptr);

    let_scheduler_work();

    shutdown = true;

    printk("Shutting down ...\n");
    size_t count = thread_count;
    while (count > 0) {
        count = get_running_count();
        printk(" ... %u threads running ...\n", count);
        thread_yield();
    }

    for (size_t i = 0; i < thread_count; i++) {
        ktest_thread_join_checked(threads[i], NULL);
    }

    ktest_passed();
}
