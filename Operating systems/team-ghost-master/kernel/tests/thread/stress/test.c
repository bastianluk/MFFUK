// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Stress test for scheduler and thread management. Create up to THREAD_COUNT
 * threads that repeatedly suspend or wake-up other threads. All threads
 * should eventually finish.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

#define THREAD_COUNT 100
#define ACTIONS_MAX 200
#define ACTIONS_MIN 100

static volatile bool all_started = false;
static size_t thread_count;
static thread_t* threads[THREAD_COUNT];

static inline unative_t get_simple_rand(unative_t* seed) {
    *seed = (*seed * 1439) % 211 + 7;
    return (*seed) >> 4;
}

static inline unative_t get_simple_rand_range(unative_t* seed, unative_t lower, unative_t upper) {
    return lower + get_simple_rand(seed) % (upper - lower);
}

static inline void thread_wakeup_robust(thread_t* thread) {
    errno_t err = thread_wakeup(thread);
    if (err == EEXITED) {
        return;
    }
    ktest_assert_errno(err, "thread_wakeup");
}

static void* worker_with_random_action(void* ignored) {
    while (!all_started) {
        thread_yield();
    }

    thread_t* myself = thread_get_current();
    unative_t seed = (((unative_t)myself) >> 4) & 0xffff;

    unsigned int loops = get_simple_rand_range(&seed, ACTIONS_MIN, ACTIONS_MAX);
    for (unsigned int i = 0; i < loops; i++) {
        int action = get_simple_rand(&seed) % 7;
        switch (action) {
        case 0:
            thread_suspend();
            break;
        case 1:
            thread_wakeup_robust(threads[get_simple_rand(&seed) % thread_count]);
            break;
        default:
            thread_yield();
        }
    }

    return NULL;
}

static size_t get_running_count() {
    size_t result = 0;
    for (size_t i = 0; i < thread_count; i++) {
        if (!thread_has_finished(threads[i])) {
            result++;
        }
    }
    return result;
}

static void make_thread_name(const char* prefix, char* buffer, size_t size, size_t thread_index) {
    uint8_t digits[sizeof(size_t) * 8];
    size_t digit_count = 0;
    if (thread_index == 0) {
        digits[0] = 0;
        digit_count = 1;
    } else {
        while (thread_index > 0) {
            digits[digit_count] = thread_index % 10;
            thread_index /= 10;
            digit_count++;
        }
    }
    while ((size > 0) && (*prefix != 0)) {
        *buffer = *prefix;
        size--;
        buffer++;
        prefix++;
    }
    while ((size > 0) && (digit_count > 0)) {
        *buffer = digits[digit_count - 1] + '0';
        size--;
        buffer++;
        digit_count--;
    }
    *buffer = 0;
}

void kernel_test(void) {
    ktest_start("thread/stress");

    for (thread_count = 0; thread_count < THREAD_COUNT; thread_count++) {
        char name[THREAD_NAME_MAX_LENGTH + 1];
        make_thread_name("worker", name, THREAD_NAME_MAX_LENGTH + 1, thread_count);
        errno_t err = thread_create(&threads[thread_count], worker_with_random_action, NULL, 0, name);
        if (err == ENOMEM) {
            break;
        }
        ktest_assert_errno(err, "thread_create");
        dprintk("Created thread #%u: %pT.\n", thread_count, threads[thread_count]);
    }

    printk("Created %u threads...\n", thread_count);

    all_started = true;

    size_t count = thread_count;
    while (count > 0) {
        for (size_t i = 0; i < thread_count; i++) {
            if (!thread_has_finished(threads[i])) {
                thread_wakeup_robust(threads[i]);
            }
        }
        count = get_running_count();
        printk(" ... %u threads running ...\n", count);
        thread_yield();
    }

    for (size_t i = 0; i < thread_count; i++) {
        ktest_thread_join_checked(threads[i], NULL);
    }

    ktest_passed();
}
