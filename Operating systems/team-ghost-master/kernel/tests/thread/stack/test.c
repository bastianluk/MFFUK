// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

/*
 * Trivial test for checking that thread stacks do not overlap.
 */

#include <ktest.h>
#include <ktest/thread.h>
#include <proc/thread.h>

/*
 * Minimal stack pointer distance in two different threads.
 *
 * We take the distance in very similar functions but we take into account
 * that there might be some differences (maybe even randomization in
 * thread_create?) and check for smaller difference than full stack size.
 */
#define MIN_STACK_POINTER_DIFFERENCE (THREAD_STACK_SIZE / 2)

typedef struct {
    thread_t* thread;
    uintptr_t stack_pointer_start;
    uintptr_t stack_pointer_end;
    volatile bool started;
    char name[THREAD_NAME_MAX_LENGTH + 1];
} info_t;

#define INFO_INIT(thread_name) \
    { \
        .thread = NULL, \
        .stack_pointer_start = 0, \
        .stack_pointer_end = 0, \
        .started = false, \
        .name = thread_name \
    }

/*
 * kernel_test
 *  [ 0] layer_one
 *  [ 1]   layer_two
 *  [ 2]     layer_three
 *  [ 3]     layer_three
 *  [ 4]   layer_two
 *  [ 5]     layer_three
 *  [ 6]     layer_three
 *  [ 7] layer_one
 *  [ 8]   layer_two
 *  [ 9]     layer_three
 *  [10]     layer_three
 *  [11]   layer_two
 *  [12]     layer_three
 *  [13]     layer_three
 */

#define THREAD_COUNT 14
static info_t infos[THREAD_COUNT] = {
    INFO_INIT("tester-00"),
    INFO_INIT("tester-01"),
    INFO_INIT("tester-02"),
    INFO_INIT("tester-03"),
    INFO_INIT("tester-04"),
    INFO_INIT("tester-05"),
    INFO_INIT("tester-06"),
    INFO_INIT("tester-07"),
    INFO_INIT("tester-08"),
    INFO_INIT("tester-09"),
    INFO_INIT("tester-10"),
    INFO_INIT("tester-11"),
    INFO_INIT("tester-12"),
    INFO_INIT("tester-13")
};

static volatile bool all_started = false;

static inline uintptr_t get_stack_pointer(void) {
    register uintptr_t sp __asm__("sp");
    return sp;
}

static void* layer_three(void* storage) {
    info_t* my_info = storage;

    my_info[0].stack_pointer_start = get_stack_pointer();

    my_info[0].started = true;

    while (!all_started) {
        thread_yield();
    }

    my_info[0].stack_pointer_end = get_stack_pointer();
    thread_yield();

    return NULL;
}

static void* layer_two(void* storage) {
    info_t* my_info = storage;

    my_info[0].stack_pointer_start = get_stack_pointer();

    ktest_thread_create_checked(&my_info[1].thread, &layer_three, my_info + 1, 0, my_info[1].name);
    ktest_thread_create_checked(&my_info[2].thread, &layer_three, my_info + 2, 0, my_info[2].name);

    my_info[0].started = true;

    while (!all_started) {
        thread_yield();
    }

    my_info[0].stack_pointer_end = get_stack_pointer();
    thread_yield();

    return NULL;
}

static void* layer_one(void* storage) {
    info_t* my_info = storage;

    my_info[0].stack_pointer_start = get_stack_pointer();

    ktest_thread_create_checked(&my_info[1].thread, &layer_two, my_info + 1, 0, my_info[1].name);
    ktest_thread_create_checked(&my_info[4].thread, &layer_two, my_info + 4, 0, my_info[4].name);

    my_info[0].started = true;

    while (!all_started) {
        thread_yield();
    }

    my_info[0].stack_pointer_end = get_stack_pointer();
    thread_yield();

    return NULL;
}

static size_t get_abs_diff(uintptr_t a, uintptr_t b) {
    if (a > b) {
        return a - b;
    } else {
        return b - a;
    }
}

void kernel_test(void) {
    ktest_start("thread/stack");

    ktest_thread_create_checked(&infos[0].thread, &layer_one, infos + 0, 0, infos[0].name);
    ktest_thread_create_checked(&infos[7].thread, &layer_one, infos + 7, 0, infos[7].name);

    while (true) {
        printk("Waiting for all threads to start ... \n");
        bool all_on_barrier = true;
        for (int i = 0; i < THREAD_COUNT; i++) {
            if (!infos[i].started) {
                all_on_barrier = false;
                break;
            }
        }
        if (all_on_barrier) {
            break;
        }
        thread_yield();
    }
    printk("All threads started.\n");
    all_started = true;
    thread_yield();

    printk("Joining threads ...\n");
    for (int i = 0; i < THREAD_COUNT; i++) {
        ktest_thread_join_checked(infos[i].thread, NULL);
    }

    for (int i = 0; i < THREAD_COUNT; i++) {
        info_t* this = &infos[i];
        printk("Checking thread %d (%s)\n", i, this->name);

        ktest_assert(this->stack_pointer_start != 0, "thread function to run at all");
        ktest_assert(this->stack_pointer_end != 0, "thread function to run at all");
        size_t sp_diff = get_abs_diff(this->stack_pointer_start, this->stack_pointer_end);
        ktest_assert(sp_diff < 1024, "$sp moves too much inside one function");

        for (int j = i + 1; j < THREAD_COUNT; j++) {
            info_t* that = &infos[j];
            size_t sp_diff = get_abs_diff(this->stack_pointer_start, that->stack_pointer_start);
            ktest_assert(sp_diff >= MIN_STACK_POINTER_DIFFERENCE,
                    "$sp of different threads are too close together");
        }
    }

    ktest_passed();
}
