// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <drivers/machine.h>
#include <ktest.h>
#include <lib/print.h>
#include <main.h>
#include <mm/as.h>
#include <mm/frame.h>
#include <mm/heap.h>
#include <proc/scheduler.h>
#include <proc/thread.h>

static void* init_thread(void* ignored) {
#ifdef KERNEL_TEST
    kernel_test();
#else
    printk("%s: Hello, World!\n", thread_get_current()->name);
    dprintk("%pT: $sp = 0x%x\n", thread_get_current(), debug_get_stack_pointer());
#endif
    printk("\nHalt.\n");
    machine_halt();

    return NULL;
}

/** This is kernel C-entry point.
 *
 * Kernel jumps here from assembly bootstrap code. Note that
 * this function runs on special stack and does not represent a
 * real thread (yet).
 *
 * When the code is compiled to run kernel test, we execute only
 * that test and terminate.
 */
void kernel_main(void) {
    frame_init();
    heap_init();
    as_init();
    scheduler_init();
    threads_init();

    thread_t* main_thread;
    errno_t err = thread_create_new_as(&main_thread, init_thread, NULL, 0, "[INIT]", 0);
    panic_if(err != EOK, "init thread creation failed (%d: %s)", err, errno_as_str(err));

    // Switch to the first thread.
    scheduler_schedule_next();

    // We are not a real thread here so we should never return here.
    panic("unexpected return to kernel_main");
}
