// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _PROC_SCHEDULER_H
#define _PROC_SCHEDULER_H

#include <proc/thread.h>
#include <errno.h>

#ifndef KERNEL_SCHEDULER_QUANTUM
/** Scheduling quantum default. */
#define KERNEL_SCHEDULER_QUANTUM 2000
#endif

typedef enum {
    queued = 0,
    suspended = 1,
    finished = 2
} thread_state_t;

void scheduler_init(void);
thread_t* scheduler_get_current_thread(void);
void scheduler_add_ready_thread(thread_t* id);
void scheduler_remove_thread(thread_t* id);
errno_t scheduler_change_thread_state(thread_state_t target_state, thread_t* await_id, void** await_result, thread_t* next_thread, void* result);
void scheduler_schedule_next(void);
bool scheduler_has_thread_finished(thread_t* id);
errno_t scheduler_wake_thread_up(thread_t* id);
void scheduler_switch_to_thread(thread_t* id);
errno_t kill_thread(thread_t* thread);

#endif
