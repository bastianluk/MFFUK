// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <debug.h>
#include <errno.h>
#include <mm/heap.h>
#include <mm/as.h>
#include <lib/print.h>
#include <proc/scheduler.h>
#include <adt/list.h>
#include <exc.h>


/** OVERVIEW
 *
 * SUBMITTED post deadline (?) (post midnight CET)
 *
 * using 1 queue (to be executed) and 2 lists (suspended and finished) to store threads in.
 *
 * scheduler takes care of moving the threads around the queues
 *  - the thread should not notice/know if it is running or not (slight deviation from what was in teh mailing list)
 *    it only calls the schedulers "API" to be handled correctly
 */


list_t thread_queue;
list_t suspended_threads;
list_t finished_threads;

typedef struct queue_thread {
    thread_t* thread;
    link_t link;
} queue_thread_t;

typedef struct suspended_thread {
    thread_t* thread;
    thread_t* await_thread;
    void** await_result;
    link_t link;
} suspended_thread_t;

typedef struct finished_thread {
    thread_t* thread;
    void* result;
    link_t link;
} finished_thread_t;

queue_thread_t* current_thread;

/** Initialize support for scheduling.
 *
 * Called once at system boot.
 */
void scheduler_init(void) {
    list_init(&thread_queue);
    list_init(&suspended_threads);
    list_init(&finished_threads);
}


/** Get the currently running thread.
 *
 * @return Pointer to the current thread.
 * @retval NULL - No thread is currently running.
 */
thread_t* scheduler_get_current_thread(void) {
    if (current_thread == NULL) {
        return NULL;
    }

    return current_thread->thread;
}

/** Get the currently running context.
 *
 * @return Pointer to the current context.
 * @retval NULL - No context is currently running.
 */
static context_t* scheduler_get_current_context(void) {
    thread_t* thread = scheduler_get_current_thread();
    if (thread == NULL) {
        return NULL;
    }

    return thread->context;
}


/** Creates a queue ready thread
 *
 * @param thread Thread to create the wrapper for.
 */
static queue_thread_t* create_queue_ready_thread(thread_t* thread) {
    queue_thread_t* queue_ready_thread = kmalloc(sizeof(queue_thread_t));
    panic_if(queue_ready_thread == NULL, "Unable to allocate memory for new queued thread.");
    queue_ready_thread->thread = thread;
    link_init(&queue_ready_thread->link);
    return queue_ready_thread;
}

/** Creates a suspended thread
 *
 * @param thread Thread to create the wrapper for.
 * @param await_thread Thread to await.
 * @param await_result The awaits result.
 */
static suspended_thread_t* create_suspended_thread(thread_t* thread, thread_t* await_thread, void** await_result) {
    suspended_thread_t* suspended_thread = kmalloc(sizeof(suspended_thread_t));
    panic_if(suspended_thread == NULL, "Unable to allocate memory for new suspended thread.");
    suspended_thread->thread = thread;
    suspended_thread->await_thread = await_thread;
    suspended_thread->await_result = await_result;
    link_init(&suspended_thread->link);
    return suspended_thread;
}

/** Creates a finished thread
 *
 * @param thread Thread to create the wrapper for.
 * @param result The result of the thread.
 */
static finished_thread_t* create_finished_thread(thread_t* thread, void* result) {
    finished_thread_t* finished_thread = kmalloc(sizeof(finished_thread_t));
    panic_if(finished_thread == NULL, "Unable to allocate memory for new finished thread.");
    finished_thread->thread = thread;
    finished_thread->result = result;
    link_init(&finished_thread->link);
    return finished_thread;
}

/** Creates a queue ready thread
 *
 * @param target_state Desired thread end state.
 * @param thread Thread to create the wrapper for.
 * @param await_thread Thread to await.
 * @param await_result The awaits result.
 * @param result The result of the thread.
 */
static void scheduler_add_thread(thread_state_t target_state, thread_t* thread, thread_t* await_thread, void** await_result, void* result) {
    switch (target_state) {
        case queued: {
            queue_thread_t* queue_ready_thread = create_queue_ready_thread(thread);
            list_append(&thread_queue, &queue_ready_thread->link);
            break;
        }
        case suspended: {
            suspended_thread_t* suspended_thread = create_suspended_thread(thread, await_thread, await_result);
            list_append(&suspended_threads, &suspended_thread->link);
            break;
        }
        case finished: {
            finished_thread_t* finished_thread = create_finished_thread(thread, result);
            list_append(&finished_threads, &finished_thread->link);
            break;
        }
    }
}

/** Marks given thread as ready to be executed.
 *
 * It is expected that this thread would be added at the end
 * of the queue to run in round-robin fashion.
 *
 * @param thread Thread to make runnable.
 */
void scheduler_add_ready_thread(thread_t* thread) {
    scheduler_add_thread(queued, thread, NULL, NULL, NULL);
}

/** Awake threads that are waiting for given thread.

 * @param thread Thread that others waite for.
 */
static void scheduler_awake_threads(thread_t* thread, void* result) {
    link_t* current_link = suspended_threads.head.next;
    while (current_link != (link_t*)&suspended_threads.head) {
        suspended_thread_t* current = list_item(current_link, suspended_thread_t, link);
        current_link = current_link->next;
        if (current->await_thread == thread) {
            scheduler_add_ready_thread(current->thread);
            list_remove(&current->link);
            if (current->await_result != NULL) {
                *current->await_result = result;
            }

            kfree(current);
        }
    }
}


/** Searches queued threads for a specified thread
 *
 * @param thread Thread to find.
 * @return Pointer to a queued thread.
 * @retval NULL Thread was not found.
 */
static queue_thread_t* find_queued_thread(thread_t* thread) {
    if (!list_is_empty(&thread_queue)) {
        list_foreach(thread_queue, queue_thread_t, link, current) {
            if (current->thread == thread) {
                return current;
            }
        }
    }

    return NULL;
}

/** Searches queued threads for a specified thread and remove it from queue
 *
 * @param thread Thread to find.
 * @return Pointer to a queued thread.
 * @retval NULL Thread was not found.
 */
static queue_thread_t* get_queued_thread(thread_t* thread) {
    queue_thread_t* queued_thread = find_queued_thread(thread);
    if (queued_thread != NULL)
    {
        list_remove(&queued_thread->link);
    }

    return queued_thread;
}

/** Searches suspended threads for a specified thread
 *
 * @param thread Thread to find.
 * @return Pointer to a suspended thread.
 * @retval NULL Thread was not found.
 */
static suspended_thread_t* find_suspended_thread(thread_t* thread) {
    if (!list_is_empty(&suspended_threads)) {
        list_foreach(suspended_threads, suspended_thread_t, link, current) {
            if (current->thread == thread) {
                return current;
            }
        }
    }

    return NULL;
}

/** Searches finished threads for a specified thread
 *
 * @param thread Thread to find.
 * @return Pointer to a finished thread.
 * @retval NULL Thread was not found.
 */
static finished_thread_t* find_finished_thread(thread_t* thread) {
    if (!list_is_empty(&finished_threads)) {
        list_foreach(finished_threads, finished_thread_t, link, current) {
            if (current->thread == thread) {
                return current;
            }
        }
    }

    return NULL;
}

/** Mark given thread as finished and awake all threads that were waiting for them.
 *
 * @param thread Thread to remove from the queue.
 * @param result .
 */
static void scheduler_finish_thread(thread_t* thread, void* result) {
    scheduler_add_thread(finished, thread, NULL, NULL, result);
    scheduler_awake_threads(thread, result);

    thread->as->used_by_count--;
    if (thread->as->used_by_count <= 0) {
        as_destroy(thread->as);
    }
}


/** Removes given thread from scheduling.
 *
 * Expected to be called when thread is suspended, terminates execution
 * etc.
 *
 * @param thread Thread to remove from the queue.
 */
void scheduler_remove_thread(thread_t* thread) {
    bool saved = interrupts_disable();
    queue_thread_t* queued = find_queued_thread(thread);
    if (queued != NULL) {
        list_remove(&queued->link);
        kfree(queued);
        kfree(thread);
        interrupts_restore(saved);
        return;
    }

    suspended_thread_t* suspended = find_suspended_thread(thread);
    if (suspended != NULL) {
        list_remove(&suspended->link);
        kfree(suspended);
        kfree(thread);
        interrupts_restore(saved);
        return;
    }

    finished_thread_t* finished = find_finished_thread(thread);
    if (finished != NULL) {
        list_remove(&finished->link);
        kfree(finished);
        kfree(thread);
        interrupts_restore(saved);
        return;
    }
}


/** Call for the CPU to switch the context.
 *
 * @param target_state Target state for the current thread.
 * @param await_thread Thread to await.
 * @param next_thread New current_thread to switch to.
 * @param result Result.
 */
static errno_t scheduler_context_switch(thread_t* previous_thread, thread_t* next_thread, bool saved_restore) {
    queue_thread_t* new_thread;
    if (next_thread == NULL) {
        link_t* new_next_thread_link = list_pop(&thread_queue);
        if (new_next_thread_link == NULL) {

            interrupts_restore(saved_restore);

            return EINVAL;
        }
        new_thread = list_item(new_next_thread_link, queue_thread_t, link);
    } else {
        new_thread = get_queued_thread(next_thread);
    }

    current_thread = new_thread;

    context_t* context = scheduler_get_current_context();
    if (previous_thread == NULL) {
        context_t* throwaway_context = kmalloc(sizeof(context_t));
        cpu_switch_context(throwaway_context, context);
        kfree(throwaway_context);
    } else {
        cpu_switch_context(previous_thread->context, context);
    }

    interrupts_restore(saved_restore);

    return EOK;
}

/** Changes the state of the current_thread to the target state, schedules a new current_thread.
 *
 * @param target_state Target state for the current thread.
 * @param await_thread Thread to await.
 * @param await_result Awaited threads result.
 * @param next_thread New current_thread to switch to.
 * @param result Result.
 */
errno_t scheduler_change_thread_state(thread_state_t target_state, thread_t* await_thread, void** await_result, thread_t* next_thread, void* result) {
    bool saved = interrupts_disable();
    if (target_state == suspended && await_thread != NULL) {
        finished_thread_t* finished_thread = find_finished_thread(await_thread);
        if (finished_thread != NULL) {
            if (await_result != NULL) {
                *await_result = finished_thread->result;
            }

            interrupts_restore(saved);

            return EOK;
        }
    }

    thread_t* current_thread_info = NULL;
    if (current_thread != NULL) {
        current_thread_info = current_thread->thread;
        queue_thread_t* pre_switch_current = current_thread;
        switch (target_state) {
            case queued: {
                list_append(&thread_queue, &current_thread->link);
                break;
            }
            case suspended: {
                scheduler_add_thread(target_state, pre_switch_current->thread, await_thread, await_result, NULL);
                kfree(current_thread);
                break;
            }
            case finished: {
                scheduler_finish_thread(current_thread->thread, result);
                kfree(current_thread);
                break;
            }
        }
    }

    return scheduler_context_switch(current_thread_info, next_thread, saved);
}


/** Switch to next thread in the queue. */
void scheduler_schedule_next(void) {
    scheduler_change_thread_state(queued, NULL, NULL, NULL, NULL);
}


/** Tells if thread is already in the finished queue.
 *
 * @param thread Thread in question.
 */
bool scheduler_has_thread_finished(thread_t* thread) {
    finished_thread_t* existing_thread = find_finished_thread(thread);
    return existing_thread != NULL;
}


/** Wakes-up a thread.
 *
 * @param thread Thread to wake-up.
 * @return Error code.
 * @retval EOK Thread was woken-up (or was already ready/running).
 * @retval EINVAL Invalid thread.
 * @retval EEXITED Thread already finished its execution.
 */
errno_t scheduler_wake_thread_up(thread_t* thread) {
    bool saved = interrupts_disable();
    if (current_thread != NULL && current_thread->thread == thread) {
        interrupts_restore(saved);

        return EOK;
    }

    if (scheduler_has_thread_finished(thread))
    {
        interrupts_restore(saved);

        return EEXITED;
    }

    queue_thread_t* queued_thread = find_queued_thread(thread);
    if (queued_thread != NULL) {
        interrupts_restore(saved);
        return EOK;
    }

    suspended_thread_t* suspended_thread = find_suspended_thread(thread);
    if (suspended_thread != NULL) {
        scheduler_add_ready_thread(thread);
        list_remove(&suspended_thread->link);
        kfree(suspended_thread);

        interrupts_restore(saved);

        return EOK;
    }

    interrupts_restore(saved);
    return EINVAL;
}


/** Switch CPU context to a different thread.
 *
 * @param thread Thread to switch to.
 */
void scheduler_switch_to_thread(thread_t* thread) {
    scheduler_change_thread_state(queued, NULL, NULL, thread, NULL);
}

/** Kill thread and schedule next one.
 *
 * @param thread Thread to kill.
 * @return Error code.
 * @retval EOK Thread was killed.
 * @retval EINVAL Invalid thread.
 * @retval EEXITED Thread already finished its execution.
 */
errno_t kill_thread(thread_t* thread) {
    bool saved = interrupts_disable();
    if (thread == current_thread->thread) {
        return scheduler_change_thread_state(finished, NULL, NULL, NULL, NULL);
    }

    if (scheduler_has_thread_finished(thread)) {
        interrupts_restore(saved);
        return EEXITED;
    }

    queue_thread_t* queued_thread = find_queued_thread(thread);
    if (queued_thread != NULL) {
        list_remove(&queued_thread->link);
        scheduler_finish_thread(queued_thread->thread, NULL);
        kfree(queued_thread);

        interrupts_restore(saved);

        return EOK;
    }

    suspended_thread_t* suspended_thread = find_suspended_thread(thread);
    if (suspended_thread != NULL) {
        list_remove(&suspended_thread->link);
        scheduler_finish_thread(suspended_thread->thread, NULL);
        kfree(suspended_thread);

        interrupts_restore(saved);

        return EOK;
    }

    return EINVAL;
}