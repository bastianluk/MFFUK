// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <proc/mutex.h>
#include <exc.h>

#define DESTR_LOCK_MUTEX_MSSG "Attempting to destroy a locked mutex.."
#define DIFFERENT_THREAD_UNLOCK_MSSG "Unlocked by a different thread than the one that locked it."

/** Initializes given mutex.
 *
 * @param mutex Mutex to initialize.
 * @return Error code.
 * @retval EOK Mutex was successfully initialized.
 */
errno_t mutex_init(mutex_t* mutex) {
    assert(mutex != NULL);

    //mutex_t member init
    mutex->locked = false;
    mutex->thread = NULL;
    list_init(&mutex->waiting_queue);

    return EOK;
}

/** Destroys given mutex.
 *
 * The function must panic if the mutex is still locked when being destroyed.
 *
 * @param mutex Mutex to destroy.
 */
void mutex_destroy(mutex_t* mutex) {
    panic_if(mutex->locked, DESTR_LOCK_MUTEX_MSSG);
}

/** Locks the mutex.
 *
 * Note that when this function returns, the mutex must be locked.
 *
 * @param mutex Mutex to be locked.
 */
void mutex_lock(mutex_t* mutex) {
    bool saved = interrupts_disable();

    thread_t *current_thread = thread_get_current();

    if (mutex->locked) {
        if (current_thread != mutex->thread) {
            add_waiting_thread(&mutex->waiting_queue, current_thread);
            thread_suspend();
        }
    } else {
        mutex->locked = true;
        mutex->thread = current_thread;
    }

    interrupts_restore(saved);
}

/** Unlocks the mutex.
 *
 * Note that when this function returns, the mutex might be already locked
 * by a different thread.
 *
 * This function shall panic if the mutex is unlocked by a different thread
 * than the one that locked it.
 *
 * @param mutex Mutex to be unlocked.
 */
void mutex_unlock(mutex_t* mutex) {
    bool saved = interrupts_disable();

    thread_t *current_thread = thread_get_current();

    panic_if(mutex->thread != current_thread, DIFFERENT_THREAD_UNLOCK_MSSG);

    if (!list_is_empty(&mutex->waiting_queue)) {
        thread_t *waiting_thread = pop_waiting_thread(&mutex->waiting_queue);
        mutex->thread = waiting_thread;

        if (waiting_thread != NULL) {
            thread_wakeup(waiting_thread);
        }
    } else {
        mutex->locked = false;
    }

    interrupts_restore(saved);
}

/** Try to lock the mutex without waiting.
 *
 * If the mutex is already locked, do nothing and return EBUSY.
 *
 * @param mutex Mutex to be locked.
 * @return Error code.
 * @retval EOK Mutex was successfully locked.
 * @retval EBUSY Mutex is currently locked by a different thread.
 */
errno_t mutex_trylock(mutex_t* mutex) {
    bool saved = interrupts_disable();

    thread_t* current_thread = thread_get_current();

    if (!mutex->locked) {
        mutex->locked = true;
        mutex->thread = current_thread;

        interrupts_restore(saved);

        return EOK;
    }

    interrupts_restore(saved);

    return EBUSY;
}
