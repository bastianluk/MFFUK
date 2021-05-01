// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <proc/sem.h>
#include <exc.h>

/** Initializes given semaphore.
 *
 * @param sem Semaphore to initialize.
 * @param value Initial semaphore value (1 effectively creates mutex).
 * @return Error code.
 * @retval EOK Semaphore was successfully initialized.
 */
errno_t sem_init(sem_t* sem, int value) {
    assert(sem != NULL);

    sem->value = value;
    list_init(&sem->waiting_queue);

    return EOK;
}

/** Block current thread.
 * Current thread is blocked and added to waiting queue.
 *
 * @param sem Semaphore to be locked.
 */
static void block_current_thread(sem_t* sem){
    thread_t* thread = thread_get_current();
    add_waiting_thread(&sem->waiting_queue, thread);
    thread_suspend();
}

/** Wake first thread in the waitin queue.
 *
 * @param sem Semaphore to be locked.
 */
static void wake_awaited_thread(sem_t* sem){
    thread_t* item = pop_waiting_thread(&sem->waiting_queue);
    thread_wakeup(item);
}

/** Destroys given semaphore.
 *
 * The function must panic if there are threads waiting for this semaphore.
 *
 * @param sem Semaphore to destroy.
 */
void sem_destroy(sem_t* sem) {
    bool saved = interrupts_disable();

    panic_if(!list_is_empty(&sem->waiting_queue), "destroying semaphor with waiting threads");

    interrupts_restore(saved);
}

/** Get current value of the semaphore.
 *
 * @param sem Semaphore to query.
 * @return Current semaphore value.
 */
int sem_get_value(sem_t* sem) {
    return sem->value;
}

/** Locks (downs) the semaphore.
 *
 * Decrement the value of this semaphore. If the new value would be negative,
 * block and wait for someone to call sem_post() first.
 *
 * @param sem Semaphore to be locked.
 */
void sem_wait(sem_t* sem) {
    bool saved = interrupts_disable();

    if (sem->value == 0) {
        block_current_thread(sem);
    } else {
        sem->value--;
    }

    interrupts_restore(saved);
}

/** Unlocks (ups/signals) the sempahore.
 *
 * Increment the value of this semaphore or wake-up one of blocked threads
 * inside sem_wait().
 *
 * @param sem Semaphore to be unlocked.
 */
void sem_post(sem_t* sem) {
    bool saved = interrupts_disable();

    if (list_is_empty(&sem->waiting_queue)) {
        sem->value++;
    } else {
        wake_awaited_thread(sem);
    }

    interrupts_restore(saved);
}

/** Try to lock the semaphore without waiting.
 *
 * If the call to sem_wait() would block, do nothing and return EBUSY.
 *
 * @param sem Semaphore to be locked.
 * @return Error code.
 * @retval EOK Semaphore was successfully locked.
 * @retval EBUSY Semaphore has value of 0 and locking would block.
 */
errno_t sem_trywait(sem_t* sem) {
    bool saved = interrupts_disable();

    if (sem->value > 0) {
        sem_wait(sem);

        interrupts_restore(saved);

        return EOK;
    } else {
        interrupts_restore(saved);

        return EBUSY;
    }
}
