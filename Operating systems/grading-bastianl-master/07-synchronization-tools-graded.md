# Process Synchronization Tools

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday November 19 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


## Atomics

**Q1** The GCC compiler provides a builtin function `int __atomic_fetch_and (int *ptr, int val, int memorder)`,
with the semantics specified as "performs the operation suggested by the name, and returns the value
that had previously been in `*ptr`" (the operation suggested by the name here is binary `and`).
Ignore the `memorder` parameter (if you know what it is, then assume it is set to `__ATOMIC_SEQ_CST`).

We have used the function to implement a spin lock like this:

```c
void spin_lock (int *lock_state) {
    while (!__atomic_fetch_and (lock_state, ~1)) {
        // Empty loop body for spinning ...
    }
}
```

Looking at the code above, can you tell what value(s) of `lock_state` represent a spin lock that is not locked ?

**A1** `__atomic_fetch_and (lock_state, ~1)` needs to return `1` so that the condition in the `while` call is `0` and we dont get stuck in the loop - we are comparing to `~1` which is `-2` if I am correct, so `lock_state` is equal to `-2`/ `~1` represents the unlocked lock.


**Q2** The GCC compiler provides a builtin function `bool __atomic_test_and_set (void *ptr, int memorder)`,
with the semantics specified as "performs an atomic test-and-set operation on the byte at `*ptr`,
the byte is set to some implementation defined nonzero `SET` value and the return value
is `true` if and only if the previous contents were `SET`".
Ignore the `memorder` parameter (if you know what it is, then assume it is set to `__ATOMIC_SEQ_CST`).

We have used the function to implement a cache friendly spin lock (a spin lock that spins with
atomic read operations rather than with atomic test-and-set operations) like this (assume
simple reads and writes of `lock_state` are atomic):

```c
void spin_lock (volatile bool *lock_state) {
    while (true) {
        while (*lock_state) {
            // Empty loop body for spinning ...
        }
        if (!__atomic_test_and_set (lock_state)) {
            break;
        }
    }
}

void spin_unlock (volatile bool *lock_state) {
    *lock_state = false;
}
```

Looking at the code above, can you tell what is the minimum and the  maximum number of the atomic test-and-set operations
performed if two threads attempt to acquire the spin lock ? Assume each thread
simply does `spin_lock (&lock_state) ; spin_unlock (&lock_state)` on
a shared `volatile bool lock_state`, once. The answer should give
the minimum and maximum totals summed across both threads.

**A2** Min is `2` - one for each thread, basically if the execution was sequential for example. and Max is `3` because if they both get to the test and set, one has to "lose" and re enter the `while(true)` loop, get stuck in the `while(*lock_state)` loop and only once the other thread unlock, it will jump out, get to the test and set, lock and continue to unlock. 


## Locks

**Q3** Consider the code of the following two threads:

```c
static pthread_mutex_t mutex_one = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_two = PTHREAD_MUTEX_INITIALIZER;

void *thread_one_function (void *dummy) {
    pthread_mutex_lock (&mutex_one);
    pthread_mutex_lock (&mutex_two);
    putchar ('A');
    pthread_mutex_unlock (&mutex_one);
    pthread_mutex_unlock (&mutex_two);
    putchar ('B');
    return (NULL);
}

void *thread_two_function (void *dummy) {
    pthread_mutex_lock (&mutex_two);
    pthread_mutex_lock (&mutex_one);
    putchar ('C');
    pthread_mutex_unlock (&mutex_two);
    pthread_mutex_unlock (&mutex_one);
    putchar ('D');
    return (NULL);
}
```

Assuming `putchar` displays the character immediately (no buffering),
what are the possible outputs of this program when both threads run ?

**A3** `CABD`, `CADB`, `CDAB`, `ABCD`, `ACBD`, `ACDB` and also if th2 locks lock2 first, then th1 locks lock1, it will cause a deadlock because both of the threads are waiting for the lock they didn't lock yet.


## Semaphores

**Q4** We have decided to use semaphores to implement locks, like this (error checking omitted for brevity):

```c
typedef struct lock {
    sem_t inner_semaphore;
} lock_t;


void lock_init (lock_t *lock) {
    sem_init (&lock->inner_semaphore, 0, 1);
}

void lock_lock (lock_t *lock) {
    sem_wait (&lock->inner_semaphore);
}

void lock_unlock (lock_t *lock) {
    sem_post (&lock->inner_semaphore);
}
```

Use `man` on Linux for information on the semaphore functions if needed.

Can you describe how, through a mixture of `lock_lock` and `lock_unlock` calls,
this implementation can be distinguished from regular locks ?
By regular lock implementation, we mean for example
the POSIX mutex functions.

**A4** ...


## Condition Variables

**Q5** Consider the code of the following two threads:

```c
static sem_t sem; // Initialized elsewhere to 0.
static pthread_cond_t cond; // Initialized elsewhere.
static pthread_mutex_t mutex; // Initialized elsewhere.

int x = 0;
char c = 'A';

void *thread_one_function (void *dummy) {
    sem_wait (&sem);
    for (int i = 0 ; i < 20 ; i ++) {
        pthread_mutex_lock (&mutex);
        x ++;
        c ++;
        pthread_cond_signal (&cond);
        pthread_mutex_unlock (&mutex);
    }
    return (NULL);
}

void *thread_two_function (void *dummy) {
    pthread_mutex_lock (&mutex);
    sem_post (&sem);
    while (x != 10) {
        pthread_cond_wait (&cond, &mutex);
    }
    putchar (c);
    pthread_mutex_unlock (&mutex);
    return (NULL);
}
```

Assuming `putchar` displays the character immediately (no buffering),
what are the possible outputs of this program when both threads run ?

**A5** More or less two main ways of getting to the same situation: If th1 runs first, it waits for the semaphore, th2 eventually has to run while th1 is waiting, set the semaphore to the correct value, while locked - meaning it will get inside the `while (x != 10)` loop and to the `pthread_cond_wait (&cond, &mutex)` which temporarly unlocks the lock so th1 can run, th1 gets into the for loop, increments `x` and `c`, calls `pthread_cond_signal (&cond)` meaning the th2 will wakeup once th1 releases the lock. th2 checks the `while (x != 10)` condition and until it fails, it will go back to waiting for the condition, th1 will run again, go to the loop, same as before... this repeats it self until x is not eqaul `10`, then it prints the `c` which was `A` in the begginig and was incremented to `K`, annd that will get printed (<--finally an answer to the question). (Second way) If th2 runs first, it locks, prepares the semaphore for th1, gets to the loop and waits, th1 tries wait, it can proceed, for loop increases the values same as before, signals same as before, back to the check in th2, wait until `x = 10` and also should print `K`.
