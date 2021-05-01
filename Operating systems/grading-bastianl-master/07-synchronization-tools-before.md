# Process Synchronization Tools

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday November 12 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the second of three self study modules that will look at the synchronization issues
when using multiple threads for concurrent programming. The goal of this module is to take a look
at common synchronization tools, such as locks, semaphores, and condition variables, in more detail.


## Lock Implementation

Last week, you read about several types of locks in _Arpaci-Dusseau Section 28 Locks_.
You should be familiar with the following:

- What are `test-and-set` or `compare-and-swap` or similar atomic instructions good for.
- How to construct a spin lock with one of these instructions, in detail.
- How to construct a lock with wait queue, roughly.

If you are not entirely sure, please take a look at the material again.


**Q1** The GCC compiler provides a builtin function that gives access to a `test-and-set`-style instruction.
The signature is `bool __atomic_test_and_set (void *ptr, int memorder)`. Ignore the `memorder` parameter
(if you know what it is, then assume it is set to `__ATOMIC_SEQ_CST`).

If two threads, A and B, both execute `__atomic_test_and_set (&X)` on variable `X` initialized to zero,
what are the possible combinations of return values observed by A and B ?
(Reply with observable pairs of `true` and `false` values.)

**A1** ...


**Q2** The GCC compiler also provides a builtin function that gives access to a `compare-and-swap`-style instruction.
The signature is `bool __atomic_compare_exchange (type *ptr, type *expected, type *desired, bool weak, int success_memorder, int failure_memorder)`.
Ignore the `weak` and `memorder` parameters (if you know what they are, then assume `weak` is `false` and both `memorder` are set to `__ATOMIC_SEQ_CST`).

We start with a variable `X` initialized to zero, and run two threads, A and B.
A has a variable `AE` initialized to 0, a variable `AD` initialized to 65, and executes `__atomic_compare_exchange (&X, &AE, &AD)`.
B has a variable `BE` initialized to 0, a variable `BD` initialized to 66, and executes `__atomic_compare_exchange (&X, &BE, &BD)`.
What are the possible combinations of return values observed by A and B, and the possible values of `AE` and `BE` ?
(Reply with observable four tuples of (retval A, `AE`, retval B, `BE`.)

**A2** ...


**Q3** You already know that with atomic `test-and-set`, an implementation of a spin wait for a lock `l` can be as simple as this:

```c
while (test_and_set (&l)) { /* busy loop with empty body */ }
```

Now, instead of `test_and_set (bool *b)`, you have `int atomic_exchange (int *var, int new)`,
which atomically writes `new` into `*val` and returns previous value of `*val`.
What would the implementation of a spin wait for a lock look like with this ?

**A3** ...


## Condition Variables

Read about condition variables in _Arpaci-Dusseau Section 30 Condition Variables_.
The text is quite long, but it is enough to read only the first five pages,
that is, the introductory part and Section 30.1.

If you would prefer a more condensed text (perhaps you are already familiar with these things),
you can simply take a look at `man pthread_cond_wait` and `man pthread_cond_broadcast`.


**Q4** Consider the code of the following two threads:

```c
static pthread_cond_t cond; // Initialized elsewhere.
static pthread_mutex_t mutex; // Initialized elsewhere.

void *thread_one_function (void) {
    pthread_mutex_lock (&mutex);
    pthread_cond_signal (&cond);
    putchar ('1');
    pthread_mutex_unlock (&mutex);
}

void *thread_two_function (void) {
    pthread_mutex_lock (&mutex);
    pthread_cond_wait (&cond, &mutex);
    putchar ('2');
    pthread_mutex_unlock (&mutex);
}
```

Assuming `putchar` displays the character immediately (no buffering), what are the possible outputs of this program when both threads run ?

**A4** ...


## Semaphores

Read about semaphores in _Arpaci-Dusseau Section 31 Semaphores_.
The text is quite long, but it is enough to read only the first six pages,
that is, the introductory part and Sections 31.1, 31.2 and 31.3.

If you would prefer a more condensed text (perhaps you are already familiar with these things),
you can simply take a look at `man sem_wait` and `man sem_post`.


**Q5** Consider the code of the following two threads:

```c
static sem_t sem; // Initialized elsewhere to 1.

void *thread_one_function (void) {
    sem_wait (&sem);
    putchar ('1');
}

void *thread_two_function (void) {
    sem_wait (&sem);
    putchar ('2');
}
```

Assuming `putchar` displays the character immediately (no buffering), what are the possible outputs of this program when both threads run ?

**A5** ...


And now the ever present mandatory summary question (note that it is not called _mandatory_ for nothing, you must come up with a question for the quiz to be accepted):

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** The concepts mentioned above are still present in languages like C# (locks, semaphores) so I am not guessing there was not much change in this since the "discovery" of these, was there? Modern OS utilize these the same way when this was introduced - omit performance improvements.
