# Process Synchronization

> Please do not forget to commit your responses in time,
> The deadline for this quiz is Thursday November 5 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


This is the first of three self study modules that will look at the synchronization issues
when using multiple threads for concurrent programming. The goal of this module is to introduce
some situations that require synchronization, and to demonstrate what can go wrong without synchronization.


## Race Conditions

Last week, you could see an example of a situation that requires synchronization
in _Arpaci-Dusseau Section 26 Concurrency Introduction_. This is what it looked
like, slightly adjusted (error checking omitted for brevity):

```c
#include <stdio.h>
#include <pthread.h>

volatile int counter = 0;

#define LOOPS 1000000

void *counter_thread_body (void *arguments) {
    for (int i = 0 ; i < LOOPS ; i ++) {
        counter ++;
    }
    return (NULL);
}

int main (void) {
    pthread_t thread_one, thread_two;

    // Launch two threads that both execute the same body.
    pthread_create (&thread_one, NULL, counter_thread_body, NULL);
    pthread_create (&thread_two, NULL, counter_thread_body, NULL);
    // Wait for the two threads to finish.
    pthread_join (thread_one, NULL);
    pthread_join (thread_two, NULL);

    printf ("Counter value is %i.\n", counter);

    return (0);
}
```

What the example does is it launches two threads and has them execute the same function,
called `counter_thread_body` in the code. The function just increments a shared integer
a million times (by default). Since the counter starts at zero, and the two threads
together execute two million increment operations, we might naively expect
the final counter value to be two million. Let us take a look.


**Q1** Compile and run the example above, with optimization enabled
(we do not want to run an artificially handicapped example, do we :-).

```shell
gcc -O main.c -o main -lpthread
./main
```
Do it a few times and report (roughly) the minimum and maximum counter value you saw printed at the end.

**A1** ...


**Q2** Change the example so that it executes just one hundred loops per thread,
rather than one million. What are the minimum and maximum counter values you
saw printed this time, and why ?

**A2** ...


**Q3** The shared `counter` variable is marked `volatile`.
Remove this keyword, compile with optimization again and run
(do not forget to return the loop counter back to one million).
What are the values you see printed this time, and why ?

**A3** ...


**Q4** Obviously, we were only observing samples of possible behaviors.
Now, imagine the counter increment operation would be internally implemented as
an atomic read of the counter variable to a processor register, followed by
an increment of the value in the processor register, followed by
an atomic write of the register to the counter variable.
(This is actually roughly what happens in reality.)

What do you think is the minimum and maximum _possible_ counter value at the end of the example ?
(In this question, we are asking what could in principle happen, not what is likely to happen.)

**A4** ...


Now that you played with the code a bit, let us add little terminology:

- The situation where the behavior of the program can diverge from expected
  depending on how the threads are scheduled is called _race condition_.

- The regions of the program where a race condition can appear
  are called _critical sections_.


## Locking

Obviously, a race condition can only occur when a critical section is visited
by multiple threads in parallel. One way to work around race conditions is
therefore making sure that there is always at most one thread visiting
the critical section. This can be achieved through locking.

Read about locking in _Arpaci-Dusseau Section 28 Locks_.
This reading is quite long, feel free to skip Sections
28.9, 28.11, 28.15 and 28.16 if you are short on time.


**Q5** In a single processor system, is it ever a good idea to use a spin lock,
or is sleeping always better than spinning ?

**A5** ...


And now the ever present mandatory summary question:

**Q0** Phrase one question you would like to ask related to the topics above,
or the topics from the previous self study blocks
(but not the same as the last time around :-).

**A0** Regarding `05-scheduling-graded` Q5 - how "lucky" does one have to be for that 0000...0 bug not to show it self? The switches happen so often it seems almost impossible for it not to have an effect eventually.