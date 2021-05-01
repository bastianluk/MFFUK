# Memory Models

> This is a graded test, do not forget to commit your responses in time !
> The deadline for this test is Thursday November 26 at 12:00 CET (noon).

> Please note that our quiz parser stops at empty lines, if you use empty lines in your answer
> replace them with a line that holds a single dot or use some other visually similar solution.


## Memory Access Optimizations

Consider a simple program where one thread waits until another tells it to go:

```c
bool go = false;

void thread_one_function (void) {
    do_something ();
    go = true;
}

void thread_two_function (void) {
    while (!go) {
        // Busy wait for signal to indicate go.
    }
    do_something_else ();
}
```

Imagine you have a tool that makes it possible to see the transformations performed by the compiler on the program.
When you use the tool on `thread_two_function`, you see that it was transformed into this code:

```c
void thread_two_function (void) {
    if (!go) {
        while (true) {
        }
    }
    do_something_else ();
}
```

**Q1** Explain why.

**A1** It is an attempt at loop inversion presumably - it would normally replace `while(cond)` loop by a `if(cond){do-while(cond})`structure (pseudo code) - `while` and `do-while` are eqvivalent when they both have the same condition and an empty body >> it left the while loop as is, but that is not really the main thing here - it replaced the `do-while(cond)` condition by a constant value because it used the fact that it is initialized to false at compile time and `!go`/`!false` = `true`. It tried to optimize the access to a variable by inserting thee value directly to the call site which in this case broke the code.

**Q2** Suggest a fix so that the program works as intended.

**A2** Making the variable go volotile will tell the compiler that it can change at any time (basically) and it cannot rely on the value stored there on compile time to make optimizations.


## Sequential Consistency

**Q3** Consider these two threads:

```c
int a, b, c;

lock_t lock; // Initialized elsewhere.
semaphore_t semaphore; // Initialized elsewhere to 0.

void thread_one_function (void) {
    a = 1;
    recursive_lock (lock);
    b = 1;
    recursive_unlock (lock);
    c = 1;
    semaphore_post (semaphore);
}

void thread_two_function (void) {
    semaphore_wait (semaphore);
    a = 2;
    recursive_lock (lock);
    b = 2;
    recursive_unlock (lock);
    c = 2;
}
```

What variables, if any, are accessed in a data race ?
Assume simple memory model rules such as those presented during the online lecture.

**A3** if this is the complete program, none of the a,b,c variables are never read from, only written to and it doesnt matter BUT if we are after the result... semaphore is at 0 so if th2 tries to start, it will immediately start waiting and will wait until the th1 gets to semaphore_post meaning th1 will change all the variables to 1, finish, th2 can finally run and it will change the variables to 2. No race.

**Q4** How would the answer to the previous question change if the semaphore were initialized to 1 ?

**A4** th2 could start executing immediately as wait would just let it go immediately. That means that who ever goes second on writing to a - his value will be the last write and it will "stick" (a is in the race). Whoever gets to the lock first also loses the race for b, because the other thread wont be able to run until be is writen to and lock is unlocked (b is also in the race). For c it is the same as for the others - who ever is the last to write, wins the race (ironically - last is the winner). So in the case of semaphore being init to 1 - all of the a,b,c variables are in a data race as we cannot deterministically guarantee the value at the end. 


## Memory Accesses

**Q5** Imagine two implementations of an integer set.
One uses a continuous array of integers with linear search, one a dynamically allocated balanced binary search tree.
The integer size is 4 bytes, the tree node size including headers is 32 bytes, the cache line size is 64 bytes.
Assume memory accesses are by far the most expensive operations when searching the set.
For what set size does a random search in the tree become consistently faster than in the array, assuming an initially empty cache ?

**A5** Linear search complexity is `n`. Balanced binary search tree search complexity and the number of nodes accessed is `log2(n)`. Cache helps the array in the begining (as the question implies) and it can save accesses in chunks of 16 (64/4) numbers. So `n/16` accesses. For the tree the cache with its size limitation (64/32 = 2 ~ place for 2 nodes) doesnt solve the worst case in the search of having a node with 2 subnodes (3 nodes total) and us having to always go to the uncached node (cache only has place for 2 - the one we "are in" and one of the children), meaning the complexity and the number of access stay at `log2(n)`. So we are looking for n from which `n/16 > log2(n)` is consistently satisfied and that is for `n >= 109`. If there was place for root node and both it's children it would cut the accesses to half.