#include <stdio.h>
#include <time.h>
#include <pthread.h>

pthread_mutex_t lock;

int main (void) {
    //int digit = 5;
    //printf ("%d", digit);
    //pthread_mutex_init(&lock, NULL);
    //pthread_mutex_lock(&lock);
    //pthread_mutex_unlock(&lock);
    clockid_t clock = CLOCK_REALTIME;
    struct timespec* stamp;
    //clock_gettime(clock, stamp);
    //printf("hello\n");
}