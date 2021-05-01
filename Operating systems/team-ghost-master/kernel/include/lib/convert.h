#ifndef CONVERTS_H_
#define CONVERTS_H_

/** Declaration of functions needed to numbers to string */

extern void itos(char* buffer, long long int number);
extern char* ultoh(char* buffer, unsigned long number, int hexa_prefix_flag, int case_flag);

#endif
