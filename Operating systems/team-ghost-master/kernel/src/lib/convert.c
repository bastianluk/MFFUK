#define INT_MAX 2147483647
#define INT_MIN -2147483648

/** Determines the case.
 *
 *  @param Integer value that indicates if the case is lowercase(0) or uppercase(1).
 */
static char select_case(int case_flag) {
    return (case_flag == 1) ? 'A' : 'a';
}

/** Converts unsigned long number to a hexadecimal string.
 *
 *  @param Specified number as a string.
 *  @param num Number to convert.
 *  @param hexa_prefix_flag Integer value that indicates if the string contains "0x" at the beginning.
 *  @param case_flag Integer value that indicates if the string is in lowercase or uppercase.
 *  @retval Start of the hexadecimal string from unsigned long in the buffer.
 */
char* ultoh(char* buffer, unsigned long number, int hexa_prefix_flag, int case_flag);

char* ultoh(char* buffer, unsigned long number, int hexa_prefix_flag, int case_flag) {
    char* it = &buffer[64];
    *it = '\0';

    //handle case
    char case_char = select_case(case_flag);

    //conversion
    int temp = 0;
    while (number != 0) {
        temp = number % 16;
        if (temp < 10) {
            *--it = temp + '0';
        } else {
            *--it = temp - 10 + case_char;
        }
        number /= 16;
    }

    if (hexa_prefix_flag == 1) {
        *--it = 'x';
        *--it = '0';
    }

    return it;
}

/** Fills buffer with a string representation of integer zero.
 *
 *  @param buffer Integer zero as a string.
 */
static void itos_zero(char* buffer) {
    buffer[0] = '0';
    buffer[1] = '\0';
}

/** Fills buffer with a string representation of the specified positive integer.
 *
 *  @param buffer Specified number as a string.
 *  @param number Number to convert.
 *  @param length Length of the specified number.
 */
static void itos_positive(char* buffer, long long int number, int length) {
    int i, remainder;
    for (i = 0; i < length; i++) {
        remainder = number % 10;
        number /= 10;
        buffer[length - (i + 1)] = remainder + '0';
    }
    buffer[length] = '\0';
}

/** Counts digits in an integer.
 *  @param number Number to count digits in.
 *  @retval Number of digits in the specified number.
 */
static int intlen(long long int number) {
    return (number == 0) ? 0 : 1 + intlen(number / 10);
}

/** Fills buffer with a string representation of the specified negative integer.
 *
 *  @param buffer Specified number as a string.
 *  @param number Number to convert.
 *  @param length Length of the specified number.
 */
static void itos_negative(char* buffer, long long int number, int length) {
    buffer[0] = '-';

    int int_min_flag = 0;
    if (number == INT_MIN) {
        number = INT_MAX;
        int_min_flag = 1;
    } else {
        number *= -1;
    }

    int i, remainder;
    for (i = 0; i < length; i++) {
        remainder = number % 10;
        number /= 10;
        buffer[length - (i + 1) + 1] = remainder + '0';
    }

    if (int_min_flag == 1) {
        buffer[10] = '8';
    }

    buffer[length + 1] = '\0';
}

/** Converts an integer to a string.
 *
 *  @param buffer An array where is the converted string stored.
 *  @param number An integer to convert.
 */
void itos(char*, long long int);

void itos(char* buffer, long long int number) {
    int length = intlen(number);

    if (number == 0) {
        itos_zero(buffer);
    } else {
        (number < 0) ? itos_negative(buffer, number, length) : itos_positive(buffer, number, length);
    }
}
