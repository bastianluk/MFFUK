// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/list.h>
#include <debug.h>
#include <drivers/printer.h>
#include <lib/convert.h>
#include <lib/print.h>
#include <lib/stdarg.h>
#include <types.h>

#define NOT_IMPLEMENTED "Not implemented"
#define STRING_NULL "(null)"
#define NULL_POINTER "(nil)"
#define LIST_T_EMPTY "[empty]"

#define SINT_BUFFER_SIZE 10
#define UINT_BUFFER_SIZE 11
#define UINTPTR_T_BUFFER_SIZE 65

enum LETTER_CASE {
    lowercase,
    uppercase
};

enum HEXA_PREFIX {
    without_hexa_prefix,
    with_hexa_prefix
};

/** Handles format specifier %c of printk function.
 *
 *  @param val Character to display.
 */
static void printk_char(char val) {
    printer_putchar(val);
}

/** Handles format specifier %d of printk function.
 *
 *  @param value Integer to display.
 */
static void printk_int(const int val) {
    char buffer[SINT_BUFFER_SIZE];
    itos(buffer, val);
    printk(buffer);
}

/** Handles format specifier %u of printk function.
 *
 *  @param value Unsigned integer to display.
 */
static void printk_uint(const unsigned int val) {
    char buffer[UINT_BUFFER_SIZE];
    itos(buffer, val);
    printk(buffer);
}

/** Handles format specifier %l of printk function.
 *
 *  @param val Number to display.
 */
static void printk_leading_hexa(const uintptr_t val) {
    char buffer[UINTPTR_T_BUFFER_SIZE];
    const char* iterator = ultoh(buffer, val, without_hexa_prefix, lowercase);

    //leading zeros code
    const char* second_iterator = iterator;
    int counter = 0;
    while (*second_iterator != '\0') {
        counter++;
        second_iterator++;
    }

    for (int i = 0; i < 8 - counter; i++) {
        printer_putchar('0');
    }
    printk(iterator);
}

/** Handles format specifier %x of printk function.
 *
 *  @param val Unsigned long to display.
 *  @param prefix_flag Determines whether the output contains '0x' at the beginning.
 *  @param case_flag Determines whether the output string is lowercase or uppercase.
 */
static void printk_hexa(const uint32_t val, int prefix_flag, int case_flag) {
    char buffer[65];
    const char* iterator = ultoh(buffer, val, prefix_flag, case_flag);
    printk(iterator);
}

/** Hadnles format specifier %s of printk function.
 *
 *  @param str String to display.
 */
static void printk_string(const char* str) {
    if (str != NULL)
    {
        while (*str != '\0')
        {
            printk_char(*str);
            str++;
        }
    }
    else
    {
        printk(STRING_NULL);
    }

}

/** Handles format specifier %p of printk function.
 *
 *  @param p Void pointer to display as hexadecimal string.
 */
static void printk_void_p(const void* p) {
    if (p == NULL) {
	printk("%s", NULL_POINTER);
    } else {
    	printk_hexa((uint32_t)p, with_hexa_prefix, lowercase);
    }
}

/** Handles format specifier %pL of printk function.
 *
 *  @param list Linked list to display.
 */
static void printk_list_t(list_t* list) {
    printk("%p", list);

    if (list_is_empty(list)) {
        printk(LIST_T_EMPTY);
    } else {
        int list_size = list_get_size(list);
        printk("[%d: ", list_size);
        int counter = 0;
        list_foreach(*list, list_t, head, item) {
            printk("%p", item);
            counter++;

            if (counter != list_size) {
                printer_putchar('-');
            }
        }
        printer_putchar(']');
    }
}

/** Handles pointer format specifier %pF of printk function.
 *
 *  @param val First four instruction from this.
 */
static void printk_pointer_function(const uintptr_t* ptr) {
    printk("0x%l[%l %l %l %l]",
            (void*)ptr,
            *ptr,
            *(ptr + 1),
            *(ptr + 2),
            *(ptr + 3));
}

//Supported format specifiers.
typedef enum format_specifier {
    character,
    sint,
    uint,
    pointer,
    string,
    leading_hexa,
    lowercase_hexa,
    uppercase_hexa,
    pointer_list_t,
    pointer_thread_t,
    pointer_function
} format_t;

char format_keys[256] = {
    ['c'] = character,
    ['d'] = sint,
    ['l'] = leading_hexa,
    ['p'] = pointer,
    ['s'] = string,
    ['u'] = uint,
    ['x'] = lowercase_hexa,
    ['F'] = pointer_function,
    ['L'] = pointer_list_t,
    ['T'] = pointer_thread_t,
    ['X'] = uppercase_hexa
};

/** Calls corresponding handler function for the given pointer type format specifier.
 *
 *  @param argp Pointer to variadic arguments from simple_printf function.
 *  @param format Current format specifier string.
 *  @retval 1 format is a single char pointer type
 *          2 format is a correct compound pointer type
 */
static int process_pointer_format(va_list* argp, char second) {
    switch (format_keys[(int)second]) {
    case pointer_function:
        printk_pointer_function(va_arg(*argp, const uintptr_t*));
        return 2;
    case pointer_list_t:
        printk_list_t(va_arg(*argp, list_t*));
        return 2;
    default:
        return 1;
    }
}

/** Calls corresponding handler function for the given format specifier (if correct).
 *
 *  @param argp Pointer to variadic arguments from simple_printf function.
 *  @param format_spec Current format specifier string.
 *  @param it Iterator of user entered string.
 *  @retval 0 success
 *          1 incorrect single char format specifier
 */
static int process_format(va_list* argp, char first, char second) {
    int result;
    switch (format_keys[(int)first]) {
    case character:
        printk_char((char)va_arg(*argp, const int));
        return 0;
    case sint:
        printk_int(va_arg(*argp, const int));
        return 0;
    case uint:
        printk_uint(va_arg(*argp, const unsigned int));
        return 0;
    case leading_hexa:
        printk_leading_hexa(va_arg(*argp, const uintptr_t));
        return 0;
    case lowercase_hexa:
        printk_hexa(va_arg(*argp, const unsigned int), without_hexa_prefix, lowercase);
        return 0;
    case uppercase_hexa:
        printk_hexa(va_arg(*argp, const unsigned int), without_hexa_prefix, uppercase);
        return 0;
    case pointer:
        result = process_pointer_format(argp, second);
        if (result == 1) {
            printk_void_p(va_arg(*argp, const void*));
            return 0;
        } else {
            return result;
        }
    case string: {
        const char* test = va_arg(*argp, const char*);
        printk_string(test);
        return 0;
    }
    default:
        return 1;
    }
}

/** Prints given formatted string to console.
 *
 * @param format printf-style formatting string.
 */
void printk(const char* format, ...) {
    va_list argp;
    va_start(argp, format);

    char first_next, second_next;
    int result;
    const char* it = format;
    while (*it != '\0') {
        //common case
        if (*it != '%') {
            printer_putchar(*it);
        } else {
            //read first two chars
            first_next = *(++it);

            if (first_next == '%') {
                printer_putchar('%');
            } else {
                second_next = *(++it);

                //parse the format specifier
                result = process_format(&argp, first_next, second_next);

                //single char format
                if (result == 0) {
                    it--;
                }

                if (result == 1) {
                    puts(NOT_IMPLEMENTED);
                }
            }
        }
        it++;
    }
    va_end(argp);
}

/** Prints given string to console, terminating it with newline.
 *
 * @param s String to print.
 */
void puts(const char* s) {
    while (*s != '\0') {
        printer_putchar(*s);
        s++;
    }
    printer_putchar('\n');
}
