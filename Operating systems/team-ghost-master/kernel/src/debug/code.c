// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <debug/code.h>
#include <lib/print.h>

/** Dump function code at given address.
 *
 * Generally, the output should look like disassembly without
 * mnemonics part.
 *
 * @param name Function name to print in header.
 * @param address Function address.
 * @instruction_count How many instructions to print.
 */
void debug_dump_function(const char* name, uintptr_t address, size_t instruction_count) {
    printk("%x <%s>:\n", address, name);

    //size of the name + <>
    int name_size = 2;
    const char* it = name;
    while (*it != '\0') {
        name_size++;
        it++;
    }

    //create a string full of spaces
    char spaces[name_size + 1];
    for (int i = 0; i < name_size; i++)
        spaces[i] = ' ';
    spaces[name_size] = '\0';

    for (size_t i = 0; i < instruction_count; i++) {
        uintptr_t ptr = address + i * sizeof(size_t);
        printk("%x:%s%l\n", ptr, spaces, *((uintptr_t*)ptr));
    }
}
