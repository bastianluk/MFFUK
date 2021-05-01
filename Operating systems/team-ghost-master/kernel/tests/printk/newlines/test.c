// SPDX-License-Identifier: Apache-2.0
// Copyright 2020 Charles University

#include <ktest.h>
#include <lib/print.h>
#include <types.h>

void kernel_test(void) {
    puts(KTEST_BLOCK_EXPECTED "This is first line.");
    puts(KTEST_BLOCK_EXPECTED "This is 2nd line.");
    printk("This is %s line.\nThis is %dnd line.\n", "first", 2);

    puts(KTEST_BLOCK_EXPECTED "This is another block of lines.");
    puts(KTEST_BLOCK_EXPECTED "This is second line.");
    puts(KTEST_BLOCK_EXPECTED "This is third line.");
    printk("This is another block of lines.%s", "\nThis is second line.\nThis is third line.\n");

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line.");
    printk(KTEST_ACTUAL "Check that no percent sign forces %s.\n", "new-line");

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: 42.");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %d.\n", 42);

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: 42.");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %u.\n", 42);

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: 2a.");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %x.\n", 42);

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: 2A.");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %X.\n", 42);

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: !.");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %c.\n", '!');

    puts(KTEST_EXPECTED "Check that no percent sign forces new-line: (nil).");
    printk(KTEST_ACTUAL "Check that no percent sign forces new-line: %p.\n", NULL);

    ktest_passed();
}
