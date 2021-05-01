// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/bitmap.h>
#include <ktest.h>

/*
 * Bitmap ranges test.
 */

#define SIZE 64

void kernel_test(void) {
    ktest_start("adt/bitmap3");

    uint8_t storage[SIZE / 8 + 1];
    bitmap_t bm;
    bitmap_init(&bm, SIZE, storage);

    size_t start;
    errno_t err = bitmap_find_range(&bm, 4, false, &start);
    ktest_assert_errno(err, "bitmap_find_range");
    ktest_assert(start == 0, "whole bitmap is free");

    bitmap_fill_range(&bm, 2, 7);
    err = bitmap_find_range(&bm, 5, false, &start);
    ktest_assert_errno(err, "bitmap_find_range");
    ktest_assert(start == 9, "has to skip full block");

    err = bitmap_find_range(&bm, 2, false, &start);
    ktest_assert_errno(err, "bitmap_find_range");
    ktest_assert(start == 0, "has to fit before full block");

    ktest_passed();
}
