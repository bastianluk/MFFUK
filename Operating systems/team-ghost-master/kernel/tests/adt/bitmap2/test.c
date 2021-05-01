// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/bitmap.h>
#include <ktest.h>

/*
 * Bitmap ranges test.
 */

#define SIZE 64

#define CHECK_RANGE(bitmap, start, count, expected) \
    do { \
        bitmap_t* __bitmap = (bitmap); \
        bool __expected = !(!(expected)); \
        size_t __start = (start); \
        size_t __end = __start + (count); \
        for (size_t __i = __start; __i < __end; __i++) { \
            bool __value = bitmap_is_set(__bitmap, __i); \
            ktest_assert(__expected == __value, \
                    "expected %d on index %u but got %d (%pB)", __expected, __i, __value, __bitmap); \
        } \
        ktest_assert(bitmap_check_range_is(__bitmap, __start, __end - __start, __expected), "bitmap_check_range_is is broken"); \
    } while (0)

void kernel_test(void) {
    ktest_start("adt/bitmap2");

    uint8_t storage[SIZE / 8 + 1];
    bitmap_t bm;
    bitmap_init(&bm, SIZE, storage);

    CHECK_RANGE(&bm, 0, SIZE, false);

    bitmap_fill_range(&bm, 4, 7);
    bitmap_fill_range(&bm, 11, 4);

    CHECK_RANGE(&bm, 0, 4, false);
    CHECK_RANGE(&bm, 4, 11, true);
    CHECK_RANGE(&bm, 15, SIZE - 15, false);

    bitmap_clear_range(&bm, 0, SIZE);
    CHECK_RANGE(&bm, 0, SIZE, false);

    bitmap_fill_range(&bm, 4, 19);
    CHECK_RANGE(&bm, 0, 4, false);
    CHECK_RANGE(&bm, 4, 19, true);
    CHECK_RANGE(&bm, 23, SIZE - 23, false);

    bitmap_clear_range(&bm, 0, SIZE);
    CHECK_RANGE(&bm, 0, SIZE, false);

    bitmap_fill_range(&bm, 2, 33);
    bitmap_clear_range(&bm, 9, 21);
    CHECK_RANGE(&bm, 0, 2, false);
    CHECK_RANGE(&bm, 2, 7, true);
    CHECK_RANGE(&bm, 9, 21, false);
    CHECK_RANGE(&bm, 31, 4, true);
    CHECK_RANGE(&bm, 35, SIZE - 35, false);

    ktest_passed();
}
