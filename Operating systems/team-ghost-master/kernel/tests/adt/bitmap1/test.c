// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/bitmap.h>
#include <ktest.h>

/*
 * Basic bitmap test. The constants are intentionally too big.
 *
 * We only test that setting bits at bytes boundary do not corrupt surrounding
 * bits.
 */

void kernel_test(void) {
    ktest_start("adt/bitmap1");

    uint8_t storage[4];
    bitmap_t bm;
    bitmap_init(&bm, 17, storage);

    for (size_t i = 0; i < 17; i++) {
        ktest_assert(!bitmap_is_set(&bm, i), "initialization resets all to 0 (%u)", i);
    }

    bitmap_set(&bm, 6, true);
    bitmap_set(&bm, 7, true);
    bitmap_set(&bm, 8, true);
    bitmap_set(&bm, 9, true);

    ktest_assert(!bitmap_is_set(&bm, 0), "index 0 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 1), "index 1 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 2), "index 2 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 3), "index 3 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 4), "index 4 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 5), "index 5 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 10), "index 10 corrupted");
    ktest_assert(!bitmap_is_set(&bm, 11), "index 11 corrupted");
    ktest_assert(bitmap_is_set(&bm, 6), "index 6 not set");
    ktest_assert(bitmap_is_set(&bm, 7), "index 7 not set");
    ktest_assert(bitmap_is_set(&bm, 8), "index 8 not set");
    ktest_assert(bitmap_is_set(&bm, 9), "index 9 not set");

    ktest_passed();
}
