// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _ADT_BITMAP_H
#define _ADT_BITMAP_H

#include <debug.h>
#include <errno.h>
#include <types.h>

/*
 * A simple bit map implementation.
 */

#define BITMAP_BITS_PER_UNIT 8

/** How many bytes (asi in sizeof()) are needed for bitmap of given length.
 *
 * Note that this could be used for static initialization when the bitmap
 * size is known in advance.
 *
 * static bitmap_t bm;
 * static uint8_t[BITMAP_GET_STORAGE_SIZE(256)] storage;
 * ...
 * bitmap_init(&bm, 256, &storage);
 */
#define BITMAP_GET_STORAGE_SIZE(length) (((length) / BITMAP_BITS_PER_UNIT) + 1)

typedef struct {
    size_t length;
    uint8_t* data;
} bitmap_t;

/** Set one bit in bitmap.
 *
 * @param bitmap Bitmap to use.
 * @param index Index to set.
 * @param bit_set New value for given index.
 */
static inline void bitmap_set(bitmap_t* bitmap, size_t index, bool bit_set) {
    assert(bitmap->length > index);

    size_t byte_index = index / BITMAP_BITS_PER_UNIT;
    uint8_t mask = 1 << (index % BITMAP_BITS_PER_UNIT);
    if (bit_set) {
        bitmap->data[byte_index] |= mask;
    } else {
        bitmap->data[byte_index] &= ~mask;
    }
}

/** Get one bit from bitmap.
 *
 * @param bitmap Bitmap to use.
 * @param index Index to get.
 * @return Bit status.
 */
static inline bool bitmap_is_set(bitmap_t* bitmap, size_t index) {
    assert(bitmap->length > index);

    size_t byte_index = index / BITMAP_BITS_PER_UNIT;
    uint8_t mask = 1 << (index % BITMAP_BITS_PER_UNIT);
    return (bitmap->data[byte_index] & mask) == mask;
}

/** Get bitmap size. */
static inline size_t bitmap_get_size(bitmap_t* bitmap) {
    return bitmap->length;
}

void bitmap_init(bitmap_t* bitmap, size_t length, uint8_t* storage);
void bitmap_fill_range(bitmap_t* bitmap, size_t start, size_t count);
void bitmap_clear_range(bitmap_t* bitmap, size_t start, size_t count);
errno_t bitmap_find_range(bitmap_t* bitmap, size_t count, bool bits_set, size_t* start);
bool bitmap_check_range_is(bitmap_t* bitmap, size_t start, size_t count, bool bits_set);

#endif
