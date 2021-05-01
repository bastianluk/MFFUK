// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/bitmap.h>

/** Initializes bitmap with given data storage.
 *
 * @param bitmap Bitmap to initialize.
 * @param length Number of items in this bitmap.
 * @param storage Where actual data will be stored, must be at least BITMAP_GET_STORAGE_SIZE big.
 */
void bitmap_init(bitmap_t* bitmap, size_t length, uint8_t* storage) {
    bitmap->length = length;
    bitmap->data = storage;
    size_t size = length / BITMAP_BITS_PER_UNIT;
    for (size_t i = 0; i < size; i++) {
        bitmap->data[i] = 0;
    }
}

static inline void apply_mask(bitmap_t* bitmap, size_t unit_index, unsigned int mask, bool bits_set) {
    if (bits_set) {
        bitmap->data[unit_index] |= mask;
    } else {
        bitmap->data[unit_index] &= ~mask;
    }
}

static void bitmap_set_range(bitmap_t* bitmap, size_t start, size_t count, bool bits_set) {
    size_t first_unit_index = start / BITMAP_BITS_PER_UNIT;
    size_t last_unit_index = (start + count) / BITMAP_BITS_PER_UNIT;

    if (first_unit_index == last_unit_index) {
        unsigned int mask = ((1 << count) - 1) << (start % BITMAP_BITS_PER_UNIT);
        apply_mask(bitmap, first_unit_index, mask, bits_set);
        return;
    }

    unsigned int first_mask = (~0U) << (start % BITMAP_BITS_PER_UNIT);
    apply_mask(bitmap, first_unit_index, first_mask, bits_set);
    for (size_t unit_index = first_unit_index + 1; unit_index < last_unit_index; unit_index++) {
        apply_mask(bitmap, unit_index, ~0U, bits_set);
    }
    unsigned int last_mask = (1 << ((start + count) % BITMAP_BITS_PER_UNIT)) - 1;
    apply_mask(bitmap, last_unit_index, last_mask, bits_set);
}

/** Fills given range with ones.
 *
 * Note that this function does not check previous values on modified indices.
 *
 * @param bitmap Bitmap to use.
 * @param start First index to set.
 * @param count Number of bits to set to 1.
 */
void bitmap_fill_range(bitmap_t* bitmap, size_t start, size_t count) {
    bitmap_set_range(bitmap, start, count, true);
}

/** Fills given range with zeros.
 *
 * Note that this function does not check previous values on modified indices.
 *
 * @param bitmap Bitmap to use.
 * @param start First index to set.
 * @param count Number of bits to set to 0.
 */
void bitmap_clear_range(bitmap_t* bitmap, size_t start, size_t count) {
    bitmap_set_range(bitmap, start, count, false);
}

/** Tries to find uninterrupted sequence of given values.
 *
 * @param bitmap Bitmap to use.
 * @param count How long the sequence shall be.
 * @param bits_set Whether the sequence shall consist of ones or zeroes.
 * @param start Where to store the first available index.
 * @return Error code.
 * @retval EOK Sequence found, start contains valid index.
 * @retval ENOENT Sequence of given length not found.
 */
errno_t bitmap_find_range(bitmap_t* bitmap, size_t count, bool bits_set, size_t* start) {
    if (count > bitmap->length) {
        return ENOENT;
    }
    size_t i = 0;
    bits_set = !!bits_set;
    while (i < bitmap->length - count + 1) {
        size_t current_size = 0;
        while (current_size < count) {
            bool state = bitmap_is_set(bitmap, i + current_size);
            if (state == bits_set) {
                current_size++;
            } else {
                break;
            }
        }
        if (current_size == count) {
            *start = i;
            return EOK;
        }
        i = i + current_size + 1;
    }
    return ENOENT;
}

/** Checks that given range is full filled or cleared.
 *
 * Note that this function is slow. Recommended to use in assertions only.
 *
 * @param bitmap Bitmap to use.
 * @param start First index to check.
 * @param count Number of bits to check.
 * @param bits_set Expected value.
 * @return Whether the sequence contains given value.
 */
bool bitmap_check_range_is(bitmap_t* bitmap, size_t start, size_t count, bool bits_set) {
    bits_set = !!bits_set;
    for (size_t i = start; i < start + count; i++) {
        if (bits_set != bitmap_is_set(bitmap, i)) {
            return false;
        }
    }
    return true;
}
