// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <adt/bits.h>
#include <mm/frame.h>
#include <types.h>
#include <main.h>
#include <adt/bitmap.h>
#include <mm/heap.h>

bitmap_t metadata;
uintptr_t physical_memory_start;

static uintptr_t get_memory_from_index(int index) {
    return  0x7fffffff & (physical_memory_start + index * FRAME_SIZE);
}

static int64_t  get_index_from_memory(uintptr_t mem) {
    int32_t offset = (mem | 0x80000000) - physical_memory_start;
    if (offset < 0 || offset % FRAME_SIZE != 0) {
        return -1;
    }
    return offset / FRAME_SIZE;
}

/**
 * Initializes frame allocator.
 *
 * Called once at system boot.
 */
void frame_init(void) {
    uintptr_t kernel_end = ((uintptr_t)&_kernel_end) + (FRAME_SIZE - ((uintptr_t)&_kernel_end % FRAME_SIZE));
    size_t memory_size = get_available_memory_size();
    uintptr_t memory_end = kernel_end + memory_size;
    size_t memory_available = memory_end - kernel_end;
    size_t total_page_count = memory_available / FRAME_SIZE - 1;
    //printk("MEMORY OF %d PAGES\n", total_page_count);
    
    uint8_t* storage = (uint8_t*)kernel_end;
    physical_memory_start = kernel_end + FRAME_SIZE;
    bitmap_init(&metadata, total_page_count, storage);
}
/**
 * Allocate continuous sequence of physical frames.
 *
 * The allocated frames can be returned by frame_free.
 *
 * Note that frame_free must be told number of frames to return
 * and frame allocator does not need to remember the size by
 * itself.
 *
 * @param count How many frames to allocate.
 * @param phys Where to store physical address of the first frame in sequence.
 * @return Error code.
 * @retval EOK Frames allocated.
 * @retval ENOMEM Not enough memory.
 * @retval EINVAL Invalid count.
 */
errno_t frame_alloc(size_t count, uintptr_t* phys) {
    size_t free_index;
    errno_t result = bitmap_find_range(&metadata, count, false, &free_index);
    // printk("free index at %d Page\n", free_index);

    if (result == ENOENT) {
        return ENOMEM;
    }

    bitmap_fill_range(&metadata, free_index, count); 
    *phys = get_memory_from_index(free_index);

    return EOK;
}

/**
 * Free continuous sequence of physical frames.
 *
 * The returned frames were previously allocated by frame_alloc.
 *
 * @param count How many frames to free.
 * @param phys Physical address of the first frame in sequence.
 * @return Error code.
 * @retval EOK Frames freed.
 * @retval ENOENT Invalid frame address.
 * @retval EBUSY Some frames were not allocated (double free).
 */
errno_t frame_free(size_t count, uintptr_t phys) {
    int64_t index_result = get_index_from_memory(phys);
    if (index_result == -1) {
        return ENOENT;
    }

    bool is_allocated = bitmap_check_range_is(&metadata, (size_t)index_result, count, true);
    if (!is_allocated) {
        return EBUSY;
    }

    bitmap_clear_range(&metadata, (size_t)index_result, count);
    return EOK;
}
