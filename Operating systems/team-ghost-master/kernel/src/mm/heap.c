// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <lib/print.h>
#include <main.h>
#include <mm/heap.h>
#include <mm/heap_err.h>
#include <mm/heap_shared.h>
#include <exc.h>
#include <mm/frame.h>

/** OVERVIEW
 *  - setup
 *  The memory is scanned and the start and end (limits/addresses) are setup, then a big block of free memory is created
 *  It will eventually be split into the requested chunks by kmalloc().
 *  - kmalloc()
 *  Starts at the begining of the heap and tries to best fit the block to return, inefficient in that it doesn't skip the already NOT_FREE blocks.
 *  - kfree()
 *  Marks correctly the block someone wants to free up and then coaleses the neighboaring blocks into one big block of free memory.
 *  The inefficiency/problem here is that no "magic number"/checks of the memory blocks are present.
 */

#define FREE 1
#define NOT_FREE 0

// The first heap memory address in the address space
uintptr_t* heap_start_address;
// The last heap memory address in the address space
uintptr_t* heap_end_address;
// The size of the available memory to be used.
size_t memory_size;
bool memoty_size_computed;

static size_t full_block_size(heap_block_header_t* block) {
    return heap_block_meta_size + block->data_size;
}

/** Iterate over memory after kernel ends and check if we can still write,
 *  because that is the memory we can use for the heap
 */
size_t get_available_memory_size(void) {
    if (memoty_size_computed)
    {
        return memory_size;
    }
    
    bool saved = interrupts_disable();
    size_t heap_size = 0;

    // volatile to disable optimalizations
    volatile uint8_t *p = (uint8_t*)&_kernel_end;
    volatile uint8_t value;

    uint8_t value_to_write = 0;
    while(true) {
        value = *p;

        // coincidentally the same value is already there
        if(value == value_to_write) {
            value_to_write++;
        }

        *p = value_to_write;

        // check if we can still write
        if(*p == value_to_write) {
            p++;
            heap_size++;
        } else {
            break;
        }
    }
    memory_size = heap_size;
    memoty_size_computed = true;

    interrupts_restore(saved);

    return heap_size;
}

/** Initialize heap block meta data by creating header directly before data and footer directly after data.
 *
 *  @param block_header Pointer to the start of the header.
 *  @param is_free Determines if the block contains data.
 *  @param data_size Size of currently allocated data.
 */
static void heap_block_init(heap_block_header_t* block_header, size_t is_free, size_t data_size) {
    //Header init
    block_header->is_free = is_free;
    block_header->data_size = data_size;

    //Footer init
    size_t* block_footer = (size_t*)((uintptr_t)block_header + sizeof(size_t) * 3 + data_size);
    *block_footer = data_size;
}

/** Heap initialization.
 *
 * Called once during system boot.
 */
void heap_init(void) {
    size_t memory_size = get_available_memory_size();
    size_t page_count = memory_size / FRAME_SIZE;
    size_t heap_page_count = 3 + page_count / 5; // Heap has eight of the memory.
    uintptr_t first_page;
    frame_alloc(heap_page_count, &first_page);
    heap_start_address = (void*)(first_page | 0x80000000);
    size_t heap_memory_size = heap_page_count * FRAME_SIZE;
    heap_end_address = (void*)((uintptr_t)heap_start_address + heap_memory_size);
    size_t heap_data_size = heap_memory_size - heap_block_meta_size;

    if(heap_init_size_check(heap_start_address, heap_end_address) == 1) {
        printk(INSUFFICIENT_HEAP_SIZE_INIT);
    }

    heap_block_init((heap_block_header_t*)heap_start_address, FREE, heap_data_size);
}

/** Returns very next heap block.
 *
 *  @param current Pointer to heap block from which is the next one calculated.
 */
static heap_block_header_t* next_block(heap_block_header_t* current) {
    uintptr_t next_address = (uintptr_t)current + full_block_size(current);
    uintptr_t end = (uintptr_t)heap_end_address;
    if (next_address < end) {
        return (heap_block_header_t*)next_address;
    }

    return NULL;
}

/** Returns very preavious heap block.
 *
 *  @param current Pointer to heap block from which is the previous one calculated.
 */
static heap_block_header_t* prev_block(heap_block_header_t* current) {
    if ((uintptr_t*)current <= heap_start_address) {
        return NULL;
    }

    uintptr_t prev_block_footer = (uintptr_t)current - heap_footer_size;
    size_t* prev_block_data_size = (size_t*)prev_block_footer;
    uintptr_t prev_block_address = (uintptr_t)current - *prev_block_data_size - heap_block_meta_size;
    return (heap_block_header_t*)prev_block_address;
}

/** Performs splitting on the specified block.
 *
 *  @param original Heap block to split.
 *  @param size Required size.
 */
static void split_block(heap_block_header_t* original, size_t size) {
    size_t next_block_size_requirement = heap_block_meta_size + size;
    size_t split_condition = next_block_size_requirement < original->data_size;

    if (split_condition) {
        size_t original_size = original->data_size;
        heap_block_init(original, NOT_FREE, size);
        heap_block_header_t* next = next_block(original);

        if (next == NULL) {
            return;
        }

        size_t next_size = original_size - next_block_size_requirement;
        heap_block_init(next, FREE, next_size);

        return;
    }

    original->is_free = NOT_FREE;
}

/** Merge free space from second block to first block.
 *
 *  @param first First heap block.
 *  @param second Second heap block.
 */
static void merge_block(heap_block_header_t* first, heap_block_header_t* second) {
    size_t merge_block_size = first->data_size + second->data_size + heap_block_meta_size;
    heap_block_init(first, FREE, merge_block_size);
}

/** Allocate size bytes on heap.
 *
 * @param size Amount of memory to allocate.
 * @return Pointer to allocated memory.
 * @retval NULL Out of memory.
 */
void* kmalloc(size_t size) {
    bool saved = interrupts_disable();

    size_t aligned_size = size + ((4 - size % 4) % 4);
    heap_block_header_t* best_fit_block_header = NULL;
    heap_block_header_t* block_header = (heap_block_header_t*)heap_start_address;
    while (block_header != NULL) {
        if (block_header->is_free) {
            if (block_header->data_size >= aligned_size) {
                if (best_fit_block_header == NULL || block_header->data_size < best_fit_block_header->data_size) {
                    best_fit_block_header = block_header;
                }

                if (best_fit_block_header->data_size == aligned_size) {
                    break;
                }
            }
        }

        block_header = next_block(block_header);
    }

    if (best_fit_block_header != NULL) {
        split_block(best_fit_block_header, aligned_size);

        interrupts_restore(saved);

        return (void*)(&(best_fit_block_header->data_block));
    }

    interrupts_restore(saved);

    return NULL;
}

/** Free memory previously allocated by kmalloc.
 *
 * @param ptr Pointer to memory to be freed.
 */
void kfree(void* ptr) {
    bool saved = interrupts_disable();

    heap_block_header_t* block_header = (heap_block_header_t*)((uintptr_t)ptr - heap_clean_header_size);
    block_header->is_free = FREE;
    heap_block_header_t* next_block_header = next_block(block_header);
    heap_block_header_t* prev_block_header = prev_block(block_header);

    if (next_block_header != NULL && next_block_header->is_free)
    {
        merge_block(block_header, next_block_header);
    }

    if (prev_block_header != NULL && prev_block_header->is_free)
    {
        merge_block(prev_block_header, block_header);
    }

    interrupts_restore(saved);
}
