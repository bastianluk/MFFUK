// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include <mm/as.h>
#include <mm/frame.h>
#include <mm/heap.h>
#include <lib/print.h>
#include <debug.h>

#define MAPPINGS_MAX_COUNT 256

uint8_t first_free_id;
uintptr_t mappings[MAPPINGS_MAX_COUNT];

/** Initializes support for address spaces.
 *
 * Called once at system boot
 */
void as_init(void) {
    first_free_id = 0;
}

/** Create new address space.
 *
 * @param size Address space size, must be aligned to PAGE_SIZE.
 * @param flags Flags (unused).
 * @return New address space.
 * @retval NULL Out of memory.
 */
as_t* as_create(size_t size, unsigned int flags) {
    uintptr_t as_start;
    size_t page_count = size / PAGE_SIZE;
    errno_t result = frame_alloc(page_count, &as_start);

    if (result != EOK) {
	    panic("Failed to allocate frames for the new address space");
    }

    as_t* as = kmalloc(sizeof(as_t));

    if (as == NULL) {
	    panic("Failed to allocate heap for the new address space");
    }

    as->size = size;
    as->id = first_free_id;
    as->used_by_count = 0;

    first_free_id++;
    mappings[as->id] = as_start;

    return as;
}

/** Get size of given address space (in bytes). */
size_t as_get_size(as_t* as) {
    return as->size;
}

/** Destroy given address space, freeing all the memory. */
void as_destroy(as_t* as) {
    uintptr_t phys = 0;
    as_get_mapping(as, 8, &phys);
    size_t page_count = as->size / FRAME_SIZE;
    errno_t result = frame_free(page_count, phys - 8);
    panic_if(result != EOK, "Failed to destroy AS %d", as->id);
    mappings[as->id] = 0;
    kfree(as);
}

/** Get mapping between virtual pages and physical frames.
 *
 * @param as Address space to use.
 * @param virt Virtual address, aligned to page size.
 * @param phys Where to store physical frame address the page is mapped to.
 * @return Error code.
 * @retval EOK Mapping found.
 * @retval ENOENT Mapping does not exist.
 */
errno_t as_get_mapping(as_t* as, uintptr_t virt, uintptr_t* phys) {
    if (virt == 0) {
	    return ENOENT;
    }

    if (virt >= as->size) {
	    return ENOENT;
    }

    uintptr_t as_start = mappings[as->id];
    (*phys) = as_start + virt;
    return EOK;
}
