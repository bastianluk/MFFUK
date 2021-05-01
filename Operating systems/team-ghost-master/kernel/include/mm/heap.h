// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _MM_HEAP_H
#define _MM_HEAP_H

#include <types.h>

/** Header structure for one block of the heap
 *
 *  @field is_free Inidicator if the heap block is free (holds no data) or not free (holds data).
 *  @field data_size Byte size of the data stored in the block.
 *  @field Property "to point to" so we have an address of the raw data segment we can return.
 */
typedef struct heap_block_header {
	size_t is_free;
	size_t data_size;
	size_t data_block[0];
} heap_block_header_t;

void heap_init(void);
void* kmalloc(size_t size);
void kfree(void* ptr);
size_t get_available_memory_size(void);

#endif
