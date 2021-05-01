#ifndef _MM_HEAP_SHARED_H
#define _MM_HEAP_SHARED_H

#include <types.h>

#define INSUFFICIENT_HEAP_SIZE_INIT "Insufficient memory was allocated for the heap."

static const int heap_header_size = sizeof(size_t) * 3; 
static const int heap_clean_header_size = heap_header_size - sizeof(size_t);
static const int heap_footer_size = sizeof(size_t);

static const int heap_block_meta_size = heap_header_size + heap_footer_size;

#endif
