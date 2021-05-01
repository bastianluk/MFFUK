#include <mm/heap_err.h>

/** Determines if there is a sufficient amount of memory for the heap.
 *  
 *  @param heap_start_address First address of the heap in the memory.
 *  @param heap_end_address Last address of the heap in the memory.
 */
int heap_init_size_check(uintptr_t *heap_start_address, uintptr_t *heap_end_address) {
	size_t heap_size = (size_t)heap_end_address - (size_t)heap_start_address;	
	return (heap_size < heap_block_meta_size);
}
