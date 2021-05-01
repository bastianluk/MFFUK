// SPDX-License-Identifier: Apache-2.0
// Copyright 2003-2019 Charles University

/*
 * Tests the functionality of kernel malloc and free functions.
 * During the test we repeatedly allocate and release memory using
 * kernel kmalloc and kfree. The allocated chunks of memory must lie
 * in the non-mapped segment of virtual address space and must not
 * overlap. This is tested by writing chunk-specific data into an
 * area after it has been allocated and checking the consistency of
 * the data before releasing it.
 *
 * The test has several phases and subphases. During each phase, the
 * test works with a specific chunk size in three randomized subphases.
 * Each subphase randomly allocates and releases chunks of memory with
 * different probability of allocations. The first phase mostly
 * allocates memory, the middle phase allocates and releases memory
 * with the same probability, and finally the third phases mostly
 * releases memory.
 */

#include "../theap.h"
#include <adt/list.h>
#include <ktest.h>
#include <mm/heap.h>
#include <types.h>

/*
 * The test consists of several phases which differ in the size of blocks
 * they allocate. The size of blocks is given as a range of minimum and
 * maximum allowed size. Each of the phases is divided into 3 subphases which
 * differ in the probability of free and alloc actions. Second subphase is
 * started when malloc returns 'out of memory'. Third subphase is started
 * after a given number of cycles. The third subphase as well as the whole
 * phase ends when all memory blocks are released.
 */

/*
 * Subphase control structures: subphase termination conditions,
 * probabilities of individual actions, subphase control structure.
 */

typedef struct {
    unsigned int max_cycles;
    unsigned int no_memory;
    unsigned int no_allocated;
} sp_term_cond_t;

typedef struct {
    unsigned int alloc;
    unsigned int free;
} sp_action_prob_t;

typedef struct {
    char* name;
    sp_term_cond_t cond;
    sp_action_prob_t prob;
} subphase_t;

/*
 * Phase control structures: The minimum and maximum block size that
 * can be allocated during the phase execution, phase control structure.
 */

typedef struct {
    unsigned int min_block_size;
    unsigned int max_block_size;
    unsigned int top;
} ph_alloc_size_t;

typedef struct {
    char* name;
    ph_alloc_size_t alloc;
    subphase_t* subphases;
} phase_t;

/*
 * Subphases are defined separately here. This is for two reasons:
 * 1) data are not duplicated, 2) we don't have to state beforehand
 * how many subphases a phase contains.
 */
// clang-format off
static subphase_t subphases_32B[] = {
    {
        .name = "Allocation",
        .cond = {
            .max_cycles = 200,
            .no_memory = 1,
            .no_allocated = 0,
        },
        .prob = {
            .alloc = 90,
            .free = 100
        }
    },
    {
        .name = "Alloc/Dealloc",
        .cond = {
            .max_cycles = 200,
            .no_memory = 0,
            .no_allocated = 0,
        },
        .prob = {
            .alloc = 50,
            .free = 100
        }
    },
    {
        .name = "Deallocation",
        .cond = {
            .max_cycles = 0,
            .no_memory = 0,
            .no_allocated = 1,
        },
        .prob = {
            .alloc = 10,
            .free = 100
        }
    },
    {
        .name = NULL // Last entry marker
    }
};

static subphase_t subphases_128K[] = {
    {
        .name = "Allocation",
        .cond = {
             .max_cycles = 0,
             .no_memory = 1,
             .no_allocated = 0,
        },
        .prob = {
            .alloc = 70,
            .free = 100
        }
    },
    {
        .name = "Alloc/Dealloc",
        .cond = {
            .max_cycles = 30,
            .no_memory = 0,
            .no_allocated = 0,
        },
        .prob = {
            .alloc = 50,
            .free = 100
        }
    },
    {
        .name = "Deallocation",
        .cond = {
            .max_cycles = 0,
            .no_memory = 0,
            .no_allocated = 1,
        },
        .prob = {
            .alloc = 30,
            .free = 100
        }
    },
    {
        .name = NULL // Last entry marker
    }
};

static subphase_t subphases_default[] = {
    {
        .name = "Allocation",
        .cond = {
            .max_cycles = 0,
            .no_memory = 1,
            .no_allocated = 0,
        },
        .prob = {
            .alloc = 90,
            .free = 100
        }
    },
    {
        .name = "Alloc/Dealloc",
        .cond = {
            .max_cycles = 200,
            .no_memory = 0,
            .no_allocated = 0,
        },
        .prob = {
            .alloc = 50,
            .free = 100
        }
    },
    {
        .name = "Deallocation",
        .cond = {
            .max_cycles = 0,
            .no_memory = 0,
            .no_allocated = 1,
        },
        .prob = {
            .alloc = 10,
            .free = 100
        }
    },
    {
        .name = NULL // Last entry marker
    }
};

/*
 * Phase definitions.
 */
static phase_t phases[] = {
    {
        .name = "32B memory blocks",
        .alloc = {
            .min_block_size = 32,
            .max_block_size = 32,
            .top = 0
        },
        .subphases = subphases_32B
    },
    {
        .name = "128KB memory blocks",
        .alloc = {
            .min_block_size = 128 * 1024,
            .max_block_size = 128 * 1024,
            .top = 0
        },
        .subphases = subphases_128K
    },
    {
        .name = "2500B memory blocks",
        .alloc = {
            .min_block_size = 2500,
            .max_block_size = 2500,
            .top = 0
        },
        .subphases = subphases_default
    },
    {
        .name = "1B .. 250000B memory blocks",
        .alloc = {
            .min_block_size = 1,
            .max_block_size = 250000,
            .top = 0
        },
        .subphases = subphases_default
    },
    {
        .name = NULL // Last entry marker
    }
};

// clang-format on

inline static unsigned long get_rand(void) {
    static unsigned long random_seed = 12435678;

    random_seed = (random_seed * 873511) % 22348977 + 7;
    return random_seed >> 8;
}

/*
 * Memory accounting: the amount of allocated memory and the
 * number and list of allocated blocks.
 */
static unsigned int mem_allocated;
static unsigned int mem_blocks_count;

static list_t mem_blocks;

typedef struct {
    void* addr; // address of the start of the block
    size_t size; // size of the memory block
    link_t link; // link to other blocks
} mem_block_t;

/**
 * init_mem
 *
 * Initializes the memory accounting structures.
 */
static void init_mem(void) {
    mem_allocated = 0;
    mem_blocks_count = 0;
    list_init(&mem_blocks);
}

/**
 * overlap_match
 * @entry:
 * @data:
 *
 * Match function for tst_list_find in test_overlap. Tests whether a block
 * specified in @data overlaps another block or its control structure
 * referenced by @entry.
 *
 * Returns 0 if the block does not overlap.
 */
typedef struct {
    void* addr;
    size_t size;
} overlap_match_data_t;

static int overlap_match(mem_block_t* mblk, overlap_match_data_t* data) {

    // entry block control structure <mbeg, mend)
    uint8_t* mbeg = (uint8_t*)mblk;
    uint8_t* mend = (uint8_t*)mblk + sizeof(mem_block_t);

    // entry block memory <bbeg, bend)
    uint8_t* bbeg = (uint8_t*)mblk->addr;
    uint8_t* bend = (uint8_t*)mblk->addr + mblk->size;

    // data block <dbeg, dend)
    uint8_t* dbeg = (uint8_t*)data->addr;
    uint8_t* dend = (uint8_t*)data->addr + data->size;

    // check for overlaps
    if ((mbeg >= dbeg && mbeg < dend) || (mend > dbeg && mend <= dend) || (bbeg >= dbeg && bbeg < dend) || (bend > dbeg && bend <= dend)) {
        return 1;
    }

    return 0;
}

/**
 * test_overlap
 * @addr: the initial address of the block
 * @size: the size of the block
 *
 * Tests whether a block starting at @addr overlaps with another, previously
 * allocated memory block or its control structure.
 *
 * Returns 0 if the block does not overlap.
 */
static void test_overlap(void* addr, size_t size) {
    overlap_match_data_t match = {
        .addr = addr,
        .size = size
    };

    list_foreach(mem_blocks, mem_block_t, link, block) {
        ktest_assert(!overlap_match(block, &match),
                "overlap found: block <%pA, %uB> and block %p at <%pA, %uB>",
                addr, size, block, block->addr, block->size);
    }
}

/**
 * checked_malloc
 * @size: the amount of memory to allocate
 *
 * Allocates @size bytes of memory and check whether the chunk comes
 * from the non-mapped memory region and whether the chunk overlaps
 * with other, previously allocated, chunks.
 *
 * Returns NULL if the allocation failed.
 */
static void* checked_malloc(size_t size) {
    void* data = kmalloc(size);
    dprintk("checked_kmalloc(%u) = %pA\n", size, data);
    if (data == NULL) {
        return NULL;
    }

    // Check that returned block is okay and there is no overlap
    ktest_check_kmalloc_result(data, size);
    test_overlap(data, size);

    return data;
}

/*
 * alloc_block
 * @size: the size of the memory block
 *
 * Allocates a block of memory of @size bytes and adds record about it into
 * the mem_blocks list. Returns a pointer to the block holder structure or
 * NULL if the allocation failed.
 */
static mem_block_t* alloc_block(size_t size) {
    mem_block_t* block;

    // Allocate the block holder
    block = (mem_block_t*)checked_malloc(sizeof(mem_block_t));
    if (block == NULL) {
        return NULL;
    }

    // Allocate the block memory
    block->addr = checked_malloc(size);
    if (block->addr == NULL) {
        kfree(block);
        return NULL;
    }

    block->size = size;

    // Register the allocated block
    list_append(&mem_blocks, &block->link);
    mem_allocated += size + sizeof(mem_block_t);
    mem_blocks_count++;

    return block;
}

/**
 * free_block
 * @block: block control structure
 *
 * Frees the block of memory and the block control structure allocated by
 * alloc_block.
 */
static void free_block(mem_block_t* block) {
    // Unregister the block
    list_remove(&block->link);
    mem_allocated -= block->size + sizeof(mem_block_t);
    mem_blocks_count--;

    // Free the memory
    kfree(block->addr);
    kfree(block);
}

/**
 * expected_value
 * @blk: memory block control structure
 * @pos: position in the memory block data area
 *
 * Computes the expected value of a byte located at @pos in memory
 * block described by @blk.
 */
static inline uint8_t expected_value(mem_block_t* blk, uint8_t* pos) {
    return ((unsigned long)blk ^ (unsigned long)pos) & 0xff;
}

/**
 * fill_block
 * @blk: memory block control structure
 *
 * Fills the memory block controlled by @blk with data.
 */
static void fill_block(mem_block_t* blk) {
#ifndef KTEST_HEAP_STRESS_FAST
    uint8_t *pos, *end;

    for (pos = blk->addr, end = pos + blk->size; pos < end; pos++) {
        *pos = expected_value(blk, pos);
    }
#endif
}

/**
 * check_block
 * @blk: memory block control structure
 *
 * Checks whether the block @blk contains the data it was filled with.
 */
static void check_block(mem_block_t* blk) {
#ifndef KTEST_HEAP_STRESS_FAST
    uint8_t *pos, *end;

    for (pos = blk->addr, end = pos + blk->size; pos < end; pos++) {
        ktest_assert(*pos == expected_value(blk, pos),
                "corrupted content at %pA (expected 0x%x, got 0x%x)",
                pos, *pos, expected_value(blk, pos));
    }
#endif
}

/**
 * get_random_block
 *
 * Selects a random memory block from the list of allocated blocks.
 *
 * Returns the block control structure or NULL if the list is empty.
 */
static mem_block_t* get_random_block(void) {
    if (mem_blocks_count == 0) {
        return NULL;
    }

    unsigned int blkidx = get_rand() % mem_blocks_count;

    mem_block_t* block = NULL;
    list_foreach(mem_blocks, mem_block_t, link, it) {
        if (blkidx == 0) {
            block = it;
            break;
        }
        blkidx--;
    }

    ktest_assert(block != NULL, "corrupted list of allocated memory blocks");

    return block;
}

static void do_subphase(phase_t* phase, subphase_t* subphase) {
    unsigned int cycles;
    for (cycles = 0; /* always */; cycles++) {

        if (subphase->cond.max_cycles && cycles >= subphase->cond.max_cycles) {
            /*
             * We have performed the required number of
             * cycles. End the current subphase.
             */
            break;
        }

        /*
         * Decide whether we alloc or free memory in this step.
         */
        unsigned int rnd = get_rand() % 100;
        if (rnd < subphase->prob.alloc) {
            /* We will allocate */
            unsigned int alloc;

            /* Compute a random number lying in interval <min_block_size,max_block_size> */
            alloc = phase->alloc.min_block_size + (get_rand() % (phase->alloc.max_block_size - phase->alloc.min_block_size + 1));

            mem_block_t* blk = alloc_block(alloc);

            if (phase->alloc.top < mem_allocated)
                phase->alloc.top = mem_allocated;

            if (blk == NULL) {
                printk("..  cycle %d (%dB allocated) "
                       "-- failed to allocate %d bytes.\n",
                        cycles, mem_allocated, alloc);
                if (subphase->cond.no_memory) {
                    /* We filled the memory. Proceed to next subphase */
                    break;
                }

            } else {
                printk("..  cycle %d (%dB allocated) "
                       "-- allocated %d bytes.\n",
                        cycles, mem_allocated, alloc);
                fill_block(blk);
            }

        } else if (rnd < subphase->prob.free) {
            /* We will free a memory block */

            mem_block_t* blk = get_random_block();
            if (blk == NULL) {
                printk("..  cycle %d (%dB allocated) "
                       "-- no block to be released.\n",
                        cycles, mem_allocated);
                if (subphase->cond.no_allocated) {
                    /* We free all the memory. Proceed to next subphase. */
                    break;
                }
            } else {
                printk("..  cycle %d (%dB allocated) "
                       "-- released %d bytes.\n",
                        cycles, mem_allocated, blk->size);
                check_block(blk);

                free_block(blk);
            }
        }
    } /* cycles */

    printk("..  finished in %d cycles\n", cycles);
}

static void do_phase(phase_t* phase) {
    for (int subno = 0; phase->subphases[subno].name != NULL; subno++) {
        subphase_t* subphase = &phase->subphases[subno];
        printk(".. Sub-phase #%d (%s)\n", subno + 1, subphase->name);
        do_subphase(phase, subphase);
    }
}

void kernel_test(void) {
    ktest_start("heap/stress");

    init_mem();

    for (int phase_number = 0; phases[phase_number].name != NULL; phase_number++) {
        phase_t* phase = &phases[phase_number];
        printk("Entering phase #%d. (%s)\n", phase_number + 1, phase->name);

        do_phase(phase);

        printk("Phase #%d finished.\n\n", phase_number + 1);
    }

    for (int phase_number = 0; phases[phase_number].name != NULL; phase_number++) {
        printk("Allocated in phase #%d: %d bytes\n", phase_number + 1,
                phases[phase_number].alloc.top);
    }

    ktest_passed();
}
