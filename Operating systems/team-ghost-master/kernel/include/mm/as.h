// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _MM_AS_H
#define _MM_AS_H

#include <errno.h>
#include <types.h>

#define PAGE_SIZE 4096

typedef struct as {
    uint8_t id;
    size_t size;
    uint8_t used_by_count;
} as_t;

void as_init(void);
as_t* as_create(size_t size, unsigned int flags);
size_t as_get_size(as_t* as);
void as_destroy(as_t* as);
errno_t as_get_mapping(as_t* as, uintptr_t virt, uintptr_t* phys);

#endif
