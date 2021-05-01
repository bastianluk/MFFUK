// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _MM_TLB_H
#define _MM_TLB_H

#include <exc.h>

#define TLB_ENTRY_COUNT 48

void handle_tlb_refill(eh_context_t* eh_context);

#endif
