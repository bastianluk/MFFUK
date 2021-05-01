// SPDX-License-Identifier: Apache-2.0
// Copyright 2020 Charles University

#include <types.h>

#ifndef KERNEL_PADDING_HEAD
#define KERNEL_PADDING_HEAD 0
#endif

uint32_t _kernel_PADDING_HEAD[KERNEL_PADDING_HEAD] __attribute__((section(".kernel_padding_head")));

#ifndef KERNEL_PADDING_TAIL
#define KERNEL_PADDING_TAIL 0
#endif

uint32_t _kernel_padding_tail[KERNEL_PADDING_TAIL] __attribute__((section(".kernel_padding_tail")));
