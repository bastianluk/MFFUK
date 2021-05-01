// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#include "../tframe.h"
#include <ktest.h>
#include <mm/frame.h>

/*
 * Tests that frame_alloc returns a valid address. We assume
 * there would be always enough free memory to allocate 2 frames
 * when the kernel starts.
 */

void kernel_test(void) {
    ktest_start("frame/basic");

    uintptr_t phys;
    errno_t err = frame_alloc(2, &phys);
    ktest_assert_errno(err, "frame_alloc");
    ktest_check_frame_alloc_result(2, phys);

    ktest_passed();
}
