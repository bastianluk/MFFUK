// SPDX-License-Identifier: Apache-2.0
// Copyright 2019 Charles University

#ifndef _KTEST_THREAD_H
#define _KTEST_THREAD_H

#include <main.h>

#define ktest_thread_join(thread, retval) \
    ((dprintk("Joining thread %pT...\n", (thread))), thread_join((thread), (retval)))

#define ktest_thread_join_checked(thread, retval) \
    do { \
        errno_t ___thread_join_err = ktest_thread_join((thread), (retval)); \
        ktest_assert_errno(___thread_join_err, "thread_join(" #thread ")"); \
    } while (0)

#define ktest_thread_create_checked(thread_out, entry_func, entry_arg, flags, name) \
    do { \
        errno_t ___thread_create_err = thread_create( \
                (thread_out), (entry_func), (entry_arg), (flags), (name)); \
        ktest_assert_errno(___thread_create_err, \
                "thread_create(" #thread_out ", " #entry_func "(" #entry_arg "))"); \
        dprintk("Created thread %pT (entry %s(%s), flags %d).\n", \
                *(thread_out), #entry_func, #entry_arg, (flags)); \
    } while (0)

#endif
