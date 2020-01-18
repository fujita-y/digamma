// Copyright (c) 2004-2020 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef SPINLOCK_H_INCLUDED
#define SPINLOCK_H_INCLUDED

#include "core.h"

class spinlock_t {
    spinlock_t(const spinlock_t&);
    spinlock_t& operator=(const spinlock_t&);
#if MTDEBUG
    int lock_count;
#endif
    volatile int32_t spinlock;

    int32_t
    interlocked_compare_exchange(volatile int32_t* target, int32_t exchange, int32_t compare)
    {
        int32_t prev;
        __asm__ __volatile__("lock; cmpxchgl %1, %2"
                                : "=a" (prev)
                                : "r" (exchange), "m" (*target), "0" (compare)
                                : "memory");
        return prev;
    }

public:
    spinlock_t() { /* should be null */ }

    void init(bool recursive = false)
    {
        assert(recursive == false);
        spinlock = 0;
#if MTDEBUG
        lock_count = 0;
#endif
    }

    void destroy() { }

    void lock()
    {
        if (spinlock) sched_yield();
        while (interlocked_compare_exchange(&spinlock, 1, 0) != 0) sched_yield();
#if MTDEBUG
        lock_count++;
#endif
    }

    void unlock()
    {
#if MTDEBUG
        lock_count--;
        assert(lock_count >= 0);
#endif
      spinlock = 0;
      MEM_STORE_FENCE; // do not remove this, digamma assume memory barrier here
    }

    void verify_locked()
    {
#if MTDEBUG
        if (lock_count == 0) {
            fatal("internal error:%s:%u verify_locked() failed.", __FILE__, __LINE__);
        }
#endif
    }
};

class scoped_spinlock {
    scoped_spinlock(const scoped_spinlock&);
    scoped_spinlock& operator=(const scoped_spinlock&);
    spinlock_t& m_lock;

public:
    scoped_spinlock(spinlock_t& lock) : m_lock(lock) { m_lock.lock(); }
    ~scoped_spinlock() { m_lock.unlock(); }
};

#endif
