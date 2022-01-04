// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef QUEUE_H_INCLUDED
#define QUEUE_H_INCLUDED

#include "core.h"

template< typename T >
class concurrent_queue_t {
    concurrent_queue_t(const concurrent_queue_t&);
    concurrent_queue_t& operator=(const concurrent_queue_t&);

    typedef T       element_t;
    pthread_mutex_t lock;
    pthread_cond_t  maybe_get;
    pthread_cond_t  maybe_put;
    int             n;
    int             capacity;
    int             head;
    int             tail;
    element_t*      buf;
    int             n_more_get;
    int             n_more_put;
    bool            terminate;

    void calc_abstime(struct timespec* abstime, int msec)
    {
        struct timeval delta;
        delta.tv_sec = msec / 1000;
        delta.tv_usec = (msec - (delta.tv_sec * 1000)) * 1000;
        struct timeval now;
        gettimeofday(&now, NULL);
        struct timeval timeout;
        timeradd(&now, &delta, &timeout);
        abstime->tv_sec = timeout.tv_sec;
        abstime->tv_nsec = timeout.tv_usec * 1000;
    }

public:
    concurrent_queue_t() { }

    void init(int nelts)
    {
        terminate = false;
        n = head = tail = n_more_get = n_more_put = 0;
        capacity = nelts;
        buf = (element_t*)malloc(capacity * sizeof(element_t));
        MTVERIFY(pthread_mutex_init(&lock, NULL));
        MTVERIFY(pthread_cond_init(&maybe_get, NULL));
        MTVERIFY(pthread_cond_init(&maybe_put, NULL));
    }

    void destroy()
    {
        if (!terminate) shutdown();
        MTVERIFY(pthread_mutex_destroy(&lock));
        MTVERIFY(pthread_cond_destroy(&maybe_get));
        MTVERIFY(pthread_cond_destroy(&maybe_put));
        free(buf);
    }

    bool put(element_t datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::put after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        while (n == capacity) {
            n_more_put++;
            MTVERIFY(pthread_cond_wait(&maybe_put, &lock));
            n_more_put--;
            if (terminate) {
                MTVERIFY(pthread_mutex_unlock(&lock));
                return false;
            }
        }
        buf[tail++] = datum;
        n++;
        if (tail == capacity) tail = 0;
        if (n_more_get) MTVERIFY(pthread_cond_signal(&maybe_get));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool put(element_t datum, int msec)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::put after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        while (n == capacity) {
            struct timespec abstime;
            calc_abstime(&abstime, msec);
            n_more_put++;
            int retval = pthread_cond_timedwait(&maybe_put, &lock, &abstime);
            n_more_put--;
            if (retval != EINTR) {
                if (retval == ETIMEDOUT) {
                    MTVERIFY(pthread_mutex_unlock(&lock));
                    return false;
                }
                MTVERIFY(retval);
            }
            if (terminate) {
                MTVERIFY(pthread_mutex_unlock(&lock));
                return false;
            }
        }
        buf[tail++] = datum;
        n++;
        if (tail == capacity) tail = 0;
        if (n_more_get) MTVERIFY(pthread_cond_signal(&maybe_get));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool get(element_t* datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::get after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (n == 0 && terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        while (n == 0) {
            n_more_get++;
            MTVERIFY(pthread_cond_wait(&maybe_get, &lock));
            n_more_get--;
            if (n == 0 && terminate) {
                MTVERIFY(pthread_mutex_unlock(&lock));
                return false;
            }
        }
        *datum = buf[head++];
        n--;
        if (head == capacity) head = 0;
        if (n_more_put) MTVERIFY(pthread_cond_signal(&maybe_put));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool get(element_t* datum, int msec)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::get after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (n == 0 && terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        while (n == 0) {
            struct timespec abstime;
            calc_abstime(&abstime, msec);
            n_more_get++;
            int retval = pthread_cond_timedwait(&maybe_get, &lock, &abstime);
            n_more_get--;
            if (retval != EINTR) {
                if (retval == ETIMEDOUT) {
                    MTVERIFY(pthread_mutex_unlock(&lock));
                    return false;
                }
                MTVERIFY(retval);
            }
            if (n == 0 && terminate) {
                MTVERIFY(pthread_mutex_unlock(&lock));
                return false;
            }
        }
        *datum = buf[head++];
        n--;
        if (head == capacity) head = 0;
        if (n_more_put) MTVERIFY(pthread_cond_signal(&maybe_put));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool try_put(element_t datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::try_put after shutdown\n", __FILE__, __LINE__);
#endif
        if (n == capacity || pthread_mutex_trylock(&lock)) return false;
        if (n == capacity || terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        buf[tail++] = datum;
        n++;
        if (tail == capacity) tail = 0;
        if (n_more_get) MTVERIFY(pthread_cond_signal(&maybe_get));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool wait_lock_try_put(element_t datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::wait_lock_try_put after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (n == capacity || terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        buf[tail++] = datum;
        n++;
        if (tail == capacity) tail = 0;
        if (n_more_get) MTVERIFY(pthread_cond_signal(&maybe_get));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool try_get(element_t* datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::try_get after shutdown\n", __FILE__, __LINE__);
#endif
        if (n == 0 || pthread_mutex_trylock(&lock)) return false;
        if (n == 0 || terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        *datum = buf[head++];
        n--;
        if (head == capacity) head = 0;
        if (n_more_put) MTVERIFY(pthread_cond_signal(&maybe_put));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    bool wait_lock_try_get(element_t* datum)
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::wait_lock_try_get after shutdown\n", __FILE__, __LINE__);
#endif
        MTVERIFY(pthread_mutex_lock(&lock));
        if (n == 0) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return false;
        }
        *datum = buf[head++];
        n--;
        if (head == capacity) head = 0;
        if (n_more_put) MTVERIFY(pthread_cond_signal(&maybe_put));
        MTVERIFY(pthread_mutex_unlock(&lock));
        return true;
    }

    void clear()
    {
#ifndef NDEBUG
        if (terminate) warning("warning:%s:%u concurrent_queue_t::clear after shutdown\n", __FILE__, __LINE__);
#endif
        if (n == 0) return;
        MTVERIFY(pthread_mutex_lock(&lock));
        if (terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return;
        }
        n = 0;
        head = 0;
        if (n_more_put) MTVERIFY(pthread_cond_signal(&maybe_put));
        MTVERIFY(pthread_mutex_unlock(&lock));
    }

    int count()
    {
        return n;
    }

    int limit()
    {
        return capacity;
    }

    void shutdown()
    {
        terminate = true;
        while (n_more_get + n_more_put) {
            MTVERIFY(pthread_mutex_lock(&lock));
            MTVERIFY(pthread_cond_signal(&maybe_get));
            MTVERIFY(pthread_cond_signal(&maybe_put));
            MTVERIFY(pthread_mutex_unlock(&lock));
        }
    }

    bool no_more_get()
    {
        MTVERIFY(pthread_mutex_lock(&lock));
        if (n == 0 && terminate) {
            MTVERIFY(pthread_mutex_unlock(&lock));
            return true;
        }
        MTVERIFY(pthread_mutex_unlock(&lock));
        return false;
    }

    bool no_more_put()
    {
        return terminate;
    }

};

class fifo_buffer_t {

    struct tag_t {
        uint8_t*    buf;
        int         bytes;
        int         bound;
    };

    mutex_t     lock;
    tag_t*      tags;
    int         count;
    uint8_t*    top;
    uint8_t*    bottom;
    uint8_t*    head;
    uint8_t*    tail;

    int add_tag(uint8_t* buf, int bytes)
    {
        lock.verify_locked();
        for (int i = 0; i < count; i++) {
            if (tags[i].buf == NULL) {
                tags[i].buf = buf;
                tags[i].bytes = tags[i].bound = bytes;
                return i;
            }
        }
        int prev = count;
        count = prev + prev / 2 + 1;
        tags = (tag_t*)realloc(tags, sizeof(tag_t) * count);
        if (tags == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
        for (int i = prev; i < count; i++) tags[i].buf = NULL;
        return add_tag(buf, bytes);
    }

    void remove(int id)
    {
        lock.verify_locked();
        uint8_t* buf = tags[id].buf;
        tags[id].buf = NULL;
        if (buf == head) {
            head = fixup(head + tags[id].bound);
            return;
        }
        for (int i = 0; i < count; i++) {
            if (tags[i].buf && fixup(tags[i].buf + tags[i].bound) == buf) {
                tags[i].bound = tags[i].bound + tags[id].bound;
                return;
            }
        }
    }

    void expand_buf(int req)
    {
        lock.verify_locked();
        int psize = bottom - top;
        int nsize = psize + psize / 2 + req;
        uint8_t* p = (uint8_t*)realloc(top, nsize);
        if (p == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
        int offset = p - top;
        top = p;
        bottom = p + nsize;
        tail = tail + offset;
        head = head + offset;
        for (int i = 0; i < count; i++) tags[i].buf = (tags[i].buf ? tags[i].buf + offset : NULL);
        if (tail < head) {
            int n = nsize - psize;
            for (int i = 0; i < count; i++) tags[i].buf = (tags[i].buf >= head ? tags[i].buf + n : tags[i].buf);
            memmove(head + n, head, (bottom - head) - n);
            head = head + n;
        }
    }

    int add_datum(uint8_t* datum, int datum_size)
    {
        lock.verify_locked();
        if (tail == head) {
            head = top;
            tail = top;
        }
        uint8_t* prev = tail;
        if (tail >= head) {
            if (bottom - tail > datum_size) {
                memcpy(tail, datum, datum_size);
                tail = tail + datum_size;
            } else if ((bottom - tail) + (head - top) > datum_size) {
                memcpy(tail, datum, bottom - tail);
                memcpy(top, datum + (bottom - tail), datum_size - (bottom - tail));
                tail = top + datum_size - (bottom - tail);
            } else {
                expand_buf(datum_size);
                return add_datum(datum, datum_size);
            }
        } else if (head - tail > datum_size) {
            memcpy(tail, datum, datum_size);
            tail += datum_size;
        } else {
            expand_buf(datum_size);
            return add_datum(datum, datum_size);
        }
        return add_tag(prev, datum_size);
    }

    inline uint8_t* fixup(uint8_t* p)
    {
        return (p > bottom) ? top + (p - bottom) : p;
    }

public:
    void init(int n) {
        count = n;
        tags = (tag_t*)malloc(sizeof(tag_t) * count);
        if (tags == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
        memset(tags, 0, sizeof(tag_t) * count);
        int bufsize = 128 * n;
        if (bufsize > 4096) bufsize = 4096;
        top = (uint8_t*)malloc(bufsize);
        bottom = top + bufsize;
        head = top;
        tail = top;
        lock.init();
    }

    int put(uint8_t* datum, int datum_size)
    {
        lock.lock();
        int id = add_datum(datum, datum_size);
        lock.unlock();
        return id;
    }

    int size(int id)
    {
        lock.lock();
        assert(tags[id].buf);
        int n = tags[id].bytes;
        lock.unlock();
        return n;
    }

    void get(int id, uint8_t* datum)
    {
        lock.lock();
        assert(datum);
        assert(tags[id].buf);
        if (tags[id].buf + tags[id].bytes > bottom) {
            int n = bottom - tags[id].buf;
            memcpy(datum, tags[id].buf, n);
            memcpy(datum + n, top, tags[id].bytes - n);
        } else {
            memcpy(datum, tags[id].buf, tags[id].bytes);
        }
        remove(id);
        lock.unlock();
    }

    bool empty()
    {
        return tail == head;
    }

    void destroy()
    {
        free(tags);
        free(top);
        lock.destroy();
    }

};

#endif
