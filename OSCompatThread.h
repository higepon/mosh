/*
 * OSCompatThread.h -
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: OSCompatThread.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_OSCOMPAT_THREAD_
#define SCHEME_OSCOMPAT_THREAD_


#if defined(__APPLE__) || defined(__CYGWIN__)
#define pthread_yield sched_yield
#endif
#include "scheme.h"
#include <sys/time.h> // gettimeofday

// Check sanity
// Boehm GC redirects pthread_create => GC_pthread_create with C macro.
#ifndef pthread_create
#error "pthread_create redirect does not exist"
#endif

// Check sanity
#ifdef __APPLE__
#  ifndef GC_DARWIN_THREADS
#    error "fatal"
#  endif
#elif defined(__FreeBSD__)
#  ifndef GC_FREEBSD_THREADS
#    error "fatal"
#  endif
#elif defined(__linux__)
#  ifdef GC_DARWIN_THREADS
#    error "fatal"
#endif
#  ifndef GC_LINUX_THREADS
#    error "fatal"
#  endif
#endif

#include <time.h>

// If you add new thread support for new operating system
// See and add some macro to scheme.h, just before include gc.h

namespace scheme {

    class Mutex : public gc_cleanup
    {
        friend class ConditionVariable; // share the mutex_
    private:
        pthread_mutex_t mutex_;

    public:
        Mutex()
        {
            pthread_mutexattr_t mattr;
            pthread_mutexattr_init(&mattr);
            pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_RECURSIVE);
            pthread_mutex_init(&mutex_, &mattr);
            pthread_mutexattr_destroy(&mattr);
        }

        ~Mutex()
        {
            pthread_mutex_destroy(&mutex_);
        }

        void lock()
        {
            pthread_mutex_lock(&mutex_);
        }

        void unlock()
        {
            pthread_mutex_unlock(&mutex_);
        }

        bool tryLock()
        {
            return pthread_mutex_trylock(&mutex_) == 0;
        }
    };

    class ConditionVariable : public gc_cleanup
    {
    private:
        pthread_cond_t cond_;
        ucs4string name_;

        void initialize()
        {
            pthread_cond_init(&cond_, NULL);
        }
    public:
        ConditionVariable() : name_(UC(""))
        {
            initialize();
        }

        ConditionVariable(const ucs4string& name) : name_(name)
        {
            initialize();
        }

        virtual ~ConditionVariable()
        {
            pthread_cond_destroy(&cond_);
        }

        ucs4string toString() const
        {
            ucs4string ret = UC("#<condition-variable");
            if (!name_.empty()) {
                ret += UC(" ");
                ret += name_;
            }
            ret += UC(">");
            return ret;
        }

        bool notify()
        {
            int ret = pthread_cond_signal(&cond_);
            return 0 == ret;
        }

        bool notifyAll()
        {
            int ret = pthread_cond_broadcast(&cond_);
            return 0 == ret;
        }

        bool wait(Mutex* mutex)
        {
            int ret = pthread_cond_wait(&cond_, &mutex->mutex_);
            return 0 == ret;
        }

        // returns false if timeout
        bool waitWithTimeout(Mutex* mutex, int msecs)
        {
            struct timeval  now;
            struct timespec timeout;
            if (gettimeofday(&now, NULL) !=0 ) {
                fprintf(stderr,"Fail to get current time\n");
                exit(-1);
            }
            now.tv_usec += msecs * 1000;
            while (now.tv_usec >= 1000000) {
                now.tv_sec++;
                now.tv_usec -= 1000000;
            }
            timeout.tv_sec = now.tv_sec;
            timeout.tv_nsec = now.tv_usec * 1000;
            while (timeout.tv_nsec >= 1000000000) {
                timeout.tv_sec++;
                timeout.tv_nsec -= 1000000000;
            }
            int ret = 0;
            do {
                ret = pthread_cond_timedwait(&cond_, &mutex->mutex_, &timeout);
            } while (ret == EINTR);
            MOSH_ASSERT(ret != EINVAL);
            return ETIMEDOUT != ret;
        }
    };

    class ThreadSpecificKey : public gc_cleanup
    {
    private:
        pthread_key_t key_;
    public:
        ThreadSpecificKey()
        {
            if (pthread_key_create(&key_, NULL) != 0) {
                fprintf(stderr, "fatal : ThreadSpecificKey create\n");
                ::exit(-1);
            }
        }

        virtual~ ThreadSpecificKey()
        {
            if (pthread_key_delete(key_) != 0) {
                fprintf(stderr, "fatal : ThreadSpecificKey delete\n");
                ::exit(-1);
            }
        }

        pthread_key_t key()
        {
            return key_;
        }
    };

    class Thread EXTEND_GC
    {
    private:
        static ThreadSpecificKey* selfKey;
    public:
        static void initialize()
        {
            selfKey = new ThreadSpecificKey;
            // Add dummy
            if (!Thread::setSpecific(selfKey, new Thread)) {
                fprintf(stderr, "fatal : Thread store self\n");
                ::exit(-1);
            }
        }

        static Thread* self()
        {
            volatile void* value = Thread::getSpecific(selfKey);
            MOSH_ASSERT(value != NULL);
            return (Thread*)value;
        }

        static bool setSpecific(ThreadSpecificKey* key, void* value)
        {
            return pthread_setspecific(key->key(), value) == 0;
        }

        static void* getSpecific(ThreadSpecificKey* key)
        {
            return pthread_getspecific(key->key());
        }


        Thread() : lastError_(0)
        {
        }

        bool create(void* (*start)(void*), void* arg);

        bool join(void** returnValue)
        {
            if (GC_pthread_join(thread_, returnValue) == 0) {
                return true;
            } else {
                setLastError();
                return false;
            }
        }

        static void yield()
        {
            pthread_yield();
        }

        static void exit(void* exitValue)
        {
            pthread_exit(exitValue);
        }

    private:
        void setLastError()
        {
            lastError_ = errno;
        }

        pthread_t thread_;
        int lastError_;
    };

}; // namespace scheme

#endif // SCHEME_OSCOMPAT_THREAD_
