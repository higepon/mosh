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

#ifdef _WIN32
    #include <windows.h>
    //#include <process.h>
    #pragma warning(disable : 4127)
#else
    #include <sys/time.h> // gettimeofday
#endif

// Check sanity
// Boehm GC redirects pthread_create => GC_pthread_create with C macro.
#ifndef _WIN32
  #ifndef pthread_create
  #error "pthread_create redirect does not exist"
  #endif
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

#ifdef _WIN32

    class Mutex : public gc_cleanup
    {
        friend class ConditionVariable; // share the mutex_
    private:
        HANDLE mutex_;

    public:
        Mutex()
        {
            mutex_ = CreateMutex(NULL, FALSE, NULL);
        }

        ~Mutex()
        {
            CloseHandle(mutex_);
        }

        void lock()
        {
            WaitForSingleObject(mutex_, INFINITE);
        }

        void unlock()
        {
            ReleaseMutex(mutex_);
        }

        bool tryLock()
        {
            return WAIT_OBJECT_0 == WaitForSingleObject(mutex_, 0);
        }
    };

#else

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

#endif

    class ConditionVariable : public gc_cleanup
    {
    private:
#ifdef _WIN32
        HANDLE cond_;
#else
        pthread_cond_t cond_;
#endif
        ucs4string name_;

        void initialize()
        {
#ifdef _WIN32
            cond_ = CreateEvent(NULL, true, false, NULL);
            if (NULL == cond_) {
                fprintf(stderr, "CreateEvent failed\n");
                exit(-1);
            }
#else
            pthread_cond_init(&cond_, NULL);
#endif
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
#ifdef _WIN32
            CloseHandle(cond_);
#else
            pthread_cond_destroy(&cond_);
#endif
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
#ifdef _WIN32
            int ret = SetEvent(cond_);
#else
            int ret = pthread_cond_signal(&cond_);
#endif
            return 0 == ret;
        }

        bool notifyAll()
        {
#ifdef _WIN32
            int ret = SetEvent(cond_);
#else
            int ret = pthread_cond_broadcast(&cond_);
#endif
            return 0 == ret;
        }

        bool wait(Mutex* mutex)
        {
#ifdef _WIN32
            mutex->unlock();
            WaitForSingleObject(cond_, INFINITE);
            mutex->lock();
            return true;
#else
            int ret = pthread_cond_wait(&cond_, &mutex->mutex_);
            return 0 == ret;
#endif
        }

        // returns false if timeout
        bool waitWithTimeout(Mutex* mutex, int msecs)
        {
#ifdef _WIN32
            mutex->unlock();
            DWORD res = WaitForSingleObject(cond_, msecs);
            mutex->lock();
            if (res == WAIT_TIMEOUT) {
                return false;
            } else {
                return true;
            }
#else
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
#endif
        }
    };

    class ThreadSpecificKey : public gc_cleanup
    {
    private:
#ifdef _WIN32
        DWORD key_;
#else
        pthread_key_t key_;
#endif
    public:
        ThreadSpecificKey()
        {
#ifdef _WIN32
            key_ = TlsAlloc();
            if (key_ == 0xFFFFFFFF) {
                fprintf(stderr, "fatal : ThreadSpecificKey create\n");
                ::exit(-1);
            }
#else
            if (pthread_key_create(&key_, NULL) != 0) {
                fprintf(stderr, "fatal : ThreadSpecificKey create\n");
                ::exit(-1);
            }
#endif

        }

        virtual~ ThreadSpecificKey()
        {
#ifdef _WIN32
            if (0 == TlsFree(key_)) {
                fprintf(stderr, "fatal : ThreadSpecificKey delete\n");
                ::exit(-1);
            }
#else
            if (pthread_key_delete(key_) != 0) {
                fprintf(stderr, "fatal : ThreadSpecificKey delete\n");
                ::exit(-1);
            }
#endif
        }

#ifdef _WIN32
        DWORD key()
#else
        pthread_key_t key()
#endif
        {
            return key_;
        }
    };

    class Thread EXTEND_GC
    {
    public:
        struct StubInfo EXTEND_GC
        {
            void* (*func)(void*);
            void* argument;
            Thread* thread;
            ThreadSpecificKey* selfKey;
            void* returnValue;
        };

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
#ifdef _WIN32
            return TlsSetValue(key->key() , value) != 0;
#else
            return pthread_setspecific(key->key(), value) == 0;
#endif
        }

        static void* getSpecific(ThreadSpecificKey* key)
        {
#ifdef _WIN32
            return TlsGetValue(key->key());
#else
            return pthread_getspecific(key->key());
#endif
        }


        Thread() : lastError_(0)
        {
        }
        virtual ~Thread()
        {
#ifdef _WIN32
            CloseHandle(thread_);
#endif
        }

        bool create(void* (*start)(void*), void* arg);

        bool join(void** returnValue)
        {
#ifdef _WIN32
            const bool ret = WaitForSingleObject(thread_, INFINITE) == WAIT_OBJECT_0;
            if(returnValue != NULL) {
                *returnValue = stubInfo_->returnValue;
            }
            return ret;
#else
            if (GC_pthread_join(thread_, returnValue) == 0) {
                return true;
            } else {
                setLastError();
                return false;
            }
#endif
        }

/// not used
//         static void yield()
//         {
//             pthread_yield();
//         }

        static void exit(void* exitValue)
        {
#ifdef _WIN32
            GC_endthreadex((unsigned int)exitValue);
#else
            pthread_exit(exitValue);
#endif
        }

    private:
        void setLastError()
        {
            lastError_ = errno;
        }

#ifdef _WIN32
        HANDLE thread_;
#else
        pthread_t thread_;
#endif
        int lastError_;
    private:
        static ThreadSpecificKey* selfKey;
        StubInfo* stubInfo_;

    };

}; // namespace scheme

#endif // SCHEME_OSCOMPAT_THREAD_
