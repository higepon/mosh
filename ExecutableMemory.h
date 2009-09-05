/*
 * ExecutableMemory.h - For FFI callback functions.
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
 *  $Id: ExecutableMemory.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_EXECUTABLE_MEMORY_
#define SCHEME_EXECUTABLE_MEMORY_

#include "scheme.h"

#ifndef _WIN32
#include <sys/mman.h>
#else
#include <windows.h>
#endif


namespace scheme {

// N.B.
// This class is allocated out of GC, since it will be passed to out of Mosh.
class ExecutableMemory
{
public:
    ExecutableMemory(int size) : size_(size), addr_(NULL), roundAddr_(NULL), index_(0)
    {
    }

    virtual ~ExecutableMemory()
    {
        delete[] addr_;
    }

    int size() const { return size_; }
    bool push(uint8_t v)
    {
        if (index_ >= size_) {
            return false;
        }
        roundAddr_[index_++] = v;
        return true;
    }

    uint8_t* address() const
    {
        return roundAddr_;
    }

    bool allocate()
    {

        // Originally from xbyak
#if defined(__GNUC__) && !defined(MOSH_MINGW32)
        // obsolete?
        addr_ = (uint8_t*)valloc(size_);

        size_t pageSize = sysconf(_SC_PAGESIZE);
        size_t iaddr = reinterpret_cast<size_t>(addr_);
        size_t roundAddr = iaddr & ~(pageSize - static_cast<size_t>(1));
        int mode = PROT_READ | PROT_WRITE | PROT_EXEC;
        roundAddr_ = (uint8_t*)roundAddr;
        return mprotect(reinterpret_cast<void*>(roundAddr), size_ + (iaddr - roundAddr), mode) == 0;
#elif defined(_WIN32)
		// addr_ = new uint8_t[size_];
        // DWORD oldProtect;
        // roundAddr_ = addr_;
        // return VirtualProtect(static_cast<void*>(addr_), size_, PAGE_EXECUTE_READWRITE, &oldProtect) != 0;
	roundAddr_ = addr_ = (uint8_t*)VirtualAlloc(NULL, size_, MEM_RESERVE|MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	return ((addr_)!=NULL);
#else
        return true;
#endif
    }

private:
    int size_;
    uint8_t* addr_;
    uint8_t* roundAddr_;
    int index_;
};

} // namespace scheme

#endif // SCHEME_EXECUTABLE_MEMORY_
