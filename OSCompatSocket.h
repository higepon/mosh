/*
 * OSCompatSocket.h -
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
 *  $Id: OSCompatSocket.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_OSCOMPATSOCKET_
#define SCHEME_OSCOMPATSOCKET_

#include <sys/types.h>
#ifdef _WIN32
	#include <winsock2.h>
	#include <ws2tcpip.h> // for socklen_t
    #include <wspiapi.h>  // Windows 2000 doesn't have freeaddrinfo
	#pragma comment(lib, "ws2_32.lib")
	#pragma comment(lib, "iphlpapi.lib")
	#define snprintf _snprintf
#else
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#endif

#include "scheme.h"

namespace scheme {

    class Socket EXTEND_GC
    {
    public:
        enum Type {
            CLIENT,
            SERVER,
        };
        Socket(int fd, enum Type type, const ucs4string& address);

        int receive(uint8_t* data, int size, int flags);
        int send(uint8_t* data, int size, int flags);
        Socket* accept();
        void shutdown(int how);
        void close();

        bool isOpen() const;
        ucs4string getLastErrorMessage() const;
        ucs4string toString() const;

        static Socket* createClientSocket(const char* node,
                                          const char* service,
                                          int ai_family,
                                          int ai_socktype,
                                          int ai_flags,
                                          int ai_protocol,
                                          bool& isErrorOccured,
                                          ucs4string& errorMessage);
        static Socket* createServerSocket(const char* service,
                                          int ai_family,
                                          int ai_socktype,
                                          int ai_protocol,
                                          bool& isErrorOccured,
                                          ucs4string& errorMessage);

    private:
        static ucs4string getAddressString(const struct addrinfo* addr);
        static ucs4string getAddressString(const struct sockaddr* addr, socklen_t addrlen);
        void setLastError();
        int socket_;
        int lastError_;
        ucs4string address_;
        enum Type type_;
    };
}; // namespace scheme

#endif // SCHEME_OSCOMPATSOCKET_
