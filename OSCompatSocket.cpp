/*
 * OSCompatSocket.cpp - socket interfaces.
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
 *  $Id: OSCompatSocket.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "OSCompat.h"
#include "OSCompatSocket.h"

using namespace scheme;

Socket::Socket(int fd, enum Type type, const ucs4string& address) :
    socket_(fd),
    lastError_(0),
    address_(address),
    type_(type)
{
}

Socket* Socket::accept()
{
    MOSH_ASSERT(isOpen());
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);

    int fd = -1;
    for (;;) {
        fd = ::accept(socket_, (sockaddr*)&addr, &addrlen);
        if (-1 == fd) {
            if (errno == EINTR) {
                continue;
            } else {
                setLastError();
                return NULL;
            }
        } else {
            break;
        }
    }
    return new Socket(fd, Socket::SERVER, getAddressString((sockaddr*)&addr, addrlen));
}

bool Socket::isOpen() const
{
    return socket_ != -1;
}

void Socket::shutdown(int how)
{
    if (!isOpen()) {
        return;
    }
    ::shutdown(socket_, how);
}

void Socket::close()
{
    if (!isOpen()) {
        return;
    }
#ifdef _WIN32
    ::shutdown(socket_, SD_SEND);
    ::closesocket(socket_);
#else
    ::close(socket_);
#endif
    socket_ = -1;

}

ucs4string Socket::getLastErrorMessage() const
{
    return getLastErrorMessageInternal(lastError_);
}

ucs4string Socket::toString() const
{
    if (address_.empty()) {
        return UC("<socket>");
    } else {
        ucs4string ret = UC("<socket ");
        if (type_ == CLIENT) {
            ret += UC("client ");
        } else {
            ret += UC("server ");
        }
        ret += address_;
        ret += UC(">");
        return ret;
    }
}

/**
   read from socket
   @param data [in] buffer to read
   @param size [in] size to read
   @param flags [in] flags
   @retval >=0 read size
   @retval -1 error
*/
int Socket::receive(uint8_t* data, int size, int flags)
{
    MOSH_ASSERT(isOpen());

    for (;;) {
        const int ret = recv(socket_, (char*)data, size, flags);
        if (ret == -1 && errno == EINTR) {
            continue;
        }
        setLastError();
        return ret;
    }
}

/**
   write to socket
   @param data [in] buffer to read
   @param size [in] size to read
   @param flags [in] flags
   @retval >=0 written size
   @retval -1 error
*/
int Socket::send(uint8_t* data, int size, int flags)
{
    MOSH_ASSERT(isOpen());
    int rest = size;
    int sizeSent = 0;
    while (rest > 0) {
        const int ret = ::send(socket_, (char*)data, size, flags);
        if (ret == -1) {
            if (errno == EINTR) {
                continue;
            } else {
                setLastError();
                return ret;
            }
        }
        sizeSent += ret;
        rest -= ret;
        data += ret;
        size -= ret;
    }
    return sizeSent;
}

// Factories
Socket* Socket::createClientSocket(const char* node,
                                   const char* service,
                                   int ai_family,
                                   int ai_socktype,
                                   int ai_flags,
                                   int ai_protocol,
                                   bool& isErrorOccured,
                                   ucs4string& errorMessage)
{
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = ai_family;
    hints.ai_socktype = ai_socktype;
    hints.ai_flags = ai_flags;
    hints.ai_protocol = ai_protocol;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    struct addrinfo* result;
    int ret;

    // TODO server socket?
    MOSH_ASSERT(!((ai_flags & AI_PASSIVE) && node == NULL));

    // check temporary failure
    do {
        ret = getaddrinfo(node, service, &hints, &result);
    } while (EAI_AGAIN == ret);


    if (ret != 0) {
        isErrorOccured = true;
#ifdef _WIN32
        errorMessage = getLastErrorMessageInternal(WSAGetLastError());
#else
        errorMessage = ucs4string::from_c_str(gai_strerror(ret));
#endif
        return NULL;
    }

    // there may be many addresses for one host
    int lastError = 0;
    for (struct addrinfo* p = result; p != NULL; p = p->ai_next) {
        const int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (-1 == fd) {
#ifdef _WIN32
            lastError = WSAGetLastError();
            printf("%s %s:%d %d\n", __func__, __FILE__, __LINE__, lastError);fflush(stdout);// debug
#else
            lastError = errno;
#endif
            continue;
        }
        if (connect(fd, p->ai_addr, p->ai_addrlen) == 0) {

            ucs4string addressString = getAddressString(p);
            freeaddrinfo(result);
            return new Socket(fd, Socket::CLIENT, addressString);
        } else {
#ifdef _WIN32
            lastError = WSAGetLastError();
            printf("%s %s:%d %d\n", __func__, __FILE__, __LINE__, lastError);fflush(stdout);// debug
#else
            lastError = errno;
#endif
#ifdef _WIN32
            ::shutdown(fd, SD_SEND);
            ::closesocket(fd);
#else
            ::close(fd);
#endif
        }
    }
    freeaddrinfo(result);
    isErrorOccured = true;
    errorMessage = getLastErrorMessageInternal(lastError);
    return NULL;
}
#ifndef __CYGWIN__
extern ucs4string my_utf16ToUtf32(const std::wstring& s);
#endif
Socket* Socket::createServerSocket(const char* service,
                                   int ai_family,
                                   int ai_socktype,
                                   int ai_protocol,
                                   bool& isErrorOccured,
                                   ucs4string& errorMessage)
{
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = ai_family;
    hints.ai_socktype = ai_socktype;
    hints.ai_flags = AI_PASSIVE;
    hints.ai_protocol = ai_protocol;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    struct addrinfo* result;
    int ret;

    // check temporary failure
    do {
        ret = getaddrinfo(NULL, service, &hints, &result);
    } while (EAI_AGAIN == ret);


    if (ret != 0) {
        isErrorOccured = true;
#ifdef _WIN32
        errorMessage = my_utf16ToUtf32(gai_strerror(ret));
#else
        errorMessage = ucs4string::from_c_str(gai_strerror(ret));
#endif
            printf("here%d", __LINE__);
        return NULL;
    }

    // there may be many addresses for one host
    int lastError = 0;
    for (struct addrinfo* p = result; p != NULL; p = p->ai_next) {
        const int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (-1 == fd) {
            lastError = errno;
            continue;
        }
        int optValue = 1;
        if(setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char*)&optValue, sizeof(optValue)) == -1) {
#ifdef _WIN32
            ::shutdown(fd, SD_SEND);
            ::closesocket(fd);
            lastError = WSAGetLastError();
#else
            ::close(fd);
            lastError = errno;
#endif

            continue;
        }

        if (bind(fd, p->ai_addr, p->ai_addrlen) == -1) {
#ifdef _WIN32
            ::shutdown(fd, SD_SEND);
            ::closesocket(fd);
            lastError = WSAGetLastError();
#else
            ::close(fd);
            lastError = errno;
#endif

            continue;
        }

        const int TRADITIONAL_BACKLOG = 5;
        if (p->ai_socktype == SOCK_STREAM ) {
            if (listen(fd, TRADITIONAL_BACKLOG) == -1) {
#ifdef _WIN32
                ::shutdown(fd, SD_SEND);
                ::closesocket(fd);
                lastError = WSAGetLastError();
#else
                ::close(fd);
                lastError = errno;
#endif

                continue;
            }
        }

        ucs4string addressString = getAddressString(p);
        freeaddrinfo(result);
        return new Socket(fd, Socket::SERVER, addressString);
    }
    freeaddrinfo(result);
    isErrorOccured = true;
    errorMessage = getLastErrorMessageInternal(lastError);
    return NULL;
}

ucs4string Socket::getAddressString(const struct sockaddr* addr, socklen_t addrlen)
{
    int ret;
    char host[NI_MAXHOST];
    char serv[NI_MAXSERV];
    do {
        ret = getnameinfo(addr,
                          addrlen,
                          host, sizeof(host),
                          serv, sizeof(serv), NI_NUMERICSERV);
    } while (EAI_AGAIN == ret);
    char name[NI_MAXSERV + NI_MAXHOST + 1];
    snprintf(name, sizeof(name), "%s:%s", host, serv);
    return ucs4string::from_c_str(name);
}
ucs4string Socket::getAddressString(const struct addrinfo* addr)
{
    return getAddressString(addr->ai_addr, addr->ai_addrlen);
}

void Socket::setLastError()
{
#ifdef _WIN32
    lastError_ = WSAGetLastError();
#else
    lastError_ = errno;
#endif
}
