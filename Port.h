/*
 * Port.h - <port>
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: Port.h 5321 2008-05-09 05:54:00Z higepon $
 */

#ifndef __SCHEME_PORT_H__
#define __SCHEME_PORT_H__

#include "scheme.h"

namespace scheme {

//----------------------------------------------------------------------
//    Binary port Interface
//----------------------------------------------------------------------
class BinaryInputPort EXTEND_GC
{
public:
    virtual ~BinaryInputPort() {};
    virtual int getU8() = 0;
    virtual ByteVector* getByteVector(int size) = 0;
    virtual ucs4string toString() = 0;
    virtual int close() = 0;
};

class BinaryOutputPort EXTEND_GC
{
public:
    virtual ~BinaryOutputPort() {};
    virtual int putU8(uint8_t v) = 0;
    virtual int putU8(uint8_t* v, int size) = 0;
    virtual int putByteVector(ByteVector bv, int start = 0) = 0;
    virtual int putByteVector(ByteVector bv, int start, int count) = 0;
    virtual int close() = 0;
};

//----------------------------------------------------------------------
//    Binary input port
//----------------------------------------------------------------------
class ByteArrayBinaryInputPort : public BinaryInputPort
{
public:
    ByteArrayBinaryInputPort(const uint8_t* buf, int size) : buf_(buf), size_(size), index_(0)
    {
    }
    ~ByteArrayBinaryInputPort() {}

    int getU8()
    {
        if (index_ >= size_) return -1;
        return buf_[index_++];
    }

    ucs4string toString() {
        return UC("<byte-array-input-port>");
    }

    ByteVector* getByteVector(int size)
    {
        fprintf(stderr, "get-byte-vector-n not implemented");
        exit(-1);
    }

    int close() { return 0; }

private:
    const uint8_t* const buf_;
    const int size_;
    int index_;
};

class FileBinaryInputPort : public BinaryInputPort
{
public:
    FileBinaryInputPort(FILE* stream) : stream_(stream), fileName_(UC("<unknown file>")) {}
    FileBinaryInputPort(ucs4string file);
    FileBinaryInputPort(const char* file);
    ucs4string toString() {
        return fileName_;
    }

    virtual ~FileBinaryInputPort() {}

    int getU8()
    {
        uint8_t c;
        if (0 == fread(&c, 1, 1, stream_)) {
            return EOF;
        } else {
            return c;
        }
    }

    ByteVector* getByteVector(int size)
    {
#ifdef USE_BOEHM_GC
        int8_t* buf = new(PointerFreeGC) int8_t[size];
#else
        int8_t* buf = new int8_t[size];
#endif
        int ret = fread(buf, 1, size, stream_);
        return new ByteVector(ret, buf);
    }

    int close()
    {
        fclose(stream_);
        return 0;
    }

private:
    FILE* stream_;
    ucs4string fileName_;
};

class CustomBinaryInputPort : public BinaryInputPort
{
public:
    CustomBinaryInputPort(Object readProc) : readProc_(readProc)
    {
    }

    virtual ~CustomBinaryInputPort() {}

    int getU8();
    ByteVector* getByteVector(int size);

    ucs4string toString() {
        return UC("<custom port>");
    }

    int close()
    {
        // todo if close! proc exists
        return 0;

    }
private:
    const Object readProc_;
};

//----------------------------------------------------------------------
//    Binary output port
//----------------------------------------------------------------------
class FileBinaryOutputPort : public BinaryOutputPort
{
public:
    FileBinaryOutputPort(FILE* stream) : stream_(stream) {}
    FileBinaryOutputPort(ucs4string file)
    {
        stream_ = fopen(file.ascii_c_str(), "w");
        if (NULL == stream_) {
            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
            // todo
            fprintf(stderr, "fopen output file port=<%s>", file.ascii_c_str());
            exit(-1);
        }
    }

    virtual ~FileBinaryOutputPort() {}

    int putU8(uint8_t v)
    {
        return fwrite(&v, 1, 1, stream_);
    }

    int putU8(uint8_t* v, int size)
    {
        return fwrite(v, size, 1, stream_);
    }

    int putByteVector(ByteVector bv, int start = 0)
    {
        return putByteVector(bv, start, bv.length() - start);
    }

    int putByteVector(ByteVector bv, int start, int count)
    {
        int8_t* buf = bv.data();
        return fwrite(&buf[start], 1, count, stream_);
    }

    int close()
    {
        fclose(stream_);
        return 0;
    }

private:
    FILE* stream_;
};

//----------------------------------------------------------------------
//    Codec and Transcoder
//----------------------------------------------------------------------
class Codec EXTEND_GC
{
public:
    virtual ~Codec() {}
    virtual int out(BinaryOutputPort* port, ucs4char c) = 0;
    virtual int out(uint8_t* buf, ucs4char c) = 0;
    virtual ucs4char in(BinaryInputPort* port) = 0;
};

class UTF8Codec : public Codec
{
public:
    // Be careful, buf is shared.
    int out(BinaryOutputPort* port, ucs4char u)
    {
        static uint8_t buf[4];
        const int size = out(buf, u);
        return port->putU8(buf, size);
    }

    int out(uint8_t* buf, ucs4char u)
    {
        // UTF8-1
        if (u < 0x80) {
            buf[0] = (uint8_t)u;
            return 1;
        // UTF8-2
        } else if (u < 0x7ff) {
            buf[0] = 0xc0 | ((u >> 6) & 0x1f);
            buf[1] = 0x80 | (u & 0x3f);
            return 2;
        // UTF8-3
        } else if (u < 0xffff) {
            buf[0] = 0xe0 | ((u >> 12) & 0xf);
            buf[1] = 0x80 | ((u >> 6) & 0x3f);
            buf[2] = 0x80 | (u & 0x3f);
            return 3;
        // UTF8-4
        } else if (u < 0x10ffff) {
            buf[0] = 0xf0 | ((u >> 18) & 0x7);
            buf[1] = 0x80 | ((u >> 12) & 0x3f);
            buf[2] = 0x80 | ((u >> 6) & 0x3f);
            buf[3] = 0x80 | (u & 0x3f);
            return 4;
        } else {
//            fprintf(stderr, "malformed utf8 error\n");
            return 0;
//            exit(-1);
        }
    }

    bool isUtf8Tail(uint8_t b)
    {
        return (0x80 <= b && b <= 0xbf);
    }

    ucs4char in(BinaryInputPort* port)
    {
        const int f = port->getU8();
        if (f == EOF) return EOF;
        uint8_t first = (uint8_t)(f & 0xff);

        // UTF8-1(ascii) = %x00-7F
        if (first < 0x80) {
            return first;
            // UTF8-2 = %xC2-DF UTF8-tail
        } else if (0xc2 <= first && first <= 0xdf) {
            uint8_t second = port->getU8();
            if (isUtf8Tail(second)) {
                return ((first & 0x1f) << 6) | (second & 0x3f);
            } else {
                // error
                printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                fprintf(stderr, "%s first = %x error\n", __func__, first);
                exit(-1);
            }
            // UTF8-3 = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
            //          %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
        } else if (0xe0 <= first && first <= 0xef) {
            uint8_t second = port->getU8();
            uint8_t third =  port->getU8();
            if (!isUtf8Tail(third)) {
                // error
                printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                fprintf(stderr, "%s first = %x error\n", __func__, first);
                exit(-1);
            } else if ((0xe0 == first && 0xa0 <= second && second <= 0xbf)    |
                       (0xed == first && 0x80 <= second && second <= 0x9f)    |
                       (0xe1 <= first && first <= 0xec && isUtf8Tail(second)) |
                       (0xee == first || 0xef == first) && isUtf8Tail(second)) {
                return ((first & 0xf) << 12) | ((second & 0x3f) << 6) | (third & 0x3f);
            } else {
                // error
                return 0;
            }
            // UTF8-4 = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
            //          %xF4 %x80-8F 2( UTF8-tail )
        } else if (0xf0 <= first && first <= 0xf4) {
            uint8_t second =  port->getU8();
            uint8_t third =  port->getU8();
            uint8_t fourth = port->getU8();
            if (!isUtf8Tail(third) || !isUtf8Tail(fourth)) {
                printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                // error
                fprintf(stderr, "%s first = %x error\n", __func__, first);
                exit(-1);
            } else if ((0xf0 == first && 0x90 <= second && second <= 0xbf)     |
                       (0xf4 == first && 0x80 <= second && second <= 0x8f)     |
                       (0xf1 <= first && first <= 0xf3 && isUtf8Tail(second))) {
                return ((first & 0x7) << 18) | ((second & 0x3f) << 12) | ((third & 0x3f) << 6) | fourth;
            } else {
                // error
                printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
                fprintf(stderr, "%s first = %x error\n", __func__, first);
                exit(-1);
            }
        } else {
            // error
            printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
            fprintf(stderr, "%s first = %x error\n", __func__, first);
            exit(-1);
        }
    }
};

class Transcoder EXTEND_GC
{
public:
    enum EolStyle
    {
        LF,
        CR,
        CRLF,
        NEL,
        CRNEL,
        LS,
        NONE,
    };
    enum ErrorHandlingMode
    {
        IGNORE_ERROR,
        RAISE,
        REPLACE,
    };
    Transcoder(Codec* codec, enum EolStyle e, enum ErrorHandlingMode m) : codec_(codec)
    {
    }

    Transcoder(Codec* codec) : codec_(codec)
    {
    }


    Codec* getCodec() const { return codec_; }

private:
    Codec* codec_;
};

//----------------------------------------------------------------------
//    Textual output port
//----------------------------------------------------------------------
class TextualOutputPort EXTEND_GC
{
public:
    TextualOutputPort()
    {
    }

    TextualOutputPort(BinaryOutputPort* port, Transcoder* coder) : port_(port), codec_(coder->getCodec()), coder_(coder)
    {
    }

    virtual ~TextualOutputPort()
    {
    }

    virtual int close()
    {
        return port_->close();
    }

    void putString(String* str)
    {
        putString(str->data());
    }

    void putString(const ucs4string& s)
    {
        for (ucs4string::size_type i = 0; i < s.size(); i++) {
            putChar(s[i]);
        }
    }

    void putString(const char* s)
    {
        const int len = strlen(s);
        for (int i = 0; i < len; i++) {
            putChar(s[i]);
        }
    }

    virtual void putChar(ucs4char c)
    {
        codec_->out(port_, c);
    }

    void putDatum(Object o, bool inList = false);
    void display(Object o, bool inList = false);
    void putPair(Object obj, bool inList = false);

    Object format(const ucs4string& fmt, Object args)
    {
        ucs4string buffer = UC("");
        for (uint32_t i = 0; i < fmt.size(); i++) {
            if (fmt[i] == '~') {
                i++;
                if (!buffer.empty()) {
                    putString(buffer);
                    buffer.clear();
                }
                switch (fmt[i]) {
                case 'a':
                case 'A':
                case 'd':
                case 'D':
                {
                    if (args.isPair()) {
                        display(args.car());
                        args = args.cdr();
                    } else {
//                    RAISE_ERROR("too few arguments for format string: ~a", f);
                    }
                    break;
                }
                case 's':
                case 'S':
                {
                    if (args.isPair()) {
                        putDatum(args.car());
                        args = args.cdr();
                    } else {
//                    RAISE_ERROR("too few arguments for format string: ~a", f);
                    }
                    break;
                }
                case '\0':
                    i--;
                    break;
                }
            } else {
                buffer += fmt[i];
            }
        }

        if (!buffer.empty()) {
            putString(buffer);
        }
        fflush(stdout); // temp
        return Object::Undef;
    }

    BinaryOutputPort* binaryPort() const { return port_; }

private:
    BinaryOutputPort* port_;
    Codec* codec_;
    Transcoder* coder_;
};

class StringTextualOutputPort : public TextualOutputPort
{
public:
    StringTextualOutputPort() : index_(0) {}
    virtual ~StringTextualOutputPort() {}

    void putChar(ucs4char c)
    {
        buffer_ += c;
        index_++;
    }

    ucs4string getString() { return buffer_; }

    int close()
    {
        return 0;
    }

private:
    ucs4string buffer_;
    ucs4string::size_type index_;
};

class TextualByteVectorOutputPort : public TextualOutputPort
{
public:
    TextualByteVectorOutputPort(Transcoder* transcoder) : transcoder_(transcoder), codec_(transcoder->getCodec()) {}
    virtual ~TextualByteVectorOutputPort() {}

    void putChar(ucs4char c)
    {
        uint8_t buf[4];
        const int size = codec_->out(buf, c);
        for (int i = 0; i < size; i++) {
            v_.push_back(buf[i]);
        }
    }

    const gc_vector<uint8_t>& getByteVector() const
    {
        return v_;
    }

    int close()
    {
        return 0;
    }


private:
    Transcoder* transcoder_;
    Codec* codec_;
    gc_vector<uint8_t> v_;
};

//----------------------------------------------------------------------
//    Textual input port
//----------------------------------------------------------------------
class TextualInputPort EXTEND_GC
{
public:
    TextualInputPort(BinaryInputPort* port, Transcoder* coder) : port_(port), codec_(coder->getCodec()), coder_(coder), line_(1)
    {
    }

    TextualInputPort() {}

    virtual ~TextualInputPort()
    {
    }

    virtual ucs4char getChar()
    {
        ucs4char c;
        if (buffer_.empty()) {
            c= codec_->in(port_);
        } else {
            c = buffer_[buffer_.size() - 1];
            buffer_.erase(buffer_.size() - 1, 1);
        }
        if (c == '\n') ++line_;
        return c;
    }

    virtual int getLine() const { return line_; }

    virtual void unGetChar(ucs4char c)
    {
        if (EOF == c) return;
        buffer_ += c;
    }

    ucs4string toString() {
        return port_->toString();
    }

    virtual Object getDatum()
    {
        return read(this);
    }

    virtual int close()
    {
        return port_->close();
    }

private:
    BinaryInputPort* port_;
    Codec* codec_;
    Transcoder* coder_;
    ucs4string buffer_;
    int line_;
};

class StringTextualInputPort : public TextualInputPort
{
public:
    StringTextualInputPort(const ucs4string& str) : buffer_(str), index_(0) {}
    virtual ~StringTextualInputPort() {}

    ucs4char getChar()
    {
        if (buffer_.size() == index_)
        {
            return EOF;
        }
        return buffer_[index_++];
    }

    void unGetChar(ucs4char c)
    {
        if (EOF == c) return;
        index_--;
    }

    int close() { return 0; }

private:
    ucs4string buffer_;
    ucs4string::size_type index_;
};

bool fileExistsP(const ucs4string& file);

}; // namespace scheme

#endif // __SCHEME_PORT_H__
