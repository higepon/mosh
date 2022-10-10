/*
 * Array.h - <f64array>
 *
 *   Copyright (c) 2022  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 */

#ifndef SCHEME_ARRAY_H_
#define SCHEME_ARRAY_H_

namespace scheme {

// TODO new with no pointer
// Support reshape!!! is easy

typedef std::pair<size_t, size_t> ArrayShape;
typedef Array<double> F64Array;

/*
 *
 *  An experimental implementation of uniform f64 array.
 *  This is introduced to make the MNIST Neural Network demo run fater.
 *  We currently only support 2D array of f64 type but we may support more.
 *  See https://github.com/higepon/mosh/issues/168.
 * 
 */
template <typename T>
class Array EXTEND_GC
{
public:
    Array(size_t nrows, size_t ncols, T value);
    ~Array();
    T ref(size_t row, size_t col) const;
    void set(size_t row, size_t col, T value);
    ArrayShape shape() const;
    ucs4string toString() const
    {
        ucs4string ret(UC("#<f64array "));
        char buf[32];
        snprintf(buf, sizeof(buf), "shape=(%zu %zu)", nrows_, ncols_);
        ret += ucs4string::from_c_str(buf);
        ret += UC(">");
        return ret;        
    }

private:
    size_t nrows_;
    size_t ncols_;
    T* data_;
};

template <typename T>
inline Array<T>::Array(size_t nrows, size_t ncols, T value) : nrows_(nrows), ncols_(ncols)
{
    const size_t n = nrows * ncols;
    data_ =  new(PointerFreeGC) T[n];
    std::fill_n(data_, n, value);
}

template <typename T>
inline T Array<T>::ref(size_t row, size_t col) const
{
    return data_[row * ncols_ + col];
}

template <typename T>
inline void Array<T>::set(size_t row, size_t col, T value)
{
    data_[row * ncols_ + col] = value;
}

template <typename T>
inline ArrayShape Array<T>::shape() const
{
    return ArrayShape(nrows_, ncols_);
}

} // namespace scheme

#endif // SCHEME_ARRAY_H_