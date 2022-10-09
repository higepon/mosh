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

/*
 *  An experimental implementation of uniform f64 array.
 *  This is introduced to make the MNIST Neural Network demo run fater.
 *  We currently only support 2D array of f64 type but we may support more.
 *  See https://github.com/higepon/mosh/issues/168.
 */
class Array EXTEND_GC
{
public:
    Array(size_t nrows, size_t ncols, double value);
    ~Array();
    double ref(size_t row, size_t col) const;
    void set(size_t row, size_t col, double value);
    ArrayShape shape() const;

private:
    size_t nrows_;
    size_t ncols_;
    double* data_;
};

inline Array::Array(size_t nrows, size_t ncols, double value) : nrows_(nrows), ncols_(ncols)
{
    const size_t n = nrows * ncols;
    data_ =  new(PointerFreeGC) double[n];
    std::fill_n(data_, n, value);
}

inline double Array::ref(size_t row, size_t col) const
{
    return data_[row * ncols_ + col];
}

inline void Array::set(size_t row, size_t col, double value)
{
    data_[row * ncols_ + col] = value;
}

inline ArrayShape Array::shape() const
{
    return ArrayShape(nrows_, ncols_);
}

} // namespace scheme

#endif // SCHEME_ARRAY_H_
