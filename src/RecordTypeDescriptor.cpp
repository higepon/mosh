/*
 * RecordTypeDescriptor.cpp - 
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
 *  $Id: RecordTypeDescriptor.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "Vector.h"
#include "Symbol.h"
#include "RecordTypeDescriptor.h"

using namespace scheme;

RecordTypeDescriptor::RecordTypeDescriptor(Object name,
                                           Object parent,
                                           Object uid,
                                           Object isSealed,
                                           Object isOpaque,
                                           Object fields) : name_(name),
                                                            parent_(parent),
                                                            uid_(uid),
                                                            isSealed_(!isSealed.isFalse()),
                                                            isOpaque_(!isOpaque.isFalse()),
                                                            fields_(fields),
                                                            fieldsLength_(fields.toVector()->length())
{
}

RecordTypeDescriptor::~RecordTypeDescriptor()
{

}

int RecordTypeDescriptor::fieldsLength() const
{
    return fieldsLength_;
}

int RecordTypeDescriptor::fieldsLengthTotal() const
{
    return fieldsLength() + parentFieldsLengthTotal();
}

int RecordTypeDescriptor::parentFieldsLengthTotal() const
{
    if (parent_.isFalse()) {
        return 0;
    } else {
        return parent_.toRecordTypeDescriptor()->fieldsLengthTotal();
    }
}

Object RecordTypeDescriptor::name() const
{
    return name_;
}

bool RecordTypeDescriptor::isOpaque() const
{
    return isOpaque_;
}

// caller should check index range
bool RecordTypeDescriptor::isFieldMutable(int index) const
{
    Vector* const fields = fields_.toVector();
    const Object field = fields->ref(index);
    if (field.car() == Symbol::intern(UC("mutable"))) {
        return true;
    } else {
        return false;
    }
}

bool RecordTypeDescriptor::isA(const RecordTypeDescriptor* rtd) const
{
//    if (this == rtd) {
    // multiple vm need this!
    if (name_ == rtd->name_) {
        return true;
    } else if (!parent_.isFalse()) {
        return parent_.toRecordTypeDescriptor()->isA(rtd);
    } else {
        return false;
    }
}

Object RecordTypeDescriptor::parent() const
{
    return parent_;
}

Object RecordTypeDescriptor::uid() const
{
    return uid_;
}

bool RecordTypeDescriptor::isSealed() const
{
    return isSealed_;
}

bool RecordTypeDescriptor::isGenerative() const
{
    return uid_.isFalse();
}

Object RecordTypeDescriptor::fieldNames() const
{
    Vector* const fields = fields_.toVector();
    const Object fieldNames = Object::makeVector(fields->length());
    Vector* const fieldNamesVector = fieldNames.toVector();
    for (int i = 0; i < fields->length(); i++) {
        fieldNamesVector->set(i, fields->ref(i).cdr().car());
    }
    return fieldNames;
}
