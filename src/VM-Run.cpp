/*
 * VM-Run.cpp - Run Loop
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
 *  $Id: VM-Run.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "VM-Run.h" // Included only here.

using namespace scheme;

Object VM::runLoop(Object* code, jmp_buf returnPoint, bool returnTable /* = false */)
{
#ifdef USE_DIRECT_THREADED_CODE
#include "labels.cpp"

    if (returnTable) {
#ifdef ENABLE_PROFILER
        labelReturn_ = reinterpret_cast<intptr_t>(&&LABEL_RETURN); // used for profiler
#endif
        return Object::makeRaw(dispatch_table);
    }
#endif
    returnCode_[0] = Object::makeRaw(INSTRUCTION(RETURN));
    returnCode_[1] = Object::makeFixnum(0);
    callCode_->set(0, Object::makeRaw(INSTRUCTION(CALL)));
    callCode_->set(1, Object::makeFixnum(0));
    callCode_->set(2, Object::makeRaw(INSTRUCTION(HALT)));

    Object operand = Object::Undef;

    // shourt cut pointers
    EqHashTable* const nameSpace = nameSpace_.toEqHashTable();
    pc_ = code;
    for (;;) {
        const Object insn = *pc_++;
        SWITCH((intptr_t)insn.val) {
        CASE(HALT)
        {
            return ac_;
        }
        CASE(CALL)
        {
            operand = fetchOperand();
        call_entry:
            #include "call.inc.cpp"
            NEXT;
        }
        CASE(TAIL_CALL)
        {
            const Object depth = fetchOperand();
            VM_ASSERT(depth.isFixnum());

            const Object diff = fetchOperand();
            VM_ASSERT(diff.isFixnum());
            sp_ = shiftArgsToBottom(sp_, depth.toFixnum(), diff.toFixnum());
            operand = depth;
            #include "call.inc.cpp"
            NEXT;
        }
        CASE(APPLY)
        {
            const Object args = pop();
            if (args.isNil()) {
                callCode_->set(1, Object::makeFixnum(0));
                pc_  = callCode_->code();
            } else {
                if (! args.isPair()) {
                    callAssertionViolationAfter(this, UC("apply"), UC("bug?"), L1(ac_));
                    NEXT;
                }
                const int length = Pair::length(args);
                const int shiftLen = length > 1 ? length - 1 : 0;
                Object* const sp = sp_ + shiftLen + 1; //unShiftArgs(sp_, 0, shiftLen);////
                pairArgsToStack(sp, 0, args);
                callCode_->set(1, Object::makeFixnum(length));
                pc_ = callCode_->code();
                sp_ = sp;
            }
            NEXT;
        }
        CASE(PUSH)
        {
            push(ac_);
            NEXT;
        }
        CASE(CONSTANT_PUSH)
        {
            const Object c = fetchOperand();
            ac_ = c;
            push(ac_);
            NEXT1;
        }
        CASE(ASSIGN_FREE)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            referFree(n).toBox()->set(ac_);
            NEXT;
        }
        CASE(ASSIGN_GLOBAL)
        {
            const Object id = fetchOperand();
            if (id.isGloc()) {
                id.toGloc()->setValue(ac_);
            } else {
                const Object val = nameSpace->ref(id, notFound_);
                if (val == notFound_) {
                    // psyntax requires this
                    const Object gloc = Object::makeGloc(ac_);
                    nameSpace->set(id, gloc);
                    *(pc_ - 1) = gloc;
                } else {
                    VM_ASSERT(val.isGloc());
                    val.toGloc()->setValue(ac_);
                    *(pc_ - 1) = val;
                }
            }
            ac_ = Object::Undef;
            NEXT1;
        }
        CASE(ASSIGN_LOCAL)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            referLocal(n.toFixnum()).toBox()->set(ac_);
            NEXT;
        }
        CASE(BOX)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            indexSet(sp_, n.toFixnum(), Object::makeBox(index(sp_, n.toFixnum())));
            NEXT;
        }
        CASE(CAAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
                if (ac_.isPair()) {
                    ac_ = ac_.car();
                } else {
                    callAssertionViolationAfter(this, UC("caar"), UC("pair required"), Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter(this, UC("caar"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CADR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
                if (ac_.isPair()) {
                    ac_ = ac_.car();
                } else {
                    callAssertionViolationAfter(this, UC("cadr"), UC("pair required"), Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter(this, UC("cadr"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
//         CASE(REFER_LOCAL_CAR)
//         {
//             const Object n = fetchOperand();
//             VM_ASSERT(n.isFixnum());
//             ac_ = referLocal(n.toFixnum());
//             if (ac_.isPair()) {
//                 ac_ = ac_.car();
//             } else {
//                 callAssertionViolationAfter(UC("car"), UC("pair required"), Pair::list1(ac_));
//             }
//             NEXT1;
//         }
//         CASE(REFER_LOCAL_CDR)
//         {
//             const Object n = fetchOperand();
//             VM_ASSERT(n.isFixnum());
//             ac_ = referLocal(n.toFixnum());
//             if (ac_.isPair()) {
//                 ac_ = ac_.cdr();
//             } else {
//                 callAssertionViolationAfter(UC("cdr"), UC("pair required"), Pair::list1(ac_));
//             }
//             NEXT1;
//         }
//         CASE(REFER_LOCAL_CONS)
//         {
//             const Object n = fetchOperand();
//             VM_ASSERT(n.isFixnum());
//             ac_ = Object::cons(pop(), referLocal(n.toFixnum()));
//             NEXT1;
//         }

        CASE(CAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
            } else {
                callAssertionViolationAfter(this, UC("car"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CAR_PUSH)
        {
            if (ac_.isPair()) {
                push(ac_.car());
            } else {
                // todo エラーにこれを入れれば便利じゃ？
//                LOG1(UC("cl=~a\n"), dc_.toClosure()->sourceInfoString());
                callAssertionViolationAfter(this, UC("car"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDAR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.car();
                if (ac_.isPair()) {
                    ac_ = ac_.cdr();
                } else {
                    callAssertionViolationAfter(this, UC("cdar"), UC("pair required"), Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter(this, UC("cdar"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDDR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
                if (ac_.isPair()) {
                    ac_ = ac_.cdr();
                } else {
                    callAssertionViolationAfter(this, UC("cddr"), UC("pair required"), Pair::list1(ac_));
                }
            } else {
                callAssertionViolationAfter(this, UC("cddr"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CDR)
        {
            if (ac_.isPair()) {
                ac_ = ac_.cdr();
            } else {
                callAssertionViolationAfter(this, UC("cdr"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
//         CASE(REFER_LOCAL_CDR_PUSH)
//         {
//             Object n = fetchOperand();
//             VM_ASSERT(n.isFixnum());
//             ac_ = referLocal(n.toFixnum());
//             // Fall Through
//         }
        CASE(CDR_PUSH)
        {
            if (ac_.isPair()) {
                push(ac_.cdr());
            } else {
                callAssertionViolationAfter(this, UC("cdr"), UC("pair required"), Pair::list1(ac_));
            }
            NEXT1;
        }
        CASE(CLOSURE)
        {
            const Object skipSizeObject      = fetchOperand();
            const Object argLengthObject     = fetchOperand();
            const Object isOptionalArgObjecg   = fetchOperand();
            const Object freeVariablesNumObject = fetchOperand();
            const Object maxStackObject         = fetchOperand();
            const Object sourceInfo    = fetchOperand();
            VM_ASSERT(skipSizeObject.isFixnum());
            const int skipSize         = skipSizeObject.toFixnum();
            VM_ASSERT(argLengthObject.isFixnum());
            const int argLength        = argLengthObject.toFixnum();
            const bool isOptionalArg   = !isOptionalArgObjecg.isFalse();
            VM_ASSERT(freeVariablesNumObject.isFixnum());
            const int freeVariablesNum = freeVariablesNumObject.toFixnum();
            VM_ASSERT(maxStackObject.isFixnum());
            const int maxStack         =maxStackObject.toFixnum();

//            LOG1("(CLOSURE) source=~a\n", sourceInfo);
            ac_ = Object::makeClosure(pc_, skipSize, argLength, isOptionalArg, (sp_ - freeVariablesNum), freeVariablesNum, maxStack, sourceInfo);
            sp_ -= freeVariablesNum;
            pc_ += skipSize - 6;
            NEXT1;
        }
        CASE(CONS)
        {
            ac_ = Object::cons(pop(), ac_);
            NEXT1;
        }
        CASE(CONSTANT)
        {
            const Object c = fetchOperand();
            ac_ = c;
            NEXT1;
        }
        CASE(PUSH_CONSTANT)
        {
            push(ac_);
            ac_ = fetchOperand();
            NEXT1;
        }
        CASE(DEFINE_GLOBAL)
        {
            // Once multiple define was forbidden.
            // But allowed to use on nmosh.
            const Object id = fetchOperand();
            nameSpace->set(id, Object::makeGloc(ac_));
            NEXT;
        }
        CASE(DISPLAY)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            const int freeVariablesNum = n.toFixnum();

            // create display closure
            const Object display = Object::makeClosure(nullptr, 0, 0, false, sp_ - freeVariablesNum, freeVariablesNum, 0, Object::False);
            display.toClosure()->prev = dc_;
            dc_ = display;
            sp_ = sp_ - freeVariablesNum;
            NEXT;
        }
        CASE(ENTER)
        {
            const Object n = fetchOperand(); // not used
            VM_ASSERT(n.isFixnum());
            fp_ = sp_ - n.toFixnum();
            NEXT;
        }
        CASE(PUSH_ENTER)
        {
            push(ac_);
            const Object n = fetchOperand(); // not used
            VM_ASSERT(n.isFixnum());
            fp_ = sp_ - n.toFixnum();
            NEXT;
        }
        CASE(EQ)
        {
            ac_ = Object::makeBool(pop().eq(ac_));
            NEXT1;
        }
        CASE(EQV)
        {
            ac_ = Object::makeBool(eqv(pop(), ac_));
            NEXT1;
        }
        CASE(EQUAL)
        {
            Equal e;
            ac_ =  Object::makeBool(e.equal(pop(), ac_));
            NEXT1;
        }
        CASE(PUSH_FRAME)
        {
            push(ac_);
            goto frame_entry;
        }
        CASE(FRAME)
        {
        frame_entry:
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            const int skipSize = n.toFixnum();
            makeCallFrame(pc_ + skipSize - 1);
            NEXT;
        }
        CASE(INDIRECT)
        {
            ac_ = ac_.toBox()->value();
            NEXT1;
        }
        CASE(LEAVE)
        {
            operand= fetchOperand();
            VM_ASSERT(operand.isFixnum());
            Object* const sp = sp_ - operand.toFixnum();

            const Object fpObject = index(sp, 0);
            VM_ASSERT(fpObject.isObjectPointer());
            fp_ = fpObject.toObjectPointer();

            dc_ = index(sp, 1);
            VM_ASSERT(dc_.isProcedure());

            sp_ = sp - 2;
            NEXT;
        }
        CASE(LET_FRAME)
        {
            const Object maxStack = fetchOperand();
            if (maxStack.toFixnum() + sp_ >= stackEnd_) {
//                printf("LET_FRAME: stack expansion\n");
                expandStack(stackSize_ / 10);
            }
            push(dc_);
            push(Object::makeObjectPointer(fp_));
            NEXT;
        }
        CASE(LIST)
        {
            const Object numObject = fetchOperand();
            VM_ASSERT(numObject.isFixnum());
            const int num = numObject.toFixnum();
            Object list = Object::Nil;
            for (int i = 0; i < num; i++) {
                list = Object::cons(index(sp_, i), list);
            }
            ac_ = list;
            sp_ = sp_ - num;
            NEXT1;
        }
        CASE(LOCAL_JMP)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            pc_ += n.toFixnum() - 1;
            NEXT;
        }
        CASE(MAKE_CONTINUATION)
        {
            const Object n = fetchOperand();
            VM_ASSERT(sp_ >= stack_);
            ac_ = Object::makeContinuation(Object::makeStack(stack_, static_cast<int>(sp_ - stack_)),
                                           n,
                                           dynamicWinders());
            NEXT1;
        }
        CASE(VECTOR)
        {
            const Object numObject = fetchOperand();
            MOSH_ASSERT(numObject.isFixnum());
            const int num = numObject.toFixnum();
            Object vec = Object::makeVector(num);
            if (num > 0) {
                Vector* const v = vec.toVector();
                Object arg = ac_;
                for (int i = num - 1; i > 0 ; i--) {
                    v->set(i, arg);
                    arg = pop();
                }
                v->set(0, arg);
            }
            ac_ = vec;
            NEXT1;
        }
        CASE(MAKE_VECTOR)
        {
            const Object n = pop();
            if (n.isFixnum()) {
                VM_ASSERT(n.isFixnum());
                ac_ = Object::makeVector(n.toFixnum(), ac_);
            } else {
                callWrongTypeOfArgumentViolationAfter(this, UC("make-vector"), UC("fixnum"), L1(n));
            }
            NEXT1;
        }
        CASE(NOP)
        {
            NEXT;
        }
        CASE(NOT)
        {
            ac_ = ac_.isFalse() ? Object::True : Object::False;
            NEXT1;
        }
        CASE(NULL_P)
        {
            ac_ = ac_.isNil() ? Object::True : Object::False;
            NEXT1;
        }
        CASE(APPEND2)
        {
            const Object head = pop();
            if (head.isList()) {
                ac_ = Pair::append2(head, ac_);
            } else {
                callWrongTypeOfArgumentViolationAfter(this, UC("append"), UC("list"), L1(head));
            }
            NEXT1;
        }
        CASE(NUMBER_ADD)
        {
            const Object n = pop();
            // short cut for Fixnum. Benmarks tell me this is strongly required.
            if (n.isFixnum() && ac_.isFixnum()) {
                const int32_t val = n.toFixnum() + ac_.toFixnum();
                ac_ = Bignum::makeInteger(val);
            } else {
                const Object v = ac_;
                ac_ = Arithmetic::add(n, v);
                if (ac_.isFalse()) {
                    callWrongTypeOfArgumentViolationAfter(this, UC("+"), UC("number"), L2(n, v));
                }
            }
            NEXT1;
        }
        CASE(NUMBER_EQUAL)
        {
            const Object n = pop();
            // short cut for Fixnum. Benchmarks tell me this is strongly required.
            if (n.isFixnum() && ac_.isFixnum()) {
                ac_ = Object::makeBool(n.toFixnum() == ac_.toFixnum());
            } else {
                if (n.isNumber() && ac_.isNumber()) {
                    ac_ = Object::makeBool(Arithmetic::eq(n, ac_));
                } else {
                    callWrongTypeOfArgumentViolationAfter(this, UC("="), UC("number"), L2(n, ac_));
                }
            }
            NEXT1;
        }
        CASE(NUMBER_GE)
        {
            NUM_CMP_LOCAL(>=, >=, ge);
            NEXT1;
        }
        CASE(NUMBER_GT)
        {
            NUM_CMP_LOCAL(>, >, gt);
            NEXT1;
        }
        CASE(NUMBER_LE)
        {
            NUM_CMP_LOCAL(<=, <=, le);
            NEXT1;
        }
        CASE(NUMBER_LT)
        {
            NUM_CMP_LOCAL(<, <, lt);
            NEXT1;
        }
        CASE(NUMBER_MUL)
        {
            const Object n = pop();
            if (n.isFlonum()) {
                if (ac_.isFlonum()) {
                    ac_ = Flonum::mul(n.toFlonum(), ac_.toFlonum());
                    NEXT1;
                } else if (Arithmetic::isRealValued(ac_)) {
                    ac_ = Object::makeFlonum(n.toFlonum()->value() * Arithmetic::realToDouble(ac_));
                    NEXT1;
                }
            }
            if (ac_.isFlonum()) {
                if (Arithmetic::isRealValued(n)) {
                    ac_ = Object::makeFlonum(ac_.toFlonum()->value() * Arithmetic::realToDouble(n));
                    NEXT1;
                }
            }

            ac_ = Arithmetic::mul(n, ac_);
            if (ac_.isFalse()) {
                callAssertionViolationAfter(this, UC("*"), UC("wrong type arguments"), L2(n, ac_));
            }
            NEXT1;
        }
        CASE(NUMBER_DIV)
        {
            bool isDiv0Error = false;
            const Object n = pop();
            ac_ = Arithmetic::div(n, ac_, isDiv0Error);
            if (isDiv0Error) {
                callAssertionViolationAfter(this, UC("/"), UC("division by zero"), L2(n, ac_));
            } else if (ac_.isFalse()) {
                callWrongTypeOfArgumentViolationAfter(this, UC("/"), UC("number"), L2(n, ac_));
            }
            NEXT1;
        }
        CASE(NUMBER_SUB)
        {
            const Object n = pop();
            // short cut for Fixnum. Benmarks tell me this is strongly required.
            if (n.isFixnum() && ac_.isFixnum()) {
                const int32_t val = n.toFixnum() - ac_.toFixnum();
                ac_ = Bignum::makeInteger(val);
            } else {
                ac_ = Arithmetic::sub(n, ac_);
                if (ac_.isFalse()) {
                    callWrongTypeOfArgumentViolationAfter(this, UC("-"), UC("number"), L2(n, ac_));
                }
            }
            NEXT1;
        }
        CASE(NUMBER_SUB_PUSH)
        {
            const Object n = pop();
            // short cut for Fixnum. Benmarks tell me this is strongly required.
            if (n.isFixnum() && ac_.isFixnum()) {
                const int32_t val = n.toFixnum() - ac_.toFixnum();
                ac_ = Bignum::makeInteger(val);
            } else {
                ac_ = Arithmetic::sub(n, ac_);
            }
            if (ac_.isFalse()) {
                callWrongTypeOfArgumentViolationAfter(this, UC("-"), UC("number"), L2(n, ac_));
            }
            push(ac_);
            NEXT1;
        }
        CASE(NUMBER_ADD_PUSH)
        {
            const Object n = pop();
            // short cut for Fixnum. Benmarks tell me this is strongly required.
            if (n.isFixnum() && ac_.isFixnum()) {
                const int32_t val = n.toFixnum() + ac_.toFixnum();
                ac_ = Bignum::makeInteger(val);
            } else {
                ac_ = Arithmetic::add(n, ac_);
                if (ac_.isFalse()) {
                    callWrongTypeOfArgumentViolationAfter(this, UC("-"), UC("number"), L2(n, ac_));
                    NEXT1;
                }
            }
            push(ac_);
            NEXT1;
        }
        CASE(PAIR_P)
        {
            ac_ = Object::makeBool(ac_.isPair());
            NEXT1;
        }
        CASE(READ)
        {
            bool errorOccured = false;
            TextualInputPort* inputPort = nullptr;
            if (ac_.isNil()) {
                inputPort = currentInputPort_.toTextualInputPort();
            } else {
                if (ac_.isTextualInputPort()) {
                    inputPort = ac_.toTextualInputPort();
                } else if (ac_.isTextualInputOutputPort()) {
                    inputPort = ac_.toTextualInputOutputPort();
                } else {
                    callAssertionViolationAfter(this, UC("read"), UC("textual input port required"), L1(ac_));
                    NEXT1;
                }
            }
            TRY_WITHOUT_DSTR
                ac_ = inputPort->getDatum(errorOccured);
                if (errorOccured) {
                    callLexicalAndIOReadAfter(this, UC("read"), inputPort->error());
                }
            CATCH(ioError)
                ioError.arg1 = (ac_.isNil()) ? currentInputPort_ : ac_;
                ioError.who = Object("read");
                callIOErrorAfter(this, ioError);
                NEXT1;
            END_TRY
            NEXT1;
        }
        CASE(READ_CHAR)
        {
            TextualInputPort* inputPort = nullptr;
            if (ac_.isNil()) {
                inputPort = currentInputPort_.toTextualInputPort();
            } else {
                if (ac_.isTextualInputPort()) {
                    inputPort = ac_.toTextualInputPort();
                } else if (ac_.isTextualInputOutputPort()) {
                    inputPort = ac_.toTextualInputOutputPort();
                } else {
                    callAssertionViolationAfter(this, UC("read"), UC("textual input port required"), L1(ac_));
                    NEXT1;
                }
            }
            TRY_WITHOUT_DSTR
                const ucs4char c = inputPort->getChar();
                ac_= c == EOF ? Object::Eof : Object::makeChar(c);
            CATCH(ioError)
                ioError.arg1 = (ac_.isNil()) ? currentInputPort_ : ac_;
                ioError.who = Object("read-char");
                callIOErrorAfter(this, ioError);
                NEXT1;
            END_TRY
            NEXT1;
        }
        CASE(REDUCE)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            sp_ = fp_ + n.toFixnum();;
            NEXT;
        }
        CASE(REFER_FREE)
        {
            operand = fetchOperand();
            VM_ASSERT(operand.isFixnum());
            ac_ = referFree(operand);
            NEXT1;
        }
        CASE(REFER_FREE_PUSH)
        {
            push(referFree(fetchOperand()));
            NEXT;
        }
        CASE(REFER_FREE_CALL)
        {
            ac_ = referFree(fetchOperand());
            operand = fetchOperand();
            #include "call.inc.cpp"
            NEXT;
        }
        CASE(REFER_GLOBAL)
        {
            const Object id = fetchOperand();
            if (id.isGloc()) {
                ac_ = id.toGloc()->value();
            } else {
                const Object val = nameSpace->ref(id, notFound_);
                if (val == notFound_) {
                    callUndefinedViolationAfter(this,
                                                L1(unGenSym(id)),
                                                UC("unbound variable")
                                                // R6RS mode requires demangle of symbol.
                                                );
                } else {
                    ac_ = val.toGloc()->value();
                    *(pc_ - 1) = val;
                }
            }
            NEXT1;
        }
        CASE(REFER_GLOBAL_PUSH)
        {
            const Object id = fetchOperand();
            if (id.isGloc()) {
                ac_ = id.toGloc()->value();
            } else {
                const Object val = nameSpace->ref(id, notFound_);
                if (val == notFound_) {
                    callAssertionViolationAfter(this,
                                                UC("eval"),
                                                UC("unbound variable"),
                                                // R6RS mode requires demangle of symbol.
                                                L1(unGenSym(id)));
                } else {
                    ac_ = val.toGloc()->value();
                    *(pc_ - 1) = val;
                }
            }
            push(ac_);
            NEXT1;
        }
        CASE(REFER_GLOBAL_CALL)
        {
            const Object id = fetchOperand();
            if (id.isGloc()) {
                ac_ = id.toGloc()->value();
            } else {
                const Object val = nameSpace->ref(id, notFound_);
                if (val == notFound_) {
                    callAssertionViolationAfter(this,
                                                UC("eval"),
                                                UC("unbound variable"),
                                                L1(unGenSym(id)));
                    NEXT1; // for error handling
                } else {
                    ac_ = val.toGloc()->value();
                    *(pc_ - 1) = val;
                }
            }
            operand = fetchOperand();
            #include "call.inc.cpp"
            NEXT;
        }
        CASE(REFER_LOCAL)
        {
            operand = fetchOperand();
            VM_ASSERT(operand.isFixnum());
            ac_ = referLocal(operand.toFixnum());
            NEXT1;
        }
        CASE(REFER_LOCAL_CALL)
        {
            operand = fetchOperand();
            VM_ASSERT(operand.isFixnum());
            ac_ = referLocal(operand.toFixnum());
            operand = fetchOperand();
            #include "call.inc.cpp"
            NEXT;
        }
        // LOCAL_CALL is lighter than CALL
        // We can omit checking closure type and arguments length.
        CASE(LOCAL_CALL)
        {
            VM_ASSERT(ac_.isClosure());
            const Closure* const c = ac_.toClosure();
            if (c->maxStack + sp_ >= stackEnd_) {
//                printf("CALL: stack expansion\n");
                expandStack(stackSize_ / 10);
            }
            COUNT_CALL(ac_);
            const Object argLength = fetchOperand();
            VM_ASSERT(argLength.isFixnum());
            dc_ = ac_;
            cl_ = ac_;
            pc_ = c->pc;
            fp_ = sp_ - argLength.toFixnum();
            NEXT;
        }
        CASE(LOCAL_TAIL_CALL)
        {
            const Object depth = fetchOperand();
            VM_ASSERT(depth.isFixnum());

            const Object diff = fetchOperand();
            VM_ASSERT(diff.isFixnum());
            sp_ = shiftArgsToBottom(sp_, depth.toFixnum(), diff.toFixnum());

            VM_ASSERT(ac_.isClosure());
            const Closure* const c = ac_.toClosure();
            if (c->maxStack + sp_ >= stackEnd_) {
//                printf("CALL: stack expansion\n");
                expandStack(stackSize_ / 10);
            }
            COUNT_CALL(ac_);
            const Object argLength = depth;
            dc_ = ac_;
            cl_ = ac_;
            pc_ = c->pc;
            fp_ = sp_ - argLength.toFixnum();
            NEXT;
        }
        CASE(REFER_LOCAL_PUSH_CONSTANT)
        {
            const Object index = fetchOperand();
            MOSH_ASSERT(index.isFixnum());
            push(referLocal(index.toFixnum()));
            ac_= fetchOperand();
            NEXT1;
        }
        // appears on typical named let loop
        CASE(REFER_LOCAL_BRANCH_NOT_NULL)
        {
            const Object i = fetchOperand();
            MOSH_ASSERT(i.isFixnum());
            ac_ = Object::makeBool(referLocal(i.toFixnum()).isNil());
            BRANCH_ON_FALSE;
            NEXT;
        }
        // appears on tak
        CASE(REFER_LOCAL_BRANCH_NOT_LT)
        {
            const Object i = fetchOperand();
            MOSH_ASSERT(i.isFixnum());
            ac_ = referLocal(i.toFixnum());
            NUM_CMP_LOCAL(<, <, lt);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // appears on fib
        CASE(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE)
        {
            const Object i = fetchOperand();
            MOSH_ASSERT(i.isFixnum());
            // we can omit "PUSH" insruction
            ac_ = fetchOperand();
            NUM_CMP(<=, <=, le, referLocal(i.toFixnum()));
            BRANCH_ON_FALSE;
            NEXT;
        }
        CASE(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE)
        {
            const Object i = fetchOperand();
            MOSH_ASSERT(i.isFixnum());
            // we can omit "PUSH" insruction
            ac_ = fetchOperand();
            NUM_CMP(>=, >=, ge, referLocal(i.toFixnum()));
            BRANCH_ON_FALSE;
            NEXT;
        }
        // appears on named let loop
        CASE(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL)
        {
            const Object i = fetchOperand();
            MOSH_ASSERT(i.isFixnum());
            // we can omit "PUSH" insruction
            ac_ = fetchOperand();
            NUM_CMP(==, =, eq, referLocal(i.toFixnum()));
            BRANCH_ON_FALSE;
            NEXT;
        }
        CASE(REFER_LOCAL_PUSH)
        {
            const Object n = fetchOperand();
            VM_ASSERT(n.isFixnum());
            push(referLocal(n.toFixnum()));
            NEXT;
        }
        CASE(RESTORE_CONTINUATION)
        {
            // Stores arguments of the continuation to values registers.
            const Object argumentsLength = fetchOperand();
            VM_ASSERT(argumentsLength.isFixnum());
            const int num = argumentsLength.toFixnum();
            if (num > maxNumValues_ + 1) {
                callAssertionViolationAfter(this, UC("values"), UC("too many values"), Pair::list1(argumentsLength));
            }
            numValues_ = num;
            if (num != 0) {
                for (int i = 0; i < num - 1; i++) {
                    values_[i] = index(sp_, num - i - 2);
                }
                ac_ = index(sp_, num -  1);
            }

            // Restore the stack
            const Object stack = fetchOperand();
            sp_ = stack_ + stack.toStack()->restore(stack_);

            // Shift unnecessary stack
            const int depth = 0;

            const Object diffObject = fetchOperand();
            VM_ASSERT(diffObject.isFixnum());
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);
            operand = Object::makeFixnum(0);
            goto return_entry;
            NEXT;
        }
//         CASE(NUMBER_ADD_RETURN)
//         {
//             const Object n = pop();
//             // short cut for Fixnum. Benmarks tell me this is strongly required.
//             if (n.isFixnum() && ac_.isFixnum()) {
//                 const int32_t val = n.toFixnum() + ac_.toFixnum();
//                 ac_ = Bignum::makeInteger(val);
//             } else {
//                 ac_ = Arithmetic::add(n, ac_);
//             }
//             operand = fetchOperand();
//             goto return_entry;
//         }
        CASE(RETURN)
        {
            operand = fetchOperand();
        return_entry:
            VM_ASSERT(operand.isFixnum());
            Object* const sp = sp_ - operand.toFixnum();

            const Object fpObject = index(sp, 0);
            VM_ASSERT(fpObject.isObjectPointer());
            fp_ = fpObject.toObjectPointer();

            cl_ = index(sp, 1);
            if (!cl_.isProcedure()) {
                VM_LOG1("proc = ~a\n", cl_);
            }
            VM_ASSERT(cl_.isProcedure());

            dc_ = index(sp, 2);
            VM_ASSERT(dc_.isProcedure());

            const Object pcObject = index(sp, 3);
            VM_ASSERT(pcObject.isObjectPointer());
            pc_ = pcObject.toObjectPointer();

            sp_ = sp - 4;
            NEXT;
        }
        CASE(SET_CAR)
        {
            const Object p = pop();
            if (!p.isPair()) {
                callAssertionViolationAfter(this, UC("set-car!"), UC("pair required"), Pair::list1(p));
                NEXT1;
            }

            p.car() = ac_;
            ac_ = Object::Undef;
            NEXT1;
        }
        CASE(SET_CDR)
        {
            const Object p = pop();
            if (!p.isPair()) {
                callAssertionViolationAfter(this, UC("set-cdr!"), UC("pair required"), Pair::list1(p));
                NEXT1;
            }
            p.cdr() = ac_;
            ac_ = Object::Undef;
            NEXT1;
        }
        //---------------------------- SHIFTJ -----------------------------
        //
        // SHIFT for embedded jump which appears in named let optimization.
        //   Two things happens.
        //   1. SHIFT the stack (same as SHIFT operation)
        //   2. Restore fp and c registers.
        //      This is necessary for jump which is across let or closure boundary.
        //      new-fp => new-sp - arg-length
        //
        CASE(SHIFTJ)
        {
            const Object depthObject = fetchOperand();
            VM_ASSERT(depthObject.isFixnum());

            const int depth = depthObject.toFixnum();

            const Object diffObject = fetchOperand();
            VM_ASSERT(diffObject.isFixnum());
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);
            fp_ = sp_ - depth;

            const Object displayCount = fetchOperand();
            VM_ASSERT(displayCount.isFixnum());
            for (int i = displayCount.toFixnum(); i > 0; i--) {
                dc_ = dc_.toClosure()->prev;
            }
            VM_ASSERT(dc_.isClosure());
            NEXT;
        }
        // (SHIFT) instruction is deprecated
        CASE(SHIFT)
        {
            const Object depthObject = fetchOperand();
            VM_ASSERT(depthObject.isFixnum());

            const int depth = depthObject.toFixnum();
            const Object diffObject = fetchOperand();
            VM_ASSERT(diffObject.isFixnum());
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);

            NEXT;
        }
        // (SHIFT_CALL) instruction is deprecated
        CASE(SHIFT_CALL)
        {
            const Object depthObject = fetchOperand();
            const Object diffObject = fetchOperand();

            VM_ASSERT(depthObject.isFixnum());
            MOSH_ASSERT(diffObject.isFixnum());
            const int depth = depthObject.toFixnum();
            const int diff  = diffObject.toFixnum();
            sp_ = shiftArgsToBottom(sp_, depth, diff);
            operand = fetchOperand();
            #include "call.inc.cpp"
            NEXT;
            MOSH_FATAL(false);
        }
        CASE(SYMBOL_P)
        {
            ac_ = Object::makeBool(ac_.isSymbol());
            NEXT1;
        }
        CASE(TEST)
        {
        test_entry:
            if (ac_.isFalse()) {
                const Object skipSize = fetchOperand();
                MOSH_ASSERT(skipSize.isFixnum());
                skip(skipSize.toFixnum() - 1);
            } else {
                pc_++;
            }
            NEXT;
        }
        CASE(NOT_TEST)
        {
            ac_ = ac_.isFalse() ? Object::True : Object::False;
            goto test_entry;
        }
        // Branch on not eq?
        CASE(BRANCH_NOT_EQ)
        {
            ac_ = Object::makeBool(pop().eq(ac_));
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not eqv?
        CASE(BRANCH_NOT_EQV)
        {
            ac_ = Object::makeBool(eqv(pop(), ac_));
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not equal?
        CASE(BRANCH_NOT_EQUAL)
        {
            Equal e;
            ac_ = Object::makeBool(e.equal(pop(), ac_));
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not less than or equal
        CASE(BRANCH_NOT_LE)
        {
            NUM_CMP_LOCAL(<=, <=, le);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not less than
        CASE(BRANCH_NOT_LT)
        {
            NUM_CMP_LOCAL(<, <, lt);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not greater than or equal
        CASE(BRANCH_NOT_GE)
        {
            NUM_CMP_LOCAL(>=, >=, ge);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not greater than
        CASE(BRANCH_NOT_GT)
        {
            NUM_CMP_LOCAL(>, >, gt);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not number equal
        CASE(BRANCH_NOT_NUMBER_EQUAL)
        {
            NUM_CMP_LOCAL(==, =, eq);
            BRANCH_ON_FALSE;
            NEXT;
        }
        // Branch on not null
        CASE(BRANCH_NOT_NULL)
        {
            ac_ = Object::makeBool(ac_.isNil());
            BRANCH_ON_FALSE;
            NEXT;
        }
        CASE(UNDEF)
        {
            ac_ = Object::Undef;
            NEXT1;
        }
        CASE(VECTOR_LENGTH)
        {
            ac_ = Object::makeFixnum(ac_.toVector()->length());
            NEXT1;
        }
        CASE(VECTOR_P)
        {
            ac_ = Object::makeBool(ac_.isVector());
            NEXT1;
        }
//         CASE(REFER_LOCAL_VECTOR_REF)
//         {
//             const Object n = fetchOperand();
//             MOSH_ASSERT(n.isFixnum());
//             ac_ = referLocal(n.toFixnum());
//             // *** Fall Through ***
//         }

        CASE(VECTOR_REF)
        {
            const Object obj = pop();
            if (obj.isVector()) {
                if (ac_.isFixnum()) {
                    const int index = ac_.toFixnum();
                    Vector* const v = obj.toVector();
                    if (v->isValidIndex(index)) {
                        ac_ = v->ref(index);
                    } else {
                        callAssertionViolationAfter(this,
                                                    UC("vector-ref"),
                                                    UC("index out of range"),
                                                    L1(ac_));
                    }
                } else {
                    callAssertionViolationAfter(this,
                                                UC("vector-ref"),
                                                UC("index exact integer required but got "),
                                                L1(ac_));
                }
            } else {
                callAssertionViolationAfter(this,
                                            UC("vector-ref"),
                                            UC("vector required"),
                                            L1(obj));
            }
            NEXT1;
        }
        CASE(SIMPLE_STRUCT_REF)
        {
            const Object obj = pop();
            if (obj.isSimpleStruct()) {
                MOSH_ASSERT(ac_.isFixnum());
                const int index = ac_.toFixnum();
                SimpleStruct* const s = obj.toSimpleStruct();
                if (s->isValidIndex(index)) {
                    ac_ = s->ref(index);
                } else {
                    callAssertionViolationAfter(this,
                                                UC("simple-struct-ref"),
                                                UC("index out of range"),
                                                L2(obj, ac_));
                }
            } else {
                callAssertionViolationAfter(this,
                                            UC("simple-struct-ref"),
                                            UC("simple-struct required"),
                                            L1(obj));
            }
            NEXT1;
        }
//         CASE(VECTOR_REF_PUSH)
//         {
//             const Object v = pop();
//             MOSH_ASSERT(ac_.isFixnum());
//             if (v.isVector()) {
//                 ac_ = v.toVector()->ref(ac_.toFixnum());
//             } else {
//                 callAssertionViolationAfter(UC("vector-ref"),
//                                             UC("vector required"),
//                                             L1(v));
//             }
//             push(ac_);
//             NEXT1;
//         }
//         CASE(PUSH_CONSTANT_VECTOR_SET)
//         {
//             push(ac_);
//             ac_ = fetchOperand();
//             goto vector_set_entry;
//         }
//         CASE(REFER_LOCAL_VECTOR_SET)
//         {
//             const Object n = fetchOperand();
//             MOSH_ASSERT(n.isFixnum());
//             ac_ = referLocal(n.toFixnum());
//             // *** Fall Through ***
//         }
        CASE(VECTOR_SET)
        {
            const Object n = pop();
            const Object obj = pop();
            if (obj.isVector()) {
                if (n.isFixnum()) {
                    const int index = n.toFixnum();
                    Vector* const v = obj.toVector();
                    if (v->isValidIndex(index)) {
                        v->set(index, ac_);
                        ac_ = Object::Undef;
                    } else {
                        callAssertionViolationAfter(this,
                                                    UC("vector-set!"),
                                                    UC("index out of range"),
                                                    L1(n));
                    }
                } else {
                    callAssertionViolationAfter(this,
                                                UC("vector-set!"),
                                                UC("index, number required"),
                                                L1(n));

                }
            } else {
                callAssertionViolationAfter(this,
                                            UC("vector-set!"),
                                            UC("vector required"),
                                            L1(obj));
            }
            NEXT1;
        }
        CASE(VALUES)
        {
            //  values stack layout
            //    (value 'a 'b 'c 'd)
            //    ==>
            //    =====
            //      a
            //    =====
            //      b
            //    =====
            //      c    [ac_] = d
            //    =====
            //  values are stored in [valuez vector] and [a-reg] like following.
            //  #(b c d)
            //  [ac_] = a
            const Object numObject = fetchOperand();
            MOSH_ASSERT(numObject.isFixnum());
            const int num = numObject.toFixnum();
            if (num > maxNumValues_ + 1) {
                callAssertionViolationAfter(this, UC("values"), UC("too many values"), Pair::list1(Object::makeFixnum(num)));
            } else {
                numValues_ = num;
                if (num >= 0) {
                    for (int i = num - 1; i > 0; i--) {
                        values_[i - 1] = ac_;
                        ac_ = index(sp_, num - i - 1);
                    }
                }

                if (numValues_ > 1) {
                    sp_ =  sp_ - (numValues_ - 1);
                } else {
                    // there's no need to push
                }
            }
            if (num == 0) {
                ac_ = Object::Undef;
            }
            NEXT;
        }
        CASE(RECEIVE)
        {
            const Object reqargsObject = fetchOperand();
            const Object optargObject = fetchOperand();
            MOSH_ASSERT(reqargsObject.isFixnum());
            const int reqargs = reqargsObject.toFixnum();
            MOSH_ASSERT(optargObject.isFixnum());
            const int optarg  = optargObject.toFixnum();
            if (numValues_ < reqargs) {
                callAssertionViolationAfter(this,
                                            UC("receive"),
                                            UC("received fewer values than expected"),
                                            L2(Object::makeFixnum(numValues_),
                                               Object::makeFixnum(reqargs)));
                NEXT;
            } else if (optarg == 0 && numValues_ > reqargs) {
                callAssertionViolationAfter(this,
                                            UC("receive"),
                                            UC("received more values than expected"),
                                            L2(Object::makeFixnum(numValues_),
                                               Object::makeFixnum(reqargs)));
                NEXT;
            }
            // (receive (a b c) ...)
            if (optarg == 0) {
                if (reqargs > 0) {
                    push(ac_);
                }
                for (int i = 0; i < reqargs - 1; i++) {
                    push(values_[i]);
                }
            // (receive a ...)
            } else if (reqargs == 0) {

                Object ret = numValues_ == 0 ? Object::Nil : Pair::list1(ac_);
                for (int i = 0; i < numValues_ - 1; i++) {
                    ret = Pair::appendD(ret, Pair::list1(values_[i]));
                }
                push(ret);
            // (receive (a b . c) ...)
            } else {
                Object ret = Object::Nil;
                push(ac_);
                for (int i = 0; i < numValues_ - 1; i++) {
                    if (i < reqargs - 1) {
                        push(values_[i]);
                    } else {
                        ret = Pair::appendD(ret, Pair::list1(values_[i]));
                    }
                }
                push(ret);
            }
            NEXT1;
        }
        CASE(COMPILE_ERROR)
        {
            const Object who = fetchOperand();
            const Object message = fetchOperand();
            const Object irritants = fetchOperand();
            callAssertionViolationAfter(this, who, message, irritants);
            NEXT;
        }
        CASE(UNFIXED_JUMP)
        {
            callAssertionViolationAfter(this, UC("UNFIXED_JUMP"), UC("bug of VM"));
            NEXT;
        }
        CASE(STOP)
        {
            printf("STOP for debug\n");
            exit(-1);
        }
        CASE(DYNAMIC_WINDERS)
        {
            ac_ = dynamicWinders();
            NEXT1;
        }
        DEFAULT
        {
            callAssertionViolationAfter(this, UC("VM"), UC("unknown instruction, bug of VM"));
            NEXT;
        }
        } // SWITCH
    }
}
