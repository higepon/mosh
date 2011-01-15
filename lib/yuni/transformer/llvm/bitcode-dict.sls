(library (yuni transformer llvm bitcode-dict)
         (export builtin-abbid
                 llvm-ir-blockid
                 record-BLOCKINFO  
                 record-MODULE_BLOCK 
                 record-PARAMATTR_BLOCK 
                 record-TYPE_BLOCK 
                 record-TYPE_SYMTAB_BLOCK 
                 record-VALUE_SYMTAB_BLOCK 
                 record-METADATA_BLOCK 
                 record-CONSTANTS_BLOCK 
                 code-linkage code-visibility code-callingconv
                 op-cast op-binary 
                 record-FUNCTION_BLOCK)
         (import (rnrs)
                 (yuni core))

(define-syntax to-symbol
  (syntax-rules ()
    ((_ input (sym val) ...)
     (case input
       ((val) 'sym)
       ...
       (else input)))))

(define-syntax to-value
  (syntax-rules ()
    ((_ input (sym val) ...)
     (case input
       ((sym) val)
       ...
       (else input)))))

(define-syntax dictconverter
  (syntax-rules ()
    ((_ input spec ...)
     (if (symbol? input)
       (to-value input spec ...)
       (to-symbol input spec ...)))))

(define-syntax define-llvm-dict
  (syntax-rules ()
    ((_ name specs ...)
     (define (name x) (dictconverter x specs ...)))))

(define-llvm-dict builtin-abbid
                  (END_BLOCK 0)
                  (ENTER_SUBBLOCK 1)
                  (DEFINE_ABBREV 2)
                  (UNABBREV_RECORD 3))

(define-llvm-dict llvm-ir-blockid
                  (MODULE_BLOCK 8)
                  (PARAMATTR_BLOCK 9)
                  (TYPE_BLOCK 10)
                  (CONSTANTS_BLOCK 11)
                  (FUNCTION_BLOCK 12)
                  (TYPE_SYMTAB_BLOCK 13)
                  (VALUE_SYMTAB_BLOCK 14)
                  (METADATA_BLOCK 15)
                  (METADATA_ATTACHMENT 16))

(define-llvm-dict record-VALUE_SYMTAB_BLOCK
                  (ENTRY 1)
                  (BBENTRY 2))

(define-llvm-dict record-TYPE_SYMTAB_BLOCK
                  (ENTRY 1))

(define-llvm-dict record-BLOCKINFO ;; id = 0
                  (SETBID 1)
                  (BLOCKNAME 2)
                  (SETRECORDNAME 3))

(define-llvm-dict record-MODULE_BLOCK
                  (VERSION 1)
                  (TRIPLE 2)
                  (DATALAYOUT 3)
                  (ASM 4)
                  (SECTIONNAME 5)
                  (DEPLIB 6)
                  (GLOBALVAR 7)
                  (FUNCTION 8)
                  (ALIAS 9)
                  (PURGEVALS 10)
                  (GCNAME 11))

(define-llvm-dict record-PARAMATTR_BLOCK
                  (ENTRY 1))

(define-llvm-dict record-TYPE_BLOCK
                  (NUMENTRY 1)
                  (VOID 2)
                  (FLOAT 3)
                  (DOUBLE 4)
                  (LABEL 5)
                  (OPAQUE 6)
                  (INTEGER 7)
                  (POINTER 8)
                  (FUNCTION 9)
                  (STRUCT 10)
                  (ARRAY 11)
                  (VECTOR 12)
                  (X86_FP80 13)
                  (FP128 14)
                  (PPC_FP128 15)
                  (METADATA 16))

(define-llvm-dict record-METADATA_BLOCK
                  (STRING 1)
                  (NODE 2) ;; deprecated
                  (FN_NODE 3) ;; deprecated
                  (NAME 4)
                  (NAMED_NODE 5) ;; deprecated
                  (KIND 6)
                  (ATTACHMENT 7) ;; deprecated
                  (NODE2 8)
                  (FN_NODE2 9)
                  (NAMED_NODE2 10)
                  (ATTACHMENT2 11))

(define-llvm-dict record-CONSTANTS_BLOCK
                  (SETTYPE 1)
                  (NULL 2)
                  (UNDEF 3)
                  (INTEGER 4) ;; signed VBR
                  (WIDE_INTEGER 5) ;; signed VBR
                  (FLOAT 6)
                  (AGGREGATE 7)
                  (STRING 8)
                  (CSTRING 9)
                  (CE_BINOP 10)
                  (CE_CAST 11)
                  (CE_GEP 12)
                  (CE_SELECT 13)
                  (CE_EXTRACTELT 14)
                  (CE_INSERTELT 15)
                  (CE_SHUFFLEVEC 16)
                  (CE_CMP 17)
                  (INLINEASM 18)
                  (CE_SHUFVEC_EX 19)
                  (CE_INBOUNDS_GEP 20)
                  (BLOCKADDRESS 21))

(define-llvm-dict op-cast
                  (TRUNC 0)
                  (ZEXT 1)
                  (SEXT 2)
                  (FPTOUI 3)
                  (FPTOSI 4)
                  (UITOFP 5)
                  (SITOFP 6)
                  (FPTRUNC 7)
                  (FPEXT 8)
                  (PTRTOINT 9)
                  (INTTOPTR 10)
                  (BITCAST 11))

(define-llvm-dict op-binary
                  (ADD 0)
                  (SUB 1)
                  (MUL 2)
                  (UDIV 3)
                  (SDIV 4)
                  (UREM 5)
                  (SREM 6)
                  (SHL 7)
                  (LSHR 8)
                  (ASHR 9)
                  (AND 10)
                  (OR 11)
                  (XOR 12))

(define-llvm-dict code-callingconv
                  (ccc 0)
                  (fastcc 8)
                  (coldcc 9)
                  (x86_stdcallcc 64)
                  (x86_fastcallcc 65)
                  (arm_apcscc 66)
                  (arm_aapcscc 67)
                  (arm_aapcs_vfpcc 68))

(define-llvm-dict code-linkage
                  (external 0)
                  (weak 1)
                  (appending 2)
                  (internal 3)
                  (linkonce 4)
                  (dllimport 5)
                  (dllexport 6)
                  (extern_weak 7)
                  (common 8)
                  (private 9)
                  (weak_odr 10)
                  (linkonce_odr 11)
                  (available_externally 12)
                  (linker_private 13))

(define-llvm-dict code-visibility
                  (default 0)
                  (hidden 1)
                  (protected 2))

(define-llvm-dict record-FUNCTION_BLOCK
                  (DECLAREBLOCKS 1)
                  (BINOP 2)
                  (CAST 3)
                  (GEP 4)
                  (SELECT 5)
                  (EXTRACTELT 6)
                  (INSERTELT 7)
                  (SHUFFLEVEC 8)
                  (CMP 9)
                  (RET 10)
                  (BR 11)
                  (SWITCH 12)
                  (INVOKE 13)
                  (UNWIND 14)
                  (UNREACHABLE 15)
                  (PHI 16)
                  (MALLOC 17)
                  (FREE 18)
                  (ALLOCA 19)
                  (LOAD 20)
                  (STORE 21) ;; deprecated
                  (CALL 22) ;; deprecated
                  (VAARG 23)
                  (STORE2 24)
                  (GETRESULT 25) ;; deprecated
                  (EXTRACTVAL 26)
                  (INSERTVAL 27)
                  (CMP2 28)
                  (VSELECT 29)
                  (INBOUNDS_GEP 30)
                  (INDIRECTBR 31)
                  (DEBUG_LOC 32) ;; deprecated
                  (DEBUG_LOC_AGAIN 33)
                  (INST_CALL2 34)
                  (DEBUG_LOC2 35))



)
