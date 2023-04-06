use crate::bug;
use crate::ports::StdLib;
/// Scheme procedures written in Rust.
/// The procedures will be exposed to the VM via free vars.
use crate::{
    as_binary_input_port_mut, as_binary_output_port_mut, as_bytevector, as_char, as_f32, as_f64,
    as_flonum, as_isize, as_output_port_mut, as_port, as_port_mut, as_sstring, as_symbol,
    as_text_input_port_mut, as_text_output_port_mut, as_transcoder, as_u8, as_usize,
    check_is_closure, check_is_closure_or_false, check_is_transcoder_or_false,
    equal::Equal,
    error::{self, Error, ErrorType},
    fasl::{FaslReader, FaslWriter},
    gc::{Gc, GcRef},
    number_lexer::NumberLexer,
    number_reader::NumberParser,
    numbers::{
        self, imag, integer_div, log2, real, Compnum, FixnumExt, Flonum, GcObjectExt, ObjectExt,
        SchemeError,
    },
    obj_as_text_input_port_mut, obj_as_text_output_port_mut,
    objects::{
        Bytevector, EqHashtable, EqvHashtable, EqvKey, GenericHashKey, GenericHashtable, Hashtable,
        Object, Pair, SString, SimpleStruct, Symbol,
    },
    ports::{
        BinaryFileInputOutputPort, BinaryFileInputPort, BinaryFileOutputPort, BinaryInputPort,
        BinaryOutputPort, BufferMode, BytevectorInputPort, BytevectorOutputPort,
        CustomBinaryInputOutputPort, CustomBinaryInputPort, CustomBinaryOutputPort,
        CustomTextInputOutputPort, CustomTextInputPort, CustomTextOutputPort, EolStyle,
        ErrorHandlingMode, Latin1Codec, OutputPort, Port, StdErrorPort, StdInputPort, StdLibExt,
        StdOutputPort, StringInputPort, StringOutputPort, TextInputPort, TextOutputPort,
        TranscodedInputOutputPort, TranscodedInputPort, TranscodedOutputPort, Transcoder,
        UTF16Codec, UTF8Codec,
    },
    vm::Vm,
};
use byteorder::{BigEndian, ByteOrder, LittleEndian};
use std::{
    env::{self, current_dir, current_exe},
    fs::{self, File, OpenOptions},
    mem,
    path::Path,
    process,
    time::{SystemTime, UNIX_EPOCH},
};

use num_bigint::BigInt;
use num_traits::{FromPrimitive, ToPrimitive, Zero};

static mut GENSYM_PREFIX: char = 'a';
static mut GENSYM_INDEX: isize = 0;

fn pair_required_error(name: &str, args: &[Object]) -> error::Result<Object> {
    type_required_error(name, "pair", args)
}
fn number_required_error(name: &str, args: &[Object]) -> error::Result<Object> {
    type_required_error(name, "number", args)
}
fn type_required_error(name: &str, type_str: &str, args: &[Object]) -> error::Result<Object> {
    return Err(error::Error::new(
        ErrorType::AssertionViolation,
        name,
        &format!("{} required", type_str),
        args,
    ));
}

#[macro_export]
macro_rules! generic_error {
    ($name:expr, $args:expr, $fmt:expr, $($fmt_arg:tt)+) => (Err(error::Error::new(
        ErrorType::AssertionViolation,
        $name,
        &format!($fmt, $($fmt_arg)+),
        $args,
    )));
}

pub fn default_free_vars(gc: &mut Gc) -> Vec<Object> {
    vec![
        gc.new_procedure(is_number, "number?"),
        gc.new_procedure(cons, "cons"),
        gc.new_procedure(consmul, "cons*"),
        gc.new_procedure(car, "car"),
        gc.new_procedure(cdr, "cdr"),
        gc.new_procedure(is_null, "null?"),
        gc.new_procedure(set_car_destructive, "set-car!"),
        gc.new_procedure(set_cdr_destructive, "set-cdr!"),
        gc.new_procedure(sys_display, "sys-display"),
        gc.new_procedure(rxmatch, "rxmatch"),
        gc.new_procedure(is_regexp, "regexp?"),
        gc.new_procedure(regexp_to_string, "regexp->string"),
        gc.new_procedure(rxmatch_start, "rxmatch-start"),
        gc.new_procedure(rxmatch_end, "rxmatch-end"),
        gc.new_procedure(rxmatch_after, "rxmatch-after"),
        gc.new_procedure(rxmatch_before, "rxmatch-before"),
        gc.new_procedure(rxmatch_substring, "rxmatch-substring"),
        gc.new_procedure(make_string, "make-string"),
        gc.new_procedure(string_set_destructive, "string-set!"),
        gc.new_procedure(string_length, "string-length"),
        gc.new_procedure(string_to_symbol, "string->symbol"),
        gc.new_procedure(string_to_number, "string->number"),
        gc.new_procedure(string_append, "string-append"),
        gc.new_procedure(string_split, "string-split"),
        gc.new_procedure(string, "string"),
        gc.new_procedure(number_to_string, "number->string"),
        gc.new_procedure(reverse, "reverse"),
        gc.new_procedure(is_eof_object, "eof-object?"),
        gc.new_procedure(read_char, "read-char"),
        gc.new_procedure(peek_char, "peek-char"),
        gc.new_procedure(is_charequal, "char=?"),
        gc.new_procedure(is_string, "string?"),
        gc.new_procedure(get_environment_variable, "get-environment-variable"),
        gc.new_procedure(get_environment_variables, "get-environment-variables"),
        gc.new_procedure(is_equal, "equal?"),
        gc.new_procedure(open_string_input_port, "open-string-input-port"),
        gc.new_procedure(open_output_string, "open-output-string"),
        gc.new_procedure(sys_port_seek, "sys-port-seek"),
        gc.new_procedure(close_output_port, "close-output-port"),
        gc.new_procedure(digit_to_integer, "digit->integer"),
        gc.new_procedure(get_remaining_input_string, "get-remaining-input-string"),
        gc.new_procedure(directory_list, "directory-list"),
        gc.new_procedure(is_file_exists, "file-exists?"),
        gc.new_procedure(delete_file, "delete-file"),
        gc.new_procedure(get_output_string, "get-output-string"),
        gc.new_procedure(string_to_regexp, "string->regexp"),
        gc.new_procedure(char_to_integer, "char->integer"),
        gc.new_procedure(integer_to_char, "integer->char"),
        gc.new_procedure(format, "format"),
        gc.new_procedure(current_input_port, "current-input-port"),
        gc.new_procedure(current_output_port, "current-output-port"),
        gc.new_procedure(
            set_current_input_port_destructive,
            "set-current-input-port!",
        ),
        gc.new_procedure(
            set_current_output_port_destructive,
            "set-current-output-port!",
        ),
        gc.new_procedure(is_char, "char?"),
        gc.new_procedure(write, "write"),
        gc.new_procedure(gensym, "gensym"),
        gc.new_procedure(is_stringequal, "string=?"),
        gc.new_procedure(caaaar, "caaaar"),
        gc.new_procedure(caaadr, "caaadr"),
        gc.new_procedure(caaar, "caaar"),
        gc.new_procedure(caadar, "caadar"),
        gc.new_procedure(caaddr, "caaddr"),
        gc.new_procedure(caadr, "caadr"),
        gc.new_procedure(caar, "caar"),
        gc.new_procedure(cadaar, "cadaar"),
        gc.new_procedure(cadadr, "cadadr"),
        gc.new_procedure(cadar, "cadar"),
        gc.new_procedure(caddar, "caddar"),
        gc.new_procedure(cadddr, "cadddr"),
        gc.new_procedure(caddr, "caddr"),
        gc.new_procedure(cadr, "cadr"),
        gc.new_procedure(cdaaar, "cdaaar"),
        gc.new_procedure(cdaadr, "cdaadr"),
        gc.new_procedure(cdaar, "cdaar"),
        gc.new_procedure(cdadar, "cdadar"),
        gc.new_procedure(cdaddr, "cdaddr"),
        gc.new_procedure(cdadr, "cdadr"),
        gc.new_procedure(cdar, "cdar"),
        gc.new_procedure(cddaar, "cddaar"),
        gc.new_procedure(cddadr, "cddadr"),
        gc.new_procedure(cddar, "cddar"),
        gc.new_procedure(cdddar, "cdddar"),
        gc.new_procedure(cddddr, "cddddr"),
        gc.new_procedure(cdddr, "cdddr"),
        gc.new_procedure(cddr, "cddr"),
        gc.new_procedure(is_symbolequal, "symbol=?"),
        gc.new_procedure(is_booleanequal, "boolean=?"),
        gc.new_procedure(is_vector, "vector?"),
        gc.new_procedure(is_list, "list?"),
        gc.new_procedure(list, "list"),
        gc.new_procedure(memq, "memq"),
        gc.new_procedure(is_eq, "eq?"),
        gc.new_procedure(is_eqv, "eqv?"),
        gc.new_procedure(member, "member"),
        gc.new_procedure(is_boolean, "boolean?"),
        gc.new_procedure(symbol_to_string, "symbol->string"),
        gc.new_procedure(string_ref, "string-ref"),
        gc.new_procedure(get_timeofday, "get-timeofday"),
        gc.new_procedure(make_eq_hashtable, "make-eq-hashtable"),
        gc.new_procedure(make_eqv_hashtable, "make-eqv-hashtable"),
        gc.new_procedure(hashtable_set_destructive, "hashtable-set!"),
        gc.new_procedure(hashtable_ref, "hashtable-ref"),
        gc.new_procedure(hashtable_keys, "hashtable-keys"),
        gc.new_procedure(string_hash, "string-hash"),
        gc.new_procedure(eqv_hash, "eqv-hash"),
        gc.new_procedure(string_ci_hash, "string-ci-hash"),
        gc.new_procedure(symbol_hash, "symbol-hash"),
        gc.new_procedure(equal_hash, "equal-hash"),
        gc.new_procedure(eq_hashtable_copy, "eq-hashtable-copy"),
        gc.new_procedure(current_error_port, "current-error-port"),
        gc.new_procedure(values, "values"),
        gc.new_procedure(vm_apply, "vm/apply"),
        gc.new_procedure(is_pair, "pair?"),
        gc.new_procedure(
            make_custom_binary_input_port,
            "make-custom-binary-input-port",
        ),
        gc.new_procedure(
            make_custom_binary_output_port,
            "make-custom-binary-output-port",
        ),
        gc.new_procedure(
            make_custom_textual_input_port,
            "make-custom-textual-input-port",
        ),
        gc.new_procedure(
            make_custom_textual_output_port,
            "make-custom-textual-output-port",
        ),
        gc.new_procedure(get_u8, "get-u8"),
        gc.new_procedure(put_u8, "put-u8"),
        gc.new_procedure(put_string, "put-string"),
        gc.new_procedure(flush_output_port, "flush-output-port"),
        gc.new_procedure(output_port_buffer_mode, "output-port-buffer-mode"),
        gc.new_procedure(bytevector_u8_set_destructive, "bytevector-u8-set!"),
        gc.new_procedure(is_port_has_port_position, "port-has-port-position?"),
        gc.new_procedure(
            is_port_has_set_port_position_destructive,
            "port-has-set-port-position!?",
        ),
        gc.new_procedure(port_position, "port-position"),
        gc.new_procedure(set_port_position_destructive, "set-port-position!"),
        gc.new_procedure(get_bytevector_n_destructive, "get-bytevector-n!"),
        gc.new_procedure(get_bytevector_some, "get-bytevector-some"),
        gc.new_procedure(get_bytevector_all, "get-bytevector-all"),
        gc.new_procedure(transcoded_port, "transcoded-port"),
        gc.new_procedure(latin_1_codec, "latin-1-codec"),
        gc.new_procedure(utf_8_codec, "utf-8-codec"),
        gc.new_procedure(utf_16_codec, "utf-16-codec"),
        gc.new_procedure(make_transcoder, "make-transcoder"),
        gc.new_procedure(eof_object, "eof-object"),
        gc.new_procedure(
            sys_open_bytevector_output_port,
            "sys-open-bytevector-output-port",
        ),
        gc.new_procedure(sys_get_bytevector, "sys-get-bytevector"),
        gc.new_procedure(bytevector_length, "bytevector-length"),
        gc.new_procedure(standard_input_port, "standard-input-port"),
        gc.new_procedure(standard_output_port, "standard-output-port"),
        gc.new_procedure(standard_error_port, "standard-error-port"),
        gc.new_procedure(get_bytevector_n, "get-bytevector-n"),
        gc.new_procedure(open_file_output_port, "open-file-output-port"),
        gc.new_procedure(open_file_input_port, "open-file-input-port"),
        gc.new_procedure(close_input_port, "close-input-port"),
        gc.new_procedure(vector, "vector"),
        gc.new_procedure(regexp_replace, "regexp-replace"),
        gc.new_procedure(regexp_replace_all, "regexp-replace-all"),
        gc.new_procedure(source_info, "source-info"),
        gc.new_procedure(eval, "eval"),
        gc.new_procedure(eval_compiled, "eval-compiled"),
        gc.new_procedure(apply, "apply"),
        gc.new_procedure(assq, "assq"),
        gc.new_procedure(assoc, "assoc"),
        gc.new_procedure(assv, "assv"),
        gc.new_procedure(exit, "exit"),
        gc.new_procedure(macroexpand_1, "macroexpand-1"),
        gc.new_procedure(memv, "memv"),
        gc.new_procedure(is_procedure, "procedure?"),
        gc.new_procedure(load, "load"),
        gc.new_procedure(is_symbol, "symbol?"),
        gc.new_procedure(is_charle, "char<=?"),
        gc.new_procedure(is_charlt, "char<?"),
        gc.new_procedure(is_charge, "char>=?"),
        gc.new_procedure(is_chargt, "char>?"),
        gc.new_procedure(read, "read"),
        gc.new_procedure(vector_to_list, "vector->list"),
        gc.new_procedure(set_source_info_destructive, "set-source-info!"),
        gc.new_procedure(call_process, "%call-process"),
        gc.new_procedure(confstr, "%confstr"),
        gc.new_procedure(dup, "%dup"),
        gc.new_procedure(start_process, "%start-process"),
        gc.new_procedure(get_closure_name, "%get-closure-name"),
        gc.new_procedure(append, "append"),
        gc.new_procedure(append2, "append2"),
        gc.new_procedure(append_destructive, "append!"),
        gc.new_procedure(pass3_find_free, "pass3/find-free"),
        gc.new_procedure(pass3_find_sets, "pass3/find-sets"),
        gc.new_procedure(pass4_fixup_labels, "pass4/fixup-labels"),
        gc.new_procedure(make_code_builder, "make-code-builder"),
        gc.new_procedure(
            code_builder_put_extra1_destructive,
            "code-builder-put-extra1!",
        ),
        gc.new_procedure(
            code_builder_put_extra2_destructive,
            "code-builder-put-extra2!",
        ),
        gc.new_procedure(
            code_builder_put_extra3_destructive,
            "code-builder-put-extra3!",
        ),
        gc.new_procedure(
            code_builder_put_extra4_destructive,
            "code-builder-put-extra4!",
        ),
        gc.new_procedure(
            code_builder_put_extra5_destructive,
            "code-builder-put-extra5!",
        ),
        gc.new_procedure(code_builder_append_destructive, "code-builder-append!"),
        gc.new_procedure(code_builder_emit, "code-builder-emit"),
        gc.new_procedure(
            code_builder_put_insn_arg0_destructive,
            "code-builder-put-insn-arg0!",
        ),
        gc.new_procedure(
            code_builder_put_insn_arg1_destructive,
            "code-builder-put-insn-arg1!",
        ),
        gc.new_procedure(
            code_builder_put_insn_arg2_destructive,
            "code-builder-put-insn-arg2!",
        ),
        gc.new_procedure(length, "length"),
        gc.new_procedure(list_to_vector, "list->vector"),
        gc.new_procedure(pass3_compile_refer, "pass3/compile-refer"),
        gc.new_procedure(pass1_find_symbol_in_lvars, "pass1/find-symbol-in-lvars"),
        gc.new_procedure(label, "$label"),
        gc.new_procedure(local_ref, "$local-ref"),
        gc.new_procedure(list_transposeadd, "list-transpose+"),
        gc.new_procedure(symbol_value, "symbol-value"),
        gc.new_procedure(set_symbol_value_destructive, "set-symbol-value!"),
        gc.new_procedure(make_hashtable, "make-hashtable"),
        gc.new_procedure(is_hashtable, "hashtable?"),
        gc.new_procedure(hashtable_size, "hashtable-size"),
        gc.new_procedure(hashtable_delete_destructive, "hashtable-delete!"),
        gc.new_procedure(is_hashtable_contains, "hashtable-contains?"),
        gc.new_procedure(hashtable_copy, "hashtable-copy"),
        gc.new_procedure(is_hashtable_mutable, "hashtable-mutable?"),
        gc.new_procedure(hashtable_clear_destructive, "hashtable-clear!"),
        gc.new_procedure(hashtable_keys, "hashtable-keys"),
        gc.new_procedure(
            hashtable_equivalence_function,
            "hashtable-equivalence-function",
        ),
        gc.new_procedure(hashtable_hash_function, "hashtable-hash-function"),
        gc.new_procedure(throw, "throw"),
        gc.new_procedure(number_lt, "<"),
        gc.new_procedure(number_le, "<="),
        gc.new_procedure(number_gt, ">"),
        gc.new_procedure(number_ge, ">="),
        gc.new_procedure(number_eq, "="),
        gc.new_procedure(number_add, "+"),
        gc.new_procedure(nuber_sub, "-"),
        gc.new_procedure(number_mul, "*"),
        gc.new_procedure(number_div, "/"),
        gc.new_procedure(max, "max"),
        gc.new_procedure(min, "min"),
        gc.new_procedure(get_char, "get-char"),
        gc.new_procedure(lookahead_char, "lookahead-char"),
        gc.new_procedure(get_string_n, "get-string-n"),
        gc.new_procedure(get_string_n_destructive, "get-string-n!"),
        gc.new_procedure(get_string_all, "get-string-all"),
        gc.new_procedure(get_line, "get-line"),
        gc.new_procedure(get_datum, "get-datum"),
        gc.new_procedure(is_bytevector, "bytevector?"),
        gc.new_procedure(current_directory, "current-directory"),
        gc.new_procedure(standard_library_path, "standard-library-path"),
        gc.new_procedure(native_endianness, "native-endianness"),
        gc.new_procedure(make_bytevector, "make-bytevector"),
        gc.new_procedure(make_bytevector, "make-bytevector"),
        gc.new_procedure(bytevector_length, "bytevector-length"),
        gc.new_procedure(is_bytevectorequal, "bytevector=?"),
        gc.new_procedure(bytevector_fill_destructive, "bytevector-fill!"),
        gc.new_procedure(bytevector_copy_destructive, "bytevector-copy!"),
        gc.new_procedure(bytevector_copy, "bytevector-copy"),
        gc.new_procedure(bytevector_u8_ref, "bytevector-u8-ref"),
        gc.new_procedure(bytevector_u8_set_destructive, "bytevector-u8-set!"),
        gc.new_procedure(bytevector_s8_ref, "bytevector-s8-ref"),
        gc.new_procedure(bytevector_s8_set_destructive, "bytevector-s8-set!"),
        gc.new_procedure(bytevector_to_u8_list, "bytevector->u8-list"),
        gc.new_procedure(u8_list_to_bytevector, "u8-list->bytevector"),
        gc.new_procedure(bytevector_u16_ref, "bytevector-u16-ref"),
        gc.new_procedure(bytevector_s16_ref, "bytevector-s16-ref"),
        gc.new_procedure(bytevector_u16_native_ref, "bytevector-u16-native-ref"),
        gc.new_procedure(bytevector_s16_native_ref, "bytevector-s16-native-ref"),
        gc.new_procedure(bytevector_u16_set_destructive, "bytevector-u16-set!"),
        gc.new_procedure(bytevector_s16_set_destructive, "bytevector-s16-set!"),
        gc.new_procedure(
            bytevector_u16_native_set_destructive,
            "bytevector-u16-native-set!",
        ),
        gc.new_procedure(
            bytevector_s16_native_set_destructive,
            "bytevector-s16-native-set!",
        ),
        gc.new_procedure(bytevector_u32_ref, "bytevector-u32-ref"),
        gc.new_procedure(bytevector_s32_ref, "bytevector-s32-ref"),
        gc.new_procedure(bytevector_u32_native_ref, "bytevector-u32-native-ref"),
        gc.new_procedure(bytevector_s32_native_ref, "bytevector-s32-native-ref"),
        gc.new_procedure(bytevector_u32_set_destructive, "bytevector-u32-set!"),
        gc.new_procedure(bytevector_s32_set_destructive, "bytevector-s32-set!"),
        gc.new_procedure(
            bytevector_u32_native_set_destructive,
            "bytevector-u32-native-set!",
        ),
        gc.new_procedure(
            bytevector_s32_native_set_destructive,
            "bytevector-s32-native-set!",
        ),
        gc.new_procedure(bytevector_u64_ref, "bytevector-u64-ref"),
        gc.new_procedure(bytevector_s64_ref, "bytevector-s64-ref"),
        gc.new_procedure(bytevector_u64_native_ref, "bytevector-u64-native-ref"),
        gc.new_procedure(bytevector_s64_native_ref, "bytevector-s64-native-ref"),
        gc.new_procedure(bytevector_u64_set_destructive, "bytevector-u64-set!"),
        gc.new_procedure(bytevector_s64_set_destructive, "bytevector-s64-set!"),
        gc.new_procedure(
            bytevector_u64_native_set_destructive,
            "bytevector-u64-native-set!",
        ),
        gc.new_procedure(
            bytevector_s64_native_set_destructive,
            "bytevector-s64-native-set!",
        ),
        gc.new_procedure(bytevector_to_string, "bytevector->string"),
        gc.new_procedure(string_to_bytevector, "string->bytevector"),
        gc.new_procedure(string_to_utf8, "string->utf8"),
        gc.new_procedure(utf8_to_string, "utf8->string"),
        gc.new_procedure(
            null_terminated_bytevector_to_string,
            "null-terminated-bytevector->string",
        ),
        gc.new_procedure(
            null_terminated_utf8_to_string,
            "null-terminated-utf8->string",
        ),
        gc.new_procedure(string_to_utf16, "string->utf16"),
        gc.new_procedure(string_to_utf32, "string->utf32"),
        gc.new_procedure(utf16_to_string, "utf16->string"),
        gc.new_procedure(utf32_to_string, "utf32->string"),
        gc.new_procedure(close_port, "close-port"),
        gc.new_procedure(make_instruction, "make-instruction"),
        gc.new_procedure(make_compiler_instruction, "make-compiler-instruction"),
        gc.new_procedure(fasl_write, "fasl-write"),
        gc.new_procedure(fasl_read, "fasl-read"),
        gc.new_procedure(get_string_n, "get-string-n"),
        gc.new_procedure(is_rational, "rational?"),
        gc.new_procedure(is_flonum, "flonum?"),
        gc.new_procedure(is_fixnum, "fixnum?"),
        gc.new_procedure(is_bignum, "bignum?"),
        gc.new_procedure(fixnum_width, "fixnum-width"),
        gc.new_procedure(least_fixnum, "least-fixnum"),
        gc.new_procedure(greatest_fixnum, "greatest-fixnum"),
        gc.new_procedure(make_rectangular, "make-rectangular"),
        gc.new_procedure(real_part, "real-part"),
        gc.new_procedure(imag_part, "imag-part"),
        gc.new_procedure(is_exact, "exact?"),
        gc.new_procedure(is_inexact, "inexact?"),
        gc.new_procedure(exact, "exact"),
        gc.new_procedure(inexact, "inexact"),
        gc.new_procedure(is_nan, "nan?"),
        gc.new_procedure(is_infinite, "infinite?"),
        gc.new_procedure(is_finite, "finite?"),
        gc.new_procedure(real_to_flonum, "real->flonum"),
        gc.new_procedure(is_flequal, "fl=?"),
        gc.new_procedure(is_fllt, "fl<?"),
        gc.new_procedure(is_flgt, "fl>?"),
        gc.new_procedure(is_flge, "fl>=?"),
        gc.new_procedure(is_flle, "fl<=?"),
        gc.new_procedure(is_flinteger, "flinteger?"),
        gc.new_procedure(is_flzero, "flzero?"),
        gc.new_procedure(is_flpositive, "flpositive?"),
        gc.new_procedure(is_flnegative, "flnegative?"),
        gc.new_procedure(is_flodd, "flodd?"),
        gc.new_procedure(is_fleven, "fleven?"),
        gc.new_procedure(is_flfinite, "flfinite?"),
        gc.new_procedure(is_flinfinite, "flinfinite?"),
        gc.new_procedure(is_flnan, "flnan?"),
        gc.new_procedure(flmax, "flmax"),
        gc.new_procedure(flmin, "flmin"),
        gc.new_procedure(fladd, "fl+"),
        gc.new_procedure(flmul, "fl*"),
        gc.new_procedure(flsub, "fl-"),
        gc.new_procedure(fldiv_op, "fl/"),
        gc.new_procedure(flabs, "flabs"),
        gc.new_procedure(fldiv, "fldiv"),
        gc.new_procedure(flmod, "flmod"),
        gc.new_procedure(fldiv0, "fldiv0"),
        gc.new_procedure(flmod0, "flmod0"),
        gc.new_procedure(flnumerator, "flnumerator"),
        gc.new_procedure(fldenominator, "fldenominator"),
        gc.new_procedure(flfloor, "flfloor"),
        gc.new_procedure(flceiling, "flceiling"),
        gc.new_procedure(fltruncate, "fltruncate"),
        gc.new_procedure(flround, "flround"),
        gc.new_procedure(flexp, "flexp"),
        gc.new_procedure(fllog, "fllog"),
        gc.new_procedure(flsin, "flsin"),
        gc.new_procedure(flcos, "flcos"),
        gc.new_procedure(fltan, "fltan"),
        gc.new_procedure(flasin, "flasin"),
        gc.new_procedure(flacos, "flacos"),
        gc.new_procedure(flatan, "flatan"),
        gc.new_procedure(flsqrt, "flsqrt"),
        gc.new_procedure(flexpt, "flexpt"),
        gc.new_procedure(fixnum_to_flonum, "fixnum->flonum"),
        gc.new_procedure(bitwise_not, "bitwise-not"),
        gc.new_procedure(bitwise_and, "bitwise-and"),
        gc.new_procedure(bitwise_ior, "bitwise-ior"),
        gc.new_procedure(bitwise_xor, "bitwise-xor"),
        gc.new_procedure(bitwise_bit_count, "bitwise-bit-count"),
        gc.new_procedure(bitwise_length, "bitwise-length"),
        gc.new_procedure(bitwise_first_bit_set, "bitwise-first-bit-set"),
        gc.new_procedure(
            bitwise_arithmetic_shift_left,
            "bitwise-arithmetic-shift-left",
        ),
        gc.new_procedure(
            bitwise_arithmetic_shift_right,
            "bitwise-arithmetic-shift-right",
        ),
        gc.new_procedure(bitwise_arithmetic_shift, "bitwise-arithmetic-shift"),
        gc.new_procedure(is_complex, "complex?"),
        gc.new_procedure(is_real, "real?"),
        gc.new_procedure(is_rational, "rational?"),
        gc.new_procedure(is_integer, "integer?"),
        gc.new_procedure(is_real_valued, "real-valued?"),
        gc.new_procedure(is_rational_valued, "rational-valued?"),
        gc.new_procedure(is_integer_valued, "integer-valued?"),
        gc.new_procedure(is_fxequal, "fx=?"),
        gc.new_procedure(is_fxgt, "fx>?"),
        gc.new_procedure(is_fxlt, "fx<?"),
        gc.new_procedure(is_fxge, "fx>=?"),
        gc.new_procedure(is_fxle, "fx<=?"),
        gc.new_procedure(is_fxzero, "fxzero?"),
        gc.new_procedure(is_fxpositive, "fxpositive?"),
        gc.new_procedure(is_fxnegative, "fxnegative?"),
        gc.new_procedure(is_fxodd, "fxodd?"),
        gc.new_procedure(is_fxeven, "fxeven?"),
        gc.new_procedure(fxmax, "fxmax"),
        gc.new_procedure(fxmin, "fxmin"),
        gc.new_procedure(fxadd, "fx+"),
        gc.new_procedure(fxmul, "fx*"),
        gc.new_procedure(fxsub, "fx-"),
        gc.new_procedure(fxdiv, "fxdiv"),
        gc.new_procedure(fxmod, "fxmod"),
        gc.new_procedure(fxdiv0, "fxdiv0"),
        gc.new_procedure(fxmod0, "fxmod0"),
        gc.new_procedure(fxnot, "fxnot"),
        gc.new_procedure(fxand, "fxand"),
        gc.new_procedure(fxior, "fxior"),
        gc.new_procedure(fxxor, "fxxor"),
        gc.new_procedure(fxif, "fxif"),
        gc.new_procedure(fxbit_count, "fxbit-count"),
        gc.new_procedure(fxlength, "fxlength"),
        gc.new_procedure(fxfirst_bit_set, "fxfirst-bit-set"),
        gc.new_procedure(is_fxbit_set, "fxbit-set?"),
        gc.new_procedure(fxcopy_bit, "fxcopy-bit"),
        gc.new_procedure(fxbit_field, "fxbit-field"),
        gc.new_procedure(fxcopy_bit_field, "fxcopy-bit-field"),
        gc.new_procedure(fxarithmetic_shift, "fxarithmetic-shift"),
        gc.new_procedure(fxarithmetic_shift_left, "fxarithmetic-shift-left"),
        gc.new_procedure(fxarithmetic_shift_right, "fxarithmetic-shift-right"),
        gc.new_procedure(fxrotate_bit_field, "fxrotate-bit-field"),
        gc.new_procedure(fxreverse_bit_field, "fxreverse-bit-field"),
        gc.new_procedure(
            bytevector_ieee_single_native_ref,
            "bytevector-ieee-single-native-ref",
        ),
        gc.new_procedure(bytevector_ieee_single_ref, "bytevector-ieee-single-ref"),
        gc.new_procedure(
            bytevector_ieee_double_native_ref,
            "bytevector-ieee-double-native-ref",
        ),
        gc.new_procedure(bytevector_ieee_double_ref, "bytevector-ieee-double-ref"),
        gc.new_procedure(
            bytevector_ieee_single_native_set_destructive,
            "bytevector-ieee-single-native-set!",
        ),
        gc.new_procedure(
            bytevector_ieee_single_set_destructive,
            "bytevector-ieee-single-set!",
        ),
        gc.new_procedure(
            bytevector_ieee_double_native_set_destructive,
            "bytevector-ieee-double-native-set!",
        ),
        gc.new_procedure(
            bytevector_ieee_double_set_destructive,
            "bytevector-ieee-double-set!",
        ),
        gc.new_procedure(is_even, "even?"),
        gc.new_procedure(is_odd, "odd?"),
        gc.new_procedure(abs, "abs"),
        gc.new_procedure(div, "div"),
        gc.new_procedure(div0, "div0"),
        gc.new_procedure(numerator, "numerator"),
        gc.new_procedure(denominator, "denominator"),
        gc.new_procedure(floor, "floor"),
        gc.new_procedure(ceiling, "ceiling"),
        gc.new_procedure(truncate, "truncate"),
        gc.new_procedure(round, "round"),
        gc.new_procedure(exp, "exp"),
        gc.new_procedure(log, "log"),
        gc.new_procedure(sin, "sin"),
        gc.new_procedure(cos, "cos"),
        gc.new_procedure(tan, "tan"),
        gc.new_procedure(asin, "asin"),
        gc.new_procedure(acos, "acos"),
        gc.new_procedure(sqrt, "sqrt"),
        gc.new_procedure(magnitude, "magnitude"),
        gc.new_procedure(angle, "angle"),
        gc.new_procedure(atan, "atan"),
        gc.new_procedure(expt, "expt"),
        gc.new_procedure(make_polar, "make-polar"),
        gc.new_procedure(string_copy, "string-copy"),
        gc.new_procedure(vector_fill_destructive, "vector-fill!"),
        gc.new_procedure(ungensym, "ungensym"),
        gc.new_procedure(disasm, "disasm"),
        gc.new_procedure(print_stack, "print-stack"),
        gc.new_procedure(is_fast_equal, "fast-equal?"),
        gc.new_procedure(native_eol_style, "native-eol-style"),
        gc.new_procedure(is_buffer_mode, "buffer-mode?"),
        gc.new_procedure(microseconds, "microseconds"),
        gc.new_procedure(local_tz_offset, "local-tz-offset"),
        gc.new_procedure(fork, "%fork"),
        gc.new_procedure(exec, "%exec"),
        gc.new_procedure(waitpid, "%waitpid"),
        gc.new_procedure(pipe, "%pipe"),
        gc.new_procedure(getpid, "%getpid"),
        gc.new_procedure(current_directory, "current-directory"),
        gc.new_procedure(set_current_directory_destructive, "set-current-directory!"),
        gc.new_procedure(is_binary_port, "binary-port?"),
        gc.new_procedure(is_input_port, "input-port?"),
        gc.new_procedure(is_port_eof, "port-eof?"),
        gc.new_procedure(lookahead_u8, "lookahead-u8"),
        gc.new_procedure(open_bytevector_input_port, "open-bytevector-input-port"),
        gc.new_procedure(ffi_open, "%ffi-open"),
        gc.new_procedure(ffi_lookup, "%ffi-lookup"),
        gc.new_procedure(ffi_call, "%ffi-call"),
        gc.new_procedure(is_ffi_supported, "%ffi-supported?"),
        gc.new_procedure(ffi_malloc, "%ffi-malloc"),
        gc.new_procedure(ffi_free, "%ffi-free"),
        gc.new_procedure(
            ffi_make_c_callback_trampoline,
            "%ffi-make-c-callback-trampoline",
        ),
        gc.new_procedure(
            ffi_free_c_callback_trampoline,
            "%ffi-free-c-callback-trampoline",
        ),
        gc.new_procedure(ffi_close, "%ffi-close"),
        gc.new_procedure(ffi_error, "%ffi-error"),
        gc.new_procedure(host_os, "host-os"),
        gc.new_procedure(is_output_port, "output-port?"),
        gc.new_procedure(is_textual_port, "textual-port?"),
        gc.new_procedure(is_port, "port?"),
        gc.new_procedure(port_transcoder, "port-transcoder"),
        gc.new_procedure(native_transcoder, "native-transcoder"),
        gc.new_procedure(put_bytevector, "put-bytevector"),
        gc.new_procedure(put_char, "put-char"),
        gc.new_procedure(write_char, "write-char"),
        gc.new_procedure(transcoder_codec, "transcoder-codec"),
        gc.new_procedure(transcoder_eol_style, "transcoder-eol-style"),
        gc.new_procedure(
            transcoder_error_handling_mode,
            "transcoder-error-handling-mode",
        ),
        gc.new_procedure(quotient, "quotient"),
        gc.new_procedure(remainder, "remainder"),
        gc.new_procedure(modulo, "modulo"),
        gc.new_procedure(open_file_input_output_port, "open-file-input/output-port"),
        gc.new_procedure(
            make_custom_binary_input_output_port,
            "make-custom-binary-input/output-port",
        ),
        gc.new_procedure(
            make_custom_textual_input_output_port,
            "make-custom-textual-input/output-port",
        ),
        gc.new_procedure(put_datum, "put-datum"),
        gc.new_procedure(list_ref, "list-ref"),
        gc.new_procedure(list_tail, "list-tail"),
        gc.new_procedure(time_usage, "time-usage"),
        gc.new_procedure(mosh_executable_path, "mosh-executable-path"),
        gc.new_procedure(is_socket, "socket?"),
        gc.new_procedure(socket_accept, "socket-accept"),
        gc.new_procedure(make_client_socket, "make-client-socket"),
        gc.new_procedure(make_server_socket, "make-server-socket"),
        gc.new_procedure(os_constant, "os-constant"),
        gc.new_procedure(socket_recv, "socket-recv"),
        gc.new_procedure(socket_recv_destructive, "socket-recv!"),
        gc.new_procedure(socket_send, "socket-send"),
        gc.new_procedure(socket_close, "socket-close"),
        gc.new_procedure(socket_shutdown, "socket-shutdown"),
        gc.new_procedure(socket_port, "socket-port"),
        gc.new_procedure(make_vm, "make-vm"),
        gc.new_procedure(vm_start_destructive, "vm-start!"),
        gc.new_procedure(is_vm, "vm?"),
        gc.new_procedure(vm_set_value_destructive, "vm-set-value!"),
        gc.new_procedure(vm_join_destructive, "vm-join!"),
        gc.new_procedure(is_main_vm, "main-vm?"),
        gc.new_procedure(vm_self, "vm-self"),
        gc.new_procedure(register, "register"),
        gc.new_procedure(whereis, "whereis"),
        gc.new_procedure(make_condition_variable, "make-condition-variable"),
        gc.new_procedure(
            condition_variable_wait_destructive,
            "condition-variable-wait!",
        ),
        gc.new_procedure(
            condition_variable_notify_destructive,
            "condition-variable-notify!",
        ),
        gc.new_procedure(
            condition_variable_notify_all_destructive,
            "condition-variable-notify-all!",
        ),
        gc.new_procedure(is_mutex, "mutex?"),
        gc.new_procedure(make_mutex, "make-mutex"),
        gc.new_procedure(mutex_lock_destructive, "mutex-lock!"),
        gc.new_procedure(mutex_try_lock_destructive, "mutex-try-lock!"),
        gc.new_procedure(mutex_unlock_destructive, "mutex-unlock!"),
        gc.new_procedure(make_vector, "make-vector"),
        gc.new_procedure(vector_length, "vector-length"),
        gc.new_procedure(vector_ref, "vector-ref"),
        gc.new_procedure(vector_set_destructive, "vector-set!"),
        gc.new_procedure(create_directory, "create-directory"),
        gc.new_procedure(delete_directory, "delete-directory"),
        gc.new_procedure(rename_file, "rename-file"),
        gc.new_procedure(create_symbolic_link, "create-symbolic-link"),
        gc.new_procedure(is_file_directory, "file-directory?"),
        gc.new_procedure(is_file_symbolic_link, "file-symbolic-link?"),
        gc.new_procedure(is_file_regular, "file-regular?"),
        gc.new_procedure(is_file_readable, "file-readable?"),
        gc.new_procedure(is_file_executable, "file-executable?"),
        gc.new_procedure(is_file_writable, "file-writable?"),
        gc.new_procedure(file_size_in_bytes, "file-size-in-bytes"),
        gc.new_procedure(file_stat_mtime, "file-stat-mtime"),
        gc.new_procedure(file_stat_atime, "file-stat-atime"),
        gc.new_procedure(file_stat_ctime, "file-stat-ctime"),
        gc.new_procedure(is_pointer, "pointer?"),
        gc.new_procedure(pointer_to_integer, "pointer->integer"),
        gc.new_procedure(integer_to_pointer, "integer->pointer"),
        gc.new_procedure(pointer_ref_c_uint8, "pointer-ref-c-uint8"),
        gc.new_procedure(pointer_ref_c_uint16, "pointer-ref-c-uint16"),
        gc.new_procedure(pointer_ref_c_uint32, "pointer-ref-c-uint32"),
        gc.new_procedure(pointer_ref_c_uint64, "pointer-ref-c-uint64"),
        gc.new_procedure(pointer_ref_c_int8, "pointer-ref-c-int8"),
        gc.new_procedure(pointer_ref_c_int16, "pointer-ref-c-int16"),
        gc.new_procedure(pointer_ref_c_int32, "pointer-ref-c-int32"),
        gc.new_procedure(pointer_ref_c_int64, "pointer-ref-c-int64"),
        gc.new_procedure(pointer_ref_c_signed_char, "pointer-ref-c-signed-char"),
        gc.new_procedure(pointer_ref_c_unsigned_char, "pointer-ref-c-unsigned-char"),
        gc.new_procedure(pointer_ref_c_signed_short, "pointer-ref-c-signed-short"),
        gc.new_procedure(pointer_ref_c_unsigned_short, "pointer-ref-c-unsigned-short"),
        gc.new_procedure(pointer_ref_c_signed_int, "pointer-ref-c-signed-int"),
        gc.new_procedure(pointer_ref_c_unsigned_int, "pointer-ref-c-unsigned-int"),
        gc.new_procedure(pointer_ref_c_signed_long, "pointer-ref-c-signed-long"),
        gc.new_procedure(pointer_ref_c_unsigned_long, "pointer-ref-c-unsigned-long"),
        gc.new_procedure(
            pointer_ref_c_signed_long_long,
            "pointer-ref-c-signed-long-long",
        ),
        gc.new_procedure(
            pointer_ref_c_unsigned_long_long,
            "pointer-ref-c-unsigned-long-long",
        ),
        gc.new_procedure(pointer_ref_c_float, "pointer-ref-c-float"),
        gc.new_procedure(pointer_ref_c_double, "pointer-ref-c-double"),
        gc.new_procedure(pointer_ref_c_pointer, "pointer-ref-c-pointer"),
        gc.new_procedure(pointer_set_c_int8_destructive, "pointer-set-c-int8!"),
        gc.new_procedure(pointer_set_c_int16_destructive, "pointer-set-c-int16!"),
        gc.new_procedure(pointer_set_c_int32_destructive, "pointer-set-c-int32!"),
        gc.new_procedure(pointer_set_c_int64_destructive, "pointer-set-c-int64!"),
        gc.new_procedure(pointer_set_c_uint8_destructive, "pointer-set-c-uint8!"),
        gc.new_procedure(pointer_set_c_uint16_destructive, "pointer-set-c-uint16!"),
        gc.new_procedure(pointer_set_c_uint32_destructive, "pointer-set-c-uint32!"),
        gc.new_procedure(pointer_set_c_uint64_destructive, "pointer-set-c-uint64!"),
        gc.new_procedure(pointer_set_c_char_destructive, "pointer-set-c-char!"),
        gc.new_procedure(pointer_set_c_short_destructive, "pointer-set-c-short!"),
        gc.new_procedure(pointer_set_c_int_destructive, "pointer-set-c-int!"),
        gc.new_procedure(pointer_set_c_long_destructive, "pointer-set-c-long!"),
        gc.new_procedure(
            pointer_set_c_long_long_destructive,
            "pointer-set-c-long-long!",
        ),
        gc.new_procedure(pointer_set_c_float_destructive, "pointer-set-c-float!"),
        gc.new_procedure(pointer_set_c_double_destructive, "pointer-set-c-double!"),
        gc.new_procedure(pointer_set_c_pointer_destructive, "pointer-set-c-pointer!"),
        gc.new_procedure(pointer_copy_destructive, "pointer-copy!"),
        gc.new_procedure(bytevector_pointer, "bytevector-pointer"),
        gc.new_procedure(shared_errno, "shared-errno"),
        gc.new_procedure(is_simple_struct, "simple-struct?"),
        gc.new_procedure(make_simple_struct, "make-simple-struct"),
        gc.new_procedure(simple_struct_ref, "simple-struct-ref"),
        gc.new_procedure(simple_struct_set_destructive, "simple-struct-set!"),
        gc.new_procedure(simple_struct_name, "simple-struct-name"),
        gc.new_procedure(lookup_nongenerative_rtd, "lookup-nongenerative-rtd"),
        gc.new_procedure(nongenerative_rtd_set_destructive, "nongenerative-rtd-set!"),
        gc.new_procedure(is_same_marksmul, "same-marks*?"),
        gc.new_procedure(is_same_marks, "same-marks?"),
        gc.new_procedure(id_to_real_label, "id->real-label"),
        gc.new_procedure(join_wraps, "join-wraps"),
        gc.new_procedure(gensym_prefix_set_destructive, "gensym-prefix-set!"),
        gc.new_procedure(current_dynamic_winders, "current-dynamic-winders"),
        gc.new_procedure(sexp_map, "sexp-map"),
        gc.new_procedure(sexp_map_debug, "sexp-map/debug"),
        gc.new_procedure(write_ss, "write/ss"),
        gc.new_procedure(monapi_message_send, "%monapi-message-send"),
        gc.new_procedure(monapi_name_whereis, "%monapi-name-whereis"),
        gc.new_procedure(monapi_message_receive, "%monapi-message-receive"),
        gc.new_procedure(monapi_name_add_destructive, "%monapi-name-add!"),
        gc.new_procedure(monapi_message_send_receive, "%monapi-message-send-receive"),
        gc.new_procedure(monapi_message_reply, "%monapi-message-reply"),
        gc.new_procedure(monapi_make_stream, "%monapi-make-stream"),
        gc.new_procedure(monapi_stream_handle, "%monapi-stream-handle"),
        gc.new_procedure(monapi_stream_write, "%monapi-stream-write"),
        gc.new_procedure(monapi_stream_read, "%monapi-stream-read"),
        gc.new_procedure(process_list, "process-list"),
        gc.new_procedure(process_terminate_destructive, "process-terminate!"),
        gc.new_procedure(socket_sslize_destructive, "socket-sslize!"),
        gc.new_procedure(is_ssl_socket, "ssl-socket?"),
        gc.new_procedure(is_ssl_supported, "ssl-supported?"),
        gc.new_procedure(file_to_string, "file->string"),
        gc.new_procedure(annotated_cons, "annotated-cons"),
        gc.new_procedure(is_annotated_pair, "annotated-pair?"),
        gc.new_procedure(get_annotation, "get-annotation"),
        gc.new_procedure(set_annotation_destructive, "set-annotation!"),
        gc.new_procedure(pointer_to_object, "pointer->object"),
        gc.new_procedure(object_to_pointer, "object->pointer"),
        gc.new_procedure(
            set_current_error_port_destructive,
            "set-current-error-port!",
        ),
        gc.new_procedure(is_port_open, "port-open?"),
        gc.new_procedure(make_f64array, "make-f64array"),
        gc.new_procedure(is_f64array, "f64array?"),
        gc.new_procedure(f64array_ref, "f64array-ref"),
        gc.new_procedure(f64array_set_destructive, "f64array-set!"),
        gc.new_procedure(f64array_shape, "f64array-shape"),
        gc.new_procedure(f64array_dot_product, "f64array-dot-product"),
    ]
}

#[macro_export]
macro_rules! check_argc {
    ($name:ident, $args:ident, $argc:expr) => {{
        if $args.len() != $argc {
            return type_required_error(
                $name,
                &format!("{} arguments required but got {}", $argc, $args.len()),
                &[],
            );
        }
    }};
}

#[macro_export]
macro_rules! check_argc_at_least {
    ($name:ident, $args:ident, $argc:expr) => {{
        if $args.len() < $argc {
            return type_required_error(
                $name,
                &format!(
                    "at least {} arguments required but got {}",
                    $argc,
                    $args.len()
                ),
                &[],
            );
        }
    }};
}

#[macro_export]
macro_rules! check_argc_max {
    ($name:ident, $args:ident, $max:expr) => {{
        if $args.len() > $max {
            return type_required_error(
                $name,
                &format!(
                    "at least {} arguments required but got {}",
                    $max,
                    $args.len()
                ),
                &[],
            );
        }
    }};
}

#[macro_export]
macro_rules! check_argc_between {
    ($name:ident, $args:ident, $min:expr, $max:expr) => {{
        if $args.len() > $max || $args.len() < $min {
            if $args.len() > $max {
                return type_required_error(
                    $name,
                    &format!(
                        "{}-{} arguments required but got {}",
                        $min,
                        $max,
                        $args.len()
                    ),
                    &[],
                );
            }
        }
    }};
}

fn is_number(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "number?";
    check_argc!(name, args, 1);
    Ok(args[0].is_number().to_obj())
}
fn cons(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cons";
    check_argc!(name, args, 2);
    Ok(vm.gc.cons(args[0], args[1]))
}
fn consmul(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cons*";
    check_argc_at_least!(name, args, 1);
    let argc = args.len();
    if argc == 1 {
        return Ok(args[0]);
    }
    let obj = vm.gc.cons(args[0], Object::Nil);
    let mut tail = obj;
    for i in 1..argc - 1 {
        let e = vm.gc.cons(args[i], Object::Nil);
        match tail {
            Object::Pair(mut pair) => {
                pair.cdr = e;
                tail = e;
            }
            _ => {
                return type_required_error(name, "pair", &[tail]);
            }
        }
    }
    match tail {
        Object::Pair(mut pair) => pair.cdr = args[argc - 1],
        _ => {
            return type_required_error(name, "pair", &[tail]);
        }
    }
    Ok(obj)
}
fn car(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cons";
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => Ok(pair.car),
        _ => type_required_error(name, "pair", &[args[0]]),
    }
}

fn cdr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdr";
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => Ok(pair.cdr),
        _ => type_required_error(name, "pair", &[args[0]]),
    }
}
fn is_null(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "null?";
    check_argc!(name, args, 1);
    Ok(args[0].is_nil().to_obj())
}
fn set_car_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-car!";
    check_argc!(name, args, 2);
    if let Object::Pair(mut p) = args[0] {
        p.car = args[1];
        Ok(Object::Unspecified)
    } else {
        type_required_error(name, "pair", &[args[0]])
    }
}
fn set_cdr_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-cdr!";
    check_argc!(name, args, 2);
    if let Object::Pair(mut p) = args[0] {
        p.cdr = args[1];
        Ok(Object::Unspecified)
    } else {
        type_required_error(name, "pair", &[args[0]])
    }
}
fn sys_display(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "display";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    let port = if argc == 1 {
        vm.current_output_port()
    } else {
        args[1]
    };
    let shared_aware = false;
    let port = obj_as_text_output_port_mut!(name, port);
    port.display(args[0], shared_aware).ok();
    Ok(Object::Unspecified)
}
fn rxmatch(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_regexp(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "regexp?";
    check_argc!(name, args, 1);
    Ok(args[0].is_regexp().to_obj())
}
fn regexp_to_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "regexp->string";
    todo!("{}({}) not implemented", name, args.len());
}
fn rxmatch_start(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch-start";
    todo!("{}({}) not implemented", name, args.len());
}
fn rxmatch_end(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch-end";
    todo!("{}({}) not implemented", name, args.len());
}
fn rxmatch_after(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch-after";
    todo!("{}({}) not implemented", name, args.len());
}
fn rxmatch_before(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch-before";
    todo!("{}({}) not implemented", name, args.len());
}
fn rxmatch_substring(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rxmatch-substring";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-string";
    check_argc_between!(name, args, 1, 2);
    match args {
        [Object::Fixnum(n)] => Ok(vm.gc.new_string(&" ".repeat(*n as usize))),
        [Object::Fixnum(n), Object::Char(c)] => {
            Ok(vm.gc.new_string(&c.to_string().repeat(*n as usize)))
        }
        _ => generic_error!(name, args, "wrong arguments {:?}", args),
    }
}
fn string_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-set!";
    check_argc!(name, args, 3);
    match args {
        [Object::String(mut s), Object::Fixnum(idx), Object::Char(c)] => {
            let idx = *idx as usize;
            s.string.replace_range(idx..idx + 1, &c.to_string());
            Ok(Object::Unspecified)
        }
        _ => type_required_error(name, "string, number and char", args),
    }
}
fn string_length(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-length";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => Ok(Object::Fixnum(s.string.chars().count().try_into().unwrap())),
        v => type_required_error(name, "string", &[v]),
    }
}
fn string_to_symbol(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->symbol";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => Ok(vm.gc.symbol_intern(&s.string)),
        v => type_required_error(name, "string", &[v]),
    }
}
fn string_to_number(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->number";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    if argc == 1 {
        let s = as_sstring!(name, args, 0);
        let mut chars: Vec<char> = s.chars().collect();
        chars.push('\0');
        let mut is_inexact_context = false;
        match NumberParser::new().parse(
            &mut vm.gc,
            &mut is_inexact_context,
            NumberLexer::new(&chars),
        ) {
            Ok(n) => Ok(n),
            // Note that string->number returns #f for any parse error.
            Err(_) => Ok(Object::False),
        }
    } else {
        let radix = args[1];
        let mut prefix: String = "".to_string();
        match radix {
            Object::Fixnum(2) => {
                prefix.push_str("#b");
            }
            Object::Fixnum(8) => {
                prefix.push_str("#o");
            }
            Object::Fixnum(10) => (),
            Object::Fixnum(16) => {
                prefix.push_str("#x");
            }
            _ => {
                return type_required_error(name, "2, 8, 10 or 16", &[args[1]]);
            }
        }
        match args[0] {
            Object::String(s) => {
                prefix.push_str(&s);
                let mut chars: Vec<char> = prefix.chars().collect();
                chars.push('\0');
                let mut is_inexact_context = false;
                match NumberParser::new().parse(
                    &mut vm.gc,
                    &mut is_inexact_context,
                    NumberLexer::new(&chars),
                ) {
                    Ok(n) => Ok(n),
                    Err(err) => generic_error!(name, args, "{:?}", err),
                }
            }
            _ => type_required_error(name, "string", &[args[0]]),
        }
    }
}
/*
checkArgumentLengthBetween(1, 2);
argumentAsString(0, text);
const ucs4string& numberString = text->data();
if (argc == 1) {
    return Ok(stringToNumber(numberString));
} else {
    argumentAsFixnum(1, radix);
    switch (radix) {
        case 2:
        {
            ucs4string text(UC("#b"));
            text += numberString;
            return Ok(stringToNumber(text));
        }
        case 8:
        {
            ucs4string text(UC("#o"));
            text += numberString;
            return Ok(stringToNumber(text));
        }
        case 10:
            return Ok(stringToNumber(numberString));
        case 16:
        {
            ucs4string text(UC("#x"));
            text += numberString;
            return Ok(stringToNumber(text));
        }
        default:
            callAssertionViolationAfter(theVM, procedureName, UC("radix should be 2, 8, 10 ro 16"), L1(argv[1]));
            return Ok(Object::Undef);
    }
}*/

fn string_append(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-append";
    let mut ret = "".to_string();
    for arg in args {
        match arg {
            Object::String(s) => {
                ret = ret + &s.string;
            }
            obj => {
                return type_required_error(name, "string", &[*obj]);
            }
        }
    }
    Ok(vm.gc.new_string(&ret))
}
fn string_split(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-split";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::String(s), Object::Char(c)) => {
            let mut l = Object::Nil;
            for w in s.string.rsplit(c) {
                let obj = vm.gc.new_string(w);
                l = vm.gc.cons(obj, l);
            }
            Ok(l)
        }
        _ => type_required_error(name, "string and char", &[args[0], args[1]]),
    }
}
fn string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string";
    let mut chars: Vec<char> = vec![];
    for obj in args {
        match obj {
            Object::Char(c) => {
                chars.push(*c);
            }
            v => {
                return type_required_error(name, "char", &[*v]);
            }
        }
    }
    let s: String = chars.into_iter().collect();
    Ok(vm.gc.new_string(&s))
}
fn number_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "number->string";
    check_argc_between!(name, args, 1, 3);
    let argc = args.len();
    let n = args[0];
    if argc == 1 {
        Ok(vm.gc.new_string(&numbers::to_string(n, 10)))
    } else {
        let radix = args[1];
        if !radix.is_fixnum() {
            return type_required_error(name, "radix number", &[args[1]]);
        }
        let radix = radix.to_isize();
        if radix == 2 || radix == 8 || radix == 10 || radix == 16 {
            Ok(vm.gc.new_string(&numbers::to_string(n, radix as usize)))
        } else {
            generic_error!(name, &[args[1]], "unsupported radix {}", args[1])
        }
    }
}
fn reverse(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "reverse";
    check_argc!(name, args, 1);
    let mut ret = Object::Nil;
    let mut p = args[0];
    while let Object::Pair(pair) = p {
        ret = vm.gc.cons(pair.car, ret);
        p = pair.cdr;
    }

    Ok(ret)
}
fn is_eof_object(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eof-object?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Eof => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn read_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "read-char";
    todo!("{}({}) not implemented", name, args.len());
}
fn peek_char(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "peek-char";
    check_argc_max!(name, args, 1);
    let port = if args.is_empty() {
        vm.current_input_port()
    } else {
        args[0]
    };
    let port = obj_as_text_input_port_mut!(name, port);
    match port.lookahead_char(vm) {
        Ok(Some(c)) => Ok(Object::Char(c)),
        Ok(None) => Ok(Object::Eof),
        Err(e) => Err(e),
    }
}
fn is_charequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char=?";
    check_argc_at_least!(name, args, 2);
    if let Object::Char(c) = args[0] {
        for i in 1..args.len() {
            if let Object::Char(c2) = args[i] {
                if c != c2 {
                    return Ok(Object::False);
                }
            } else {
                return type_required_error(name, "char", &[args[i]]);
            }
        }
        Ok(Object::True)
    } else {
        type_required_error(name, "char", &[args[0]])
    }
}
fn is_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn get_environment_variable(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-environment-variable";
    check_argc!(name, args, 1);
    if let Object::String(key) = args[0] {
        match env::var(&key.string) {
            Ok(value) => Ok(vm.gc.new_string(&value)),
            Err(_) => Ok(Object::False),
        }
    } else {
        type_required_error(name, "string key", &[args[0]])
    }
}
fn get_environment_variables(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-environment-variables";
    check_argc!(name, args, 0);
    let mut ret = Object::Nil;
    for (key, value) in env::vars() {
        let key = vm.gc.new_string(&key);
        let value = vm.gc.new_string(&value);
        let kons = vm.gc.cons(key, value);
        ret = vm.gc.cons(kons, ret);
    }
    Ok(ret)
}
fn is_equal(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "equal?";
    check_argc!(name, args, 2);
    let e = Equal::new();
    Ok(e.is_equal(&mut vm.gc, &args[0], &args[1]).to_obj())
}
fn open_string_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "open-string-input-port";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => {
            let port = StringInputPort::new(&s.string);
            Ok(Object::StringInputPort(vm.gc.alloc(port)))
        }
        _ => type_required_error(name, "string", &[args[0]]),
    }
}
fn open_output_string(vm: &mut Vm, _args: &mut [Object]) -> error::Result<Object> {
    Ok(Object::StringOutputPort(
        vm.gc.alloc(StringOutputPort::new()),
    ))
}
fn sys_port_seek(vm: &mut Vm, _args: &mut [Object]) -> error::Result<Object> {
    Ok(vm.gc.new_string("sys-port-seek dummy return value"))
}
fn close_output_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "close-output-port";
    check_argc!(name, args, 1);
    let port = as_output_port_mut!(name, args, 0);
    port.close();
    Ok(Object::Unspecified)
}
fn digit_to_integer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "digit->integer";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::Char(c), Object::Fixnum(radix)) => match c.to_digit(radix as u32) {
            Some(v) => Ok(Object::Fixnum(v as isize)),
            None => {
                generic_error!(name, args, "could not convert ({}, {})", args[0], args[1])
            }
        },
        _ => type_required_error(name, "char and number", args),
    }
}
fn get_remaining_input_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-remaining-input-string";
    todo!("{}({}) not implemented", name, args.len());
}
fn directory_list(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "directory-list";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_exists(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-exists?";
    check_argc!(name, args, 1);
    if let Object::String(s) = args[0] {
        if Path::new(&s.string).exists() {
            Ok(Object::True)
        } else if s.string.starts_with("/embed/stdlib") {
            Ok(StdLib::exists(&s.string).to_obj())
        } else {
            Ok(Object::False)
        }
    } else {
        type_required_error(name, "string", &[args[0]])
    }
}
fn delete_file(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "delete-file";
    check_argc!(name, args, 1);
    let path = as_sstring!(name, args, 0);
    match fs::remove_file(&path.string) {
        Ok(_) => Ok(Object::Unspecified),
        Err(e) => {
            Error::assertion_violation(name, &format!("delete file failed {}", e), &[args[0]])
        }
    }
}
fn get_output_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-output-string";
    check_argc!(name, args, 1);
    if let Object::StringOutputPort(mut s) = args[0] {
        Ok(vm.gc.new_string(&s.string()))
    } else {
        type_required_error(name, "string-output-port", args)
    }
}
fn string_to_regexp(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->regexp";
    todo!("{}({}) not implemented", name, args.len());
}
fn char_to_integer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char->integer";
    check_argc!(name, args, 1);
    if let Object::Char(c) = args[0] {
        Ok(Object::Fixnum(c as isize))
    } else {
        type_required_error(name, "char", &[args[0]])
    }
}
fn integer_to_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "integer->char";
    check_argc!(name, args, 1);
    if let Object::Fixnum(n) = args[0] {
        match char::from_u32(n as u32) {
            Some(c) => Ok(Object::Char(c)),
            None => Error::assertion_violation(name, "integer out of range", &[args[0]]),
        }
    } else {
        type_required_error(name, "integer number", &[args[0]])
    }
}
fn format(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "format";
    let argc = args.len();
    if argc >= 2 {
        match (args[0], args[1]) {
            (Object::False, Object::String(s)) => {
                let mut port = StringOutputPort::new();
                port.format(&s.string, &mut args[2..])?;
                Ok(vm.gc.new_string(&port.string()))
            }
            (obj, Object::String(s)) if obj.is_textual_output_port() => {
                let port = obj_as_text_output_port_mut!(name, obj);
                port.format(&s.string, &mut args[2..])?;
                Ok(Object::Unspecified)
            }

            (Object::String(s), _) => {
                let mut port = StringOutputPort::new();
                port.format(&s.string, &mut args[1..])?;
                Ok(vm.gc.new_string(&port.string()))
            }
            (x, y) => bug!("x={} y={}", x, y),
        }
    } else {
        Ok(Object::Unspecified)
    }
}
fn current_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "current-input-port";
    check_argc!(name, args, 0);
    Ok(vm.current_input_port())
}
fn current_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "current-output-port";
    check_argc!(name, args, 0);
    Ok(vm.current_output_port())
}
fn set_current_input_port_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-current-input-port!";
    check_argc!(name, args, 1);
    vm.set_current_input_port(args[0]);
    Ok(Object::Unspecified)
}
fn set_current_output_port_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-current-output-port!";
    check_argc!(name, args, 1);
    if args[0].is_textual_port() && args[0].is_output_port() {
        vm.set_current_output_port(args[0]);
    } else {
        return Error::assertion_violation(name, "text output port required", &[args[0]]);
    }
    Ok(Object::Unspecified)
}
fn is_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Char(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn write(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "write";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    let port = if argc == 1 {
        vm.current_output_port()
    } else {
        args[1]
    };
    let shared_aware = false;
    let port = obj_as_text_output_port_mut!(name, port);
    port.write(args[0], shared_aware).ok();
    Ok(Object::Unspecified)
}
fn gensym(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "gensym";
    check_argc_max!(name, args, 1);
    let argc = args.len();

    if argc == 1 {
        let name = unsafe { format!("{}{}@", GENSYM_PREFIX, GENSYM_INDEX) };
        unsafe { GENSYM_INDEX += 1 };
        match args[0] {
            Object::Symbol(s) => {
                let name = name + &s.string;
                Ok(vm.gc.symbol_intern(&name))
            }
            _ => Ok(vm.gc.symbol_intern(&name)),
        }
    } else {
        let name = unsafe { format!("{}{}", GENSYM_PREFIX, GENSYM_INDEX) };
        unsafe { GENSYM_INDEX += 1 };
        Ok(vm.gc.symbol_intern(&name))
    }
}
fn is_stringequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        match (args[i], args[i + 1]) {
            (Object::String(s1), Object::String(s2)) => {
                if s1.string.eq(&s2.string) {
                    continue;
                } else {
                    return Ok(Object::False);
                }
            }
            _ => {
                return type_required_error(name, "string", &[args[i], args[i + 1]]);
            }
        }
    }
    Ok(Object::True)
}

fn caaaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caaaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caaadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caaadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => Ok(pair3.car),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caadar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caadar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caaddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caaddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => Ok(pair3.car),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caar";
    todo!("{}({}) not implemented", name, args.len());
}
fn cadaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cadaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cadadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cadadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cadar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cadar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => Ok(pair3.car),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caddar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caddar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cadddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cadddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.car),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn caddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "caddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => Ok(pair3.car),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => Ok(pair2.car),
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdaaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdaaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdaadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdaadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => Ok(pair3.cdr),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdadar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdadar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}

fn cdaddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdaddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.car {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => Ok(pair3.cdr),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdar";
    todo!("{}({}) not implemented", name, args.len());
}
fn cddaar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cddaar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cddadr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cddadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.car {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cddar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cddar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => Ok(pair3.cdr),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdddar(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdddar";
    match args {
        [Object::Pair(pair)] => match pair.car {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cddddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cddddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => match pair3.cdr {
                    Object::Pair(pair4) => Ok(pair4.cdr),
                    _ => pair_required_error(name, args),
                },
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cdddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cdddr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => match pair2.cdr {
                Object::Pair(pair3) => Ok(pair3.cdr),
                _ => pair_required_error(name, args),
            },
            _ => pair_required_error(name, args),
        },
        _ => pair_required_error(name, args),
    }
}
fn cddr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cddr";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_symbolequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "symbol=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        if args[i].is_symbol() && args[i + 1].is_symbol() {
            if args[i].scheme_eq(&args[i + 1]) {
                continue;
            } else {
                return Ok(Object::False);
            }
        } else {
            return type_required_error(name, "symbol", &[args[i], args[i + 1]]);
        }
    }
    Ok(Object::True)
}
fn is_booleanequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "boolean=?";
    check_argc_at_least!(name, args, 2);
    let argc = args.len();
    for i in 0..argc - 1 {
        if args[i].is_boolean() && args[i + 1].is_boolean() && args[i].scheme_eq(&args[i + 1]) {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_vector(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_list(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "list?";
    check_argc!(name, args, 1);
    Ok(args[0].is_list().to_obj())
}
fn list(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let mut obj = Object::Nil;
    let argc = args.len() as isize;
    let mut i = argc - 1;
    loop {
        if i < 0 {
            break;
        }
        obj = vm.gc.cons(args[i as usize], obj);
        i -= 1;
    }
    Ok(obj)
}

fn memq(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "memq";
    check_argc!(name, args, 2);
    let key = args[0];
    let mut list = args[1];
    if !list.is_list() {
        return type_required_error(name, "list", &[list]);
    }

    loop {
        if list.is_nil() {
            return Ok(Object::False);
        }
        match list {
            Object::Pair(pair) => {
                if pair.car == key {
                    return Ok(list);
                }
                list = pair.cdr;
            }
            _ => {
                return type_required_error(name, "list", &[list]);
            }
        }
    }
}
fn is_eq(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eq?";
    check_argc!(name, args, 2);
    Ok(args[0].scheme_eq(&args[1]).to_obj())
}
fn is_eqv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eqv?";
    todo!("{}({}) not implemented", name, args.len());
}
fn member(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "member";
    check_argc!(name, args, 2);
    let key = args[0];
    let mut list = args[1];
    if !list.is_list() {
        return type_required_error(name, "list", &[list]);
    }

    let e = Equal::new();

    loop {
        if list.is_nil() {
            return Ok(Object::False);
        }
        match list {
            Object::Pair(pair) => {
                if e.is_equal(&mut vm.gc, &pair.car, &key) {
                    return Ok(list);
                }
                list = pair.cdr;
            }
            _ => {
                return type_required_error(name, "list", &[list]);
            }
        }
    }
}
fn is_boolean(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "boolean?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::True => Ok(Object::True),
        Object::False => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn symbol_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "symbol->string";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Symbol(s) => Ok(vm.gc.new_string(&s.string)),
        obj => type_required_error(name, "symbol", &[obj]),
    }
}
fn string_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-ref";
    check_argc!(name, args, 2);
    match args {
        [Object::String(s), Object::Fixnum(idx)] => {
            let idx = *idx as usize;
            match s.string.chars().nth(idx) {
                Some(c) => Ok(Object::Char(c)),
                _ => {
                    generic_error!(name, args, "string index out of bound {:?}", args)
                }
            }
        }
        _ => type_required_error(name, "string and number", &[args[0], args[1]]),
    }
}
fn get_timeofday(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-timeofday";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_eq_hashtable(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-eq-hashtable";
    check_argc_max!(name, args, 1);
    Ok(vm.gc.new_eq_hashtable())
}
fn make_eqv_hashtable(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-eqv-hashtable";
    check_argc_max!(name, args, 1);
    Ok(vm.gc.new_eqv_hashtable())
}
fn hashtable_set_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-set!";
    check_argc!(name, args, 3);
    match args[0] {
        Object::EqHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.set(args[1], args[2])
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::EqvHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.set(EqvKey::new(args[1]), args[2])
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::GenericHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                let key = args[1];
                let hash_obj = vm.call_closure1(hashtable.hash_func, key)?;
                let key = GenericHashKey::new(hash_obj, key);
                hashtable.set(key, args[2])
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }

        _ => {
            return Error::assertion_violation(name, "hashtable required", &[args[0]]);
        }
    }
    Ok(Object::Unspecified)
}
fn hashtable_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-ref";
    check_argc_between!(name, args, 2, 3);
    match args[0] {
        Object::EqHashtable(hashtable) => {
            if args.len() == 2 {
                Ok(hashtable.get(args[1], Object::False))
            } else {
                Ok(hashtable.get(args[1], args[2]))
            }
        }
        Object::EqvHashtable(hashtable) => {
            if args.len() == 2 {
                Ok(hashtable.get(EqvKey::new(args[1]), Object::False))
            } else {
                Ok(hashtable.get(EqvKey::new(args[1]), args[2]))
            }
        }
        Object::GenericHashtable(hashtable) => {
            let key = args[1];
            let hash_obj = vm.call_closure1(hashtable.hash_func, key)?;
            let key = GenericHashKey::new(hash_obj, key);

            if args.len() == 2 {
                Ok(hashtable.get(key, Object::False))
            } else {
                Ok(hashtable.get(key, args[2]))
            }
        }
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
fn hashtable_keys(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-keys";
    check_argc!(name, args, 1);
    let mut keys: Vec<Object> = vec![];
    match args[0] {
        Object::EqHashtable(t) => {
            for k in t.hash_map.keys() {
                keys.push(*k);
            }
        }
        Object::EqvHashtable(t) => {
            for k in t.hash_map.keys() {
                keys.push(k.obj);
            }
        }
        Object::GenericHashtable(t) => {
            for k in t.hash_map.keys() {
                keys.push(k.org_key);
            }
        }
        _ => {}
    }
    Ok(vm.gc.new_vector(&keys))
}
fn string_hash(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-hash";
    check_argc!(name, args, 1);
    let s = as_sstring!(name, args, 0);
    Ok(Object::Fixnum(string_hash_one(&s.string)))
}
fn eqv_hash(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eqv-hash";
    check_argc!(name, args, 1);
    Ok(Object::Fixnum(equal_hash_one(args[0])))
}
fn string_ci_hash(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-ci-hash";
    check_argc!(name, args, 1);
    let s = as_sstring!(name, args, 0);
    Ok(Object::Fixnum(string_hash_ci_one(&s.string)))
}
fn symbol_hash(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "symbol-hash";
    check_argc!(name, args, 1);
    let s = as_symbol!(name, args, 0);
    Ok(Object::Fixnum(symbol_hash_one(s)))
}
fn equal_hash(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "equal-hash";
    check_argc!(name, args, 1);
    Ok(Object::Fixnum(equal_hash_one(args[0])))
}

fn symbol_hash_one(s: GcRef<Symbol>) -> isize {
    // Symbol is interned, we can use pointer as hash value.
    s.pointer.as_ptr() as isize
}

fn string_hash_one(s: &str) -> isize {
    let mut hash = 0;
    let chars: Vec<char> = s.chars().collect();
    for ch in chars.iter() {
        hash = (hash << 5) - hash + (*ch as isize);
    }
    hash
}
fn string_hash_ci_one(s: &str) -> isize {
    let mut hash = 0;
    let chars: Vec<char> = s.chars().collect();
    for ch in chars.iter() {
        hash = (hash << 5) - hash
            + match ch.to_uppercase().next() {
                Some(uch) => uch as isize,
                None => *ch as isize,
            };
    }
    hash
}
fn equal_hash_one(obj: Object) -> isize {
    // borrowed from ypsilon scheme by Yoshikatsu Fujita
    match obj {
        Object::Pair(p) => {
            let hash1: isize = equal_hash_one(p.car);
            let hash2: isize = equal_hash_one(p.cdr);
            hash1 + hash2.wrapping_mul(64) - hash2
        }
        Object::Vector(v) => {
            let mut hash: isize = 1;
            for e in v.data.iter() {
                hash = hash.wrapping_mul(32) - hash + equal_hash_one(*e);
            }
            hash
        }
        Object::String(s) => string_hash_one(&s.string),
        Object::Symbol(s) => symbol_hash_one(s),
        _ => &obj as *const Object as isize,
    }
}

fn eq_hashtable_copy(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eq-hashtable-copy";
    check_argc!(name, args, 1);
    if let Object::EqHashtable(e) = args[0] {
        Ok(Object::EqHashtable(vm.gc.alloc(e.copy())))
    } else {
        Error::assertion_violation(name, "eq-hashtable required", &[args[0]])
    }
}
fn current_error_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "current-error-port";
    check_argc!(name, args, 0);
    Ok(vm.current_error_port())
}
fn values(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    vm.values(args)
}
fn vm_apply(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm/apply";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_pair(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pair?";
    check_argc!(name, args, 1);
    Ok(args[0].is_pair().to_obj())
}
fn make_custom_binary_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-custom-binary-input-port";
    check_argc!(name, args, 5);
    let id = &as_sstring!(name, args, 0).string;
    let read_proc = check_is_closure!(name, args, 1);
    let pos_proc = check_is_closure_or_false!(name, args, 2);
    let set_pos_proc = check_is_closure_or_false!(name, args, 3);
    let close_proc = check_is_closure_or_false!(name, args, 4);
    Ok(Object::CustomBinaryInputPort(vm.gc.alloc(
        CustomBinaryInputPort::new(id, read_proc, pos_proc, set_pos_proc, close_proc),
    )))
}
fn make_custom_binary_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-custom-binary-output-port";
    check_argc!(name, args, 5);
    let id = &as_sstring!(name, args, 0).string;
    let write_proc = check_is_closure!(name, args, 1);
    let pos_proc = check_is_closure_or_false!(name, args, 2);
    let set_pos_proc = check_is_closure_or_false!(name, args, 3);
    let close_proc = check_is_closure_or_false!(name, args, 4);
    Ok(Object::CustomBinaryOutputPort(vm.gc.alloc(
        CustomBinaryOutputPort::new(id, write_proc, pos_proc, set_pos_proc, close_proc),
    )))
}
fn make_custom_textual_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-custom-textual-input-port";
    check_argc!(name, args, 5);
    let id = &as_sstring!(name, args, 0).string;
    let read_proc = check_is_closure!(name, args, 1);
    let pos_proc = check_is_closure_or_false!(name, args, 2);
    let set_pos_proc = check_is_closure_or_false!(name, args, 3);
    let close_proc = check_is_closure_or_false!(name, args, 4);
    Ok(Object::CustomTextInputPort(vm.gc.alloc(
        CustomTextInputPort::new(id, read_proc, pos_proc, set_pos_proc, close_proc),
    )))
}
fn make_custom_textual_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-custom-textual-output-port";
    check_argc!(name, args, 5);
    let id = &as_sstring!(name, args, 0).string;
    let write_proc = check_is_closure!(name, args, 1);
    let pos_proc = check_is_closure_or_false!(name, args, 2);
    let set_pos_proc = check_is_closure_or_false!(name, args, 3);
    let close_proc = check_is_closure_or_false!(name, args, 4);
    Ok(Object::CustomTextOutputPort(vm.gc.alloc(
        CustomTextOutputPort::new(id, write_proc, pos_proc, set_pos_proc, close_proc),
    )))
}
fn get_u8(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-u8";
    check_argc!(name, args, 1);
    let mut buf: Vec<u8> = vec![0; 1];
    let port = as_binary_input_port_mut!(name, args, 0);
    match port.read(vm, &mut buf) {
        Ok(0) => Ok(Object::Eof),
        Ok(_) => Ok(Object::Fixnum(buf[0] as isize)),
        Err(_) => Ok(Object::Eof),
    }
}
fn put_u8(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "put-u8";
    check_argc!(name, args, 2);
    let value = as_usize!(name, args, 1);

    match u8::from_usize(value) {
        Some(value) => {
            let port = as_binary_output_port_mut!(name, args, 0);
            match port.put_u8(value) {
                Ok(_size) => Ok(Object::Unspecified),
                Err(err) => {
                    Error::assertion_violation(name, &format!("{:?}", err), &[args[0], args[1]])
                }
            }
        }
        None => Error::assertion_violation(name, "u8 value required", &[args[1]]),
    }
}
fn put_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "put-string";
    check_argc_between!(name, args, 2, 4);
    let argc = args.len();
    let str = as_sstring!(name, args, 1);
    let start = if argc >= 3 {
        as_usize!(name, args, 2)
    } else {
        0
    };
    let count = if argc >= 4 {
        as_usize!(name, args, 3)
    } else {
        str.len() - start
    };

    let str = &str[start..start + count];
    let port = as_text_output_port_mut!(name, args, 0);
    match port.put_string(str) {
        Ok(_) => Ok(Object::Unspecified),
        Err(e) => Err(e),
    }
}

fn flush_output_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flush-output-port";
    check_argc!(name, args, 1);
    let port = as_output_port_mut!(name, args, 0);
    port.flush();
    Ok(Object::Unspecified)
}
fn output_port_buffer_mode(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "output-port-buffer-mode";
    check_argc!(name, args, 1);
    let port = as_port!(name, args, 0);
    Ok(vm
        .gc
        .symbol_intern(&port.buffer_mode().to_string().to_lowercase()))
}
fn bytevector_u8_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u8-set!";
    check_argc!(name, args, 3);
    match (args[0], args[1], args[2]) {
        (Object::Bytevector(mut bv), Object::Fixnum(index), Object::Fixnum(v))
            if (index as usize) < bv.len() && (0..=255).contains(&v) =>
        {
            bv.set_u8_unchecked(index as usize, v as u8);
            Ok(Object::Unspecified)
        }
        _ => type_required_error(name, "bytevector index u8 value", args),
    }
}
fn is_port_has_port_position(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port-has-port-position?";
    check_argc!(name, args, 1);
    let port = as_port!(name, args, 0);
    Ok(port.has_position().to_obj())
}
fn is_port_has_set_port_position_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "port-has-set-port-position!?";
    check_argc!(name, args, 1);
    let port = as_port!(name, args, 0);
    Ok(port.has_set_position().to_obj())
}
fn port_position(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port-position";
    check_argc!(name, args, 1);
    let port = as_port_mut!(name, args, 0);
    if port.has_position() {
        let pos = port.position(vm)?;
        Ok(pos.to_obj(&mut vm.gc))
    } else {
        Error::assertion_violation(name, "port doesn't support port-position", &[args[0]])
    }
}
fn set_port_position_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-port-position!";
    check_argc!(name, args, 2);
    let port = as_port_mut!(name, args, 0);
    if port.has_set_position() {
        let pos = as_usize!(name, args, 1);
        match port.set_position(vm, pos) {
            Ok(_) => Ok(Object::Unspecified),
            Err(_) => {
                error::Error::io_invalid_position(name, &format!("invalid position {}", pos), args)
            }
        }
    } else {
        Error::assertion_violation(name, "port doesn't support set-ort-position!", &[args[0]])
    }
}
fn get_bytevector_n_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-bytevector-n!";
    check_argc!(name, args, 4);
    let mut bv = as_bytevector!(name, args, 1);
    let start = as_usize!(name, args, 2);
    let count = as_usize!(name, args, 3);
    if bv.len() < start + count {
        return Error::assertion_violation(
            name,
            "bytevector must be a bytevector with at least start + count elements.",
            &[args[2], args[3]],
        );
    }
    let buf = &mut bv.data[start..start + count];
    let port = as_binary_input_port_mut!(name, args, 0);
    match port.read(vm, buf) {
        Ok(0) => Ok(Object::Eof),
        Ok(size) => Ok(Object::Fixnum(size as isize)),
        Err(_) => Ok(Object::Eof),
    }
}
fn get_bytevector_some(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-bytevector-some";
    check_argc!(name, args, 1);
    let port = as_binary_input_port_mut!(name, args, 0);
    let mut buf: Vec<u8> = vec![];
    port.read_all(vm, &mut buf).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("read error {}", e),
            &[args[0]],
        )
    })?;
    if buf.is_empty() {
        Ok(Object::Eof)
    } else {
        Ok(vm.gc.new_bytevector_u8(&buf))
    }
}
fn get_bytevector_all(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-bytevector-all";
    check_argc!(name, args, 1);
    let port = as_binary_input_port_mut!(name, args, 0);
    let mut buf: Vec<u8> = vec![];
    port.read_all(vm, &mut buf).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("read error {}", e),
            &[args[0]],
        )
    })?;
    Ok(vm.gc.new_bytevector_u8(&buf))
}
fn transcoded_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "transcoded-port";
    todo!("{}({}) not implemented", name, args.len());
}
fn latin_1_codec(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "latin-1-codec";
    check_argc!(name, args, 0);
    Ok(Object::Latin1Codec(vm.gc.alloc(Latin1Codec::new())))
}
fn utf_8_codec(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "utf-8-codec";
    check_argc!(name, args, 0);
    Ok(Object::UTF8Codec(vm.gc.alloc(UTF8Codec::new())))
}
fn utf_16_codec(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "utf-16-codec";
    check_argc!(name, args, 0);
    Ok(Object::UTF16Codec(vm.gc.alloc(UTF16Codec::new())))
}
fn make_transcoder(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-transcoder";
    check_argc_between!(name, args, 1, 3);
    let codec = args[0];
    let mut eol_style = EolStyle::Lf;
    let mut mode = ErrorHandlingMode::ReplaceError;
    if args.len() >= 2 {
        let _eol_symbol = as_symbol!(name, args, 1);
        let eol_symbol = args[1];
        if eol_symbol == vm.gc.symbol_intern("lf") {
            eol_style = EolStyle::Lf;
        } else if eol_symbol == vm.gc.symbol_intern("cr") {
            eol_style = EolStyle::Cr;
        } else if eol_symbol == vm.gc.symbol_intern("nel") {
            eol_style = EolStyle::Nel;
        } else if eol_symbol == vm.gc.symbol_intern("ls") {
            eol_style = EolStyle::Ls;
        } else if eol_symbol == vm.gc.symbol_intern("crnel") {
            eol_style = EolStyle::CrNel;
        } else if eol_symbol == vm.gc.symbol_intern("crlf") {
            eol_style = EolStyle::CrLf;
        } else if eol_symbol == vm.gc.symbol_intern("none") {
            eol_style = EolStyle::ENone;
        } else {
            return Error::assertion_violation(name, "invalid eol-style", &[args[1]]);
        }
    }

    if args.len() >= 3 {
        let _mode_symbol = as_symbol!(name, args, 2);
        let mode_symbol = args[2];
        if mode_symbol.scheme_eq(&vm.gc.symbol_intern("raise")) {
            mode = ErrorHandlingMode::RaiseError;
        } else if mode_symbol.scheme_eq(&vm.gc.symbol_intern("ignore")) {
            mode = ErrorHandlingMode::IgnoreError;
        } else if mode_symbol.scheme_eq(&vm.gc.symbol_intern("replace")) {
            mode = ErrorHandlingMode::ReplaceError;
        } else {
            return Error::assertion_violation(name, "invalid error-handling-mode", &[args[2]]);
        }
    }

    Ok(Object::Transcoder(
        vm.gc.alloc(Transcoder::new(codec, eol_style, mode)),
    ))
}
fn eof_object(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eof-object";
    check_argc!(name, args, 0);
    Ok(Object::Eof)
}
fn sys_open_bytevector_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sys-open-bytevector-output-port";
    check_argc_max!(name, args, 1);
    let argc = args.len();
    if argc == 0 || args[0].is_false() {
        Ok(Object::BytevectorOutputPort(
            vm.gc.alloc(BytevectorOutputPort::new()),
        ))
    } else {
        let _transcoder = as_transcoder!(name, args, 0);
        let out_port = Object::BytevectorOutputPort(vm.gc.alloc(BytevectorOutputPort::new()));
        Ok(Object::TranscodedOutputPort(
            vm.gc.alloc(TranscodedOutputPort::new(out_port, args[0])),
        ))
    }
}

fn sys_get_bytevector(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sys-get-bytevector";
    check_argc!(name, args, 1);
    match args[0] {
        Object::BytevectorOutputPort(mut port) => Ok(port.to_bytevector(&mut vm.gc)),
        Object::TranscodedOutputPort(port) => match port.out_port {
            Object::BytevectorOutputPort(mut out_port) => Ok(out_port.to_bytevector(&mut vm.gc)),
            _ => Error::assertion_violation(
                name,
                "transcoded port is supposed to have bytevector output port",
                &[args[0]],
            ),
        },
        _ => Error::assertion_violation(name, "bytevector output port required", &[args[0]]),
    }
}
fn bytevector_length(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-length";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Bytevector(bv) => Ok(Object::Fixnum(bv.len() as isize)),
        _ => type_required_error(name, "bytevector", &[args[0]]),
    }
}
fn standard_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "standard-input-port";
    check_argc!(name, args, 0);
    Ok(Object::StdInputPort(vm.gc.alloc(StdInputPort::new())))
}
fn standard_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "standard-output-port";
    check_argc!(name, args, 0);
    Ok(Object::StdOutputPort(vm.gc.alloc(StdOutputPort::new())))
}
fn standard_error_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "standard-error-port";
    check_argc!(name, args, 0);
    Ok(Object::StdErrorPort(vm.gc.alloc(StdErrorPort::new())))
}
fn get_bytevector_n(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-bytevector-n";
    check_argc!(name, args, 2);
    let size = as_usize!(name, args, 1);
    let mut buf: Vec<u8> = vec![0; size];
    let port = as_binary_input_port_mut!(name, args, 0);
    match port.read(vm, &mut buf) {
        Ok(0) => Ok(Object::Eof),
        Ok(size) => Ok(Object::Bytevector(
            vm.gc.alloc(Bytevector::new(&buf[0..size].to_vec())),
        )),
        Err(_) => Ok(Object::Eof),
    }
}

/*
    file-options

    (file-options)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create)
      If file exists:     truncate
      If does not exist:  raise &file-does-not-exist
    (file-options no-fail)
      If file exists:     truncate
      If does not exist:  create new file
    (file-options no-truncate)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
    (file-options no-create no-fail)
      If file exists:     truncate
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
    (file-options no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  create new file
    (file-options no-create no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  raise &file-does-not-exist
    (file-options no-create no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist

*/
fn open_file_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "open-file-output-port";
    check_argc_between!(name, args, 1, 4);

    let path = match args[0] {
        Object::String(s) => s.string.to_owned(),
        _ => {
            return type_required_error(name, "path", &[args[0]]);
        }
    };
    let file_exists = Path::new(&path).exists();

    let argc = args.len();
    let mut open_options = OpenOptions::new();
    open_options.write(true).create(true);

    if argc == 1 {
        if file_exists {
            return error::Error::io_file_already_exist(
                name,
                &format!("file already exists {}", path),
                args,
            );
        }
        let file = match open_options.open(&path) {
            Ok(file) => file,
            Err(err) => {
                return generic_error!(name, &[args[0]], "{}", err);
            }
        };
        Ok(Object::BinaryFileOutputPort(
            vm.gc
                .alloc(BinaryFileOutputPort::new(file, BufferMode::Block)),
        ))
    } else {
        let file_options = match args[1] {
            Object::SimpleStruct(s) => s.field(1),
            _ => {
                return type_required_error(name, "file-options", &[args[1]]);
            }
        };
        let empty_p = file_options.is_nil();
        let sym_no_create = vm.gc.symbol_intern("no-create");
        let sym_no_truncate = vm.gc.symbol_intern("no-truncate");
        let sym_no_fail = vm.gc.symbol_intern("no-fail");
        let no_create_p = !memq(vm, &mut [sym_no_create, file_options])
            .unwrap()
            .is_false();
        let no_truncate_p = !memq(vm, &mut [sym_no_truncate, file_options])
            .unwrap()
            .is_false();
        let no_fail_p = !memq(vm, &mut [sym_no_fail, file_options])
            .unwrap()
            .is_false();

        if file_exists && empty_p {
            return error::Error::io_file_already_exist(
                name,
                &format!("file already exists {}", path),
                args,
            );
        } else if no_create_p && no_truncate_p {
            if !file_exists {
                return error::Error::io_file_not_exist(
                    name,
                    &format!("file-options no-create: file not exist {}", path),
                    args,
                );
            }
        } else if no_create_p {
            if file_exists {
                open_options.truncate(true);
            } else {
                return error::Error::io_file_not_exist(
                    name,
                    &format!("file-options no-create: file not exist {}", path),
                    args,
                );
            }
        } else if no_fail_p && no_truncate_p {
            if !file_exists {
                open_options.truncate(true);
            }
        } else if no_fail_p {
            open_options.truncate(true);
        } else if no_truncate_p {
            if file_exists {
                return error::Error::io_file_already_exist(
                    name,
                    &format!("file already exists {}", path),
                    args,
                );
            } else {
                open_options.truncate(true);
            }
        }

        // We ignore buffer-mode. This implmentation is not buffered at this momement.
        // We may revisit this. Once we conform R7RS and R6RS.
        let buffer_mode = if argc < 3 {
            BufferMode::None
        } else {
            match symbol_to_buffer_mode(as_symbol!(name, args, 2)) {
                Some(mode) => mode,
                None => {
                    return Error::assertion_violation(name, "invalid buffer-mode", &[args[2]]);
                }
            }
        };

        let mut transcoder: Option<Object> = None;
        if argc == 4 {
            match args[3] {
                Object::Transcoder(_) => transcoder = Some(args[3]),
                Object::False => {}
                _ => {
                    return Error::assertion_violation(
                        name,
                        "transcoder or #f required",
                        &[args[3]],
                    );
                }
            }
        }

        let file = match open_options.open(path) {
            Ok(file) => file,
            Err(err) => return Error::assertion_violation(name, &format!("{}", err), &[args[0]]),
        };

        match transcoder {
            Some(t) => {
                let in_port = Object::BinaryFileOutputPort(
                    vm.gc.alloc(BinaryFileOutputPort::new(file, buffer_mode)),
                );
                let port = TranscodedOutputPort::new(in_port, t);
                Ok(Object::TranscodedOutputPort(vm.gc.alloc(port)))
            }
            None => Ok(Object::BinaryFileOutputPort(
                vm.gc.alloc(BinaryFileOutputPort::new(file, buffer_mode)),
            )),
        }
    }
}

fn symbol_to_buffer_mode(sym: GcRef<Symbol>) -> Option<BufferMode> {
    if sym.string == "line" {
        Some(BufferMode::Line)
    } else if sym.string == "block" {
        Some(BufferMode::Block)
    } else if sym.string == "none" {
        Some(BufferMode::None)
    } else {
        None
    }
}

fn open_file_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "open-file-input-port";
    check_argc_between!(name, args, 1, 4);
    let argc = args.len();

    let path = &as_sstring!(name, args, 0).string;
    let mut transcoder: Option<Object> = None;

    // We first check if the file is available in the binary as embdedd file.
    if let Some(file) = StdLib::get(path) {
        let in_port =
            Object::BytevectorInputPort(vm.gc.alloc(BytevectorInputPort::new(&file.data)));
        let transcoder = create_native_transcoder(&mut vm.gc);
        let port = TranscodedInputPort::new(in_port, transcoder);
        return Ok(Object::TranscodedInputPort(vm.gc.alloc(port)));
    }

    // N.B. As R6RS says, we ignore "file-options" for input-port.
    let file = match File::open(path) {
        Ok(file) => file,
        Err(err) => return Error::scheme_error(name, &format!("{}", err), &[args[0]]),
    };

    // We also ignore buffer-mode so input port is always buffered.
    let buffer_mode = if argc < 3 {
        BufferMode::None
    } else {
        match symbol_to_buffer_mode(as_symbol!(name, args, 2)) {
            Some(mode) => mode,
            None => {
                return Error::assertion_violation(name, "invalid buffer-mode", &[args[2]]);
            }
        }
    };

    if argc == 4 {
        match args[3] {
            Object::Transcoder(_) => transcoder = Some(args[3]),
            Object::False => {}
            _ => {
                return Error::assertion_violation(name, "transcoder or #f required", &[args[3]]);
            }
        }
    }

    match transcoder {
        Some(t) => {
            let in_port = Object::BinaryFileInputPort(
                vm.gc.alloc(BinaryFileInputPort::new(file, buffer_mode)),
            );
            let port = TranscodedInputPort::new(in_port, t);
            Ok(Object::TranscodedInputPort(vm.gc.alloc(port)))
        }
        None => Ok(Object::BinaryFileInputPort(
            vm.gc.alloc(BinaryFileInputPort::new(file, buffer_mode)),
        )),
    }
}
fn close_input_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "close-input-port";
    check_argc!(name, args, 1);
    if args[0].is_binary_input_port() {
        let port = as_binary_input_port_mut!(name, args, 0);
        port.close();
    } else if args[0].is_textual_input_port() {
        let port = as_text_input_port_mut!(name, args, 0);
        port.close();
    } else {
        return Error::assertion_violation(name, "input_port required", &[args[0]]);
    }
    Ok(Object::Unspecified)
}
fn vector(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector";
    todo!("{}({}) not implemented", name, args.len());
}
fn regexp_replace(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "regexp-replace";
    todo!("{}({}) not implemented", name, args.len());
}
fn regexp_replace_all(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "regexp-replace-all";
    todo!("{}({}) not implemented", name, args.len());
}
fn source_info(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "source-info";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Pair(p) => Ok(p.src),
        Object::Closure(c) => Ok(c.src),
        _ => Ok(Object::False),
    }
}
pub fn eval(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eval";
    check_argc!(name, args, 2);
    vm.eval_after(args[0])
}
fn eval_compiled(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "eval-compiled";
    check_argc!(name, args, 1);
    vm.eval_compiled(args[0])
}

// We make apply public so that Vm can access.
pub fn apply(_vm: &mut Vm, _args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "apply";
    bug!("{} should not be called. It is handled in call in vm", name);
}
fn assq(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "assq";
    check_argc!(name, args, 2);
    let key = args[0];
    let mut alist = args[1];
    if !alist.is_list() {
        return type_required_error(name, "list", &[alist]);
    }
    loop {
        if alist.is_nil() {
            return Ok(Object::False);
        }
        match alist {
            Object::Pair(pair) => match pair.car {
                Object::Pair(pair2) => {
                    if key == pair2.car {
                        return Ok(pair.car);
                    }
                    alist = pair.cdr;
                }
                _ => {
                    return type_required_error(name, "alist", &[pair.car]);
                }
            },
            _ => {
                return type_required_error(name, "alist", &[alist]);
            }
        }
    }
}
fn assoc(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "assoc";
    check_argc!(name, args, 2);
    let obj = args[0];
    let list = args[1];
    if !list.is_list() {
        return type_required_error(name, "list", &[list]);
    }
    let mut o = list;
    loop {
        if o.is_nil() {
            break;
        }
        let e = Equal::new();
        if e.is_equal(&mut vm.gc, &obj, &o.car_unchecked().car_unchecked()) {
            return Ok(o.car_unchecked());
        }
        o = o.cdr_unchecked();
    }
    Ok(Object::False)
}
fn assv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "assv";
    check_argc!(name, args, 2);
    let obj = args[0];
    let list = args[1];
    if !list.is_list() {
        return type_required_error(name, "list", &[list]);
    }
    let mut o = list;
    loop {
        if o.is_nil() {
            break;
        }
        if obj.eqv(&o.car_unchecked().car_unchecked()) {
            return Ok(o.car_unchecked());
        }
        o = o.cdr_unchecked();
    }
    Ok(Object::False)
}
fn exit(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "exit";
    check_argc_max!(name, args, 1);
    if args.is_empty() {
        process::exit(0)
    } else {
        match args[0] {
            Object::Fixnum(fx) => process::exit(fx as i32),
            Object::False => process::exit(-1),
            _ => type_required_error(name, "integer or boolean", &[args[0], args[1]]),
        }
    }
}

fn macroexpand_1(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "macroexpand-1";
    todo!("{}({}) not implemented", name, args.len());
}
fn memv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "memv";
    check_argc!(name, args, 2);
    let arg1 = args[0];
    let p = args[1];
    if !p.is_list() {
        return type_required_error(name, "list", &[p]);
    }
    let mut o = p;
    loop {
        if o.is_nil() {
            break;
        }
        if o.car_unchecked().eqv(&arg1) {
            return Ok(o);
        }
        o = o.cdr_unchecked();
    }
    Ok(Object::False)
}

fn is_procedure(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "procedure?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Procedure(_) | Object::Closure(_) | Object::Continuation(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn load(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "load";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_symbol(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "symbol?";
    check_argc!(name, args, 1);
    Ok(args[0].is_symbol().to_obj())
}
fn is_charle(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char<=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() {
        match args[i] {
            Object::Char(c) => {
                if i == args.len() - 1 {
                    break;
                }
                match args[i + 1] {
                    Object::Char(cnext) => {
                        if c > cnext {
                            return Ok(Object::False);
                        }
                    }
                    obj => {
                        return type_required_error(name, "char", &[obj]);
                    }
                }
            }
            obj => {
                return type_required_error(name, "char", &[obj]);
            }
        }
    }
    Ok(Object::True)
}
fn is_charlt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char<?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() {
        match args[i] {
            Object::Char(c) => {
                if i == args.len() - 1 {
                    break;
                }
                match args[i + 1] {
                    Object::Char(cnext) => {
                        if c >= cnext {
                            return Ok(Object::False);
                        }
                    }
                    obj => {
                        return type_required_error(name, "char", &[obj]);
                    }
                }
            }
            obj => {
                return type_required_error(name, "char", &[obj]);
            }
        }
    }
    Ok(Object::True)
}
fn is_charge(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char>=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() {
        match args[i] {
            Object::Char(c) => {
                if i == args.len() - 1 {
                    break;
                }
                match args[i + 1] {
                    Object::Char(cnext) => {
                        if c < cnext {
                            return Ok(Object::False);
                        }
                    }
                    obj => {
                        return type_required_error(name, "char", &[obj]);
                    }
                }
            }
            obj => {
                return type_required_error(name, "char", &[obj]);
            }
        }
    }
    Ok(Object::True)
}
fn is_chargt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "char>?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() {
        match args[i] {
            Object::Char(c) => {
                if i == args.len() - 1 {
                    break;
                }
                match args[i + 1] {
                    Object::Char(cnext) => {
                        if c <= cnext {
                            return Ok(Object::False);
                        }
                    }
                    obj => {
                        return type_required_error(name, "char", &[obj]);
                    }
                }
            }
            obj => {
                return type_required_error(name, "char", &[obj]);
            }
        }
    }
    Ok(Object::True)
}
fn read(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "read";
    let argc = args.len();
    if argc == 0 {
        match vm.read() {
            Ok(obj) => Ok(obj),
            Err(e) => Error::assertion_violation(name, &format!("{:?}", e), &[]),
        }
    } else if argc == 1 {
        let port = as_text_input_port_mut!(name, args, 0);
        match port.read(vm) {
            Ok(obj) => Ok(obj),
            Err(err) => {
                return generic_error!(name, &[args[0]], "{:?}", err);
            }
        }
    } else {
        todo!("{}({}) not implemented", name, args.len());
    }
}
fn vector_to_list(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector->list";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Vector(v) => Ok(vm.gc.listn(&v.data[..])),
        obj => type_required_error(name, "vector", &[obj]),
    }
}
fn set_source_info_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-source-info!";
    check_argc!(name, args, 2);
    match args[0] {
        Object::Pair(mut p) => {
            p.src = args[1];
            Ok(args[0])
        }
        Object::Closure(mut c) => {
            c.src = args[1];
            Ok(args[0])
        }
        obj => type_required_error(name, "pair", &[obj]),
    }
}
fn call_process(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%call-process";
    todo!("{}({}) not implemented", name, args.len());
}
fn confstr(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%confstr";
    todo!("{}({}) not implemented", name, args.len());
}
fn dup(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%dup";
    todo!("{}({}) not implemented", name, args.len());
}
fn start_process(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%start-process";
    todo!("{}({}) not implemented", name, args.len());
}
fn get_closure_name(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%get-closure-name";
    todo!("{}({}) not implemented", name, args.len());
}
fn append(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "append";
    if args.is_empty() {
        return Ok(Object::Nil);
    }
    let mut ret = args[args.len() - 1];
    let mut i = args.len() as isize - 2;
    loop {
        if i < 0 {
            break;
        }
        let p = args[i as usize];
        if !p.is_list() {
            return type_required_error(name, "list", &[p]);
        }
        ret = vm.gc.append2(p, ret)?;
        i -= 1;
    }
    Ok(ret)
}
fn append2(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "append2";
    todo!("{}({}) not implemented", name, args.len());
}
fn append_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "append!";
    match args {
        &mut [] => Ok(Object::Nil),
        _ => {
            let mut ret = args[args.len() - 1];
            let mut i = args.len() as isize - 2;
            loop {
                if i < 0 {
                    break;
                }
                if !args[i as usize].is_list() {
                    return type_required_error(name, "list", &[args[i as usize]]);
                }
                ret = Pair::append_destructive(args[i as usize], ret)?;
                i -= 1;
            }
            Ok(ret)
        }
    }
}
fn pass3_find_free(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pass3/find-free";
    todo!("{}({}) not implemented", name, args.len());
}
fn pass3_find_sets(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pass3/find-sets";
    todo!("{}({}) not implemented", name, args.len());
}
fn pass4_fixup_labels(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pass4/fixup-labels";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_code_builder(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-code-builder";
    println!("{}({}) not implemented", name, args.len());
    Ok(Object::False)
}
fn code_builder_put_extra1_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-put-extra1!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra2_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-put-extra2!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra3_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-put-extra3!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra4_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-put-extra4!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra5_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-put-extra5!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_append_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-append!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_emit(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "code-builder-emit";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg0_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "code-builder-put-insn-arg0!";
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg1_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "code-builder-put-insn-arg1!";
    println!("arg1={} {} {}", args[0], args[1], args[2]);
    todo!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg2_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "code-builder-put-insn-arg2!";
    todo!("{}({}) not implemented", name, args.len());
}
fn length(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "length";
    check_argc!(name, args, 1);
    if !Pair::is_list(args[0]) {
        vm.print_stack();
        return type_required_error(name, "list", &[args[0]]);
    }
    let mut len = 0;
    let mut obj = args[0];
    loop {
        if obj.is_nil() {
            break;
        }
        match obj {
            Object::Pair(p) => {
                obj = p.cdr;
                len += 1;
            }
            _ => {
                return type_required_error(name, "list", &[args[0]]);
            }
        }
    }
    Ok(Object::Fixnum(len))
}
fn list_to_vector(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "list->vector";
    check_argc!(name, args, 1);
    if !Pair::is_list(args[0]) {
        return type_required_error(name, "list", &[args[0]]);
    }
    let mut v = vec![];
    let mut obj = args[0];
    loop {
        if obj.is_nil() {
            break;
        }
        v.push(obj.car_unchecked());
        obj = obj.to_pair().cdr;
    }
    Ok(vm.gc.new_vector(&v))
}
fn pass3_compile_refer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pass3/compile-refer";
    todo!("{}({}) not implemented", name, args.len());
}
fn pass1_find_symbol_in_lvars(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pass1/find-symbol-in-lvars";
    todo!("{}({}) not implemented", name, args.len());
}
fn label(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "$label";
    todo!("{}({}) not implemented", name, args.len());
}
fn local_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "$local-ref";
    todo!("{}({}) not implemented", name, args.len());
}

// Originaly from Ypsilon Scheme.
fn do_transpose(vm: &mut Vm, each_len: usize, args: &mut [Object]) -> Object {
    let mut ans = Object::Nil;
    let mut ans_tail = Object::Nil;
    for _ in 0..each_len {
        let elt = vm.gc.cons(args[0].car_unchecked(), Object::Nil);
        let mut elt_tail = elt;
        args[0] = args[0].cdr_unchecked();
        for n in 1..args.len() {
            elt_tail.to_pair().cdr = vm.gc.cons(args[n].car_unchecked(), Object::Nil);
            elt_tail = elt_tail.cdr_unchecked();
            args[n] = args[n].cdr_unchecked();
        }
        if ans == Object::Nil {
            ans = vm.gc.cons(elt, Object::Nil);
            ans_tail = ans;
        } else {
            ans_tail.to_pair().cdr = vm.gc.cons(elt, Object::Nil);
            ans_tail = ans_tail.cdr_unchecked();
        }
    }
    ans
}

fn list_transposeadd(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "list-transpose+";
    check_argc_at_least!(name, args, 1);
    let lst0 = args[0];
    if !lst0.is_list() {
        return Ok(Object::False);
    }
    let length = Pair::list_len(lst0);
    for i in 1..args.len() {
        let lst = args[i];
        if lst.is_list() {
            if Pair::list_len(lst) != length {
                return Ok(Object::False);
            }
        } else {
            return Ok(Object::False);
        }
    }
    Ok(do_transpose(vm, length, args))
}

fn symbol_value(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "symbol-value";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Symbol(symbol) => match vm.global_value(symbol) {
            Some(&value) => Ok(value),
            None => {
                generic_error!(name, args, "identifier {} not found", symbol.string)
            }
        },
        obj => type_required_error(name, "symbol", &[obj]),
    }
}
fn set_symbol_value_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-symbol-value!";
    check_argc!(name, args, 2);
    match args[0] {
        Object::Symbol(sym) => {
            vm.set_global_value(sym, args[1]);
            Ok(Object::Unspecified)
        }
        obj => type_required_error(name, "symbol", &[obj]),
    }
}
fn make_hashtable(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-hashtable";
    check_argc_between!(name, args, 2, 3);
    if args[0].is_callable() && args[1].is_callable() {
        Ok(vm.gc.new_generic_hashtable(args[0], args[1]))
    } else {
        Error::assertion_violation(
            name,
            "hash function and eq function required",
            &[args[0], args[1]],
        )
    }
}
fn is_hashtable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(_) => Ok(Object::True),
        Object::EqvHashtable(_) => Ok(Object::True),
        Object::GenericHashtable(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn hashtable_size(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-size";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(hashtable) => Ok(Object::Fixnum(hashtable.size() as isize)),
        Object::EqvHashtable(hashtable) => Ok(Object::Fixnum(hashtable.size() as isize)),
        Object::GenericHashtable(hashtable) => Ok(Object::Fixnum(hashtable.size() as isize)),
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
fn hashtable_delete_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-delete!";
    check_argc!(name, args, 2);
    match args[0] {
        Object::EqHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.delte(args[1])
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::EqvHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.delte(EqvKey::new(args[1]))
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::GenericHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                let key = args[1];
                let hash_obj = vm.call_closure1(hashtable.hash_func, key)?;
                let key = GenericHashKey::new(hash_obj, key);
                hashtable.delte(key);
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        _ => {
            return Error::assertion_violation(name, "hashtable required", &[args[0]]);
        }
    }
    Ok(Object::Unspecified)
}
fn is_hashtable_contains(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-contains?";
    check_argc!(name, args, 2);
    match args[0] {
        Object::EqHashtable(hashtable) => Ok(hashtable.contains(args[1]).to_obj()),
        Object::EqvHashtable(hashtable) => Ok(hashtable.contains(EqvKey::new(args[1])).to_obj()),
        Object::GenericHashtable(hashtable) => {
            let key = args[1];
            let hash_obj = vm.call_closure1(hashtable.hash_func, key)?;
            let key = GenericHashKey::new(hash_obj, key);
            Ok(hashtable.contains(key).to_obj())
        }
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
fn hashtable_copy(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-copy";
    check_argc_between!(name, args, 1, 2);
    match args[0] {
        Object::EqHashtable(hashtable) => {
            let mut ret = vm.gc.alloc(EqHashtable::new());
            for (key, value) in &hashtable.hash_map {
                ret.set(*key, *value);
            }
            if args.len() == 2 && !args[1].is_false() {
                ret.is_mutable = true;
            } else {
                ret.is_mutable = false;
            }
            Ok(Object::EqHashtable(ret))
        }
        Object::EqvHashtable(hashtable) => {
            let mut ret = vm.gc.alloc(EqvHashtable::new());
            for (key, value) in &hashtable.hash_map {
                ret.set(*key, *value);
            }
            if args.len() == 2 && !args[1].is_false() {
                ret.is_mutable = true;
            } else {
                ret.is_mutable = false;
            }
            Ok(Object::EqvHashtable(ret))
        }
        Object::GenericHashtable(hashtable) => {
            let mut ret = vm.gc.alloc(GenericHashtable::new(
                hashtable.hash_func,
                hashtable.eq_func,
            ));
            for (key, value) in &hashtable.hash_map {
                ret.set(*key, *value);
            }
            if args.len() == 2 && !args[1].is_false() {
                ret.is_mutable = true;
            } else {
                ret.is_mutable = false;
            }
            Ok(Object::GenericHashtable(ret))
        }
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
fn is_hashtable_mutable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-mutable?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(hashtable) => Ok(hashtable.is_mutable().to_obj()),
        Object::EqvHashtable(hashtable) => Ok(hashtable.is_mutable().to_obj()),
        Object::GenericHashtable(hashtable) => Ok(hashtable.is_mutable().to_obj()),
        _ => Ok(Object::False),
    }
}
fn hashtable_clear_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-clear!";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.clear()
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::EqvHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.clear()
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        Object::GenericHashtable(mut hashtable) => {
            if hashtable.is_mutable() {
                hashtable.clear()
            } else {
                return Error::assertion_violation(name, "hashtable is immutable", &[args[0]]);
            }
        }
        _ => {
            return Error::assertion_violation(name, "hashtable required", &[args[0]]);
        }
    }
    Ok(Object::Unspecified)
}

fn hashtable_equivalence_function(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-equivalence-function";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(_) => Ok(vm.gc.new_procedure(is_eq, "eq?")),
        Object::EqvHashtable(_) => Ok(vm.gc.new_procedure(is_eqv, "eqv?")),
        Object::GenericHashtable(hashtable) => Ok(hashtable.eq_func),
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
fn hashtable_hash_function(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "hashtable-hash-function";
    check_argc!(name, args, 1);
    match args[0] {
        Object::GenericHashtable(hashtable) => Ok(hashtable.hash_func),
        _ => Error::assertion_violation(name, "hashtable required", &[args[0]]),
    }
}
// When non-continuable, we just print it and exit.
fn throw(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "throw";
    check_argc!(name, args, 1);
    eprintln!("{}", args[0]);
    vm.show_stack_trace();
    process::exit(-1);
}
fn number_lt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "<";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        if args[i].is_number() && args[i + 1].is_number() {
            if numbers::lt(args[i], args[i + 1]) {
                continue;
            } else {
                return Ok(Object::False);
            }
        } else {
            return type_required_error(name, "number", &[args[i], args[i + 1]]);
        }
    }
    Ok(Object::True)
}
fn number_le(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "<=";
    todo!("{}({}) not implemented", name, args.len());
}
fn number_gt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = ">";
    for i in 0..args.len() - 1 {
        if args[i].is_number() && args[i + 1].is_number() {
            if numbers::gt(args[i], args[i + 1]) {
                continue;
            } else {
                return Ok(Object::False);
            }
        } else {
            return type_required_error(name, "number", &[args[i], args[i + 1]]);
        }
    }
    Ok(Object::True)
}
fn number_ge(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = ">=";
    todo!("{}({}) not implemented", name, args.len());
}
fn number_eq(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "=";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        if args[i].is_number() && args[i + 1].is_number() {
            if numbers::eqv(args[i], args[i + 1]) {
                continue;
            } else {
                return Ok(Object::False);
            }
        } else {
            return type_required_error(name, "number", &[args[i], args[i + 1]]);
        }
    }
    Ok(Object::True)
}

fn number_add(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "+";
    let argc = args.len();
    if argc == 0 {
        Ok(Object::Fixnum(0))
    } else if argc == 1 {
        if args[0].is_number() {
            Ok(args[0])
        } else {
            return number_required_error(name, &[args[0]]);
        }
    } else {
        let mut ret = Object::Fixnum(0);
        for arg in args.iter() {
            ret = numbers::add(&mut vm.gc, ret, *arg);
        }
        Ok(ret)
    }
}

fn nuber_sub(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "-";
    let argc = args.len();
    if argc == 0 {
        Ok(Object::Fixnum(0))
    } else if argc == 1 {
        if args[0].is_number() {
            Ok(numbers::negate(&mut vm.gc, args[0]))
        } else {
            return number_required_error(name, &[args[0]]);
        }
    } else {
        let mut ret = args[0];
        for i in 1..argc - 1 {
            ret = numbers::sub(&mut vm.gc, ret, args[i]);
        }
        Ok(ret)
    }
}
fn number_mul(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "*";
    let argc = args.len();
    if argc == 0 {
        Ok(Object::Fixnum(1))
    } else if argc == 1 {
        if args[0].is_number() {
            Ok(args[0])
        } else {
            return number_required_error(name, &[args[0]]);
        }
    } else {
        let mut ret = Object::Fixnum(1);
        for obj in args {
            if !obj.is_number() {
                return number_required_error(name, &[*obj]);
            }
            ret = numbers::mul(&mut vm.gc, ret, *obj);
        }
        Ok(ret)
    }
}
fn number_div(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "/";
    check_argc_at_least!(name, args, 1);
    let argc = args.len();
    if argc == 1 {
        match numbers::div(&mut vm.gc, Object::Fixnum(1), args[0]) {
            Ok(value) => Ok(value),
            Err(SchemeError::Div0) => {
                generic_error!(name, &[args[0]], "division by zero {}", args[0])
            }
            _ => bug!(),
        }
    } else {
        todo!();
    }
}
fn max(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "max";
    check_argc_at_least!(name, args, 1);
    let mut max_num = Object::Flonum(Flonum::new(f64::NEG_INFINITY));
    let mut is_exact = true;
    for n in args.iter() {
        if n.is_real() {
            let is_flonum = n.is_flonum();
            if is_flonum && n.to_flonum().is_nan() {
                return Ok(*n);
            }
            if is_flonum {
                is_exact = false;
            }
            if numbers::gt(*n, max_num) {
                max_num = *n;
            }
        } else {
            return number_required_error(name, &[*n]);
        }
    }
    if is_exact {
        Ok(max_num)
    } else {
        Ok(numbers::inexact(&mut vm.gc, max_num))
    }
}

fn min(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "min";
    let mut min = Object::Flonum(Flonum::new(f64::INFINITY));
    for obj in args {
        if !obj.is_number() {
            return number_required_error(name, &[*obj]);
        }
        if obj.is_flonum() && obj.to_flonum().is_nan() {
            return Ok(*obj);
        }
        if numbers::lt(*obj, min) {
            min = *obj;
        }
    }
    Ok(min)
}
fn get_char(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-char";
    check_argc!(name, args, 1);
    let port = as_text_input_port_mut!(name, args, 0);
    match port.read_char(vm) {
        Ok(Some(c)) => Ok(Object::Char(c)),
        Ok(None) => Ok(Object::Eof),
        Err(e) => Err(e),
    }
}
fn lookahead_char(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "lookahead-char";
    check_argc!(name, args, 1);
    let port = as_text_input_port_mut!(name, args, 0);
    match port.lookahead_char(vm) {
        Ok(Some(c)) => Ok(Object::Char(c)),
        Ok(None) => Ok(Object::Eof),
        Err(e) => Err(e),
    }
}
fn get_string_n(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-string-n";
    check_argc!(name, args, 2);
    let n = as_usize!(name, args, 1);
    let mut s = String::new();
    let port = as_text_input_port_mut!(name, args, 0);
    match port.read_n_to_string(vm, &mut s, n) {
        Ok(_) => {
            if !s.is_empty() {
                Ok(vm.gc.new_string(&s))
            } else {
                Ok(Object::Eof)
            }
        }
        Err(_) => Ok(Object::Eof),
    }
}

fn get_string_n_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-string-n!";
    check_argc!(name, args, 4);
    let port = as_text_input_port_mut!(name, args, 0);
    let mut dest = as_sstring!(name, args, 1);
    let start = as_usize!(name, args, 2);
    let count = as_usize!(name, args, 3);
    if dest.len() < start + count {
        return error::Error::assertion_violation(
            name,
            "string must be a string with at least start + count elements.",
            args,
        );
    }
    let mut s = String::new();
    port.read_n_to_string(vm, &mut s, count).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("read error {}", e),
            &[args[0]],
        )
    })?;
    if s.is_empty() {
        Ok(Object::Eof)
    } else {
        dest.string.replace_range(start..start + s.len(), &s);
        Ok(s.len().to_obj(&mut vm.gc))
    }
}
fn get_string_all(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-string-all";
    check_argc!(name, args, 1);
    let port = as_text_input_port_mut!(name, args, 0);
    let mut s = String::new();
    match port.read_to_string(vm, &mut s) {
        Ok(_) => Ok(vm.gc.new_string(&s)),
        Err(e) => Err(e),
    }
}
fn get_line(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-line";
    check_argc!(name, args, 1);
    let mut s = String::new();
    let port = as_text_input_port_mut!(name, args, 0);
    match port.read_line(vm, &mut s) {
        Ok(size) => {
            if size == 0 {
                Ok(Object::Eof)
            } else {
                Ok(vm.gc.new_string(&s))
            }
        }
        Err(_) => Ok(Object::Eof),
    }
}
fn get_datum(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-datum";
    check_argc!(name, args, 1);
    let port = as_text_input_port_mut!(name, args, 0);
    port.read(vm).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("read error {:?}", e),
            &[args[0]],
        )
    })
}
fn is_bytevector(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Bytevector(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn current_directory(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "current-directory";
    check_argc!(name, args, 0);
    match current_dir() {
        Ok(path_buf) => match path_buf.as_os_str().to_str() {
            Some(s) => Ok(vm.gc.new_string(s)),
            None => {
                generic_error!(name, args, "{} conversion error", "os_str")
            }
        },
        Err(err) => {
            generic_error!(name, args, "{}", err)
        }
    }
}
fn standard_library_path(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "standard-library-path";
    check_argc!(name, args, 0);
    Ok(vm.gc.new_string("."))
}
fn native_endianness(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "native-endianness";
    check_argc!(name, args, 0);
    if cfg!(target_endian = "big") {
        Ok(Object::Symbol(vm.gc.intern("big")))
    } else {
        Ok(Object::Symbol(vm.gc.intern("little")))
    }
}
fn make_bytevector(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-bytevector";
    check_argc_between!(name, args, 1, 2);
    let value: u8 = if args.len() == 1 {
        0
    } else {
        as_u8!(name, args, 1)
    };
    match args[0] {
        Object::Fixnum(len) => {
            let v: Vec<u8> = vec![value; len as usize];
            Ok(Object::Bytevector(vm.gc.alloc(Bytevector::new(&v))))
        }
        _ => number_required_error(name, &[args[0]]),
    }
}

fn is_bytevectorequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector=?";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_fill_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-fill!";
    let mut bv = as_bytevector!(name, args, 0);
    let value = as_u8!(name, args, 1);
    bv.fill(value);
    Ok(Object::Unspecified)
}
fn bytevector_copy_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-copy!";
    check_argc!(name, args, 5);

    match (args[0], args[1], args[2], args[3], args[4]) {
        (
            Object::Bytevector(src),
            Object::Fixnum(src_start),
            Object::Bytevector(mut dst),
            Object::Fixnum(dst_start),
            Object::Fixnum(k),
        ) => {
            if (src_start <= src_start + k)
                && (src_start + k <= (src.len() as isize))
                && (0 <= dst_start)
                && (dst_start + k <= (dst.len() as isize))
            {
                if dst == src && dst_start > src_start {
                    for i in (0..k).rev() {
                        let dst_idx = (dst_start + i) as usize;
                        let src_idx = (src_start + i) as usize;
                        dst.data[dst_idx] = src.data[src_idx];
                    }
                } else {
                    for i in 0..k {
                        let dst_idx = (dst_start + i) as usize;
                        let src_idx = (src_start + i) as usize;
                        dst.data[dst_idx] = src.data[src_idx];
                    }
                }
            } else {
                return generic_error!(
                    name,
                    args,
                    "invalid range src-start-{} dst-strt={} k={}",
                    src_start,
                    dst_start,
                    k
                );
            }
        }
        _ => {
            return type_required_error(name, "(bv1 start1 bv2 start2 k)", args);
        }
    }
    Ok(Object::Unspecified)
}
fn bytevector_copy(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-copy";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Bytevector(bv) => Ok(Object::Bytevector(vm.gc.alloc(bv.copy()))),
        _ => type_required_error(name, "bytevector", &[args[0]]),
    }
}
fn bytevector_u8_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u8-ref";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::Bytevector(bv), Object::Fixnum(index)) => match bv.ref_u8(index as usize) {
            Some(v) => Ok(Object::Fixnum(v as isize)),
            None => generic_error!(name, args, "index out of range {}", index),
        },
        _ => type_required_error(name, "bytevector and index", args),
    }
}

fn bytevector_s8_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s8-ref";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::Bytevector(bv), Object::Fixnum(index)) => match bv.ref_i8(index as usize) {
            Some(v) => Ok(Object::Fixnum(v as isize)),
            None => generic_error!(name, args, "index out of range {}", index),
        },
        _ => type_required_error(name, "bytevector and index", args),
    }
}
fn bytevector_s8_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s8-set!";
    check_argc!(name, args, 3);
    match (args[0], args[1], args[2]) {
        (Object::Bytevector(mut bv), Object::Fixnum(index), Object::Fixnum(v))
            if (index as usize) < bv.len() && (-128..=127).contains(&v) =>
        {
            bv.set_i8_unchecked(index as usize, v as i8);
            Ok(Object::Unspecified)
        }
        _ => type_required_error(name, "bytevector index i8", args),
    }
}
fn bytevector_to_u8_list(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector->u8-list";
    check_argc!(name, args, 1);
    let mut ret = Object::Nil;
    if let Object::Bytevector(bv) = args[0] {
        for i in 0..bv.len() {
            ret = vm.gc.cons(
                Object::Fixnum(bv.ref_u8_unchecked(bv.len() - i - 1) as isize),
                ret,
            );
        }
        Ok(ret)
    } else {
        type_required_error(name, "bytevector", &[args[0]])
    }
}
fn u8_list_to_bytevector(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "u8-list->bytevector";
    check_argc!(name, args, 1);
    match Bytevector::from_list(args[0]) {
        Some(bv) => Ok(Object::Bytevector(vm.gc.alloc(bv))),
        None => type_required_error(name, "u8 list", &[args[0]]),
    }
}
fn bytevector_u16_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u16-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_u16_little(index)
    } else {
        bv.ref_u16_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s16_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s16-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 2 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_s16_little(index)
    } else {
        bv.ref_s16_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_u16_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u16-native-ref";
    check_argc!(name, args, 2);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 2 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let ret = if cfg!(target_endian = "big") {
        bv.ref_u16_big(index)
    } else {
        bv.ref_u16_little(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s16_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s16-native-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_set_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u16-set!";
    check_argc!(name, args, 4);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let value = as_isize!(name, args, 2) as u16;
    let _endianness = as_symbol!(name, args, 3);
    let ret = if args[3] == vm.gc.symbol_intern("little") {
        bv.set_u16_little(index, value)
    } else {
        bv.set_u16_big(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s16_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s16-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-u16-native-set!";
    check_argc!(name, args, 3);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 2 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let value = as_isize!(name, args, 2) as u16;
    let ret = if cfg!(target_endian = "big") {
        bv.set_u16_big(index, value)
    } else {
        bv.set_u16_little(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s16_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-s16-native-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u32-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_u32_little(index)
    } else {
        bv.ref_u32_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s32_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s32-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_s32_little(index)
    } else {
        bv.ref_s32_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_u32_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u32-native-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s32-native-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u32-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s32-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-u32-native-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-s32-native-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u64-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_u64_little(index)
    } else {
        bv.ref_u64_big(index)
    };
    match ret {
        Some(v) => Ok(v.to_obj(&mut vm.gc)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_s64_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s64-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_s64_little(index)
    } else {
        bv.ref_s64_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Fixnum(v as isize)),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_u64_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u64-native-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s64-native-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-u64-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-s64-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-u64-native-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-s64-native-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector->string";
    let bv = as_bytevector!(name, args, 0);
    let mut transcoder = as_transcoder!(name, args, 1);

    let mut raw_port = BytevectorInputPort::new(&bv.data);
    let port: &mut dyn BinaryInputPort = &mut raw_port;

    // Set the port to i/o decoding error.
    let s = &transcoder.read_string(vm, port).map_err(|mut e| {
        e.irritants = vec![Object::BytevectorInputPort(vm.gc.alloc(raw_port))];
        e
    })?;
    Ok(vm.gc.new_string(s))
}
fn string_to_bytevector(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->bytevector";
    check_argc!(name, args, 2);
    let text = as_sstring!(name, args, 0);
    let _transcoder = as_transcoder!(name, args, 1);
    let transcoder = args[1];
    let out_port = Object::BytevectorOutputPort(vm.gc.alloc(BytevectorOutputPort::new()));

    let mut port = TranscodedOutputPort::new(out_port, transcoder);
    for ch in text.string.chars() {
        port.write_char(ch).map_err(|e| {
            Error::new(
                ErrorType::IoEncodingError,
                name,
                &format!("write error {}", e),
                &[Object::Char(ch), out_port],
            )
        })?;
    }
    Ok(out_port
        .to_bytevector_output_port()
        .to_bytevector(&mut vm.gc))
}
fn string_to_utf8(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->utf8";
    check_argc!(name, args, 1);
    if let Object::String(s) = args[0] {
        Ok(Object::Bytevector(
            vm.gc.alloc(Bytevector::new(&s.string.as_bytes().to_vec())),
        ))
    } else {
        type_required_error(name, "string", &[args[0]])
    }
}
fn utf8_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "utf8->string";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Bytevector(bv) => match std::str::from_utf8(&bv.data) {
            Ok(s) => Ok(Object::String(vm.gc.alloc(SString::new(s)))),
            Err(err) => {
                generic_error!(name, args, "{}", err)
            }
        },
        _ => type_required_error(name, "bytevector", &[args[0]]),
    }
}
fn null_terminated_bytevector_to_string(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "null-terminated-bytevector->string";
    todo!("{}({}) not implemented", name, args.len());
}
fn null_terminated_utf8_to_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "null-terminated-utf8->string";
    todo!("{}({}) not implemented", name, args.len());
}
fn string_to_utf16(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->utf16";
    check_argc_between!(name, args, 1, 2);
    let is_little = if args.len() == 2 {
        args[1] == vm.gc.symbol_intern("little")
    } else {
        false // Default is Big.
    };
    let s = as_sstring!(name, args, 0);
    let utf16_bytes: Vec<u16> = s.encode_utf16().collect();
    let mut data = Vec::new();
    for u in utf16_bytes {
        if is_little {
            let bytes = u.to_le_bytes();
            data.extend_from_slice(&bytes);
        } else {
            let bytes = u.to_be_bytes();
            data.extend_from_slice(&bytes);
        }
    }
    Ok(Object::Bytevector(vm.gc.alloc(Bytevector::new(&data))))
}
fn string_to_utf32(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string->utf32";
    check_argc_between!(name, args, 1, 2);
    let is_little = if args.len() == 2 {
        args[1] == vm.gc.symbol_intern("little")
    } else {
        false // Default is Big.
    };
    let s = as_sstring!(name, args, 0);
    let chars: Vec<char> = s.chars().collect();
    let mut data = Vec::new();
    for ch in chars.iter() {
        if is_little {
            let bytes = (*ch as u32).to_le_bytes();
            data.extend_from_slice(&bytes);
        } else {
            let bytes = (*ch as u32).to_be_bytes();
            data.extend_from_slice(&bytes);
        }
    }
    Ok(Object::Bytevector(vm.gc.alloc(Bytevector::new(&data))))
}

#[derive(Clone, Debug, PartialEq)]
enum BomType {
    Le,
    Be,
    No,
}

fn utf16_bom_type(bv: &GcRef<Bytevector>) -> BomType {
    if bv.len() >= 2 {
        if bv.data[0] == 0xfe && bv.data[1] == 0xff {
            BomType::Be
        } else if bv.data[0] == 0xff && bv.data[1] == 0xfe {
            BomType::Le
        } else {
            BomType::No
        }
    } else {
        BomType::No
    }
}

fn u8_to_u16(input: &[u8], is_little_endian: bool) -> Vec<u16> {
    let mut output: Vec<u16> = Vec::with_capacity(input.len() / 2);

    for chunk in input.chunks_exact(2) {
        let value = if is_little_endian {
            u16::from_le_bytes([chunk[0], chunk[1]])
        } else {
            u16::from_be_bytes([chunk[0], chunk[1]])
        };
        output.push(value);
    }
    output
}

fn utf16_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "utf16->string";
    check_argc_between!(name, args, 2, 3);
    let bv = as_bytevector!(name, args, 0);
    let mut endianness = BomType::No;
    let mut skip_bom = false;
    if args.len() == 2 {
        endianness = utf16_bom_type(&bv);
        if endianness != BomType::No {
            skip_bom = true;
        }
    }

    let endianness_mandatory = args.len() == 3 && !args[2].is_false();
    if endianness_mandatory || endianness == BomType::No {
        let _endianness_sym = as_symbol!(name, args, 1);
        if args[1] == vm.gc.symbol_intern("little") {
            endianness = BomType::Le;
        } else if args[1] == vm.gc.symbol_intern("big") {
            endianness = BomType::Be;
        } else {
            return error::Error::assertion_violation(
                name,
                "endianness should be little or big",
                &[args[1]],
            );
        }
    }
    let is_little = endianness == BomType::Le;
    let skip_size = if skip_bom { 2 } else { 0 };
    let data = &bv.data[skip_size..];
    let u16_data = u8_to_u16(data, is_little);
    let s = String::from_utf16_lossy(&u16_data);
    Ok(vm.gc.new_string(&s))
}

fn utf32_bom_type(bv: &GcRef<Bytevector>) -> BomType {
    if bv.len() >= 4 {
        if bv.data[0] == 0x00 && bv.data[1] == 0x00 && bv.data[2] == 0xfe && bv.data[3] == 0xff {
            BomType::Be
        } else if bv.data[3] == 0x00
            && bv.data[2] == 0x00
            && bv.data[1] == 0xfe
            && bv.data[0] == 0xff
        {
            BomType::Le
        } else {
            BomType::No
        }
    } else {
        BomType::No
    }
}

fn utf32_to_string(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "utf32->string";
    check_argc_between!(name, args, 2, 3);
    let bv = as_bytevector!(name, args, 0);
    let mut endianness = BomType::No;
    let mut skip_bom = false;
    if args.len() == 2 {
        endianness = utf32_bom_type(&bv);
        if endianness != BomType::No {
            skip_bom = true;
        }
    }

    let endianness_mandatory = args.len() == 3 && !args[2].is_false();
    if endianness_mandatory || endianness == BomType::No {
        let _endianness_sym = as_symbol!(name, args, 1);
        if args[1] == vm.gc.symbol_intern("little") {
            endianness = BomType::Le;
        } else if args[1] == vm.gc.symbol_intern("big") {
            endianness = BomType::Be;
        } else {
            return error::Error::assertion_violation(
                name,
                "endianness should be little or big",
                &[args[1]],
            );
        }
    }
    let is_little = endianness == BomType::Le;
    let skip_size = if skip_bom { 4 } else { 0 };
    let data = &bv.data[skip_size..];
    let mut s = String::new();
    let mut i = 0;
    loop {
        if i + 4 > data.len() {
            break;
        }
        let bytes = &data[i..i + 4];
        let value: u32 = match is_little {
            true => LittleEndian::read_u32(bytes),
            false => BigEndian::read_u32(bytes),
        };
        match char::from_u32(value) {
            Some(ch) => s.push(ch),
            None => {
                return error::Error::assertion_violation(
                    name,
                    &format!("invalid utf32 char {:x}", value),
                    &[args[0]],
                );
            }
        }
        i += 4;
    }
    Ok(vm.gc.new_string(&s))
}
fn close_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "close-port";
    check_argc!(name, args, 1);
    let port = as_port_mut!(name, args, 0);
    port.close();
    Ok(Object::Unspecified)
}
fn make_instruction(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-instruction";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(n) => Ok(Object::Instruction(
            FromPrimitive::from_u8(n as u8).expect("unknown Op"),
        )),
        _ => number_required_error(name, &[args[0]]),
    }
}
fn make_compiler_instruction(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-compiler-instruction";
    todo!("{}({}) not implemented", name, args.len());
}
fn fasl_write(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fasl-write";
    check_argc!(name, args, 2);
    if let Object::BinaryFileOutputPort(mut port) = args[1] {
        let fasl = FaslWriter::new();
        let port = unsafe { port.pointer.as_mut() };
        let bin_port: &mut dyn BinaryOutputPort = port;
        match fasl.write(bin_port, args[0]) {
            Ok(()) => Ok(Object::Unspecified),
            Err(err) => {
                generic_error!(name, args, "{} {} {}", err, args[0], args[1])
            }
        }
    } else {
        type_required_error(name, "file path", &[args[0]])
    }
}
fn fasl_read(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fasl-read";
    check_argc!(name, args, 1);
    if let Object::BinaryFileInputPort(mut port) = args[0] {
        let mut content = Vec::new();
        port.read_to_end(&mut content).ok();
        let mut fasl = FaslReader::new(&content[..]);
        match fasl.read(&mut vm.gc) {
            Ok(sexp) => Ok(sexp),
            Err(err) => {
                return Err(error::Error::new(
                    ErrorType::IoError,
                    name,
                    &format!("{}", err),
                    &[args[0]],
                ))
            }
        }
    } else {
        type_required_error(name, "file path", &[args[0]])
    }
}

fn is_rational(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rational?";
    check_argc!(name, args, 1);
    Ok(args[0].is_rational().to_obj())
}
fn is_flonum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flonum?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_fixnum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fixnum?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn is_bignum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bignum?";
    todo!("{}({}) not implemented", name, args.len());
}
fn fixnum_width(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fixnum-width";
    check_argc!(name, args, 0);
    Ok(Object::Fixnum(isize::BITS as isize))
}
fn least_fixnum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "least-fixnum";
    check_argc!(name, args, 0);
    Ok(Object::Fixnum(isize::MIN))
}
fn greatest_fixnum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "greatest-fixnum";
    check_argc!(name, args, 0);
    Ok(Object::Fixnum(isize::MAX))
}
fn make_rectangular(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-rectangular";
    check_argc!(name, args, 2);
    let n1 = args[0];
    let n2 = args[1];
    if n1.is_real() && n2.is_real() {
        Ok(Object::Compnum(vm.gc.alloc(Compnum::new(n1, n2))))
    } else {
        type_required_error(name, "real numbers", &[n1, n2])
    }
}
fn real_part(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "real-part";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(real(args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn imag_part(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "imag-part";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(imag(args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn is_exact(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "exact?";
    check_argc!(name, args, 1);
    Ok(args[0].is_exact().to_obj())
}
fn is_inexact(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "inexact?";
    check_argc!(name, args, 1);
    Ok((!args[0].is_exact()).to_obj())
}
fn exact(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "exact";
    check_argc!(name, args, 1);
    Ok(numbers::exact(&mut vm.gc, args[0]))
}
fn inexact(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "inexact";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::inexact(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn is_nan(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "nan?";
    check_argc!(name, args, 1);
    Ok((match args[0] {
        Object::Flonum(fl) => fl.is_nan(),
        _ => false,
    })
    .to_obj())
}
fn is_infinite(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "infinite?";
    check_argc!(name, args, 1);
    Ok((match args[0] {
        Object::Flonum(fl) => fl.is_infinite(),
        _ => false,
    })
    .to_obj())
}
fn is_finite(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "finite?";
    check_argc!(name, args, 1);
    Ok((match args[0] {
        Object::Flonum(fl) => fl.is_finite(),
        _ => true,
    })
    .to_obj())
}
fn real_to_flonum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "real->flonum";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_flequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fl1 = as_f64!(name, args, i);
        let fl2 = as_f64!(name, args, i + 1);
        if fl1 == fl2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fllt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl<?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fl1 = as_f64!(name, args, i);
        let fl2 = as_f64!(name, args, i + 1);
        if fl1 < fl2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_flgt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl>?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fl1 = as_f64!(name, args, i);
        let fl2 = as_f64!(name, args, i + 1);
        if fl1 > fl2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_flge(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl>=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fl1 = as_f64!(name, args, i);
        let fl2 = as_f64!(name, args, i + 1);
        if fl1 >= fl2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_flle(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl<=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fl1 = as_f64!(name, args, i);
        let fl2 = as_f64!(name, args, i + 1);
        if fl1 <= fl2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_flinteger(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flinteger?";
    check_argc!(name, args, 1);
    let fl = as_flonum!(name, args, 0);
    Ok(fl.is_integer().to_obj())
}
fn is_flzero(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flzero?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(fl.is_zero().to_obj())
}
fn is_flpositive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flpositive?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok((fl > 0.0).to_obj())
}
fn is_flnegative(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flnegative?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok((fl < 0.0).to_obj())
}
fn is_flodd(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flodd?";
    check_argc!(name, args, 1);
    let fl = as_flonum!(name, args, 0);
    Ok((!fl.is_even()).to_obj())
}
fn is_fleven(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fleven?";
    check_argc!(name, args, 1);
    let fl = as_flonum!(name, args, 0);
    Ok(fl.is_even().to_obj())
}
fn is_flfinite(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flfinite?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(fl.is_finite().to_obj())
}
fn is_flinfinite(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flinfinite?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(fl.is_infinite().to_obj())
}
fn is_flnan(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flnan?";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(fl.is_nan().to_obj())
}
fn flmax(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flmax";
    check_argc_at_least!(name, args, 1);
    let mut max = f64::NEG_INFINITY;
    for i in 0..args.len() {
        let fl = as_f64!(name, args, i);
        if fl.is_nan() {
            return Ok(Object::Flonum(Flonum::new(f64::NAN)));
        }
        if fl > max {
            max = fl;
        }
    }
    Ok(Object::Flonum(Flonum::new(max)))
}
fn flmin(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flmin";
    check_argc_at_least!(name, args, 1);
    let mut min = f64::INFINITY;
    for i in 0..args.len() {
        let fl = as_f64!(name, args, i);
        if fl.is_nan() {
            return Ok(Object::Flonum(Flonum::new(f64::NAN)));
        }
        if fl < min {
            min = fl;
        }
    }
    Ok(Object::Flonum(Flonum::new(min)))
}
fn fladd(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl+";
    let argc = args.len();
    if 0 == argc {
        Ok(Object::Flonum(Flonum::new(0.0)))
    } else if 1 == argc {
        let _ = as_f64!(name, args, 0);
        Ok(args[0])
    } else {
        let mut ret = 0.0;
        for i in 0..args.len() {
            let fl = as_f64!(name, args, i);
            ret += fl;
        }
        Ok(Object::Flonum(Flonum::new(ret)))
    }
}
fn flmul(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl*";
    let argc = args.len();
    if 0 == argc {
        Ok(Object::Flonum(Flonum::new(1.0)))
    } else if 1 == argc {
        let _ = as_f64!(name, args, 0);
        Ok(args[0])
    } else {
        let mut ret = 1.0;
        for i in 0..args.len() {
            let fl = as_f64!(name, args, i);
            ret *= fl;
        }
        Ok(Object::Flonum(Flonum::new(ret)))
    }
}
fn flsub(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl-";
    check_argc_at_least!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    if args.len() == 1 {
        return Ok(args[0].neg(&mut vm.gc));
    }

    let mut ret = fl;
    for i in 1..args.len() {
        let fl = as_f64!(name, args, i);
        ret -= fl;
    }
    Ok(Object::Flonum(Flonum::new(ret)))
}
fn fldiv_op(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fl/";
    check_argc_at_least!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    if args.len() == 1 {
        match numbers::div(&mut vm.gc, Object::Flonum(Flonum::new(1.0)), args[0]) {
            Ok(v) => return Ok(v),
            Err(_) => {
                return Error::assertion_violation(
                    name,
                    "division by zero",
                    &[args[0], args[1], args[2], args[3]],
                );
            }
        }
    }

    let mut ret = fl;
    for i in 1..args.len() {
        let fl = as_f64!(name, args, i);
        ret /= fl;
    }
    Ok(Object::Flonum(Flonum::new(ret)))
}
fn flabs(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flabs";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.abs())))
}
fn fldiv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fldiv";
    check_argc!(name, args, 2);
    let fl1 = as_flonum!(name, args, 0);
    let fl2 = as_flonum!(name, args, 1);
    Ok(Object::Flonum(fl1.integer_div(&fl2)))
}
fn flmod(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flmod";
    check_argc!(name, args, 2);
    let fl1 = as_flonum!(name, args, 0);
    let fl2 = as_flonum!(name, args, 1);
    Ok(Object::Flonum(fl1.integer_mod(&fl2)))
}
fn fldiv0(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fldiv0";
    check_argc!(name, args, 2);
    let fl1 = as_flonum!(name, args, 0);
    let fl2 = as_flonum!(name, args, 1);
    Ok(Object::Flonum(fl1.integer_div0(&fl2)))
}
fn flmod0(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flmod0";
    check_argc!(name, args, 2);
    let fl1 = as_flonum!(name, args, 0);
    let fl2 = as_flonum!(name, args, 1);
    Ok(Object::Flonum(fl1.integer_mod0(&fl2)))
}
fn flnumerator(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flnumerator";
    check_argc!(name, args, 1);
    let fl = as_flonum!(name, args, 0);
    Ok(fl.numerator(&mut vm.gc))
}
fn fldenominator(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fldenominator";
    check_argc!(name, args, 1);
    let fl = as_flonum!(name, args, 0);
    Ok(fl.denominator(&mut vm.gc))
}
fn flfloor(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flfloor";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.floor())))
}
fn flceiling(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flceiling";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.ceil())))
}
fn fltruncate(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fltruncate";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.trunc())))
}
fn flround(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flround";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.round())))
}
fn flexp(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flexp";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.exp())))
}
fn fllog(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fllog";
    check_argc_between!(name, args, 1, 2);
    let fl1 = as_f64!(name, args, 0);
    if args.len() == 1 {
        Ok(Object::Flonum(Flonum::new(fl1.ln())))
    } else {
        let fl2 = as_f64!(name, args, 1);
        Ok(Object::Flonum(Flonum::new(fl1.log(fl2))))
    }
}
fn flsin(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flsin";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.sin())))
}
fn flcos(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flcos";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.cos())))
}
fn fltan(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fltan";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.tan())))
}
fn flasin(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flasin";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.asin())))
}
fn flacos(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flacos";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.acos())))
}
fn flatan(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flatan";
    check_argc_between!(name, args, 1, 2);
    let fl1 = as_f64!(name, args, 0);
    if args.len() == 1 {
        Ok(Object::Flonum(Flonum::new(fl1.atan())))
    } else {
        let fl2 = as_f64!(name, args, 1);
        Ok(Object::Flonum(Flonum::new(fl1.atan2(fl2))))
    }
}
fn flsqrt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flsqrt";
    check_argc!(name, args, 1);
    let fl = as_f64!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fl.sqrt())))
}
fn flexpt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "flexpt";
    check_argc!(name, args, 2);
    let fl1 = as_f64!(name, args, 0);
    let fl2 = as_f64!(name, args, 1);
    Ok(Object::Flonum(Flonum::new(fl1.powf(fl2))))
}
fn fixnum_to_flonum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fixnum->flonum";
    let fx1 = as_isize!(name, args, 0);
    Ok(Object::Flonum(Flonum::new(fx1.to_f64().unwrap())))
}
fn bitwise_not(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-not";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(fx) => Ok(Object::Fixnum(!fx)),
        Object::Bignum(b) => Ok((!b.value.clone()).to_obj(&mut vm.gc)),
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_and(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-and";
    if args.is_empty() {
        Ok(Object::Fixnum(-1))
    } else if args.len() == 1 {
        Ok(args[0])
    } else {
        let mut accum = match args[0] {
            Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
            Object::Bignum(b) => b.value.clone(),
            _ => {
                return Error::assertion_violation(name, "exact integer required", &[args[0]]);
            }
        };
        for i in 1..args.len() {
            let v = match args[i] {
                Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
                Object::Bignum(b) => b.value.clone(),
                _ => {
                    return Error::assertion_violation(name, "exact integer required", &[args[0]]);
                }
            };
            accum &= v;
        }
        Ok(accum.to_obj(&mut vm.gc))
    }
}
fn bitwise_ior(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-ior";
    if args.is_empty() {
        Ok(Object::Fixnum(-1))
    } else if args.len() == 1 {
        Ok(args[0])
    } else {
        let mut accum = match args[0] {
            Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
            Object::Bignum(b) => b.value.clone(),
            _ => {
                return Error::assertion_violation(name, "exact integer required", &[args[0]]);
            }
        };
        for i in 1..args.len() {
            let v = match args[i] {
                Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
                Object::Bignum(b) => b.value.clone(),
                _ => {
                    return Error::assertion_violation(name, "exact integer required", &[args[0]]);
                }
            };
            accum |= v;
        }
        Ok(accum.to_obj(&mut vm.gc))
    }
}
fn bitwise_xor(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-xor";
    if args.is_empty() {
        Ok(Object::Fixnum(0))
    } else if args.len() == 1 {
        Ok(args[0])
    } else {
        let mut accum = match args[0] {
            Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
            Object::Bignum(b) => b.value.clone(),
            _ => {
                return Error::assertion_violation(name, "exact integer required", &[args[0]]);
            }
        };
        for i in 1..args.len() {
            let v = match args[i] {
                Object::Fixnum(fx) => BigInt::from_isize(fx).unwrap(),
                Object::Bignum(b) => b.value.clone(),
                _ => {
                    return Error::assertion_violation(name, "exact integer required", &[args[0]]);
                }
            };
            accum ^= v;
        }
        Ok(accum.to_obj(&mut vm.gc))
    }
}

fn bigint_count_ones(b: &BigInt) -> usize {
    let mut num: usize = 0;
    let mut value = b.clone();
    let one = BigInt::from_isize(1).unwrap();
    while !value.is_zero() {
        if value.clone() & one.clone() == one {
            num += 1;
        }
        value >>= 1;
    }
    num
}

fn bitwise_bit_count(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-bit-count";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(fx) => {
            if fx >= 0 {
                Ok(Object::Fixnum(fx.count_ones() as isize))
            } else {
                let v = (!fx).count_ones() as isize;
                Ok(Object::Fixnum(!v))
            }
        }
        Object::Bignum(b) => {
            if b.value >= BigInt::from_isize(0).unwrap() {
                Ok(Object::Fixnum(bigint_count_ones(&b.value) as isize))
            } else {
                let v = bigint_count_ones(&(!b.value.clone())) as isize;
                Ok(Object::Fixnum(!v))
            }
        }
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_length(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-length";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(fx) => {
            let size_in_bytes = mem::size_of::<isize>();
            let size_in_bits = size_in_bytes * 8;
            let fx = if fx < 0 { !fx } else { fx };
            Ok(Object::Fixnum(
                (size_in_bits - fx.leading_zeros() as usize) as isize,
            ))
        }
        Object::Bignum(b) => {
            let b = if b.value < BigInt::from_isize(0).unwrap() {
                !b.value.clone()
            } else {
                b.value.clone()
            };
            Ok(Object::Fixnum((b.bits()) as isize))
        }
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_first_bit_set(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-first-bit-set";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Fixnum(fx) => {
            if fx == 0 {
                Ok(Object::Fixnum(-1))
            } else {
                let idx = fx.trailing_zeros();
                Ok(Object::Fixnum(idx as isize))
            }
        }
        Object::Bignum(b) => match b.trailing_zeros() {
            Some(idx) => Ok(Object::Fixnum(idx as isize)),
            None => Ok(Object::Fixnum(-1)),
        },
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_arithmetic_shift_left(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-arithmetic-shift-left";
    check_argc!(name, args, 2);
    let offset = as_isize!(name, args, 1);
    if offset < 0 {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    match args[0] {
        Object::Fixnum(fx) => {
            let b = BigInt::from_isize(fx).unwrap();
            let shifted = b << offset;
            Ok(shifted.to_obj(&mut vm.gc))
        }
        Object::Bignum(b) => {
            let shifted = b.value.clone() << offset;
            Ok(shifted.to_obj(&mut vm.gc))
        }
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_arithmetic_shift_right(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-arithmetic-shift-right";
    check_argc!(name, args, 2);
    let offset = as_isize!(name, args, 1);
    if offset < 0 {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    match args[0] {
        Object::Fixnum(fx) => {
            let b = BigInt::from_isize(fx).unwrap();
            let shifted = b >> offset;
            Ok(shifted.to_obj(&mut vm.gc))
        }
        Object::Bignum(b) => {
            let shifted = b.value.clone() >> offset;
            Ok(shifted.to_obj(&mut vm.gc))
        }
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn bitwise_arithmetic_shift(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bitwise-arithmetic-shift";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::Bignum(b), Object::Fixnum(fx)) => {
            if fx >= 0 {
                let shifted = b.value.clone() << fx;
                Ok(shifted.to_obj(&mut vm.gc))
            } else {
                let shifted = b.value.clone() >> (-fx);
                Ok(shifted.to_obj(&mut vm.gc))
            }
        }
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => {
            let b = BigInt::from_isize(fx1).unwrap();
            if fx2 >= 0 {
                let shifted = b << fx2;
                Ok(shifted.to_obj(&mut vm.gc))
            } else {
                let shifted = b >> (-fx2);
                Ok(shifted.to_obj(&mut vm.gc))
            }
        }
        _ => Error::assertion_violation(name, "exact integer required", &[args[0]]),
    }
}
fn is_complex(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "complex?";
    check_argc!(name, args, 1);
    Ok(args[0].is_complex().to_obj())
}
fn is_real(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "real?";
    check_argc!(name, args, 1);
    Ok(args[0].is_real().to_obj())
}

fn is_integer(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "integer?";
    check_argc!(name, args, 1);
    Ok(args[0].is_integer(&mut vm.gc).to_obj())
}
fn is_real_valued(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "real-valued?";
    check_argc!(name, args, 1);
    let n = args[0];
    if n.is_number() && n.is_real_valued() {
        Ok(Object::True)
    } else {
        Ok(Object::False)
    }
}
fn is_rational_valued(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rational-valued?";
    check_argc!(name, args, 1);
    let n = args[0];
    if n.is_number() && n.is_rational_valued() {
        Ok(Object::True)
    } else {
        Ok(Object::False)
    }
}
fn is_integer_valued(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "integer-valued?";
    check_argc!(name, args, 1);

    if args[0].is_number() {
        Ok(args[0].is_integer_valued(&mut vm.gc).to_obj())
    } else {
        Ok(Object::False)
    }
}
fn is_fxequal(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fx1 = as_isize!(name, args, i);
        let fx2 = as_isize!(name, args, i + 1);
        if fx1 == fx2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fxgt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx>?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fx1 = as_isize!(name, args, i);
        let fx2 = as_isize!(name, args, i + 1);
        if fx1 > fx2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fxlt(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx<?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fx1 = as_isize!(name, args, i);
        let fx2 = as_isize!(name, args, i + 1);
        if fx1 < fx2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fxge(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx>=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fx1 = as_isize!(name, args, i);
        let fx2 = as_isize!(name, args, i + 1);
        if fx1 >= fx2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fxle(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx<=?";
    check_argc_at_least!(name, args, 2);
    for i in 0..args.len() - 1 {
        let fx1 = as_isize!(name, args, i);
        let fx2 = as_isize!(name, args, i + 1);
        if fx1 <= fx2 {
            continue;
        } else {
            return Ok(Object::False);
        }
    }
    Ok(Object::True)
}
fn is_fxzero(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxzero?";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok((fx == 0).to_obj())
}
fn is_fxpositive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxpositive?";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok((fx > 0).to_obj())
}
fn is_fxnegative(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxnegative?";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok((fx < 0).to_obj())
}
fn is_fxodd(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxodd?";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok((fx % 2 != 0).to_obj())
}
fn is_fxeven(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxeven?";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok((fx % 2 == 0).to_obj())
}
fn fxmax(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxmax";
    check_argc_at_least!(name, args, 1);
    let mut max = isize::MIN;
    for i in 0..args.len() {
        let fx = as_isize!(name, args, i);
        if fx > max {
            max = fx
        }
    }
    Ok(Object::Fixnum(max))
}
fn fxmin(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxmin";
    check_argc_at_least!(name, args, 1);
    let mut min = isize::MAX;
    for i in 0..args.len() {
        let fx = as_isize!(name, args, i);
        if fx < min {
            min = fx
        }
    }
    Ok(Object::Fixnum(min))
}
fn fxadd(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx+";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.checked_add(fx2) {
        Some(v) => Ok(Object::Fixnum(v)),
        None => Error::implementation_restriction_violation(
            name,
            "result is not fixnum",
            &[args[0], args[1]],
        ),
    }
}
fn fxmul(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx*";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.checked_mul(fx2) {
        Some(v) => Ok(Object::Fixnum(v)),
        None => Error::implementation_restriction_violation(
            name,
            "result is not fixnum",
            &[args[0], args[1]],
        ),
    }
}
fn fxsub(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fx-";
    check_argc_between!(name, args, 1, 2);
    let fx1 = as_isize!(name, args, 0);
    if args.len() == 1 {
        match fx1.checked_neg() {
            Some(v) => Ok(Object::Fixnum(v)),
            None => Error::implementation_restriction_violation(
                name,
                "result is not fixnum",
                &[args[0]],
            ),
        }
    } else {
        let fx2 = as_isize!(name, args, 1);
        match fx1.checked_sub(fx2) {
            Some(v) => Ok(Object::Fixnum(v)),
            None => Error::implementation_restriction_violation(
                name,
                "result is not fixnum",
                &[args[0], args[1]],
            ),
        }
    }
}
fn fxdiv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxdiv";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.integer_div(fx2) {
        Ok(v) => Ok(Object::Fixnum(v)),
        Err(SchemeError::Overflow) => Error::implementation_restriction_violation(
            name,
            "result is not fixnum",
            &[args[0], args[1]],
        ),
        _ => Error::assertion_violation(name, "result is not fixnum", &[args[0], args[1]]),
    }
}
fn fxmod(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxmod";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.modulo(fx2) {
        Ok(v) => Ok(Object::Fixnum(v)),
        Err(e) => Error::assertion_violation(name, &format!("{:?}", e), &[args[0], args[1]]),
    }
}
fn fxdiv0(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxdiv0";
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.div0(fx2) {
        Ok(v) => Ok(Object::Fixnum(v)),
        Err(SchemeError::Overflow) => Error::implementation_restriction_violation(
            name,
            "result is not fixnum",
            &[args[0], args[1]],
        ),
        _ => Error::assertion_violation(name, "result is not fixnum", &[args[0], args[1]]),
    }
}
fn fxmod0(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxmod0";
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    match fx1.modulo0(fx2) {
        Ok(v) => Ok(Object::Fixnum(v)),
        Err(_) => Error::assertion_violation(name, "result is not fixnum", &[args[0], args[1]]),
    }
}
fn fxnot(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxnot";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok(Object::Fixnum(!fx))
}
fn fxand(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxand";
    check_argc_at_least!(name, args, 1);
    let mut ret = -1;
    for i in 0..args.len() {
        let fx = as_isize!(name, args, i);
        ret &= fx;
    }
    Ok(Object::Fixnum(ret))
}
fn fxior(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxior";
    check_argc_at_least!(name, args, 1);
    let mut ret = 0;
    for i in 0..args.len() {
        let fx = as_isize!(name, args, i);
        ret |= fx;
    }
    Ok(Object::Fixnum(ret))
}
fn fxxor(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxxor";
    check_argc_at_least!(name, args, 1);
    let mut ret = 0;
    for i in 0..args.len() {
        let fx = as_isize!(name, args, i);
        ret ^= fx;
    }
    Ok(Object::Fixnum(ret))
}
fn fxif(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxif";
    check_argc!(name, args, 3);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    let fx3 = as_isize!(name, args, 2);
    Ok(Object::Fixnum(isize::fxif(fx1, fx2, fx3)))
}
fn fxbit_count(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxbit-count";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok(Object::Fixnum(fx.bit_count()))
}
fn fxlength(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxlength";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    Ok(Object::Fixnum(fx.length() as isize))
}
fn fxfirst_bit_set(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxfirst-bit-set";
    check_argc!(name, args, 1);
    let fx = as_isize!(name, args, 0);
    if fx == 0 {
        Ok(Object::Fixnum(-1))
    } else {
        Ok(Object::Fixnum(fx.trailing_zeros() as isize))
    }
}
fn is_fxbit_set(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxbit-set?";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    if fx2 > (isize::BITS as isize) || fx2 < 0 {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    Ok((((fx1 >> fx2) & 1) == 1).to_obj())
}
fn fxcopy_bit(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxcopy-bit";
    check_argc!(name, args, 3);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    let fx3 = as_isize!(name, args, 2);

    if (fx2 < 0 || fx2 >= (isize::BITS as isize)) || (fx3 != 0 && fx3 != 1) {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    let mask = 1 << fx2;
    Ok(Object::Fixnum(isize::fxif(mask, fx3 << fx2, fx1)))
}
fn fxbit_field(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxbit-field";
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    let fx3 = as_isize!(name, args, 2);

    if (fx2 < 0 || fx2 >= (isize::BITS as isize))
        || (fx3 < 0 || fx3 >= (isize::BITS as isize))
        || fx2 > fx3
    {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    Ok(Object::Fixnum(isize::fxbitfield(fx1, fx2, fx3)))
}
fn fxcopy_bit_field(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxcopy-bit-field";
    check_argc!(name, args, 4);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    let fx3 = as_isize!(name, args, 2);
    let fx4 = as_isize!(name, args, 3);

    if (fx2 < 0 || fx2 >= (isize::BITS as isize))
        || (fx3 < 0 || fx3 >= (isize::BITS as isize))
        || fx2 > fx3
    {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }
    Ok(Object::Fixnum(isize::fxbit_copy_bitfield(
        fx1, fx2, fx3, fx4,
    )))
}
fn fxarithmetic_shift(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxarithmetic-shift";
    check_argc!(name, args, 2);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    if fx2.abs() >= (isize::BITS) as isize {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }

    if fx2 >= 0 {
        // Check overflow.
        let x = if fx1 < 0 { !fx1 } else { fx1 };
        if (x >> (mem::size_of_val(&x) as isize * 8 - 1 - fx2)) != 0 {
            return Error::implementation_restriction_violation(
                name,
                "result is not fixnum1",
                &[args[0], args[1]],
            );
        }
        match fx1.checked_shl(fx2 as u32) {
            Some(v) => Ok(Object::Fixnum(v)),
            None => Error::assertion_violation(name, "result is not fixnum", &[args[0], args[1]]),
        }
    } else {
        match fx1.checked_shr((-fx2) as u32) {
            Some(v) => Ok(Object::Fixnum(v)),
            None => Error::assertion_violation(name, "result is not fixnum", &[args[0], args[1]]),
        }
    }
}

fn fxarithmetic_shift_left(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxarithmetic-shift-left";
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    if fx2 < 0 || fx2.abs() >= (isize::BITS) as isize {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }

    // Check overflow.
    let x = if fx1 < 0 { !fx1 } else { fx1 };
    if (x >> (mem::size_of_val(&x) as isize * 8 - 1 - fx2)) != 0 {
        return Error::implementation_restriction_violation(
            name,
            "result is not fixnum1",
            &[args[0], args[1]],
        );
    }

    match fx1.checked_shl(fx2 as u32) {
        Some(v) => Ok(Object::Fixnum(v)),
        None => Error::implementation_restriction_violation(
            name,
            "result is not fixnum2",
            &[args[0], args[1]],
        ),
    }
}

fn fxarithmetic_shift_right(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxarithmetic-shift-right";
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    if fx2 < 0 || fx2.abs() >= (isize::BITS) as isize {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1]]);
    }

    match fx1.checked_shr(fx2 as u32) {
        Some(v) => Ok(Object::Fixnum(v)),
        None => Error::implementation_restriction_violation(
            name,
            "result is not fixnum",
            &[args[0], args[1]],
        ),
    }
}
fn fxrotate_bit_field(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxrotate-bit-field";
    check_argc!(name, args, 4);
    let fx1 = as_isize!(name, args, 0);
    let fx2 = as_isize!(name, args, 1);
    let fx3 = as_isize!(name, args, 2);
    let fx4 = as_isize!(name, args, 3);

    if (fx2 < 0 || fx2.abs() >= (isize::BITS) as isize)
        || (fx3 < 0 || fx3.abs() >= (isize::BITS) as isize)
        || (fx4 < 0 || fx4.abs() >= (isize::BITS) as isize)
        || fx2 > fx3
        || fx4 >= (fx3 - fx2)
    {
        return Error::implementation_restriction_violation(
            name,
            "out of range",
            &[args[0], args[1], args[2], args[3]],
        );
    }

    let width = fx3 - fx2;
    if width > 0 {
        let count = match fx4.modulo(width) {
            Ok(v) => v,
            Err(_) => {
                return Error::assertion_violation(
                    name,
                    "division by zero",
                    &[args[0], args[1], args[2], args[3]],
                );
            }
        };
        let field0 = isize::fxbitfield(fx1, fx2, fx3);
        let field1 = field0 << fx4;
        let field2 = field0 >> (width - count);
        let field = field1 | field2;
        Ok(Object::Fixnum(isize::fxbit_copy_bitfield(
            fx1, fx2, fx3, field,
        )))
    } else {
        Ok(Object::Fixnum(fx1))
    }
}
fn fxreverse_bit_field(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fxreverse-bit-field";
    let mut bits = as_usize!(name, args, 0);
    let mut start = as_isize!(name, args, 1);
    let mut end = as_isize!(name, args, 2);
    if start > (isize::BITS as isize) || start < 0 || end > (isize::BITS as isize) || end < 0 {
        return Error::assertion_violation(name, "out of range", &[args[0], args[1], args[2]]);
    }
    end -= 1;

    while start < end {
        let sbit = (bits >> start) & 1;
        let ebit = (bits >> end) & 1;

        bits &= usize::MAX - (1 << end);
        bits |= sbit << end;
        bits &= usize::MAX - (1 << start);
        bits |= ebit << start;

        start += 1;
        end -= 1;
    }
    Ok(Object::Fixnum(bits as isize))
}
fn bytevector_ieee_single_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-single-native-ref";
    check_argc!(name, args, 2);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 4 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let ret = if cfg!(target_endian = "big") {
        bv.ref_f32_big(index)
    } else {
        bv.ref_f32_little(index)
    };
    match ret {
        Some(v) => Ok(Object::Flonum(Flonum::new(v as f64))),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_single_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-single-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_f32_little(index)
    } else {
        bv.ref_f32_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Flonum(Flonum::new(v as f64))),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_double_native_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-double-native-ref";
    check_argc!(name, args, 2);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 8 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let ret = if cfg!(target_endian = "big") {
        bv.ref_f64_big(index)
    } else {
        bv.ref_f64_little(index)
    };
    match ret {
        Some(v) => Ok(Object::Flonum(Flonum::new(v))),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_double_ref(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-double-ref";
    check_argc!(name, args, 3);
    let bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let _endianness = as_symbol!(name, args, 2);
    let ret = if args[2] == vm.gc.symbol_intern("little") {
        bv.ref_f64_little(index)
    } else {
        bv.ref_f64_big(index)
    };
    match ret {
        Some(v) => Ok(Object::Flonum(Flonum::new(v))),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_single_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-single-native-set!";
    check_argc!(name, args, 3);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 4 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let value = as_f32!(name, args, 2);
    let ret = if cfg!(target_endian = "big") {
        bv.set_f32_big(index, value)
    } else {
        bv.set_f32_little(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_single_set_destructive(
    vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-single-set!";
    check_argc!(name, args, 4);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let value = as_f32!(name, args, 2);
    let _endianness = as_symbol!(name, args, 3);
    let ret = if args[3] == vm.gc.symbol_intern("little") {
        bv.set_f32_little(index, value)
    } else {
        bv.set_f32_big(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_double_native_set_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-double-native-set!";
    check_argc!(name, args, 3);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    if index % 8 != 0 {
        return error::Error::assertion_violation(name, "index not aligned", &[args[1]]);
    }
    let value = as_f64!(name, args, 2);
    let ret = if cfg!(target_endian = "big") {
        bv.set_f64_big(index, value)
    } else {
        bv.set_f64_little(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn bytevector_ieee_double_set_destructive(
    vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "bytevector-ieee-double-set!";
    check_argc!(name, args, 4);
    let mut bv = as_bytevector!(name, args, 0);
    let index = as_usize!(name, args, 1);
    let value = as_f64!(name, args, 2);
    let _endianness = as_symbol!(name, args, 3);
    let ret = if args[3] == vm.gc.symbol_intern("little") {
        bv.set_f64_little(index, value)
    } else {
        bv.set_f64_big(index, value)
    };
    match ret {
        Some(_) => Ok(Object::Unspecified),
        None => error::Error::assertion_violation(name, "index out of range", &[args[1]]),
    }
}
fn is_even(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "even?";
    check_argc!(name, args, 1);
    if args[0].is_integer(&mut vm.gc) {
        Ok(args[0].is_even().to_obj())
    } else {
        type_required_error(name, "integer value", &[args[0]])
    }
}
fn is_odd(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "odd?";
    check_argc!(name, args, 1);
    if args[0].is_integer(&mut vm.gc) {
        Ok((!args[0].is_even()).to_obj())
    } else {
        type_required_error(name, "integer value", &[args[0]])
    }
}
fn abs(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "abs";
    check_argc!(name, args, 1);
    if args[0].is_real() {
        Ok(numbers::abs(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "real number", &[args[0]])
    }
}
fn div(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "div";
    check_argc!(name, args, 2);
    let n1 = args[0];
    let n2 = args[1];
    if n1.is_real() && n2.is_real() {
        match integer_div(&mut vm.gc, n1, n2) {
            Ok(v) => Ok(v),
            Err(SchemeError::Div0) => {
                Error::assertion_violation(name, "division by zero", &[n1, n2])
            }
            Err(SchemeError::NanOrInfinite) => {
                Error::assertion_violation(name, "nan.0 or inf.0 not allowed", &[n1, n2])
            }
            _ => bug!(),
        }
    } else {
        type_required_error(name, "real numbers", &[n1, n2])
    }
}
fn div0(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "div0";
    check_argc!(name, args, 2);
    let n1 = args[0];
    let n2 = args[1];
    if !n1.is_real() || !n2.is_real() {
        return Error::assertion_violation(name, "real numbers required", &[n1, n2]);
    }
    let d = div(vm, &mut [n1, n2])?;
    let d2 = numbers::mul(&mut vm.gc, d, n2);
    let modulo = numbers::sub(&mut vm.gc, n1, d2);
    // We know div zero won't happen here.
    let d3 = numbers::div(&mut vm.gc, n2, Object::Fixnum(2)).unwrap();
    if numbers::lt(modulo, numbers::abs(&mut vm.gc, d3)) {
        Ok(d)
    } else if n2.is_negative() {
        Ok(numbers::sub(&mut vm.gc, d, Object::Fixnum(1)))
    } else {
        Ok(numbers::add(&mut vm.gc, d, Object::Fixnum(1)))
    }
}

/*
   Object div = integerDivEx(theVM, argc, argv);
   if (div.isUndef()) {
       return Object::Undef;
   }
   Object mod = Arithmetic::sub(n1, Arithmetic::mul(div, n2));
   // we can ignore isDiv0Error parameter of Arithmetic::div.
   // Because we know division by zero never occur.
   bool isDiv0Error = false;
   if (Arithmetic::lt(mod, Arithmetic::abs(Arithmetic::div(n2, Object::makeFixnum(2), isDiv0Error)))) {
       return div;
   } else {
       if (Arithmetic::isNegative(n2)) {
           return Arithmetic::sub(div, Object::makeFixnum(1));
       } else {
           return Arithmetic::add(div, Object::makeFixnum(1));
       }
   }

*/

fn numerator(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "numerator";
    check_argc!(name, args, 1);
    if args[0].is_rational() {
        Ok(numbers::numerator(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "rational number", &[args[0]])
    }
}
fn denominator(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "denominator";
    check_argc!(name, args, 1);
    if args[0].is_rational() {
        Ok(numbers::denominator(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "rational number", &[args[0]])
    }
}
fn floor(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "floor";
    check_argc!(name, args, 1);
    if args[0].is_real() {
        Ok(numbers::floor(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "real number", &[args[0]])
    }
}
fn ceiling(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "ceiling";
    if args[0].is_real() {
        Ok(numbers::ceiling(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "real number", &[args[0]])
    }
}
fn truncate(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "truncate";
    check_argc!(name, args, 1);
    if !args[0].is_real() {
        return type_required_error(name, "real number", &[args[0]]);
    }
    Ok(numbers::truncate(&mut vm.gc, args[0]))
}
fn round(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "round";
    if args[0].is_real() {
        Ok(numbers::round(&mut vm.gc, args[0]))
    } else {
        type_required_error(name, "real number", &[args[0]])
    }
}

fn exp(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "exp";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::exp(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn log(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "log";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    if argc == 1 {
        let n = args[0];
        if !n.is_number() {
            return number_required_error(name, &[n]);
        }
        if n.is_exact_zero() {
            Error::assertion_violation(name, " nonzero required but got", &[n])
        } else {
            Ok(numbers::log(&mut vm.gc, n))
        }
    } else {
        let n1 = args[0];
        let n2 = args[1];
        if n1.is_exact_zero() && n2.is_exact_zero() {
            return type_required_error(name, "nonzero", args);
        }
        match log2(&mut vm.gc, n1, n2) {
            Ok(ret) => Ok(ret),
            Err(SchemeError::Div0) => {
                generic_error!(name, args, "div by zero {} {}", n1, n2)
            }
            _ => bug!(),
        }
    }
}

fn sin(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sin";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::sin(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn cos(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "cos";
    if args[0].is_number() {
        Ok(numbers::cos(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn tan(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "tan";
    if args[0].is_number() {
        match numbers::tan(&mut vm.gc, args[0]) {
            Ok(v) => Ok(v),
            Err(SchemeError::Div0) => {
                generic_error!(name, args, "div by zero {}", args[0])
            }
            Err(_) => {
                bug!()
            }
        }
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn asin(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "asin";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::asin(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn acos(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "acos";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::acos(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn sqrt(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sqrt";
    check_argc!(name, args, 1);
    Ok(numbers::sqrt(&mut vm.gc, args[0]))
}
fn magnitude(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "magnitude";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::magnitude(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn angle(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "angle";
    check_argc!(name, args, 1);
    if args[0].is_number() {
        Ok(numbers::angle(&mut vm.gc, args[0]))
    } else {
        number_required_error(name, &[args[0]])
    }
}
fn atan(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "atan";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    if argc == 1 {
        let n = args[0];
        match numbers::atan(&mut vm.gc, n) {
            Ok(v) => Ok(v),
            Err(SchemeError::Div0) => {
                generic_error!(name, args, "div by zero {}", n)
            }
            _ => bug!(),
        }
    } else {
        let n1 = args[0];
        let n2 = args[1];
        if n1.is_real() && n2.is_real() {
            Ok(numbers::atan2(&mut vm.gc, n1, n2))
        } else {
            type_required_error(name, "real numbers", &[n1, n2])
        }
    }
}

fn expt(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "expt";
    check_argc!(name, args, 2);
    let n1 = args[0];
    let n2 = args[1];
    if n1.is_number() && n2.is_number() {
        if n2.is_bignum() {
            return generic_error!(name, args, "number ({}, {}) too big", n1, n2);
        }
        Ok(numbers::expt(&mut vm.gc, n1, n2))
    } else {
        type_required_error(name, "numbers", &[n1, n2])
    }
}

fn make_polar(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-polar";
    check_argc!(name, args, 2);
    let n1 = args[0];
    let n2 = args[1];
    if n1.is_number() && n2.is_number() {
        Ok(numbers::make_polar(&mut vm.gc, n1, n2))
    } else {
        type_required_error(name, "numbers", &[n1, n2])
    }
}
fn string_copy(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "string-copy";
    check_argc_between!(name, args, 1, 3);
    let argc = args.len();
    if argc == 1 {
        match args[0] {
            Object::String(s) => Ok(Object::String(vm.gc.alloc(SString::new(&s.string)))),
            _ => type_required_error(name, "string", &[args[0]]),
        }
    } else {
        match args[0] {
            Object::String(s) => {
                let start = args[1];
                if !start.is_fixnum() {
                    return number_required_error(name, &[args[1]]);
                }
                let start = start.to_isize();
                let len = s.string.len() as isize;
                if start < 0 || start > len {
                    return generic_error!(name, args, "start out of range {}", args[1]);
                }
                if argc == 2 {
                    let start = start as usize;
                    let end = len as usize;
                    Ok(Object::String(
                        vm.gc.alloc(SString::new(&s.string[start..end])),
                    ))
                } else {
                    let end = args[2];
                    if !end.is_fixnum() {
                        return number_required_error(name, &[args[2]]);
                    }
                    let end = end.to_isize();
                    if end < 0 || start > end || end > len {
                        return generic_error!(name, args, "end out of range {}", args[1]);
                    }
                    let start = start as usize;
                    let end = end as usize;
                    Ok(Object::String(
                        vm.gc.alloc(SString::new(&s.string[start..end])),
                    ))
                }
            }
            _ => type_required_error(name, "string", &[args[0]]),
        }
    }
}
fn vector_fill_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector-fill!";
    check_argc!(name, args, 2);
    match args[0] {
        Object::Vector(mut v) => v.fill(args[1]),
        _ => {
            return type_required_error(name, "vector", &[args[0]]);
        }
    }
    Ok(Object::Unspecified)
}
fn ungensym(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "ungensym";
    check_argc!(name, args, 1);
    if let Object::Symbol(sym) = args[0] {
        let splitted: Vec<String> = sym.string.split('@').map(|s| s.to_string()).collect();
        if splitted.len() == 2 {
            Ok(vm.gc.new_string(&splitted[1]))
        } else {
            Ok(args[0])
        }
    } else {
        type_required_error(name, "symbol", &[args[0]])
    }
}
fn disasm(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "disasm";
    todo!("{}({}) not implemented", name, args.len());
}
fn print_stack(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "print-stack";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_fast_equal(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "fast-equal?";
    check_argc!(name, args, 2);
    let e = Equal::new();
    Ok(e.is_equal(&mut vm.gc, &args[0], &args[1]).to_obj())
}
fn native_eol_style(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "native-eol-style";
    check_argc!(name, args, 0);
    Ok(if std::env::consts::OS == "windows" {
        vm.gc.symbol_intern("crlf")
    } else {
        vm.gc.symbol_intern("lf")
    })
}
fn is_buffer_mode(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "buffer-mode?";
    check_argc!(name, args, 1);
    let _mode = as_symbol!(name, args, 0);
    Ok((args[0] == vm.gc.symbol_intern("none")
        || args[0] == vm.gc.symbol_intern("line")
        || args[0] == vm.gc.symbol_intern("block"))
    .to_obj())
}
fn microseconds(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "microseconds";
    check_argc!(name, args, 0);
    let now = SystemTime::now();

    let since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
    let microseconds = since_epoch.subsec_micros();
    Ok(Object::Fixnum(microseconds as isize))
}
fn local_tz_offset(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "local-tz-offset";
    todo!("{}({}) not implemented", name, args.len());
}
fn fork(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%fork";
    todo!("{}({}) not implemented", name, args.len());
}
fn exec(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%exec";
    todo!("{}({}) not implemented", name, args.len());
}
fn waitpid(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%waitpid";
    todo!("{}({}) not implemented", name, args.len());
}
fn pipe(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%pipe";
    todo!("{}({}) not implemented", name, args.len());
}
fn getpid(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%getpid";
    todo!("{}({}) not implemented", name, args.len());
}
fn set_current_directory_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-current-directory!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_binary_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "binary-port?";
    check_argc!(name, args, 1);
    Ok(args[0].is_binary_port().to_obj())
}
fn is_input_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "input-port?";
    check_argc!(name, args, 1);
    Ok(args[0].is_input_port().to_obj())
}
fn is_port_eof(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port-eof?";
    check_argc!(name, args, 1);
    if args[0].is_textual_input_port() {
        let port = as_text_input_port_mut!(name, args, 0);
        if !port.is_open() {
            todo!()
        }
        Ok((port.lookahead_char(vm) == Ok(None)).to_obj())
    } else if args[0].is_binary_input_port() {
        let port = as_binary_input_port_mut!(name, args, 0);
        if !port.is_open() {
            todo!()
        }
        Ok((port.lookahead_u8(vm) == Ok(None)).to_obj())
    } else {
        bug!()
    }
}
fn lookahead_u8(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "lookahead-u8";
    check_argc!(name, args, 1);
    let port = as_binary_input_port_mut!(name, args, 0);
    match port.lookahead_u8(vm) {
        Ok(Some(u)) => Ok(Object::Fixnum(u as isize)),
        Ok(None) => Ok(Object::Eof),
        Err(e) => Err(e),
    }
}

fn open_bytevector_input_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "open-bytevector-input-port";
    check_argc_between!(name, args, 1, 2);
    let bv = as_bytevector!(name, args, 0);
    if args.len() == 1 {
        Ok(Object::BytevectorInputPort(
            vm.gc.alloc(BytevectorInputPort::new(&bv.data)),
        ))
    } else {
        let transcoder = check_is_transcoder_or_false!(name, args, 1);
        if transcoder.is_false() {
            Ok(Object::BytevectorInputPort(
                vm.gc.alloc(BytevectorInputPort::new(&bv.data)),
            ))
        } else {
            let bin_port =
                Object::BytevectorInputPort(vm.gc.alloc(BytevectorInputPort::new(&bv.data)));
            let port = TranscodedInputPort::new(bin_port, transcoder);
            Ok(Object::TranscodedInputPort(vm.gc.alloc(port)))
        }
    }
}

fn ffi_open(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-open";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_lookup(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-lookup";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_call(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-call";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_ffi_supported(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-supported?";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_malloc(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-malloc";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_free(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-free";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_make_c_callback_trampoline(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-make-c-callback-trampoline";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_free_c_callback_trampoline(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-free-c-callback-trampoline";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_close(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-close";
    todo!("{}({}) not implemented", name, args.len());
}
fn ffi_error(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%ffi-error";
    todo!("{}({}) not implemented", name, args.len());
}
fn host_os(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "host-os";
    check_argc!(name, args, 0);
    Ok(vm.gc.new_string(env::consts::OS))
}
fn is_output_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "output-port?";
    check_argc!(name, args, 1);
    Ok(args[0].is_output_port().to_obj())
}
fn is_textual_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "textual-port?";
    check_argc!(name, args, 1);
    Ok(args[0].is_textual_port().to_obj())
}
fn is_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port?";
    check_argc!(name, args, 1);
    Ok(args[0].is_port().to_obj())
}
fn port_transcoder(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port-transcoder";
    todo!("{}({}) not implemented", name, args.len());
}
fn create_native_transcoder(gc: &mut Box<Gc>) -> Object {
    let codec = Object::UTF8Codec(gc.alloc(UTF8Codec::new()));
    let eol_style = if std::env::consts::OS == "windows" {
        EolStyle::CrLf
    } else {
        EolStyle::Lf
    };
    Object::Transcoder(gc.alloc(Transcoder::new(
        codec,
        eol_style,
        ErrorHandlingMode::RaiseError,
    )))
}

fn native_transcoder(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "native-transcoder";
    check_argc!(name, args, 0);
    Ok(create_native_transcoder(&mut vm.gc))
}

fn put_bytevector(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "put-bytevector";
    check_argc_between!(name, args, 2, 4);
    let bv = as_bytevector!(name, args, 1);
    let start = if args.len() >= 3 {
        as_usize!(name, args, 2)
    } else {
        0
    };
    let count = if args.len() >= 4 {
        as_usize!(name, args, 3)
    } else {
        bv.len() - start
    };
    if bv.len() < start + count {
        return Error::assertion_violation(
            name,
            "Bytevector must have a length of at least start + count.",
            &[args[1], args[2]],
        );
    }

    let buf = &bv.data[start..start + count];
    let port = as_binary_output_port_mut!(name, args, 0);
    match port.write(buf) {
        Ok(_size) => Ok(Object::Unspecified),
        Err(e) => Err(e),
    }
}
fn put_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "put-char";
    check_argc!(name, args, 2);
    let port = as_text_output_port_mut!(name, args, 0);
    let ch = as_char!(name, args, 1);
    port.write_char(ch).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("write error {}", e),
            &[args[0]],
        )
    })?;
    Ok(Object::Unspecified)
}
fn write_char(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "write-char";
    check_argc_between!(name, args, 1, 2);
    let c = as_char!(name, args, 0);
    let port = if args.len() == 1 {
        vm.current_output_port()
    } else {
        args[1]
    };
    let port = obj_as_text_output_port_mut!(name, port);
    match port.write_char(c) {
        Ok(_) => Ok(Object::Unspecified),
        Err(_) => error::Error::assertion_violation(name, "write-char failed", &[args[0]]),
    }
}
fn transcoder_codec(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "transcoder-codec";
    check_argc!(name, args, 1);
    Ok(args[0].to_transcoder().codec)
}
fn transcoder_eol_style(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "transcoder-eol-style";
    check_argc!(name, args, 1);
    Ok(match args[0].to_transcoder().eol_style {
        EolStyle::Lf => vm.gc.symbol_intern("lf"),
        EolStyle::Cr => vm.gc.symbol_intern("cr"),
        EolStyle::Nel => vm.gc.symbol_intern("nel"),
        EolStyle::Ls => vm.gc.symbol_intern("ls"),
        EolStyle::CrNel => vm.gc.symbol_intern("crnel"),
        EolStyle::CrLf => vm.gc.symbol_intern("crlf"),
        EolStyle::ENone => vm.gc.symbol_intern("none"),
    })
}
fn transcoder_error_handling_mode(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "transcoder-error-handling-mode";
    check_argc!(name, args, 1);
    Ok(match args[0].to_transcoder().mode {
        ErrorHandlingMode::IgnoreError => vm.gc.symbol_intern("ignore"),
        ErrorHandlingMode::RaiseError => vm.gc.symbol_intern("raise"),
        ErrorHandlingMode::ReplaceError => vm.gc.symbol_intern("replace"),
    })
}
fn quotient(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "quotient";
    check_argc!(name, args, 2);
    match numbers::quotient(&mut vm.gc, args[0], args[1]) {
        Ok(v) => Ok(v),
        Err(SchemeError::NonZeroRequired) => {
            generic_error!(
                name,
                args,
                "none zero required but got {} {}",
                args[0],
                args[1]
            )
        }
        _ => bug!(),
    }
}
fn remainder(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "remainder";
    check_argc!(name, args, 2);
    match numbers::remainder(&mut vm.gc, args[0], args[1]) {
        Ok(v) => Ok(v),
        Err(SchemeError::NonZeroRequired) => {
            generic_error!(
                name,
                args,
                "none zero required but got {} {}",
                args[0],
                args[1]
            )
        }
        _ => bug!(),
    }
}
fn modulo(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "modulo";
    match numbers::modulo(&mut vm.gc, args[0], args[1]) {
        Ok(v) => Ok(v),
        Err(SchemeError::NonZeroRequired) => {
            generic_error!(
                name,
                args,
                "none zero required but got {} {}",
                args[0],
                args[1]
            )
        }
        _ => bug!(),
    }
}
fn open_file_input_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "open-file-input/output-port";
    check_argc_between!(name, args, 1, 4);
    let path = &as_sstring!(name, args, 0).string;
    let file_exists = Path::new(&path).exists();
    /*let file_readable = match fs::metadata(path) {
        Ok(m) => !m.permissions().readonly(),
        Err(_) => false,
    };*/
    let argc = args.len();
    let mut open_options = OpenOptions::new();
    open_options.write(true).create(true).read(true);
    let mut transcoder: Option<Object> = None;
    let file: File;
    if argc == 1 {
        if file_exists {
            return error::Error::io_file_already_exist(
                name,
                &format!("file already exists {}", path),
                args,
            );
        }
        file = match open_options.open(path) {
            Ok(file) => file,
            Err(err) => {
                return error::Error::assertion_violation(
                    name,
                    &format!("file open error {}: {}", path, err),
                    args,
                );
            }
        };
    } else {
        let file_options = match args[1] {
            Object::SimpleStruct(s) => s.field(1),
            _ => {
                return type_required_error(name, "file-options", &[args[1]]);
            }
        };
        let empty_p = file_options.is_nil();
        let sym_no_create = vm.gc.symbol_intern("no-create");
        let sym_no_truncate = vm.gc.symbol_intern("no-truncate");
        let sym_no_fail = vm.gc.symbol_intern("no-fail");
        let no_create_p = !memq(vm, &mut [sym_no_create, file_options])
            .unwrap()
            .is_false();
        let no_truncate_p = !memq(vm, &mut [sym_no_truncate, file_options])
            .unwrap()
            .is_false();
        let no_fail_p = !memq(vm, &mut [sym_no_fail, file_options])
            .unwrap()
            .is_false();

        if file_exists && empty_p {
            return error::Error::io_file_already_exist(
                name,
                &format!("file already exists {}", path),
                args,
            );
        } else if no_create_p && no_truncate_p {
            if !file_exists {
                return error::Error::io_file_not_exist(
                    name,
                    &format!("file-options no-create: file not exist {}", path),
                    args,
                );
            }
        } else if no_create_p {
            if file_exists {
                open_options.truncate(true);
            } else {
                return error::Error::io_file_not_exist(
                    name,
                    &format!("file-options no-create: file not exist {}", path),
                    args,
                );
            }
        } else if no_fail_p && no_truncate_p {
            if !file_exists {
                open_options.truncate(true);
            }
        } else if no_fail_p {
            open_options.truncate(true);
        } else if no_truncate_p {
            if file_exists {
                return error::Error::io_file_already_exist(
                    name,
                    &format!("file already exists {}", path),
                    args,
                );
            } else {
                open_options.truncate(true);
            }
        }

        if argc == 4 {
            match args[3] {
                Object::Transcoder(_) => transcoder = Some(args[3]),
                Object::False => {}
                _ => {
                    return Error::assertion_violation(
                        name,
                        "transcoder or #f required",
                        &[args[3]],
                    );
                }
            }
        }
        file = match open_options.open(path) {
            Ok(file) => file,
            Err(err) => {
                return generic_error!(name, args, "{}", err);
            }
        };
    }

    // We ignore buffer-mode. This implmentation is not buffered at this momement.
    // We may revisit this. Once we conform R7RS and R6RS.
    let buffer_mode = if argc < 3 {
        BufferMode::None
    } else {
        match symbol_to_buffer_mode(as_symbol!(name, args, 2)) {
            Some(mode) => mode,
            None => {
                return Error::assertion_violation(name, "invalid buffer-mode", &[args[2]]);
            }
        }
    };

    match transcoder {
        Some(t) => {
            let bin_port = Object::BinaryFileInputOutputPort(
                vm.gc
                    .alloc(BinaryFileInputOutputPort::new(file, buffer_mode)),
            );
            let port = TranscodedInputOutputPort::new(bin_port, t);
            Ok(Object::TranscodedInputOutputPort(vm.gc.alloc(port)))
        }
        None => Ok(Object::BinaryFileInputOutputPort(
            vm.gc
                .alloc(BinaryFileInputOutputPort::new(file, buffer_mode)),
        )),
    }
}
fn make_custom_binary_input_output_port(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-custom-binary-input/output-port";
    check_argc!(name, args, 6);
    let id = &as_sstring!(name, args, 0).string;
    let read_proc = check_is_closure!(name, args, 1);
    let write_proc = check_is_closure!(name, args, 2);
    let pos_proc = check_is_closure_or_false!(name, args, 3);
    let set_pos_proc = check_is_closure_or_false!(name, args, 4);
    let close_proc = check_is_closure_or_false!(name, args, 5);
    Ok(Object::CustomBinaryInputOutputPort(vm.gc.alloc(
        CustomBinaryInputOutputPort::new(
            id,
            read_proc,
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        ),
    )))
}
fn make_custom_textual_input_output_port(
    vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "make-custom-textual-input/output-port";
    check_argc!(name, args, 6);
    let id = &as_sstring!(name, args, 0).string;
    let read_proc = check_is_closure!(name, args, 1);
    let write_proc = check_is_closure!(name, args, 2);
    let pos_proc = check_is_closure_or_false!(name, args, 3);
    let set_pos_proc = check_is_closure_or_false!(name, args, 4);
    let close_proc = check_is_closure_or_false!(name, args, 5);
    Ok(Object::CustomTextInputOutputPort(vm.gc.alloc(
        CustomTextInputOutputPort::new(
            id,
            read_proc,
            write_proc,
            pos_proc,
            set_pos_proc,
            close_proc,
        ),
    )))
}
fn put_datum(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "put-datum";
    check_argc!(name, args, 2);
    let port = as_text_output_port_mut!(name, args, 0);
    port.write(args[1], false).map_err(|e| {
        Error::new(
            ErrorType::IoError,
            name,
            &format!("write error {}", e),
            &[args[0]],
        )
    })?;
    Ok(Object::Unspecified)
}
fn list_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "list-ref";
    check_argc!(name, args, 2);
    let mut obj = args[0];
    match args[1] {
        Object::Fixnum(mut index) => loop {
            index -= 1;
            if index < 0 {
                break;
            }
            if let Object::Pair(p) = obj {
                obj = p.cdr;
            } else {
                return type_required_error(name, "pair", &[obj]);
            }
        },
        _ => {
            return number_required_error(name, &[args[1]]);
        }
    }
    if obj.is_pair() {
        Ok(obj.car_unchecked())
    } else {
        type_required_error(name, "pair", &[obj])
    }
}

fn list_tail(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "list-tail";
    check_argc!(name, args, 2);
    let index = args[1];
    if !index.is_fixnum() || index.to_isize() < 0 {
        return type_required_error(name, "number index > 0", &[args[1]]);
    }
    let mut index = index.to_isize();
    let mut obj = args[0];
    while index > 0 {
        if obj.is_pair() {
            obj = obj.cdr_unchecked();
        } else {
            return type_required_error(name, "proper list", &[obj]);
        }
        index -= 1;
    }
    Ok(obj)
}
fn time_usage(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "time-usage";
    todo!("{}({}) not implemented", name, args.len());
}
fn mosh_executable_path(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "mosh-executable-path";
    check_argc!(name, args, 0);
    match current_exe() {
        Ok(path_buf) => match path_buf.as_os_str().to_str() {
            Some(s) => Ok(vm.gc.new_string(s)),
            None => {
                generic_error!(name, args, "{} conversion error", "os_str")
            }
        },
        Err(err) => {
            generic_error!(name, args, "{}", err)
        }
    }
}
fn is_socket(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket?";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_accept(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-accept";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_client_socket(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-client-socket";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_server_socket(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-server-socket";
    todo!("{}({}) not implemented", name, args.len());
}
fn os_constant(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "os-constant";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_recv(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-recv";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_recv_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-recv!";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_send(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-send";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_close(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-close";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_shutdown(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-shutdown";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_port(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-port";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_vm(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-vm";
    todo!("{}({}) not implemented", name, args.len());
}
fn vm_start_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm-start!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_vm(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm?";
    todo!("{}({}) not implemented", name, args.len());
}
fn vm_set_value_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm-set-value!";
    todo!("{}({}) not implemented", name, args.len());
}
fn vm_join_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm-join!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_main_vm(_vm: &mut Vm, _args: &mut [Object]) -> error::Result<Object> {
    let _name: &str = "main-vm?";
    Ok(Object::True)
}
fn vm_self(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vm-self";
    todo!("{}({}) not implemented", name, args.len());
}
fn register(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "register";
    todo!("{}({}) not implemented", name, args.len());
}
fn whereis(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "whereis";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_condition_variable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-condition-variable";
    todo!("{}({}) not implemented", name, args.len());
}
fn condition_variable_wait_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "condition-variable-wait!";
    todo!("{}({}) not implemented", name, args.len());
}
fn condition_variable_notify_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "condition-variable-notify!";
    todo!("{}({}) not implemented", name, args.len());
}
fn condition_variable_notify_all_destructive(
    _vm: &mut Vm,
    args: &mut [Object],
) -> error::Result<Object> {
    let name: &str = "condition-variable-notify-all!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_mutex(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "mutex?";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_mutex(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-mutex";
    todo!("{}({}) not implemented", name, args.len());
}
fn mutex_lock_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "mutex-lock!";
    todo!("{}({}) not implemented", name, args.len());
}
fn mutex_try_lock_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "mutex-try-lock!";
    todo!("{}({}) not implemented", name, args.len());
}
fn mutex_unlock_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "mutex-unlock!";
    todo!("{}({}) not implemented", name, args.len());
}
fn make_vector(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-vector";
    todo!("{}({}) not implemented", name, args.len());
}
fn vector_length(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector-length";
    todo!("{}({}) not implemented", name, args.len());
}
fn vector_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn vector_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "vector-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn create_directory(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "create-directory";
    check_argc!(name, args, 1);
    if let Object::String(path) = args[0] {
        match fs::create_dir(&path.string) {
            Ok(()) => Ok(Object::Unspecified),
            Err(err) => {
                generic_error!(name, args, "{}", err)
            }
        }
    } else {
        type_required_error(name, "path", &[args[0]])
    }
}
fn delete_directory(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "delete-directory";
    todo!("{}({}) not implemented", name, args.len());
}
fn rename_file(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "rename-file";
    todo!("{}({}) not implemented", name, args.len());
}
fn create_symbolic_link(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "create-symbolic-link";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_directory(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-directory?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_symbolic_link(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-symbolic-link?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_regular(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-regular?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_readable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-readable?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_executable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-executable?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_file_writable(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-writable?";
    todo!("{}({}) not implemented", name, args.len());
}
fn file_size_in_bytes(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-size-in-bytes";
    todo!("{}({}) not implemented", name, args.len());
}
fn file_stat_mtime(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-stat-mtime";
    check_argc!(name, args, 1);
    if let Object::String(path) = args[0] {
        if path.string.starts_with("/embed/stdlib") {
            if let Some(file) = StdLib::get(&path.string) {
                match file.metadata.last_modified() {
                    Some(last_modified) => Ok(last_modified.to_obj(&mut vm.gc)),
                    None => {
                        generic_error!(
                            name,
                            args,
                            "failed to retrieve last modified for {}",
                            path.string
                        )
                    }
                }
            } else {
                generic_error!(
                    name,
                    args,
                    "failed to retrieve metadata for {}",
                    path.string
                )
            }
        } else {
            let metadata = File::open(&path.string)
                .map(|file| file.metadata())
                .unwrap_or_else(|_| {
                    bug!("failed to retrieve metadata for {}", path.string);
                });

            // Get the last modification time
            let mtime = metadata
                .map(|metadata| metadata.modified())
                .unwrap_or_else(|_| {
                    bug!("failed to retrieve modification time for {}", path.string);
                });

            // Convert the last modification time to a system time
            let mtime = mtime.unwrap_or(SystemTime::now());
            let mtime_seconds = mtime
                .duration_since(UNIX_EPOCH)
                .unwrap_or_else(|_| {
                    bug!("system time before UNIX epoch");
                })
                .as_secs();

            Ok(Object::Fixnum(mtime_seconds as isize))
        }
    } else {
        type_required_error(name, "file path", &[args[0]])
    }
}
fn file_stat_atime(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-stat-atime";
    todo!("{}({}) not implemented", name, args.len());
}
fn file_stat_ctime(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file-stat-ctime";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_pointer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer?";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_to_integer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer->integer";
    todo!("{}({}) not implemented", name, args.len());
}
fn integer_to_pointer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "integer->pointer";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint8(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-uint8";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint16(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-uint16";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint32(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-uint32";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint64(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-uint64";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int8(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-int8";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int16(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-int16";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int32(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-int32";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int64(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-int64";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-signed-char";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_char(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-unsigned-char";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_short(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-signed-short";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_short(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-unsigned-short";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_int(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-signed-int";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_int(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-unsigned-int";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_long(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-signed-long";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_long(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-unsigned-long";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_long_long(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-signed-long-long";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_long_long(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-unsigned-long-long";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_float(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-float";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_double(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-double";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_pointer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-ref-c-pointer";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int8_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-int8!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int16_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-int16!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int32_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-int32!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int64_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-int64!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint8_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-uint8!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint16_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-uint16!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint32_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-uint32!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint64_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-uint64!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_char_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-char!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_short_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-short!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-int!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_long_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-long!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_long_long_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-long-long!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_float_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-float!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_double_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-double!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_pointer_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-set-c-pointer!";
    todo!("{}({}) not implemented", name, args.len());
}
fn pointer_copy_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer-copy!";
    todo!("{}({}) not implemented", name, args.len());
}
fn bytevector_pointer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "bytevector-pointer";
    todo!("{}({}) not implemented", name, args.len());
}
fn shared_errno(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "shared-errno";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_simple_struct(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "simple-struct?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::SimpleStruct(_) => Ok(Object::True),
        _ => Ok(Object::False),
    }
}
fn make_simple_struct(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-simple-struct";
    check_argc!(name, args, 3);
    match args[1] {
        Object::Fixnum(len) => {
            let mut s = vm.gc.alloc(SimpleStruct::new(args[0], len as usize));
            s.initialize(args[2]);
            Ok(Object::SimpleStruct(s))
        }
        obj => number_required_error(name, &[obj]),
    }
}
fn simple_struct_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "simple-struct-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn simple_struct_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "simple-struct-set!";
    check_argc!(name, args, 3);
    match (args[0], args[1]) {
        (Object::SimpleStruct(mut s), Object::Fixnum(index)) => {
            s.set(index as usize, args[2]);
            Ok(Object::Unspecified)
        }
        _ => type_required_error(name, "simple struct and number", &[args[0], args[1]]),
    }
}
fn simple_struct_name(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "simple-struct-name";
    check_argc!(name, args, 1);
    match args[0] {
        Object::SimpleStruct(s) => Ok(s.name),
        obj => type_required_error(name, "simple struct", &[obj]),
    }
}
fn lookup_nongenerative_rtd(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "lookup-nongenerative-rtd";
    check_argc!(name, args, 1);
    Ok(vm.lookup_rtd(args[0]))
}
fn nongenerative_rtd_set_destructive(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "nongenerative-rtd-set!";
    check_argc!(name, args, 2);
    vm.set_rtd(args[0], args[1]);
    Ok(Object::Unspecified)
}

/* psyntax/expander.ss
(define (same-marks*? mark* mark** si)
    (if (null? si)
        #f
        (if (same-marks? mark* (vector-ref mark** (car si)))
            (car si)
            (same-marks*? mark* mark** (cdr si)))))
*/
fn is_same_marksmul(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "same-marks*?";
    check_argc!(name, args, 3);
    let mark_mul = args[0];
    let mark_mul_mul = args[1];
    let mut si = args[2];
    loop {
        if si.is_nil() {
            return Ok(Object::False);
        }
        if is_same_marks_raw(
            mark_mul,
            mark_mul_mul.to_vector().data[si.car_unchecked().to_isize() as usize],
        ) {
            return Ok(si.car_unchecked());
        }
        si = si.cdr_unchecked();
    }
}

fn is_same_marks(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "same-marks?";
    check_argc!(name, args, 2);
    Ok(is_same_marks_raw(args[0], args[1]).to_obj())
}

/* psyntax/expander.ss
  ;;; Two lists of marks are considered the same if they have the
  ;;; same length and the corresponding marks on each are eq?.
  (define same-marks?
    (lambda (x y)
      (or (and (null? x) (null? y)) ;(eq? x y)
          (and (pair? x) (pair? y)
               (eq? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))
*/
fn is_same_marks_raw(x: Object, y: Object) -> bool {
    let mut x = x;
    let mut y = y;
    loop {
        if x.is_nil() && y.is_nil() {
            return true;
        }
        if x.is_nil() && !y.is_nil() {
            return false;
        }
        if !x.is_nil() && y.is_nil() {
            return false;
        }
        if x.is_pair() && !y.is_pair() {
            return false;
        }
        if !x.is_pair() && y.is_pair() {
            return false;
        }
        if x.car_unchecked() != y.car_unchecked() {
            return false;
        }
        x = x.cdr_unchecked();
        y = y.cdr_unchecked();
    }
}

/* psyntax/expander.ss
(define id->real-label
    (lambda (id)
      (let ((sym (id->sym id)))
        (let search ((subst* (stx-subst* id)) (mark* (stx-mark* id)))
          (cond
            ((null? subst*) #f)
            ((eq? (car subst*) 'shift)
             ;;; a shift is inserted when a mark is added.
             ;;; so, we search the rest of the substitution
             ;;; without the mark.
             (search (cdr subst*) (cdr mark*)))
            (else
             (let ((rib (car subst*)))
               (cond
                 ((rib-sealed/freq rib) =>
                  (lambda (ht)
                    (let ((si (hashtable-ref ht sym #f)))
                      (let ((i (and si
                            (same-marks*? mark*
                              (rib-mark** rib) (reverse si)))))
                        (if i
                          (vector-ref (rib-label* rib) i)
                        (search (cdr subst*) mark*))))))
;                 ((find-label rib sym mark*))
                 (else
                  (let f ((sym* (rib-sym* rib))
                          (mark** (rib-mark** rib))
                          (label* (rib-label* rib)))
                    (cond
                      ((null? sym*) (search (cdr subst*) mark*))
                      ((and (eq? (car sym*) sym)
                            (same-marks? (car mark**) mark*))
                       (car label*))
                      (else (f (cdr sym*) (cdr mark**) (cdr label*))))))))))))))
*/

fn id_to_real_label(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "id->real-label";
    check_argc!(name, args, 1);
    if let Object::SimpleStruct(id) = args[0] {
        let sym = id.field(0);
        let mut mark_mul = id.field(1);
        let mut subst_mul = id.field(2);
        let shift_symbol = vm.gc.symbol_intern("shift");
        loop {
            if subst_mul.is_nil() {
                return Ok(Object::False);
            }

            if subst_mul.car_unchecked() == shift_symbol {
                subst_mul = subst_mul.cdr_unchecked();
                mark_mul = mark_mul.cdr_unchecked();
                continue;
            } else {
                let rib = subst_mul.car_unchecked();
                let rib_sealed_freq = rib.to_simple_struct().field(3);
                if !rib_sealed_freq.is_false() {
                    let si = rib_sealed_freq.to_eq_hashtable().get(sym, Object::False);
                    let i;
                    if si.is_false() {
                        i = Object::False;
                    } else {
                        let mut xs: [Object; 3] = [Object::Unspecified; 3];
                        xs[0] = mark_mul;
                        xs[1] = rib.to_simple_struct().field(1);
                        xs[2] = Pair::reverse(&mut vm.gc, si);
                        i = is_same_marksmul(vm, &mut xs).unwrap();
                    }
                    if i.is_false() {
                        subst_mul = subst_mul.cdr_unchecked();
                        continue;
                    } else {
                        return Ok(
                            rib.to_simple_struct().field(2).to_vector().data[i.to_isize() as usize]
                        );
                    }
                } else {
                    let mut sym_mul = rib.to_simple_struct().field(0);
                    let mut mark_mul_mul = rib.to_simple_struct().field(1);
                    let mut label_mul = rib.to_simple_struct().field(2);
                    loop {
                        if sym_mul.is_nil() {
                            subst_mul = subst_mul.cdr_unchecked();
                            break;
                        } else if sym == sym_mul.car_unchecked()
                            && is_same_marks_raw(mark_mul_mul.car_unchecked(), mark_mul)
                        {
                            return Ok(label_mul.car_unchecked());
                        } else {
                            sym_mul = sym_mul.cdr_unchecked();
                            mark_mul_mul = mark_mul_mul.cdr_unchecked();
                            label_mul = label_mul.cdr_unchecked();
                        }
                    }
                }
            }
        }
    } else {
        type_required_error(name, "simple-struct", &[args[0]])
    }
}

fn f(gc: &mut Box<Gc>, x: Object, ls1: Object, ls2: Object) -> Object {
    if ls1.is_nil() {
        ls2.cdr_unchecked()
    } else {
        let kdr = f(gc, ls1.car_unchecked(), ls1.cdr_unchecked(), ls2);
        gc.cons(x, kdr)
    }
}

fn cancel(gc: &mut Box<Gc>, ls1: Object, ls2: Object) -> Object {
    f(gc, ls1.car_unchecked(), ls1.cdr_unchecked(), ls2)
}

fn join_wraps(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "join-wraps";
    check_argc!(name, args, 4);
    let m1_mul = args[0];
    let s1_mul = args[1];
    let ae1_mul = args[2];
    let e = args[3];
    let m2_mul = e.to_simple_struct().field(1);
    let s2_mul = e.to_simple_struct().field(2);
    let ae2_mul = e.to_simple_struct().field(3);
    if !m1_mul.is_nil() && !m2_mul.is_nil() && m2_mul.car_unchecked().is_false() {
        let x = cancel(&mut vm.gc, m1_mul, m2_mul);
        let y = cancel(&mut vm.gc, s1_mul, s2_mul);
        let z = cancel(&mut vm.gc, ae1_mul, ae2_mul);
        let values = [x, y, z];
        Ok(vm.values(&values))?
    } else {
        let x = vm.gc.append2(m1_mul, m2_mul)?;
        let y = vm.gc.append2(s1_mul, s2_mul)?;
        let z = vm.gc.append2(ae1_mul, ae2_mul)?;
        let values = [x, y, z];
        Ok(vm.values(&values))?
    }
}

fn gensym_prefix_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "gensym-prefix-set!";
    check_argc!(name, args, 1);
    if let Object::Symbol(s) = args[0] {
        unsafe { GENSYM_PREFIX = s.string.chars().next().unwrap() };
        Ok(Object::Unspecified)
    } else {
        type_required_error(name, "symbol", &[args[0]])
    }
}

fn current_dynamic_winders(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "current-dynamic-winders";
    check_argc_max!(name, args, 1);
    let argc = args.len();
    if argc == 0 {
        Ok(vm.dynamic_winders)
    } else {
        vm.dynamic_winders = args[0];
        Ok(Object::Unspecified)
    }
}
fn sexp_map(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sexp-map";
    todo!("{}({}) not implemented", name, args.len());
}
fn sexp_map_debug(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "sexp-map/debug";
    todo!("{}({}) not implemented", name, args.len());
}
fn write_ss(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "write/ss";
    check_argc_between!(name, args, 1, 2);
    let argc = args.len();
    let port = if argc == 1 {
        vm.current_output_port()
    } else {
        args[1]
    };
    let shared_aware = true;
    let port = obj_as_text_output_port_mut!(name, port);
    port.write(args[0], shared_aware).ok();
    Ok(Object::Unspecified)
}
fn monapi_message_send(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-message-send";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_name_whereis(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-name-whereis";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_message_receive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-message-receive";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_name_add_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-name-add!";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_message_send_receive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-message-send-receive";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_message_reply(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-message-reply";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_make_stream(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-make-stream";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_handle(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-stream-handle";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_write(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-stream-write";
    todo!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_read(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "%monapi-stream-read";
    todo!("{}({}) not implemented", name, args.len());
}
fn process_list(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "process-list";
    todo!("{}({}) not implemented", name, args.len());
}
fn process_terminate_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "process-terminate!";
    todo!("{}({}) not implemented", name, args.len());
}
fn socket_sslize_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "socket-sslize!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_ssl_socket(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "ssl-socket?";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_ssl_supported(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "ssl-supported?";
    todo!("{}({}) not implemented", name, args.len());
}
fn file_to_string(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "file->string";
    todo!("{}({}) not implemented", name, args.len());
}
fn annotated_cons(vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "annotated-cons";
    check_argc_between!(name, args, 2, 3);
    if args.len() == 2 {
        Ok(vm.gc.cons(args[0], args[1]))
    } else {
        let p = vm.gc.cons(args[0], args[1]);
        p.to_pair().src = args[2];
        Ok(p)
    }
}
fn is_annotated_pair(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "annotated-pair?";
    check_argc!(name, args, 1);
    Ok(args[0].is_pair().to_obj())
}
fn get_annotation(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "get-annotation";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Pair(p) => Ok(p.src),
        obj => type_required_error(name, "pair", &[obj]),
    }
}
fn set_annotation_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-annotation!";
    check_argc!(name, args, 2);
    match args[0] {
        Object::Pair(mut p) => {
            p.src = args[1];
            Ok(Object::Unspecified)
        }
        obj => type_required_error(name, "pair", &[obj]),
    }
}
fn pointer_to_object(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "pointer->object";
    todo!("{}({}) not implemented", name, args.len());
}
fn object_to_pointer(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "object->pointer";
    todo!("{}({}) not implemented", name, args.len());
}
fn set_current_error_port_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "set-current-error-port!";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_port_open(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "port-open?";
    check_argc!(name, args, 1);
    let port = as_port!(name, args, 0);
    Ok(port.is_open().to_obj())
}
fn make_f64array(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "make-f64array";
    todo!("{}({}) not implemented", name, args.len());
}
fn is_f64array(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "f64array?";
    todo!("{}({}) not implemented", name, args.len());
}
fn f64array_ref(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "f64array-ref";
    todo!("{}({}) not implemented", name, args.len());
}
fn f64array_set_destructive(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "f64array-set!";
    todo!("{}({}) not implemented", name, args.len());
}
fn f64array_shape(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "f64array-shape";
    todo!("{}({}) not implemented", name, args.len());
}
fn f64array_dot_product(_vm: &mut Vm, args: &mut [Object]) -> error::Result<Object> {
    let name: &str = "f64array-dot-product";
    todo!("{}({}) not implemented", name, args.len());
}
