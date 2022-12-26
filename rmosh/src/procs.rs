/// Scheme procedures written in Rust.
/// The procedures will be exposed to the VM via free vars.
use crate::{
    gc::Gc,
    objects::{InputPort, Object, Pair},
    vm::Vm,
};

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
            panic!(
                "{}: {} arguments required but got {}",
                $name,
                $argc,
                $args.len()
            );
        }
    }};
}

#[macro_export]
macro_rules! check_argc_at_least {
    ($name:ident, $args:ident, $argc:expr) => {{
        if $args.len() < $argc {
            panic!(
                "{}: at least {} arguments required but got {}",
                $name,
                $argc,
                $args.len()
            );
        }
    }};
}

#[macro_export]
macro_rules! check_argc_between {
    ($name:ident, $args:ident, $min:expr, $max:expr) => {{
        if $args.len() > $max || $args.len() < $min {
            panic!(
                "{}: {}-{} arguments required but got {}",
                $name,
                $min,
                $max,
                $args.len()
            );
        }
    }};
}

fn is_number(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "number?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Number(_) => Object::True,
        _ => Object::False,
    }
}
fn cons(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cons";
    check_argc!(name, args, 2);
    vm.gc.cons(args[0], args[1])
}
fn consmul(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cons*";
    check_argc_at_least!(name, args, 1);
    let argc = args.len();
    if argc == 1 {
        return args[0];
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
                panic!("{}: pair required but got {}", name, tail);
            }
        }
    }
    match tail {
        Object::Pair(mut pair) => pair.cdr = args[argc - 1],
        _ => {
            panic!("{}: pair required but got {}", name, tail);
        }
    }
    obj
}
fn car(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cons";
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => pair.car,
        _ => {
            panic!("{}: pair required", name)
        }
    }
}

fn cdr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdr";
    assert_eq!(args.len(), 1);
    match args[0] {
        Object::Pair(pair) => pair.cdr,
        _ => {
            panic!("{}: pair required", name)
        }
    }
}
fn is_null(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "null?";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_car_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-car!";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_cdr_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-cdr!";
    panic!("{}({}) not implemented", name, args.len());
}
fn sys_display(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sys-display";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_regexp(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "regexp?";
    panic!("{}({}) not implemented", name, args.len());
}
fn regexp_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "regexp->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch_start(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch-start";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch_end(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch-end";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch_after(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch-after";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch_before(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch-before";
    panic!("{}({}) not implemented", name, args.len());
}
fn rxmatch_substring(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rxmatch-substring";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_string(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-string";
    check_argc_between!(name, args, 1, 2);
    match args {
        [Object::Number(n)] => vm.gc.new_string(&" ".repeat(*n as usize)),
        [Object::Number(n), Object::Char(c)] => {
            vm.gc.new_string(&*c.to_string().repeat(*n as usize))
        }
        _ => {
            panic!("{}: wrong arguments {:?}", name, args)
        }
    }
}
fn string_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-set!";
    check_argc!(name, args, 3);
    match args {
        [Object::String(mut s), Object::Number(idx), Object::Char(c)] => {
            let idx = *idx as usize;
            s.string.replace_range(idx..idx + 1, &c.to_string());
            Object::Unspecified
        }
        _ => {
            panic!(
                "{}: string, number and char required but got {:?}",
                name, args
            )
        }
    }
}
fn string_length(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-length";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => Object::Number(s.string.chars().count().try_into().unwrap()),
        v => {
            panic!("{}: string required but got {}", name, v)
        }
    }
}
fn string_to_symbol(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->symbol";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => vm.gc.symbol_intern(&s.string),
        v => {
            panic!("{}: string required but got {}", name, v)
        }
    }
}
fn string_to_number(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->number";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => match s.string.parse::<isize>() {
            Ok(n) => Object::Number(n),
            Err(err) => {
                panic!("{}: can't convert to numver {:?}", name, err)
            }
        },
        v => {
            panic!("{}: string required but got {}", name, v)
        }
    }
}
fn string_append(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-append";
    let mut ret = "".to_string();
    for arg in args {
        match arg {
            Object::String(s) => {
                ret = ret + &s.string;
            }
            _ => {
                panic!("{}: string required but got {:?}", name, args);
            }
        }
    }
    vm.gc.new_string(&ret)
}
fn string_split(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-split";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::String(s), Object::Char(c)) => {
            let mut l = Object::Nil;
            for w in s.string.rsplit(c) {
                let obj = vm.gc.new_string(w);
                l = vm.gc.cons(obj, l);
            }
            l
        }
        _ => {
            panic!("{}: string and char required but got {:?}", name, args);
        }
    }
}
fn string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_to_string(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "number->string";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Number(n) => vm.gc.new_string(&format!("{}", n)[..]),
        v => {
            panic!("{}: number required but got {}", name, v)
        }
    }
}
fn reverse(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "reverse";
    check_argc!(name, args, 1);
    let mut ret = Object::Nil;
    let mut p = args[0];
    loop {
        match p {
            Object::Pair(pair) => {
                ret = vm.gc.cons(pair.car, ret);
                p = pair.cdr;
            }
            _ => {
                break;
            }
        }
    }
    return ret;
}
fn is_eof_object(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eof-object?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Eof => Object::True,
        _ => Object::False,
    }
}
fn read_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "read-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn peek_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "peek-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_charequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "char=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(_) => Object::True,
        _ => Object::False,
    }
}
fn get_environment_variable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-environment-variable";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_environment_variables(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-environment-variables";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_equal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "equal?";
    panic!("{}({}) not implemented", name, args.len());
}
fn open_string_input_port(vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-string-input-port";
    check_argc!(name, args, 1);
    match args[0] {
        Object::String(s) => match InputPort::open(&s.string) {
            Ok(port) => Object::InputPort(vm.gc.alloc(port)),
            Err(err) => {
                panic!("{}: {:?}", name, err);
            }
        },
        _ => {
            panic!("{}: string required but got {:?}", name, args);
        }
    }
}
fn open_output_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-output-string";
    panic!("{}({}) not implemented", name, args.len());
}
fn sys_port_seek(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sys-port-seek";
    panic!("{}({}) not implemented", name, args.len());
}
fn close_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "close-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn digit_to_integer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "digit->integer";
    check_argc!(name, args, 2);
    match (args[0], args[1]) {
        (Object::Char(c), Object::Number(radix)) => match c.to_digit(radix as u32) {
            Some(v) => Object::Number(v as isize),
            None => {
                panic!("{}: could not convert ({}, {})", name, args[0], args[1]);
            }
        },
        _ => {
            panic!(
                "{}: char and number required but got {} and {}",
                name, args[0], args[1]
            );
        }
    }
}
fn get_remaining_input_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-remaining-input-string";
    panic!("{}({}) not implemented", name, args.len());
}
fn directory_list(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "directory-list";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_exists(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-exists?";
    panic!("{}({}) not implemented", name, args.len());
}
fn delete_file(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "delete-file";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_output_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-output-string";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_to_regexp(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->regexp";
    panic!("{}({}) not implemented", name, args.len());
}
fn char_to_integer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "char->integer";
    panic!("{}({}) not implemented", name, args.len());
}
fn integer_to_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "integer->char";
    panic!("{}({}) not implemented", name, args.len());
}
fn format(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "format";
    panic!("{}({}) not implemented", name, args.len());
}
fn current_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "current-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn current_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "current-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_current_input_port_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-current-input-port!";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_current_output_port_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-current-output-port!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "char?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Char(_) => Object::True,
        _ => Object::False,
    }
}
fn write(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "write";
    panic!("{}({}) not implemented", name, args.len());
}
fn gensym(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "gensym";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_stringequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn caaaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caaaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn caaadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caaadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn caaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn caadar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caadar";
    panic!("{}({}) not implemented", name, args.len());
}
fn caaddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caaddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn caadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn caar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cadaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cadaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cadadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cadadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cadar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cadar";
    panic!("{}({}) not implemented", name, args.len());
}
fn caddar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caddar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cadddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cadddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn caddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "caddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cadr";
    match args {
        [Object::Pair(pair)] => match pair.cdr {
            Object::Pair(pair2) => return pair2.car,
            _ => {
                panic!("{}: pair required but got {:?}", name, args);
            }
        },
        _ => {
            panic!("{}: pair required but got {:?}", name, args);
        }
    }
}
fn cdaaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdaaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdaadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdaadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdadar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdadar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdaddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdaddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cddaar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cddaar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cddadr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cddadr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cddar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cddar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdddar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdddar";
    panic!("{}({}) not implemented", name, args.len());
}
fn cddddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cddddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cdddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cdddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn cddr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cddr";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_symbolequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "symbol=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_booleanequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "boolean=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_vector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_list(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "list?";
    check_argc!(name, args, 1);
    Object::make_bool(args[0].is_list())
}
fn list(vm: &mut Vm, args: &[Object]) -> Object {
    let mut obj = Object::Nil;
    let argc = args.len() as isize;
    let mut i = argc - 1;
    loop {
        if i < 0 {
            break;
        }
        obj = vm.gc.cons(args[i as usize], obj);
        i = i - 1;
    }
    obj
}

fn memq(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "memq";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_eq(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eq?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_eqv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eqv?";
    panic!("{}({}) not implemented", name, args.len());
}
fn member(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "member";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_boolean(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "boolean?";
    panic!("{}({}) not implemented", name, args.len());
}
fn symbol_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "symbol->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-ref";
    check_argc!(name, args, 2);
    match args {
        [Object::String(s), Object::Number(idx)] => {
            let idx = *idx as usize;
            match s.string.chars().nth(idx) {
                Some(c) => Object::Char(c),
                _ => {
                    panic!("{}: string index out of bound {:?}", name, args)
                }
            }
        }
        _ => {
            panic!("{}: string and number required but got {:?}", name, args)
        }
    }
}
fn get_timeofday(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-timeofday";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_eq_hashtable(vm: &mut Vm, _args: &[Object]) -> Object {
    vm.gc.new_eq_hashtable()
}
fn make_eqv_hashtable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-eqv-hashtable";
    panic!("{}({}) not implemented", name, args.len());
}
fn hashtable_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-set!";
    check_argc!(name, args, 3);
    match args[0] {
        Object::EqHashtable(mut hashtable) => hashtable.set(args[1], args[2]),
        _ => {
            panic!("{}: hashtable required but got {:?}", name, args)
        }
    }
    Object::Unspecified
}
fn hashtable_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-ref";
    check_argc_between!(name, args, 2, 3);
    match args[0] {
        Object::EqHashtable(hashtable) => {
            if args.len() == 2 {
                hashtable.get(args[1], Object::False)
            } else {
                hashtable.get(args[1], args[2])
            }
        }
        _ => {
            panic!("{}: hashtable required but got {:?}", name, args)
        }
    }
}
fn hashtable_keys(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-keys";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_hash(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-hash";
    panic!("{}({}) not implemented", name, args.len());
}
fn eqv_hash(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eqv-hash";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_ci_hash(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-ci-hash";
    panic!("{}({}) not implemented", name, args.len());
}
fn symbol_hash(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "symbol-hash";
    panic!("{}({}) not implemented", name, args.len());
}
fn equal_hash(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "equal-hash";
    panic!("{}({}) not implemented", name, args.len());
}
fn eq_hashtable_copy(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eq-hashtable-copy";
    panic!("{}({}) not implemented", name, args.len());
}
fn current_error_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "current-error-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn values(vm: &mut Vm, args: &[Object]) -> Object {
    vm.values(args)
}
fn vm_apply(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm/apply";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_pair(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pair?";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_binary_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-binary-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_binary_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-binary-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_textual_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-textual-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_textual_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-textual-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_u8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-u8";
    panic!("{}({}) not implemented", name, args.len());
}
fn put_u8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "put-u8";
    panic!("{}({}) not implemented", name, args.len());
}
fn put_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "put-string";
    panic!("{}({}) not implemented", name, args.len());
}
fn flush_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flush-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn output_port_buffer_mode(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "output-port-buffer-mode";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u8_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u8-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_port_has_port_position(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-has-port-position?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_port_has_set_port_position_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-has-set-port-position!?";
    panic!("{}({}) not implemented", name, args.len());
}
fn port_position(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-position";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_port_position_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-port-position!";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_bytevector_n_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-bytevector-n!";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_bytevector_some(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-bytevector-some";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_bytevector_all(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-bytevector-all";
    panic!("{}({}) not implemented", name, args.len());
}
fn transcoded_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "transcoded-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn latin_1_codec(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "latin-1-codec";
    panic!("{}({}) not implemented", name, args.len());
}
fn utf_8_codec(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "utf-8-codec";
    panic!("{}({}) not implemented", name, args.len());
}
fn utf_16_codec(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "utf-16-codec";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_transcoder(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-transcoder";
    panic!("{}({}) not implemented", name, args.len());
}
fn eof_object(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eof-object";
    panic!("{}({}) not implemented", name, args.len());
}
fn sys_open_bytevector_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sys-open-bytevector-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn sys_get_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sys-get-bytevector";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_length(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-length";
    panic!("{}({}) not implemented", name, args.len());
}
fn standard_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "standard-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn standard_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "standard-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn standard_error_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "standard-error-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_bytevector_n(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-bytevector-n";
    panic!("{}({}) not implemented", name, args.len());
}
fn open_file_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-file-output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn open_file_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-file-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn close_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "close-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector";
    panic!("{}({}) not implemented", name, args.len());
}
fn regexp_replace(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "regexp-replace";
    panic!("{}({}) not implemented", name, args.len());
}
fn regexp_replace_all(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "regexp-replace-all";
    panic!("{}({}) not implemented", name, args.len());
}
fn source_info(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "source-info";
    panic!("{}({}) not implemented", name, args.len());
}
fn eval(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eval";
    panic!("{}({}) not implemented", name, args.len());
}
fn eval_compiled(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "eval-compiled";
    panic!("{}({}) not implemented", name, args.len());
}

// We make apply public so that Vm can access.
pub fn apply(_vm: &mut Vm, _args: &[Object]) -> Object {
    let name: &str = "apply";
    panic!("{} should not be called. It is handled in call in vm", name);
}
fn assq(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "assq";
    panic!("{}({}) not implemented", name, args.len());
}
fn assoc(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "assoc";
    panic!("{}({}) not implemented", name, args.len());
}
fn assv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "assv";
    panic!("{}({}) not implemented", name, args.len());
}
fn exit(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "exit";
    panic!("{}({}) not implemented", name, args.len());
}
fn macroexpand_1(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "macroexpand-1";
    panic!("{}({}) not implemented", name, args.len());
}
fn memv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "memv";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_procedure(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "procedure?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Procedure(_) | Object::Closure(_) => Object::True,
        _ => Object::False,
    }
}
fn load(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "load";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_symbol(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "symbol?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_charle(_vm: &mut Vm, args: &[Object]) -> Object {
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
                            return Object::False;
                        }
                    }
                    obj => {
                        panic!("{}: char required but got {}", name, obj);
                    }
                }
            }
            obj => {
                panic!("{}: char required but got {}", name, obj);
            }
        }
    }
    Object::True
}
fn is_charlt(_vm: &mut Vm, args: &[Object]) -> Object {
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
                            return Object::False;
                        }
                    }
                    obj => {
                        panic!("{}: char required but got {}", name, obj);
                    }
                }
            }
            obj => {
                panic!("{}: char required but got {}", name, obj);
            }
        }
    }
    Object::True
}
fn is_charge(_vm: &mut Vm, args: &[Object]) -> Object {
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
                            return Object::False;
                        }
                    }
                    obj => {
                        panic!("{}: char required but got {}", name, obj);
                    }
                }
            }
            obj => {
                panic!("{}: char required but got {}", name, obj);
            }
        }
    }
    Object::True
}
fn is_chargt(_vm: &mut Vm, args: &[Object]) -> Object {
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
                            return Object::False;
                        }
                    }
                    obj => {
                        panic!("{}: char required but got {}", name, obj);
                    }
                }
            }
            obj => {
                panic!("{}: char required but got {}", name, obj);
            }
        }
    }
    Object::True
}
fn read(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "read";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector_to_list(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector->list";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_source_info_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-source-info!";
    panic!("{}({}) not implemented", name, args.len());
}
fn call_process(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%call-process";
    panic!("{}({}) not implemented", name, args.len());
}
fn confstr(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%confstr";
    panic!("{}({}) not implemented", name, args.len());
}
fn dup(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%dup";
    panic!("{}({}) not implemented", name, args.len());
}
fn start_process(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%start-process";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_closure_name(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%get-closure-name";
    panic!("{}({}) not implemented", name, args.len());
}
fn append(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "append";
    panic!("{}({}) not implemented", name, args.len());
}
fn append2(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "append2";
    panic!("{}({}) not implemented", name, args.len());
}
fn append_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "append!";
    match args {
        &[] => Object::Nil,
        _ => {
            let mut ret = args[args.len() - 1];
            let mut i = args.len() as isize - 2;
            loop {
                if i < 0 {
                    break;
                }
                if !args[i as usize].is_list() {
                    panic!("{}: list required but got {}", name, args[i as usize]);
                }
                ret = Pair::append_destructive(args[i as usize], ret);
                i = i - 1;
            }
            ret
        }
    }
}
fn pass3_find_free(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pass3/find-free";
    panic!("{}({}) not implemented", name, args.len());
}
fn pass3_find_sets(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pass3/find-sets";
    panic!("{}({}) not implemented", name, args.len());
}
fn pass4_fixup_labels(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pass4/fixup-labels";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_code_builder(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-code-builder";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra1_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-extra1!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra2_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-extra2!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra3_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-extra3!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra4_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-extra4!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_extra5_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-extra5!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_append_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-append!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_emit(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-emit";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg0_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-insn-arg0!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg1_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-insn-arg1!";
    panic!("{}({}) not implemented", name, args.len());
}
fn code_builder_put_insn_arg2_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "code-builder-put-insn-arg2!";
    panic!("{}({}) not implemented", name, args.len());
}
fn length(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "length";
    panic!("{}({}) not implemented", name, args.len());
}
fn list_to_vector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "list->vector";
    panic!("{}({}) not implemented", name, args.len());
}
fn pass3_compile_refer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pass3/compile-refer";
    panic!("{}({}) not implemented", name, args.len());
}
fn pass1_find_symbol_in_lvars(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pass1/find-symbol-in-lvars";
    panic!("{}({}) not implemented", name, args.len());
}
fn label(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "$label";
    panic!("{}({}) not implemented", name, args.len());
}
fn local_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "$local-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn list_transposeadd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "list-transpose+";
    panic!("{}({}) not implemented", name, args.len());
}
fn symbol_value(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "symbol-value";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_symbol_value_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-symbol-value!";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_hashtable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-hashtable";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_hashtable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(_) => Object::True,
        _ => Object::False,
    }
}
fn hashtable_size(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-size";
    check_argc!(name, args, 1);
    match args[0] {
        Object::EqHashtable(hashtable) => Object::Number(hashtable.size() as isize),
        _ => {
            panic!("{}: hashtable required but got {:?}", name, args)
        }
    }
}
fn hashtable_delete_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-delete!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_hashtable_contains(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-contains?";
    panic!("{}({}) not implemented", name, args.len());
}
fn hashtable_copy(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-copy";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_hashtable_mutable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-mutable?";
    panic!("{}({}) not implemented", name, args.len());
}
fn hashtable_clear_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-clear!";
    panic!("{}({}) not implemented", name, args.len());
}

fn hashtable_equivalence_function(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-equivalence-function";
    panic!("{}({}) not implemented", name, args.len());
}
fn hashtable_hash_function(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "hashtable-hash-function";
    panic!("{}({}) not implemented", name, args.len());
}
fn throw(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "throw";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_lt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "<";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_le(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "<=";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_gt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = ">";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_ge(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = ">=";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_eq(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "=";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_add(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "+";
    panic!("{}({}) not implemented", name, args.len());
}
fn nuber_sub(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "-";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_mul(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "*";
    panic!("{}({}) not implemented", name, args.len());
}
fn number_div(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "/";
    panic!("{}({}) not implemented", name, args.len());
}
fn max(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "max";
    panic!("{}({}) not implemented", name, args.len());
}
fn min(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "min";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn lookahead_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "lookahead-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_string_n(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-string-n";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_string_n_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-string-n!";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_string_all(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-string-all";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_line(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-line";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_datum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-datum";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector?";
    panic!("{}({}) not implemented", name, args.len());
}
fn current_directory(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "current-directory";
    panic!("{}({}) not implemented", name, args.len());
}
fn standard_library_path(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "standard-library-path";
    panic!("{}({}) not implemented", name, args.len());
}
fn native_endianness(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "native-endianness";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-bytevector";
    panic!("{}({}) not implemented", name, args.len());
}

fn is_bytevectorequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_fill_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-fill!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_copy_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-copy!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_copy(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-copy";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u8_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u8-ref";
    panic!("{}({}) not implemented", name, args.len());
}

fn bytevector_s8_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s8-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s8_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s8-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_to_u8_list(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector->u8-list";
    panic!("{}({}) not implemented", name, args.len());
}
fn u8_list_to_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "u8-list->bytevector";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u16-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s16_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s16-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u16-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s16_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s16-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u16-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s16_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s16-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u16_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u16-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s16_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s16-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u32-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s32-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u32-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s32-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u32-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s32-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u32_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u32-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s32_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s32-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u64-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s64-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u64-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s64-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u64-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s64-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_u64_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-u64-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_s64_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-s64-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_to_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->bytevector";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_to_utf8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->utf8";
    panic!("{}({}) not implemented", name, args.len());
}
fn utf8_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "utf8->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn null_terminated_bytevector_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "null-terminated-bytevector->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn null_terminated_utf8_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "null-terminated-utf8->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_to_utf16(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->utf16";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_to_utf32(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string->utf32";
    panic!("{}({}) not implemented", name, args.len());
}
fn utf16_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "utf16->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn utf32_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "utf32->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn close_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "close-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_instruction(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-instruction";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_compiler_instruction(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-compiler-instruction";
    panic!("{}({}) not implemented", name, args.len());
}
fn fasl_write(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fasl-write";
    panic!("{}({}) not implemented", name, args.len());
}
fn fasl_read(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fasl-read";
    panic!("{}({}) not implemented", name, args.len());
}

fn is_rational(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rational?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flonum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flonum?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fixnum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fixnum?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_bignum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bignum?";
    panic!("{}({}) not implemented", name, args.len());
}
fn fixnum_width(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fixnum-width";
    panic!("{}({}) not implemented", name, args.len());
}
fn least_fixnum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "least-fixnum";
    panic!("{}({}) not implemented", name, args.len());
}
fn greatest_fixnum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "greatest-fixnum";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_rectangular(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-rectangular";
    panic!("{}({}) not implemented", name, args.len());
}
fn real_part(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "real-part";
    panic!("{}({}) not implemented", name, args.len());
}
fn imag_part(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "imag-part";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_exact(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "exact?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_inexact(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "inexact?";
    panic!("{}({}) not implemented", name, args.len());
}
fn exact(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "exact";
    panic!("{}({}) not implemented", name, args.len());
}
fn inexact(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "inexact";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_nan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "nan?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_infinite(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "infinite?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_finite(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "finite?";
    panic!("{}({}) not implemented", name, args.len());
}
fn real_to_flonum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "real->flonum";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fllt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl<?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flgt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl>?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flge(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl>=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flle(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl<=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flinteger(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flinteger?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flzero(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flzero?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flpositive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flpositive?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flnegative(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flnegative?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flodd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flodd?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fleven(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fleven?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flfinite(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flfinite?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flinfinite(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flinfinite?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_flnan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flnan?";
    panic!("{}({}) not implemented", name, args.len());
}
fn flmax(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flmax";
    panic!("{}({}) not implemented", name, args.len());
}
fn flmin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flmin";
    panic!("{}({}) not implemented", name, args.len());
}
fn fladd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl+";
    panic!("{}({}) not implemented", name, args.len());
}
fn flmul(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl*";
    panic!("{}({}) not implemented", name, args.len());
}
fn flsub(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl-";
    panic!("{}({}) not implemented", name, args.len());
}
fn fldiv_op(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fl/";
    panic!("{}({}) not implemented", name, args.len());
}
fn flabs(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flabs";
    panic!("{}({}) not implemented", name, args.len());
}
fn fldiv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fldiv";
    panic!("{}({}) not implemented", name, args.len());
}
fn flmod(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flmod";
    panic!("{}({}) not implemented", name, args.len());
}
fn fldiv0(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fldiv0";
    panic!("{}({}) not implemented", name, args.len());
}
fn flmod0(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flmod0";
    panic!("{}({}) not implemented", name, args.len());
}
fn flnumerator(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flnumerator";
    panic!("{}({}) not implemented", name, args.len());
}
fn fldenominator(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fldenominator";
    panic!("{}({}) not implemented", name, args.len());
}
fn flfloor(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flfloor";
    panic!("{}({}) not implemented", name, args.len());
}
fn flceiling(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flceiling";
    panic!("{}({}) not implemented", name, args.len());
}
fn fltruncate(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fltruncate";
    panic!("{}({}) not implemented", name, args.len());
}
fn flround(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flround";
    panic!("{}({}) not implemented", name, args.len());
}
fn flexp(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flexp";
    panic!("{}({}) not implemented", name, args.len());
}
fn fllog(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fllog";
    panic!("{}({}) not implemented", name, args.len());
}
fn flsin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flsin";
    panic!("{}({}) not implemented", name, args.len());
}
fn flcos(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flcos";
    panic!("{}({}) not implemented", name, args.len());
}
fn fltan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fltan";
    panic!("{}({}) not implemented", name, args.len());
}
fn flasin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flasin";
    panic!("{}({}) not implemented", name, args.len());
}
fn flacos(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flacos";
    panic!("{}({}) not implemented", name, args.len());
}
fn flatan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flatan";
    panic!("{}({}) not implemented", name, args.len());
}
fn flsqrt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flsqrt";
    panic!("{}({}) not implemented", name, args.len());
}
fn flexpt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "flexpt";
    panic!("{}({}) not implemented", name, args.len());
}
fn fixnum_to_flonum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fixnum->flonum";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_not(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-not";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_and(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-and";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_ior(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-ior";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_xor(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-xor";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_bit_count(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-bit-count";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_length(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-length";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_first_bit_set(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-first-bit-set";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_arithmetic_shift_left(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-arithmetic-shift-left";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_arithmetic_shift_right(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-arithmetic-shift-right";
    panic!("{}({}) not implemented", name, args.len());
}
fn bitwise_arithmetic_shift(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bitwise-arithmetic-shift";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_complex(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "complex?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_real(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "real?";
    panic!("{}({}) not implemented", name, args.len());
}

fn is_integer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "integer?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_real_valued(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "real-valued?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_rational_valued(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rational-valued?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_integer_valued(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "integer-valued?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxequal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxgt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx>?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxlt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx<?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxge(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx>=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxle(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx<=?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxzero(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxzero?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxpositive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxpositive?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxnegative(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxnegative?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxodd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxodd?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxeven(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxeven?";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxmax(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxmax";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxmin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxmin";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxadd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx+";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxmul(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx*";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxsub(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fx-";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxdiv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxdiv";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxmod(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxmod";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxdiv0(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxdiv0";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxmod0(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxmod0";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxnot(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxnot";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxand(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxand";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxior(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxior";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxxor(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxxor";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxif(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxif";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxbit_count(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxbit-count";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxlength(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxlength";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxfirst_bit_set(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxfirst-bit-set";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fxbit_set(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxbit-set?";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxcopy_bit(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxcopy-bit";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxbit_field(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxbit-field";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxcopy_bit_field(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxcopy-bit-field";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxarithmetic_shift(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxarithmetic-shift";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxarithmetic_shift_left(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxarithmetic-shift-left";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxarithmetic_shift_right(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxarithmetic-shift-right";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxrotate_bit_field(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxrotate-bit-field";
    panic!("{}({}) not implemented", name, args.len());
}
fn fxreverse_bit_field(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fxreverse-bit-field";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_single_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-single-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_single_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-single-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_double_native_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-double-native-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_double_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-double-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_single_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-single-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_single_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-single-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_double_native_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-double-native-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_ieee_double_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-ieee-double-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_even(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "even?";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Number(n) => Object::make_bool(n % 2 == 0),
        _ => {
            panic!("{}: required number but got {}", name, args[0]);
        }
    }
}
fn is_odd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "odd?";
    panic!("{}({}) not implemented", name, args.len());
}
fn abs(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "abs";
    check_argc!(name, args, 1);
    match args[0] {
        Object::Number(n) => Object::Number(n.abs()),
        _ => {
            panic!("{}: number required but got {}", name, args[0])
        }
    }
}
fn div(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "div";
    check_argc!(name, args, 2);
    match args {
        [Object::Number(_), Object::Number(0)] => {
            panic!("{}: division by zero", name)
        }
        [Object::Number(x), Object::Number(y)] => {
            let ret;
            if *x == 0 {
                ret = 0;
            } else if *x > 0 {
                ret = *x / *y;
            } else if *y > 0 {
                ret = (*x - *y + 1) / *y;
            } else {
                ret = (*x + *y + 1) / *y;
            }
            return Object::Number(ret);
        }
        _ => {
            panic!("{}: numbers required but got {:?}", name, args)
        }
    }
}
fn div0(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "div0";
    panic!("{}({}) not implemented", name, args.len());
}
fn numerator(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "numerator";
    panic!("{}({}) not implemented", name, args.len());
}
fn denominator(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "denominator";
    panic!("{}({}) not implemented", name, args.len());
}
fn floor(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "floor";
    panic!("{}({}) not implemented", name, args.len());
}
fn ceiling(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "ceiling";
    panic!("{}({}) not implemented", name, args.len());
}
fn truncate(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "truncate";
    panic!("{}({}) not implemented", name, args.len());
}
fn round(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "round";
    panic!("{}({}) not implemented", name, args.len());
}
fn exp(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "exp";
    panic!("{}({}) not implemented", name, args.len());
}
fn log(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "log";
    panic!("{}({}) not implemented", name, args.len());
}
fn sin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sin";
    panic!("{}({}) not implemented", name, args.len());
}
fn cos(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "cos";
    panic!("{}({}) not implemented", name, args.len());
}
fn tan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "tan";
    panic!("{}({}) not implemented", name, args.len());
}
fn asin(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "asin";
    panic!("{}({}) not implemented", name, args.len());
}
fn acos(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "acos";
    panic!("{}({}) not implemented", name, args.len());
}
fn sqrt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sqrt";
    panic!("{}({}) not implemented", name, args.len());
}
fn magnitude(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "magnitude";
    panic!("{}({}) not implemented", name, args.len());
}
fn angle(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "angle";
    panic!("{}({}) not implemented", name, args.len());
}
fn atan(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "atan";
    panic!("{}({}) not implemented", name, args.len());
}
fn expt(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "expt";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_polar(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-polar";
    panic!("{}({}) not implemented", name, args.len());
}
fn string_copy(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "string-copy";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector_fill_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector-fill!";
    panic!("{}({}) not implemented", name, args.len());
}
fn ungensym(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "ungensym";
    panic!("{}({}) not implemented", name, args.len());
}
fn disasm(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "disasm";
    panic!("{}({}) not implemented", name, args.len());
}
fn print_stack(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "print-stack";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_fast_equal(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "fast-equal?";
    panic!("{}({}) not implemented", name, args.len());
}
fn native_eol_style(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "native-eol-style";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_buffer_mode(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "buffer-mode?";
    panic!("{}({}) not implemented", name, args.len());
}
fn microseconds(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "microseconds";
    panic!("{}({}) not implemented", name, args.len());
}
fn local_tz_offset(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "local-tz-offset";
    panic!("{}({}) not implemented", name, args.len());
}
fn fork(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%fork";
    panic!("{}({}) not implemented", name, args.len());
}
fn exec(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%exec";
    panic!("{}({}) not implemented", name, args.len());
}
fn waitpid(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%waitpid";
    panic!("{}({}) not implemented", name, args.len());
}
fn pipe(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%pipe";
    panic!("{}({}) not implemented", name, args.len());
}
fn getpid(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%getpid";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_current_directory_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-current-directory!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_binary_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "binary-port?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "input-port?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_port_eof(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-eof?";
    panic!("{}({}) not implemented", name, args.len());
}
fn lookahead_u8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "lookahead-u8";
    panic!("{}({}) not implemented", name, args.len());
}
fn open_bytevector_input_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-bytevector-input-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_open(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-open";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_lookup(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-lookup";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_call(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-call";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_ffi_supported(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-supported?";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_malloc(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-malloc";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_free(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-free";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_make_c_callback_trampoline(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-make-c-callback-trampoline";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_free_c_callback_trampoline(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-free-c-callback-trampoline";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_close(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-close";
    panic!("{}({}) not implemented", name, args.len());
}
fn ffi_error(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%ffi-error";
    panic!("{}({}) not implemented", name, args.len());
}
fn host_os(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "host-os";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "output-port?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_textual_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "textual-port?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port?";
    panic!("{}({}) not implemented", name, args.len());
}
fn port_transcoder(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-transcoder";
    panic!("{}({}) not implemented", name, args.len());
}
fn native_transcoder(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "native-transcoder";
    panic!("{}({}) not implemented", name, args.len());
}
fn put_bytevector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "put-bytevector";
    panic!("{}({}) not implemented", name, args.len());
}
fn put_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "put-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn write_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "write-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn transcoder_codec(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "transcoder-codec";
    panic!("{}({}) not implemented", name, args.len());
}
fn transcoder_eol_style(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "transcoder-eol-style";
    panic!("{}({}) not implemented", name, args.len());
}
fn transcoder_error_handling_mode(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "transcoder-error-handling-mode";
    panic!("{}({}) not implemented", name, args.len());
}
fn quotient(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "quotient";
    panic!("{}({}) not implemented", name, args.len());
}
fn remainder(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "remainder";
    panic!("{}({}) not implemented", name, args.len());
}
fn modulo(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "modulo";
    panic!("{}({}) not implemented", name, args.len());
}
fn open_file_input_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "open-file-input/output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_binary_input_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-binary-input/output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_custom_textual_input_output_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-custom-textual-input/output-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn put_datum(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "put-datum";
    panic!("{}({}) not implemented", name, args.len());
}
fn list_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "list-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn list_tail(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "list-tail";
    panic!("{}({}) not implemented", name, args.len());
}
fn time_usage(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "time-usage";
    panic!("{}({}) not implemented", name, args.len());
}
fn mosh_executable_path(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "mosh-executable-path";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_socket(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket?";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_accept(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-accept";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_client_socket(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-client-socket";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_server_socket(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-server-socket";
    panic!("{}({}) not implemented", name, args.len());
}
fn os_constant(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "os-constant";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_recv(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-recv";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_recv_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-recv!";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_send(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-send";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_close(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-close";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_shutdown(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-shutdown";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_port(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-port";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_vm(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-vm";
    panic!("{}({}) not implemented", name, args.len());
}
fn vm_start_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm-start!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_vm(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm?";
    panic!("{}({}) not implemented", name, args.len());
}
fn vm_set_value_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm-set-value!";
    panic!("{}({}) not implemented", name, args.len());
}
fn vm_join_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm-join!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_main_vm(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "main-vm?";
    panic!("{}({}) not implemented", name, args.len());
}
fn vm_self(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vm-self";
    panic!("{}({}) not implemented", name, args.len());
}
fn register(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "register";
    panic!("{}({}) not implemented", name, args.len());
}
fn whereis(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "whereis";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_condition_variable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-condition-variable";
    panic!("{}({}) not implemented", name, args.len());
}
fn condition_variable_wait_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "condition-variable-wait!";
    panic!("{}({}) not implemented", name, args.len());
}
fn condition_variable_notify_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "condition-variable-notify!";
    panic!("{}({}) not implemented", name, args.len());
}
fn condition_variable_notify_all_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "condition-variable-notify-all!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_mutex(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "mutex?";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_mutex(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-mutex";
    panic!("{}({}) not implemented", name, args.len());
}
fn mutex_lock_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "mutex-lock!";
    panic!("{}({}) not implemented", name, args.len());
}
fn mutex_try_lock_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "mutex-try-lock!";
    panic!("{}({}) not implemented", name, args.len());
}
fn mutex_unlock_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "mutex-unlock!";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_vector(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-vector";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector_length(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector-length";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn vector_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "vector-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn create_directory(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "create-directory";
    panic!("{}({}) not implemented", name, args.len());
}
fn delete_directory(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "delete-directory";
    panic!("{}({}) not implemented", name, args.len());
}
fn rename_file(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "rename-file";
    panic!("{}({}) not implemented", name, args.len());
}
fn create_symbolic_link(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "create-symbolic-link";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_directory(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-directory?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_symbolic_link(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-symbolic-link?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_regular(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-regular?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_readable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-readable?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_executable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-executable?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_file_writable(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-writable?";
    panic!("{}({}) not implemented", name, args.len());
}
fn file_size_in_bytes(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-size-in-bytes";
    panic!("{}({}) not implemented", name, args.len());
}
fn file_stat_mtime(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-stat-mtime";
    panic!("{}({}) not implemented", name, args.len());
}
fn file_stat_atime(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-stat-atime";
    panic!("{}({}) not implemented", name, args.len());
}
fn file_stat_ctime(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file-stat-ctime";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_pointer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer?";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_to_integer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer->integer";
    panic!("{}({}) not implemented", name, args.len());
}
fn integer_to_pointer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "integer->pointer";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-uint8";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint16(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-uint16";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint32(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-uint32";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_uint64(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-uint64";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int8(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-int8";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int16(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-int16";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int32(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-int32";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_int64(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-int64";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-signed-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_char(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-unsigned-char";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_short(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-signed-short";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_short(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-unsigned-short";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_int(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-signed-int";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_int(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-unsigned-int";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_long(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-signed-long";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_long(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-unsigned-long";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_signed_long_long(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-signed-long-long";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_unsigned_long_long(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-unsigned-long-long";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_float(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-float";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_double(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-double";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_ref_c_pointer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-ref-c-pointer";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int8_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-int8!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int16_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-int16!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int32_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-int32!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int64_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-int64!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint8_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-uint8!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint16_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-uint16!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint32_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-uint32!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_uint64_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-uint64!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_char_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-char!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_short_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-short!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_int_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-int!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_long_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-long!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_long_long_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-long-long!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_float_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-float!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_double_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-double!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_set_c_pointer_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-set-c-pointer!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_copy_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer-copy!";
    panic!("{}({}) not implemented", name, args.len());
}
fn bytevector_pointer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "bytevector-pointer";
    panic!("{}({}) not implemented", name, args.len());
}
fn shared_errno(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "shared-errno";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_simple_struct(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "simple-struct?";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_simple_struct(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-simple-struct";
    panic!("{}({}) not implemented", name, args.len());
}
fn simple_struct_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "simple-struct-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn simple_struct_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "simple-struct-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn simple_struct_name(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "simple-struct-name";
    panic!("{}({}) not implemented", name, args.len());
}
fn lookup_nongenerative_rtd(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "lookup-nongenerative-rtd";
    panic!("{}({}) not implemented", name, args.len());
}
fn nongenerative_rtd_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "nongenerative-rtd-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_same_marksmul(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "same-marks*?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_same_marks(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "same-marks?";
    panic!("{}({}) not implemented", name, args.len());
}
fn id_to_real_label(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "id->real-label";
    panic!("{}({}) not implemented", name, args.len());
}
fn join_wraps(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "join-wraps";
    panic!("{}({}) not implemented", name, args.len());
}
fn gensym_prefix_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "gensym-prefix-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn current_dynamic_winders(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "current-dynamic-winders";
    panic!("{}({}) not implemented", name, args.len());
}
fn sexp_map(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sexp-map";
    panic!("{}({}) not implemented", name, args.len());
}
fn sexp_map_debug(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "sexp-map/debug";
    panic!("{}({}) not implemented", name, args.len());
}
fn write_ss(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "write/ss";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_message_send(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-message-send";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_name_whereis(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-name-whereis";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_message_receive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-message-receive";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_name_add_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-name-add!";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_message_send_receive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-message-send-receive";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_message_reply(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-message-reply";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_make_stream(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-make-stream";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_handle(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-stream-handle";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_write(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-stream-write";
    panic!("{}({}) not implemented", name, args.len());
}
fn monapi_stream_read(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "%monapi-stream-read";
    panic!("{}({}) not implemented", name, args.len());
}
fn process_list(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "process-list";
    panic!("{}({}) not implemented", name, args.len());
}
fn process_terminate_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "process-terminate!";
    panic!("{}({}) not implemented", name, args.len());
}
fn socket_sslize_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "socket-sslize!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_ssl_socket(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "ssl-socket?";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_ssl_supported(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "ssl-supported?";
    panic!("{}({}) not implemented", name, args.len());
}
fn file_to_string(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "file->string";
    panic!("{}({}) not implemented", name, args.len());
}
fn annotated_cons(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "annotated-cons";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_annotated_pair(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "annotated-pair?";
    panic!("{}({}) not implemented", name, args.len());
}
fn get_annotation(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "get-annotation";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_annotation_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-annotation!";
    panic!("{}({}) not implemented", name, args.len());
}
fn pointer_to_object(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "pointer->object";
    panic!("{}({}) not implemented", name, args.len());
}
fn object_to_pointer(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "object->pointer";
    panic!("{}({}) not implemented", name, args.len());
}
fn set_current_error_port_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "set-current-error-port!";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_port_open(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "port-open?";
    panic!("{}({}) not implemented", name, args.len());
}
fn make_f64array(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "make-f64array";
    panic!("{}({}) not implemented", name, args.len());
}
fn is_f64array(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "f64array?";
    panic!("{}({}) not implemented", name, args.len());
}
fn f64array_ref(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "f64array-ref";
    panic!("{}({}) not implemented", name, args.len());
}
fn f64array_set_destructive(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "f64array-set!";
    panic!("{}({}) not implemented", name, args.len());
}
fn f64array_shape(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "f64array-shape";
    panic!("{}({}) not implemented", name, args.len());
}
fn f64array_dot_product(_vm: &mut Vm, args: &[Object]) -> Object {
    let name: &str = "f64array-dot-product";
    panic!("{}({}) not implemented", name, args.len());
}
