#[macro_export]
macro_rules! as_bytevector {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_bytevector() {
            return error::Error::assertion_violation($name, "bytevector required", &[o]);
        }
        o.to_bytevector()
    }};
}

#[macro_export]
macro_rules! as_char {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_char() {
            return error::Error::assertion_violation($name, "char required", &[o]);
        }
        o.to_char()
    }};
}

#[macro_export]
macro_rules! as_sstring {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_string() {
            return error::Error::assertion_violation($name, "string required", &[o]);
        }
        o.to_sstring()
    }};
}

#[macro_export]
macro_rules! as_simple_struct {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_simple_struct() {
            return error::Error::assertion_violation($name, "simple_struct required", &[o]);
        }
        o.to_simple_struct()
    }};
}

#[macro_export]
macro_rules! as_symbol {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_symbol() {
            return error::Error::assertion_violation($name, "symbol required", &[o]);
        }
        o.to_symbol()
    }};
}

#[macro_export]
macro_rules! as_usize {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        o.to_isize() as usize
    }};
}

#[macro_export]
macro_rules! as_isize {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        o.to_isize()
    }};
}

#[macro_export]
macro_rules! as_f64 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum().value()
    }};
}

#[macro_export]
macro_rules! as_f32 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum().value() as f32
    }};
}

#[macro_export]
macro_rules! as_flonum {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum()
    }};
}

// For bytevectors.
#[macro_export]
macro_rules! as_u8 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        let fx = o.to_isize();
        if -128 <= fx && fx <= 255 {
            fx as u8
        } else {
            return error::Error::assertion_violation($name, "u8 value required", &[o]);
        }
    }};
}

#[macro_export]
macro_rules! as_transcoder {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_transcoder() {
            return error::Error::assertion_violation($name, "transcoder required", &[o]);
        }
        o.to_transcoder()
    }};
}

#[macro_export]
macro_rules! as_port {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::BinaryFileInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn Port
            },
            Object::CustomBinaryInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomBinaryInputOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::CustomBinaryOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomTextInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomTextInputOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::CustomTextOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::BinaryFileOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::BytevectorInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::BytevectorOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::StdErrorPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::StdInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::StdOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::StringInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::StringOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::FileInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::TranscodedInputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::TranscodedOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::TranscodedInputOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            Object::FileOutputPort(p) => unsafe { p.pointer.as_ref() as &dyn Port },
            _ => return error::Error::assertion_violation($name, "port required", &[o]),
        };
        port
    }};
}

#[macro_export]
macro_rules! as_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::BinaryFileInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn Port
            },
            Object::CustomBinaryInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomBinaryInputOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::CustomBinaryOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomTextInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::CustomTextInputOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::CustomTextOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },            
            Object::BinaryFileOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::BytevectorInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::BytevectorOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::StdErrorPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::StdInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::StdOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::StringInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::StringOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::FileInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::TranscodedInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::TranscodedOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn Port
            },
            Object::FileOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn Port },
            _ => return error::Error::assertion_violation($name, "port required", &[o]),
        };
        port
    }};
}

#[macro_export]
macro_rules! as_output_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::CustomBinaryOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            Object::CustomBinaryInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },            
            Object::CustomTextOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },  
            Object::CustomTextInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },                       
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            Object::BinaryFileOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            Object::BytevectorOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            Object::StdErrorPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn OutputPort },
            Object::StdOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn OutputPort },
            Object::StringOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn OutputPort },
            Object::FileOutputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn OutputPort },
            Object::TranscodedOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn OutputPort
            },
            _ => return error::Error::assertion_violation($name, "output port required", &[o]),
        };
        port
    }};
}

#[macro_export]
macro_rules! as_text_input_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = obj_as_text_input_port_mut!($name, o);
        port
    }};
}

#[macro_export]
macro_rules! obj_as_text_input_port_mut {
    ($name:ident, $obj:expr) => {{
        let port = match $obj {
            Object::CustomTextInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            Object::CustomTextInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },            
            Object::TranscodedInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            Object::StringInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            Object::FileInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn TextInputPort },
            _ => {
                return error::Error::assertion_violation(
                    $name,
                    "text input port required",
                    &[$obj],
                )
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! obj_as_text_output_port_mut {
    ($name:ident, $obj:expr) => {{
        let port = match $obj {
            Object::CustomTextOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },        
            Object::CustomTextInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },                 
            Object::StringOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::FileOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::TranscodedOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            _ => {
                return error::Error::assertion_violation(
                    $name,
                    "text output port required",
                    &[$obj],
                )
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! as_binary_input_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::CustomBinaryInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::CustomBinaryInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },            
            Object::BinaryFileInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::BytevectorInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::StdInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },            
            _ => {
                return error::Error::assertion_violation($name, "binary input port required", &[o])
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! obj_as_binary_input_port_mut_or_panic {
    ($obj:expr) => {{
        let port = match $obj {
            Object::CustomBinaryInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::CustomBinaryInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },            
            Object::BinaryFileInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::BytevectorInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },
            Object::StdInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryInputPort
            },            
            _ => {
                panic!("BUG: binary-input-port expected")
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! obj_as_text_input_port_mut_or_panic {
    ($obj:expr) => {{
        let port = match $obj {
            Object::FileInputPort(mut p) => unsafe { p.pointer.as_mut() as &mut dyn TextInputPort },
            Object::TranscodedInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            Object::CustomTextInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },            
            Object::CustomTextInputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },                 
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextInputPort
            },
            _ => {
                panic!("BUG: text-input-port expected")
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! as_binary_output_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::CustomBinaryInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },            
            Object::CustomBinaryOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BinaryFileOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BytevectorOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::StdOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },              
            Object::StdErrorPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },            
            _ => {
                return error::Error::assertion_violation(
                    $name,
                    "binary output port required",
                    &[o],
                )
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! obj_as_binary_output_port_mut_or_panic {
    ($obj:expr) => {{
        let port = match $obj {
            Object::CustomBinaryInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },            
            Object::CustomBinaryOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BinaryFileOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BinaryFileInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::BytevectorOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },
            Object::StdOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },              
            Object::StdErrorPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn BinaryOutputPort
            },            
            _ => {
                panic!("binary output port required but got {}", $obj);
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! as_text_output_port_mut {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        let port = match o {
            Object::TranscodedOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::TranscodedInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::CustomTextOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },            
            Object::CustomTextInputOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },                
            Object::StringOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            Object::FileOutputPort(mut p) => unsafe {
                p.pointer.as_mut() as &mut dyn TextOutputPort
            },
            _ => {
                return error::Error::assertion_violation($name, "text output port required", &[o])
            }
        };
        port
    }};
}

#[macro_export]
macro_rules! check_is_closure {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_closure() {
            return error::Error::assertion_violation($name, "procedure required", &[o]);
        }
        o
    }};
}

#[macro_export]
macro_rules! check_is_closure_or_false {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !(o.is_closure() || o.is_false()) {
            return error::Error::assertion_violation($name, "procedure or #f required", &[o]);
        }
        o
    }};
}
