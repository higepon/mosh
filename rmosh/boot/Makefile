TARGET=../src/compiler.rs

$(TARGET): ../../boot/compiler.scm
    # Generate compiler and base library for Rust.
	make -C ../../boot baselib-rust.scmc
	(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm ../boot/baselib-rust.scmc) > $@
