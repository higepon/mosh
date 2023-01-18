TARGET=../src/compiler.rs

$(TARGET): ../../boot/compiler.scm ../src/psyntax.rs
    # Generate compiler and base library for Rust.
	make -C ../../boot baselib-rust.scmc
	(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm ../boot/baselib-rust.scmc) > $@

../src/psyntax.rs: ../../boot/runtimes/psyntax-mosh/psyntax.scm
	make -C ../../boot psyntax-mosh-image.scmc
	(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm ../boot/psyntax-mosh-image.scmc) > $@	