all:
	(cd .. && mosh --loadpath=scripts scripts/fasl_writer2.scm ../boot/baselib.scmc)  > 2.txt
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm boot/baselib.op) > 1.txt
	//(cd ../../boot && gosh vm.scm compile-file-with-macro-insn ../rmosh/boot/base.scm > ../rmosh/boot/base.op)
	//(cd ../../boot && gosh vm.scm compile-file-with-macro-insn ../boot/baselib.scm > ../rmosh/boot/baselib.op)	
	//(cd .. && mosh --loadpath=scripts scripts/gen_vm_lib.scm boot/base.op)
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm boot/base.op)
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm boot/base2.op)	
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm boot/baselib.op)	
	//(cd ../../boot && gosh vm.scm compile-file-with-macro-insn ../rmosh/boot/base.scm > ../rmosh/boot/base.op)
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer.scm boot/base.op)	> 1.txt
	//(cd ../../boot && gosh vm.scm compile-file-with-macro ../rmosh/boot/base.scm > ../rmosh/boot/base2.op)
	//(cd .. && mosh --loadpath=scripts scripts/fasl_writer2.scm boot/base2.op) > 2.txt
