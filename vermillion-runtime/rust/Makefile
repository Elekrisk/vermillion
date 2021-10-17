
all:
	cargo build

ir:
	-rm target/debug/deps/*.ll
	cargo rustc -- --emit=llvm-ir
	mv target/debug/deps/*.ll runtime.ll