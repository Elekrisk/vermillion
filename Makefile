
all: a.out

a.out: main.ll
	opt -O3 -S main.ll -o main-opt.ll
	clang main.ll -lrust -lpthread -ldl -lm -Lvermillion-runtime/rust/target/release -o a.out

a-opt.out: main.ll
	opt -O3 -S main.ll -o main-opt.ll
	clang main-opt.ll -lrust -lpthread -ldl -lm -Lvermillion-runtime/rust/target/release -o a-opt.out

main.ll: playground.verm
	./target/debug/vermillion
	opt --alloca-hoisting -S main.ll -o main.ll

run: a.out
	./a.out

run-opt: a-opt.out
	./a-opt.out

clean:
	-rm main.ll

build:
	cargo build
