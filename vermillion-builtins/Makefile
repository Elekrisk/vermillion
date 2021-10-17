SRC := $(shell find src -name '*.c')
OBJS := $(patsubst src/%.c, obj/%.ll, $(SRC))

all: builtins

builtins: builtins.ll
	clang builtins.ll -o builtins

builtins.ll: $(OBJS)
	llvm-link -S $^ -o $@
	#opt -O3 -S builtins.ll -o builtins.ll

obj/%.ll: src/%.c
	clang -g -O0 -S -emit-llvm $< -o $@

clean:
	-rm $(OBJS) builtins.ll