#include <stdio.h>

#include "../inc/bigint.h"

typedef struct {
    uint64_t ref;
    uint32_t cap;
    uint32_t len;
    uint32_t sgn;
    uint32_t data[5];
} Value;

void __attribute__((optnone)) sink(int count, ...) {

}

int main() {
    BigIntPtr left = bigint_create_unsigned(50041228421);
    BigIntPtr right = bigint_create_unsigned(13142302135);
    BigIntPtr ret = bigint_mul(left, right);
    printf("");
    return 0;
}
