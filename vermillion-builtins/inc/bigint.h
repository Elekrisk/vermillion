#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef void* BigIntPtr;

#define CMP_LT 0
#define CMP_EQ 1
#define CMP_GT 2

BigIntPtr bigint_create_unsigned(uint64_t value);
BigIntPtr bigint_create_signed(int64_t value_signed);
BigIntPtr bigint_create_raw(bool negative, uint32_t length, uint32_t *const digits);
void bigint_inc_ref(BigIntPtr ptr);
void bigint_destroy(BigIntPtr ptr);
void bigint_drop(BigIntPtr ptr);
BigIntPtr bigint_canonicalize(BigIntPtr ptr);

bool bigint_gt(BigIntPtr left, BigIntPtr right);
bool bigint_gteq(BigIntPtr left, BigIntPtr right);
bool bigint_lt(BigIntPtr left, BigIntPtr right);
bool bigint_lteq(BigIntPtr left, BigIntPtr right);
bool bigint_eq(BigIntPtr left, BigIntPtr right);
bool bigint_neq(BigIntPtr left, BigIntPtr right);

BigIntPtr bigint_shr_u64(BigIntPtr left, uint64_t right);
BigIntPtr bigint_shr(BigIntPtr left, BigIntPtr right);
BigIntPtr bigint_shl(BigIntPtr left, BigIntPtr right);

BigIntPtr bigint_add_unsigned(BigIntPtr left, BigIntPtr right);
BigIntPtr bigint_add(BigIntPtr left, BigIntPtr right);

BigIntPtr bigint_negate(BigIntPtr val);
BigIntPtr bigint_sub_positive(BigIntPtr left, BigIntPtr right);
BigIntPtr bigint_sub(BigIntPtr left, BigIntPtr right);

BigIntPtr bigint_mul(BigIntPtr left, BigIntPtr right);