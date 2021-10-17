#include "../inc/bigint.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>

// BigInt layout
//
// | Offset | Size | Data
// |--------|------|-----
// | 0x00   | 0x08 | Ref counter
// | 0x08   | 0x04 | Capacity
// | 0x0C   | 0x04 | Length
// | 0x10   | 0x04 | Sign (0: positive, 1: negative)
// | 0x14   | 4*n  | Base 2^32 digits

#define REF(ptr) ((uint64_t *)((ptr) + 0))
#define CAP(ptr) ((uint32_t *)((ptr) + 8))
#define LEN(ptr) ((uint32_t *)((ptr) + 12))
#define SGN(ptr) ((uint32_t *)((ptr) + 16))
#define DAT(ptr) ((uint32_t *)((ptr) + 20))

#define MAX(a, b) ((a) >= (b) ? (a) : (b))
#define MIN(a, b) ((a) >= (b) ? (b) : (a))

BigIntPtr bigint_create_unsigned(uint64_t value)
{
    bool is_64_bit = (value >> 32) > 0;
    size_t size = 20 + (is_64_bit ? 8 : 4);

    unsigned char *ptr = malloc(size);
    *(uint64_t *)ptr = 1;                        // Ref counter
    *(uint32_t *)(ptr + 8) = is_64_bit ? 2 : 1;  // Capacity
    *(uint32_t *)(ptr + 12) = is_64_bit ? 2 : 1; // Length
    *(uint32_t *)(ptr + 16) = 0;                 // Sign
    *(uint32_t *)(ptr + 20) = (uint32_t)value;
    if (is_64_bit)
    {
        *(uint32_t *)(ptr + 24) = (uint32_t)(value >> 32);
    }

    return ptr;
}

BigIntPtr bigint_create_signed(int64_t value_signed)
{
    uint64_t value = value_signed >= 0 ? (uint64_t)value_signed : (uint64_t)-value_signed;
    bool is_64_bit = (value >> 32) > 0;
    size_t size = 20 + sizeof(uint32_t) * (is_64_bit ? 1 : 2);

    unsigned char *ptr = malloc(size);
    *(uint64_t *)ptr = 1;                        // Ref counter
    *(uint32_t *)(ptr + 8) = is_64_bit ? 4 : 2;  // Capacity
    *(uint32_t *)(ptr + 12) = is_64_bit ? 2 : 1; // Length
    *(uint32_t *)(ptr + 16) = value_signed < 0;  // Sign
    *(uint32_t *)(ptr + 20) = (uint32_t)value;
    if (is_64_bit)
    {
        *(uint32_t *)(ptr + 16) = (uint32_t)(value >> 32);
    }

    return ptr;
}

BigIntPtr bigint_create_raw(bool negative, uint32_t length, uint32_t *const digits)
{
    size_t size = 20 + 4 * length;

    unsigned char *ptr = malloc(size);
    *(uint64_t *)ptr = 1;                       // Ref count
    *(uint64_t *)(ptr + 8) = length + 2;        // Capacity
    *(uint64_t *)(ptr + 12) = length;           // Length
    *(uint64_t *)(ptr + 16) = negative ? 1 : 0; // Sign
    if (digits == NULL)
    {
        memset(ptr + 20, 0, 4 * length);
    }
    else
    {
        memcpy(ptr + 20, digits, sizeof(uint32_t) * length);
    }

    return ptr;
}

void bigint_inc_ref(BigIntPtr ptr)
{
    *REF(ptr) += 1;
}

void bigint_destroy(BigIntPtr ptr)
{
    free(ptr);
}

void bigint_drop(BigIntPtr ptr)
{
    *REF(ptr) -= 1;
    if (*REF(ptr) == 0)
    {
        bigint_destroy(ptr);
    }
}

BigIntPtr bigint_clone(BigIntPtr ptr)
{
    BigIntPtr new = malloc(20 + 4 * *CAP(ptr));
    memcpy(new, ptr, 20 + 4 * *CAP(ptr));
    *REF(new) = 1;
    return new;
}

BigIntPtr bigint_canonicalize(BigIntPtr ptr)
{
    uint32_t length = *LEN(ptr);

    for (; length > 0; --length)
    {
        if (DAT(ptr)[length - 1] > 0)
        {
            break;
        }
    }

    *LEN(ptr) = length;

    if (*CAP(ptr) / 4 > length && *REF(ptr) == 1)
    {
        BigIntPtr new_ptr = malloc(20 + 4 * length);
        memcpy(new_ptr, ptr, 20 + 4 * length);
        *CAP(new_ptr) = length;
        bigint_destroy(ptr);
        return new_ptr;
    }
    else
    {
        return ptr;
    }
}

int bigint_cmp(BigIntPtr left, BigIntPtr right)
{
    int ret = CMP_EQ;
    if (*LEN(left) > *LEN(right))
    {
        ret = CMP_GT;
    }
    else if (*LEN(left) < *LEN(right))
    {
        ret = CMP_LT;
    }
    else
    {
        for (int i = *LEN(left) - 1; i >= 0; --i)
        {
            if (DAT(left)[i] > DAT(right)[i])
            {
                ret = CMP_GT;
                break;
            }
            else if (DAT(left)[i] < DAT(right)[i])
            {
                ret = CMP_LT;
                break;
            }
        }
    }
    bigint_drop(left);
    bigint_drop(right);
    return ret;
}

bool bigint_eq(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) == CMP_EQ;
}

bool bigint_neq(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) != CMP_EQ;
}

bool bigint_gt(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) == CMP_GT;
}

bool bigint_gteq(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) != CMP_LT;
}

bool bigint_lt(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) == CMP_LT;
}

bool bigint_lteq(BigIntPtr left, BigIntPtr right)
{
    return bigint_cmp(left, right) != CMP_GT;
}

BigIntPtr bigint_shr_u64(BigIntPtr left, uint64_t right)
{
    uint32_t digits = right / 32;
    uint8_t bits = right % 32;

    if (digits >= *LEN(left))
    {
        if (*REF(left) == 1)
        {
            *LEN(left) = 0;
            *SGN(left) = 0;
            return left;
        }
        else
        {
            BigIntPtr ret = bigint_create_raw(false, 0, NULL);
            return ret;
        }
    }

    uint32_t needed_size = *LEN(left) - digits;

    BigIntPtr ret;
    bool reused_left;

    if (*REF(left) == 1)
    {
        ret = left;
        reused_left = true;
    }
    else
    {
        ret = bigint_create_raw(false, needed_size, NULL);
        reused_left = false;
    }

    memmove(DAT(ret), DAT(left) + digits, (*LEN(left) - digits) * 4);
    *LEN(ret) = *LEN(left) - digits;

    uint32_t carry_mask = (1 << bits) - 1;

    uint32_t carry = 0;
    for (long long i = 0; i < *LEN(ret); ++i)
    {
        uint32_t temp = DAT(ret)[i] >> bits;
        temp = temp | carry;
        carry = (DAT(ret)[i] & carry_mask) << (32 - bits);
        DAT(ret)
        [i] = temp;
    }

    if (!reused_left)
    {
        bigint_drop(left);
    }

    return bigint_canonicalize(ret);
}

uint64_t bigint_and_u64(BigIntPtr left, uint64_t right)
{
    uint64_t ret;
    switch (*LEN(left))
    {
    case 0:
        ret = 0;
        break;
    case 1:
        ret = DAT(left)[0];
        break;
    default:
        ret = (uint64_t)DAT(left)[0] | ((uint64_t)DAT(left)[1] << 32);
        break;
    }
    bigint_drop(left);
    return ret & right;
}

BigIntPtr bigint_shr(BigIntPtr left, BigIntPtr right)
{
    bigint_inc_ref(right);
    uint8_t bits = bigint_and_u64(right, 0x1F);
    BigIntPtr bi_digits = bigint_shr_u64(right, 5);

    uint32_t digits;
    switch (*LEN(bi_digits))
    {
    case 0:
        digits = 0;
        break;
    case 1:
        digits = DAT(bi_digits)[0];
        break;
    default:
        digits = UINT32_MAX;
        break;
    }
    bigint_drop(bi_digits);

    if (digits >= *LEN(left))
    {
        if (*REF(left) == 1)
        {
            *LEN(left) = 0;
            *SGN(left) = 0;
            return left;
        }
        else
        {
            BigIntPtr ret = bigint_create_raw(false, 0, NULL);
            return ret;
        }
    }

    uint32_t needed_size = *LEN(left) - digits;

    BigIntPtr ret;
    bool reused_left;

    if (*REF(left) == 1)
    {
        ret = left;
        reused_left = true;
    }
    else
    {
        ret = bigint_create_raw(false, needed_size, NULL);
        reused_left = false;
    }

    memmove(DAT(ret), DAT(left) + digits, (*LEN(left) - digits) * 4);
    *LEN(ret) = needed_size;

    uint32_t carry_mask = (1 << bits) - 1;

    uint32_t carry = 0;
    for (long long i = 0; i < *LEN(ret); ++i)
    {
        uint32_t temp = DAT(ret)[i] >> bits;
        temp = temp | carry;
        carry = (DAT(ret)[i] & carry_mask) << (32 - bits);
        DAT(ret)
        [i] = temp;
    }

    if (!reused_left)
    {
        bigint_drop(left);
    }

    return bigint_canonicalize(ret);
}

BigIntPtr bigint_shl_u64(BigIntPtr left, uint64_t right)
{
    uint32_t digits = right / 32;
    uint8_t bits = right % 32;

    uint32_t needed_size = *LEN(left) + digits + 1;

    BigIntPtr ret;
    bool reused_left;

    if (*REF(left) == 1 && *CAP(left) >= needed_size)
    {
        ret = left;
        reused_left = true;
        memset(DAT(ret), 0, 4 * digits);
    }
    else
    {
        ret = bigint_create_raw(false, needed_size, NULL);
        reused_left = false;
    }

    if (digits > 0 || !reused_left)
    {
        memmove(DAT(ret) + digits, DAT(left), *LEN(left) * 4);
    }

    *LEN(ret) = needed_size - 1;

    if (bits > 0)
    {
        uint32_t carry_mask = ((1 << bits) - 1) << (32 - bits);

        uint32_t carry = 0;
        for (long long i = digits; i < *LEN(ret); ++i)
        {
            uint32_t temp = DAT(ret)[i] << bits;
            temp = temp | carry;
            carry = (DAT(ret)[i] & carry_mask) >> (32 - bits);
            DAT(ret)
            [i] = temp;
        }

        if (carry) {
            DAT(ret)[*LEN(ret)] = carry;
            *LEN(ret) += 1;
        }
    }

    if (!reused_left)
    {
        bigint_drop(left);
    }

    return bigint_canonicalize(ret);
}

BigIntPtr bigint_shl(BigIntPtr left, BigIntPtr right)
{
    bigint_inc_ref(right);
    uint8_t bits = bigint_and_u64(right, 0x1F);
    BigIntPtr bi_digits = bigint_shr_u64(right, 5);

    uint32_t digits;
    switch (*LEN(bi_digits))
    {
    case 0:
        digits = 0;
        break;
    case 1:
        digits = DAT(bi_digits)[0];
        break;
    default:
        digits = UINT32_MAX;
        break;
    }
    bigint_drop(bi_digits);

    uint32_t needed_size = *LEN(left) + digits + 1;

    BigIntPtr ret;
    bool reused_left;

    if (*REF(left) == 1 && *CAP(left) >= needed_size)
    {
        ret = left;
        reused_left = true;
    }
    else
    {
        ret = bigint_create_raw(false, needed_size, NULL);
        reused_left = false;
    }

    memset(DAT(ret), 0, 4 * digits);
    memmove(DAT(ret) + digits, DAT(left), *LEN(left) * 4);
    *LEN(ret) = needed_size - 1;

    uint32_t carry_mask = ((1 << bits) - 1) << (32 - bits);

    uint32_t carry = 0;
    for (long long i = digits; i < *LEN(ret); ++i)
    {
        uint32_t temp = DAT(ret)[i] << bits;
        temp = temp | carry;
        carry = (DAT(ret)[i] & carry_mask) >> (32 - bits);
        DAT(ret)
        [i] = temp;
    }

    if (carry) {
        DAT(ret)[*LEN(ret)] = carry;
        *LEN(ret) += 1;
    }

    if (!reused_left)
    {
        bigint_drop(left);
    }

    return bigint_canonicalize(ret);
}

BigIntPtr bigint_add_unsigned(BigIntPtr left, BigIntPtr right)
{
    uint32_t max_len = MAX(*LEN(left), *LEN(right));
    uint32_t min_len = MIN(*LEN(left), *LEN(right));
    uint32_t digits = max_len + 1;
    if (digits == 0)
    {
        exit(1);
    }

    BigIntPtr largest = *LEN(left) >= *LEN(right) ? left : right;
    BigIntPtr other = *LEN(left) >= *LEN(right) ? right : left;
    BigIntPtr ret;
    bool reused_largest;
    if (*REF(largest) == 1 && *CAP(largest) >= digits)
    {
        ret = largest;
        reused_largest = true;
    }
    else
    {
        ret = bigint_create_raw(false, digits, NULL);
        *LEN(ret) -= 1;
        reused_largest = false;
    }

    uint64_t carry = 0;
    for (int i = 0; i < min_len; ++i)
    {
        uint64_t temp = (uint64_t)DAT(largest)[i] + (uint64_t)DAT(other)[i] + carry;
        carry = temp >> 32;
        DAT(ret)
        [i] = (uint32_t)temp;
    }

    for (int i = min_len; i < *LEN(largest); ++i)
    {
        uint64_t temp = (uint64_t)DAT(largest)[i] + carry;
        carry = temp >> 32;
        DAT(ret)
        [i] = (uint32_t)temp;
        if (reused_largest && carry == 0) {
            break;
        }
    }

    for (int i = min_len; i < *LEN(other); ++i)
    {
        uint64_t temp = (uint64_t)DAT(other)[i] + carry;
        carry = temp >> 32;
        DAT(ret)
        [i] = (uint32_t)temp;
        if (reused_largest && carry == 0) {
            break;
        }
    }

    if (carry)
    {
        DAT(ret)
        [*LEN(ret)] = carry;
        *LEN(ret) += 1;
    }

    if (!reused_largest)
    {
        bigint_drop(largest);
    }
    bigint_drop(other);
    return bigint_canonicalize(ret);
}

BigIntPtr bigint_add(BigIntPtr left, BigIntPtr right)
{
    if (*SGN(left) == *SGN(right))
    {
        uint32_t sign = *SGN(left);
        BigIntPtr ret = bigint_add_unsigned(left, right);
        *SGN(ret) = sign;
        return ret;
    }
    else
    {
        if (*SGN(left) == 1)
        {
            return bigint_sub_positive(right, bigint_negate(left));
        }
        else
        {
            return bigint_sub_positive(left, bigint_negate(right));
        }
    }
}

BigIntPtr bigint_negate(BigIntPtr val)
{
    if (*REF(val) == 1)
    {
        *SGN(val) = *SGN(val) == 0 ? 1 : 0;
        return val;
    }
    else
    {
        BigIntPtr ret = bigint_clone(val);

        *SGN(ret) = *SGN(ret) == 0 ? 1 : 0;
        return ret;
    }
}

BigIntPtr bigint_sub_positive(BigIntPtr left, BigIntPtr right)
{
    BigIntPtr larger;
    BigIntPtr smaller;
    BigIntPtr ret;
    bool reused_larger;
    bool switched;

    bigint_inc_ref(left);
    bigint_inc_ref(right);
    switch (bigint_cmp(left, right))
    {
    case CMP_GT:
        larger = left;
        smaller = right;
        switched = false;
        break;
    case CMP_LT:
        larger = right;
        smaller = left;
        switched = true;
        break;
    case CMP_EQ:
        if (*REF(left) == 1)
        {
            *LEN(left) = 0;
            *SGN(left) = 0;
            bigint_drop(right);
            return left;
        }
        else if (*REF(right) == 1)
        {
            *LEN(right) = 0;
            *SGN(right) = 0;
            bigint_drop(left);
            return right;
        }
        else
        {
            bigint_drop(left);
            bigint_drop(right);
            return bigint_create_raw(false, 0, NULL);
        }
    }

    if (*REF(larger) == 1)
    {
        ret = larger;
        reused_larger = true;
    }
    else
    {
        ret = bigint_create_raw(false, *LEN(larger), NULL);
    }

    uint64_t borrow = 0;
    for (int i = 0; i < *LEN(smaller); ++i)
    {
        uint64_t temp = (uint64_t)DAT(larger)[i] - (uint64_t)DAT(smaller)[i] - borrow;
        borrow = (temp >> 32) > 0 ? 1 : 0;
        DAT(ret)
        [i] = temp;
    }

    for (int i = *LEN(smaller); i < *LEN(larger); ++i)
    {
        uint64_t temp = (uint64_t)DAT(larger)[i] - borrow;
        borrow = (temp >> 32) > 0 ? 1 : 0;
        DAT(ret)
        [i] = temp;
        if (reused_larger && borrow == 0) {
            break;
        }
    }

    if (switched)
    {
        *SGN(ret) = 1;
    }
    if (!reused_larger)
    {
        bigint_drop(larger);
    }
    bigint_drop(smaller);
    return bigint_canonicalize(ret);
}

BigIntPtr bigint_sub(BigIntPtr left, BigIntPtr right)
{
    if (*SGN(left) == 0 && *SGN(right) == 0)
    {
        return bigint_sub_positive(left, right);
    }
    else if (*SGN(left) == 0 && *SGN(right) == 1)
    {
        return bigint_add_unsigned(left, bigint_negate(right));
    }
    else if (*SGN(left) == 1 && *SGN(right) == 0)
    {
        return bigint_negate(bigint_add_unsigned(bigint_negate(left), right));
    }
    else if (*SGN(left) == 1 && *SGN(right) == 1)
    {
        return bigint_negate(bigint_sub_positive(bigint_negate(left), bigint_negate(right)));
    }
    return NULL;
}

BigIntPtr bigint_mul_unsigned(BigIntPtr left, BigIntPtr right)
{
    BigIntPtr ret = bigint_create_raw(false, *LEN(left) + *LEN(right), NULL);
    *LEN(ret) = 0;

    if (*REF(left) > 1)
    {
        BigIntPtr temp = bigint_clone(left);
        bigint_drop(left);
        left = temp;
    }
    if (*REF(right) > 1)
    {
        BigIntPtr temp = bigint_clone(right);
        bigint_drop(right);
        right = temp;
    }

    uint32_t *last = DAT(left) + *LEN(left) - 1;
    for (uint32_t *didx = DAT(left); didx < last; ++didx)
    {
        uint32_t bidx = 32;
        for (uint32_t digit = *didx; digit > 0; bidx--, digit = digit >> 1 )
        {
            if (digit & 1)
            {
                bigint_inc_ref(right);
                ret = bigint_add_unsigned(ret, right);
            }
            right = bigint_shl_u64(right, 1);
        }
        if (bidx > 0) {
            right = bigint_shl_u64(right, bidx);
        }
    }
    
    for (uint32_t digit = *last; digit > 0; digit = digit >> 1)
    {
        if (digit & 1)
        {
            bigint_inc_ref(right);
            ret = bigint_add_unsigned(ret, right);
        }
        right = bigint_shl_u64(right, 1);
    }

    return ret;
}

BigIntPtr bigint_mul(BigIntPtr left, BigIntPtr right)
{

    if (*SGN(left) == *SGN(right))
    {
        return bigint_mul_unsigned(left, right);
    }
    else
    {
        return bigint_negate(bigint_mul_unsigned(bigint_negate(left), bigint_negate(right)));
    }
}
