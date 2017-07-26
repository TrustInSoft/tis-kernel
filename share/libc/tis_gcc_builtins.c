/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2014-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <stdint.h>

int16_t __sync_fetch_and_add_int16_t (int16_t *ptr, int16_t value, ...)
{
  int16_t tmp = *ptr;
  *ptr += value;
  return tmp;
}

int16_t __sync_fetch_and_sub_int16_t (int16_t *ptr, int16_t value, ...)
{
  int16_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int32_t __sync_fetch_and_add_int32_t (int32_t *ptr, int32_t value, ...)
{
  int32_t tmp = *ptr;
  *ptr += value;
  return tmp;
}

int32_t __sync_fetch_and_sub_int32_t (int32_t *ptr, int32_t value, ...)
{
  int32_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int64_t __sync_fetch_and_add_int64_t (int64_t *ptr, int64_t value, ...)
{
  int64_t tmp = *ptr;
  *ptr += value;
  return tmp;
}

int64_t __sync_fetch_and_sub_int64_t (int64_t *ptr, int64_t value, ...)
{
  int64_t tmp = *ptr;
  *ptr -= value;
  return tmp;
}

int16_t __sync_add_and_fetch_int16_t (int16_t *ptr, int16_t value, ...)
{
  *ptr += value;
  return *ptr;
}

int16_t __sync_sub_and_fetch_int16_t (int16_t *ptr, int16_t value, ...)
{
  *ptr -= value;
  return *ptr;
}

int32_t __sync_add_and_fetch_int32_t (int32_t *ptr, int32_t value, ...)
{
  *ptr += value;
  return *ptr;
}

int32_t __sync_sub_and_fetch_int32_t (int32_t *ptr, int32_t value, ...)
{
  *ptr -= value;
  return *ptr;
}

int64_t __sync_add_and_fetch_int64_t (int64_t *ptr, int64_t value, ...)
{
  *ptr += value;
  return *ptr;
}

int64_t __sync_sub_and_fetch_int64_t (int64_t *ptr, int64_t value, ...)
{
  *ptr -= value;
  return *ptr;
}

int32_t __sync_lock_test_and_set_int32_t (int32_t *ptr, int32_t value, ...)
{
  int32_t tmp = *ptr;
  *ptr = value;
  return tmp;
}

void __sync_lock_release_int32_t (int32_t *ptr, ...)
{
  *ptr = 0;
}

int __sync_bool_compare_and_swap_uint16_t (uint16_t *ptr, uint16_t oldval, uint16_t newval, ...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

int __sync_bool_compare_and_swap_uint32_t (uint32_t *ptr, uint32_t oldval, uint32_t newval, ...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

int __sync_bool_compare_and_swap_uint64_t (uint64_t *ptr, uint64_t oldval, uint64_t newval, ...)
{
   if (*ptr == oldval) {
     *ptr = newval;
     return 1;
   } else {
     return 0;
   }
}

void __builtin_ia32_lfence (void) {
  return;
}

void __builtin_ia32_mfence (void) {
  return;
}

void __builtin_ia32_sfence (void) {
  return;
}

uint32_t __builtin_bswap32 (uint32_t x) {
  return  ((x & 0x000000ffUL) << 24) |
    ((x & 0x0000ff00UL) << 8) |
    ((x & 0x00ff0000UL) >> 8) |
    ((x & 0xff000000UL) >> 24);
}


/*@
  requires TRAP: \false;
  assigns \nothing;
*/
void __builtin_trap() __attribute__((noreturn));

/*@
  requires UNREACHABLE: \false;
  assigns \nothing;
*/
void __builtin_unreachable() __attribute__((noreturn));
