/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#ifndef __TIS_BUILTIN_H
#define __TIS_BUILTIN_H

#ifdef	__cplusplus
// Do not include the TrustInSoft Kernel libc headers when compiling C++,
// because they do not fulfill the requirements of the C++ standard library.
# define __BEGIN_DECLS	extern "C" {
# define __END_DECLS	}
#include <cstddef>

#else

#include <features.h>
#include <__fc_define_size_t.h>

#endif


__BEGIN_DECLS

extern int tis_entropy_source;

/*@ requires \valid(p + (0 .. l-1));
    assigns p[0 .. l-1] \from tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures \initialized(p + (0 .. l-1));
*/
void tis_make_unknown(char *p, size_t l) __THROW;

/*@ assigns \result \from a, b, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures \result == a || \result == b;
 */
int tis_nondet(int a, int b) __THROW;

/*@ assigns \result \from a, b, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures \result == a || \result == b;
 */
void *tis_nondet_ptr(void *a, void *b) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
int tis_interval(int min, int max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max ;
 */
int tis_interval_split(int min, int max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
unsigned char tis_unsigned_char_interval(unsigned char min,
                                         unsigned char max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
char tis_char_interval(char min, char max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
unsigned short tis_unsigned_short_interval(unsigned short min,
                                           unsigned short max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
short tis_short_interval(short min, short max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
unsigned int tis_unsigned_int_interval(unsigned int min, unsigned int max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
int tis_int_interval(int min, int max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
unsigned long tis_unsigned_long_interval(unsigned long min,
                                         unsigned long max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
long tis_long_interval(long min, long max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
unsigned long long tis_unsigned_long_long_interval
     (unsigned long long min, unsigned long long max) __THROW;

/*@ requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures min <= \result <= max;
 */
long long tis_long_long_interval(long long min, long long max) __THROW;

/*@ requires \is_finite(min) && \is_finite(max);
    requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures \is_finite(\result) && min <= \result <= max;
 */
float tis_float_interval(float min, float max) __THROW;

/*@ requires \is_finite(min) && \is_finite(max);
    requires min <= max;
    assigns \result \from min, max, tis_entropy_source;
    assigns tis_entropy_source \from tis_entropy_source;
    ensures \is_finite(\result) && min <= \result <= max;
 */
double tis_double_interval(double min, double max) __THROW;

/*@ // Signals an error;
    requires \false;
    assigns \nothing;
*/
void tis_abort(void) __THROW __attribute__ ((noreturn));

/*@ assigns \nothing; */
void tis_watch_cardinal(void *p, size_t s, int maximal_cardinal, int n) __THROW;
/*@ assigns \nothing; */
void tis_watch_value(void *p, size_t s, int forbidden_value, int n) __THROW;
/*@ assigns \nothing; */
void tis_watch_address(void *p, size_t s, int n) __THROW;
/*@ assigns \nothing; */
void tis_watch_garbled(void *p, size_t s, int n) __THROW;

/*@ ghost extern int __fc_heap_status __attribute__((FRAMA_C_MODEL)); */
/*@ allocates \result;
    assigns __fc_heap_status \from size, __fc_heap_status;
    assigns \result \from size, __fc_heap_status;
*/
void *tis_alloc_size(size_t size) __THROW;
/*@ allocates \result;
    assigns __fc_heap_status \from size, __fc_heap_status;
    assigns \result \from size, __fc_heap_status;
*/
void *tis_alloc(size_t size) __THROW;

/*@ allocates \result;
    assigns __fc_heap_status \from size, __fc_heap_status;
    assigns \result \from size, __fc_heap_status;
*/
void *tis_alloc_weak(size_t size) __THROW;

/*@ assigns \result \from indirect:p ; */
size_t tis_block_size(const void *p) __THROW;

/*@ assigns __fc_heap_status \from indirect:p, __fc_heap_status;
*/
void tis_free(const void *p) __THROW;

/*@ assigns \nothing; */
void tis_variable_split(void *p, size_t s, int limit) __THROW;

/*@ assigns \result \from p;
    ensures \result == (void *)\base_addr(p);
*/
void *tis_base_addr(void *p) __THROW;

/*@ requires n != 0;
    requires \valid_read((char *)src1 + (0 .. n - 1));
    requires \valid_read((char *)src2 + (0 .. n - 1));
    assigns \nothing;
*/
void tis_check_included(const void *src1, size_t n, const void *src2) __THROW;

/*@ assigns \nothing; */
void tis_print_subexps(const char *description, ...) __THROW;

/*@ assigns \result \from p, start, end; */
int tis_ptr_is_within(void *p, void *start, void *end) __THROW;

/*@ assigns \result \from p1, p2; */
int tis_ptr_is_less_than(void *p1, void *p2) __THROW;

/*@ assigns \nothing; */
void tis_deps_show_deps(void) __THROW;

/*@ assigns \nothing; */
void tis_deps_show_pathdeps(void) __THROW;

/*@ assigns \nothing; */
void tis_deps_show_open_pathdeps(void) __THROW;

/*@ assigns \nothing; */
void tis_deps_show_file_generalizable_bytes(void) __THROW;

/*@ assigns \nothing; */
void tis_show_allocated(void) __THROW;

/*@ assigns \nothing; */
void tis_sa_show_each();

/*@ assigns \nothing; */
void tis_sa_dump_each(void);

__END_DECLS

#endif /* tis_builtin.h */
