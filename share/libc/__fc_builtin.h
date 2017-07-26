/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*  TrustInSoft Kernel is a fork of Frama-C. All the differences are:     */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef Frama_C_BUILTIN
#define Frama_C_BUILTIN
#include "__fc_define_size_t.h"
#include "features.h"
__BEGIN_DECLS

extern int Frama_C_entropy_source;

/*@ requires \valid(p + (0 .. l-1));
    assigns p[0 .. l-1] \from Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \initialized(p + (0 .. l-1));
*/
void Frama_C_make_unknown(char *p, size_t l);

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \result == a || \result == b ;
 */
int Frama_C_nondet(int a, int b) __THROW;

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \result == a || \result == b ;
 */
void *Frama_C_nondet_ptr(void *a, void *b);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
int Frama_C_interval(int min, int max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
int Frama_C_interval_split(int min, int max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
unsigned char Frama_C_unsigned_char_interval
  (unsigned char min, unsigned char max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
char Frama_C_char_interval(char min, char max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
unsigned short Frama_C_unsigned_short_interval(unsigned short min, unsigned short max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
short Frama_C_short_interval(short min, short max);


/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
unsigned int Frama_C_unsigned_int_interval(unsigned int min, unsigned int max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
int Frama_C_int_interval(int min, int max);


/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
unsigned long Frama_C_unsigned_long_interval
     (unsigned long min, unsigned long max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
long Frama_C_long_interval(long min, long max);


/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
unsigned long long Frama_C_unsigned_long_long_interval
     (unsigned long long min, unsigned long long max);

/*@ requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures min <= \result <= max ;
 */
long long Frama_C_long_long_interval(long long min, long long max);


/*@ requires \is_finite(min) && \is_finite(max);
    requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \is_finite(\result) && min <= \result <= max ;
 */
float Frama_C_float_interval(float min, float max);

/*@ requires \is_finite(min) && \is_finite(max);
    requires min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures \is_finite(\result) && min <= \result <= max ;
 */
double Frama_C_double_interval(double min, double max);

/*@ // Signals an error;
  requires \false;
  assigns \nothing;
*/
void Frama_C_abort(void) __THROW __attribute__ ((noreturn));

void *Frama_C_alloc_size(size_t size);
__END_DECLS

#endif
