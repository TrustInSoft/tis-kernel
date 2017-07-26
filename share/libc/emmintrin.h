/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file provides intrinsics for SSE2. */

#ifndef __FC_EMMINTRIN_H_INCLUDED
#define __FC_EMMINTRIN_H_INCLUDED

#ifndef __SSE2__
# error "SSE2 instruction set not enabled"
#else

#include <xmmintrin.h>

typedef long long int __m128i;

inline void __attribute__((always_inline))
_mm_lfence (void)
{
  __builtin_ia32_lfence ();
}

inline void __attribute__((always_inline))
_mm_mfence (void)
{
  __builtin_ia32_mfence ();
}

#endif /* __SSE2__  */

#endif
