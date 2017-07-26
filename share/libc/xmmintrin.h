/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file provides intrinsics for SSE. */

#ifndef __FC_XMMINTRIN_H_INCLUDED
#define __FC_XMMINTRIN_H_INCLUDED

#ifndef __SSE__
# error "SSE instruction set not enabled"
#else

inline void __attribute__((always_inline))
_mm_pause (void)
{
  return;
}

inline void __attribute__((always_inline))
_mm_sfence (void)
{
  __builtin_ia32_sfence ();
}

#endif /* __SSE__ */
#endif
