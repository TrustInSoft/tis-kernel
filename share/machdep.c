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

/****************************************************************************/
/*                                                                          */
/*  Copyright (C) 2001-2003                                                 */
/*   George C. Necula    <necula@cs.berkeley.edu>                           */
/*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          */
/*   Wes Weimer          <weimer@cs.berkeley.edu>                           */
/*   Ben Liblit          <liblit@cs.berkeley.edu>                           */
/*  All rights reserved.                                                    */
/*                                                                          */
/*  Redistribution and use in source and binary forms, with or without      */
/*  modification, are permitted provided that the following conditions      */
/*  are met:                                                                */
/*                                                                          */
/*  1. Redistributions of source code must retain the above copyright       */
/*  notice, this list of conditions and the following disclaimer.           */
/*                                                                          */
/*  2. Redistributions in binary form must reproduce the above copyright    */
/*  notice, this list of conditions and the following disclaimer in the     */
/*  documentation and/or other materials provided with the distribution.    */
/*                                                                          */
/*  3. The names of the contributors may not be used to endorse or          */
/*  promote products derived from this software without specific prior      */
/*  written permission.                                                     */
/*                                                                          */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       */
/*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          */
/*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     */
/*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    */
/*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        */
/*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      */
/*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       */
/*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         */
/*  POSSIBILITY OF SUCH DAMAGE.                                             */
/*                                                                          */
/*  File modified by CEA (Commissariat à l'énergie atomique et aux          */
/*                        énergies alternatives)                            */
/*               and INRIA (Institut National de Recherche en Informatique  */
/*                          et Automatique).                                */
/****************************************************************************/

/* Example to build and run machdep.c:
     gcc machdep.c && ./a.out
     clang machdep.c && ./a.out
*/

#include <stdio.h>
#include <stddef.h>

/* Configure this file according to your machine dependency structure. */
#if 1
#define TYPE_SIZE_T "unsigned long"
#define TYPE_WCHAR_T "int"
#define TYPE_PTRDIFF_T "long"
#endif

#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#define COMPILER "other"

#ifdef __TURBOC__
#define LONGLONG long long
#define CONST_STRING_LITERALS "false"
#define VERSION __TURBOC__
#define VERSION_MAJOR 0
#define VERSION_MINOR 0
#endif

#ifdef __GNUC__
#define LONGLONG long long
#define CONST_STRING_LITERALS "true"
#define VERSION __VERSION__
#define VERSION_MAJOR __GNUC__
#define VERSION_MINOR __GNUC_MINOR__
#undef COMPILER
#define COMPILER "gcc"
#endif

#ifdef _MSVC
#define LONGLONG __int64
#define CONST_STRING_LITERALS "false"
#define VERSION "Microsoft C"
#define VERSION_MAJOR (_MSC_VER / 100)
#define VERSION_MINOR (_MSC_VER % 100)
#undef COMPILER
#define COMPILER "msvc"
#endif

#ifndef __TURBOC__
#ifndef __GNUC__
#ifndef _MSVC
#error "Please define one of __TURBOC__ __GNUC__ _MSVC."
#endif
#endif
#endif

int main(void) {
  fprintf(stderr, "Generating machine dependency information for CIL\n\n");

  printf("(* Generated by code in %s *)\n", __FILE__);
  printf("open Cil_types\n");
  printf("let mach = {\n");
#ifdef __TURBOC__
  printf("  version = \"%d\";\n", VERSION);
#else
  printf("  version = \"%s\";\n", VERSION);
#endif
  // Size of certain types
  printf("  sizeof_short = %zu;\n", sizeof(short));
  printf("  sizeof_int = %zu;\n", sizeof(int));
  printf("  sizeof_long = %zu;\n", sizeof(long));
  printf("  sizeof_longlong = %zu;\n", sizeof(LONGLONG));
  printf("  sizeof_ptr = %zu;\n", sizeof(int *));
  printf("  sizeof_float = %zu;\n", sizeof(float));
  printf("  sizeof_double = %zu;\n", sizeof(double));
  printf("  sizeof_longdouble = %zu;\n", sizeof(long double));
#ifdef __TURBOC__
  printf("  sizeof_void = %zu;\n", (size_t)0);
#else
  printf("  sizeof_void = %zu;\n", sizeof(void));
#endif
#ifdef __GNUC__
  printf("  sizeof_fun = %zu;\n", sizeof(main));
#else
  printf("  sizeof_fun = %zu;\n", (size_t)0);
#endif
  // definition of size_t
  printf("  size_t = \"%s\";\n", TYPE_SIZE_T);
  printf("  wchar_t = \"%s\";\n", TYPE_WCHAR_T);
  printf("  ptrdiff_t = \"%s\";\n", TYPE_PTRDIFF_T);
  // __int128 type
#if defined(__GNUC__) && defined(__SIZEOF_INT128__)
  printf("  has_int128 = true;\n");
  printf("  sizeof_int128 = %zu;\n", __SIZEOF_INT128__);
  printf("  alignof_int128 = %zu;\n", __SIZEOF_INT128__);
#else
  printf("  has_int128 = false;\n");
  printf("  sizeof_int128 = 0;\n");
  printf("  alignof_int128 = 0;\n");
#endif

  // The alignment of a short
  struct shortstruct {
    char c;
    short s;
  };
  printf("  alignof_short = %zu;\n", offsetof(struct shortstruct, s));

  // The alignment of an int
  struct intstruct {
    char c;
    int i;
  };
  printf("  alignof_int = %zu;\n", offsetof(struct intstruct, i));

  // The alignment of a long
  struct longstruct {
    char c;
    long l;
  };
  printf("  alignof_long = %zu;\n", offsetof(struct longstruct, l));

  // The alignment of long long
  struct longlong {
    char c;
    LONGLONG ll;
  };
  printf("  alignof_longlong = %zu;\n", offsetof(struct longlong, ll));

  // The alignment of a ptr
  struct ptrstruct {
    char c;
    int *p;
  };
  printf("  alignof_ptr = %zu;\n", offsetof(struct ptrstruct, p));

  // Unnamed members
  struct S0 {
    int;
    // If you are reading this, it's probably because your C compiler
    // rejected the above. Good for you! It is not allowed by C99.
    // See discussion thread at:
    // https://lists.cs.illinois.edu/lists/arc/c-semantics/2011-08/
    // You can comment out this block.
    int f1;
  };
  if (sizeof(struct S0) != 2*sizeof(int)) {
    printf("(* WARNING: This compiler handles unnamed struct members\n");
    printf("   differently from TrustInSoft Kernel.\n");
    printf("   To be analyzed correctly, your programs must *NOT* use\n");
    printf("   this language extension. *)\n");
  }

  // long long bit-fields
  struct LLS {
    long long int f:2;
    // If you are reading this, it's probably because your C compiler
    // rejected the above. Good for you! It is only allowed by C99
    // as an extension.
    // You can comment out this block.
  } lls;
  if (sizeof(1 + lls.f) != sizeof(int)) {
    printf("(* WARNING: This compiler handles long long bit-fields\n");
    printf("   differently from TrustInSoft Kernel.\n");
    printf("   To be analyzed correctly, your programs must *NOT* use\n");
    printf("   this language extension. *)\n");
  }

  // The alignment of a float
  struct floatstruct {
    char c;
    float f;
  };
  printf("  alignof_float = %zu;\n", offsetof(struct floatstruct, f));

  // The alignment of double
  struct s1 {
    char c;
    double d;
  };
  printf("  alignof_double = %zu;\n", offsetof(struct s1, d));

  // The alignment of long  double
  struct s2 {
    char c;
    long double ld;
  };
  printf("  alignof_longdouble = %zu;\n", offsetof(struct s2, ld));

#ifdef __GNUC__
  printf("  alignof_str = %zu;\n",
         __alignof("a string"));
#else
  printf("  alignof_str = %zu;\n", (size_t)0);
#endif

#ifdef __GNUC__
  printf("  alignof_fun = %zu;\n",
         __alignof(main));
#else
  printf("  alignof_fun = %zu;\n", (size_t)0);
#endif

  // The alignment of char array
  struct s3 {
    char c;
    char ca[2];
  };
  //printf("  alignof_char_array = %zu;\n", offsetof(struct s3, ca));

  /* The alignement of an __aligned__ type */
#ifdef __TURBOC__
  printf("  alignof_aligned = 8;\n");
#else
  char __attribute__((aligned)) c;
  long double __attribute__((aligned)) ld;
  if (__alignof(c) != __alignof(ld)) {
    printf("(*__attribute__((aligned)) has a different effect \
              on different types.  alignments may be computed \
              incorrectly.*)\n");
  };
  printf("  alignof_aligned = %zu;\n",__alignof(c));
#endif

  // Whether char is unsigned
  printf("  char_is_unsigned = %s;\n",
         ((char)0xff) > 0 ? "true" : "false");

  // Whether int bit-field is unsigned
  union {
    signed int init ;
    struct {
      int width8 : 8;
    } sign ;
  } bitfield;

  bitfield.init=-1;
  printf("  (* int_bitfield_is_unsigned = %s; *)\n",
         (bitfield.sign.width8 > 0 ? "true" : "false"));
  if (bitfield.sign.width8 > 0) {
    // 'int width8 : 8' is an unsigned bit-field.
    printf("(* WARNING: This compiler handles int bit-fields\n");
    printf("   differently from TrustInSoft Kernel.\n");
    printf("   To be analyzed correctly, your programs must *NOT* use\n");
    printf("   'int' bit-fields, but 'unsigned int' bit-fields. *)\n");
  }

  // Whether string literals contain constant characters
  puts("  const_string_literals = " CONST_STRING_LITERALS ";");

  // endianness
  int e = 0x11223344;
  if (0x44 == *(char*)&e)
      printf("  little_endian = %s;\n", "true");
  else if (0x11 == *(char*)&e)
      printf("  little_endian = %s;\n", "false");
  else {
      fprintf(stderr, "ERROR: unknown endianness.\n");
      return 1;
  }

  // __builtin_val_list
#ifdef HAVE_BUILTIN_VA_LIST
  printf("  has__builtin_va_list = true;\n");
#else
  printf("  has__builtin_va_list = false;\n");
#endif

  // __thread_is_keyword
#ifdef THREAD_IS_KEYWORD
  printf("  __thread_is_keyword = true;\n");
#else
  printf("  __thread_is_keyword = false;\n");
#endif

  // compiler
  printf("  compiler = \"" COMPILER "\"");

  printf(" }\n");

  return 0;
}
