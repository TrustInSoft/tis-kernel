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

#ifndef __FC_STRING_H_
#define __FC_STRING_H_

#include "__fc_string_axiomatic.h"
#include "stddef.h"
#include "limits.h"
#include "features.h"

__BEGIN_DECLS

// Query memory

/*@ requires \valid_read(((char*)__s1)+(0 .. __n - 1));
  @ requires \valid_read(((char*)__s2)+(0 .. __n - 1));
  @ assigns \result \from ((char*)__s1)[0 .. __n - 1],
                          ((char*)__s2)[0 .. __n - 1];
  @ ensures \result == memcmp{Pre,Pre}((char*)__s1,(char*)__s2,__n);
  @*/
extern int memcmp (const void *__s1, const void *__s2, size_t __n) __THROW;

/*@ requires \valid_read(((unsigned char*)__s)+(0..__n - 1));
  @ assigns \result \from __s, __c, ((unsigned char*)__s)[0..__n-1];
  @ behavior found:
  @   assumes memchr((char*)__s,__c,__n);
  @   ensures \base_addr(\result) == \base_addr(__s);
  @   ensures *(char*)\result == __c;
  @   ensures \forall integer i;
  @     0 <= i < __n ==> *((unsigned char*)__s+i) == __c
  @     ==> \result <= __s+i;
  @ behavior not_found:
  @   assumes ! memchr((char*)__s,__c,__n);
  @   ensures \result == \null;
  @*/
extern void *memchr(const void *__s, int __c, size_t __n) __THROW;

// Copy memory

/*@ requires valid_dst: \valid(((char*)__dest)+(0..__n - 1));
  @ requires valid_src: \valid_read(((char*)__src)+(0..__n - 1));
  @ requires \separated(((char *)__dest)+(0..__n-1),((char *)__src)+(0..__n-1));
  @ assigns ((char*)__dest)[0..__n - 1] \from ((char*)__src)[0..__n-1];
  @ assigns \result \from __dest;
  @ ensures memcmp{Post,Pre}((char*)__dest,(char*)__src,__n) == 0;
  @ ensures \result == __dest;
  @*/
extern void *memcpy(void *restrict __dest,
		    const void *restrict __src, size_t __n) __THROW;

/*@ requires valid_dst: \valid(((char*)__dest)+(0..__n - 1));
  @ requires valid_src: \valid_read(((char*)__src)+(0..__n - 1));
  @ assigns ((char*)__dest)[0..__n - 1] \from ((char*)__src)[0..__n-1];
  @ assigns \result \from __dest;
  @ ensures memcmp{Post,Pre}((char*)__dest,(char*)__src,__n) == 0;
  @ ensures \result == __dest;
  @*/
extern void *memmove(void *__dest, const void *__src, size_t __n) __THROW;

// Set memory

/*@ requires \valid(((char*)__s)+(0..__n - 1));
  @ assigns ((char*)__s)[0..__n - 1] \from __c;
  @ assigns \result \from __s;
  @ ensures memset((char*)__s,__c,__n);
  @ ensures \result == __s;
  @*/
extern void *memset(void *__s, int __c, size_t __n) __THROW;

// Query strings

/*@ requires valid_string_src: valid_read_string(__s);
  @ assigns \result \from __s[0..];
  @ ensures \result == strlen(__s);
  @*/
extern size_t strlen (const char *__s) __THROW;

/*@ requires valid_string_src: valid_read_string(__s); // over-strong
  @ assigns \result \from __s[0..];
  @ ensures \result == strlen(__s) || \result == __n;
  @*/
extern size_t strnlen (const char *__s, size_t __n) __THROW;

/*@ requires valid_string_s1: valid_read_string(__s1);
  @ requires valid_string_s2: valid_read_string(__s2);
  @ assigns \result \from __s1[0..], __s2[0..];
  @ ensures \result == strcmp(__s1,__s2);
  @*/
extern int strcmp (const char *__s1, const char *__s2) __THROW;

/*@ requires valid_string_s1: valid_read_string(__s1); // over-strong
  @ requires valid_string_s2: valid_read_string(__s2); // over-strong
  @ assigns \result \from __s1[0 .. __n-1], __s2[0 ..__n-1];
  @ ensures \result == strncmp(__s1,__s2,__n);
  @*/
extern int strncmp (const char *__s1, const char *__s2, size_t __n) __THROW;

/*@ requires valid_string_s1: valid_read_string(__s1); // over-strong
  @ requires valid_string_s2: valid_read_string(__s2); // over-strong
  @ assigns \result \from __s1[0..], __s2[0..];
  @*/
extern int strcoll (const char *__s1, const char *__s2) __THROW;

/*@ requires valid_string_src: valid_read_string(__s);
  @ assigns \result \from __s, __s[0..],__c;
  @ behavior found:
  @   assumes strchr(__s,__c);
  @   ensures *\result == __c;
  @   ensures \base_addr(\result) == \base_addr(__s);
  @   ensures __s <= \result < __s + strlen(__s);
  @   ensures valid_read_string(\result);
  @   ensures \forall char* p; __s<=p<\result ==> *p != __c;
  @ behavior not_found:
  @   assumes ! strchr(__s,__c);
  @   ensures \result == \null;
  @ behavior default:
  @ ensures \result == \null || \base_addr(\result) == \base_addr(__s);
  @*/
extern char *strchr(const char *__s, int __c) __THROW;

/*@ requires valid_string_src: valid_read_string(__s);
  @ assigns \result \from __s, __s[0..],__c;
  @ behavior found:
  @   assumes strchr(__s,__c);
  @   ensures *\result == __c;
  @   ensures \base_addr(\result) == \base_addr(__s);
  @   ensures valid_read_string(\result);
  @ behavior not_found:
  @   assumes ! strchr(__s,__c);
  @   ensures \result == \null;
  @ behavior default:
  @ ensures \result == \null || \base_addr(\result) == \base_addr(__s);
  @*/
extern char *strrchr(const char *__s, int __c) __THROW;

/*@ requires valid_string_src: valid_read_string(__s);
  @ requires valid_string_reject: valid_read_string(__reject);
  @ assigns \result \from __s[0..], __reject[0..];
  @ ensures 0 <= \result <= strlen(__s);
  @*/
extern size_t strcspn(const char *__s, const char *__reject) __THROW;

/*@ requires valid_string_src: valid_read_string(__s);
  @ requires valid_string_accept: valid_read_string(__accept);
  @ assigns \result \from __s[0..], __accept[0..];
  @ ensures 0 <= \result <= strlen(__s);
  @*/
extern size_t strspn(const char *__s, const char *__accept) __THROW;

/*@ requires valid_string_src: valid_read_string(__s);
  @ requires valid_string_accept: valid_read_string(__accept);
  @ assigns \result \from __s, __s[0..], __accept[0..];
  @ ensures \result == 0 || \base_addr(\result) == \base_addr(__s);
  @*/
extern char *strpbrk(const char *__s, const char *__accept) __THROW;

/*@ requires valid_string_haystack: valid_read_string(__haystack);
  @ requires valid_string_needle: valid_read_string(__needle);
  @ assigns \result \from __haystack, __haystack[0..], __needle[0..];
  @ ensures \result == 0
  @      || (\subset(\result, __haystack+(0..)) && \valid_read(\result)
  @          && memcmp{Pre,Pre}(\result,__needle,strlen(__needle)) == 0);
  @*/
extern char *strstr(const char *__haystack, const char *__needle) __THROW;

/*@ requires valid_string_src: valid_string_or_null(__s);
  @ requires valid_string_delim: valid_read_string(__delim);
  @ assigns \result \from __s, __s[0..], __delim[0..];
  @ ensures \result == \null
            || \base_addr(\result) == \base_addr(__s);
  @*/
extern char *strtok(char *restrict __s, const char *restrict __delim) __THROW;

#ifdef _BSD_SOURCE
/*@ requires valid_string_src: \valid(__stringp) && valid_string(*__stringp);
  @ requires valid_string_delim: valid_read_string(__delim);
  @ assigns *__stringp \from __delim[..], *__stringp[..];
  @ assigns \result \from __delim[..], *__stringp[..];
  @*/
extern char *strsep (char **__stringp, const char *__delim) __THROW;
#endif

/*@ assigns \result \from __errnum;
  @ ensures valid_read_string(\result);
  @*/
extern char *strerror(int __errnum) __THROW;

// Copy strings

/*@ requires valid_string_src: valid_read_string(__src);
  @ requires room_string: \valid(__dest+(0..strlen(__src)));
  @ assigns __dest[0..strlen(__src)] \from __src[0..strlen(__src)];
  @ assigns \result \from __dest;
  @ ensures strcmp(__dest,__src) == 0;
  @ ensures \result == __dest;
  @*/
extern char *strcpy(char *restrict __dest, const char *restrict __src) __THROW;

/*@
  @ requires valid_string_src: valid_read_string(__src);
  @ requires room_nstring: \valid(__dest+(0 .. __n-1));
  @ assigns __dest[0..__n - 1] \from __src[0..__n-1];
  @ assigns \result \from __dest;
  @ ensures \result == __dest;
  @ ensures \initialized(__dest+(0 .. __n-1));
  @ behavior full:
  @   assumes strlen(__src) < __n;
  @   ensures strcmp(__dest,__src) == 0;
  @ behavior partial:
  @   assumes __n <= strlen(__src);
  @   ensures memcmp{Post,Post}(__dest,__src,__n) == 0;
  @*/
extern char *strncpy(char *restrict __dest,
		     const char *restrict __src, size_t __n) __THROW;

// stpcpy is POSIX.1-2008
#ifdef _POSIX_C_SOURCE
# if _POSIX_C_SOURCE >= 200809L
/*@ requires valid_string_src: valid_read_string(__src);
  @ requires room_string: \valid(__dest+(0..strlen(__src)));
  @ assigns __dest[0..strlen(__src)] \from __src[0..strlen(__src)];
  @ assigns \result \from __dest;
  @ ensures strcmp(__dest,__src) == 0;
  @ ensures \result == __dest + strlen(__dest);
  @*/
extern char *stpcpy(char *restrict __dest, const char *restrict __src) __THROW;
# endif
#endif

/*@ // missing: separation
  @ requires valid_string_src: valid_read_string(__src);
  @ requires valid_string_dst: valid_string(__dest);
  @ requires room_string: \valid(__dest+(0..strlen(__dest) + strlen(__src)));
  @ assigns __dest[strlen(__dest)..strlen(__dest) + strlen(__src)]
  @   \from __src[0..strlen(__src)];
  @ ensures strlen(__dest) == \old(strlen(__dest) + strlen(__src));
  @ assigns \result \from __dest;
  @ ensures \result == __dest;
  @*/
extern char *strcat(char *restrict __dest, const char *restrict __src) __THROW;

/*@ // missing: separation
  @ requires valid_string_src:
      valid_read_string(__src) || \valid_read(__src+(0..__n-1));
  @ requires valid_string_dst: valid_string(__dest);
  @ requires room_string:
      \valid(__dest + (strlen(__dest) .. strlen(__dest) + __n)) ;
  @ assigns __dest[strlen(__dest) .. strlen(__dest) + __n] \from __src[0..__n];
  @ assigns \result \from __dest;
  @ ensures \result == __dest;
  @ behavior full:
  @   assumes valid_read_string(__src) && strlen(__src) <= __n;
  @   assigns __dest[strlen(__dest)..strlen(__dest) + strlen(__src)]
  @   \from __src[0..strlen(__src)];
  @   assigns \result \from __dest;
  @   ensures strlen(__dest) == \old(strlen(__dest) + strlen(__src));
  @ behavior partial:
  @   assumes ! (valid_read_string(__src) && strlen(__src) <= __n);
  @   assigns __dest[strlen(__dest)..strlen(__dest) + __n]
  @   \from __src[0..strlen(__src)];
  @   assigns \result \from __dest;
  @   ensures strlen(__dest) == \old(strlen(__dest)) + __n;
  @*/
extern char *strncat(char *restrict __dest,
                     const char *restrict __src, size_t __n) __THROW;

/*@ requires valid_dest: \valid(__dest+(0..__n - 1));
  @ requires valid_string_src: valid_read_string(__src);
  @ assigns __dest[0..__n - 1] \from __src[0..], __n;
  @ assigns \result \from __dest;
  @*/
extern size_t strxfrm (char *restrict __dest,
		       const char *restrict __src, size_t __n) __THROW;

// Allocate strings

/*@ requires valid_string_src: valid_read_string(__s);
  @ assigns \result; // FIXME
  @ ensures \valid(\result+(0..strlen(__s))) && strcmp(\result,__s) == 0;
  @*/
extern char *strdup (const char *__s) __THROW;

/*@ requires valid_string_src: valid_read_string(__s); // FIXME
  @ assigns \result; // FIXME
  @ ensures \valid(\result+(0..minimum(strlen(__s),__n)))
  @         && valid_string(\result) && strlen(\result) <= __n
  @         && strncmp(\result,__s,__n) == 0;
  @*/
extern char *strndup (const char *__s, size_t __n) __THROW;

#ifdef _GNU_SOURCE

/*@
  requires valid_string_s1: valid_read_string(__s1);
  requires valid_string_s2: valid_read_string(__s2);
  assigns \result \from __s1[..], __s2[..];
 */
int strverscmp(const char *__s1, const char *__s2);

#endif

#ifdef _BSD_SOURCE
/*@ requires \valid(__dest+(0..__n-1));
  @ requires valid_string_src: valid_read_string(__src);
  @ ensures \result == strlen(__dest)+strlen(__src);
  @ behavior useless:
  @   assumes __n == 0;
  @   assigns \nothing;
  @ behavior full:
  @   assumes strlen(__src) < __n;
  @   assigns __dest[0..strlen(__src)-1] \from __src[0..strlen(__src)-1];
  @   assigns __dest[strlen(__src)];
  @   ensures __dest[strlen(__src)] == 0;
  @ behavior truncation:
  @   assumes 0 < __n <= strlen(__src);
  @   assigns __dest[0..__n-2] \from __src[0..__n-2];
  @   assigns __dest[__n-1];
  @   ensures __dest[__n-1] == 0;
  @*/
extern size_t strlcpy(char *__dest, const char *__src, size_t __n) __THROW;

/*@ requires valid_string_dst: valid_string(__dest);
  @ requires \valid(__dest+(0..__n-1));
  @ requires valid_string_src: valid_read_string(__src);
  @ ensures \result >= strlen(__src);
  @ behavior full:
  @   assumes \exists ℤ dlen; dlen < __n && __dest[dlen] == '\0';
  @   assumes strlen(__dest)+strlen(__src) < __n;
  @   assigns __dest[strlen(__dest)..strlen(__dest)+strlen(__src)-1]
  @     \from __src[0..strlen(__src)-1];
  @   assigns __dest[strlen(__dest)+strlen(__src)];
  @   ensures __dest[strlen(__dest)+strlen(__src)] == 0;
  @   ensures \result == strlen(__dest) + strlen(__src);
  @ behavior truncation:
  @   assumes \exists ℤ dlen; dlen < __n && __dest[dlen] == '\0';
  @   assumes 0 < __n <= strlen(__dest)+strlen(__src);
  @   assigns __dest[strlen(__dest)..__n-2]
  @     \from __src[0..__n-strlen(__dest)-2];
  @   assigns __dest[__n-1];
  @   ensures __dest[__n-1] == 0;
  @ behavior unchanged:
  @   assumes !(\exists ℤ dlen; dlen < __n && __dest[dlen] == '\0');
  @   ensures \result == __n + strlen(__src);
  @   assigns \nothing;
  @*/
extern size_t strlcat(char *__dest, const char *__src, size_t __n) __THROW;
#endif

__END_DECLS

/* Include strings.h: this is what BSD does, and glibc does something
   equivalent (having copied prototypes to string.h). */
#include <strings.h>

#endif /* _STRING_H_ */
