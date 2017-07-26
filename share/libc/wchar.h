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

/* ISO C: 7.25 */
#ifndef __FC_WCHAR_H
#define __FC_WCHAR_H

#include "__fc_define_wchar_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_file.h"
#include "__fc_define_locale_t.h"
#include "__fc_define_tm.h"
#include "__fc_define_wint_t.h"
#include "__fc_define_wctype_t.h"
#include "__fc_define_mbstate_t.h"
#include "__fc_define_va_list.h"
#include "features.h"
#include "__fc_define_wchar_limits.h"
#define WEOF 0xffffffffu

__BEGIN_DECLS
wint_t        btowc(int);
wint_t        fgetwc(FILE *);
wchar_t      *fgetws(wchar_t *restrict, int, FILE *restrict);
wint_t        fputwc(wchar_t, FILE *);
int           fputws(const wchar_t *restrict, FILE *restrict);
int           fwide(FILE *, int);
int           fwprintf(FILE *restrict, const wchar_t *restrict, ...);
int           fwscanf(FILE *restrict, const wchar_t *restrict, ...);
wint_t        getwc(FILE *);
wint_t        getwchar(void);
int           iswalnum(wint_t);
int           iswalpha(wint_t);
int           iswcntrl(wint_t);
int           iswctype(wint_t, wctype_t);
int           iswdigit(wint_t);
int           iswgraph(wint_t);
int           iswlower(wint_t);
int           iswprint(wint_t);
int           iswpunct(wint_t);
int           iswspace(wint_t);
int           iswupper(wint_t);
int           iswxdigit(wint_t);
size_t        mbrlen(const char *restrict, size_t, mbstate_t *restrict);
size_t        mbrtowc(wchar_t *restrict, const char *restrict, size_t,
                  mbstate_t *restrict);
int           mbsinit(const mbstate_t *);
size_t        mbsnrtowcs(wchar_t *restrict, const char **restrict,
                  size_t, size_t, mbstate_t *restrict);
size_t        mbsrtowcs(wchar_t *restrict, const char **restrict, size_t,
                  mbstate_t *restrict);
FILE         *open_wmemstream(wchar_t **, size_t *);
wint_t        putwc(wchar_t, FILE *);
wint_t        putwchar(wchar_t);
int           swprintf(wchar_t *restrict, size_t,
                  const wchar_t *restrict, ...);
int           swscanf(const wchar_t *restrict,
                  const wchar_t *restrict, ...);
wint_t        towlower(wint_t);
wint_t        towupper(wint_t);
wint_t        ungetwc(wint_t, FILE *);
int           vfwprintf(FILE *restrict, const wchar_t *restrict, va_list);
int           vfwscanf(FILE *restrict, const wchar_t *restrict, va_list);
int           vswprintf(wchar_t *restrict, size_t,
                  const wchar_t *restrict, va_list);
int           vswscanf(const wchar_t *restrict, const wchar_t *restrict,
                  va_list);

/*@ assigns \result \from format; // ... */
int vwprintf(const wchar_t * restrict format, va_list ap);

int           vwscanf(const wchar_t *restrict, va_list);
wchar_t      *wcpcpy(wchar_t *restrict, const wchar_t *restrict);
wchar_t      *wcpncpy(wchar_t *restrict, const wchar_t *restrict, size_t);
size_t        wcrtomb(char *restrict, wchar_t, mbstate_t *restrict);
int           wcscasecmp(const wchar_t *, const wchar_t *);
int           wcscasecmp_l(const wchar_t *, const wchar_t *, locale_t);

/*@ assigns s1[..] \from s2[..]; */
wchar_t      *wcscat(wchar_t *restrict s1, const wchar_t *restrict s2);
wchar_t *wcschr(const wchar_t *s, wchar_t c);

/*@ assigns \result \from s1[..], s2[..]; */
int wcscmp(const wchar_t *s1, const wchar_t *s2);
int           wcscoll(const wchar_t *, const wchar_t *);
int           wcscoll_l(const wchar_t *, const wchar_t *, locale_t);

/*@ assigns s1[..] \from s2[..]; */
wchar_t      *wcscpy(wchar_t *restrict s1, const wchar_t *restrict s2);
size_t        wcscspn(const wchar_t *, const wchar_t *);
wchar_t      *wcsdup(const wchar_t *);
size_t        wcsftime(wchar_t *restrict, size_t,
                  const wchar_t *restrict, const struct tm *restrict);

/*@ assigns \result \from str[..]; */
size_t wcslen(const wchar_t *str);
int           wcsncasecmp(const wchar_t *, const wchar_t *, size_t);
int           wcsncasecmp_l(const wchar_t *, const wchar_t *, size_t,
                  locale_t);
wchar_t      *wcsncat(wchar_t *restrict, const wchar_t *restrict, size_t);

/*@ assigns \result \from s1[..], s2[..], n; */
int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n);

wchar_t      *wcsncpy(wchar_t *restrict, const wchar_t *restrict, size_t);

/*@ assigns \result \from str[..], len; */
size_t wcsnlen(const wchar_t *str, size_t len);
size_t        wcsnrtombs(char *restrict, const wchar_t **restrict, size_t,
                  size_t, mbstate_t *restrict);
wchar_t      *wcspbrk(const wchar_t *, const wchar_t *);
wchar_t      *wcsrchr(const wchar_t *, wchar_t);
size_t        wcsrtombs(char *restrict, const wchar_t **restrict,
                  size_t, mbstate_t *restrict);
size_t        wcsspn(const wchar_t *, const wchar_t *);
wchar_t      *wcsstr(const wchar_t *restrict, const wchar_t *restrict);
double        wcstod(const wchar_t *restrict, wchar_t **restrict);
float         wcstof(const wchar_t *restrict, wchar_t **restrict);
wchar_t      *wcstok(wchar_t *restrict, const wchar_t *restrict,
                  wchar_t **restrict);
long          wcstol(const wchar_t *restrict, wchar_t **restrict, int);
long double   wcstold(const wchar_t *restrict, wchar_t **restrict);
long long     wcstoll(const wchar_t *restrict, wchar_t **restrict, int);
unsigned long wcstoul(const wchar_t *restrict, wchar_t **restrict, int);
unsigned long long
              wcstoull(const wchar_t *restrict, wchar_t **restrict, int);
int           wcswidth(const wchar_t *, size_t);
size_t        wcsxfrm(wchar_t *restrict, const wchar_t *restrict, size_t);
size_t        wcsxfrm_l(wchar_t *restrict, const wchar_t *restrict,
                  size_t, locale_t);
int           wctob(wint_t);
wctype_t      wctype(const char *);
int           wcwidth(wchar_t);
wchar_t *wmemchr(const wchar_t *s, wchar_t c, size_t n);
int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t      *wmemcpy(wchar_t *restrict, const wchar_t *restrict, size_t);
/*@ assigns s1[..] \from s2[..], n ; */
wchar_t * wmemcpy(wchar_t * s1, const wchar_t * s2, size_t n);
/*@ assigns s1[..] \from s2[..], n ; */
wchar_t * wmemmove(wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t * wmemset(wchar_t *s, wchar_t c, size_t n);
/*@ assigns \result \from format; // ... */
int wprintf(const wchar_t * restrict format, ...);
int           wscanf(const wchar_t *restrict, ...);

__END_DECLS

#endif
