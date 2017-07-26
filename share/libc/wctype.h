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

/* ISO C: 7.26 */
#ifndef __FC_WCTYPE_H
#define __FC_WCTYPE_H

#include "__fc_define_locale_t.h"
#include "__fc_define_wint_t.h"
#include "__fc_define_wctrans_t.h"
#include "__fc_define_wctype_t.h"
#include "features.h"

__BEGIN_DECLS
int       iswalnum(wint_t __w);
int       iswalnum_l(wint_t __w, locale_t __l);
int       iswalpha(wint_t __w);
int       iswalpha_l(wint_t __w, locale_t __l);
int       iswblank(wint_t __w);
int       iswblank_l(wint_t __w, locale_t __l);
int       iswcntrl(wint_t __w);
int       iswcntrl_l(wint_t __w, locale_t __l);
int       iswctype(wint_t __w, wctype_t __t);
int       iswctype_l(wint_t __w, wctype_t __t, locale_t __l);
int       iswdigit(wint_t __w);
int       iswdigit_l(wint_t __w, locale_t __l);
int       iswgraph(wint_t __w);
int       iswgraph_l(wint_t __w, locale_t __l);
int       iswlower(wint_t __w);
int       iswlower_l(wint_t __w, locale_t __l);
int       iswprint(wint_t __w);
int       iswprint_l(wint_t __w, locale_t __l);
int       iswpunct(wint_t __w);
int       iswpunct_l(wint_t __w, locale_t __l);
int       iswspace(wint_t __w);
int       iswspace_l(wint_t __w, locale_t __l);
int       iswupper(wint_t __w);
int       iswupper_l(wint_t __w, locale_t __l);
int       iswxdigit(wint_t __w);
int       iswxdigit_l(wint_t __w, locale_t __l);
wint_t    towctrans(wint_t __w, wctrans_t __t);
wint_t    towctrans_l(wint_t __w, wctrans_t __t, locale_t __l);
wint_t    towlower(wint_t __w);
wint_t    towlower_l(wint_t __w, locale_t __l);
wint_t    towupper(wint_t __w);
wint_t    towupper_l(wint_t __w, locale_t __l);
wctrans_t wctrans(const char *__c);
wctrans_t wctrans_l(const char *__c, locale_t __l);
wctype_t  wctype(const char *__c);
wctype_t  wctype_l(const char *__c, locale_t __l);

__END_DECLS

#endif
