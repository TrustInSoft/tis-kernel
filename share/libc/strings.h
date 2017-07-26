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

#ifndef __FC_STRINGS_H_
#define __FC_STRINGS_H_
#include "__fc_define_size_t.h"
#include "features.h"

__BEGIN_DECLS

int    bcmp(const void *, const void *, size_t) __THROW;
void   bcopy(const void *, void *, size_t) __THROW;


/*@ requires \valid (((char*) s)+(0 .. n-1));
  assigns ((char*) s)[0 .. n-1] \from \nothing; */
void   bzero(void *s, size_t n) __THROW;
int    ffs(int) __THROW;
char   *index(const char *, int) __THROW;
char   *rindex(const char *, int) __THROW;
int    strcasecmp(const char *, const char *) __THROW;
int    strncasecmp(const char *, const char *, size_t) __THROW;

__END_DECLS

#endif
