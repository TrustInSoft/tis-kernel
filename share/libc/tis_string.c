/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <stdlib.h>
#include <string.h>

/*
  Copyright (C) 2005 Free Software Foundation, Inc.
  Written by Kaveh R. Ghazi <ghazi@caip.rutgers.edu>.
  License: LGPL 2

  source: http://www.opensource.apple.com/source/gcc/gcc-5666.3/libiberty/strndup.c
 */
char *
strndup (const char *s, size_t n)
{
  char *result;
  size_t len = strlen (s);
  if (n < len)
    len = n;
  result = (char *) malloc (len + 1);
  if (!result)
    return 0;
  result[len] = '\0';
  return (char *) memcpy (result, s, len);
}
