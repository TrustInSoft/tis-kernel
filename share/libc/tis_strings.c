/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <strings.h>
#include <__fc_define_null.h>

char *
index(const char *s, int c)
{
	while (1) {
		if (*s == c)
			return s;
		if (!*s)
			return NULL;
		s++;
	}
}
