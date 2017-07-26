/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* POSIX header */

#ifndef __FC_SYS_TIMEB_H__
#define __FC_SYS_TIMEB_H__

#include "../features.h"
#include "../__fc_define_time_t.h"

__BEGIN_DECLS

struct timeb {
  time_t time;
  unsigned short millitm;
  short timezone;
  short dstflag;
};

int ftime(struct timeb *timebuf);

__END_DECLS

#endif
