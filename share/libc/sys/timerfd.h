/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file is Linux-specific. */

#ifndef	__FC_SYS_TIMERFD_H
#define	__FC_SYS_TIMERFD_H

#include <time.h>

enum {
  TFD_CLOEXEC = 02000000,
#define TFD_CLOEXEC TFD_CLOEXEC
  TFD_NONBLOCK = 00004000
#define TFD_NONBLOCK TFD_NONBLOCK
};

/*@ assigns \result \from clock_id, flags; */
int timerfd_create (int clock_id, int flags);

int timerfd_settime (int ufd, int flags,
                     const struct itimerspec *utmr,
                     struct itimerspec *otmr);

int timerfd_gettime (int ufd, struct itimerspec *otmr);

#endif
