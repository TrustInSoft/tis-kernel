/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2015-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_TIMEVAL_H
#define __FC_DEFINE_TIMEVAL_H

#include "__fc_define_time_t.h"
#include "__fc_define_suseconds_t.h"

struct timeval {
  time_t         tv_sec;
  suseconds_t    tv_usec;
};

#endif
