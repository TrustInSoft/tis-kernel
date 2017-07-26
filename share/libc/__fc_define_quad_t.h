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

#ifndef __FC_DEFINE_QUAD_T
#define __FC_DEFINE_QUAD_T
#include "features.h"
#include "__fc_machdep.h"
__BEGIN_DECLS
typedef	 __UINT64_T u_quad_t;
typedef	__INT64_T quad_t;
typedef	quad_t *qaddr_t;
__END_DECLS
#endif
