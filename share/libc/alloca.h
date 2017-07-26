/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ALLOCA_H
#define __FC_ALLOCA_H

#include "__fc_define_size_t.h"
#include "features.h"

__BEGIN_DECLS

/*@ ghost extern int __fc_heap_status __attribute__((FRAMA_C_MODEL)); */

/*@
  @ assigns __fc_heap_status \from size, __fc_heap_status;
  @ assigns \result \from size, __fc_heap_status;
*/
void *alloca(size_t size);

__END_DECLS

#endif
