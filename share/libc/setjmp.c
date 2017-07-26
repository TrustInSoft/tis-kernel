/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include "setjmp.h"
#include "__fc_builtin.h"

void longjmp(jmp_buf env, int val)
{
  Frama_C_abort();
  return;
}
