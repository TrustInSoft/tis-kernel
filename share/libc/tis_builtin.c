/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <tis_builtin.h>

/* There remains a slight difference with tis_float/double_interval
   and +0./-0., because the specification is not sufficient to
   exclude -0. when requiring >= +0.
*/

float tis_float_interval(float min, float max)
{
  tis_update_entropy();
  return tis_entropy_source ? min : max;
}

double tis_double_interval(double min, double max)
{
  tis_update_entropy();
  return tis_entropy_source ? min : max;
}
