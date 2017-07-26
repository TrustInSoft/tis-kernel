/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file provides a nonstandard GNU extension. */

#ifndef __FC_IO_STDIO_H
#define __FC_IO_STDIO_H

#include "__fc_define_ssize_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_off_t.h"

typedef ssize_t cookie_read_function_t
  (void *cookie, char *buf, size_t size);

typedef ssize_t cookie_write_function_t
  (void *cookie, const char *buf, size_t size);

typedef int cookie_seek_function_t
  (void *cookie, off64_t *offset, int whence);

typedef int cookie_close_function_t(void *cookie);

typedef struct
{
  cookie_read_function_t  *read;
  cookie_write_function_t *write;
  cookie_seek_function_t  *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;

#endif
