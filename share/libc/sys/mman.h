/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file is specified by POSIX. */

#ifndef __FC_SYS_MMAN_H_
#define __FC_SYS_MMAN_H_
#include "__fc_define_size_t.h"
#include "__fc_define_off_t.h"

#define PROT_READ	0x1		/* Page can be read.  */
#define PROT_WRITE	0x2		/* Page can be written.  */
#define PROT_EXEC	0x4		/* Page can be executed.  */
#define PROT_NONE	0x0		/* Page can not be accessed.  */

#define MAP_SHARED	0x01		/* Share changes.  */
#define MAP_PRIVATE	0x02		/* Changes are private.  */
#define MAP_FIXED	0x10		/* Interpret addr exactly */

#define MAP_FAILED	((void *) -1)

void *mmap(void *addr, size_t length, int prot, int flags,
                  int fd, off_t offset);
int munmap (void *addr, size_t len);
int mlock (const void *addr, size_t len);

#endif
