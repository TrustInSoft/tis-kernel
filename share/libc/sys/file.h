/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

/* This file only defines flock. */

#ifndef	__FC_SYS_FILE_H
#define	__FC_SYS_FILE_H

#ifndef	__FC_FCNTL
# include <fcntl.h>
#endif

#define	LOCK_SH	1	/* Shared lock.  */
#define	LOCK_EX	2 	/* Exclusive lock.  */
#define	LOCK_UN	8	/* Unlock.  */
#define	LOCK_NB	4	/* Don't block when locking.  */

int flock(int fd, int operation);

#endif
