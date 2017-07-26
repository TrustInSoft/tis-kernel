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

#ifndef	__FC_SYS_IO_H
#define	__FC_SYS_IO_H

int iopl(int level);

// These assigns are not precise. There value may change with time.

//@ assigns \result \from port;
inline unsigned char
inb(unsigned short int port);

//@ assigns \result \from port;
inline unsigned short int
inw (unsigned short int port);

//@ assigns \result \from port;
inline unsigned int
inl (unsigned short int port);

//@ assigns \nothing;
inline void
outb_p (unsigned char value, unsigned short int port);

//@ assigns \nothing;
inline void
outw_p (unsigned short int value, unsigned short int port);

//@ assigns \nothing;
inline void
outl_p (unsigned int value, unsigned short int port);

#endif
