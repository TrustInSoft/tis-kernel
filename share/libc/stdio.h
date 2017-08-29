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

/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.19 */
#ifndef __FC_STDIO
#define __FC_STDIO
#include "features.h"
#include "__fc_machdep.h"
#include "stdarg.h"
#include "stddef.h"
#include "errno.h"
#include "__fc_define_stat.h"
#include "__fc_define_fpos_t.h"
#include "__fc_define_file.h"
#include "__fc_define_null.h"

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define BUFSIZ __FC_BUFSIZ
#define EOF __FC_EOF
#define FOPEN_MAX __FC_FOPEN_MAX
#define FILENAME_MAX __FC_FILENAME_MAX
#define L_tmpnam __FC_L_tmpnam

#include "__fc_define_seek_macros.h"

#define TMP_MAX __FC_TMP_MAX

__BEGIN_DECLS

extern FILE * __fc_stderr;
#define stderr (__fc_stderr)

extern FILE * __fc_stdin;
#define stdin (__fc_stdin)

extern FILE * __fc_stdout;
#define stdout (__fc_stdout)

/*
  Note: currently some functions only consider the __fc_stdio_id field of FILE.
        This models the fact that operations on different files are considered
        non-interferent between them.
*/

/*@ assigns \nothing; */
int remove(const char *__filename);

/*@ assigns \nothing; */
int rename(const char *__old_name, const char *__new_name);

/*@ assigns \nothing;
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE))) ; */
FILE *tmpfile(void);

/*@
  assigns \result \from __s[..];
  assigns __s[..] \from \nothing;
  // TODO: more precise behaviors from ISO C 7.19.4.4
*/
char *tmpnam(char *__s);

/*@
  requires \valid(__stream);
  assigns \result \from __stream, __stream->__fc_stdio_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.4.1
*/
int fclose(FILE *__stream);

/*@
  requires __stream == \null || \valid_read(__stream);
  assigns \result \from __stream, __stream->__fc_stdio_id;
  ensures \result == 0 || \result == EOF;
  // TODO: more precise behaviors from ISO C 7.19.5.2
 */
int fflush(FILE *__stream);

FILE __fc_fopen[__FC_FOPEN_MAX];
extern FILE* const __p_fc_fopen = __fc_fopen;

/*@
  assigns \result \from __filename[..],__mode[..], __p_fc_fopen;
  ensures
  \result==\null
  || (\subset(\result,&__fc_fopen[0 .. __FC_FOPEN_MAX-1])) ;
*/
FILE *fopen(const char * restrict __filename,
     const char * restrict __mode);

/*@ assigns \result \from __fildes,__mode[..];
  ensures \result==\null || (\valid(\result) && \fresh(\result,sizeof(FILE)));
 */
FILE *fdopen(int __fildes, const char *__mode);

/*@
  assigns *__stream;
  ensures \result==\null || \result==__stream ; */
FILE *freopen(const char * restrict __filename,
              const char * restrict __mode,
              FILE * restrict __stream);

/*@ assigns *__stream \from buf; */
void setbuf(FILE * restrict __stream,
     char * restrict buf);

/*@ assigns *__stream \from buf,__mode,size; */
int setvbuf(FILE * restrict __stream,
     char * restrict buf,
     int __mode, size_t size);

/*@ assigns *__stream \from __stream->__fc_stdio_id; */
// unsupported...
int fprintf(FILE * restrict __stream,
     const char * restrict format, ...);

/*@ assigns *__stream \from __stream->__fc_stdio_id;
// unsupported...
 */
int fscanf(FILE * restrict __stream,
     const char * restrict format, ...);

/*@ assigns *__fc_stdout \from format[..];
// unsupported...
*/
int printf(const char * restrict format, ...);

/*@ assigns *__fc_stdin;
// unsupported...
 */
int scanf(const char * restrict format, ...);

/*@ assigns __s[0..n-1];
// unsupported...
 */
int snprintf(char * restrict __s, size_t n,
    const char * restrict format, ...);

/*@ assigns __s[0..];
// unsupported...
 */
int sprintf(char * restrict __s,
     const char * restrict format, ...);

// unsupported...
int sscanf(const char * restrict __s,
     const char * restrict format, ...);

/*@ assigns *__stream \from format[..], arg; */
int vfprintf(FILE * restrict __stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *__stream \from format[..], *__stream;
// TODO: assign arg too. */
int vfscanf(FILE * restrict __stream,
     const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdout \from arg; */
int vprintf(const char * restrict format,
     va_list arg);

/*@ assigns *__fc_stdin \from format[..];
// TODO: assign arg too. */
int vscanf(const char * restrict format,
     va_list arg);

/*@ assigns __s[0..n-1] \from format[..], arg;
 */
int vsnprintf(char * restrict __s, size_t n,
     const char * restrict format,
     va_list arg);

/*@ assigns __s[0..] \from format[..], arg;
 */
int vsprintf(char * restrict __s,
     const char * restrict format,
     va_list arg);

/* @ TODO: assigns arg ; */
int vsscanf(const char * restrict __s,
     const char * restrict format,
     va_list arg);

/*@ assigns *__stream;
 */
int fgetc(FILE *__stream);

/*@ assigns __s[0..n-1],*__stream \from *__stream;
  assigns \result \from __s,n,*__stream;
  ensures \result == \null || \result==__s;
 */
char *fgets(char * restrict __s, int n,
    FILE * restrict __stream);

/*@ assigns *__stream ; */
int fputc(int c, FILE *__stream);

/*@ assigns *__stream \from __s[..]; */
int fputs(const char * restrict __s,
     FILE * restrict __stream);

/*@ assigns \result,*__stream \from *__stream; */
int getc(FILE *__stream);

/*@ assigns \result \from *__fc_stdin ; */
int getchar(void);

/*@ assigns __s[..] \from *__fc_stdin ;
  assigns \result \from __s, __fc_stdin;
  ensures \result == __s || \result == \null;
 */
char *gets(char *__s);

/*@ assigns *__stream \from c; */
int putc(int c, FILE *__stream);

/*@ assigns *__fc_stdout \from c; */
int putchar(int c);

/*@ assigns *__fc_stdout \from __s[..]; */
int puts(const char *__s);

/*@ assigns *__stream \from c; */
int ungetc(int c, FILE *__stream);

/*@
  requires \valid(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(__stream);
  assigns *(((char*)ptr)+(0..(nmemb*size)-1)) \from size, nmemb, *__stream;
  assigns \result \from size, *__stream;
  ensures \result <= nmemb;
  ensures \initialized(((char*)ptr)+(0..(\result*size)-1));
  //TODO: specify precise fields from struct FILE
*/
size_t fread(void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict __stream);

/*@
  requires \valid_read(((char*)ptr)+(0..(nmemb*size)-1));
  requires \valid(__stream);
  assigns *__stream, \result \from *(((char*)ptr)+(0..(nmemb*size)-1));
  ensures \result <= nmemb;
  //TODO: specify precise fields from struct FILE
*/
size_t fwrite(const void * restrict ptr,
     size_t size, size_t nmemb,
     FILE * restrict __stream);

/*@ assigns *pos \from *__stream ; */
int fgetpos(FILE * restrict __stream,
     fpos_t * restrict pos);

/*@ assigns *__stream \from __offset, __whence ;
  assigns __FC_errno ; */
int fseek(FILE *__stream, long int __offset, int __whence);

/*@ assigns *__stream \from *pos; */
int fsetpos(FILE *__stream, const fpos_t *pos);

/*@ assigns \result, __FC_errno \from *__stream ;*/
long int ftell(FILE *__stream);

/*@  assigns *__stream \from \nothing; */
void rewind(FILE *__stream);

/*@  assigns *__stream  \from \nothing; */
void clearerr(FILE *__stream);

/*@ assigns \result \from *__stream ;*/
int feof(FILE *__stream);

/*@ assigns \result \from *__stream ;*/
int fileno(FILE *__stream);

/*@ assigns *__stream \from \nothing ;*/
void flockfile(FILE *__stream);

/*@ assigns *__stream \from \nothing ;*/
void funlockfile(FILE *__stream);

/*@ assigns \result,*__stream \from \nothing ;*/
int ftrylockfile(FILE *__stream);

/*@ assigns \result \from *__stream ;*/
int ferror(FILE *__stream);

/*@ assigns __fc_stdout \from __FC_errno, __s[..]; */
void perror(const char *__s);

/*@ assigns \result,*__stream \from *__stream; */
int getc_unlocked(FILE *__stream);
/*@ assigns \result \from *__fc_stdin ; */
int getchar_unlocked(void);
/*@ assigns *__stream \from c; */
int putc_unlocked(int c, FILE *__stream);
/*@ assigns *__fc_stdout \from c; */
int putchar_unlocked(int c);

/*@  assigns *__stream  \from \nothing; */
void clearerr_unlocked(FILE *__stream);
/*@ assigns \result \from *__stream ;*/
int feof_unlocked(FILE *__stream);
/*@ assigns \result \from *__stream ;*/
int ferror_unlocked(FILE *__stream);
/*@ assigns \result \from *__stream ;*/
int fileno_unlocked(FILE *__stream);
int fflush_unlocked(FILE *__stream);
int fgetc_unlocked(FILE *__stream);
int fputc_unlocked(int c, FILE *__stream);
size_t fread_unlocked(void *ptr, size_t size, size_t n,
                             FILE *__stream);
size_t fwrite_unlocked(const void *ptr, size_t size, size_t n,
		       FILE *__stream);

char *fgets_unlocked(char *__s, int n, FILE *__stream);
int fputs_unlocked(const char *__s, FILE *__stream);

#ifdef _GNU_SOURCE

/*@ ghost extern int __fc_heap_status __attribute__((FRAMA_C_MODEL)); */

/*@ assigns __fc_heap_status \from fmt[..], __fc_heap_status; */
int asprintf(char **dest, const char *fmt, ...);

int vasprintf(char **dest, const char *fmt, va_list va);

#endif

#ifdef _POSIX_C_SOURCE
# if _POSIX_C_SOURCE >= 200112L
#include "__fc_define_off_t.h"

off_t ftello(FILE *__stream);
int   fseeko(FILE *__stream, off_t __offset, int __whence);

#endif
#endif

#define IOV_MAX 1024
__END_DECLS

#endif
