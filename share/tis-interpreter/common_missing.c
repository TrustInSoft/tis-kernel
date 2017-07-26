/**************************************************************************/
/*                                                                        */
/*  This file is part of tis-interpreter.                                 */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  tis-interpreter is released under GPLv2                               */
/*                                                                        */
/**************************************************************************/

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "__fc_builtin.h"


unsigned int getpid(void)
{
  return 42;
}

void empty_handler(int i) {}
void (*signal(int signum, void (*handler)(int)  ))(int)
{
    return empty_handler;
}

int vfprintf(FILE *stream, const char *format, va_list ap)
{
    Frama_C_show_each_vfprintf(stream, format, ap);
    return 0;
}

void abort(void) {
}

int atoi(const char *nptr) {
  return (int)strtol(nptr, (char **)NULL, 10);
}

/*@ assigns \nothing; // wrong, obviously */
int sscanf(const char *s, const char *fmt, ...);

int puts(const char *s)
{
  return printf("%s\n", s);
}

int fputs(const char *s, FILE *stream)
{
  extern FILE __fc_initial_stdout;
  if (stream == &__fc_initial_stdout)
    return printf("%s", s);
  extern FILE __fc_initial_stderr;
  if (stream == &__fc_initial_stdout)
    return fprintf(stderr, "%s", s);
  return fprintf(stream, "%s", s);
}
