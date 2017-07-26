/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include <unistd.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

/* The function strtoull below comes from FreeBSD, under the
 * license:
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
unsigned long long
strtoull(const char *nptr, char **endptr, int base)
{
  const char *s;
  unsigned long long acc;
  char c;
  unsigned long long cutoff;
  int neg, any, cutlim;

  s = nptr;
  do {
    c = *s++;
  } while (isspace((unsigned char)c));
  if (c == '-') {
    neg = 1;
    c = *s++;
  } else {
    neg = 0;
    if (c == '+')
      c = *s++;
  }
  if ((base == 0 || base == 16) &&
      c == '0' && (*s == 'x' || *s == 'X') &&
      ((s[1] >= '0' && s[1] <= '9') ||
       (s[1] >= 'A' && s[1] <= 'F') ||
       (s[1] >= 'a' && s[1] <= 'f'))) {
    c = s[1];
    s += 2;
    base = 16;
  }
  if (base == 0)
    base = c == '0' ? 8 : 10;
  acc = any = 0;
  if (base < 2 || base > 36)
    goto noconv;

  cutoff = ULLONG_MAX / base;
  cutlim = ULLONG_MAX % base;
  for ( ; ; c = *s++) {
    if (c >= '0' && c <= '9')
      c -= '0';
    else if (c >= 'A' && c <= 'Z')
      c -= 'A' - 10;
    else if (c >= 'a' && c <= 'z')
      c -= 'a' - 10;
    else
      break;
    if (c >= base)
      break;
    if (any < 0 || acc > cutoff || (acc == cutoff && c > cutlim))
      any = -1;
    else {
      any = 1;
      acc *= base;
      acc += c;
    }
  }
  if (any < 0) {
    acc = ULLONG_MAX;
    errno = ERANGE;
  } else if (!any) {
noconv:
    errno = EINVAL;
  } else if (neg)
    acc = -acc;

  char *end = (char *)(any ? s - 1 : nptr);
  /* if execution crashes in strlen below, it means that the string passed to 
     strto*l is not a valid string in contradiction of C11 7.22.1.4:2 */
  strlen(end);

  if (endptr != NULL)
    *endptr = end;
  return (acc);
}

unsigned long
strtoul(const char *nptr, char **endptr, int base)
{
    unsigned long long res;
    res = strtoull(nptr, endptr, base);
    if (res > ULONG_MAX) {
        errno = ERANGE;
        res = ULONG_MAX;
    }

    return res;
}

long long
strtoll(const char *nptr, char **endptr, int base)
{
  const char *s;
  char c;
  unsigned long long res;
  int neg;
  char *end;
 
  s = nptr;
  do {
    c = *s++;
  } while (isspace((unsigned char)c));
 
  if (c == '-')
    neg = 1;
  else
    neg = 0;
 
  res = strtoull(nptr, &end, base);
 
  if (endptr) *endptr = end;
 
  if (end != nptr) {
    if (neg) {
      if (res != 0 && res < (unsigned long long)LLONG_MIN) {
        errno = ERANGE;
        res = LLONG_MIN;
      }
    }
    else if (res > (unsigned long long)LLONG_MAX) {
      errno = ERANGE;
      res = LLONG_MAX;
    }
  }
 
  return res;
}

long
strtol(const char *nptr, char **endptr, int base)
{
    long long res;
    res = strtoll(nptr, endptr, base);

    if (res > LONG_MAX) {
        errno = ERANGE;
        res = LONG_MAX;
    }
    else if (res < LONG_MIN) {
        errno = ERANGE;
        res = LONG_MIN;
    }

    return res;
}



/* iterative qsort implementation */
/*
 * This structure is used to handle position on the array.
 * The x contains the start and y contains the end.
 */
typedef struct pair {
    int x;
    int y;
} qsort_pair_t;

/*
 * This structure is the stack used by the algorithm to know what to sort.
 */
typedef struct qsort_stack {
    qsort_pair_t stack[64]; /* 64 should be enough for every implementations */
    int position;
} qsort_stack_t;

static void push(qsort_stack_t *s, qsort_pair_t p)
{
    if (s->position >= 64) {
        /*@ assert \false; */
    }

    s->stack[s->position++] = p;
}

static qsort_pair_t pop(qsort_stack_t *s)
{
    if (s -> position == 0) {
        /*@ assert \false; */
    }

    return s->stack[--s->position];
}

static void swap(char *p, int i, int j, size_t size) {
    char *pi = p + i * size;
    char *pj = p + j * size;

    for (size_t b = 0; b < size; b++) {
        char tmp = pi[b];
        pi[b] = pj[b];
        pj[b] = tmp;
    }    
}

static int partition(char *p, int start, int end, size_t size,
                     int (*cmp)(const void *, const void *))
{
    int i = start;

    for (int j = start; j <= end - 1; j++)
    {
        if (cmp(p + (j * size), p + (end * size)) <= 0)
        {
            swap (p, i, j, size);
            i++;
        }
    }
    
    swap (p, i, end, size);
    return i;
}

static void quick_stub(void *p, int start, int end, size_t size,
           int (*cmp)(const void *, const void *)) { 
    qsort_pair_t initial_pair = { .x = start, .y = end };
    qsort_stack_t stack = { .position = 0 };

    push(&stack, initial_pair);
    while (stack.position != 0) {
        qsort_pair_t next;
        int    middle;

        next = pop(&stack);
        if (next.x > next.y) { /* start > end */
            continue;
        }
        
        middle = partition(p, next.x, next.y, size, cmp);
        
        if (middle - 1 > next.x) {
            qsort_pair_t left  = { .x = next.x, .y = middle - 1 };
            push(&stack, left);
        }

        if (middle + 1 < next.y) {
            qsort_pair_t right = { .x = middle + 1, .y = next.y };
            push(&stack, right);
        }
    }
}

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *))
{
    quick_stub(base, 0, nmemb - 1, size, compar);
}
/* end of qsort implementation */
