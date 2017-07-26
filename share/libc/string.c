/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#include "string.h"
#include "stdlib.h"

char *strcpy(char *s1, const char *s2)
{
  size_t l = strlen(s2);
  l++;
  memcpy(s1, s2, l);
  return s1;
}

char *strdup(const char *s)
{
  size_t l = strlen(s);
  l++;
  char *p = malloc(l);
  memcpy(p, s, l);
  return p;
}

// stpcpy is POSIX.1-2008
#ifdef _POSIX_C_SOURCE
# if _POSIX_C_SOURCE >= 200809L
char *stpcpy(char * dest, const char * from)
{
  char c;
  while (1) {
    c = *from;
    *dest = c;
    if (!c) break;
    else {
      from++;
      dest++;
    }
  }
  return dest;
}
# endif
#endif

size_t strspn(const char *s1, const char *s2)
{
  const char *p = s1, *spanp;
  char c, sc;
  size_t ret;
cont:
  c = *p;
  spanp = s2;
  while (1) {
    sc = *spanp;
    if (!sc) break;
    else if (sc == c) {
      p++;
      goto cont;
    }
    else {
      spanp++;
    }
  }
  ret = p - s1;
  return ret;
}

/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */
char *strncpy(char *s1, const char *s2, size_t n)
{
  size_t l = strnlen(s2, n);
  if (l < n) {
    /*@ assert \separated(s1 + (0 .. n-1), s2 + (0 .. l)); */
    memset(s1, 0, n);
  }

  memcpy(s1, s2, l);
  return s1;
}

char *strrchr(const char *s, int c)
{
  char *ret;
  c = (char)c;
  if (c) {
    ret = 0;
    while (1) {
      s = strchr(s, c);
      if (!s) {
        break;
      }
      else {
        ret = s;
        s++;
      }
    }
  }
  else
    ret = strchr(s, c);
  return ret;
}

char *strstr(const char *str, const char *sub)
{
  int c;
  char *ret = 0;
  size_t subl = strlen(sub);
  size_t strl = strlen(str);
  if (subl <= strl) {
    strl -= subl;
    while (1) {
      c = memcmp(str, sub, subl);
      if (!c) {
        ret = str;
        break;
      }
      else if (!strl) break;
      else {
        str++; strl--;
      }
    }
  }
  return ret;
}

char *strncat(char *dest, const char *src, size_t n)
{
  size_t dlen = strlen(dest);
  char *w = dest + dlen;
  size_t slen = strlen(src);
  if (slen < n) {
    memcpy(w, src, slen + 1);
  }
  else {
    memcpy(w, src, n);
    /*@ assert \separated(w + slen, src + (0 .. n-1)); */
    w[slen] = '\0';
  }
  return dest;
}

char *strerror(int errnum)
{
  char *res = "strerror message by tis-interpreter";
  return res;
}

int strcasecmp(const char *s1, const char *s2)
{
  int c1, c2;
  int res;
  while (1) {
    c1 = (unsigned char)*s1;
    c2 = (unsigned char)*s2;
    res = c1 - c2 + 32 * ((c1 >= 'A' & c1 <= 'Z') - (c2 >= 'A' & c2 <= 'Z'));
    if ((!c1) | (res != 0)) break;
    else {
      s1++; s2++;
    }
  }
  return res;
}

int strncasecmp (const char *s1, const char *s2, size_t n) {
  int c1, c2;
  int res = 0;
  while (1) {
    if (!n) break;
    else {
      c1 = (unsigned char)*s1;
      c2 = (unsigned char)*s2;
      res = c1 - c2 + 32 * ((c1 >= 'A' & c1 <= 'Z') - (c2 >= 'A' & c2 <= 'Z'));
      if ((!c1) | (res != 0)) break;
      else {
        n--; s1++; s2++;
      }
    }
  }
  return res;
}

size_t
strlcpy(char *dst, const char *src, size_t n)
{
    size_t srclen = strlen(src);
    size_t cnt;

    if (n != 0) {
        cnt = n - 1;
        if (cnt > srclen)
            cnt = srclen;
        if (cnt != 0) {
            memcpy(dst, src, cnt);
            dst[cnt] = '\0';
        }
    }

    return srclen;
}

size_t
strlcat(char *dst, const char *src, size_t n)
{
    size_t dstlen;
    size_t srclen = strlen(src);
    size_t cnt = n;

    while (cnt != 0 && *dst != '\0') {
        --cnt; ++dst;
    }
    if (cnt == 0) {
        /* no NUL terminator found */
        return n + srclen;
    }

    dstlen = n - cnt;
    --cnt; /* room for NUL */
    if (cnt > srclen)
        cnt = srclen;
    if (cnt != 0) {
        memcpy(dst, src, cnt);
        dst[cnt] = '\0';
    }

    return dstlen + srclen;
}
