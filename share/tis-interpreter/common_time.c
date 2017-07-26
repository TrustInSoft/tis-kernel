/**************************************************************************/
/*                                                                        */
/*  This file is part of tis-interpreter.                                 */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  tis-interpreter is released under GPLv2                               */
/*                                                                        */
/**************************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/timeb.h>
#include <unistd.h>

/*

http://www.musl-libc.org/
Definitions of LEAPOCH, DAYS_PER_400Y, DAYS_PER_100Y, DAYS_PER_4Y and the
function __secs_to_tm comes from musl-libc, under the license:

----------------------------------------------------------------------
Copyright © 2005-2014 Rich Felker, et al.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
----------------------------------------------------------------------

 */

/* 2000-03-01 (mod 400 year, immediately after feb29 */
#define LEAPOCH (946684800LL + 86400*(31+29))

#define DAYS_PER_400Y (365*400 + 97)
#define DAYS_PER_100Y (365*100 + 24)
#define DAYS_PER_4Y   (365*4   + 1)

int __secs_to_tm(long long t, struct tm *tm)
{
	long long days, secs;
	int remdays, remsecs, remyears;
	int qc_cycles, c_cycles, q_cycles;
	int years, months;
	int wday, yday, leap;
	static const char days_in_month[] = {31,30,31,30,31,31,30,31,30,31,31,29};

	/* Reject time_t values whose year would overflow int */
	if (t < INT_MIN * 31622400LL || t > INT_MAX * 31622400LL)
		return -1;

	secs = t - LEAPOCH;
	days = secs / 86400;
	remsecs = secs % 86400;
	if (remsecs < 0) {
		remsecs += 86400;
		days--;
	}

	wday = (3+days)%7;
	if (wday < 0) wday += 7;

	qc_cycles = days / DAYS_PER_400Y;
	remdays = days % DAYS_PER_400Y;
	if (remdays < 0) {
		remdays += DAYS_PER_400Y;
		qc_cycles--;
	}

	c_cycles = remdays / DAYS_PER_100Y;
	if (c_cycles == 4) c_cycles--;
	remdays -= c_cycles * DAYS_PER_100Y;

	q_cycles = remdays / DAYS_PER_4Y;
	if (q_cycles == 25) q_cycles--;
	remdays -= q_cycles * DAYS_PER_4Y;

	remyears = remdays / 365;
	if (remyears == 4) remyears--;
	remdays -= remyears * 365;

	leap = !remyears && (q_cycles || !c_cycles);
	yday = remdays + 31 + 28 + leap;
	if (yday >= 365+leap) yday -= 365+leap;

	years = remyears + 4*q_cycles + 100*c_cycles + 400*qc_cycles;

	for (months=0; days_in_month[months] <= remdays; months++)
		remdays -= days_in_month[months];

	if (years+100 > INT_MAX || years+100 < INT_MIN)
		return -1;

	tm->tm_year = years + 100;
	tm->tm_mon = months + 2;
	if (tm->tm_mon >= 12) {
		tm->tm_mon -=12;
		tm->tm_year++;
	}
	tm->tm_mday = remdays + 1;
	tm->tm_wday = wday;
	tm->tm_yday = yday;

	tm->tm_hour = remsecs / 3600;
	tm->tm_min = remsecs / 60 % 60;
	tm->tm_sec = remsecs % 60;

	return 0;
}

struct tm *gmtime(const time_t *timer)
{
  long long l = *timer;
  __secs_to_tm(l, &__fc_time_tm);
  __fc_time_tm.tm_isdst = 0;
  return &__fc_time_tm;
}

struct tm *gmtime_r(const time_t *timer, struct tm *result)
{
  long long l = *timer;
  __secs_to_tm(l, result);
  result->tm_isdst = 0;
  return result;
}

// 2016-06-19
#define __CURRENT_TIME 1466335969L

time_t time(time_t *p)
{
  if (p) *p = __CURRENT_TIME;
  return __CURRENT_TIME;
}

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
  tv->tv_sec = __CURRENT_TIME;
  tv->tv_usec = 455745;
  return 0;
}

int ftime(struct timeb *timebuf)
{
  time(&timebuf->time);
  timebuf->millitm = 0;
  timebuf->timezone = 0;
  timebuf->dstflag = 0;
  return 0;
}
