/**************************************************************************/
/*                                                                        */
/*  This file is part of TrustInSoft Kernel.                              */
/*                                                                        */
/*    Copyright (C) 2013-2017 TrustInSoft                                 */
/*                                                                        */
/*  TrustInSoft Kernel is released under GPLv2                            */
/*                                                                        */
/**************************************************************************/

#ifndef	__FC_SCHED_H
#define	__FC_SCHED_H
#include "features.h"
__BEGIN_DECLS
#include "__fc_define_pid_t.h"
#include "__fc_define_time_t.h"
#include "__fc_define_timespec.h"

/* CPU sets are Linux-specific. */
#define CPU_SETSIZE 1024

typedef int cpu_set_t;

/*@ assigns *set \from \nothing; */
void CPU_ZERO(cpu_set_t *set);

/*@ assigns *set \from *set, cpu; */
void CPU_SET(int cpu, cpu_set_t *set);

/*@ assigns *set \from *set, cpu; */
void CPU_CLR (int cpu, cpu_set_t *set);

/*@ assigns \result \from cpu, *set; */
int CPU_ISSET (int cpu, const cpu_set_t *set);

/* */
struct sched_param {
    int sched_priority;
    int sched_ss_low_priority;
    struct timespec sched_ss_repl_period;
    struct timespec sched_ss_init_budget;
    int sched_ss_max_repl;
};

#define SCHED_FIFO 1
#define SCHED_RR 2
#define SCHED_SPORADIC 3
#define SCHED_OTHER 4

int sched_yield(void);
int sched_setscheduler(pid_t, int, const struct sched_param *);
int sched_setparam(pid_t, const struct sched_param *);
int sched_rr_get_interval(pid_t, struct timespec *);
int sched_getscheduler(pid_t);
int sched_getparam(pid_t, struct sched_param *);
int sched_get_priority_max(int);
int sched_get_priority_min(int);
__END_DECLS
#endif
