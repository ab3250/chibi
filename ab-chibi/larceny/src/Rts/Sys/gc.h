/* Copyright 1998 Lars T Hansen             -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Public garbage collector interface.
 */

#ifndef INCLUDED_GC_H
#define INCLUDED_GC_H

#include "larceny-types.h"

#define KILOBYTE                     1024
#define MEGABYTE                     (KILOBYTE*KILOBYTE)

#define MAX_GENERATIONS              6400  /* If GCLIB_LARGE_TABLE is set */
#define MAX_REMSETS                  (2*MAX_GENERATIONS+2)
#define DEFAULT_AREAS                2
#define DEFAULT_NURSERY_SIZE         (4*MEGABYTE)
#define DEFAULT_STOPCOPY_SIZE        (8*MEGABYTE)
#define DEFAULT_EPHEMERAL_INCREMENT  (4*MEGABYTE)
#define DEFAULT_DYNAMIC_INCREMENT    (4*MEGABYTE)
#define DEFAULT_REGIONAL_NURSERY_SIZE (1*MEGABYTE)
#define DEFAULT_REGIONAL_REGION_SIZE (5*MEGABYTE)

#define DEFAULT_MMU_BUFFER_SIZE      5000

/* NP collector */
#define DEFAULT_STEPS                8
#define DEFAULT_STEPSIZE             (256*KILOBYTE)
#define DEFAULT_LOAD_FACTOR          3.0

/* RROF collector */
#define DEFAULT_LOAD_FACTOR_HARD     10.0
#define SMIRCY_MISC_BOUND            2000

/* Felix's dissertation described three sets of parameters that seem */
/* to work well.  One of those sets is wired in as the default set.  */
/* By changing the following defines, you can change that default.   */

#define PARAMETERS8221 1
#define PARAMETERS6121 0
#define PARAMETERS4231 0

struct nursery_info {           /* Generational gc nursery */
  int size_bytes;               /* size of area in bytes, > 0 */
};

struct sc_info {                /* Any two-space copying area */
  int    size_bytes;		/* Size of area in bytes, > 0 */
  double load_factor;           /* Inverse load factor (dynamic generation) */
  double load_factor_hard;      /* Strict load bound (above target is soft) */
  int    dynamic_min;		/* 0 or lower bound on expandable area */
  int    dynamic_max;		/* 0 or upper bound on expandable area */
};

struct np_info {                /* Non-predictive dynamic area */
  int    steps;                 /* Number of steps, > 0 */
  int    stepsize;              /* Size of a step in bytes, > 0 */
  int    size_bytes;            /* Total size */
  int    dynamic_min;           /* 0 or lower bound on expandable area */
  int    dynamic_max;           /* 0 or upper bound on expandable area */
  int    extra_remset_limit;    /* 0 .. INT_MAX */
  double load_factor;           /* Inverse load factor */
  double luck;                  /* 0.0 .. 1.0 (ought to have been 0 .. 6) */
  double phase_detection;       /* -1.0 or 0.0 .. 1.0 */
};

struct bdw_info {
  int    divisor;               /* Allocation divisor */
  double load_factor;           /* Inverse load factor */
  double expansion_factor;      /* Inverse expansion factor */
  int    dynamic_min;           /* 0 or lower bound on collected area */
  int    dynamic_max;           /* 0 or upper bound on collected area */
};

struct gc_param {               /* Parameter structure passed to create_gc() */
  /* Overall flags to select the mode */
  bool is_conservative_system;
  bool is_generational_system;
  bool is_stopcopy_system;
  bool is_regional_system;
  bool use_static_area;                /* In the nonconservative systems */
  bool use_non_predictive_collector;   /* In the generational system */
  bool use_incremental_bdw_collector;  /* In the conservative system */
  bool dont_shrink_heap;               /* In the nonconservative systems */
  bool use_oracle_to_update_remsets;   /* In the regional system. */
  int  mark_period;		       /* In the regional system. */
  bool   has_popularity_factor;	       /* In the regional system. */
  double popularity_factor;	       /* In the regional system. */
  bool   has_infamy_factor;	       /* In the regional system. */
  double infamy_factor;		       /* In the regional system. */
  bool   has_refine_factor;	       /* In the regional system. */
  double refinement_factor;	       /* In the regional system. */
  bool   alloc_mark_bmp_once;	       /* In the regional system. */
  bool   has_sumzbudget;	       /* In the regional system. */
  double sumzbudget_inv;	       /* In the regional system. */
  bool   has_sumzcoverage;	       /* In the regional system. */
  double sumzcoverage_inv;	       /* In the regional system. */
  bool   has_sumz_retries;	       /* In the regional system. */
  int    max_sumz_retries;	       /* In the regional system. */
  bool print_float_stats_cycle;        /* In the regional system. */
  bool print_float_stats_major;        /* In the regional system. */
  bool print_float_stats_minor;        /* In the regional system. */
  bool print_float_stats_refine;       /* In the regional system. */

  bool chose_rhashrep;
  bool chose_rbitsrep;

  /* Common parameters */
  word *globals;		/* globals table used by collector */

  /* Generational precise system */
  nursery_info_t nursery_info;
  int            ephemeral_area_count;  /* Number of ephemeral areas */
  sc_info_t      *ephemeral_info;       /* an array of these */
  sc_info_t      dynamic_sc_info;
  np_info_t      dynamic_np_info;

  /* Stop-and-copy precise system */
  sc_info_t sc_info;

  /* Conservative system */
  bdw_info_t bdw_info;

  /* Remembered-set values (could be set-by-set; are global) */
  unsigned rhash;		/* # elements in each remset hash tbl */
  unsigned ssb;			/* # elements in each remset SSB */

  int mmu_buf_size;             /* If 0, use default; if < 0, no MMU stats */

  bool rrof_prefer_big_summ;
  bool rrof_prefer_lil_summ;
  bool rrof_prefer_lat_summ;

  int oracle_countdown;         /* 0 => none; 1 => oracle; o/w countdown */
};

/* In memmgr.c */
gc_t *create_gc( gc_param_t *params, /* OUT */ int *actual_generations );
gc_t *create_bdw_gc( gc_param_t *params, /* OUT */ int *actual_generations );

#endif

/* eof */
