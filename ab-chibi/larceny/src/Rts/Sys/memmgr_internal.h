struct promotion_counts {
  int words_promoted;
  int count_promotions;
};

typedef struct gc_data gc_data_t;

/* The 'remset' table in the gc structure has one extra element if the
   collector uses the non-predictive dynamic area: that extra element is
   the young->old remembered set for the non-predictive collector.  The
   extra element is always the last one, and the slots in the SSB tables
   for that remset are the last slots.  The index is indicated by the
   np_remset member of gc_t.
   */
struct gc_data {
  bool is_partitioned_system;   /* True if system has partitioned heap */
  bool use_np_collector;	/* True if dynamic area is non-predictive */
  bool shrink_heap;		/* True if heap can be shrunk */
  bool fixed_ephemeral_area;    /* True iff ephemeral_area_count is invariant */
  bool remset_undirected;       /* Regional (vs gen'l directed remsets) */
  bool mut_activity_bounded;    /* True for RROF alone (for now). */

  int  dynamic_min;		/* 0 or lower limit of expandable area */
  int  dynamic_max;		/* 0 or upper limit of expandable area */
  int  nonexpandable_size;	/* Size of nonexpandable areas (but RROF?) */

  word *globals;
  word *handles;               /* array of handles */
  int  nhandles;               /* current array length */
  int  in_gc;                  /* a counter: > 0 means in gc */
  int  generations;            /* number of generations (incl. static) */
  int  generations_after_gc;   /* number of generations in rts after gc complete */
  int  static_generation;	/* Generation number of static area */
  word **ssb_bot;
  word **ssb_top;
  word **ssb_lim;
  word *satb_ssb_bot;
  word *satb_ssb_top;
  word *satb_ssb_lim;
  int  ssb_entry_count;

  old_heap_t **ephemeral_area;
    /* In precise collectors: An array of pointers to ephemeral areas;
       the number of entries is held in ephemeral_area_count.  May be NULL.
       */
  int ephemeral_area_count;
    /* The number of entries in the ephemeral_area table.
       */
  int region_count;
    /* For regional collector.  During cycle of collections, the regions
        { ephemeral_area[i] | region_count <= i < ephemeral_area_count }
       are new and should not be processed until cycle completes
       */
  old_heap_t *dynamic_area;
    /* In precise collectors: A pointer to a dynamic area, or NULL.
       */

  bool rrof_currently_minor_gc;
    /* true implies the collector is currently doing a minor collection. */
  int rrof_to_region;
    /* In RROF collector, the to-space for minor collections. */
  int rrof_next_region;
    /* In RROF collector, the next region scheduled for major collection. */
  int rrof_last_tospace;
    /* In RROF collector, the region used as a to-space in the last collect */
  int rrof_cycle_majors_sofar;
    /* In RROF collector, number of major gc's so far during current cycle. */
  int rrof_cycle_majors_total;
    /* In RROF collector, number of major gc's of complete current cycle.
     */

  double rrof_load_factor_soft; /* L_soft */
  double rrof_load_factor_hard; /* L_hard */
    /* Lars put a load factor in each old-heap; RROF uses a uniform policy
       for all the regions.  The two factors serve distinct roles: 
       we aim to keep the heap at size L_soft*Peak
       and *guarantee* that the heap never exceeds L_hard*Peak
       (where Peak is the peak size for past snapshots). */

  struct {
    /* limits size of summaries */
    double popularity_factor; /* Will calls this S. */
    int popularity_limit_words; 
    double infamy_factor;

    double coverage_inv;
    /* denoted by C in comments below; Will calls this F1 */
    double budget_inv;
    /* denoted by B in comments below; Will calls this F2 */
    int max_retries;
    /* Will calls this F3; XXX still needs documentation in code */

    /* In RROF collector, (1/C)*(N/R) is number of summaries that we
       will try to construct ("summary coverage") during each heap 
       scan during a wave of summary construction, and 
       (1/C)*(1/B)*(N/R) is the number of summaries that we need to
       have fully constructed (ie none waved off) before we declare
       that wave of summary construction complete ("met budget").
    */
  } rrof_sumz_params;

  bool   rrof_has_refine_factor; /* With factor R,                         */
  double rrof_refinement_factor; /*   countdown = ceil((R*heap) / nursery) */
  bool rrof_alloc_mark_bmp_once;
    /* Allocate one bitmap to cover *entire* addr range at outset. 
     * (band-aid for hack of expanding bitmap on the fly during 
     *  cheney object forwards). */
  int rrof_refine_mark_period;
  int rrof_refine_mark_countdown;
    /* In RROF collector, #nursery evacuations until refine remset via mark.
       If negative, then that is number of nursery evacuations we've had
       since the mark was scheduled to occur. */

  int rrof_last_live_estimate; 
    /* In RROF collector, gradual approximation of live storage in bytes.
     * (It is continually reset based on marking results and then 
     *  refined by repeated averaging with the sum of major collection
     *  sizes.)
     */
  int rrof_cycle_count;
    /* In RROF collector, number of cycles that have been completed
     * (a cycle is complete when every region present at the start
     *  of the cycle has been considered to take part in a major
     *  collection.)
     */
  bool rrof_last_gc_rolled_cycle;

  bool rrof_prefer_big_summ;
  bool rrof_prefer_lil_summ;
  bool rrof_prefer_lat_summ;

  int rrof_words_per_region_max; /* opts allows region sizes to differ; */
  int rrof_words_per_region_min; /* track min and max for policy calc's */

  bool enumerate_major_with_minor_remsets;
  summary_t summary;            /* NULL or summarization of remset array */
  bool      use_summary_instead_of_remsets;
  int       next_summary_to_use;
  summ_matrix_t *summaries;

  semispace_t *secondary_space; /* NULL or space for when tospace overflows */

  gc_mmu_log_t *mmu_log;

  stats_id_t pause_timer_elapsed;
  stats_id_t pause_timer_cpu;
  int last_pause_elapsed;
  int last_pause_cpu;
  unsigned major_page_fault_count_at_gc_start;
  unsigned minor_page_fault_count_at_gc_start;

  int stat_last_ms_remset_sumrize;
  int stat_last_ms_remset_sumrize_cpu;
  int stat_last_ms_smircy_mark;
  int stat_last_ms_smircy_mark_cpu;
  int stat_last_ms_smircy_refine;
  int stat_last_ms_smircy_refine_cpu;
  int stat_length_minor_gc_run;

  bool print_float_stats_each_cycle;
  bool print_float_stats_each_major;
  bool print_float_stats_each_minor;
  bool print_float_stats_each_refine;

  /* these are precise measures according to heap snapshots */
  int last_live_words;
  int max_live_words;

  int last_live_words_at_time_cycle_began;    /* N_old */
  int max_live_words_at_time_cycle_began;     /* P_old */

  int total_heap_words_allocated; 
  int allocation_target_for_cycle; /* Will's A variable */

  /* need to track these separately, since storage is allocated
   * concurrently with snapshotting. */
  struct promotion_counts since_finished_snapshot_began;
  struct promotion_counts since_developing_snapshot_began;
  struct promotion_counts since_cycle_began;

  struct promotion_counts since_finished_snapshot_at_time_cycle_began_began;
  /* parse the above name as ``promotions since the beginning of the
     last completed snapshot at the time when this cycle itself
     began.'' */

  struct mutator_effort {
    int rrof_ssb_flushes;
    long long rrof_ssb_entries_flushed_total;
    struct {int full_cycle; int sumz_cycle;} rrof_ssb_entries_flushed_this;
    struct {int full_cycle; int sumz_cycle;} rrof_ssb_max_entries_flushed_any;

    int satb_ssb_flushes;
    long long satb_ssb_entries_flushed_total;
    struct {int full_cycle; int sumz_cycle;} satb_ssb_entries_flushed_this;
    struct {int full_cycle; int sumz_cycle;} satb_ssb_max_entries_flushed_any;

    struct {int full_cycle; int sumz_cycle;} words_promoted_this;
    struct {int full_cycle; int sumz_cycle;} max_words_promoted_any;

    bool forcing_collector_to_progress;
  } mutator_effort;

  int oracle_countdown;
  int oracle_pointsrun;

  int rrof_mark_cycles_begun_in_this_full_cycle;
  int rrof_mark_cycles_run_in_this_full_cycle;
  bool rrof_smircy_step_on_minor_collections_alone;
};

#define DATA(gc) ((gc_data_t*)(gc->data))

#define INCLUDE_POP_RGNS_IN_LOADCALC 1
