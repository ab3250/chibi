/* Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
 *
 * $Id$
 *
 * Larceny run-time system -- garbage collector data structure.
 */

#ifndef INCLUDED_GC_T_H
#define INCLUDED_GC_T_H

#include "config.h"
#include "larceny-types.h"
#include "gset_t.h"
#include "smircy.h"
#include "summary_t.h" /* for loc_t definition */

typedef enum {
  gno_state_normal,  /* default */
  gno_state_popular, /* points-into summary overflowed recently */
  gno_state_polling, /* measuring current points-into state */
  gno_state_hasbeen /* last measured points-into state was acceptably small */
} gno_state_t;

struct gc { 
  char *id;
    /* A human-readable string identifying the collector, its heaps,
       and its policies.
       */

  los_t *los;
    /* In precise collectors: A large-object-space data structure.
       */

  young_heap_t *young_area;
    /* In precise collectors: A pointer to the allocation area (a nursery
       or a stop-and-copy heap).
       */

  static_heap_t *static_area;
    /* In precise collectors: A pointer to a static area, or NULL.
       */

  uremset_t *the_remset;
    /* In precise collectors: A pointer to the unified remset, or NULL. 
       */

  smircy_context_t *smircy;
    /* The current mark/refine context, or NULL if no mark has been
       initiated since last refinement was completed.
       */

  void *smircy_completion;
    /* A completed clone of the smircy object.  (Its a msgc_context_t*)
       */

  seqbuf_t **ssb;
    /* ssb[i] is sequential store buffer for region i.
       The barrier associated with an update to object o (in region j
       with a reference into region i != j) inserts o into SSB[i].
       When SSB[i] is full, the SSB processing function determines how
       to distribute its contents across the remsets and summaries.
       */

  seqbuf_t *satb_ssb;
  /* Sequential store buffer for the Yuasa-style write barrier of
     snapshot-at-the-beginning incremental marking algorithm. */

  int gno_count;
    /* The number of generations/regions.
       */

  int np_remset;
    /* In a non-predictive collector, the index in the remset array of
       the extra non-predictive remembered set, otherwise -1.
       */

  int scan_update_remset;
    /* 1 iff is a collector where objects may be forwarded into
       distinct generations and therefore the remembered sets of
       referring objects must be updated during cheney scan; otherwise 0.
       (This might be synonymous with the barrier_gc flag in cheney_env_t; 
       Felix cannot tell from the current codebase.)
       */

  int stat_max_entries_remset_scan;
  long long stat_total_entries_remset_scan;
  int stat_max_remset_scan;
  int stat_max_remset_scan_cpu;
  int stat_total_remset_scan;
  int stat_total_remset_scan_cpu;
  int stat_remset_scan_count;

  int stat_last_ms_gc_cheney_pause;
  int stat_last_ms_gc_cheney_pause_cpu;
  int stat_last_gc_pause_ismajor;

  int words_from_nursery_last_gc;

  void *data;
    /* Private data.
       */

  int (*initialize)( gc_t *gc );
    /* A method that is run after any other system initialization has
       taken place.  It runs the initialize() method on each heap
       controlled by the collector, and initializes the write barrier.
       */

  word *(*allocate)( gc_t *gc, int nbytes, bool no_gc, bool atomic );
    /* A method that allocates an object of size at least `nbytes'.  
       If `no_gc' == 1, then no garbage collection may be performed 
       during the allocation.  `nbytes' must not be larger than 
       LARGEST_OBJECT (defined in "larceny.h").
       Returns a pointer to the allocated object.

       FIXME: a more self-documenting interface would take a single int
       argument that would be the bitewise OR of e.g.
          ALLOC_NO_GC
	  ALLOC_ATOMIC_DATUM
       */

  word *(*allocate_nonmoving)( gc_t *gc, int nbytes, bool atomic );
    /* A method that allocates a non-moving object of size at least `nbytes'.
       Returns a pointer to the allocated object.
       */

  void (*make_room)( gc_t *gc );
    /* Ensures that at least 4KB of contiguous space is available at 
       the allocation pointer; does not perform any actual allocation.
       */

  void (*collect)( gc_t *gc, int gen, int bytes_needed, gc_type_t type );
    /* A method that requests that a garbage collection be performed in
       generation `gen', such that at least `bytes_needed' bytes can be
       allocated following the collection.
       */

  void (*incremental)( gc_t *gc );
    /* A method that allows some work to be done by incremental collectors.
       May be called thousands of times per second.
       */

  void (*set_policy)( gc_t *gc, int heap, int x, int y );

  word *(*data_load_area)( gc_t *gc, int nbytes );
    /* Return a pointer to a data area with the following properties:
       - it is contiguous
       - it will hold exactly as many bytes as requested 
       - it is uninitialized and may hold garbage
       - it is actually allocated - further calls to this function will not
         return a pointer to the area
       */

  word *(*text_load_area)( gc_t *gc, int nbytes );

  int  (*iflush)( gc_t *gc, int generation );
    /* A method that returns 1 if the instruction cache must be flushed
       after collecting the named generation.
       */

  word (*creg_get)( gc_t *gc );
  void (*creg_set)( gc_t *gc, word k );
  void (*stack_overflow)( gc_t *gc );
  void (*stack_underflow)( gc_t *gc );

  /* Remembered sets */
  int  (*compact_all_ssbs)( gc_t *gc );

#if defined(SIMULATE_NEW_BARRIER)
  /* Support for simulated write barrier */
  int (*isremembered)( gc_t *gc, word w );
#endif

  /* Support for non-predictive collector */
  void (*compact_np_ssb)( gc_t *gc );
  void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim );

  int  (*dump_heap)( gc_t *gc, const char *filename, bool compact );
    /* Method that dumps the heap image into the named file.  Compact
       the heap first iff compact is non-zero.

       Returns 0 on success, a negative error code (defined in heapio.h)
       on error.
       */

  int  (*load_heap)( gc_t *gc, heapio_t *h );
    /* Method that loads the heap image from the file into the heap.
       The heap image in the file must be recognizable by the garbage
       collector; different collectors use different heap formats.

       If the heap image was not recognized or could not be loaded,
       0 is returned, otherwise 1.
       */

  word *(*make_handle)( gc_t *gc, word obj );
    /* Store obj in a location visible to the garbage collector and and 
       return a pointer to the location.  The location may be modified
       and referenced through the pointer, but the contents of the
       location may be changed by the garbage collector.
       */
       
  void (*free_handle)( gc_t *gc, word *handle );
    /* Given a handle returned by make_handle(), return the location
       to the pool of available locations.
       */

  /* PRIVATE */
  /* Internal to the collector implementation. */
  gno_state_t (*gno_state)( gc_t *gc, int gno );
  void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *), void * );
  void (*enumerate_smircy_roots)( gc_t *gc, void (*f)( word*, void *), void * );
  void (*enumerate_remsets_complement)( gc_t *gc, gset_t genset,
				        bool (*f)(word, void*),
				        void * );
     /* Invokes f on every word in the remsets in the complement of genset.
        If f returns TRUE then word argument is retained in the remset 
        being traversed; otherwise word is removed (see interface for 
        rs_enumerate() for more info).
        */
  void (*enumerate_remembered_locations)( gc_t *gc, gset_t genset, 
                                          void (*f)( loc_t, void* ), void*fd,
                                          bool (*g)( word, void* ), void*gd );
     /* Invokes f on a superset of locations (each represented as
      * tagged-word + byte offset) in the remembered set, passing
      * along the accumulator scan_data.
      * OR 
      * Invokes enumerate_remsets_complement( gc, genset, g, gd )
      *
      * (Its the receivers choice.)
      */

  void (*enumerate_hdr_address_ranges)( gc_t *gc, int gno, 
                                        void (*f)( word *s,word *l,void *d ),
                                        void *d );
     /* Invokes f on series of address ranges [s,l) for gno.
      * Guarantees: 
      * - Every [s,l) will represent a half-open range [s,l+k) of 
      *   well-formatted storage (for some k >= 0).
      * - For every object o in generation/region gno, f will 
      *   eventually be invoked on a range that covers the start of o.
      *
      * Note that the enumeration might include headers (or first
      * words) of objects (pairs) unreachable from the roots.
      */

  semispace_t *(*fresh_space)(gc_t *gc);
     /* Creates a fresh space to copy objects into with a 
      * distinct generation number.
      */
  semispace_t *(*find_space)(gc_t *gc, unsigned bytes_needed, semispace_t *ss);
     /* modifies: ss
      * The returned semispace is guaranteed to have sufficient space
      * to store an object of size bytes_needed.  It will either be ss
      * or an entirely fresh semispace (but even when it is fresh, it
      * may share the same gen_no as another semispace).
      */
  
  int (*allocated_to_areas)( gc_t *gc, gset_t gs );
  int (*maximum_allotted)( gc_t *gc, gset_t gs );
  bool (*is_nonmoving)( gc_t *gc, int gen_no );
  bool (*is_address_mapped)( gc_t *gc, word *addr, bool noisy );
  void (*check_remset_invs)( gc_t *gc, word src, word tgt );
  void (*points_across)( gc_t *gc, word lhs, int offset, word rhs );
  old_heap_t *(*heap_for_gno)(gc_t *gc, int gen_no );
  region_group_t (*region_group_for_gno)(gc_t *gc, int gen_no );

  void (*check_invariants_between_fwd_and_free)( gc_t *gc, int gen_no );
};

/* Operations.  For prototypes, see the method specs above. */

#define gc_initialize( gc )           ((gc)->initialize( gc ))
#define gc_allocate( gc, n, nogc, a ) ((gc)->allocate( gc, n, nogc, a ))
#define gc_allocate_nonmoving( gc,n,a ) ((gc)->allocate_nonmoving( gc, n,a ))
#define gc_make_room( gc )            ((gc)->make_room( gc ))
#define gc_collect( gc,gen,n,t )      ((gc)->collect( gc,gen,n,t ))
#define gc_incremental( gc )          ((gc)->incremental( gc ))
#define gc_set_policy( gc,h,x,y )     ((gc)->set_policy( gc,h,x,y ))
#define gc_data_load_area( gc,n )     ((gc)->data_load_area( gc,n ))
#define gc_text_load_area( gc,n )     ((gc)->text_load_area( gc,n ))
#define gc_iflush( gc )               ((gc)->iflush( gc, -1 ))
#define gc_creg_get( gc )             ((gc)->creg_get( gc ))
#define gc_creg_set( gc,k )           ((gc)->creg_set( gc, k ))
#define gc_stack_overflow( gc )       ((gc)->stack_overflow( gc ))
#define gc_stack_underflow( gc )      ((gc)->stack_underflow( gc ))
#define gc_compact_all_ssbs( gc )     ((gc)->compact_all_ssbs( gc ))
#if defined(SIMULATE_NEW_BARRIER)
#define gc_isremembered( gc, w )      ((gc)->isremembered( gc, w ))
#endif
#define gc_compact_np_ssb( gc )       ((gc)->compact_np_ssb( gc ))
#define gc_dump_heap( gc, fn, c )     ((gc)->dump_heap( gc, fn, c ))
#define gc_load_heap( gc, h )         ((gc)->load_heap( gc, h ))
#define gc_gno_state( gc,n )          ((gc)->gno_state( (gc),(n) ))
#define gc_enumerate_roots( gc,s,d )  ((gc)->enumerate_roots( gc, s, d ))
#define gc_enumerate_smircy_roots( gc,s,d ) \
  ((gc)->enumerate_smircy_roots( (gc),(s),(d) ))
#define gc_np_remset_ptrs( gc, t, l ) ((gc)->np_remset_ptrs( gc, t, l ))
#define gc_enumerate_remsets_complement( gc, gset, s, d ) \
  ((gc)->enumerate_remsets_complement( gc, gset, s, d ))
#define gc_enumerate_remembered_locations( gc, gset, s, d, s2, d2) \
  ((gc)->enumerate_remembered_locations( gc, gset, s, d, s2, d2 ))
#define gc_enumerate_hdr_address_ranges( gc, gno, s, d ) \
  ((gc)->enumerate_hdr_address_ranges( gc, gno, s, d ))
#define gc_make_handle( gc, o )       ((gc)->make_handle( gc, o ))
#define gc_free_handle( gc, h )       ((gc)->free_handle( gc, h ))
#define gc_find_space( gc, n, ss )    ((gc)->find_space( gc, n, ss ))
#define gc_fresh_space( gc )          ((gc)->fresh_space( gc ))

#define gc_allocated_to_areas( gc, gs ) ((gc)->allocated_to_areas( gc, gs ))
#define gc_maximum_allotted( gc, gs )   ((gc)->maximum_allotted( gc, gs ))
#define gc_is_nonmoving( gc, gno )      ((gc)->is_nonmoving( (gc), (gno) ))
#define gc_is_address_mapped( gc,a,n )  ((gc)->is_address_mapped( (gc), (a), (n) ))
#define gc_check_remset_invs( gc,s,t )  ((gc)->check_remset_invs( (gc), (s), (t) ))
#define gc_points_across( gc,l,o,r )    ((gc)->points_across( (gc), (l), (o), (r) ))
#define gc_heap_for_gno( gc,gno )       ((gc)->heap_for_gno( (gc), (gno) ))
#define gc_region_group_for_gno( gc,gno ) ((gc)->region_group_for_gno( (gc), (gno) ))

#define gc_check_invariants_between_fwd_and_free( gc, from_gno )        \
  ((gc)->check_invariants_between_fwd_and_free( (gc), (from_gno) ))

extern void gc_check_rise_to_infamy( gc_t *gc, 
                                     old_heap_t *heap, 
                                     int incoming_words_estimate );

extern void gc_check_infamy_drop_to_hasbeen( gc_t *gc, 
                                             old_heap_t *heap, 
                                             int incoming_words_estimate );

gc_t 
*create_gc_t(char *id,
	     void *data,
	     int  (*initialize)( gc_t *gc ),
	     word *(*allocate)( gc_t *gc, int nbytes, bool no_gc, bool atomic),
	     word *(*allocate_nonmoving)( gc_t *gc, int nbytes, bool atomic ),
	     void (*make_room)( gc_t *gc ),
	     void (*collect)( gc_t *gc, int gen, int bytes, gc_type_t req ),
             void (*incremental)( gc_t *gc ),
	     void (*set_policy)( gc_t *gc, int heap, int x, int y ),
	     word *(*data_load_area)( gc_t *gc, int nbytes ),
	     word *(*text_load_area)( gc_t *gc, int nbytes ),
	     int  (*iflush)( gc_t *gc, int generation ),
	     word (*creg_get)( gc_t *gc ),
	     void (*creg_set)( gc_t *gc, word k ),
	     void (*stack_overflow)( gc_t *gc ),
	     void (*stack_underflow)( gc_t *gc ),
	     int  (*compact_all_ssbs)( gc_t *gc ),
#if defined(SIMULATE_NEW_BARRIER)
	     int  (*isremembered)( gc_t *gc, word w ),
#endif
	     void (*compact_np_ssb)( gc_t *gc ),
	     void (*np_remset_ptrs)( gc_t *gc, word ***ssbtop, word ***ssblim),
	     int  (*load_heap)( gc_t *gc, heapio_t *h ),
	     int  (*dump_heap)( gc_t *gc, const char *filename, bool compact ),
	     word *(*make_handle)( gc_t *gc, word object ),
	     void (*free_handle)( gc_t *gc, word *handle ),
	     gno_state_t (*gno_state)( gc_t *gc, int gno ), 
	     void (*enumerate_roots)( gc_t *gc, void (*f)( word*, void *),
				     void * ),
	     void (*enumerate_smircy_roots)( gc_t *gc, 
				             void (*f)( word*, void *),
				             void * ),
	     void (*enumerate_remsets_complement)
	        ( gc_t *gc, gset_t genset,
		  bool (*f)(word, void*),
		  void *data ),
	     void (*enumerate_remembered_locations)
	        ( gc_t *gc, gset_t genset, 
	          void (*f)( loc_t, void* ), void*fd,
	          bool (*g)( word, void* ), void*gd),
	     void (*enumerate_hdr_address_ranges)
	        ( gc_t *gc, int gno, 
	          void (*f)( word *s,word *l,void *d), void *d),
	     semispace_t *(*fresh_space)( gc_t *gc ),
	     semispace_t *(*find_space)( gc_t *gc, unsigned bytes_needed,
					 semispace_t *ss ),
	     int (*allocated_to_areas)( gc_t *gc, gset_t gs ),
	     int (*maximum_allotted)( gc_t *gc, gset_t gs ),
	     bool (*is_nonmoving)( gc_t *gc, int gen_no ),
	     bool (*is_address_mapped)( gc_t *gc, word *addr, bool noisy ),
	     void (*check_remset_invs)( gc_t *gc, word src, word tgt ),
	     void (*points_across)( gc_t *gc, word lhs, int offset, word rhs ),
	     old_heap_t *(*heap_for_gno)(gc_t *gc, int gen_no ),
	     region_group_t (*region_group_for_gno)(gc_t *gc, int gen_no ),
	     void (*check_invariants_between_fwd_and_free)( gc_t *gc, int gen_no )
	     );

void gc_parameters( gc_t *gc, int op, int *ans );

#endif   /* INCLUDED_GC_T_H */

/* eof */
