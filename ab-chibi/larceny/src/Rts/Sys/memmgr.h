/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 * 
 * Larceny run-time system -- internal interfaces for precise GC.
 */

#ifndef INCLUDED_MEMMGR_H
#define INCLUDED_MEMMGR_H

#include <stdio.h>
#include "larceny-types.h"
#include "gclib.h"

/* GC parameters */

#define GC_LARGE_OBJECT_LIMIT    PAGESIZE
#define GC_CHUNK_SIZE            (256*1024)

/* Heap codes (for load/dump matching) */

#define HEAPCODE_STATIC_2SPACE   0     /* text & data areas */
#define HEAPCODE_YOUNG_2SPACE    1     /* normal two-space */
#define HEAPCODE_OLD_2SPACE      2     /* normal two-space */
#define HEAPCODE_OLD_2SPACE_NP   3     /* non-predictive two-space */
#define HEAPCODE_YOUNG_1SPACE    4     /* nursery */
#define HEAPCODE_DOF             5     /* deferred-oldest-first */

/* Policy codes (for set_policy()).  See also Lib/gcctl.sch. */

#define GCCTL_J_FIXED            0
#define GCCTL_J_PERCENT          1
#define GCCTL_INCR_FIXED         2
#define GCCTL_INCR_PERCENT       3
#define GCCTL_DECR_FIXED         4
#define GCCTL_DECR_PERCENT       5
#define GCCTL_HIMARK             6
#define GCCTL_LOMARK             7
#define GCCTL_OFLOMARK           8
#define GCCTL_GROW               9

/* In memmgr.c */

int gc_compute_dynamic_size( gc_t *gc, int D, int S, int Q, double L, 
			     int lower_limit, int upper_limit );

void gc_signal_moving_collection( gc_t *gc );
  /* Increment the moving gc counters
     (globals[G_GC_CNT] and globals[G_MAJORGC_CNT])
     to tell clients that object addresses may have changed.
     */
    
void gc_signal_minor_collection( gc_t *gc );
  /* Adjust globals[G_MAJORGC_CNT] 
     to tell clients that the addresses of objects that had
     already survived a previous collection have not changed.
     */
    
void gc_dump_mmu_data( gc_t *gc, FILE *f );

/* In nursery.c */

young_heap_t *
create_nursery( int gen_no, gc_t *gc, nursery_info_t *info, word *globals );

/* In sc-heap.c */

young_heap_t *
create_sc_heap( int gen_no, gc_t *gc, sc_info_t *info, word *globals );
  /* Create a stop-and-copy expandable young area.
     */

semispace_t *
yhsc_data_area( young_heap_t *heap );
  /* Returns the current semispace structure for a stop-and-copy expandable
     young area.
     */

/* In old-heap.c */

old_heap_t *
create_sc_area( int gen_no, gc_t *gc, sc_info_t *info, oh_type_t oh_type );

old_heap_t *
clone_sc_area( old_heap_t *heap, int gen_no );
  /* Returns a fresh area with generation number gen_no, 
   * using heap as a basis for assigning all other 
   * internal properties (such as maximum size).
   */

semispace_t *
ohsc_data_area( old_heap_t *heap );
  /* Returns the current semispace structure for an old area.
     (It is not necessarily legal to expand the returned space.)
     */

/* In np-sc-heap.c */

old_heap_t *
create_np_dynamic_area( int gen_no, int *gen_allocd, gc_t *gc,
		       np_info_t *info );

void
np_gc_parameters( old_heap_t *heap, int *k, int *j );

/* In static-heap.c */

static_heap_t *
create_static_area( int gen_no, gc_t *gc );


#endif /* INCLUDED_MEMMGR_H */

/* eof */
