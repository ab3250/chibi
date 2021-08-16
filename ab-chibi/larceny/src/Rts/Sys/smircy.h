/* Copyright 2008 Felix S Klock II        -*- indent-tabs-mode: nil -*-
 *
 * $Id: smircy.h 5790 2008-09-08 17:25:21Z pnkfelix $
 *
 * Interface for incremental marking machine.
 */

#ifndef INCLUDED_SMIRCY_H
#define INCLUDED_SMIRCY_H

#include "larceny-types.h"

smircy_context_t *smircy_begin( gc_t *gc, int num_rgns );

smircy_context_t *smircy_begin_opt( gc_t *gc, int num_rgns, 
                                    bool cover_full_address_range );

void smircy_push_roots( smircy_context_t *context );

void smircy_push_remset( smircy_context_t *context, remset_t *rs );
void smircy_push_locset( smircy_context_t *context, locset_t *rs );

void smircy_push_elems( smircy_context_t *context, word *bot, word *top );

void smircy_progress( smircy_context_t *context, 
                      int mark_max, int trace_max, int mark_words_max,
                      int misc_max,
                      int *marked, int *traced, int *words_marked,
                      int *misc );

bool smircy_stack_empty_p( smircy_context_t *context );

bool smircy_in_construction_stage_p( smircy_context_t *context );
void smircy_enter_refinement_stage( smircy_context_t *context );
bool smircy_in_refinement_stage_p( smircy_context_t *context );
void smircy_exit_refinement_stage( smircy_context_t *context );
bool smircy_in_completed_stage_p( smircy_context_t *context );

bool smircy_object_marked_p( smircy_context_t *context, word obj );

void smircy_when_object_forwarded( smircy_context_t *context, 
                                   word obj_orig, int gen_orig,
                                   word obj_new, int gen_new );

void smircy_enumerate_stack_of_rgn( smircy_context_t *context, 
                                    int rgn, 
                                    void (*visit)(word *w, void *data),
                                    void *orig_data );

void smircy_jit_process_stack_for_rgn( smircy_context_t *context, int rgn );
  /* Iteratively marks all objects on stack for rgn and pushes their
   * unmarked constituents, until the stack for rgn is empty.
   * 
   * (The crucial detail is that the processing is isolated to just
   *  the stack for rgn; the stacks for other regions may grow but
   *  will not be otherwised modified or traversed.) */

void smircy_end( smircy_context_t *context );

void smircy_set_object_visitor( smircy_context_t *context, 
                                void* (*visitor)( word obj, 
                                                  word src, 
                                                  void *data ),
                                void *visit_data );

void* smircy_get_object_visitor_data( smircy_context_t *context );

void smircy_expand_gnos( smircy_context_t *context, int gno );
void smircy_swap_gnos( smircy_context_t *context, int gno1, int gno2 );
void smircy_drop_cleared_stack_entries( smircy_context_t *context, int gno );

int smircy_objs_marked( smircy_context_t *context );
int smircy_arcs_traced( smircy_context_t *context );
int smircy_words_marked( smircy_context_t *context );

#endif /* INCLUDED_SMIRCY_H */

/* eof */
