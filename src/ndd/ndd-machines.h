/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-machines.h  :  State machines operating over unbounded  **/
/**                 integer variables.                             **/
/**                                                                **/
/**    02/22/99  :  Creation. (BB)                                 **/
/**    02/26/99  :  Continued. (BB)                                **/
/**    03/08/99  :  Reorganized. (BB)                              **/
/**    03/10/99  :  Continued. (BB)                                **/
/**    06/03/99  :  Support for atomic control locations. (BB)     **/
/**    02/03/00  :  Modified structure for the transition          **/
/**                 relation. (BB)                                 **/
/**    02/09/00  :  Efficient handling of meta-transitions. (BB)   **/
/**    02/12/01  :  New function (LL+BB).                          **/
/**    07/09/02  :  Reorganization. (BB)                           **/
/**                                                                **/
/**  Copyright (c) 1998-2002, Universite de Liege (ULg). All       **/
/**  rights reserved. This package is provided for evaluation      **/
/**  purposes and educational use only. No guarantee is expressed  **/
/**  or implied by the distribution of this software. The          **/
/**  commercial use and the redistribution of this product or of   **/
/**  any part of it are not permitted without our prior, express,  **/
/**  and written consent.                                          **/
/**                                                                **/
/********************************************************************/

#ifndef LASH_NDD_MACHINES_H
#define LASH_NDD_MACHINES_H

#include "ndd.h"

/**  Representation of a set of states of a machine with finite
                      control and unbounded integer data. The
		      representation consists of an NDD whose each
		      accepted vector corresponds to one state.  The
		      first ctrl_len components of the vector describe
		      the control part of the state; the remaining
		      components correspond to the data part of the
		      state.

		      Each control location is either atomic (in
                      which case it is identified by a negative
                      number) or non-atomic (in which case its number
                      us positive or zero). An atomic location is one
                      that prevents the global machine from following 
                      the transitions belonging to the other
                      processes.                                   **/

typedef struct {
  ndd   *ndd;
  uint4  ctrl_len;
} ndd_states;

/**  Representation of the reachability information between states of 
                      a machine with finite control and unbounded
                      integer data. This information is represented
                      by a linked list of transducers and of 
                      meta-transition information structures. In order
                      to follow once the (meta-)transition relation,
                      one must apply individually each item in the
                      list, and take the union of the resulting
                      sets.                                        **/


typedef struct _ndd_relation_el {
  union {
    ndd                  *ndd;
    linear_star_info     *lsi;
  } inf;
  struct _ndd_relation_el *next;
  uint1                    type;
} ndd_relation_el;

typedef struct {
  ndd_relation_el *list;
  uint4            ctrl_len, nb_vars;
  uint1            base, msdf;
} ndd_relation;

/**  Values of the field 'type' in structure ndd_relation_el.      **/

#define  NDD_RELATION_NDD       0x01
#define  NDD_RELATION_STAR      0x02

/**  Function prototypes.                                          **/

ndd_states   *ndd_states_new_empty(uint1, uint4, uint4, int);
int           ndd_states_add_data(ndd_states *, sint4 *, ndd *);
ndd          *ndd_states_get_data(const ndd_states *, sint4 *, int *);
int           ndd_states_free(ndd_states *);
ndd_relation *ndd_relation_new_empty(uint1, uint4, uint4, int);
int           ndd_relation_add_transition(ndd_relation *,  uint4,
                  sint4, sint4, linear_transf *);
int           ndd_relation_add_metatransition(ndd_relation *, 
                  sint4 *, int *, linear_transf *);
int           ndd_relation_free(ndd_relation *);
sint8         ndd_relation_size(ndd_relation *);
ndd_states   *ndd_relation_succ(ndd_relation *, ndd_states *);
ndd_states   *ndd_relation_star_succ(ndd_relation *, ndd_states *,
                  int (*)(const ndd_states *));
int           ndd_relation_add_sync_transition(ndd_relation  *,
                  sint4 *, sint4 *, int *, linear_transf *);

#endif  /* LASH_NDD_MACHINES_H */

/****  End of ndd-machines.h  ****/
