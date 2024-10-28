/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-machines.h  :  State machines operating over unbounded  **/
/**                 FIFO queues.                                   **/
/**                                                                **/
/**    10/29/99  :  Creation. (JMF)                                **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_QDD_MACHINES_H
#define LASH_QDD_MACHINES_H

#include "lash-qdd.h"
#include "datastruct.h"

/**  Representation of a set of states of a machine with finite
                      control and unbounded FIFO queue. The
		      representation consists of an automaton whose
		      each accepted word corresponds to one state.
		      The first ctrl_len symbols of the word describe
		      the control part of the state; the remaining
		      components correspond to the data part of the
		      state.

		      Each control symbol of the control word is an
		      integer.  The i-th control symbol is smaller
		      than the i-th component of nb_control.       **/

typedef struct {
  automaton  *states;
  uint4       ctrl_len, *nb_control;
  uint1       nb_queues, *queue_alphabet_size;
  bit_table **atomic;
} qdd_states;

/**  Representation of the reachability information between states of 
                      a machine with finite control and unbounded
                      queues content. This information is represented
                      by a linked list of transitions (origin,
		      destination, sequence of operations to be
                      applied and 'meta' flag). In order
                      to follow once the (meta-)transition relation,
                      one must apply individually each item in the
                      list, and take the union of the resulting
                      sets.                                        **/

typedef struct _qdd_relation_el {
  queue_op_sequence       *seq;
  uint4                    np, no, nd;
  uint4                    dest_nd;
  uint1                    type;
  struct _qdd_relation_el *next;
} qdd_relation_el;

typedef struct {
  qdd_relation_el *list;
  uint4            ctrl_len, *nb_control;
} qdd_relation;

/**  Values of the field 'type' in structure qdd_relation_el.      **/

#define  QDD_RELATION_SEQ       0x01
#define  QDD_RELATION_STAR      0x02

/**  Function prototypes.                                          **/

qdd_states   *qdd_states_new_empty(uint4, uint4 *, uint1, uint1 *,
				   uint4 *);
int           qdd_states_free(qdd_states *);
qdd_states   *qdd_states_copy(qdd_states *);
int           qdd_states_add_atomic(qdd_states *, uint4, uint4);
int           qdd_states_isatomic(qdd_states *, uint4, uint4);
int           qdd_states_add_data(qdd_states *, uint4 *, qdd *);
qdd          *qdd_states_get_data(qdd_states *, uint4 *, int *);
qdd_relation *qdd_relation_new_empty(uint4, uint4 *);
int           qdd_relation_free(qdd_relation *);
int           qdd_relation_add_transition(qdd_relation *, uint4,
					  uint4, uint4,
					  queue_op_sequence *);
int           qdd_relation_add_metatransition(qdd_relation *, uint4,
					      uint4, uint4,
					      queue_op_sequence *);
qdd_states   *qdd_relation_succ(qdd_relation *, qdd_states *);
qdd_states   *qdd_relation_star_succ(qdd_relation *, qdd_states *,
				     int (*)(qdd_states *));

/**  Macros                                                        **/

/**  states_queue_alphabet_size(s, i)  :  Returns the number of
                     symbols in the i-th queue of the QDDs
		     held in the states of the states set *s.      **/

#define  qdd_states_queue_alphabet_size(s, i) \
     (s -> queue_alphabet_size[i])

/**  states_nb_queues(s)  :  Returns the number of queues of the QDDs
                     held in the states of the states set *s.      **/

#define  qdd_states_nb_queues(s) (s -> nb_queues)

/**  states_states(s)  :  Returns the automaton holding the states of
                     the set of states s.                          **/

#define  qdd_states_states(s) (s -> states)

/**  states_ctrl_len(s)  :  Returns the len of the control part (in
                     number of integers) of the states *s.         **/
                     
#define  qdd_states_ctrl_len(s) (s -> ctrl_len)

#endif  /* LASH_QDD_MACHINES_H */

/****  End of qdd-machines.h  ****/
