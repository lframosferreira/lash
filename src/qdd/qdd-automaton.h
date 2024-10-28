/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-automaton.h  :  Functions associated to the underlying  **/
/**                 automaton of QDDs.                             **/
/**                                                                **/
/**    01/19/00  :  Creation (split qdd.h in 2 parts). (JMF)       **/
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

#ifndef LASH_QDD_AUTOMATON_H
#define LASH_QDD_AUTOMATON_H

#include "lash-qdd.h"

/**  Function prototypes.                                          **/

int  qdd_remove_i_states(qdd *);
int  qdd_add_new_i_state(qdd *, uint4);
int  qdd_mark_accepting_state(qdd *, uint4);
int  qdd_unmark_accepting_state(qdd *, uint4);
int  qdd_add_new_transition(qdd *, uint4, uint4, uint4,
			    queue_symbol *);
int  qdd_apply_homomorphism(qdd *, int (*)(uint4, uint1 *, uint4 *,
					   uint1 **, uint1, uint1),
			    uint1);
int  qdd_minimize(qdd *);
int  qdd_normalize(qdd *);
int  qdd_is_normal(qdd *);

/**  Macro  qdd_alphabet_nbytes  :  Size (in bytes) of a QDD symbol
                                                                   **/

#define  qdd_alphabet_nbytes  (sizeof(queue_symbol))

/**  Macro  qdd_nb_init_tuples(q) :  Returns the number of init_tuples
                     of the underlying qdd *q                      **/

#define  qdd_nb_init_tuples(q)  ((q) -> nb_it)

/**  Macro  qdd_init_tuples_element(q, n) :  Returns a pointer to the
                     n th init_tuple of the underlying qdd *q      **/

#define  qdd_init_tuples_element(q, n)  ((q) -> init + (n))

/** The following macros adapt functions and macros related to
    automaton (mainly defined in lash-auto.h) to qdd. See lash-auto.h
    or auto.c for precise comments.                                **/

#define  qdd_nb_out_transitions(q, m, p) \
     auto_nb_out_transitions(qdd_automaton(q), (m), (p)) 

#define  qdd_transition(q, m, n) \
     auto_transition(qdd_automaton(q), (m), (n))

#define  qdd_accepting_state(q, p)  \
     auto_accepting_state(qdd_automaton(q), (p)) 

#define  qdd_transition_dest(t)  auto_transition_dest(t)

#define  qdd_transition_label_ptr(t) \
     ((queue_symbol *) \
      auto_transition_label_ptr((t), qdd_alphabet_nbytes))

#define  qdd_transition_length(t) auto_transition_length(t)

#define  qdd_nb_states(q)  auto_nb_states(qdd_automaton(q)) 

#define  qdd_nb_i_states(q)  auto_nb_i_states(qdd_automaton(q))

#define  qdd_i_state(q, s, sp) \
     auto_i_state(qdd_automaton(q), (s), (sp))

#define  qdd_state(q, n, s) auto_state(qdd_automaton(q), (n), (s))

#define  qdd_add_new_state(q, s) \
     auto_add_new_state(qdd_automaton(q), (s))

#endif  /* LASH_QDD_AUTOMATON_H  */

/****  End of qdd-automaton.h  ****/
