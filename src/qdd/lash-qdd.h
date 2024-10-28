/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**  lash-qdd.h  :  Visible data structures and prototypes for     **/
/**                 manipulating Queue Decision Diagrams.          **/
/**                                                                **/
/**    11/03/99  :  Creation. (JMF)                                **/
/**    12/08/99  :  Reorganisation. (JMF)                          **/
/**    12/16/99  :  Enhanced queue/queue alphabet support. (JMF)   **/
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

#ifndef LASH_LASH_QDD_H
#define LASH_LASH_QDD_H

#include "lash-auto.h"
#include "qdd-symbol.h"

/**  typedef init_tuple : Type of a list of tuples (q, c).
                     This list describes the sequence of init states
		     in a tuple of automata, each of these automata is
		     given by the part of the underlying QDD from
		     state q to the state of the next tuple (or to a
		     final state if (q, c) is the last tuple of the
		     list) through edges involving only queue c.
		     
		     If a list of tuple is composed of a single
		     element (q, QUEUE_UNK_QUEUE) then the
		     underlying QDD accepts the empty word (i.e. the
		     epsilon word).

		     An adequate sequence of these lists provide
		     enough information to view the underlying QDD as
		     a finite union of tuple of regular contents (one
		     for each queue).                              **/

typedef struct {
  uint4  state;
  queue  queue;
} state_queue;

typedef struct {
  state_queue *list;
  uint1        nb_sc;
} init_tuple;

/** Definition of a QDD.                                           **/

typedef struct {
  automaton     *automaton;
  uint1          properties;
  uint1          nb_queues;
  uint1         *queue_alphabet_size;
  uint4          nb_it;
  init_tuple    *init;
} qdd;

#include "queue-operations.h"
#include "qdd-operations.h"
#include "qdd-perform.h"
#include "qdd-one-queue.h"
#include "qdd-one-iterations.h"
#include "qdd-iterations.h"
#include "qdd-machines.h"

/**  Function prototypes.                                          **/

qdd *qdd_new_empty(uint1, uint1 *);
qdd *qdd_new_empty_queue(uint1, uint1 *);
int  qdd_normalize(qdd *);
int  qdd_free(qdd *);
qdd *qdd_copy(qdd *);

/**  Macro  qdd_nb_queues(q)  :  Returns the number of queues of the
                     automaton q.                                  **/

#define  qdd_nb_queues(q) ((q) -> nb_queues)

/**  Macro  qdd_queue_alphabet_size(q, qu)  :  Returns the number of
                     symbols of the alphabet of qdd q's qu-th queue.
                                                                   **/

#define  qdd_queue_alphabet_size(q, qu) \
             (((q) -> queue_alphabet_size)[qu])

#endif  /* LASH_LASH_QDD_H */

/****  End of lash-qdd.h  ****/
