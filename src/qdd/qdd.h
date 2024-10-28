/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**     qdd.h  :  Visible data structures and prototypes           **/
/**                  for manipulating QDD.                         **/
/**                                                                **/
/**     04/22/98  :  Creation. (GC)                                **/
/**     10/15/99  :  Upgraded to lash-v0.9. (JMF)                  **/
/**     12/16/99  :  Enhanced queue/queue alphabet support. (JMF)  **/
/**     01/19/00  :  Reorganisation. (JMF)                         **/
/**     07/08/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_QDD_H
#define LASH_QDD_H

#include "lash-auto-operations.h"
#include "lash-qdd.h"

/**  Function prototypes.                                          **/

int  qdd_compute_init_tuples(qdd *);
int  qdd_free_init_tuples(qdd *);
qdd *qdd_auto2qdd(automaton *, uint1, uint1 *);
int  qdd_same_alphabet(qdd *, qdd *);

/**  Definitions for qdd.properties (or-able).                     **/

#define QDD_PROP_NOTHING           0x00  /*  No known property.     */
#define QDD_PROP_VALID_INIT_TUPLES 0x01  /*  The sequence of
					     init_tuples is valid.  */
#define QDD_PROP_ONE_QUEUE         0x02  /*  The QDD is operates on a
					     single queue. Note that
					     it doesn't mean that the
					     QDD is a one-queue QDD
					     (i.e. that its nb_queues
					     fields equals 1), but
					     that you can't find two
					     symbols on the labels of
					     the transitions of
					     the QDD such that the
					     'queue number' part of
					     those symbols are
					     different.             */

/**  Macros                                                        **/

/**  Macro  qdd_known_properties(q)  :  Returns the known properties
                     of the qdd *q.                                **/

#define  qdd_known_properties(q) ((q) -> properties)

/**  Macro  qdd_reset_property(q, p)  :  Marks the property p as
                     being invalid for the qdd *q.                 **/

#define  qdd_reset_property(q, p) ((q) -> properties &= ~(p))

/**  Macro  qdd_set_property(q, p)  :  Marks the property p as
                     being valid for the qdd *q.                   **/

#define  qdd_set_property(q, p) ((q) -> properties |= (p))

/**  Macro  qdd_test_property(q, p)  :  Returns 1 if the property p
                     is known to be valid for the qdd *q, and 0
                     otherwise.                                    **/

#define  qdd_test_property(q, p) (!!((q) -> properties & (p)))

/**  Macro  qdd_automaton(q) :  Returns a pointer to the underlying
                     automaton of the qdd *q                       **/

#define  qdd_automaton(q) ((q) -> automaton)

#include "qdd-one-queue.h"
#include "qdd-automaton.h"

#endif  /* LASH_QDD_H  */

/****  End of qdd.h  ****/
