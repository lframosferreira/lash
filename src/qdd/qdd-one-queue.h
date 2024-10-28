/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-one-queue.h  :  Implementation of one queue QDDs and    **/
/**                 operations over one queue QDDs.                **/
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

#ifndef LASH_QDD_ONE_QUEUE_H
#define LASH_QDD_ONE_QUEUE_H

#include "lash-qdd.h"

/**  Function prototypes.                                          **/

int qdd_one_queue(qdd *, uint1);
int qdd_one_sequence(qdd *, queue_op_sequence *);
int qdd_one_sequence_send(qdd *, queue_op_sequence *);
int qdd_one_sequence_receive(qdd *, queue_op_sequence *);
int qdd_one_sequence_queue(qdd *, queue_op_sequence *, queue);
int qdd_one_send(qdd *, queue_symbol *);
int qdd_one_send_union(qdd *, queue_symbol *);
int qdd_one_receive(qdd *, queue_symbol *);
int qdd_one_receive_union(qdd *, queue_symbol *);

#endif  /* LASH_QDD_ONE_QUEUE_H */

/****  End of qdd-one-queue.h  ****/
