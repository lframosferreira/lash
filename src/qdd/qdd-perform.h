/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-perform.h  :  QDD operation on a specified queue.       **/
/**                                                                **/
/**    11/20/99  :  Creation. (JMF)                                **/
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

#ifndef LASH_QDD_PERFORM_H
#define LASH_QDD_PERFORM_H

#include "lash-qdd.h"
#include "queue-operations.h"

/**  Function prototypes.                                          **/

int  qdd_perform_fnct(qdd *, queue, int (*)(qdd *, void *), void *);
int  qdd_apply(qdd *, queue_op_sequence *);

#endif  /* LASH_QDD_PERFORM_H */

/****  End of qdd-perform.h  ****/
