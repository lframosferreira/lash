/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-operations.h  :  Elementary operations and tests over   **/
/**                 sets represented as QDDs.                      **/
/**                                                                **/
/**    01/17/00  :  Creation. (JMF)                                **/
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

#ifndef LASH_QDD_OPERATIONS_H
#define LASH_QDD_OPERATIONS_H

#include "lash-qdd.h"

/**  Function prototypes.                                          **/

int  qdd_merge(qdd *, qdd *);
qdd *qdd_union(qdd *, qdd *);
int  qdd_equality(qdd *, qdd *);
int  qdd_empty(qdd *);

#endif  /* LASH_QDD_OPERATIONS_H  */

/****  End of qdd-operations.h  ****/
