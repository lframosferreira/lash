/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**  qdd-symbol.h  :  Definition of the alphabet of QDDs.          **/
/**                                                                **/
/**    12/08/99  :  Creation. (JMF)                                **/
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

#ifndef LASH_QDD_SYMBOL_H
#define LASH_QDD_SYMBOL_H

#include "lash-types.h"

/** Definition of symbols and queues                               **/

typedef uint1 queue;
typedef uint1 type;
typedef uint1 symbol;

#define QUEUE_UNK_QUEUE            0xFF  /*  Unknown queue          */

typedef struct {
  queue   queue;
  symbol  symbol;
} queue_symbol;

/**  Function prototypes.                                          **/

int qdd_qm_cmp(queue_symbol *, queue_symbol *);
int qdd_symbol_cmp(symbol *, symbol *);

/**  Macros.                                                       **/

#define queuesymbol_queue(qm) ((qm) -> queue)
#define queuesymbol_symbol(qm) ((qm) -> symbol)

#endif  /* LASH_QDD_SYMBOL_H */

/****  End of qdd-symbol.h  ****/
