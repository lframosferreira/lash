/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-iterations-utils.h  :  Utility functions used to        **/
/**                 compute the closure of a sequence of           **/
/**                 operations.                                    **/
/**                                                                **/
/**    02/15/00  :  Creation. (JMF)                                **/
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

#ifndef LASH_QDD_ITERATIONS_UTILS_H
#define LASH_QDD_ITERATIONS_UTILS_H

#include "lash-types.h"
#include "datastruct.h"
#include "queue-operations.h"
#include "qdd.h"

/****  Type definition.                                          ****/

#define PTABLE_GROWTH_QUANTUM  (0x10)

typedef struct {
  void  **ptr;
  uint4   nb_p;
  uint4   nb_entries;
} p_table;

/**  Function prototypes.                                          **/

p_table    *p_table__new_empty(void);
int         p_table__free(p_table *, int (*)(void *));
int         p_table__add(p_table *, void *);
uint4       p_table__nb_elements(p_table *);
void       *p_table__element(p_table *, uint4);
int         p_table__find(p_table *, void *, int (*)(void *, void *),
			  uint4 *);
int         repeated_union_seq(qdd *, queue_op_sequence *, uint4,
			       int);

#endif  /* LASH_QDD_ITERATIONS_UTILS_H */

/****  End of qdd-iterations-utils.h  ****/
