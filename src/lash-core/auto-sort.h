/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       auto-sort.h  :  Automata sorting algorithms.             **/
/**                                                                **/
/**        01/23/02  :  Creation. (LL)                             **/
/**        07/15/02  :  Reorganization. (BB)                       **/
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

#ifndef LASH_AUTO_SORT_H
#define LASH_AUTO_SORT_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lash-auto.h"
#include "datastruct.h"

/**  Prototype of public function.                                 **/

int auto_topological_sort(automaton *, uint4 **, int *);

#endif  /* LASH_AUTO_SORT_H */

/****  End of auto-sort.h  ****/
