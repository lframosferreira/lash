/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  auto-count.h  :  Counting words accepted by automata.         **/
/**                                                                **/
/**      20/07/00  :  Creation. (LL)                               **/
/**      18/10/00  :  Some corrections. (LL)                       **/
/**      02/12/01  :  Reorganization. (BB)                         **/
/**      07/08/02  :  Reorganization. (BB)                         **/
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

#ifndef LASH_AUTO_COUNT_H
#define LASH_AUTO_COUNT_H

#include "lash-types.h"
#include "biguint.h"

typedef struct _s_count_info {
  uint4 s;
  uint4 prev;
  uint4 nb_trans_ex;
  uint1 init;
} s_count_info;

typedef struct _h_count_info {
  uint1 loop;
  uint1 in_stack;
  biguint *node_count;
} h_count_info;

typedef struct _s_count_prep_info {
  uint4 s, prev_s;
  uint4 nb_trans_ex;
  uint1 init;
} s_count_prep_info;

typedef struct _h_count_prep_info {
  uint1 in_stack;
  uint1 leads_to_cycle;
} h_count_prep_info;

/**  Function prototype.                                           **/

int auto_count_accept(biguint *, automaton *);

#endif /* LASH_AUTO_COUNT_H */

/****  End of auto-count.h  ****/
