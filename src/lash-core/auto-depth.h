/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  auto-depth.h  :  Computing depth of  automata.                **/
/**                                                                **/
/**      06/19/01  :  Creation. (LL)                               **/
/**      01/25/02  :  Modification auto_depth. (LL)                **/
/**      01/26/02  :  Add functions (topological depth). (LL)      **/
/**      07/11/02  :  Reorganization. (BB)                         **/
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

#ifndef LASH_AUTO_DEPTH_H
#define LASH_AUTO_DEPTH_H

#include "lash-types.h"
#include "datastruct.h"
#include "lash-auto.h"
#include "auto.h"
#include "diag.h"
#include "lash-diag.h"
#include "resource.h"
#include "lash.h"
#include "auto-sort.h"
#include "auto-determinize.h"
#include "auto-minimize.h"

typedef struct _q_depth_info {
  uint4 state;
  uint4 depth;
} q_depth_info;

/**  Function prototypes.                                          **/

int auto_depth(automaton *, uint4 *);
int auto_max_path_length(automaton *, uint4 *, int *);
int auto_states_max_path_length(automaton *, uint4 **, int *);
int auto_max_path_rev_lex(automaton *, tran **, uint4 *, int *, 
			  int *);
int auto_min_path_lex(automaton *, tran **, uint4 *, int *);

#endif  /* LASH_AUTO_DEPTH_H */

/****  End of auto-depth.h  ****/
