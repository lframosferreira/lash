/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    lash-auto-operations.h  :  Basic operations over finite-    **/
/**                     state automata.                            **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        09/18/98  :  Quotient module. (BB)                      **/
/**        08/16/01  :  Counting module. (BB)                      **/
/**        07/08/02  :  Reorganization. (BB)                       **/
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

#ifndef LASH_LASH_AUTO_OPERATIONS_H
#define LASH_LASH_AUTO_OPERATIONS_H

#include "lash-auto.h"
#include "auto-determinize.h"
#include "auto-minimize.h"
#include "auto-product.h"
#include "auto-sequential.h"
#include "auto-difference.h"
#include "auto-normalize.h"
#include "auto-quotient.h"
#include "auto-count.h"

/**  Function prototypes.                                          **/

int         auto_close(automaton *);
int         auto_concatenate(automaton *, automaton *);
int         auto_merge(automaton *, automaton *);
automaton  *auto_union(automaton *, automaton *);
int         auto_apply_homomorphism(automaton *, int (*)(uint4, 
                uint1 *, uint4 *, uint1 **, uint1, uint1), uint1);

#endif  /* LASH_LASH_AUTO_OPERATIONS_H */

/****  End of lash-auto-operations.h  ****/
