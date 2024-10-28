/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-normalize.h  :  Normalization of finite-state          **/
/**                     automata.                                  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        03/17/01  :  Weak det. automata normal form. (SJ)       **/
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

#ifndef LASH_AUTO_NORMALIZE_H
#define LASH_AUTO_NORMALIZE_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_WEAK_NORM_HSIZE  :  Default size of the hash table
                     used by the normalization algorithm for weak
		     automata.                                     **/

#define AUTO_DEFAULT_WEAK_NORM_HSIZE    0x0100

/**  Function prototypes.                                          **/

void        auto_set_weak_norm_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8       auto_get_weak_norm_ncolls(void);
void        auto_reset_weak_norm_ncolls(void);
uint8       auto_get_weak_norm_nins(void);
void        auto_reset_weak_norm_nins(void);
#endif  /* >= 1 */

int  auto_is_normal(automaton *);
int  auto_normalize(automaton *);
int  auto_weak_normalize(automaton *);

#endif  /* LASH_AUTO_NORMALIZE_H */

/****  End of auto-normalize.h  ****/
