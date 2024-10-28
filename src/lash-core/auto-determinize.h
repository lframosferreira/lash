/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-determinize.h  :  Determinization of finite-state      **/
/**                     automata.                                  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        03/01/01  :  Determinization of co-Buchi automata. (SJ) **/
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

#ifndef LASH_AUTO_DETERMINIZE_H
#define LASH_AUTO_DETERMINIZE_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_DETERM_HSIZE  :  Default size of the hash table used
                     by the determinization algorithm.             **/

#define AUTO_DEFAULT_DETERM_HSIZE  0x1000

/**  Function prototypes.                                          **/

void   auto_set_determ_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_determ_ncolls(void);
void   auto_reset_determ_ncolls(void);
uint8  auto_get_determ_nins(void);
void   auto_reset_determ_nins(void);
#endif  /* >= 1 */

int    auto_determinize(automaton *);

#endif  /* LASH_AUTO_DETERMINIZE_H */

/****  End of auto-determinize.h  ****/
