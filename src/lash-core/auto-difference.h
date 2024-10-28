/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-difference.h  :  Difference-based operations over      **/
/**                     finite-state automata.                     **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        03/16/01  :  Difference with weak det. automata. (SJ)   **/
/**        03/17/01  :  Buchi automata emptiness test. (SJ)        **/
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

#ifndef LASH_AUTO_DIFFERENCE_H
#define LASH_AUTO_DIFFERENCE_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_DIFF_HSIZE  :  Default size of the hash table used
                     by the difference algorithm.                  **/

#define AUTO_DEFAULT_DIFF_HSIZE    0x0100

/**  Function prototypes.                                          **/

void        auto_set_diff_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8       auto_get_diff_ncolls(void);
void        auto_reset_diff_ncolls(void);
uint8       auto_get_diff_nins(void);
void        auto_reset_diff_nins(void);
#endif  /* >= 1 */

automaton  *auto_difference(automaton *, automaton *);
int         auto_empty_language(automaton *);
int         auto_inclusion(automaton *, automaton *);
int         auto_equality(automaton *, automaton *);

#endif  /* LASH_AUTO_DIFFERENCE_H */

/****  End of auto-difference.h  ****/
