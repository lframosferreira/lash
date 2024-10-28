/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-quotient.h  :  Quotient and quotient-related finite-   **/
/**                     operations over finite-state automata.     **/
/**                                                                **/
/**        09/18/98  :  Creation. (BB)                             **/
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

#ifndef LASH_AUTO_QUOTIENT_H
#define LASH_AUTO_QUOTIENT_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_QUOTIENT_HSIZE  :  Default size of the hash table
                     used by the quotient algorithm.               **/
 
#define AUTO_DEFAULT_QUOTIENT_HSIZE    0x1000

/**  Function prototypes.                                          **/

void        auto_set_quotient_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8       auto_get_quotient_ncolls(void);
void        auto_reset_quotient_ncolls(void);
uint8       auto_get_quotient_nins(void);
void        auto_reset_quotient_nins(void);
#endif  /* >= 1 */


automaton  *auto_reverse(automaton *);
automaton  *auto_quotient(automaton *, automaton *);
automaton  *auto_right_quotient(automaton *, automaton *);

#endif  /* LASH_AUTO_QUOTIENT_H */

/****  End of auto-quotient.h  ****/
