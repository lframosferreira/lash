/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-tests.h  :  Elementary predicates over sets             **/
/**                 represented as RVAs.                           **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    07/10/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_RVA_TESTS_H
#define LASH_RVA_TESTS_H

#include "rva.h"

/**  Function prototypes.                                          **/

int  rva_is_empty(rva *);
int  rva_inclusion(rva *, rva *);
int  rva_equality(rva *, rva *);
int  rva_disjoint(rva *, rva *);

#endif  /* LASH_RVA_TESTS_H */

/****  End of rva-tests.h  ****/
