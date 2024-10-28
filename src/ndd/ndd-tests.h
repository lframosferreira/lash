/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-tests.h  :  Elementary predicates over sets             **/
/**                 represented as NDDs.                           **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    07/09/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_NDD_TESTS_H
#define LASH_NDD_TESTS_H

#include "ndd.h"

/**  Function prototypes.                                          **/

int  ndd_is_empty(ndd *);
int  ndd_inclusion(ndd *, ndd *);
int  ndd_equality(ndd *, ndd *);
int  ndd_disjoint(ndd *, ndd *);

#endif  /* LASH_NDD_TESTS_H */

/****  End of ndd-tests.h  ****/
