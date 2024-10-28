/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-operations.h  :  Elementary operations over sets        **/
/**                 represented as NDDs.                           **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    10/14/98  :  New function. (BB)                             **/
/**    02/02/99  :  Interleave function. (BB)                      **/
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

#ifndef LASH_NDD_OPERATIONS_H
#define LASH_NDD_OPERATIONS_H

#include "ndd.h"

/**  Function prototypes.                                          **/

ndd *ndd_product(ndd *, ndd *);
ndd *ndd_difference(ndd *, ndd *);
ndd *ndd_union(ndd *, ndd *);
int  ndd_merge(ndd *, ndd *);
ndd *ndd_intersection(ndd *, ndd *);
ndd *ndd_base_reduce(ndd *);
ndd *ndd_base_expand(ndd *);
ndd *ndd_interleave_z(ndd *, uint4, uint4);

#endif  /* LASH_NDD_OPERATIONS_H */

/****  End of ndd-operations.h  ****/
