/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-operations.h  :  Elementary operations over sets        **/
/**                 represented as RVAs.                           **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    08/20/01  :  Minor reorganization. (SJ)                     **/
/**    09/26/01  :  Serialization of RVAs. (SJ)                    **/
/**    10/12/01  :  Added 'rva_interleave'. (SJ)                   **/
/**    11/07/01  :  Reorganization with 'rva-tools.h'. (SJ)        **/
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

#ifndef LASH_RVA_OPERATIONS_H
#define LASH_RVA_OPERATIONS_H

#include "rva.h"

/**  Function prototypes.                                          **/

rva *rva_product(rva *, rva *);
rva *rva_difference(rva *, rva *);
rva *rva_union(rva *, rva *);
int  rva_merge(rva *, rva *);
rva *rva_intersection(rva *, rva *);
rva *rva_interleave_rn(rva *, uint4, uint4);
rva *rva_interleave(rva *, rva *, uint4, uint4);

#endif  /* LASH_RVA_OPERATIONS_H */

/****  End of rva-operations.h  ****/
