/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-sets.h  :  Construction of RVAs representing            **/
/**                   elementary sets.                             **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    03/26/01  :  Continued. (SJ)                                **/
/**    08/13/01  :  New function 'rva_create_eq2'. (SJ)            **/
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

#ifndef LASH_RVA_SETS_H
#define LASH_RVA_SETS_H

#include "rva.h"
#include "datastruct.h"

/**  Function prototypes.                                          **/

rva *rva_create_empty(uint1, uint4);
rva *rva_create_rn(uint1, uint4);
rva *rva_create_zn(uint1, uint4);
rva *rva_create_rzn(uint1, uint4, int *);
rva *rva_create_eq2(uint1);

#endif  /* LASH_RVA_SETS_H */

/****  End of rva-sets.h  ****/
