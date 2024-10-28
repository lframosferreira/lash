/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-tools.h  :  Tools for sets represented as RVAs.         **/
/**                                                                **/
/**    11/07/01  :  Creation. (SJ)                                 **/
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

#ifndef LASH_RVA_TOOLS_H
#define LASH_RVA_TOOLS_H

#include "rva.h"

/**  Function prototypes.                                          **/

rva *rva_serialize(rva *);
rva *rva_unserialize(rva *);

#endif  /* LASH_RVA_TOOLS_H */

/****  End of rva-tools.h  ****/
