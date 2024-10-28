/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-sets.h  :  Construction of NDDs representing            **/
/**                   elementary sets.                             **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    02/22/99  :  Empty NDD. (BB)                                **/
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

#ifndef LASH_NDD_SETS_H
#define LASH_NDD_SETS_H

#include "ndd.h"

/**  Shortcuts.                                                    **/

#define  ndd_create_empty_msdf(r, n)  ndd_create_empty((r), (n), 1)
#define  ndd_create_empty_lsdf(r, n)  ndd_create_empty((r), (n), 0)

#ifdef NDD_MSDF
#define ndd_create_eq2  ndd_create_eq2_msdf
#define ndd_create_zn   ndd_create_zn_msdf
#endif

#ifdef NDD_LSDF
#define ndd_create_eq2  ndd_create_eq2_lsdf
#define ndd_create_zn   ndd_create_zn_lsdf
#endif

/**  Function prototypes.                                          **/

ndd *ndd_create_empty(uint1, uint4, int);
ndd *ndd_create_eq2_lsdf(uint1);
ndd *ndd_create_eq2_msdf(uint1);
ndd *ndd_create_zn_lsdf(uint1, uint4);
ndd *ndd_create_zn_msdf(uint1, uint4);

#endif  /* LASH_NDD_SETS_H */

/****  End of ndd-sets.h  ****/
