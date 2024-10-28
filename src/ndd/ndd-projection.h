/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-projection.h  :  Projection and projection-related      **/
/**                 operations over sets represented as NDDs.      **/
/**                                                                **/
/**    10/06/98  :  Creation. (BB)                                 **/
/**    10/14/98  :  Continued. (BB)                                **/
/**    02/16/99  :  Projection over multiple components. (BB)      **/
/**    02/18/99  :  Hash table statistics. (BB)                    **/
/**    07/09/02  :  Reorganization. (BB)                           **/
/**    09/11/02  :  New function ndd_multi_projection_arb. (LL)    **/
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

#ifndef LASH_NDD_PROJECTION_H
#define LASH_NDD_PROJECTION_H

#include "ndd.h"

/** NDD_DEFAULT_PROJ_HSIZE  :  Default size of the hash tables used
                     by the projection algorithms.                 **/
 
#define NDD_DEFAULT_PROJ_HSIZE    0x0100

/**  Function prototypes.                                          **/

void   ndd_set_proj_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8  ndd_get_proj_ncolls(void);
void   ndd_reset_proj_ncolls(void);
uint8  ndd_get_proj_nins(void);
void   ndd_reset_proj_nins(void);
#endif  /* >= 1 */

ndd   *ndd_projection(ndd *, uint4);
ndd   *ndd_multi_projection(ndd *, uint4, uint4);
ndd   *ndd_multi_projection_arb(ndd *, uint4_set *);

#endif  /* LASH_NDD_PROJECTION_H */

/****  End of ndd-projection.h  ****/
