/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-projection.h  :  Projection and projection-related      **/
/**                 operations over sets represented as RVAs.      **/
/**                                                                **/
/**    03/26/01  :  Creation. (SJ)                                 **/
/**    10/30/01  :  'rva_canonicalize' added. (SJ)                 **/
/**    04/17/02  :  'rva_multiple_projection' added. (SJ)          **/
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

#ifndef LASH_RVA_PROJECTION_H
#define LASH_RVA_PROJECTION_H

#include "rva.h"

/** RVA_DEFAULT_PROJ_HSIZE  :  Default size of the hash tables used
                     by the projection algorithms.                 **/

#define RVA_DEFAULT_PROJ_HSIZE    0x0100

/**  Shortcuts for backward compatibility.                         **/

#define rva_multi_projection(r, p, u) \
        rva_modulo_projection((r), (p), (u))

/**  Function prototypes.                                          **/

void   rva_set_proj_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_proj_ncolls(void);
void   rva_reset_proj_ncolls(void);
uint8  rva_get_proj_nins(void);
void   rva_reset_proj_nins(void);
#endif  /* >= 1 */

rva   *rva_projection(rva *, uint4);
rva   *rva_modulo_projection(rva *, uint4, uint4);
rva   *rva_multiple_projection(rva *, int *);
int    rva_canonicalize(rva *);

#endif  /* LASH_RVA_PROJECTION_H */

/****  End of rva-projection.h  ****/
