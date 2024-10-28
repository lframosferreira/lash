/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-equations.h  :  Construction of NDDs representing the   **/
/**                 sets of solutions of linear equations and      **/
/**                 inequations.                                   **/
/**                                                                **/
/**    09/29/98  :  Creation. (BB)                                 **/
/**    10/02/98  :  Continued. (BB)                                **/
/**    10/05/98  :  Inequations. (BB)                              **/
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

#ifndef LASH_NDD_EQUATIONS_H
#define LASH_NDD_EQUATIONS_H

#include "ndd.h"

/**  Shortcuts.                                                    **/

#ifdef NDD_MSDF
#define ndd_create_equation    ndd_create_equation_msdf
#define ndd_create_inequation  ndd_create_inequation_msdf
#endif

#ifdef NDD_LSDF
#define ndd_create_equation    ndd_create_equation_lsdf
#define ndd_create_inequation  ndd_create_inequation_lsdf
#endif

/**  Function prototypes.                                          **/

uint8  ndd_get_equation_ncolls(void);
void   ndd_reset_equation_ncolls(void);
uint8  ndd_get_equation_nins(void);
void   ndd_reset_equation_nins(void);
ndd   *ndd_create_equation_msdf(uint1, uint4, sint4 *, sint4);
ndd   *ndd_create_equation_lsdf(uint1, uint4, sint4 *, sint4);
ndd   *ndd_create_inequation_msdf(uint1, uint4, sint4 *, sint4);
ndd   *ndd_create_inequation_lsdf(uint1, uint4, sint4 *, sint4);

#endif  /* LASH_NDD_EQUATIONS_H */

/****  End of ndd-equations.h  ****/
