/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-equations.h  :  Construction of RVAs representing the   **/
/**                 sets of solutions of linear equations and      **/
/**                 inequations.                                   **/
/**                                                                **/
/**    03/02/01  :  Creation. (SJ)                                 **/
/**    03/15/01  :  Statistics added. (SJ)                         **/
/**    03/24/01  :  Usage of the rva type. (SJ)                    **/
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

#ifndef RVA_EQUATIONS_H
#define RVA_EQUATIONS_H

#include "rva.h"

/**  Function prototypes.                                          **/

uint8  rva_get_equation_ncolls(void);
void   rva_reset_equation_ncolls(void);
uint8  rva_get_equation_nins(void);
void   rva_reset_equation_nins(void);
rva   *rva_create_equation(uint1, uint4, sint4 *, sint4);
rva   *rva_create_inequation(uint1, uint4, sint4 *, sint4);

#endif  /* LASH_RVA_EQUATIONS_H */

/****  End of rva-equations.h  ****/
