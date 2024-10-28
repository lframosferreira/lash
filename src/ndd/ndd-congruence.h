/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-congruence.h  :  Construction of NDDs representing      **/
/**                   congruences.                                 **/
/**                                                                **/
/**    02/22/05  :  Creation. (LL)                                 **/
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

#ifndef LASH_NDD_CONGRUENCE_H
#define LASH_NDD_CONGRUENCE_H

#include "ndd.h"

ndd   *ndd_create_congruence_msdf(uint1, uint4, sint4 *, sint4, uint4);
ndd   *ndd_create_congruence_lsdf(uint1, uint4, sint4 *, sint4, uint4);

#endif /* LASH_NDD_CONGRUENCE_H */
