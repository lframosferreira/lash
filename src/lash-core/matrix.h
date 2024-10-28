/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    matrix.h  :  Operations over matrices.                      **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/12/99  :  Computation of characteristic polynomial. (BB) **/
/**    01/15/99  :  Power of matrices. (BB)                        **/
/**    01/18/99  :  Zero test. (BB)                                **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_MATRIX_H
#define LASH_MATRIX_H

#include "arithmetic.h"

/**  Function prototypes.                                          **/

int   matrix__add(sint4 *, sint4 *, sint4 *, uint4, uint4);
int   matrix__mult(sint4 *, sint4 *, sint4 *, uint4, uint4, uint4);
int   matrix__power(sint4 *, sint4 *, uint4, uint4);
int   matrix__const_mult(sint4 *, uint4, uint4, sint4);
int   matrix__is_zero(sint4 *, uint4, uint4);
poly *matrix__characteristic_poly(sint4 *, uint4);

#endif  /* LASH_MATRIX_H */

/****  End of matrix.h  ****/
