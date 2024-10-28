/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    arithmetic.h  :  Arithmetic operations.                     **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/14/99  :  Polynomials. (BB)                              **/
/**    01/15/99  :  Integer power. (BB)                            **/
/**    01/18/99  :  Small adaptations. (BB)                        **/
/**    09/20/00  :  Function 'sint4__multi_gcd' added. (SJ)        **/
/**    10/03/00  :  'sint4__floor' and 'sint4__ceil' added. (SJ)   **/
/**    08/13/01  :  Small adaptation. (BB)                         **/
/**    07/08/02  :  Reorganization. (BB)                           **/
/**    05/01/05  :  Add sint4__rem. (LL)                           **/
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

#ifndef LASH_ARITHMETIC_H
#define LASH_ARITHMETIC_H

#include "lash-types.h"
#include "resource.h"

/**  Polynomial type definition.                                   **/

typedef struct {
  uint4  degree;
  sint4 *coeff;
} poly;

/**  Function prototypes.                                          **/

int    sint4__add(sint4 *, sint4, sint4);
int    sint4__mult(sint4 *, sint4, sint4);
int    uint4__add(uint4 *, uint4, uint4);
int    uint4__mult(uint4 *, uint4, uint4);
int    suint4__mult(sint4 *, sint4, uint4);
int    sint4__from_uint4(sint4 *, uint4);
uint4  uint4__gcd(uint4, uint4);
uint4  sint4__multi_gcd(sint4 *, uint4);
int    uint4__lcm(uint4 *, uint4, uint4);
int    sint4__power(sint4 *, sint4, uint4);
int    sint4__floor(sint4, sint4, sint4 *);
int    sint4__ceil(sint4, sint4, sint4 *);
int    sint4__rem(sint4, sint4, sint4 *);
poly  *poly__new(uint4, sint4 *);
void   poly__free(poly *);
void   poly__reduce(poly *);
poly  *poly__sub(poly *, poly *);
poly  *poly__uint4_mult(poly *, uint4);
poly  *poly__uint4_div(poly *, uint4);
poly  *poly__mult(poly *, poly *);
poly  *poly__div(poly *, poly *, uint4 *);
poly  *poly__mod(poly *, poly *, uint4 *);
poly  *poly__gcd(poly *, poly *);

/**  Macro  poly__is_zero(p)  :  Returns 1 if the polynomial *p is
                     constant and equal to zero, and 0 otherwise.  **/

#define  poly__is_zero(p) (!((p) -> degree) && !((p) -> coeff[0]))

/**  Macro  sint4__abs(v)  :  Returns the absolute value of v.     **/

#define  sint4__abs(v)  (((v) < ZERO_INT4) ? (-(v)) : (v))

/**  Macro  sint4__over_uint4(n, d)  :  Divides the signed number n
                     by the unsigned number d and returns the integer
                     quotient.                                     **/

#define  sint4__over_uint4(n, d)  ((n) / ((sint4) (d)))

#endif  /* LASH_ARITHMETIC_H */

/****  End of arithmetic.h  ****/
