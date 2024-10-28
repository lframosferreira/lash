/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       biguint.h  :  Functions manipulating large unsigned      **/
/**              	integer.                                   **/
/**                                                                **/
/**        07/07/00  :  Creation. (LL)                             **/
/**        07/08/02  :  Reorganization. (BB)                       **/
/**        05/01/05  :  Add biguint__new. (LL)                     **/
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

#ifndef LASH_BIGUINT_H
#define LASH_BIGUINT_H

#include "lash-types.h"

typedef struct {
  uint4 alloc_uint4, nb_uint4;
  uint4 *array;
  uint1 is_inf;
} biguint;

/* prototypes */

biguint *biguint__new_zero(void);
biguint *biguint__new(uint4);
void biguint__free(biguint *);
int biguint__copy(biguint *, biguint *);
int biguint__add(biguint *, biguint *, biguint *);
int biguint__add1(biguint *, biguint *);
int biguint__equal(biguint *, biguint *);
int biguint__is_zero(biguint *);
int biguint__setzero(biguint *);
int biguint__is_inf(biguint *);
int biguint__setinf(biguint *);
char *biguint__tostring(biguint *);

#endif  /* LASH_BIGUINT_H */

/****  End of biguint.h  ****/
