/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    cyclotomic.h  :  Table of the first cyclotomic              **/
/**                     polynomials.                               **/
/**                                                                **/
/**    01/20/99  :  Creation. (BB)                                 **/
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

#ifndef LASH_NDD_CYCLOTOMIC_H
#define LASH_NDD_CYCLOTOMIC_H

#include "lash-types.h"

/**  Maximal degree of the polynomials contained in the tables. This
                         degree must be stricly less than 256. If
			 the value of CYCLO_MAX_DEGREE is modified,
			 then the program 'cyclo-generate' should be
                         recompiled and executed in order to generate
                         an updated version of 'cyclotomic.c'.    **/

#define  CYCLO_MAX_DEGREE  255

/**  Variable containing an array of triples of values. Each triple
                         (n, i, j) is composed of the index n of a
                         cyclotomic polynomial, the offset i of the
                         first byte related to Phi(n) in the variable
                         cyclo__bytes, and the offset j of of the
                         first byte related to the next polynomial in
                         cyclo__bytes.                             **/

extern uint4  cyclo__index[];

/**  Variable containing the number of entries in cyclo__index.    **/

extern uint4  cyclo__index_size;

/**  Variable containing an array of pairs of bytes. Each pair is
                         composed of the (unsigned) degree of a term
                         of a polynomial and the (signed) coefficient
                         of this term. The terms belonging to each
			 polynomial appear in decreasing order of
			 degree. As an example, the polynomial
                         -1 + x^2 + 3 x^7 is represented as the bytes
                         0x07, 0x03, 0x02, 0x01, 0x00, 0xff.       **/

extern uint1  cyclo__bytes[];

#endif  /* LASH_NDD_CYCLOTOMIC_H */

/****  End of cyclotomic.h  ****/
