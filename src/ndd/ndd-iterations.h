/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-iterations.h  :  Computation of the repeated effect of  **/
/**                 linear transformations over sets represented   **/
/**                 as Number Decision Diagrams.                   **/
/**                                                                **/
/**    01/15/99  :  Creation. (BB)                                 **/
/**    01/26/99  :  Image by closure of transformations. (BB)      **/
/**    02/10/00  :  Efficient data structures for iterations. (BB) **/
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

#ifndef LASH_NDD_ITERATIONS_H
#define LASH_NDD_ITERATIONS_H

#include "ndd.h"

/**  Information that can be associed to a vector in order to speed up
                     the computation of the image of a set by repeated
                     translations along this vector.               **/

typedef struct {
  uint4  dim;        /*  number of NDDs in the structure.           */
  ndd  **ndd;
} translation_info;
	
/**  Information that can be associed to a Presburger-iterable linear 
                     transformation in order to speed up the 
                     computation of the image of a set by the closure
                     of this transformation.
		                                                   **/
typedef struct {
  uint4             p;
  ndd              *tr, *trp, *tr2p;
  translation_info *ti;
} presburger_star_info;
                              
/**  Information that can be associed to a linear transformation in
                     order to speed up the computation of the image
                     of a set by the closure of this transformation.
		                                                   **/
typedef struct {
  union {
    presburger_star_info *psi; 
                     /*  if type == NDD_ITERATE_PRESBURGER.         */
  }      val;
  uint1  type;
} linear_star_info;

/**  Values of the field 'type' in structure linear_star_info.     **/

#define  NDD_ITERATE_NONE        0x00
#define  NDD_ITERATE_PRESBURGER  0x01
#define  NDD_ITERATE_GENERAL     0x02

/**  Function prototypes.                                          **/

int               ndd_definable_closure(uint1, linear_transf *, int *,
                      uint4 *, uint4 *);
ndd              *ndd_image_by_star_transf(ndd *, linear_transf *);
linear_star_info *ndd_create_star_info(linear_transf *, uint1, int);
ndd              *ndd_image_by_star_info(ndd *, linear_star_info *);
int               ndd_star_info_free(linear_star_info *);
sint8             ndd_star_info_size(linear_star_info *);

#endif  /* LASH_NDD_ITERATIONS_H */

/****  End of ndd-iterations.h  ****/
