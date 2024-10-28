/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-transformations.h  :  Linear transformations over sets  **/
/**                 represented as Number Decision Diagrams.       **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/12/99  :  Continued. (BB)                                **/
/**    01/28/99  :  Power of transformations. (BB)                 **/
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

#ifndef LASH_NDD_TRANSFORMATIONS_H
#define LASH_NDD_TRANSFORMATIONS_H

#include "ndd.h"

/**  Linear transformation type definition. The field dim contains
                     the dimension of the vectors to which the
                     transformation can be applied. The coefficients
                     of the transformation matrix are given in *coeff,
                     line by line. The offset vector is in *offset.
                     The number of inequations in the guard is given
                     by gdim. The coefficients of the guard and the
                     vector of second members are given by *gcoeff 
                     and *goffset. Thus, the transformation can be
                     expressed as

                       (*gcoeff) X <= (*goffset) -->
                           X := (*coeff) X + (*offset).            **/

typedef struct {
  uint4   dim, gdim;
  sint4  *coeff, *offset, *gcoeff, *goffset;
} linear_transf;

/**  Transducer that can be associated to a linear transformation in
                     order to speed up the computation of the image
                     of a set by this transformation.              **/

typedef ndd  linear_tr_info;

/**  Function prototypes.                                          **/

linear_transf  *ndd_create_identity_transf(uint4);
linear_transf  *ndd_create_transf(uint4, uint4, sint4 *, sint4 *,
                    sint4 *, sint4 *);
linear_transf  *ndd_create_assign_transf(uint4, uint4, sint4 *, 
                    sint4);
linear_transf  *ndd_create_inequ_transf(uint4, sint4 *, sint4);
linear_transf  *ndd_create_equ_transf(uint4, sint4 *, sint4);
linear_transf  *ndd_transf_compose(linear_transf *, linear_transf *);
linear_transf  *ndd_transf_power(linear_transf *, uint4);
int             ndd_transf_free(linear_transf *);
linear_transf  *ndd_transf_copy(linear_transf *);
ndd            *ndd_image_by_transf(ndd *, linear_transf *);
linear_tr_info *ndd_create_transf_info(linear_transf *, uint1, int);
ndd            *ndd_image_by_transf_info(ndd *, linear_tr_info *);
int             ndd_transf_info_free(linear_tr_info *);

#endif  /* LASH_NDD_TRANSFORMATIONS_H */

/****  End of ndd-transformations.h  ****/
