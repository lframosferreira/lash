/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-transformations.h  :  Linear transformations over sets  **/
/**                 represented as Real Vector Automata.           **/
/**                                                                **/
/**    03/26/01  :  Creation. (SJ)                                 **/
/**    03/28/01  :  Continued. (SJ)                                **/
/**    04/18/02  :  Added 'rva_transf_info_copy'. (SJ)             **/
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

#ifndef LASH_RVA_TRANSFORMATIONS_H
#define LASH_RVA_TRANSFORMATIONS_H

#include "rva.h"
#include "datastruct.h"

/**  Linear transformation type definition. The field dim contains
                     the dimension of the vectors to which the
                     transformation can be applied. The coefficients
                     of the transformation matrix are given in *coeff,
                     line by line. The offset vector is in *offset.
		     The transformation of each variable is balanced
		     by a multiplicative coefficient stored in *ratio.
		     The number of inequations in the guard is
		     given by gdim. The coefficients of the guard and
		     the vector of second members are respectively
		     given by *gcoeff and *goffset. Thus, the
		     transformation can be expressed as

                       (*gcoeff) X <= (*goffset) -->
                           X := ((*coeff) X + (*offset)) / (*ratio). 

		     where we have allowed the division term
		     by term of two vectors.                       **/

typedef struct {
  uint4       dim, gdim;
  sint4      *coeff, *offset, *gcoeff, *goffset;
  uint4      *ratio;
} rlinear_transf;

/**  Transducer that can be associated to a linear transformation in
                     order to speed up the computation of the image
                     of a set by this transformation.              **/

typedef rva  rlinear_tr_info;

/**  Function prototypes.                                          **/

rlinear_transf  *rva_create_identity_transf(uint4);
rlinear_transf  *rva_create_transf(uint4, uint4, sint4 *, sint4 *,
                    sint4 *, sint4 *, sint4 *, sint4 *, sint4 *, 
				   sint4 *);
rlinear_transf  *rva_create_assign_transf(uint4, uint4, sint4 *, 
					  sint4 *, sint4, sint4);
rlinear_transf  *rva_create_inequ_transf(uint4, sint4 *, sint4 *, 
					 sint4, sint4);
rlinear_transf  *rva_create_equ_transf(uint4, sint4 *, sint4 *, sint4,
				       sint4);
rlinear_transf  *rva_transf_compose(rlinear_transf *, 
				    rlinear_transf *);
rlinear_transf  *rva_transf_power(rlinear_transf *, uint4);
int              rva_transf_free(rlinear_transf *);
rlinear_transf  *rva_transf_copy(rlinear_transf *);
rva             *rva_image_by_transf(rva *, rlinear_transf *);
rlinear_tr_info *rva_create_transf_info(rlinear_transf *, uint1);
rva             *rva_image_by_transf_info(rva *, rlinear_tr_info *);
int              rva_transf_info_free(rlinear_tr_info *);
rlinear_tr_info *rva_transf_info_copy(rlinear_tr_info *);

#endif  /* LASH_RVA_TRANSFORMATIONS_H */

/****  End of rva-transformations.h  ****/
