/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   output.h  :  Presburger expressions output.                  **/
/**                                                                **/
/**     12/14/00  :  Creation. (LL)                                **/
/**     08/29/02  :  Reorganization. (BB)                          **/
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

#ifndef PRESB_OUTPUT_H
#define PRESB_OUTPUT_H

#include "lash-types.h"

/**  Prototype of public functions.                                **/

int  out_expression_string(exp_gen_condition  *);
int  out_expression_ndd(ndd  *, exp_variable *, uint4, uint4);
int  out_expression_dot(ndd *);

#endif  /* PRESB_OUTPUT_H */

/****  End of output.h  ****/
