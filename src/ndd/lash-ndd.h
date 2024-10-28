/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-ndd.h  :  Visible data structures and prototypes for     **/
/**                 manipulating Number Decision Diagrams.         **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/24/98  :  Continued. (BB)                                **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    09/29/98  :  Continued. (BB)                                **/
/**    10/06/98  :  Projection module. (BB)                        **/
/**    01/05/99  :  Transformation module. (BB)                    **/
/**    01/15/99  :  Iteration module. (BB)                         **/
/**    02/22/99  :  State machine module. (BB)                     **/
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

#ifndef LASH_LASH_NDD_H
#define LASH_LASH_NDD_H

#include "lash-auto.h"

/**  NDD type definition. The base must be greater than 1.         **/

typedef struct {
  uint4      dim;
  automaton *automaton;
  uint1      base, properties;
} ndd;

/**  Default direction of NDDs (Most/Least Significant Digit
     First).                                                       **/

#define  NDD_MSDF

#include "ndd-sets.h"
#include "ndd-operations.h"
#include "ndd-tests.h"
#include "ndd-equations.h"
#include "ndd-projection.h"
#include "ndd-transformations.h"
#include "ndd-iterations.h"
#include "ndd-machines.h"

/**  Function prototypes.                                          **/

int  ndd_free(ndd *);
ndd *ndd_copy(ndd *);

#endif  /* LASH_LASH_NDD_H */

/****  End of lash-ndd.h  ****/
