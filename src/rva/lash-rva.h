/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-rva.h  :  Visible data structures and prototypes for     **/
/**                 manipulating Real Vector Automata.             **/
/**                                                                **/
/**    03/15/01  :  Creation. (SJ)                                 **/
/**    08/20/01  :  0-dimensional RVAs. (SJ)                       **/
/**    11/02/01  :  Inclusion of 'rva-shifting.h'. (SJ)            **/
/**    11/07/01  :  Inclusion of 'rva-tools.h'. (SJ)               **/
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

#ifndef LASH_LASH_RVA_H
#define LASH_LASH_RVA_H

#include "lash-auto.h"

/**  RVA type definition. The base must be greater than 1. The
     field universal is only relevant for 0-dimensional RVAs. If
     dimension = 0 and universal = 1, the RVA represents the 
     universal set { () }. If dimension = 0 and universal = 0, it
     represents the empty set { }. When dimension = 0, the field
     automaton must be NULL and the fields base and properties are
     not relevant.                                                 **/

typedef struct {
  uint4      dim;
  automaton *automaton;
  uint1      base, properties;
  uint1      universal;
} rva;

/**  Files containing the definitions for RVA operations           **/

#include "auto-weak-convert.h"
#include "rva-equations.h"
#include "rva-operations.h"
#include "rva-projection.h"
#include "rva-sets.h"
#include "rva-shifting.h"
#include "rva-tests.h"
#include "rva-tools.h"
#include "rva-transformations.h"

/**  Function prototypes.                                          **/

int  rva_free(rva *);
rva *rva_copy(rva *);

#endif  /* LASH_LASH_RVA_H */

/****  End of lash-rva.h  ****/
