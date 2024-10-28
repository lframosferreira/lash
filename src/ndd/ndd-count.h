/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**   ndd-count.h  :  Counting vectors accepted by NDDs.           **/
/**                                                                **/
/**      20/07/00  :  Creation. (LL)                               **/
/**      18/10/00  :  Some corrections. (LL)                       **/
/**      02/12/01  :  Reorganization. (BB)                         **/
/**      07/15/02  :  Reorganization. (BB)                         **/
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

#ifndef LASH_NDD_COUNT_H
#define LASH_NDD_COUNT_H

#include "ndd.h"
#include "biguint.h"

/**  Function prototypes.                                          **/

int ndd_remove_sign_loop(ndd *, automaton **);
int ndd_count(ndd *, biguint *);
int ndd_max(ndd *, uint4, sint4 *, int *, int *);
int ndd_min(ndd *, uint4, sint4 *, int *, int *);

#endif /* LASH_NDD_COUNT_H */

/****  End of ndd-count.h  ****/
