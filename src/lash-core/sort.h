/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**          sort.h  :  General-purpose sorting algorithms.        **/
/**                                                                **/
/**        10/27/98  :  Creation. (BB)                             **/
/**        02/19/99  :  Minor correction. (BB)                     **/
/**        08/12/99  :  Improved sorting function for arbitrary    **/
/**                     values. (BB)                               **/
/**        07/08/02  :  Reorganization. (BB)                       **/
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

#ifndef LASH_SORT_H
#define LASH_SORT_H

#include <string.h>
#include "lash-types.h"

/**  Function prototypes.                                          **/

int  uint4__sort_and_pack(uint4 *, uint4 *);
int  bytes__sort(void *, uint4, uint4, int (*)
         (const void *, const void *));
int  bytes__sort_and_pack(void *, uint4 *, uint4, int (*)
         (const void *, const void *), void (*)(const void *));

#endif  /* LASH_SORT_H */

/****  End of sort.h  ****/
