/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       lash.h  :  Visible general definitions.                  **/
/**                                                                **/
/**     03/12/98  :  Creation. (BB)                                **/
/**     06/09/98  :  Minor corrections. (BB)                       **/
/**     07/08/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_LASH_H
#define LASH_LASH_H

#include "lash-types.h"

/**  Consistency check level.    0 = no check (use with caution),  **/
/**                              1 = default check level,          **/
/**                              2 = debug check level (slow).     **/

#define LASH_CHECK_LEVEL     1

/**  Function prototypes for initializing and shutting down the    **/
/**  package.                                                      **/

int  lash_init(void);
int  lash_end(void);

#endif  /* LASH_LASH_H */

/****  End of lash.h  ****/
