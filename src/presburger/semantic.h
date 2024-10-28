/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   semantic.h  :  Semantic routines.                            **/
/**                                                                **/
/**     05/14/99  :  Creation. (BB)                                **/
/**     05/27/99  :  Continued. (BB)                               **/
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

#ifndef PRESB_SEMANTIC_H
#define PRESB_SEMANTIC_H

#include "lash-types.h"
#include "datastruct.h"

/**  Public variable.                                              **/

extern int (*sem_function[])(stack *, void **);

/**  Prototypes of public functions.                               **/

int  sem_init(void);
int  sem_finish(void);
int  sem_end(void);

#endif  /* PRESB_SEMANTIC_H */

/****  End of semantic.h  ****/
