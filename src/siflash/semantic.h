/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (Siflash) compiler -- v0.9                **/
/**  ======================================                        **/
/**                                                                **/
/**   semantic.h  :  Semantic routines.                            **/
/**                                                                **/
/**     11/15/99  :  Creation. (LL)                                **/
/**     09/05/02  :  Reorganization. (BB)                          **/
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

#ifndef SIFLASH_SEMANTIC_H
#define SIFLASH_SEMANTIC_H

#include "lash-types.h"
#include "datastruct.h"

/**  Declaration of public variable.                               **/

extern int (*sem_function[])(stack *, void **, uint1 *);

/**  Prototypes of public functions.                               **/

int  sem_init(void);
int  sem_finish(void);
int  sem_end(void);

#endif  /* SIFLASH_SEMANTIC_H */

/****  End of semantic.h  ****/
