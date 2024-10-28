/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  datastruct-io.h  :  Data structures specific to I/O           **/
/**                      operations.                               **/
/**                                                                **/
/**         02/12/01  :  Creation. (JMF+BB)                        **/
/**         07/08/02  :  Reorganization. (BB)                      **/
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

#ifndef LASH_DATASTRUCT_IO_H
#define LASH_DATASTRUCT_IO_H

#include <stdio.h>
#include "lash-auto.h"

/**  Datatype definition.                                          **/

typedef struct {
  uint1  *beginning;
  uint4   size;
} mem_block;

/**  Function prototypes.                                          **/

mem_block *mem_block__new(void);
int        mem_block__free(mem_block *);
int        mem_block__content_free(mem_block *);
int        mem_block__content_set(mem_block *, uint1 *, uint4);

#endif  /* LASH_DATASTRUCT_IO_H */

/****  End of datastruct-io.h  ****/
