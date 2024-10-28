/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**    explore.h  :  Interface with state-space exploration tool.  **/
/**                                                                **/
/**     06/03/99  :  Creation. (BB)                                **/
/**     07/17/02  :  Reorganization. (BB)                          **/
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

#ifndef SPLASH_EXPLORE_H
#define SPLASH_EXPLORE_H

#include "lash-types.h"

/**  Prototype of public function.                                 **/

int  expl_init(pgm_program *);

#endif  /* SPLASH_EXPLORE_H */

/****  End of explore.h  ****/
