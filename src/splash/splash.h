/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**     splash.h  :  Front-end.                                    **/
/**                                                                **/
/**     05/05/99  :  Creation. (BB)                                **/
/**     06/03/99  :  Continued. (BB)                               **/
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

#ifndef SPLASH_SPLASH_H
#define SPLASH_SPLASH_H

#include "lash-types.h"

/**  Verbosity flag.                                               **/

extern int   splash_verbose;
extern int   splash_explore;
extern char *splash_sm_name;

/**  Prototypes of public functions.                               **/

void  report_lash_error(char *);
void  report_splash_error(char *);
int   main(int, char *[]);

/**  Macros.                                                       **/

#define  report_splash_memory_error() \
             report_splash_error("Compiler error: Not enough memory")

#define  report_splash_warning(s)  report_splash_error(s)

#endif  /* SPLASH_SPLASH_H */

/****  End of splash.h  ****/
