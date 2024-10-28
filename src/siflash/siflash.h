/********************************************************************/
/**                                                                **/
/**  Simple IF LASH (Siflash) compiler -- v0.9                     **/
/**  =================================                             **/
/**                                                                **/
/**     siflash.h  :  Front-end.                                   **/
/**                                                                **/
/**     03/01/01  :  Creation. (LL)                                **/
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

#ifndef SIFLASH_SIFLASH_H
#define SIFLASH_SIFLASH_H

#include "lash-types.h"

/**  External global variables.                                    **/

extern int   siflash_verbose;                   /*  Verbosity flag  */
extern int   siflash_explore;
extern int   siflash_save_steps;
extern char *siflash_sm_name;
extern char *siflash_sndd_name;
extern char *siflash_sdot_name;

/**  Prototypes of public functions.                               **/

void  report_lash_error(char *);
void  report_siflash_error(char *);
int   main(int, char *[]);

/**  Macros.                                                       **/

#define  report_siflash_memory_error() \
             report_siflash_error("Compiler error: Not enough memory")

#define  report_siflash_warning(s)  report_siflash_error(s)

#endif  /* SIFLASH_SIFLASH_H */

/****  End of siflash.h  ****/
