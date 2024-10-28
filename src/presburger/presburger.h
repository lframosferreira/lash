/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   presburger.h                                                 **/
/**                                                                **/
/**     12/14/00  : Creation. (LL)                                 **/
/**     03/27/02  : base and msdf global variables. (LL)           **/
/**     08/29/02  : Reorganization. (BB)                           **/
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

#ifndef PRESB_PRESB_H
#define PRESB_PRESB_H

#include "lash-types.h"

/**  Verbosity flag.                                               **/

extern char *presb_se_name;
extern char *presb_sndd_name;
extern char *presb_sdot_name;
extern uint1 base,msdf;

/**  Prototypes of public functions.                               **/

void  report_lash_error(char *);
void  report_presb_error(char *);
int   main(int, char *[]);

/**  Macros.                                                       **/

#define  report_presb_memory_error() \
             report_presb_error("Compiler error: Not enough memory")
#define  report_presb_corrupted() \
             report_presb_error("Compiler error: Corrupted argument")

#define  report_presb_warning(s)  report_presb_error(s)

#endif  /* PRESB_PRESB_H */

/****  End of presburger.h  ****/
