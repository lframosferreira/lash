/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-diag.h  :  Runtime diagnoses and statistics.             **/
/**                                                                **/
/**     03/12/98  :  Creation. (BB)                                **/
/**     06/10/98  :  Minor corrections. (BB)                       **/
/**     07/24/98  :  Got rid of operations count. (BB)             **/
/**     07/29/98  :  New error codes. (BB)                         **/
/**     08/26/98  :  New error code. (BB)                          **/
/**     09/03/98  :  New error codes. (BB)                         **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/07/98  :  Minor corrections. (BB)                       **/
/**     09/24/98  :  New error codes. (BB)                         **/
/**     10/08/98  :  New error code. (BB)                          **/
/**     01/05/99  :  New error code. (BB)                          **/
/**     01/12/99  :  New error code. (BB)                          **/
/**     01/28/99  :  New error code. (BB)                          **/
/**     02/24/99  :  New error code. (BB)                          **/
/**     07/07/00  :  New error codes. (JMF)                        **/
/**     02/05/01  :  New error code. (SJ)                          **/
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

#ifndef LASH_LASH_DIAG_H
#define LASH_LASH_DIAG_H

#include "lash-types.h"

/**  Variable containing the most recent error code.               **/

extern uint4  lash_errno;

/**  Error codes.                                                  **/

#define LASH_ERR_NOERROR     0x0000    /*  No error.                */
#define LASH_ERR_ALREADY     0x0001    /*  Already performed.       */
#define LASH_ERR_BAD_ARCH    0x0002    /*  Bad architecture.        */
#define LASH_ERR_IN_USE      0x0003    /*  Object(s) in use.        */
#define LASH_ERR_NOT_INIT    0x0004    /*  Package not initialized. */
#define LASH_ERR_NO_MEM      0x0005    /*  Not enough memory.       */
#define LASH_ERR_LOGFILE     0x0006    /*  Cannot create logfile.   */
#define LASH_ERR_BAD_STATE   0x0007    /*  No such state.           */
#define LASH_ERR_BAD_TRAN    0x0008    /*  No such transition.      */
#define LASH_ERR_CORRUPT     0x0009    /*  Corrupt data.            */
#define LASH_ERR_PROP        0x000a    /*  Inconsistent property.   */
#define LASH_ERR_BAD_TYPE    0x000b    /*  Bad type of object.      */
#define LASH_ERR_ALPHABET    0x000c    /*  Alphabet mismatch.       */
#define LASH_ERR_ALPH_SIZE   0x000d    /*  Alphabet too large.      */
#define LASH_ERR_BAD_VALUE   0x000e    /*  Bad value of parameter.  */
#define LASH_ERR_ABORT       0x000f    /*  Operation aborted.       */
#define LASH_ERR_BASE        0x0010    /*  Base mismatch.           */
#define LASH_ERR_DIMENSION   0x0011    /*  Dimension mismatch.      */
#define LASH_ERR_NOT_IMPL    0x0012    /*  Not (yet) implemented.   */
#define LASH_ERR_OVERFLOW    0x0013    /*  Arithmetic overflow.     */
#define LASH_ERR_EXCEPTION   0x0014    /*  Arithmetic exception.    */
#define LASH_ERR_ITERATION   0x0015    /*  Not iterable.            */
#define LASH_ERR_INTERRUPT   0x0016    /*  User interrupt.          */
#define LASH_ERR_IO          0x0017    /*  IO error.                */
#define LASH_ERR_FILE        0x0018    /*  File format error.       */
#define LASH_ERR_TOO_BIG     0x0019    /*  Object too large.        */


/**  Function prototype for error reporting.                       **/

void   lash_perror(char *);

/**  Function prototypes for runtime statistics.                   **/

#if LASH_CHECK_LEVEL >= 1

uint8  lash_get_mem_usage(void);
uint8  lash_get_max_mem_usage(void);
void   lash_reset_max_mem_usage(void);

#endif  /* >= 1 */

#endif  /* LASH_LASH_DIAG_H */

/****  End of lash-diag.h  ****/
