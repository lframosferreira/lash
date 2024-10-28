/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    diag.h  :  Invisible definitions for runtime diagnoses      **/
/**               and statistics.                                  **/
/**                                                                **/
/**  03/12/98  :  Creation. (BB)                                   **/
/**  06/10/98  :  Minor modifications. (BB)                        **/
/**  07/24/98  :  Got rid of operations count. (BB)                **/
/**  08/04/98  :  'log_message' added. (BB)                        **/
/**  09/04/98  :  Reorganization. (BB)                             **/
/**  09/07/98  :  Minor corrections. (BB)                          **/
/**  07/08/02  :  Reorganization. (BB)                             **/
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

#ifndef LASH_DIAG_H
#define LASH_DIAG_H

#include "lash-diag.h"

/**  External reference.                                           **/

extern int  lash_initialized;      /*  (see lash.c).                */

/**  Name of logfile.                                               */

#if LASH_CHECK_LEVEL >= 2

#define LOG_FILE_NAME  "lash.log"

#endif  /* >= 2 */

/**  Prototypes of invisible functions.                             */

#if LASH_CHECK_LEVEL >= 2

int   diag__log_init(void);
void  diag__log_entry(char *);
void  diag__log_return(void);
void  diag__log_fail(void);
void  diag__log_message(char *);
void  diag__log_uint4(uint4);
void  diag__log_end(void);

#endif  /* >= 2 */

/**  Macro diag__enter(char *s, rc)  :  Indicates the beginning    **/
/**                  of the execution of the function named after  **/
/**                  the argument s. If the consistency checks     **/
/**                  performed according to the current check      **/
/**                  level (see lash.h) fail, the current function **/
/**                  returns immediately with the return code      **/
/**                  provided by the argument rc.                  **/

#if LASH_CHECK_LEVEL == 0
#define diag__enter(s, rc)
#endif  /* 0 */

#if LASH_CHECK_LEVEL == 1
#define diag__enter(s, rc)  \
          { if (!lash_initialized) { \
                lash_errno = LASH_ERR_NOT_INIT; return(rc); }}
#endif  /* 1 */

#if LASH_CHECK_LEVEL >= 2
#define diag__enter(s, rc)  \
          {  \
            if (!lash_initialized) { \
                lash_errno = LASH_ERR_NOT_INIT; return(rc);}  \
            diag__log_entry(s);  \
	  }
#endif  /* >= 2 */

/**  Macro diag__return(rc)  :  Indicates the end of the           **/
/**                  execution of the current function, which      **/
/**                  returns immediately with the return code      **/
/**                  provided by the argument rc.                  **/

#if LASH_CHECK_LEVEL <= 1
#define diag__return(rc)  return(rc)
#endif  /* <= 1 */

#if LASH_CHECK_LEVEL >= 2
#define diag__return(rc)  \
          {  \
            diag__log_return();  \
            return(rc);  \
	  }
#endif  /* >= 2 */

/**  Macro diag__fail(ec, rc)  :  Indicates a failure of the       **/
/**                  execution of the current function, which      **/
/**                  returns immediately with the return code      **/
/**                  provided by the argument rc, and sets         **/
/**                  lash_errno (see lash-diag.c) to the value     **/
/**                  provided by the argument ec.                  **/

#if LASH_CHECK_LEVEL <= 1
#define diag__fail(ec, rc)  \
          {  \
            lash_errno = ec;  \
            return(rc);  \
	  }
#endif  /* <= 1 */

#if LASH_CHECK_LEVEL >= 2
#define diag__fail(ec, rc)  \
          {  \
            lash_errno = ec;  \
            diag__log_fail();  \
            return(rc);  \
	  }
#endif  /* >= 2 */

/**  Memory logging functions.                                     **/

#if LASH_CHECK_LEVEL == 0
#define diag__log_alloc(x)
#define diag__log_unalloc(x)
#endif  /* 0 */

#if LASH_CHECK_LEVEL >= 1
void  diag__log_alloc(uint8);
void  diag__log_unalloc(uint8);
#endif  /* >= 1 */

#endif  /* LASH_DIAG_H */

/****  End of diag.h  ****/
