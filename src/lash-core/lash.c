/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       lash.c  :  General function(s).                          **/
/**                                                                **/
/**     03/12/98  :  Creation. (BB)                                **/
/**     06/09/98  :  Minor corrections. (BB)                       **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/07/98  :  Minor corrections. (BB)                       **/
/**     02/24/01  :  New consistency check. (BB)                   **/
/**     07/02/02  :  Reorganization. (BB)                          **/
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

#include "lash.h"
#include "diag.h"

/****  Global variable(s).                                       ****/

int  lash_initialized = 0;         /*  Is the package initialized?  */

/****  Public visible function(s).                               ****/

/**  lash_init()  :  Initializes the package by performing some
                     internal consistency checks. If the check
                     level is zero (see lash.h), then this function
                     does not perform checks and always returns
                     successfully.

                     If success, returns 0. If error, returns -1
                     and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_ALREADY  : Already initialized.
                         LASH_ERR_BAD_ARCH : Bad architecture.
                                             Results from unadapted
                                             type definitions in
                                             "lash-types.h".
                         LASH_ERR_IN_USE   : Objects already in use.
                                             Indicates an internal
                                             inconsistency.        
                         LASH_ERR_LOGFILE  : Cannot create 
                                             logfile.              **/

int  lash_init()
{

#if LASH_CHECK_LEVEL >= 1

  if (lash_initialized != 0)
    {
      lash_errno = LASH_ERR_ALREADY;
      return -1;
    }

  if ((((uint1) -1) != ((uint1) 0xff)) ||
      (sizeof(uint4) != 4 * sizeof(uint1)) ||
      (sizeof(uint8) != 8 * sizeof(uint1)) ||
      (sizeof(uintptr) != sizeof (void *)))
    {
      lash_errno = LASH_ERR_BAD_ARCH;
      return -1;
    }

  if (lash_get_mem_usage())
    {
      lash_errno = LASH_ERR_IN_USE;
      return -1;
    }

#endif

#if LASH_CHECK_LEVEL >= 2

  if (diag__log_init() < 0)
    {
      lash_errno = LASH_ERR_LOGFILE;
      return -1;
    } 

#endif

  lash_initialized = 1;

  return 0;
}

/**  lash_end()   :  Shuts the package down and performs some  
                     internal consistency checks. If the check
                     level is zero (see lash.h), then this function
                     does not perform checks and always returns
                     successfully.

                     If success, returns 0. If error, returns -1
                     and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.    
                         LASH_ERR_IN_USE   : Objects still in use,  
                                             cannot shutdown.      **/
int  lash_end()
{

#if LASH_CHECK_LEVEL >= 1

  if (!lash_initialized)
    {
      lash_errno = LASH_ERR_NOT_INIT;
      return -1;
    }

  if (lash_get_mem_usage())
    {
      lash_errno = LASH_ERR_IN_USE;
      return -1;
    }

#endif

#if LASH_CHECK_LEVEL >= 2

  diag__log_end();

#endif

  lash_initialized = 0;

  return 0;
}

/****  End of lash.c  ****/
