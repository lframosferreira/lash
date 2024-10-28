/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       rva.c  :  Manipulation of Real Vector Automata.          **/
/**                                                                **/
/**    03/15/01  :  Creation. (SJ)                                 **/
/**    08/20/01  :  0-dimensional RVAs. (SJ)                       **/
/**    07/10/02  :  Reorganization. (BB)                           **/
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

#include <stdio.h>
#include "rva.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto-operations.h"

/****  Public visible functions.                                 ****/

/**  int  rva_free(rv)  :  Frees the rva *rv. If successful, returns
                     0. In the case of an error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  rva_free(rv)
  rva *rv;
{
 diag__enter("rva_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!rv ||
      ((rv -> dim) && !rv -> automaton) ||
      (!(rv -> dim) && rv -> automaton))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (rv -> automaton)
    auto_free(rv -> automaton);

  resr__free_object(rv, rva);

  diag__return(0);
}

/**  rva *rva_copy(rv)  :  Returns a copy of the RVA *rv, or a NULL
                     pointer in the case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt RVA.          **/

rva *rva_copy(rv)
  rva *rv;
{
  register rva *rv2;

  diag__enter("rva_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  rv2 = resr__new_object(rva);
  if (!rv2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rv2 -> dim        = rv -> dim;
  rv2 -> base       = rv -> base;
  rv2 -> properties = rv -> properties;
  rv2 -> universal  = rv -> universal;

  if (rv -> automaton)
    {
      rv2 -> automaton = auto_copy(rv -> automaton);
      if (!(rv2 -> automaton))
	{
	  resr__free_object(rv2, rva);
	  diag__fail(lash_errno, NULL);
	}
    }
  else
    rv2 -> automaton = NULL;

  diag__return(rv2);
}

/****  End of rva.c  ****/
