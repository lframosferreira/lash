/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       ndd.c  :  Manipulation of Number Decision Diagrams.      **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/25/98  :  Continued. (BB)                                **/
/**    07/09/02  :  Reorganization. (BB)                           **/
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
#include "ndd.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto-operations.h"

/****  Public visible functions.                                 ****/

/**  int  ndd_free(nd)  :  Frees the ndd *nd. If successful, returns
                     0. In the case of an error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  ndd_free(nd)
  ndd *nd;
{
 diag__enter("auto_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !nd -> automaton)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  auto_free(nd -> automaton);
  resr__free_object(nd, ndd);

  diag__return(0);
}

/**  ndd *ndd_copy(nd)  :  Returns a copy of the NDD *nd, or a NULL
                     pointer in the case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt NDD.          **/

ndd *ndd_copy(nd)
  ndd *nd;
{
  register ndd *nd2;

  diag__enter("ndd_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  nd2 = resr__new_object(ndd);
  if (!nd2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  nd2 -> dim        = nd -> dim;
  nd2 -> base       = nd -> base;
  nd2 -> properties = nd -> properties;
  nd2 -> automaton = auto_copy(nd -> automaton);

  if (!(nd2 -> automaton))
    {
      resr__free_object(nd2, ndd);
      diag__fail(lash_errno, NULL);
    }

  diag__return(nd2);
}

/****  End of ndd.c  ****/
