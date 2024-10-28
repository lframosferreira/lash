/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-tests.c  :  Elementary predicates over sets             **/
/**                 represented as NDDs.                           **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    02/19/99  :  Minor corrections. (BB)                        **/
/**    02/24/99  :  Minor correction. (BB)                         **/
/**    08/17/01  :  Minor correction. (BB)                         **/
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

/**  int  ndd_is_empty(nd)  :  Tests whether the NDD *nd represents
                     the empty set. In the case of success, this
                     function returns a Boolean value (0 if the set
                     is not empty, 1 otherwise). In the case of an
                     error, the function returns -1 and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

int  ndd_is_empty(nd)
  ndd *nd;
{
  register int  r;

  diag__enter("ndd_is_empty", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif 

  r = auto_empty_language(nd -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  int  ndd_inclusion(nd1, nd2)  :  Checks whether the set 
                     represented by the NDD *nd1 is included in the
                     set represented by the NDD *nd2. These two NDD
                     must use the same base and be of the same type
                     and dimension.

		     In the case of success, this function returns a
                     Boolean value (1 if the first set is a subset of
                     the second one, 0 otherwise). In the case of an
                     error, the function returns -1 and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

int  ndd_inclusion(nd1, nd2)
  ndd *nd1, *nd2;
{
  register int  r;

  diag__enter("ndd_inclusion", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  r = auto_inclusion(nd1 -> automaton, nd2 -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  int  ndd_equality(nd1, nd2)  :  Checks whether the set 
                     represented by the NDD *nd1 is equal to the
                     set represented by the NDD *nd2. These two NDD
                     must use the same base and be of the same type
                     and dimension.

		     In the case of success, this function returns a
		     Boolean value (0 if the sets are different, 1
		     otherwise). In the case of an error, the function
		     returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

int  ndd_equality(nd1, nd2)
  ndd *nd1, *nd2;
{
  register int  r;

  diag__enter("ndd_equality", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  r = auto_equality(nd1 -> automaton, nd2 -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}


/**  int  ndd_disjoint(nd1, nd2)  :  Checks whether the sets 
                     represented by the NDD *nd1 and *nd2 are
                     disjoint.  These two NDD must use the same base
                     and be of the same type and dimension.

		     In the case of success, this function returns a
                     Boolean value (0 if the sets are not disjoint, 1
                     otherwise). In the case of an error, the function
                     returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

int  ndd_disjoint(nd1, nd2)
  ndd *nd1, *nd2;
{
  register int  r;

  diag__enter("ndd_disjoint", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  r = auto_empty_intersection(nd1 -> automaton, nd2 -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/****  End of ndd-tests.c  ****/
