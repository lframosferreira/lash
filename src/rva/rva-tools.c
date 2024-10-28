/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-tools.h  :  Tools for sets represented as RVAs.         **/
/**                                                                **/
/**    11/07/01  :  Creation. (SJ)                                 **/
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
#include "auto-serialize.h"
#include "auto-minimize.h"

/****  Public visible functions.                                 ****/

/**  rva  *rva_serialize(rv)  :  Computes a serialized RVA that
                     accepts the same set than the synchronized RVA
		     *rv. In other words, the RVA returned by this
		     function reads its vector components one by
		     one rather than as a tuple.

                     This function does not modify *rv, and returns
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_serialize(rv)
  rva *rv;
{
  register rva        *rvr;
  register automaton  *a;
  register uint1      *separ;
  register uint4       i;

  diag__enter("rva_serialize", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv -> dim &&
      auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (rv -> dim <= 1)
    {
      rvr = rva_copy(rv);
      if (!rvr)
	diag__fail(lash_errno, NULL);

      rvr -> properties = rv -> properties | RVA_PROP_SERIAL;

      diag__return(rvr);
    }

  if (rv -> properties & RVA_PROP_SERIAL)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  separ = resr__new_objects(uint1, rv -> dim);
  if (!separ)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  for (i=0 ; i<rv -> dim ; i++)
    separ[i] = rv -> base;

  a = auto_serialize(rv -> automaton, separ);
  resr__free_objects(separ, uint1, rv -> dim);

  if (!a)
    diag__fail(lash_errno, NULL);

  if (rv -> properties & RVA_PROP_RESTRICTED)
    if (auto_determinize(a) < 0 ||
	auto_convert_to_weak(a) < 0 ||
	auto_minimize(a) < 0)
      {
	auto_free(a);
	diag__fail(lash_errno, NULL);
      }

  rvr = resr__new_object(rva);
  if (!rvr)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rvr -> dim        = rv -> dim;
  rvr -> base       = rv -> base;
  rvr -> properties = rv -> properties | RVA_PROP_SERIAL;
  rvr -> automaton  = a;
  
  diag__return(rvr);
}

/**  rva  *rva_unserialize(rv)  :  Computes a synchronized RVA that
                     accepts the same set than the serialized RVA
		     *rv. In other words, the RVA returned by this
		     function reads its vector components as a
		     tuple rather than one by one.

                     This function does not modify *rv, and returns
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_unserialize(rv)
  rva *rv;
{
  register rva        *rvr;
  register automaton  *a;

  diag__enter("rva-unserialize", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv -> dim &&
      auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (rv -> dim <= 1)
    {
      rvr = rva_copy(rv);
      if (!rvr)
	diag__fail(lash_errno, NULL);

      rvr -> properties = rv -> properties & ~RVA_PROP_SERIAL;

      diag__return(rvr);
    }

  if (!(rv -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  a = auto_unserialize(rv -> automaton, rv -> dim,
		       (uint1 *) &(rv -> base));
  if (!a)
    diag__fail(lash_errno, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rvr -> dim        = rv -> dim;
  rvr -> base       = rv -> base;
  rvr -> properties = rv -> properties & ~RVA_PROP_SERIAL;
  rvr -> automaton  = a;
  
  diag__return(rvr);
}

/****  End of rva-tools.c  ****/
