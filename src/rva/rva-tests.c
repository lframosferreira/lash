/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-tests.c  :  Elementary predicates over sets             **/
/**                 represented as RVAs.                           **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    05/04/01  :  Minor correction. (SJ)                         **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/09/01  :  Minor modifications. (SJ)                      **/
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

/**  int  rva_is_empty(rv)  :  Tests whether the RVA *rv represents
                     the empty set. In the case of success, this
                     function returns a Boolean value (0 if the set
                     is not empty, 1 otherwise). In the case of an
                     error, the function returns -1 and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt RVA.        
			 LASH_ERR_TOO_BIG  : Automaton with too many
			                     states.               **/

int  rva_is_empty(rv)
  rva *rv;
{
  register int  r;

  diag__enter("rva_is_empty", -1);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (rv -> dim &&
      (auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE ||
       (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_BUCHI &&
	auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK)))
    diag__fail(LASH_ERR_CORRUPT, -1);

#endif 

  if (!(rv -> dim))
    diag__return(!(rv -> universal));

  r = auto_empty_language(rv -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  int  rva_inclusion(rv1, rv2)  :  Checks whether the set 
                     represented by the RVA *rv1 is included in the
                     set represented by the RVA *rv2. These two RVAs
                     must use the same base and be of the same type
                     and dimension.

		     In the case of success, this function returns a
                     Boolean value (1 if the first set is a subset of
                     the second one, 0 otherwise). In the case of an
                     error, the function returns -1 and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        
			 LASH_ERR_TOO_BIG    : Automaton with too many
			                       states.             **/

int  rva_inclusion(rv1, rv2)
  rva *rv1, *rv2;
{
  register int  r;

  diag__enter("rva_inclusion", -1);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (rv1 -> dim &&
      auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
      (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
       rv1 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_CORRUPT, -1);

#endif

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (!(rv1 -> dim))
    diag__return(!(rv1 -> universal) || rv2 -> universal);
  
  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (!(rv2 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, -1);

  if (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK ||
      !(auto_test_property(rv2 -> automaton, AUTO_PROP_DETERM)))
    diag__fail(LASH_ERR_CORRUPT, -1);

  r = auto_inclusion(rv1 -> automaton, rv2 -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  int  rva_equality(rv1, rv2)  :  Checks whether the set 
                     represented by the RVA *rv1 is equal to the
                     set represented by the RVA *rv2. These two RVAs
                     must use the same base and be of the same type
                     and dimension.

		     In the case of success, this function returns a
		     Boolean value (0 if the sets are different, 1
		     otherwise). In the case of an error, the function
		     returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        
			 LASH_ERR_TOO_BIG    : Automaton with too many
			                       states.             **/

int  rva_equality(rv1, rv2)
  rva *rv1, *rv2;
{
  register int  r;

  diag__enter("rva_equality", -1);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, -1);

#endif

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (!(rv1 -> dim))
    diag__return((rv1 -> universal && rv2 -> universal) ||
		 (!(rv1 -> universal) && !(rv2 -> universal)));

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (!(rv1 -> properties & RVA_PROP_RESTRICTED) ||
      !(rv2 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, -1);

  if (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK ||
      !(auto_test_property(rv1 -> automaton, AUTO_PROP_DETERM)) ||
      auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK ||
      !(auto_test_property(rv1 -> automaton, AUTO_PROP_DETERM)))
    diag__fail(LASH_ERR_CORRUPT, -1);

  r = auto_equality(rv1 -> automaton, rv2 -> automaton);
  if (r < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}


/**  int  rva_disjoint(rv1, rv2)  :  Checks whether the sets 
                     represented by the RVA *rv1 and *rv2 are
                     disjoint.  These two RVA must use the same base
                     and be of the same type and dimension.

		     In the case of success, this function returns a
                     Boolean value (0 if the sets are not disjoint, 1
                     otherwise). In the case of an error, the function
                     returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        
			 LASH_ERR_TOO_BIG    : Automaton with too many
			                       states.             **/

int  rva_disjoint(rv1, rv2)
  rva *rv1, *rv2;
{
  register rva *inter;
  register int  r;

  diag__enter("rva_disjoint", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, -1);

  if ((rv1 -> dim &&
       auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv1 -> properties & RVA_PROP_RESTRICTED)) ||

      (rv2 -> dim &&
       auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv2 -> properties & RVA_PROP_RESTRICTED)))

    diag__fail(LASH_ERR_CORRUPT, -1);

#endif

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (!(rv1 -> dim))
    diag__return(!(rv1 -> universal && rv2 -> universal));

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  inter = rva_intersection(rv1, rv2);
  if (!inter)
    diag__fail(lash_errno, -1);

  r = auto_empty_language(inter -> automaton);
  if (r < 0)
    {
      rva_free(inter);
      diag__fail(lash_errno, -1);
    }

  rva_free(inter);

  diag__return(r);
}

/****  End of rva-tests.c  ****/
