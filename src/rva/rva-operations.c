/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-operations.c  :  Elementary operations over sets        **/
/**                 represented as RVAs.                           **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/09/01  :  Minor modifications. (SJ)                      **/
/**    08/10/01  :  Management of RVAs with zero component. (SJ)   **/
/**    08/20/01  :  Continued. (SJ)                                **/
/**    09/26/01  :  Serialization of RVAs. (SJ)                    **/
/**    10/12/01  :  Added 'rva_interleave'. (SJ)                   **/
/**    11/07/01  :  Reorganization with 'rva-tools.h'. (SJ)        **/
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
#include "lash-auto-operations.h"
#include "auto-minimize.h"

/****  Public visible functions.                                 ****/

/**  rva *rva_product(rv1, rv2)  :  Computes a RVA representing the
                     Cartesian product of the sets represented by the
                     two RVAs *rv1 and *rv2. These two RVAs must use
                     the same base and be of the same type.
 
                     This function does not modify *rv1 or *rv2, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_ALPH_SIZE  : Alphabet of resulting
                                               RVA is too large.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_product(rv1, rv2)
  rva *rv1, *rv2;
{
  register rva    *rvr;
  register uint1  *s1, *s2;

  diag__enter("rva_product", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim && 
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv1 -> properties & RVA_PROP_RESTRICTED)) ||

      (rv2 -> dim &&
       auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv2 -> properties & RVA_PROP_RESTRICTED)))

    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (!(rv1 -> dim))
    {
      if (rv1 -> universal)
	rvr = rva_copy(rv2);
      else
	rvr = rva_create_empty(rv2 -> base, rv2 -> dim);

      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (!(rv2 -> dim))
    {
      if (rv2 -> universal)
	rvr = rva_copy(rv1);
      else
	rvr = rva_create_empty(rv1 -> base, rv1 -> dim);

      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv1 -> dim + rv2 -> dim;
  rvr -> base = rv1 -> base;
  rvr -> properties = rv1 -> properties;
  if (!(rv2 -> properties & RVA_PROP_RESTRICTED))
    rvr -> properties = rvr -> properties & ~RVA_PROP_RESTRICTED;

  if (rv1 -> properties & RVA_PROP_SERIAL)
    rvr -> automaton  =
      auto_seq_product_separ(rv1 -> automaton, rv2 -> automaton, 
          rv1 -> dim, rv2 -> dim, (uint1 *) &rv1 -> base);
  else
    {
      s1 = resr__new_objects(uint1, rv1 -> dim);
      if (!s1)
	{
	  resr__free_object(rvr, rva);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      s2 = resr__new_objects(uint1, rv2 -> dim);
      if (!s2)
	{
	  resr__free_object(rvr, rva);
	  resr__free_objects(s1, uint1, rv1 -> dim);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      memset(s1, rvr -> base, rv1 -> dim);
      memset(s2, rvr -> base, rv2 -> dim);

      rvr -> automaton =
	auto_product_separ(rv1 -> automaton, rv2 -> automaton, 
			   s1, s2);

      resr__free_objects(s1, uint1, rv1 -> dim);
      resr__free_objects(s2, uint1, rv2 -> dim);
    }

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (rvr -> properties & RVA_PROP_RESTRICTED)
    {
      auto_accept_type(rvr -> automaton) = AUTO_ACCEPT_WEAK;
      /* This line avoids explicit conversion to weak automaton :
	 the minimization procedure will do the job. */
      
      if (auto_minimize(rvr -> automaton) < 0)
	{
	  auto_free(rvr -> automaton);
	  resr__free_object(rvr, rva);
	  diag__fail(lash_errno, NULL);
	}
    }

  diag__return(rvr);
}

/**  rva *rva_difference(rv1, rv2)  :  Computes a RVA representing
                     the difference of the sets represented by
                     the two RVAs *rv1 and *rv2. These two RVAs must
                     use the same base and be of the same type and
                     dimension.
 
                     This function does not modify *rv1 or *rv2, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_NOT_IMPL   : Not implemented.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_difference(rv1, rv2)
  rva *rv1, *rv2;
{
  register rva *rvr;

  diag__enter("rva_difference", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv1 -> dim &&
      auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
      (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
       rv1 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv1 -> dim))
    {
      if (rv1 -> universal && !(rv2 -> universal))
	rvr = rva_copy(rv1);
      else
	rvr = rva_create_empty(rv1 -> base, 0);

      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  if (!(rv2 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK ||
      !(auto_test_property(rv2 -> automaton, AUTO_PROP_DETERM)))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv1 -> dim;
  rvr -> base = rv1 -> base;
  rvr -> properties = rv1 -> properties; 
  rvr -> automaton  = auto_difference(rv1 -> automaton,
      rv2 -> automaton);

  if (rvr -> properties & RVA_PROP_RESTRICTED)
    {
      if (rvr -> automaton)
	auto_accept_type(rvr -> automaton) = AUTO_ACCEPT_WEAK;
      
      if (!(rvr -> automaton) || auto_minimize(rvr -> automaton) < 0)
	{
	  resr__free_object(rvr, rva);
	  diag__fail(lash_errno, NULL);
	}
    }

  diag__return(rvr);
}

/**  rva *rva_union(rv1, rv2)  :  Computes a RVA representing the
                     union of the sets represented by the two RVAs
                     *rv1 and *rv2. These two RVAs must use the same
                     base and be of the same type and dimension.

                     This function does not modify *rv1 or *rv2, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_union(rv1, rv2)
  rva *rv1, *rv2;
{
  register rva *rvr;

  diag__enter("rva_union", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv1 -> properties & RVA_PROP_RESTRICTED)) ||

      (rv2 -> dim &&
       auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv2 -> properties & RVA_PROP_RESTRICTED)))
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv1 -> dim))
    {
      rvr = rva_copy(rv1 -> universal ? rv1 : rv2);
      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv1 -> dim;
  rvr -> base = rv1 -> base;
  rvr -> properties = rv1 -> properties;
  if (!(rv2 -> properties & RVA_PROP_RESTRICTED))
    rvr -> properties = rvr -> properties & ~RVA_PROP_RESTRICTED;  

  rvr -> automaton  = auto_union(rv1 -> automaton, rv2 -> automaton);

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (rvr -> properties & RVA_PROP_RESTRICTED)
    {
      if (auto_determinize(rvr -> automaton) < 0 ||
	  auto_convert_to_weak(rvr -> automaton) < 0 ||
	  auto_minimize(rvr -> automaton) < 0)
	{
	  resr__free_object(rvr, rva);
	  diag__fail(lash_errno, NULL);
	}
    }

  diag__return(rvr);
}

/**  int  rva_merge(rv1, rv2)  :  Computes a RVA representing the 
                     union of the sets represented by the two RVAs
                     *rv1 and *rv2, and replaces *rv1 by the result of
                     the operation. The two RVAs *rv1 and *rv2 must
                     use the same base and be of the same type and 
                     dimension.

                     This function does not modify *rv2, and
                     returns 0 in the case of success, -1 in the
                     case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
			 LASH_ERR_NOT_IMPL   : Not implemented.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

int  rva_merge(rv1, rv2)
  rva *rv1, *rv2;
{
  register int  r;

  diag__enter("rva_merge", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, -1);

#endif  /* >= 1 */

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (!(rv1 -> dim))
    {
      rv1 -> universal = rv1 -> universal || rv2 -> universal;
      diag__return(0);
    }

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (!(rv1 -> properties & RVA_PROP_RESTRICTED) ||
      !(rv2 -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, -1);
      
  if (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK ||
      auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK)
    diag__fail(LASH_ERR_CORRUPT, -1);

  r = auto_merge(rv1 -> automaton, rv2 -> automaton);
  if (r < 0 ||
      auto_determinize(rv1 -> automaton) < 0 ||
      auto_convert_to_weak(rv1 -> automaton) < 0 ||
      auto_minimize(rv1 -> automaton) < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  rva *rva_intersection(rv1, rv2)  :  Computes a RVA representing
                     the intersection of the sets represented by the
                     two RVAs *rv1 and *rv2. These two RVAs must use
                     the same base and be of the same type and
                     dimension.

                     This function does not modify *rv1 or *rv2, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_intersection(rv1, rv2)
  rva *rv1, *rv2;
{
  register rva *rvr;

  diag__enter("rva_intersection", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv1 -> properties & RVA_PROP_RESTRICTED)) ||
      (rv2 -> dim &&
       auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv2 -> properties & RVA_PROP_RESTRICTED)))
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (rv1 -> dim != rv2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv1 -> dim))
    {
      rvr = rva_copy(rv1 -> universal ? rv2 : rv1);
      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((rv1 -> properties & RVA_PROP_SERIAL) ^
      (rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv1 -> dim;
  rvr -> base = rv1 -> base;
  rvr -> properties = rv1 -> properties;
  if (!(rv2 -> properties & RVA_PROP_RESTRICTED))
    rvr -> properties = rvr -> properties & ~RVA_PROP_RESTRICTED;

  rvr -> automaton  = auto_intersection(rv1 -> automaton,
      rv2 -> automaton);

  if (rvr -> automaton)
    auto_accept_type(rvr -> automaton) = AUTO_ACCEPT_WEAK;

  if (!(rvr -> automaton) || auto_minimize(rvr -> automaton) < 0)
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(rvr);
}

/**  rva *rva_interleave_rn(rv, nl, nr)  :  This function
                     takes as arguments a RVA *rv representing a set
                     S of dimension n as well as two integers nl and
		     nr.  Only one of these two integers may be
		     different from zero, and this nonzero integer (if
		     any) must divide n.  If nl != 0, then the
		     function computes a RVA representing the set of
		     all the vectors of dimension n(nl+1)/nl of the
		     form (x[1], x[2], ..., x[nl], y[1], x[nl+1],
		     x[nl+2], ..., x[2*nl], y[2], ..., x[n-nl+1],
		     x[n-nl+2], ..., x[n], y[n/nl]), where (x[1], ...,
		     x[n]) is a vector in S, and (y[1], ..., y[n/nl])
		     is arbitrary. If nr != 0, then the function
		     computes a RVA representing the set of all the
		     vectors of dimension n(nr+1)/nr of the form
		     (y[1], x[1], x[2], ..., x[nr], y[2], x[nr+1],
		     x[nr+2], ..., x[2*nr], ..., y[n/nr], x[n-nr+1],
		     x[n-nr+2], ..., x[n]), where (x[1], ..., x[n]) is
		     a vector in S, and (y[1], ..., y[n/nr]) is
		     arbitrary.

		     This function does not modify *rv, and returns
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad parameters.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented
                                               for non-serial RVAs.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_interleave_rn(rv, nl, nr)
  rva   *rv;
  uint4  nl, nr;
{
  register rva *rvr, *rvz;

  diag__enter("rva_interleave_rn", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv -> dim &&
      auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv -> dim &&
      auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK &&
      (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_BUCHI ||
       rv -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if ((nl && nr) || (nl && (rv -> dim % nl)) ||
      (nr && (rv -> dim % nr))) 
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(rv -> dim))
    {
      rvr = rva_create_rn(rv -> base, nl + nr);
      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  if (!(rv -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (!nl && !nr)
    {
      rvr = rva_copy(rv);
      if (rvr)
	diag__return(rvr);
      diag__fail(lash_errno, NULL);
    }

  rvz = rva_create_rn(rv -> base, 1);

  if (!rvz)
    diag__fail(lash_errno, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    {
      rva_free(rvz);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rvr -> dim  = ((rv -> dim) / (nl + nr)) * (nl + nr + 1);
  rvr -> base = rv -> base;
  rvr -> properties = rv -> properties;
  if (!(rv -> properties & RVA_PROP_RESTRICTED))
    rvr -> properties = rvr -> properties & ~RVA_PROP_RESTRICTED;

  rvr -> automaton = nl ? 
    auto_seq_product_separ(rv -> automaton, rvz -> automaton,
			   nl, 1, &rv -> base) :
    auto_seq_product_separ(rvz -> automaton, rv -> automaton, 
			   1, nr, &rv -> base);
  
  rva_free(rvz);

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (rv -> properties & RVA_PROP_RESTRICTED)
    {
      auto_accept_type(rvr -> automaton) = AUTO_ACCEPT_WEAK;

      if (auto_minimize(rvr -> automaton) < 0)
	{
	  resr__free_object(rvr, rva);
	  diag__fail(lash_errno, NULL);
	}
    }

  diag__return(rvr);
}

/**  rva *rva_interleave(rv1, rv2, n1, n2)  :  This function
                     takes as arguments two RVAs *rv1 and *rv2 of
		     respective dimensions d1 and d2, respectively
		     representing the sets S1 and S2, as well as two
		     integers n1 and n2. These integers must be such
		     that n1 divides d1, n2 divides d2 and d1/n1 =
		     d2/n2. The function computes a RVA representing
		     the set of all the vectors of dimension d1+d2 of
		     the form (x[1], ..., x[n1], y[1], ..., y[n2],
		     x[n1+1], ..., x[2*n2], y[n2+1], ..., y[2*n2],
		     ..., x[d1-n1+1], ..., x[d1], y[d2-n2+1], ...,
		     y[d2]), where (x[1], ..., x[d1]) is a vector in
		     S1, and (y[1], ..., y[d2]) is a vector in S2. In
		     other words, this function interleaves the vector
		     components of *rv1 and *rv2, taking n1 components
		     from *rv1, then n2 components from *rv2, then n1
		     components from *rv1,...

		     This function does not modify *rv1 nor *rv2, and
		     returns (in the case of success) a pointer to a
		     newly allocated RVA. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_BAD_VALUE  : Bad parameters.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented
                                               for non-serial RVAs.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_interleave(rv1, rv2, n1, n2)
  rva   *rv1, *rv2;
  uint4  n1, n2;
{
  register rva *rvr;

  diag__enter("rva_interleave", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv1 || !rv2)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_word_type(rv1 -> automaton) != AUTO_WORDS_INFINITE) ||
      (rv2 -> dim &&
       auto_word_type(rv2 -> automaton) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if ((rv1 -> dim &&
       auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv1 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv1 -> properties & RVA_PROP_RESTRICTED)) ||
      (rv2 -> dim &&
       auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_WEAK &&
       (auto_accept_type(rv2 -> automaton) != AUTO_ACCEPT_BUCHI ||
	rv2 -> properties & RVA_PROP_RESTRICTED)))
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (!n1 || !n2 || rv1 -> dim % n1 || rv2 -> dim % n2 ||
      (rv1 -> dim) * n2 != (rv2 -> dim) * n1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(rv1 -> dim && rv2 -> dim))
    {
      rvr = rva_copy(rv1 -> dim ? rv1 : rv2);
      if (!rvr)
	diag__fail(lash_errno, NULL);
      diag__return(rvr);
    }
      
  if (rv1 -> base != rv2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (!(rv1 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (!(rv2 -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv1 -> dim + rv2 -> dim;
  rvr -> base = rv1 -> base;
  rvr -> properties = rv2 -> properties;
  if (!(rv1 -> properties & RVA_PROP_RESTRICTED))
    rvr -> properties = rvr -> properties & ~RVA_PROP_RESTRICTED;

  rvr -> automaton = 
    auto_seq_product_separ(rv1 -> automaton, rv2 -> automaton,
			   n1, n2, &rvr -> base);
  
  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (rvr -> properties & RVA_PROP_RESTRICTED)
    {
      auto_accept_type(rvr -> automaton) = AUTO_ACCEPT_WEAK;

      if (auto_minimize(rvr -> automaton) < 0)
	{
	  resr__free_object(rvr, rva);
	  diag__fail(lash_errno, NULL);
	}
    }

  diag__return(rvr);
}

/****  End of rva-operations.c  ****/
