/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-operations.c  :  Elementary operations over sets        **/
/**                 represented as NDDs.                           **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    10/14/98  :  New function. (BB)                             **/
/**    02/02/99  :  Interleave function. (BB)                      **/
/**    02/17/98  :  Minor corrections. (BB)                        **/
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

/**  ndd *ndd_product(nd1, nd2)  :  Computes an NDD representing the
                     Cartesian product of the sets represented by the
                     two NDDs *nd1 and *nd2. These two NDDs must use
                     the same base and be of the same type.
 
                     This function does not modify *nd1 or *nd2, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
			 LASH_ERR_ALPH_SIZE  : Alphabet of resulting
                                               NDD is too large.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_product(nd1, nd2)
  ndd *nd1, *nd2;
{
  register ndd *ndr;

  diag__enter("ndd_product", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  if (!(nd1 -> dim))
    {
      ndr = ndd_copy(nd2);
      if (ndr)
	diag__return(ndr);
      diag__fail(lash_errno, NULL);
    }

  if (!(nd2 -> dim))
    {
      ndr = ndd_copy(nd1);
      if (ndr)
	diag__return(ndr);
      diag__fail(lash_errno, NULL);
    }

  ndr = resr__new_object(ndd);
  if (!ndr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ndr -> dim  = nd1 -> dim + nd2 -> dim;
  ndr -> base = nd1 -> base;
  ndr -> properties = nd1 -> properties;
  ndr -> automaton  = (nd1 -> properties & NDD_PROP_SERIAL) ?
      auto_seq_product(nd1 -> automaton, nd2 -> automaton, 
          nd1 -> dim, nd2 -> dim) :
      auto_product(nd1 -> automaton, nd2 -> automaton);

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  ndd *ndd_difference(nd1, nd2)  :  Computes an NDD representing
                     the difference of the sets represented by
                     the two NDDs *nd1 and *nd2. These two NDDs must
                     use the same base and be of the same type and
                     dimension.
 
                     This function does not modify *nd1 or *nd2, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_difference(nd1, nd2)
  ndd *nd1, *nd2;
{
  register ndd *ndr;

  diag__enter("ndd_difference", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  ndr = resr__new_object(ndd);
  if (!ndr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ndr -> dim  = nd1 -> dim;
  ndr -> base = nd1 -> base;
  ndr -> properties = nd1 -> properties; 
  ndr -> automaton  = auto_difference(nd1 -> automaton,
      nd2 -> automaton);

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  ndd *ndd_union(nd1, nd2)  :  Computes an NDD representing the
                     union of the sets represented by the two NDDs
                     *nd1 and *nd2. These two NDDs must use the same
                     base and be of the same type and dimension.

                     This function does not modify *nd1 or *nd2, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_union(nd1, nd2)
  ndd *nd1, *nd2;
{
  register ndd *ndr;

  diag__enter("ndd_union", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  ndr = resr__new_object(ndd);
  if (!ndr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ndr -> dim  = nd1 -> dim;
  ndr -> base = nd1 -> base;
  ndr -> properties = nd1 -> properties;
  ndr -> automaton  = auto_union(nd1 -> automaton, nd2 -> automaton);

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  int  ndd_merge(nd1, nd2)  :  Computes an NDD representing the 
                     union of the sets represented by the two NDDs
                     *nd1 and *nd2, and replaces *nd1 by the result of
                     the operation. The two NDDs *nd1 and *nd2 must
                     use the same base and be of the same type and 
                     dimension.

                     This function does not modify *nd2, and
                     returns 0 in the case of success, -1 in the
                     case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

int  ndd_merge(nd1, nd2)
  ndd *nd1, *nd2;
{
  register int  r;

  diag__enter("ndd_merge", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  r = auto_merge(nd1 -> automaton, nd2 -> automaton);
  if (r < 0 || auto_minimize(nd1 -> automaton) < 0)
    diag__fail(lash_errno, -1);

  diag__return(r);
}

/**  ndd *ndd_intersection(nd1, nd2)  :  Computes an NDD representing
                     the intersection of the sets represented by the
                     two NDDs *nd1 and *nd2. These two NDDs must use
                     the same base and be of the same type and
                     dimension.

                     This function does not modify *nd1 or *nd2, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BASE       : Base mismatch.
			 LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_intersection(nd1, nd2)
  ndd *nd1, *nd2;
{
  register ndd *ndr;

  diag__enter("ndd_intersection", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd1 || !nd2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd1 -> base != nd2 -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (nd1 -> dim != nd2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (nd1 -> properties != nd2 -> properties)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  ndr = resr__new_object(ndd);
  if (!ndr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ndr -> dim  = nd1 -> dim;
  ndr -> base = nd1 -> base;
  ndr -> properties = nd1 -> properties;
  ndr -> automaton  = auto_intersection(nd1 -> automaton,
      nd2 -> automaton);

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  ndd *ndd_base_reduce(nd)  :  Computes an NDD representing the
                     set { x / r^k | x in S and k in N }, where S is
                     the subset of Z represented by the NDD *nd
                     (supposed to be of dimension 1 and to use the
                     numeration base r).

		     Warning : The use of this function is strongly
                     discouraged, since it might no longer appear
		     in future releases of the package.

                     This function does not modify *nd, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_base_reduce(nd)
  ndd *nd;
{
  register ndd       *ndr;
  register automaton *a;
  static   uint1      zero[1] = { 0 };
  static   uint4      s;

  diag__enter("ndd_base_reduce", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd -> dim != 1)
     diag__fail(LASH_ERR_DIMENSION, NULL);

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL); 

  if (auto_add_new_state(a, &s) < 0 ||
      auto_add_new_transition(a, s, s, 1, zero) < 0 ||
      auto_add_new_i_state(a, s) < 0)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_mark_accepting_state(a, s);

  ndr = resr__new_object(ndd);
  if (!ndr)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  ndr -> dim  = 1;
  ndr -> base = nd -> base;
  ndr -> properties = nd -> properties;
  ndr -> automaton  = (nd -> properties & NDD_PROP_MSDF) ?
      auto_right_quotient(nd -> automaton, a) :
      auto_quotient(nd -> automaton, a);

  auto_free(a);

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  ndd *ndd_base_expand(nd)  :  Computes an NDD representing the
                     set { r^k x | x in S and k in N }, where S is
                     the subset of Z represented by the NDD *nd
                     (supposed to be of dimension 1 and to use the
                     numeration base r).

		     Warning : The use of this function is strongly
                     discouraged, since it might no longer appear
		     in future releases of the package.

                     This function does not modify *nd, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_base_expand(nd)
  ndd *nd;
{
  register ndd       *ndr;
  register automaton *a;
  static   uint1      zero[1] = { 0 };
  static   uint4      s;

  diag__enter("ndd_base_expand", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd -> dim != 1)
     diag__fail(LASH_ERR_DIMENSION, NULL);

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL); 

  if (auto_add_new_state(a, &s) < 0 ||
      auto_add_new_transition(a, s, s, 1, zero) < 0 ||
      auto_add_new_i_state(a, s) < 0)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_mark_accepting_state(a, s);

  ndr = resr__new_object(ndd);
  if (!ndr)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (nd -> properties & NDD_PROP_MSDF)
    {
      ndr -> automaton = auto_copy(nd -> automaton);
      if (!(ndr -> automaton))
	{
	  auto_free(a);
	  resr__free_object(ndr, ndd);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      if (auto_concatenate(ndr -> automaton, a) < 0)
	{
	  auto_free(a);
	  auto_free(ndr -> automaton);
	  resr__free_object(ndr, ndd);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      auto_free(a);
    }
  else
    {
      if (auto_concatenate(a, nd -> automaton) < 0)
	{
	  auto_free(a);
	  resr__free_object(ndr, ndd);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
     ndr -> automaton = a;
    }

  ndr -> dim  = 1;
  ndr -> base = nd -> base;
  ndr -> properties = nd -> properties;

  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  diag__return(ndr);
}

/**  ndd *ndd_interleave_z(nd, nl, nr)  :  This function takes as
                     arguments an NDD *nd representing a set S of
		     dimension n as well as two integers nl and nr.
		     Only one of these two integers may be different
		     from zero, and this nonzero integer (if any) must
		     divide n.  If nl != 0, then the function computes
		     an NDD representing the set of all the vectors of
		     dimension n(nl+1)/nl of the form (x[1], x[2],
		     ..., x[nl], y[1], x[nl+1], x[nl+2], ..., x[2*nl],
		     y[2], ..., x[n-nl+1], x[n-nl+2], ...  x[n],
		     y[n/nl]), where (x[1], x[2], ..., x[n]) is a 
                     vector in S, and (y[1], y[2], ..., y[n/nl]) is
		     arbitrary. If nr != 0, then the function computes
		     an NDD representing the set of all the vectors of
		     dimension n(nr+1)/nr of the form (y[1], x[1],
		     x[2], ..., x[nr], y[2], x[nr+1], x[nr+2], ...,
		     x[2*nr], ..., y[n/nr], x[n-nr+1], x[n-nr+2], ...
		     x[n]), where (x[1], x[2], ..., x[n]) is a vector
		     in S, and (y[1], y[2], ..., y[n/nr]) is 
                     arbitrary.

		     This function does not modify *nd, and returns
		     (in the case of success) a pointer to a newly
		     allocated NDD. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad parameters.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented
                                               for non-serial NDDs.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

ndd *ndd_interleave_z(nd, nl, nr)
  ndd   *nd;
  uint4  nl, nr;
{
  register ndd *ndr, *ndz;

  diag__enter("ndd_interleave_z", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if ((nl && nr) || (nl && (nd -> dim % nl)) ||
      (nr && (nd -> dim % nr))) 
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(nd -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (!nl && !nr)
    {
      ndr = ndd_copy(nd);
      if (ndr)
	diag__return(ndr);
      diag__fail(lash_errno, NULL);
    }

  if (!(nd -> dim))
    {
      ndr = (nd -> properties & NDD_PROP_MSDF) ? 
          ndd_create_zn_msdf(nd -> base, nl + nr) :
          ndd_create_zn_lsdf(nd -> base, nl + nr);
      if (ndr)
	diag__return(ndr);
      diag__fail(lash_errno, NULL);
    }

  ndz = (nd -> properties & NDD_PROP_MSDF) ? 
      ndd_create_zn_msdf(nd -> base, 1) :
      ndd_create_zn_lsdf(nd -> base, 1);
    
  if (!ndz)
    diag__fail(lash_errno, NULL);

  ndr = resr__new_object(ndd);
  if (!ndr)
    {
      ndd_free(ndz);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  ndr -> dim  = ((nd -> dim) / (nl + nr)) * (nl + nr + 1);
  ndr -> base = nd -> base;
  ndr -> properties = nd -> properties;
  ndr -> automaton = nl ? 
    auto_seq_product(nd -> automaton, ndz -> automaton, nl, 1) :
    auto_seq_product(ndz -> automaton, nd -> automaton, 1, nr);
  
  ndd_free(ndz);
  if (!(ndr -> automaton) || auto_minimize(ndr -> automaton) < 0)
    {
      resr__free_object(ndr, ndd);
      diag__fail(lash_errno, NULL);
    }

  diag__return(ndr);
}

/****  End of ndd-operations.c  ****/
