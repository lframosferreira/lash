/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-sets.c  :  Construction of NDDs representing            **/
/**                   elementary sets.                             **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    10/02/98  :  Minor corrections. (BB)                        **/
/**    02/22/99  :  Empty NDD. (BB)                                **/
/**    08/16/01  :  Construction of 0-dimensional NDDs. (BB)       **/
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

/**  ndd *ndd_create_empty(r, n, msdf)  :  Returns a pointer to a new 
                     serial NDD accepting the empty set. The
		     numeration base is r, and the dimension of the
		     set is n. The vectors are read most significant
		     digit first if msdf is different from zero, and
		     least significant digit first otherwise. In the
		     case of an error, this function sets lash_errno
		     and returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_empty(r, n, msdf)
  uint1  r;
  uint4  n;
  int    msdf;
{
  register ndd       *nd;
  register automaton *a; 

  diag__enter("ndd_create_empty", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  nd -> automaton  = a;
  nd -> dim        = n;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL | (msdf ? NDD_PROP_MSDF : 0);

  diag__return(nd);
}

/**  ndd *ndd_create_eq2_lsdf(r)  :  Returns a pointer to a new NDD
                     accepting the set { (x, x) | x \in Z } and
                     operating in base r (with r > 1), serially and
                     least significant digit first. In the case of an
                     error, this function sets lash_errno and returns
                     a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_eq2_lsdf(r)
  uint1  r;
{
  register uint4      i, *st_to_num;
           uint1      m;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_eq2_lsdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  st_to_num = resr__new_objects(uint4, r + 2);
  if (!st_to_num)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < r + 2; i++)
    if (auto_add_new_state(a, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 2);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  for (m = 1; m < r - 1; m++)
    if (auto_add_new_transition(a, st_to_num[0], 
            st_to_num[m + 1], 1, &m) < 0 ||
        auto_add_new_transition(a, st_to_num[m + 1],
            st_to_num[0], 1, &m) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 2);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  for (m = 0; m < r; m += r - 1)
    if (auto_add_new_transition(a, st_to_num[0], 
            st_to_num[m + 1], 1, &m) < 0 ||
        auto_add_new_transition(a, st_to_num[m + 1],
            st_to_num[r + 1], 1, &m) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 2);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  for (m = 0; m < r; m++)
    if (auto_add_new_transition(a, st_to_num[r + 1], 
            st_to_num[m + 1], 1, &m) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 2);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }
  auto_mark_accepting_state(a, st_to_num[r + 1]);

  if (auto_add_new_i_state(a, st_to_num[0]) < 0)
    {
      resr__free_objects(st_to_num, uint4, r + 2);
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  resr__free_objects(st_to_num, uint4, r + 2);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_MINIMAL);

  nd -> automaton  = a;
  nd -> dim        = 2;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL;

  diag__return(nd);
}

/**  ndd *ndd_create_eq2_msdf(r)  :  Returns a pointer to a new NDD
                     accepting the set { (x, x) | x \in Z } and
                     operating in base r (with r > 1), serially and
                     most significant digit first. In the case of an
                     error, this function sets lash_errno and returns
                     a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_eq2_msdf(r)
  uint1  r;
{
  register uint4      i, *st_to_num;
           uint1      m;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_eq2_msdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  st_to_num = resr__new_objects(uint4, r + 4);
  if (!st_to_num)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < r + 4; i++)
    if (auto_add_new_state(a, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 4);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  for (m = 0; m < r; m += r - 1)
    {
      i = m ? 2 : 1;
      if (auto_add_new_transition(a, st_to_num[0], 
            st_to_num[i], 1, &m) < 0 ||
          auto_add_new_transition(a, st_to_num[i],
            st_to_num[3], 1, &m) < 0)
	{
	  resr__free_objects(st_to_num, uint4, r + 4);
	  auto_free(a);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
    }

  for (m = 0; m < r; m++)
    if (auto_add_new_transition(a, st_to_num[3], 
            st_to_num[m + 4], 1, &m) < 0 ||
        auto_add_new_transition(a, st_to_num[m + 4],
            st_to_num[3], 1, &m) < 0)
      {
	resr__free_objects(st_to_num, uint4, r + 4);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  auto_mark_accepting_state(a, st_to_num[3]);

  if (auto_add_new_i_state(a, st_to_num[0]) < 0)
    {
      resr__free_objects(st_to_num, uint4, r + 4);
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  resr__free_objects(st_to_num, uint4, r + 4);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_MINIMAL);

  nd -> automaton  = a;
  nd -> dim        = 2;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL | NDD_PROP_MSDF;

  diag__return(nd);
}

/**  ndd *ndd_create_zn_lsdf(r, n)  :  Returns a pointer to a new NDD
                     accepting the set Z^n and operating in base r
                     (with r > 1 and n >= 0), serially and least
                     significant digit first. In the case of an error,
                     this function sets lash_errno and returns a NULL
                     pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid value.      **/

ndd *ndd_create_zn_lsdf(r, n)
  uint1  r;
  uint4  n;
{
  register uint4      i, *st_to_num, ns;
           uint1      m;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_zn_lsdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (r == 2)
    ns = n + 1;
  else
    ns = n ? (2 * n) : 1;

  st_to_num = resr__new_objects(uint4, ns);
  if (!st_to_num)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
 
  for (i = 0; i < ns; i++)
    if (auto_add_new_state(a, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, ns);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }
 
  auto_mark_accepting_state(a, st_to_num[n]);

  if (auto_add_new_i_state(a, st_to_num[0]) < 0)
    {
      resr__free_objects(st_to_num, uint4, ns);
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (n)
    {
      i  = (n == 1) ? 0 : n + 1;
      
      for (m = 1; m < r - 1; m++)
	if (auto_add_new_transition(a, st_to_num[0],
				    st_to_num[i], 1, &m) < 0 ||
	    auto_add_new_transition(a, st_to_num[n],
				    st_to_num[i], 1, &m) < 0)
	  {
	    resr__free_objects(st_to_num, uint4, ns);
	    auto_free(a);
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	  }
      
      for (m = 0; m < r; m += r - 1)
	if (auto_add_new_transition(a, st_to_num[0],
				    st_to_num[1], 1, &m) < 0 ||
	    auto_add_new_transition(a, st_to_num[n],
				    st_to_num[1], 1, &m) < 0)
	  {
	    resr__free_objects(st_to_num, uint4, ns);
	    auto_free(a);
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	  }
      
      for (i = 1; i < n; i++)
	for (m = 0; m < r; m++)
	  if (((r > 2) && auto_add_new_transition(a,
	        st_to_num[n + i], st_to_num[(i == n - 1) ? 0 :
	        (n + i + 1)], 1, &m) < 0) ||
	      ((!m || m == r - 1) && auto_add_new_transition(a,
	         st_to_num[i], st_to_num[i + 1], 1, &m) < 0) ||
	      (m && m < r - 1 && auto_add_new_transition(a,
		 st_to_num[i], st_to_num[(i == n - 1) ? 0 :
		 (n + i + 1)], 1, &m) < 0))
	    {
	      resr__free_objects(st_to_num, uint4, ns);
	      auto_free(a);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
    }

  resr__free_objects(st_to_num, uint4, ns);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_MINIMAL);

  nd -> automaton  = a;
  nd -> dim        = n;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL;

  diag__return(nd);
}

/**  ndd *ndd_create_zn_msdf(r, n)  :  Returns a pointer to a new NDD
                     accepting the set Z^n and operating in base r
                     (with r > 1 and n >= 0), serially and most
                     significant digit first. In the case of an error,
                     this function sets lash_errno and returns a NULL
                     pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid value.      **/

ndd *ndd_create_zn_msdf(r, n)
  uint1  r;
  uint4  n;
{
  register uint4      i, *st_to_num, ns;
           uint1      m;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_zn_msdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1 )
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a = auto_new_empty(1);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ns = (r == 2) ? (n + 1) : (n ? (2 * n) : 1);

  st_to_num = resr__new_objects(uint4, ns);
  if (!st_to_num)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < ns; i++)
    if (auto_add_new_state(a, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, ns);
	auto_free(a);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  for (i = 0; i < n; i++)
    {
      for (m = 0; m < r; m += r - 1)
	if (auto_add_new_transition(a, st_to_num[i],
                st_to_num[i + 1], 1, &m) < 0)
	  {
	    resr__free_objects(st_to_num, uint4, ns);
	    auto_free(a);
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	  }

      if (r > 2)
	for (m = 0; m < r; m++)
	  if (auto_add_new_transition(a, st_to_num[n + i],
              st_to_num[n + ((i + 1) % n)], 1, &m) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, ns);
	      auto_free(a);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
    }

  if (r == 2 && n)
    for (m = 0; m < r; m += r - 1)
	  if (auto_add_new_transition(a, st_to_num[n],
              st_to_num[1], 1, &m) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, ns);
	      auto_free(a);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }

  auto_mark_accepting_state(a, st_to_num[n]); 

  if (auto_add_new_i_state(a, st_to_num[0]) < 0)
    {
      resr__free_objects(st_to_num, uint4, ns);
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  resr__free_objects(st_to_num, uint4, ns);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_MINIMAL);

  nd -> automaton  = a;
  nd -> dim        = n;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL | NDD_PROP_MSDF;

  diag__return(nd);
}

/****  End of ndd-sets.c  ****/
