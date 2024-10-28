/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-sets.c  :  Construction of RVAs representing            **/
/**                   elementary sets.                             **/
/**                                                                **/
/**    03/24/01  :  Creation. (SJ)                                 **/
/**    03/26/01  :  Continued. (SJ)                                **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/09/01  :  Minor modifications. (SJ)                      **/
/**    08/13/01  :  New function 'rva_create_eq2'. (SJ)            **/
/**    08/14/01  :  Minor adaptation. (BB)                         **/
/**    08/17/01  :  Management of RVAs with zero component. (SJ)   **/
/**    08/20/01  :  Continued (SJ).                                **/
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

/****  Prototypes of private functions.                          ****/

static int  generate_integer_part(automaton *, uint1, uint4, uint4 *);

/****  Private functions.                                        ****/

/**  int  generate_integer_part(a, r, n, sync)  :  Adds to the
                     automaton *a the states and the transitions
		     needed to accept the integer parts of all the
		     vectors of dimension n in base r. Actually, if
		     *a was empty, this function modifies *a such
		     that it shares the same structure as the NDD
		     representing Z^n in base r. At the end of the
		     execution, *sync contains the state index of
		     the accepting state of that NDD.

		     This function modifies *a, and returns 0 in the
		     case of success). If an error occurs, it returns
		     -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  generate_integer_part(a, r, n, sync)
     automaton *a;
     uint1      r;
     uint4      n;
     uint4     *sync;
{
  register uint4      i, *st_to_num, ns;
           uint1      m;

  diag__enter("generate_integer_part", -1);

  ns = (r == 2) ? (n + 1) : (2 * n);

  st_to_num = resr__new_objects(uint4, ns);
  if (!st_to_num)
    diag__fail(LASH_ERR_NO_MEM, -1);

  for (i = 0; i < ns; i++)
    if (auto_add_new_state(a, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, ns);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  for (i = 0; i < n; i++)
    {
      for (m = 0; m < r; m += r - 1)
	if (auto_add_new_transition(a, st_to_num[i],
                st_to_num[i + 1], 1, &m) < 0)
	  {
	    resr__free_objects(st_to_num, uint4, ns);
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  }

      if (r > 2)
	for (m = 0; m < r; m++)
	  if (auto_add_new_transition(a, st_to_num[n + i],
              st_to_num[n + ((i + 1) % n)], 1, &m) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, ns);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
    }

  if (r == 2)
    for (m = 0; m < r; m += r - 1)
	  if (auto_add_new_transition(a, st_to_num[n],
              st_to_num[1], 1, &m) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, ns);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }

  *sync = st_to_num[n];

  if (auto_add_new_i_state(a, st_to_num[0]) < 0)
    {
      resr__free_objects(st_to_num, uint4, ns);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  resr__free_objects(st_to_num, uint4, ns);
  diag__return(0);
}


/****  Public visible functions.                                 ****/

/**  rva *rva_create_empty(r, n)  :  Returns a pointer to a new 
                     serial RVA accepting the empty set. The
		     numeration base is r, and the dimension of the
		     set is n. In the case of an error, this function
		     sets lash_errno and returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

rva *rva_create_empty(r, n)
  uint1  r;
  uint4  n;
{
  register rva       *rv;
  register automaton *a; 

  diag__enter("rva_create_empty", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!n)
    a = NULL;
  else
    {
      a = auto_new_empty(1);
      if (!a)
	diag__fail(lash_errno, NULL);

      auto_word_type(a) = AUTO_WORDS_INFINITE;
      auto_accept_type(a) = AUTO_ACCEPT_WEAK;
      auto_set_property(a, AUTO_PROP_DETERM);
      auto_set_property(a, AUTO_PROP_MINIMAL);
      auto_set_property(a, AUTO_PROP_WEAK_NORMAL);
    }

  rv = resr__new_object(rva);
  if (!rv)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rv -> automaton  = a;
  rv -> dim        = n;
  rv -> base       = r;
  rv -> universal  = 0;
  rv -> properties = (n) ? RVA_PROP_SERIAL | RVA_PROP_RESTRICTED :
    RVA_PROP_NOTHING;

  diag__return(rv);
}


/**  rva *rva_create_rn(r, n)  :  Returns a pointer to a new RVA
                     accepting the set R^n and operating in base r
                     (with r > 1 and n >= 0) and serially. In the case
		     of an error, this function sets lash_errno and
		     returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid value.      **/

rva *rva_create_rn(r, n)
  uint1  r;
  uint4  n;
{
  register rva       *rv;
  register automaton *a;
           uint4      sync, cur;
	   uint1      j;

  diag__enter("rva_create_rn", NULL);

  if (r <= 1 || r == 0xFF)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!n)
    a = NULL;
  else
    {
      a = auto_new_empty(1);
      if (!a)
	diag__fail(lash_errno, NULL);

      auto_word_type(a) = AUTO_WORDS_INFINITE;
      auto_accept_type(a) = AUTO_ACCEPT_WEAK;

      if (generate_integer_part(a, r, n, &sync) < 0 ||
	  auto_add_new_state(a, &cur) < 0 ||
	  auto_add_new_transition(a, sync, cur, 1, (char *) &r) < 0)
	{
	  auto_free(a);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      for (j=0 ; j<r ; j++)
	if (auto_add_new_transition(a, cur, cur, 1, (char *) &j) < 0)
	  {
	    auto_free(a);
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	  }

      auto_mark_accepting_state(a, cur);

      auto_word_type(a) = AUTO_WORDS_INFINITE;
      auto_accept_type(a) = AUTO_ACCEPT_WEAK;
      auto_set_property(a, AUTO_PROP_DETERM);
      auto_set_property(a, AUTO_PROP_MINIMAL);
      auto_set_property(a, AUTO_PROP_WEAK_NORMAL);
    }

  rv = resr__new_object(rva);
  if (!rv)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  rv -> automaton  = a;
  rv -> dim        = n;
  rv -> base       = r;
  rv -> universal  = (n) ? 0 : 1;
  rv -> properties = (n) ? RVA_PROP_SERIAL | RVA_PROP_RESTRICTED :
    RVA_PROP_NOTHING;

  diag__return(rv);
}


/**  rva *rva_create_zn(r, n)  :  Returns a pointer to a new RVA
                     accepting the set Z^n and operating in base r
                     (with r > 1 and n > 0) and serially. In the case
		     of an error, this function sets lash_errno and
		     returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid value.      **/

rva *rva_create_zn(r, n)
  uint1  r;
  uint4  n;
{
  register rva       *rv;
  register automaton *a;
  register uint4     *st, pos;
  register uint1     *label;
           uint4      cur, next;

  diag__enter("rva_create_zn", NULL);

  if (r <= 1 || r == 0xFF || !n)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  a = auto_new_empty(1);
  if (!a)
    diag__fail(lash_errno, NULL);

  st = resr__new_objects(uint4, n+1);
  if (!st)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (generate_integer_part(a, r, n, &cur) < 0 ||
      auto_add_new_state(a, st) < 0 ||
      auto_add_new_transition(a, cur, st[0], 1, (char *) &r) < 0)
    {
      auto_free(a);
      resr__free_objects(st, uint4, n);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  label = resr__new_objects(uint1, n);
  if (!label)
    {
      auto_free(a);
      resr__free_objects(st, uint4, n+1);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (pos=0 ; pos<n-1 ; )
    {
      label[pos] = r-1;
      if (auto_add_new_state(a, st+pos+1) < 0 ||
	  auto_add_new_transition(a, st[pos], st[pos+1], 1, 
				  label + pos) < 0)
	{
	  auto_free(a);
	  resr__free_objects(st, uint4, n+1);
	  resr__free_objects(label, uint1, n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      auto_mark_accepting_state(a, st[++pos]);
    }

  auto_mark_accepting_state(a,st[0]);

  label[n-1] = 0;
  for ( pos = n ; pos ; )
    {
      pos --;
      label[pos] = label[pos] ? 0 : r-1;

      while (pos < n)
	{
	  if (auto_add_new_state(a, st+pos+1) < 0 ||
	      auto_add_new_transition(a,st[pos], st[pos+1], 1, 
				      label + pos) < 0)
	    {
	      auto_free(a);
	      resr__free_objects(st, uint4, n+1);
	      resr__free_objects(label, uint1, n);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	  auto_mark_accepting_state(a, st[++pos]);
	}

      cur = st[pos];
      for (pos = 0 ; pos<n-1 ; pos ++)
	{
	  if (auto_add_new_state(a, &next) < 0 ||
	      auto_add_new_transition(a, cur, next, 1, 
				      label + pos) < 0)
	    {
	      auto_free(a);
	      resr__free_objects(st, uint4, n+1);
	      resr__free_objects(label, uint1, n);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	  auto_mark_accepting_state(a, next);
	  cur = next;
	}

      if (auto_add_new_transition(a, cur, st[n], 1, 
				  label + pos) < 0)
	{
	  auto_free(a);
	  resr__free_objects(st, uint4, n+1);
	  resr__free_objects(label, uint1, n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      if (!label[n-1])
	{
	  label[n-1] = r-1;
	  pos = n-1;
	  while (pos && !label[pos-1])
	    label[--pos] = r-1;
	}
      else
	pos = n;
    }

  resr__free_objects(st, uint4, n+1);
  resr__free_objects(label, uint1, n);

  rv = resr__new_object(rva);
  if (!rv)
    {
      auto_free(a);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_word_type(a) = AUTO_WORDS_INFINITE;
  auto_accept_type(a) = AUTO_ACCEPT_WEAK;
  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_WEAK_NORMAL);

  auto_minimize(a);

  rv -> automaton  = a;
  rv -> dim        = n;
  rv -> base       = r;
  rv -> properties = RVA_PROP_SERIAL | RVA_PROP_RESTRICTED;

  diag__return(rv);
}


/**  rva *rva_create_rzn(r, n, integer)  :  This function encapsulates
                     the calls needed to build a RVA operating
		     serially in base r and accepting a subset of R^n
		     (with r > 1 and n > 0) of the form (x[1],
		     x[2], ..., x[n]) such that the variable x[i]
		     belongs to the set Z of integers if and only
		     if integer[i - 1] is nonzero.

		     In the case of an error, this function sets
		     lash_errno and returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid value.      **/

rva *rva_create_rzn(r, n, integer)
     uint1       r;
     uint4       n;
     int         *integer;
{
  register rva    *rv, *rv1, *rv2;
  register uint4   i, j;
  register uint1   cur_int;
  
  diag__enter("rva_create_rzn", NULL);

  if (r <= 1 || r == 0xFF || !n || !integer)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!n)
    {
      rv = rva_create_rn(r, 0);
      if (rv)
	diag__return(rv);
      diag__fail(lash_errno, NULL);
    }

  rv = NULL;
  for (i = 0; i < n; )
    {
      cur_int = integer[i] ? 1 : 0;
      for (j = i; j < n && (cur_int ? integer[j] : !integer[j]);
	   j++);

      rv1 = (cur_int ? rva_create_zn : rva_create_rn) (r, j - i);
      if (!rv1)
	{
	  if (rv)
	    rva_free(rv);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      if (rv)
	{
	  rv2 = rva_product(rv, rv1);
	  rva_free(rv);
	  rva_free(rv1);
	  rv = rv2;
	  if (!rv)
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      else
	rv = rv1;

      i = j;
    }

  diag__return(rv);
}

/**  rva *rva_create_eq2(r)  :  Returns a pointer to a new RVA
                     accepting the set { (x, x) | x \in R } and
                     operating serially in base r (with r > 1). In
		     the case of an error, this function sets 
		     lash_errno and returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

rva *rva_create_eq2(r)
  uint1  r;
{
  register rva   *rv;
           sint4  a[2];

  diag__enter("rva_interleave_z", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1 || r == 0xFF)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  a[0] = 1;
  a[1] = -1;

  rv = rva_create_equation(r, 2, a, 0);
  if (!rv)
    diag__fail(lash_errno, NULL);

  diag__return(rv);
}

/****  End of rva-sets.c  ****/
