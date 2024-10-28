/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-iterations-utils.c  :  Utility functions used to        **/
/**                 compute the closure of a sequence of           **/
/**                 operations.                                    **/
/**                                                                **/
/**    02/15/00  :  Creation. (JMF)                                **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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

#include "lash-types.h"
#include "datastruct.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "queue-operations.h"
#include "qdd.h"
#include "qdd-iterations-utils.h"

/**  p_table *p_table__new_empty()  :  Creates a new empty pointer
                     table.  Returns a pointer to this table if
		     successful, and NULL in the case of insufficient
		     memory.                                       **/

p_table *p_table__new_empty()
{
  register p_table *pt;

  if (!(pt = resr__new_object(p_table)))
    return NULL;

  pt -> ptr = NULL;
  pt -> nb_p = ZERO_INT4;
  pt -> nb_entries = ZERO_INT4;

  return pt;
}

/**  int p_table__free(pt, fnct)  :  Frees the pointer table *pt.  If
                     fnct is not NULL, the function *fnct is applied 
                     to each entry before the table is freed.

		     fnct must return 0 if successful, or -1 (and set
		     lash_errno) in the case of an error.
		     If one (or more) call(s) to fnct produces -1,
		     then p_table__free returns -1.                **/

int p_table__free(pt, fnct)
     p_table *pt;
     int    (*fnct)(void *);
{
  int  retval = 0;

#if LASH_CHECK_LEVEL >= 2
  if (!pt)
    return -1;
#endif   /* >= 2 */

  if (fnct)
    {
      uint4  i;
     
      for(i = ZERO_INT4; i < p_table__nb_elements(pt); i++)
	if (fnct(p_table__element(pt, i)))
	  retval = -1;
    }

  resr__free_objects(pt -> ptr, void *, pt -> nb_entries);
  resr__free_object(pt, p_table);

  return(retval);
}

/**  int p_table__add(pt, p)  :  Adds the pointer p to the pointer
                     table *pt.
		     
		     Returns  -1 in the case of an insufficient
		     memory, 0 otherwise.                          **/

int p_table__add(pt, p)
     p_table *pt;
     void    *p;
{
  void **r;

#if LASH_CHECK_LEVEL >= 2
  if (!pt)
    return -1;
#endif   /* >= 2 */
  
  if (pt -> nb_p >= pt -> nb_entries)
    {
      if (!(r = resr__resize_objects(pt -> ptr, void *,
				     pt -> nb_entries + 
				         PTABLE_GROWTH_QUANTUM,
				     pt -> nb_entries)))
	return -1;
      
      pt -> nb_entries += PTABLE_GROWTH_QUANTUM;
      pt -> ptr = r;
    }

  (pt -> ptr)[pt -> nb_p++] = p;

  return 0;
}

/**  uint4 p_table__nb_elements(pt)  :  Returns the number of elements
                     in the table *pt.                             **/

uint4 p_table__nb_elements(pt)
     p_table *pt;
{
#if LASH_CHECK_LEVEL >= 2
  if (!pt)
    return ZERO_INT4;
#endif   /* >= 2 */

  return pt -> nb_p;
}

/**  void *p_table__element(pt, n)  :  Return the n-th element of the
                     table *pt (n = 0, 1,...).                     **/

void *p_table__element(pt, n)
     p_table *pt;
     uint4    n;
{
#if LASH_CHECK_LEVEL >= 2
  if (!pt || n >= p_table__nb_elements(pt))
    return NULL;
#endif   /* >= 2 */

  return (pt -> ptr)[n];
}

/**  int p_table__find(pt, p, cmp, pn)  :  Finds an entry *p in the
                     table *pt.  Returns 1 if the entry is found, 0
		     if it's not.

		     *cmp is a function returning 1 if its arguments
		     are equal, 0 if not, -1 if an error occurs.

		     If *p is found and pn != NULL, then *pn is set to
		     the number of the matching entry (pn = 0, 1,...).

		     Returns -1 if cmp reports an error.
		     
		     Possible error codes:

		         Depends of the function cmp.              **/

int p_table__find(pt, p, cmp, pn)
     p_table *pt;
     void    *p;
     int    (*cmp)(void *, void *);
     uint4   *pn;
{
  register uint4  i, nb_el;

#if LASH_CHECK_LEVEL >= 2
  if (!pt || !cmp)
    return 0;
#endif   /* >= 2 */

  nb_el = p_table__nb_elements(pt);

  for (i = ZERO_INT4; i < nb_el; i++)
    {
      switch(cmp(p, p_table__element(pt, i)))
	{
	case 0:
	  continue;

	case 1:
	  if (pn)
	    *pn = i;
	  return 1;

	case -1:
	default:
	  return -1;
	}
    }

  return 0;
}

/**  int repeated_union_seq(q, seq, sup, one)  :  Given a QDD *q and
                     a sequence *seq, computes the union of the
                     automata APPLY(*q, seq^i) with
		     i = 0, 1,..., sup-1 (APPLY computes the
		     effect of the sequence given as its second
		     argument to the QDD given as its firts argument).
		     
		     If one is not null, than *q is supposed to be
		     one-queued.

		     Return 0 if successful, -1 and sets lash_errno
		     in the case of an error.
		     
		     Possible error codes:
		     
		         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
			 LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_BAD_STATE  : Automaton contains
			                       reference to an
			                       invalid state.      **/

int repeated_union_seq(q, seq, sup, one)
     qdd               *q;
     queue_op_sequence *seq;
     uint4              sup;
{
  qdd            *q2;
  register uint4  i;
  int (*f)(qdd *, queue_op_sequence *);

#if LASH_CHECK_LEVEL >= 2
  if (!q || !seq)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */
  /*TODO: et qwe Charles ?  Cleanifier les one-queue QDD's*/

  if (sup == 0 || sup == 1)
    return 0;

  if (!(q2 = qdd_copy(q)))
    return -1;
  
  if (one)
    f = qdd_one_sequence;
  else
    f = qdd_apply;

  for (i = ZERO_INT4; i < sup-1; i++)
    {
      if (f(q2, seq))
	{
	  qdd_free(q2);
	  return -1;
	}
      
      if (!qdd_nb_i_states(q2))
	break;
	
      if (qdd_merge(q, q2))
	{
	  qdd_free(q2);
	  return -1;
	}
    }
  
  if (qdd_free(q2))
    return -1;

  return 0;
}

/****  End of qdd-iterations-utils.c  ****/
