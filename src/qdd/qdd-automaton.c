/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-automaton.c  :  Functions associated to the underlying  **/
/**                 automaton of QDDs.                             **/
/**                                                                **/
/**    01/19/00  :  Creation. (JMF)                                **/
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

#include "lash-auto.h"
#include "qdd.h"
#include "qdd-automaton.h"

/**  int qdd_remove_i_states(q)  :  Empties the set of initial states
                     of the QDD *q.                                **/

int qdd_remove_i_states(q)
     qdd *q;
{
  if (qdd_free_init_tuples(q))
    return -1;

  auto_remove_i_states(qdd_automaton(q));
  return 0;
}

/**  int qdd_add_new_i_state(q, p)  :  Adds a new initial state, 
                     specified by the index p, to the QDD given by
                     *q.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int qdd_add_new_i_state(q, p)
     qdd   *q;
     uint4  p;
{
  if (qdd_free_init_tuples(q))
    return -1;

  return auto_add_new_i_state(qdd_automaton(q), p);
}

/**  int qdd_mark_accepting_state(q, s)  :  Sets the first byte
                     of the accepting status of the state p of the
                     QDD *a to 1.

		     Return 0 if successful, -1 and sets lash_errno
		     in the case of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int qdd_mark_accepting_state(q, s)
     qdd *q;
     uint4 s;
{
  if (qdd_free_init_tuples(q))
    return -1;

  auto_mark_accepting_state(qdd_automaton(q), s);
  return 0;
}
     
/**  int qdd_unmark_accepting_state(q, s)  :  Sets the first byte
                     of the accepting status of the state p of the
                     QDD *q to 0.

		     Returns 0 if successful, -1 and sets lash_errno
		     in the case of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int qdd_unmark_accepting_state(q, s)
     qdd  *q;
     uint4 s;
{
  if (qdd_free_init_tuples(q))
    return -1;
  
  auto_unmark_accepting_state(qdd_automaton(q), s);
  return 0;
}

/**  int  qdd_add_new_transition(a, p, q, n, l)  :  Adds a new
                     transition to the QDD *q. The parameters
                     p and q specify the origin and the end of
                     the transition (in terms of state indices),
                     n is the number of symbols of the label, and
                     l is a pointer to an array of bytes 
                     containing the successive symbols of the
                     label (each symbol being represented by 
                     (a -> alphabet_nbytes) bytes in the array).

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int qdd_add_new_transition(q, s1, s2, n, l)
     qdd          *q;
     uint4         s1, s2, n;
     queue_symbol *l;
{
  qdd_reset_property(q, QDD_PROP_ONE_QUEUE);
  
  if (auto_add_new_transition(qdd_automaton(q), s1, s2, n,
			      (uint1 *) l) ||
      qdd_free_init_tuples(q))
    return -1;

  return 0;
}

/**  int qdd_apply_homomorphism(q, f, na2)  :  Applies the 
                     homomorphism *f to the transition labels of the
                     QDD *q. The function f takes six arguments n, ptr,
                     rn, rptr, na1, na2.
                     The first two (n and ptr) give the number
                     of symbols and a pointer to the label of a
                     transition. The next two (rn, rptr) are pointers
                     to locations in which the function *f must return
		     the new number of symbols and a pointer to the
                     new label (this label will be copied by
                     auto_apply_homomorphism).  The last arguments na1
                     and na2 give the number of bytes required for
                     storing one symbol of the alphabet of *q before
                     and after applying f. The argument na2 is
                     identical in value to the corresponding argument
                     of qdd_apply_homomorphism.  The function *f must
                     return 0 in the case of success, and -1 if the
                     operation is to be aborted.

                     The function qdd_apply_homomorphism returns 0 in
                     the case of success. In the case of an error, it
                     returns -1, sets lash_errno, and leaves in *a an
                     automaton that may not accept the same language
                     as the original one.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_BAD_VALUE  : Bad value.
                         LASH_ERR_ABORT      : Operation aborted.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int qdd_apply_homomorphism(q, f, na2)
     qdd   *q;
     int  (*f)(uint4, uint1 *, uint4 *, uint1 **, uint1, uint1);
     uint1  na2;
{
  qdd_reset_property(q, QDD_PROP_ONE_QUEUE);

  if (auto_apply_homomorphism(qdd_automaton(q), f, na2))
    return -1;

  return 0;
}

/** int qdd_minimize(q)  :  Minimize the QDD q.
   
                     Returns 0 in the case of success, -1 in the case
                     of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_PROP       : Automaton with wrong
                                               known properties.   **/

int qdd_minimize(q)
     qdd *q;
{ 
  if (auto_minimize(q -> automaton) || qdd_free_init_tuples(q))
    return -1;

  return 0;
}

/**  int  qdd_normalize(q)  :  Normalizes the qdd *q, i.e.,
                     transforms it into a qdd accepting
                     the same language, but whose transitions are
		     all of length less or equal to 1.

                     In case of success, the function returns 0.
                     In case of error, it returns -1 and leaves in
                     *q an automaton that accepts the same language
                     as the original one, but not necessarily
                     normal and possibly containing useless
                     components.
 
                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : Automaton contains
                                               reference to an
                                               invalid state.
                         LASH_ERR_PROP       : Automaton with wrong
                                               known properties.   **/

int  qdd_normalize(q)
     qdd *q;
{
  if (auto_normalize(q -> automaton) || qdd_free_init_tuples(q))
    return -1;
  
  return(0);
}

/**  int qdd_is_normal(q)  :  Returns 1 if the QDD *q is normal, or 0
                     if it is not.

		     The function returns -1 if an error occurs,
		     and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         LASH_ERR_PROP     : Automaton with wrong
                                             known properties.     **/

int  qdd_is_normal(q)
     qdd *q;
{
  switch(auto_is_normal(q -> automaton))
    {
    case 0:
      return(0);

    case 1:
      return(1);
      
    default:
      return(-1);
    }
}

/****  End of qdd-automaton.c  ****/
