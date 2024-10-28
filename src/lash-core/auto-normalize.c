/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-normalize.c  :  Normalization of finite-state          **/
/**                     automata.                                  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        27/02/01  :  Weak det. automata normal form. (SJ)       **/
/**        03/01/01  :  Improvements. (SJ)                         **/
/**        03/03/01  :  Bug fixed in the original algorithm. (SJ)  **/
/**        03/17/01  :  Integration into the LASH package. (SJ)    **/
/**        03/29/01  :  Minor correction. (SJ)                     **/
/**        09/26/01  :  Bug fixed. (SJ)                            **/
/**        10/23/01  :  Bug fixed. (SJ)                            **/
/**        07/08/02  :  Reorganization. (BB)                       **/
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
#include <string.h>
#include <stdlib.h>
#include "auto.h"
#include "auto-normalize.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "graph-scc.h"

/****  Private types.                                            ****/

/** typedef nf_info  :  Type used to give the coloring algorithm
                     for weak automata all the necessary data for
		     coloring an automaton. *ht is a hash table
		     that associates a color to all the already
		     treated s.c.c. of the automaton. k is
		     an integer that gives the greatest color
		     number for a s.c.c. of the given automaton. See
		     [Lod01] for more informations.                **/

typedef struct {
  hash_table  *ht;
  uint4        k;
} nf_info;

/****  Definitions.                                              ****/

/**  INFINITY  :  Symbolic representation of an infinite number
                     (or close enough).                            **/

#define INFINITY   ((uint4) -1)


/****  Global variables.                                         ****/

/**  auto_weak_norm_hsize  :  Size of the hash table used by the     
                     normalization algorithm for weak automata.    **/

static uint4  auto_weak_norm_hsize = AUTO_DEFAULT_WEAK_NORM_HSIZE;

/**  auto_weak_norm_ncolls  :  Number of collisions observed in the   
                     hash table used by the normalization 
                     algorithm for weak automata.                  **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_weak_norm_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_weak_norm_nins  :  Number of insertions performed in the   
                     hash table used by the normalization 
                     algorithm for weak automata.                  **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_weak_norm_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototype of private functions.                           ****/

static int    break_transition(automaton *, tran *, uint1);
static uint4  compute_weak_norm_hsize(automaton *);
static int    scc_color(automaton *, stack *, uint4, uint4 *, 
		      uint1, uint1, void *);

/****  Private functions.                                        ****/

/**  int  break_transition(a, t, accept)  :  Breaks the transition *t
                     of the automaton *a (supposed to be labeled by
		     more than one symbol) into a sequence of
		     transitions of length 1. The intermediate states
		     are marked as accepting if and only if accept
		     is set. Returns 0 in the case of success, and
                     returns -1 in the case of an error.           **/

static int  break_transition(a, t, accept)
  automaton *a;
  tran      *t;
  uint1      accept;
{
  register uint4  len, i, s2;
  register uint1  nbr, *label;
           uint4  s1;

  len   = auto_transition_length(t);
  nbr   = auto_alphabet_nbytes(a);
  label = auto_transition_label_ptr(t, nbr);

  s2 = auto_transition_dest(t);
  for (i = len - 1; i > 0; i--)
    {
      if (auto_add_new_state(a, &s1) < 0)
        return -1;
      if (accept)
	auto_mark_accepting_state(a, s1);
      auto_reset_property(a, AUTO_PROP_MINIMAL);
      if (auto_add_new_transition(a, s1, s2, 1, label + i * nbr)
           < 0)
	return -1;
      s2 = s1;
    }

  auto_resize_transition_label(a, t, 1, len);
  auto_redirect_transition(t, s2);

  return 0;
}

/**  uint4  compute_weak_norm_hsize(a)  :  Adjusts (heuristically) the
                     size of the hash table needed for normalizing
                     the weak automaton *a.                        **/

static uint4  compute_weak_norm_hsize(a)
  automaton *a;
{
  register uint4 n;

  n = auto_nb_states(a) >> 3;

  if (!n)
    n = 1;

  if (auto_weak_norm_hsize < n)
    return n;

  return auto_weak_norm_hsize;
}

/**  int  scc_color(a, st, root, scc_num, accept, transient, arg) :
                     This routine is part of the normalization
		     algorithm for weak deterministic automata. It is
		     invoked by the strongly connected components
		     detection algorithm.  The weak automaton that is
		     being normalized is *a. The job of this function
		     is to color the s.c.c. whose root is root. The
		     states of the s.c.c. are on the top of the stack
		     *st, upon the state index root.  scc_num is an
		     array that contains the number of all deeper
		     s.c.c.'s in the directed acyclic graph of the
		     s.c.c.'s of *a. The flag accept is set if and
		     only if the s.c.c. contains at least one
		     accepting state. The flag transient is set if and
		     only if the s.c.c. is a transient state
		     (i.e. does not admit a cycle). The structure *arg
		     is of type *nf_info and contains additional
		     informations about the current normalization
		     process.

		     This function proceeds according to [Lod01].
		     It first computes the minimal color l among
		     the colors of the successor s.c.c.'s. If the
		     s.c.c. is transient, the current s.c.c. gets
		     this color. If it is not transient, the 
		     function colors the current s.c.c. by the
		     even (resp. odd) color immediately inferior to
		     color l if the s.c.c. is accepting (resp. non
		     accepting).

		     In the case of an error, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  scc_color(a, st, root, scc_num, accept, transient, arg)
     automaton  *a;
     stack      *st;
     uint4       root;
     uint4      *scc_num;
     uint1       accept, transient;
     void       *arg;
{
  register uint4     tmp, w, i, c, l;
  register uint1     leaf;
  register nf_info  *info;
           uint4     n, u;
	   tran     *t;
	   void    **r;

  info = (nf_info *) arg;
  
  /* Computes the minimal color of all the successor s.c.c.'s. */
  
  l = INFINITY;
  leaf = 1;
  tmp = 0;
  do
    {
      w = *((uint4 *) stack__pick(st, tmp));

      if (auto_nb_out_transitions(a, w, &n) < 0)
	return -1;

      for (i=0 ; i<n ; i++)
	{
	  t = auto_transition(a, w, i);
	  if (!t || auto_transition_length(t) != 1)
	    /* The current automaton is corrupted or non 
               deterministic. */
	    return -1;

	  u = scc_num[auto_transition_dest(t)];
	  if (u != scc_num[root])
	    {
	      leaf = 0;
	      
	      /* Gets the color of the successor s.c.c. of number 
                 u. */

	      c = *(uint4 *) hash__lookup_bytes
		(info -> ht, (uint1 *) &u, sizeof(uint4));

	      if (c < l)
		l = c;
	    }
	}

      tmp ++;      
    }
  while (w!=root);

  /* Computes the color of the current s.c.c. */

  if (leaf)
    {
      /* The current scc is a leaf. */
      if (transient)
	c = info -> k+1;
      else if (accept)
	c = info -> k;
      else
	c = info -> k-1;
    }

  else
    {
      /* l is the minimal color among the successor s.c.c.'s */
      if ( transient ||
	   ((l % 2 == 0) && accept) ||
	   ((l % 2 == 1) && !accept) )
	c = l;
      else
	c = l-1;
    }

  /* Associates in the hash table the current s.c.c. number
     to the color that has just been computed. */

  if (hash__insert_bytes
      (info -> ht, (uint1 *) &scc_num[root], sizeof(uint4), &r
#if LASH_CHECK_LEVEL >= 1
       , &auto_weak_norm_ncolls, &auto_weak_norm_nins
#endif
       ) < 0 || !r)
    return -1;

  (*r) = (void *) resr__new_object(uint4);
  if (!(*r)) 
    return -1;

  **(uint4 **)r = c;

  /* Sets the accepting status of the states of the current s.c.c. */

  do
    {
      w = *((uint4 *) stack__top(st));
      stack__pop(st, NULL);

      if (c % 2 == 0)
	auto_mark_accepting_state(a, w);
      else
	auto_unmark_accepting_state(a, w);  
    }
  while (w != root);
  
  return 0;
}


/****  Public visible functions.                                 ****/

/**  void  auto_set_weak_norm_hsize(s)  :  Sets the size of the
                     hash table used by the weak automaton
		     normalization algorithm to s.
                     This function does not report errors.         **/

void  auto_set_weak_norm_hsize(s)
  uint4 s;
{
  if (s)
    auto_weak_norm_hsize = s;
}

/**  uint8  auto_get_weak_norm_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by
                     weak automaton normalization algorithm.
		     This function does not report errors.         **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_weak_norm_ncolls()
{
  return auto_weak_norm_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_weak_norm_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by
                     weak automaton normalization algorithm.
		     This function does not report errors.         **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_weak_norm_ncolls()
{
  auto_weak_norm_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_weak_norm_nins()  :  Returns the number of
                     insertions performed in the hash table used by
                     the weak automaton normalization algorithm.
		     This function does not report errors.         **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_weak_norm_nins()
{
  return auto_weak_norm_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_weak_norm_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the weak automaton normalization algorithm.
		     This function does not report errors.         **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_weak_norm_nins()
{
  auto_weak_norm_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  int  auto_is_normal(a)  :  Returns 1 if the finite-state 
                     automaton a is normal, i.e., if all its
                     *transitions have a length less or equal to
                     *1. This function returns the value 0 if the
                     *automaton is not normal, and returns -1 and sets
                     *lash_errno in the case of an error.

                     If the consistency check level is less than 2,
                     then this function takes into account the known
                     properties of the automaton. If the check level
                     is greater or equal to 2, this function
                     explicitly checks the normality of the automaton
                     even if it is already known to be true.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         LASH_ERR_PROP     : Automaton with wrong
                                             known properties.     **/

int  auto_is_normal(a)
  automaton *a;
{
  register uint4 i, j, n;
  register tran *t;
           uint4 m;

  diag__enter("auto_is_normal", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

#if LASH_CHECK_LEVEL < 2
  if (auto_test_property(a, AUTO_PROP_NORMAL))
    diag__return(1);
#endif  /* < 2 */

  n = auto_nb_states(a);
  for (i = 0; i < n; i++)
    {
#if LASH_CHECK_LEVEL >= 1
      if (auto_nb_out_transitions(a, i, &m) < 0)
        diag__fail(LASH_ERR_CORRUPT, -1);
#else
      auto_nb_out_transitions(a, i, &m);
#endif  /* >= 1 */

      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
#if LASH_CHECK_LEVEL >= 1
          if (!t)
            diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
          if (auto_transition_length(t) > 1)
	    {
#if LASH_CHECK_LEVEL >= 2
	      if (auto_test_property(a, AUTO_PROP_NORMAL))
		diag__fail(LASH_ERR_PROP, -1);
#endif  /* >= 2 */
              diag__return(0);
	    }
	}
    }

  diag__return(1);
}

/**  int  auto_normalize(a)  :  Normalizes the finite-state
                     automaton *a, i.e., transforms it into an
                     automaton accepting the same language, but
                     whose transitions are all of length less
                     or equal to 1.

                     This function proceeds by adding new states
                     to the automaton. The indices of the original
                     states are preserved. The new states are
                     created with an accepting status filled with
                     zeroes.

                     In case of success, the function returns 0.
                     In case of error, it returns -1 and leaves in
                     *a an automaton that accepts the same language
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
int  auto_normalize(a)
  automaton *a;
{
  register uint4 i, j, n;
  register tran *t;
  register uint1 accept;
           uint4 m;

  diag__enter("auto_normalize", -1);

  switch(auto_is_normal(a))
    {
    case 0:
      break;
    case 1: 
      diag__return(0);
    case -1:
      diag__fail(lash_errno, -1);
    }

  n = auto_nb_states(a);
  for (i = 0; i < n; i++)
    {
#if LASH_CHECK_LEVEL >= 1
      if (auto_nb_out_transitions(a, i, &m) < 0)
        diag__fail(LASH_ERR_CORRUPT, -1);
#else
      auto_nb_out_transitions(a, i, &m);
#endif  /* >= 1 */

      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
#if LASH_CHECK_LEVEL >= 1
          if (!t)
            diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
	  accept = (auto_word_type(a) == AUTO_WORDS_INFINITE &&
		    auto_accept_type(a) == AUTO_ACCEPT_WEAK &&
		    auto_accepting_state(a, i));
          if (auto_transition_length(t) > 1)
	    if (break_transition(a, t, accept) < 0)
	      diag__fail(LASH_ERR_NO_MEM, -1);
	}
    }

  auto_set_property(a, AUTO_PROP_NORMAL);
  diag__return(0);
}

/**  int  auto_weak_normalize(a)  :  Normalizes the finite-state
                     weak deterministic automaton *a, i.e. change
		     the accepting status of all its s.c.c. in order
	             to be able to support a direct application
		     of the minimization algorithm for automata on
		     finite words. *a must be deterministic and
		     cannot contain epsilon transitions.

		     This function proceeds as suggested in [Lod01].
		     It uses the strongly connected components
		     detection algorithm to compute a maximal
		     coloring of *a. When a s.c.c. is discovered,
		     it is colored by an even or odd number. If this
		     color is even (resp. odd), all of the states of
		     the s.c.c. are marked as accepting (resp. non
		     accepting).
		     
		     This function also works correctly with
		     inherently weak automata [BJW01], if their
		     acceptance type has been previously changed
		     to AUTO_ACCEPT_WEAK. In this case, a s.c.c. is
		     considered as accepting if and only if it
		     contains at least one accepting state. As a
		     result, *a becomes a weak automaton.

                     In case of success, the function returns 0.
                     In case of error, it returns -1 and leaves in
                     *a an automaton that accepts the same language
                     as the original one, but whose states may have
		     changed of accepting status.
 
                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : Automaton contains
                                               reference to an
                                               invalid state.
                         LASH_ERR_PROP       : Automaton with wrong
                                               known properties.
			 LASH_ERR_TOO_BIG    : Automaton with too many
			                       states.             **/

int auto_weak_normalize(a)
     automaton   *a;
{
  register uint4    *scc;
           uint4     count;
	   nf_info   info;

  diag__enter("auto_weak_normalize", -1);

  if (auto_word_type(a) != AUTO_WORDS_INFINITE ||
      auto_accept_type(a) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(a, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (auto_test_property(a, AUTO_PROP_WEAK_NORMAL))
    diag__return(0);

  if (auto_normalize(a) < 0)
    diag__fail(lash_errno, -1);

  if (auto_nb_states(a) >= INFINITY-1)
    diag__fail(LASH_ERR_TOO_BIG, -1);

  /* Resources allocation. */

  scc = resr__new_objects(uint4, auto_nb_states(a));
  if (!scc)
    diag__fail(LASH_ERR_NO_MEM, -1);

  info.ht = hash__new_empty(compute_weak_norm_hsize(a));
  if (!info.ht)
    {
      resr__free_objects(scc, sizeof(uint4), auto_nb_states(a));
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  /* Reduction to the normal form. */

  info.k = auto_nb_states(a);
  if ((info.k % 2) != 0)
    info.k ++;

  if (scc_algorithm(a, scc, &count, 0, &scc_color, &info) < 0)
    {
      resr__free_objects(scc, uint4, auto_nb_states(a));
      hash__free(info.ht, (void (*)(void *)) uint4__free,
		 (void (*)(void *)) uint4__free);
      diag__fail(lash_errno, -1);
    }

  auto_accept_type(a) = AUTO_ACCEPT_WEAK;
  auto_set_property(a, AUTO_PROP_WEAK_NORMAL);

  /* Frees memory. */

  resr__free_objects(scc, uint4, auto_nb_states(a));
  hash__free(info.ht, (void (*)(void *)) uint4__free,
	     (void (*)(void *)) uint4__free);

  diag__return(0);
}

/****  End of auto-normalize.c  ****/
