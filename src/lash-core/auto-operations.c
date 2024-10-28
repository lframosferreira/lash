/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-operations.c  :  Basic operations over finite-state    **/
/**                     automata.                                  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        09/28/98  :  Minor corrections. (BB)                    **/
/**        03/24/01  :  Merging of coBuchi and weak allowed. (SJ)  **/
/**        05/01/01  :  Union of Buchi automata allowed. (SJ)      **/
/**        07/05/02  :  Concatenation of a finite-word automaton   **/
/**                     with a infinite-word automaton. (SJ+YB)    **/
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
#include "lash-auto-operations.h"
#include "auto-sequential.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"

/****  Public visible functions.                                 ****/

/**  int  auto_close(a)  :  Replaces the finite-state automaton on
                     finite words *a by an automaton accepting the
                     Kleene closure of the language accepted by
                     *a. This function proceeds by adding a state and
                     some transitions to *a. The indices of all the
                     states of *a are preserved by this operation.

                     In case of success, the function returns 0.  In
                     case of error, it returns -1, sets lash_errno,
                     and leaves in *a an automaton that does not
                     necessarily accept the language accepted by the
                     original automaton or the closure of this
                     language.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  auto_close(a)
  automaton *a;
{
  register uint4  i, n;
           uint4  s, m;

  diag__enter("auto_close", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE) 
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  n = auto_nb_states(a);

  if (auto_add_new_state(a, &s) < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);

  for (i = 0; i < n; i++)
    if (auto_accepting_state(a, i))
      {
	if (auto_add_new_transition(a, i, s, 0, NULL) < 0)
	  diag__fail(LASH_ERR_NO_MEM, -1);
	auto_unmark_accepting_state(a, i);
      }

  n = auto_nb_i_states(a);
  for (i = 0; i < n; i++)
    if (auto_i_state(a, i, &m) < 0 ||
	auto_add_new_transition(a, s, m, 0, NULL) < 0)
      diag__fail((lash_errno == LASH_ERR_NO_MEM ? LASH_ERR_NO_MEM
          : LASH_ERR_CORRUPT), -1);

  auto_remove_i_states(a);
  auto_mark_accepting_state(a, s);
  if (auto_add_new_i_state(a, s) < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);

  diag__return(0);
}
                  
/**  int  auto_concatenate(a1, a2)  :  Replaces the finite-state
                     automaton on finite words *a1 by an automaton
                     accepting the concatenation of the language
                     accepted by *a1 and of the one accepted by the
                     finite-state automaton on finite or infinite
		     words *a2. The two pointers a1 and a2 may refer
		     to the same object. If *a2 is an automaton on
		     infinite words, *a1 becomes also one on infinite
		     words.

		     In case of success, the function returns 0.  In
                     case of error, it returns -1, sets lash_errno,
                     and leaves in *a1 an automaton that does not
                     necessarily accept the language accepted by the
                     original *a1. This function does not modify the
                     automaton *a2, provided that this automaton is
                     distinct from *a1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  auto_concatenate(a1, a2)
  automaton *a1, *a2;
{
  register uint4  n1, n2, i, j, alph_nbytes, *st_to_num;
  register tran  *t;
           uint4  m;

  diag__enter("auto_concatenate", -1);

  if (auto_word_type(a1) != AUTO_WORDS_FINITE ||
      (auto_word_type(a2) != AUTO_WORDS_FINITE &&
       auto_word_type(a2) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a2) == AUTO_WORDS_INFINITE &&
       auto_accept_type(a2) != AUTO_ACCEPT_BUCHI &&
       auto_accept_type(a2) != AUTO_ACCEPT_WEAK))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  alph_nbytes = auto_alphabet_nbytes(a1);
  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, -1);

  n1 = auto_nb_states(a1);
  n2 = auto_nb_states(a2);

  st_to_num = resr__new_objects(uint4, n2 + 1);
  if (!st_to_num)
    diag__fail(LASH_ERR_NO_MEM, -1);

  for (i = 0; i <= n2; i++)
    {
      if (auto_add_new_state(a1, st_to_num + i) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n2 + 1);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      if (i && auto_accepting_state(a2, i - 1))
	auto_mark_accepting_state(a1, st_to_num[i]);
    }

  for (i = 0; i < n1; i++)
    if (auto_accepting_state(a1, i))
      {
	auto_unmark_accepting_state(a1, i);
        if (auto_add_new_transition(a1, i, st_to_num[0],
            0, NULL) < 0)
	  {
	    resr__free_objects(st_to_num, uint4, n2 + 1);
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  }
      }
  
  j = auto_nb_i_states(a2);
  for (i = 0; i < j; i++)
    if (auto_i_state(a2, i, &m) < 0 ||
	auto_add_new_transition(a1, st_to_num[0],
            st_to_num[m + 1], 0, NULL) < 0)
      {
	resr__free_objects(st_to_num, uint4, n2 + 1);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  for (i = 0; i < n2; i++)
    {
      if (auto_nb_out_transitions(a2, i, &m) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n2 + 1);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a2, i, j);
	  if (auto_transition_dest(t) < n2 &&
              auto_add_new_transition(a1, st_to_num[i + 1],
	      st_to_num[auto_transition_dest(t) + 1],
              auto_transition_length(t),
              auto_transition_label_ptr(t, alph_nbytes)) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, n2 + 1);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }  
	}
    }

  resr__free_objects(st_to_num, uint4, n2 + 1);

  if (auto_word_type(a2) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(a1) = AUTO_WORDS_INFINITE;
      auto_accept_type(a1) = auto_accept_type(a2);
    }

  diag__return(0);
}                   

/**  int  auto_merge(a1, a2)  :  Replaces the finite-state automaton
                     on finite (resp.) infinite words *a1 by an
		     automaton accepting the union of the language 
		     accepted by *a1 and of the one accepted by the
		     finite-state automaton on finite (resp.
		     infinite) words *a2. The two pointers a1 and a2
		     may refer to the same object.

		     If *a1 and *a2 are automata on infinite words,
		     they must be both co-Buchi or they must be both
		     weak in order to allow a further
		     determinization. If *a1 or *a2 is Buchi, the
		     merging is aborted.

                     In case of success, the function returns 0.  In
                     case of error, it returns -1, sets lash_errno,
                     and leaves in *a1 an automaton that does not
                     necessarily accept the language accepted by the
                     original *a1. This function does not modify the
                     automaton *a2, provided that this automaton is
                     distinct from *a1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : Automaton contains
                                               reference to an
                                               invalid state.      **/

int auto_merge(a1, a2)
  automaton *a1, *a2;
{
  register uint4  n1, n2, i, j, alph_nbytes, *st_to_num;
  register tran  *t;
           uint4  m;

  diag__enter("auto_merge", -1);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
       (auto_accept_type(a1) != auto_accept_type(a2) ||
	(auto_accept_type(a1) != AUTO_ACCEPT_COBUCHI &&
	 auto_accept_type(a1) != AUTO_ACCEPT_WEAK))))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  alph_nbytes = auto_alphabet_nbytes(a1);
  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, -1);

  if (a1 == a2)
    diag__return(0);

  auto_reset_property(a1, AUTO_PROP_DETERM);

  n1 = auto_nb_states(a1);
  n2 = auto_nb_states(a2);

  st_to_num = resr__new_objects(uint4, n2);
  if (!st_to_num)
    diag__fail(LASH_ERR_NO_MEM, -1);

  for (i = 0; i < n2; i++)
    {
      if (auto_add_new_state(a1, st_to_num + i) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n2);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      if (auto_accepting_state(a2, i))
	auto_mark_accepting_state(a1, st_to_num[i]);
    }

  j = auto_nb_i_states(a2);
  for (i = 0; i < j; i++)
    if (auto_i_state(a2, i, &m) < 0 ||
	auto_add_new_i_state(a1, st_to_num[m]) < 0)
      {
	resr__free_objects(st_to_num, uint4, n2);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  for (i = 0; i < n2; i++)
    {
      if (auto_nb_out_transitions(a2, i, &m) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n2);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a2, i, j);
	  if (auto_add_new_transition(a1, st_to_num[i],
	      st_to_num[auto_transition_dest(t)],
              auto_transition_length(t),
              auto_transition_label_ptr(t, alph_nbytes)) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, n2);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }  
	}
    }

  resr__free_objects(st_to_num, uint4, n2);
  diag__return(0);
}

/**  automaton *auto_union(a1, a2)  :  Computes a finite-state 
                     automaton on finite or infinite words that
		     accepts the union of the languages accepted by
		     the two finite-state automata on finite or
		     infinite words *a1 and *a2.

		     If *a1 and *a2 are automata on infinite words,
		     they must be both Buchi or weak, or they must
		     be both co-Buchi. If both are Buchi, then one
		     applies the product-based union. Otherwise,
		     the two automata are merged.
 
		     This function does not modify *a1 or *a2, and
                     returns (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

automaton *auto_union(a1, a2)
  automaton *a1, *a2;
{
  register automaton *ar;

  diag__enter("auto_union", NULL);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  if (auto_word_type(a1) == AUTO_WORDS_FINITE ||
      ((auto_accept_type(a1) == AUTO_ACCEPT_COBUCHI &&
	auto_accept_type(a2) == AUTO_ACCEPT_COBUCHI) ||
       (auto_accept_type(a1) == AUTO_ACCEPT_WEAK &&
	auto_accept_type(a2) == AUTO_ACCEPT_WEAK)))
    {
      ar = auto_copy(a1);
      if (!ar)
	diag__fail(LASH_ERR_NO_MEM, NULL);
      
      if (a1 != a2 && auto_merge(ar, a2) < 0)
	{
	  auto_free(ar);
	  diag__fail(lash_errno, NULL);
	}
      
      diag__return(ar);
    }

  else if ((auto_accept_type(a1) == AUTO_ACCEPT_BUCHI ||
	    auto_accept_type(a1) == AUTO_ACCEPT_WEAK) &&
	   (auto_accept_type(a2) == AUTO_ACCEPT_BUCHI ||
	    auto_accept_type(a2) == AUTO_ACCEPT_WEAK))
    {
      ar = auto_product_union(a1, a2);

      if (ar)
	diag__return(ar);

      diag__fail(lash_errno, NULL);
    }

  else
    diag__fail(LASH_ERR_BAD_TYPE, NULL);
}

/**  int auto_apply_homomorphism(a, f, na2)  :  Applies the 
                     homomorphism *f to the transition labels of the
                     finite-state automaton on finite words *a. The
                     function f takes six arguments n, ptr, rn, rptr,
                     na1, na2. The first two (n and ptr) give the
                     number of symbols and a pointer to the label of a
                     transition. The next two (rn, rptr) are pointers
                     to locations in which the function *f must return
                     the new number of symbols and a pointer to the
                     new label (this label will be copied by
                     auto_apply_homomorphism).  The last arguments na1
                     and na2 give the number of bytes required for
                     storing one symbol of the alphabet of *a before
                     and after applying f. The argument na2 is
                     identical in value to the corresponding argument
                     of auto_apply_homomorphism.  The function *f must
                     return 0 in the case of success, and -1 if the
                     operation is to be aborted.

                     The function auto_apply_homomorphism returns 0 in
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

int  auto_apply_homomorphism(a, f, na2)
  automaton   *a;
  int        (*f)(uint4, uint1 *, uint4 *, uint1 **, uint1, uint1);
  uint1        na2;
{
  register uint4       i, j, *st_to_num, n;
  register uint1       na1;
  register automaton  *ar;
  register tran       *t;
           uint4       p, rn;
           uint1      *rptr;

  diag__enter("auto_apply_homomorphism", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE) 
    diag__fail(LASH_ERR_BAD_TYPE, -1);
  
  n   = auto_nb_states(a);
  na1 = auto_alphabet_nbytes(a);

  ar = auto_new_empty(na2);
  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, -1);

  st_to_num = resr__new_objects(uint4, n);
  if (!st_to_num)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  for (i = 0; i < n; i++)
    if (auto_add_new_state(ar, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, n);
	auto_free(ar);
        diag__fail(LASH_ERR_CORRUPT, -1); 
      }

  for (i = 0; i < n; i++)
    {
      if (auto_nb_out_transitions(a, i, &p) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n);
	  auto_free(ar);
	  diag__fail(LASH_ERR_CORRUPT, -1); 
	  }
      for (j = 0; j < p; j++)
	{
	  t = auto_transition(a, i, j);
	  if (f(auto_transition_length(t),
                auto_transition_label_ptr(t, na1),
		&rn, &rptr, na1, na2) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, n);
	      auto_free(ar);
	      diag__fail(LASH_ERR_ABORT, -1); 
	    }
	  if (auto_add_new_transition(ar, st_to_num[i],
              st_to_num[auto_transition_dest(t)],
	      rn, rptr) < 0)
	    {
	      resr__free_objects(st_to_num, uint4, n);
	      auto_free(ar);
	      diag__fail(LASH_ERR_CORRUPT, -1); 
	    }
	  }

      if (auto_accepting_state(a, i))
	auto_mark_accepting_state(ar, st_to_num[i]);
    }
  
  j = auto_nb_i_states(a);
  for (i = 0; i < j; i++)
    {
      auto_i_state(a, i, &p);
      if (auto_add_new_i_state(ar, st_to_num[p]) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n);
	  auto_free(ar);
	  diag__fail(LASH_ERR_CORRUPT, -1); 
	}
    }
  
  resr__free_objects(st_to_num, uint4, n);  
  auto_replace(a, ar);
  
  diag__return(0); 
}

/****  End of auto-operations.c  ****/
