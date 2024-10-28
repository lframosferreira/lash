/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-difference.c  :  Difference-based operations over      **/
/**                     finite-state automata.                     **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        09/14/98  :  Got rid of recursion. (BB)                 **/
/**        11/12/99  :  Bug corrected in hash table size           **/
/**                     computation. (BB)                          **/
/**        02/04/00  :  Bug corrected in complementation. (BB)     **/
/**        03/09/00  :  Minor correction. (JMF + BB)               **/
/**        03/16/01  :  Difference with weak det. automata. (SJ)   **/
/**        03/17/01  :  Buchi automata emptiness test. (SJ)        **/
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
#include "auto-difference.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "graph-scc.h"

/****  Global variables.                                         ****/

/**  auto_diff_hsize  :  Size of the hash table used by the     
                    difference algorithm.                          **/

static uint4  auto_diff_hsize = AUTO_DEFAULT_DIFF_HSIZE;

/**  auto_diff_ncolls  :  Number of collisions observed in the   
                     hash table used by the difference
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_diff_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_diff_nins  :  Number of insertions performed in the   
                     hash table used by the difference
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_diff_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static uint4  compute_diff_hsize(automaton *, automaton *);
static int    diff_add_symbols(hash_table *, uint1 **, uint4 *,
                  automaton *);
static int    complete_automaton(automaton *, hash_table *, uint1 *,
                  uint4, uint1);
static void   complement_automaton(automaton *);
static int    explore_accepting_states(automaton *, bit_table *, 
                  uint4);
static int    test_buchi_scc(automaton *, stack *, uint4, uint4 *,
		  uint1, uint1, void *);

/****  Private functions.                                        ****/

/**  uint4  compute_diff_hsize(a1, a2)  :  Adjusts (heuristically) the
                     size of the hash table needed for computing the
                     difference of the automata *a1 and *a2.       **/

static uint4  compute_diff_hsize(a1, a2)
  automaton *a1, *a2;
{
  register uint4  n;

  n = 8 * auto_alphabet_nbytes(a1);

  if (!n)
    n = 1;

  if (n < 32 && auto_diff_hsize >> n)
    return (1 << n);

  if (auto_diff_hsize < n)
    return n;

  return auto_diff_hsize;
}

/**  int  diff_add_symbols(ht, sp, np, a)  :  Adds all the symbols
                     appearing on the transition labels of the
                     finite-state automaton on finite words *a to the
                     hash table *ht. The payload of each symbol in
                     that table is a pointer to an integer
                     corresponding to the index of the symbol (0, 1,
                     ...). The number of symbols in the table is kept
                     in *np. An array of symbols is kept in *sp. This
                     function updates *np and *sp. In the case of an
                     error, this function returns -1. Otherwise, It
                     returns 0.                                    **/

static int  diff_add_symbols(ht, sp, np, a)
  hash_table  *ht;
  uint1      **sp;
  uint4       *np;
  automaton   *a;
{
  register uint4  i, j, k, nb_states, len, *v;
  register uint1  alph_nbytes;
  register tran  *t;
  register uint1 *p, *s;
           uint4  m;
           void **r;

  nb_states   = auto_nb_states(a);
  alph_nbytes = auto_alphabet_nbytes(a);

  for (i = 0; i < nb_states; i++)
    {
      if (auto_nb_out_transitions(a, i, &m) < 0)
	return -1;
      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
	  len = auto_transition_length(t);
	  for (k = 0, p = auto_transition_label_ptr(t, alph_nbytes);
               k < len; k++, p += alph_nbytes)
	    {
#if LASH_CHECK_LEVEL >= 1
	      if (hash__insert_bytes(ht, p, alph_nbytes, &r,
                  &auto_diff_ncolls, &auto_diff_nins) < 0)
#else
	      if (hash__insert_bytes(ht, p, alph_nbytes, &r) < 0)
#endif  /* >= 1 */	      
		return -1;
	      if (r)
		{
		  v = resr__new_object(uint4);
		  if (!v)
		    return -1;
		  *r = v;
		  *v = *np;
		  s = (uint1 *) resr__resize_objects(*sp, uint1,
                      ((*np) + 1) * alph_nbytes, 
                      (*np) * alph_nbytes);
		  if (!s)
		    return -1;
		  *sp = s;
		  memcpy(s + (*np)++ * alph_nbytes,
                      p, alph_nbytes);
		}
	    }
	}
    }

  return 0;
}

/**  int  complete_automaton(a, ht, s, n, na)  :  This routine is part
                     of the difference algorithm. It adds to the
                     deterministic and normal automaton *a a dummy
                     non-accepting state, and adds to each state of *a
                     as many outgoing transitions to that state as
                     necessary, so as to obtain an automaton accepting
                     the same language as *a, but such that each state
                     has an outgoing transition labeled by every
                     symbol of the alphabet. An hash table associating
                     an index (0, 1, ...) to each symbol is given by
                     ht. The number of distinct symbols is n. An array
                     of symbols is given by s. The number of bytes
                     required for storing one symbol is na.  In the
                     case of an error, this function returns
                     -1. Otherwise, It returns 0.                  **/

static int  complete_automaton(a, ht, s, n, na)
  automaton  *a;
  hash_table *ht;
  uint1      *s, na;
  uint4       n;
{
  register uint4      i, j, nb_states, *p;
  register tran      *t;
  register bit_table *bt;
           uint4      m, s1;

  if (auto_add_new_state(a, &s1) < 0)
    return -1;

  nb_states = auto_nb_states(a);

  bt = bit__new_empty(n);
  if (!bt)
    return -1;

  for (i = 0; i < nb_states; i++)
    {
      if (auto_nb_out_transitions(a, i, &m) < 0)
	{
	  bit__free(bt);
	  return -1;
	}
      bit__empty_content(bt);
      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
	  if (!auto_transition_length(t))
	    continue;
	  p = (uint4 *) hash__lookup_bytes(ht,
	      auto_transition_label_ptr(t, na), na);
	  if (p)
	    bit__add(bt, *p);
	}
      for (j = 0; j < n; j++)
	if (!bit__member(bt, j) &&
	    auto_add_new_transition(a, i, s1, 1, s + j * na) < 0)
	  {
	    bit__free(bt);
	    return -1;
	  }
    }

  bit__free(bt);
  return 0;
}  

/**  void complement_automaton(a)  :  This routine is part of the
                     difference algorithm. It simply exchanges the
                     accepting and the non-accepting states of the
                     deterministic, normal and complete automaton
                     *a.                                           **/

static void  complement_automaton(a)
  automaton *a;
{
  register uint4  i, nb_states;

  nb_states = auto_nb_states(a);
  for (i = 0; i < nb_states; i++)
    if (auto_accepting_state(a, i))
      {
	auto_unmark_accepting_state(a, i);
      }
    else
      auto_mark_accepting_state(a, i);
}

/**  typedef  eas_info  :  Type of the data placed of the exploration
                     stack of the function explore_accepting_states.
                     The first field is the index of a state; the 
                     second field is the number of outgoing 
                     transitions that have already been followed 
                     from that state.                              **/

typedef struct {
  uint4  state, nb_transitions;
} eas_info;

/**  int  explore_accepting_states(a, bt, s)  :  Explores all the
                     reachable states of the finite-state automaton
                     *a, starting at the state of index s. The bit
                     table *bt contains the indices of the states that
                     have already been visited. This function returns
                     1 if an accepting state is found, 0 if there is
                     no accepting state, and -1 in the case of an
                     error.                                        **/

static int  explore_accepting_states(a, bt, s)
  automaton *a;
  bit_table *bt;
  uint4      s;
{
           uint4     m;
  register tran     *t;
  register stack    *st;
           eas_info  e;
  
  st = stack__new_empty(eas_info);
  if (!st)
    return -1;

  e.state = s;
  e.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &e) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &e);
      if (!e.nb_transitions)
        {
	  if (auto_accepting_state(a, e.state))
	    {
	      stack__free(st);
	      return 1;
	    }

          if (bit__member(bt, e.state))
            continue;

          bit__add(bt, e.state);
        }
      if (auto_nb_out_transitions(a, e.state, &m) < 0)
        {
          stack__free(st);
          return -1;
        }
      if (e.nb_transitions >= m)
        continue;
     
      t = auto_transition(a, e.state, e.nb_transitions);

      e.nb_transitions++;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }

      e.state = auto_transition_dest(t);
      e.nb_transitions = ZERO_INT4;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  test_buchi_scc(a,st,root,scc_num,accept,transient,arg) :
                     Function invoked by the strongly connected
		     components detection algorithm. It checks
		     whether or not the current s.c.c. contains
		     an accepting cycle. This amounts to test whether
		     the s.c.c. contains an accepting state that is 
		     not transient. For that purpose, this function
		     only uses the arguments accept and transient.

		     This function does not need to know the
		     states belonging to the current s.c.c., so
		     it does not use the stack *st. For this reason,
		     the parameter SCC_AUTOMATIC_POP must be used in
		     every call to Tarjan's algorithm that uses this
		     function.

		     This function cannot incur an error. Its
		     return value is 1 if the s.c.c. contains an
		     accepting cycle. Its return value will be 0
		     otherwise..                                   **/

static int test_buchi_scc(a, st, root, scc_num, accept, transient, 
			  arg)
     automaton   *a;
     stack       *st;
     uint4        root;
     uint4       *scc_num;
     uint1        accept, transient;
     void        *arg;
{
  return (accept && !transient);
}

/****  Public visible functions.                                 ****/

/**  void  auto_set_diff_hsize(s)  :  Sets the size of the hash table
                     used by the difference algorithm to s.
                     This function does not report errors.         **/

void  auto_set_diff_hsize(s)
  uint4 s;
{
  if (s)
    auto_diff_hsize = s;
}

/**  uint8  auto_get_diff_ncolls()  :  Returns the number of 
                     collisions observed in the hash table used by the
                     difference algorithm.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_diff_ncolls()
{
  return auto_diff_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_diff_ncolls()  :  Resets the number of 
                     collisions observed in the hash table used by the
                     difference algorithm.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_diff_ncolls()
{
  auto_diff_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_diff_nins()  :  Returns the number of insertions
                     performed in the hash table used by the
                     difference algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_diff_nins()
{
  return auto_diff_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_diff_nins()  :  Resets the number of insertions
                     performed in the hash table used by the
                     difference algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_diff_nins()
{
  auto_diff_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  automaton *auto_difference(a1, a2)  :  Computes a finite-state
                     automaton on finite (resp. infinite) words that
		     accepts the difference of the languages accepted
		     by the two finite-state automata on finite words
		     *a1 and *a2 (resp. on infinite words, in which
		     case *a2 must be a weak deterministic automaton,
		     and *a1 must be a Buchi or weak automaton).
 
		     This function does not modify *a1 or *a2, and
                     returns (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_difference(a1, a2)
  automaton *a1, *a2;
{
  register uint1       alph_nbytes;
  register automaton  *ar, *ab;
  register hash_table *h_symbols;
  register int         r;
           uint1      *symbols;
           uint4       nb_symbols;

  diag__enter("auto_difference", NULL);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
	((auto_accept_type(a1) != AUTO_ACCEPT_BUCHI &&
	  auto_accept_type(a1) != AUTO_ACCEPT_WEAK) ||
	 auto_accept_type(a2) != AUTO_ACCEPT_WEAK ||
	 !auto_test_property(a2, AUTO_PROP_DETERM))))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes = auto_alphabet_nbytes(a1);

  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, NULL);

  ab = auto_copy(a2);
  if (!ab)
    diag__fail(lash_errno, NULL);

  if ((auto_word_type(a2) == AUTO_WORDS_FINITE &&
       auto_determinize(ab) < 0) ||
      auto_normalize(ab) < 0)
    {
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  if (!(auto_nb_i_states(ab)))
    {
      auto_free(ab);
      ab = auto_copy(a1);
      if (!ab)
	diag__fail(lash_errno, NULL);
      diag__return(ab);
    }

  h_symbols = hash__new_empty(compute_diff_hsize(a1, ab));
  if (!h_symbols)
    {
      auto_free(ab);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  nb_symbols = ZERO_INT4;
  symbols = NULL;

  if (diff_add_symbols(h_symbols, &symbols, &nb_symbols, a1) < 0 ||
      diff_add_symbols(h_symbols, &symbols, &nb_symbols, ab) < 0)
    {
      bytes__prepare_free(alph_nbytes);
      hash__free(h_symbols,
          (void (*)(void *)) bytes__free,
          (void (*)(void *)) uint4__free);
      resr__free_objects(symbols, uint1, 
          nb_symbols * alph_nbytes);
      auto_free(ab);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  r = complete_automaton(ab, h_symbols, symbols, nb_symbols,
          alph_nbytes);

  bytes__prepare_free(alph_nbytes);
  hash__free(h_symbols, (void (*)(void *)) bytes__free,
      (void (*)(void *)) uint4__free);
  resr__free_objects(symbols, uint1, nb_symbols * alph_nbytes);

  if (r < 0)
    {
      auto_free(ab);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  complement_automaton(ab);

  if (auto_test_property(a2, AUTO_PROP_DETERM))
    auto_set_property(ab, AUTO_PROP_DETERM);

  ar = auto_intersection(a1, ab);

  auto_free(ab);
  if (!ar)
    diag__fail(lash_errno, NULL);

  diag__return(ar);
}

/**  int  auto_empty_language(a)  :  Checks whether the language 
                     accepted by the finite-state automaton on finite
		     or infinite words *a is empty. This function
		     returns 1 if this language is empty, 0 if it is
		     not empty, and -1 in the case of an error (in
		     which case it sets lash_errno). This function
		     does not modify *a.

		     If *a is an automaton on infinite words, it can
		     only be Buchi or weak. In this case, the
		     function checks whether *a admits an accepting
		     run. This is done by extracting all the
		     reachable s.c.c. of *a and checking for each of
		     them whether it admits an accepting cycle. For
		     that purpose, this function uses Tarjan's
		     algorithm.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  auto_empty_language(a)
  automaton *a;
{
  register uint4      i, n, m;
  register bit_table *bt;
  register int        res;
           uint4      p;

  diag__enter("auto_empty_language", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE &&
      (auto_word_type(a) != AUTO_WORDS_INFINITE ||
       (auto_accept_type(a) != AUTO_ACCEPT_BUCHI &&
	auto_accept_type(a) != AUTO_ACCEPT_WEAK)))
    diag__fail(LASH_ERR_BAD_TYPE, -1);
  
  n = auto_nb_states(a);
  m = auto_nb_i_states(a);

  if (!m)
    diag__return(1);

  switch (auto_word_type(a))
    {
    case AUTO_WORDS_FINITE :

      bt = bit__new_empty(n);
      if (!bt)
	diag__fail(LASH_ERR_NO_MEM, -1);
 
      for (i = 0; i < m; i++)
	{
	  if (auto_i_state(a, i, &p) < 0)
	    {
	      bit__free(bt);
	      diag__fail(LASH_ERR_CORRUPT, -1);
	    }
	  switch(explore_accepting_states(a, bt, p))
	    {
	    case 1 : 
	      bit__free(bt);
	      diag__return(0);
	    case 0 : 
	      continue;
	    default :
	      bit__free(bt);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	}

      bit__free(bt);
      diag__return(1);
      break;

    case AUTO_WORDS_INFINITE :

      res = scc_algorithm(a, NULL, &p, SCC_ONLY_REACHABLE |
			  SCC_AUTOMATIC_POP, test_buchi_scc, NULL);
      
      if (res<0)
	diag__fail(lash_errno, -1);
      
      if (res>=0)
	res = !res;
      
      diag__return(res);
      break;

    default :
      diag__fail(LASH_ERR_BAD_TYPE, -1);
    }
}

/**  int  auto_inclusion(a1, a2)  :  Checks whether the language
                     accepted by the finite-state automaton on finite
                     (resp. infinite) words *a1 is a subset of the
		     language accepted by the finite-state automaton
		     on finite (resp. infinite) words *a2. If it is
		     the case, then this function returns 1.
		     Otherwise, it returns 0. In the case of an
		     error, the function returns -1 and sets
		     lash_errno.

		     When one deals with automata on infinite words,
		     *a1 must be Buchi or weak and *a2 must be weak
		     deterministic.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Automaton with too
			                       many states.        **/

int  auto_inclusion(a1, a2)
  automaton *a1, *a2;
{
  register uint4       alph_nbytes;
  register automaton   *ab;
  register hash_table *h_symbols;
  register int         r;
           uint1      *symbols;
           uint4       nb_symbols;

  diag__enter("auto_inclusion", -1);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      ( auto_word_type(a1) == AUTO_WORDS_INFINITE &&
	( (auto_accept_type(a1) != AUTO_ACCEPT_BUCHI &&
	   auto_accept_type(a1) != AUTO_ACCEPT_WEAK) ||
	  auto_accept_type(a2) != AUTO_ACCEPT_WEAK ||
	  !auto_test_property(a2, AUTO_PROP_DETERM) ) ))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  switch (auto_word_type(a1))
    {
    case AUTO_WORDS_FINITE :

      alph_nbytes = auto_alphabet_nbytes(a1);

      if (alph_nbytes != auto_alphabet_nbytes(a2))
	diag__fail(LASH_ERR_ALPHABET, -1);

      ab = auto_copy(a2);
      if (!ab)
	diag__fail(lash_errno, -1);

      if (auto_determinize(ab) < 0 || auto_normalize(ab) < 0)
	{
	  auto_free(ab);
	  diag__fail(lash_errno, -1);
	}

      if (!(auto_nb_i_states(ab)))
	{
	  auto_free(ab);
	  r = auto_empty_language(a1);
	  if (r < 0)
	    diag__fail(lash_errno, -1);
	  diag__return(r);
	}

      h_symbols = hash__new_empty(compute_diff_hsize(a1, ab));
      if (!h_symbols)
	{
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      nb_symbols = ZERO_INT4;
      symbols = NULL;

      if (diff_add_symbols(h_symbols, &symbols, &nb_symbols, a1) < 0 
          ||
          diff_add_symbols(h_symbols, &symbols, &nb_symbols, ab) < 0)
	{
	  bytes__prepare_free(alph_nbytes);
	  hash__free(h_symbols,
		     (void (*)(void *)) bytes__free,
		     (void (*)(void *)) uint4__free);
	  resr__free_objects(symbols, uint1, nb_symbols * 
			     alph_nbytes);
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      r = complete_automaton(ab, h_symbols, symbols, nb_symbols,
			     alph_nbytes);

      bytes__prepare_free(alph_nbytes);
      hash__free(h_symbols, (void (*)(void *)) bytes__free,
		 (void (*)(void *)) uint4__free);
      resr__free_objects(symbols, uint1, nb_symbols * alph_nbytes);

      if (r < 0)
	{
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      complement_automaton(ab);

      r = auto_empty_intersection(a1, ab);
      auto_free(ab);

      if (r < 0)
	diag__fail(lash_errno, -1);

      diag__return(r);
      break;

    case AUTO_WORDS_INFINITE :

      ab = auto_difference(a1, a2);
      if (!ab)
	diag__fail(lash_errno, -1);

      r = auto_empty_language(ab);
      auto_free(ab);

      if (r < 0)
	diag__fail(lash_errno, -1);

      diag__return(r);
      break;

    default :
      diag__fail(LASH_ERR_BAD_TYPE, -1);
    }
}

/**  int  auto_equality(a1, a2)  :  Checks whether the languages
                     accepted by the two finite-state automata on
                     finite or infinite words *a1 and *a2 are equal.
		     If it is the case, then this function returns 1.
		     Otherwise, it returns 0. In the case of an
		     error, the function returns -1 and sets
		     lash_errno.

		     When one deals with automata on infinite words,
		     both *a1 and *a2 must be weak deterministic
		     automata.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Automaton with too
			                       many states.        **/

int  auto_equality(a1, a2)
  automaton *a1, *a2;
{
  register uint4       alph_nbytes;
  register automaton   *aa, *ab;
  register hash_table *h_symbols;
  register int         ra, rb;
           uint1      *symbols;
           uint4       nb_symbols;

  diag__enter("auto_equality", -1);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE ) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
       (auto_accept_type(a1) != AUTO_ACCEPT_WEAK ||
	auto_accept_type(a2) != AUTO_ACCEPT_WEAK ||
	!auto_test_property(a1, AUTO_PROP_DETERM) ||
	!auto_test_property(a2, AUTO_PROP_DETERM))))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  switch (auto_word_type(a1))
    {
    case AUTO_WORDS_FINITE :

      alph_nbytes = auto_alphabet_nbytes(a1);

      if (alph_nbytes != auto_alphabet_nbytes(a2))
	diag__fail(LASH_ERR_ALPHABET, -1);

      aa = auto_copy(a1);
      if (!aa)
	diag__fail(lash_errno, -1);

      ab = auto_copy(a2);
      if (!ab)
	{
	  auto_free(aa);
	  diag__fail(lash_errno, -1);
	}

      if (auto_determinize(aa) < 0 || auto_normalize(aa) < 0 ||
	  auto_determinize(ab) < 0 || auto_normalize(ab) < 0)
	{
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(lash_errno, -1);
	}

      ra = auto_empty_language(aa);
      rb = auto_empty_language(ab);
      if (ra < 0 || rb < 0)
	{
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(lash_errno, -1);
	}

      if (ra || rb)
	{
	  auto_free(aa);
	  auto_free(ab);
	  diag__return(ra && rb);
	}

      h_symbols = hash__new_empty(compute_diff_hsize(aa, ab));
      if (!h_symbols)
	{
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      nb_symbols = ZERO_INT4;
      symbols = NULL;

      if (diff_add_symbols(h_symbols, &symbols, &nb_symbols, aa) < 0 
          ||
	  diff_add_symbols(h_symbols, &symbols, &nb_symbols, ab) < 0)
	{
	  bytes__prepare_free(alph_nbytes);
	  hash__free(h_symbols,
		     (void (*)(void *)) bytes__free,
		     (void (*)(void *)) uint4__free);
	  resr__free_objects(symbols, uint1, nb_symbols * 
			     alph_nbytes);
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      ra = complete_automaton(aa, h_symbols, symbols, nb_symbols,
			      alph_nbytes);
      rb = complete_automaton(ab, h_symbols, symbols, nb_symbols,
			      alph_nbytes);

      bytes__prepare_free(alph_nbytes);
      hash__free(h_symbols, (void (*)(void *)) bytes__free,
		 (void (*)(void *)) uint4__free);
      resr__free_objects(symbols, uint1, nb_symbols * alph_nbytes);

      if (ra < 0 || rb < 0)
	{
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      complement_automaton(ab);

      switch(auto_empty_intersection(aa, ab))
	{
	case 0 :
	  auto_free(aa);
	  auto_free(ab);
	  diag__return(0);
	case 1:
	  break;
	default :
	  auto_free(aa);
	  auto_free(ab);
	  diag__fail(lash_errno, -1);
	}

      complement_automaton(aa);
      complement_automaton(ab);

      ra = auto_empty_intersection(ab, aa);

      auto_free(aa);
      auto_free(ab);

      if (ra < 0)
	diag__fail(lash_errno, -1);

      diag__return(ra);
      break;

    case AUTO_WORDS_INFINITE :

      switch(auto_inclusion(a1, a2))
	{
	case 0  : diag__return(0);
	case 1  : break;
	default : diag__fail(lash_errno, -1);
	}

      ra = auto_inclusion(a2, a1);

      if (ra < 0)
	diag__fail(lash_errno, -1);

      diag__return(ra);
      
      break;
      
    default :
      diag__fail(LASH_ERR_BAD_TYPE, -1);
    }
}

/****  End of auto-difference.c  ****/
