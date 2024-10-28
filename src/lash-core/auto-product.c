/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-product.c  :  Product-based operations over finite-    **/
/**                     state automata.                            **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        09/16/98  :  Got rid of recursion. (BB)                 **/
/**        09/18/98  :  Minor corrections. (BB)                    **/
/**        02/05/01  :  Product-based union. (SJ)                  **/
/**        03/14/01  :  Integration of an intersection operation   **/
/**                     suitable for Buchi automata. (SJ)          **/
/**        03/20/01  :  Product for Buchi automata. (SJ)           **/
/**        03/29/01  :  Minor corrections. (SJ)                    **/
/**        05/01/01  :  Product with separator. (SJ)               **/
/**        07/02/02  :  Reorganization. (BB)                       **/
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
#include "auto-product.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"

/****  Global variables.                                         ****/

/**  auto_prod_hsize  :  Size of the hash table used by the     
                    product algorithms.                            **/

static uint4  auto_prod_hsize = AUTO_DEFAULT_PROD_HSIZE;

/**  auto_prod_ncolls  :  Number of collisions observed in the   
                     hash table used by the product
                     algorithms.                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_prod_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_prod_nins  :  Number of insertions performed in the   
                     hash table used by the product
                     algorithms.                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_prod_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static uint4      compute_prod_hsize(uint4, uint4);
static int        prod_generate(automaton *, automaton *, uint1,
                      uint1, uint1 *, uint1 *, automaton *, uint4,
		      uint4, uint4 *, hash_table *);
static int        prod_generate_init(automaton *, automaton *,
                      uint1, uint1, automaton *, hash_table *,
                      stack *);
static int        prod_generate_loop(automaton *, automaton *,
                      uint1, uint1, uint1 *, uint1 *, automaton *,
		      hash_table *, stack *);
static int        prod_generate_one(automaton *, automaton *, uint1,
                      uint1, automaton *, uint4, uint4, uint1, uint4,
		      tran *, tran *, hash_table *, stack *);
static automaton *product(automaton *, automaton *, uint4, uint4,
                      uint1, uint1, uint1 *, uint1 *);
static int        inter_generate(automaton *, automaton *, uint1,
                      automaton *, uint4, uint4, uint4 *, 
                      hash_table *);
static int        inter_generate_init(automaton *, automaton *,
                      uint1, automaton *, hash_table *, stack *);
static int        inter_generate_loop(automaton *, automaton *,
                      uint1, automaton *, hash_table *, stack *);
static int        inter_generate_one(automaton *, automaton *, uint1,
                      automaton *, uint4, uint4, uint1, uint4, tran *,
                      tran *, hash_table *, stack *);
static automaton *intersection(automaton *, automaton *, uint4,
                      uint4, uint1);
static int        empty_inter_generate(automaton *, automaton *,
                      uint1, uint4, uint4, hash_table *);
static int        empty_inter_generate_init(automaton *, automaton *,
                      uint1, hash_table *, stack *);
static int        empty_inter_generate_loop(automaton *, automaton *,
                      uint1, hash_table *, stack *);
static int        empty_intersection(automaton *, automaton *, uint4,
                      uint4, uint1);
static automaton *union_proc(automaton *, automaton *, uint4,
                      uint4, uint1);
static int        union_generate(automaton *, automaton *, uint1,
                      automaton *, uint4, uint4, uint4 *, 
                      hash_table *);
static int        union_generate_init(automaton *, automaton *,
                      uint1, automaton *, hash_table *, stack *);
static int        union_generate_loop(automaton *, automaton *,
                      uint1, automaton *, hash_table *, stack *);
static int        union_generate_one(automaton *, automaton *, uint1,
                      automaton *, uint4, uint4, uint4, tran *,
                      tran *, hash_table *, stack *);

/****  Private functions.                                        ****/

/**  uint4  compute_prod_hsize(n1, n2)  :  Adjusts (heuristically) the
                     size of the hash table needed for computing the
                     product of an automaton with n1 states by one
                     with n2 states.                               **/

static uint4  compute_prod_hsize(n1, n2)
  uint4  n1, n2;
{
  register uint8  nmin, nmax;

  nmin = 8 * (n1 + n2);
  nmax = (((uint8) n1) * ((uint8) n2));
  nmax = (nmax / 4 + 1) * 5;

  if (nmin > ((uint4) -1))
    nmin = auto_prod_hsize;

  if (nmax > ((uint4) -1))
    nmax = auto_prod_hsize;

  if (!nmin)
    nmin = 1;

  if (!nmax)
    nmax = 1;

  if (auto_prod_hsize > nmax)
    return nmax;

  if (auto_prod_hsize < nmin)
    return nmin;

  return auto_prod_hsize;
}

/** typedef pg_info : Type of the data placed on the exploration stack
		     of the function prod_generate.  The two first
		     fields give the current states of the automaton
		     whose product is being computed. The third field
		     is only relevant for automata on infinite
		     words. It is a flag that is set if and only if a
		     final state of the first automaton has been
		     visited since the last visit in a final state of
		     the second automaton. The fourth field is a mode
		     field; its value is PG_ROOT if the function is
		     invoked for the first time in the current product
		     operation, PG_FIRST if it is called for the first
		     time with the current pair of states, and
		     PG_LEFT, PG_RIGHT or PG_BOTH if transitions from
		     the left operand, right operand or both operands
		     have already been followed from the current pair
		     of states. The values of the next fields are
		     mode-dependent. If mode == PG_ROOT, there is one
		     field that gives a pointer to a location to which
		     the function should return the index of the
		     topmost created state. If mode == PG_FIRST, there
		     are three fields specifying the origin, the
		     label, and the label length of a transition to be
		     created (the label is allocated dynamically and
		     must be freed when the stack shrinks). Otherwise,
		     there are two fields corresponding to the number
		     of transitions that have already been explored in
		     the operands, a third field used when mode ==
		     PG_BOTH for keeping the index of the first
		     transition of non-zero length in the second
		     operand, and a fourth field giving the index of
		     the state of the resulting automaton that
		     corresponds to the current pair of states of the
		     operands.  **/

typedef struct {
  uint4  m1, m2;
  uint1  mode;
  uint1  f1_visited;   /* Flag for Buchi and weak automata */
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
      uint4  length;
    } tr;
    struct {
      uint4  k1, k2, k2first, origin;
    } st;
  } v;
} pg_info;

#define  PG_ROOT   0x01
#define  PG_FIRST  0x02
#define  PG_LEFT   0x03
#define  PG_RIGHT  0x04
#define  PG_BOTH   0x05

/**  int  prod_generate(a1, a2, na1, na2, s1, s2, ar, m1, m2, mp, 
                        ht)  :
                     This routine is part of the synchronous product
                     operation. The two automata whose product is
                     being computed are *a1 and *a2, and the numbers
                     of bytes required for storing one symbol of
                     their alphabets are na1 and na2. The label *s1
		     (resp. *s2) describes a separator that demarcates
		     an area in the structure of the automaton *a1
		     (resp. *a2). These can be NULL, in which case
		     there is no separator. The partially computed
		     product is in *ar, and a hash table associating
		     a state index of *ar to each pair of state
		     indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted 
                     lexicographically.

                     The goal of this function is to generate a
                     part of *ar, starting from the pair of state
                     indices of *a1 and *a2 given by (m1, m2). This
                     routine proceeds transitively, updates the hash
                     table, and sets accepting states. The index of
                     of the state of *ar corresponding to the pair
                     (m1, m2) is returned in *mp. If we deal with
		     automata on infinite words, a state is
		     furthermore characterized by a flag that
		     memorizes if a final state of *a1 has been met
		     since the last visit in a final state of *a2.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  prod_generate(a1, a2, na1, na2, s1, s2, ar, m1, m2, mp, 
                          ht)
  automaton  *a1, *a2, *ar;
  uint1       na1, na2, *s1, *s2;
  uint4       m1, m2, *mp;
  hash_table *ht;
{
  register stack   *st;
  register uint1    m;
           pg_info  p;

  st = stack__new_empty(pg_info);
  if (!st)
    return -1;

  p.m1         = m1;
  p.m2         = m2;
  p.f1_visited = 1;
  p.mode       = PG_ROOT;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((pg_info *) stack__top(st)) -> mode;
      if (((m == PG_ROOT || m == PG_FIRST) && 
          prod_generate_init(a1, a2, na1, na2, ar, ht, st) < 0) ||
          ((m == PG_LEFT || m == PG_RIGHT || m == PG_BOTH) &&
          prod_generate_loop(a1, a2, na1, na2, s1, s2, ar, ht, 
                             st) < 0))
        {
	  while (!stack__is_empty(st))
	    {
	      stack__pop(st, (void *) &p);
	      if (p.mode == PG_FIRST)
		resr__free_objects(p.v.tr.label, uint1, na1 + na2);
	    }

          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  prod_generate_init(a1, a2, na1, na2, ar, ht, st)  :  This
                     function is part of the synchronous product
                     algorithm. The two automata whose product is
                     being computed are *a1 and *a2, and the numbers
                     of bytes required for storing one symbol of their
                     alphabets are na1 and na2. The partially computed
                     product is in *ar, and a hash table associating a
                     state index of *ar to each pair of state indices
                     of *a1 and *a2 is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of states placed on top of the exploration stack
                     *st (supposed non-empty and of mode equal to
                     either PG_ROOT or PG_FIRST), inserting the
                     corresponding entry in the hash table *ht. If we
		     deal with automata on infinite words, a state is
		     furthermore characterized by a flag that
		     memorizes if a final state of *a1 has been met
		     since the last visit in a final state of *a2. It
                     then updates the exploration stack such that the
                     next calls to the function prod_generate_loop
                     will create transitively all the successor states
                     of the new state as well as their outgoing
                     transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  prod_generate_init(a1, a2, na1, na2, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na1, na2;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register pg_info  *p;
  static   uint4     buf_uint4[3];
           void    **r;
 
  p = (pg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  PG_ROOT && p -> mode != PG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;
  buf_uint4[2] = p -> f1_visited;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 2 : 3) 
       * sizeof(uint4), &r, &auto_prod_ncolls, 
       &auto_prod_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 2 : 3) 
       * sizeof(uint4), &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (auto_add_new_state(ar, buf_uint4) < 0)
    return -1;

  *v = buf_uint4[0];

  if (p -> mode == PG_ROOT)
    {
      if (p -> v.return_state)
	*(p -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, p -> v.tr.origin, *v,
        p -> v.tr.length, p -> v.tr.label) < 0)
      return -1;

  if ( (auto_word_type(a1) == AUTO_WORDS_FINITE &&
	/* Final state test for automata on finite words */
	auto_accepting_state(a1, p -> m1) && 
	auto_accepting_state(a2, p -> m2)) ||

       (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
	/* Final state test for Buchi or weak automata */
	auto_accepting_state(a2, p -> m2) &&
	p -> f1_visited) )

    auto_mark_accepting_state(ar, *v);

  if (p -> mode == PG_FIRST)
    resr__free_objects(p -> v.tr.label, uint1, na1 + na2);

  p -> mode        = PG_LEFT;
  p -> v.st.k1     = ZERO_INT4;
  p -> v.st.origin = *v;

  return 0;
}

/**  int  prod_generate_loop(a1, a2, na1, na2, s1, s2, ar, ht, st)  :
                     This function is part of the synchronous product
                     algorithm. The two automata whose product is
                     being computed are *a1 and *a2, and the numbers
                     of bytes required for storing one symbol of their
                     alphabets are na1 and na2. The label *s1
		     (resp. *s2) describes a separator that demarcates
		     an area in the structure of the automaton *a1
		     (resp. *a2). These can be NULL, in which case
		     there is no separator. Transitions labeled by *s1
		     can be crossed in *a1 if and only if a transition
		     labeled by *s2 is simultaneously crossed in *a2.
		     The partially computed product is in *ar, and a
		     hash table associating a state index of *ar to
		     each pair of state indices of *a1 and *a2 is
		     given by *ht.  The outgoing transitions from each
		     state of *a1 and *a2 are supposed to be sorted
		     lexicographically.
 
                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to PG_LEFT, PG_RIGHT
                     or PG_BOTH), updating the stack such that the
                     next calls to this function and to the function
                     prod_generate_init will create transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  prod_generate_loop(a1, a2, na1, na2, s1, s2, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na1, na2, *s1, *s2;
  hash_table *ht;
  stack      *st;
{
  register uint4    k1, k2, k2first, m1, m2, st1;
  register uint1    f1_visited, ms1, ms2;
  register pg_info *p;
  register tran    *t1, *t2;
           uint4    n1, n2;

  p = (pg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != PG_LEFT && p -> mode != PG_RIGHT &&
      p -> mode != PG_BOTH)
    return -1;
#endif

  m1         = p -> m1;
  m2         = p -> m2;
  f1_visited = p -> f1_visited;
  k1         = p -> v.st.k1;
  k2         = p -> v.st.k2;
  k2first    = p -> v.st.k2first;
  st1        = p -> v.st.origin;

  if (auto_nb_out_transitions(a1, m1, &n1) < 0 ||
      auto_nb_out_transitions(a2, m2, &n2) < 0)
    return -1;

  switch (p -> mode)
    {
    case PG_LEFT:
      if (k1 >= n1)
	{
	  p -> mode    = PG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      t1 = auto_transition(a1, m1, k1);
      if (auto_transition_length(t1))
	{
	  p -> mode    = PG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
       p -> v.st.k1++;
      return prod_generate_one(a1, a2, na1, na2, ar, m1, m2, 
          f1_visited, st1, t1, NULL, ht, st);
      
    case PG_RIGHT:
      if (k2 >= n2)
	{
	  stack__pop(st, NULL);
	  return 0;
	}
      t2 = auto_transition(a2, m2, k2);
      if (auto_transition_length(t2))
	{
	  p -> mode = PG_BOTH;
          p -> v.st.k2first = p -> v.st.k2;
	  return 0;
	}
      p -> v.st.k2++;
      return prod_generate_one(a1, a2, na1, na2, ar, m1, m2, 
          f1_visited, st1, NULL, t2, ht, st);
      
    case PG_BOTH:
      if (k1 >= n1)
	{
	  stack__pop(st, NULL);
	  return 0;
	}
      if (k2 >= n2)
	{
	  p -> v.st.k1++;
	  p -> v.st.k2 = k2first;
	  return 0;
	}
      t1 = auto_transition(a1, m1, k1);
      t2 = auto_transition(a2, m2, k2);
      p -> v.st.k2++;

      ms1 = s1 && s2 && auto_transition_length(t1) &&
	!memcmp(auto_transition_label_ptr(t1, na1), s1, na1);
      ms2 = s1 && s2 && auto_transition_length(t2) &&
	!memcmp(auto_transition_label_ptr(t2, na2), s2, na2);

      if ((ms1 && ms2) || (!ms1 && !ms2))
	/* Either no separator has been read in *a1 and *a2,
	   or a separator has been read in both *a1 and *a2 */
	return prod_generate_one(a1, a2, na1, na2, ar, m1, m2,
				 f1_visited, st1, t1, t2, ht, st);

      return 0;

    default:
      return -1;
    }
}

/**  int  prod_generate_one(a1, a2, na1, na2, ar, m1, m2, f1_vis, m,
                     t1, t2, ht, st)  :  This routine is part of the 
                     synchronous product operation. The two automata
                     whose product is being computed are *a1 and *a2,
                     and the numbers of bytes required for storing one
                     symbol of their alphabets are na1 and na2. The
                     partially computed product is in *ar, and a hash
                     table associating a state index of *ar to each
                     pair of state indices of *a1 and *a2 is given by
                     *ht.  The outgoing transitions from each state of
                     *a1 and *a2 are supposed to be sorted
                     lexicographically.

                     The goal of this function is to update the
                     exploration stack *st so as to add to the
                     automaton *ar a new transition outgoing from the
                     state of index m, and corresponding to the pair
                     (m1, m2) of states of *a1 and *a2. This
                     transition can be obtained by composing the two
                     transitions *t1 and *t2 (or only one of them if
                     the argument corresponding to the other one is a
                     NULL pointer). The length of each transition is 1
                     if both of them are given, and 0 if the other
                     transition is not given.

		     If we deal with automata on infinite words, a
		     state is furthermore characterized by the flag
		     f1_vis that memorizes if a final state of *a1
		     has been met since the last visit in a final
		     state of *a2.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  prod_generate_one(a1, a2, na1, na2, ar, m1, m2, f1_vis, 
                              m, t1, t2, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na1, na2;
  uint4       m1, m2;
  uint1       f1_vis;
  uint4       m;
  tran       *t1, *t2;
  hash_table *ht;
  stack      *st;
{
  register uint1   *buffer;
  register uint4    length, rc;
  static   uint4    dest[3];
           pg_info  p;
           void    *r;

  buffer = resr__new_objects(uint1, na1 + na2);
  if (!buffer)
    return -1;

  if (t1 && t2)
    {
      memcpy(buffer, auto_transition_label_ptr(t1, na1), na1);  
      memcpy(buffer + na1, auto_transition_label_ptr(t2, na2), na2);
      length = 1;
    }
  else
    length = ZERO_INT4;
  
  dest[0] = t1 ? auto_transition_dest(t1) : m1;
  dest[1] = t2 ? auto_transition_dest(t2) : m2;

  if (auto_word_type(a1) == AUTO_WORDS_FINITE)
    {
      dest[2] = 1;   /* Not necessary */
      r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
    }

  else
    {
      dest[2] = ( auto_accepting_state(a1, dest[0]) ||
		  (f1_vis && !auto_accepting_state(a2, m2)) );
      r = hash__lookup_bytes(ht, (uint1 *) dest, 3 * sizeof(uint4));
    }

  if (r)
    {
      rc = auto_add_new_transition(ar, m, *((uint4 *) r), length,
          buffer);
      resr__free_objects(buffer, uint1, na1 + na2);
      return rc;
    }

  p.mode        = PG_FIRST;
  p.m1          = dest[0];
  p.m2          = dest[1];
  p.f1_visited  = dest[2];
  p.v.tr.origin = m;
  p.v.tr.label  = buffer;
  p.v.tr.length = length;

  if (stack__push(st, (void *) &p) < 0)
    {
      resr__free_objects(buffer, uint1, na1 + na2);
      return -1;
    }

  return 0;
}

/**  automaton *product(a1, a2, n1, n2, na1, na2, s1, s2)  :  
                     Computes the synchronous product of the two
                     finite-state automata on (in)finite words *a1 and
                     *a2. Those two automata are supposed to be in
                     normal form, and the outgoing transitions from
                     each state are supposed to be sorted
                     lexicographically.  Their number of states and
                     size of alphabets are given by n1, n2, na1 and
                     na2. The values of these parameters are supposed
                     to not exceed the bounds imposed by the
                     representation of automata.

		     The label *s1 describes a separator that
		     demarcates an area in the structure of the
		     automaton *a1 which can be crossed if and only
		     if a transition labeled by *s2 is simultaneously
		     crossed in *a2. *s1 and *s2 can be NULL, in
		     which case there is no separator.

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *product(a1, a2, n1, n2, na1, na2, s1, s2)
  automaton *a1, *a2;
  uint4      n1, n2;
  uint1      na1, na2, *s1, *s2;
{
  register uint4       i, j, k1, k2;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  ar = auto_new_empty(na1 + na2);
  if (!ar)
    return NULL;

  if (auto_word_type(a1) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_BUCHI;
    }

  ht = hash__new_empty(compute_prod_hsize(n1, n2));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      {
        if (auto_i_state(a1, i, dest) < 0 ||
  	    auto_i_state(a2, j, dest + 1) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
                AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
	        (void (*)(void *)) uint4__free);
             return NULL;
	  }
	r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
        if (r)
	  m = *((uint4 *) r);
	else
	  if (prod_generate(a1, a2, na1, na2, s1, s2, ar, dest[0], 
              dest[1], &m, ht) < 0)
	    {
	      auto_free(ar);
	      bytes__prepare_free((auto_word_type(a1) == 
                  AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	      hash__free(ht, (void (*)(void *)) bytes__free,
	          (void (*)(void *)) uint4__free);
	      return NULL;
	    }
	if (auto_add_new_i_state(ar, m) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
                AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
	      (void (*)(void *)) uint4__free);
            return NULL;
  	  }
      }
 
  bytes__prepare_free((auto_word_type(a1) == AUTO_WORDS_INFINITE
		       ? 3 : 2) * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
      (void (*)(void *)) uint4__free);
  
  return ar;
}

/**  typedef ig_info  :  Type of the data placed of the exploration
                     stack of the function inter_generate.  The two
                     first fields give the current states of the
                     automaton whose intersection is being computed.
                     The second field is a mode field; its value is
                     IG_ROOT if the function is invoked for the first
                     time in the current intersection operation,
                     IG_FIRST if it is called for the first time with
                     the current pair of states, and IG_LEFT, IG_RIGHT
                     or IG_BOTH if transitions from the left operand,
                     right operand or both operands have already been
                     followed from the current pair of states. The
                     values of the next fields are mode-dependent. If
                     mode == IG_ROOT, there is one field that gives a
                     pointer to a location to which the function
                     should return the index of the topmost created
                     state. If mode == IG_FIRST, there are three
                     fields specifying the origin, the label, and the
                     label length of a transition to be created (the
                     label is allocated statically and does not have
                     to be freed when the stack shrinks). Otherwise,
                     there are two fields corresponding to the number
                     of transitions that have already been explored in
                     the operands, a third field giving the index of
                     the first transition of the second operand that
                     matches the current transition of the first
                     operand, and a fourth field giving the index
                     of the state of the resulting automaton that
                     corresponds to the current pair of states of the
                     operands.

                     Furthermore, if we deal with Buchi or weak
		     automata, there is a flag that memorizes the fact
		     that the current path has visited a state of the
		     final states set of *a1 since its last visit to
		     the final states set of *a2.                  **/

typedef struct {
  uint4  m1, m2;
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
      uint4  length;
    } tr;
    struct {
      uint4  k1, k2, k2first, origin;
    } st;
  } v;
  uint1  f1_visited;   /* Flag for Buchi and weak automata */
} ig_info;

#define  IG_ROOT   0x01
#define  IG_FIRST  0x02
#define  IG_LEFT   0x03
#define  IG_RIGHT  0x04
#define  IG_BOTH   0x05

/**  int  inter_generate(a1, a2, na, ar, m1, m2, mp, ht)  :  This 
                     routine is part of the intersection
                     operation. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     intersection is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.

                     The goal of this function is to generate a
                     part of *ar, starting from the pair of state
                     indices of *a1 and *a2 given by (m1, m2). This
                     routine proceeds transitively, updates the hash
                     table, and sets accepting states. The index of
                     of the state of *ar corresponding to the pair
                     (m1, m2) is returned in *mp. If we deal with
		     automata on infinite words, a state is
		     furthermore characterized by a flag that
		     memorizes if a final state of *a1 has been met
		     since the last visit in a final state of *a2.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inter_generate(a1, a2, na, ar, m1, m2, mp, ht)
  automaton  *a1, *a2, *ar;
  uint1       na;
  uint4       m1, m2, *mp;
  hash_table *ht;
{
  register stack   *st;
  register uint1    m;
           ig_info  p;

  st = stack__new_empty(ig_info);
  if (!st)
    return -1;

  p.m1         = m1;
  p.m2         = m2;
  p.f1_visited = 1;
  p.mode       = IG_ROOT;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((ig_info *) stack__top(st)) -> mode;
      if (((m == IG_ROOT || m == IG_FIRST) && 
          inter_generate_init(a1, a2, na, ar, ht, st) < 0) ||
          ((m == IG_LEFT || m == IG_RIGHT || m == IG_BOTH) &&
          inter_generate_loop(a1, a2, na, ar, ht, st) < 0))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  inter_generate_init(a1, a2, na1, na2, ar, ht, st)  :  This
                     function is part of the intersection
                     algorithm. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     intersection is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of states placed on top of the exploration stack
                     *st (supposed non-empty and of mode equal to
                     either IG_ROOT or IG_FIRST), inserting the
                     corresponding entry in the hash table *ht. If we
		     deal with automata on infinite words, a state is
		     furthermore characterized by a flag that
		     memorizes if a final state of *a1 has been met
		     since the last visit in a final state of *a2. It
                     then updates the exploration stack such that the
                     next calls to the function inter_generate_loop
                     will create transitively all the successor states
                     of the new state as well as their outgoing
                     transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inter_generate_init(a1, a2, na, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register ig_info  *p;
  static   uint4     buf_uint4[3];
           void    **r;
 
  p = (ig_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  IG_ROOT && p -> mode != IG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;
  buf_uint4[2] = p -> f1_visited;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 2 : 3) * 
          sizeof(uint4), &r, &auto_prod_ncolls, &auto_prod_nins) 
      < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 2 : 3) * 
          sizeof(uint4), &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (auto_add_new_state(ar, buf_uint4) < 0)
    return -1;

  *v = buf_uint4[0];

  if (p -> mode == IG_ROOT)
    {
      if (p -> v.return_state)
	*(p -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, p -> v.tr.origin, *v,
        p -> v.tr.length, p -> v.tr.label) < 0)
      return -1;

  if ( (auto_word_type(a1) == AUTO_WORDS_FINITE &&
	auto_accepting_state(a1, p -> m1) && 
	auto_accepting_state(a2, p -> m2)) ||

       /* Final state test for Buchi automata */
       (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
	auto_accepting_state(a2, p -> m2) &&
	p -> f1_visited) )
    auto_mark_accepting_state(ar, *v);

  p -> mode        = IG_LEFT;
  p -> v.st.k1     = ZERO_INT4;
  p -> v.st.origin = *v;

  return 0;
}

/**  int  inter_generate_loop(a1, a2, na1, na2, ar, ht, st)  :  This
                     function is part of the intersection
                     algorithm. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     intersection is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.
 
                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to IG_LEFT, IG_RIGHT
                     or IG_BOTH), updating the stack such that the
                     next calls to this function and to the function
                     inter_generate_init will create transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  inter_generate_loop(a1, a2, na, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    m1, m2, st1;
  register uint1    f1_visited;
  register int      c;
  register ig_info *p;
  register tran    *t1, *t2;
           uint4    n1, n2;

  p = (ig_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != IG_LEFT && p -> mode != IG_RIGHT &&
      p -> mode != IG_BOTH)
    return -1;
#endif

  m1          = p -> m1;
  m2          = p -> m2;
  f1_visited  = p -> f1_visited;
  st1         = p -> v.st.origin;

  if (auto_nb_out_transitions(a1, m1, &n1) < 0 ||
      auto_nb_out_transitions(a2, m2, &n2) < 0)
    return -1;

  switch (p -> mode)
    {
    case IG_LEFT:
      if (p -> v.st.k1 >= n1)
	{
	  p -> mode    = IG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      t1 = auto_transition(a1, m1, p -> v.st.k1);
      if (auto_transition_length(t1))
	{
	  p -> mode    = IG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      p -> v.st.k1++;

      return inter_generate_one(a1, a2, na, ar, m1, m2, f1_visited,
          st1, t1, NULL, ht, st);
      
    case IG_RIGHT:
      if (p -> v.st.k2 >= n2)
	{
	  stack__pop(st, NULL);
	  return 0;
	}
      t2 = auto_transition(a2, m2, p -> v.st.k2);
      if (auto_transition_length(t2))
	{
	  p -> mode = IG_BOTH;
	  p -> v.st.k2first = p -> v.st.k2;
	  return 0;
	}
      p -> v.st.k2++;

      return inter_generate_one(a1, a2, na, ar, m1, m2, f1_visited,
          st1, NULL, t2, ht, st);
      
    case IG_BOTH:
      auto_transition_cmp_prepare(a1);
      while (p -> v.st.k1 < n1)
	{
	  if (p -> v.st.k2 >= n2)
	    {
	      p -> v.st.k1++;
	      p -> v.st.k2 = p -> v.st.k2first;
	      continue;
	    }

	  t1 = auto_transition(a1, m1, p -> v.st.k1);
	  t2 = auto_transition(a2, m2, p -> v.st.k2);
	  c  = auto_transition_cmp(t1, t2);
	  if (c < 0)
	    {
	      p -> v.st.k1++;
	      p -> v.st.k2 = p -> v.st.k2first;
	      continue;
	    }
	  if (c > 0)
	    {
	      p -> v.st.k2first = ++(p -> v.st.k2);
	      continue;
	    }
	  p -> v.st.k2++;

	  return inter_generate_one(a1, a2, na, ar, m1, m2, 
              f1_visited, st1, t1, t2, ht, st);
	}

      stack__pop(st, NULL);
      return 0;

    default:
      return -1;
    }
}

/**  int  inter_generate_one(a1, a2, na, ar, m1, m2, f1_vis, m, t1, 
                     t2, ht, st)  :  
                     This function is part of the intersection
                     algorithm. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     intersection is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.

                     The goal of this function is to update the
                     exploration stack *st so as to add to the
                     automaton *ar a new transition outgoing from the
                     state of index m, and corresponding to the pair
                     (m1, m2) of states of *a1 and *a2 (resp.
		     (m1, m2, f1_vis) for Buchi automata). This
                     transition can be obtained by composing the two
                     transitions *t1 and *t2 (or only one of them if
                     the argument corresponding to the other one is a
                     NULL pointer). The length of each transition is 1
                     if both of them are given (in which case they are
                     known to be labeled by the same symbol), and 0 if
                     the other transition is not given.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inter_generate_one(a1, a2, na, ar, m1, m2, f1_vis, m, t1, 
    t2, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  uint4       m1, m2;
  uint1       f1_vis;
  uint4       m;
  tran       *t1, *t2;
  hash_table *ht;
  stack      *st;
{
  register uint4    length;
  static   uint4    dest[3];
           ig_info  p;
           void    *r;

  length  = (t1 && t2) ? 1 : ZERO_INT4;
  dest[0] = t1 ? auto_transition_dest(t1) : m1;
  dest[1] = t2 ? auto_transition_dest(t2) : m2;

  if (auto_word_type(a1) == AUTO_WORDS_FINITE)
    {
      dest[2] = 1;   /* Not necessary */
      r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
    }

  else
    {
      dest[2] = ( auto_accepting_state(a1, dest[0]) ||
		  (f1_vis && !auto_accepting_state(a2, m2)) );
      r = hash__lookup_bytes(ht, (uint1 *) dest, 3 * sizeof(uint4));
    }

  if (r)
    return auto_add_new_transition(ar, m, *((uint4 *) r), length,
        auto_transition_label_ptr(t1, na));

  p.mode        = IG_FIRST;
  p.m1          = dest[0];
  p.m2          = dest[1];
  p.f1_visited  = dest[2];
  p.v.tr.origin = m;
  p.v.tr.label  = auto_transition_label_ptr(t1, na);
  p.v.tr.length = length;

  if (stack__push(st, (void *) &p) < 0)
    return -1;

  return 0;
}

/**  automaton *intersection(a1, a2, n1, n2, na)  :  Computes the
                     intersection of the two finite-state automata on
                     finite or infinite words *a1 and *a2. Those two
		     automata are supposed to be in normal form, and
		     the outgoing transitions from each state are
		     supposed to be sorted lexicographically.  Their
		     number of states are given by n1, n2.  The number
		     of bytes required for storing one symbol of their
		     alphabet is na.

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *intersection(a1, a2, n1, n2, na)
  automaton *a1, *a2;
  uint4      n1, n2;
  uint1      na;
{
  register uint4       i, j, k1, k2;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  ar = auto_new_empty(na);
  if (!ar)
    return NULL;

  if (auto_word_type(a1) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_BUCHI;
    }

  ht = hash__new_empty(compute_prod_hsize(n1, n2));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      {
        if (auto_i_state(a1, i, dest) < 0 ||
  	    auto_i_state(a2, j, dest + 1) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
                AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
                (void (*)(void *)) uint4__free);
	    return NULL;
	  }
	r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
        if (r)
	  m = *((uint4 *) r);
	else
	  if (inter_generate(a1, a2, na, ar, dest[0], dest[1], &m, ht)
              < 0)
	    {
	      auto_free(ar);
	      bytes__prepare_free((auto_word_type(a1) == 
                  AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	      hash__free(ht, (void (*)(void *)) bytes__free,
                  (void (*)(void *)) uint4__free);
	      return NULL;
	    }
	if (auto_add_new_i_state(ar, m) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
                AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
                (void (*)(void *)) uint4__free);
            return NULL;
  	  }
      }
 
  bytes__prepare_free((auto_word_type(a1) == 
      AUTO_WORDS_INFINITE ? 3 : 2) * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
      (void (*)(void *)) uint4__free);
  
  return ar;
}

/**  typedef eg_info  :  Type of the data placed of the exploration
                     stack of the function empty_inter_generate.  The
                     first two fields give the current states of the
                     automata whose intersection is being checked.
                     The third field is a mode field; its value is
                     EG_ROOT if the function is invoked for the first
                     time in the current intersection operation,
                     EG_FIRST if it is called for the first time with
                     the current pair of states, and EG_LEFT, EG_RIGHT
                     or EG_BOTH if transitions from the left operand,
                     right operand or both operands have already been
                     followed from the current pair of states. The
                     next two fields are only used if the mode is
                     equal to EG_BOTH and correspond to the number of
                     transitions that have already been explored in
                     the operands. The last field gives the index of
                     the first transition of the second operand that
                     matches the current transition of the first
                     operand.                                      **/

typedef struct {
  uint4  m1, m2;
  uint1  mode;
  uint4  k1, k2, k2first;
} eg_info;

#define  EG_ROOT   0x01
#define  EG_FIRST  0x02
#define  EG_LEFT   0x03
#define  EG_RIGHT  0x04
#define  EG_BOTH   0x05

/**  int  empty_inter_generate(a1, a2, na, m1, m2, ht)  :  This 
                     routine is part of the intersection emptiness
                     check operation. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. A hash table storing each pair
                     of state indices of *a1 and *a2 that have already
                     been visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.

                     The goal of this function is to explore a part of
                     the intersection, starting from the pair of state
                     indices of *a1 and *a2 given by (m1, m2). This
                     routine proceeds transitively, updating the hash
                     table.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns a Boolean value.    **/

static int  empty_inter_generate(a1, a2, na, m1, m2, ht)
  automaton  *a1, *a2;
  uint1       na;
  uint4       m1, m2;
  hash_table *ht;
{
  register stack   *st;
  register uint1    m;
  register int      rc;
           eg_info  p;

  st = stack__new_empty(eg_info);
  if (!st)
    return -1;

  p.m1   = m1;
  p.m2   = m2;
  p.mode = EG_ROOT;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((eg_info *) stack__top(st)) -> mode;
      switch(m)
	{
	case EG_ROOT:
	case EG_FIRST:
	  rc = empty_inter_generate_init(a1, a2, na, ht, st);
	  if (rc <= 0)
	    {
	      stack__free(st);
	      return rc;
	    }
	  continue;

	case EG_LEFT:
	case EG_RIGHT:
	case EG_BOTH:
	  if (empty_inter_generate_loop(a1, a2, na, ht, st) < 0)
	    {
	      stack__free(st);
	      return -1;
	    }
	  continue;
	default:
	  stack__free(st);
	  return -1;
	}
    }

  stack__free(st);
  return 1;
}

/**  int  empty_inter_generate_init(a1, a2, na, ht, st)  :  This
                     function is part of the intersection emptiness 
                     check algorithm. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. A hash table storing each pair
                     of state indices of *a1 and *a2 that have already
                     been visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.
 
                     The goal of this function is to explore the
                     intersection of *a1 and *a2, starting from the
                     state corresponding to the pair of states placed
                     on top of the exploration stack *st (supposed
                     non-empty and of mode equal to either EG_ROOT or
                     EG_FIRST), inserting the corresponding entry in
                     the hash table *ht. It then updates the
                     exploration stack such that the next calls to the
                     function empty_inter_generate_loop will explore
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns a Boolean value (0
                     meaning that the intersection is not empty, 1
                     meaning that it is either empty or non-empty).
                                                                   **/

static int  empty_inter_generate_init(a1, a2, na, ht, st)
  automaton  *a1, *a2;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register eg_info  *p;
  static   uint4     buf_uint4[2];
           void    **r;
 
  p = (eg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  EG_ROOT && p -> mode != EG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &auto_prod_ncolls, &auto_prod_nins) < 0)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0)
#endif  /* >= 1 */
    return -1;

  if (!r)
    {
      stack__pop(st, NULL);
      return 1;
    }

  *r = NULL;

  if (auto_accepting_state(a1, p -> m1) && 
      auto_accepting_state(a2, p -> m2))
    return 0;

  p -> mode = EG_LEFT;
  p -> k1   = ZERO_INT4;

  return 1;
}

/**  int  empty_inter_generate_loop(a1, a2, na, ht, st)  :  This
                     function is part of the intersection emptiness
                     check algorithm. The two automata that are being
                     intersected are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. A hash table storing each pair
                     of state indices of *a1 and *a2 that have already
                     been visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.
 
                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to EG_LEFT, EG_RIGHT
                     or EG_BOTH), updating the stack such that the
                     next calls to this function and to the function
                     inter_generate_init will explore transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  empty_inter_generate_loop(a1, a2, na, ht, st)
  automaton  *a1, *a2;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    m1, m2;
  register int      c;
  register eg_info *p;
           eg_info  q;
  register tran    *t1, *t2;
           uint4    n1, n2;

  p = (eg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != EG_LEFT && p -> mode != EG_RIGHT &&
      p -> mode != EG_BOTH)
    return -1;
#endif

  m1 = p -> m1;
  m2 = p -> m2;

  if (auto_nb_out_transitions(a1, m1, &n1) < 0 ||
      auto_nb_out_transitions(a2, m2, &n2) < 0)
    return -1;

  switch (p -> mode)
    {
    case EG_LEFT:
      if (p -> k1 >= n1)
	{
	  p -> mode = EG_RIGHT; 
	  p -> k2   = ZERO_INT4;
	  return 0;
	}
      t1 = auto_transition(a1, m1, p -> k1);
      if (auto_transition_length(t1))
	{
	  p -> mode = EG_RIGHT; 
	  p -> k2   = ZERO_INT4;
	  return 0;
	}
      p -> k1++;

      q.m1   = auto_transition_dest(t1);
      q.m2   = m2;
      q.mode = EG_FIRST;

      if (stack__push(st, (void *) &q) < 0)
        return -1;
      return 0;
      
    case EG_RIGHT:
      if (p -> k2 >= n2)
	{
	  stack__pop(st, NULL);
	  return 0;
	}
      t2 = auto_transition(a2, m2, p -> k2);
      if (auto_transition_length(t2))
	{
	  p -> mode    = EG_BOTH;
	  p -> k2first = p -> k2;
	  return 0;
	}
      p -> k2++;

      q.m1   = m1;
      q.m2   = auto_transition_dest(t2);
      q.mode = EG_FIRST;

      if (stack__push(st, (void *) &q) < 0)
        return -1;
      return 0;
      
    case EG_BOTH:
      auto_transition_cmp_prepare(a1);
      while (p -> k1 < n1)
	{
	  if (p -> k2 >= n2)
	    {
	      p -> k1++;
	      p -> k2 = p -> k2first;
	      continue;
	    }

	  t1 = auto_transition(a1, m1, p -> k1);
	  t2 = auto_transition(a2, m2, p -> k2);
	  c  = auto_transition_cmp(t1, t2);
	  if (c < 0)
	    {
	      p -> k1++;
	      p -> k2 = p -> k2first;
	      continue;
	    }
	  if (c > 0)
	    {
	      p -> k2first = ++(p -> k2);
	      continue;
	    }
	  p -> k2++;

	  q.m1   = auto_transition_dest(t1);
	  q.m2   = auto_transition_dest(t2);
	  q.mode = EG_FIRST;

	  if (stack__push(st, (void *) &q) < 0)
	    return -1;
	  return 0;
	}

      stack__pop(st, NULL);
      return 0;

    default:
      return -1;
    }
}

/**  int  empty_intersection(a1, a2, n1, n2, na)  :  Checks whether
                     the intersection of the two finite-state
                     automata on finite words *a1 and *a2 accepts an
                     empty language. Those two automata are supposed
                     to be in normal form, and the outgoing
                     transitions from each state are supposed to be
                     sorted lexicographically.  Their number of
                     states are given by n1, n2.  The number of
                     bytes required for storing one symbol of their
                     alphabet is na.

                     In the case of an error, this function returns 
                     -1.                                           **/

static  int empty_intersection(a1, a2, n1, n2, na)
  automaton *a1, *a2;
  uint4      n1, n2;
  uint1      na;
{
  register uint4       i, j, k1, k2;
  register hash_table *ht;
  register int         e;
           uint4       dest[2];

  ht = hash__new_empty(compute_prod_hsize(n1, n2));
  if (!ht)
    return -1;

  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      {
        if (auto_i_state(a1, i, dest) < 0 ||
  	    auto_i_state(a2, j, dest + 1) < 0)
	  {
	    bytes__prepare_free(2 * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free, NULL);
	    return -1;
	  }
	if (!hash__lookup_bytes(ht, (uint1 *) dest, 
            2 * sizeof(uint4)))
	  {
	    e = empty_inter_generate(a1, a2, na, dest[0], dest[1],
                ht);
	    if (e <= 0)
	      {
		bytes__prepare_free(2 * sizeof(uint4));
	        hash__free(ht, (void (*)(void *)) bytes__free,
	            NULL);
	        return e;
	      }
	  }
      }
 
  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free, NULL);
  
  return 1;
}

/**  typedef ug_info  :  Type of the data placed of the exploration
                     stack of the function union_generate.  The two
                     first fields give the current states of the
                     automaton whose union is being computed.
                     The second field is a mode field; its value is
                     UG_ROOT if the function is invoked for the first
                     time in the current union operation,
                     UG_FIRST if it is called for the first time with
                     the current pair of states, and UG_LEFT, UG_RIGHT
                     or UG_BOTH if transitions from the left operand,
                     right operand or both operands have already been
                     followed from the current pair of states. The
                     values of the next fields are mode-dependent. If
                     mode == UG_ROOT, there is one field that gives a
                     pointer to a location to which the function
                     should return the index of the topmost created
                     state. If mode == UG_FIRST, there are three
                     fields specifying the origin, the label, and the
                     label length of a transition to be created (the
                     label is allocated statically and does not have
                     to be freed when the stack shrinks). Otherwise,
                     there are two fields corresponding to the number
                     of transitions that have already been explored in
                     the operands, a third field giving the index of
                     the first transition of the second operand that
                     matches the current transition of the first
                     operand, and a fourth field giving the index
                     of the state of the resulting automaton that
                     corresponds to the current pair of states of the
                     operands.                                     **/

typedef struct {
  uint4  m1, m2;
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
      uint4  length;
    } tr;
    struct {
      uint4  k1, k2, k2first, origin;
    } st;
  } v;
} ug_info;

#define  UG_ROOT     0x01
#define  UG_FIRST    0x02
#define  UG_LEFT     0x03
#define  UG_RIGHT    0x04
#define  UG_BOTH     0x05

/**  UNION_GAP is a symbolic representation for a gap state
     added to *a1 or *a2 when computing the union of *a1 and *a2. */

#define  UNION_GAP   (uint4) -1

/**  int  union_generate(a1, a2, na, ar, m1, m2, mp, ht)  :  This 
                     routine is part of the union operation. The two
		     automata that are being merged are *a1 and *a2,
		     and the number of bytes required for storing one
		     symbol of their alphabets is na. The partially
		     computed union is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.

                     The goal of this function is to generate a
                     part of *ar, starting from the pair of state
                     indices of *a1 and *a2 given by (m1, m2). This
                     routine proceeds transitively, updates the hash
                     table, and sets accepting states. The index of
                     of the state of *ar corresponding to the pair
                     (m1, m2) is returned in *mp.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  union_generate(a1, a2, na, ar, m1, m2, mp, ht)
  automaton  *a1, *a2, *ar;
  uint1       na;
  uint4       m1, m2, *mp;
  hash_table *ht;
{
  register stack   *st;
  register uint1    m;
           ug_info  p;

  st = stack__new_empty(ug_info);
  if (!st)
    return -1;

  p.m1         = m1;
  p.m2         = m2;
  p.mode       = UG_ROOT;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((ug_info *) stack__top(st)) -> mode;
      if (((m == UG_ROOT || m == UG_FIRST) && 
          union_generate_init(a1, a2, na, ar, ht, st) < 0) ||
          ((m == UG_LEFT || m == UG_RIGHT || m == UG_BOTH) &&
          union_generate_loop(a1, a2, na, ar, ht, st) < 0))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  union_generate_init(a1, a2, na1, na2, ar, ht, st)  :  This
                     function is part of the union
                     algorithm. The two automata that are being
                     merged are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     union automaton is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of states placed on top of the exploration stack
                     *st (supposed non-empty and of mode equal to
                     either UG_ROOT or UG_FIRST), inserting the
                     corresponding entry in the hash table *ht. It
                     then updates the exploration stack such that the
                     next calls to the function union_generate_loop
                     will create transitively all the successor states
                     of the new state as well as their outgoing
                     transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  union_generate_init(a1, a2, na, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register ug_info  *p;
  static   uint4     buf_uint4[2];
           void    **r;
 
  p = (ug_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  UG_ROOT && p -> mode != UG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &auto_prod_ncolls, &auto_prod_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (auto_add_new_state(ar, buf_uint4) < 0)
    return -1;

  *v = buf_uint4[0];

  if (p -> mode == UG_ROOT)
    {
      if (p -> v.return_state)
	*(p -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, p -> v.tr.origin, *v,
        p -> v.tr.length, p -> v.tr.label) < 0)
      return -1;

  if ( (p -> m1 != UNION_GAP && auto_accepting_state(a1, p -> m1)) ||
       (p -> m2 != UNION_GAP && auto_accepting_state(a2, p -> m2)) )
    auto_mark_accepting_state(ar, *v);

  p -> mode        = UG_LEFT;
  p -> v.st.k1     = ZERO_INT4;
  p -> v.st.origin = *v;

  return 0;
}

/**  int  union_generate_loop(a1, a2, na1, na2, ar, ht, st)  :  This
                     function is part of the union
                     algorithm. The two automata that are being
                     merged are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     union automaton is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.
 
                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to UG_LEFT, UG_RIGHT
                     or UG_BOTH), updating the stack such that the
                     next calls to this function and to the function
                     union_generate_init will create transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  union_generate_loop(a1, a2, na, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    m1, m2, st1, i1, i2;
  register int      c;
  register ug_info *p;
  register tran    *t1, *t2;
           uint4    n1, n2;

  p = (ug_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != UG_LEFT && p -> mode != UG_RIGHT &&
      p -> mode != UG_BOTH)
    return -1;
#endif

  m1          = p -> m1;
  m2          = p -> m2;
  st1         = p -> v.st.origin;

  if (m1 == UNION_GAP)
    n1 = 0;
  else
    {
      if (auto_nb_out_transitions(a1, m1, &n1) < 0)
	return -1;
    }

  if (m2 == UNION_GAP)
    n2 = 0;
  else
    {
      if (auto_nb_out_transitions(a2, m2, &n2) < 0)
	return -1;
    }

  switch (p -> mode)
    {
    case UG_LEFT:
   /* Eliminates epsilon transitions from state p->v.st.k1 in *a1. */
      if (p -> v.st.k1 >= n1)
	{
	  p -> mode    = UG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      t1 = auto_transition(a1, m1, p -> v.st.k1);
      if (auto_transition_length(t1))
	{
   /* All epsilon transitions from p->v.st.k1 have been treated. */
	  p -> mode    = UG_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      p -> v.st.k1++;

      return union_generate_one(a1, a2, na, ar, m1, m2,
          st1, t1, NULL, ht, st);
      
    case UG_RIGHT:
   /* Eliminates epsilon transitions from state p->v.st.k2 in *a2. */
      if (p -> v.st.k2 >= n2)
	{
	  if (p -> v.st.k1 >= n1)
	    stack__pop(st, NULL);
	  else
	    {
	      p -> mode = UG_BOTH;
	      p -> v.st.k2first = n2;
	    }
	  return 0;
	}
      t2 = auto_transition(a2, m2, p -> v.st.k2);
      if (auto_transition_length(t2))
	{
	  /* All epsilon transitions treated : preparing for treating
	     non-epsilon ones. */
	  p -> mode = UG_BOTH;
	  p -> v.st.k2first = p -> v.st.k2;
	  return 0;
	}
      p -> v.st.k2++;

      return union_generate_one(a1, a2, na, ar, m1, m2,
          st1, NULL, t2, ht, st);
      
    case UG_BOTH:
      /* Merging the outgoing non-epsilon transitions. */
      if (p -> v.st.k1 >= n1 && p -> v.st.k2 >= n2)
	{
	  stack__pop(st, NULL);
	  return 0;
	}

      auto_transition_cmp_prepare(a1);
      t1 = (p -> v.st.k1 < n1) ? 
          auto_transition(a1, m1, p -> v.st.k1) : NULL;
      t2 = (p -> v.st.k2 < n2) ? 
          auto_transition(a2, m2, p -> v.st.k2) : NULL;
      
      if (t1 && t2)
	c = auto_transition_cmp(t1, t2);
      else if (t1)
	c = -1;
      else
	c = 1;

      i1 = p -> v.st.k1;
      i2 = p -> v.st.k2;

      if (c < 0)
	{
	  i1 ++;
	  t2 = NULL;
	}

      else if (c > 0)
	{
	  i2 ++;
	  if ( i2 < n2 &&
	       auto_transition_cmp(auto_transition(a2, m2, i2), t2)
                   > 0 )
	    p -> v.st.k2first = i2;
	  t1 = NULL;
	}

      else
	{
	  if ( i2 + 1 < n2 && 
	       auto_transition_cmp(auto_transition(a2, m2, i2+1), t2)
                   == 0 )
	    i2 ++;

	  else if ( i1 + 1 < n1 && auto_transition_cmp(auto_transition
		                     (a1, m1, i1+1), t2) == 0 )
	    {
	      i1 ++;
	      i2 = p -> v.st.k2first;
	    }

	  else
	    {
	      i1 ++;
	      i2 ++;
	      p -> v.st.k2first = i2;
	    }
	}

      p -> v.st.k1 = i1;
      p -> v.st.k2 = i2;

      return union_generate_one(a1, a2, na, ar, t1 ? m1 : UNION_GAP, 
				t2 ? m2 : UNION_GAP, st1, t1, t2, ht,
                                st);
    default:
      return -1;
    }
}

/**  int  union_generate_one(a1, a2, na, ar, m1, m2, m, t1, t2,
                     ht, st)  :  This function is part of the
                     union algorithm. The two automata that are
                     being merged are *a1 and *a2, and the number
                     of bytes required for storing one symbol of their
                     alphabets is na. The partially computed
                     union automaton is in *ar, and a hash table
                     associating a state index of *ar to each pair of
                     state indices of *a1 and *a2 is given by *ht.
                     The outgoing transitions from each state of *a1
                     and *a2 are supposed to be sorted
                     lexicographically.

                     The goal of this function is to update the
                     exploration stack *st so as to add to the
                     automaton *ar a new transition outgoing from the
                     state of index m, and corresponding to the pair
                     (m1, m2) of states of *a1 and *a2. This
                     transition can be obtained by composing the two
                     transitions *t1 and *t2 (or only one of them if
                     the argument corresponding to the other one is a
                     NULL pointer). The length of each transition is 1
                     if both of them are given (in which case they are
                     known to be labeled by the same symbol), and 0 if
                     the other transition is not given.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  union_generate_one(a1, a2, na, ar, m1, m2, m, t1, 
    t2, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na;
  uint4       m1, m2;
  uint4       m;
  tran       *t1, *t2;
  hash_table *ht;
  stack      *st;
{
  register uint4    length;
  static   uint4    dest[2];
           ug_info  p;
           void    *r;

  dest[0] = t1 ? auto_transition_dest(t1) : m1;
  dest[1] = t2 ? auto_transition_dest(t2) : m2;

  if (dest[0] == UNION_GAP && dest[1] == UNION_GAP)
    return 0;

  r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));

  if (t1 && t2)
    length = 1;
  else
    length = t1 ? auto_transition_length(t1) : 
        auto_transition_length(t2);

  if (r)
    return auto_add_new_transition(ar, m, *((uint4 *) r), length,
        auto_transition_label_ptr(t1 ? t1 : t2, na));

  p.mode        = UG_FIRST;
  p.m1          = dest[0];
  p.m2          = dest[1];
  p.v.tr.origin = m;
  p.v.tr.label  = auto_transition_label_ptr(t1 ? t1 : t2, na);
  p.v.tr.length = length;

  if (stack__push(st, (void *) &p) < 0)
    return -1;

  return 0;
}

/**  automaton *union_proc(a1, a2, n1, n2, na)  :  Computes the
                     union of the two finite-state automata on
                     finite or infinite words *a1 and *a2. Those two
		     automata are supposed to be in normal form, and
		     the outgoing transitions from each state are
		     supposed to be sorted lexicographically.  Their
		     number of states are given by n1, n2.  The
		     number of bytes required for storing one symbol
		     of their alphabet is na.

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *union_proc(a1, a2, n1, n2, na)
  automaton *a1, *a2;
  uint4      n1, n2;
  uint1      na;
{
  register uint4       i, j, k1, k2;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  ar = auto_new_empty(na);
  if (!ar)
    return NULL;

  if (auto_word_type(a1) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_BUCHI;
    }

  ht = hash__new_empty(compute_prod_hsize(n1, n2));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      {
        if (auto_i_state(a1, i, dest) < 0 ||
  	    auto_i_state(a2, j, dest + 1) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free(2 * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
                (void (*)(void *)) uint4__free);
             return NULL;
	  }
	r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
        if (r)
	  m = *((uint4 *) r);
	else
	  if (union_generate(a1, a2, na, ar, dest[0], dest[1], &m, ht)
              < 0)
	    {
	      auto_free(ar);
	      bytes__prepare_free(2 * sizeof(uint4));
	      hash__free(ht, (void (*)(void *)) bytes__free,
                  (void (*)(void *)) uint4__free);
	      return NULL;
	    }
	if (auto_add_new_i_state(ar, m) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free(2 * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
                (void (*)(void *)) uint4__free);
            return NULL;
  	  }
      }
 
  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
      (void (*)(void *)) uint4__free);
  
  return ar;
}

/****  Public visible functions.                                 ****/

/**  void  auto_set_prod_hsize(s)  :  Sets the size of the hash table
                     used by the product algorithms to s.
                     This function does not report errors.         **/

void  auto_set_prod_hsize(s)
  uint4 s;
{
  if (s)
    auto_prod_hsize = s;
}

/**  uint8  auto_get_prod_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     product algorithms.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_prod_ncolls()
{
  return auto_prod_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_prod_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     product algorithms.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_prod_ncolls()
{
  auto_prod_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_prod_nins()  :  Returns the number of insertions
                     performed in the hash table used by the
                     product algorithms.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_prod_nins()
{
  return auto_prod_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_prod_nins()  :  Resets the number of insertions
                     performed in the hash table used by the
                     product algorithms.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_prod_nins()
{
  auto_prod_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  automaton *auto_product_separ(a1, a2, s1, s2)  :  Computes the
                     synchronous product of the two finite-state
		     automata on finite or infinite words *a1 and *a2.
		     In the case of automata on infinite words, both
		     must be Buchi or weak. Each symbol of the 
		     alphabet of the resulting automaton is obtained
		     by concatenating a symbol belonging to the 
		     alphabet of *a1 to one belonging to the alphabet
		     of *a2. The label *s1 (resp. *s2) describes a
		     separator that demarcates an area in the 
		     structure of the automaton *a1 (resp. *a2). A
		     transition labeled by *s1 can only be crossed in
		     *a1 if a transition labeled by *s2 is 
		     simultaneously crossed in *a2, and conversely.
		     *s1 and *s2 can be NULL, in which case there is
		     no separator.
 
		     This function does not modify *a1 or *a2, and
                     returns (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPH_SIZE  : Alphabet too large.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_product_separ(a1, a2, s1, s2)
  automaton *a1, *a2;
  uint1     *s1, *s2;
{
  register uint4      alph_nbytes_1, alph_nbytes_2;
  register automaton *ar, *aa, *ab;

  diag__enter("auto_product_separ", NULL);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
       ((auto_accept_type(a1) != AUTO_ACCEPT_BUCHI &&
	 auto_accept_type(a1) != AUTO_ACCEPT_WEAK) ||
	(auto_accept_type(a2) != AUTO_ACCEPT_BUCHI &&
	 auto_accept_type(a2) != AUTO_ACCEPT_WEAK))))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes_1 = auto_alphabet_nbytes(a1);
  alph_nbytes_2 = auto_alphabet_nbytes(a2);

  if (alph_nbytes_1 + alph_nbytes_2 > 0xff)
    diag__fail(LASH_ERR_ALPH_SIZE, NULL);

  aa = auto_copy(a1);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ab = auto_copy(a2);
  if (!ab)
    {
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  if (auto_normalize(ab) < 0)
    {
      auto_free(aa);
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  auto_sort_transitions(aa);
  auto_sort_transitions(ab);

  ar = product(aa, ab, auto_nb_states(aa), auto_nb_states(ab),
           alph_nbytes_1, alph_nbytes_2, s1, s2);

  auto_free(aa);
  auto_free(ab);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  if (auto_test_property(a1, AUTO_PROP_DETERM) &&
      auto_test_property(a2, AUTO_PROP_DETERM))
    auto_set_property(ar, AUTO_PROP_DETERM);

  if (auto_test_property(a1, AUTO_PROP_MINIMAL) &&
      auto_test_property(a2, AUTO_PROP_MINIMAL))
    auto_set_property(ar, AUTO_PROP_MINIMAL);

  diag__return(ar);
}

/**  automaton *auto_intersection(a1, a2)  :  Computes a finite-state
                     automaton on finite or infinite words that 
                     accepts the intersection of the languages
		     accepted by the two finite-state automata on
		     finite or infinite words *a1 and *a2. In the case
		     of automata on infinite words, both must be Buchi
		     or weak.

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

automaton *auto_intersection(a1, a2)
  automaton *a1, *a2;
{
  register uint4      alph_nbytes;
  register automaton *ar, *aa, *ab;

  diag__enter("auto_intersection", NULL);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
       ((auto_accept_type(a1) != AUTO_ACCEPT_BUCHI &&
	 auto_accept_type(a1) != AUTO_ACCEPT_WEAK) ||
	(auto_accept_type(a2) != AUTO_ACCEPT_BUCHI &&
	 auto_accept_type(a2) != AUTO_ACCEPT_WEAK))))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes = auto_alphabet_nbytes(a1);

  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, NULL);

  aa = auto_copy(a1);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ab = auto_copy(a2);
  if (!ab)
    {
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  if (auto_normalize(ab) < 0)
    {
      auto_free(aa);
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  auto_sort_transitions(aa);
  auto_sort_transitions(ab);

  ar = intersection(aa, ab, auto_nb_states(aa), auto_nb_states(ab),
           alph_nbytes);

  auto_free(aa);
  auto_free(ab);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  if (auto_test_property(a1, AUTO_PROP_DETERM) &&
      auto_test_property(a2, AUTO_PROP_DETERM))
    auto_set_property(ar, AUTO_PROP_DETERM);

  diag__return(ar);
}

/**  int  auto_empty_intersection(a1, a2)  :  Checks whether the
                     intersection of the languages accepted by the two
                     finite-state automata on finite words *a1 and
                     *a2 is empty or not.
 
		     This function does not modify *a1 or *a2, and
                     returns (in the case of success) a Boolean value
                     (1 if the intersection is empty, 0 if it is not).
                     In the case of an error, it returns -1 and sets
                     lash_errno.

		     For automata on infinite words, one must use the
		     classical construction : (i) compute the
		     intersection of the languages accepted by *a1
		     and *a2, (ii) checks whether or not this
		     language is empty.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  auto_empty_intersection(a1, a2)
  automaton *a1, *a2;
{
  register uint4      alph_nbytes;
  register automaton *aa, *ab;
  register int        r;

  diag__enter("auto_empty_intersection", -1);

  if (auto_word_type(a1) != AUTO_WORDS_FINITE ||
      auto_word_type(a2) != AUTO_WORDS_FINITE) 
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  alph_nbytes = auto_alphabet_nbytes(a1);

  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, -1);

  aa = auto_copy(a1);
  if (!aa)
    diag__fail(lash_errno, -1);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, -1);
    }

  ab = auto_copy(a2);
  if (!ab)
    {
      auto_free(ab);
      diag__fail(lash_errno, -1);
    }

  if (auto_normalize(ab) < 0)
    {
      auto_free(aa);
      auto_free(ab);
      diag__fail(lash_errno, -1);
    }

  auto_sort_transitions(aa);
  auto_sort_transitions(ab);

  r = empty_intersection(aa, ab, auto_nb_states(aa),
          auto_nb_states(ab), alph_nbytes);

  auto_free(aa);
  auto_free(ab);

  if (r < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  diag__return(r);
}

/**  automaton *auto_product_union(a1, a2)  :  Computes a
                     finite-state automaton on finite or infinite
		     words that accepts the union of the languages
		     accepted by the two finite-state automata on
		     finite or infinite words *a1 and *a2. In the
		     case of automata on infinite words, both must
		     be Buchi or weak. 

		     This function uses a product-based algorithm ;
		     the classical auto_union function can not cope
		     with general Buchi automata, since the resulting
		     automaton of that function is systematically
		     non deterministic, now the Buchi automata
		     determinization is very costly and even not
		     currently implemented.

		     This function is also applicable to automata
		     on finite words *a1 and *a2, in which case the
		     resulting automaton is constructed by a direct
		     product construction, instead of merging these
		     automata. So, one need not determinizing the
		     resulting automaton if *a1 and *a2 were
		     deterministic, which is not the case with
		     the function auto_union.
 
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
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Automaton with too many
			                       states.             **/

automaton *auto_product_union(a1, a2)
  automaton *a1, *a2;
{
  register uint4      alph_nbytes;
  register automaton *ar, *aa, *ab;

  diag__enter("buchi_union", NULL);

  if (auto_word_type(a1) != auto_word_type(a2) ||
      (auto_word_type(a1) != AUTO_WORDS_FINITE &&
       auto_word_type(a1) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
       ((auto_accept_type(a1) != AUTO_ACCEPT_BUCHI &&
	  auto_accept_type(a1) != AUTO_ACCEPT_WEAK) ||
	(auto_accept_type(a2) != AUTO_ACCEPT_BUCHI &&
	 auto_accept_type(a2) != AUTO_ACCEPT_WEAK))))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  if (auto_nb_states(a1) > UNION_GAP ||
      auto_nb_states(a2) > UNION_GAP)
    diag__fail(LASH_ERR_TOO_BIG, NULL);

  alph_nbytes = auto_alphabet_nbytes(a1);

  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, NULL);

  aa = auto_copy(a1);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ab = auto_copy(a2);
  if (!ab)
    {
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  if (auto_normalize(ab) < 0)
    {
      auto_free(aa);
      auto_free(ab);
      diag__fail(lash_errno, NULL);
    }

  auto_sort_transitions(aa);
  auto_sort_transitions(ab);

  ar = union_proc(aa, ab, auto_nb_states(aa), auto_nb_states(ab),
           alph_nbytes);

  auto_free(aa);
  auto_free(ab);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  if (auto_test_property(a1, AUTO_PROP_DETERM) &&
      auto_test_property(a2, AUTO_PROP_DETERM))
    auto_set_property(ar, AUTO_PROP_DETERM);

  diag__return(ar);
}

/****  End of auto-product.c  ****/
