/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-sequential.c  :  Sequential operations over finite-    **/
/**                     state automata.                            **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/09/98  :  Minor corrections. (BB)                    **/
/**        09/17/98  :  Got rid of recursion. (BB)                 **/
/**        02/15/99  :  Minor corrections. (BB)                    **/
/**        03/21/01  :  Operations with separator. (SJ)            **/
/**        03/29/01  :  Minor correction. (SJ)                     **/
/**        04/17/02  :  'auto_seq_multi_projection' added.(SJ)     **/
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
#include "auto-sequential.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"

/****  Global variables.                                         ****/

/**  auto_seq_prod_hsize  :  Size of the hash table used by the     
                    sequential product algorithm.                  **/

static uint4  auto_seq_prod_hsize = AUTO_DEFAULT_SEQ_PROD_HSIZE;

/**  auto_seq_prod_ncolls  :  Number of collisions observed in the   
                     hash table used by the sequential product
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_seq_prod_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_seq_prod_nins  :  Number of insertions performed in the   
                     hash table used by the sequential product
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_seq_prod_nins = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_seq_proj_hsize  :  Size of the hash table used by the     
                    sequential projection algorithm.               **/

static uint4  auto_seq_proj_hsize = AUTO_DEFAULT_SEQ_PROJ_HSIZE;

/**  auto_seq_proj_ncolls  :  Number of collisions observed in the   
                     hash table used by the sequential projection
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_seq_proj_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_seq_proj_nins  :  Number of insertions performed in the   
                     hash table used by the sequential projection
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_seq_proj_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static uint4      compute_seq_prod_hsize(uint4, uint4, uint4, uint4);
static int        seq_prod_generate(automaton *, automaton *, uint4,
                      uint4, uint1, automaton *, uint4, uint4, uint4,
                      uint4, uint4 *, hash_table *, uint1 *);
static int        seq_prod_generate_init(automaton *, automaton *,
                      uint4, uint4, uint1, automaton *, hash_table *,
		      stack *);
static int        seq_prod_generate_loop(automaton *, automaton *,
                      uint4, uint4, uint1, automaton *, hash_table *,
		      stack *, uint1 *);
static int        seq_prod_generate_one(automaton *, automaton *,
                      uint4, uint4, uint1, automaton *, uint4, uint4,
                      uint1, uint4, uint4, uint4, tran *, tran *, 
                      hash_table *, stack *);
static automaton *seq_product(automaton *, automaton *, uint4, uint4,
                      uint4, uint4, uint1, uint1 *);
static uint4      compute_seq_proj_hsize(uint4, uint4);
static int        seq_proj_generate(automaton *, uint4, uint4, uint1,
                      automaton *, uint4, uint4, uint4 *, 
                      hash_table *, uint1 *);
static int        seq_proj_generate_init(automaton *, uint4, uint4,
                      uint1, automaton *, hash_table *, stack *);
static int        seq_proj_generate_loop(automaton *, uint4, uint4,
                      uint1, automaton *, hash_table *, stack *, 
                      uint1 *);
static automaton *seq_project(automaton *, uint4, uint4, uint4, 
                      uint1, uint1 *);
static int        seq_multi_proj_generate(automaton *, uint4, 
		      int *, uint1, automaton *, uint4,
		      uint4, uint4 *, hash_table *, uint1 *);
static int        seq_multi_proj_generate_init(automaton *, uint4, 
		      int *, uint1, automaton *, hash_table *,
		      stack *);
static int        seq_multi_proj_generate_loop(automaton *, uint4,
		      int *, uint1, automaton *, hash_table *, 
		      stack *, uint1 *);
static automaton *seq_multi_project(automaton *, uint4, int *,
		      uint4, uint1, uint1 *);

/****  Private functions.                                        ****/

/**  uint4  compute_seq_prod_hsize(n1, n2, q1, q2)  :  Adjusts
                     (heuristically) the size of the hash table needed
                     for computing the sequential product of an
                     automaton with n1 states by one with n2 states,
                     using the offsets q1 and q2.                  **/

static uint4  compute_seq_prod_hsize(n1, n2, q1, q2)
  uint4  n1, n2, q1, q2;
{
  register uint8  nmin, nmax;

  nmin = 8 * (n1 + n2);
  nmax = (((uint8) n1) * ((uint8) n2));
  nmax = (nmax / 4 + 1) * 5;

  q1 += q2;
  nmin *= q1;
  nmax *= q1 * q1;

  if (nmin > ((uint4) -1))
    nmin = auto_seq_prod_hsize;

  if (nmax > ((uint4) -1))
    nmax = auto_seq_prod_hsize;

  if (!nmin)
    nmin = 1;

  if (!nmax)
    nmax = 1;

  if (auto_seq_prod_hsize > nmax)
    return nmax;

  if (auto_seq_prod_hsize < nmin)
    return nmin;

  return auto_seq_prod_hsize;
}

/**  typedef pr_info  :  Type of the data placed on the exploration
                     stack of the function seq_prod_generate.  The
                     first four fields give the current states of the
                     automaton whose product is being computed as well
		     as the corresponding offsets. The fifth field is
		     only relevant for automata on infinite words.
		     It is a flag that is set if and only if a
		     final state of the first automaton has been
		     visited since the last visit in a final state of
		     the second automaton. The sixth field is
                     a mode field; its value is PR_ROOT if the
                     function is invoked for the first time in the
                     current product operation, PR_FIRST if it is
                     called for the first time with the current pair
                     of states and pair of offsets, and PR_LEFT,
                     PR_RIGHT or PR_BOTH if transitions from the left
                     operand, right operand or both operands have
                     already been followed from the current pair of
                     states. The values of the next fields are
                     mode-dependent. If mode == PR_ROOT, there is one
                     field that gives a pointer to a location to which
                     the function should return the index of the
                     topmost created state. If mode == PR_FIRST, there
                     are three fields specifying the origin, the
                     label, and the label length of a transition to be
                     created (the label is allocated statically and
                     does not have to be freed when the stack
                     shrinks). Otherwise, there are two fields
                     corresponding to the number of transitions that
                     have already been explored in the operands, and a
                     third field giving the index of the state of the
                     resulting automaton that corresponds to the
                     current pair of states of the operands and pair
                     of offsets.                                   **/

typedef struct {
  uint4  m1, m2, p1, p2;
  uint1  f1_visited;         /* Flag for Buchi and weak automata */
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
      uint4  length;
    } tr;
    struct {
      uint4  k1, k2, origin;
    } st;
  } v;
} pr_info;

#define  PR_ROOT   0x01
#define  PR_FIRST  0x02
#define  PR_LEFT   0x03
#define  PR_RIGHT  0x04
#define  PR_BOTH   0x05

/**  int  seq_prod_generate(a1, a2, q1, q2, na, ar, m1, m2, p1, p2,
                     mp, ht, separ)  :  This routine is part of the 
                     sequential product operation. The two automata
                     whose product is being computed are *a1 and *a2,
                     and the number of bytes required for storing one
                     symbol of their alphabets is na. The offsets
                     specifying the parameters of the operation are q1
                     and q2. The label *separ describes a separator
		     that demarcates an area in the structure of the
		     automata *a1 and *a2. *separ can be NULL, in
		     which case there is no separator. The partially
		     computed product is in *ar, and a hash table
		     associating a state index of *ar to each
		     pair of state indices of *a1 and *a2
                     and pair of partial offsets is given by *ht. The
                     outgoing transitions from each state of *a1 and
                     *a2 are supposed to be sorted lexicographically.

                     The goal of this function is to generate a part
                     of *ar, starting from the pair of state indices
                     of *a1 and *a2 given by (m1, m2) and the pair of
                     partial offsets (p1, p2). This routine proceeds
                     transitively, updates the hash table, and sets
                     accepting states. The index of of the state of
                     *ar corresponding to the tuple (m1, m2, p1, p2)
                     is returned in *mp. If we deal with automata on
		     infinite words, a state is furthermore
		     characterized by a flag that memorizes if a
		     final state of *a1 has been met since the last
		     visit in a final state of *a2.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_prod_generate(a1, a2, q1, q2, na, ar, m1, m2, p1, p2, 
    mp, ht, separ)
  automaton  *a1, *a2, *ar;
  uint4       q1, q2, m1, m2, p1, p2, *mp;
  uint1       na;
  hash_table *ht;
  uint1      *separ;
{
  register stack   *st;
  register uint1    m;
           pr_info  p;

  st = stack__new_empty(pr_info);
  if (!st)
    return -1;

  p.m1         = m1;
  p.m2         = m2;
  p.p1         = p1;
  p.p2         = p2;
  p.mode       = PR_ROOT;
  p.f1_visited = 1;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((pr_info *) stack__top(st)) -> mode;
      if (((m == PR_ROOT || m == PR_FIRST) && 
          seq_prod_generate_init(a1, a2, q1, q2, na, ar, ht, st)
              < 0) ||
          ((m == PR_LEFT || m == PR_RIGHT || m == PR_BOTH) &&
          seq_prod_generate_loop(a1, a2, q1, q2, na, ar, ht, st, 
				 separ)
              < 0))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  seq_prod_generate_init(a1, a2, q1, q2, na, ar, ht, st)  :  
                     This function is part of the sequential product
                     algorithm. The two automata whose product is
                     being computed are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The offsets specifying the
                     operation are q1 and q2.  The partially computed
                     product is in *ar, and a hash table associating a
                     state index of *ar to each pair of state indices
                     of *a1 and *a2 and pair of partial offsets is
                     given by *ht.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of states and pair of partial offsets placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to either PR_ROOT or
                     PR_FIRST), inserting the corresponding entry in
                     the hash table *ht. It then updates the
                     exploration stack such that the next calls to the
                     function seq_prod_generate_loop will create
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_prod_generate_init(a1, a2, q1, q2, na, ar, ht, st)
  automaton  *a1, *a2, *ar;
  uint4       q1, q2;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register pr_info  *p;
  static   uint4     buf_uint4[5];
           void    **r;
 
  p = (pr_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  PR_ROOT && p -> mode != PR_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;
  buf_uint4[2] = p -> p1;
  buf_uint4[3] = p -> p2;
  buf_uint4[4] = p -> f1_visited;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 4 : 5) * 
			 sizeof(uint4),
      &r, &auto_seq_prod_ncolls, &auto_seq_prod_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 
      (auto_word_type(a1) == AUTO_WORDS_FINITE ? 4 : 5) * 
			 sizeof(uint4),
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

  if (p -> mode == PR_ROOT)
    {
      if (p -> v.return_state)
	*(p -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, p -> v.tr.origin, *v,
        p -> v.tr.length, p -> v.tr.label) < 0)
      return -1;

  if ( (auto_word_type(a1) == AUTO_WORDS_FINITE &&
	p -> p1 == q1 && p -> p2 == q2 &&
	auto_accepting_state(a1, p -> m1) && 
	auto_accepting_state(a2, p -> m2) ) ||

       (auto_word_type(a1) == AUTO_WORDS_INFINITE &&
	auto_accepting_state(a2, p -> m2) &&
	p -> f1_visited) )
    auto_mark_accepting_state(ar, *v);

  p -> mode        = PR_LEFT;
  p -> v.st.k1     = ZERO_INT4;
  p -> v.st.origin = *v;

  return 0;
}

/**  int  seq_prod_generate_loop(a1, a2, q1, q2, na, ar, ht, st, 
                     separ)  :  This function is part of the 
                     sequential product algorithm. The two automata
                     whose product is being computed are *a1 and *a2,
                     and the number of bytes required for storing one
                     symbol of their alphabets is na. The offsets
                     specifying the operation are q1 and q2. The label
                     *separ describes a separator that demarcates an
                     area in the structure of the automata *a1 and
                     *a2.  *separ can be NULL, in which case there is
                     no separator. Transitions labeled by *separ can
                     be crossed in *a1 if a transition labeled by
                     *separ is simultaneously crossed in *a2 and if
                     the offset is zero, and conversely.  The
                     partially computed product is in *ar, and a hash
                     table associating a state index of *ar to each
                     pair of state indices of *a1 and *a2 is given by
                     *ht. The partially computed product is in *ar,
                     and a hash table associating a state index of *ar
                     to each pair of state indices of *a1 and *a2 and
                     pair of partial offsets is given by *ht.

                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 and pair
                     of partial offsets placed on top of the
                     exploration stack *st (supposed non-empty and of
                     mode equal to PR_LEFT, PR_RIGHT or PR_BOTH),
                     updating the stack such that the next calls to
                     this function and to the function
                     seq_prod_generate_init will create transitively
                     all the successor states of the new state as well
                     as their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  seq_prod_generate_loop(a1, a2, q1, q2, na, ar, ht, st, 
				   separ)
  automaton  *a1, *a2, *ar;
  uint4       q1, q2;
  uint1       na;
  hash_table *ht;
  stack      *st;
  uint1      *separ;
{
  register uint4    k1, k2, m1, m2, p1, p2, st1, d;
  register uint1    f1_visited;
  register pr_info *p;
  register tran    *t;
           uint4    n1, n2;

  p = (pr_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != PR_LEFT && p -> mode != PR_RIGHT &&
      p -> mode != PR_BOTH)
    return -1;
#endif

  m1  = p -> m1;
  m2  = p -> m2;
  p1  = p -> p1;
  p2  = p -> p2;
  k1  = p -> v.st.k1;
  k2  = p -> v.st.k2;
  st1 = p -> v.st.origin;
  f1_visited = p -> f1_visited;

  if (auto_nb_out_transitions(a1, m1, &n1) < 0 ||
      auto_nb_out_transitions(a2, m2, &n2) < 0)
    return -1;

  switch (p -> mode)
    {
    case PR_LEFT:
      if (k1 >= n1)
	{
	  p -> mode    = PR_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      t = auto_transition(a1, m1, k1);
      if (auto_transition_length(t))
	{
	  p -> mode    = PR_RIGHT; 
	  p -> v.st.k2 = ZERO_INT4;
	  return 0;
	}
      p -> v.st.k1++;
      return seq_prod_generate_one(a1, a2, q1, q2, na, ar, m1, m2,
          f1_visited, p1, p2, st1, t, NULL, ht, st);
      
    case PR_RIGHT:
      if (k2 >= n2)
	{
	  p -> mode = PR_BOTH;
	  return 0;
	}
      t = auto_transition(a2, m2, k2);
      if (auto_transition_length(t))
	{
	  p -> mode = PR_BOTH;
	  return 0;
	}
      p -> v.st.k2++;
      return seq_prod_generate_one(a1, a2, q1, q2, na, ar, m1, m2,
          f1_visited, p1, p2, st1, NULL, t, ht, st);
      
    case PR_BOTH:
      if (p1 && p1 != q1 + 1)
	{
	  if (k1 >= n1)
	    {
	      stack__pop(st, NULL);
	      return 0;
	    }
	  p -> v.st.k1++;
	  t = auto_transition(a1, m1, k1);

	  if (separ && auto_transition_length(t) &&
	      !memcmp(auto_transition_label_ptr(t, na), separ, na))
	    {
	      if (p1 != q1)
		return 0;
	      p -> p1 = q1 + 1;
	      p1 = p -> p1;
	    }

	  else
	    return seq_prod_generate_one(a1, a2, q1, q2, na, ar, m1, 
					 m2, f1_visited, p1, p2, st1,
					 t, NULL, ht, st);
	}

      if (p1 == q1 + 1)
	d = auto_transition_dest(auto_transition(a1, m1, 
						 p -> v.st.k1 - 1));
      else
	d = (uint4) -1;  /* This will never happen */

      for ( ; k2 < n2 ; k2++)
	{
	  p -> v.st.k2++;

	  t = auto_transition(a2, m2, k2);

	  if (p1 == q1 + 1)
	    {
	      if (!auto_transition_length(t) ||
		  memcmp(auto_transition_label_ptr(t, na), separ, na))
		continue;

	      return seq_prod_generate_one(a1, a2, q1, q2, na, ar, d,
					   m2, f1_visited, 0, 1, st1,
					   NULL, t, ht, st); 
	    }

	  else
	    if (!separ || (auto_transition_length(t) &&
		memcmp(auto_transition_label_ptr(t, na), separ, na)))
	      return seq_prod_generate_one(a1, a2, q1, q2, na, ar,
					   m1, m2, f1_visited, p1, 
					   p2, st1, NULL, t, ht, st); 
	}

      if (p1 == q1 + 1)
	p -> p1 = q1;

      stack__pop(st, NULL);
      return 0;

    default:
      return -1;
    }
}

/**  int  seq_prod_generate_one(a1, a2, q1, q2, na, ar, m1, m2, 
                     f1_vis, p1, p2, m, t1, t2, ht, st)  :  This 
		     function is part of the sequential product
                     algorithm. The two automata whose product is
                     being computed are *a1 and *a2, and the number of
                     bytes required for storing one symbol of their
                     alphabets is na. The offsets specifying the
                     operation are q1 and q2.  The partially computed
                     product is in *ar, and a hash table associating a
                     state index of *ar to each pair of state indices
                     of *a1 and *a2 and pair of partial offsets is
                     given by *ht.

                     The goal of this function is to update the
                     exploration stack *st so as to add to the
                     automaton *ar a new transition outgoing from the
                     state of index m, and corresponding to the pair
                     (m1, m2) of states of *a1 and *a2 and pair (p1,
                     p2) or partial offsets. This transition
                     corresponds to either *t1 or *t2, whichever
                     exists (i.e., is not given by a NULL pointer).

		     If we deal with automata on infinite words, a
		     state is furthermore characterized by the flag
		     f1_vis that memorizes if a final state of *a1
		     has been met since the last visit in a final
		     state of *a2.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_prod_generate_one(a1, a2, q1, q2, na, ar, m1, m2, 
				  f1_vis, p1, p2, m, t1, t2, ht, st)
  automaton  *a1, *a2, *ar;
  uint1       na, f1_vis;
  uint4       q1, q2, m1, m2, p1, p2, m;
  tran       *t1, *t2;
  hash_table *ht;
  stack      *st;
{
  register uint4    length;
  register uint1   *label;
  static   uint4    dest[5];
           pr_info  p;
           void    *r;

  label  = auto_transition_label_ptr((t1 ? t1 : t2), na);
  length = auto_transition_length(t1 ? t1 : t2);

  dest[0] = t1 ? auto_transition_dest(t1) : m1;
  dest[1] = t2 ? auto_transition_dest(t2) : m2;
  dest[2] = (t1 && length) ? (p1 - 1) : p1;
  dest[3] = (t2 && length) ? (p2 - 1) : p2;

  if (!(dest[3]))
    {
      dest[2] = q1;
      dest[3] = q2;
    }

  if (auto_word_type(a1) == AUTO_WORDS_FINITE)
    {
      dest[4] = 1;  /* Not necessary */
      r = hash__lookup_bytes(ht, (uint1 *) dest, 4 * sizeof(uint4));
    }
      
  else
    {
      dest[4] = ( auto_accepting_state(a1, dest[0]) ||
		  (f1_vis && !auto_accepting_state(a2, m2)) );
      r = hash__lookup_bytes(ht, (uint1 *) dest, 5 * sizeof(uint4));
    }

  if (r)
    return auto_add_new_transition(ar, m, *((uint4 *) r),
        length, label);

  p.mode        = PR_FIRST;
  p.m1          = dest[0];
  p.m2          = dest[1];
  p.p1          = dest[2];
  p.p2          = dest[3];
  p.f1_visited  = dest[4];
  p.v.tr.origin = m;
  p.v.tr.label  = label;
  p.v.tr.length = length;

  if (stack__push(st, (void *) &p) < 0)
    return -1;

  return 0;
}

/**  automaton *seq_product(a1, a2, q1, q2, n1, n2, na, separ)  :  
                     Computes the sequential product of the two
		     finite-state automata on finite or infinite
		     words *a1 and *a2 with respect to the offsets
		     q1 and q2. The automata *a1 and *a2 are
		     supposed to be in normal form, and
                     the outgoing transitions from each state are
                     supposed to be sorted lexicographically.  Their
                     numbers of states are given by n1, n2. The number
                     of bytes required for storing one symbol of their
                     alphabets is na. The two offsets q1 and q2 must 
                     be different from zero.

		     The label *separ describes a separator
		     that demarcates an area in the structure of the
		     automata *a1 and *a2 and that can be crossed in
		     *a1 if and only if it is simultaneously crossed
		     in *a2 and if the offset is zero. Crossing it
		     does not change the offset. *separ can be
		     NULL, in which case there is no separator.

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *seq_product(a1, a2, q1, q2, n1, n2, na, separ)
  automaton *a1, *a2;
  uint4      q1, q2, n1, n2;
  uint1      na, *separ;
{
  register uint4       i, j, k1, k2;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[4];
           void       *r;

  ar = auto_new_empty(na);
  if (!ar)
    return NULL;

  if (auto_word_type(a1) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_BUCHI;
    }

  ht = hash__new_empty(compute_seq_prod_hsize(n1, n2, q1, q2));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  dest[2] = q1;
  dest[3] = q2;

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      {
        if (auto_i_state(a1, i, dest) < 0 ||
  	    auto_i_state(a2, j, dest + 1) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
				 AUTO_WORDS_INFINITE
				 ? 5 : 4) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
	        (void (*)(void *)) uint4__free);
             return NULL;
	  }
	r = hash__lookup_bytes(ht, (uint1 *) dest,
            4 * sizeof(uint4));
        if (r)
	  m = *((uint4 *) r);
	else
	  if (seq_prod_generate(a1, a2, q1, q2, na, ar,
              dest[0], dest[1], q1, q2, &m, ht, separ) < 0)
	    {
	      auto_free(ar);
	      bytes__prepare_free((auto_word_type(a1) == 
				   AUTO_WORDS_INFINITE
				   ? 5 : 4) * sizeof(uint4));
	      hash__free(ht, (void (*)(void *)) bytes__free,
	          (void (*)(void *)) uint4__free);
	      return NULL;
	    }
	if (auto_add_new_i_state(ar, m) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free((auto_word_type(a1) == 
				 AUTO_WORDS_INFINITE
				 ? 5 : 4) * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
	      (void (*)(void *)) uint4__free);
            return NULL;
  	  }
      }
 
  bytes__prepare_free((auto_word_type(a1) == 
		       AUTO_WORDS_INFINITE
		       ? 5 : 4) * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
      (void (*)(void *)) uint4__free);
  
  return ar;
}

/**  uint4  compute_seq_proj_hsize(n, p)  :  Adjusts (heuristically)
                     the size of the hash table needed for computing
                     the sequential projection of an automaton with n
                     states, with respect to the period p.         **/

static uint4  compute_seq_proj_hsize(n, p)
  uint4 n, p;
{
  if (!n)
    n = 1;

  if (!p)
    p = 1;
  
  if (auto_seq_proj_hsize > (n * p))
    return 2 * n * p;

  if (auto_seq_proj_hsize < n * (p / 2 + 1))
    return n * (p / 2 + 1);
  
  return auto_seq_proj_hsize;
}

/**  typedef pj_info  :  Type of the data placed on the exploration
                     stack of the function seq_proj_generate.  The
                     first two fields give the current state of the
                     automaton whose projection is being computed as
                     well as the current offset. The third field is a
                     mode field; its value is PJ_ROOT if the function
                     is invoked for the first time in the current
                     projection operation, PJ_FIRST if it is called
                     for the first time with the current pair of state
                     and offset, and PJ_NEXT otherwise.  The values of
                     the next fields are mode-dependent. If mode ==
                     PJ_ROOT, there is one field that gives a pointer
                     to a location to which the function should return
                     the index of the topmost created state. If mode
                     == PJ_FIRST, there are three fields specifying
                     the origin, the label, and the label length of a
                     transition to be created (the label is allocated
                     statically and does not have to be freed when the
                     stack shrinks). Otherwise, there are two fields
                     corresponding to the number of transitions that
                     have already been explored and the index of the
                     state of the resulting automaton that corresponds
                     to the current pair of state and offset.      **/

typedef struct {
  uint4  m, q;
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
      uint4  length;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} pj_info;

#define  PJ_ROOT   0x01
#define  PJ_FIRST  0x02
#define  PJ_NEXT   0x03

/**  int  seq_proj_generate(a, p, u, na, ar, m, q, mp, ht, separ)  :
                     This routine is part of the sequential
		     projection operation. The automaton being
		     projected is *a, and the number of bytes
		     required for storing one symbol of its alphabet 
		     is na. The period and the offset specifying the
		     projection are p and u.  The partially 
		     computed projection is in *ar, and a hash table
		     associating a state index of *ar to each pair of
		     state index of *a and current offset is given by
		     *ht. The label *separ describes a separator that 
		     demarcates an area in the structure of the
		     automaton *a. *separ can be NULL, in which case
		     there is no separator.

                     The goal of this function is to generate a part
		     of *ar, starting from the pair of state index of
		     *a and current offset (m, q). This routine
		     proceeds transitively, updates the hash table,
		     and sets accepting states. The index of of the
		     state of *ar corresponding to the tuple (m, q) is
		     returned in *mp.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_proj_generate(a, p, u, na, ar, m, q, mp, ht, separ) 
  automaton  *a, *ar;
  uint4       p, u, m, q, *mp;
  uint1       na;
  hash_table *ht;
  uint1      *separ;
{
  register stack   *st;
  register uint1    mo;
           pj_info  pj;

  st = stack__new_empty(pj_info);
  if (!st)
    return -1;

  pj.m    = m;
  pj.q    = q;
  pj.mode = PJ_ROOT;
  pj.v.return_state = mp;

  if (stack__push(st, (void *) &pj) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      mo = ((pj_info *) stack__top(st)) -> mode;
      if (((mo == PJ_ROOT || mo == PJ_FIRST) && 
          seq_proj_generate_init(a, p, u, na, ar, ht, st)
              < 0) ||
          (mo == PJ_NEXT &&
          seq_proj_generate_loop(a, p, u, na, ar, ht, st,
              separ) < 0))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  seq_proj_generate_init(a, p, u, na, ar, ht, st)  :  This
                     routine is part of the sequential projection
		     operation. The automaton being projected is *a,
		     and the number of bytes required for storing one
		     symbol of its alphabet is na. The period and the
		     offset specifying the projection are p and u.
		     The partially computed projection is in *ar, and
		     a hash table associating a state index of *ar to
		     each pair of state index of *a and current offset
		     is given by *ht.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of state and current offset placed on top of the
                     exploration stack *st (supposed non-empty and of
                     mode equal to either PJ_ROOT or PJ_FIRST),
                     inserting the corresponding entry in the hash
                     table *ht. It then updates the exploration stack
                     such that the next calls to the function
                     seq_proj_generate_loop will create transitively
                     all the successor states of the new state as well
                     as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_proj_generate_init(a, p, u, na, ar, ht, st)
  automaton  *a, *ar;
  uint4       p, u;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register pj_info  *pj;
  static   uint4     buf_uint4[2];
           void    **r;
 
  pj = (pj_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (pj -> mode !=  PJ_ROOT && pj -> mode != PJ_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = pj -> m;
  buf_uint4[1] = pj -> q;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &auto_seq_proj_ncolls, &auto_seq_proj_nins) < 0 || !r)
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

  if (pj -> mode == PJ_ROOT)
    {
      if (pj -> v.return_state)
	*(pj -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, pj -> v.tr.origin, *v,
        pj -> v.tr.length, pj -> v.tr.label) < 0)
      return -1;

  if ((auto_word_type(a) == AUTO_WORDS_INFINITE || !(pj -> q)) &&
      auto_accepting_state(a, pj -> m))
    auto_mark_accepting_state(ar, *v);

  pj -> mode        = PJ_NEXT;
  pj -> v.st.k      = ZERO_INT4;
  pj -> v.st.origin = *v;

  return 0;
}

/**  int  seq_proj_generate_loop(a, p, u, na, ar, ht, st, separ)  :
                     This routine is part of the sequential
		     projection operation. The automaton 
		     being projected is *a, and the number of bytes
		     required for storing one symbol of its alphabet 
		     is na. The period and the offset specifying the
		     projection are p and u. The partially
		     computed projection is in *ar, and a hash table
		     associating a state index of *ar to each pair of
		     state index of *a and current offset is given by
		     *ht. The label *separ describes an atomic 
		     separator that demarcates an area in the
		     structure of the automaton *a. It can be NULL, in
		     which case there is no separator. 
 
                     This function explores the outgoing transitions
                     from the pair of state of *a and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     PJ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     seq_proj_generate_init will create transitively
                     all the successor states of the new state as well
                     as their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  seq_proj_generate_loop(a, p, u, na, ar, ht, st, separ)
  automaton  *a, *ar;
  uint4       p, u;
  uint1       na;
  hash_table *ht;
  stack      *st;
  uint1      *separ;
{
  register uint4    k, m, q, st1, length;
  static   uint4    dest[2];
  register pj_info *pj;
  register tran    *t;
           uint4    n;
           pj_info  pj2;
           void    *r;

  pj = (pj_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (pj -> mode != PJ_NEXT)
    return -1;
#endif

  m   = pj -> m;
  q   = pj -> q;
  k   = pj -> v.st.k;
  st1 = pj -> v.st.origin;

  if (auto_nb_out_transitions(a, m, &n) < 0)
    return -1;

  if (k >= n)
    {
      stack__pop(st, NULL);
      return 0;
    }

  pj -> v.st.k++;

  t       = auto_transition(a, m, k);
  dest[0] = auto_transition_dest(t);

  if (separ &&
      !memcmp(auto_transition_label_ptr(t, na), separ, na))
    {
      /* A separator is being crossed ; this only allowed
         when the offset is zero. */
      if (q != 0)
	return 0;
      dest[1] = 0;
      length = auto_transition_length(t);
    }
  else
    {
      dest[1] = auto_transition_length(t) ? ((q + 1) % p) : q;
      length = (q == u) ? 0 : auto_transition_length(t);
    }

  r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (r)
    return auto_add_new_transition(ar, st1, *((uint4 *) r),
        length, auto_transition_label_ptr(t, na));

  pj2.mode        = PJ_FIRST;
  pj2.m           = dest[0];
  pj2.q           = dest[1];
  pj2.v.tr.origin = st1;
  pj2.v.tr.label  = auto_transition_label_ptr(t, na);
  pj2.v.tr.length = length;

  if (stack__push(st, (void *) &pj2) < 0)
    return -1;

  return 0;
}

/**  automaton *seq_project(a, p, u, n, na, separ)  :  Computes the
                     sequential projection of the finite-state 
		     automaton on finite or infinite words *a with
		     respect to the period p and the offset u.
		     The automaton *a is supposed to be in normal
		     form.  Its number of states is n. The number of
		     bytes required for storing one symbol of its
		     alphabet is na. The period p must be different
		     from zero, and the offset u must be less than p.

		     The label *separ describes an atomic separator
		     that demarcates an area in the structure of the
		     automaton *a. A separator can only be crossed
		     when the offset is zero and it does not change
		     the offset. *separ can be NULL, in which case
		     there is no separator.		     

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *seq_project(a, p, u, n, na, separ)
  automaton *a;
  uint4      p, u, n;
  uint1      na, *separ;
{
  register uint4       i, k;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  ar = auto_new_empty(na);
  if (!ar)
    return NULL;

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = auto_word_type(a);
      auto_accept_type(ar) = auto_accept_type(a);
    }

  ht = hash__new_empty(compute_seq_proj_hsize(n, p));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  dest[1] = 0;
  k = auto_nb_i_states(a);

  for (i = 0; i < k; i++)
    {
      if (auto_i_state(a, i, dest) < 0)
	{
	  auto_free(ar);
	  bytes__prepare_free(2 * sizeof(uint4));
	  hash__free(ht, (void (*)(void *)) bytes__free,
	      (void (*)(void *)) uint4__free);
	  return NULL;
	}
      r = hash__lookup_bytes(ht, (uint1 *) dest,
          2 * sizeof(uint4));
      if (r)
        m = *((uint4 *) r);
      else
	if (seq_proj_generate(a, p, u, na, ar, dest[0], 0,
            &m, ht, separ) < 0)
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

/**  int  seq_multi_proj_generate(a, p, var, na, ar, m, q, mp,
                     ht, separ)  :  This routine is part of the
		     multiple sequential projection operation. The
		     automaton being projected is *a, and the number
		     of bytes required for storing one symbol of its
		     alphabet is na. The period specifying the
		     projection is p, and the variables to keep are
		     denoted by non-zero values of their associated
		     entries in the array var.  The partially computed
		     projection is in *ar, and a hash table
		     associating a state index of *ar to each pair of
		     state index of *a and current offset is given by
		     *ht. The label *separ describes a separator that
		     demarcates an area in the structure of the
		     automaton *a. *separ can be NULL, in which case
		     there is no separator.

                     The goal of this function is to generate a part
		     of *ar, starting from the pair of state index of
		     *a and current offset (m, q). This routine
		     proceeds transitively, updates the hash table,
		     and sets accepting states. The index of of the
		     state of *ar corresponding to the tuple (m, q) is
		     returned in *mp.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_multi_proj_generate(a, p, var, na, ar, m, q, mp, ht, 
				    separ) 
  automaton  *a, *ar;
  uint4       p;
  int        *var;
  uint4       m, q, *mp;
  uint1       na;
  hash_table *ht;
  uint1      *separ;
{
  register stack   *st;
  register uint1    mo;
           pj_info  pj;

  st = stack__new_empty(pj_info);
  if (!st)
    return -1;

  pj.m    = m;
  pj.q    = q;
  pj.mode = PJ_ROOT;
  pj.v.return_state = mp;

  if (stack__push(st, (void *) &pj) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      mo = ((pj_info *) stack__top(st)) -> mode;
      if (((mo == PJ_ROOT || mo == PJ_FIRST) && 
          seq_multi_proj_generate_init(a, p, var, na, ar, ht, st)
              < 0) ||
          (mo == PJ_NEXT &&
          seq_multi_proj_generate_loop(a, p, var, na, ar, ht, st,
              separ) < 0))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  seq_multi_proj_generate_init(a, p, var, na, ar, ht, st)  :
                     This routine is part of the multiple sequential
                     projection operation. The automaton being
                     projected is *a, and the number of bytes required
                     for storing one symbol of its alphabet is na. The
                     period specifying the projection is p, and the
                     variables to keep are denoted by non-zero values
                     of their associated entries in the array var. The
                     partially computed projection is in *ar, and a
                     hash table associating a state index of *ar to
                     each pair of state index of *a and current offset
                     is given by *ht.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of state and current offset placed on top of the
                     exploration stack *st (supposed non-empty and of
                     mode equal to either PJ_ROOT or PJ_FIRST),
                     inserting the corresponding entry in the hash
                     table *ht. It then updates the exploration stack
                     such that the next calls to the function
                     seq_multi_proj_generate_loop will create
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  seq_multi_proj_generate_init(a, p, var, na, ar, ht, st)
  automaton  *a, *ar;
  uint4       p;
  int        *var;
  uint1       na;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register pj_info  *pj;
  static   uint4     buf_uint4[2];
           void    **r;
 
  pj = (pj_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (pj -> mode !=  PJ_ROOT && pj -> mode != PJ_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = pj -> m;
  buf_uint4[1] = pj -> q;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &auto_seq_proj_ncolls, &auto_seq_proj_nins) < 0 || !r)
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

  if (pj -> mode == PJ_ROOT)
    {
      if (pj -> v.return_state)
	*(pj -> v.return_state) = *v;
    }
  else
    if (auto_add_new_transition(ar, pj -> v.tr.origin, *v,
        pj -> v.tr.length, pj -> v.tr.label) < 0)
      return -1;

  if ((auto_word_type(a) == AUTO_WORDS_INFINITE || !(pj -> q)) &&
      auto_accepting_state(a, pj -> m))
    auto_mark_accepting_state(ar, *v);

  pj -> mode        = PJ_NEXT;
  pj -> v.st.k      = ZERO_INT4;
  pj -> v.st.origin = *v;

  return 0;
}

/**  int  seq_multi_proj_generate_loop(a, p, var, na, ar, ht, st, 
                     separ)  :  This routine is part of the 
                     multiple sequential projection operation. The
		     automaton being projected is *a, and the number
		     of bytes required for storing one symbol of its
		     alphabet is na. The period specifying the
		     projection is p, and the variables to keep are
		     denoted by non-zero values of their associated
		     entries in the array var.  The partially computed
		     projection is in *ar, and a hash table
		     associating a state index of *ar to each pair of
		     state index of *a and current offset is given by
		     *ht. The label *separ describes an atomic
		     separator that demarcates an area in the
		     structure of the automaton *a. It can be NULL, in
		     which case there is no separator.
 
                     This function explores the outgoing transitions
                     from the pair of state of *a and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     PJ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     seq_multi_proj_generate_init will create
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  seq_multi_proj_generate_loop(a, p, var, na, ar, ht, st, 
					 separ)
  automaton  *a, *ar;
  uint4       p;
  int        *var;
  uint1       na;
  hash_table *ht;
  stack      *st;
  uint1      *separ;
{
  register uint4    k, m, q, st1, length;
  static   uint4    dest[2];
  register pj_info *pj;
  register tran    *t;
           uint4    n;
           pj_info  pj2;
           void    *r;

  pj = (pj_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (pj -> mode != PJ_NEXT)
    return -1;
#endif

  m   = pj -> m;
  q   = pj -> q;
  k   = pj -> v.st.k;
  st1 = pj -> v.st.origin;

  if (auto_nb_out_transitions(a, m, &n) < 0)
    return -1;

  if (k >= n)
    {
      stack__pop(st, NULL);
      return 0;
    }

  pj -> v.st.k++;

  t       = auto_transition(a, m, k);
  dest[0] = auto_transition_dest(t);

  if (separ &&
      !memcmp(auto_transition_label_ptr(t, na), separ, na))
    {
      /* A separator is being crossed ; this only allowed
         when the offset is zero. */
      if (q != 0)
	return 0;
      dest[1] = 0;
      length = auto_transition_length(t);
    }
  else
    {
      dest[1] = auto_transition_length(t) ? ((q + 1) % p) : q;
      length = var[q] ? auto_transition_length(t) : 0;
    }

  r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (r)
    return auto_add_new_transition(ar, st1, *((uint4 *) r),
        length, auto_transition_label_ptr(t, na));

  pj2.mode        = PJ_FIRST;
  pj2.m           = dest[0];
  pj2.q           = dest[1];
  pj2.v.tr.origin = st1;
  pj2.v.tr.label  = auto_transition_label_ptr(t, na);
  pj2.v.tr.length = length;

  if (stack__push(st, (void *) &pj2) < 0)
    return -1;

  return 0;
}

/**  automaton *seq_multi_project(a, p, var, n, na, separ)  :
                     Computes the mutliple sequential projection of
		     the finite-state automaton on finite or infinite
		     words *a with respect to the period p. The
		     variables to keep are denoted by non-zero values
		     of their associated entries in the array var.
		     The automaton *a is supposed to be in normal
		     form.  Its number of states is n.  The number of
		     bytes required for storing one symbol of its
		     alphabet is na. The period p must be different
		     from zero, and the offset u must be less than p.

		     The label *separ describes an atomic separator
		     that demarcates an area in the structure of the
		     automaton *a. A separator can only be crossed
		     when the offset is zero and it does not change
		     the offset. *separ can be NULL, in which case
		     there is no separator.		     

                     In the case of an error, this function simply
                     returns a NULL pointer.                       **/

static automaton *seq_multi_project(a, p, var, n, na, separ)
  automaton *a;
  uint4      p;
  int       *var;
  uint4      n;
  uint1      na, *separ;
{
  register uint4       i, k;
  register automaton  *ar;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  ar = auto_new_empty(na);
  if (!ar)
    return NULL;

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = auto_word_type(a);
      auto_accept_type(ar) = auto_accept_type(a);
    }

  ht = hash__new_empty(compute_seq_proj_hsize(n, p));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  dest[1] = 0;
  k = auto_nb_i_states(a);

  for (i = 0; i < k; i++)
    {
      if (auto_i_state(a, i, dest) < 0)
	{
	  auto_free(ar);
	  bytes__prepare_free(2 * sizeof(uint4));
	  hash__free(ht, (void (*)(void *)) bytes__free,
	      (void (*)(void *)) uint4__free);
	  return NULL;
	}
      r = hash__lookup_bytes(ht, (uint1 *) dest,
          2 * sizeof(uint4));
      if (r)
        m = *((uint4 *) r);
      else
	if (seq_multi_proj_generate(a, p, var, na, ar, dest[0], 0,
            &m, ht, separ) < 0)
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

/**  void  auto_set_seq_prod_hsize(s)  :  Sets the size of the hash
                     table used by the sequential product algorithm to
                     s.  This function does not report errors.     **/

void  auto_set_seq_prod_hsize(s)
  uint4  s;
{
  if (s)
    auto_seq_prod_hsize = s;
}

/**  uint8  auto_get_seq_prod_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     sequential product algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_seq_prod_ncolls()
{
  return auto_seq_prod_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_seq_prod_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     sequential product algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_seq_prod_ncolls()
{
  auto_seq_prod_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_seq_prod_nins()  :  Returns the number of 
                     insertions performed in the hash table used by
                     the sequential product algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_seq_prod_nins()
{
  return auto_seq_prod_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_seq_prod_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the sequential product algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_seq_prod_nins()
{
  auto_seq_prod_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  void  auto_set_seq_proj_hsize(s)  :  Sets the size of the hash 
                     table used by the sequential projection algorithm
                     to s.  This function does not report errors.  **/

void  auto_set_seq_proj_hsize(s)
  uint4  s;
{
  if (s)
    auto_seq_proj_hsize = s;
}

/**  uint8  auto_get_seq_proj_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     sequential projection algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_seq_proj_ncolls()
{
  return auto_seq_proj_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_seq_proj_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     sequential projection algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_seq_proj_ncolls()
{
  auto_seq_proj_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_seq_proj_nins()  :  Returns the number of 
                     insertions performed in the hash table used by
                     the sequential projection algorithm.  This
                     function does not report errors.              **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_seq_proj_nins()
{
  return auto_seq_proj_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_seq_proj_nins()  :  Resets the number of 
                     insertions performed in the hash table used by
                     the sequential projection algorithm.  This
                     function does not report errors.              **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_seq_proj_nins()
{
  auto_seq_proj_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  automaton *auto_seq_product_separ(a1, a2, q1, q2, separ)  :
                     Computes the sequential product of the two
		     finite-state automata on finite or infinite
		     words *a1 and *a2 with respect to the offsets
		     q1 and q2. The result of this operation is a
		     finite-state automaton on finite or infinite
                     words that accepts the language { a1 a2 b1 b2
                     ... z1 z2 | a1 b1 ... z1 \in L1 and a2 b2 ... z2
                     \in L2 and |a1| = |b1| = ... |z1| = q1 and
                     |a2| = |b2| = ... |z2| = q2 }, where L1 and L2
                     are the languages accepted by *a1 and *a2.
 
		     The label *separ describes a separator
		     that demarcates an area in the structure of the
		     automata *a1 and *a2 and that can be crossed in
		     *a1 if and only if it is simultaneously crossed
		     in *a2 and if the offset is zero. Crossing it
		     does not change the offset. *separ can be
		     NULL, in which case there is no separator.

                     This function does not modify *a1 or *a2, and
                     returns (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_seq_product_separ(a1, a2, q1, q2, separ)
  automaton *a1, *a2;
  uint4      q1, q2;
  uint1     *separ;
{
  register uint1      alph_nbytes;
  register automaton *ar, *aa, *ab;

  diag__enter("auto_seq_product_separ", NULL);

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
 
  if (!q1 || !q2)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
 
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

  ar = seq_product(aa, ab, q1, q2, auto_nb_states(aa),
           auto_nb_states(ab), alph_nbytes, separ);

  auto_free(aa);
  auto_free(ab);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (auto_test_property(a1, AUTO_PROP_DETERM) &&
      auto_test_property(a2, AUTO_PROP_DETERM))
    auto_set_property(ar, AUTO_PROP_DETERM);

  diag__return(ar);
}

/** automaton *auto_seq_projection_separ(a, p, u, separ)  :
                     Computes the sequential projection of the
		     finite-state automaton on finite or infinite
		     words *a with respect to the period p and
		     offset u.  The result of this operation is a
		     finite-state automaton on finite or infinite
		     words that accepts the language { a0 a1
                     ... a(u-1) a(u+1) ... a(p-1) b0 b1 ... b(u-1)
                     b(u+1) ... b(p-1) ...  z0 z1 ... z(u-1) z(u+1)
                     ... z(p-1) | a0 a1 ... a(p-1) b0 b1 ... b(p-1)
                     ... z0 z1 ... z(p-1) \in L(a) and |a0| = |a1| =
                     ... |z(p-1)| = 1 }, where L(a) is the language
                     accepted by *a.
 
		     The label *separ describes an atomic separator
		     that demarcates an area in the structure of the
		     automaton *a. A separator can only be crossed
		     when the offset is zero and it does not change
		     the offset. *separ can be NULL, in which case
		     there is no separator.		     

                     This function does not modify *a, and returns (in
                     the case of success) a pointer to a newly
                     allocated automaton. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_seq_projection_separ(a, p, u, separ)
  automaton *a;
  uint4      p, u;
  uint1     *separ;
{
  register uint4      alph_nbytes;
  register automaton *ar, *aa;

  diag__enter("auto_seq_projection_separ", NULL);

  if ((auto_word_type(a) != AUTO_WORDS_FINITE &&
       auto_word_type(a) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a) == AUTO_WORDS_INFINITE &&
       auto_accept_type(a) != AUTO_ACCEPT_BUCHI &&
       auto_accept_type(a) != AUTO_ACCEPT_WEAK))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes = auto_alphabet_nbytes(a);
 
  if (!p || u >= p)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
 
  aa = auto_copy(a);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ar = seq_project(aa, p, u, auto_nb_states(aa), alph_nbytes, separ);

  auto_free(aa);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return(ar);
}

/** automaton *auto_seq_multi_projection_separ(a, p, var, separ)  :
                     Computes the sequential projection of the
		     finite-state automaton on finite or infinite
		     words *a with respect to the period p and the set
		     of variables var, represented by its
		     characteristic array (the i-th variable is to be
		     kept iff var[i-1] != 0).  If the variables to
		     keep (specified by var) form the ordered set {x1,
		     ..., xk}, the result of this operation is a
		     finite-state automaton on finite or infinite
		     words that accepts the language { a0 ... a(x1-1)
		     a(x1+1) ...  a(x2-1) a(x2+1) ... a(xk-1) a(xk+1)
		     ... a(p-1) b0 ... b(x1-1) b(x1+1) ...  b(x2-1)
		     b(x2+1) ... b(xk-1) b(xk+1) ... b(p-1) [...] z0
		     ... z(x1-1) z(x1+1) ...  z(x2-1) z(x2+1)
		     ... z(xk-1) z(xk+1) ... z(p-1) | a0 a1 ... a(p-1)
		     b0 b1 ... b(p-1) ... z0 z1 ... z(p-1) \in L(a)
		     and |a0| = |a1| = ... |z(p-1)| = 1 }, where L(a)
		     is the language accepted by *a.
 
		     The label *separ describes an atomic separator
		     that demarcates an area in the structure of the
		     automaton *a. A separator can only be crossed
		     when the offset is zero and it does not change
		     the offset. *separ can be NULL, in which case
		     there is no separator.		     

                     This function does not modify *a, and returns (in
                     the case of success) a pointer to a newly
                     allocated automaton. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_seq_multi_projection_separ(a, p, var, separ)
  automaton *a;
  uint4      p;
  int       *var;
  uint1     *separ;
{
  register uint4      alph_nbytes;
  register automaton *ar, *aa;

  diag__enter("auto_seq_projection_separ", NULL);

  if ((auto_word_type(a) != AUTO_WORDS_FINITE &&
       auto_word_type(a) != AUTO_WORDS_INFINITE) ||
      (auto_word_type(a) == AUTO_WORDS_INFINITE &&
       auto_accept_type(a) != AUTO_ACCEPT_BUCHI &&
       auto_accept_type(a) != AUTO_ACCEPT_WEAK))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes = auto_alphabet_nbytes(a);
 
  if (!p || !var)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
 
  aa = auto_copy(a);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ar = seq_multi_project
    (aa, p, var, auto_nb_states(aa), alph_nbytes, separ);

  auto_free(aa);

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return(ar);
}

/****  End of auto-sequential.c  ****/
