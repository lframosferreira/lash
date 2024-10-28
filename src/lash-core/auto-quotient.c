/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-quotient.c  :  Quotient and quotient-related           **/
/**                     operations over finite-state automata.     **/
/**                                                                **/
/**        09/18/98  :  Creation. (BB)                             **/
/**        06/29/00  :  Minor correction. (JMF)                    **/
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
#include "auto-quotient.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"

/****  Global variables.                                         ****/

/**  auto_quotient_hsize  :  Size of the hash table used by the     
                    quotient algorithm.                            **/

static uint4  auto_quotient_hsize = AUTO_DEFAULT_QUOTIENT_HSIZE;

/**  auto_quotient_ncolls  :  Number of collisions observed in the   
                     hash table used by the quotient
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_quotient_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_quotient_nins  :  Number of insertions performed in the   
                     hash table used by the quotient
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_quotient_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static void       reverse_label(uint1 *, uint1 *, uint4, uint1);
static uint4      compute_quotient_hsize(uint4, uint4);
static int        set_i_states(automaton *, uint4_set *);
static int        quotient_generate(automaton *, automaton *,
                      uint1, uint4, uint4, uint4_set *, bit_table *,
                      hash_table *);
static int        quotient_generate_init(automaton *, automaton *,
                      uint1, uint4_set *, bit_table *, hash_table *,
                      stack *);
static int        quotient_generate_loop(automaton *, automaton *,
                      uint1, uint4_set *, bit_table *, hash_table *,
                      stack *);
static uint4_set *quotient_states(automaton *, automaton *, uint4,
                      uint4, uint1);

/****  Private functions.                                        ****/

/**  uint4  compute_quotient_hsize(n1, n2)  :  Adjusts (heuristically)
                     the size of the hash table needed for computing
                     the quotient of an automaton with n1 states by
                     one with n2 states.                           **/

static uint4  compute_quotient_hsize(n1, n2)
  uint4  n1, n2;
{
  register uint8  nmin, nmax;

  nmin = 8 * (n1 + n2);
  nmax = (((uint8) n1) * ((uint8) n2));
  nmax = (nmax / 4 + 1) * 5;

  if (nmin > ((uint4) -1))
    nmin = auto_quotient_hsize;

  if (nmax > ((uint4) -1))
    nmax = auto_quotient_hsize;

  if (!nmin)
    nmin = 1;

  if (!nmax)
    nmax = 1;

  if (auto_quotient_hsize > nmax)
    return nmax;

  if (auto_quotient_hsize < nmin)
    return nmin;

  return auto_quotient_hsize;
}

/**  void  reverse_label(sd, ss, len, na)  :  Copies the label *ss
                    into *sd, reading from right to left instead of
                    the other way around. The number of symbols in
                    the label is len, and na bytes are needed for
                    storing each symbol.                           **/

static void  reverse_label(sd, ss, len, na)
  uint1  *sd, *ss, na;
  uint4   len;
{
  register uint4   i;
  register uint1  *ps, *pd;

  if (len * na)
    for (i = 0, ps = ss + na * (len - 1), pd = sd; i < len;
	 i++, ps -= na, pd += na)
      memcpy(pd, ps, na);
}

/**  int  set_i_states(a, s)  :  Replaces the set of initial states
                     of the finite-state automaton *a by the set *s.
                     Returns 0 in case of success, and -1 if there
                     is not enough memory.                         **/

static int  set_i_states(a, s)
  automaton *a;
  uint4_set *s;
{
  register uint4  i, n;

  auto_remove_i_states(a);

  n = set__nb_elements(s);
  for (i = 0; i < n; i++)
    if (auto_add_new_i_state(a, set__element(s, i)) < 0)
      return -1;

  return 0;
}

/**  typedef qg_info  :  Type of the data placed of the exploration
                     stack of the function quotient_generate.  The
                     first two fields give the current states of the
                     automata whose quotient is being computed.
                     The third field is a mode field; its value is
                     QG_ROOT if the function is invoked for the first
                     time in the current quotient computation,
                     QG_FIRST if it is called for the first time with
                     the current pair of states, and QG_LEFT, QG_RIGHT
                     or QG_BOTH if transitions from the left operand,
                     right operand or both operands have already been
                     followed from the current pair of states. The
                     next two fields are only used if the mode is
                     equal to QG_BOTH and correspond to the number of
                     transitions that have already been explored in
                     the operands. The last field gives the index of
                     the first transition of the second operand that
                     matches the current transition of the first
                     operand.                                      **/

typedef struct {
  uint4  m1, m2;
  uint1  mode;
  uint4  k1, k2, k2first;
} qg_info;

#define  QG_ROOT   0x01
#define  QG_FIRST  0x02
#define  QG_LEFT   0x03
#define  QG_RIGHT  0x04
#define  QG_BOTH   0x05

/**  int  quotient_generate(a1, a2, na, m1, m2, sr, bt, ht)  :  This 
                     routine is part of the quotient algorithm.  The
                     two automata whose quotient is being computed are
                     *a1 and *a2, and the number of bytes required for
                     storing one symbol of their alphabets is na. The
                     partially computed set of states of *a1 is given
                     by *sr, and a coresponding bit table is given by
                     *bt. A hash table storing each pair of state
                     indices of *a1 and *a2 that have already been
                     visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.

                     The goal of this function is to explore a part of
                     the intersection of *a1 by *a2, starting from the
                     pair of state indices of *a1 and *a2 given by
                     (m1, m2). This routine proceeds transitively,
                     updating the hash table.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  quotient_generate(a1, a2, na, m1, m2, sr, bt, ht)
  automaton  *a1, *a2;
  uint1       na;
  uint4       m1, m2;
  uint4_set  *sr;
  bit_table  *bt;
  hash_table *ht;
{
  register stack   *st;
  register uint1    m;
           qg_info  p;

  st = stack__new_empty(qg_info);
  if (!st)
    return -1;

  p.m1   = m1;
  p.m2   = m2;
  p.mode = QG_ROOT;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((qg_info *) stack__top(st)) -> mode;
      switch(m)
        {
        case QG_ROOT:
        case QG_FIRST:
          if (quotient_generate_init(a1, a2, na, sr, bt, ht, st)
              < 0)
            {
              stack__free(st);
              return -1;
            }
          continue;

        case QG_LEFT:
        case QG_RIGHT:
        case QG_BOTH:
          if (quotient_generate_loop(a1, a2, na, sr, bt, ht, st)
              < 0)
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
  return 0;
}

/**  int  quotient_generate_init(a1, a2, na, sr, bt, ht, st)  :  This
                     routine is part of the quotient algorithm.  The
                     two automata whose quotient is being computed are
                     *a1 and *a2, and the number of bytes required for
                     storing one symbol of their alphabets is na. The
                     partially computed set of states of *a1 is given
                     by *sr, and a coresponding bit table is given by
                     *bt. A hash table storing each pair of state
                     indices of *a1 and *a2 that have already been
                     visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.
 
                     The goal of this function is to explore the
                     intersection of *a1 and *a2, starting from the
                     state corresponding to the pair of states placed
                     on top of the exploration stack *st (supposed
                     non-empty and of mode equal to either QG_ROOT or
                     QG_FIRST), inserting the corresponding entry in
                     the hash table *ht. It then updates the
                     exploration stack such that the next calls to the
                     function empty_inter_generate_loop will explore
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.
                     This function updates the bit table *bt as well
                     as the set of states *sr.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  quotient_generate_init(a1, a2, na, sr, bt, ht, st)
  automaton  *a1, *a2;
  uint1       na;
  uint4_set  *sr;
  bit_table  *bt;
  hash_table *ht;
  stack      *st;
{
  register qg_info  *p;
  static   uint4     buf_uint4[2];
           void    **r;
 
  p = (qg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  QG_ROOT && p -> mode != QG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  buf_uint4[0] = p -> m1;
  buf_uint4[1] = p -> m2;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &auto_quotient_ncolls, &auto_quotient_nins) < 0)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0)
#endif  /* >= 1 */
    return -1;

  if (!r)
    {
      stack__pop(st, NULL);
      return 0;
    }

  *r = NULL;

  if (auto_accepting_state(a2, p -> m2) && !bit__member(bt, p -> m1))
    {
      if (set__add(sr, p -> m1) < 0)
        return -1;
      bit__add(bt, p -> m1);
    }

  p -> mode = QG_LEFT;
  p -> k1   = ZERO_INT4;

  return 0;
}

/**  int  quotient_generate_loop(a1, a2, na, sr, bt, ht, st)  :  This
                     routine is part of the quotient algorithm.  The
                     two automata whose quotient is being computed are
                     *a1 and *a2, and the number of bytes required for
                     storing one symbol of their alphabets is na. The
                     partially computed set of states of *a1 is given
                     by *sr, and a coresponding bit table is given by
                     *bt. A hash table storing each pair of state
                     indices of *a1 and *a2 that have already been
                     visited is given by *ht.  The outgoing
                     transitions from each state of *a1 and *a2 are
                     supposed to be sorted lexicographically.
 
                     This function explores the outgoing transitions
                     from the pair of states of *a1 and *a2 placed on
                     top of the exploration stack *st (supposed
                     non-empty and of mode equal to QG_LEFT, QG_RIGHT
                     or QG_BOTH), updating the stack such that the
                     next calls to this function and to the function
                     inter_generate_init will explore transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  quotient_generate_loop(a1, a2, na, sr, bt, ht, st)
  automaton  *a1, *a2;
  uint1       na;
  uint4_set  *sr;
  bit_table  *bt;
  hash_table *ht;
  stack      *st;
{
  register uint4    m1, m2;
  register int      c;
  register qg_info *p;
           qg_info  q;
  register tran    *t1, *t2;
           uint4    n1, n2;

  p = (qg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode != QG_LEFT && p -> mode != QG_RIGHT &&
      p -> mode != QG_BOTH)
    return -1;
#endif

  m1 = p -> m1;
  m2 = p -> m2;

  if (auto_nb_out_transitions(a1, m1, &n1) < 0 ||
      auto_nb_out_transitions(a2, m2, &n2) < 0)
    return -1;

  switch (p -> mode)
    {
    case QG_LEFT:
      if (p -> k1 >= n1)
        {
          p -> mode = QG_RIGHT; 
          p -> k2   = ZERO_INT4;
          return 0;
        }
      t1 = auto_transition(a1, m1, p -> k1);
      if (auto_transition_length(t1))
        {
          p -> mode = QG_RIGHT; 
          p -> k2   = ZERO_INT4;
          return 0;
        }
      p -> k1++;

      q.m1   = auto_transition_dest(t1);
      q.m2   = m2;
      q.mode = QG_FIRST;

      if (stack__push(st, (void *) &q) < 0)
        return -1;
      return 0;
      
    case QG_RIGHT:
      if (p -> k2 >= n2)
        {
          stack__pop(st, NULL);
          return 0;
        }
      t2 = auto_transition(a2, m2, p -> k2);
      if (auto_transition_length(t2))
        {
          p -> mode    = QG_BOTH;
          p -> k2first = p -> k2;
          return 0;
        }
      p -> k2++;

      q.m1   = m1;
      q.m2   = auto_transition_dest(t2);
      q.mode = QG_FIRST;

      if (stack__push(st, (void *) &q) < 0)
        return -1;
      return 0;
      
    case QG_BOTH:
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
          q.mode = QG_FIRST;

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

/**  uint4_set *quotient_states(a1, a2, n1, n2, na)  :  Returns the
                    set of indices of all the states of the finite-
                    state automaton *a1 that can be reached by
                    following a path labeled by a word accepted by
                    the finite-state automaton *a2. The numbers of
                    of states of *a1 and *a2 are respectively n1 and
                    n2. The number of bytes required for storing one
                    symbol of their alphabets is na. The two
                    automata are supposed to be in normal form, and
                    the outgoing transitions from each of their 
                    states are supposed to be sorted 
                    lexicographically.

                    In there is not enough memory, this function
                    returns a NULL pointer.                        **/

static uint4_set *quotient_states(a1, a2, n1, n2, na)
  automaton *a1, *a2;
  uint4      n1, n2;
  uint1      na;
{
  register uint4       i, j, k1, k2;
  register uint4_set  *sr;
  register bit_table  *bt;
  register hash_table *ht;
           uint4       dest[2];

  sr = set__new_empty();
  if (!sr)
    return NULL;

  bt = bit__new_empty(n1);
  if (!bt)
    {
      set__free(sr);
      return NULL;
    }

  ht = hash__new_empty(compute_quotient_hsize(n1, n2));
  if (!ht)
    {
      bit__free(bt);
      set__free(sr);
      return NULL;
    }
  
  k1 = auto_nb_i_states(a1);
  k2 = auto_nb_i_states(a2);

  for (i = 0; i < k1; i++)
    for (j = 0; j < k2; j++)
      if (auto_i_state(a1, i, dest) < 0 ||
	  auto_i_state(a2, j, dest + 1) < 0 ||
          (!hash__lookup_bytes(ht, (uint1 *) dest,
           2 * sizeof(uint4)) &&
           quotient_generate(a1, a2, na, dest[0], dest[1], sr,
               bt, ht) < 0))
	{
	  bytes__prepare_free(2 * sizeof(uint4));
	  hash__free(ht, (void (*)(void *)) bytes__free, NULL);
	  bit__free(bt);
	  set__free(sr);
	  return NULL;
	}

  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free, NULL);
  bit__free(bt);

  return sr;
}

/****  Public visible functions.                                 ****/

/**  void  auto_set_quotient_hsize(s)  :  Sets the size of the hash
                     table used by the quotient algorithm to s.
                     This function does not report errors.         **/

void  auto_set_quotient_hsize(s)
  uint4 s;
{
  if (s)
    auto_quotient_hsize = s;
}

/**  uint8  auto_get_quotient_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     quotient algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_quotient_ncolls()
{
  return auto_quotient_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_quotient_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     quotient algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_quotient_ncolls()
{
  auto_quotient_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_quotient_nins()  :  Returns the number of
                     insertions performed in the hash table used by
                     the quotient algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_quotient_nins()
{
  return auto_quotient_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_quotient_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the quotient algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_quotient_nins()
{
  auto_quotient_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  automaton *auto_reverse(a)  :  Returns an automaton accepting
                    the language accepted by the finite-state
                    automaton on finite words *a in which each word
                    has been reversed (i.e., is read from right to
                    left instead of the other way around).

		    In the case of an error, this function returns
                    a NULL pointer and sets lash_errno.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_reverse(a)
  automaton *a;
{
  register uint4      i, j, n, na, *st_to_num, nbytes;
  register automaton *ar;
  register tran      *t;
  register uint1     *buffer, *new;
           uint4      m;

  diag__enter("auto_reverse", NULL);

  if (auto_word_type(a) != AUTO_WORDS_FINITE)
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  n  = auto_nb_states(a);
  na = auto_alphabet_nbytes(a);

  ar = auto_new_empty(na);
  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  st_to_num = resr__new_objects(uint4, n);
  if (!st_to_num)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < n; i++)
    if (auto_add_new_state(ar, st_to_num + i) < 0 ||
        (auto_accepting_state(a, i) &&
	 (auto_add_new_i_state(ar, st_to_num[i]) < 0)))
      {
	resr__free_objects(st_to_num, uint4, n);
	auto_free(ar);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  j = auto_nb_i_states(a);
  for (i = 0; i < j; i++)
    {
      if (auto_i_state(a, i, &m) < 0)
	{
	  resr__free_objects(st_to_num, uint4, n);
	  auto_free(ar);
	  diag__fail(LASH_ERR_CORRUPT, NULL);
	}
      auto_mark_accepting_state(ar, st_to_num[m]);
    }
  
  buffer = NULL;
  nbytes = ZERO_INT4;

  for (i = 0; i < n; i++)
    {
      if (auto_nb_out_transitions(a, i, &m) < 0)
	{
	  resr__free_objects(buffer, uint4, nbytes);
	  resr__free_objects(st_to_num, uint4, n);
	  auto_free(ar);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
	  if (auto_transition_length(t) * na > nbytes)
	    {
	      new = resr__resize_objects(buffer, uint1, 
                  auto_transition_length(t) * na, nbytes);
	      if (!new)
		{
		  resr__free_objects(buffer, uint4, nbytes);
		  resr__free_objects(st_to_num, uint4, n);
		  auto_free(ar);
		  diag__fail(LASH_ERR_NO_MEM, NULL);
		}
	      nbytes = auto_transition_length(t) * na;
	      buffer = new;
	    }
          reverse_label(buffer, auto_transition_label_ptr(t, na),
             auto_transition_length(t), na);
	  if (auto_add_new_transition(ar, 
              st_to_num[auto_transition_dest(t)], st_to_num[i],
	      auto_transition_length(t), buffer) < 0)
	    {
	      resr__free_objects(buffer, uint4, nbytes);
	      resr__free_objects(st_to_num, uint4, n);
	      auto_free(ar);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	}
    }
  resr__free_objects(buffer, uint1, nbytes);
  resr__free_objects(st_to_num, uint4, n);
  
  diag__return(ar);
}

/**  automaton *auto_quotient(a1, a2)  :  Returns a finite-state
                    automaton on finite words that accepts the left
                    quotient of the language accepted by the
                    finite-state automaton *a1 by the language
                    accepted by the finite-state automaton *a2. (The
                    left quotient of L1 by L2 is the set of all the
                    words w such for which there exist w1 in L1 and w2
                    in L2 such that w1 = w2 w.

                    In the case of an error, this function returns
                    a NULL pointer and sets lash_errno.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_quotient(a1, a2)
  automaton *a1, *a2;
{
  register uint4      alph_nbytes;
  register automaton *aa, *ab;
  register uint4_set *s;

  diag__enter("auto_quotient", NULL);

  if (auto_word_type(a1) != AUTO_WORDS_FINITE ||
      auto_word_type(a2) != AUTO_WORDS_FINITE) 
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

  s = quotient_states(aa, ab, auto_nb_states(aa), 
      auto_nb_states(ab), alph_nbytes);

  auto_free(ab);

  if (!s)
    {
      auto_free(aa);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (set_i_states(aa, s) < 0 || auto_prune(aa) < 0)
    {
      set__free(s);
      auto_free(aa);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  set__free(s);
  diag__return(aa);
}

/**  automaton *auto_right_quotient(a1, a2)  :  Returns a finite-state
                    automaton on finite words that accepts the right
                    quotient of the language accepted by the
                    finite-state automaton *a1 by the language
                    accepted by the finite-state automaton *a2. (The
                    right quotient of L1 by L2 is the set of all the
                    words w such for which there exist w1 in L1 and w2
                    in L2 such that w1 = w w2.

                    In the case of an error, this function returns
                    a NULL pointer and sets lash_errno.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_right_quotient(a1, a2)
  automaton *a1, *a2;
{
  register uint4      alph_nbytes;
  register automaton *aa, *ab;
  register uint4_set *s;

  diag__enter("auto_right_quotient", NULL);

  if (auto_word_type(a1) != AUTO_WORDS_FINITE ||
      auto_word_type(a2) != AUTO_WORDS_FINITE) 
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  alph_nbytes = auto_alphabet_nbytes(a1);

  if (alph_nbytes != auto_alphabet_nbytes(a2))
    diag__fail(LASH_ERR_ALPHABET, NULL);

  aa = auto_reverse(a1);
  if (!aa)
    diag__fail(lash_errno, NULL);

  if (auto_normalize(aa) < 0)
    {
      auto_free(aa);
      diag__fail(lash_errno, NULL);
    }

  ab = auto_reverse(a2);
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

  s = quotient_states(aa, ab, auto_nb_states(aa), 
      auto_nb_states(ab), alph_nbytes);

  auto_free(ab);

  if (!s)
    {
      auto_free(aa);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (set_i_states(aa, s) < 0 || auto_prune(aa) < 0)
    {
      set__free(s);
      auto_free(aa);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  ab = auto_reverse(aa);

  auto_free(aa);
  set__free(s);

  if (!ab)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return(ab);
}

/****  End of auto-quotient.c  ****/
