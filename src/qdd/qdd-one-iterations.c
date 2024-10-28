/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-one-iterations.c  :  Computation of the effect of the   **/
/**                 closure of a sequence of operations on a       **/
/**                 one-queued QDD.                                **/
/**                                                                **/
/**    12/17/99  :  Creation. (JMF)                                **/
/**    02/15/00  :  Reorganisation. (JMF)                          **/
/**    02/12/01  :  Minor correction. (BB)                         **/
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

#include <stdlib.h>
#include "lash-types.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "arithmetic.h"
#include "queue-operations.h"
#include "qdd.h"
#include "qdd-one-iterations.h"
#include "qdd-iterations-utils.h"

/****  Type definition.                                          ****/

typedef struct {
  bit_table          *t;
  uint4               t_nb_el;
  uint4               hash;
  unsigned short int  hash_valid;
} hbit_table;

/**  Macros.                                                       **/

#define filter_istates_intersect(q, s) filter_istates((q), (s), NULL,\
                                       0)
#define filter_istates_substract(q, s) filter_istates((q), (s), NULL,\
                                       1)
#define filter_istates_intsub(q, s1, s2) filter_istates((q), (s1), \
                                         (s2), 3)
#define append_loop(q, w, wl) append_n_loop((q), (w), (wl), 1)

/****  Prototypes of private functions.                          ****/

static uint4       bit__hash(bit_table *, uint4);
static hbit_table *hbit__new_empty(uint4);
static void        hbit__free(hbit_table *);
static uint4       hbit__nb_elements(hbit_table *);
static int         hbit__member(hbit_table *, uint4);
static int         hbit__add(hbit_table *, uint4);
static void        hbit_hash(hbit_table *);
static int         hbit__equal(hbit_table *, hbit_table *);
static int         hbit__included(hbit_table *, hbit_table *);
static int         inthbit__free(hbit_table *);
static int         istates2hbittable(qdd *, hbit_table *);
static int         hbittable2istates(hbit_table *, qdd *);
static int         append_n_loop(qdd *, queue_symbol *, uint4, uint4);
static int         nearest_states(qdd *, qdd *, uint4_set *, uint4);
static int         filter_istates(qdd *, uint4_set *, uint4_set *,
				  unsigned short int);

/****  Private functions.                                        ****/

/**  uint4 bit__hash(t, n)  :  Hashes the bit table *t.
                     The table contains n entries.
                     WARNING : this function depends of the internal
		     representation of bit tables (see datastruct.h) !
		                                                   **/

static uint4 bit__hash(t, n)
     bit_table *t;
     uint4      n;
{
  register uint4  i;
  register uint1 *p;
  register uint4  v;

#if LASH_CHECK_LEVEL >= 2
  if (!t)
    return ZERO_INT4;
#endif   /* >= 2 */

  p = t -> bits;
  v = n;

  for (i = ZERO_INT4; i < n; i++)
    {
      v *= 0x4001;
      v = (v >> 31) + (v * 2);
      v = v + (uint4) p[i];
    }

  return v;
}

/**  void hbit_hash(t)  :  Hashes the hash bit table *t.           **/

static void hbit_hash(ht)
     hbit_table *ht;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ht)
    return;
#endif   /* >= 2 */
  
#if LASH_CHECK_LEVEL < 2
  if (!ht -> hash_valid)
#endif   /* >= 2 */
    {
      ht -> hash = bit__hash(ht -> t, ht -> t_nb_el);
      ht -> hash_valid = 1;
    }
}

/**  hbit_table *hbit__new_empty(n)  :  Creates a new empty hashed bit
                     table capable of holding n entries. Returns a
                     pointer to the new table, or a NULL pointer if
                     there is not enough memory.                   **/

static hbit_table *hbit__new_empty(n)
     uint4  n;
{
  register hbit_table *ht;

  if (!(ht = resr__new_object(hbit_table)))
    return NULL;

  if (!(ht -> t = bit__new_empty(n)))
    {
      resr__free_object(ht, hbit_table);
      return NULL;
    }

  ht -> t_nb_el = n;
  ht -> hash_valid = 0;

  return ht;
}

/**  void  hbit__free(t)  :  Frees the hash bit table *t.          **/

static void hbit__free(ht)
     hbit_table *ht;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ht)
    return;
#endif   /* >= 2 */
  
  bit__free(ht -> t);
  resr__free_object(ht, hbit_table);
}

/**  uint4 hbit__nb_elements(ht)  :  Returns the number of elements
                     in the hashed bit table *hbt.                 **/

static uint4 hbit__nb_elements(ht)
     hbit_table *ht;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ht)
    return ZERO_INT4;
#endif  /* >= 2 */

  return (ht -> t_nb_el);
}

/**  int hbit__member(ht, n)  :  Returns 1 if the bit number n (n = 0,
                     1, 2, ...) is set in the hashed bit table *ht,
		     and 0 otherwise.                              **/

static int hbit__member(ht, n)
     hbit_table *ht;
     uint4       n;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ht || n >= hbit__nb_elements(ht))
    return 0;
#endif  /* >= 2 */

  return bit__member(ht -> t, n);
}

/**  int hbit__add(ht, n)  :  Sets the bit number n (n = 0, 1, ...)
                     in the hash bit table *ht.  Returns -1 in the
                     case of an out of table error, else 0.        **/

static int hbit__add(ht, n)
     hbit_table *ht;
     uint4       n;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ht)
    return -1;
#endif  /* >= 2 */
  
  ht -> hash_valid = 0;
  
  if (ht -> t_nb_el <= n)
    return -1;

  bit__add(ht -> t, n);
  return 0;
}

/**  int hbit__equal(ht1, ht2)  :  Returns 1 if ht1 and ht2 are equal
                     (i.e. if n is in ht1, then n is in ht2 and vice-
		     versa), else returns 0.                       **/

static int hbit__equal(ht1, ht2)
     hbit_table *ht1, *ht2;
{
  register uint4       i, t_nb_el;
  register hbit_table *ht;

#if LASH_CHECK_LEVEL >= 2
  if (!ht1 || !ht2)
    return -1;
#endif   /* >= 2 */
  
  hbit_hash(ht1);
  hbit_hash(ht2);

  if (ht1 -> hash != ht2 -> hash)
    return 0;

  t_nb_el = (ht1 -> t_nb_el < ht2 -> t_nb_el) ?
    ht1 -> t_nb_el : ht2 -> t_nb_el;

  for (i = ZERO_INT4; i < t_nb_el; i++)
    if (bit__member(ht1 -> t, i) != bit__member(ht2 -> t, i))
      return 0;

   if (ht1 -> t_nb_el < ht2 -> t_nb_el)
    {
      t_nb_el = ht1 -> t_nb_el;
      ht = ht1;
    }
  else
    {
      t_nb_el = ht2 -> t_nb_el;
      ht = ht2;
    }

  for (; i < t_nb_el; i++)
    if (bit__member(ht -> t, i))
      return 0;

  return 1;
}

/**  int hbit__included(ht1, ht2)  :  Returns 1 if all the bits sets
                     in *ht1 are set in *ht2, else returns 0.      **/

static int hbit__included(ht1, ht2)
     hbit_table *ht1, *ht2;
{
  register uint4  i, t_nb_el;

#if LASH_CHECK_LEVEL >= 2
  if (!ht1 || !ht2)
    return 0;
#endif   /* >= 2 */
  
  t_nb_el = (ht1 -> t_nb_el < ht2 -> t_nb_el) ?
    ht1 -> t_nb_el : ht2 -> t_nb_el;


  for (i = ZERO_INT4; i < t_nb_el; i++)
    if (bit__member(ht1 -> t, i) && !bit__member(ht2 -> t, i))
      return 0;

  for (; i < t_nb_el; i++)
    if (bit__member(ht1 -> t, i))
      return 0;
	
  return 1;
}

/**  int inthbit__free(hbt)  :  Frees the hashed bit table hbt.
                     Always returns 0.                             **/

static int inthbit__free(hbt)
     hbit_table *hbt;
{
  hbit__free(hbt);

  return 0;
}

/** int istates2hbittable(q, hbt)  :  Adds all the inital states of
                     the QDD *q to the hashed bit table *hbt.  If an
                     initial state doesn't fit into the table, it is
                     not added.
                     
                     Returns -1 in the case of an error and sets
		     lash_errno, else returns 0.

                     Possible error codes:

                         LASH_ERR_CORRUPT    : Corrupt data.       **/

static int istates2hbittable(q, hbt)
     qdd        *q;
     hbit_table *hbt;
{
  register uint4  i, nb_i_states;
  uint4           cur_i_state;

#if LASH_CHECK_LEVEL >= 2
  if (!q || !hbt)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif   /* >= 2 */

  nb_i_states = qdd_nb_i_states(q);

  for (i = 0; i < nb_i_states; i++)
    {
#if LASH_CHECK_LEVEL >= 1
      if (qdd_i_state(q, i, &cur_i_state))
	  return -1;
#else  /* < 1 */
      qdd_i_state(q, i, &cur_i_state);
#endif   /* >= 1 */
      
      if (hbit__nb_elements(hbt) > cur_i_state &&
	  hbit__add(hbt, cur_i_state))
	{
	  lash_errno = LASH_ERR_CORRUPT;
	  return -1;
	}
     }

  return 0;
}

/** int hbittable2istates(ht, q)  :  Marks the states of the QDD *q
                     corresponding to set bits in the hashed bit
		     table *hbt as inital.
                     
                     Returns -1 in the case of an error and sets
		     lash_errno, else returns 0.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

static int hbittable2istates(ht, q)
     hbit_table *ht;
     qdd        *q;
{
  uint4  i, t_nb_el;

#if LASH_CHECK_LEVEL >= 2
  if (!ht || !q)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif   /* >= 2 */

  t_nb_el = hbit__nb_elements(ht);
 
  for (i = ZERO_INT4; i < t_nb_el; i++)
    if (hbit__member(ht, i) && qdd_add_new_i_state(q, i))
      return -1;
  
  return 0;
}

/**  int append_n_loop(q, w, wl, n)  :  Changes the one-queued
                     QDD *q (accepting the language L) in order to get
		     a QDD which represents L.(w^n)* (L concatenated
		     with (w^n)*).
		     If *q is normal before the call to append_loop,
		     this property won't change during the
		     transformation.

		     wl is the length of the word w.

		     Returns -1 and sets lash_errno in the case of an
		     error, else returns 0.

		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
		         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_BAD_STATE  : No such state.
		         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int append_n_loop(q, w, wl, n)
     qdd           *q;
     queue_symbol  *w;
     uint4          wl, n;
{
  register uint4  cur_state_nb, nb_states, cur_state, cur_symbol_nb;
  uint4           first_state;

#if LASH_CHECK_LEVEL >= 2
  if (!q || (!w  && wl))
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif   /* >= 2 */
  
  if (!wl || !n)
    return 0;

  nb_states = qdd_nb_states(q);

  if (qdd_add_new_state(q, &first_state))
    return -1;

  cur_state = first_state;

  for (cur_state_nb = ZERO_INT4; cur_state_nb < nb_states; 
       cur_state_nb++)
    {
      uint4  f_state;

      if (qdd_state(q, cur_state_nb, &f_state))
	return -1;

      if (qdd_accepting_state(q, f_state))
	if (qdd_add_new_transition(q, f_state, first_state, 1, w))
	  return -1;
    }

  cur_symbol_nb = 1;
  
  for (; n; n--)
    {
      for (; cur_symbol_nb < wl; cur_symbol_nb++)
	{
	  uint4  new_state;
	  
	  if (qdd_add_new_state(q, &new_state) ||
	      qdd_add_new_transition(q, cur_state, new_state, 1,
				     (w + cur_symbol_nb)))
	    return -1;
	  
	  cur_state = new_state;
	}

      cur_symbol_nb = ZERO_INT4;
    }
  
  if (qdd_add_new_transition(q, cur_state, first_state, 1, w))
    return -1;
  
  qdd_mark_accepting_state(q, cur_state);

  return 0; 
}

/** int nearest_states(q, q_s0, s, dist)  :  Adds to the set *s the
                     states of the normal QDD *q connected to one
		     (or several) state(s) t1 (t2, t3...) of *q and
		     such that :
		     - there exists a state in the QDD *q with the
		     same number as ti (i = 1, 2, 3,...);
		     - the linking path is made with a number of
		     symbols less or equal to dist.

		     Returns 0 if successful, or a non-null value and
		     sets lash_errno in the case of an error.
		     
                     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
		         LASH_ERR_CORRUPT    : Corrupt data.
			 LASH_ERR_BAD_STATE  : Bad state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int nearest_states(q, q_s0, s, dist)
     qdd       *q, *q_s0;
     uint4_set *s;
     uint4      dist;
{
  register uint4  i, s0_nb_states;
  bit_table      *explored;
  stack          *to_explore1, *to_explore2;
  uint4_set      *to_mark;

#if LASH_CHECK_LEVEL >= 2
  if (!q || !q_s0 || !s)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
  {
    int  r = qdd_normal(q);
    
    if (!r)
      {
	lash_errno = LASH_ERR_PROP;
	return -1;
      }
    
    if (r == -1)
      return -1;
  }
#endif   /* >= 2 */

  if (!(explored = bit__new_empty(qdd_nb_states(q))))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (!(to_explore1 = stack__new_empty(uint4)))
    {
      bit__free(explored);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (!(to_explore2 = stack__new_empty(uint4)))
    {
      stack__free(to_explore1);
      bit__free(explored);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  s0_nb_states = qdd_nb_states(q_s0);
  for (i = ZERO_INT4; i < s0_nb_states; i++)
    {
      uint4  state;
      
      if (qdd_state(q_s0, i, &state) ||
	  stack__push(to_explore1, &state) ||
	  set__add(s, state))
	{
	  stack__free(to_explore1);
	  stack__free(to_explore2);
	  bit__free(explored);
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}

      bit__add(explored, state);
    }
  
  if (!(to_mark = set__new_empty()))
    {
      stack__free(to_explore1);
      stack__free(to_explore2);
      bit__free(explored);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  do
    {
      register uint4  nb_elem, elem_nb;
      stack          *exchange;

      while (!stack__is_empty(to_explore1))
	{
	  register uint4  tran_nb;
	  uint4           nb_tran, state, t_dest;
	  tran           *t;
	  
	  stack__pop(to_explore1, &state);

	  qdd_nb_out_transitions(q, state, &nb_tran);

	  for(tran_nb = ZERO_INT4; tran_nb < nb_tran; tran_nb++)
	    {
	      if (!(t = qdd_transition(q, state, tran_nb)))
		{
		  set__free(to_mark);
		  stack__free(to_explore1);
		  stack__free(to_explore2);
		  bit__free(explored);
		  return -1;
		}
	      
	      t_dest = qdd_transition_dest(t);
	      
	      if (!bit__member(explored, t_dest))
		{
		  if (qdd_transition_length(t))
		    {
		      if (dist && !set__member(to_mark, t_dest))
			if (set__add(to_mark, t_dest) ||
			    set__add(s, t_dest) ||
			    stack__push(to_explore2, &t_dest))
			  {
			    set__free(to_mark);
			    stack__free(to_explore1);
			    stack__free(to_explore2);
			    bit__free(explored);
			    lash_errno = LASH_ERR_NO_MEM;
			    return -1;
			  }
		    }
		  else
		    {
		      bit__add(explored, t_dest);
		      
		      if (set__add(s, t_dest) ||
			  stack__push(to_explore1, &t_dest))
			{
			  set__free(to_mark);
			  stack__free(to_explore1);
			  stack__free(to_explore2);
			  bit__free(explored);
			  lash_errno = LASH_ERR_NO_MEM;
			  return -1;
			}
		    }
		}
	    }
	}
      
      nb_elem = set__nb_elements(to_mark);
      for(elem_nb = ZERO_INT4; elem_nb < nb_elem; elem_nb++)
	bit__add(explored, set__element(to_mark, elem_nb));
      set__free_content(to_mark);
      
      exchange = to_explore1;
      to_explore1 = to_explore2;
      to_explore2 = exchange;
      
      dist--;
    } while(!stack__is_empty(to_explore1));
  
  set__free(to_mark);

  bit__free(explored);
  stack__free(to_explore1);
  stack__free(to_explore2);

  return 0;
}

/**  int filter_istates(q, s, s2, f)  :  Filters the initial states of
                     the QDD *q.  If (f == 0), the initial states of
		     *q which are not in the set *s are removed (i.e.
		     flagged as not being initial).  If (f == 1), the
		     initial states of *q which are in the set *s are
		     removed. If (f == 3), the initial states of *q
		     which are not in the set (*s2 \ *s) are removed.
		     
		     Returns 0 if successful, -1 and sets lash_errno
		     in the case of an error.
		     
		     Possible error codes:
		     
		        LASH_ERR_NOT_INIT  : Not initialized.
			LASH_ERR_NO_MEM    : Not enough memory.    **/

static int filter_istates(q, s, s2, f)
     qdd                *q;
     uint4_set          *s, *s2;
     unsigned short int  f;
{
  register uint4  i, nb_istates;
  uint4_set      *ns;

#if LASH_CHECK_LEVEL >= 2
  if (!q || !s || (f == 3 && !s2))
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (!(ns = set__new_empty()))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  nb_istates = qdd_nb_i_states(q);
  for (i = ZERO_INT4; i < nb_istates; i++)
    {
      uint4  istate;
      
#if LASH_CHECK_LEVEL >= 2
      if (qdd_i_state(q, i, &istate))
	return -1;
#else  /* < 2 */
      qdd_i_state(q, i, &istate);
#endif  /* >= 2 */

      if (((f == 1 && !set__member(s, istate)) ||
	   (f == 0 && set__member(s, istate)) ||
	   (f == 3 && !set__member(s, istate) && 
	    !set__member(s2, istate))) &&
	  set__add(ns, istate))
	{
	  set__free(ns);
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
    }
  
  qdd_remove_i_states(q);

  nb_istates = set__nb_elements(ns);  
  for (i = ZERO_INT4; i < nb_istates; i++)
    if (qdd_add_new_i_state(q, set__element(ns, i)))
      {
	set__free(ns);
	return -1;
      }
    
  set__free(ns);
  return 0;
}


/****  Public functions.                                         ****/

/**  int qdd_one_closure(q, seq)  :  Computes the effect of the
                     closure of the operation sequence *seq on the
		     one-queued QDD *q.

		     This algorithm comes from :
		     B. Boigelot, Symbolic Methods for Exploring
		     Infinite State Spaces, FSA - Universite de Liege,
		     Belgium, pp. 155-157, 1999.

		     Returns 0 if successful; else returns -1 and
		     sets lash_errno.

		     Possible error codes:
		     
		        LASH_ERR_NOT_INIT   : Not initialized.
		        LASH_ERR_CORRUPT    : Corrupt data.
			LASH_ERR_BAD_STATE  : Automaton contains
			                      reference to an
                                              invalid state.
			LASH_ERR_OVERFLOW   : Arithmetic overflow.
			LASH_ERR_PROP       : Automaton with wrong
			                      known properties.
		        LASH_ERR_NO_MEM     : Not enough memory.   **/

int qdd_one_closure(q, seq)
     qdd               *q;
     queue_op_sequence *seq;
{ 
  uint4           seq_send_nb, seq_receive_nb, n2, p, b, i0, s0_size;
  p_table        *i_alpha = NULL;
  hbit_table     *hbt;
  register uint4  n1 = 0;
  qdd            *a_prime, *a_i0_prime, *a_gamma;
  uint4_set      *s1;

  diag__enter("qdd_one_closure", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
  /*TODO: Verifier le one-queue.*/
  if (qdd_normalize(q))
    diag__fail(lash_errno, -1);

  sequence_count_type(seq, &seq_send_nb, &seq_receive_nb);

  /* Sequence is send only */
  if (!seq_receive_nb)
    {
      queue_symbol *qm_word;

      if (seq_send_nb)
	{
	  if (!(qm_word = resr__new_objects(queue_symbol, 
					    seq_send_nb)))
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  
	  sequence_project_type(seq, NULL, qm_word, NULL);
	  
	  if (append_loop(q, qm_word, seq_send_nb))
	    {
	      resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	      diag__fail(lash_errno, -1);
	    }
      
	  resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	}
      
      diag__return(0);
    }

  s0_size = qdd_nb_states(q);

  if (!(hbt = hbit__new_empty(s0_size)))
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (istates2hbittable(q, hbt))
    diag__fail(lash_errno, -1);

  /* Sequence is receive only */
  if (!seq_send_nb)
    {
      for (;;)
	{
	  hbit_table *hbt2;

	  if (qdd_one_sequence(q, seq))
	    {
	      hbit__free(hbt);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (!(hbt2 = hbit__new_empty(qdd_nb_states(q))))
	    {
	      hbit__free(hbt);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  if (istates2hbittable(q, hbt2))
	    {
	      hbit__free(hbt2);
	      hbit__free(hbt);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (hbit__included(hbt2, hbt))
	    {
	      hbit__free(hbt2);
	      break;
	    }
	  
	  hbit__free(hbt2);

	  if (istates2hbittable(q, hbt))
	    {
	      hbit__free(hbt);
	      diag__fail(lash_errno, -1);
	    }
	}
      
      hbittable2istates(hbt, q);

      hbit__free(hbt);
      diag__return(0);
    }

  if (!(a_i0_prime = qdd_copy(q)))
    {
      hbit__free(hbt);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if (!(i_alpha = p_table__new_empty()))
    {
      qdd_free(a_i0_prime);
      hbit__free(hbt);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  if (p_table__add(i_alpha, (void *)hbt))
    {
      qdd_free(a_i0_prime);
      p_table__free(i_alpha, (int (*)(void *))inthbit__free);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  /* Computes n1 */
  for (;;)
    {
      n1++;

      if (qdd_one_sequence(a_i0_prime, seq))
	{
	  qdd_free(a_i0_prime);
	  p_table__free(i_alpha, (int (*)(void *))inthbit__free);
	  diag__fail(lash_errno, -1);
	}
      
      if (!(hbt = hbit__new_empty(s0_size)))
	{
	  qdd_free(a_i0_prime);
	  p_table__free(i_alpha, (int (*)(void *))inthbit__free);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      if (istates2hbittable(a_i0_prime, hbt))
	{
	  qdd_free(a_i0_prime);
	  hbit__free(hbt);
	  p_table__free(i_alpha, (int (*)(void *))inthbit__free);
	  diag__fail(lash_errno, -1);
	}

      if (p_table__find(i_alpha, hbt,
			(int (*)(void *, void *))hbit__equal,
			&n2))
	{
	  hbit__free(hbt);
	  p_table__free(i_alpha, (int (*)(void *))inthbit__free);
	  break;
	}

      if (p_table__add(i_alpha, (void *)hbt))
	{
	  qdd_free(a_i0_prime);
	  hbit__free(hbt);
	  p_table__free(i_alpha, (int (*)(void *))inthbit__free);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
    }
  
  /* Computes p, b and i0 */
  if (uint4__lcm(&p, n1-n2, seq_send_nb) || 
      uint4__add(&b, n2, (seq_send_nb - (uint4) 1)) ||
      uint4__mult(&b, seq_send_nb, b / seq_send_nb))
    {
      qdd_free(a_i0_prime);
      diag__fail(LASH_ERR_OVERFLOW, -1);
    }
  
  i0 = p / seq_send_nb;
  
  if (uint4__mult(&i0, i0, (uint4) 3) ||
      uint4__mult(&i0, i0, seq_receive_nb))
    {
      qdd_free(a_i0_prime);
      diag__fail(LASH_ERR_OVERFLOW, -1);
    }
  
  i0 -= b;
  
  if (uint4__add(&i0, i0, (p - (uint4) 1)))
    {
      qdd_free(a_i0_prime);
      diag__fail(LASH_ERR_OVERFLOW, -1);
    }
  
  i0 /= p;

  if (uint4__add(&i0, i0, 1))
    {
      qdd_free(a_i0_prime);
      diag__fail(LASH_ERR_OVERFLOW, -1);
    }

  /* Computes the union of the automata APPLY-ONE(A[0], *seq^(b + pi))
     with i = 0, 1, ..., i0 - 1 (see p. 157, figure 7.20, line 65). */
  {
    qdd   *a_prime_init;
    uint4  seq_times;

    if (b >= n1)
      {
	a_prime_init = a_i0_prime;
	seq_times = b - n1;
      }
    else
      {
	a_prime_init = q;
	seq_times = b;
      }
    
    if (!(a_prime = qdd_copy(a_prime_init)))
      {
	qdd_free(a_i0_prime);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
    
    queue_op_sequence_repeat(seq, seq_times);

    if (qdd_one_sequence(a_prime, seq) ||
	queue_op_sequence_divide(seq, seq_times))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	diag__fail(lash_errno, -1);
      }
    
    queue_op_sequence_repeat(seq, p);

    if (repeated_union_seq(a_prime, seq, i0, 1) ||
	queue_op_sequence_divide(seq, p))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	diag__fail(lash_errno, -1);
      }
  }

  /* Finds A _i0 ^' */
  { 
    uint4  nb_i0;

    if (uint4__mult(&nb_i0, p, i0) ||
	uint4__add(&nb_i0, nb_i0, b))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	diag__fail(LASH_ERR_OVERFLOW, -1);
      }
    
    nb_i0 -= n1;
    
    queue_op_sequence_repeat(seq, nb_i0);

    if (qdd_one_sequence(a_i0_prime, seq) ||
	queue_op_sequence_divide(seq, nb_i0))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	diag__fail(lash_errno, -1);
      }
  }

  /* Computes S1 */
  if (!(s1 = set__new_empty()))
    {
      qdd_free(a_prime);
      qdd_free(a_i0_prime);
      diag__fail(lash_errno, -1);
    }
  {
    uint4  dist;
    
    if (uint4__mult(&dist, p, seq_receive_nb))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(LASH_ERR_OVERFLOW, -1);
      }

    if (nearest_states(a_i0_prime, q, s1, dist))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(lash_errno, -1);
      }
  }

  /* Computes A ^alphabeta (first step of the computation of A ^'
     page 65, figure 7.20) */
  {
    qdd *a_alphabeta;

    if (!(a_alphabeta = qdd_copy(a_i0_prime)))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(lash_errno, -1);
      }
    
    if (filter_istates_intersect(a_alphabeta, s1))
      {
	qdd_free(a_alphabeta);
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(lash_errno, -1);
      }
    
    {
      queue_symbol *qm_word;
      
      if (!(qm_word = resr__new_objects(queue_symbol, seq_send_nb)))
	{
	  qdd_free(a_alphabeta);
	  qdd_free(a_prime);
	  qdd_free(a_i0_prime);
	  set__free(s1);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      
      sequence_project_type(seq, NULL, qm_word, NULL);
      
      if (append_n_loop(a_prime, qm_word, seq_send_nb, p) ||
	  qdd_merge(a_prime, a_alphabeta))
	{
	  resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	  qdd_free(a_alphabeta);
	  qdd_free(a_prime);
	  qdd_free(a_i0_prime);
	  set__free(s1);
	  diag__fail(lash_errno, -1);
	}
    
      resr__free_objects(qm_word, queue_symbol, seq_send_nb);
    }

    if (qdd_free(a_alphabeta))
      {
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(lash_errno, -1);
      }
  }

  /* Computes A _i0 ^gamma' */
  if (!(a_gamma = qdd_copy(a_i0_prime)))
    {
      qdd_free(a_prime);
      qdd_free(a_i0_prime);
      set__free(s1);
      diag__fail(lash_errno, -1);
    }

  if (filter_istates_substract(a_gamma, s1))
    {
      qdd_free(a_gamma);
      qdd_free(a_prime);
      qdd_free(a_i0_prime);
      set__free(s1);
      diag__fail(lash_errno, -1);
    }
   
  /* Modifies A _i0 ^' in order to get A _i0+1 ^' (see page 155,
     figure 7.18, line 23) */
  queue_op_sequence_repeat(seq, p);

  if (qdd_one_sequence(a_i0_prime, seq) ||
      queue_op_sequence_divide(seq, p))
    {
      qdd_free(a_gamma);
      qdd_free(a_prime);
      qdd_free(a_i0_prime);
      set__free(s1);
      diag__fail(lash_errno, -1);
    }

  /* Modifies A _i0+1 ^' in order to get A _i0+1 ^delta' (see page
     156, figure 7.19, line 29) */
  {
    uint4      dist;
    uint4_set *s2;

    if (uint4__mult(&dist, p, seq_receive_nb) ||
	uint4__mult(&dist, dist, 2))
      {
	qdd_free(a_gamma);
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(LASH_ERR_OVERFLOW, -1);
      }

    if (!(s2 = set__new_empty()))
      {
	qdd_free(a_gamma);
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

    if (nearest_states(a_i0_prime, q, s2, dist))
      {
	qdd_free(a_gamma);
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	set__free(s2);
	diag__fail(lash_errno, -1);
      }

    if (filter_istates_intsub(a_i0_prime, s1, s2))
      {
	qdd_free(a_gamma);
	qdd_free(a_prime);
	qdd_free(a_i0_prime);
	set__free(s1);
	set__free(s2);
	diag__fail(lash_errno, -1);
      }

    set__free(s2);
  }

  set__free(s1);

  /* The following test appears at line 30, figure 7.19, p. 156. */
  if (seq_send_nb >= seq_receive_nb)
    {
      uint4  d;

      d = p / seq_send_nb;
      
      if (uint4__mult(&d, d, seq_send_nb - seq_receive_nb))
	{
	  qdd_free(a_gamma);
	  qdd_free(a_prime);
	  qdd_free(a_i0_prime);
	  diag__fail(LASH_ERR_OVERFLOW, -1);
	}

      { /* Computes A ^gamma' and A ^deltaepsilon' */
	/* Computes A ^gamma' */
	queue_symbol *qm_word;
	
	if (!(qm_word = resr__new_objects(queue_symbol, seq_send_nb)))
	  {
	    qdd_free(a_gamma);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  }
	
	sequence_project_type(seq, NULL, qm_word, NULL);
	
	if (append_n_loop(a_gamma, qm_word, seq_send_nb, d) ||
	    qdd_merge(a_prime, a_gamma))
	  {
	    resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	    qdd_free(a_gamma);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	if (qdd_free(a_gamma))
	  {
	    resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	/* Computes A ^deltaepsilon' */
	if (append_n_loop(a_i0_prime, qm_word, seq_send_nb, d) ||
	    append_n_loop(a_i0_prime, qm_word, seq_send_nb, p))
	  {
	    resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	
	if (qdd_merge(a_prime, a_i0_prime))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	if (qdd_free(a_i0_prime))
	  {
	    qdd_free(a_prime);
	    diag__fail(lash_errno, -1);
	  }
      }
    }
  else
    {
      { /* Computes A ^gamma' */
	uint4  i1 = p / seq_send_nb;
	uint4  divisor;

	if (uint4__mult(&divisor, i1, seq_receive_nb - seq_send_nb) ||
	    uint4__mult(&i1, i1, i0 - 1) ||
	    uint4__mult(&i1, i1, seq_receive_nb) ||
	    uint4__add(&i1, i1, b ) ||
	    uint4__add(&i1, i1, divisor - 1))
	  {
	    qdd_free(a_gamma);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  }

	i1 /= divisor;

	queue_op_sequence_repeat(seq, p);

	if (repeated_union_seq(a_gamma, seq, i1 - i0, 1) ||
	    queue_op_sequence_divide(seq, p) ||
	    qdd_merge(a_prime, a_gamma))
	  {
	    qdd_free(a_gamma);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	if (qdd_free(a_gamma))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
      }
      { /* Computes A ^deltaR' */
	uint4  i;
	sint4  i2 = p / seq_send_nb, l, tmp, tmp2;

	if (sint4__mult(&tmp, i2, seq_receive_nb - seq_send_nb) ||
	    sint4__mult(&i2, i2, ((sint4) i0) - ((sint4) 2)) ||
	    sint4__mult(&i2, i2, seq_receive_nb) ||
	    sint4__add(&i2, i2, b) ||
	    sint4__add(&i2, i2, tmp - ((sint4) 1)))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  }

	i2 /= tmp;
	
	if (sint4__add(&i2, 1, ((((sint4) i0) > i2) ? 
				(sint4) i0 : i2)) ||
	    sint4__mult(&tmp, -4, p) ||
	    sint4__mult(&tmp, tmp, seq_receive_nb) ||
	    sint4__add(&l, i0, 2) ||
	    sint4__mult(&l, l, p) ||
	    sint4__add(&l, l, b) ||
	    sint4__mult(&l, l, seq_send_nb) ||
	    sint4__add(&l, l, tmp) ||
	    uint4__add(&i, i2, p))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  }
	
	i -= p;
	i -= 2;
	
	if (sint4__mult(&tmp, p, ((sint4) seq_send_nb) -
			((sint4) seq_receive_nb)) ||
	    sint4__mult(&tmp2, p, seq_send_nb) )
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  }
	
	for (; i; i--)
	  if (l >= 0)
	    { 
	      queue_op_sequence_repeat(seq, p);
	      
	      if (repeated_union_seq(a_i0_prime, seq, 2, 1) ||
		  queue_op_sequence_divide(seq, p))
		{
		  qdd_free(a_prime);
		  qdd_free(a_i0_prime);
		  diag__fail(lash_errno, -1);
		}
	      
	      if (sint4__add(&l, l, tmp))
		{
		  qdd_free(a_prime);
		  qdd_free(a_i0_prime);
		  diag__fail(LASH_ERR_OVERFLOW, -1);
		}
	    }
	  else
	    {
	      queue_op_sequence_repeat(seq, p);

	      if (qdd_one_sequence_send(a_i0_prime, seq) ||
		  queue_op_sequence_divide(seq, p))
		{
		  qdd_free(a_prime);
		  qdd_free(a_i0_prime);
		  diag__fail(lash_errno, -1);
		}
	      
	      if (sint4__add(&l, l, tmp2))
		{
		  qdd_free(a_prime);
		  qdd_free(a_i0_prime);
		  diag__fail(LASH_ERR_OVERFLOW, -1);
		}
	    }
      }
      { /* Computes A ^deltaepsilon' */
	queue_symbol *qm_word;

	if (!(qm_word = resr__new_objects(queue_symbol, seq_send_nb)))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  }
	
	sequence_project_type(seq, NULL, qm_word, NULL);
	
	if (append_n_loop(a_i0_prime, qm_word, seq_send_nb, p))
	  {
	    resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }
	
	resr__free_objects(qm_word, queue_symbol, seq_send_nb);
	
	queue_op_sequence_repeat(seq, p);

	if (repeated_union_seq(a_i0_prime, seq, 3, 1) ||
	    queue_op_sequence_divide(seq, p) ||
	    qdd_merge(a_prime, a_i0_prime))
	  {
	    qdd_free(a_prime);
	    qdd_free(a_i0_prime);
	    diag__fail(lash_errno, -1);
	  }

	if (qdd_free(a_i0_prime))
	  {
	    qdd_free(a_prime);
	    diag__fail(lash_errno, -1);
	  }
      }
    }
  
  if (repeated_union_seq(a_prime, seq, p, 1))
    {
      qdd_free(a_prime);
      diag__fail(lash_errno, -1);
    }
  
  /* Computes the union of the automata APPLY-ONE(*q, *seq^i) with
     i = 0, 1, ..., b - 1 (see p. 157, figure 7.20, line 66). */
  if (repeated_union_seq(q, seq, b, 1))
    {
      qdd_free(a_prime);
      diag__fail(lash_errno, -1);
    }
  
  if (qdd_merge(q, a_prime))
    {
      qdd_free(a_prime);
      diag__fail(lash_errno, -1);
    }

  if (qdd_free(a_prime))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/****  End of qdd-one-iterations.c  ****/
