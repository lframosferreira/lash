/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-perform.c  :  QDD operation on a specified queue.       **/
/**                                                                **/
/**    10/29/99  :  Creation. (JMF)                                **/
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

#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "auto.h"
#include "lash-auto-operations.h"
#include "qdd-iterations-utils.h"
#include "qdd.h"

/****  Private datatypes.                                        ****/

typedef struct {                    /* Argument of                  */
  queue_op_sequence *seq;           /* qdd_one_sequence_onequeue    */
  queue              qu;
} queueseq;

/****  Prototypes of private functions.                          ****/
static uint4  hash_set(uint4_set *, uint4);

static int connected_fstates(qdd *, uint4_set *, uint4, queue);
static unsigned short tran_lt(tran *, queue);
static unsigned short tran_gt(tran *, queue);
static unsigned short tran_eq(tran *, queue);
static int cut_auto(automaton *, automaton *, uint4_set *,
		    uint4_set *, unsigned short (*)(tran *, queue),
		    queue);
static int qdd_one_sequence_onequeue(qdd *, queueseq *);

/****  Private functions.                                        ****/

/**  int connected_fstates(q, s, st, ch)  : Adds to the set s the
                     final states of q connected to the state st by
		     paths involving queue ch only (i.e. the
		     transitions along the path are epsilon
		     transitions or labeled with symbols involving
		     ch).

		     q must be normal (transitions are made of 0 or
		     1 symbol).

                     If successful, returns 0. In the case of
		     insufficient memory, returns -1.              **/

static int connected_fstates(q, s, st, ch)
     qdd       *q;
     uint4_set *s;
     uint4      st;
     queue      ch;
{
  stack     *to_explore;
  bit_table *explored;

#if LASH_CHECK_LEVEL >= 2
  {
    int  n = qdd_normal(q);

    if (n == 0 || n == -1)
      return -1;
  }
#endif  /* >=2 */

  if (!(to_explore = stack__new_empty_from_size(sizeof(uint4))))
    return -1;
  
  if (!(explored = bit__new_empty(qdd_nb_states(q))))
    {
      stack__free(to_explore);
      return -1;
    }

  if (stack__push(to_explore, &st))
    {
      stack__free(to_explore);
      bit__free(explored);
      return -1;
    }
  bit__add(explored, st);

  while (!stack__is_empty(to_explore))
    {
      uint4  cur_state, nb_outtrans, outtrans_nb;

      stack__pop(to_explore, &cur_state);

#if LASH_CHECK_LEVEL >= 2
      if (qdd_nb_out_transitions(q, cur_state, &nb_outtrans))
	{
	  stack__free(to_explore);
	  bit__free(explored);
	  return -1;
	}
#else  /* < 2 */
      qdd_nb_out_transitions(q, cur_state, &nb_outtrans);
#endif  /* >= 2 */

      for (outtrans_nb = ZERO_INT4; outtrans_nb < nb_outtrans; 
	   outtrans_nb++)
	{
	  tran  *outtrans = qdd_transition(q, cur_state, outtrans_nb);
	  uint4  trans_dest;

#if LASH_CHECK_LEVEL >= 2
	  if (!outtrans)
	    {
	      stack__free(to_explore);
	      bit__free(explored);
	      return -1;
	    }
#endif  /* >= 2 */
	  
	  trans_dest = qdd_transition_dest(outtrans);
	  
	  if ((qdd_transition_length(outtrans) &&
	       ((qdd_transition_label_ptr(outtrans))[0].queue != ch))
	      ||
	      bit__member(explored, auto_transition_dest(outtrans)))
	    continue;
	  
	  if (stack__push(to_explore, &trans_dest))
	    {
	      stack__free(to_explore);
	      bit__free(explored);
	      return -1;
	    }
	  bit__add(explored, trans_dest);

	  if (qdd_accepting_state(q, trans_dest) &&
	      set__add(s, trans_dest))
	    {
	      stack__free(to_explore);
	      bit__free(explored);
	      return -1;
	    }
	}
    }
 
  stack__free(to_explore);
  bit__free(explored);
  
  return 0;
}

/**  unsigned short tran_lt(t, i)  :  t is a normal qdd's transition.
                     Returns 1 if t is an epsilon transition or if
		     the queue number of its symbol is smaller than
                     i.  Else, returns 0.                          **/

static unsigned short tran_lt(t, i)
     tran    *t;
     queue    i;
{
  return (qdd_transition_length(t)) ?
    (queuesymbol_queue(qdd_transition_label_ptr(t)) < i) : 1;
}

/**  unsigned short tran_gt(t, i)  :  t is a normal qdd's transition.
                     Returns 1 if t is an epsilon transition or if
		     the queue number of its symbol is greater than
                     i.  Else, returns 0.                          **/
		     
static unsigned short tran_gt(t, i)
     tran    *t;
     queue    i;
{
  return (qdd_transition_length(t)) ?
    (queuesymbol_queue(qdd_transition_label_ptr(t)) > i) : 1;
}

/**  unsigned short tran_eq(t, i)  :  t is a normal qdd's transition.
                     Returns 1 if t is an epsilon transition or if
		     the queue number of its symbol is equal to i.
                     Else, returns 0.                              **/

static unsigned short tran_eq(t, i)
     tran    *t;
     queue    i;
{
  return (qdd_transition_length(t)) ?
    (queuesymbol_queue(qdd_transition_label_ptr(t)) == i) : 1;
}

/**  int cut_auto(a, na, i_states, f_states, fcmp, i)  :  Given an
                     automaton on finite words *a, constructs a new
		     one (*na) which accepts the same langage as an
		     automaton similar to *a, but :
		     - The set of initial states of a is changed.
		       The new one is i_states;  if i_states is NULL,
		       then the set of initial states if unchanged.
		     - The set of final state is changed.  The new
		       one is f_states; if f_states is NULL,
		       then the set of final states if unchanged.
		     - Some transitions are removed.  fcmp is called
		       before looking at a transition of a.  The
		       second argument of fcmp is the same as the
		       last of cut_auto (i).  If fcmp returns 1, the
		       transition is kept; if fcmp returns 0, it is
		       removed.

		     *na should be an empty automaton.
		     *a is not changed.

		     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.
		     
		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt data.
			 LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int cut_auto(a, na, i_states, f_states, fcmp, i)
     automaton       *a, *na;
     uint4_set       *i_states, *f_states;
     unsigned short (*fcmp)(tran *, queue);
     queue            i;
{
  register uint4      nb_states, nb_fistates, fistates_nb;
  register bit_table *explored_states;
  register stack     *to_explore;
  register uint4     *st_to_num;  /* st_to_num[i] is na's state number
				     corresponding to state i of a. */

#if LASH_CHECK_LEVEL >= 2
  if (!a || !na || !fcmp)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 1 */

  nb_states = auto_nb_states(a);

  if (!(explored_states = bit__new_empty(nb_states)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (!(to_explore = stack__new_empty(uint4)))
    {
      bit__free(explored_states);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (!(st_to_num = resr__new_objects(uint4, nb_states)))
    {
      stack__free(to_explore);
      bit__free(explored_states);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (i_states)
    nb_fistates = set__nb_elements(i_states);
  else
    nb_fistates = auto_nb_i_states(a);
  
  for (fistates_nb = ZERO_INT4;
       fistates_nb < nb_fistates; fistates_nb++)
    {
      uint4  cur_istate;

      if (i_states)
	cur_istate = set__element(i_states, fistates_nb);
      else
	auto_i_state(a, fistates_nb, &cur_istate);

      if (auto_add_new_state(na, st_to_num + cur_istate)
	  || auto_add_new_i_state(na, st_to_num[cur_istate]))
	{
	  resr__free_objects(st_to_num, uint4, nb_states);
	  stack__free(to_explore);
	  bit__free(explored_states);
	  return -1;
	}
   
      if (stack__push(to_explore, &cur_istate))
	{
	  resr__free_objects(st_to_num, uint4, nb_states);
	  stack__free(to_explore);
	  bit__free(explored_states);
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}

      bit__add(explored_states, cur_istate);

      if (!f_states && auto_accepting_state(a, cur_istate))
	auto_mark_accepting_state(na, st_to_num[cur_istate]);
    }

  while (!stack__is_empty(to_explore))
    {
      uint4           cur_state, nb_tran;
      register uint4  tran_nb;

      stack__pop(to_explore, &cur_state);
      
      if (auto_nb_out_transitions(a, cur_state, &nb_tran))
	{
	  resr__free_objects(st_to_num, uint4, nb_states);
	  stack__free(to_explore);
	  bit__free(explored_states);
	  return -1;
	}

      for (tran_nb = ZERO_INT4; tran_nb < nb_tran; tran_nb++)
	{
	  register tran *cur_tran;

	  if (!(cur_tran = auto_transition(a, cur_state, tran_nb)))
	    {
	      resr__free_objects(st_to_num, uint4, nb_states);
	      stack__free(to_explore);
	      bit__free(explored_states);
	      return -1;
	    }

	  if (fcmp(cur_tran, i))
	    {
	      uint4  cur_tran_dest = auto_transition_dest(cur_tran);
	      uint4  label_length = auto_transition_length(cur_tran);
	      uint1 *label_ptr =
		auto_transition_label_ptr(cur_tran,
					  auto_alphabet_nbytes(a));

	      if (!bit__member(explored_states, cur_tran_dest))
		{
		  bit__add(explored_states, cur_tran_dest);
		  
		  if (auto_add_new_state(na,
					 st_to_num + cur_tran_dest))
		    {
		      resr__free_objects(st_to_num, uint4, nb_states);
		      stack__free(to_explore);
		      bit__free(explored_states);
		      return -1;
		    }
		  
		  if (stack__push(to_explore, &cur_tran_dest))
		    {
		      resr__free_objects(st_to_num, uint4, nb_states);
		      stack__free(to_explore);
		      bit__free(explored_states);
		      lash_errno = LASH_ERR_NO_MEM;
		      return -1;
		    }

		  if (!f_states && 
		      auto_accepting_state(a, cur_tran_dest))
		    auto_mark_accepting_state(na, 
					  st_to_num[cur_tran_dest]);
		}
	      
	      if (auto_add_new_transition(na, st_to_num[cur_state],
					  st_to_num[cur_tran_dest],
					  label_length, label_ptr))
		{
		  resr__free_objects(st_to_num, uint4, nb_states);
		  stack__free(to_explore);
		  bit__free(explored_states);
		  return -1;
		}
	    }
	}
    }
  
  if (f_states)
    for (nb_fistates = set__nb_elements(f_states), fistates_nb = 
	   ZERO_INT4;
	 fistates_nb < nb_fistates; fistates_nb++)
      {
	register uint4  cur_fstate = set__element(f_states, 
						  fistates_nb);
	
	if (bit__member(explored_states, cur_fstate))
	  auto_mark_accepting_state(na, st_to_num[cur_fstate]);
      }
  
  resr__free_objects(st_to_num, uint4, nb_states);
  stack__free(to_explore);
  bit__free(explored_states);
  
  return 0;
}

/**  int  qdd_one_sequence_onequeue(qdd *, queueseq *)  :  Calls the
                     function qdd_one_sequence_queue with the
		     arguments q, queueseq->seq and queueseq->qu.  **/

static int qdd_one_sequence_onequeue(q, qs)
     qdd      *q;
     queueseq *qs;
{
#if LASH_CHECK_LEVEL >= 2
  if (!q || !qs)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  return qdd_one_sequence_queue(q, qs -> seq, qs -> qu);
}

static uint4  hash_set(s, m)
  uint4_set *s;
  uint4      m;
{
  register uint4 i, n, v;

  v = n = s -> nb;  
  v = (2654435769UL * v) >> 16;

  for (i = 0; i < n; i++)
    v = (v >> 31) + (v * 2U) + 
          ((2654435769UL * (s -> ptr[i])) >> 16);

  return (v % m);
}
/****  Public functions.                                         ****/

/**  int qdd_perform_fnct(q, i, fn, fn_arg)  :  Modify the qdd q in 
                     order to apply the fonction fn (given fn_arg as
                     an argument) to the part of q involving queue
                     number i (first queue is queue 0).
		     
		     fn takes 2 arguments : a single queue qdd* and
		     fn_arg.  fn returns 0 if successful, -1 in the
		     case of error (and sets lash_errno).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_PROP       : Automaton with wrong
                                               known properties.   **/

int qdd_perform_fnct(q, i, fn, fn_arg)
     qdd             *q;
     queue            i;
     int            (*fn)(qdd *, void *);
     void            *fn_arg; 
{
  register uint4  cur_init_tuple_nb = ZERO_INT4, hs;
  init_tuple     *cur_init_tuple;
  automaton      *aprime;
  uint4_set      *hset;
  p_table        *pts, *ptsprime;
  uint4           ns, nsprime;

  diag__enter("qdd_perform_fnct", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !fn || !fn_arg)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  /* WARNTODO : epsilon transitions not allowed in 
     qdd_compute_init_tuple */

  if (qdd_normalize(q) || qdd_compute_init_tuples(q) ||
      !(aprime = auto_new_empty(qdd_alphabet_nbytes)))
    diag__fail(lash_errno, -1);
  
  if (!(hset = set__new_empty()))
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  if (!(ptsprime = p_table__new_empty()))
    {
      set__free(hset);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  if (!(pts = p_table__new_empty()))
    {
      set__free(hset);
      p_table__free(ptsprime, NULL);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  for (cur_init_tuple = qdd_init_tuples_element(q, 0);
       cur_init_tuple_nb < qdd_nb_init_tuples(q);
       cur_init_tuple++, cur_init_tuple_nb++)
    {
      register uint1          cur_sc_nb = ZERO_INT1;
      register state_queue   *cur_sc;
      register uint4_set     *s_set, *sprime_set = NULL;
      qdd                    *a1, *a2, *a3;

      if (!(s_set = set__new_empty()))
	{
	  set__free(hset);
	  p_table__free(pts, (int (*)(void *))set__free);
	  p_table__free(ptsprime, (int (*)(void *))set__free);
	  auto_free(aprime);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      cur_sc = cur_init_tuple -> list;

      if (cur_sc -> queue == QUEUE_UNK_QUEUE)
	{ /* Epsilon word */
	  if (set__add(s_set, cur_sc -> state))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime, (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(s_set);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  if (!(sprime_set = set__new_empty()))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime, (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(s_set);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  if (set__add(sprime_set, cur_sc -> state))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime, (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(s_set);
	      set__free(sprime_set);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	}
      else
	for (; cur_sc_nb < cur_init_tuple -> nb_sc;
	     cur_sc++, cur_sc_nb++)
	  if (cur_sc -> queue >= i)
	    { /* Found the right state_queue */
	      if (!(sprime_set = set__new_empty()))
		{
		  set__free(hset);
		  p_table__free(pts, (int (*)(void *))set__free);
		  p_table__free(ptsprime,
				(int (*)(void *))set__free);
		  auto_free(aprime);
		  set__free(s_set);
		  diag__fail(LASH_ERR_NO_MEM, -1);
		}
	      
	      if (set__add(s_set, cur_sc -> state))
		{
		  set__free(hset);
		  p_table__free(pts, (int (*)(void *))set__free);
		  p_table__free(ptsprime,
				(int (*)(void *))set__free);
		  auto_free(aprime);
		  set__free(s_set);
		  set__free(sprime_set);
		  diag__fail(LASH_ERR_NO_MEM, -1);
		}
	      
	      if (cur_sc -> queue == i)
		{
		  if (cur_sc_nb + 1 < cur_init_tuple -> nb_sc)
		    {
		      if  (set__add(sprime_set,
				    (cur_sc + 1) -> state))
			{
			  set__free(hset);
			  p_table__free(pts,
					(int (*)(void *))set__free);
			  p_table__free(ptsprime,
					(int (*)(void *))set__free);
			  auto_free(aprime);
			  set__free(s_set);
			  set__free(sprime_set);
			  diag__fail(LASH_ERR_NO_MEM, -1);
			}
		    }
		  else
		    if (connected_fstates(q, sprime_set,
					  cur_sc -> state, i))
		      {
			set__free(hset);
			p_table__free(pts,
				      (int (*)(void *))set__free);
			p_table__free(ptsprime,
				      (int (*)(void *))set__free);
			auto_free(aprime);
			set__free(s_set);
			set__free(sprime_set);
			diag__fail(LASH_ERR_NO_MEM, -1);
		      }
		}
	      else
		if (set__add(sprime_set, cur_sc -> state))
		  {
		    set__free(hset);
		    p_table__free(pts, (int (*)(void *))set__free);
		    p_table__free(ptsprime,
				  (int (*)(void *))set__free);
		    auto_free(aprime);
		    set__free(s_set);
		    set__free(sprime_set);
		    diag__fail(LASH_ERR_NO_MEM, -1);
		  }
	      
	      break;
	    }

      if (cur_sc_nb == cur_init_tuple -> nb_sc)
	{ /* No init_tuple found */
	  cur_sc--;

	  if (connected_fstates(q, s_set, cur_sc -> state,
				cur_sc -> queue)
	      || !(sprime_set = set__new_copy(s_set)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(s_set);
	      diag__fail(lash_errno, -1);
	    }
	}
      
      if (!set__member(hset, hs = hash_set(s_set, 1 << 30) +
		       hash_set(sprime_set, 1 << 30)) ||
	  !p_table__find(pts, (void*)s_set,
			 (int (*)(void*, void*))set__equal,
			 &ns) ||
	  !p_table__find(ptsprime, (void*)sprime_set,
			 (int (*)(void*, void*))set__equal,
			 &nsprime) ||
	  ns != nsprime)
	{
	  if (set__add(hset, hs) ||
	      p_table__add(pts, s_set))
	    
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(s_set);
	      set__free(sprime_set);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  if (p_table__add(ptsprime, sprime_set))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      set__free(sprime_set);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  if (!(a1 = qdd_new_empty(qdd_nb_queues(q),
				   q -> queue_alphabet_size)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (!(a2 = qdd_new_empty(qdd_nb_queues(q),
				   q -> queue_alphabet_size)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (cut_auto(qdd_automaton(q), qdd_automaton(a1), NULL,
		       s_set, tran_lt, i)
	      || cut_auto(qdd_automaton(q), qdd_automaton(a2), s_set,
			  sprime_set, tran_eq, i))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a2);
	      diag__fail(lash_errno, -1);
	    }
	  
	  qdd_reset_property(a1, qdd_known_properties(a1));
	  qdd_reset_property(a2, qdd_known_properties(a2));
	  qdd_set_property(a2, QDD_PROP_ONE_QUEUE);
	  
	  if (qdd_free_init_tuples(a1) || qdd_free_init_tuples(a2))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a2);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (fn(a2, fn_arg))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a2);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (auto_concatenate(qdd_automaton(a1), qdd_automaton(a2)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a2);
	      diag__fail(lash_errno, -1);
	    }
	  
	  qdd_reset_property(a1, qdd_known_properties(a1));
	  
	  if (qdd_free(a2) ||
	      !(a3 = qdd_new_empty(qdd_nb_queues(q),
				   q -> queue_alphabet_size)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (cut_auto(qdd_automaton(q), qdd_automaton(a3),
		       sprime_set, NULL, tran_gt, i))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a3);
	      diag__fail(lash_errno, -1);
	    }
	  
	  qdd_reset_property(a3, qdd_known_properties(a3));
	  
	  if (qdd_free_init_tuples(a3) ||
	      auto_concatenate(qdd_automaton(a1), qdd_automaton(a3)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      qdd_free(a3);
	      diag__fail(lash_errno, -1);
	    }
	  
	  qdd_reset_property(a1, qdd_known_properties(a1));
	  
	  if (qdd_free(a3))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (auto_merge(aprime, qdd_automaton(a1)))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      qdd_free(a1);
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (qdd_free(a1))
	    {
	      set__free(hset);
	      p_table__free(pts, (int (*)(void *))set__free);
	      p_table__free(ptsprime,
			    (int (*)(void *))set__free);
	      auto_free(aprime);
	      diag__fail(lash_errno, -1);
	    }
	}
      else
	{
	  set__free(s_set);
	  set__free(sprime_set);
	}
    }

  set__free(hset);
  p_table__free(pts, (int (*)(void *))set__free);
  p_table__free(ptsprime, (int (*)(void *))set__free);
  
  if (qdd_free_init_tuples(q))
    {
      auto_free(aprime);
      diag__fail(lash_errno, -1);
    }

  qdd_reset_property(q, qdd_known_properties(q));
  
  if (auto_minimize(aprime) ||
      auto_replace(qdd_automaton(q), aprime))
    diag__fail(lash_errno, -1);
  
  diag__return(0);
}

/**  int  qdd_apply(q, seq)  :  Applies the sequence *seq to the QDD
                     *q.

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt data.
			 LASH_ERR_BAD_TYPE   : Bad type of QDD.
			 LASH_ERR_BAD_STATE  : Reference to an invalid
			                       state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_apply(q, seq)
     qdd               *q;
     queue_op_sequence *seq;
{
  uint1     i, n;
  queueseq  qs;

  diag__enter("qdd_apply", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  qs.seq = seq;
  n = qdd_nb_queues(q);
  
  for (i = ZERO_INT1; i < n; i++)
    {
      qs.qu = i;

      if (sequence_count_queue(seq, i) &&
	  qdd_perform_fnct(q, i,
			   (int (*)(qdd *, void *))
			   qdd_one_sequence_onequeue, &qs))
	diag__fail(lash_errno, -1);
    }

  diag__return(0);
}

/****  End of qdd-perform.c  ****/
