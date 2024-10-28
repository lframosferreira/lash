/********************************************************************/
/**                                                                **/
/**   LASH Queue Decision Diagrams package -- v0.9                 **/
/**   ====================================                         **/
/**                                                                **/
/**    qdd-one-queue.c  :  Implementation of one-queue QDDs and    **/
/**                 operations over one-queue QDDs.                **/
/**                                                                **/
/**    10/29/99  :  Creation. (JMF)                                **/
/**    12/07/99  :  Reorganisation. (JMF)                          **/
/**    12/08/99  :  Minor correction. (JMF)                        **/
/**    01/28/00  :  qdd_one_sequence : improved algorithm. (JMF)   **/
/**    03/24/00  :  Minor bug fix. (JMF)                           **/
/**    08/14/01  :  Small adaptation. (BB)                         **/
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
#include "queue-operations.h"
#include "qdd.h"

/****  Prototype of private function.                            ****/

static int  epsilon_connected(automaton *, uint4_set *);
static int  qdd_one_receive_u(qdd *, queue_symbol *, unsigned short);
static int  qdd_one_send_u(qdd *, queue_symbol *, unsigned short,
			   stack *, stack *);
static int  fstates2stack(qdd *q, stack *);
static int  qdd_one_sequence_project(qdd *, queue_op_sequence *,
				     unsigned short int, queue);

/****  Private function.                                         ****/

/**  int  epsilon_connected(a, s)  :  Add to the set s the states
                     of the automaton *a connected to one (or more)
		     element(s) of s by a path involving epsilon
		     transitions (only).

		     *a is not modified.

		     Returns -1 and sets lash_errno in the case of
                     an error; else, returns 0.
		     
		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  epsilon_connected(a, s)
     automaton *a;
     uint4_set *s;
{
  register stack     *to_explore;
  register bit_table *explored;
  register uint4      cur_state_nb, nb_states;

#if LASH_CHECK_LEVEL >= 2
  if (!a || !s)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* < 2 */

  if (!(to_explore = stack__new_empty(uint4)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (!(explored = bit__new_empty(auto_nb_states(a))))
    {
      stack__free(to_explore);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  nb_states = set__nb_elements(s);

  for (cur_state_nb = ZERO_INT4; cur_state_nb < nb_states;
       cur_state_nb++)
    {
     uint4  cur_state = set__element(s, cur_state_nb);

     if (stack__push(to_explore, &cur_state))
       {
	 bit__free(explored);
	 stack__free(to_explore);
	 lash_errno = LASH_ERR_NO_MEM;
	 return -1;
       }

     bit__add(explored, cur_state);
    }

  while (!stack__is_empty(to_explore))
    {
      uint4           cur_state, nb_tran;
      register uint4  tran_nb;

      stack__pop(to_explore, &cur_state);

#if LASH_CHECK_LEVEL >= 2
      if (auto_nb_out_transitions(a, cur_state, &nb_tran))
	{
	  bit__free(explored);
	  stack__free(to_explore);
	  return -1;
	}
#else  /* < 2 */
      auto_nb_out_transitions(a, cur_state, &nb_tran);
#endif  /* >= 2 */

      for (tran_nb = ZERO_INT4; tran_nb < nb_tran; tran_nb++)
	{
	  register tran  *cur_tran;

	  cur_tran = auto_transition(a, cur_state, tran_nb);

#if LASH_CHECK_LEVEL >= 2  
	  if (!cur_tran)
	    {
	      bit__free(explored);
	      stack__free(to_explore);
	      return -1;
	    }
#endif  /* >= 2 */
	    
	  if (!auto_transition_length(cur_tran))
	    {
	      uint4  cur_tran_dest = auto_transition_dest(cur_tran);
	      
	      if (!bit__member(explored, cur_tran_dest))
		{
		  if (stack__push(to_explore, &cur_tran_dest))
		    {
		      bit__free(explored);
		      stack__free(to_explore);
		      lash_errno = LASH_ERR_NO_MEM;
		      return -1;
		    }
		  
		  if (set__add(s, cur_tran_dest))
		    {
		      bit__free(explored);
		      stack__free(to_explore);
		      lash_errno = LASH_ERR_NO_MEM;
		      return -1;  
		    }
		  
		  bit__add(explored, cur_tran_dest);
		}
	    }
	}
    }

  bit__free(explored);
  stack__free(to_explore);
  return 0;
}

/**  int  fstates2stack(q, s)  :  Pushes the final states of the QDD
                     *q on the stack *s.

		     *q is not modified.

		     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  fstates2stack(q, s)
     qdd *q;
     stack *s;
{
  uint4  nb_states, cur_state;

#if LASH_CHECK_LEVEL >= 2
   if (!q || !s)
     {
       lash_errno = LASH_ERR_CORRUPT;
       return -1;
     }
#endif  /* >= 2 */
   
   for (nb_states = qdd_nb_states(q), cur_state = ZERO_INT4;
	cur_state < nb_states; cur_state++)
     {
       uint4  state;

#if LASH_CHECK_LEVEL >= 2
       qdd_state(q, cur_state, &state);
#else  /* < 2 */
       if (qdd_state(q, cur_state, &state))
	 return -1;
#endif  /* >= 2 */
       
       if (qdd_accepting_state(q, state))
	 if (stack__push(s, &state))
	   {
	     lash_errno = LASH_ERR_NO_MEM;
	     return -1;
	   }
     }
   
   return 0;
}

/**  int  qdd_one_receive_u(q, rqm, u)  :  If u is null, modifies
                     the qdd *q (which accepts the language L) in
		     order to get a qdd which represents the effect
                     of the receive action
		     rqm -> queue?rqm -> symbol.

		     If u is not null, modifies the qdd *q in order
		     to get a qdd accepting L U (union) L modified by
		     the receive action rqm -> queue?rqm -> symbol.

		     The qdd q represents the content of a simple
		     queue, so obviously this queue should be
		     rqm -> queue.

		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_BAD_VALUE  : Not a receive action.
                         LASH_ERR_BAD_TRAN   : No such transion.   **/

static int  qdd_one_receive_u(q, rqm, u)
     qdd             *q;
     queue_symbol    *rqm;
     unsigned short   u;
{
  register uint4  nb_states, state_nb;
  uint4_set      *ie_states, *new_istates;

#if LASH_CHECK_LEVEL >= 1
  if (!q || !rqm)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
  
  switch(qdd_one_queue(q, queuesymbol_queue(rqm)))
    {
    case -1:
      return -1;
      
    case 0:
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 1 */

  qdd_reset_property(q, QDD_PROP_VALID_INIT_TUPLES);

  if (qdd_normalize(q))
    return -1;

  if (!(ie_states = set__new_empty()))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (!(new_istates = set__new_empty()))
    {
      set__free(ie_states);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  nb_states = qdd_nb_i_states(q);

  for (state_nb = ZERO_INT4; state_nb < nb_states; state_nb++)
    {
      uint4  cur_state;

#if LASH_CHECK_LEVEL >= 2
      if (qdd_i_state(q, state_nb, &cur_state))
	{
	  set__free(new_istates);
	  set__free(ie_states);
	  return -1;
	}
#else  /* < 2 */
      qdd_i_state(q, state_nb, &cur_state);
#endif /* >= 2 */

      if (set__add(ie_states, cur_state))
	{
	  set__free(new_istates);
	  set__free(ie_states);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}

      if (u && set__add(new_istates, cur_state))
	{
	  set__free(new_istates);
	  set__free(ie_states);
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
    }

  qdd_remove_i_states(q);

  if (epsilon_connected(qdd_automaton(q), ie_states))
    {
      set__free(ie_states);
      set__free(new_istates);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    } 

  nb_states = set__nb_elements(ie_states);

  for (state_nb = ZERO_INT4; state_nb < nb_states; state_nb++)
    {
     register uint4  cur_state = set__element(ie_states, state_nb);
     register uint4  cur_tran_nb;
     uint4           nb_trans;
     
     if (qdd_nb_out_transitions(q, cur_state, &nb_trans))
       {
	 set__free(new_istates);
	 set__free(ie_states);
	 return -1;
       }

     for (cur_tran_nb = ZERO_INT4; cur_tran_nb < nb_trans; 
	  cur_tran_nb++)
       {
	 register tran *cur_tran;
	 
	 if (!(cur_tran = qdd_transition(q, cur_state, cur_tran_nb)))
	   {
	     set__free(new_istates);
	     set__free(ie_states);
	     return -1;
	   }
	 
	 if (qdd_transition_length(cur_tran)
	     && !qdd_qm_cmp(qdd_transition_label_ptr(cur_tran), rqm)
	     && set__add(new_istates, qdd_transition_dest(cur_tran)))
	   {
	     set__free(new_istates);
	     set__free(ie_states);
	     lash_errno = LASH_ERR_NO_MEM;
	     return -1;
	   }
       }
    }
 
  set__free(ie_states);

  if (epsilon_connected(qdd_automaton(q), new_istates))
    {
      set__free(new_istates);
      return -1;
    } 

  nb_states = set__nb_elements(new_istates);
  
  for (state_nb = ZERO_INT4; state_nb < nb_states; state_nb++)
    if (qdd_add_new_i_state(q, set__element(new_istates, state_nb)))
	{
	  set__free(new_istates);
	  return -1;
	}

  set__free(new_istates);
  return 0;
}

/**  int  qdd_one_send_u(q, sqm, u, f)  :    If u is null, modifies
                     the qdd *q (which accepts the language L) in
		     order to get a qdd which represents the effect
                     of the send action sqm -> queue!sqm -> symbol.

		     If u is not null, modifies the qdd *q in order
		     to get a qdd accepting L U (union) L modified by
		     the send action sqm -> queue!sqm -> symbol.

		     If f2 != NULL, qdd_one_send_u will push the new
		     final states of *q on the stack *f2.

		     The QDD *q represents the content of a simple
		     queue, so obviously this queue should be
		     sqm -> queue.
		     
		     f can be NULL, or a pointer to a stack holding
		     the final states of *q.  In this latter case,
		     *f will be an empty stack after a successful
		     termination of qdd_one_send_u.

		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  qdd_one_send_u(q, sqm, u, f, f2)
     qdd             *q;
     queue_symbol    *sqm;
     unsigned short   u;
     stack           *f, *f2;
{
  register uint4      nb_states;
  uint4               new_state, cur_state;
  unsigned short int  freef = 0;

#if LASH_CHECK_LEVEL >= 1
 if (!q || !sqm)
      {
	lash_errno = LASH_ERR_CORRUPT;
	return -1;
      }
 
 switch(qdd_one_queue(q, queuesymbol_queue(sqm)))
   {
   case 0:
     lash_errno = LASH_ERR_CORRUPT;
     return -1;
     
   case 1:
     break;
     
   default:
     return -1;
   }
#endif  /* >= 1 */
  
  qdd_reset_property(q, QDD_PROP_VALID_INIT_TUPLES);
  
  if (qdd_add_new_state(q, &new_state))
    return -1;
  
  if (!f)
    { 
      if (!(f = stack__new_empty(uint4)))
	diag__fail(LASH_ERR_NO_MEM, -1);
      freef = 1;
      if (fstates2stack(q, f))
	diag__fail(lash_errno, -1);
    }

  for (nb_states = stack__nb_elements(f), cur_state = ZERO_INT4;
       cur_state < nb_states; cur_state++)
    {
      uint4  state;
      
#if LASH_CHECK_LEVEL < 2
      state = *(uint4 *) stack__pick(f, cur_state);
#else  /* >= 2 */
      {
	uint4 *pstate;
	
	if (!(pstate = (uint4 *) stack__pick(f, cur_state)))
	  {
	    if (freef)
	      stack__free(f);
	    return -1;
	  }
	
	state = *pstate;
      }
#endif  /* < 2 */

      if (u)
	{
	  if (f2 && stack__push(f2, &state))
	    {
	      if (freef)
		stack__free(f);
	      lash_errno = LASH_ERR_NO_MEM;
	      return -1;
	    }
	}
      else
	qdd_unmark_accepting_state(q, state);
    }
  
  while (!stack__is_empty(f))
    {
      uint4  state;

      stack__pop(f, &state);

      if (qdd_add_new_transition(q, state, new_state, 1, sqm))
	{
	  if (freef)
	    stack__free(f);
	  return -1;
	}
    }
  
  qdd_mark_accepting_state(q, new_state);
  if (f2 && stack__push(f2, &new_state))
    {
      if (freef)
	stack__free(f);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (freef)
    stack__free(f);
  return 0;
}

/**  Constants used in qdd_one_sequence_project.                    */

#define PROJ_NONE     0
#define PROJ_SEND     1
#define PROJ_RECEIVE  2

/**  int  qdd_one_sequence_project(q, seq, proj, qu)  :  Modify the
                     single-queue QDD *q in order to get a qdd which
		     represents the effect of a certain projection of
		     the operations sequence *seq.

		     The operations of the sequence *seq which do not
		     involve the queue qu are ignored.

		     If proj == PROJ_SEND then the send operations of
		     the sequence *seq are applied.  If proj ==
		     PROJ_RECEIVE then the receive operations of
		     the sequence *seq are applied.  If proj ==
		     PROJ_NONE then the sequence *seq is applied
		     without any modification.
		     
		     State indexes are not modified (no minimisation
		     applied).
		     
                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  qdd_one_sequence_project(q, seq, proj, qu)
     qdd                *q;
     queue_op_sequence  *seq;
     unsigned short int  proj;
     queue               qu;
{
  register uint4  cur_op_nb, seq_length;
  stack          *s1 = NULL, *s2 = NULL;

#if LASH_CHECK_LEVEL >= 1
  if (!q || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  seq_length = queue_op_sequence_length(seq);
  if (seq_length == ZERO_INT4)
    return 0;
  
#if LASH_CHECK_LEVEL >= 1
  switch(qdd_one_queue(q, qu))
    {
    case 0:
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
      
    case 1:
      break;
      
    default:
      return -1;
    }
#endif  /* >= 1 */

  if (proj != PROJ_RECEIVE)
    {
      if (!(s1 = stack__new_empty(uint4)) ||
	  !(s2 = stack__new_empty(uint4)))
	{
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
      
      if (fstates2stack(q, s1))
	return -1;
    }

  for (cur_op_nb = ZERO_INT4; cur_op_nb < seq_length; cur_op_nb++)
    {
      queue_operation *cur_op = queue_op_sequence_element(seq, 
							  cur_op_nb);

      if (queue_operation_isinternal(cur_op) ||
	  queue_operation_queue(cur_op) != qu)
	continue;

      if (proj != PROJ_RECEIVE && queue_operation_issend(cur_op))
	{
	  stack *s_temp;

	  if (qdd_one_send_u(q, &(cur_op -> qm), 0, s1, s2))
	    {
	      stack__free(s1);
	      stack__free(s2);
	      return -1;
	    }

	  s_temp = s2;
	  s2 = s1;
	  s1 = s_temp;
	}

      if (proj != PROJ_SEND && queue_operation_isreceive(cur_op)
	  && qdd_one_receive(q, &(cur_op -> qm)))
	{
	  if (s1)
	    {
	      stack__free(s1);
	      stack__free(s2);
	    }
	  
	  return -1;
	}
    }

  if (s1)
    {
      stack__free(s1);
      stack__free(s2);
    }
  
  return 0;
}

/****  Public functions.                                         ****/

/**  int  qdd_one_queue(q, i)  :  Returns 1 if the qdd *q operates
                     on the single queue i (i.e. all the symbols
		     labelling its non-epsilon transitions involve
		     queue i), 0 if it is not.
		     Sets *q's properties accordingly, otherwise
		     *q is not modified.
                     
		     If check level is 2 (or greater), tests if the
		     qdd is single queued, even if it is flaged with
		     QDD_PROP_ONE_QUEUE.

                     Returns -1 and sets lash_errno in the case of
                     an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_queue(q, i)
     qdd          *q;
     uint1         i;
{
  register uint4  nb_states, cur_state = ZERO_INT4;

  diag__enter("qdd_one_queue", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || i >= qdd_nb_queues(q))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

#if LASH_CHECK_LEVEL <= 1
  if (qdd_test_property(q, QDD_PROP_ONE_QUEUE))
    diag__return(1);
#endif  /* <= 1 */

  if (qdd_nb_queues(q) == 1)
    {
      qdd_set_property(q, QDD_PROP_ONE_QUEUE);
      diag__return(1);
    }

  for (nb_states = qdd_nb_states(q); cur_state < nb_states;
       cur_state++)
    {
      uint4  nb_trans, cur_trans_nb = ZERO_INT4;
     
#if LASH_CHECK_LEVEL >= 2
      if (qdd_nb_out_transitions(q, cur_state, &nb_trans))
	  diag__fail(lash_errno, -1);
#else   /* < 2 */
      qdd_nb_out_transitions(q, cur_state, &nb_trans);
#endif  /* >= 2 */

      for (; cur_trans_nb < nb_trans; cur_trans_nb++)
	{
	  tran  *cur_trans = qdd_transition(q, cur_state,
					    cur_trans_nb);
	  uint4  symb_nb = ZERO_INT4;

#if LASH_CHECK_LEVEL >= 2
	  if (!cur_trans)
	    diag__fail(lash_errno, -1);
#endif  /* >= 2 */

	  for (; symb_nb < auto_transition_length(cur_trans); 
	       symb_nb++)
	    if (i != 
		(qdd_transition_label_ptr(cur_trans)[symb_nb].queue))
	      diag__return(0);
	}
    }

  qdd_set_property(q, QDD_PROP_ONE_QUEUE);

  diag__return(1);
}

/**  int  qdd_one_receive(q, rqm)  :  Modifies the qdd *q (which
                     accepts the language L) in order to get a qdd
		     which represents the effect of the receive action
                     rqm -> queue?rqm -> symbol.

		     The qdd q represents the content of a simple
		     queue, so obviously this queue should be
		     rqm -> queue.

		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:
		     
                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
					       invalid state.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_BAD_VALUE  : Not a receive action.
                         LASH_ERR_BAD_TRAN   : No such transion.   **/

int  qdd_one_receive(q, rqm)
     qdd           *q;
     queue_symbol  *rqm;
{  
  diag__enter("qdd_one_receive", -1);
 
  if (qdd_one_receive_u(q, rqm, 0))
    diag__fail(lash_errno, -1);
  
  diag__return(0); 
}

/**  int  qdd_one_receive_union(q, rqm)  :  Modifies the qdd *q in
		     order to get a qdd accepting L U (union) L
		     modified by the receive operation
		     rqm -> queue?rqm -> symbol.

		     The qdd q represents the content of a simple
		     queue, so obviously this queue should be
		     rqm -> queue.
		     
		     State indexes are not modified (no minimisation
		     applied).
		     
                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_BAD_VALUE  : Not a receive action.
                         LASH_ERR_BAD_TRAN   : No such transion.   **/

int  qdd_one_receive_union(q, rqm)
     qdd           *q;
     queue_symbol  *rqm;
{  
  diag__enter("qdd_one_receive_union", -1);

  if (qdd_one_receive_u(q, rqm, 1))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  int  qdd_one_send(q, sqm)  :  Modifies the qdd *q (which
                     accepts the language L) in order to get a qdd
		     which represents the effect of the send action
                     sqm -> queue!sqm -> symbol.

		     The qdd q represents the content of a simple
		     queue, so obviously this queue should be
		     sqm -> queue.
		     
                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_send(q, sqm)
     qdd          *q;
     queue_symbol *sqm;
{
  diag__enter("qdd_one_send", -1);
  
  if (qdd_one_send_u(q, sqm, 0, NULL, NULL))
    diag__fail(lash_errno, -1);
  
  diag__return(0);
}

/**  int  qdd_one_send_union(q, sqm)  :  Modifies the qdd *q
                     (accepting the language L) in order to get a
		     qdd accepting L U (union) L modified by the send
		     action sqm -> queue!sqm -> symbol.

		     The qdd q represents the content of a simple
		     queue, so obviously this queue should be
		     sqm -> queue.
		     
		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_send_union(q, sqm)
     qdd          *q;
     queue_symbol *sqm;
{ 
  diag__enter("qdd_one_send_union", -1);
  
  if (qdd_one_send_u(q, sqm, 1, NULL, NULL))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  int  qdd_one_sequence  :  Modify the single-queue QDD *q in order
                     to get a qdd which represents the effect of a
		     the operations sequence *seq.
		     
		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_sequence(q, seq)
     qdd                *q;
     queue_op_sequence  *seq;
{
  queue  qu = ZERO_INT1;

  diag__enter("qdd_one_sequence", -1);

  if (queue_op_sequence_length(seq))
    qu = queue_operation_queue(queue_op_sequence_firstelement(seq));
  
  diag__return(qdd_one_sequence_project(q, seq, PROJ_NONE, qu));
}

/**  int  qdd_one_sequence_send(q, seq)  :  Modify the single-queue
                     QDD *q in order to get a qdd which represents the
		     effect of a the operations sequence (*seq)_!
		     (i.e. the sequence of operation *seq without the
		     receive operations).
		     
		     State indexes are not modified (no minimisation
		     applied).
		     
                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_sequence_send(q, seq)
     qdd               *q;
     queue_op_sequence *seq;
{
  queue  qu = ZERO_INT1;

  diag__enter("qdd_one_sequence_send", -1);

  if (queue_op_sequence_length(seq))
    qu = queue_operation_queue(queue_op_sequence_firstelement(seq));

  diag__return(qdd_one_sequence_project(q, seq, PROJ_SEND, qu));
}

/**  int  qdd_one_sequence_receive(q, seq)  :  Modify the single-queue
                     QDD *q in order to get a qdd which represents the
		     effect of a the operations sequence (*seq)_? 
		     (i.e.  the sequence of operation *seq without the
		     send operations).

		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_STATE  : Qdd contains
                                               reference to an
                                               invalid state.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_sequence_receive(q, seq)
     qdd               *q;
     queue_op_sequence *seq;
{
  queue  qu = ZERO_INT1;

  diag__enter("qdd_one_sequence_receive", -1);

  if (queue_op_sequence_length(seq))
    qu = queue_operation_queue(queue_op_sequence_firstelement(seq));
  
  diag__return(qdd_one_sequence_project(q, seq, PROJ_RECEIVE, qu));
}

/**  int  qdd_one_sequence_queue(q, seq, qu)  :  Modifies the single-
                     queue QDD *q in order to get a qdd which 
		     represents the effect of a the operations 
		     sequence (*seq)|qu (i.e. the sequence of
		     operations *seq involving the qu-th queue).

		     State indexes are not modified (no minimisation
		     applied).

                     If successful, returns 0. In the case of error,
                     returns -1 and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_STATE  : QDD contains
                                               reference to an
                                               invalid state.
			 LASH_ERR_CORRUPT    : Corrupt data.
                         LASH_ERR_PROP       : Underlying automaton
			                       with wrong known
                                               properties.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  qdd_one_sequence_queue(q, seq, qu)
     qdd               *q;
     queue_op_sequence *seq;
     queue              qu;
{
  diag__enter("qdd_one_sequence_queue", -1);

  if (qdd_one_sequence_project(q, seq, PROJ_NONE, qu))
    diag__fail(lash_errno, -1);
  
  diag__return(0);
}

/****  End of qdd-one-queue.c  ****/
