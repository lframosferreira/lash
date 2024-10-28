/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-machines.c  :  State machines operating over unbounded  **/
/**                 FIFO queues.                                   **/
/**                                                                **/
/**     10/29/99  :  Creation. (JMF)                               **/
/**     02/12/01  :  Minor correction. (BB)                        **/
/**     07/10/02  :  Reorganization. (BB)                          **/
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
#include <string.h>
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto.h"
#include "auto.h"
#include "lash-auto-operations.h"
#include "lash-qdd.h"
#include "qdd.h"
#include "qdd-machines.h"
#include "qdd-automaton.h"

/****  Prototypes of private functions.                          ****/

static int              qdd_relation_el_free(qdd_relation_el *);
static int              qdd_states_same_alphabet(qdd_states *, qdd *);
static automaton       *qdd_states_ctrl(uint4, uint4 *);
static automaton       *qdd_states_allctrl(uint4, uint4 *, uint4,
					   uint4);
static queue_symbol    *uint42qm(uint4 *, queue_symbol *);
static uint4           *qm2uint4(queue_symbol *, uint4 *);
static qdd_relation_el *qdd_relation_add(qdd_relation *, uint4, uint4,
					 uint4, queue_op_sequence *);
static automaton       *tran_succ(qdd_states *, uint4, uint4, uint4,
				queue_op_sequence *,
				int (*)(qdd *, queue_op_sequence *));
static automaton       *tran_succ_one(uint4 *, uint4, uint4, uint4,
				    qdd *, queue_op_sequence *,
				    int (*)(qdd *,
					    queue_op_sequence *));

/****  Private functions.                                        ****/

/** int  qdd_relation_el_free(qre)  :  Frees the transition relation
                     element qre.

		     Returns 0 if successful, -1 in the case of an
                     error.

		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT  : Corrupt automaton.    **/

static int  qdd_relation_el_free(qre)
     qdd_relation_el *qre;
{
  int  rv = 0;

  queue_op_sequence_free(qre -> seq);
  resr__free_object(qre, qdd_relation_el);

  return rv;
}

/** int  qdd_states_same_alphabet(s, q)  :  Returns 1 if the alphabets
                     of the QDDs held in the states of *s are equal
		     to those of *q; else returns 0.               **/

static int  qdd_states_same_alphabet(s, q)
     qdd_states *s;
     qdd        *q;
{
  uint1  i;

#if LASH_CHECK_LEVEL >= 2
  if (!s || !q)
      return -1;
#endif  /* >= 2 */

  if (qdd_nb_queues(q) != s -> nb_queues)
    return 0;

  for (i = ZERO_INT1; i < s -> nb_queues; i++)
    if (qdd_queue_alphabet_size(q, i) !=
	s -> queue_alphabet_size[i])
      return 0;

  return 1;
}

/**  automaton *qdd_state_ctrl(cl, cs)  :  Returns a new automaton
                     accepting the encoding of the control state cs.
		     cs has a control state divided in cl control
		     components.

		     Returns a pointer to the new automaton if
		     successful, or sets lash_errno and returns NULL
		     in the case of an error.

		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static automaton *qdd_states_ctrl(cl, cs)
     uint4  cl, *cs;
{
  automaton *a;
  uint4      ns, i;
  
#if LASH_CHECK_LEVEL >= 2
  if (!cl || !cs)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif  /* >= 2 */

  if (!(a = auto_new_empty(qdd_alphabet_nbytes)))
    return NULL;
  
  if (auto_add_new_state(a, &ns) ||
      auto_add_new_i_state(a, ns))
	{
	  auto_free(a);
	  return NULL;
	}

  for (i = ZERO_INT4; i < cl; i++)
    {
      uint4         ns2;
      queue_symbol  qm;
      
#if LASH_CHECK_LEVEL >= 2
      if (auto_add_new_state(a, &ns2) ||
	  !uint42qm(cs + i, &qm) ||
	  auto_add_new_transition(a, ns, ns2, 1, (uint1*) &qm))
#else  /* < 2 */
	if (auto_add_new_state(a, &ns2) ||
	    auto_add_new_transition(a, ns, ns2, 1,
				    (uint1*) uint42qm(cs + i, &qm)))
#endif  /* >= 2 */
	  {
	    auto_free(a);
	    return NULL;
	  }
      
      ns = ns2;
    }
  
  auto_mark_accepting_state(a, ns);
  
  return a;
}

/**  automaton *qdd_state_allctrl(cl, nb_ctrl, np, n)  :  Returns a
                     new automaton accepting all the encodings of
		     control states with cl components and such that
		     the i-th control symbol is smaller than
		     nb_ctrl[i] (i = 0, 1,..., cl) and the np-th - 1
		     is n.
		     
		     If np is equal to cl, then n is not taken into
		     account.

		     Returns a pointer to the new automaton if
		     successful, or sets lash_errno and returns NULL
		     in the case of an error.

		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static automaton *qdd_states_allctrl(cl, nb_ctrl, np, n)
     uint4  cl, np, n, *nb_ctrl;
{
  automaton *a;
  uint4      ns, ns2, i;
  
#if LASH_CHECK_LEVEL >= 2
  if (!cl || !nb_ctrl || np > cl)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif  /* >= 2 */

  if (!(a = auto_new_empty(qdd_alphabet_nbytes)))
    return NULL;
  
  if (auto_add_new_state(a, &ns) ||
      auto_add_new_i_state(a, ns))
	{
	  auto_free(a);
	  return NULL;
	}

  for (i = ZERO_INT4; i < cl; i++)
    {
      uint4         j;
      queue_symbol  qm;
      
      if (auto_add_new_state(a, &ns2))
	{
	  auto_free(a);
	  return NULL;
	}
      
      if (i == np)
	{
	  if (!(n < nb_ctrl[i]))
	    {
	      lash_errno = LASH_ERR_BAD_VALUE;
	      auto_free(a);
	      return NULL;
	    } 
	  
#if LASH_CHECK_LEVEL >= 2
	  if (!uint42qm(&n, &qm) ||
	      auto_add_new_transition(a, ns, ns2, 1, (uint1*) &qm))
#else  /* < 2 */
	    if (auto_add_new_transition(a, ns, ns2, 1,
					(uint1*) uint42qm(&n, &qm)))
#endif  /* >= 2 */ 
	      {
		auto_free(a);
		return NULL;
	      }
	}
      else
	for (j = ZERO_INT4; j < nb_ctrl[i]; j++)
#if LASH_CHECK_LEVEL >= 2
	  if (!uint42qm(&j, &qm) ||
	      auto_add_new_transition(a, ns, ns2, 1, (uint1*) &qm))
#else  /* < 2 */
	    if (auto_add_new_transition(a, ns, ns2, 1,
					(uint1*) uint42qm(&j, &qm)))
#endif  /* >= 2 */
	      {
		auto_free(a);
		return NULL;
	      }
      
      ns = ns2;
    }
  
  auto_mark_accepting_state(a, ns);
  
  return a;
}

/**  queue_symbol *uint42qm(ui, qm)  :  Converts an unsigned 
                     integer *ui to a queue_symbol *qm.  Returns
		     qm.                                           **/

static queue_symbol *uint42qm(ui, qm)
     uint4        *ui;
     queue_symbol *qm;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ui || !qm || *ui > 0xFFFFUL)
    return NULL;
#endif  /* >= 2 */

  qm -> queue = (*ui >> 8) % (uint4)256;
  qm -> symbol = *ui % (uint4)256;

  return qm;
}

/**  uint4 *qm2uint4(qm, ui)  :  Converts queue_symbol *qmto an 
                     unsigned integer *ui.  Returns ui.            **/

static uint4 *qm2uint4(qm, ui)
     uint4        *ui;
     queue_symbol *qm;
{
#if LASH_CHECK_LEVEL >= 2
  if (!ui || !qm)
    return NULL;
#endif  /* >= 2 */

  *ui = (uint4)qm -> queue * (uint4)(1 << 8) + (uint4)qm -> symbol;

  return ui;
}

/**  qdd_relation_el *qdd_relation_add(t, np, no, nd, seq)  :  Adds
                     a transition to the transition relation *t.
		     This transition links the control states such
		     that the (np+1)-th component of their control
		     descriptor is equal to no to those for which
		     this component is equal to nd (the value of
		     the other components of the control descriptor
		     does not change).  The transition is labeled by
		     the sequence of operation *seq.

		     The type (simple/meta transition) of the new
		     transition is left in an undetermined state.

		     In the case of success, this function returns
		     the new transition.
		     In the case of an error, it returns NULL.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt structure
			                       automaton.          **/

static qdd_relation_el *qdd_relation_add(t, np, no, nd, seq)
     qdd_relation      *t;
     uint4              np, no, nd;
     queue_op_sequence *seq;
{
  qdd_relation_el *te;

#if LASH_CHECK_LEVEL >= 2
  if (!t || !(t -> nb_control))
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif  /* >= 2 */

  if (!(te = resr__new_object(qdd_relation_el)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  te -> np = np;
  te -> no = no;
  te -> nd = nd;

  te -> seq = seq;
  te -> next = t -> list;
  t -> list = te;

  return te;
}

/**  struct state_tran  :  This data structure is put on the
                     exploration stack of the function tran_succ.
		     The field state is the state number of the
		     visited state.
		     The field tran_nb is the number of next
		     transition (of the state 'state') to visit.
		     The field isepsilon equals 1 iff the
		     tran_nb-th transition is an epsilon transition.
		                                                  **/

typedef struct {
  uint4               state, tran_nb;
  unsigned short int  isepsilon;
} state_tran;

/**  automaton *tran_succ(s, np, no, nd, seq, f)  :  Returns an
                     automaton which represents a set of states.
		     This set of state is the image of the set *s
		     by a transition labeled with the sequence of
		     operations *seq.  This transition involve the
		     np+1-th component of the control state; this
		     component is no for the origin of the transition,
		     nd for the destination of the transition.

		     The function *f computes the effect of *seq to
		     a given QDD.  It takes 2 arguments; the first is
		     the QDD, the second the sequence of queue
		     operations.  It must return 0 if successful, -1
		     (and sets lash_errno) in the case of an error.

		     The automaton underlying the set *s is first
		     minimized and normalized.

		     Possible error codes:

                         Depends of the function f.                **/

static automaton *tran_succ(s, np, no, nd, seq, f)
     qdd_states        *s;
     uint4              np, no, nd;
     int              (*f)(qdd *, queue_op_sequence *);
     queue_op_sequence *seq;
{
  stack     *t;
  uint4     *origin, depth, i_state_nb;
  automaton *a, *rv;

#if LASH_CHECK_LEVEL >= 2
  if (!s || !(s -> states) || !(s -> ctrl_len) ||
      np >= s -> ctrl_len || !seq || !f)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif  /* >= 2 */   
  
  if (auto_minimize(s -> states) ||
      auto_normalize(s -> states))
    return NULL;
  
  a = s -> states;

  if (!(origin = resr__new_objects(uint4, s -> ctrl_len)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  if (!(rv = auto_new_empty(qdd_alphabet_nbytes)))
    {
      resr__free_objects(origin, uint4, s -> ctrl_len);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  if (!(t = stack__new_empty(state_tran)))
    {
      resr__free_objects(origin, uint4, s -> ctrl_len);
      auto_free(rv);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  for (i_state_nb = ZERO_INT4; i_state_nb < auto_nb_i_states(a);
       i_state_nb++)
    {
      state_tran  st;

      if (auto_i_state(a, i_state_nb, &st.state))
	{
	  resr__free_objects(origin, uint4, s -> ctrl_len);
	  stack__free(t);
	  return NULL;
	}
      
      st.tran_nb = ZERO_INT4;
      st.isepsilon = 0;

      if (stack__push(t, (void *)&st))
	{
	  resr__free_objects(origin, uint4, s -> ctrl_len);
	  auto_free(rv);
	  stack__free(t);
	  lash_errno = LASH_ERR_NO_MEM;
	  return NULL;
	}

      depth = ZERO_INT4;

      while(!stack__is_empty(t))
	if (depth < s -> ctrl_len)
	  {
	    state_tran *st = stack__top(t), ne;
	    uint4       nb_out_tran;
	    int         isatomic = 0;

#if LASH_CHECK_LEVEL >= 2
	    if (!st)
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		auto_free(rv);
		stack__free(t);
		lash_errno = LASH_ERR_CORRUPT;
		return NULL;
	      }
#endif  /* >= 2 */
	    
	    if (auto_nb_out_transitions(a, st -> state, &nb_out_tran))
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		auto_free(rv);
		stack__free(t);
		return NULL;
	      }

	    if (nb_out_tran > st -> tran_nb)
	      {
		queue_symbol *label;
		uint4         ui;
		tran         *cur_tran =
		  auto_transition(a, st -> state, st -> tran_nb++);

#if LASH_CHECK_LEVEL >= 1
		if (!cur_tran)
		  {
		    resr__free_objects(origin, uint4, s -> ctrl_len);
		    auto_free(rv);
		    stack__free(t);
		    return NULL;
		  }
#endif  /* >= 1 */

		if (auto_transition_length(cur_tran))
		  {
		    label = (queue_symbol *)
		      auto_transition_label_ptr(cur_tran,
						qdd_alphabet_nbytes);
		    
#if LASH_CHECK_LEVEL >= 2
		    if (!qm2uint4(label, &ui))
		      {
			resr__free_objects(origin, uint4,
					   s -> ctrl_len);
			auto_free(rv);
			stack__free(t);
			return NULL;
		      }
#else  /* < 2 */
		    qm2uint4(label, &ui);
#endif  /* >= 2 */
		  
		    ne.isepsilon = 0;
		  }
		else
		  ne.isepsilon = 1;

		if (depth == np)
		  isatomic = 0;
		else
		  if (!ne.isepsilon &&
		      (isatomic =
		       qdd_states_isatomic(s, depth, ui)) == -1)
		    {
		      resr__free_objects(origin, uint4,
					 s -> ctrl_len);
		      auto_free(rv);
		      stack__free(t);
		      lash_errno = LASH_ERR_NO_MEM;
		      return NULL;
		    }

		if (ne.isepsilon || (!isatomic && (depth != np ||
		    ui == no)))
		  {
		    ne.state = auto_transition_dest(cur_tran);
		    ne.tran_nb = ZERO_INT4;
		      
		    if (!ne.isepsilon)
		      origin[depth++] = ui;

		    if (stack__push(t, (state_tran *)&ne))
		      {
			resr__free_objects(origin, uint4,
					   s -> ctrl_len);
			auto_free(rv);
			stack__free(t);
			lash_errno = LASH_ERR_NO_MEM;
			return NULL;
		      }
		  }
	      }
	    else
	      {
		state_tran  *top = stack__top(t);
		
#if LASH_CHECK_LEVEL >= 2
		if (!top)
		  {
		    resr__free_objects(origin, uint4, s -> ctrl_len);
		    auto_free(rv);
		    stack__free(t);
		    lash_errno = LASH_ERR_CORRUPT;
		    return NULL;
		  }
#endif  /* >= 2 */
		
		if (!top -> isepsilon)
		  depth--;
		
		stack__pop(t, NULL);
	      }
	  }
	else
	  {
	    automaton *a_result;
	    qdd       *q;

	    if (!(q = qdd_states_get_data(s, origin, NULL)))
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		stack__free(t);
		auto_free(rv);
		return NULL;
	      }
	    
	    if (!(a_result =
		  tran_succ_one(origin, s -> ctrl_len, np, nd, q, 
				seq, f)))
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		stack__free(t);
		qdd_free(q);
		auto_free(rv);
		return NULL;
	      }

	    if (qdd_free(q) || auto_merge(rv, a_result))
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		stack__free(t);
		auto_free(a_result);
		auto_free(rv);
		return NULL;
	      }
	    
	    if (auto_free(a_result))
	      {
		resr__free_objects(origin, uint4, s -> ctrl_len);
		stack__free(t);
		auto_free(rv);
		return NULL;
	      }
	    {
	      state_tran  *top = stack__top(t);
	      
#if LASH_CHECK_LEVEL >= 2
	      if (!top)
		{
		  resr__free_objects(origin, uint4, s -> ctrl_len);
		  auto_free(rv);
		  stack__free(t);
		  lash_errno = LASH_ERR_CORRUPT;
		  return NULL;
		}
#endif  /* >= 2 */

	      if (!top -> isepsilon)
		  depth--;
	      
	      stack__pop(t, NULL);		
	    }
	  }
    }
  
  resr__free_objects(origin, uint4, s -> ctrl_len);
  stack__free(t);
  return rv;
}

/**  automaton *tran_succ_one(origin, ctrl_len, np, nd, q, seq, f)  :
                     Returns an automaton which is a representation of
		     a set of state with 1 element.  The control part
		     of this element is given by the vector of
		     integers origin (of size ctrl_len) with its
		     np-th element set to nd.  The data part of the
		     element is the QDD *q modified by the function
		     f (seq is a sequence of queue operations given
		     as an argument).

		     f takes 2 arguments; the first is a QDD, the
		     second a sequence of queue operations.  It
		     must return 0 if successful, -1 (and sets
		     lash_errno) in the case of an error.

		     Possible error codes:

                         Depends of the function f.                **/

static automaton *tran_succ_one(origin, ctrl_len, np, nd, q, seq, f)
     uint4             *origin, ctrl_len, np, nd;
     qdd               *q;
     queue_op_sequence *seq;
     int              (*f)(qdd *, queue_op_sequence *);
{
  automaton *a;
  uint4      no;

#if LASH_CHECK_LEVEL >= 2
  if (!origin || np >= ctrl_len || !seq || !f)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif  /* >= 2 */

  if (f(q, seq))
    return NULL;

  no = origin[np];
  origin[np] = nd;

  if (!(a = qdd_states_ctrl(ctrl_len, origin)))
    return NULL;

  origin[np] = no;

  if (auto_concatenate(a, qdd_automaton(q)))
    {
      auto_free(a);
      return NULL;
    }
  
  return a;
}

/****  Public functions.                                         ****/

/**  qdd_states *qdd_states_new_empty(cl, mc, nq, qas, o)  :  Creates
                     a new empty set of states for a machine whose
		     control states are described by cl unsigned
		     integers and whose data is composed by the
		     content of nq empty queues (the alphabet of the
		     queues is described in qas).  mc holds the
		     number of control locations in each process.
		     qas and mc are copied.

		     The set of queue content is empty for every
		     state but *o; the set of queue content of the
		     state *o has 1 element : the empty queue.
		     
		     Returns a pointer to the new qdd_states if
		     successful, or sets lash_errno and returns NULL
		     in the case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
			 LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

qdd_states *qdd_states_new_empty(cl, mc, nq, qas, o)
     uint4  cl, *mc, *o;
     uint1  nq, *qas;
{
  qdd_states *qs;
  automaton  *a;
  qdd        *q;
  uint4       i;

  diag__enter("qdd_states_new_empty", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!cl || !o || !nq || nq == QUEUE_UNK_QUEUE || !qas)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!(qs = resr__new_object(qdd_states)))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  qs -> ctrl_len = cl;
  qs -> nb_queues = nq;

  if (!(qs -> queue_alphabet_size = resr__new_objects(uint1, nq)))
    {
      resr__free_object(qs, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(qs -> queue_alphabet_size, qas, nq * sizeof(uint1));

  if (!(qs -> nb_control = resr__new_objects(uint4, cl)))
    {
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = ZERO_INT4; i < cl; i++)
    if ((qs -> nb_control[i] = mc[i]) > 0xFFFF + 1)
      {
	resr__free_objects(qs -> nb_control, uint4, cl);
	resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
	resr__free_object(qs, qdd_states);
	diag__fail(LASH_ERR_BAD_VALUE, NULL);
      }

  if (!(a = qdd_states_ctrl(cl, o)))
    {
      resr__free_objects(qs -> nb_control, uint4, cl);
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(lash_errno, NULL);
    }

  if (!(q = qdd_new_empty_queue(nq, qas)))
    {
      auto_free(a);
      resr__free_objects(qs -> nb_control, uint4, cl);
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(lash_errno, NULL);
    }
  
  if (auto_concatenate(a, qdd_automaton(q)) ||
      auto_minimize(a))
    {
      auto_free(a);
      qdd_free(q);
      resr__free_objects(qs -> nb_control, uint4, cl);
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(lash_errno, NULL);
    }

  if (qdd_free(q))
    {
      auto_free(a);
      resr__free_objects(qs -> nb_control, uint4, cl);
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(lash_errno, NULL);
    }

  if (!(qs -> atomic = resr__new_objects(bit_table *, cl)))
    {
      auto_free(a);
      resr__free_objects(qs -> nb_control, uint4, cl);
      resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
      resr__free_object(qs, qdd_states);
      diag__fail(lash_errno, NULL);
    }

  for (i = ZERO_INT4; i < cl; i++)
    if (!((qs -> atomic)[i] =
	  bit__new_empty((qs -> nb_control)[i])))
      {
	while(i)
	  bit__free((qs -> atomic)[--i]);
	
	resr__free_objects(qs -> atomic, bit_table *, cl);
	auto_free(a);
	resr__free_objects(qs -> nb_control, uint4, cl);
	resr__free_objects(qs -> queue_alphabet_size, uint1, nq);
	resr__free_object(qs, qdd_states);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }
  
  qs -> states = a;

  diag__return(qs);
}

/**  qdd_states *qdd_states_copy(s)  :  Builds a copy of the set of
                     states *s.

		     In the case of success, this function returns a
		     pointer to the new set of states.
                     In the case of an error, it returns -1 and sets
		     lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

qdd_states *qdd_states_copy(s)
     qdd_states *s;
{
  qdd_states *rv;
  uint4       i;

  diag__enter("qdd_states_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !(s -> states) || !(s -> ctrl_len) || !(s -> nb_queues) ||
      !(s -> nb_control) || !(s -> queue_alphabet_size))
    diag__fail(LASH_ERR_CORRUPT, NULL);   
#endif  /* >= 1 */

  if (!(rv = resr__new_object(qdd_states)))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (!(rv -> nb_control = resr__new_objects(uint4, s -> ctrl_len)))
    {
      resr__free_object(rv, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(rv -> nb_control, s -> nb_control,
	 s -> ctrl_len * sizeof(uint4));
  rv -> ctrl_len = s -> ctrl_len;

  if (!(rv -> queue_alphabet_size =
	resr__new_objects(uint1, s -> nb_queues)))
    {
      resr__free_objects(rv -> nb_control, uint4, s -> ctrl_len);
      resr__free_object(rv, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(rv -> queue_alphabet_size, s -> queue_alphabet_size,
	 s -> nb_queues * sizeof(uint1));
  rv -> nb_queues = s -> nb_queues;

  if (!(rv -> states = auto_copy(s -> states)))
    {
      resr__free_objects(rv -> nb_control, uint4, s -> ctrl_len);
      resr__free_objects(rv -> queue_alphabet_size, uint1,
			 s -> nb_queues);
      resr__free_object(rv, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (!(rv -> atomic = resr__new_objects(bit_table *, s -> ctrl_len)))
    {
      auto_free(rv -> states);
      resr__free_objects(rv -> nb_control, uint4, s -> ctrl_len);
      resr__free_objects(rv -> queue_alphabet_size, uint1,
			 s -> nb_queues);
      resr__free_object(rv, qdd_states);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = ZERO_INT4; i < s -> ctrl_len; i++)
    if (!((rv -> atomic)[i] =
	  bit__new_copy((s -> atomic)[i])))
      {
	while(i)
	  bit__free((rv -> atomic)[--i]);
	
	resr__free_objects(rv -> atomic, bit_table *, s -> ctrl_len);
	auto_free(rv -> states);
	resr__free_objects(rv -> nb_control, uint4, s -> ctrl_len);
	resr__free_objects(rv -> queue_alphabet_size, uint1,
			   s -> nb_queues);
	resr__free_object(rv, qdd_states);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  diag__return(rv);
}

/**  int  qdd_states_free(s)  :  Frees the set of states *s.

                     In the case of success, this function returns 0.
                     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  qdd_states_free(s)
     qdd_states *s;
{
  uint4  i;

  diag__enter("qdd_states_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !(s -> states))
    diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  if (auto_free(s -> states))
    diag__fail(lash_errno, -1); 
  
  for (i = ZERO_INT4; i < s -> ctrl_len; i++)
    bit__free((s -> atomic)[i]);

  resr__free_objects(s -> atomic, bit_table *, s -> ctrl_len);
  resr__free_objects(s -> queue_alphabet_size, uint1, s -> nb_queues);
  resr__free_objects(s -> nb_control, uint4, s -> ctrl_len);
  resr__free_object(s, qdd_states);

  diag__return(0);
}

/**  int  qdd_states_add_atomic(s, np, ni)  :  Declares the (ni+1)-th
                     control location of the (np+1)-th control
		     component as atomic.

		     Returns 0 if successful, -1 in the case of an
		     error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupted data.     **/

int  qdd_states_add_atomic(s, np, ni)
     qdd_states *s;
     uint4       np, ni;
{
  diag__enter("qdd_states_add_atomic", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !s -> atomic || !s -> nb_control ||
      !np >= s -> ctrl_len || ni > (s -> nb_control)[np])
    diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  bit__add(s -> atomic[np], ni);
  
  diag__return(0);
}

/**  int  qdd_states_isatomic(s, np, ni)  :  Returns 1 if the
                     (ni+1)-th control location of the (ni+1)-th
		     control component of the set of states *s is
		     atomic.

		     Returns -1 in the case of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupted data.     **/

int  qdd_states_isatomic(s, np, ni)
     qdd_states *s;
     uint4       np, ni;
{
  diag__enter("qdd_states_isatomic", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !s -> atomic || !s -> nb_control ||
      !np >= s -> ctrl_len || ni > (s -> nb_control)[np])
    diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

   diag__return(bit__member(s -> atomic[np], ni));
}

/**  int  qdd_states_add_data(s, ctrl, q)  :  Adds a state with
                     control ctrl and data *q in the set of states
		     *s.

		     Return 0 if successful, -1 (and sets lash_errno)
		     in the case of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_ALPHABET   : Bad alphabet.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_BAD_STATE  : Automaton contains
			                       reference to an
					       invalid state.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  qdd_states_add_data(s, ctrl, q)
     qdd_states *s;
     uint4      *ctrl;
     qdd        *q;
{
  automaton *a;

  diag__enter("qdd_states_add_data", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !(s -> states) || !ctrl)
    diag__fail(LASH_ERR_CORRUPT, -1);

#if LASH_CHECK_LEVEL >= 2    
  switch(qdd_states_same_alphabet(s, q))
    {
    case 0:
      diag__fail(LASH_ERR_ALPHABET, -1);
    case 1:
      break;
    default:
      diag__fail(LASH_ERR_CORRUPT, -1);
    }
#else  /* < 2 */
  if (!qdd_states_same_alphabet(s, q))
    diag__fail(LASH_ERR_ALPHABET, -1);
#endif  /* >= 2 */
#endif  /* >= 1 */

  if (!(a = qdd_states_ctrl(s -> ctrl_len, ctrl)))
    diag__fail(lash_errno, -1);

  if (auto_concatenate(a, qdd_automaton(q)) ||
      auto_merge(s -> states, a) ||
      auto_minimize(s -> states))
    {
      auto_free(a);
      diag__fail(lash_errno, -1);
    }
  
  if (auto_free(a))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  qdd *qdd_states_get_data(s, cs, pm)  :  Computes a
                     representation of the set of data values
		     associated to at least one control state
		     described by the array of integers cs associated
		     with the array of masks *pm (the value of a
		     component cs[i] is only considered if pm[i] != 0,
		     otherwise it is ignored).

		     If cs is NULL then it's equivalent to an array
		     pm filled with zeroes.

		     If pm is NULL (but cs isn't) then all the
		     components of cs are relevant.

                     In the case of success, this function returns a
                     pointer to a newly allocated QDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

qdd *qdd_states_get_data(s, cs, pm)
     qdd_states *s;
     uint4      *cs;
     int        *pm;
{
  qdd       *q;
  uint4      i;
  automaton *a, *data;

  diag__enter("qdd_states_get_data", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!s || !(s -> states))
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */
 
  if (pm)
    {
      if (!(a = qdd_states_allctrl(s -> ctrl_len, s -> nb_control,
				   s -> ctrl_len, ZERO_INT4)))
	diag__fail(lash_errno, NULL);
      
      for (i = ZERO_INT4; i < s -> ctrl_len; i++)
	{
	  if (cs && pm[i])
	    {
	      automaton *a2, *ai;
	      
	      if (!(a2 = qdd_states_allctrl(s -> ctrl_len, 
					    s -> nb_control,
					    i, cs[i])))
		{
		  auto_free(a);
		  diag__fail(lash_errno, NULL);
		}
	      
	      if (!(ai = auto_intersection(a, a2)))
		{
		  auto_free(a);
		  auto_free(a2);
		  diag__fail(lash_errno, NULL);
		}
	      
	      if (auto_free(a))
		{
		  auto_free(a2);
		  diag__fail(lash_errno, NULL);
		}
	      
	      if (auto_free(a2))
		diag__fail(lash_errno, NULL);
	      
	      a = ai;
	    }
	}
    }
  else
    {
      if (!(a = qdd_states_ctrl(s -> ctrl_len, cs)))
	diag__fail(lash_errno, NULL);
    }

  if (!(data = auto_quotient(s -> states, a)))
    {
      auto_free(a);
      diag__fail(lash_errno, NULL);
    }

  if (auto_free(a))
    {
      auto_free(data);
      diag__fail(lash_errno, NULL);
    }
  
  if (!(q = qdd_auto2qdd(data, s -> nb_queues,
			 s -> queue_alphabet_size)))
    {
      auto_free(data);
      diag__fail(lash_errno, NULL);
    }

  diag__return(q);
}

/**  qdd_relation *qdd_relation_new_empty(cl, qm)  :  Creates a new,
                     empty transition relation for a machine whose
		     control states are described by cl integers.
		     The ith integer (i = 0, 1,...) is smaller than
		     qm[i]. qm is copied.

		     In the case of success, this function returns a
                     pointer to the newly created structure. In the
                     case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

qdd_relation *qdd_relation_new_empty(cl, qm)
     uint4  cl, *qm;
{
  qdd_relation *qr;

  diag__enter("qdd_relation_new_empty", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!cl || !qm)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!(qr = resr__new_object(qdd_relation)))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (!(qr -> nb_control = resr__new_objects(uint4, cl)))
    {
      resr__free_object(qr, qdd_relation);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(qr -> nb_control, qm, cl * sizeof(uint4));

  qr -> list = NULL;
  qr -> ctrl_len = cl;

  diag__return(qr);
}

/**  int qdd_relation_free(qr)  :  Frees the transition relation *qr.

		     Returns 0 if successful, -1 in the case of
		     an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupted data.     **/

int  qdd_relation_free(qr)
     qdd_relation *qr;
{
  qdd_relation_el *el;
  int              rv = 0;

  diag__enter("qdd_relation_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!qr || !(qr -> nb_control))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
  
  el = qr -> list;
  
  while (el)
    {
      qdd_relation_el *next = el -> next;

      if (qdd_relation_el_free(el))
	rv = 1;
      
      el = next;
    }

  resr__free_objects(qr -> nb_control, uint4, qr -> ctrl_len);
  resr__free_object(qr, qdd_relation);

  if (rv)
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  int  qdd_states_add_transition(t, np, no, nd, seq)  :  Adds a
                     transition to the transition relation *t.
		     This transition links the control states such
		     that the (np+1)-th component of their control
		     descriptor is equal to no to those for which
		     this component is equal to nd (the value of
		     the other components of the control descriptor
		     does not change).  The transition is labeled by
		     the sequence of operation *seq.

		     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt structure
			                       automaton.          **/

int  qdd_relation_add_transition(t, np, no, nd, seq)
     qdd_relation      *t;
     uint4              np, no, nd;
     queue_op_sequence *seq;
{
  qdd_relation_el *te;

  diag__enter("qdd_relation_add_transition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!t || !(t -> nb_control) || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */  

  if (!(seq = queue_op_sequence_copy(seq)))
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (!(te = qdd_relation_add(t, np, no, nd, seq)))
    diag__fail(lash_errno, -1);
 
  te -> type = QDD_RELATION_SEQ;

  diag__return(0);
}

/**  int  qdd_states_add_metatransition(t, np, no, nd, seq)  :  Adds
                     a meta-transition to the transition relation *t.
		     This transition links the control states such
		     that the (np+1)-th component of their control
		     descriptor is equal to no to those for which
		     this component is equal to nd (the value of
		     the other components of the control descriptor
		     does not change).  The transition is labeled by
		     the sequence of operation *seq.

		     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt structure
			                       automaton.          **/

int  qdd_relation_add_metatransition(t, np, no, nd, seq)
     qdd_relation      *t;
     uint4              np, no, nd;
     queue_op_sequence *seq;
{
  qdd_relation_el *te;

  diag__enter("qdd_relation_add_metatransition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!t || !(t -> nb_control) || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!(seq = queue_op_sequence_copy(seq)))
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (!(te = qdd_relation_add(t, np, no, nd, seq)))
    diag__fail(lash_errno, -1);
 
  te -> type = QDD_RELATION_STAR;

  diag__return(0);
}

/**  qdd_states *qdd_relation_succ(r, s)  :  Computes the set of
                     immediate successors of the states belonging to
		     the set *s by the transition relation *r.

		     In the case of success, this function returns a
                     pointer to a newly allocated set of states. In
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
			 LASH_ERR_BAD_VALUE  : Bad value.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch.      **/

qdd_states *qdd_relation_succ(r, s)
     qdd_relation *r;
     qdd_states   *s;
{
  qdd_states      *ns;
  qdd_relation_el *rel;

  diag__enter("qdd_relation_succ", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!r || !s || !(s -> states) || !(s -> nb_control) ||
      !(s -> queue_alphabet_size) || !(r -> nb_control))
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */  
   
  if (s -> ctrl_len != r -> ctrl_len ||
      memcmp(s -> nb_control, r -> nb_control,
	     s -> ctrl_len * sizeof(uint4)))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
  
  if (!(ns = qdd_states_copy(s)))
      diag__fail(lash_errno, NULL);

  for (rel = r -> list; rel; rel = rel -> next)
    {
      automaton *a;
      int      (*f)(qdd *, queue_op_sequence *);
      
      if (rel -> type == QDD_RELATION_SEQ)
	f = qdd_apply;
      else
	f = qdd_closure;

      auto_minimize(s->states);
      
      if (!(a = tran_succ(s, rel -> np, rel -> no, rel -> nd,
			  rel -> seq, f)))
	{
	  qdd_states_free(ns);
	  diag__fail(lash_errno, NULL);
	}
      
      if (auto_merge(ns -> states, a))
	{
	  auto_free(a);
	  qdd_states_free(ns);
	  diag__fail(lash_errno, NULL);
	}
      
      if (auto_free(a))
	{
	  qdd_states_free(ns);
	  diag__fail(lash_errno, NULL);
	}
    }
  
  diag__return(ns);
}

/**  qdd_states *qdd_relation_star_succ(r, s, callback)  :
                     Computes transitively the set of successors of
		     the states belonging to the set *s by the
		     transition relation represented by *r. If the
		     pointer callback is not NULL, then the function
		     *callback is called at each step of the fixpoint
		     computation with the current computed set of
		     reachable states (this set cannot be modified by
		     the callback function). If the callback function
		     returns0, then then computation is
		     resumed. Otherwise, it is immediately
		     interrupted. If the value of the parameter
		     callback is NULL, then no callback function is
		     used.

		     In the case of success, this function returns a
                     pointer to a newly allocated set of states. In
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
			 LASH_ERR_ALPHABET   : Alphabet mismatch.
			 LASH_ERR_BAD_VALUE  : Bad value.
                         LASH_ERR_INTERRUPT  : User interruption.  **/

qdd_states *qdd_relation_star_succ(r, s, callback)
     qdd_relation *r;
     qdd_states   *s;
     int         (*callback)(qdd_states *);
{
  qdd_states *sr, *sc;

  diag__enter("qdd_relation_star_succ", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!r || !s || !(s -> states) || !(s -> nb_control) ||
      !(s -> queue_alphabet_size) || !(r -> nb_control))
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */  

  if (s -> ctrl_len != r -> ctrl_len ||
      memcmp(s -> nb_control, r -> nb_control,
	     s -> ctrl_len * sizeof(uint4)))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
  
  if (!(sr = qdd_states_copy(s)))
     diag__fail(lash_errno, NULL);
    
  if (!(sc = qdd_states_copy(s)))
    {
      qdd_states_free(sr);
      diag__fail(lash_errno, NULL);
    }

  for (;;)
    {
      qdd_states *sr2;
      automaton  *qa;
      int         res;

      if (!(sr2 = qdd_relation_succ(r, sc)))
	{
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}

      if (!(qa = auto_difference(sr2 -> states, sr -> states)))
	{
	  qdd_states_free(sr2);
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}

      if (auto_minimize(qa))
	{
	  auto_free(qa);
	  qdd_states_free(sr2);
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}

      if (qdd_states_free(sr2))
	{
	  auto_free(qa);
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}

      if (auto_free(sc -> states))
	{
	  auto_free(qa);
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}

      sc -> states = qa;

      if ((res = auto_empty_language(qa)) == 1)
	break;
      else
	if (res == -1)
	  {
	    qdd_states_free(sr);
	    qdd_states_free(sc);
	    diag__fail(lash_errno, NULL);
	  }
	  
      if (auto_merge(sr -> states, sc -> states) ||
	  auto_minimize(sr -> states))
	{
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(lash_errno, NULL);
	}
      
      if (callback && callback(sr))
	{
	  qdd_states_free(sr);
	  qdd_states_free(sc);
	  diag__fail(LASH_ERR_INTERRUPT, NULL);
	}
    }

  if (qdd_states_free(sc))
    diag__fail(lash_errno, NULL);

  diag__return(sr);
}

/****  End of qdd-machines.c  ****/
