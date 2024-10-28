/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**     qdd.c  :  Visible data structures and prototypes           **/
/**                  for manipulating QDD.                         **/
/**                                                                **/
/**     04/22/98  :  Creation. (GC)                                **/
/**     09/22/99  :  qdd_free_init_tuples : bug fix. (GC)          **/
/**     12/16/99  :  Enhanced queue/queue alphabet support. (JMF)  **/
/**     01/17/00  :  qdd_copy : bug fix. (JMF)                     **/
/**     02/22/00  :  qdd_free_init_tuples : minor bug fix. (JMF)   **/
/**     02/28/00  :  qdd_new_empty : minor bug fix. (JMF)          **/
/**     03/08/00  :  qdd_compute_init_tuples : bug fix. (JMF)      **/
/**     03/09/00  :  New function : qdd_new_empty_queue. (JMF)     **/
/**     03/13/00  :  qdd_init_init_tuples : major bug fix. (JMF)   **/
/**     02/12/01  :  Minor correction. (BB)                        **/
/**     05/15/01  :  Minor corrections. (BB)                       **/
/**     07/08/02  :  Reorganization. (BB)                          **/
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
#include "resource.h"
#include "datastruct.h"
#include "diag.h"
#include "qdd.h"

/****  Prototypes of private functions.                          ****/

static int          qdd_transition_cmp(tran *, tran *);

static int          queue_symbol_cmp(queue_symbol *,
				      queue_symbol *);

static object_set **border2border_states(automaton *);
static int          queue_state_cmp(state_queue *,
				      state_queue *);
static void         qdd_sort_transitions(qdd *);
static void         b2bs_free_resources(automaton *, stack *,
					bit_table *, uint4_set *,
					object_set **, int);
static int          b2bs_search(automaton *, uint4, uint4 *,
				queue *, uint4_set *); 

static int          qdd_init_init_tuples(automaton *, object_set **,
					 uint4, stack *);

static int          add_a_greater_border_state(object_set **,
					       stack *, state_queue,
					       int *); 

static int          init_tuple_from_stack(automaton *, stack *,
					  object_set **, stack *);
static int          unepsilonify(automaton *, uint4);
static int          epsilon_it(qdd *, uint4);

/****  Private functions.                                        ****/

/**  int  qdd_transition_cmp(t1, t2)  :  Compares the two transitions
                     *t1 and *t2 of a qdd. Returns 0 if the labels of
		     the two transition are equal, a negative value if
		     the label of *t1 is lexicographically less than
                     the label of *t2, and a positive value otherwise.
                                                                   **/

static int qdd_transition_cmp(t1, t2)
  tran *t1, *t2;
{
  register int            test;
  register queue_symbol *p1, *p2;
  register uint4          i, min, l1, l2;

  l1 = t1 -> nb_symbols;
  l2 = t2 -> nb_symbols;
  p1 = (queue_symbol *)
    auto_transition_label_ptr(t1, sizeof(queue_symbol));
  p2 = (queue_symbol *)
    auto_transition_label_ptr(t2, sizeof(queue_symbol));
  
  min = (l1 < l2) ? l1:l2;
  
  for (i = 0; i < min; i++)
      if ((test = queue_symbol_cmp(p1 + i, p2 + i)))
	return test;

  return l1 - l2;
}

/**  queue_symbol_cmp(p1, p2)  :   Compares the two
                     queue_symbol *p1 and *p2. Returns a
		     negative value if (p1 -> queue) <
		     (p2 -> queue), a positive value if
		     (p1 -> queue) > (p2 -> queue) and the
		     comparison of (p1 -> symbol) and (p2 -> symbol)
		     if (p1 -> queue) == (p2 -> queue).            **/

static int queue_symbol_cmp(p1, p2)
    queue_symbol *p1, *p2;
{
  if ((queuesymbol_queue(p1)) < (queuesymbol_queue(p2)))
    return -1;
  
  if ((queuesymbol_queue(p1)) > (queuesymbol_queue(p2)))
    return  1;
  
  return (queuesymbol_symbol(p1)) - (queuesymbol_symbol(p2));
}

/**  void  qdd_sort_transitions(q)  :  Sorts the outgoing transitions
                    of each state of the qdd *q in
		    increasing order relatively to the queue part
		    of their labels and for a same queue,
		    relatively to the symbol part of their labels.

		    This function does not detect errors.          **/

static void  qdd_sort_transitions(q)
  qdd *q;
{
  register uint4      i, n;
  register automaton *a;

#if LASH_CHECK_LEVEL >= 1
  if (!q || ! q -> automaton)
    return;
#endif

  a = q -> automaton;
  n = a -> nb_states;

  for (i = 0; i < n; i++)
    if (a -> states[i].nb_trans)
      qsort(((void *) (a -> states[i].trans)), 
	    a -> states[i].nb_trans, sizeof(tran),
          (int (*)(const void *, const void *)) qdd_transition_cmp);

  return;
}

/**  object_set **border2border_states(a)  :  This is a subroutine of
                     qdd_compute_init_tuples.

		     Returns the value of r a pointer to an array
		     of n pointers to object_set, n being the number
		     of states in the automaton *a. For a state q, if
		     q is not an initial state or a reachable border
		     state then *(r[q]) is an empty set. Othewise, 
		     set *(r[q]) is the list of state_queue
		     (s,ch) such that s is another border state and
		     there exists, from q to s, a non empty path,
		     involving only queue ch, .

		     Every set *(r[q]) is sorted by increasing
		     number of the queue part of the elements (s,ch).

		     *a is supposed to be an automaton such that all
		     of its label is composed by a unique symbol of
		     type state_queue  .

		     The outgoing transitions of states of *a are
		     supposed to be increasingly sorted relatively to
		     the queue part of their symbol.               **/

static object_set **border2border_states(a)
     automaton *a;
{
  stack         *sk;
  bit_table     *bt;
  object_set   **result;
  uint4_set     *set;
  uint4          n, i, j, m, q;
  state_queue    sc;

  n = auto_nb_states(a);

  sk = stack__new_empty(uint4);
  bt =  bit__new_empty(n);
  set = set__new_empty();  
  result = resr__new_objects(object_set *, n);
  
  if (!(sk && bt && set && result))
    {
      if (sk)  stack__free(sk);
      if (bt)  bit__free(bt);
      if (set) set__free(set);
      if (result)
	resr__free_objects(result, object_set *, n);
      return NULL;
    }

  /* initializes result as an array of empty sets */
  for (i = 0; i < n; i++)
    {
      result[i] = object_set__new_empty(state_queue,
					(int (*) (const void *,
						  const void *))
					 queue_state_cmp);

      if (!result[i])
	{
	  for (i++; i < n; i++)
	    result[i] = NULL;
	  b2bs_free_resources(a, sk, bt, set, result, 1);
	  return NULL;
	}
    }
  
  /* initializes the stack */
  n = auto_nb_i_states(a);
  for (i = 0; i < n; i++)
    {
      auto_i_state(a, i, &q);
      if (!bit__member(bt, q))
	{
	  bit__add(bt, q);
	  if (stack__push(sk, &q))
	    {
	      b2bs_free_resources(a, sk, bt, set, result, 1);
	      return NULL;
	    }
	}
    }
  
  /* main loop */
  while (!stack__is_empty(sk))
    {
      stack__pop(sk, &q);
      auto_nb_out_transitions(a, q, &n);
      i = 0;
      while (i < n)
	{
	  if (b2bs_search(a, q, &i, &sc.queue, set))
	    {
	      b2bs_free_resources(a, sk, bt, set, result, 1);
	      return NULL;
	    }
	  m = set__nb_elements(set);
	  for (j = 0; j < m; j++)
	    {

	      sc.state = set__element(set, j);

	      if (object_set__add(result[q], &sc))
		{
		  b2bs_free_resources(a, sk, bt, set, result, 1);
		  return NULL;
		}
	      if (!bit__member(bt, sc.state))
		{
		  bit__add(bt, sc.state);
		  if (stack__push(sk, &sc.state))
		    {
		      b2bs_free_resources(a, sk, bt, set, result, 1);
		      return NULL;
		    }
		}
	    }
	}
    }

  n = auto_nb_states(a);
  for (i = 0; i < n; i++)
    object_set__sort(result[i]);
  
  b2bs_free_resources(a, sk, bt, set, result, 0);
  return result;
}

/**  queue_state_cmp(p1, p2)  :   Compares the two state_queue *p1
                     and *p2. Returns a negative value if
		     (p1 -> queue) < (p2 -> queue), a positive
		     value if (p1 -> queue) > (p2 -> queue) and
		     the comparison of (p1 -> state) and (p2 -> state)
		     if (p1 -> queue) == (p2 -> queue).            **/

static int queue_state_cmp(p1, p2)
    state_queue  *p1, *p2;
{
  if ((p1 -> queue) < (p2 -> queue))
    return -1;
  else 
    if ((p1 -> queue) > (p2 -> queue))
      return  1;
    else
      return (p1 -> state) - (p2 -> state);
}

/**  void b2bs_free_resources(a, sk, bt, set, result, free_r)  :
                     This is a subroutine of border2border_states.

		     For each p in {sk, bt, set}, frees the
		     ressources pointed by p if p is non null.

		     If free_r is non null, frees the ressources
		     pointed by result.                            **/
		     
static void b2bs_free_resources(a, sk, bt, set, result, free_r)
     automaton   *a;
     stack       *sk;
     bit_table   *bt;
     object_set **result;
     uint4_set   *set;
     int          free_r;
{
  stack__free(sk);
  bit__free(bt);
  set__free(set);
  
  if (free_r)
    { 
      uint4  n, i;
      
      n = auto_nb_states(a);

      for (i = 0; i < n; i++)
	if (!result[i])
	  object_set__free(result[i]);

      resr__free_objects(result, object_set *, n);
    }
}

/**  typedef stack_b2bs  :  Type of the data placed in the exploration
                     stack of the function b2bs_search.

		     Let sb be an object of type stack_b2bs. sb.state
		     gives the current state, sb.indice gives the
		     indice of the current transition of sb.state. **/
		     

typedef struct {
  uint4 state;
  uint4 indice;
} stack_b2bs;

#define  TREATED 1

/** int b2bs_search(a, q, pi, pch, set)  :  This is a subroutine of
                     border2border_states.

		     Computes and set in *pch the queue involved in
		     the transition *pi of state q of the automaton
		     *a. Then, computes *set, the set of all border
		     states s such that there exists a non empty path
		     from q to s involving queue *pch. At the end,
		     *pi is set such that (*pi - 1) is the last
		     transition of q involving queue *pch. 

		     Returns -1 in case of error and 0 othewise.

		     *a is supposed to be an automaton such that all
		     of its label is composed by a unique symbol of
		     type state_queue  .

		     The outgoing transitions of states of *a are
		     supposed to be increasingly sorted relatively to
		     the queue part of their symbol.               **/
    
static int b2bs_search(a, q, pi, pch, set)
     automaton *a;
     uint4     *pi, q;
     queue     *pch;
     uint4_set *set;
{
  uint4       i, n, dest, depthPath;
  stack      *sk;
  stack_b2bs  sb, *top;
  bit_table  *bt;
  queue       ch, cht = ZERO_INT1;
  tran       *t;
  int         currentTrans;
  
  set__free_content(set);

  sk = stack__new_empty(stack_b2bs);
  if (!sk)
    return -1;
  
  n = auto_nb_states(a);
  bt =  bit__new_empty(n);
  if (!bt)
    {
      stack__free(sk);
      return -1;
    }

  i = *pi;
  t = auto_transition(a, q, i);
  ch = queuesymbol_queue((queue_symbol *)
	auto_transition_label_ptr(t, sizeof(queue_symbol)));

  *pch = ch;
  
  /* initializes the stack */
  sb.state = q;
  sb.indice = 0;
  stack__push(sk, &sb);
  bit__add(bt, q);
  
  /* main loop */
  
  /* The first transition of the initial state is not yet treated */
  currentTrans = ! TREATED;

  /* the current path has a zero length */
  depthPath = 0;
  
  while (!stack__is_empty(sk))
    {
      top = (stack_b2bs *) stack__top(sk);

      if (currentTrans == TREATED)
	/* the current transition has been treated
	   so we prepare for the next one */
    	{
	  (top -> indice) ++;
	  currentTrans = ! TREATED;
	  continue;
	}

      q = top -> state;
      i = top -> indice;
      auto_nb_out_transitions(a, q, &n);
      if (i < n)
	/* the current transition is not yet treated,
	   so we treat it  */ 
	{	  

	  if (i == 0)
	    /* we look for the first transition of q
	       involving a queue greater or equals to ch */
	    {
	      for (; i < n; i++)
		{
		  t = auto_transition(a, q, i);
		  cht = queuesymbol_queue((queue_symbol *)
			 auto_transition_label_ptr(t,
			     sizeof(queue_symbol)));
		  if (ch <= cht)
		    break;
		}
	      top -> indice = i;
	    }
	  else
	    {
	      t = auto_transition(a, q, i);
	      cht = queuesymbol_queue((queue_symbol *)
		     auto_transition_label_ptr(t,
					       sizeof(queue_symbol)));
	    }
	  if (cht == ch)
	    {
	  
	      dest = auto_transition_dest(t);
	      if (bit__member(bt, dest))
		currentTrans = TREATED;
	      else
		{
		  bit__add(bt, dest);
		  sb.state = dest;
		  sb.indice = 0;
		  if (stack__push(sk, &sb))
		    {
		      stack__free(sk);
		      bit__free(bt);
		      return -1;
		    }
		  depthPath++;
		}
	      goto CONTINUE;
	    }
	}

      /* all transitions of top -> state have been treated
         or ch < cht  */

      if (((0 < n && i < n && ch < cht) || auto_accepting_state(a, q))
	  && depthPath)
	if (set__add(set, q))
	  {
	    stack__free(sk);
	    bit__free(bt);
	    return -1;
	  }
      
	  
      /* treatment of top -> state is finished */
      stack__pop(sk, NULL);
      depthPath--;
      currentTrans = TREATED;

    CONTINUE:
      ; /* end of the while loop */
    }
  *pi = i++; 
  stack__free(sk);
  bit__free(bt);

  /*  printf("b2bresult:\n");
  print_set(set);
  printf("b2b---:\n");*/

  return 0;
}

/**  typedef stack_init  :  Type of the data placed in the exploration
                     stack of the functions qdd_init_init_tuples,
		     init_tuple_from_stack and
		     add_a_greater_border_state.

		     Let si be an object of type stack_init. si.state
		     gives the current state, si.indice gives the
		     indice in b2bs[si.state] of the current
		     state_queue (s, ch). The couple (s, ch)
		     represents the fact that s is a border state
		     reachable from si.state by a non null path
		     involving only queue ch. si.found is non null
		     iff during the process, we meet a final state
		     reachable from si.state through a non null path
		     involving only one queue. 

		     At any moment, the content of the stack
		     represents a path, in the underlying automaton,
		     from border states to border states involving
		     queues in increasing order.
		     When the last border state is a final state,
		     then from the content of the stack, we deduce a
		     init_tuple.                                   **/

typedef struct {
  uint4 state;
  uint4 indice;
  int found;
} stack_init;

/**  int qdd_init_init_tuples(a, b2bs, q, stinit)  :  This is a
                     subroutine of qdd_compute_init_tuples.

		     *a is the underlying automaton. 

		     b2bs is a pointer to an array of n pointers to
		     object_sets, n being the number of states of
		     *a. For a state q, if q is not an initial state
		     or a reachable border state then *(r[q]) is an
		     empty set. Othewise, *(b2bs[q]) is the list of
		     state_queue (s,ch) such that s is a border
		     state and there exists from q to s a non empty
		     path involving only queue ch. Every set
		     *(b2bs[q]) is supposed to be sorted by increasing
		     number of the queue part of its elements. 

		     q is a state of *a.
		     
		     *stinit  is a stack whose elements are of type
		     init_tuple.

		     Add in *stinit all init_tuples which starts
		     from q

		     Returns -1 in case of trouble, 0 otherwise.   **/

#define  TREATED 1
/* main loop */
static int  qdd_init_init_tuples(a, b2bs, q, stinit)
     automaton   *a;
     object_set **b2bs;
     uint4        q;
     stack       *stinit;
{
  stack        *sk;
  int           currentTrans, ok;
  stack_init   *top, si;
  state_queue   sc;

  sk = stack__new_empty(stack_init);
  if (!sk)
    return -1;

  si.state = q;
  si.indice = 0;
  si.found = 0;

  if (stack__push(sk, &si))
    return -1;

  /* first transition of the initial state is not yet treated */
  currentTrans = ! TREATED;

  while (!stack__is_empty(sk))
    {
      top = (stack_init *) stack__top(sk);

      if (currentTrans == TREATED)
	/* the current transition has been treated
	   so we prepare for the next one */
    	{
	  (top -> indice)++;
	  currentTrans = ! TREATED;
	  continue;
	}

      if (top -> indice <
	  object_set__nb_elements(b2bs[top -> state]))
	/* the current transition is not yet treated,
	   so we treat it  */ 
	{	  
	  sc = *(state_queue *)
	    object_set__element(b2bs[top -> state], top -> indice);

	  if (auto_accepting_state(a, sc.state) && top -> found != 1)
	    {
	      top -> found = 1;
	      if (top -> found && init_tuple_from_stack(a, sk, b2bs,
							stinit))
		{
		  stack__free(sk);
		  return -1;
		} 
	    }
	  
	  if (add_a_greater_border_state(b2bs, sk, sc, &ok))
	    {
	      stack__free(sk);
	      return -1;
	    }

	  if (ok)
	    continue;
	}
      
      /* treatment of top -> state is finished */
      stack__pop(sk, NULL);
      currentTrans = TREATED;
    }

  stack__free(sk);
  return 0;
}

/**  int add_a_greater_border_state(b2bs, sk, sc, pok)  :  This is a
                     subroutine of qdd_init_init_tuples.

		     b2bs is a pointer to an array of n pointers to
		     object_sets, n being the number of states of
		     *a. For a state q, if q is not an initial state
		     or a reachable border state then *(r[q]) is an
		     empty set. Othewise, *(b2bs[q]) is the list of
		     state_queue (s,ch) such that s is a border
		     state and there exists from q to s a non empty
		     path involving only queue ch. Every set
		     *(b2bs[q]) is supposed to be sorted by increasing
		     number of the queue part of its elements.

		     *sk is a stack whose elements are of type
		     stack_init.

		     sc is the state_queue (s, ch) corresponding to
		     the top of sk. 

		     pok is a pointer to an int variable.

		     If there exists a border state q reachable from s
		     through a path involving a queue ch2 greater
		     than ch, then this subroutine pushes the
		     appropriate stack_init element corresponding to
		     (s, (q, ch2)) into *sk and sets *pok to 1.
		     Otherwise, just sets *pok to 0.

		     Returns -1 in case of lack of memory, and 0
		     otherwise.                                    **/
     
static int add_a_greater_border_state(b2bs, sk, sc, pok)
     object_set  **b2bs;
     stack        *sk;
     state_queue sc;
     int          *pok;
{
  uint4       i, n;
  stack_init  si;
  object_set *set;
    
  set = b2bs[sc.state];
  n = object_set__nb_elements(set);
  for (i = 0; i < n; i++)
    {
      if (sc.queue <
	  ((state_queue *) object_set__element(set, i)) -> queue)
	{
	  si.state = sc.state;
	  si.indice = i;
	  si.found = 0;
	  if (stack__push(sk, &si))
	    return -1;
	  *pok = 1;

	  return 0;
	}
    }

  *pok = 0;
  return 0;
}

/**  int init_tuple_from_stack(a, sk, b2bs, stinit)  :  This is a
                     subroutine of qdd_init_init_tuples.

		     *a is the underlying automaton. 

		     *sk is a stack whose elements are of type
		     stack_init.

		     b2bs is a pointer to an array of n pointers to
		     object_sets, n being the number of states of
		     *a. For a state q, if q is not an initial state
		     or a reachable border state then *(r[q]) is an
		     empty set. Otherwise, *(b2bs[q]) is the list of
		     state_queue (s, ch) such that s is a border
		     state and there exists from q to s a non empty
		     path involving only queue ch. Every set
		     *(b2bs[q]) is supposed to be sorted by increasing
		     number of the queue part of its elements. 

		     *stinit  is a stack whose elements are of type
		     init_tuple.

		     Add in *stinit all init_tuples which corresponds
		     to the current content of *sk.

		     Returns -1 in case of lack of memory, and 0
		     othewise.

		     Note that *sk is supposed to be non empty
		     here. This is hopefully ensured by
		     qdd_init_init_tuples.                         **/

static int init_tuple_from_stack(a, sk, b2bs, stinit)
     automaton   *a;
     stack       *sk;
     object_set **b2bs;
     stack       *stinit;
{
  uint4          i, n, m;
  state_queue   *p1, *p2, sc;
  queue          ch = ZERO_INT1;
  stack_init     si;
  init_tuple     it;
  object_set    *set;
  
  n = stack__nb_elements(sk);
  p1 = resr__new_objects(state_queue, n);
  if (!p1)
    return -1;

  /* set in the array p1 the sequence of state_queues
     corresponding to the content of *sk. The last
     queue is not meaningful for the moment.         */
  for (i = 0; i < n; i++)
    {
      si = *(stack_init *) stack__element(sk, i);
      sc.state = si.state;

      sc.queue =
	((state_queue *)
	 object_set__element(b2bs[si.state], si.indice)) -> queue;
      p1[i] = sc;
    }

  /* searches all queues that can ends the sequence represented
     by p1 and computes the corresponding init_tuples.          */

  set = b2bs[sc.state];
  m = object_set__nb_elements(set);

  if (n == 1)
    for (i = 0; i < m; i++)
      {
	sc = *(state_queue *) object_set__element(set, i);

	if (auto_accepting_state(a, sc.state))
	  {
	    ch = sc.queue;
	    p2 = resr__new_object(state_queue);

	    if (!p2)
	      {
		resr__free_objects(p1, state_queue, 1);
		return -1;
	      }

	    p2 -> state = p1 -> state;
	    p2 -> queue = ch;
	    it.nb_sc = 1;
	    it.list = p2;

	    if (stack__push(stinit, &it))
	      {
		resr__free_objects(p1, state_queue, 1);
		resr__free_object(p2, state_queue);
		return -1;
	      }

	    i++;
	    break;
	  }
      }
  else
    {
      i = 0;
      ch = p1[n-2].queue;
    }
 
  for (; i < m; i++)
    {
      sc = *(state_queue *) object_set__element(set, i);

      if (ch < sc.queue)
	{
	  ch = sc.queue;

	  if (auto_accepting_state(a, sc.state))
	    {
	      p2 = resr__new_objects(state_queue, n);

	      if (!p2)
		{
		  resr__free_objects(p1, state_queue, n);
		  return -1;
		}

	      memcpy(p2, p1, n * sizeof(state_queue));
	      p2[n-1].queue = ch;
	      it.nb_sc = n;
	      it.list = p2;

	      if (stack__push(stinit, &it))
		{
		  resr__free_objects(p1, state_queue, n);
		  resr__free_objects(p2, state_queue, n);
		  return -1;
		}
	    }
	}
    }

  resr__free_objects(p1, state_queue, n);
  return 0;
}

/**  int  unepsilonify(a, s)  :  Modify the automaton *a such that
                     the initial and final state s is not final
		     anymore.
		     The language accepted by *a remains the same,
		     accepted that the resulting automaton may not
		     accept the empty word anymore.

		     Returns 0, or -1 in the case of an error.
		     
		     Possible error codes:
		         
                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_TRAN   : No such transition.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  unepsilonify(a, s)
     automaton *a;
     uint4      s;
{
  uint4           n, nb_trans, cur_tran_nb;
  register uint4  cur_state_nb, nb_states;

  if (auto_add_new_state(a, &n))
    return -1;

  nb_states = auto_nb_states(a);

  for (cur_state_nb = ZERO_INT4; cur_state_nb < nb_states;
       cur_state_nb++)
    {
      uint4  cur_state;
      
      if (auto_state(a, cur_state_nb, &cur_state) ||
          auto_nb_out_transitions(a, cur_state, &nb_trans))
        return -1;
      
      for (cur_tran_nb = ZERO_INT4; cur_tran_nb < nb_trans;
           cur_tran_nb++)
        {
          tran *cur_tran = auto_transition(a, cur_state, cur_tran_nb);

          if (!cur_tran)
            return -1;

          if (auto_transition_dest(cur_tran) == s)
            auto_redirect_transition(cur_tran, n);
        }
    }

  if (auto_nb_out_transitions(a, s, &nb_trans))
    return -1;

  for (cur_tran_nb = ZERO_INT4; cur_tran_nb < nb_trans;
       cur_tran_nb++)
    {
      tran  *cur_tran = auto_transition(a, s, cur_tran_nb);
      uint1 *cur_tran_label =
	auto_transition_label_ptr(cur_tran, 
				  auto_alphabet_nbytes(a));
      
      if (auto_add_new_transition(a, n, 
				  auto_transition_dest(cur_tran),
				  auto_transition_length(cur_tran),
				  cur_tran_label))
	return -1;
    }
    
  auto_unmark_accepting_state(a, s);
  auto_mark_accepting_state(a, n);
  
  return 0;
}

/** int  epsilon_it(q, eps_state)  :  Marks the state eps_state of
                     the automaton *q as final and adds a new
		     init_tuple which shows that *q accepts the
		     empty word.

		     Return 0 if successful, of -1 if there is not
		     enough available memory.                      **/

static int  epsilon_it(q, eps_state)
     qdd   *q;
     uint4  eps_state;
{
  init_tuple *it;

  auto_mark_accepting_state(q -> automaton, eps_state);
      
  if (!(it = resr__resize_objects(q -> init, init_tuple,
				  q -> nb_it + 1,
				  q -> nb_it)))
    return -1;
      
  q -> init = it;
  q -> nb_it++;
 
  it = q -> init + (q -> nb_it - 1);
      
  if (!(it -> list = resr__new_object(state_queue)))
    return -1;
  
  it -> list -> state = eps_state;
  it -> list -> queue = QUEUE_UNK_QUEUE;
  it -> nb_sc = 1;

  return 0;
}

/****  Public visible functions.                                 ****/

/**  qdd *qdd_new_empty(n, alphasizes)  :  Creates a new, empty QDD
                     (there is no queue content - i.e. the underlying
		     automaton has no accepting state).

                     n is the number of queues of the new QDD,
		     alphasize is an array such that alphasize[i] is
		     the number of symbols in the i-th queue's
		     alphabet (i = 0,..., n-1).  alphasize is copied.

                     If a QDD is created, the function
                     returns a pointer to this QDD. Otherwise,
                     it returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt data.
                         LASH_ERR_NO_MEM   : Not enough memory.    **/

qdd *qdd_new_empty(n, alphasizes)
     uint1  n, *alphasizes;
{
  register qdd *qd;

  diag__enter("qdd_new_empty", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!n || !alphasizes)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  qd = resr__new_object(qdd);

  if (!qd)
    diag__fail(LASH_ERR_NO_MEM, NULL);
    
  qd -> properties = QDD_PROP_NOTHING;
  
  qdd_set_property(qd, QDD_PROP_ONE_QUEUE);
  
  qd -> nb_it = ZERO_INT4;
  qd -> init = NULL;
  
  if (!(qd -> automaton = auto_new_empty(sizeof(queue_symbol))))
    {
      resr__free_object(qd, qdd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  qd -> nb_queues = n;
  if (!(qd -> queue_alphabet_size = resr__new_objects(uint1, n)))
    {
      auto_free(qd -> automaton);
      resr__free_object(qd, qdd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }  
  
  memcpy(qd -> queue_alphabet_size, alphasizes, n * sizeof(uint1));

  diag__return(qd);
}

/**  qdd *qdd_new_empty_queue(n, alphasizes)  :  Creates a new,
                     empty QDD (i.e. the content of every queue
                     is empty).

                     n is the number of queues of the new QDD,
		     alphasize is an array such that alphasize[i] is
		     the number of symbols in the i-th queue's
		     alphabet (i = 0,..., n-1).  alphasize is copied.

                     If a QDD is created, the function
                     returns a pointer to this QDD. Otherwise,
                     it returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt data.
                         LASH_ERR_NO_MEM   : Not enough memory.    **/

qdd *qdd_new_empty_queue(n, alphasizes)
     uint1  n, *alphasizes;
{
  register qdd *q;
  uint4         ns;

  diag__enter("qdd_new_empty_queue", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!n || !alphasizes)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(q = qdd_new_empty(n, alphasizes)) ||
      qdd_free_init_tuples(q) ||
      auto_add_new_state(q -> automaton, &ns) ||
      auto_add_new_i_state(q -> automaton, ns))
    diag__fail(lash_errno, NULL);
  
  auto_mark_accepting_state(q -> automaton, ns);

  diag__return(q);
}

/**  int qdd_compute_init_tuples(q)  :  Actualizes the set of
                     init_tuples of the underlying automaton of *q.

		     The QDD *q is first normalized.
		     
                     Returns a non nul value in case of error.
		     Returns 0 if everything is all right.

		     NOTE : THE UNDERLYING AUTOMATON OF Q IS SUPPOSED
		     FREE OF EPSILON TRANSITIONS
		     (this restriction is intended to simplify the
		     sub fonction b2bs_search).                    **/
   
int qdd_compute_init_tuples(q)
     qdd *q;
{
  stack       *stinit;
  automaton   *a;
  init_tuple  *init;
  object_set **b2bs;
  uint4        i, n, s, eps_state = ZERO_INT4;
  int          isempty, epsilon = 0;

  diag__enter("qdd_compute_init_tuples", -1);
  
  if (qdd_free_init_tuples(q) || auto_normalize(q -> automaton))
    diag__fail(lash_errno, -1);

  { /* Handles the special case of initial and final states. */
    uint4      cur_istate_nb, nb_istates;
    uint4_set *iset;

    nb_istates = auto_nb_i_states(q -> automaton);
    
    if (!(iset = set__new_empty()))
      diag__fail(LASH_ERR_NO_MEM, -1);

    for (cur_istate_nb = ZERO_INT4; cur_istate_nb < nb_istates;
	 cur_istate_nb++)
      {
	uint4  cur_istate;

	if (auto_i_state(q -> automaton, cur_istate_nb, &cur_istate))
	  {
	    set__free(iset);
	    diag__fail(lash_errno, -1);
	  }

	if (set__add(iset, cur_istate))
	    {
	      set__free(iset);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
      }

     for (cur_istate_nb = ZERO_INT4; cur_istate_nb < nb_istates;
	  cur_istate_nb++)
      {
	uint4  cur_istate = set__element(iset, cur_istate_nb);
	
	if (auto_accepting_state(q -> automaton, cur_istate))
	  {
	    epsilon = 1;
	    
	    if (unepsilonify(q -> automaton,
			     eps_state = cur_istate))
	      {
		set__free(iset);
		diag__fail(lash_errno, -1);
	      } 
	  }
      }

    set__free(iset);
  }

  if ((isempty = qdd_empty(q))  == -1)
    diag__fail(lash_errno, -1);
      
  if (isempty)
    {
      if (epsilon &&
	  (!(q -> init = resr__new_objects(init_tuple, 0)) ||
	   epsilon_it(q, eps_state)))
	diag__fail(LASH_ERR_NO_MEM, -1);
      
      qdd_set_property(q, QDD_PROP_VALID_INIT_TUPLES);
      
      diag__return(0);	
    }
  
  qdd_sort_transitions(q);
  
  stinit = stack__new_empty(init_tuple);
  if (!stinit)
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  a = qdd_automaton(q);
  b2bs = border2border_states(a);
  
  if (!b2bs)
    {
      stack__free(stinit);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  /* Computes and adds in *stinit all init_tuples started from
     initial states.                                                */

  n = auto_nb_i_states(a);

  for (i = ZERO_INT4; i < n; i++)
    {
      auto_i_state(a, i, &s);

      if (qdd_init_init_tuples(a, b2bs, s, stinit))
	{
	  /* Not enough memory. Free allocated ressources then exit */
	  n = stack__nb_elements(stinit);

	  for (i = 0; i < n; i++)
	    {
	      init = (init_tuple *) stack__element(stinit, i);
	      resr__free_objects(init -> list, state_queue,
				 init -> nb_sc);
	    }

	  stack__free(stinit);
	  
	  n = auto_nb_states(a);
	  for (i = 0; i < n; i++)
	    object_set__free(b2bs[i]);

	  resr__free_objects(b2bs, object_set *, n);
	  
  	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
    }

  /* Copies content of the stack to an array pointed by q -> init   */
     
  n = stack__nb_elements(stinit);
  init = resr__new_objects(init_tuple, n);

  if (!init)
    {
      for (i = 0; i < n; i++)
	{
	  init = (init_tuple *) stack__element(stinit, i);
	  resr__free_objects(init -> list, state_queue,
			     init -> nb_sc);
	}

      stack__free(stinit);
	  
      n = auto_nb_states(a);
      for (i = 0; i < n; i++)
	object_set__free(b2bs[i]);

      resr__free_objects(b2bs, object_set *, n + (uint4)epsilon);
     
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  for (i = 0; i < n; i++)
    init[i] = *(init_tuple *) stack__element(stinit, i);
      
  { /* frees useless ressources */
    uint4  nb;
    
    stack__free(stinit);
    nb = auto_nb_states(a);
    for (i = 0; i < nb; i++)
      object_set__free(b2bs[i]);
    resr__free_objects(b2bs, object_set *, nb);
  }

  q -> init = init;
  q -> nb_it = n;
  
  if (epsilon && epsilon_it(q, eps_state))
    diag__fail(LASH_ERR_NO_MEM, -1);
 
  qdd_set_property(q, QDD_PROP_VALID_INIT_TUPLES);

  diag__return(0);
}

/**  int  qdd_free_init_tuples(q)  :  Frees the init tuples of the
                     qdd *q and reset the property
		     QDD_PROP_VALID_INIT_TUPLES.

		     Returns 0 in case of success.
		     Returns -1 in case of error and set lash_errno.
		      
		      Possible error codes:

                         LASH_ERR_NOT_INIT  : Not initialized.
			 LASH_ERR_CORRUPT   : Corrupt automaton.   **/

int qdd_free_init_tuples(q)
     qdd  *q;
{
  uint4       i, n;
  init_tuple *p;
  
  diag__enter("qdd_free_init_tuples", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!q -> init && !q -> nb_it)
    diag__return(0);
    if (!q -> init || !q -> nb_it)
      diag__fail(LASH_ERR_CORRUPT, -1);

  n = q -> nb_it;
  p = q -> init;
  for (i = 0; i < n; i++, p++)
      resr__free_objects(p -> list, state_queue, p -> nb_sc);
  
  resr__free_objects(q -> init, init_tuple, q -> nb_it);

  q -> init = NULL;
  q -> nb_it = 0;
  qdd_reset_property(q, QDD_PROP_VALID_INIT_TUPLES);

  diag__return(0);
}
  
/**  int  qdd_free(q)  :  Unallocates the qdd given by *q.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  qdd_free(q)
  qdd *q;
{
  diag__enter("qdd_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !(q -> automaton))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (qdd_free_init_tuples(q))
    diag__fail(lash_errno, -1);
  
  if (auto_free(q -> automaton))
    diag__fail(lash_errno, -1);

  resr__free_objects(q -> queue_alphabet_size, uint1, q -> nb_queues);
  resr__free_object(q, qdd);

  diag__return(0);
}

/**  qdd *qdd_copy(q)  :  Returns a copy of the qdd *q, or a
                     NULL pointer in the case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

qdd *qdd_copy(q)
  qdd *q;
{
  qdd *r;

  diag__enter("qdd_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!q)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  r = resr__new_object(qdd);
  if (!r)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  r -> properties = q -> properties;
  r -> nb_queues = q -> nb_queues;

  if (!(r -> queue_alphabet_size =
	resr__new_objects(uint1, r -> nb_queues)))
    {
      resr__free_object(r, qdd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  memcpy(r -> queue_alphabet_size, q -> queue_alphabet_size,
	 r -> nb_queues * sizeof(uint1));

  if (!(r -> automaton = auto_copy(q -> automaton)))
    {
      resr__free_objects(r -> queue_alphabet_size, uint1,
			 r -> nb_queues);
      resr__free_object(r, qdd);
      diag__fail(lash_errno, NULL);
    }

  r -> init = NULL;
  r -> nb_it = ZERO_INT4;
  
  diag__return(r);
}

/**  qdd *auto2qdd(a)  :  Converts a given automaton *a into a QDD.

                     Returns the QDD if sucessful, or -1 in the case
		     of an error.

		     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

qdd *qdd_auto2qdd(a, nq, qas)
     automaton *a;
     uint1      nq, *qas;
{
  qdd *q;
  
  diag__enter("qdd_auto2qdd", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !nq || !qas)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(q = qdd_new_empty(nq, qas)))
    diag__fail(lash_errno, NULL);

  if (auto_free(q -> automaton))
    {
      qdd_free(q);
      diag__fail(lash_errno, NULL);
    }

  q -> automaton = a;

  qdd_reset_property(q, qdd_known_properties(q));

  return q;
}

/**  int  qdd_same_alphabet(q1, q2)  :  Returns 1 if QDDs *q1 and *q2
                     have the same alphabet, 0 if not.

		     Returns -1 and sets lash_errno in the case of an
		     error.
		     
		     Possible error codes:

                         LASH_ERR_NOT_INIT  : Not initialized.
			 LASH_ERR_CORRUPT   : Corrupt QDD/automaton.
                                                                   **/

int  qdd_same_alphabet(q1, q2)
     qdd *q1, *q2;
{
  uint1  i;

  diag__enter("qdd_same_alphabet", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q1 || !q2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
    
  if (qdd_nb_queues(q1) != qdd_nb_queues(q1))
    diag__return(0);
 
  for (i = ZERO_INT1; i < q1 -> nb_queues; i++)
    if (qdd_queue_alphabet_size(q1, i) !=
	qdd_queue_alphabet_size(q2, i))
      diag__return(0);

  diag__return(1);
}

/****  End of qdd.c  ****/
