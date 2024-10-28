/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    graph-scc.c  :  Functions related to the search of          **/
/**                    strongly connected components in a graph.   **/
/**                                                                **/
/**        02/27/01  :  Creation. (SJ)                             **/
/**        03/04/01  :  Smoother parameterization. (SJ)            **/
/**        03/16/01  :  Got rid of recursion. (SJ)                 **/
/**        03/17/01  :  Reorganization. (SJ)                       **/
/**        05/03/01  :  Reorganization. (SJ)                       **/
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
#include "graph-scc.h"
#include "resource.h"
#include "diag.h"

/****  Prototypes of private functions.                          ****/

static int    tarjan(automaton *, uint4, uint4 *, uint4 *, uint1,
		  int (*) (automaton *, stack *, uint4, uint4 *, 
			   uint1, uint1, void *),
		  void *, stack *, stack *, uint4 *, uint4 *, 
                  bit_table *);

/****  Private functions.                                        ****/

/**  typedef tarjan_info  :  Type of the data placed on the
                     exploration stack of Tarjan's algorithm. The
		     first field gives the current state of the
		     automaton. The second field corresponds to the
		     number of transitions that have already been
		     explored in this current state. The third field
		     is a flag that is set if and only if the current
		     state is linked to itself through a transition 
		     among those that have already been explored.  **/

typedef struct {
  uint4  v;
  uint4  k;
  uint1  linked_to_itself;
} tarjan_info;

/**  int  tarjan(a, v, scc_num, scc_count, param, op, arg, active, st,
		     rank, lowlink, placed) : This function is the
		     core of a variant of Tarjan's algorithm, namely
		     the one proposed in [Nuu95]. It discovers all the
		     s.c.c.'s of the automaton *a reachable from the
		     state of index v. It attributes a same number to
		     all the states of an s.c.c., and stores the
		     number of the s.c.c. of the state i in 
                     scc_num[i], where *scc_num is an array that must
                     be wide enough to assign a number to each state 
                     of *a. scc_num can be NULL, in which case the
		     user does not care about the numbering of the
		     s.c.c. The argument *scc_count is a pointer to
		     a counter that enumerates the s.c.c. At the end
		     of this function, *scc_count is equal to its
		     initial value plus the number of s.c.c.'s 
                     reachable from v. The argument param 
                     parameterizes the algorithm (see the file
                     "graph-scc.h"). The value SCC_ONLY_REACHABLE 
                     is ignored here. *op is an optional function 
                     that is called whenever a s.c.c. has been
                     discovered and numbered (see the function
                     "scc_algorithm" for more informations). *arg is
                     an optional pointer of any type that is given as
                     a parameter to *op whenever it is called. 
                     *active is the exploration stack that contains a
                     request for each state still to be explored :
                     its elements are of type tarjan_info. *st is a
                     stack on which the state indices are pushed
                     whenever the corresponding states are met for
                     the first time (see [Nuu95]) : its elements are
                     of type uint4. *rank is an array that enumerates
                     each state in the order according to which they
                     are met in the depth first search. *lowlink is an
		     array such that lowlink[i] is a candidate state
		     for the root of the component containing the
		     state i. *placed is a bit table that is relevant
		     if and only if the *scc_num array is NULL : in
		     that case, it indicates the states that have
		     already been reached.

		     Tarjan's algorithm might end with the following
		     return values : (i) -1 if the optional function
		     *op returned a negative value, (ii) -2 in case of
		     a memory lack, (iii) -3 if *a is corrupted,
		     (iv) zero if Tarjan's algorithm has visited all
		     the s.c.c.'s reachable from v in *a, or (v) a
		     positive value that has been given by *op, in
		     which case the exploration is aborted.        **/

static int tarjan(a, v, scc_num, scc_count, param, op, arg, active, 
                  st, rank, lowlink, placed)
     automaton   *a;
     uint4        v;
     uint4       *scc_num, *scc_count;
     uint1        param;
     int        (*op)(automaton *, stack *, uint4, uint4 *, 
		      uint1, uint1, void *);
     void        *arg;
     stack       *active, *st;
     uint4       *rank, *lowlink;
     bit_table   *placed;
{
  register tarjan_info  *top;
  register tran         *t;
  register uint4         u, tmp, rank_cnt;
  register uint1         linked_to_itself, accept;
  register int           res;
           tarjan_info   tar;
           uint4         n;

  rank_cnt = 0;
  
  /* Pushes the first state on the exploration stack. */

  tar.v = v;
  tar.k = 0;
  tar.linked_to_itself = 0;

  if (stack__push(active, (void *) &tar) < 0)
    return -2;

  while (!stack__is_empty(active))
    {
      /* Reads the active vertex v. */
      top = (tarjan_info *) stack__top(active);
      v = top -> v;

      if (top -> k == 0)
	{
	  /* This is a vertex visited for the first time. */
	  rank[v] = ++ rank_cnt;
	  lowlink[v] = v;
	  if (stack__push(st, (void *) &v) < 0)
	    return -2;
	}
      
      if (auto_nb_out_transitions(a, v, &n) < 0)
	return -3;
      
      /* Loop to reach transitively all the successors of v. */
      if (top -> k < n)
	{
	  t = auto_transition(a, v, (top -> k) ++);
	  if (!t)
	    return -3;

	  u = auto_transition_dest(t);
	  if (u == v && auto_transition_length(t) > 0)
	    top -> linked_to_itself = 1;

	  /* u is a successor of v. */
	  if (!rank[u])
	    {
	      /* We have not seen u already. */
	      tar.v = u;
	      tar.k = 0;
	      tar.linked_to_itself = 0;
	      if (stack__push(active, (void *) &tar) < 0)
		return -2;
	    }
	  else
	    {
	      /* Updates if needed the lowlink information. */
	      if ( ( (scc_num  && scc_num[u] == SCC_UNREACHABLE) ||
		     (!scc_num && !bit__member(placed, u)) ) &&
		   rank[lowlink[v]] >= rank[lowlink[u]] )
		lowlink[v] = lowlink[u];
	    }
	}

      else
	{
	  /* v is mature : all of its successors have been visited. */
	  if (lowlink[v] == v)
	    {
	      /* v is the root of a new s.c.c. whose states are
		 on the top of the stack *st. */
	      linked_to_itself = top -> linked_to_itself;

	      tmp = 0;
	      accept = 0;
	      do
		{
		  if (op == NULL || (param & SCC_AUTOMATIC_POP))
		    {
		      u = *((uint4 *) stack__top(st));
		      stack__pop(st, NULL);
		    }
		  else
		    u = *((uint4 *) stack__pick(st, tmp));
		  
		  if (scc_num)
		    scc_num[u] = *scc_count;
		  else
		    bit__add(placed, u);

		  accept = accept || auto_accepting_state(a, u);
		  
		  tmp ++;
		}
	      while (u != v);
	      
	      if (scc_num && (param & SCC_MARK_TRANSIENT) &&
		  tmp == 1 && !linked_to_itself )
		scc_num[v] = SCC_TRANSIENT_NODE;
	      else
		(*scc_count) ++;

	      if (op != NULL)
		{
		  res = op(a, st, v, scc_num, accept,
			   (tmp == 1 && !linked_to_itself), arg);
		  if (res < 0) return -1;
		  if (res)     return res;
		}
	    }

	  /* Updates the lowlink information for 
	     the parent of v in the depth first search. */
	  stack__pop(active, NULL);
	  if (!stack__is_empty(active))
	    {
	      u = ((tarjan_info *) stack__top(active)) -> v;
	      if ( ( (scc_num  && scc_num[v] == SCC_UNREACHABLE) ||
		     (!scc_num && !bit__member(placed, v)) ) &&
		   rank[lowlink[u]] >= rank[lowlink[v]] )
		lowlink[u] = lowlink[v];
	    }
	}
    }

  return 0;  
}


/****  Public visible functions.                                 ****/

/** int scc_algorithm(a, scc_num, scc_count, param, op, arg) :
                     Function that encapsulates a call to Tarjan's
                     algorithm. The automaton whose s.c.c.'s are
                     wanted is *a. *scc_num is an array that is filled
                     during the exploration. At the end of the call,
                     it gives for each state of *a, the number of the
                     s.c.c. to which it belongs (it might be
                     SCC_UNREACHABLE or SCC_TRANSIENT_NODE if the
                     corresponding options have been set). *scc_count
                     is a pointer to a variable that will contain the
                     number of s.c.c.'s of *a at the end of the
                     execution. param parameterizes the algorithm
                     using the or-able values of "graph-scc.h".

		     *op is an optional function that is called
		     whenever a s.c.c. has been discovered and
		     numbered. It allows a post-treatment on each
		     s.c.c. It receives the following arguments :
		     - the automaton being explored ;
		     - a pointer to an active stack on which the
		     state indices are stored when they are met for
		     the first time (the type of the elements of
		     this stack is uint4) ;
		     - the state index of the root of the s.c.c. ;
		     - the array containing the s.c.c. numbers for all
		     the states belonging to the s.c.c.'s that have
		     already been discovered ;
		     - a flag set iff the discovered s.c.c. contains
		     at least one accepting state ;
		     - a flag set iff the discovered s.c.c. contains
		     just a transient state (i.e. a s.c.c. reduced to
		     one state that is not linked to itself), implying
		     that there is no cycle in that s.c.c. ;
		     - an optional argument that can be of any type,
		     which will be equal to the pointer *arg.

		     If the parameter SCC_AUTOMATIC_POP is not set,
		     the optional function need not know the states
		     composing the discovered s.c.c. In this case,
		     these ones are placed on the top of the stack,
		     upon the state index of root, when the function
		     *op is called. At the end of its execution, the
		     optional function is responsible to pop the stack
		     down to the state index of root (inclusively) in
		     order to ensure the correct work of Tarjan's
		     algorithm. If no optional function is given or if
		     SCC_AUTOMATIC_POP is set, Tarjan's algorithm
		     automatically removes the states from the stack.

		     In case of an error, this function returns -1
		     and sets lash_errno. If the exploration has
		     entirely been achieved, it returns zero. If
		     the optional function returns a positive
		     non-zero value, Tarjan's algorithm ends and
		     returns that value.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
			 LASH_ERR_ABORT    : The optional function
			                     has stopped with a
					     negative value.
			 LASH_ERR_TOO_BIG  : Automaton with too many
			                     states.               **/

int scc_algorithm(a, scc_num, scc_count, param, op, arg)
     automaton   *a;
     uint4       *scc_num, *scc_count;
     uint1        param;
     int        (*op)(automaton *, stack *, uint4, uint4 *, 
		      uint1, uint1, void *);
     void        *arg;
{
  register stack      *active, *st;
  register uint4      *rank, *lowlink;
  register bit_table  *placed;
           uint4       i, j;
	   int         res;

  diag__enter("scc_algorithm", -1);

  /* Verification of the arguments */

  if (auto_nb_states(a) > SCC_TRANSIENT_NODE)
    diag__fail(LASH_ERR_TOO_BIG, -1);

  /* Resource allocation */

  active = stack__new_empty(tarjan_info);
  if (!active)
    diag__fail(LASH_ERR_NO_MEM, -1);

  st = stack__new_empty(uint4);
  if (!active)
    {
      stack__free(active);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  rank = resr__new_objects(uint4, auto_nb_states(a));
  if (!rank)
    {
      stack__free(active);
      stack__free(st);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  lowlink = resr__new_objects(uint4, auto_nb_states(a));
  if (!lowlink)
    {
      stack__free(active);
      stack__free(st);
      resr__free_objects(rank, sizeof(uint4), auto_nb_states(a));
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if (!scc_num)
    {
      placed = bit__new_empty(auto_nb_states(a));
      if (!placed)
	{
	  stack__free(active);
	  stack__free(st);
	  resr__free_objects(rank, sizeof(uint4), auto_nb_states(a));
	  resr__free_objects(lowlink, sizeof(uint4), auto_nb_states(a));
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
    }
  else
    placed = NULL;

  /* Shared variables initialization */

  for (i=0 ; i<auto_nb_states(a) ; i++)
    {
      rank[i] = 0;
      if (scc_num)
	scc_num[i] = SCC_UNREACHABLE;
    }

  *scc_count = 0;
  res = 0;

  /* Tarjan's algorithm */

  if (param & SCC_ONLY_REACHABLE)
    {
      /* Limits Tarjan's algorithm to reachable scc */
      for (i=0 ; i<auto_nb_i_states(a) && res == 0 ; i++)
	{
	  if (auto_i_state(a, i, &j) < 0)
	    res = -3;

	  else if (rank[j] == 0)
	    res = tarjan(a, j, scc_num, scc_count, param,
			 op, arg, active, st, rank, lowlink, placed);
	}
    }
 
  else
    {
      for (i=0 ; i<auto_nb_states(a) && res == 0 ; i++)
	if (rank[i] == 0)
	  res = tarjan(a, i, scc_num, scc_count, param,
		       op, arg, active, st, rank, lowlink, placed);
    }

  /* Freeing memory */

  stack__free(active);
  stack__free(st);
  resr__free_objects(rank, sizeof(uint4), auto_nb_states(a));
  resr__free_objects(lowlink, sizeof(uint4), auto_nb_states(a));

  if (placed)
    bit__free(placed);

  if (res>=0) 
    {
      diag__return(res);
    }
  else 
    {
      switch (res) {
      case -2 : diag__fail(LASH_ERR_NO_MEM, -1); break;
      case -3 : diag__fail(LASH_ERR_CORRUPT, -1); break;
      default : diag__fail(LASH_ERR_ABORT, -1); break;
      }
    }
}

/**  uint4  *scc_compute(a, count)  :  This function is used for
		     numbering the s.c.c.'s of an automaton *a. It
		     returns an array that gives for each state,
		     the number of the s.c.c. to which it
		     belongs. In this array, transient states are
		     marked by the value SCC_TRANSIENT_STATE.
		     This function is not limited to reachable
		     s.c.c. At the end of the execution, the
		     variable *count contains the number of s.c.c.
		     in *a. Actually, this function simply
		     encapsulates a call to scc_algorithm.

		     In the case of an error, this function
		     returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
			 LASH_ERR_TOO_BIG  : Automaton with too many
			                     states.               **/

uint4 *scc_compute(a, count)
     automaton   *a;
     uint4       *count;
{
  register uint4  *scc;

  diag__enter("scc_compute", NULL);

  scc = resr__new_objects(uint4, auto_nb_states(a));
  if (!scc)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (scc_algorithm(a, scc, count, SCC_MARK_TRANSIENT, NULL, NULL)
      < 0)
    {
      resr__free_objects(scc, sizeof(uint4), auto_nb_states(a));
      diag__fail(lash_errno, NULL);
    }

  else
    diag__return(scc);
}

/****  End of graph-scc.c ****/
