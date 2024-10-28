/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       auto-sort.c  :  Automata sorting algorithms.             **/
/**                                                                **/
/**        01/23/02  :  Creation. (LL)                             **/
/**        07/15/02  :  Reorganization. (BB)                       **/
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
#include "auto-sort.h"
#include "auto.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"

/****  Prototype of private function.                            ****/

static int  init_predecessors(automaton *, uint4_set ***, 
			      uint4_set **);

/****  Private function.                                         ****/

/** int  init_predecessors(a, predecessors, without_pred) : Computes
                   the predecessors of each state of the automaton *a.
                   In the case of success, it returns 0 and *stores in
                   predecessors a pointer to a newly *allocated array
                   of uint4_set * whose size is the *number of states
                   in the automaton.  The i-th set *in the array
                   contains the predecessors of state *i.  It also
                   stores in without_pred a pointer to *the uint4_set
                   containing all the states that *have no incoming
                   transition.  In the case of *failure, the function
                   returns -1.

                   Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_NO_MEM     : Not enough memory. **/

static int  init_predecessors(a, predecessors, without_pred)
     automaton   *a;
     uint4_set ***predecessors;
     uint4_set  **without_pred;
{
  register uint4       i, j, k;
           uint4       nb_out;
  register tran       *tr;
  register uint4_set **pred;

  diag__enter("init_predecessors", -1);
  
  if (!a || !predecessors || !without_pred)
    diag__fail(LASH_ERR_BAD_VALUE, -1);

  *predecessors = (uint4_set **) 
    resr__new_objects(uint4_set *, auto_nb_states(a));
  if (!*predecessors)
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  for (i = 0, pred = *predecessors; i < auto_nb_states(a); i++)
    {
      pred[i] = set__new_empty();
      if (!pred[i])
	{
	  for (j = 0, pred = *predecessors; j < i ; j++)
	    set__free(pred[j]);
	  resr__free_objects(*predecessors, uint4_set *,
			     auto_nb_states(a)); 
	  diag__fail(LASH_ERR_NO_MEM, -1);	  
	}
    }

  *without_pred = set__new_empty();
  if (!(*without_pred))
    {
      for (k = 0 , pred = *predecessors; k < auto_nb_states(a); k++)
	set__free(pred[k]);
      resr__free_objects(*predecessors, uint4_set *,
			 auto_nb_states(a)); 
      diag__fail(LASH_ERR_NO_MEM, -1);	  
    }

  for (i = 0; i < auto_nb_states(a); i++)
    if (set__add(*without_pred,i) < 0)
      {
	for (k = 0 , pred = *predecessors; 
	     k < auto_nb_states(a); k++)
	  set__free(pred[k]);
	resr__free_objects(*predecessors, uint4_set *,
			   auto_nb_states(a)); 
	diag__fail(LASH_ERR_NO_MEM, -1);	  
      }
  
  for (i = 0; i < auto_nb_states(a); i++)
    {
      if ( auto_nb_out_transitions(a, i, &nb_out) < 0)
	{
	  for (pred = *predecessors, k = 0; 
	       k < auto_nb_states(a); k++)
	    set__free(pred[k]);
	  resr__free_objects(*predecessors, uint4_set *,
			     auto_nb_states(a)); 
	  set__free(*without_pred);
	  diag__fail(lash_errno, -1);
	}
      for (j = 0; j < nb_out; j++) 
	{
	  if (!(tr = auto_transition(a, i, j))) 
	    {
	      for (pred = *predecessors, k = 0; 
		   k < auto_nb_states(a); k++)
		set__free(pred[k]);
	      resr__free_objects(*predecessors, uint4_set *,
				 auto_nb_states(a)); 
	      set__free(*without_pred);
	      diag__fail(lash_errno, -1);
	    }
	  if (set__add((*predecessors)[auto_transition_dest(tr)],
		       i) < 0)
	    {
	      for (pred = *predecessors, k = 0;
		   k < auto_nb_states(a); k++)
		set__free(pred[k]);
	      resr__free_objects(*predecessors, uint4_set *,
				 auto_nb_states(a)); 
	      set__free(*without_pred);
	      diag__fail(lash_errno, -1);
	    }
	  
	  set__remove(*without_pred, auto_transition_dest(tr));
	}
    }
  
  diag__return(0);
}

/****  Public visible function.                                  ****/

/** int auto_topological_sort(a, order, cyclic): Sorts the states of
		     the automaton *a according to a topological
		     order.  Since topological order is relevant only
		     if the graph is acyclic (DAG), the automaton must
		     be ayclic.  The automaton *a is left unchanged.
		     In the case of success, it returns 0 and stores
		     in *order a permutation of the states of the
		     automaton *a and corresponding to the topological
		     order generated: if there is a transition from
		     state i to state j, then the index of i in *order
		     is smaller than the index of j in *order.  If the
		     automaton is cyclic then *cyclic is set to 1 and
		     0 otherwise.  In the case of error, it returns -1
		     and set lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int  auto_topological_sort(a, ranking, cyclic)
     automaton *a;
     uint4    **ranking;
     int       *cyclic;
{
  /*
    The basic idea of the algorithm is the following:
     while not done
       if the graph is empty, then we're done (exit the loop)
       pick a node with no predecessors
       if no such node exists , then the graph is cyclic (exit the 
          loop)
       output that node (number that node)
       delete that node from the graph
     end while
  */

           uint4_set **predecessors, *without_pred;
  register uint4       n, j, k, dest, rank;
           uint4       nb_out;
  register tran       *tr;

  diag__enter("auto_topological_sort", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!a || !ranking || !cyclic)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif

  if (auto_nb_states(a) == 0)
    {
      *ranking = NULL;
      *cyclic = 0;
      diag__return(0);
    }
  
  if (!(*ranking = resr__new_objects(uint4,  auto_nb_states(a))))
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (init_predecessors(a, &predecessors, &without_pred) < 0)
    {
      resr__free_objects(*ranking, uint4, auto_nb_states(a));
      diag__fail(lash_errno, -1);
    }

  rank = 0;
  while (set__nb_elements(without_pred) > 0)
    {
      n = set__element(without_pred, 0);
      (*ranking)[rank++] = n;
      
      if ( auto_nb_out_transitions(a, n, &nb_out) < 0)
	{
	  for (k = 0; k < auto_nb_states(a); k++)
	    set__free(predecessors[k]);
	  resr__free_objects(predecessors, uint4_set *,
			     auto_nb_states(a)); 
	  set__free(without_pred);
	  diag__fail(lash_errno, -1);
	}

      for (j = 0; j < nb_out; j++) 
	{
	  if (!(tr = auto_transition(a, n, j)) )
	    {
	      for (k = 0; k < auto_nb_states(a); k++)
		set__free(predecessors[k]);
	      
	      resr__free_objects(predecessors, uint4_set *,
				 auto_nb_states(a)); 
	      set__free(without_pred);
	      diag__fail(lash_errno, -1);
	    }

	  dest = auto_transition_dest(tr);
	  set__remove(predecessors[dest], n);
	  
	  if ((set__nb_elements(predecessors[dest]) == 0)
	      && (set__add(without_pred, dest) < 0))
	    {
	      for (k = 0; k < auto_nb_states(a); k++)
		set__free(predecessors[k]);

	      resr__free_objects(predecessors, uint4_set *,
				 auto_nb_states(a)); 
	      set__free(without_pred);
	      diag__fail(lash_errno, -1);
	    }
	}    
      set__remove(without_pred, n);
    }
  
  set__free(without_pred);  
  for (j = 0; j < auto_nb_states(a); j++)
    set__free(predecessors[j]);

  resr__free_objects(predecessors, uint4_set *, auto_nb_states(a));  
  
  if (rank < auto_nb_states(a))
    {
      *cyclic = 1;
      resr__free_objects(*ranking, uint4,  auto_nb_states(a));
    }
  else
    *cyclic = 0;

  diag__return(0);
}

/****  End of auto-sort.c  ****/
