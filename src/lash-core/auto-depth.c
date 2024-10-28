/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  auto-depth.c  :  Computes the depth of automata.              **/
/**                                                                **/
/**      06/19/00  :  Creation. (LL)                               **/
/**      04/16/02  :  Minor corrections. (LL)                      **/
/**      07/29/02  :  Reorganization. (BB)                         **/
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

#include "auto-depth.h"

/**  Private functions declarations                                **/

static int auto_depth_init(automaton *, fifo **, bit_table **) ;

/**  Private Functions                                             **/
 
/** int auto_depth_init(a, q, bt)  :  Creates a new empty queue and a
                     new empty bit_table and stores their addresses
		     in **q and **bt respectively.

		     In addition, creates one q_depth_info structure
		     per initial state of the automaton *a and stores
		     them in the queue **q.  Return 0 in the case of
		     success and -1 in the case of failure.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.  
			 LASH_ERR_CORRUPT    : Corrupt argument(s). 
                                                                   **/

static int  auto_depth_init(a, q, bt)
     automaton  *a;
     fifo      **q;
     bit_table **bt ;
{
  register int           i;
           uint4         s;
           q_depth_info  qdi ;
  
  diag__enter("auto_depth_init", -1);

  if (!a || !q || !bt)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (!(*q = fifo__new_empty(q_depth_info)))
    diag__fail(LASH_ERR_NO_MEM, -1);

    if (!(*bt = bit__new_empty(a -> nb_states))) 
      {
	fifo__free(*q);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  for (i = 0; i < auto_nb_i_states(a); i++)
    {
      if  (auto_i_state(a, i, &s) < 0)
	{
	  fifo__free(*q);
	  bit__free(*bt);
	  diag__fail(lash_errno, -1);
	}

      qdi.state = s;
      qdi.depth = 0;
      
      if (fifo__add(*q, &qdi) < 0)
	{
	  fifo__free(*q);
	  bit__free(*bt);
	  diag__fail(LASH_ERR_NO_MEM, -1);
  	} 
      
      bit__add(*bt, s);
     }

  diag__return(0);
}
 
/**  Public Functions                                              **/

/*  int  auto_depth(a, depth)  :  Computes the depth of the automaton 
                     a, defined as the maximum length of the shortest
                     path to reach a state (this maximum length
                     being computed over all states).

		     The function returns 0 and stores the computed
                     depth of the automata *a in *depth in the case of
                     success. It returns -1 in the case of failure.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  auto_depth(a, depth)
     automaton *a;
     uint4 *depth;
{
  register uint4  i, s, dpth;
  register tran  *t;
           uint4  nb_trans;
           fifo  *q;
    q_depth_info  qdi, qdi_next;
       bit_table *bt;
  
  diag__enter("auto_depth", -1);
  
  if (auto_depth_init(a, &q, &bt) < 0)
    diag__fail(lash_errno, -1);

  dpth = ZERO_INT4;

  while (!fifo__is_empty(q))
    {
      fifo__remove(q, &qdi);
	   
      if (qdi.depth > dpth)
	dpth++;

      if (auto_nb_out_transitions(a, qdi.state, &nb_trans )  < 0)
	{
	  fifo__free(q);
	  bit__free(bt);
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}

      for (i = 0; i < nb_trans; i++)
	{
	  if (!(t = auto_transition(a, qdi.state, i)))
	    {
	      fifo__free(q);
	      bit__free(bt) ;
	      diag__fail(LASH_ERR_CORRUPT, -1);
	    }
	  
	  s = auto_transition_dest(t);

	  if (!(bit__member(bt, s)))
	    {
	      /* state visited for the first time */

	      qdi_next.state = s;
	      qdi_next.depth = dpth + 1;
	      if (fifo__add(q, &qdi_next) < 0)
		{
		  fifo__free(q);
		  bit__free(bt);
		  diag__fail(LASH_ERR_NO_MEM, -1);
		}
	      bit__add(bt, s); 
	    }
	}
    }
  
  fifo__free(q);
  bit__free(bt);

  if (depth)
    *depth = dpth;

  diag__return(0);  
}

/** int auto_max_path_length(a, length, cyclic) : Computes the maximum
	             path length of the automaton *a, i.e. the maximum
	             number of symbols (composing the labels of
	             transitions) starting from the state until
	             reaching a state with no successors.  If the
	             automaton is cyclic, then there is at least one
	             loop and the maximum length is infinite. In this
	             case, *cyclic is set to 1. Otherwise, *length is
	             set equal to the computed length and *cyclic is
	             set to 0.  Returns 0 in the case of success and
	             -1 in the case of failure .                   **/

int  auto_max_path_length(a, length, cyclic)
     automaton *a;
     uint4     *length;
     int       *cyclic;
{  
           uint4 *lengths, s;
  register uint4  nb, i, rlength;

  diag__enter("auto_max_path_length", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !length || !cyclic)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif
  
  if (auto_nb_states(a) == 0)
    {
      *cyclic = 0;
      *length = 0;
      diag__return(0);
    }

  if (auto_states_max_path_length(a, &lengths, cyclic) <  0)
    diag__fail(lash_errno, -1);

  /* initialization */

  if (*cyclic)
    diag__return(0);

  rlength = 0;
  
  nb = auto_nb_i_states(a);

  for (i = 0; i < nb ; i++)
    {
      if (auto_i_state(a, i, &s) < 0)
	{
	  resr__free_objects(lengths, uint4, auto_nb_states(a));
	  diag__fail(lash_errno, -1);
	}
 
      if (lengths[s] > rlength)
	rlength = lengths[s];
    }

  resr__free_objects(lengths, uint4, auto_nb_states(a));

  if (length)
    *length = rlength;
  
  diag__return(0);
}

/** int auto_states_max_path_length(a, lengths, cyclic) : Computes for
                     each state of the automaton *a the maximum path
                     length starting from this state, i.e. the maximum
                     number of symbols (composing the labels of
                     transitions) starting from the state until
                     reaching a state with no successors.  If the
                     automaton is cyclic, then there is at least one
                     loop and some lengths are infinite. In this case,
                     *cyclic is set to 1. Otherwise, lengths[i] is set
                     equal to the maximum path length starting from
                     the ith state and *cyclic is set to 0.  Returns 0
                     in the case of success and -1 in the case of
                     failure .

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_VALUE  : Bad value of 
			                       parameter. 
                         LASH_ERR_NO_MEM     : Not enough memory. 
                                                                   **/
int  auto_states_max_path_length(a, lengths, cyclic)
     automaton *a;
     uint4    **lengths;
     int       *cyclic;
{
           uint4 *ranking, nb_out; 
  register uint4  i, j, l, s, lg;
  register tran  *tr;

  diag__enter("auto_states_max_path_length", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !lengths || !cyclic)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif

  if (auto_nb_states(a) == 0)
    {
      *lengths = NULL;
      *cyclic = 0;
      diag__return(0);
    }

  if (auto_topological_sort(a, &ranking, cyclic) < 0)
    diag__fail(lash_errno, -1);

  if (*cyclic)
    diag__return(0);

  *lengths = (uint4 *) resr__new_objects(uint4, auto_nb_states(a));
  if (!(*lengths))
    {
      resr__free_objects(ranking, uint4, auto_nb_states(a)); 
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  /* initialization */ 

  for (i = 0; i < auto_nb_states(a); i++)
    (*lengths)[ranking[i]] = 0;
  
  for (l = 0; l < auto_nb_states(a); l++)
    {
      i = auto_nb_states(a) - 1 - l;

      /* We take the states in the reverse topological order.
	 In this order, either the current state has no successor
	 or all its successors are already processed.
	 Processing a state means finding the longest path until
	 reaching a state with no successor. The value is either
	 0 if the state has no successors or it is 1 + the
	 largest length starting from one successor.
      */  
      
      if (auto_nb_out_transitions(a, ranking[i], &nb_out) < 0)
	{
	  resr__free_objects(ranking, uint4,auto_nb_states(a)); 
	  resr__free_objects(*lengths, uint4,auto_nb_states(a)); 
	  diag__fail(lash_errno, -1);
	}

      if (nb_out == 0)
	(*lengths)[ranking[i]] = 0;

      else
	for (j = 0; j < nb_out ; j++) 
	  {
	    if (!(tr = auto_transition(a, ranking[i], j))) 
	      {
		resr__free_objects(ranking, uint4, 
				   auto_nb_states(a)); 
		resr__free_objects(*lengths, uint4,
				   auto_nb_states(a)); 
		diag__fail(lash_errno, -1);
	      }

	    s = auto_transition_dest(tr);
	    lg = auto_transition_length(tr);

	    if ((*lengths)[s] + lg > (*lengths)[ranking[i]])
	      (*lengths)[ranking[i]] = (*lengths)[s] + lg;
	  }
    }
  
  resr__free_objects(ranking, uint4, auto_nb_states(a)); 
  
  diag__return(0);
}

/** int auto_max_path_rev_lex(a, trs, l, cyclic, empty) : Stores the
                     sequence of transitions corresponding to the
                     longest path within the automata *a in *trs and
                     the length of this path in *l. If the automaton
                     contains a loop, then *cyclic is set to 1.
		     If it is empty, then *empty is set to 1.
		     
                     The automaton *a is modified by this function: It
                     is transformed into a deterministic, minimal and
                     strongly normal one, and its transitions are
                     sorted lexicographically.  If more than one path
                     have the maximum length, then the path of choice
                     is selected on an reverse lexicographical order,
                     e.g., caacba will be selected even if accccc,
                     bbbbbb or caacaa exist.

		     The function returns 0 in the case of success
		     and -1 in the case of failure.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_BAD_TYPE   : Bad type of automata
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int auto_max_path_rev_lex(a, trs, l, cyclic, empty)
     automaton *a;
     tran **trs;
     uint4 *l;
     int   *cyclic, *empty;
{
  register uint4  i, s_next, max, cur_depth;
           uint4 *lengths, s, nb_out; 
  register tran  *tr, *t_max;

  diag__enter("auto_max_path", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !trs || !l || !cyclic || !empty)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif

  if ((auto_determinize(a) < 0) || (auto_minimize(a) < 0))
    diag__fail(lash_errno, -1);

  if (auto_nb_states(a) == 0)
    {
      *empty = 1;
      *cyclic = 0;
      *trs = NULL;
      *l = 0;
      diag__return(0);
    }
 
  *empty = 0;

  auto_sort_transitions(a);

  if (auto_states_max_path_length(a, &lengths, cyclic) < 0)
    diag__fail(lash_errno, -1);

  if (*cyclic)
    {
      resr__free_objects(lengths, uint4, auto_nb_states(a));
      *trs = NULL;
      *l = 0;
      diag__return(0);
    }
  
  if (auto_i_state(a, 0, &s) < 0)
    {
      resr__free_objects(lengths, uint4, auto_nb_states(a));
      diag__fail(lash_errno, -1);
    }
  
  cur_depth = 0;
  *trs = (tran *) resr__new_objects(tran, lengths[s]);
  *l = lengths[s];

  while(lengths[s])
    {
      if (auto_nb_out_transitions(a, s, &nb_out) < 0)
	{
	  resr__free_objects(lengths, uint4, auto_nb_states(a));
	  resr__free_objects(*trs, tran, *l);
	  diag__fail(lash_errno, -1);
	}

#if LASH_CHECK_LEVEL >= 1
      if (nb_out == 0)
	{
	  resr__free_objects(lengths, uint4, auto_nb_states(a));
	  resr__free_objects(*trs, tran, *l);
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}
#endif

      max = 0;
      t_max = NULL;
      for (i = 0; i < nb_out ; i++)
	{
	  if (!(tr = auto_transition(a, s, i)))
	    {
	      resr__free_objects(lengths, uint4, auto_nb_states(a));
	      resr__free_objects(*trs, tran, *l);
	      diag__fail(LASH_ERR_CORRUPT, -1);
	    }

	  s_next = auto_transition_dest(tr);
	  if (lengths[s_next] >= max)
	    {
	      t_max = tr;
	      max = lengths[s_next];
	    }
	}

      (*trs)[cur_depth] = *t_max;
      s = auto_transition_dest(t_max) ;
      cur_depth++;

    } /* while (lengths[s]) */

  resr__free_objects(lengths, uint4, auto_nb_states(a));
  diag__return(0);
}

/** int auto_min_path_lex(a, trs, l, empty) : Stores the sequence of
		     transitions corresponding to the smallest path
		     within the automaton *a in *trs and the length of
		     this path in *l. The smallest path is the
		     smallest path leading to an accepting state. The
		     automaton *a is modified by this function: It is
		     transformed into a deterministic, minimal and
		     strongly normal one, and its transitions are
		     sorted lexicographically.  If there is no
		     reachable accepting state, *empty is set to 1.
		     If more than one path have the minimum length,
		     then the path is selected on a lexicographical
		     order, e.g., : aaaaaa will be selected even if
		     accccc, bbbbbb or caacaa exist.

		     The function returns 0 in the case of success and
		     -1 in the case of failure.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
			 LASH_ERR_BAD_TYPE   : Bad type of automata
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

int auto_min_path_lex(a, trs, l, empty)
     automaton *a;
     tran    **trs;
     uint4    *l;
     int      *empty;
{
  register uint4         i, cur_depth, *pred;
           uint4         s, s0, nb_trans;
  register fifo         *q;
           q_depth_info  qdi, qdi_next;
  register bit_table    *bt;
  register tran         *t, **incoming_tran;

  diag__enter("auto_min_path", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !trs || !l || !empty)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif

  if ((auto_determinize(a) < 0) || (auto_minimize(a) < 0))
    diag__fail(lash_errno, -1);

  if (auto_nb_states(a) == 0)
    {
      *empty = 1;
      *trs = NULL;
      *l = 0;
      diag__return(0);
    }
  
  *empty = 0;

  auto_sort_transitions(a);

   if (!(q = fifo__new_empty(q_depth_info)))
    diag__fail(LASH_ERR_NO_MEM, -1);

    if (!(bt = bit__new_empty(a -> nb_states))) 
      {
	fifo__free(q);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

    if (!(pred = resr__new_objects(uint4, auto_nb_states(a))))
      {
	fifo__free(q);
	bit__free(bt);
	diag__fail(lash_errno, -1);
      }
 
    if (!(incoming_tran = 
	  resr__new_objects(tran *, auto_nb_states(a))))
      {
	fifo__free(q);
	bit__free(bt);
	resr__free_objects(pred, uint4, auto_nb_states(a));	
	diag__fail(lash_errno, -1);
      }
    
     if (auto_i_state(a, 0, &s0) < 0)
      {
	fifo__free(q);
	resr__free_objects(pred, uint4, auto_nb_states(a));
	bit__free(bt);
	resr__free_objects(incoming_tran, tran *, auto_nb_states(a)); 
	diag__fail(lash_errno, -1) ;
      }
     
    qdi.state = s0;
    qdi.depth = ZERO_INT4;
    
    if (fifo__add(q, &qdi) < 0 )
      {
	bit__free(bt);	
        fifo__free(q);
	resr__free_objects(pred, uint4, auto_nb_states(a));
	resr__free_objects(incoming_tran, tran *, auto_nb_states(a)); 
	diag__fail(LASH_ERR_NO_MEM, -1);
      } 
    
    bit__add(bt, s0);
    cur_depth = ZERO_INT4;
    
    for(;;)
      {
	fifo__remove(q, &qdi);
	
	if (qdi.depth > cur_depth)
	  cur_depth++;
      
	if (auto_accepting_state(a, qdi.state))
	  {
	    if (qdi.depth == ZERO_INT4)
	      {
		*trs = NULL;
		*l = 0;
	      }
	    else 
	      {
		*l = cur_depth;

		if (!(*trs = resr__new_objects(tran, cur_depth)))
		  {
		    bit__free(bt);	
		    fifo__free(q);
		    resr__free_objects(pred, uint4, 
				       auto_nb_states(a));
		    resr__free_objects(incoming_tran, tran *, 
				       auto_nb_states(a)); 
		    diag__fail(LASH_ERR_NO_MEM, -1);
		  }

		s = qdi.state;

		while (s != s0)
		  {
		    (*trs)[cur_depth - 1] = *(incoming_tran[s]);
		    s = pred[s];
		    cur_depth--;
		  }
	      }
	    
	    bit__free(bt);	
	    fifo__free(q);
	    resr__free_objects(pred, uint4, auto_nb_states(a));
	    resr__free_objects(incoming_tran, tran *, 
			       auto_nb_states(a)); 
	    diag__return(0);
	  }

	if (auto_nb_out_transitions(a, qdi.state, &nb_trans ) < 0)
	  {
	    fifo__free(q);
	    bit__free(bt);
	    resr__free_objects(pred, uint4, auto_nb_states(a));
	    resr__free_objects(incoming_tran, tran *, 
			       auto_nb_states(a)); 
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}

	for (i = 0; i < nb_trans; i++)
	  {
	    if (!(t = auto_transition(a, qdi.state, i)))
	      {
		fifo__free(q);
		bit__free(bt);
		resr__free_objects(pred, uint4, auto_nb_states(a));
		resr__free_objects(incoming_tran, tran *, 
				   auto_nb_states(a)); 
		diag__fail(LASH_ERR_CORRUPT, -1);
	      }
	  
	    s = auto_transition_dest(t);
	  
	    if (!(bit__member(bt, s)))
	      {
		/* state visited for the first time */

		qdi_next.state = s;
		qdi_next.depth = cur_depth + 1;

		if (fifo__add(q, &qdi_next) < 0)
		  {
		    fifo__free(q);
		    bit__free(bt);
		    resr__free_objects(pred, uint4, 
				       auto_nb_states(a));
		    resr__free_objects(incoming_tran, tran *,
				       auto_nb_states(a)); 
		    diag__fail(LASH_ERR_NO_MEM, -1);
		  }
		bit__add(bt, s); 
		pred[s] = qdi.state;
		incoming_tran[s] = t;
	      }
	  }
      }
    
    resr__free_objects(pred, uint4, auto_nb_states(a));
    resr__free_objects(incoming_tran, tran *, auto_nb_states(a)); 
    fifo__free(q);
    bit__free(bt);
    diag__return(0);   
}

/****  End of auto-depth.c  ****/
