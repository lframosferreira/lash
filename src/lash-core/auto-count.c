/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  auto-count.c  :  Counting words accepted by automata.         **/
/**                                                                **/
/**      20/07/00  :  Creation. (LL)                               **/
/**      18/10/00  :  Some corrections. (LL)                       **/
/**      02/12/01  :  Reorganization. (BB)                         **/
/**      08/13/01  :  Minor correction. (BB)                       **/
/**      07/08/02  :  Reorganization. (BB)                         **/
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
#include "datastruct.h"
#include "lash-auto.h"
#include "biguint.h"
#include "auto.h"
#include "diag.h"
#include "resource.h"
#include "lash-auto-io.h"
#include "lash-auto-operations.h"
#include "auto-count.h"

/** count_count_ncolls  :  Number of collisions observed in the   
                     hash table used in the DFS for counting
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  count_count_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  count_count_nins  :  Number of insertions performed in the   
                     hash table used in the DFS for counting
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  count_count_nins = ZERO_INT8;
#endif  /* >= 1 */

/** Private Functions                                              **/

void free_h_count(void *);

/** Definition of private functions                                **/

void free_h_count(void *p)
{
  h_count_info *ph;  
  
  if (!p)
    return;

  ph = (h_count_info *) p;  
  biguint__free(ph -> node_count);
  resr__free_object(ph, h_count_info);
  return;
}

/**  Public Functions                                             **/ 

/*  int  auto_count_accept(nb_paths, a) : Computes the number of 
                         paths leading to an accepting state in the
                         minimized automaton a and store this number
                         in *nb_paths.

			 Returns 0 in the case of success, and -1 in 
                         the case of failure.                       */

int  auto_count_accept(biguint *nb_paths, automaton *a)
{
  uint4 s, nb_trans;
  stack *st;
  hash_table *ht;
  s_count_info *sel,  *sel_next;
  h_count_info *pay, *prev_pay;
  tran *t;
  void **r;

  diag__enter("auto_count_accept", -1);

  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);
  
  if (biguint__setzero(nb_paths) < 0)
    diag__fail(lash_errno, -1);

  if (!(auto_test_property(a, AUTO_PROP_DETERM)))
    diag__fail(LASH_ERR_PROP, -1);

  if (auto_nb_states(a) == 0)
    diag__return(0);
  
  if (!(st = stack__new_empty_from_size(sizeof(s_count_info))))
    diag__fail(lash_errno, -1);
  
  if (!(ht = hash__new_empty(a -> nb_states))) 
    diag__fail(lash_errno, -1);
      
  if (!(sel = resr__new_object(s_count_info)))
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  if (auto_i_state(a, 0, &s) < 0)
    diag__fail(lash_errno, -1);
  
  sel -> s = s ;
  sel -> nb_trans_ex = ZERO_INT4 ;
  sel -> init = 1;
  sel -> prev = 0; 
  
  if (stack__push(st, sel) < 0)
    diag__fail(lash_errno, -1);
  
  resr__free_object(sel, s_count_info) ;
  
  while (!stack__is_empty(st))
    {
      if(!(sel = (s_count_info *) stack__top(st)))
	{
	  stack__free(st);
	  bytes__prepare_free(sizeof(h_count_info)); 
	  hash__free(ht,
		     (void (*)(void *)) uint4__free, 
		     (void (*)(void *)) bytes__free);
	  
	  diag__fail(lash_errno, -1);
	}
	   
      if (sel -> nb_trans_ex == ZERO_INT4)
	{
		      
	  /* node visited for the first time */ 
#if LASH_CHECK_LEVEL >= 1
	  if (hash__insert_bytes(ht, (uint1 *) &s, sizeof(s),
              &r, &count_count_ncolls, &count_count_nins) < 0)  
#else
	    if (hash__insert_bytes(ht, (uint1 *) &s, 
                sizeof(s), &r) < 0)
#endif
	      {
		stack__free(st);
		bytes__prepare_free(sizeof(h_count_info)); 
		hash__free(ht,
			   (void (*)(void *)) uint4__free, 
			   (void (*)(void *)) bytes__free);
	  
		diag__fail(lash_errno, -1);
	      }

	  if (!r)
	    {
	      stack__free(st);
	      bytes__prepare_free(sizeof(h_count_info)); 
	      hash__free(ht,
			 (void (*)(void *)) uint4__free, 
			 (void (*)(void *)) bytes__free);
	  

	      diag__fail(lash_errno, -1);
	    }

	  pay = resr__new_object(h_count_info);
	  *r = pay;
	  pay -> in_stack = 1;
	  if (!(pay -> node_count =  biguint__new_zero()))
	   {
		  stack__free(st);
		  bytes__prepare_free(sizeof(h_count_info)); 
		  hash__free(ht,
			     (void (*)(void *)) uint4__free, 
			     (void (*)(void *)) bytes__free);
	  
		  diag__fail(lash_errno, -1);
		}
 
	  pay -> loop = 0 ;
	    
	  if (auto_accepting_state(a, s) == 1)
	    {
	      if ( biguint__add1(pay -> node_count, 
				 pay -> node_count) < 0)
		{
		  stack__free(st);
		  bytes__prepare_free(sizeof(h_count_info)); 
		  hash__free(ht,
			     (void (*)(void *)) uint4__free, 
			     (void (*)(void *)) bytes__free);
	  

		  diag__fail(lash_errno, -1);
		}
	    }	    
		
	}  /*     if (nb_trans_ex == ZERO_INT4)   */
      
      if ( auto_nb_out_transitions(a, sel -> s, &nb_trans )  < 0)
	{
	  bytes__prepare_free(sizeof(h_count_info)); 
	  hash__free(ht,
		     (void (*)(void *)) uint4__free, 
		     (void (*)(void *)) bytes__free);
	  

	  stack__free(st);
	  diag__fail(lash_errno, -1);
	}

      if (sel -> nb_trans_ex < nb_trans)
	{
	  if (!(t = auto_transition(a, sel -> s, sel -> nb_trans_ex)))
	    {
	      bytes__prepare_free(sizeof(h_count_info)); 
	      hash__free(ht,
			 (void (*)(void *)) uint4__free, 
			 (void (*)(void *)) bytes__free);
	  
	      stack__free(st);
	      diag__fail(lash_errno, -1);
	    }
	  
	  s = auto_transition_dest(t);

	  (sel -> nb_trans_ex)++;
	
	  if ((pay = (h_count_info *) hash__lookup_bytes(ht, 
	      (uint1 *) &s, sizeof(uint4)))) 
	    {
	      /* node already visited */
	      if ( pay -> in_stack == 1)
		pay -> loop = 1;
	      else 
		if ((!(prev_pay = (h_count_info *) 
                       hash__lookup_bytes(ht, (uint1 *) 
                           &sel -> s, sizeof(uint4))))
		    ||  (biguint__add(prev_pay -> node_count,
				      prev_pay -> node_count,
				     pay -> node_count) < 0))
		  {
		    stack__free(st);
		    bytes__prepare_free(sizeof(h_count_info)); 
		    hash__free(ht,
			       (void (*)(void *)) uint4__free, 
			       (void (*)(void *)) bytes__free);
		    diag__fail(lash_errno, -1);
		  }
	      
	    }
	  else
	    {
	      if (!(sel_next = resr__new_object(s_count_info)))
		{		
		  bytes__prepare_free(sizeof(h_count_info)); 
		  hash__free(ht,
			     (void (*)(void *)) uint4__free, 
			     (void (*)(void *)) bytes__free);
	  

		  stack__free(st);
		  diag__fail(LASH_ERR_NO_MEM, -1);
		}
	      
	      sel_next -> s = s ;
	      sel_next -> prev = sel -> s ;
	      sel_next -> nb_trans_ex = ZERO_INT4 ;
	      sel_next -> init = 0;
	      
	      stack__push(st, sel_next);
			
	      resr__free_object(sel_next, s_count_info);
	    }
	}
      else  /* no more transition to be explored */
	{
	  pay = (h_count_info *) 
	    hash__lookup_bytes(ht, (uint1 *) &sel -> s, 
                sizeof(uint4));
	     
	    if ( sel -> init != 1) 
	      prev_pay = (h_count_info *) 
		hash__lookup_bytes(ht, (uint1 *) &sel -> prev,
                    sizeof(uint4));
	    else
	      prev_pay = NULL;
	
	    if ((!pay) || (( sel -> init != 1) && (!prev_pay)))
	      {
		stack__free(st);
		bytes__prepare_free(sizeof(h_count_info)); 
		hash__free(ht,
			   (void (*)(void *)) uint4__free, 
			   (void (*)(void *)) bytes__free);
		
		diag__fail(lash_errno, -1);
	      }
	    
	    if ((( pay -> loop == 1) 
		 && (!biguint__is_zero(pay -> node_count)))
		|| (biguint__is_inf(pay -> node_count)))
	      {
		if ( biguint__setinf(nb_paths))
		  {
		    bytes__prepare_free(sizeof(h_count_info)); 
		    hash__free(ht,
			       (void (*)(void *)) uint4__free, 
			       (void (*)(void *)) bytes__free);
		    
		    
		    stack__free(st);
		    diag__fail(lash_errno, -1);
		  } 
		else
		  {
		    bytes__prepare_free(sizeof(h_count_info)); 
		    hash__free(ht,
			       (void (*)(void *)) uint4__free, 
			       (void (*)(void *)) bytes__free);
		    
		    
		    stack__free(st);
		    diag__return(0);
		  }
	      }
	    else 
	      if ((( sel -> init != 1) 
		   &&  (biguint__add(prev_pay -> node_count,
				     prev_pay -> node_count,
				     pay -> node_count) < 0))
		  || (( sel -> init == 1)
		      && (biguint__add(nb_paths, 
				       nb_paths,
				       pay -> node_count) < 0)))
		{
		  bytes__prepare_free(sizeof(h_count_info)); 
		  hash__free(ht,
			     (void (*)(void *)) uint4__free, 
			     (void (*)(void *)) bytes__free);
		  
		  
		  stack__free(st);
		  diag__fail(lash_errno, -1);
		} 
	    
	    pay -> in_stack = 0; 
	    stack__pop(st, NULL);	  
	}
    }
  
  hash__free(ht,
	     (void (*)(void *)) uint4__free, 
	     (void (*)(void *)) free_h_count);
  
  stack__free(st);
  diag__return(0);
  
}

/****  End of auto-count.c  ****/
