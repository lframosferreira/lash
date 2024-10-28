/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**   ndd-count.c  :  Counting vectors accepted by NDDs.           **/
/**                                                                **/
/**      07/20/00  :  Creation. (LL)                               **/
/**      10/18/00  :  Some corrections. (LL)                       **/
/**      12/13/00  :  Update to match algorithm (LL)               **/
/**      02/06/01  :  Correction in remove_sign_loop (LL)          **/
/**      02/13/01  :  Reorganization. (BB)                         **/
/**      04/04/01  :  Reorganization. (LL)                         **/
/**      04/10/01  :  ndd_max and ndd_min (LL)                     **/
/**      05/29/01  :  Handling of empty set. (LL)                  **/
/**      10/19/01  :  Correction project_over_y (LL)               **/
/**      10/19/01  :  Simplification get_bound (LL)                **/
/**      01/21/02  :  Correction get_bound (LL)                    **/
/**      07/03/02  :  Minor correction (LL)                        **/
/**      07/15/02  :  Reorganization. (BB)                         **/
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
#include "ndd.h"
#include "biguint.h"
#include "auto-count.h"
#include "datastruct.h"
#include "lash-auto.h"
#include "biguint.h"
#include "auto.h"
#include "ndd.h"
#include "diag.h"
#include "resource.h"
#include "lash-auto-io.h"
#include "lash-auto-operations.h"
#include "arithmetic.h"
#include "auto-depth.h"
#include "ndd-count.h"

/** ndd_count_ncolls  :  Number of collisions observed in the   
                     hash table used in the DFS for counting
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  ndd_count_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  ndd_count_nins  :  Number of insertions performed in the   
                     hash table used in the DFS for counting
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  ndd_count_nins = ZERO_INT8;
#endif  /* >= 1 */

/**  Private functions prototypes.                                 **/

static int remove_sign_loop_init(ndd *, automaton **, stack **,
				 hash_table **, uint4 *);
static int register_state(uint4, hash_table *);
static int fetch_next_state(s_count_prep_info *, stack *, 
			    hash_table *, automaton *, uint1 *, 
			    uint4);
static ndd *project_over_y(ndd *, uint4);
static int get_lower_bound(ndd *, int *, sint4 *);
static int get_upper_bound(ndd *, int *, sint4 *);
static int convert_path_to_sint(tran *, uint4 , uint1, sint4 *);

/**  Private macro.                                                **/

#define free_count_structures(st, ht, n) { \
	      stack__free(st); \
	      bytes__prepare_free(n);  \
	      hash__free((ht), (void (*)(void *)) uint4__free, \
			 (void (*)(void *)) bytes__free); }

/**  Private functions.                                            **/

/* int remove_sign_loop_init(nd, a, st , ht): This function is the
                    first step of the algorithm aimed at removing the
                    sign loop.  The nd must be serial. The ndd nd is
                    left unchanged.  It initializes the automaton a,
                    the stack st and the hash-table ht.  Return 0 in
                    the case of success and -1 in the case of failure.
			 
                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
			 LASH_ERR_PROP       : Bad property.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_STATE  : Automaton contains
                                               reference to an
                                               invalid state.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

static int remove_sign_loop_init(nd, a, st , ht, new_s) 
     ndd         *nd ;
     automaton  **a;
     stack      **st;
     hash_table **ht;
     uint4       *new_s;
{
           s_count_prep_info  sel_next;
           uint4              s;
  register automaton         *a1;

  diag__enter("remove_sign_loop_init", -1);

  if (!(nd -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_PROP, -1);
  
  if (!(*a = auto_copy(nd -> automaton)))
    diag__fail(lash_errno, -1);
 
  if ((!(auto_test_property(*a, AUTO_PROP_STRONG)))
      && (auto_normalize(*a) < 0))
    diag__fail(lash_errno, -1);
  
  if ((!(auto_test_property(*a, AUTO_PROP_DETERM)))
      && (auto_determinize(*a) < 0 ))
    diag__fail(lash_errno, -1);
  
  if (auto_nb_states(*a) == 0)
    diag__return(0);
    
  if (!(nd -> properties & NDD_PROP_MSDF)) 
    {
      if (!(a1 = auto_reverse(*a)))
	diag__fail(lash_errno, -1);

      auto_free(*a); 
      *a = NULL;

      if ((auto_determinize(a1) < 0) || (auto_minimize(a1) < 0))
	diag__fail(lash_errno, -1);

      *a = a1;
    }
  
  if (!(*st = stack__new_empty_from_size(sizeof(s_count_prep_info))))
    {
      auto_free(*a); 
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if (!(*ht = hash__new_empty(auto_nb_states(*a))))
    {
      auto_free(*a); 
      stack__free(*st);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if ((auto_add_new_state(*a,new_s) < 0) || 
      (auto_i_state(*a, 0, &s) < 0))
    {
      free_count_structures(*st, *ht, sizeof(h_count_prep_info));
      auto_free(*a); 
      diag__fail(lash_errno, -1);
    }   

  sel_next.s = s ;
  sel_next.nb_trans_ex = ZERO_INT4;
  sel_next.init = 1;

  if (stack__push(*st, &sel_next) < 0)
    {
      free_count_structures(*st, *ht, sizeof(h_count_prep_info));
      auto_free(*a); 
      diag__fail(lash_errno, -1);
    } 

  diag__return(0);
}

/* int register_state(s, ht) : This function is part of the algorithm
                    aimed at removing the sign loop.  Stores the state
                    s in the hash table and initializes the payload to
                    1 in the case s is an accepting state of a or 0
                    otherwise.  Return 0 in the case of success and -1
                    in the case of failure.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

static int register_state(s, ht) 
  uint4 s;
  hash_table *ht;
{
  void **r;
  register h_count_prep_info *pay; 

  diag__enter("register_state", -1);

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) &s, sizeof(s),
			 &r, &ndd_count_ncolls, 
			 &ndd_count_nins) < 0)  
#else
    if (hash__insert_bytes(ht, (uint1 *) &s, sizeof(s), &r) < 0)
#endif
      {
	diag__fail(lash_errno, -1);
      }
  
  if (!r)
      diag__fail(LASH_ERR_CORRUPT, -1);

  if (!(pay = resr__new_object(h_count_prep_info)))
      diag__fail(LASH_ERR_NO_MEM, -1);

  *r = pay;
  pay -> in_stack = 1;
  pay -> leads_to_cycle = 0;	
  diag__return(0);
}

/* int fetch_next_state(sel, st, ht, a, loop, null_state) :  This 
                    function is part of the algorithm aimed at
                    removing the sign loop.  Examines the next
                    successor of the state corresponding to sel and
                    updates the stack st if required.  If a loop is
                    identified, the uint1 pointed by loop is set to 1.
                    Return 0 in the case of success and -1 in the case
                    of failure.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automata.   **/

static int fetch_next_state(sel, st, ht, a, loop, null_state)
  s_count_prep_info *sel;
  stack *st;
  hash_table *ht;
  automaton *a;
  uint1 *loop;
  uint4 null_state;
{
  register tran  *t;
           uint4  s;
  s_count_prep_info sel_next;
  register h_count_prep_info *pay, *next_pay;
  
  diag__enter("fetch_next_state", -1);
  
  if (!(t = auto_transition(a, sel -> s, sel -> nb_trans_ex)))
    diag__fail(LASH_ERR_CORRUPT, -1);
  
  s = auto_transition_dest(t);
  sel -> nb_trans_ex++;
	
  if ((next_pay = (h_count_prep_info *)
       hash__lookup_bytes(ht, (uint1 *) &s, sizeof(uint4)))) 
    {
      /* node already visited */
      if (next_pay -> leads_to_cycle == 1)
	{
	  *loop = 1;
	  diag__return(0);
	}
	      
      if (next_pay -> in_stack == 1)
	{
	  if (!(pay = (h_count_prep_info *)
		hash__lookup_bytes(ht, (uint1 *) &sel -> s, 
				   sizeof(uint4))))
	      diag__fail(LASH_ERR_CORRUPT, -1);
  
	  if (pay -> leads_to_cycle == 1)
	    {
	      *loop = 1 ;
	      diag__return(0);
	    }
		  
	  pay -> leads_to_cycle = 1;
	  auto_redirect_transition(t , null_state);
	}
    }
  else
    {
      /* Node never met before */
      sel_next.s = s ;
      sel_next.nb_trans_ex = ZERO_INT4 ;
      sel_next.prev_s = sel -> s;
      sel_next.init = 0;
      stack__push(st, &sel_next);			
    }

  diag__return(0);
}

/* ndd *project_over_y(nd, y) : Computes an NDD representing the 

                     projection of the set represented by the NDD *nd
                     over the y-th vector component. The dimension of
                     *nd must be at least equal to y.

                     This function does not modify *nd, and returns
                     (in the case of success) a pointer to a newly
                     allocated NDD of dimension 1. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_BAD_TYPE   : Bad type of NDD(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD.        **/

static ndd *project_over_y(nd, y)
     ndd *nd;
     uint4 y;
{
  register ndd  *nd_y, *nd1 ;
  register uint4 i;

  diag__enter("project_over_y", NULL);

  if (!nd || (nd -> dim < y))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(nd_y = ndd_copy(nd)))
    diag__fail(lash_errno, NULL);

  if (nd -> dim > 1)
    for (i = nd -> dim ; i > 0 ; i--)
      {
	nd1 = nd_y;
	
	if (i == y)
	  continue;
	
	if (!(nd_y =  ndd_projection(nd1, i - 1)))
	  {
	    ndd_free(nd1);
	    diag__fail(lash_errno, NULL);
	  }
	
	ndd_free(nd1);
      } 
  
  diag__return(nd_y);
}

/** int get_upper_bound(nd, bounded, val): Computes the upper bound in
                    the element values of the set represented by the
                    NDD *nd.
		 
		    If such a bound does not exists, then, it sets the
		    *bounded to 0, otherwise, *bounded is set to 1,
		    *and the bound is stored in val.
			      
		    The NDD *nd must be of dimension 1.

		    This function does not modify nd.
			      
		    Returns 0 in the case of success and -1 in the
		    case of failure and sets lash_errno.
 
                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.     
			 LASH_ERR_OVERFLOW   : Arithmetic overflow
			 LASH_ERR_BAD_VALUE  : Bad argument value(s)
                                                                   **/
                          
static  int get_upper_bound(nd, bounded, val) 
     ndd   *nd;
     int   *bounded;
     sint4 *val;
{
           sint4          a, b, p, q;
  register linear_transf *tr;
  register ndd           *nd_pos, *nd_neg;
  register biguint       *nb_sol;
           automaton     *au;
           tran          *trs;
           int            empty, cyclic;
           uint4          l;

  diag__enter("get_upper_bound", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || (nd -> dim != 1) || !val || !bounded)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif 

  if (ndd_is_empty(nd))
    diag__fail(LASH_ERR_BAD_VALUE, -1);
     
  a = 1; b = 0; p = -1;  q = 0 ; 
  
  if (!(tr = ndd_create_transf(1, 1, &a, &b, &p, &q)))
    diag__fail(lash_errno, -1);
   
  if (!(nd_pos = ndd_image_by_transf(nd, tr)))
    {
      ndd_transf_free(tr);
      diag__fail(lash_errno, -1);
    }
  
  if (ndd_transf_free(tr) < 0)
    {
      ndd_free(nd_pos);
      diag__fail(lash_errno, -1);
     }
 
   if (!(nb_sol = biguint__new_zero())) 
     {
       ndd_free(nd_pos); 
       diag__fail(LASH_ERR_NO_MEM, -1);
     }
   
   if (ndd_count(nd_pos, nb_sol) < 0)
     {
       ndd_free(nd_pos); 
       diag__fail(lash_errno, -1);
     }
   
   if (biguint__is_inf(nb_sol))
     {
       ndd_free(nd_pos); 
       biguint__free(nb_sol);
       *bounded = 0; 
       diag__return(0) ;
     }
   else
     *bounded = 1;
   
   biguint__free(nb_sol);

   if (ndd_is_empty(nd_pos))
     {
       a = 1; b = 0; p = 1; q = -1; 
       
       if (!(tr = ndd_create_transf(1, 1, &a, &b, &p, &q)))
	 diag__fail(lash_errno, -1);
       
       if (!(nd_neg = ndd_image_by_transf(nd, tr)))
	 {
	   ndd_free(nd_pos);
	   ndd_transf_free(tr);
	   diag__fail(lash_errno, -1);
	 }
       
       if (ndd_transf_free(tr) < 0)
	 {
	   ndd_free(nd_pos); 
	   ndd_free(nd_neg);
	   diag__fail(lash_errno, -1);
	 }
       
       if (auto_min_path_lex(nd_neg -> automaton, &trs, &l, 
			     &empty) < 0)
	 {
	   ndd_free(nd_pos); 
	   ndd_free(nd_neg);
	   diag__fail(lash_errno, -1);
	 }   

       if  (convert_path_to_sint(trs,  l, nd -> base, val) < 0)
	 {
	   ndd_free(nd_pos); 
	   ndd_free(nd_neg);
	   resr__free_objects(trs, tran, l);
	   diag__fail(lash_errno, -1);
	 }
       
       ndd_free(nd_neg);
     }
   else
     {
       if (ndd_remove_sign_loop(nd_pos, &au) < 0)
	 {
	   ndd_free(nd_pos); 
	   diag__fail(lash_errno, -1);
	 }
       if (!au)
	 {
	   ndd_free(nd_pos);
	   diag__fail(LASH_ERR_CORRUPT, -1);
	 }
       
       if (auto_max_path_rev_lex(au, &trs, &l, &cyclic, &empty) < 0)
	 {
	   ndd_free(nd_pos);  
	   auto_free(au);
	   diag__fail(lash_errno, -1);
	 }   
       if (cyclic || empty)
	 {
	   ndd_free(nd_pos); 
	   auto_free(au);
	   diag__fail(LASH_ERR_CORRUPT, -1);
	 }   

       auto_free(au);
 
       if (convert_path_to_sint(trs,  l, nd -> base, val) < 0)
	 {
	   ndd_free(nd_pos); 
	   resr__free_objects(trs, tran, l);
	   diag__fail(lash_errno, -1);
	 }
     }
   
   resr__free_objects(trs, tran, l);
   ndd_free(nd_pos); 
   diag__return(0);
}

/** int get_lower_bound(nd, bounded, val) : Computes the lower bound
                    in the element values of the set represented by
                    the NDD *nd.
		 
		    If such a bound does not exists, then, it sets
		    *bounded to 0, otherwise, *bounded is set to 1,
		    and the bound is stored in val.
			      
		    The NDD *nd must be of dimension 1.

		    This function does not modify nd.
			      
		    Returns 0 in the case of success and -1 in the
		    case of failure and sets lash_errno.
 
                    Possible error codes:
                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.     
			 LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_BAD_VALUE  : Bad argument value(s).
                                                                   **/
static  int get_lower_bound(nd, bounded, val) 
     ndd   *nd;
     int   *bounded;
     sint4 *val;
{
           sint4          a, b, p, q;
  register linear_transf *tr;
  register ndd           *nd_pos, *nd_neg;
  register biguint       *nb_sol;
           automaton     *au;
           tran          *trs;
           int            empty, cyclic;
           uint4          l;

  diag__enter("get_lower_bound", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || (nd -> dim != 1) || !val || !bounded)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif 

  if (ndd_is_empty(nd))
    diag__fail(LASH_ERR_BAD_VALUE, -1);
      
  a = 1; b = 0; p = 1; q = -1; 
   
  if (!(tr = ndd_create_transf(1, 1, &a, &b, &p, &q)))
    diag__fail(lash_errno, -1);
  
  if (!(nd_neg = ndd_image_by_transf(nd, tr)))
    {
      ndd_transf_free(tr);
      diag__fail(lash_errno, -1);
    }
  
  if ( ndd_transf_free(tr) < 0)
    {
      ndd_free(nd_neg);
      diag__fail(lash_errno, -1);
    }
  
  if (!(nb_sol = biguint__new_zero())) 
    {
      ndd_free(nd_neg);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  if (ndd_count(nd_neg, nb_sol) < 0)
    {
      ndd_free(nd_neg);
      diag__fail(lash_errno, -1);
    }
  
  if (biguint__is_inf(nb_sol))
    {
      ndd_free(nd_neg);
      biguint__free(nb_sol);

      *bounded = 0; 
      diag__return(0) ;
    }
  else
    *bounded = 1;
  
  biguint__free(nb_sol);

  if (ndd_is_empty(nd_neg))
    { 
      a = 1; b = 0; p = -1; q = 0; 
      
      if (!(tr = ndd_create_transf(1, 1, &a, &b, &p, &q)))
	{
	  ndd_free(nd_neg);
	  diag__fail(lash_errno, -1);
	}
      
      if (!(nd_pos = ndd_image_by_transf(nd, tr)))
	{
	  ndd_free(nd_neg);
	  ndd_transf_free(tr);
	  diag__fail(lash_errno, -1);
	}
      
      if ( ndd_transf_free(tr) < 0)
	{
	  ndd_free(nd_neg);   ndd_free(nd_pos);
	  diag__fail(lash_errno, -1);
	}     
      
      if (auto_min_path_lex(nd_pos -> automaton, &trs, 
			    &l, &empty) < 0)
	{
	  ndd_free(nd_pos); ndd_free(nd_neg);
	  diag__fail(lash_errno, -1);
	}

      if  (convert_path_to_sint(trs,  l, nd -> base, val) < 0)
	{
	  ndd_free(nd_pos); ndd_free(nd_neg);
	  resr__free_objects(trs, tran, l);
	  diag__fail(lash_errno, -1);
	}

      ndd_free(nd_pos);      
    }
  else
    {
      if (ndd_remove_sign_loop(nd_neg, &au) < 0)
	{
	  ndd_free(nd_neg);
	  diag__fail(lash_errno, -1);
	}
      if (!au)
	{
	  ndd_free(nd_neg);
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}
      
      if (auto_max_path_rev_lex(au, &trs, &l, &cyclic, &empty) < 0)
	{
	  ndd_free(nd_neg); auto_free(au);
	  diag__fail(lash_errno, -1);
	}   
      if (cyclic || empty)
	{
	  ndd_free(nd_neg); auto_free(au);
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}   
      auto_free(au); 
      if  (convert_path_to_sint(trs,  l, nd -> base, val) < 0)
	{
	  ndd_free(nd_neg);
	  resr__free_objects(trs, tran, l);
	  diag__fail(lash_errno, -1);
	}
    }
  
  resr__free_objects(trs, tran, l);
  ndd_free(nd_neg);
  diag__return(0);
}

/** int convert_path_to_sint(trs, l, val, base) : Converts the 
                      sequence of l transitions *trs into a sint4 and
                      stores the computed value in *val.  The
                      transitions must have a label of length 1 (1
                      digit).  base is the base and the sequence of
                      corresponding digits is interpreted as MSDF.
                      Return 0 in the case of success and -1 in the
                      case of failure.

		      Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_VALUE  : Bad value of parameter.
                         LASH_ERR_OVERFLOW   : Overflow.           **/

static int convert_path_to_sint(trs, l, base, val)
     tran *trs;
     uint4 l;
     uint1 base;
     sint4 *val;
{
  register uint4 i;
  register uint1 label, init ;
           sint4 v;

  diag__enter("convert_path_to_sint", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!val)
    diag__fail(LASH_ERR_BAD_VALUE, -1);
#endif
  
  for (i = 0, init = 1 ; i < l ; i++)
    { 
      label = *(uint1 *) auto_transition_label_ptr(trs + i, 1);
      
      if (init == 1 )
	{ 
	  if (label == base - 1 )
	    v = -1;
	  else
	    v = 0;
	}
      else
	{
	  if ((sint4__mult(&v, v, (sint4) base) < 0 )
	      || (sint4__add(&v, v, (sint4) label) < 0))
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	}
      init = 0;
    }
  
  *val = v;
  diag__return(0);
}

/**  Public Functions.                                             **/

/* ndd_remove_sign_loop(nd, a) : This function is required to
                    eliminate the multiple encodings of the vectors in
                    order to count them properly.
                             
                    Its main purpose is therefore to remove the sign
                    loop. In the case of success, the function returns
                    0 and, if the number of solutions described by the
                    NDD nd is finite, the automata where the sign
                    loops are removed (which therefore is acyclic) is
                    stored in *a. If there are an infinite number of
                    solutions (and therefore a loop different than a
                    pure sign loop ), *a is set to NULL.  In the case
                    of failure, the function returns -1.

		    The NDD nd must be serial.

                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_PROP   : Bad property of NDD.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.     
			 LASH_ERR_BAD_VALUE  : Bad argument(s).    **/

int ndd_remove_sign_loop(ndd *nd, automaton **a)
{
           uint4              nb_trans, null_state;
           stack             *st;
           hash_table        *ht;
           s_count_prep_info *sel;
  register h_count_prep_info *pay, *prev_pay;
           uint1             flag_loop;

  diag__enter("ndd_remove_sign_loop", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !a)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif

  if (remove_sign_loop_init(nd, a, &st , &ht, &null_state) < 0)
    diag__fail(lash_errno, -1) ;
  
  if (auto_nb_states(*a) == 0)
    diag__return(0);
  
  flag_loop = 0;
  
  while (!stack__is_empty(st))
    {
      if (!(sel = (s_count_prep_info *) stack__top(st)))
	{
	  free_count_structures(st,ht,sizeof(h_count_prep_info));
	  auto_free(*a); 
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}
      
      if ((sel -> nb_trans_ex == ZERO_INT4) 
	  && (register_state(sel -> s , ht) < 0))
	{
	  free_count_structures(st,ht,sizeof(h_count_prep_info));
	  auto_free(*a); 
	  diag__fail(lash_errno, -1);
	}
      
      if (auto_nb_out_transitions(*a, sel -> s, &nb_trans ) < 0)
	{
	  free_count_structures(st,ht,sizeof(h_count_prep_info));
	  auto_free(*a); 
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}
      
      if (sel -> nb_trans_ex < nb_trans)
	{ 
	  if (fetch_next_state(sel, st, ht, *a, &flag_loop, 
			       null_state) < 0)
	    {
	      free_count_structures(st,ht,sizeof(h_count_prep_info)) ;
	      auto_free(*a); 
	      diag__fail(lash_errno, -1);
	    }
	  
	  if (flag_loop == 1)
	    break;
	}
      
      else  /* no more transition to be explored */
	{
	  if (!(pay = (h_count_prep_info *) 
		hash__lookup_bytes(ht, 
				   (uint1 *) &sel -> s, 
				   sizeof(uint4)))) 
	    {
	      free_count_structures(st,ht,sizeof(h_count_prep_info)) ;
	      auto_free(*a); 
	      diag__fail(LASH_ERR_CORRUPT, -1);
	    }
	  
	  if ((sel -> init != 1) 
	      && (pay -> leads_to_cycle == 1))
	    {
	      if (!(prev_pay = (h_count_prep_info *) 
		    hash__lookup_bytes(ht, (uint1 *) &sel -> prev_s, 
				       sizeof(uint4))))
		{
		  free_count_structures(st, ht, 
					sizeof(h_count_prep_info)) ;
		  auto_free(*a); 
		  diag__fail(LASH_ERR_CORRUPT, -1);
		}
	      prev_pay -> leads_to_cycle = 1;
	    }
	  
    	  pay -> in_stack = 0; 
	  stack__pop(st, NULL);	  
	}
    }  
    
  free_count_structures(st,ht,sizeof(h_count_prep_info));
  
  if (flag_loop == 1)
    { 
      if (auto_free(*a) < 0)
	diag__fail(lash_errno, -1);
      *a = NULL;
    }
  
  if ((*a) && (!(nd -> properties & NDD_PROP_MSDF)))
    {
      automaton *a1;
      if (!(a1 =auto_reverse(*a)))
	diag__fail(lash_errno, -1);
      auto_free(*a);
      *a = NULL;
      if (auto_determinize(a1) < 0)
	return -1;
      *a = a1;
    }
  
  return 0;
}

/* int ndd_count(nd, nb) : computes the number of elements of the set
		   represented by the NDD nd and store this number in
		   *nb (which must hold an already allocated biguint
		   object).

		   The NDD nd must be serial, otherwise, an error is
		   generated.

		   Returns 0 in the case of success, and -1 in the
		   case of failure.

                   Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_PROP   : Bad property of NDD.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.        **/

int  ndd_count(nd, nb)
     biguint *nb;
     ndd *nd;
{
  automaton *a;

  diag__enter("ndd_count", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nb || !nd)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif
  
  if (ndd_remove_sign_loop(nd, &a) < 0)
    diag__fail(lash_errno, -1);
  
  if (a)
    {
      if (auto_count_accept(nb, a) < 0)
	{
	  auto_free(a);
	  diag__fail(lash_errno, -1);
	}
      
      if (auto_free(a) < 0)
	diag__fail(lash_errno, -1);
    }
  
  else 
    biguint__setinf(nb) ;
  
  diag__return(0);  
}

/* ndd_min(nd, v, val, flag_inf, flag_empty ) : Computes the minimum
                     value of the v-th component for the elements in
                     the set represented by the ndd nd and stores that
                     value in val If nd is empty, then it sets
                     flag_empty to 1 and to 0 otherwise.  If the v-th
                     component has no lower bound then sets flag_inf
                     to 1, and to 0 otherwise.  Returns 0 and stores
                     the result in val in the case of success and -1
                     in the case of failure.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_PROP   : Bad property of ndd.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.  
			 LASH_ERR_BAD_VALUE  : Bad argument value(s).
                         LASH_ERR_OVERFLOW   : Arithmetic overflow. 
                                                                   **/

int ndd_min(nd, v, val, flag_inf, flag_empty)
     ndd *nd;
     uint4 v;
     sint4 *val;
     int   *flag_inf, *flag_empty;
{
  register ndd *nd_v;
           int  bounded, is_empty;

  diag__enter("ndd_min", -1);

#if LASH_CHECK_LEVEL >= 1
  if ((!nd) || (!val) || (!flag_inf) || (!flag_empty))
   {
     fprintf(stderr, "ndd_min : args") ;
     diag__fail(LASH_ERR_BAD_VALUE, -1);
   }
#endif 
     
  is_empty =  ndd_is_empty(nd)  ;
  if (is_empty == -1)
    diag__fail(lash_errno, -1) ;
  if (is_empty == 1)
    {
      *flag_empty = 1 ;
      *flag_inf = 0 ;
      diag__return(0) ;
    }
  else
    *flag_empty = 0 ;

  if (!(nd_v = project_over_y(nd, v)))
    diag__fail(lash_errno, -1);

  if (get_lower_bound(nd_v, &bounded, val) < 0)
    {
      ndd_free(nd_v);
      diag__fail(lash_errno, -1);
    }

  if (bounded == 0)
    *flag_inf = 1 ;
  else
    *flag_inf = 0 ;
 
  ndd_free(nd_v);

  diag__return(0);
}

/* ndd_max(nd, v, val, flag_inf, flag_empty ): Compute the maximum
                     value of the  v-th component for the elements
                     in the set represented by the ndd
		     nd and stores that value in val
		     If nd is empty, then it set flag_empty to 1 and
		     to 0 otherwise.
		     If the v-th component has no upper bound then
		     sets flag_inf to 1, and to 0 otherwise.
		     Returns 0 and stores the result in val in the
		     case of success and -1 in the case of failure.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_PROP   : Bad property of ndd.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt ndd.  
			 LASH_ERR_BAD_VALUE  : Bad argument value(s).
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.

   **/

int ndd_max(nd, v, val, flag_inf, flag_empty)
     ndd  *nd;
     uint4 v;
     sint4 *val;
     int   *flag_inf, *flag_empty;
{
  register ndd *nd_v;
           int  bounded;
  register int  is_empty;

  diag__enter("ndd_max", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !val || !flag_inf || !flag_empty)
    {
      diag__fail(LASH_ERR_BAD_VALUE, -1);
    }
#endif 

  is_empty = ndd_is_empty(nd);
  if (is_empty < 0)
    diag__fail(lash_errno, -1);

  if (is_empty)
    {
      *flag_empty = 1;
      *flag_inf = 0;
      diag__return(0);
    }
  else
    *flag_empty = 0;
  
  if (!(nd_v = project_over_y(nd, v)))
    diag__fail(lash_errno, -1);
  
  if (get_upper_bound(nd_v, &bounded, val) < 0)
    {
      ndd_free(nd_v);
      diag__fail(lash_errno, -1);
    }
  
  if (!bounded)
    *flag_inf = 1;
  else
    *flag_inf = 0;
  
  ndd_free(nd_v);
  
  diag__return(0);
}

/****  End of ndd-count.c  ****/
