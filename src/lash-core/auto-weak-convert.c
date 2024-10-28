/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-weak-convert.c  :  Conversion of Buchi and co-Buchi    **/
/**                     automata under weak form. (SJ)             **/
/**                                                                **/
/**        02/05/01  :  Creation. (SJ)                             **/
/**        02/27/01  :  Reorganization. (SJ)                       **/
/**        06/28/02  :  Moved to the LASH core directory. (SJ)     **/
/**        07/08/02  :  Reorganization. (BB)                       **/
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

#include "auto-weak-convert.h"
#include "datastruct.h"
#include "diag.h"
#include "graph-scc.h"

/****  Prototypes of private functions.                          ****/

static int scc_convert_proc(automaton *, stack *, uint4, uint4 *,
			     uint1, uint1, void *);

/****  Private functions.                                        ****/

/**  int  scc_convert_proc(a,st,root,scc_num,accept,transient,arg) :
                     Function invoked by the strongly connected
		     components detection algorithm. It converts the
		     s.c.c. whose root is root in the automaton *a
		     into a s.c.c. whose all states are either final 
		     or non final. The states of the s.c.c. are on the
		     top of the stack *st, upon the state index root.
		     The flag accept is set if and only if the s.c.c.
		     contains at least one accepting state. Arguments
		     transient and arg are not used.

		     The applied rule is the following. Buchi automata
		     are supposed to be inherently weak [BJW01].
		     Thus, if one state is final, then all the states
		     of the s.c.c. are marked as final. If none of
		     the states is final, then no change is made to
		     the s.c.c. Co-Buchi automata are supposed to
		     have no s.c.c. with both an accepting and a non
		     accepting cycles. Thus, if none of the states of
		     the s.c.c. is final, then all the cycles in the
		     s.c.c. are co-Buchi accepting, so all the states
		     of the s.c.c. are marked as accepting. 
		     
		     Conversely, if there is at least one final state,
		     the s.c.c.  is marked as non accepting.

		     This function cannot incur an error. Its
		     return value will always be 0.                **/

static int scc_convert_proc(a, st, root, scc_num, accept, transient, 
			    arg)
     automaton   *a;
     stack       *st;
     uint4        root;
     uint4       *scc_num;
     uint1        accept, transient;
     void        *arg;
{
  register uint4   w;

  do
    {
      w = *((uint4 *) stack__top(st));
      stack__pop(st,NULL);

      switch (auto_accept_type(a))
	{
	case AUTO_ACCEPT_BUCHI :
	  if (accept)
	    auto_mark_accepting_state(a, w);
	  break;

	case AUTO_ACCEPT_COBUCHI :
	  if (accept)
	    auto_unmark_accepting_state(a, w);
	  else
	    auto_mark_accepting_state(a, w);
	  break;

#if LASH_CHECK_LEVEL >= 2
	default:
	  return -1;	    
#endif
	}
    }
  while (w!=root);

  return 0;
}

/****  Public visible functions.                                 ****/

/**  int  auto_convert_to_weak(a) : This function converts an
		     inherently weak Buchi automaton or a co-Buchi
		     automaton which has no s.c.c. with both an
		     accepting and a non accepting cycle, into an
		     equivalent weak automaton. The automaton that
		     is to be converted is *a. This function
		     modifies *a. It uses the s.c.c. detection
		     algorithm.

		     Please note that no check is performed to verify
		     whether the automaton *a is a suitable Buchi or
		     co-Buchi automaton. If *a is already known to
		     be a weak automaton, the function does not
		     perform any action.

		     If an error occurs, the automaton *a still
		     accepts the same language, but some of its
		     s.c.c. may have changed with respect to its
		     previous accepting status. If this happens,
		     the function sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Too many states.    **/

int  auto_convert_to_weak(a)
     automaton   *a;
{
  uint4   count;

  diag__enter("buchi__to_weak",-1);

 if (auto_word_type(a) != AUTO_WORDS_INFINITE ||
     (auto_accept_type(a) != AUTO_ACCEPT_BUCHI &&
      auto_accept_type(a) != AUTO_ACCEPT_WEAK &&
      auto_accept_type(a) != AUTO_ACCEPT_COBUCHI))
   diag__fail(LASH_ERR_BAD_TYPE, -1);      

 if (auto_accept_type(a) == AUTO_ACCEPT_WEAK)
   diag__return(0);

  if (scc_algorithm(a, NULL, &count, 0, scc_convert_proc, NULL) < 0)
    diag__fail(lash_errno, -1);      

  auto_accept_type(a) = AUTO_ACCEPT_WEAK;
  diag__return(0);
}

/****  End of auto-weak-convert.c  ****/
