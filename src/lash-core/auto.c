/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       auto.c  :  Manipulation of finite-state automata.        **/
/**                                                                **/
/**     03/13/98  :  Creation. (BB)                                **/
/**     06/11/98  :  Additional functions and minor corrections.   **/
/**                  (BB)                                          **/
/**     08/26/98  :  Minor modifications. (BB)                     **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/14/98  :  Minor corrections. (BB)                       **/
/**     09/18/98  :  New function. (BB)                            **/
/**     09/22/98  :  Minor corrections. (BB)                       **/
/**     01/25/99  :  Minor correction. (BB)                        **/
/**     03/10/99  :  New function 'auto_state'. (BB+GC)            **/
/**     08/12/99  :  Improved sorting function. (BB)               **/
/**     11/03/01  :  New function 'auto_remove_trans'. (LL)        **/
/**     05/31/02  :  Minor corrections. (LL)                       **/
/**     07/11/02  :  Reorganization. (BB)                          **/
/**     05/27/03  :  Minor correction. (LL)                        **/
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

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "auto.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "sort.h"

/****  Global variable.                                          ****/

/**  uint1  trans_cmp_alphabet_nbytes  :  Used for storing the number
                     of symbols required for one symbol of the
                     alphabet before calling auto_transition_cmp. The
                     use of side effects is unavoidable since the
                     function 'qsort' of the C library does not allow
                     to add additional parameters to the comparison
                     function being used.                          **/

static uint1  trans_cmp_alphabet_nbytes;

/****  Prototypes of private functions.                          ****/

static void  free_states(automaton *);
static void  free_trans(state *, uint1);
static int   tran_cpy(tran *, tran *, uint1);
static int   state_cpy(state *, state *, uint1, uint1);

/****  Private functions.                                        ****/

/**  void  free_states(a)  :  Frees the list of states of the
                     automaton a.                                  **/

static void  free_states(a)
  automaton *a;
{
  register uint4  i, n;
  register uint1  na, nacc;
  register state *s;

  n    = a -> nb_states;
  na   = a -> alphabet_nbytes;
  nacc = a -> accept_nbytes;

  for (i = 0, s = a -> states; i < n; i++, s++)
    {
      free_trans(s, na);
      resr__free_info_objects(&s -> accepting, uint1, nacc);
    }
    
  if (n || a -> states)
    resr__free_objects(a -> states, state, n);

  a -> states = NULL;
}

/**  void  free_trans(s, n)  :  Frees the list of transitions of
                     the state to which s points. The number of
                     bytes allocated for each symbol of the
                     alphabet is n.                                **/

static void  free_trans(s, n)
  state *s;
  uint1  n;
{
  register uint4 i, nb;
  register tran *t;

  nb = s -> nb_trans;

  for (i = 0, t = s -> trans; i < nb; i++, t++)
    resr__free_info_objects(&t -> label,
			    uint1, (t -> nb_symbols) * n);
      
  if (nb || s -> trans)
    resr__free_objects(s -> trans, tran, nb);
  
  s -> trans = NULL;
}

/**  int  tran_cpy(td, ts, alph_nbytes)  :  This routine is part of
                     the copy algorithm for finite-state automata. It
                     copies the transition *ts to the transition
                     *td. The parameter alph_nbytes specifies the
                     number of bytes required for one symbol of the
                     alphabet.  Returns -1 in the case of an error,
                     and 0 otherwise.                              **/

static int  tran_cpy(td, ts, alph_nbytes)
  tran  *td, *ts;
  uint1  alph_nbytes;
{
  register uint4 n;

#if LASH_CHECK_LEVEL >= 1
  if (!td || !ts)
    return -1;
#endif

  td -> end_state  = ts -> end_state;
  td -> nb_symbols = ts -> nb_symbols;

  n = (ts -> nb_symbols) * alph_nbytes;

  if (!resr__new_info_objects(&td -> label, uint1, n))
    return -1;

  if (info__cpy(&td -> label, &ts -> label, n) < 0)
    {
      resr__free_info_objects(&td -> label, uint1, n);
      return -1;
    }

 return 0;
} 

/**  int  state_cpy(sd, ss, alph_nbytes, acc_nbytes)  :  This routine
                     is part of the copy algorithm for finite-state
                     automata. It copies the state *ss to the state
                     *sd, as well as all the outgoing transitions
                     from that state. The parameters alph_nbytes
                     and acc_nbytes specify the number of bytes
                     required for one symbol and one accepting 
                     status. Returns -1 in the case of an error, and
                     0 otherwise.                                  **/

static int  state_cpy(sd, ss, alph_nbytes, acc_nbytes)
  state *sd, *ss;
  uint1  alph_nbytes, acc_nbytes;
{
  register uint4  i, j;
  register tran  *t, *ts;

#if LASH_CHECK_LEVEL >= 1
  if (!sd || !ss)
    return -1;
#endif

  sd -> nb_trans = ZERO_INT4;
  if (info__cpy(&sd -> accepting, &ss -> accepting, acc_nbytes) < 0)
    return -1;

  sd -> nb_trans  = ss -> nb_trans;
  if (sd -> nb_trans  == 0)
      sd -> trans = NULL;
  else
    {
      t = resr__new_objects(tran, sd -> nb_trans);
      if (!t)
	{
	  resr__free_info_objects(&sd -> accepting, uint1, acc_nbytes);
	  return -1;
	}

      sd -> trans = t;

      for (i = 0, ts = ss -> trans; i < sd -> nb_trans ; i++, t++, ts++)
	if (tran_cpy(t, ts, alph_nbytes) < 0)
	  {
	    for (j = 0, t = sd -> trans; j < i; j++, t++)
	      resr__free_info_objects(&t -> label, uint1,
				      (t -> nb_symbols) * alph_nbytes);
	    resr__free_info_objects(&sd -> accepting, uint1, acc_nbytes);
	    resr__free_objects(sd -> trans, tran, sd -> nb_trans);
	    
	    sd -> trans = NULL;
	    sd -> nb_trans = ZERO_INT4;
	    
	    return -1;
	  }
    }
  return 0;
}

/****  Public visible functions.                                 ****/

/**  automaton *auto_new_empty(n)  :  Creates a new finite-state
                     automaton on finite words that accepts the
                     empty language. The argument n specifies the
                     number of bytes that are needed for storing
                     one symbol of the alphabet.    

                     If an automaton is created, the function
                     returns a pointer to this automaton. Otherwise,
                     it returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : Not enough memory.    **/

automaton *auto_new_empty(n)
  uint1  n;
{
  register automaton *a;

  diag__enter("auto_new_empty", NULL);

  a = resr__new_object(automaton);
  if (!a)
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  a -> word_type        = AUTO_WORDS_FINITE;
  a -> accept_type      = 0;
  a -> properties       = AUTO_PROP_DETERM | AUTO_PROP_MINIMAL |
                          AUTO_PROP_STRONG | AUTO_PROP_NORMAL;
  a -> alphabet_nbytes  = n;
  a -> accept_nbytes    = 1;
  a -> nb_i_states      = ZERO_INT4;
  a -> i_states         = NULL;
  a -> nb_states        = ZERO_INT4;
  a -> states           = NULL;

  diag__return(a);
}

/**  int  auto_free(a)  :  Unallocates the finite-state automaton 
                     given by *a.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  auto_free(a)
  automaton *a;
{
  diag__enter("auto_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (a -> nb_i_states || a -> i_states)
    resr__free_objects(a -> i_states, uint4, a -> nb_i_states);

  free_states(a);
  resr__free_object(a, automaton);

  diag__return(0);
}

/**  int  auto_add_new_state(a, p)  :  Adds a new state to the 
                     automaton *a, and places the index of that state
                     at the memory location specified by p. The state 
                     is created without any outgoing transition, and
                     its accepting information is filled with zeroes.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         LASH_ERR_NO_MEM   : Not enough memory.    **/

int  auto_add_new_state(a, p)
  automaton *a;
  uint4     *p;
{
  register state *s;
  register uint1 *u;

  diag__enter("auto_add_new_state", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  s = resr__resize_objects(a -> states, state,
      a -> nb_states + 1, a -> nb_states);

  if (!s)
    diag__fail(LASH_ERR_NO_MEM, -1); 

  a -> states = s;
  s += a -> nb_states;

  u = resr__new_info_objects(&s -> accepting, uint1,
      a -> accept_nbytes);

  if (!u)
    {
      a -> states = resr__resize_objects(a -> states, state,
          a -> nb_states, a -> nb_states + 1);

      diag__fail(LASH_ERR_NO_MEM, -1);
    }
 
  memset(u, 0, a -> accept_nbytes);

  s -> nb_trans = ZERO_INT4;
  s -> trans    = NULL;

  auto_reset_property(a, AUTO_PROP_MINIMAL);
  
  if (p)
    *p = a -> nb_states;

  a -> nb_states++;

  diag__return(0);
}

/**  int  auto_add_new_transition(a, p, q, n, l)  :  Adds a new
                     transition to the automaton *a. The parameters
                     p and q specify the origin and the end of
                     the transition (in terms of state indices),
                     n is the number of symbols of the label, and
                     l is a pointer to an array of bytes 
                     containing the successive symbols of the
                     label (each symbol being represented by 
                     (a -> alphabet_nbytes) bytes in the array).

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int  auto_add_new_transition(a, p, q, n, l)
  automaton *a;
  uint4      p, q, n;
  uint1     *l;
{
  register state *s;
  register tran  *t;
  register uint1 *u;

  diag__enter("auto_add_new_transition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (p >= a -> nb_states || q >= a -> nb_states)
    diag__fail(LASH_ERR_BAD_STATE, -1);
#endif  /* >= 1 */

  s = a -> states + p;
  
  t = resr__resize_objects(s -> trans, tran,
      s -> nb_trans + 1, s -> nb_trans);

  if (!t)
    diag__fail(LASH_ERR_NO_MEM, -1); 
  
  s -> trans = t;
  t += s -> nb_trans;

  if (n > 0)
    {
      u = resr__new_info_objects(&t -> label, uint1, 
				 a -> alphabet_nbytes * n);
      
      if (!u)
	{
	  s -> trans = resr__resize_objects(s -> trans, tran,
					    s -> nb_trans, s -> nb_trans + 1);
	  
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      
      memcpy(u, l, a -> alphabet_nbytes * n);
    }

  t -> end_state  = q;
  t -> nb_symbols = n;

  auto_reset_property(a, AUTO_PROP_DETERM);
  auto_reset_property(a, AUTO_PROP_MINIMAL);

  if (n > 1)
    {
      auto_reset_property(a, AUTO_PROP_STRONG);
      auto_reset_property(a, AUTO_PROP_NORMAL);
    }
  else
    if (!n)
      auto_reset_property(a, AUTO_PROP_STRONG);

  s -> nb_trans++;

  diag__return(0);
}

/**  int  auto_add_new_i_state(a, p)  :  Adds a new initial state, 
                     specified by the index p, to the automaton given
                     by *a.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int  auto_add_new_i_state(a, p)
  automaton *a;
  uint4      p;
{
  register uint4 *u;

  diag__enter("auto_add_new_i_state", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (p >= a -> nb_states)
    diag__fail(LASH_ERR_BAD_STATE, -1); 
#endif  /* >= 1 */

  u = resr__resize_objects(a -> i_states, uint4,
      a -> nb_i_states + 1, a -> nb_i_states);  

  if (!u)
    diag__fail(LASH_ERR_NO_MEM, -1); 

  if (a -> nb_i_states)
    auto_reset_property(a, AUTO_PROP_DETERM);

  auto_reset_property(a, AUTO_PROP_MINIMAL);

  a -> i_states = u;
  u[a -> nb_i_states++] = p;

  diag__return(0);
}

/**  int  auto_nb_out_transitions(a, m, p)  :  Gets the number of
                     transitions outgoing from the state number m
                     of the automaton *a and copies that number to
                     the location specified by the pointer p.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int  auto_nb_out_transitions(a, m, p)
  automaton *a;
  uint4      m;
  uint4     *p;
{
  diag__enter("auto_nb_out_transitions", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (m >= a -> nb_states)
    diag__fail(LASH_ERR_BAD_STATE, -1); 
#endif  /* >= 1 */

  if (p)
    *p = a -> states[m].nb_trans;

  diag__return(0); 
}

/**  tran *auto_transition(a, m, n)  :  Returns a pointer to the 
                     transition number n of the state number m 
                     (transition and state numbers start at 0) of the
                     automaton *a. In the case of an error, returns a
                     NULL pointer. This function does not allocate
                     memory and returns a pointer to data that may
                     subsequently change.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_BAD_TRAN   : No such transition. **/

tran *auto_transition(a, m, n)
  automaton *a;
  uint4      m, n;
{
  diag__enter("auto_transition", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (m >= a -> nb_states)
    diag__fail(LASH_ERR_BAD_STATE, NULL); 

  if (n >= a -> states[m].nb_trans)
    diag__fail(LASH_ERR_BAD_TRAN, NULL); 
#endif  /* >= 1 */

  diag__return(a -> states[m].trans + n); 
}

/**  int  auto_state(a, m, p)  :  Gets the number of the m-th state of
                     the automaton a (m = 0, 1, ...). The resulting
                     number is copied to the location pointed to by p.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int  auto_state(a, m, p)
  automaton *a;
  uint4      m;
  uint4     *p;
{
  diag__enter("auto_state", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (m >= a -> nb_states)
    diag__fail(LASH_ERR_BAD_STATE, -1); 
#endif  /* >= 1 */

  if (p)
    *p = m;

  diag__return(0); 
}

/**  int  auto_i_state(a, m, p)  :  Gets the number of the m-th 
                     initial state of the automaton a (m = 0, 1,
                     ...). The resulting number is copied to the
                     location pointed to by p.

                     If successful, returns 0. In the case of error,
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

int  auto_i_state(a, m, p)
  automaton *a;
  uint4      m;
  uint4     *p;
{
  diag__enter("auto_i_state", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);

  if (m >= a -> nb_i_states)
    diag__fail(LASH_ERR_BAD_STATE, -1); 
#endif  /* >= 1 */

  if (p)
    *p = a -> i_states[m];

  diag__return(0); 
}

/**  uint1 *auto_resize_transition_label(a, t, p, q)  :  Resizes the
                     label of the transition *t of the automaton *a to
                     be p symbols (previous size was q symbols).  The
                     min(p, q) first symbols of the label are
                     preserved by this operation.

                     In the case of an error, returns a NULL 
                     pointer. If successful, returns a pointer to
                     the new label content.
  
		     This function is for internal use of the LASH
                     package, and should not be called by external
		     applications!

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

uint1 *auto_resize_transition_label(a, t, p, q)
  automaton *a;
  tran      *t;
  uint4      p, q;
{
  register uint1  n;

  n = a -> alphabet_nbytes;

  diag__enter("auto_resize_transition_label", NULL);

  if (!resr__resize_info_objects(&t -> label, uint1,
      p * n, q * n))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  t -> nb_symbols = p;

  if (p < q)
    auto_reset_property(a, AUTO_PROP_DETERM);

  if (p != q)
    auto_reset_property(a, AUTO_PROP_MINIMAL);
  
  if (p > 1)
    {
      auto_reset_property(a, AUTO_PROP_STRONG);
      auto_reset_property(a, AUTO_PROP_NORMAL);
    }
  else
    if (!p)
      auto_reset_property(a, AUTO_PROP_STRONG);

  diag__return(auto_transition_label_ptr(t, n));
}

/**  void  auto_transition_cmp_prepare(a)  :  Assigns to the global
                     variable trans_cmp_alphabet_nbytes the number
		     of bytes required to store one symbol of the
		     alphabet of the automaton *a. This function must
		     be called before using auto_transition_free,
                     auto_transition_cmp, or auto_transition_full_cmp.
                                                                   **/

void  auto_transition_cmp_prepare(a)
  automaton *a;
{
  trans_cmp_alphabet_nbytes = a -> alphabet_nbytes;
}

/**  void  auto_transition_free(t)  :  Frees the label of the
                     transition *t. The number of bytes required to
		     store one symbol of the alphabet is taken from
		     the value of the global variable
		     trans_cmp_alphabet_nbytes, which can be set by
		     calling the function auto_transition_cmp_prepare.
		                                                   **/
void  auto_transition_free(t)
  tran *t;
{
  resr__free_info_objects(&t -> label, uint1, (t -> nb_symbols) *
      trans_cmp_alphabet_nbytes);
}

/**  int  auto_transition_cmp(t1, t2)  :  Compares the two transitions
                     *t1 and *t2 of the same automaton. Returns 0 if
                     the labels of the two transition are equal, a
                     negative value if the label of *t1 is
                     lexicographically less than the label of *t2, and
                     a positive value otherwise.

                     It should be noted that the lexicographical order
                     between labels is defined bytewise, regardless of
                     the number of bytes needed for storing each
                     symbol. The values of the bytes are considered to
                     be unsigned.

                     Warning: This function relies on the value of
                     the global variable trans_cmp_alphabet_nbytes.
                     The value of this variable must be appropriately
                     set by a call to auto_transition_cmp_prepare 
                     before calling auto_transition_cmp.           **/

int  auto_transition_cmp(t1, t2)
  tran *t1, *t2;
{
  register uint4  l1, l2, n;
  register int    r;  

  l1 = t1 -> nb_symbols;
  l2 = t2 -> nb_symbols;
  
  n  = trans_cmp_alphabet_nbytes;
  
  if (l1 == l2)
    return memcmp(auto_transition_label_ptr(t1, n),
        auto_transition_label_ptr(t2, n), n * l1);

  if (l1 < l2)
    {
      r = memcmp(auto_transition_label_ptr(t1, n),
          auto_transition_label_ptr(t2, n), n * l1);
      if (!r)      
        return -1;
      return r;
    }

  r = memcmp(auto_transition_label_ptr(t1, n),
      auto_transition_label_ptr(t2, n), n * l2);

  if (!r)      
    return 1;
  return r;
}

/**  int  auto_transition_full_cmp(t1, t2)  :  Compares the two
                     transitions *t1 and *t2 of the same automaton.
                     Returns 0 if the two transitions have the same 
                     label and the same destination state, a negative
                     value if the label of *t1 is lexicographically
                     less than the label of *t2, or if the two labels
                     are equal and the destination of *t1 is less than
                     the destination of *t2, and a positive value
                     otherwise.

                     It should be noted that the lexicographical order
                     between labels is defined bytewise, regardless of
                     the number of bytes needed for storing each
                     symbol. The values of the bytes are considered to
                     be unsigned.

                     Warning: This function relies on the value of
                     the global variable trans_cmp_alphabet_nbytes.
                     The value of this variable must be appropriately
                     set by a call to auto_transition_cmp_prepare 
                     before calling auto_transition_full_cmp.      **/

int  auto_transition_full_cmp(t1, t2)
  tran *t1, *t2;
{
  register int r;

  r = auto_transition_cmp(t1, t2);
  if (r)
    return r;

  return (t1 -> end_state) - (t2 -> end_state);
}

/**  void  auto_mark_accepting_state(a, p)  :  Sets the first byte
                     of the accepting status of the state p of the
                     automaton *a to 1.                            **/

void  auto_mark_accepting_state(a, p)
  automaton *a;
  uint4      p;
{
#if LASH_CHECK_LEVEL >= 1
  if (!a || p >= a -> nb_states)
    return;
#endif    

  auto_accepting_info_ptr(a, p)[0] = 1;
  auto_reset_property(a, AUTO_PROP_MINIMAL);
}

/**  void  auto_unmark_accepting_state(a, p)  :  Sets the first byte
                     of the accepting status of the state p of the
                     automaton *a to 0.                            **/

void  auto_unmark_accepting_state(a, p)
  automaton *a;
  uint4      p;
{
#if LASH_CHECK_LEVEL >= 1
  if (!a || p >= a -> nb_states)
    return;
#endif    

  auto_accepting_info_ptr(a, p)[0] = 0;
  auto_reset_property(a, AUTO_PROP_MINIMAL);
}

/**  int  auto_replace(a1, a2)  :  Frees the components of the 
                     automaton *a1 and replaces them by the 
                     components of *a2. Then, frees the automaton *a2.

                     If successful, returns 0. In the case of an
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  auto_replace(a1, a2)
  automaton *a1, *a2;
{
  diag__enter("auto_replace", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a1 || !a2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (a1 -> nb_i_states || a1 -> i_states)
    resr__free_objects (a1 -> i_states, uint4, a1 -> nb_i_states);

  free_states(a1);
  *a1 = *a2;
  resr__free_object(a2, automaton);

  diag__return(0);
}

/**  automaton *auto_copy(a)  :  Returns a copy of the finite-state
                     automaton *a, or a NULL pointer in the case of
                     an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

automaton *auto_copy(a)
  automaton *a;
{
  register automaton *ar;
  register uint4      i, j, n, *p;
  register state     *s, *sa;

  diag__enter("auto_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  ar = resr__new_object(automaton);
  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  *ar = *a;

  if (a -> nb_i_states)
    {
      p = resr__new_objects(uint4, a -> nb_i_states);
      if (!p)
	{
	  resr__free_object(ar, automaton);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
    }
  else
    p = NULL;

  ar -> i_states = p;
  memcpy(p, a -> i_states, (a -> nb_i_states) * sizeof(uint4));

  n = a -> nb_states;

  if (n)
    {
      s = resr__new_objects(state, n);
      if (!s)
	{
	  resr__free_objects(p, uint4, a -> nb_i_states);
	  resr__free_object(ar, automaton);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
    }
  else
    s = NULL;

  ar -> states = s;
  for (i = 0, sa = a -> states; i < n; i++, s++, sa++)
    if (state_cpy(s, sa, a -> alphabet_nbytes, 
        a -> accept_nbytes) < 0)
      {
	for (j = 0; j < i; j++)
	  {
	    free_trans(ar -> states + j, ar -> alphabet_nbytes);
	    resr__free_info_objects(&ar -> states[j].accepting,
	        uint1, ar -> accept_nbytes);
	  }
	resr__free_objects(ar -> states, state, n);
	resr__free_objects(p, uint4, a -> nb_i_states);
	resr__free_object(ar, automaton);
	diag__fail(LASH_ERR_NO_MEM, NULL);
      }

  diag__return(ar);
}

/**  int  auto_search_out_transition(a, s, l, p, r)  :  Searches the
                     outgoing transitions of the state number s of
                     the automaton *a for a transition labeled by the
                     sequence of l symbols given at *p. Those outgoing
                     transitions are supposed to be sorted in
                     lexicographical order of label. The function
                     returns -1 if no suitable transition is found.
                     Otherwise, it returns 0 and writes the index of
                     the transition that has been found into *r.

		     This function is for internal use of the lash
                     package, and should not be called by external
		     applications!                                 **/

int  auto_search_out_transition(a, s, l, p, r)
  automaton *a;
  uint4      s, l, *r;
  uint1     *p;
{
  register tran  *tf;
  register state *st;
           tran   t;

#if LASH_CHECK_LEVEL >= 1
  if (!a || s >= a -> nb_states)
    return -1;
#endif

  st = a -> states + s;
  if (!(st -> nb_trans))
    return -1;
  
  t.nb_symbols = l;
  info__link_ptr(&t.label, p, l * auto_alphabet_nbytes(a));

  auto_transition_cmp_prepare(a);

  tf = (tran *) bsearch((void *) &t, st -> trans, st -> nb_trans,
      sizeof(tran), (int (*)(const void *, const void *)) 
      auto_transition_cmp);

  if (!tf)
    return -1;

  if (r)
    *r =  tf - (st -> trans);

  return 0;
}

/**  void  auto_pack_out_transitions(a, s)  :  Eliminates duplicate
		     outgoing transitions from the state number s of
		     the finite-state automaton *a. This function does
		     not detect errors.

		     This function is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

void  auto_pack_out_transitions(a, s)
  automaton *a;
  uint4 s;
{
  register uint4  n1, n2, i;
           uint4  n;
  register tran  *ps, *pt;
  register state *st;

#if LASH_CHECK_LEVEL >= 1
  if (!a || s >= a -> nb_states)
    return;
#endif

  st = a -> states + s;
 
  n1 = n = st -> nb_trans;
  ps = st -> trans;

  auto_transition_cmp_prepare(a);

  if (bytes__sort_and_pack((void *) ps, &n, sizeof(tran),
     (int (*) (const void *, const void *)) auto_transition_full_cmp,
     (void (*) (const void *)) auto_transition_free) < 0) 
    {
      if (n1 > 1)
	qsort((void *) ps, n1, sizeof(tran), 
            (int (*) (const void *, const void *)) 
            auto_transition_full_cmp);

      for (i = 0, pt = ps, n2 = 0; i < n1; i++, ps++)
	if (!i || auto_transition_full_cmp(ps, ps - 1))
	  {
	    *(pt++) = *ps;
	    n2++;
	  }
	else
          resr__free_info_objects(&ps -> label, 
            uint1, (ps -> nb_symbols) * (a -> alphabet_nbytes));
    }
  else
    n2 = n;

  if (n1 != n2)
    {
      st -> trans = resr__resize_objects(st -> trans, tran, n2, n1);
      st -> nb_trans = n2;
    }
}
                    
/**  void  auto_sort_transitions(a)  :  Sorts the outgoing transitions
                     of each state of the automaton *a according to
                     the lexicographical order of their labels. This
                     function does not detect errors.

		     This function is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

void  auto_sort_transitions(a)
  automaton *a;
{
  register uint4 i, n;

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    return;
#endif

  n = a -> nb_states;
  auto_transition_cmp_prepare(a);

  for (i = 0; i < n; i++)
    if (a -> states[i].nb_trans > 1 &&
        bytes__sort(((void *) (a -> states[i].trans)),
          a -> states[i].nb_trans, sizeof(tran),
          (int (*)(const void *, const void *)) 
          auto_transition_cmp) < 0)
      qsort(((void *) (a -> states[i].trans)),
          a -> states[i].nb_trans, sizeof(tran),
          (int (*)(const void *, const void *)) 
          auto_transition_cmp);
}

/**  void  auto_remove_i_states(a)  :  Empties the set of initial
                     states of the automaton *a.                   **/

void  auto_remove_i_states(a)
  automaton *a;
{
#if LASH_CHECK_LEVEL >= 1
  if (!a)
    return;
#endif

  if (a -> nb_i_states || a -> i_states)
    resr__free_objects(a -> i_states, uint4, a -> nb_i_states);

  a -> i_states    = NULL;
  a -> nb_i_states = ZERO_INT4;
}

/**  void  auto_remove_trans(a, s)  :  Removes all the transitions
                     outgoing from the state of index  s of the
		     automaton pointed by a.
 
		     This function is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

void  auto_remove_trans(a, s)
  automaton *a;
  uint4  s;
{
  register state *st;

#if LASH_CHECK_LEVEL >= 1
  if (!a || s >= a -> nb_states)
    return;
#endif 

  free_trans(st = a -> states + s, a -> alphabet_nbytes);

  st -> nb_trans = ZERO_INT4;
  st -> trans = NULL;
}

/****  End of auto.c  ****/
