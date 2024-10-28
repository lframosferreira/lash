/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-determinize.c  :  Determinization of finite-state      **/
/**                     automata.                                  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        09/15/98  :  Got rid of recursion. (BB)                 **/
/**        09/16/98  :  Minor corrections. (BB)                    **/
/**        10/26/98  :  Minor corrections. (BB)                    **/
/**        11/09/98  :  More efficient data structures. (BB)       **/
/**        11/12/99  :  Bug corrected in hash table size           **/
/**                     computation. (BB)                          **/
/**        03/01/01  :  Determinization of co-Buchi automata. (SJ) **/
/**        04/30/01  :  Heuristics to speed up co-Buchi            **/
/**                     automata determinization. (SJ)             **/
/**        08/14/01  :  Small adaptations. (BB)                    **/
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
#include <string.h>
#include <stdlib.h>
#include "auto.h"
#include "auto-determinize.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "graph-scc.h"

/****  Definitions for co-Buchi determinization.                 ****/

/**  COBUCHI_HEURISTICS  :  Parameter that tells the
                     determinization algorithm for co-Buchi
		     automata whether or not it must use a
		     heuristics to speed up the determinization
		     process.                                      **/

#define  COBUCHI_HEURISTICS    1

/**  Management of R and S sets.                                   **/

/**  encode_s_set_elem(e)  (resp. encode_r_set_elem(e) )  :
		     Macros that encode a state index of the S set
		     (resp. the R set) by not setting (resp. by
		     setting) its most significant bit. This allows
		     to store only one set of state indices for
		     each state of the determinized automaton.     **/

#define  encode_s_set_elem(e)  (e)
#define  encode_r_set_elem(e)  ((e) | MSB_MASK_UINT4)

/**  extract_sr_state(e)  :  Macro that decodes a state index
		     that belongs to the set associated with
		     a state of the determinized automaton.        **/

#define  extract_sr_state(e)   ((e) & ~MSB_MASK_UINT4)

/**  test_r_set_elem(e)  :  Macro that tests if e is an element of
		     the R set.                                    **/

#define  test_r_set_elem(e)    ((e) & MSB_MASK_UINT4)

/**  is_cobuchi_accepting_state(a, s)  :  Macro that checks whether 
                     the state of index s in the co-Buchi automaton *a
		     is accepting or not. One must inverse the result
		     if *a is a weak automaton.                    **/

#define  is_cobuchi_accepting_state(a, s) \
         ( auto_accept_type( (a) ) == AUTO_ACCEPT_WEAK ? \
	   ! auto_accepting_state( (a), (s) ) : \
	     auto_accepting_state( (a), (s) ) )


/****  Global variables.                                         ****/

/**  auto_determ_hsize  :  Size of the hash table used by the     
                     determinization algorithm.                    **/

static uint4  auto_determ_hsize = AUTO_DEFAULT_DETERM_HSIZE;

/**  auto_determ_ncolls  :  Number of collisions observed in the   
                     hash table used by the determinization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_determ_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_determ_nins  :  Number of insertions performed in the   
                     hash table used by the determinization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_determ_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static uint4_set  *i_states_set(automaton *);
static uint4_set  *successor_states(automaton *, uint4_set *,
                       uint4, uint1 *);
static uint4_set  *epsilon_successor_states(automaton *, uint4_set *);
static int         epsilon_explore(automaton *, uint4, bit_table *,
                       uint4_set *);
static uint4_set  *symbol_one_successor_states(automaton *,
                       uint4_set *, uint1 *);
static uint4       compute_determ_hsize(automaton *);
static int         determ_generate(automaton *, automaton *,
                       uint4_set *, hash_table *, uint4 *);
static int         determ_generate_init(automaton *, automaton *,
                       hash_table *, stack *);
static int         determ_generate_loop(automaton *, automaton *,
                       hash_table *, stack *);
static int         determ_generate_one(automaton *, automaton *,
                       uint4, uint4_set *, uint1 *, hash_table *,
                       stack *);
static void        determ_set_accepting_status(automaton *, uint4,
                       automaton *, uint4_set *);
static uint4_set  *extract_sr_set(uint4_set *, uint1);
static int         unmark_transient_states(automaton *, stack *,
                       uint4, uint4 *, uint1, uint1, void *);

/****  Private functions.                                        ****/

/**  uint4_set *i_states_set(a)  :  Returns the set of the indices of
                     all the initial states of the automaton *a. In
                     case of insufficient memory, returns a NULL
                     pointer.                                      **/

static uint4_set *i_states_set(a)
  automaton *a;
{
  register uint4      n, i;
           uint4      m;
  register uint4_set *s;

  s = set__new_empty();
  if (!s)
    return NULL;

  n = auto_nb_i_states(a);
  for (i = 0; i < n; i++)
    {
#if LASH_CHECK_LEVEL >= 1
      if (auto_i_state(a, i, &m) < 0)
	{
	  set__free(s);
	  return NULL;
	}
#else
      auto_i_state(a, i, &m);
#endif
      if (set__add(s, m) < 0)
	{
	  set__free(s);
	  return NULL;
	}
    }

  return s;
}

/**  uint4_set *successor_states(a, s, l, p)  :  Returns the set of
                     the indices of all the states of the automaton
                     *a that can be reached from states in the set *s
                     by following a path of transitions labeled by the
                     sequence of l symbols given in *p. The automaton
                     *a must be normal, and its states must have
                     outgoing transitions sorted lexicographically.
                     In case of insufficient memory, returns a NULL
                     pointer.                                      **/

static uint4_set *successor_states(a, s, l, p)
  automaton *a;
  uint4_set *s;
  uint4      l;
  uint1     *p;
{
  register uint4_set *s1, *s2;
  register uint4      i, n;

  n = auto_alphabet_nbytes(a);

  s1 = epsilon_successor_states(a, s);
  if (!s1)
    return NULL;  

  for (i = 0; i < l; i++)
    {
      s2 = symbol_one_successor_states(a, s1, p + i * n);
      set__free(s1);
      if (!s2)
	return NULL;
      s1 = epsilon_successor_states(a, s2);
      set__free(s2);
      if (!s1)
	return NULL;        
    }

  return s1;
}

/**  uint4_set *epsilon_successor_states(a, s)  :  Returns the set of
                     the indices of all the states of the automaton *a
                     that can be reached from states in the set *s by
                     following a path of transitions labeled by the
                     empty word.  The automaton *a must be normal, and
                     its states must have outgoing transitions sorted
                     lexicographically.  In case of insufficient
                     memory, returns a NULL pointer.               **/

static uint4_set *epsilon_successor_states(a, s)
  automaton *a;
  uint4_set *s;
{
  register uint4      i, n;
  register bit_table *bt;
  register uint4_set *s1;

  s1 = set__new_empty();
  if (!s1)
    return NULL;

  n = set__nb_elements(s);
  if (!n)
    return s1;

  bt = bit__new_empty(auto_nb_states(a));
  if (!bt)
    {
      set__free(s1);
      return NULL;
    }

  for (i = 0; i < n; i++)
    if (epsilon_explore(a, set__element(s, i), bt, s1) < 0)
      {
	set__free(s1);
	bit__free(bt);
        return NULL;
      }
  
  bit__free(bt);
  return s1;
}

/**  typedef  ee_info  :  Type of the data placed of the exploration
                     stack of the function epsilon_explore. The first
                     field is the index of a state; the second field
                     is the number of outgoing transitions that have
                     already been followed from that state.        **/

typedef struct {
  uint4  state, nb_transitions;
} ee_info;

/**  int  epsilon_explore(a, i, bt, s)  :  Adds to the set *s the
                     indices of all the unvisited states of the
                     automaton *a that can be reached from the state
                     of index i by following a path of transitions
                     labeled by the empty word. The bit table *bt
                     contains the already visited states. This table
                     is updated by the function. The function returns
                     -1 if there is not enough memory, and 0
                     otherwise.                                    **/

static int  epsilon_explore(a, i, bt, s)
  automaton *a;
  uint4      i;
  bit_table *bt;
  uint4_set *s;
{
           uint4    m;
  register tran    *t;
  register stack   *st;
           ee_info  e;
  
  st = stack__new_empty(ee_info);
  if (!st)
    return -1;

  e.state = i;
  e.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &e) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &e);
      if (!e.nb_transitions)
        {
          if (bit__member(bt, e.state))
            continue;

          bit__add(bt, e.state);

          if (set__add(s, e.state) < 0) 
            {
              stack__free(st);
              return -1;
            }
        }
      if (auto_nb_out_transitions(a, e.state, &m) < 0)
        {
          stack__free(st);
          return -1;
        }
      if (e.nb_transitions >= m)
        continue;
     
      t = auto_transition(a, e.state, e.nb_transitions);
      if (auto_transition_length(t) > 0)
        continue;

      e.nb_transitions++;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }

      e.state = auto_transition_dest(t);
      e.nb_transitions = ZERO_INT4;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  uint4_set *symbol_one_successor_states(a, s, p)  :  Returns the
                     set of the indices of all the states of the
                     automaton *a that can be reached from states in
                     the set *s by following one transition labeled by
                     the symbol given by *p. The automaton *a must be
                     normal, and its states must have outgoing
                     transitions sorted lexicographically.  In case of
                     insufficient memory, returns a NULL pointer.  **/

static uint4_set *symbol_one_successor_states(a, s, p)
  automaton *a;
  uint4_set *s;
  uint1     *p;
{
  register uint4      i, j, k, n;
  register uint4_set *s1;
  register tran      *t, *tq;
           uint4      m, q;

  s1 = set__new_empty();
  if (!s1)
    return NULL;

  n = set__nb_elements(s);
  if (!n)
    return s1;

  for (i = 0; i < n; i++)
    {
      j = set__element(s, i);
      
      if (auto_search_out_transition(a, j, 1, p, &q) < 0)
        continue;

      tq = auto_transition(a, j, q);

      if (auto_nb_out_transitions(a, j, &m) < 0)
	{
	  set__free(s1);
          return NULL;
	}

      auto_transition_cmp_prepare(a);

      for (k = q; k; k--)
	{
	  t = auto_transition(a, j, k - 1);
	  if (!auto_transition_cmp(tq, t) && 
              set__add(s1, auto_transition_dest(t)) < 0)
             {
	       set__free(s1);
	       return NULL;
	     }
	}
       
      for (k = q; k < m; k++)
	{
	  t = auto_transition(a, j, k);
	  if (!auto_transition_cmp(tq, t) &&
	      set__add(s1, auto_transition_dest(t)) < 0)
	    {
	      set__free(s1);
	      return NULL;
	    }	      
	}
    }

  return s1;
}

/**  uint4  compute_determ_hsize(a)  :  Adjusts (heuristically) the
                     size of the hash table needed for determinizing
                     the automaton *a.                             **/

static uint4  compute_determ_hsize(a)
  automaton *a;
{
  register uint4  n;

  n = auto_nb_states(a);

  if (!n)
    n = 1;

  if (n < 32 && auto_determ_hsize >> n)
    return (1 << n);

  if (auto_determ_hsize < ((3 * n) / 2))
    return (3 * n) / 2;

  return auto_determ_hsize;
}

#define WHICH_S_SET   0x01
#define WHICH_R_SET   0x02

/**  uint4_set  *extract_sr_set(set, w)  :  Given a set *set of
		     states of a co-Buchi automaton being 
		     determinized, this function extracts the states
		     of *set that belongs to the S set (resp. R set)
		     if which equals WHICH_S_SET (resp. WHICH_R_SET).
		     In case of insufficient memory or wrong
		     argument, it returns a NULL pointer.          **/

static uint4_set  *extract_sr_set(set, w)
     uint4_set  *set;
     uint1       w;
{
  register uint4_set  *s;
  register uint4       i, e, n;
  
#if LASH_CHECK_LEVEL >= 1
  if (!set || !(w == WHICH_S_SET || w == WHICH_R_SET))
    return NULL;
#endif  /* >= 1 */

  s = set__new_empty();
  if (!s)
    return NULL;

  n = set__nb_elements(set);

  for (i=0 ; i<n ; i++)
    {
      e = set__element(set, i);

      if ((w == WHICH_S_SET &&
	   set__add(s, extract_sr_state(e)) < 0) ||

	  (w == WHICH_R_SET &&
	   test_r_set_elem(e) &&
	   set__add(s, extract_sr_state(e)) < 0))
	{
	  set__free(s);
	  return NULL;
	}
    }

  return s;
}

/**  typedef  dg_info  :  Type of the data placed of the exploration
                     stack of the function determ_generate.
                     The first field is a pointer to the set of
                     states of the automaton being determinized that
                     corresponds to the current state of the
                     resulting automaton. In the case of a co-Buchi
		     automaton, a state of the resulting automaton
		     is a pair (S, R) of sets of states that are
		     merged into a single set by setting the
		     most significant bit of the states of R. The
		     second field is a mode field; its value is
		     DG_ROOT if the function is invoked for the
		     first time with the current automaton, 
		     DG_FIRST if it is called for the first time with 
		     the current set of states, and DG_NEXT 
		     otherwise. The values of the next fields
                     are mode-dependent. If mode == DG_ROOT, there is
                     one field that gives a pointer to a location to
                     which the function should return the index of the
                     topmost created state. If mode == DG_FIRST, there
                     are two fields specifying the origin and the
                     label of a transition to be created. If mode ==
                     DG_NEXT, there are two fields corresponding to an
                     array of transition indices that have already
                     been explored and an array of bounds to these
                     indices, as well as a third field giving the
                     index of the state of the result automaton
                     that corresponds to the current set of states of
                     the original one.                             **/

typedef struct {
  uint4_set *set;
  uint1      mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
    } tr;
    struct {
      uint4 *ncur, *nmax, origin;
    } ar;
  } v;
} dg_info;

#define  DG_ROOT   0x01
#define  DG_FIRST  0x02
#define  DG_NEXT   0x03

/**  int  determ_generate(a, nda, s, ht, p)  :  This function is part 
                     of the determinization algorithm. It adds to the
                     automaton *a a state corresponding to the set of
                     states of *nda *s, inserting the corresponding
                     entry in the hash table *ht. The state index
                     corresponding to the newly created state of *a is
                     returned in *p. The function also creates
                     transitively all the successor states of the new
                     state as well as its outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  determ_generate(a, nda, s, ht, p)
  automaton  *a, *nda;
  uint4_set  *s;
  hash_table *ht;
  uint4      *p;
{
  register stack    *st;
  register uint4     n;
  register uint1     m;
           dg_info   d;

  st = stack__new_empty(dg_info);
  if (!st)
    return -1;

  d.set = set__new_copy(s);
  if (!d.set)
    {
      stack__free(st);
      return -1;
    }

  d.mode = DG_ROOT;
  d.v.return_state = p;

  if (stack__push(st, (void *) &d) < 0)
    {
      set__free(d.set);
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      m = ((dg_info *) stack__top(st)) -> mode;
      if ((m != DG_NEXT && 
	   determ_generate_init(a, nda, ht, st) < 0) ||
	  (m == DG_NEXT &&
           determ_generate_loop(a, nda, ht, st) < 0))
	{
  	  while (!stack__is_empty(st))
	    {
	      stack__pop(st, (void *) &d);
	      if (d.mode == DG_NEXT)
	        {
	  	  n = set__nb_elements(d.set);
		  resr__free_objects(d.v.ar.ncur, uint4, n);
		  resr__free_objects(d.v.ar.nmax, uint4, n);
		}
	      set__free(d.set);
	    }
	  stack__free(st);
	  return -1;
	}
    }
  
  stack__free(st);
  return 0;
}

/**  int  determ_generate_init(a, nda, ht, st)  :  This function is 
                     part of the determinization algorithm. It adds to
                     the automaton *a a state corresponding to the set
                     of states of *nda placed on top of the
                     exploration stack *st (supposed non-empty and of
                     mode equal to either DG_ROOT or DG_FIRST),
                     inserting the corresponding entry in the hash
                     table *ht. It then updates the exploration stack
                     such that the next calls to the function
                     determ_generate_loop will create transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  determ_generate_init(a, nda, ht, st)
  automaton  *a, *nda;
  hash_table *ht;
  stack      *st;
{
  register uint4     e, i, n, *nmax, *ncur, *v;
  register dg_info  *d;
           void    **r;
           uint4     st1;

  d = (dg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (d -> mode !=  DG_ROOT && d -> mode != DG_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;
  
#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_set(ht, d -> set, &r, &auto_determ_ncolls,
      &auto_determ_nins) < 0)
#else
  if (hash__insert_set(ht, d -> set, &r) < 0)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  if (!r)
    {
      uint4__free(v);
      return -1;
    }
      
  *r = (void *) v;
  
  if (auto_add_new_state(a, &st1) < 0)
    return -1;
      
  *v = st1;

  if (d -> mode == DG_ROOT)
    {
      if (d -> v.return_state)
	*(d -> v.return_state) = st1;
    }
  else
    if (auto_add_new_transition(a, d -> v.tr.origin, st1, 
        1, d -> v.tr.label) < 0)
      return -1;

  determ_set_accepting_status(a, st1, nda, d -> set); 

  n  = set__nb_elements(d -> set);

  nmax = resr__new_objects(uint4, n);
  if (!nmax)
    return -1;

  ncur = resr__new_objects(uint4, n);
  if (!ncur)
    {
      resr__free_objects(nmax, uint4, n);
      return -1;
    }

  for (i = 0; i < n; i++)
    {
      e = set__element(d -> set, i);
      if (auto_word_type(nda) == AUTO_WORDS_INFINITE)
	e = extract_sr_state(e);      

      if (auto_nb_out_transitions(nda, e, nmax + i) < 0)
        {
          resr__free_objects(nmax, uint4, n);
          resr__free_objects(ncur, uint4, n);
          return -1;      
        }
      ncur[i] = 0;
    }

  d -> mode        = DG_NEXT;
  d -> v.ar.ncur   = ncur;
  d -> v.ar.nmax   = nmax;
  d -> v.ar.origin = st1;

  return 0;
}

/**  int  determ_generate_loop(a, nda, ht, st)  :  This function is 
                     part of the determinization algorithm. The
                     partially computed deterministic automaton is in
                     *a. This function explores the outgoing
                     transitions from the set of states of *nda placed
                     on top of the exploration stack *st (supposed
                     non-empty and of mode equal to DG_NEXT), updating
                     the stack such that the next calls to this
                     function and to the function determ_generate_init
                     will create transitively all the successor states
                     of the new state as well as their outgoing
                     transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  determ_generate_loop(a, nda, ht, st)
  automaton  *a, *nda;
  hash_table *ht;
  stack      *st;
{
  register uint4     e, i, n, nb, *ncur, *nmax, st1;
  register dg_info  *d;
  register tran     *t, *first;

  d = (dg_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (d -> mode !=  DG_NEXT)
    return -1;
#endif  /* >= 1 */

  n    = set__nb_elements(d -> set);
  nb   = auto_alphabet_nbytes(nda);
  ncur = d -> v.ar.ncur;
  nmax = d -> v.ar.nmax;
  st1  = d -> v.ar.origin;

  first = NULL;
  auto_transition_cmp_prepare(nda);

  for (i = 0; i < n; i++)
    if (ncur[i] < nmax[i])
      {
	e = set__element(d -> set, i);
	if (auto_word_type(nda) == AUTO_WORDS_INFINITE)
	  e = extract_sr_state(e);

	t = auto_transition(nda, e, ncur[i]);
	if (!first || (auto_transition_cmp(t, first) < 0))
	  first = t;
      }

  if (!first)
    {
      resr__free_objects(nmax, uint4, n);
      resr__free_objects(ncur, uint4, n);
      set__free(d -> set);
      stack__pop(st, NULL);
      return 0;
    }

  auto_transition_cmp_prepare(nda);

  for (i = 0; i < n; i++)
    while (ncur[i] < nmax[i])
      {
	e = set__element(d -> set, i);
	if (auto_word_type(nda) == AUTO_WORDS_INFINITE)
	  e = extract_sr_state(e);

	t = auto_transition(nda, e, ncur[i]);
	if (auto_transition_cmp(t, first))
	  break;
	ncur[i]++;
      }
  
  if (auto_transition_length(first) > 0 &&
      determ_generate_one(a, nda, st1, d -> set,
      auto_transition_label_ptr(first, nb), ht, st) < 0)
    return -1;

  return 0;
}

/**  int  determ_generate_one(a, nda, s1, s2, lash, ht, st)  :  This
                     function is part of the determinization
                     algorithm. It updates the exploration stack *st
                     so as to add to the automaton *a an outgoing
                     transition from the state s1 (which is associated
                     to the set of states indices of *nda *s2), this
                     transition being labeled by the symbol *lash. The
                     hash table used for relating the states of *a to
                     the sets of states of *nda is given by *ht.
                    
                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  determ_generate_one(a, nda, s1, s2, lash, ht, st)
  automaton  *a, *nda;
  uint4       s1;
  uint4_set  *s2;
  uint1      *lash;
  hash_table *ht;
  stack      *st;
{
  register uint4_set *new_r, *new_s, *new_set, *tmp;
  register void      *r;
  register uint4      n, i, e;
           dg_info    d;

  switch (auto_word_type(nda))
    {
    case AUTO_WORDS_FINITE :

      new_set = successor_states(nda, s2, 1, lash);
      if (!new_set)
	return -1;

      break;

    case AUTO_WORDS_INFINITE :

      /* Generates the S set of the successor node */

      tmp = extract_sr_set(s2, WHICH_S_SET);
      if (!tmp)
	return -1;
  
      new_s = successor_states(nda, tmp, 1, lash);
  
      set__free(tmp);
      if (!new_s)
	return -1;      
  
      /* Extracts the R set of the current node */

      tmp = extract_sr_set(s2, WHICH_R_SET);
      if (!tmp)
	return -1;

      if (set__nb_elements(tmp))
	{
	  /* R is not empty : generates the R set of the successor 
             node */

	  new_r = successor_states(nda, tmp, 1, lash);
      
	  set__free(tmp);
	  if (!new_r)
	    {
	      set__free(new_s);
	      return -1;      
	    }
      
	  /* Generates the set of states for the successor node */
      
	  new_set = set__new_empty();

	  n = set__nb_elements(new_s);
	  for (i=0 ; i<n ; i++)
	    {
	      e = set__element(new_s, i);
	  
	      if (! is_cobuchi_accepting_state(nda, e) &&
		  set__member(new_r, e))
		set__add(new_set, encode_r_set_elem(e));
	      else
		set__add(new_set, encode_s_set_elem(e));
	    }

	  set__free(new_r);
	  set__free(new_s);
	}

      else
	{
	  /* The current R set is empty : a breakpoint has been 
             reached */

	  new_set = tmp;

	  n = set__nb_elements(new_s);
	  for (i=0 ; i<n ; i++)
	    {
	      e = set__element(new_s, i);
	  
	      if (! is_cobuchi_accepting_state(nda, e))
		set__add(new_set, encode_r_set_elem(e));
	      else
		set__add(new_set, encode_s_set_elem(e));
	    }
	  
	  set__free(new_s);
	}

      break;

    default :
      return -1;
    }

  r = hash__lookup_set(ht, new_set);
  if (r)
    {
      set__free(new_set);
      return auto_add_new_transition(a, s1, *((uint4 *) r), 1,
          lash);
    }

  d.mode        = DG_FIRST;
  d.set         = new_set;
  d.v.tr.origin = s1;
  d.v.tr.label  = lash;

  if (stack__push(st, (void *) &d) < 0)
    {
      set__free(d.set);
      return -1;
    }

  return 0;
}

/**  void  determ_set_accepting_status(a1, s1, a2, s2)  :  This
                     function is part of the determinization
                     algorithm. It marks the state of index s1 of the
                     automaton *a1 as accepting if and only if the set
                     *s2 contains the index of an accepting state of
                     the automaton *a2. In the case of automaton on
		     infinite words, a state is accepting if and only
		     if it is a breakpoint (i.e. if and only if its
		     R set is empty).                              **/

static void  determ_set_accepting_status(a1, s1, a2, s2)
  automaton *a1, *a2;
  uint4      s1;
  uint4_set *s2;
{
  register uint4  i, n;
 
  n = set__nb_elements(s2);
  for (i = 0; i < n; i++)
    switch (auto_word_type(a2))
      {
      case AUTO_WORDS_FINITE :
	if (auto_accepting_state(a2, set__element(s2, i)))
	  {
	    auto_mark_accepting_state(a1, s1);
	    return;
	  }
	break;

      case AUTO_WORDS_INFINITE :
	if (test_r_set_elem(set__element(s2, i)))
	  return;
	break;
      }

  if (auto_word_type(a2) == AUTO_WORDS_INFINITE)
    auto_mark_accepting_state(a1, s1);
}


/**  int  unmark_transient_states(a, st, root, scc_num, accept,
		     transient, arg)  :
		     Function invoked by the strongly connected
		     components detection algorithm. It turns
		     each transient state into an accepting state,
		     which does not change the accepted language
		     if *a is a co-Buchi automaton. A state is said
		     to be transient if it is the only state of a
		     s.c.c. and if it is not linked to itself. The
		     argument transient is true if and only if
		     the state root is transient in the automaton *a.

		     Marking as accepting the transient states of
		     a co-Buchi automaton is a heuristics to speed
		     up the determinization process.

		     This function does not need to know the
		     states belonging to the current s.c.c., so
		     it does not use the stack *st. For this reason,
		     the parameter SCC_AUTOMATIC_POP must be used in
		     every call to Tarjan's algorithm that uses this
		     function.

		     This function cannot incur an error. Its
		     return value will always be 0.                **/

static int  unmark_transient_states(a, st, root, scc_num, accept,
				    transient, arg)
     automaton   *a;
     stack       *st;
     uint4        root;
     uint4       *scc_num;
     uint1        accept, transient;
     void        *arg;
{
  if (transient)
    {
      if (auto_accept_type(a) == AUTO_ACCEPT_WEAK)
	auto_unmark_accepting_state(a, root);
      else
	auto_mark_accepting_state(a, root);
    }

  return 0;
}

/****  Public visible functions.                                 ****/

/**  void  auto_set_determ_hsize(s)  :  Sets the size of the hash
                     table used by the determinization algorithm to s.
                     This function does not report errors.         **/

void  auto_set_determ_hsize(s)
  uint4 s;
{
  if (s)
    auto_determ_hsize = s;
}

/**  uint8  auto_get_determ_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     determinization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_determ_ncolls()
{
  return auto_determ_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_determ_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     determinization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_determ_ncolls()
{
  auto_determ_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_determ_nins()  :  Returns the number of
                     insertions performed in the hash table used by
                     the determinization algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_determ_nins()
{
  return auto_determ_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_determ_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the determinization algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_determ_nins()
{
  auto_determ_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  int  auto_determinize(a)  :  Determinizes the finite-state
                     automaton on finite or infinite words *a, i.e.,
		     transforms it into a deterministic automaton
		     accepting the same language. If *a is an
		     automaton on infinite words, it must be co-Buchi
		     or weak, in which case the breakpoint 
                     construction is applied [MH84,BJW01].

                     In case of success, the function returns 0.  In
                     case of error, it returns -1 and leaves in *a an
                     automaton that accepts the same language as the
                     original one, but not necessarily deterministic
                     or normal and possibly containing useless
                     components.
 
                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Automaton with too 
			                       many states.        **/

int  auto_determinize(a)
  automaton *a;
{
  register automaton  *ad;
  register uint4_set  *s1, *s2;
  register hash_table *ht;
           uint4       i;

  diag__enter("auto_determinize", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE &&
      (auto_word_type(a) != AUTO_WORDS_INFINITE ||
       (auto_accept_type(a) != AUTO_ACCEPT_COBUCHI &&
	auto_accept_type(a) != AUTO_ACCEPT_WEAK)))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (auto_test_property(a, AUTO_PROP_DETERM))
    diag__return(0);

  if (auto_normalize(a) < 0)
    diag__fail(lash_errno, -1);
    
  if (auto_word_type(a) == AUTO_WORDS_INFINITE &&
      (auto_nb_states(a) & MSB_MASK_UINT4))
    diag__fail(LASH_ERR_TOO_BIG, -1);

#if COBUCHI_HEURISTICS >= 1

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      if (scc_algorithm(a, NULL, &i, SCC_ONLY_REACHABLE 
                                   | SCC_AUTOMATIC_POP,
			unmark_transient_states, NULL) < 0)
	diag__fail(lash_errno, -1);
    }

#endif  /* >= 1 */

  auto_sort_transitions(a);

  ad = auto_new_empty(auto_alphabet_nbytes(a));
  if (!ad)
    diag__fail(LASH_ERR_NO_MEM, -1);

  ht = hash__new_empty(compute_determ_hsize(a));
  if (!ht)
    {
      auto_free(ad);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  s1 = i_states_set(a);
  if (!s1)
    {
      auto_free(ad);
      hash__free(ht, NULL, NULL);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  s2 = successor_states(a, s1, 0, NULL);
  set__free(s1);
  if (!s2)
    {
      auto_free(ad);
      hash__free(ht, NULL, NULL);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if (determ_generate(ad, a, s2, ht, &i) < 0)
    {
      set__free(s2);
      auto_free(ad);
      hash__free(ht, (void (*)(void *)) set__free,
          (void (*)(void *)) uint4__free);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  hash__free(ht, (void (*)(void *)) set__free,
    (void (*)(void *)) uint4__free);
  set__free(s2);

  if (auto_add_new_i_state(ad, i) < 0)
    {
      auto_free(ad);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ad) = AUTO_WORDS_INFINITE;
      auto_accept_type(ad) = AUTO_ACCEPT_COBUCHI;
    }

  auto_replace(a, ad);

  auto_set_property(a, AUTO_PROP_DETERM);
  diag__return(0);
}

/****  End of auto-determinize.c  ****/
