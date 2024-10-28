/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-projection.c  :  Projection and projection-related      **/
/**                 operations over sets represented as RVAs.      **/
/**                                                                **/
/**    03/26/01  :  Adaptation for RVAs of the algorithms for      **/
/**                 NDDs. (SJ)                                     **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/10/01  :  Management of RVAs with zero component. (SJ)   **/
/**    10/30/01  :  'rva_canonicalize' added. (SJ)                 **/
/**    11/16/01  :  Minor modification. (SJ)                       **/
/**    04/17/02  :  'rva_multiple_projection' added. (SJ)          **/
/**    07/10/02  :  Reorganization. (BB)                           **/
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
#include "rva.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto-operations.h"
#include "auto-minimize.h"

/**  typedef prefix_info  :  Structure associated to each state
                     of a prefix automaton (which recognizes all the
                     sign digit combinations present in an automaton
                     undergoing the canonicalization operation). The
                     fields of this structure are defined as follows :

                         ptr : Index of the state of the main
                             automaton to which the state of the
                             prefix automaton is associated (this
			     field is modified during the execution
			     of the canonicalization algorithm).

                         nb_pred : Number of predecessors of
                             the current state in the prefix
                             automaton.

			*pred : Array of indices of the
                             predecessors of the current state.

                        *succ : Array of indices of the successors
                             of the current state (one for each sign
                             digit). If a successor is undefined
                             then, by convention, the index is
                             identical to the current state.

                         origin : Index of the origin state of the 
                             main automaton associated to the current
                             terminal state of the prefix automaton.
			     This field is only modified in the
			     initial stage of the canonicalization
			     algorithm.

                         nb_states : Number of states of the main
                             automaton associated to the current
                             terminal state of the prefix automaton.

			 states : Array of indices of the states of
                             the main automaton associated to the 
                             current terminal state of the prefix 
                             automaton.

			 next : Index of the next terminal state
                             in the lists of computed or of not-yet-
                             computed terminal states of the prefix
                             automaton. If this next state is 
                             undefined, then the value of this field
                             is, by convention, equal to the initial
			     state.
                             
                         flag, absent : Flags used by the 
			     canonicalization algorithm.

                         computed :  Flag set whenever the current
                             terminal state of the prefix automaton
                             belongs to the list of already computed
                             states.

                         ptr_ready : Flag set whenever the field ptr
                             contains the index of a valid state.
                                                                   **/
typedef struct {
  uint4                 ptr, nb_pred, origin, nb_states, *states,
                       *pred, succ[2], next;
  int                   flag : 1, absent : 1, computed : 1,
                        ptr_ready : 1;
} prefix_info;

/**  typedef prefix_automaton  :  Automaton recognizing all the 
                     sign digit combinations read by an automaton
                     undergoing the canonicalization operation. The
                     fields of this structure are defined as follows :
                         
                         nb_states : Number of states.

		        *states  : Array of states.

                         i_state : Index of the initial state.

			*r_states : Linked list of the terminal states
                             that are not yet computed.

			*c_states : Linked list of the terminal states
                             that are already computed.
                                                                   **/
                        
typedef struct {
  uint4         nb_states, i_state, r_states, c_states;
  prefix_info  *states;
} prefix_automaton;

/****  Prototypes of private functions.                          ****/

static uint4             compute_canon_hsize(uint4, uint4);
static prefix_automaton *new_empty_prefix_automaton(void);
static int               prefix_add_new_state(prefix_automaton *,
                             uint4 *);
static int               prefix_add_new_transition(prefix_automaton *,
                             uint4, uint4, uint1);
static prefix_automaton *initial_prefix_automaton(automaton *, uint4,
                             uint4);
static int               initial_prefix_init(automaton *, 
                             prefix_automaton *, uint4, hash_table *, 
                             stack *);
static int               initial_prefix_loop(automaton *,
			     prefix_automaton *, uint4, uint4,
			     hash_table *, stack *);
static void              free_prefix_automaton(prefix_automaton *);
static int               reverse_explore(prefix_automaton *, uint4,
			     int);
static int               direct_explore(prefix_automaton *);
static void              remove_pred_state(prefix_automaton *, uint4,
                             uint4);
static int               refine_prefix_automaton(prefix_automaton *,
			     uint4, uint4, uint4, uint4);
static int               refine_prefix_init(prefix_automaton *,
                             uint4 *, uint4, hash_table *, stack *);
static int               refine_prefix_loop(prefix_automaton *,
                             uint4 *, hash_table *, stack *);
static int               match_transitions(prefix_automaton *,
                             automaton *, uint4, uint4, uint4, uint4,
                             uint1);
static int               fill_step(prefix_automaton *, automaton *,
                             uint4, uint4 *, uint1);
static int               fill_prefix_automaton(prefix_automaton *,
                             automaton *, uint4, uint4);
static int               update_automaton(automaton *,
                             prefix_automaton *, uint4, uint4);
static int               canonicalize(automaton *, uint4, uint4);

/****  Global variables.                                         ****/

/**  canon_hsize  :  Size of the hash tables used by the projection 
                     algorithms.                                   **/

static uint4  canon_hsize = RVA_DEFAULT_PROJ_HSIZE;


/**  canon_ncolls  :  Number of collisions observed in the hash 
                     tables used by the canonicalization algorithm.
                                                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  canon_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  canon_nins    :  Number of insertions performed in the hash
                     tables used by the canonicalization algorithm.
                                                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  canon_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Private functions.                                        ****/

/**  uint4  compute_canon_hsize(nb, n)  :  Adjusts (heuristically) the
                     size of the hash tables needed for carrying out
                     the canonicalization of an automaton with nb
                     states operating over vectors of dimension n.
                                                                   **/

static uint4  compute_canon_hsize(nb, n)
  uint4  nb, n;
{
  if (canon_hsize > nb)
    return nb;

  if (canon_hsize < 16 * n)
    return 16 * n;

  return canon_hsize;
}

/**  prefix_automaton *new_empty_prefix_automaton()  :  This function
                     is part of the canonicalization operation. It
		     returns a pointer to a newly allocated prefix
		     automaton, or a NULL pointer in the case of
		     insufficient memory.                          **/

static prefix_automaton *new_empty_prefix_automaton()
{
  register prefix_automaton *pa;

  pa = resr__new_object(prefix_automaton);
  if (!pa)
    return NULL;

  pa -> nb_states = pa -> i_state = pa -> r_states = 
      pa -> c_states = ZERO_INT4;
  pa -> states = NULL;

  return pa;
}

/**  int  prefix_add_new_state(pa, ps)  :  This function is part of
                     the canonicalization operation. It adds a new
		     state without outgoing transitions to the
		     prefix automaton *pa, and returns the index of
		     that state in *ps. In the case of success, the
		     function returns 0. In the case of insufficient
		     memory, it returns -1.                        **/

static int  prefix_add_new_state(pa, ps)
  prefix_automaton *pa;
  uint4            *ps;
{
  register prefix_info *pi;
  register uint4        n;

#if LASH_CHECK_LEVEL >= 1
  if (!pa)
    return -1;
#endif  /* >= 1 */

  n  = pa -> nb_states;
  pi = resr__resize_objects(pa -> states, prefix_info, n + 1, n);
  if (!pi)
    return -1;

  pa -> states    = pi;
  pa -> nb_states = n + 1; 

  memset(pi += n, 0, sizeof(prefix_info));

  pi -> succ[0] = pi -> succ[1] = n;

  if (ps)
    *ps = n;

  return 0;
}

/**  int  prefix_add_new_transition(pa, no, nd, l)  :  This function 
                     is part of the canonicalization operation. It
		     adds a new transition from the state of index no
		     to the state of index nd and labeled by the
		     symbol 0 if l == 0 or by (numeration base - 1) if
		     l != 0 to the prefix automaton *pa. In the case
		     of success, the function returns 0. In the case
		     of insufficient memory, it returns -1.        **/

static int  prefix_add_new_transition(pa, no, nd, l)
  prefix_automaton *pa;
  uint4             no, nd;
  uint1             l;
{
  register prefix_info *s1, *s2;
  register uint4        n, *p;
  
#if LASH_CHECK_LEVEL >= 1
  if (!pa || no >= pa -> nb_states || nd >= pa -> nb_states)
    return -1;
#endif  /* >= 1 */

  s1 = pa -> states + no;
  s2 = pa -> states + nd;

  n  = s2 -> nb_pred;
  p  = resr__resize_objects(s2 -> pred, uint4, n + 1, n);
  if (!p)
    return -1;

  s2 -> pred    = p;
  s2 -> nb_pred = n + 1;
  s2 -> pred[n] = no;

  s1 -> succ[l ? 1 : 0] = nd;

  return 0;
}

/**  typedef  ip_info  :  Type of the data placed on the exploration
                     stack of the function initial_prefix_automaton,
		     which is part of the canonicalization algorithm.
		     The first two fields give the current state of
		     the automaton being canonicalized as well as the
		     current depth. The third field is a mode field;
		     its value is IP_ROOT if the function is invoked
		     for the first time in the current
		     canonicalization operation, IP_FIRST if it is
		     called for the first time with the current pair
		     of state and depth, and IP_NEXT otherwise. The
		     value of the next fields is mode-dependent. If
		     mode == IP_ROOT, there is no relevant field. If
		     mode == IP_FIRST, there are two fields specifying
		     the origin index and the label of a transition to
		     be created in the prefix automaton. Otherwise,
		     there are two fields corresponding to the number
		     of transitions that have already been explored
		     and the index of the created state that
		     corresponds to the current pair of state and
		     depth.                                        **/

typedef struct {
  uint4  m, d;
  uint1  mode;
  union {
    struct {
      uint4  origin;
      uint1  label;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} ip_info;

#define  IP_ROOT   0x01
#define  IP_FIRST  0x02
#define  IP_NEXT   0x03

/**  prefix_automaton *initial_prefix_automaton(a, n, rm1)  :  This
                     function is part of the canonicalization 
                     algorithm. The automation being canonicalized
                     is *a, and the dimension of the vectors it
                     recognizes is n. The numeration base vectors is
                     (rm1 + 1). The automaton *a is assumed to be
                     deterministic and strongly normal.

                     The goal of this function is to create and
                     initialize a prefix automaton corresponding to
                     *a. In the case of success, the function returns
                     a pointer to a newly allocated prefix automaton.
                     In the case of insufficient memory, the function
                     returns a NULL pointer.                       **/
   
static prefix_automaton *initial_prefix_automaton(a, n, rm1)
  automaton *a;
  uint4      n, rm1;
{
  register prefix_automaton *pa;
  register hash_table       *ht;
  register stack            *st;
  register uint1             mode;
           ip_info           ip;
           uint4             m0;

  if (auto_i_state(a, 0, &m0) < 0)
    return NULL;

  pa = new_empty_prefix_automaton();
  if (!pa)
    return NULL;

  ht = hash__new_empty(compute_canon_hsize(auto_nb_states(a), n));
  if (!ht)
    {
      free_prefix_automaton(pa);
      return NULL;
    }

  st = stack__new_empty(ip_info);
  if (!st)
    {
      hash__free(ht, NULL, NULL);
      free_prefix_automaton(pa);
      return NULL;
    }

  ip.m    = m0;
  ip.d    = ZERO_INT4;
  ip.mode = IP_ROOT;

  if (stack__push(st, (void *) &ip) < 0)
    {
      stack__free(st);
      hash__free(ht, NULL, NULL);
      free_prefix_automaton(pa);
      return NULL;
    }

  while (!stack__is_empty(st))
    {
      mode = ((ip_info *) stack__top(st)) -> mode;
      if (((mode == IP_ROOT || mode == IP_FIRST) &&
	  initial_prefix_init(a, pa, n, ht, st) < 0) ||
	  (mode == IP_NEXT &&
	  initial_prefix_loop(a, pa, n, rm1, ht, st) < 0))
	{
	  bytes__prepare_free(2 * sizeof(uint4));
	  stack__free(st);
	  hash__free(ht, (void (*) (void *)) bytes__free,
		     (void (*) (void *)) uint4__free);
	  free_prefix_automaton(pa);
	  return NULL;
	}
    }

  bytes__prepare_free(2 * sizeof(uint4));
  stack__free(st);
  hash__free(ht, (void (*) (void *)) bytes__free,
	     (void (*) (void *)) uint4__free);

  return pa;
}

/**  int  initial_prefix_init(a, pa, n, ht, st)  :  This function is 
                     part of the canonicalization operation. The
                     automaton being canonicalized is *a, and the
                     prefix automaton currently being build is *pa.
                     The dimension of the vectors read by *a is n. A
                     hash table that stores pairs (m, d) composed of a
                     state index m and a depth d corresponding to an
                     already visited state is given by *ht. This hash
                     table associates to each pair the index of the
                     corresponding created state of *pa.

		     The goal of this function is to explore a part
		     of *a, starting from the pair of state index of
		     *a and current offset placed on top of the
		     exploration stack *st (supposed non-empty and
		     of mode equal to either IP_ROOT or IP_FIRST).
		     This routine proceeds transitively and updates
		     the hash table. It also updates the exploration
		     stack such that the next calls to the function
		     initial_prefix_loop will explore transitively
		     the remaining part of *a.

		     In the case of an error, this function returns
		     -1. Otherwise, it returns 0.                  **/

static int  initial_prefix_init(a, pa, n, ht, st)
  automaton        *a;
  prefix_automaton *pa;
  uint4             n;
  hash_table       *ht;
  stack            *st;
{
  register uint4   *v;
  register ip_info *ip;
  static   uint4    buf_uint4[2];
           void   **r;

  ip = (ip_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (ip -> mode != IP_ROOT && ip -> mode != IP_FIRST)
    return -1;
#endif  /* >= 1 */

  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = ip -> m;
  buf_uint4[1] = ip -> d;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      2 * sizeof(uint4), &r, &canon_ncolls, &canon_nins)
      < 0 || !r) 
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4,
      2 * sizeof(uint4), &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (prefix_add_new_state(pa, buf_uint4) < 0)
    return -1;

  *v = buf_uint4[0];

  if (ip -> d == n)
    {
      pa -> states[*v].origin = ip -> m;
      pa -> states[*v].next   = pa -> r_states;
      pa -> r_states          = *v;
    }

  if (ip -> mode == IP_ROOT)
    pa -> i_state = pa -> c_states = pa -> r_states = *v;
  else
    if (prefix_add_new_transition(pa, ip -> v.tr.origin, *v,
        ip -> v.tr.label) < 0)
      return -1;
  
  ip -> mode = IP_NEXT;
  ip -> v.st.k = ZERO_INT4;
  ip -> v.st.origin = *v;

  return 0;
}

/**  int  initial_prefix_loop(a, pa, n, rm1, ht, st)  :  This 
                     function is part of the canonicalization
		     operation. The automaton being canonicalized is
		     *a, and the prefix automaton currently being
		     build is *pa. The dimension of the vectors read
		     by *a is n, and the numeration base is (rm1 +
		     1). A hash table that stores pairs (m, d)
		     composed of a state index m and a depth d
		     corresponding to an already visited state is
		     given by *ht. This hash table associates to each
		     pair the index of the corresponding created state
		     of *pa.

		     The goal of this function is to explore a part of
		     *a, starting from the pair of state index of *a
		     and current depth placed on top of the
		     exploration stack *st (supposed non-empty and of
		     mode equal to IP_NEXT).  This routine proceeds
		     transitively and updates the hash table. It also
		     updates the exploration stack such that the next
		     calls to the function initial_prefix_init will
		     explore transitively the remaining part of *a.

		     In the case of an error, this function returns
		     -1. Otherwise, it returns 0.                  **/

static int  initial_prefix_loop(a, pa, n, rm1, ht, st)
  automaton        *a;
  prefix_automaton *pa;
  uint4             n, rm1;
  hash_table       *ht;
  stack            *st;
{
  register uint4    k, m, d, st1;
  register uint1    l;
  register ip_info *ip;
  register tran    *t;
  static   uint4    dest[2];
           uint4    nt;
	   void    *r;
	   ip_info  ip2;

  ip = (ip_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (ip -> mode != IP_NEXT)
    return -1;
#endif

  d = ip -> d;
  if (d >= n)
    {
      stack__pop(st, NULL);
      return 0;
    }

  m   = ip -> m;
  st1 = ip -> v.st.origin;

  if (auto_nb_out_transitions(a, m, &nt) < 0)
    return -1;

  while (ip -> v.st.k < nt)
    {
      k = ip -> v.st.k++;
      t = auto_transition(a, m, k);
      l = auto_transition_label_ptr(t, 1)[0];

      /* New instruction for RVAs */
      if (l == rm1+1)
	continue;
      /* End */
      
      if (l != 0 && l != rm1)
	continue;

      dest[0] = auto_transition_dest(t);
      dest[1] = d + 1;

      r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
      if (r)
	return prefix_add_new_transition(pa, st1, *((uint4 *) r), l);

      ip2.mode = IP_FIRST;
      ip2.m    = dest[0];
      ip2.d    = dest[1];
      ip2.v.tr.origin = st1;
      ip2.v.tr.label  = l;

      if (stack__push(st, (void *) &ip2) < 0)
	return -1;

      return 0;
    }
  
  stack__pop(st, NULL);
  return 0;
}

/**  void  free_prefix_automaton(pa)  :  This function is part of the
                     canonicalization operation. It frees the prefix
		     automaton *pa. This function does not report
		     errors.                                       **/

static void  free_prefix_automaton(pa)
  prefix_automaton *pa;
{
  register uint4  i, n;

#if LASH_CHECK_LEVEL >= 1
  if (!pa)
    return;
#endif  /* >= 1 */

  n = pa -> nb_states;
 
  for (i = 0; i < n; i++)
    {
      if (pa -> states[i].pred)
	resr__free_objects(pa -> states[i].pred, uint4,
	    pa -> states[i].nb_pred);
      if (pa -> states[i].states)
	resr__free_objects(pa -> states[i].states, uint4,
	    pa -> states[i].nb_states);
    }

  if (pa -> states)
    resr__free_objects(pa -> states, prefix_info, pa -> nb_states);

  resr__free_object(pa, prefix_automaton);
}

/**  typedef  re_info  :  Type of the data placed on the exploration
                     stack of the function reverse_explore. The
		     first field is the index of a state; the second
		     field is the number of incoming transitions to
		     that state that have already been followed.   **/

typedef struct {
  uint4  state, nb_transitions;
} re_info;

/**  int  reverse_explore(pa, si, v)  :  This function is part of the
                     canonicalization operation. Its goal is to
		     perform a backward exploration of the prefix
		     automaton *pa, starting from the state of index
		     si. All the states reached during the exploration
		     are assumed to have the flag field different from
		     v. For each such state s, the function assigns
		     the Boolean value v to s.flag. In the case of
		     insufficient memory, this function returns
		     -1. Otherwise, it returns 0.                  **/

static int  reverse_explore(pa, si, v)
  prefix_automaton *pa;
  uint4             si;
  int               v;
{
  register stack   *st;
           re_info  r;

  st = stack__new_empty(re_info);
  if (!st)
    return -1;

  r.state          = si;
  r.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &r) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &r);
      if (!r.nb_transitions)
	{
	  if (!((pa -> states[r.state].flag ^ v) & 0x01))
	    continue;
	  pa -> states[r.state].flag = v;
	}

      if (r.nb_transitions >= pa -> states[r.state].nb_pred)
	continue;
      
      si = pa -> states[r.state].pred[r.nb_transitions++];
      
      if (stack__push(st, &r) < 0)
	{
	  stack__free(st);
	  return -1;
	}
      
      r.state = si;
      r.nb_transitions = ZERO_INT4;
      if (stack__push(st, &r) < 0)
	{
	  stack__free(st);
	  return -1;
	}
    }

  stack__free(st);

  return 0;
}

/**  typedef  de_info  :  Type of the data placed on the exploration
                     stack of the function direct_explore. The
		     first field is the index of a state; the second
		     field is the number of outgoing transitions from
		     that state that have already been followed.   **/

typedef struct {
  uint4  state, nb_transitions;
} de_info;

/**  int  direct_explore(pa)  :  This function is part of the
                     canonicalization operation. Its goal is to
		     perform a forward exploration of the prefix
		     automaton *pa, starting from the initial state
		     and restricting the exploration to the states
		     whose flag field is true. For each such state s,
		     this function sets s.ptr_ready and s.absent to
		     0. In the case of insufficient memory, this
		     function returns -1. Otherwise, it returns 0. **/

static int  direct_explore(pa)
  prefix_automaton *pa;
{
  register stack     *st;
  register bit_table *bt;
  register uint4      s = 0;
  register int        found;
           de_info    d;

  st = stack__new_empty(de_info);
  if (!st)
    return -1;

  bt = bit__new_empty(pa -> nb_states);
  if (!bt)
    {
      stack__free(st);
      return -1;
    }

  d.state          = pa -> i_state;
  d.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &d) < 0)
    {
      bit__free(bt);
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &d);
      if (!d.nb_transitions)
	{
	  if (bit__member(bt, d.state))
	    continue;
	  bit__add(bt, d.state);
	  pa -> states[d.state].ptr_ready = 
              pa -> states[d.state].absent = 0;
	}
      
      for (found = 0; !found && d.nb_transitions < 2;)
	{
	  s = pa -> states[d.state].succ[d.nb_transitions++];
	  if (s != d.state && pa -> states[s].flag)
	    found = 1;
	}

      if (!found)
	continue;

      if (stack__push(st, &d) < 0)
	{
	  bit__free(bt);
	  stack__free(st);
	  return -1;
	}

      d.state = s;
      d.nb_transitions = ZERO_INT4;
      if (stack__push(st, &d) < 0)
	{
	  bit__free(bt);
	  stack__free(st);
	  return -1;
	}
    }

  bit__free(bt);
  stack__free(st);

  return 0;
}

/**  void  remove_pred_state(pa, si, pi)  :  This function is part of
                     the canonicalization operation. Its goal is to
                     remove the state index pi from the list of
                     predecessors of the state of index si in the
		     prefix automaton *pa. This function does not
		     report errors.                                **/

static void  remove_pred_state(pa, si, pi)
  prefix_automaton *pa;
  uint4             si, pi;
{
  register uint4  i, n;
  register int    flag;

  n = pa -> states[si].nb_pred;

  for (i = 0, flag = 0; i < n; i++)
    if (pa -> states[si].pred[i] == pi)
      flag = 1;
    else
      if (flag)
	pa -> states[si].pred[i - 1] = pa -> states[si].pred[i];

  if (flag)
    {
      pa -> states[si].pred = resr__resize_objects(
          pa -> states[si].pred, uint4, n - 1, n);
      pa -> states[si].nb_pred = n - 1;
    }
}

/**  typedef rp_info  :  Type of the data placed on the exploration
                     stack of the function refine_prefix_automaton.
		     The first field gives the current state of the
		     prefix automaton being explored. The second field
		     is a mode field; its value is RP_FIRST if the
		     function is invoked for the first time with the
		     current pair of state and depth, and RP_NEXT
		     otherwise. The values of the other fields are
		     mode-dependent. If mode == RP_FIRST, there are
		     two fields specifying the origin index and the
		     label of a transition to be created. Otherwise,
		     there are two fields corresponding to the number
		     of transitions that have already been explored
		     and the index of the newly created state that
		     corresponds to the current state.             **/

typedef struct {
  uint4  m;
  uint1  mode;
  union {
    struct {
      uint4  origin;
      uint1  label;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} rp_info;

#define  RP_FIRST  0x02
#define  RP_NEXT   0x03

/**  int  refine_prefix_automaton(pa, ns, nt, d, n)  :  This function 
                     is part of the canonicalization operation. The
		     prefix automaton associated to the automaton
		     being canonicalized is *pa. The dimension of the
		     vectors read by the automaton is n. The goal of
		     this function is to create a new state in *pa,
		     and then to modify the destination of the
		     transition of index nt outgoing from the state of
		     index ns (whose depth is d) so as to make it
		     equal to the new state.  Then, the function
		     copies recursively the part of *pa that can be
		     reached from the original destination to the new
		     one. The function returns 0 in case of success,
		     and -1 in case of insufficient memory.        **/

static int  refine_prefix_automaton(pa, ns, nt, d, n)
  prefix_automaton *pa;
  uint4             ns, nt, d, n;
{
  register uint4       ds, mode;
  register stack      *st;
  register hash_table *ht;
           rp_info     rp;
           uint4       depth;

  st = stack__new_empty(rp_info);
  if (!st)
    return -1;

  ht = hash__new_empty(compute_canon_hsize(pa -> nb_states, n));
  if (!ht)
    {
      stack__free(st);
      return -1;
    }

  ds = pa -> states[ns].succ[nt];
  remove_pred_state(pa, ds, ns);

  rp.m = ds;
  rp.mode = RP_FIRST;
  rp.v.tr.origin = ns;
  rp.v.tr.label  = nt;
  
  depth = d + 1;

  if (stack__push(st, (void *) &rp) < 0)
    {
      hash__free(ht, NULL, NULL);
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      mode = ((rp_info *) stack__top(st)) -> mode;
      if ((mode == RP_FIRST && (refine_prefix_init(pa, &depth, n, ht, 
          st) < 0)) ||
          (mode == RP_NEXT  && (refine_prefix_loop(pa, &depth, ht, st)
          < 0)))
	{
	  hash__free(ht, (void (*)(void *)) uint4__free, 
              (void (*)(void *)) uint4__free);
	  stack__free(st);
	  return -1;
	}
    }

  hash__free(ht, (void (*)(void *)) uint4__free, 
              (void (*)(void *)) uint4__free);

  stack__free(st);
  return 0;
}

/**  int  refine_prefix_init(pa, pd, n, ht, st)  :  This function is
                     part of the canonicalization operation. The
		     prefix automaton being constructed is *pa. The
		     depth of the state on top of the exploration
		     stack *st is *pd. The dimension of the vectors
		     read by the automaton being canonicalized is n. A
		     hash table containing the indices of the states
		     that have already been explored and associating
		     them with the indices of the corresponding 
                     created states is given by *ht.

		     The goal of this function is to add to the
		     prefix automaton *pa a new state corresponding
		     to the state placed on top of the exploration
		     stack (supposed non-empty and of mode equal to
		     RP_FIRST), inserting the corresponding entry
		     in *ht. It then updates the exploration stack
		     and the value of *pd such that the next calls
		     to the function refine_prefix_loop will create
		     transitively all the successor states of the
		     new state as well as their outgoing transitions.

                     In the case of success, this function returns 0.
		     Otherwise, it returns -1.                     **/

static int  refine_prefix_init(pa, pd, n, ht, st)
  prefix_automaton *pa;
  uint4            *pd, n;
  hash_table       *ht;
  stack            *st;
{
  register rp_info *rp;
  register uint4   *v, *p;
  static   uint4    buf;
           void   **r;

  rp = (rp_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (rp -> mode != RP_FIRST)
    return -1;
#endif  /* >= 1 */

  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf = rp -> m;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) &buf, sizeof(uint4),
      &r, &canon_ncolls, &canon_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) &buf, sizeof(uint4),
      &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;
  
  if (prefix_add_new_state(pa, &buf) < 0)
    return -1;

  *v = buf;

  if (prefix_add_new_transition(pa, rp -> v.tr.origin, *v,
      rp -> v.tr.label) < 0)
    return -1;

  if (*pd >= n)
    {
      pa -> states[*v].ptr_ready = pa -> states[*v].flag = 0; 
      pa -> states[*v].ptr       = ZERO_INT4;
      pa -> states[*v].origin    = pa -> states[rp -> m].origin;
      if ((pa -> states[*v].computed = 
          pa -> states[rp -> m].computed))
	{
	  if (pa -> states[rp -> m].nb_states)
	    {
	      p = resr__new_objects(uint4, 
                  pa -> states[rp -> m].nb_states);
	      if (!p)
		return -1;
	      pa -> states[*v].states = p;
	      memcpy(p, pa -> states[rp -> m].states,
                  pa -> states[rp -> m].nb_states * sizeof(uint4));
	    }
	  else
	    pa -> states[*v].states  = NULL;
	  pa -> states[*v].nb_states = 
              pa -> states[rp -> m].nb_states;
	  pa -> states[*v].next = pa -> c_states;
	  pa -> c_states = *v;
	}
      else
	{
	  pa -> states[*v].states    = NULL;
	  pa -> states[*v].nb_states = ZERO_INT4;
          pa -> states[*v].next = pa -> r_states;
	  pa -> r_states = *v;
	}
    }

  rp -> mode   = RP_NEXT;
  rp -> v.st.k = ZERO_INT4;
  rp -> v.st.origin = *v;

  return 0;
}

/**  int  refine_prefix_loop(pa, pd, ht, st)  :  This function is part
                     of the canonicalization operation. The prefix
		     automaton being constructed is *pa. The depth of
		     the state on top of the exploration stack *st is
		     *pd. A hash table containing the indices of the
		     states that have already been explored and 
                     associating them with the indices of the 
                     corresponding created states is given by *ht.

                     This function explores the outgoing transitions
		     from the state of *pa placed on top of the
		     exploration stack *st (supposed non-empty and
		     of mode equal to RP_NEXT), updating the stack
		     such that the next calls to this function and to
		     the function refine_prefix_init will create
		     transitively all the successor states of the
		     new state as well as their outgoing transitions.

                     In the case of success, this function returns 0.
		     Otherwise, it returns -1.                     **/

static int  refine_prefix_loop(pa, pd, ht, st)
  prefix_automaton *pa;
  uint4            *pd;
  hash_table       *ht;
  stack            *st;
{
  register uint4    m, k, st1;
  register int      found;
  register rp_info *rp;
           uint4    s = 0;
           rp_info  rp2;
           void    *r;

  rp = (rp_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (rp -> mode != RP_NEXT)
    return -1;
#endif  /* >= 1 */

  m   = rp -> m;
  st1 = rp -> v.st.origin;

  for (found = 0; !found && rp -> v.st.k < 2;)
    {
      s = pa -> states[m].succ[rp -> v.st.k++];
      if (s != m)
	found = 1;
    }

  if (!found)
    {
      --*pd;
      stack__pop(st, NULL);
      return 0;
    }

  k = rp -> v.st.k - 1;

  r = hash__lookup_bytes(ht, (uint1 *) &s, sizeof(uint4));
  if (r)
    return prefix_add_new_transition(pa, st1, *((uint4 *) r), k);

  rp2.mode = RP_FIRST;
  rp2.m    = s;
  rp2.v.tr.origin = st1;
  rp2.v.tr.label  = k;
  ++*pd;

  if (stack__push(st, (void *) &rp2) < 0)
    return -1;
  
  return 0;
}

/**  int  match_transitions(pa, a, s, l, d, n, rm1)  :  This function 
                     is the inner core of the canonicalization
		     algorithm.  The prefix automaton that is explored
		     is *pa.  The automaton being canonicalized is
		     *a. The depth of the current state is d. The
		     dimension of the vectors read by the automaton is
		     n.  The base is rm1 + 1. The goal of this
		     function is to check the outgoing transition of
		     the state of index s of *pa that is labeled by l
		     * (numeration_base - 1), and determine whether
		     there exists a matching transition of *a. As a
		     side effect of this check, the prefix automaton
		     *pa might be refined during the operation. The
		     function returns 1 if a matching transition has
		     been found, 0 if it has not, and -1 in the case
		     of insufficient memory.                       **/

static int  match_transitions(pa, a, s, l, d, n, rm1)
  prefix_automaton *pa;
  automaton        *a;
  uint4             s, l, d, n;
  uint1             rm1;
{
  register uint4  sa, i, ds;
  register int    c;
  register tran  *t = NULL;
           uint4  m; 

  sa = pa -> states[s].ptr;
  if (auto_nb_out_transitions(a, sa, &m) < 0)
    return -1;

  for (c = i = 0; i < m; i++)
    {
      t = auto_transition(a, sa, i);
      
      /* New instruction for RVAs */
      if (auto_transition_label_ptr(t, 1)[0] == rm1+1)
	continue;
      /* End */

      if ((!l && !(auto_transition_label_ptr(t, 1)[0])) ||
           (l && (auto_transition_label_ptr(t, 1)[0] == rm1)))
	{
	  c = 1;
	  break;
	}
    }

  ds = pa -> states[s].succ[l];

  if (c)
    {
      if (!(pa -> states[ds].ptr_ready) && !(pa -> states[ds].absent))
	{
	  pa -> states[ds].ptr_ready = 1;
	  pa -> states[ds].ptr = auto_transition_dest(t);
	  return 1;
	}

      if (pa -> states[ds].ptr_ready && (pa -> states[ds].ptr ==
          auto_transition_dest(t)))
	return 0;
    }
  else
    if (!(pa -> states[ds].ptr_ready))
      {
	pa -> states[ds].absent = 1;
	return 0;
      }

  if (refine_prefix_automaton(pa, s, l, d, n) < 0)
    return -1;
  
  return 0;
}

/**  typedef fs_info  :  Type of the data placed on the exploration
                     stack of the function fill_step. The first
                     field contains the index of a state of the prefix
		     automaton being explored; the second field
		     contains the number of outgoing transitions that
		     have already been explored from that state.   **/

typedef struct {
  uint4  state, nb_transitions;
} fs_info;

/**  int  fill_step(pa, a, n, ps, rm1)  :  This function is part of
                     the canonicalization operation. Its goal is to
		     explore the product of the prefix automaton *pa
		     by the automaton being canonicalized *a. The
		     dimension of the vectors read by *a is n. The
		     base is rm1 + 1. The exploration starts at the
		     state *ps of *a, and its effects are to modify
		     *ps and to possibly refine *pa. In the case of
		     insufficient memory, this function returns
		     -1. Otherwise, it returns 0.                  **/

static int  fill_step(pa, a, n, ps, rm1)
  prefix_automaton *pa;
  automaton        *a;
  uint4             n, *ps;
  uint1             rm1;
{
  register stack   *st;
  register uint4    d, s = 0;
  register int      found;
           fs_info  fs;

  if (!(pa -> states[pa -> i_state].flag))
    return 0;

  if (direct_explore(pa) < 0)
    return -1;

  st = stack__new_empty(fs_info);
  if (!st)
    return -1;

  d = ZERO_INT4;
  fs.state = pa -> i_state;
  fs.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &fs) < 0)
    {
      stack__free(st);
      return -1;
    }

  pa -> states[pa -> i_state].ptr = *ps;
  pa -> states[pa -> i_state].ptr_ready = 1;

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &fs);

      if (d >= n)
	{
	  *ps = pa -> states[fs.state].ptr;
	  d--;
	  continue;
	}

      for (found = 0; !found && fs.nb_transitions < 2;)
	{
	  s = pa -> states[fs.state].succ[fs.nb_transitions++];
	  if (s != fs.state && pa -> states[s].flag)
	    found = 1;
	}
      
      if (!found)
	{
	  d--;
	  continue;
	}

      if (stack__push(st, (void *) &fs) < 0)
	{
	  stack__free(st);
	  return -1;
	}

      switch(match_transitions(pa, a, fs.state, 
          fs.nb_transitions - 1, d, n, rm1))
	{
	case 0 :
	  break;

	case 1:
	  fs.state = s;
	  fs.nb_transitions = ZERO_INT4;
          d++;

	  if (stack__push(st, (void *) &fs) < 0)
	    {
	      stack__free(st);
	      return -1;
	    }
	  break;

	default :
	  stack__free(st);
	  return -1;
	}
    }

  stack__free(st);
  return 0;
}

/**  int  fill_prefix_automaton(pa, a, n, rm1)  :  This function is 
                     part of the canonicalization operation. Its goal
		     is to compute the values of the fields states and
		     nb_states at each terminal state of the prefix
		     automaton *pa. The automaton that is being
		     canonicalized is *a. The dimension of the vectors
		     read by *a is n, and the numeration base is (rm1
		     + 1). In the case of insufficient memory, this
		     function returns -1. Otherwise, it returns 0. **/

static int  fill_prefix_automaton(pa, a, n, rm1)
  prefix_automaton *pa;
  automaton        *a;
  uint4             n, rm1;
{
  register bit_table   *b1;
  register uint4        si, i, *p;
           uint4        sa;

  b1 = bit__new_empty(auto_nb_states(a));  
  if (!b1)
    return -1;

  while (pa -> r_states != pa -> i_state)
    {
      si = pa -> r_states;
      pa -> r_states = pa -> states[si].next;

      if (reverse_explore(pa, si, 1) < 0)
	{
	  bit__free(b1);
	  return -1;
	}

      for (sa = pa -> states[si].origin;;)
	{
          if (bit__member(b1, sa))
	    {
	      pa -> states[si].next = pa -> c_states;
	      pa -> c_states = si;
	      pa -> states[si].computed = 1;
	      if (reverse_explore(pa, si, 0) < 0)
		{
		  bit__free(b1);
		  return -1;
		}
	      for (i = 0; i < pa -> states[si].nb_states; i++)
		bit__remove(b1, pa -> states[si].states[i]);

	      break;
	    }

	  bit__add(b1, sa);
	  p = resr__resize_objects(pa -> states[si].states, uint4,
             pa -> states[si].nb_states + 1, 
             pa -> states[si].nb_states);
	  if (!p)
	    {
	      bit__free(b1);
	      return -1;
	    }

	  pa -> states[si].states = p;
	  pa -> states[si].states[pa -> states[si].nb_states++] = sa;

	  if (fill_step(pa, a, n, &sa, rm1) < 0)
	    {
	      bit__free(b1);
	      return -1;
	    }
	}
    }

  bit__free(b1);
  return 0;
}

/**  int  update_automaton(a, pa, n, rm1)  :  This function is 
                     part of the canonicalization operation. Its goal
                     is to perform the canonicalization of the
                     automaton *a based on the informations contained
                     in the prefix automaton *pa. The dimension of the
                     vectors read by *a is n. The numeration base is
                     (rm1 + 1). In the case of insufficient memory,
                     this function returns -1. Otherwise, it returns
                     0.                                            **/

static int  update_automaton(a, pa, n, rm1)
  automaton        *a;
  prefix_automaton *pa;
  uint4             n, rm1;
{
  register uint4  i, j, k, *p;
  static   uint1  digit[2];

  if (!(pa -> nb_states))
    return 0;

  p = resr__new_objects(uint4, pa -> nb_states);
  if (!p)
    return -1;

  for (i = 0; i < pa -> nb_states; i++)
    if (auto_add_new_state(a, p + i) < 0)
      {
	resr__free_objects(p, uint4, pa -> nb_states);
	return -1;
      }
  
  digit[0] = ZERO_INT1;
  digit[1] = (uint1) rm1;

  for (i = 0; i < pa -> nb_states; i++)
    for (j = 0; j < 2; j++)
      if ((pa -> states[i].succ[j] != i) && 
          (auto_add_new_transition(a, p[i], 
          p[pa -> states[i].succ[j]], 1, digit + j) < 0))
	{
	  resr__free_objects(p, uint4, pa -> nb_states);
	  return -1;
	}

  for (i = pa -> c_states; i != pa -> i_state; 
      i = pa -> states[i].next)
    for (j = 0; j < pa -> states[i].nb_states; j++)
      {
	k = pa -> states[i].states[j];
	
	if (auto_add_new_transition(a, p[i], k, 0, NULL) < 0)
	  {
	    resr__free_objects(p, uint4, pa -> nb_states);
	    return -1;
	  } 
      }

  auto_remove_i_states(a);
  if (auto_add_new_i_state(a, p[pa -> i_state]) < 0)
    {
      resr__free_objects(p, uint4, pa -> nb_states);
      return -1;
    }

  resr__free_objects(p, uint4, pa -> nb_states);
  return 0;
}

/**  int  canonicalize(a, n, rm1)  :  This function takes as input a
                     deterministic and strongly normal finite-state
                     automaton *a that is supposed to accept _some_
                     sequential encodings of integer vectors of
                     dimension n in base (rm1 + 1). (More precisely,
                     for each vector, either no encoding is accepted,
                     of at least all the encodings whose length
                     exceeds a given value are accepted.) The purpose
                     of this function is to transform *a so as to make
                     it accept _all_ the encodings of those vectors.
                     In the case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  canonicalize(a, n, rm1)
  automaton *a;
  uint4      n, rm1;
{
  register  prefix_automaton *pa;
  register  int               rc;

  if (!n || !auto_nb_i_states(a))
    return 0;

  pa = initial_prefix_automaton(a, n, rm1);
  if (!pa)
    return -1;

  if (fill_prefix_automaton(pa, a, n, rm1) < 0)
    {
      free_prefix_automaton(pa);
      return -1;
    }

  rc = update_automaton(a, pa, n, rm1);
  free_prefix_automaton(pa);

  return rc;
}

/****  Public visible functions.                                 ****/

/**  void  rva_set_proj_hsize(s)  :  Sets the size of the hash 
                     tables used by the projection algorithms to s.
                     This function does not report errors.         **/

void  rva_set_proj_hsize(s)
  uint4 s;
{
  if (s)
    canon_hsize = s;
}

/**  uint8  rva_get_proj_ncolls()  :  Returns the number of collisions
                     observed in the hash tables used by the
                     projection algorithms.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_proj_ncolls()
{
  return canon_ncolls;
}
#endif  /* >= 1 */

/**  void  rva_reset_proj_ncolls()  :  Resets the number of collisions
                     observed in the hash tables used by the
                     projection algorithms.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_proj_ncolls()
{
  canon_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  rva_get_proj_nins()  :  Returns the number of insertions
                     performed in the hash tables used by the
                     projection algorithms.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_proj_nins()
{
  return canon_nins;
}
#endif  /* >= 1 */

/**  void  rva_reset_proj_nins()  :  Resets the number of insertions
                     performed in the hash tables used by the
                     projection algorithms.  This function does not
                     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_proj_nins()
{
  canon_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  rva *rva_projection(rv, p)  :  Computes a RVA representing the
                     projection of the set represented by the RVA *rv
                     over all the vector components but the one of
                     index p (the first vector component has the index
                     0). The dimension of *rv must be at least equal
                     to 1. This function is only currently implemented
                     for RVAs that operate serially.
 
                     This function does not modify *rv, and returns
                     (in the case of success) a pointer to a newly
                     allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_BAD_VALUE  : Invalid value of p.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_projection(rv, p)
  rva   *rv;
  uint4  p;
{
  register rva       *rvr;
  register int        empty;

  diag__enter("rva_projection", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(rv -> dim))
    diag__fail(LASH_ERR_DIMENSION, NULL);
 
  if (p >= rv -> dim)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL) ||
      !(rv -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (rv -> dim == 1)
    {
      rvr = rva_create_empty(rv -> base, 0);
      if (!rvr)
	diag__fail(lash_errno, NULL);

      empty = rva_is_empty(rvr);
      if (empty < 0)
	{
	  empty = lash_errno;
	  rva_free(rvr);
	  diag__fail(empty, NULL);
	}

      rvr -> universal = !empty;

      diag__return(rvr);
    }

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = rv -> dim - 1;
  rvr -> base = rv -> base;
  rvr -> properties = rv -> properties;

  rvr -> automaton  = auto_seq_projection_separ(rv -> automaton, 
      rv -> dim, p, &rv -> base);

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0 ||
      canonicalize(rvr -> automaton, rvr -> dim, rv -> base - 1) < 0)
    {
      resr__free_object(rvr, rva);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0)
    {
      rva_free(rvr);
      diag__fail(lash_errno, NULL);
    }

  diag__return(rvr);
}

/**  rva *rva_modulo_projection(rv, p, u)  :  Computes a RVA 
                     representing the projection of the set 
		     represented by the RVA *rv over all the vector
		     components but the ones that are equal to u
		     modulo p. The dimension of *rv must be at least
		     equal to 1.  This function is currently only
		     implemented for RVAs that operate serially, and
		     whose dimension is an integer multiple of p.
 
                     This function does not modify *rv, and returns
                     (in the case of success) a pointer to a newly
                     allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_modulo_projection(rv, p, u)
  rva   *rv;
  uint4  p, u;
{
  register rva       *rvr;
  register int        empty;

  diag__enter("rva_modulo_projection", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(rv -> dim))
    diag__fail(LASH_ERR_DIMENSION, NULL);
 
  if (!p || p > rv -> dim || u >= p || (rv -> dim % p))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL) ||
      !(rv -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim  = (rv -> dim / p) * (p - 1);

  if (!(rvr -> dim))
    {
      rva_free(rvr);

      rvr = rva_create_empty(rv -> base, 0);
      if (!rvr)
	diag__fail(lash_errno, NULL);

      empty = rva_is_empty(rvr);
      if (empty < 0)
	{
	  empty = lash_errno;
	  rva_free(rvr);
	  diag__fail(empty, NULL);
	}

      rvr -> universal = !empty;

      diag__return(rvr);
    }

  rvr -> base = rv -> base;
  rvr -> properties = rv -> properties;
  rvr -> automaton  = auto_seq_projection_separ(rv -> automaton, 
      p, u, &rv -> base);

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0 ||
      canonicalize(rvr -> automaton, rvr -> dim, rv -> base - 1) < 0)
    {
      resr__free_object(rvr, rva);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0)
    {
      rva_free(rvr);
      diag__fail(lash_errno, NULL);
    }

  diag__return(rvr);
}

/**  rva *rva_mutliple_projection(rv, var)  :  Computes a RVA 
                     representing the projection of the set 
		     represented by the RVA *rv over the vector
		     components specified by the table *var
                     (var[i] must be nonzero in order to keep
                     the (i+1)-th variable). The dimension of *rv
                     must be at least equal to 1, and must match the
                     size of the array *var  This function is 
                     currently only implemented for RVAs that operate
                     serially.
 
                     This function does not modify *rv, and returns
                     (in the case of success) a pointer to a newly
                     allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_multiple_projection(rv, var)
  rva       *rv;
  int       *var;
{
  register rva       *rvr;
  register uint4      i;
  register int        empty;

  diag__enter("rva_multiple_projection", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(rv -> dim))
    diag__fail(LASH_ERR_DIMENSION, NULL);
 
  if (!var)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL) ||
      !(rv -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  rvr = resr__new_object(rva);
  if (!rvr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rvr -> dim = 0;
  for (i = 0 ; i < rv -> dim ; i++)
    if (var[i])
      rvr -> dim++;

  if (!(rvr -> dim))
    {
      rva_free(rvr);

      rvr = rva_create_empty(rv -> base, 0);
      if (!rvr)
	diag__fail(lash_errno, NULL);

      empty = rva_is_empty(rvr);
      if (empty < 0)
	{
	  empty = lash_errno;
	  rva_free(rvr);
	  diag__fail(empty, NULL);
	}

      rvr -> universal = !empty;

      diag__return(rvr);
    }

  rvr -> base = rv -> base;
  rvr -> properties = rv -> properties;
  rvr -> automaton  = auto_seq_multi_projection_separ
    (rv -> automaton, rv -> dim, var, &rv -> base);

  if (!(rvr -> automaton))
    {
      resr__free_object(rvr, rva);
      diag__fail(lash_errno, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0 ||
      canonicalize(rvr -> automaton, rvr -> dim, rv -> base - 1) < 0)
    {
      resr__free_object(rvr, rva);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (auto_determinize(rvr -> automaton) < 0 ||
      auto_convert_to_weak(rvr -> automaton) < 0 ||
      auto_minimize(rvr -> automaton) < 0)
    {
      rva_free(rvr);
      diag__fail(lash_errno, NULL);
    }

  diag__return(rvr);
}

/**  int  rva_canonicalize(rv)  :  This function takes as input a
                     RVA restricted to the additive theory of real
		     and integer variables. The purpose of this
		     function is to transform *rv so as to make it
		     accept _all_ the encodings of the vectors that
		     it represents.

		     This function is for internal use of the RVA
		     package, and should not be called by external
		     applications !

                     This function modifies *rv, and returns 0 in
		     the case of success. In the case of an error, 
		     it returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_BAD_TYPE   : Bad type of RVA(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

int rva_canonicalize(rv)
     rva   *rv;
{
  diag__enter("rva_canonicalize", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!(rv -> dim))
    diag__fail(LASH_ERR_DIMENSION, -1);
 
  if (!(rv -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, -1);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (canonicalize(rv -> automaton, rv -> dim, rv -> base - 1) < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (auto_determinize(rv -> automaton) < 0 ||
      auto_convert_to_weak(rv -> automaton) < 0 ||
      auto_minimize(rv -> automaton) < 0)
    {
      rva_free(rv);
      diag__fail(lash_errno, -1);
    }

  diag__return(0);
}

/****  End of rva-projection.c  ****/
