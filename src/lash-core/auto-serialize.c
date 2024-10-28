/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-serialize.c  :  Serialization and unserialization of   **/
/**                         automata (NDD and RVA).                **/
/**                                                                **/
/**        09/29/00  :  Creation. (SJ)                             **/
/**        04/12/01  :  Reorganization. (SJ)                       **/
/**        05/01/01  :  Integration in the LASH package. (SJ)      **/
/**        09/26/01  :  Serialization of automata. (SJ)            **/
/**        07/29/02  :  Reorganization. (BB)                       **/
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

#include <string.h>
#include "auto.h"
#include "auto-serialize.h"
#include "datastruct.h"
#include "resource.h"
#include "diag.h"

/****  Global variables.                                         ****/

/**  auto_unser_ncolls  :  Number of collisions observed in the   
                     hash table used by the synchronization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_unser_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_unser_nins  :  Number of insertions performed in the   
                     hash table used by the synchronization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_unser_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static int  unser_generate(automaton *, automaton *, uint4,
                      uint1 *, uint4, uint4 *, hash_table *);
static int  unser_generate_loop(automaton *, automaton *,
                      uint4, uint1 *, hash_table *, stack *, uint1 *);
static int  unser_explore(automaton *, automaton *, uint4, uint4, 
			  uint4, uint4, uint1 *, uint1 *, 
			  hash_table *, stack *);

/****  Private functions.                                        ****/

/** typedef  unser_info  :  Type of data placed on the exploration
                     stack of the function unser_generate. The
		     field m gives the current state of the automaton
		     being unserialized, whose offset need to be equal
		     to zero. The second field is a mode
		     field ; its value is US_ROOT if the function is
		     invoked for the first time in the current
		     unserialization process or US_NEXT otherwise.
		     The values of the next fields are mode-dependent.
		     If mode == PG_ROOT, there is one field that gives
                     a pointer to a location to which the function
                     should return the index of the topmost created
                     state. Otherwise, there are two fields
		     specifying the origin and the label of a
		     transition to be created.                     **/

typedef struct {
  uint4  m;
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1 *label;
    } tr;
  } v;
} unser_info;

#define  US_ROOT   0x01
#define  US_NEXT   0x02

/**  int  unser_generate(a, ar, depth, separ, init, mp, ht)  :
                     This routine is part of the unserialization
                     operation. The automaton being unserialized is
		     *a that is supposed to be in strong normal form,
		     and the partially computed automaton is *ar.
		     The value depth tells the algorithm the number
		     of components in the set represented by the
		     automaton *a, which indicates the depth where
		     the exploration for a successor state must stop.
		     The value *separ specifies the label of the
		     separator that demarcates an area in the
		     structure of *a (it can be NULL, in which case
		     there is no separator). ht is a hash table
		     associating a state index of *ar to each state
		     index of *a at a depth equals to zero.

                     The goal of this function is to generate a
                     part of *ar, starting from the state index
		     init of *a. This routine proceeds transitively,
		     updates the hash table, and sets accepting
		     states. The index of the state of *ar
		     corresponding to the index init is returned
		     in *mp.

		     If *a is not in strong normal form, this
		     functions returns -2. If a memory lack occurs,
		     it returns -1. Otherwise, if returns 0.       **/

static int  unser_generate(a, ar, depth, separ, init, mp, ht)
  automaton  *a, *ar;
  uint1      *separ;
  uint4       depth, init, *mp;
  hash_table *ht;
{
  register stack      *st;
  register uint1       na, *label;
  register int         res;
           unser_info  p;

  na = auto_alphabet_nbytes(ar);

  /* Pushes a request for a new root */

  st = stack__new_empty(unser_info);
  if (!st)
    return -1;

  p.m              = init;
  p.mode           = US_ROOT;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0 ||
      !(label = resr__new_objects(uint1, na)))
    {
      stack__free(st);
      return -1;
    }

  /* Exploration */

  while (!stack__is_empty(st))
    {
      res = unser_generate_loop(a, ar, depth, separ, ht, st, label);
      if (res < 0)
        {
	  /* Releases the memory occupied by the labels on the stack 
	   */
	  while (!stack__is_empty(st)) 
	    {
	      p = *((unser_info *) stack__top(st));
	      stack__pop(st, NULL);
	      if (p.mode == US_ROOT)
		resr__free_objects(p.v.tr.label, uint1, na);
	    }	  
          stack__free(st);
	  resr__free_objects(label, uint1, na);
          return res;
	}
    }

  /* Ressources freeing */
  
  stack__free(st);
  resr__free_objects(label, uint1, na);
  return 0;
}

/**  int  unser_generate_loop(a, ar, depth, separ, ht, st, label) :
                     This function is a part of the unserialization
		     algorithm. The automaton being unserialized is
		     *a ; the partially computed product is in *ar,
		     and a hash table associating a state index of
		     *ar to each state at a depth equals to zero
		     is given by *ht. The value depth tells the
		     algorithm the number of components in the set
		     represented by the automaton *a, which indicates
		     the depth where the exploration for a successor
		     state must stop. The value *separ specifies the
		     label of the separator that demarcates an area
		     in the structure of *a. The value label is a
		     pointer to a memory area wide enough to contain
		     a transition label of *ar.

                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the state
                     placed on top of the exploration stack *st
		     (supposed non-empty), inserting the corresponding
		     entry in the hash table *ht. It then pushes on
		     the exploration stack all the successor states of
		     the new state as well as their outgoing
		     transitions.

		     If *a is not in strong normal form, this
		     functions returns -2. If a memory lack occurs,
		     it returns -1. Otherwise, if returns 0.       **/

static int  unser_generate_loop(a, ar, depth, separ, ht, st, label)
     automaton   *a, *ar;
     uint4        depth;
     uint1       *separ;
     hash_table  *ht;
     stack       *st;
     uint1       *label;
{
  register uint4       *v;
  register unser_info   p;
  register uint1        na;
           uint4        m, buf_uint4;
           void       **r;

  na = auto_alphabet_nbytes(ar);

  /* Gets the state placed on the top of the stack */

  p = *((unser_info *) stack__top(st));
  stack__pop(st, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (p.mode !=  US_ROOT && p.mode != US_NEXT)
    return -1;
#endif  /* >= 1 */
  
  /* Checks whether this state has already been generated */

  buf_uint4 = p.m;
  r = hash__lookup_bytes(ht, (uint1 *) &buf_uint4, sizeof(uint4));

  if (!r)
    {
      /* New state : inserts this state in the hash table */

#if LASH_CHECK_LEVEL >= 1
      if (hash__insert_bytes(ht, (uint1 *) &buf_uint4, sizeof(uint4),
	  &r, &auto_unser_ncolls, &auto_unser_nins) < 0 || !r)
#else
      if (hash__insert_bytes(ht, (uint1 *) &buf_uint4, sizeof(uint4),
          &r) < 0 || !r)
#endif  /* >= 1 */
	{
	  return -1;
	}

      /* Creates a new state in *ar */

      v = resr__new_object(uint4);
      if (!v)
	return -1;

      *r = (void *) v;

      if (auto_add_new_state(ar, &buf_uint4) < 0)
	return -1;

      m = *v = buf_uint4;

      if (auto_accepting_state(a, p.m))
	auto_mark_accepting_state(ar, m);

      if (p.mode == US_ROOT)
	{
	  /* Tells the calling function the index of the state
	     of *ar corresponding to the root of the current
	     unserialization process. */
	  if (p.v.return_state)
	    *(p.v.return_state) = m;
	}
      else
	{
	  /* Not a root : adding the transition */
	  if (auto_add_new_transition(ar, p.v.tr.origin, m,
				      1, p.v.tr.label) < 0)
	    return -1;
      
	  resr__free_objects(p.v.tr.label, uint1, na);
	}
      
      /* Pushes the set of successor nodes for m on *st */
      return unser_explore(a, ar, m, p.m, 0, depth, separ, label, ht,
			   st);
    }

  else
    {
      /* State already reached : adding only the transition */
      m = *((uint4 *) r);

      if (auto_add_new_transition(ar, p.v.tr.origin, m,
				  1, p.v.tr.label) < 0)
	return -1;

      resr__free_objects(p.v.tr.label, uint1, na);

      return 0;
    }
}

/**  int  unser_explore(a,ar,origin,v,cur,depth,separ,label,ht,st) :
                     This function is part of the unserialization
		     algorithm.  The automaton being unserialized is
		     *a ; the partially computed product is in *ar,
		     and a hash table associating a state index of
		     *ar to each state at a depth equals to zero
		     is given by *ht. The value depth tells the
		     algorithm the number of components in the set
		     represented by the automaton *a, which indicates
		     the depth where the exploration for a successor
		     state must stop. The value *separ specifies the
		     label of the separator that demarcates an area
		     in the structure of *a. The value *label is a
		     pointer to a memory area wide enough to contain
		     a transition label of *ar.

		     The goal of this function is to compute the set
		     of successor nodes of origin in *ar. A request
		     for each successor is placed on the stack *st.
		     This function proceeds recursively by exploring
		     deeper and deeper successors of the state of *a
		     corresponding to origin. The current node of *a
		     in the exploration is v, which is at depth
		     depth from the state of *a corresponding to
		     origin. As the exploration proceeds, the label
		     of the transition from origin to each successor
		     is progressively stored in *label. If a separator
		     is found, the exploration stops. Furthermore, if
		     it is found at a depth zero, the node origin is
		     linked to the current node with a separator
		     suited for *ar. We can use recursion because the
		     recursion depth is known in advance.

		     If *a is not in strong normal form, this
		     functions returns -2. If a memory lack occurs,
		     it returns -1. Otherwise, if returns 0.       **/

static int  unser_explore(a, ar, origin, v, cur, depth, separ, label,
			  ht, st)
     automaton   *a, *ar;
     uint4        origin, v, cur, depth;
     uint1       *separ, *label;
     hash_table  *ht;
     stack       *st;
{
  register uint1       na, *l;
  register uint4       j, w;
  register tran       *tr;
  register void      **r;
  register int         res;
           uint4       out;
           unser_info  info;

  if (cur == depth)
    {
      /* v is a successor of origin in *ar */
      r = hash__lookup_bytes(ht, (uint1 *) &v, sizeof(uint4));

      if (r)
	{
	  /* A node corresponding to v has already been created */
	  w = *((uint4 *) r);
	  if (auto_add_new_transition(ar, origin, w, 1, label) < 0)
	    return -1;
	}
   
      else
	{
	  /* v has still not been created : pushes a request for it */
	  na                = depth * auto_alphabet_nbytes(a);
	  info.m            = v;
	  info.mode         = US_NEXT;
	  info.v.tr.origin  = origin;
	  info.v.tr.label   = resr__new_objects(uint1, na);
	  
	  if (!info.v.tr.label)
	    return -1;
	  
	  memcpy(info.v.tr.label, label, na);
	  
	  if (stack__push(st, (void *) &info) < 0)
	    return -1;
	}
    }

  else
    {
      na = auto_alphabet_nbytes(a);

      /* Explores all the successors of v in *a */

      if (auto_nb_out_transitions(a, v, &out) < 0)
	return -1;
      
      for (j=0 ; j<out ; j++)
	{
	  tr = auto_transition(a, v, j);
	  w = auto_transition_dest(tr);
	  l = auto_transition_label_ptr(tr, na);
	  
	  if (!auto_transition_length(tr))
	    return -2;

	  if (!separ || memcmp(l, separ, na))
	    {
	      /* The transition label is not a separator */
	      memcpy(label+cur*na, l, na);
	      res = unser_explore(a, ar, origin, w, cur+1, depth,
				  separ, label, ht, st);
	      if (res < 0)
		return res;
	    }
	  else
	    {
	      /* A separator has been reached */
	      if (!cur)
		{
		  /* It has been found at depth zero from origin */
		  l = label;
		  while (cur++ < depth)
		    {
		      memcpy(l, separ, na);
		      l += na;
		    }

		  res = unser_explore(a, ar, origin, w, depth,
				      depth, separ, label, ht, st);
		  if (res < 0)
		    return res;
		}
	    }
	}
    }

  return 0;
}

/****  Public visible functions.                                 ****/

/**  uint8  auto_get_unser_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     synchronization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_unser_ncolls()
{
  return auto_unser_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_unser_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     synchronization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_unser_ncolls()
{
  auto_unser_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_unser_nins()  :  Returns the number of
                     insertions performed in the hash table used by
                     the synchronization algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_unser_nins()
{
  return auto_unser_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_unser_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the synchronization algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_unser_nins()
{
  auto_unser_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  automaton  *auto_serialize(a, separ)  :  Computes a
                     finite-state automaton that accepts the
		     serialized language accepted by the
		     finite-state automaton *a (i.e. the automaton
		     returned by this function reads its "vector"
		     components one by one rather than as a tuple).

		     The value separ specifies the label of a
		     separator that demarcates an area in the
		     structure of *a. In the resulting automaton,
		     the separator is compacted to its first 
		     symbol. separ can be NULL, in which case there
		     is no separator. 

		     This function does not modify *a, and returns
		     (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton *auto_serialize(a, separ)
     automaton  *a;
     uint1      *separ;
{
  register automaton *ar;
  register uint4      i, j, k, pos, len;
  register tran      *tr;
  register uint1     *newl, *oldl, na;
           uint4      nb;
 
  diag__enter("auto_serialize", NULL);

  na = auto_alphabet_nbytes(a);
  if (na <= 1)
    {
      ar = auto_copy(a);
      if (!ar)
	diag__fail(lash_errno, NULL);
      diag__return(a);
    }

  ar = auto_new_empty(1);
  if (!ar)
    diag__fail(lash_errno, NULL);

  for (i=0 ; i<auto_nb_states(a) ; i++)
    if (auto_add_new_state(ar, &nb) < 0)
      {
	auto_free(ar);
	diag__fail(lash_errno, NULL);
      }

  for (i=0 ; i<auto_nb_i_states(a) ; i++)
    {
      if (auto_i_state(a, i, &nb) < 0 ||
	  auto_add_new_i_state(ar, nb) < 0)
	{
	  auto_free(ar);
	  diag__fail(lash_errno, NULL);
	}
    }

  for (i=0 ; i<auto_nb_states(a) ; i++)
    {
      if (auto_accepting_state(a, i))
	auto_mark_accepting_state(ar, i);

      if (auto_nb_out_transitions(a, i, &nb) < 0)
	{
	  auto_free(ar);
	  diag__fail(LASH_ERR_CORRUPT, NULL);
	}

      for (j=0 ; j<nb ; j++)
	{
	  tr = auto_transition(a, i, j);
	  if (!tr)
	    {
	      auto_free(ar);
	      diag__fail(LASH_ERR_CORRUPT, NULL);
	    }

	  len = auto_transition_length(tr);
	  oldl = auto_transition_label_ptr(tr, na);
	  newl = resr__new_objects(uint1, len);
	  if (!newl)
	    {
	      auto_free(ar);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }

	  pos = 0;
	  for (k=0 ; k<len ; k++)
	    {
	      if (!separ || memcmp(oldl + k*na, separ, na))
		{
		  memcpy(newl+pos, oldl + k*na, na);
		  pos += na;
		}
	      else
		{
		  newl[pos] = separ[0];
		  pos ++;
		}
	    }
	 
	  if (auto_add_new_transition(ar, i, auto_transition_dest(tr),
				      pos, newl) < 0)
	    {
	      auto_free(ar);
	      diag__fail(lash_errno, NULL);
	    }

	  resr__free_objects(newl, uint1, len);
	}
    }

  auto_word_type(ar) = auto_word_type(a);
  auto_accept_type(ar) = auto_accept_type(a);

  diag__return(ar);
}

/**  automaton  *auto_unserialize(a, depth, separ)  :  Computes a
                     finite-state automaton that accepts the
		     synchronized language accepted by the
		     finite-state automaton *a (i.e. the automaton
		     returned by this function reads its "vector"
		     components as a tuple rather than one by one).

		     The value depth tells the algorithm the
		     "vector" dimension of the set represented by the
		     automaton *a, which indicates the depth where
		     the exploration for a successor state must stop.
		     The value separ specifies the label of a
		     separator that demarcates an area in the
		     structure of *a. This one can be NULL, in which
		     case there is no separator.

		     This function does not modify *a, and returns
		     (in the case of success) a pointer to a
                     newly allocated automaton. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

automaton  *auto_unserialize(a, depth, separ)
     automaton  *a;
     uint4       depth;
     uint1      *separ;
{
  register automaton   *ar;
  register hash_table  *ht;
  register uint1        is, i;
  register int          res;
           uint4        m, dest;
	   void        *r;

  diag__enter("auto_unserialize", NULL);

  if (!depth)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (depth == 1)
    {
      ar = auto_copy(a);

      if (!ar)
	diag__fail(LASH_ERR_NO_MEM, NULL);

      diag__return(ar);
    }

  if (auto_normalize(a) < 0)
    diag__fail(lash_errno, NULL);

  /* Ressources allocation */

  ar = auto_new_empty(auto_alphabet_nbytes(a) * depth);
  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  auto_word_type(ar) = auto_word_type(a);
  auto_accept_type(ar) = auto_accept_type(a);
  
  if (auto_nb_states(a) == 0)
    diag__return(ar);

  ht = hash__new_empty(2*auto_nb_states(a)/depth + 1);
  if (!ht)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  /* Exploration of the automaton */

  is = auto_nb_i_states(a);

  for (i=0 ; i<is ; i++)
    {
      if (auto_i_state(a, i, &dest) < 0)
	{
	  bytes__prepare_free(sizeof(uint4));
	  hash__free(ht, (void (*)(void *)) bytes__free,
		     (void (*)(void *)) uint4__free);
	  diag__fail(LASH_ERR_CORRUPT, NULL);
	}

      r = hash__lookup_bytes(ht, (uint1 *) &dest, sizeof(uint4));

      if (r)
	m = *((uint4 *) r);
      else
	{
	  res = unser_generate(a, ar, depth, separ, dest, &m, ht);
	  if (res < 0)
	    {
	      auto_free(ar);
	      bytes__prepare_free(sizeof(uint4));
	      hash__free(ht, (void (*)(void *)) bytes__free,
			 (void (*)(void *)) uint4__free);
	      switch (res)
		{
		case -2 : diag__fail(LASH_ERR_BAD_VALUE, NULL);
		default : diag__fail(LASH_ERR_NO_MEM, NULL);
		}	      
	    }
	}

      if (auto_add_new_i_state(ar, m) < 0)
	  {
	    auto_free(ar);
	    bytes__prepare_free(sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
		       (void (*)(void *)) uint4__free);
	    diag__fail(LASH_ERR_NO_MEM, NULL);
  	  }
    }

  /* Freeing resources */

  bytes__prepare_free(sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
	     (void (*)(void *)) uint4__free);
  
  if (auto_test_property(a, AUTO_PROP_DETERM))
    auto_set_property(ar, AUTO_PROP_DETERM);

  diag__return(ar);
}

/****  End of auto-serialize.c  ****/
