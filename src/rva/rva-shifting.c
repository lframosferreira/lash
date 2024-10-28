/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-shifting.c  :  Shifting of sets represented as RVAs.    **/
/**                                                                **/
/**    11/02/01  :  Creation. (SJ)                                 **/
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
#include "rva.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "auto-determinize.h"
#include "auto-minimize.h"
#include "auto-weak-convert.h"
#include "rva-shifting.h"

/****  Global variables.                                         ****/

/**  rva_shift_hsize  :  Size of the hash table used by the     
                    shifting algorithms.                           **/

static uint4  rva_shift_hsize = RVA_DEFAULT_SHIFT_HSIZE;

/**  rva_shift_ncolls  :  Number of collisions observed in the   
                     hash table used by the shifting
                     algorithms.                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  rva_shift_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  rva_shift_nins  :  Number of insertions performed in the   
                     hash table used by the shifting
                     algorithms.                                   **/

#if LASH_CHECK_LEVEL >= 1
static uint8  rva_shift_nins = ZERO_INT8;
#endif  /* >= 1 */


/****  Prototypes of private functions.                          ****/

static uint4  compute_shift_hsize(uint4, uint4);
static int    shl_generate(automaton *, automaton *, uint1, uint4,
                      uint4 *, hash_table *, uint4);
static int    shl_generate_init(automaton *, automaton *,
                      hash_table *, stack *, uint4);
static int    shl_generate_loop(automaton *, automaton *, uint1, 
		      hash_table *, stack *, uint4);
static int    shl_generate_one(automaton *, automaton *, uint4, uint1,
		      uint4, uint4, uint1, hash_table *, stack *);
static int    shl(automaton *, automaton *, uint1, uint4);


/****  Private functions.                                        ****/

/**  uint4  compute_shift_hsize(n, q)  :  Adjusts (heuristically) the
                     size of the hash table needed for computing the
                     left shifting of an automaton with with n states.
		     The maximal exploration depth, that is the number
		     of vector components of the RVA times the number
		     of positions to be shifted, is given by q.    **/

static uint4  compute_shift_hsize(n, q)
  uint4  n, q;
{
  register uint8  nmin, nmax;

  nmin = 16 * n;
  nmax = (((uint8) n) * ((uint8) q));
  nmax = (nmax / 4 + 1) * 5;

  if (nmin > ((uint4) -1))
    nmin = rva_shift_hsize;

  if (nmax > ((uint4) -1))
    nmax = rva_shift_hsize;

  if (!nmin)
    nmin = 1;

  if (!nmax)
    nmax = 1;

  if (rva_shift_hsize > nmax)
    return nmax;

  if (rva_shift_hsize < nmin)
    return nmin;

  return rva_shift_hsize;
}

/**  typedef shl_info  :  Type of the data placed on the exploration
                     stack of the function shl_generate. The first
		     field gives the current state of the automaton
		     associated to the RVA being shifted to the left.
		     The second field is zero while the decimal
		     separator has not already been crossed. When it
		     has been crossed, this field memorizes the
		     current depth from the separator. If q
		     represents the maximal depth of the exploration
		     (that is, the dimension of the -serial- RVA 
		     times the number of positions to be shifted), 
		     the field is bounded by q+2, which means that 
		     a decimal separator has been introduced in the
		     result automaton ; and its value is q+1 when a
		     separator is to be introduced next in the result
		     automaton. The third field is a mode field; its
		     value is SHL_ROOT if the function is invoked for
		     the first time in the current shifting 
		     operation, SHL_FIRST if it is called for the
		     first time with the current state and offset, 
		     and SHL_NEXT otherwise. The values of the next
		     fields are mode-dependent. If mode == SHL_ROOT,
		     there is one field that gives a pointer to a
		     location to which the function should return 
		     the index of the topmost created state. If mode
		     == SHL_FIRST, there are three fields specifying
		     the origin and the label of a transition to be
		     created (if the boolean value empty is set, the
		     transition is to be labeled by the empty word).
		     If mode == SHL_NEXT, there is one field
		     corresponding to the number of transitions that
		     have already been explored in the current state
		     as well as a second one giving the index of the
		     state of the result automaton that corresponds 
		     to the current state in the original one.     **/

typedef struct {
  uint4  m;
  uint4  off;
  uint1  mode;
  union {
    uint4  *return_state;
    struct {
      uint4  origin;
      uint1  label;
      uint1  empty;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} shl_info;

#define  SHL_ROOT   0x01
#define  SHL_FIRST  0x02
#define  SHL_NEXT   0x03

/**  int  shl_generate(ar, a, separ, m, mp, ht, q)  :  This routine
                     is part of the left shifting operation. The
		     automaton being shifted is *a. The label separ
		     describes the decimal separator that demarcates 
		     an area in the structure of the RVA associated
		     to *a. The partially computed shifting is in *ar,
		     and a hash table associating a state index of *ar
		     to each pair of state index and exploration 
		     depth in *a, is given by *ht. The maximal
		     exploration depth, that is the number of vector
		     components of the RVA times the number of
		     positions to be shifted, is given by q.

                     The goal of this function is to generate a part
		     of *ar, starting from the pair of state index
		     and exploration depth in *a given by (m, 0). 
		     This routine proceeds transitively, updates the
		     hash table, and sets accepting states. The index
		     of the state of *ar corresponding to the pair
		     (m, 0) is returned in *mp.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  shl_generate(ar, a, separ, m, mp, ht, q)
  automaton  *ar, *a;
  uint1       separ;
  uint4       m,  *mp;
  hash_table *ht;
  uint4       q;
{
  register stack    *st;
  register uint1     mode;
           shl_info  p;

  st = stack__new_empty(shl_info);
  if (!st)
    return -1;

  p.m              = m;
  p.off            = 0;
  p.mode           = SHL_ROOT;
  p.v.return_state = mp;

  if (stack__push(st, (void *) &p) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      mode = ((shl_info *) stack__top(st)) -> mode;
      if (((mode == SHL_ROOT || mode == SHL_FIRST) && 
          shl_generate_init(ar, a, ht, st, q) < 0) ||
          (mode == SHL_NEXT &&
	   shl_generate_loop(ar, a, separ, ht, st, q)))
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  int  shl_generate_init(ar, a, ht, st, q)  :  This function is
                     part of the left shifting algorithm. The
		     automaton being shifted is *a. The partially 
		     computed shifting is in *ar, and a hash table
		     associating a state index of *ar to each pair
		     of state index and exploration depth in *a, is
		     given by *ht. The maximal exploration depth,
		     that is the number of vector components of the
		     RVA times the number of positions to be shifted,
		     is given by q.
 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of state index and current depth placed on top
		     of the exploration stack *st (supposed non-empty
		     and of mode equal to either SHL_ROOT or
		     SHL_FIRST), inserting the corresponding entry in
		     the hash table *ht. It then updates the 
		     exploration stack such that the next calls to the
		     function shl_generate_loop will create 
		     transitively all the successor states of the new
		     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  shl_generate_init(ar, a, ht, st, q)
  automaton  *ar, *a;
  hash_table *ht;
  stack      *st;
  uint4       q;
{
  register uint4     *v;
  register shl_info  *p;
  static   uint4      buf_uint4[2];
           void     **r;
           uint4      st1;

  p = (shl_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (p -> mode !=  SHL_ROOT && p -> mode != SHL_FIRST)
    return -1;
#endif  /* >= 1 */
  
  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = p -> m;
  buf_uint4[1] = p -> off;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &rva_shift_ncolls, &rva_shift_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0 || !r)
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
  
  if (auto_add_new_state(ar, &st1) < 0)
    return -1;

  *v = st1;

  if (p -> mode == SHL_ROOT)
    {
      if (p -> v.return_state)
	*(p -> v.return_state) = st1;
    }
  else
    if (auto_add_new_transition(ar, p -> v.tr.origin, st1, 
        p -> v.tr.empty ? 0 : 1, &(p -> v.tr.label)) < 0)
      return -1;

  if (p -> off == q+2 && auto_accepting_state(a, p -> m))
    auto_mark_accepting_state(ar, st1);

  p -> mode        = SHL_NEXT;
  p -> v.st.k      = ZERO_INT4;
  p -> v.st.origin = st1;

  return 0;
}

/**  int  shl_generate_loop(ar, a, separ, ht, st, q)  : This function
                     is part of the left shifting algorithm. The
		     automaton being shifted is *a. The label separ
		     describes the decimal separator that demarcates
		     an area in the structure of the RVA associated to
		     *a. The partially computed shifting is in *ar,
		     and a hash table associating a state index of *ar
		     to each pair of state index and exploration depth
		     of *a is given by *ht. The maximal exploration
		     depth, that is the number of vector components of
		     the RVA times the number of positions to be
		     shifted, is given by q.
 
                     This function explores the outgoing transitions
                     from the pair of state index and current depth
		     placed on top of the exploration stack *st
		     (supposed non-empty and of mode equal to
		     SHL_NEXT), updating the stack such that the next
		     calls to this function and to the function 
		     shl_generate_init will create transitively all
                     the successor states of the new state as well as
                     their outgoing transitions.

                     In case of insufficient memory, this function
                     returns -1. Otherwise, it returns 0.          **/

static int  shl_generate_loop(ar, a, separ, ht, st, q)
  automaton  *ar, *a;
  uint1       separ;
  hash_table *ht;
  stack      *st;
  uint4       q;
{
  register uint4      m, k, off, dest;
  register uint1      l;
  register shl_info  *d;
  register tran      *t;
           uint4      n;

  d = (shl_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (d -> mode != SHL_NEXT)
    return -1;
#endif  /* >= 1 */

  m    = d -> m;
  off  = d -> off; 
  k    = d -> v.st.k;

  if (auto_nb_out_transitions(a, m, &n) < 0)
    return -1;

  if (k >= n)
    stack__pop(st, NULL);

  else
    {
      d -> v.st.k ++;
      t = auto_transition(a, m, k);

      if (!t || auto_transition_length(t) != 1)
	return -1;

      l    = *(uint1 *) auto_transition_label_ptr(t, 1);
      dest = auto_transition_dest(t);

      if (off == q+1)
	return shl_generate_one(ar, a, d -> v.st.origin, separ,
				m, q+2, 0, ht, st);

      else
	{
	  if (l != separ)
	    return shl_generate_one
	      (ar, a, d -> v.st.origin, l, dest,
	       off + (off == q+2 || !off ? 0 : 1), 0, ht, st);

	  else if (!off)
	    return shl_generate_one(ar, a, d -> v.st.origin, 0,
				    dest, 1, 1, ht, st);
	}
    }

  return 0;
}

/**  int  shl_generate_one(ar, a, m, lab, dm, doff, empty, ht, st)  :
                     This routine is part of the left shifting
		     operation. The automaton being shifted is *a. The
                     partially computed shifting is in *ar, and a hash
                     table associating a state index of *ar to each
                     pair of state index and exploration depth of *a
		     is given by *ht.

                     The goal of this function is to update the
                     exploration stack *st so as to add to the
                     automaton *ar a new transition outgoing from the
                     state of index m, and corresponding to the pair
                     (dm, doff) of state index and exploration depth
		     in *a. If empty is 1, the transition is to be
		     labeled by the empty word ; otherwise, it must
		     be labeled by lab.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  shl_generate_one(ar, a, m, lab, dm, doff, empty, ht, st)
  automaton  *ar, *a;
  uint4       m;
  uint1       lab;   
  uint4       dm, doff;
  uint1       empty;
  hash_table *ht;
  stack      *st;
{
  register void     *r;
  static   uint4     dest[2];
	   shl_info  p;

  dest[0] = dm;
  dest[1] = doff;

  r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));

  if (r)
    return auto_add_new_transition(ar, m, *((uint4 *) r),
				   empty ? 0 : 1, &lab);

  p.mode        = SHL_FIRST;
  p.m           = dest[0];
  p.off         = dest[1];
  p.v.tr.origin = m;
  p.v.tr.label  = lab;
  p.v.tr.empty  = empty;

  return stack__push(st, (void *) &p);
}

/** static int  shl(ar, a, separ, ht, q)  :  Computes the left
                     shifting of the automaton *a. This automaton is
		     supposed to be in strong normal form. The
		     destination automaton is in *ar. The label
		     separ describes the decimal separator that
		     demarcates an area in the structure of the
		     RVA associated to *a. The maximal exploration 
		     depth, that is the number of vector components 
		     of the RVA times the number of positions to be
		     shifted, is given by q.

		     This function does not modify *a, and returns 0
		     (in the case of success). In the case of an
                     error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

static int shl(ar, a, separ, q)
  automaton   *ar, *a;
  uint1        separ;
  uint4        q;
{
  register uint4       i, k;
  register hash_table *ht;
           uint4       m, dest[2];
           void       *r;

  diag__enter("shl", -1);
  
  k = auto_nb_states(a);
  ht = hash__new_empty(compute_shift_hsize(k, q));
  if (!ht)
    diag__fail(LASH_ERR_NO_MEM, -1);

  k = auto_nb_i_states(a);

  dest[1] = 0;

  for (i = 0; i < k ; i++)
    {
      if (auto_i_state(a, i, dest) < 0)
	diag__fail(LASH_ERR_CORRUPT, -1);

      r = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));

      if (r)
	m = *((uint4 *) r);
      else
	if (shl_generate(ar, a, separ, dest[0], &m, ht, q) < 0)
	  {
	    bytes__prepare_free(2 * sizeof(uint4));
	    hash__free(ht, (void (*)(void *)) bytes__free,
		       (void (*)(void *)) uint4__free);
	    diag__fail(LASH_ERR_CORRUPT, -1);
	  }

      if (auto_add_new_i_state(ar, m) < 0)
	{
	  bytes__prepare_free(2 * sizeof(uint4));
	  hash__free(ht, (void (*)(void *)) bytes__free,
		     (void (*)(void *)) uint4__free);
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}
    }
 
  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*)(void *)) bytes__free,
	     (void (*)(void *)) uint4__free);
  diag__return(0);
}


/****  Public visible functions.                                 ****/

/**  uint8  rva_get_shift_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     shifting algorithms.  This function does not
		     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_shift_ncolls()
{
  return rva_shift_ncolls;
}
#endif  /* >= 1 */

/**  void  rva_reset_shift_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     shifting algorithms.  This function does not
		     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_shift_ncolls()
{
  rva_shift_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  rva_get_shift_nins()  :  Returns the number of 
                     insertions performed in the hash table used by
                     the shifting algorithms.  This function does not
		     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_shift_nins()
{
  return rva_shift_nins;
}
#endif  /* >= 1 */

/**  void  rva_reset_shift_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the shifting algorithm.  This function does not
		     report errors.                                **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_shift_nins()
{
  rva_shift_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  rva *rva_shift_left(rv, p)  :  Computes a RVA representing the
                     left shifting by p (p>0) positions of the set
		     represented by the RVA *rv. In other words, it
		     computes a RVA representing the set
		     { r^p . (x[1], ..., x[n]) }, where r is the base
		     of the RVA *rv and (x[1], ..., x[n]) is a vector
		     belonging to the set represented by *rv.
 
                     This function does not modify *rv, and returns 
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva  *rva_shift_left(rv, p)
     rva   *rv;
     uint4  p;
{
  register automaton  *ad;
  register rva        *rvr;
  register int         err;

  diag__enter("rva_shift_left", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (!rv -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (!auto_test_property(rv -> automaton, AUTO_PROP_STRONG))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  ad = auto_new_empty(1);
  if (!ad)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  if (shl(ad, rv -> automaton, rv -> base, rv -> dim * p) < 0)
    {
      err = lash_errno;
      auto_free(ad);
      diag__fail(err, NULL);
    }

  auto_word_type(ad) = auto_word_type(rv -> automaton);
  auto_accept_type(ad) = auto_accept_type(rv -> automaton);

  if (rv -> properties & RVA_PROP_RESTRICTED)
    if (auto_determinize(ad) < 0 ||
	auto_convert_to_weak(ad) < 0 ||
	auto_minimize(ad) < 0)
      {
	err = lash_errno;
	auto_free(ad);
	diag__fail(err, NULL);
      }

  rvr = resr__new_object(rva);
  if (!rvr)
    {
      auto_free(ad);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rvr -> dim        = rv -> dim;
  rvr -> base       = rv -> base;
  rvr -> properties = rv -> properties;
  rvr -> automaton  = ad;

  if (rva_canonicalize(rvr) < 0)
    {
      err = lash_errno;
      rva_free(rvr);
      diag__fail(err, NULL);
    }

  diag__return(rvr);
}

/**  rva *rva_shift_right(rv, p)  :  Computes a RVA representing the
                     right shifting by p (p>0) positions of the set
		     represented by the RVA *rv. In other words, it
		     computes a RVA representing the set
		     { r^(-p) . (x[1], ..., x[n]) }, where r is the
		     base of the RVA *rv and (x[1], ..., x[n]) is a
		     vector belonging to the set represented by *rv.
 
                     This function does not modify *rv, and returns 
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva  *rva_shift_right(rv, p)
     rva   *rv;
     uint4  p;
{
  register automaton  *ad, *rev;
  register rva        *rvr;
  register int         err;

  diag__enter("rva_shift_right", NULL);

#if LASH_CHECK_LEVEL >= 1

  if (!rv)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (auto_word_type(rv -> automaton) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_CORRUPT, NULL);

#endif  /* >= 1 */

  if (!rv -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (!auto_test_property(rv -> automaton, AUTO_PROP_STRONG))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  auto_word_type(rv -> automaton) = AUTO_WORDS_FINITE;
  rev = auto_reverse(rv -> automaton);
  auto_word_type(rv -> automaton) = AUTO_WORDS_INFINITE;
  if (!rev)
    diag__fail(lash_errno, NULL);

  ad = auto_new_empty(1);
  if (!ad)
    {
      auto_free(rev);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (shl(ad, rev, rv -> base, rv -> dim * p) < 0)
    {
      err = lash_errno;
      auto_free(rev);
      auto_free(ad);
      diag__fail(err, NULL);
    }

  auto_free(rev);

  rev = auto_reverse(ad);
  if (!rev)
    {
      err = lash_errno;
      auto_free(ad);
      diag__fail(err, NULL);
    }
  auto_free(ad);

  auto_word_type(rev) = auto_word_type(rv -> automaton);
  auto_accept_type(rev) = auto_accept_type(rv -> automaton);

  if (rv -> properties & RVA_PROP_RESTRICTED)
    if (auto_determinize(rev) < 0 ||
	auto_convert_to_weak(rev) < 0 ||
	auto_minimize(rev) < 0)
      {
	err = lash_errno;
	auto_free(rev);
	diag__fail(err, NULL);
      }

  rvr = resr__new_object(rva);
  if (!rvr)
    {
      auto_free(rev);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rvr -> dim        = rv -> dim;
  rvr -> base       = rv -> base;
  rvr -> properties = rv -> properties;
  rvr -> automaton  = rev;

  diag__return(rvr);
}

/**  rva *rva_shift(rv, p)  :  Computes a RVA representing the
                     right shifting by p positions of the set
		     represented by the RVA *rv. In other words, it
		     computes an RVA representing the set
		     { r^p . (x[1], ..., x[n]) }, where r is the
		     base of the RVA *rv and (x[1], ..., x[n]) is a
		     vector belonging to the set represented by *rv.
		     p can be positive or negative.
 
                     This function does not modify *rv, and returns 
		     (in the case of success) a pointer to a newly
		     allocated RVA. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Invalid dimension.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
			 LASH_ERR_BAD_TYPE   : Bad type of RVA.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt RVA.        **/

rva *rva_shift(rv, p)
     rva   *rv;
     sint4  p;
{
  register rva  *rvr;

  diag__enter("rva_shift", NULL);

  if (!p)
    rvr = rva_copy(rv);
  else if (p > 0)
    rvr = rva_shift_left(rv, p);
  else
    rvr = rva_shift_right(rv, -p);

  if (!rvr)
    diag__fail(lash_errno, NULL);

  diag__return(rvr);
}

/****  End of rva-shifting.c  ****/
