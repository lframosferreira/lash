/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-equations.c  :  Construction of NDDs representing the   **/
/**                 sets of solutions of linear equations and      **/
/**                 inequations.                                   **/
/**                                                                **/
/**    09/29/98  :  Creation. (BB)                                 **/
/**    10/02/98  :  Continued. (BB)                                **/
/**    10/05/98  :  Inequations. (BB)                              **/
/**    01/14/99  :  Improved integer arithmetic. (BB)              **/
/**    08/16/01  :  0-dimensional NDDs. (BB)                       **/
/**    07/09/02  :  Reorganization. (BB)                           **/
/**    05/01/05  :  Minor Correcitons Reorganization. (LL)         **/
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
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto-operations.h"

/****  Global variables.                                         ****/

/**  ndd_equation_ncolls  :  Number of collisions observed in the   
                     hash table used by the equation generation
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  ndd_equation_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  ndd_equation_nins  :  Number of insertions performed in the   
                     hash table used by the equation generation
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  ndd_equation_nins = ZERO_INT8;
#endif  /* >= 1 */


/****  Prototypes of private functions.                          ****/

static uint4  compute_equation_hsize(uint1, uint4, sint4 *, sint4);
static int    equation_step(uint1, uint4, sint4 *, sint4 *, uint4 *,
                  uint4 *, uint1 *, sint4 *, sint4 *);
static int    equation_generate(uint1, uint4, sint4 *, sint4,
                  automaton *, hash_table *);
static int    equation_generate_init(automaton *, uint4, hash_table *,
                  stack *);
static int    equation_generate_loop(uint1, uint4, sint4 *,
                  automaton *, sint4 *, sint4 *, hash_table *,
                  stack *); 
static int    inequation_step(uint1, uint4, sint4 *, sint4 *, uint4 *,
                  uint4 *, uint1 *, sint4 *);
static int    inequation_generate(uint1, uint4, sint4 *, sint4,
                  automaton *, hash_table *);
static int    inequation_generate_init(automaton *, uint4, 
                  hash_table *, stack *);
static int    inequation_generate_loop(uint1, uint4, sint4 *,
                  automaton *, sint4 *, hash_table *, stack *); 
static ndd   *create_inequation_lsdf(uint1, uint4, sint4 *, sint4);

/****  Private functions.                                        ****/

/**  uint4  compute_equation_hsize(r, n, c, b)  :  Computes the size 
                     of the hash table needed for computing an NDD
                     accepting the set of solutions in (x0, ...,
                     x(n-1)) of the linear equation c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b.                    **/

static uint4  compute_equation_hsize(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register uint4  l, i, nn;
  register sint8  ap, am;

  if (!b)
    l = 1;
  else
    for (l = 0; b; l++, b /= (sint4) 2);
  
  for (ap = am = ZERO_INT8, i = ZERO_INT4; i < n; i++, c++)
    if (*c > 0)
      ap += *c;
    else
      am += *c;

  if (n <= 10)
    nn = 1 << n;
  else
    nn = 1024;

  ap = (5 * ((ap - am) * l * (r - 1) * nn + 1)) / 4;
  if (ap > (sint8)((uint4) -1))
    return ((uint4) -1);

  return ((uint4) ap);
}

/**  int  equation_step(r, n, c, bp, qp, kp, lp, cmin, cmax)  :  This
                     function of part of the equation generation
		     algorithm. The equation being generated is
		     characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the second
		     member *bp. The numeration base is r. The goal of
		     this function is to determine an outgoing
		     transition from the state associated to the right
		     member *bp and current offset *qp, taking into
		     account the fact that *kp transitions have
		     already been explored from that state. The arrays
		     cmin and cmax contain lower and upper bounds on
		     the values of the right member that can possibly
		     have ingoing transitions labeled by a sign
		     digit. These arrays are indexed with respect to
		     the offset associated with states.

		     If the function is able to determine the
                     parameters of a transition, then it returns 1,
                     writes the label of the transition into *lp,
                     writes the destination of the transition into
                     (*bp, *qp), and updates *kp. Otherwise, the
                     function returns 0.                           **/

static int  equation_step(r, n, c, bp, qp, kp, lp, cmin, cmax)
  uint1  r, *lp;
  uint4  n, *qp, *kp;
  sint4 *c, *bp, *cmin, *cmax;
{
  register uint4  i, q, k;
  register sint4  b, b2, btmp;

  q = *qp;
  b = *bp;
  k = *kp;

  if (q > n)         /*  The transition must read a sign digit.     */
    {
      i = q - n;
      if (i < n && k <= 1 && b >= cmin[i] && b <= cmax[i])
	{
	  *lp  = k ? r - 1 : 0;
	  *bp += k ? c[n - i - 1] : ZERO_INT4;
	  *qp = q + 1;
	  *kp = k + 1;
	  return 1;
	}
      else
	return 0;
    }

  for (; k < r; k++)
    {                /*  Can we read a digit between 0 and r - 1 ?  */
      b2 = b - c[n - q - 1] * (sint4) k;
      if (q == n - 1)
	{
	  btmp = b2 / (sint4) r;
	  if (btmp * ((sint4) r) != b2)
	    continue;
	  b2 = btmp;
	}
      *lp = k;
      *bp = b2;
      *qp = (q + 1) % n;
      *kp = k + 1;
      return 1;
    }

  if (!q)
    {
      if (k == r && b >= cmin[0] && b <= cmax[0])
	{            /*  We read a first sign digit equal to 0.     */
	  *lp = 0;
	  *qp = n + 1;
	  *kp = k + 1;
	  return 1;
	}
      else
	if (k == r + 1)
	  {          /*  We read a first sign digit equal to r - 1. */
	    *lp = r - 1;
	    *bp = b + c[n - 1];
	    *qp = n + 1;
	    *kp = k + 1;
	    return 1;
	  }
    }

  return 0;
}

/**  typedef eq_info  :  Type of the data placed on the exploration
                     stack of the function equation_generate. The
		     first two fields give the current right member
		     and the current offset associated to the state
		     being explored. The third field is a mode field;
		     its value if EQ_ROOT if the function is invoked
		     for the first time in the current generation
		     operation, EQ_FIRST if it is called for the first
		     time with the current pair of right member and
		     offset, and EQ_NEXT otherwise.  The values of the
		     next fields are mode-dependent. If mode ==
		     EQ_ROOT, there is no relevant field. If mode
		     == EQ_FIRST, there are two fields specifying the
		     destination and the label of a transition to be
		     created. Otherwise, there are two fields
		     corresponding to the number of transitions that
		     have already been explored and the index of the
		     state of the resulting automaton that corresponds
		     to the current pair of right member and offset.
		                                                   **/
typedef struct {
  sint4  b;
  uint4  q;
  uint1  mode;
  union {
    struct {
      uint4  dest;
      uint1  label;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} eq_info;

#define  EQ_ROOT   0x01
#define  EQ_FIRST  0x02
#define  EQ_NEXT   0x03

/**  int  equation_generate(r, n, c, b, ar, ht)  :  This routine is
                     part of the equation generation algorithm. The
		     equation being generated is characterized by the
		     dimension n, the coefficients c[0] ... c[n - 1]
		     and the second member b. The numeration base is
		     r. The partially computed automaton is in *ar,
		     and a hash table associating a state index of *ar
		     to pairs of second member and current offset is
		     given by *ht. The generation is carried out
		     serially, most significant digit first.

		     The goal of this function is to generate *ar,
		     starting from the second member b and offset
		     0. This routine proceeds transitively, updates
		     the hash table, and sets the accepting as well as
		     the initial states.

		     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  equation_generate(r, n, c, b, ar, ht)
  uint1       r;
  uint4       n;
  sint4      *c, b;
  automaton  *ar;
  hash_table *ht;
{
  register stack   *st;
  register uint1    mo;
  register uint4    i;
  register sint4   *cmin, *cmax, c1, c2;
           eq_info  eq;

  st = stack__new_empty(eq_info);
  if (!st)
    return -1;

  eq.b    = b;
  eq.q    = ZERO_INT4;
  eq.mode = EQ_ROOT;

  if (stack__push(st, (void *) &eq) < 0)
    {
      stack__free(st);
      return -1;
    }
  
  cmin = resr__new_objects(sint4, n);
  if (!cmin)
    {
      stack__free(st);
      return -1;
    }

  cmax = resr__new_objects(sint4, n);
  if (!cmax)
    {
      resr__free_objects(cmin, sint4, n);
      stack__free(st);
      return -1;
    }

  for (i = 0, c1 = c2 = ZERO_INT4; i < n; i++)
    {
      if (c[i] > 0)
        c1 -= c[i];
      else
	c2 -= c[i];
      cmin[n - i - 1] = c1;
      cmax[n - i - 1] = c2;
    }

  while (!stack__is_empty(st))
    {
      mo = ((eq_info *) stack__top(st)) -> mode;
      if (((mo == EQ_ROOT || mo == EQ_FIRST) && 
          equation_generate_init(ar, n, ht, st) < 0) ||
          (mo == EQ_NEXT &&
          equation_generate_loop(r, n, c, ar, cmin, cmax, ht, st)
              < 0))
        {
	  resr__free_objects(cmin, sint4, n);
	  resr__free_objects(cmax, sint4, n);
          stack__free(st);
          return -1;
        }
    }

  resr__free_objects(cmin, sint4, n);
  resr__free_objects(cmax, sint4, n);
  stack__free(st);
  return 0;
}

/**  int  equation_generate_init(ar, n, ht, st)  :  This routine is
                     part of the equation generation algorithm. The
                     partially computed automaton is in *ar, and a
                     hash table associating a state index of *ar to
                     pairs of second member and current offset is
                     given by *ht. The dimension of the NDD that has
                     to be generated is n. The generation is carried
                     out serially, most significant digit first.
                 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of second member and current offset placed on top
                     of the exploration stack *st (supposed non-empty
                     and of mode equal to either EQ_ROOT or EQ_FIRST),
                     inserting the corresponding entry in the hash
                     table *ht. It then updates the exploration stack
                     such that the next calls to the function
                     equation_generate_loop will create transitively
                     all the predecessor states of the new state as
                     well as their ingoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  equation_generate_init(ar, n, ht, st)
  automaton  *ar;
  uint4       n;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register eq_info  *eq;
  static   uint4     buf_uint4[2];
           uint4     m;
           void    **r;

  eq = (eq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode !=  EQ_ROOT && eq -> mode != EQ_FIRST)
    return -1;
#endif  /* >= 1 */

  if (eq -> b && (eq -> q == 2 * n))
    {
      stack__pop(st, NULL);
      return 0;
    }

  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = (uint4) (eq -> b);
  buf_uint4[1] = eq -> q;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &ndd_equation_ncolls, &ndd_equation_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (auto_add_new_state(ar, &m) < 0)
    return -1;

  *v = m;

  if (eq -> mode == EQ_ROOT)
    auto_mark_accepting_state(ar, m);
  else
    if (auto_add_new_transition(ar, m, eq -> v.tr.dest,
        1, &eq -> v.tr.label) < 0)
      return -1;
  
  if (!(eq -> b) && (eq -> q == 2 * n))
    {
      if (auto_add_new_i_state(ar, m) < 0)
	return -1;
      stack__pop(st, NULL);
      return 0;
    }

  eq -> mode        = EQ_NEXT;
  eq -> v.st.k      = ZERO_INT4;
  eq -> v.st.origin = m;

  return 0;
}

/**  int  equation_generate_loop(r, n, c, ar, cmin, cmax, ht, st)  :
                     This routine is part of the equation generation
                     algorithm. The equation being generated is
                     characterized by the dimension n and the
                     coefficients c[0] ... c[n - 1]. The numeration
                     base is r. The partially computed automaton is in
                     *ar, and a hash table associating a state index
                     of *ar to pairs of second member and current
                     offset is given by *ht. The generation is carried
                     out serially, most significant digit first. The
                     two arrays cmin and cmax contain lower and upper
                     bounds on the values of the right member that can
                     possibly have outgoing transitions labeled by a
                     sign digit. These arrays are indexed with respect
                     to the offset associated with states.
                 
                     This function explores the ingoing transitions
                     to the pair of right member and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     EQ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     equation_generate_init will create transitively
                     all the predecessor states of the new state as 
                     well as their ingoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  equation_generate_loop(r, n, c, ar, cmin, cmax, ht, st)
  uint1       r;
  uint4       n;
  sint4      *c, *cmin, *cmax;
  automaton  *ar;
  hash_table *ht;
  stack      *st;
{
  register uint4    st1;
           uint4    k, q;
           sint4    b;
  static   uint4    dest[2];
           uint1    label;
  register eq_info *eq;
           eq_info  eq2;
  register void    *rr;

  eq = (eq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode != EQ_NEXT)
    return -1;
#endif

  b   = eq -> b;
  q   = eq -> q;
  k   = eq -> v.st.k;
  st1 = eq -> v.st.origin;

  if (!equation_step(r, n, c, &b, &q, &k, &label, cmin, cmax))
    {
      stack__pop(st, NULL);
      return 0;
    }

  eq -> v.st.k = k;

  dest[0] = (uint4) b;
  dest[1] = q;

  rr = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (rr)
    return auto_add_new_transition(ar, *((uint4 *) rr), st1,
        1, &label);

  eq2.mode        = EQ_FIRST;
  eq2.b           = b;
  eq2.q           = q;
  eq2.v.tr.dest   = st1;
  eq2.v.tr.label  = label;

  if (stack__push(st, (void *) &eq2) < 0)
    return -1;

  return 0;
}

/**  int  inequation_step(r, n, c, bp, qp, kp, lp, cmin)  :  This
                     function of part of the inequation generation
		     algorithm. The inequation being generated is
		     characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the second
		     member *bp. The numeration base is r. The goal of
		     this function is to determine an outgoing
		     transition from the state associated to the right
		     member *bp and current offset *qp, taking into
		     account the fact that *kp transitions have
		     already been explored from that state. The array
		     cmin contains lower bounds on the values of the
		     right member that can possibly have outgoing
		     transitions labeled by a sign digit. This array
		     is indexed with respect to the offset associated
		     with states.

		     If the function is able to determine the
                     parameters of a transition, then it returns 1,
                     writes the label of the transition into *lp,
                     writes the destination of the transition into
                     (*bp, *qp), and updates *kp. Otherwise, the
                     function returns 0.                           **/

static int  inequation_step(r, n, c, bp, qp, kp, lp, cmin)
  uint1  r, *lp;
  uint4  n, *qp, *kp;
  sint4 *c, *bp, *cmin;
{
  register uint4  i, q, k;
  register sint4  b, b2, btmp;

  q = *qp;
  b = *bp;
  k = *kp;

  if (q > n)         /*  The transition must read a sign digit.     */
    {
      i = q - n;
      if (i < n && k <= 1 && b >= cmin[i])
	{
	  *lp  = k ? r - 1 : 0;
	  *bp += k ? c[n - i - 1] : ZERO_INT4;
	  *qp = q + 1;
	  *kp = k + 1;
	  return 1;
	}
      else
	return 0;
    }

  if (k < r)
    {                /*  We read a digit between 0 and r - 1.       */
      b2 = b - c[n - q - 1] * (sint4) k;
      if (q == n - 1)
	{
	  btmp = b2 / (sint4) r;
	  if (btmp * ((sint4) r) != b2 && b2 < ZERO_INT4)
	    btmp--;
	  b2 = btmp;
	}
      *lp = k;
      *bp = b2;
      *qp = (q + 1) % n;
      *kp = k + 1;
      return 1;
    }

  if (!q)
    {
      if (k == r && b >= cmin[0])
	{            /*  We read a first sign digit equal to 0.     */
	  *lp = 0;
	  *qp = n + 1;
	  *kp = k + 1;
	  return 1;
	}
      else
	if (k == r + 1)
	  {          /*  We read a first sign digit equal to r - 1. */
	    *lp = r - 1;
	    *bp = b + c[n - 1];
	    *qp = n + 1;
	    *kp = k + 1;
	    return 1;
	  }
    }

  return 0;
}

/**  typedef iq_info  :  Type of the data placed on the exploration
                     stack of the function inequation_generate. The
		     first two fields give the current right member
		     and the current offset associated to the state
		     being explored. The third field is a mode field;
		     its value if IQ_ROOT if the function is invoked
		     for the first time in the current generation
		     operation, IQ_FIRST if it is called for the first
		     time with the current pair of right member and
		     offset, and IQ_NEXT otherwise.  The values of the
		     next fields are mode-dependent. If mode ==
		     IQ_ROOT, there is no relevant field. If mode
		     == IQ_FIRST, there are two fields specifying the
		     origin and the label of a transition to be
		     created. Otherwise, there are two fields
		     corresponding to the number of transitions that
		     have already been explored and the index of the
		     state of the resulting automaton that corresponds
		     to the current pair of right member and offset.
		                                                   **/
typedef struct {
  sint4  b;
  uint4  q;
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
} iq_info;

#define  IQ_ROOT   0x01
#define  IQ_FIRST  0x02
#define  IQ_NEXT   0x03

/**  int  inequation_generate(r, n, c, b, ar, ht)  :  This routine is
                     part of the inequation generation algorithm. The
		     inequation being generated is characterized by 
                     the dimension n, the coefficients c[0] ... c[n -
                     1] and the second member b. The numeration base
                     is r. The partially computed automaton is in *ar,
                     and a hash table associating a state index of *ar
                     to pairs of second member and current offset is
                     given by *ht. The generation is carried out
                     serially, least significant digit first.

		     The goal of this function is to generate *ar,
		     starting from the second member b and offset
		     0. This routine proceeds transitively, updates
		     the hash table, and sets the accepting as well as
		     the initial states.

		     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inequation_generate(r, n, c, b, ar, ht)
  uint1       r;
  uint4       n;
  sint4      *c, b;
  automaton  *ar;
  hash_table *ht;
{
  register stack   *st;
  register uint1    mo;
  register uint4    i;
  register sint4   *cmin, c1;
           iq_info  iq;

  st = stack__new_empty(iq_info);
  if (!st)
    return -1;

  iq.b    = b;
  iq.q    = ZERO_INT4;
  iq.mode = IQ_ROOT;

  if (stack__push(st, (void *) &iq) < 0)
    {
      stack__free(st);
      return -1;
    }
  
  cmin = resr__new_objects(sint4, n);
  if (!cmin)
    {
      stack__free(st);
      return -1;
    }

  for (i = 0, c1 = ZERO_INT4; i < n; i++)
    {
      if (c[i] > 0)
        c1 -= c[i];
      cmin[n - i - 1] = c1;
    }

  while (!stack__is_empty(st))
    {
      mo = ((iq_info *) stack__top(st)) -> mode;
      if (((mo == IQ_ROOT || mo == IQ_FIRST) && 
          inequation_generate_init(ar, n, ht, st) < 0) ||
          (mo == IQ_NEXT &&
          inequation_generate_loop(r, n, c, ar, cmin, ht, st)
              < 0))
        {
	  resr__free_objects(cmin, sint4, n);
          stack__free(st);
          return -1;
        }
    }

  resr__free_objects(cmin, sint4, n);
  stack__free(st);
  return 0;
}

/**  int  inequation_generate_init(ar, n, ht, st)  :  This routine is
                     part of the inequation generation algorithm. The
                     partially computed automaton is in *ar, and a
                     hash table associating a state index of *ar to
                     pairs of second member and current offset is
                     given by *ht. The dimension of the NDD that has
                     to be generated is n. The generation is carried
                     out serially, least significant digit first.
                 
                     The goal of this function is to add to the
		     automaton *ar a state corresponding to the pair
		     of second member and current offset placed on top
		     of the exploration stack *st (supposed non-empty
		     and of mode equal to either IQ_ROOT or IQ_FIRST),
		     inserting the corresponding entry in the hash
                     table *ht. It then updates the exploration stack
                     such that the next calls to the function
                     inequation_generate_loop will create transitively
                     all the successor states of the new state as well
                     as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inequation_generate_init(ar, n, ht, st)
  automaton  *ar;
  uint4       n;
  hash_table *ht;
  stack      *st;
{
  register uint4    *v;
  register iq_info  *iq;
  static   uint4     buf_uint4[2];
           uint4     m;
           void    **r;

  iq = (iq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (iq -> mode !=  IQ_ROOT && iq -> mode != IQ_FIRST)
    return -1;
#endif  /* >= 1 */

  if ((iq -> b < ZERO_INT4) && (iq -> q == 2 * n))
    {
      stack__pop(st, NULL);
      return 0;
    }

  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = (uint4) (iq -> b);
  buf_uint4[1] = iq -> q;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &ndd_equation_ncolls, &ndd_equation_nins) < 0 || !r)
#else
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r) < 0 || !r)
#endif  /* >= 1 */
    {
      uint4__free(v);
      return -1;
    }

  *r = (void *) v;

  if (auto_add_new_state(ar, &m) < 0)
    return -1;

  *v = m;

  if (iq -> mode == IQ_ROOT)
    {
      if (auto_add_new_i_state(ar, m) < 0)
	return -1;
    }
  else
    if (auto_add_new_transition(ar, iq -> v.tr.origin, m,
        1, &iq -> v.tr.label) < 0)
      return -1;
  
  if ((iq -> b >= ZERO_INT4) && (iq -> q == 2 * n))
    {
      auto_mark_accepting_state(ar, m);
      stack__pop(st, NULL);
      return 0;
    }

  iq -> mode        = IQ_NEXT;
  iq -> v.st.k      = ZERO_INT4;
  iq -> v.st.origin = m;

  return 0;
}

/**  int  inequation_generate_loop(r, n, c, ar, cmin, ht, st)  :
                     This routine is part of the inequation generation
                     algorithm. The inequation being generated is
                     characterized by the dimension n and the
                     coefficients c[0] ... c[n - 1]. The numeration
                     base is r. The partially computed automaton is in
                     *ar, and a hash table associating a state index
                     of *ar to pairs of second member and current
                     offset is given by *ht. The generation is carried
                     out serially, least significant digit first. The
                     array cmin contains the lower bounds on the
                     values of the right member that can possibly have
                     outgoing transitions labeled by a sign
                     digit. This array is indexed with respect to the
                     offset associated with states.
                 
                     This function explores the outgoing transitions
                     from the pair of right member and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     IQ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     equation_generate_init will create transitively
                     all the successor states of the new state as well
                     as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  inequation_generate_loop(r, n, c, ar, cmin, ht, st)
  uint1       r;
  uint4       n;
  sint4      *c, *cmin;
  automaton  *ar;
  hash_table *ht;
  stack      *st;
{
  register uint4    st1;
           uint4    k, q;
           sint4    b;
  static   uint4    dest[2];
           uint1    label;
  register iq_info *iq;
           iq_info  iq2;
  register void    *rr;

  iq = (iq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (iq -> mode != IQ_NEXT)
    return -1;
#endif

  b   = iq -> b;
  q   = iq -> q;
  k   = iq -> v.st.k;
  st1 = iq -> v.st.origin;

  if (!inequation_step(r, n, c, &b, &q, &k, &label, cmin))
    {
      stack__pop(st, NULL);
      return 0;
    }

  iq -> v.st.k = k;

  dest[0] = (uint4) b;
  dest[1] = q;

  rr = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (rr)
    return auto_add_new_transition(ar, st1, *((uint4 *) rr),
        1, &label);

  iq2.mode        = IQ_FIRST;
  iq2.b           = b;
  iq2.q           = q;
  iq2.v.tr.origin = st1;
  iq2.v.tr.label  = label;

  if (stack__push(st, (void *) &iq2) < 0)
    return -1;

  return 0;
}

/**  ndd *create_inequation_lsdf(r, n, c, b)  :  Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear inequation c[0] x0 + c[1]
                     x1 + ... + c[n-1] x(n-1) <= b. This NDD operates
                     serially in base r, least significant digit
                     first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     insufficient memory, it returns a NULL pointer.
		                                                   **/

static ndd *create_inequation_lsdf(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register automaton  *ar;
  register hash_table *ht;
  register ndd        *nd;

  ar = auto_new_empty(1);
  if (!ar)
    return NULL;

  ht = hash__new_empty(compute_equation_hsize(r, n, c, b));
  if (!ht)
    {
      auto_free(ar);
      return NULL;
    }

  if (inequation_generate(r, n, c, b, ar, ht) < 0)
    {
      bytes__prepare_free(2 * sizeof(uint4));
      hash__free(ht, (void (*) (void *)) bytes__free,
         (void (*) (void *)) uint4__free);
      auto_free(ar);
      return NULL;
    }

  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*) (void *)) bytes__free,
      (void (*) (void *)) uint4__free);

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(ar);
      return NULL;
    }

  nd -> automaton  = ar;
  nd -> dim        = n;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL;

  return nd; 
}

/****  Public visible functions.                                 ****/

/**  uint8  ndd_get_equation_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     equation generation algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  ndd_get_equation_ncolls()
{
  return ndd_equation_ncolls;
}
#endif  /* >= 1 */

/**  void  ndd_reset_equation_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     equation generation algorithm.  This function 
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  ndd_reset_equation_ncolls()
{
  ndd_equation_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  ndd_get_equation_nins()  :  Returns the number of 
                     insertions performed in the hash table used by
                     the equation generation algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  ndd_get_equation_nins()
{
  return ndd_equation_nins;
}
#endif  /* >= 1 */

/**  void  ndd_reset_equation_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the equation generation algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  ndd_reset_equation_nins()
{
  ndd_equation_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  ndd *ndd_create_equation_msdf(r, n, c, b)  :  Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear equation c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b. This NDD operates
                     serially in base r, most significant digit first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_equation_msdf(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register automaton  *ar;
  register hash_table *ht;
  register ndd        *nd;

  diag__enter("ndd_create_equation_msdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!n)
    {
      nd = b ? ndd_create_empty(r, 0, 1) : 
 	       ndd_create_zn_msdf(r, 0);
      if (nd)
	{
	  diag__return(nd);
	}
      else
	{
	  diag__fail(lash_errno, NULL);
	}
    }

  ar = auto_new_empty(1);
  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ht = hash__new_empty(compute_equation_hsize(r, n, c, b));
  if (!ht)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (equation_generate(r, n, c, b, ar, ht) < 0)
    {
      bytes__prepare_free(2 * sizeof(uint4));
      hash__free(ht, (void (*) (void *)) bytes__free,
         (void (*) (void *)) uint4__free);
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*) (void *)) bytes__free,
      (void (*) (void *)) uint4__free);

  if (auto_prune(ar) < 0)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(ar, AUTO_PROP_DETERM);

  nd -> automaton  = ar;
  nd -> dim        = n;
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL | NDD_PROP_MSDF;

  diag__return(nd);
}

/**  ndd *ndd_create_equation_lsdf(r, n, c, b)  :  Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear equation c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b. This NDD operates
                     serially in base r, least significant digit 
                     first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_equation_lsdf(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register sint4     *cr;
  register uint4      i;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_equation_lsdf", NULL);

  if (!n)
    {
      nd = b ? ndd_create_empty(r, 0, 0) : 
 	       ndd_create_zn_lsdf(r, 0);
      if (nd)
	{
	diag__return(nd);
	}
      else
	{
	  diag__fail(lash_errno, NULL);      
	}
    }

  cr = resr__new_objects(sint4, n);
  if (!cr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  for (i = 0; i < n; i++)
    cr[i] = c[n - i - 1];

  nd = ndd_create_equation_msdf(r, n, cr, b);

  resr__free_objects(cr, sint4, n);
  if (!nd)
    diag__fail(lash_errno, NULL);

  a = auto_reverse(nd -> automaton);
  if (!a || auto_minimize(a) < 0)
    {
      ndd_free(nd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_free(nd -> automaton);
  nd -> automaton = a;
  nd -> properties = NDD_PROP_SERIAL;

  diag__return(nd);
}

/**  ndd *ndd_create_inequation_lsdf(r, n, c, b)  :  Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear inequation c[0] x0 + c[1]
                     x1 + ... + c[n-1] x(n-1) <= b. This NDD operates
                     serially in base r, least significant digit
                     first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_inequation_lsdf(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register sint4     *cr;
  register uint4      i;
  register ndd       *nd;

  diag__enter("ndd_create_inequation_lsdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!n)
    {
      nd = (b < ZERO_INT4) ? ndd_create_empty(r, 0, 0) : 
 	       ndd_create_zn_lsdf(r, 0);
      if (nd)
	{
	  diag__return(nd);
	}
      else
	{
	  diag__fail(lash_errno, NULL);      
	}
    }

  cr = resr__new_objects(sint4, n);
  if (!cr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  for (i = 0; i < n; i++)
    cr[i] = c[n - i - 1];

  nd = create_inequation_lsdf(r, n, cr, b);

  resr__free_objects(cr, sint4, n);
  if (!nd)
    diag__fail(LASH_ERR_NO_MEM, NULL);
 
  if (auto_minimize(nd -> automaton) < 0)
    {
      ndd_free(nd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  diag__return(nd);
}

/**  ndd *ndd_create_inequation_msdf(r, n, c, b)  :  Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear inequation c[0] x0 + c[1]
                     x1 + ... + c[n-1] x(n-1) <= b. This NDD operates
                     serially in base r, most significant digit
                     first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.       **/

ndd *ndd_create_inequation_msdf(r, n, c, b)
  uint1  r;
  uint4  n;
  sint4 *c, b;
{
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_inequation_msdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  if (!n)
    {
      nd = (b < ZERO_INT4) ? ndd_create_empty(r, 0, 1) : 
 	       ndd_create_zn_msdf(r, 0);
      if (nd)
	{
	  diag__return(nd);
	}
      else
	{
	  diag__fail(lash_errno, NULL);      
	}
    }

  nd = create_inequation_lsdf(r, n, c, b);
  if (!nd)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  a = auto_reverse(nd -> automaton);  
  if (!a || auto_minimize(a) < 0)
    {
      ndd_free(nd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_free(nd -> automaton);
  nd -> automaton = a;
  nd -> properties = NDD_PROP_SERIAL | NDD_PROP_MSDF;

  diag__return(nd);
}

/****  End of ndd-equations.c  ****/
