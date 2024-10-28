/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-equations.c  :  Construction of RVAs representing the   **/
/**                 sets of solutions of linear equations and      **/
/**                 inequations.                                   **/
/**                                                                **/
/**    09/30/00  :  Creation. (SJ)                                 **/
/**    02/03/01  :  Second version. (SJ)                           **/
/**    03/03/01  :  Continued. (SJ)                                **/
/**    03/05/01  :  Bug fixed. (SJ)                                **/
/**    03/14/01  :  Minor correction. (SJ)                         **/
/**    03/24/01  :  Usage of the RVA type. (SJ)                    **/
/**    04/12/01  :  Minor modification. (SJ)                       **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/10/01  :  Management of RVAs with zero component. (SJ)   **/
/**    08/13/01  :  Small adaptation. (BB)                         **/
/**    08/20/01  :  Continued. (SJ)                                **/
/**    10/31/01  :  Bug fixed. (SJ)                                **/
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
#include "arithmetic.h"
#include "lash-auto-operations.h"
#include "auto-minimize.h"

/****  Global variables.                                         ****/

/**  GENER_EQUATION and GENER_INEQUATION  :  Parameter to tell
                     the (in)equation generation algorithm whether
		     it must build a RVA representing the set of
		     solutions to an equation or to an inequation. **/

#define GENER_EQUATION    0x01
#define GENER_INEQUATION  0x02

/**  rva_equation_ncolls  :  Number of collisions observed in the   
                     hash table used by the equation generation
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  rva_equation_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  rva_equation_nins  :  Number of insertions performed in the   
                     hash table used by the equation generation
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  rva_equation_nins = ZERO_INT8;
#endif  /* >= 1 */


/****  Prototypes of private functions.                          ****/

static uint4  compute_equation_hsize(uint1, uint4, sint4 *, sint4);
static int    int_equation_generate(uint1, uint4, sint4 *, sint4, 
		  automaton *, hash_table *, sint4 *, sint4 *, 
                  uint4 *, uint1, sint4);
static int    int_equation_generate_init(automaton *, uint4, 
                  hash_table *, stack *, uint1);
static int    int_equation_generate_loop(uint1, uint4, sint4 *,
                  automaton *, sint4 *, sint4 *, hash_table *,
                  stack *, uint1, sint4); 
static int    int_equation_step(uint1, uint4, sint4 *, sint4 *, 
                  uint4 *, uint4 *, uint1 *, sint4 *, sint4 *, 
                  uint1, sint4);
static int    equation_integer_part(uint1, uint4, uint4, sint4 *, 
                  sint4 *, automaton *, uint4 *, uint1, sint4);
static int    frac_equation_generate(uint1, uint4, sint4 *, sint4,
                  automaton *, hash_table *, uint4 *, sint4, sint4, 
                  uint1);
static int    frac_equation_generate_init(automaton *, uint4, 
                  hash_table *, stack *, uint1);
static int    frac_equation_generate_loop(uint1, uint4, sint4 *,
                  automaton *, hash_table *, stack *, sint4, sint4, 
                  uint1); 
static int    frac_equation_step(uint1, uint4, sint4 *, sint4 *, 
                  uint4 *, uint4 *, uint1 *, sint4, sint4, uint1);
static int    equation_fractional_part(uint1, uint4, uint4, sint4 *, 
                  sint4 *, automaton *, uint4 *, sint4, sint4, uint1);
static void   compute_alphas(sint4 *, uint4, sint4 *, sint4 *);
static automaton *rva_generate(uint1, uint4, sint4 *, sint4, uint1);


/****  Private functions.                                        ****/

/**  uint4  compute_equation_hsize(r, n, c, b)  :  Computes the size 
                     of the hash table needed for computing a RVA
                     accepting the set of solutions in (x0, ...,
                     x(n-1)) of the linear (in)equation c[0] x0 +
		     c[1] x1 + ... + c[n-1] x(n-1) = b.            **/

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


/**  Functions for integer part generation.                        **/

/**  typedef ieq_info  :  Type of the data placed on the exploration
                     stack of the function int_equation_generate. The
		     first two fields give the current right member
		     and the current offset associated to the state
		     being explored. The third field is a mode field;
		     its value if IEQ_ROOT if the function is invoked
		     for the first time in the current generation
		     operation, IEQ_FIRST if it is called for the
		     first time with the current pair of right member
		     and offset, and IEQ_NEXT otherwise.  The values
		     of the next fields are mode-dependent. If mode ==
		     IEQ_ROOT, there is one field that gives a pointer
		     to a location to which the function should return
		     the index of the topmost created state. If mode
		     == IEQ_FIRST, there are two fields specifying the
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
    uint4  *return_state;
    struct {
      uint4  dest;
      uint1  label;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} ieq_info;

#define  IEQ_ROOT   0x01
#define  IEQ_FIRST  0x02
#define  IEQ_NEXT   0x03

/**  int  int_equation_generate(r, n, c, b, ar, ht, cmin, cmax,
                     p, oper, gcd)  :  This routine is part of the
		     integer part generation algorithm for equations
		     or inequations. The (in)equation being generated
		     is characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the second
		     member b. The numeration base is r. The partially
		     computed automaton is in *ar, and a hash table
		     associating a state index of *ar to pairs of
		     second member and current offset is given by
		     *ht. The generation is carried out serially. If
		     oper is GENER_EQUATION, the function generates
		     the integer part of the equation for these
		     coefficients, and if oper is GENER_INEQUATION,
		     it generates the integer part of the
		     corresponding inequation. The two arrays cmin
		     and cmax contain lower and upper bounds on the
		     values of the right member that can possibly
		     have outgoing transitions labeled by a sign
		     digit. These arrays are indexed with respect
		     to the offset associated with states. If an
		     inequation is being generated, the argument gcd
		     must contain the value of
		     gcd(c[0] ,..., c[n - 1]).

		     The goal of this function is to generate *ar,
		     starting from the second member b and offset
		     0. The state index corresponding to this pair
		     (b,0) is returned in *p. This routine proceeds
		     transitively, updates the hash table, and sets
		     the accepting as well as the initial states.

		     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  int_equation_generate(r, n, c, b, ar, ht, cmin, cmax,
				  p, oper, gcd)
  uint1       r;
  uint4       n;
  sint4      *c, b;
  automaton  *ar;
  hash_table *ht;
  sint4      *cmin, *cmax;
  uint4      *p;
  uint1       oper;
  sint4       gcd;
{
  register stack   *st;
  register uint1    mo;
  register uint4    s;
           ieq_info  eq;
	   void   **rr;
	   uint4    buf[2];

  buf[0] = b;
  buf[1] = 0;
  rr = hash__lookup_bytes(ht, (uint1 *) buf, 2 * sizeof(uint4));
  if (rr)
    {
      s = *(uint4 *) rr;
      if (p)  *p = s;
      return 0;
    }

  st = stack__new_empty(ieq_info);
  if (!st)
    return -1;

  eq.b              = b;
  eq.q              = ZERO_INT4;
  eq.mode           = IEQ_ROOT;
  eq.v.return_state = p;

  if (stack__push(st, (void *) &eq) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      mo = ((ieq_info *) stack__top(st)) -> mode;
      if (((mo == IEQ_ROOT || mo == IEQ_FIRST) && 
          int_equation_generate_init(ar, n, ht, st, oper) < 0) ||
          (mo == IEQ_NEXT && int_equation_generate_loop
	   (r, n, c, ar, cmin, cmax, ht, st, oper, gcd) < 0))
        {
          stack__free(st);
          return -1;
        }
    }

  stack__free(st);
  return 0;
}

/**  int  int_equation_generate_init(ar, n, ht, st, oper)  :  This
                     routine is part of the integer part generation
		     algorithm for equations or inequations. The
                     partially computed automaton is in *ar, and a
                     hash table associating a state index of *ar to
                     pairs of second member and current offset is
                     given by *ht. The dimension of the RVA that has
                     to be generated is n. The generation is carried
                     out serially.  If oper is GENER_EQUATION, the
		     algorithm is generating the integer part of an
		     equation, and if oper is GENER_INEQUATION, it
		     is generating the integer part of the
		     corresponding inequation.
                 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of second member and current offset placed on top
                     of the exploration stack *st (supposed non-empty
                     and of mode equal to either IEQ_ROOT or
                     IEQ_FIRST), inserting the corresponding entry in
                     the hash table *ht. It then updates the
                     exploration stack such that the next calls to the
                     function int_equation_generate_loop will create
                     transitively all the predecessor states of the
                     new state as well as their ingoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  int_equation_generate_init(ar, n, ht, st, oper)
  automaton  *ar;
  uint4       n;
  hash_table *ht;
  stack      *st;
  uint1       oper;
{
  register uint4    *v;
  register ieq_info  *eq;
  static   uint4     buf_uint4[2];
           uint4     m;
           void    **r;

  eq = (ieq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode !=  IEQ_ROOT && eq -> mode != IEQ_FIRST)
    return -1;
#endif  /* >= 1 */

  if ( (oper == GENER_EQUATION ? eq -> b : (eq -> b < ZERO_INT4)) &&
       (eq -> q == 2 * n) )
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
      &r, &rva_equation_ncolls, &rva_equation_nins) < 0 || !r)
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

  if (eq -> mode == IEQ_ROOT)
    {
      if (eq -> v.return_state)
	*(eq -> v.return_state) = m;
    }
  else
    if (auto_add_new_transition(ar, m, eq -> v.tr.dest,
        1, &eq -> v.tr.label) < 0)
      return -1;
  
  if ((oper == GENER_EQUATION ? !(eq -> b) : (eq -> b >= ZERO_INT4))
      && (eq -> q == 2 * n))
    {
      if (auto_add_new_i_state(ar, m) < 0)
	return -1;
      stack__pop(st, NULL);
      return 0;
    }

  eq -> mode        = IEQ_NEXT;
  eq -> v.st.k      = ZERO_INT4;
  eq -> v.st.origin = m;

  return 0;
}

/**  int  int_equation_generate_loop(r, n, c, ar, cmin, cmax, ht, st,
                     oper, gcd)  : This routine is part of the
		     integer part generation algorithm for equations
		     or inequations. The (in)equation being generated
		     is characterized by the dimension n and the
		     coefficients c[0] ... c[n - 1]. The numeration
		     base is r. The partially computed automaton is in
		     *ar, and a hash table associating a state index
		     of *ar to pairs of second member and current
		     offset is given by *ht. The generation is carried
		     out serially. If oper is GENER_EQUATION, the
		     algorithm is generating the integer part of the
		     equation for these coefficients, and if oper is
		     GENER_INEQUATION, it is generating the integer
		     part of the corresponding inequation.  The two
		     arrays cmin and cmax contain lower and upper
		     bounds on the values of the right member that can
		     possibly have outgoing transitions labeled by a
		     sign digit. These arrays are indexed with respect
		     to the offset associated with states. If an
		     inequation is being generated, the argument gcd
		     must contain the value of gcd(c[0] ,..., c[n -
		     1]).

                     This function explores the ingoing transitions to
                     the pair of right member and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     IEQ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     int_equation_generate_init will create
                     transitively all the predecessor states of the
                     new state as well as their ingoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  int_equation_generate_loop(r, n, c, ar, cmin, cmax, 
				       ht, st, oper, gcd)
  uint1       r;
  uint4       n;
  sint4      *c, *cmin, *cmax;
  automaton  *ar;
  hash_table *ht;
  stack      *st;
  uint1       oper;
  sint4       gcd;
{
  register uint4    st1;
           uint4    k, q;
           sint4    b;
  static   uint4    dest[2];
           uint1    label;
  register ieq_info *eq;
           ieq_info  eq2;
  register void    *rr;

  eq = (ieq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode != IEQ_NEXT)
    return -1;
#endif

  b   = eq -> b;
  q   = eq -> q;
  k   = eq -> v.st.k;
  st1 = eq -> v.st.origin;

  if (!int_equation_step(r, n, c, &b, &q, &k, &label, cmin, cmax, 
			 oper, gcd))
    {
      stack__pop(st, NULL);
      return 0;
    }

  eq -> v.st.k = k;

  dest[0] = (uint4) b;
  dest[1] = q;

  rr = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (rr)
    return auto_add_new_transition(ar,  *((uint4 *) rr), st1,
				   1, &label);

  eq2.mode        = IEQ_FIRST;
  eq2.b           = b;
  eq2.q           = q;
  eq2.v.tr.dest   = st1;
  eq2.v.tr.label  = label;

  if (stack__push(st, (void *) &eq2) < 0)
    return -1;

  return 0;
}

/**  int  int_equation_step(r, n, c, bp, qp, kp, lp, cmin, cmax, oper,
                     gcda)  :  This function is part of the integer
		     part generation algorithm for equations
		     or inequations. The (in)equation being generated
		     is characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the second
		     member *bp. The numeration base is r. 

		     If oper is GENER_EQUATION, the algorithm is
		     generating the integer part of the equation for
		     these coefficients, and if oper is
		     GENER_INEQUATION, it is generating the integer
		     part of the corresponding inequation.  The goal
		     of this function is to determine an outgoing
		     transition from the state associated to the right
		     member *bp and current offset *qp, taking into
		     account the fact that *kp transitions have
		     already been explored from that state. The arrays
		     cmin and cmax contain lower and upper bounds on
		     the values of the right member that can possibly
		     have ingoing transitions labeled by a sign
		     digit. These arrays are indexed with respect to
		     the offset associated with states. Furthermore,
		     if an inequation is being generated, gcda must
		     contain the value of gcd(c[0] ,..., c[n - 1]).

		     If the function is able to determine the
                     parameters of a transition, then it returns 1,
                     writes the label of the transition into *lp,
                     writes the destination of the transition into
                     (*bp, *qp), and updates *kp. Otherwise, the
                     function returns 0.                           **/

static int  int_equation_step(r, n, c, bp, qp, kp, lp, cmin, cmax, 
			      oper, gcda)
  uint1  r, *lp;
  uint4  n, *qp, *kp;
  sint4 *c, *bp, *cmin, *cmax;
  uint1  oper;
  sint4  gcda;
{
  register uint4  i, q, k;
  register sint4  b, btmp;
           sint4  b2;

  q = *qp;
  b = *bp;
  k = *kp;

  if (q > n)
    {
      i = q - n;
      if (i < n && k <= 1 && b >= cmin[i] &&
	  (oper == GENER_INEQUATION || b <= cmax[i]))
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
    {
      b2 = b - c[n - q - 1] * (sint4) k;
      if (q == n - 1)
	switch (oper)
	  {
	  case GENER_EQUATION :
	    btmp = b2 / (sint4) r;
	    if (btmp*((sint4) r) != b2)
	      continue;
	    b2 = btmp;
	    break;
	    
	  case GENER_INEQUATION :
	    sint4__floor(b2, (sint4) r * gcda, &b2);
	    b2 = b2 * gcda;
	    break;
	  }
      
      *lp = k;
      *bp = b2;
      *qp = (q + 1) % n;
      *kp = k + 1;
      return 1;
    }

  if (!q)
    /* All ingoing transitions have been treated :
       adding sign digits if the offset equals zero */
    {
      if (k == r && b >= cmin[0] &&
	  (oper == GENER_INEQUATION || b <= cmax[0]))
	{
	  *lp = 0;
	  *qp = n + 1;
	  *kp = k + 1;
	  return 1;
	}
      else
	if (k == r + 1)
	  {
	    *lp = r - 1;
	    *bp = b + c[n - 1];
	    *qp = n + 1;
	    *kp = k + 1;
	    return 1;
	  }
    }

  return 0;
}

/**  int  equation_integer_part(r, n, nb_members, c, b, ar, links,
		     oper, gcd)  :  This function generates the 
                     integer part of the RVA corresponding to a
		     disjunction of nb_members equations or
		     inequations sharing the same coefficients. The
		     (in)equations composing the disjunction are
		     characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the array of
		     second members *b of size nb_numbers. The
		     numeration base is r. If oper is GENER_EQUATION,
		     the function is to generate the integer part of
		     the equation for these coefficients, and if oper
		     is GENER_INEQUATION, it must generate the integer
		     part of the corresponding inequation. If an
		     inequation is being generated, the argument gcd
		     must contain the value of gcd(c[0] ,..., c[n -
		     1]). The generated structure is integrated into
		     the automaton *ar.  At the end of the generation,
		     links[i] contains the index of the final state
		     corresponding to the (in)equation whose second
		     member is b[i].

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  equation_integer_part(r, n, nb_members, c, b, ar, links,
				  oper, gcd)
  uint1       r;
  uint4       n;
  uint4       nb_members;
  sint4      *c, *b;
  automaton  *ar;
  uint4      *links;
  uint1       oper;
  sint4       gcd;
{
  register sint4      *cmin, *cmax, c1, c2;
  register uint4       i, size;
  register hash_table *ht;

  diag__enter("equation_integer_part", 0);

  cmin = resr__new_objects(sint4, n);
  if (!cmin)
    return -1;

  if (oper == GENER_EQUATION)
    {
      cmax = resr__new_objects(sint4, n);
      if (!cmax)
	{
	  resr__free_objects(cmin, sint4, n);
	  return -1;
	}
    }
  else
    cmax = NULL;

  for (i = size = 0 ; i < nb_members ; i++)
    if (abs(b[i]) > size)
      size = abs(b[i]);

  ht = hash__new_empty(compute_equation_hsize(r, n, c, size));
  if (!ht)
    {
      resr__free_objects(cmin, sint4, n);
      if (cmax) resr__free_objects(cmax, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  for (i = 0, c1 = c2 = ZERO_INT4; i < n; i++)
    {
      if (c[i] > 0)
	c1 -= c[i];
      else
	c2 -= c[i];

      cmin[n - i - 1] = c1;
      if (cmax)
	cmax[n - i - 1] = c2;
    }

  for (i=0 ; i<nb_members ; i++)
    if (int_equation_generate(r, n, c, b[i], ar, ht, 
			      cmin, cmax, links + i, oper, gcd) < 0)
      {
	resr__free_objects(cmin, sint4, n);
	if (cmax)  resr__free_objects(cmax, sint4, n);
	bytes__prepare_free(2 * sizeof(uint4));
	hash__free(ht, (void (*) (void *)) bytes__free,
		   (void (*) (void *)) uint4__free);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  resr__free_objects(cmin, sint4, n);
  if (cmax)  resr__free_objects(cmax, sint4, n);
  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*) (void *)) bytes__free,
	     (void (*) (void *)) uint4__free);
  diag__return(0);
}


/**  Functions for fractional part generation.                     **/

/**  typedef feq_info  :  Type of the data placed on the exploration
                     stack of the function frac_equation_generate. The
		     first two fields give the current right member
		     and the current offset associated to the state
		     being explored. The third field is a mode field;
		     its value if FEQ_ROOT if the function is invoked
		     for the first time in the current generation
		     operation, FEQ_FIRST if it is called for the
		     first time with the current pair of right member
		     and offset, and FEQ_NEXT otherwise.  The values
		     of the next fields are mode-dependent. If mode ==
		     FEQ_ROOT, there is one field that gives a pointer
		     to a location to which the function should return
		     the index of the topmost created state. If mode
		     == FEQ_FIRST, there are two fields specifying the
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
    uint4  *return_state;
    struct {
      uint4  dest;
      uint1  label;
    } tr;
    struct {
      uint4  k, origin;
    } st;
  } v;
} feq_info;


#define  FEQ_ROOT   0x01
#define  FEQ_FIRST  0x02
#define  FEQ_NEXT   0x03

/**  int  frac_equation_generate(r, n, c, b, ar, ht, p, alpham,
		     alphap, oper)  :  This routine is part of the
		     fractional part generation algorithm for
		     equations or inequations. The (in)equation being
		     generated is characterized by the dimension n,
		     the coefficients c[0] ... c[n - 1] and the second
		     member b. The numeration base is r. The partially
		     computed automaton is in *ar, and a hash table
		     associating a state index of *ar to pairs of
		     second member and current offset is given by
		     *ht. The generation is carried out serially. If
		     oper is GENER_EQUATION, the function generates
		     the fractional part of the equation for these
		     coefficients, and if oper is GENER_INEQUATION, it
		     generates the fractional part of the
		     corresponding inequation. alpham (resp. alphap)
		     must contain the sum of the negative (resp.
		     positive) coefficients in *c.

		     The goal of this function is to generate *ar,
		     starting from the second member b and offset
		     0. The state index corresponding to this pair
		     (b,0) is returned in *p. This routine proceeds
		     transitively, updates the hash table, and sets
		     the accepting as well as the initial states.

		     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  frac_equation_generate(r, n, c, b, ar, ht, p,
				   alpham, alphap, oper)
  uint1       r;
  uint4       n;
  sint4      *c, b;
  automaton  *ar;
  hash_table *ht;
  uint4      *p;
  sint4       alpham, alphap;
  uint1       oper;
{
  register stack   *st;
  register uint1    mo;
  register uint4    s;
           feq_info  eq;
	   void   **rr;
	   uint4    buf[2];

  buf[0] = b;
  buf[1] = 0;
  rr = hash__lookup_bytes(ht, (uint1 *) buf, 2 * sizeof(uint4));
  if (rr)
    {
      s = *(uint4 *) rr;
      if (p)  *p = s;
      return 0;
    }

  st = stack__new_empty(feq_info);
  if (!st)
    return -1;

  eq.b              = b;
  eq.q              = ZERO_INT4;
  eq.mode           = FEQ_ROOT;
  eq.v.return_state = p;

  if (stack__push(st, (void *) &eq) < 0)
    {
      stack__free(st);
      return -1;
    }
  
  while (!stack__is_empty(st))
    {
      mo = ((feq_info *) stack__top(st)) -> mode;
      if (((mo == FEQ_ROOT || mo == FEQ_FIRST) && 
          frac_equation_generate_init(ar, n, ht, st, oper) < 0) ||
          (mo == FEQ_NEXT && frac_equation_generate_loop
	   (r, n, c, ar, ht, st, alpham, alphap, oper) < 0))
        {
          stack__free(st);
          return -1;
        }
    }

  stack__free(st);
  return 0;
}

/**  int  frac_equation_generate_init(ar, n, ht, st, oper)  :  This
                     routine is part of the fractional part generation
		     algorithm for equations or inequations. The
                     partially computed automaton is in *ar, and a
                     hash table associating a state index of *ar to
                     pairs of second member and current offset is
                     given by *ht. The dimension of the RVA that has
                     to be generated is n. The generation is carried
                     out serially.  If oper is GENER_EQUATION, the
		     algorithm is generating the fractional part of an
		     equation, and if oper is GENER_INEQUATION, it
		     is generating the fractional part of the
		     corresponding inequation.
                 
                     The goal of this function is to add to the
                     automaton *ar a state corresponding to the pair
                     of second member and current offset placed on top
                     of the exploration stack *st (supposed non-empty
                     and of mode equal to either FEQ_ROOT or
                     FEQ_FIRST), inserting the corresponding entry in
                     the hash table *ht. It then updates the
                     exploration stack such that the next calls to the
                     function frac_equation_generate_loop will create
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  frac_equation_generate_init(ar, n, ht, st, oper)
  automaton  *ar;
  uint4       n;
  hash_table *ht;
  stack      *st;
  uint1       oper;
{
  register uint4    *v;
  register feq_info  *eq;
  static   uint4     buf_uint4[2];
           uint4     m;
           void    **r;

  eq = (feq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode !=  FEQ_ROOT && eq -> mode != FEQ_FIRST)
    return -1;
#endif  /* >= 1 */

  v = resr__new_object(uint4);
  if (!v)
    return -1;

  buf_uint4[0] = (uint4) (eq -> b);
  buf_uint4[1] = eq -> q;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_bytes(ht, (uint1 *) buf_uint4, 2 * sizeof(uint4),
      &r, &rva_equation_ncolls, &rva_equation_nins) < 0 || !r)
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
 
  auto_mark_accepting_state(ar, m);

  *v = m;

  if (eq -> mode == FEQ_ROOT)
    {
      if (eq -> v.return_state)
	*(eq -> v.return_state) = m;
    }
  else
    if (auto_add_new_transition(ar, eq -> v.tr.dest, m,
        1, &eq -> v.tr.label) < 0)
      return -1;
  
  eq -> mode        = FEQ_NEXT;
  eq -> v.st.k      = ZERO_INT4;
  eq -> v.st.origin = m;

  return 0;
}

/**  int  frac_equation_generate_loop(r, n, c, ar, ht, st, alpham,
                     alphap, oper)  : This routine is part of the
		     fractional part generation algorithm for
                     equations or inequations. The (in)equation being
                     generated is characterized by the dimension n and
                     the coefficients c[0] ... c[n - 1]. The
                     numeration base is r. The partially computed
                     automaton is in *ar, and a hash table associating
                     a state index of *ar to pairs of second member
                     and current offset is given by *ht. The
                     generation is carried out serially. If oper is
                     GENER_EQUATION, the algorithm is generating the
                     fractional part of the equation for these
                     coefficients, and if oper is GENER_INEQUATION, it
                     is generating the fractional part of the
                     corresponding inequation. alpham (resp. alphap)
                     must contain the sum of the negative
                     (resp. positive) coefficients in *c.

                     This function explores the outgoing transitions
                     from the pair of right member and current offset
                     placed on top of the exploration stack *st
                     (supposed non-empty and of mode equal to
                     FEQ_NEXT), updating the stack such that the next
                     calls to this function and to the function
                     frac_equation_generate_init will create
                     transitively all the successor states of the new
                     state as well as their outgoing transitions.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  frac_equation_generate_loop(r, n, c, ar, ht, st,
					alpham, alphap, oper)
  uint1       r;
  uint4       n;
  sint4      *c;
  automaton  *ar;
  hash_table *ht;
  stack      *st;
  sint4       alpham, alphap;
  uint1       oper;
{
  register uint4    st1;
           uint4    k, q;
           sint4    b;
  static   uint4    dest[2];
           uint1    label;
  register feq_info *eq;
           feq_info  eq2;
  register void    *rr;

  eq = (feq_info *) stack__top(st);

#if LASH_CHECK_LEVEL >= 1
  if (eq -> mode != FEQ_NEXT)
    return -1;
#endif

  b   = eq -> b;
  q   = eq -> q;
  k   = eq -> v.st.k;
  st1 = eq -> v.st.origin;

  if (!frac_equation_step(r, n, c, &b, &q, &k, &label, alpham, 
			  alphap, oper))
    {
      stack__pop(st, NULL);
      return 0;
    }

  eq -> v.st.k = k;

  dest[0] = (uint4) b;
  dest[1] = q;

  rr = hash__lookup_bytes(ht, (uint1 *) dest, 2 * sizeof(uint4));
  if (rr)
    return auto_add_new_transition(ar, st1, *((uint4 *) rr),
        1, &label);

  eq2.mode        = FEQ_FIRST;
  eq2.b           = b;
  eq2.q           = q;
  eq2.v.tr.dest   = st1;
  eq2.v.tr.label  = label;

  if (stack__push(st, (void *) &eq2) < 0)
    return -1;

  return 0;
}

/**  int  frac_equation_step(r, n, c, bp, qp, kp, lp, alpham, alphap,
                     oper)  :  This function is part of the fractional
		     part generation algorithm for equations or
		     inequations. The (in)equation being generated is
		     characterized by the dimension n, the
		     coefficients c[0] ... c[n - 1] and the second
		     member *bp. The numeration base is r.  If oper is
		     GENER_EQUATION, the algorithm is generating the
		     fractional part of the equation for these
		     coefficients, and if oper is GENER_INEQUATION, it
		     is generating the fractional part of the
		     corresponding inequation.  The goal of this
		     function is to determine an outgoing transition
		     from the state associated to the right member *bp
		     and current offset *qp, taking into account the
		     fact that *kp transitions have already been
		     explored from that state. alpham (resp. alphap)
		     must contain the sum of the negative
		     (resp. positive) coefficients in *c. Thus, alpham
		     and alphap delimits a bound for the admissible
		     values of right members with an offset equal to
		     zero.

		     If the function is able to determine the
                     parameters of a transition, then it returns 1,
                     writes the label of the transition into *lp,
                     writes the destination of the transition into
                     (*bp, *qp), and updates *kp. Otherwise, the
                     function returns 0.                           **/

static int  frac_equation_step(r, n, c, bp, qp, kp, lp, alpham, 
			       alphap, oper)
  uint1  r, *lp;
  uint4  n, *qp, *kp;
  sint4 *c, *bp;
  sint4  alpham, alphap;
  uint1  oper;
{
  register uint4  q, k;
  register sint4  b, b2;

  q = *qp;
  b = *bp;
  k = *kp;

  if (q == 0)
    b = (sint4) r * b;

  for (; k < r; k++)
    {
      b2 = b - c[q] * (sint4) k;

      if (q == n-1)
	{
	  if (b2 < alpham)
	    continue;
      
	  if (b2 > alphap)
	    switch (oper)
	      {
	      case GENER_EQUATION   :  
		continue;
	      case GENER_INEQUATION :  
		b2 = alphap; break;
	      }
	}

      *lp = k;
      *bp = b2;
      *qp = (q + 1) % n;
      *kp = k + 1;
      return 1;
    }

  return 0;
}

/**  int  equation_fractional_part(r, n, nb_members, c, b, ar, 
                     int_links, alpham, alphap, oper)  :  This 
		     function generates the fractional part of the RVA
		     corresponding to a disjunction of nb_members
		     equations or inequations sharing the same
		     multiplicative coefficients. The (in)equations
		     composing the disjunction are characterized by
		     the dimension n, the coefficients c[0] ... c[n -
		     1] and the array of second members *b of size
		     nb_numbers. The numeration base is r.  If oper is
		     GENER_EQUATION, the function is to generate the
		     fractional part of the equations for these
		     coefficients, and if oper is GENER_INEQUATION, it
		     must generate the fractional part of the
		     corresponding inequations. alpham (resp.  alphap)
		     must contain the sum of the negative (resp.
		     positive) coefficients in *c. The generated
		     structure is added to the automaton *ar that is
		     assumed to already accept the integer part of the
		     solutions corresponding to the disjunction of
		     these (in)equations. *int_links is an array of
		     size nb_numbers that contains the state index of
		     the final states in the integer part of the
		     automaton corresponding to the solution to each
		     (in)equation of the disjunction. This function
		     also adds to *ar a transition labeled by a
		     decimal separator between each final state of the
		     integer part and the corresponding initial state
		     of the fractional part.

                     In the case of an error, this function returns
                     -1. Otherwise, it returns 0.                  **/

static int  equation_fractional_part(r, n, nb_members, c, b, ar, 
				     int_links, alpham, alphap, oper)
  uint1       r;
  uint4       n;
  uint4       nb_members;
  sint4      *c, *b;
  automaton  *ar;
  uint4      *int_links;
  sint4       alpham, alphap;
  uint1       oper;
{
  register uint4       i, size;
  register hash_table *ht;
  uint4                frac_state;
  uint1                dummy;

  diag__enter("equation_fractional_part", 0);

  for (i = size = 0 ; i < nb_members ; i++)
    if (abs(b[i]) > size)
      size = abs(b[i]);

  ht = hash__new_empty(compute_equation_hsize(r, n, c, size));
  if (!ht)
    diag__fail(LASH_ERR_NO_MEM, -1);

  dummy = r;

  for (i=0 ; i < nb_members ; i++)
    if (frac_equation_generate(r, n, c, b[i], ar, ht, &frac_state,
			       alpham, alphap, oper) < 0 ||
	/* Adds a bridge between integer and fractional parts */
	    auto_add_new_transition(ar, int_links[i], frac_state, 1, 
				    &r) 
	< 0)
      {
	bytes__prepare_free(2 * sizeof(uint4));
	hash__free(ht, (void (*) (void *)) bytes__free,
		   (void (*) (void *)) uint4__free);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }

  bytes__prepare_free(2 * sizeof(uint4));
  hash__free(ht, (void (*) (void *)) bytes__free,
	     (void (*) (void *)) uint4__free);
  diag__return(0);
}


/**  Combination of integer and fractional parts.                  **/

/**  void  compute_alphas(x, size, alpham, alphap)  :  This function
		     computes the sum of the negative (resp. positive)
		     integers in the array *x of size size and stores
		     the result into *alpham (resp. *alphap). This
		     function cannot incur an error.               **/

static void compute_alphas(x, size, alpham, alphap)
  sint4 *x;
  uint4 size;
  sint4 *alpham, *alphap;
{
  register uint4  i;
  *alpham = *alphap = ZERO_INT4;
  for (i=0 ; i < size ; i++) {
    if (x[i] < 0)
      *alpham = *alpham + x[i];
    else
      *alphap = *alphap + x[i];
  }
}

/**  automaton  *rva_generate(r, n, a, b, oper)  :  If oper ==
		     GENER_INEQUATION, this function generates the
		     automaton of the RVA corresponding to the set
		     of solutions in (x0, ..., x(n-1)) of
		     the linear inequation a[0] x0 + c[1] x1 + ... +
		     a[n-1] x(n-1) <= b. Otherwise, if oper ==
		     GENER_EQUATION, the generated automaton
		     represents the set of solutions of the linear
		     equation a[0] x0 + c[1] x1 + ... + 
		     a[n-1] x(n-1) = b. This RVA operates serially in
		     base r.

		     In the case of success, this function returns a
                     pointer to the newly created automaton. In the
		     case of insufficient memory, it returns a NULL
		     pointer.                                      **/

static automaton *rva_generate(r, n, a, b, oper)
  uint1  r;
  uint4  n;
  sint4 *a, b;
  uint1  oper;
{
  uint4 gcd, nb, *links_int;
  sint4 alpham, alphap;
  sint4 inf, sup, i;
  sint4 *members;
  automaton *ar;

  diag__enter("rva_generate", NULL);

  compute_alphas(a, n, &alpham, &alphap);
  gcd = sint4__multi_gcd(a, n);
  sint4__ceil  (b - alphap, (sint4) gcd, &inf);
  sint4__floor (b - alpham, (sint4) gcd, &sup);
  nb = sup - inf + 1;

  if (oper == GENER_INEQUATION &&
      /** This following line is an optimisation. **/
      (sint4)sup * gcd != b - alphap)  
    nb++;

  members = resr__new_objects(sint4, nb);
  if (!members)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  links_int = resr__new_objects(uint4, nb);
  if (!links_int)
    {
      resr__free_objects(members, sint4, nb);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  ar = auto_new_empty(1);
  if (!ar)
    {
      resr__free_objects(members, sint4, nb);
      resr__free_objects(links_int, uint4, nb);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0 ; inf <= sup ; inf++)
    members[i++] = (sint4)inf * gcd;

  if (oper == GENER_INEQUATION)
    members[nb - 1] = b - alphap;

  if (equation_integer_part(r, n, nb, a, members, ar, links_int, oper,
			    (sint4) gcd) < 0)
    {
      auto_free(ar);
      resr__free_objects(members,sint4,nb);
      resr__free_objects(links_int,uint4,nb);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0 ; i < nb ; i++)
    members[i] = b - members[i];

  if (equation_fractional_part(r, n, nb, a, members, ar, links_int,
			       alpham, alphap, oper) < 0)
    {
      auto_free(ar);
      resr__free_objects(members,sint4,nb);
      resr__free_objects(links_int,uint4,nb);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  resr__free_objects(members,sint4,nb);
  resr__free_objects(links_int,uint4,nb);

  diag__return(ar);
}


/****  Public visible functions.                                 ****/

/**  uint8  rva_get_equation_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     (in)equation generation algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_equation_ncolls()
{
  return rva_equation_ncolls;
}
#endif  /* >= 1 */

/**  void  rva_reset_equation_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     (in)equation generation algorithm.  This function
                     does not report errors.                       **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_equation_ncolls()
{
  rva_equation_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  rva_get_equation_nins()  :  Returns the number of 
                     insertions performed in the hash table used by
                     the (in)equation generation algorithm.  This
                     function does not report errors.              **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_equation_nins()
{
  return rva_equation_nins;
}
#endif  /* >= 1 */

/**  void  rva_reset_equation_nins()  :  Resets the number of
                     insertions performed in the hash table used by
                     the (in)equation generation algorithm.  This 
                     function does not report errors.              **/

#if LASH_CHECK_LEVEL >= 1
void  rva_reset_equation_nins()
{
  rva_equation_nins = ZERO_INT8;
}
#endif  /* >= 1 */


/**  rva  *rva_create_equation(r, n, a, b)  :  Creates a new RVA
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear equation c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b. This RVA operates
                     serially in base r.
                     
                     In the case of success, this function returns a
                     pointer to the newly created RVA. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base or 
                                               dimension.          **/

rva *rva_create_equation(r, n, a, b)
  uint1  r;
  uint4  n;
  sint4 *a, b;
{
  register automaton  *ar;
  register rva        *rv;

  diag__enter("rva_create_equation", NULL);

  if (r <= 1 || r == 0xff)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!n)
    ar = NULL;
  else
    {
      ar = rva_generate(r,n,a,b,GENER_EQUATION);
      if (!ar)
	diag__fail(LASH_ERR_NO_MEM, NULL);

      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_WEAK;
      auto_set_property(ar, AUTO_PROP_DETERM);

      if (auto_minimize(ar) < 0)
	{
	  auto_free(ar);
	  diag__fail(lash_errno, NULL);
	}
    }

  rv = resr__new_object(rva);
  if (!rv)
    {
      if (n)
	auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  rv -> automaton  = ar;
  rv -> dim        = n;
  rv -> base       = r;
  rv -> properties = (n) ? RVA_PROP_SERIAL | RVA_PROP_RESTRICTED : 
    RVA_PROP_NOTHING;
  rv -> universal  = (n != 0 || b != 0) ? 0 : 1;

  diag__return(rv);
}

/**  rva  *rva_create_inequation(r, n, a, b)  :  Creates a new RVA
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear inequation c[0] x0 + c[1]
                     x1 + ... + c[n-1] x(n-1) <= b. This RVA operates
                     serially in base r.
                     
                     In the case of success, this function returns a
                     pointer to the newly created RVA. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base or 
                                               dimension.          **/
 
rva *rva_create_inequation(r, n, a, b)
  uint1  r;
  uint4  n;
  sint4 *a, b;
{
  register automaton  *ar;
  register rva        *rv;

  diag__enter("rva_create_inequation", NULL);

  if (r <= 1 || r == 0xff)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (!n)
    ar = NULL;
  else
    {
      ar = rva_generate(r,n,a,b,GENER_INEQUATION);

      if (!ar)
	diag__fail(LASH_ERR_NO_MEM, NULL);

      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_WEAK;

      if (auto_determinize(ar) < 0 ||
	  auto_convert_to_weak(ar) < 0 ||
	  auto_minimize(ar) < 0)
	{
	  auto_free(ar);
	  diag__fail(lash_errno, NULL);
	}
    }

  rv = resr__new_object(rva);
  if (!rv)
    {
      if (n)
	auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  rv -> automaton  = ar;
  rv -> dim        = n;
  rv -> base       = r;
  rv -> properties = (n) ? RVA_PROP_SERIAL | RVA_PROP_RESTRICTED : 
    RVA_PROP_NOTHING;
  rv -> universal  = (n != 0 || b < 0) ? 0 : 1;

  diag__return(rv);
}

/****  End of rva-equations.c  ****/
