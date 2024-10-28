/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-congruence.c  :  Construction of NDDs representing      **/
/**                   congruences.                                 **/
/**                                                                **/
/**    02/22/05  :  Creation. (LL)                                 **/
/**    05/01/05  :  Minor Corrections (LL)                          **/
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


#include "ndd-congruence.h"
#include "ndd.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "lash-auto-operations.h"
#include "arithmetic.h"
#include "lash-auto-io.h"

#define  LASH_NDD_CONGRUENCE_NOT_SET -1

/****  Prototypes of private functions.                          ****/
static int    congruence_reduce(uint4, sint4 *, sint4, uint4, uint4 *, 
			    uint4 *, uint4 *); 
static int    create_congruence_msdf_step(automaton *, uint1, uint4 *, uint4,
				     uint4, uint4 *, uint4 *, uint1);

/****  Private functions.                                        ****/
/* int  congruence_reduce(n, c, b, m, new_c, new_b, new_m):
   Tests whether gcd(c[0],..c[n-1], b, m) = gcd(c[0],..c[n-1], m). If
   this is not the case, it returns 0.
   Otherwise, it returns 1 and simplifies the 
   congruence c.x = b mod m into new_c.x = new_b mod new_m where :
		    GCD      = gcd(c[0],..c[n-1], m)
		    new_m    = m / GCD
		    new_c[i] = (1/GCD) min { a >= 0 | 
		          (\exists k) (a = c[i] + k * m) } 
		    new_b    = (1/GCD) min { a >= 0 | 
		          (\exists k) (a = b + k * m) } 
  Returns -1 if there is an arithmetic overflow.
			  
**/
static int  congruence_reduce(n, c, b, m, new_c, new_b, new_m)
     uint4  n, m, *new_b, *new_c, *new_m;
     sint4 *c, b;
{
  register uint4 GCD, GCD2, i;
           sint4 ms;


  if (sint4__from_uint4(&ms, m) < 0)
    return -1;

  for (i = ZERO_INT4; i < n; i++)
    {
      if (sint4__rem(c[i], ms, new_c + i) < 0)
	return -1;
    }
  if (sint4__rem(b, ms, new_b) < 0)
    return -1;
  
  GCD= m;
  for (i = ZERO_INT4; i < n; i++)
    GCD = uint4__gcd(GCD, new_c[i]);
  GCD2 = uint4__gcd(GCD, *new_b);
  if (GCD != GCD2)
    return 0;
  
  *new_b = *new_b / GCD;
  for (i = ZERO_INT4; i < n; i++)
    new_c[i] = new_c[i] / GCD;
  *new_m = m / GCD;
  return 1;
}

/** int   create_congruence_msdf_step(a, r, c, m, index, orig_st_to_num, 
             dest_st_to_num, sign): This function is part of 
	     the modulo creation algorithm.
	     The automaton being generated is *a, r is the base,
	     c is the  coefficient vector,  and m is the modulo.
	     index indicated the index of the component whose
             transitions are being generated.
	     orig_st_to_num and dest_st_to_num are arrays containing
             indexes of states of *a. orig_st_to_num[i] corresponds
             to the state s whose label is (i, index), i.e. all paths
             leading to s labeld by w satisfy |w| =  k * n for some k
             and c . dec_r(w) = i mod m.
	     sign indicated whether the
             transitions generated correspond to signed transitions
             or not.
	     
	     Returns 0 in the case of success and -1 in the case of
	     failure.

	     Possible error code.

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_OVERFLOW   : Arith. Overflow.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/

static int   create_congruence_msdf_step(ar, r, c, m, index,  orig_st_to_num, 
             dest_st_to_num, sign)
     automaton *ar; 
     uint1      r, sign;
     uint4     *c, m, *orig_st_to_num, *dest_st_to_num, index;
{
  register uint4  j, dest;
           sint4  val;
           uint1  d;
           sint4  q, ms;

  diag__enter("create_congruence_msdf_step", -1);

  if (sint4__from_uint4(&ms, m) < 0)
    diag__fail(LASH_ERR_OVERFLOW, -1);

  for (j = ZERO_INT4; j < m; j++)
    {
      if (orig_st_to_num[j] != LASH_NDD_CONGRUENCE_NOT_SET)
	for (d = 0; d < r; d++)
	  {
	    if (sign && (d > 0) && (d < r-1))
	      continue;
	    
	    if (sint4__mult(&val, (sint4) d, c[index]) < 0)
	      diag__fail(LASH_ERR_OVERFLOW, -1);

	    if ((sign) && (sint4__floor(val, 1-r, &val) < 0))
	      diag__fail(LASH_ERR_OVERFLOW, -1);

	    if (sint4__mult(&q, j, (!index) ? r : 1) < 0)
	      diag__fail(LASH_ERR_OVERFLOW, -1);
	    val += q;

	    if (sint4__rem(val, ms, &val) < 0)
	      diag__fail(LASH_ERR_OVERFLOW, -1);

	    dest = (uint4) val;
	    if ((dest_st_to_num[dest] == LASH_NDD_CONGRUENCE_NOT_SET)
		&& (auto_add_new_state(ar, dest_st_to_num + dest) < 0))
	      diag__fail(lash_errno, -1);
	    if (auto_add_new_transition(ar, orig_st_to_num[j],
			       dest_st_to_num[dest], 1, &d) < 0)
	      diag__fail(lash_errno, -1);
	  }
    }
  diag__return(0);
}


/** ndd   *ndd_create_congruence_msdf(r, n, c, b, m): Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear modulo c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b mod m. This NDD operates
                     serially in base r, most significant digit first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base or modulo.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/
ndd   *ndd_create_congruence_msdf(r, n, c, b, m)
     uint1  r;
     uint4  n, m;
     sint4 *c, b;
{
  register automaton  *ar;
  register uint4      *st_to_num, i, dest;
  register ndd        *nd;
           uint4      *new_c, new_b, new_m;
	   sint4       q, ms;
	   int         ret;

  diag__enter("ndd_create_congruence_msdf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if ((r <= 1) || (!m))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */


  if (!n)
    {
      if ((sint4__from_uint4(&ms, m) < 0)
	  || (sint4__rem(b, ms, &q) < 0))
	diag__fail(LASH_ERR_OVERFLOW, NULL);

      nd = (q != ZERO_INT4) ? ndd_create_empty(r, 0, 1) : 
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

  new_c = resr__new_objects(uint4, n);
  if (!new_c)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ret = congruence_reduce(n, c, b, m, new_c, &new_b, &new_m);
  if (ret < 0)
    diag__fail(LASH_ERR_OVERFLOW, NULL);
  if (ret == ZERO_INT4)
    {
      nd = ndd_create_empty(r, n, 1);
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
    {
      resr__free_objects(new_c, uint4, n);
      diag__fail(lash_errno, NULL);
    }

  st_to_num = resr__new_objects(uint4, 2 * new_m * n);
  if (!st_to_num)
    {
      auto_free(ar);
      resr__free_objects(new_c, uint4, n);
      diag__fail(lash_errno, NULL);      
    }

  /* The new_m * n first indexes are used for sign states 
   while the next new_m * n are used for "normal" states. */
  /* Note: sign states are added on request, except the initial state. 
     While there is no incoming transitions, st_to_num is set to
     LASH_NDD_CONGRUENCE_NOT_SET.*/
     
  for (i = 0; i < n * new_m; i++)
    st_to_num[i] = LASH_NDD_CONGRUENCE_NOT_SET;

  for (i = ZERO_INT4; i < new_m * n; i++)
    if (auto_add_new_state(ar, st_to_num + n * new_m + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, 2 * new_m * n);
	auto_free(ar);
	resr__free_objects(new_c, uint4, n);
	diag__fail(lash_errno, NULL);
      }

  auto_mark_accepting_state(ar, st_to_num[n * new_m + new_b]);

 if ((auto_add_new_state(ar, st_to_num) < 0)
      ||  (auto_add_new_i_state(ar, st_to_num[0]) < 0))
   {
     resr__free_objects(st_to_num, uint4, new_m * n);
     auto_free(ar);
     resr__free_objects(new_c, uint4, n);
     diag__fail(lash_errno, NULL);
   }

  for (i = 0; i < 2 * n; i++)
    {
      dest = (i == 2 * n -1) ? n : i + 1;
      if (create_congruence_msdf_step(ar, r, new_c, new_m, i % n, 
				  st_to_num + i * new_m, 
				  st_to_num + dest * new_m, 
				  (i < n)) < 0)
	{
	  resr__free_objects(st_to_num, uint4, new_m * n);
	  auto_free(ar);
	  resr__free_objects(new_c, uint4, n);
	  diag__fail(lash_errno, NULL);
	}
    }

  resr__free_objects(new_c, uint4, n);
  resr__free_objects(st_to_num, uint4, 2 * new_m * n);
  
  nd = resr__new_object(ndd);
  if (!nd)
    {
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  auto_set_property(ar, AUTO_PROP_DETERM);

  if (auto_minimize(ar) < 0)
    {
      auto_free(ar);
      diag__fail(lash_errno, NULL);
    }
  
  nd -> automaton  = ar;
  nd -> dim        = n; 
  nd -> base       = r;
  nd -> properties = NDD_PROP_SERIAL | NDD_PROP_MSDF;

  diag__return(nd);
}

/** ndd   *ndd_create_congruence_lsdf(r, n, c, b, m): Creates a new NDD
                     representing the set of solutions in (x0, ...,
                     x(n-1)) of the linear modulo c[0] x0 + c[1] x1
                     + ... + c[n-1] x(n-1) = b mod m. This NDD operates
                     serially in base r, least significant digit first.
                     
                     In the case of success, this function returns a
                     pointer to the newly created NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base or modulo.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.      **/
ndd   *ndd_create_congruence_lsdf(r, n, c, b, m)
     uint1  r;
     uint4  n, m;
     sint4 *c, b;
{
  register sint4     *cr;
  register uint4      i;
  register ndd       *nd;
  register automaton *a;

  diag__enter("ndd_create_congruence_lsdf", NULL);

  if (!n)
    {
      nd = (b % m) ? ndd_create_empty(r, 0, 0) : 
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

  nd = ndd_create_congruence_msdf(r, n, cr, b, m);

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

