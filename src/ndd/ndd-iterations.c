/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-iterations.c  :  Computation of the repeated effect of  **/
/**                 linear transformations over sets represented   **/
/**                 as Number Decision Diagrams.                   **/
/**                                                                **/
/**    01/15/99  :  Creation. (BB)                                 **/
/**    01/18/99  :  Continued. (BB)                                **/
/**    01/21/99  :  Improved cyclotomic test. (BB)                 **/
/**    01/28/99  :  Image by closure of transformations. (BB)      **/
/**    02/02/99  :  Improved handling of transducers. (BB)         **/
/**    02/16/99  :  Improved projection. (BB)                      **/
/**    02/19/99  :  Minor corrections. (BB)                        **/
/**    11/18/99  :  Improved algorithm for the computation of      **/
/**                 transformation closures. (BB)                  **/
/**    02/01/00  :  Correction of arithmetic bug. (BB)             **/
/**    02/10/00  :  Efficient data structures for iterations. (BB) **/
/**    07/09/02  :  Reorganization. (BB)                           **/
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
#include <math.h>
#include "ndd.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "arithmetic.h"
#include "cyclotomic.h"
#include "matrix.h"
#include "lash-auto-operations.h"

/****  Prototypes of private functions.                          ****/

static int               rational_log(uint4, uint1, uint4 *, uint4 *);
static poly             *cyclotomic_from_table(uint4);
static int               base_analyze(poly *, uint1, int *, uint4 *,
                             uint4 *);
static poly             *scale_transformation(uint1, linear_transf *, 
                             uint4, 
                             uint4);
static int               magnitude_one_roots(poly *, int *, uint4 *);
static int               diagonal_test(uint1, linear_transf *, uint4,
                             uint4, uint4, int *);
static int               power_safe(sint4 *, uint4, uint4);
static translation_info *create_translation_info(uint1, uint4,
                             sint4 *, int);
static void              translation_info_free(translation_info *);
static uint8             translation_info_size(translation_info *);
static ndd              *apply_translation(ndd *, translation_info *);
static linear_star_info *create_presburger_star_info(linear_transf *,
                             uint1, uint4, int);
static ndd              *image_by_presburger_star_info(ndd *,
                             presburger_star_info *);

/****  Private functions.                                        ****/

/**  int  rational_log(a, r, pnum, pden)  :  This function is part of
                     the closure check for linear transformations. It
                     computes the logarithm in base r > 1 of the
                     positive number a. If the result is a positive
                     rational, then the function returns 0 and sets
                     *pnum and *pden respectively to the numerator and
                     the denominator of this rational (these two
                     values are returned relatively prime to each
                     other). Otherwise, the function returns -1.   **/

static int  rational_log(a, r, pnum, pden)
  uint4  a, *pnum, *pden;
  uint1  r;
{
  register uint1  f;
  register uint4  i, j, u = 1, v = 1, g;
  register int    first;

  for (f = 2, first = 1; r > 1 && f <= 13; f++)
    if (!(r % f))
      {
	for (i = 0; !(r % f); i++)
	  r /= f;
	for (j = 0; !(a % (uint4) f); j++)
	  a /= f;
        if (first)
	  {
	    g = uint4__gcd(i, j);
	    u = j / g;
	    v = i / g;
	    first = 0;
	  }
	else
	  if (((uint8) i) * ((uint8) u) !=
              ((uint8) j) * ((uint8) v))
	    return -1;
      }

  if (a != 1)
    return -1;

  if (pnum)
    *pnum = u;
  if (pden)
    *pden = v;

  return 0;
}

/**  poly *cyclotomic_from_table(n)  :  Returns the (n+1)-th
                     polynomial in the table defined by 
                     'cyclotomic.c'. (This table contains all the
                     cyclotomic polynomials up to a given degree,
                     in increasing order of degree).

		     In the case of success, the function returns a
		     pointer to a newly allocated polynomial. In the
		     case of insufficient memory, it returns a NULL
		     pointer.                                      **/

static poly *cyclotomic_from_table(n)
  uint4  n;
{
  register uint4  d, i;
  register uint4 *p;
  register sint4 *c;
  register poly  *pr;

#if LASH_CHECK_LEVEL >= 1
  if (n >= cyclo__index_size)
    return NULL;
#endif

  p = cyclo__index + (n * 3) + 1;
  d = (uint4) cyclo__bytes[p[0]];

  c = resr__new_objects(sint4, d + 1);
  if (!c)
    return NULL;

  memset(c, 0, (d + 1) * sizeof(sint4));
  
  for (i = p[0]; i < p[1]; i += 2)
    c[cyclo__bytes[i]] = (cyclo__bytes[i + 1] > 0x7f) ?
	-((sint4) (0x100 - cyclo__bytes[i + 1])) :
	 ((sint4) cyclo__bytes[i + 1]);

  pr = poly__new(d, c);
  resr__free_objects(c, sint4, d + 1);

  return pr;
}

/**  int  base_analyze(p, r, pb, py, pz)  :  This function is part of
                     the closure check for linear transformations. It
                     checks the roots of the polynomial *p (which is
		     assumed to be the characteristic polynomial of an
		     integer matrix) against the base r, computing the
		     value of several variables used by the main
		     algorithm (see Figure 8.8 of [Boigelot98] for
		     details).  In the case of success, the algorithm
		     sets *pb to 1 if the test succeeds and to 0
		     otherwise.  In the positive case, the values of
		     the variables y and z of the algorithm of
		     [Boigelot98] are respectively returned in *py and
		     *pz.  This function returns 0 in the case of
		     success.  In the event of an error, it returns -1
		     and sets lash_errno.                          **/

static int  base_analyze(p, r, pb, py, pz)
  poly  *p;
  uint1  r;
  int   *pb;
  uint4 *py, *pz;
{
  register sint4  a = 0;
  register uint4  i, n;
           uint4  u, v, y, z;

  for (i = 0, n = p -> degree; i <= p -> degree; i++, n--)
    if ((a = p -> coeff[i]))
      break;

  if (!(p -> degree) || (i > p -> degree) || !n)
    {
      if (pb)
        *pb = 1;
      if (*py)
	*py = ZERO_INT4;
      if (*pz)
	*pz = ZERO_INT4;
      return 0;
    }

  a /= p -> coeff[p -> degree];

  if (r == 1)
    {
      if (sint4__abs(a) != 1)
	{
	  if (pb)
	    *pb = 0;
	  return 0;
	}
      u = v = 1;
    }
  else
    if (rational_log(sint4__abs(a), r, &u, &v) < 0)
      {
	if (pb)
	  *pb = 0;
	return 0;
      }

#if LASH_CHECK_LEVEL < 1
  v *= n;
#else
  if (uint4__mult(&v, n, v) < 0)
    {
      lash_errno = LASH_ERR_OVERFLOW;
      return -1;
    }
#endif  /* < 1 */

  z = v / uint4__gcd(v, u);

#if LASH_CHECK_LEVEL < 1
  y = z * u;
#else
  if (uint4__mult(&y, z, u) < 0)
    {
      lash_errno = LASH_ERR_OVERFLOW;
      return -1;
    }
#endif  /* < 1 */
  
  y /= v;

  if (pb)
    *pb = 1;
  if (py)
    *py = y;
  if (pz)
    *pz = z;

  return 0;
}

/**  poly *scale_transformation(r, tr, y, z)  :  This function is part
                     of the closure check for linear transformations.
		     It computes the characteristic polynomial of the
		     z-th power of the transformation matrix of *tr,
		     and then scales this polynomial so as to
		       - get rid of the zero roots,
		       - ensure that the product of the magnitude of
		         the remaining roots is 1.
		     The product of the magnitude of the nonzero roots
		     is known to be equal to r^y.

		     In the case of success, this function returns
                     a pointer to a newly allocated polynomial. In the
		     case of an error, it returns a NULL pointer and
		     sets lash_errno.                              **/

static poly *scale_transformation(r, tr, y, z)
  uint1          r;
  linear_transf *tr;
  uint4          y, z;
{
  register uint4  n, i, j;
  register sint4 *a;
  register poly  *pol;
           sint4  m, v;
  
  n = tr -> dim;
  a = resr__new_objects(sint4, n * n);
  if (!a)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  if (matrix__power(a, tr -> coeff, n, z) < 0)
    {
      resr__free_objects(a, sint4, n * n);
      return NULL;
    }

  pol = matrix__characteristic_poly(a, n);
  resr__free_objects(a, sint4, n * n);
  if (!pol)
    return NULL;

  n = pol -> degree;

  for (i = 0; i <= n && !(pol -> coeff[i]); i++);
  
  if (i)
    {
      for (j = 0; j <= (n - i); j++)
	pol -> coeff[j] = pol -> coeff[j + i];
      pol -> degree = n - i;
      pol -> coeff  = resr__resize_objects(pol -> coeff, sint4,
          pol -> degree + 1, n + 1);
      n -= i;
    }

  if (sint4__power(&m, ((sint4) r), y) < 0)
    {
      poly__free(pol);
      lash_errno = LASH_ERR_OVERFLOW;
      return NULL;
    }

  for (i = 0, v = 1; i <= n; i++)
    if ((i && (sint4__mult(&v, v, m) < 0)) ||
	(sint4__mult(pol -> coeff + i, pol -> coeff[i], v) < 0))
      {
	poly__free(pol);
	lash_errno = LASH_ERR_OVERFLOW;
	return NULL;
      }

  return pol;
}

/**  int  magnitude_one_roots(p, pb, pl)  :  This function is part of
                     the closure check for linear transformations. It
                     checks that all the roots of the polynomial *p
                     are of magnitude 1. In the case of success, the
                     algorithm sets *pb to 1 and stores in *pl an
                     integer l such that the l-th power of any root of
                     *p is equal to 1. If the test is negative, the
                     function sets *pb to 0. In the event of an error,
                     it returns -1 and sets lash_errno.            **/

static int  magnitude_one_roots(p, pb, pl)
  poly  *p;
  int   *pb;
  uint4 *pl; 
{
  register uint4  i;
  register poly  *pi, *pi2, *pol;
  register int    rc;
           uint4  l, dummy;

  pi2 = poly__new(p -> degree, p -> coeff);
  if (!pi2)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  poly__reduce(pi2);

  for (i = 0, l = 1; (i < cyclo__index_size) && (pi2 -> degree); i++)
    {
      pol = cyclotomic_from_table(i);
      if (!pol)
	{
	  poly__free(pi2);
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}

      if (pol -> degree > pi2 -> degree)
	{
	  poly__free(pi2);
	  poly__free(pol);
	  if (pb)
	    *pb = 0;
	  return 0;
	}

      pi = poly__mod(pi2, pol, &dummy);
      if (!pi)
	{
	  poly__free(pi2);
	  poly__free(pol);
	  return -1;
	}

      rc = !poly__is_zero(pi);
      poly__free(pi);
      if (rc)
	{
	  poly__free(pol);
	  continue;
	}

      if (uint4__lcm(&l, l, cyclo__index[3 * i]) < 0)
	{
	  poly__free(pol);
	  poly__free(pi2);
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}

      do
	{
	  pi = poly__div(pi2, pol, &dummy);
	  if (!pi)
	    {
	      poly__free(pol);
	      poly__free(pi2);
	      return -1;
	    }
	  poly__free(pi2);
          poly__reduce(pi);
	  pi2 = pi;

	  if (pi2 -> degree < pol -> degree)
	    break;

	  pi = poly__mod(pi2, pol, &dummy);
	  if (!pi)
	    {
	      poly__free(pol);
	      poly__free(pi2);
	      return -1;
	    }
	  rc = poly__is_zero(pi);
	  poly__free(pi);
	}
      while (rc);

      poly__free(pol);
    }

  if (pi2 -> degree > CYCLO_MAX_DEGREE)
    {
      poly__free(pi2);
      lash_errno = LASH_ERR_OVERFLOW;
      return -1;
    }

  rc = !(pi2 -> degree);
  poly__free(pi2);
  if (!rc)
    {
      if (pb)
	*pb = 0;
      return 0;
    }
  if (pb)
    *pb = 1;
  if (pl)
    *pl = l;
  return 0;
}

/**  int  diagonal_test(r, tr, m, p, n2, pb)  :  This function is part
                     of the closure check for linear transformations.
		     It checks that the transformation matrix of the
		     linear transformation *tr satisfies some
		     requirements detailed in [Boigelot98]. The values
		     of the variables r, m, p and n' of the decision
		     procedure proposed in [Boigelot98] are
		     respectively given in r, m, p and n2. If the
		     function succeeds, the (Boolean) result of the
		     test is stored in *pb, and the function returns
		     0.  Otherwise, the function returns -1 and sets
		     lash_errno.                                   **/

static int  diagonal_test(r, tr, m, p, n2, pb)
  uint1          r;
  linear_transf *tr;
  uint4          m, p, n2;
  int           *pb;
{
  register uint4   n, i;
  register sint4  *ap, *a1, *a2;
  register int     rc, flag;
           sint4   rm;

  n = tr -> dim;
  ap = resr__new_objects(sint4, n * n);
  if (!ap)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (matrix__power(ap, tr -> coeff, n, p) < 0)
    {
      resr__free_objects(ap, sint4, n * n);
      return -1;
    }

  if (matrix__is_zero(ap, n, n))
    {
      resr__free_objects(ap, sint4, n * n);
      if (pb)
	*pb = 1;
      return 0;
    }

  a1 = resr__new_objects(sint4, n * n);
  if (!a1)
    {
      resr__free_objects(ap, sint4, n * n);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  if (n2 < n)
    {
      memcpy(a1, ap, n * n * sizeof(sint4));
      flag = 1;
    }
  else
    flag = 0;

  if (n2)
    {
      if (sint4__power(&rm, ((sint4) r), m) < 0)
	{
	  resr__free_objects(a1, sint4, n * n);
	  resr__free_objects(ap, sint4, n * n);
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}
      for (i = 0; i < n * n; i += n + 1)
	if (sint4__add(ap + i, ap[i], -rm) < 0)
	  {
	    resr__free_objects(a1, sint4, n * n);
	    resr__free_objects(ap, sint4, n * n);
	    lash_errno = LASH_ERR_OVERFLOW;
	    return -1;
	  }
      if (flag)
	{
	  a2 = resr__new_objects(sint4, n * n);
	  if (!a2)
	    {
	      resr__free_objects(a1, sint4, n * n);
	      resr__free_objects(ap, sint4, n * n);
	      lash_errno = LASH_ERR_NO_MEM;
	      return -1;
	    }
	  rc = matrix__mult(a2, ap, a1, n, n, n);
	  resr__free_objects(a1, sint4, n * n);
	  if (rc < 0)
	    {
	      resr__free_objects(a2, sint4, n * n);
	      resr__free_objects(ap, sint4, n * n);
	      lash_errno = LASH_ERR_OVERFLOW;
	      return -1;	  
	    }
	  a1 = a2;
	}
      else
	{
	  memcpy(a1, ap, n * n * sizeof(sint4));
	  flag = 1;
	}
    }
  
  resr__free_objects(ap, sint4, n * n);
  if (pb)
    *pb = flag ? matrix__is_zero(a1, n, n) : 0;
  resr__free_objects(a1, sint4, n * n);

  return 0;
}

/**  int  power_safe(m, n, p)  :  Checks whether the square matrix 
                     whose coefficients are given by *m is identical
                     to its p-th power. The dimension of the matrix
                     is n. This function returns 1 if the answer is
                     positive, 0 if it is negative, and -1 in the
                     case of insufficient memory or arithmetic 
                     overflow (the variable lash_errno is then set
                     to the appropriate value).                    **/
                    
static int  power_safe(m, n, p)
  sint4 *m;
  uint4  n, p;
{
  register sint4 *mp;
  register int    rc;

  if (p == 1)
    return 1;

  mp = resr__new_objects(sint4, n * n);
  if (!mp)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (matrix__power(mp, m, n, p) < 0)
    { 
      resr__free_objects(mp, sint4, n * n);
      return -1;
    }

  rc = memcmp(mp, m, sizeof(sint4) * n * n);
  resr__free_objects(mp, sint4, n * n);

  return rc ? 0 : 1;
}  

/**  translation_info *create_translation_info(r, n, b, msdf)  :  
                     Creates an information structure corresponding to
                     repeated translations of the n-th dimensional
                     vector b. The NDD that are part of the structure
                     operate in base r. They read value of msdf is 
                     zero, and most significant digit first otherwise.
		     
		     In the case of success, this function returns a
		     pointer to a newly allocated structure. In the 
                     case of insufficient memory, it returns a NULL 
                     pointer.                                      **/

static translation_info *create_translation_info(r, n, b, msdf)
  uint1  r;
  uint4  n;
  sint4 *b;
  int    msdf;
{
  register sint4            *s;
  register uint4             i, j, nb;
  register translation_info *ti;
  register ndd              *nd;
  register ndd              *(*f)(uint1, uint4, sint4 *, sint4);
  register linear_transf    *tr;
  register ndd             **p;

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1 || !n || !b)
    return NULL;
#endif

  ti = resr__new_object(translation_info);
  if (!ti)
    return NULL;

  ti -> dim = n + 1;
  ti -> ndd = resr__new_objects(ndd *, n + 1);
  if (!(ti -> ndd))
    {
      resr__free_object(ti, translation_info);
      return NULL;
    }

  s = resr__new_objects(sint4, n + 1);
  if (!s)
    {
      resr__free_objects(ti -> ndd, ndd *, (ti -> dim));
      resr__free_object(ti, translation_info);
      return NULL;
    }

  memset(s, 0, (n + 1) * sizeof(sint4));

  f = msdf ? ndd_create_inequation_msdf : 
       ndd_create_inequation_lsdf;

  s[0] = -1;
  nd = f(r, 1, s, ZERO_INT4);
  if (!nd)
    {
      resr__free_objects(s, sint4, n + 1);
      resr__free_objects(ti -> ndd, ndd *, (ti -> dim));
      resr__free_object(ti, translation_info);
      return NULL;
    }
  
  ti -> ndd[0] = nd;

  s[0] = ZERO_INT4;

  for (i = 0, nb = 1; i < n; i++)
    if (b[i])
      {
	s[i] = 1;
	s[n] = b[i];

	tr = ndd_create_assign_transf(n + 1, i, s, ZERO_INT4);
	if (!tr)
	  {
	    for (j = 0; j < nb; j++)
	      ndd_free(ti -> ndd[j]);
	    resr__free_objects(s, sint4, n + 1);
	    resr__free_objects(ti -> ndd, ndd *, (ti -> dim));
	    resr__free_object(ti, translation_info);
	    return NULL;
	  }

	nd = (ndd *) ndd_create_transf_info(tr, r, msdf);
	ndd_transf_free(tr);
	
	if (!nd)
	  {
	    for (j = 0; j < nb; j++)
	      ndd_free(ti -> ndd[j]);
	    resr__free_objects(s, sint4, n + 1);
	    resr__free_objects(ti -> ndd, ndd *, (ti -> dim));
	    resr__free_object(ti, translation_info);
	    return NULL;
	  }
	ti -> ndd[nb++] = nd;
	s[i] = ZERO_INT4;
      }
  
  if (ti -> dim != nb)
    {
      p = resr__resize_objects(ti -> ndd, ndd *, nb, ti -> dim);
      if (!p)
	{
	  for (j = 0; j < nb; j++)
	    ndd_free(ti -> ndd[j]);
	  resr__free_objects(s, sint4, n + 1);
	  resr__free_objects(ti -> ndd, ndd *, (ti -> dim));
	  resr__free_object(ti, translation_info);
	  return NULL;
	}
      ti -> ndd = p;
      ti -> dim = nb;
    }

  resr__free_objects(s, sint4, n + 1);
  return ti;
}

/**  void  translation_info_free(ti)  :  Frees the translation 
                     information structure *ti. This function does
                     not detect errors.                            **/

static void  translation_info_free(ti)
  translation_info *ti;
{
  register uint4  i;

  for (i = 0; i < ti -> dim; i++)
    ndd_free(ti -> ndd[i]);

  resr__free_objects(ti -> ndd, ndd *, ti -> dim);
  resr__free_object(ti, translation_info);
}

/**  uint8  translation_info_size(ti)  :  Returns the size of the
                      translation information structure *ti, expressed
                      in individual NDD states. This function does not
                      detect errors.                               **/

static uint8  translation_info_size(ti)
  translation_info *ti;
{
  register uint4  i;
  register uint8  r;

  for (i = 0, r = ZERO_INT8; i < ti -> dim; i++)
    r += auto_nb_states((ti -> ndd[i]) -> automaton);

  return r;
}

/**  ndd *apply_translation(nd, ti)  :  Computes the image of the
                     set represented by the NDD *nd by the repeated
                     translation associated to the structure *ti.
                     In the case of success, this function returns
                     a newly allocated NDD. In the case of an error,
                     it returns a NULL pointer.                    **/

static ndd *apply_translation(nd, ti)
  ndd              *nd;
  translation_info *ti;
{
  register uint4  i;
  register ndd   *nd1, *nd2;

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !ti)
    return NULL;
#endif 

  nd1 = ndd_product(nd, ti -> ndd[0]);
  if (!nd1)
    return NULL;

  for (i = 1; i < ti -> dim; i++)
    {
      nd2 = ndd_image_by_transf_info(nd1, (linear_tr_info *)
          ti -> ndd[i]);
      ndd_free(nd1);
      if (!nd2)
	return NULL;
      nd1 = nd2;
    }
  
  nd2 = ndd_projection(nd1, nd1 -> dim - 1);
  ndd_free(nd1);

  return nd2;
}

/**  linear_star_info *create_presburger_star_info(tr, r, p, msdf)  :
                     Creates a block of information associated to the
                     linear transformation *tr, whose purpose is to
                     speed up the computation of the image of sets by
                     the closure of *tr. The base used for creating
                     that block of information is r. Numbers are
                     accepted most significant digit first if msdf has
                     a nonzero value, and least significant digit
                     first otherwise. It is known that the p-th power
                     of the transformation matrix of tr is
                     diagonalizable and that its eigenvalues are
                     either 0 or 1.

                     This function does not modify *tr, and returns
                     (in the case of success) a pointer to a newly
                     allocated closure information. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.                                   **/

static  linear_star_info *create_presburger_star_info(tr, r, p, msdf)
  linear_transf *tr;
  uint1          r;
  uint4          p;
  int            msdf;
{
  register linear_transf        *tp, *tp2;
  register linear_star_info     *lsi;
  register presburger_star_info *psi;
  register sint4                *b;
  register uint4                 n;
  register int                   rc;

#if LASH_CHECK_LEVEL >= 1
  if (!tr || r <= 1)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return NULL;
    }
#endif

  lsi = resr__new_object(linear_star_info);
  if (!lsi)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  psi = resr__new_object(presburger_star_info);
  if (!psi)
    {
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  lsi -> type    = NDD_ITERATE_PRESBURGER;
  lsi -> val.psi = psi;
  
  psi -> p  = p;
  psi -> tr = (ndd *) ndd_create_transf_info(tr, r, msdf);
  if (!(psi -> tr))
    {
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);
 
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  tp = ndd_transf_power(tr, p);
  if (!tp)
    {
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  psi -> trp = (ndd *) ndd_create_transf_info(tp, r, msdf);
  if (!(psi -> trp))
    {
      ndd_transf_free(tp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  tp2 = ndd_transf_power(tp, 2);
  if (!tp2)
    {
      ndd_transf_free(tp);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

   psi -> tr2p = (ndd *) ndd_create_transf_info(tp2, r, msdf);
   ndd_transf_free(tp2);
   if (!(psi -> tr2p))
    {
      ndd_transf_free(tp);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  n = tp -> dim;
  b = resr__new_objects(sint4, n);
  if (!b)
    {
      ndd_transf_free(tp);
      ndd_free(psi -> tr2p);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }
  
  rc = matrix__mult(b, tp -> coeff, tp -> offset, n, n, 1);

  ndd_transf_free(tp);
  if (rc < 0)
    {
      resr__free_objects(b, sint4, n);
      ndd_free(psi -> tr2p);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_OVERFLOW;
      return NULL;
    }

  psi -> ti = create_translation_info(r, n, b, msdf);
  resr__free_objects(b, sint4, n);

  if (!(psi -> ti))
    {
      ndd_free(psi -> tr2p);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr);
      resr__free_object(psi, presburger_star_info);
      resr__free_object(lsi, linear_star_info);

      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  return lsi;
}
 
/**  ndd *image_by_presburger_star_info(nd, psi)  :  Computes the
                     image of the set represented by the NDD *nd by
                     the closure of the linear transformation encoded
                     by the structure *psi.

                     In the case of success, this functions returns
                     a pointer to a newly allocated NDD. In the case
                     of insufficient memory, it returns a NULL 
                     pointer.                                      **/

static  ndd *image_by_presburger_star_info(nd, psi)
  ndd                  *nd;
  presburger_star_info *psi;
{
  register ndd  *nd2, *ndt, *nd1;
  register uint4 i;
  register int   rc;

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !psi)
    return NULL;
#endif

  nd2 = ndd_image_by_transf_info(nd, 
      (linear_tr_info *) (psi -> tr2p));
  if (!nd2)
    return NULL;

  ndt = apply_translation(nd2, psi -> ti);
  if (!ndt)
    {
      ndd_free(nd2);
      return NULL;
    }

  if (ndd_merge(ndt, nd) < 0)
    {
      ndd_free(ndt);
      ndd_free(nd2);
      return NULL;
    }
  
  nd1 = ndd_image_by_transf_info(ndt, 
      (linear_tr_info *) (psi -> trp));
  ndd_free(ndt);
  if (!nd1)
    {
      ndd_free(nd2);
      return NULL;
    }

  rc = ndd_merge(nd1, nd2);
  ndd_free(nd2);
  if (rc < 0)
    {
      ndd_free(nd1);
      return NULL;
    }

  rc = ndd_merge(nd1, nd);
  if (rc < 0)
    {
      ndd_free(nd1);
      return NULL;
    }

  ndt = ndd_copy(nd1);
  if (!ndt)
    {
      ndd_free(nd1);
      return NULL;
    }

  for (i = 1; i < psi -> p; i++)
    {
      nd2 = ndd_image_by_transf_info(ndt, 
        (linear_tr_info *) (psi -> tr));
      ndd_free(ndt);
      if (ndd_merge(nd1, nd2) < 0)
	{
	  ndd_free(nd1);
	  ndd_free(nd2);
          return NULL;
	}
      ndt = nd2;
    }

  ndd_free(ndt);

  return nd1;
}
   
/****  Public visible functions.                                 ****/

/**  int  ndd_definable_closure(r, tr, pb, pm, pp)  :  Tests whether
                     the transformation matrix A of the linear
                     transformation *tr has a nonzero integer power p
                     such that A^p is diagonalizable and has all its
                     eigenvalues in the set { 0, r^m }, where m is a
                     positive or zero integer. If the test succeeds,
                     then the function writes 1 at the location pb,
                     and sets respectively *pp and *pm to the values
                     of p and m that have been determined. If the test
                     fails, then the function writes 0 at the location
                     pb.

                     In the case of an error, the function returns -1
                     and does not set *pb, *pm or *pp. Otherwise, it
                     returns 0.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_BAD_VALUE  : Invalid value of r.
                         LASH_ERR_CORRUPT    : Corrupt transformation
                                                                   **/

int  ndd_definable_closure(r, tr, pb, pm, pp)
  uint1          r;
  linear_transf *tr;
  int           *pb;
  uint4         *pm, *pp;
{
  register poly  *pol;
  register int    rc;
  register uint4  n2;
           uint4  y, z, l, m, p, v1, v2;
           int    b;

  diag__enter("ndd_definable_closure", -1);

  if (!r)
    diag__fail(LASH_ERR_BAD_VALUE, -1);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  pol = matrix__characteristic_poly(tr -> coeff, tr -> dim);
  if (!pol)
    diag__fail(lash_errno, -1);

  rc = base_analyze(pol, r, &b, &y, &z);
  poly__free(pol);

  if (rc < 0)
    diag__fail(lash_errno, -1);
  
  if (!b)
    {
      if (pb)
	*pb = 0;
      diag__return(0);
    }

  if (!z)
    {
      if (pb)
	*pb = 1;
      if (*pm)
	*pm = ZERO_INT4;
      if (*pp)
	*pp = tr -> dim;
      diag__return(0);
    }

  pol = scale_transformation(r, tr, y, z);
  if (!pol)
    diag__fail(lash_errno, -1);

  rc = magnitude_one_roots(pol, &b, &l);

  n2 = pol -> degree;
  poly__free(pol);
  if (rc < 0)
    diag__fail(lash_errno, -1);

  if (!b)
    {
      if (pb)
	*pb = 0;
      diag__return(0);
    }

#if LASH_CHECK_LEVEL < 1
  v1 = z * l;
#else
  if (uint4__mult(&v1, z, l) < 0)
    diag__fail(LASH_ERR_OVERFLOW, -1);
#endif  /* < 1 */

  if ((v1 < tr -> dim) && (n2 < tr -> dim))
    {
      if (v1 <= 1)
	rc = 0;
      else
	{
	  rc = power_safe(tr -> coeff, tr -> dim, v1);

	  if (rc < 0)
	    diag__fail(lash_errno, -1);
	}

      if (!rc)
	{
#if LASH_CHECK_LEVEL < 1
          l *= (tr -> dim + v1 - 1) / v1;
#else     
          if (uint4__add(&v2, tr -> dim, v1) < 0)
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  v2 = ((uint4) (v2 - 1)) / v1;
	  if (uint4__mult(&l, l, v2) < 0)
	    diag__fail(LASH_ERR_OVERFLOW, -1);
#endif  /* < 1 */
        }
    }

#if LASH_CHECK_LEVEL < 1
  m = y * l;
  p = z * l;
#else
  if (uint4__mult(&m, y, l) < 0 ||
      uint4__mult(&p, z, l) < 0)
    diag__fail(LASH_ERR_OVERFLOW, -1);
#endif  /* < 1 */

  if (diagonal_test(r, tr, m, p, n2, &b) < 0)
    diag__fail(lash_errno, -1);

  if (!b)
    {
      if (pb)
	*pb = 0;
      diag__return(0);
    }
  
  if (pb)
    *pb = 1;
  if (pm)
    *pm = m;
  if (pp)
    *pp = p;

  diag__return(0);
}

/**  ndd *ndd_image_by_star_transf(nd, t)  :  Computes the image of
                     the set represented by the ndd *nd by the closure
                     of the linear transformation *t. This function
                     does not modify *nd, and returns (in the case of
                     success) a pointer to a newly allocated NDD. In
                     the case of an error, it returns a NULL pointer
                     and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_ITERATION  : The transformation is
		                               not always iterable.
			 LASH_ERR_NOT_IMPL   : Closures outside of
			                       Presburger arithmetic
					       and operations over
					       non-serial NDDs are
					       not (yet) implemented.
                         LASH_ERR_CORRUPT    : Corrupt NDD or 
                                               transformation.     **/

ndd *ndd_image_by_star_transf(nd, t)
  ndd           *nd;
  linear_transf *t;
{
  register linear_star_info *lsi;
  register ndd              *ndr;

  diag__enter("ndd_image_by_star_transf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !t)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd -> dim != t -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(nd -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  lsi = ndd_create_star_info(t, nd -> base, 
      !!(nd -> properties & NDD_PROP_MSDF));
  if (!lsi)
    diag__fail(lash_errno, NULL);

  ndr = ndd_image_by_star_info(nd, lsi);
  
  ndd_star_info_free(lsi);
  if (!ndr)
    diag__fail(lash_errno, NULL);
  
  diag__return(ndr);
}

/**  linear_star_info *ndd_create_star_info(tr, r, msdf)  :  Creates
                     a block of information  associated to the linear
                     transformation *tr, whose purpose is to speed up
		     the computation of the image of sets by the
		     closure of *tr. The base used for creating that
		     block of information is r. Numbers are accepted
		     most significant digit first if msdf has a
		     nonzero value, and least significant digit first
		     otherwise.

                     This function does not modify *tr, and returns
                     (in the case of success) a pointer to a newly
                     allocated closure information. In the case of an
                     error, it returns a NULL pointer and sets
                     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_ITERATION  : The transformation is
		                               not always iterable.
			 LASH_ERR_NOT_IMPL   : Closures outside of
			                       Presburger arithmetic
					       are not (yet) 
					       implemented.
                         LASH_ERR_CORRUPT    : Corrupt transformation.
                                                                   **/

linear_star_info *ndd_create_star_info(tr, r, msdf)
  linear_transf *tr;
  uint1          r;
  int            msdf;
{
  register linear_star_info *ri;
           uint4             m, p;
           int               b;

  diag__enter("ndd_create_star_info", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  if (ndd_definable_closure(r, tr, &b, &m, &p) < 0)
    diag__fail(lash_errno, NULL);

  if (!b)
    diag__fail(LASH_ERR_ITERATION, NULL);

  if (m)
    {
      diag__fail(LASH_ERR_NOT_IMPL, NULL);
    }
  else
    ri = create_presburger_star_info(tr, r, p, msdf);

  if (!ri)
    diag__fail(lash_errno, NULL);
      
  diag__return(ri);
}

/**  ndd *ndd_image_by_star_info(nd, si)  :  Computes the image of
                     the set represented by the ndd *nd by the closure
                     of the linear transformation associated with the
                     information *si.

                     This function does not modify *nd or *si, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_BASE       : Base mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch.
			 LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt NDD or 
                                               transformation info. 
                                                                   **/

ndd *ndd_image_by_star_info(nd, si)
  ndd              *nd;
  linear_star_info *si;
{
  register ndd *nr;

  diag__enter("ndd_image_by_star_info", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !si)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(nd -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  switch(si -> type)
    {
    case NDD_ITERATE_PRESBURGER:
      nr = image_by_presburger_star_info(nd, si -> val.psi);
      if (!nr)
        diag__fail(lash_errno, NULL);
      break;

    default:
      diag__fail(LASH_ERR_CORRUPT, NULL);
    }
  diag__return(nr);
}

/**  int  ndd_star_info_free(si)  :  Frees the linear transformation 
                     closure information  *si.

                     If successful, returns 0. In the case of an 
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt transformation
                                             information.          **/

int  ndd_star_info_free(si)
  linear_star_info *si;
{
  register presburger_star_info *psi;

  diag__enter("ndd_star_info_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!si)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  switch(si -> type)
    {
    case NDD_ITERATE_PRESBURGER:

      psi = si -> val.psi;

#if LASH_CHECK_LEVEL >= 1
      if (!psi)
	diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

      ndd_free(psi -> tr);
      ndd_free(psi -> trp);
      ndd_free(psi -> tr2p);
      translation_info_free(psi -> ti);
      resr__free_object(psi, presburger_star_info);    

      break;
    default:
      diag__fail(LASH_ERR_CORRUPT, -1);
    }
  
  resr__free_object(si, linear_star_info);

  diag__return(0);
}

/**  sint8  ndd_star_info_size(lsi)  :  Returns the size of the
                     meta-transition relation structure *sli,
                     expressed in individual NDD states. In the case
                     of an error, this function returns -1.

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

sint8  ndd_star_info_size(lsi)
  linear_star_info *lsi;
{
  register sint8                 r = ZERO_INT8;
  register presburger_star_info *psi;

  diag__enter("ndd_star_info_size", ((sint8) -1));

#if LASH_CHECK_LEVEL >= 1
  if (!lsi)
    diag__fail(LASH_ERR_CORRUPT, ((sint8) -1));
#endif  /* >= 1 */

  switch(lsi -> type)
    {
    case NDD_ITERATE_PRESBURGER:

      psi = lsi -> val.psi;

#if LASH_CHECK_LEVEL >= 1
      if (!psi)
	diag__fail(LASH_ERR_CORRUPT, ((sint8) -1));
#endif  /* >= 1 */

      r  = auto_nb_states(psi -> tr -> automaton);
      r += auto_nb_states(psi -> trp -> automaton);
      r += auto_nb_states(psi -> tr2p -> automaton);
      r += translation_info_size(psi -> ti);
      break;

    default:
      diag__fail(LASH_ERR_CORRUPT, ((sint8)-1));
    }

  diag__return(r);
}

/****  End of ndd-iterations.c  ****/
