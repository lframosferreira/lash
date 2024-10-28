/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    arithmetic.c  :  Arithmetic operations.                     **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/07/99  :  Continued. (BB)                                **/
/**    01/14/99  :  Polynomials. (BB)                              **/
/**    01/15/99  :  Integer power. (BB)                            **/
/**    01/18/99  :  Small adaptations. (BB)                        **/
/**    02/23/01  :  Corrected memory allocation problem. (BB)      **/
/**    09/20/00  :  Function 'sint4__multi_gcd' added. (SJ)        **/
/**    10/03/00  :  'sint4__floor' and 'sint4__ceil' added. (SJ)   **/
/**    08/13/01  :  Minor correction. (SJ)                         **/
/**    08/13/01  :  Small adaptation. (BB)                         **/
/**    07/02/02  :  Reorganization. (BB)                           **/
/**    05/01/05  :  Add sint4__rem. (LL)                           **/
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
#include "lash-diag.h"
#include "resource.h"
#include "arithmetic.h"

/****  Private type definition.                                  ****/

typedef struct {
  sint4  num;
  uint4  den;
} rational;

/****  Prototypes of private functions.                          ****/

static int  rational_add(rational *, rational *, rational *);
static int  rational_sub(rational *, rational *, rational *);
static int  rational_mult(rational *, rational *, rational *);
static int  rational_div(rational *, rational *, rational *);
static int  poly_divide(poly *, poly *, rational **, rational **);

/****  Private functions.                                        ****/

/**  int  rational_add(rr, r1, r2)  :  Adds the two rational numbers
                     *r1 and *r2, and returns their sum in *rr. The
                     pointers r1, r2 and rr may coincide.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/

static int  rational_add(rr, r1, r2)
  rational *rr, *r1, *r2;
{
  register uint4  g, m1, m2;
           uint4  d;
           sint4  n, n1, n2;

  g  = uint4__gcd(r1 -> den, r2 -> den);
  m1 = (r2 -> den) / g;
  m2 = (r1 -> den) / g;
  
  if (suint4__mult(&n1, r1 -> num, m1) < 0 ||
      suint4__mult(&n2, r2 -> num, m2) < 0 ||
      sint4__add(&n, n1, n2) < 0 ||
      uint4__mult(&d, r1 -> den, m1) < 0)
    return -1;

  g = uint4__gcd(sint4__abs(n), d);
  rr -> num = sint4__over_uint4(n, g);
  rr -> den = d / g;

  return 0;
}

/**  int  rational_sub(rr, r1, r2)  :  Subtracts the two rational 
                     numbers r1 and *r2, and returns their difference
                     *in *rr. The pointers r1, r2 and rr may coincide.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/

static int  rational_sub(rr, r1, r2)
  rational *rr, *r1, *r2;
{
  register uint4  g, m1, m2;
           uint4  d;
           sint4  n, n1, n2;

  g  = uint4__gcd(r1 -> den, r2 -> den);
  m1 = (r2 -> den) / g;
  m2 = (r1 -> den) / g;
  
  if (suint4__mult(&n1, r1 -> num, m1) < 0 ||
      suint4__mult(&n2, r2 -> num, m2) < 0 ||
      sint4__add(&n, n1, -n2) < 0 ||
      uint4__mult(&d, r1 -> den, m1) < 0)
    return -1;

  g = uint4__gcd(sint4__abs(n), d);
  rr -> num = sint4__over_uint4(n, g);
  rr -> den = d / g;

  return 0;
}

/**  int  rational_mult(rr, r1, r2)  :  Multiplies the two rational 
                     numbers r1 and *r2, and returns their product
                     *in *rr. The pointers r1, r2 and rr may coincide.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/

static int  rational_mult(rr, r1, r2)
  rational *rr, *r1, *r2;
{
  register uint4  g1, g2;
           sint4  n;
           uint4  d;

  g1 = uint4__gcd(r1 -> den, sint4__abs(r2 -> num));
  g2 = uint4__gcd(r2 -> den, sint4__abs(r1 -> num));

  if (sint4__mult(&n, sint4__over_uint4(r1 -> num, g2),
      sint4__over_uint4(r2 -> num, g1)) < 0 ||
      uint4__mult(&d, (r1 -> den) / g1, (r2 -> den) / g2) < 0)
    return -1;

  rr -> num = n;
  rr -> den = d;

  return 0;
} 

/**  int  rational_div(rr, r1, r2)  :  Divides the two rational 
                     numbers r1 and *r2, and returns their quotient
                     *in *rr. The pointers r1, r2 and rr may coincide.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow or exception occurs, 
                     it returns -1.                                **/

static int  rational_div(rr, r1, r2)
  rational *rr, *r1, *r2;
{
  rational  inv;

  if (!(r2 -> num))
    return -1;

  if (r2 -> num < ZERO_INT4)
    {
      inv.den = (uint4) -(r2 -> num);
      if (sint4__from_uint4(&inv.num, r2 -> den) < 0)
	return -1;
      inv.num = -inv.num;
    }
  else
    {
      inv.den = (uint4) (r2 -> num);
      if (sint4__from_uint4(&inv.num, r2 -> den) < 0)
	return -1;
    }

  return rational_mult(rr, r1, &inv);
}

/**  int  poly_divide(p1, p2, quo, rem)  :  This routine is part of
                     the division and modulo algorithms for integer
                     polynomials. It divides the two polynomials
                     *p1 and *p2, and returns respectively their
                     quotient and remainder in *quo and *rem (each
                     of which points to an array of rational 
                     coefficients). The degree of *p1 must not be
                     less than the degree of *p2.

		     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1
                     and sets lash_errno.                          **/

static int poly_divide(p1, p2, quo, rem)
  poly      *p1, *p2;
  rational **quo, **rem;
{
  register  uint4     i, j, drem, dquo, ddiv;
  register  rational *div, *prem, *pquo, *pr, *pd;
            rational  rat;

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2 || !quo || !rem || (p1 -> degree) < (p2 -> degree)
      || poly__is_zero(p2))
    return -1;
#endif  /* >= 1 */

  drem = p1 -> degree;
  dquo = (p1 -> degree) - (p2 -> degree);
  ddiv = p2 -> degree;

  *rem = resr__new_objects(rational, drem + 1);
  if (!*rem)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  *quo = resr__new_objects(rational, dquo + 1);
  if (!*quo)
    {
      resr__free_objects(*rem, rational, drem + 1);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  div = resr__new_objects(rational, ddiv + 1);
  if (!div)
    {
      resr__free_objects(*quo, rational, dquo + 1);
      resr__free_objects(*rem, rational, drem + 1);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  for (i = 0; i <= drem; i++)
    {
      (*rem)[i].num = p1 -> coeff[i];
      (*rem)[i].den = 1;
    }

   for (i = 0; i <= ddiv; i++)
    {
      div[i].num = p2 -> coeff[i];
      div[i].den = 1;
    }

   for (i = 0, pquo = (*quo) + dquo, prem = (*rem) + drem; i <= dquo;
       i++, pquo--, prem--)
    {
      if (rational_div(pquo, prem, div + ddiv) < 0)
	{
	  resr__free_objects(*quo, rational, dquo + 1);
	  resr__free_objects(*rem, rational, drem + 1);
          resr__free_objects(div, rational, ddiv + 1);
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}
      for (j = 0, pr = prem - 1, pd = div + ddiv - 1; j < ddiv;
           j++, pr--, pd--)

	if (rational_mult(&rat, pd, pquo) < 0 ||
	    rational_sub(pr, pr, &rat) < 0)
	  {
	    resr__free_objects(*quo, rational, dquo + 1);
	    resr__free_objects(*rem, rational, drem + 1);
            resr__free_objects(div, rational, ddiv + 1);
	    lash_errno = LASH_ERR_OVERFLOW;
	    return -1;
	  }
    }
  resr__free_objects(div, rational, ddiv + 1);
  *rem = resr__resize_objects(*rem, rational, ddiv + 1, drem + 1);

  return 0;
}

/****  Public invisible functions.                               ****/

/**  int  sint4__add(p, v1, v2)  :  Adds the two values v1 and v2 and
                     returns their sum in the location pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  sint4__add(p, v1, v2)
  sint4 *p, v1, v2;
{
  register sint8  r;

  r = ((sint8) v1) + ((sint8) v2);
  if (((r <  ZERO_INT8) && 
      ((((uint8) r) >> 31) != (((uint8) -1) >> 31))) ||
      ((r >= ZERO_INT8) && (r >> 31)))
    return -1;

  if (p)
    *p = (sint4) r;

  return 0;
}

/**  int  sint4__mult(p, v1, v2)  :  Multiplies the two signed values
                     v1 and v2 and returns their product in the
                     location pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  sint4__mult(p, v1, v2)
  sint4 *p, v1, v2;
{
  register sint8  r;

  r = ((sint8) v1) * ((sint8) v2);
  if (((r <  ZERO_INT8) && 
      ((((uint8) r) >> 31) != (((uint8) -1) >> 31))) ||
      ((r >= ZERO_INT8) && (r >> 31)))
    return -1;

  if (p)
    *p = (sint4) r;

  return 0;
}

/**  int  uint4__add(p, v1, v2)  :  Adds the two unsigned values v1
                     and v2 and returns their sum in the location
                     pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  uint4__add(p, v1, v2)
  uint4 *p, v1, v2;
{
  register uint8  r;

  r = ((uint8) v1) + ((uint8) v2);
  if (r >> 32)
    return -1;

  if (p)
    *p = (uint4) r;

  return 0;
}

/**  int  uint4__mult(p, v1, v2)  :  Multiplies the two unsigned 
                     values v1 and v2 and returns their product in the
                     location pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  uint4__mult(p, v1, v2)
  uint4 *p, v1, v2;
{
  register uint8  r;

  r = ((uint8) v1) * ((uint8) v2);
  if (r >> 32)
    return -1;

  if (p)
    *p = (uint4) r;

  return 0;
}

/**  int  suint4__mult(p, v1, v2)  :  Multiplies the signed value v1
                     by the unsigned value v2 and returns their
                     product in the location pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  suint4__mult(p, v1, v2)
  sint4 *p, v1;
  uint4     v2;
{
  register sint8  r;

  r = ((sint8) v1) * ((sint8) v2);
  if (((r <  ZERO_INT8) && 
      ((((uint8) r) >> 31) != (((uint8) -1) >> 31))) ||
      ((r >= ZERO_INT8) && (r >> 31)))
    return -1;

  if (p)
    *p = (sint4) r;

  return 0;
}

/**  int  sint4__from_uint4(p, n)  :  Converts the unsigned number n
                     into a signed number, and returns the converted
                     number in the location pointed by p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/

int  sint4__from_uint4(p, n)
  sint4  *p;
  uint4   n;
{
  if (n >> 31)
    return -1;

  if (p)
    *p = (sint4) n;

  return 0;
}

/**  uint4  uint4__gcd(n1, n2)  :  Computes the greatest common
                     divisor of the two unsigned numbers n1 and n2.
                     This function cannot incur an error.          **/

uint4  uint4__gcd(n1, n2)
  uint4  n1, n2;
{
  register uint4  tmp;

  if (n1 > n2)  
    {
      tmp = n1;
      n1 = n2;
      n2 = tmp;
    }
  
  while(n1)
    {
      tmp = n2 % n1;
      n2 = n1;
      n1 = tmp;
    }

  return (n2 ? n2 : 1);
}

/**  uint4  sint4__multi_gcd(x, size)  :  Computes the
                     greatest common divisor of the signed numbers
		     stored in the array *x of size size.
                     This function cannot incur an error.          **/

uint4  sint4__multi_gcd(x, size)
  sint4  *x;
  uint4  size;
{
  register uint4  i, tmp;

  if (!size)
    return 1;

  tmp = x[0] >= 0 ? x[0] : -x[0];

  for (i = 1 ; i < size ; i++)
    tmp = uint4__gcd(tmp, x[i]>=0 ? x[i] : -x[i]);

  return (tmp ? tmp : 1);
}

/**  int  uint4__lcm(p, n1, n2)  :  Computes the lowest common
                     multiple of the two unsigned numbers n1 and n2,
                     and stores the result at the location p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  uint4__lcm(p, n1, n2)
  uint4 *p, n1, n2;
{
  register uint4  g;

  g = uint4__gcd(n1, n2);

  return  uint4__mult(p, (n1 / g), n2);
}

/**  int  sint4__power(p, x, y)  :  Computes the y-th power of x
                     and stores the result at the location p.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int sint4__power(p, x, y)
  sint4 *p, x;
  uint4  y;
{
  sint4  m, r;
  
  for (m = x, r = 1; y; y /= (sint4) 2)
    {
      if (y & 1)
#if LASH_CHECK_LEVEL < 1
	r *= m;
#else
        if (sint4__mult(&r, r, m) < 0)
	  return -1;
#endif

#if LASH_CHECK_LEVEL < 1
      m *= m;
#else
      if (sint4__mult(&m, m, m) < 0)
        return -1;
#endif
    }

  if (p)
    *p = r;

  return 0;
}

/**  int  sint4__floor(n, d, dest)  :  Computes the greatest integer
                     less or equal to the quotient of the
                     two signed numbers n and d, and stores the
		     result at the location dest.

                     In the case of success, the function returns 0.
                     If a division by zero occurs, it returns -1.  **/

int sint4__floor(n, d, dest)
     sint4 n, d, *dest;
{
  if (!d)
    return -1;

  if (d < 0)
    {
      n = -n;
      d = -d;
    }

  if (dest)
    {
      if (n >= 0 || !((-n) % d))
	*dest = n / d;
      else
	*dest = n / d - 1;  
    }

  return 0;
}

/**  int  sint4__ceil(n, d, dest)  :  Computes the lowest integer
                     greater or equal to the quotient of n by d and
		     stores the result at the location dest.

                     In the case of success, the function returns 0.
                     If a division by zero occurs, it returns -1.  **/

int sint4__ceil(n, d, dest)
     sint4 n, d, *dest;
{
  if (!d)
    return -1;

  if (d < 0)
    {
      n = -n;
      d = -d;
    }

  if (dest)
    {
      if (n < 0 || !(n % d))
	*dest = n / d;
      else
	*dest = n / d + 1;
    }
  
  return 0;
}

/**  int  sint4__rem(n, d, dest)  :  Computes r such that 0 <= r < d
                     and r = n mod d, with d > 0.

                     In the case of success, the function returns 0.
                     If d < 1,  returns -1.  **/
int  sint4__rem(n, d, dest)
     sint4 n, d, *dest;
{
  if (d < 1)
    return -1;
  *dest = n % d;
  if (*dest < 0)
    *dest += d;
  return 0;
}


/**  poly *poly__new(d, c)  :  Creates a new polynomial of degree d,
                     with the coefficients c[0], c[1], ..., c[d] (the
                     coefficient of the term of degree k is c[k]).

                     In the case of success, the function returns a
		     pointer to a newly allocated polynomial. In the
		     case of insufficient memory or of arithmetic
		     overflow, the function returns NULL and sets
		     lash_errno.                                   **/

poly *poly__new(d, c)
  uint4  d;
  sint4 *c;
{
  register poly  *pr;
  register sint4 *p;

  while (d && !(c[d]))
    d--;

  pr = resr__new_object(poly);
  if (!pr)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  p = resr__new_objects(sint4, d + 1);
  if (!p)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pr -> coeff  = p;
  pr -> degree = d;

  memcpy(p, c, (d + 1) * sizeof(sint4));

  return pr;
}

/**  void  poly__free(p)  :  Unallocates the polynomial *p.        **/

void  poly__free(p)
  poly *p;
{
#if LASH_CHECK_LEVEL >= 1
  if (!p)
    return;
#endif  /* >= 1 */

  resr__free_objects(p -> coeff, sint4, p -> degree + 1);
  resr__free_object(p, poly);
}

/**  void  poly__reduce(p)  :  Divides the coefficients of the 
                     polynomial *p by their greatest common divisor.
                     This function cannot incur an error.          **/

void  poly__reduce(p)
  poly *p;
{
  register uint4  i;
           uint4  v;

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    return;
#endif

  for (i = 1, v = sint4__abs(p -> coeff[0]);
       (i <= p -> degree) && (v > 1); i++)
    v = uint4__gcd(v, sint4__abs(p -> coeff[i]));

  if (v > 1)
    for (i = 0; i <= p -> degree; i++)
      p -> coeff[i] = sint4__over_uint4(p -> coeff[i], v);
}

/**  poly *poly__sub(p1, p2)  :  Computes the difference of the
                     polynomial *p1 and of the polynomial *p2.  In the
                     case of success, the function returns a pointer
                     to a newly allocated polynomial. In the case of
                     insufficient memory or of arithmetic overflow,
                     the function returns NULL and sets lash_errno.
		                                                   **/

poly *poly__sub(p1, p2)
  poly *p1, *p2;
{
  register uint4  d, i;
  register poly  *pr;
  register sint4 *p;

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2)
    return NULL;
#endif  /* >= 1 */

  pr = resr__new_object(poly);
  if (!pr)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  if (p1 -> degree == p2 -> degree)
    {
      d = p1 -> degree;
      while(d && p1 -> coeff[d] == p2 -> coeff[d])
        d--;
    }
  else
    d = (p1 -> degree > p2 -> degree) ? p1 -> degree : p2 -> degree;

  p = resr__new_objects(sint4, d + 1);
  if (!p)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pr -> degree = d;
  pr -> coeff  = p;

  if (p1 -> degree > p2 -> degree)
    {
      for (i = 0; i <= p2 -> degree; i++)
#if LASH_CHECK_LEVEL < 1
	p[i] = p1 -> coeff[i] - p2 -> coeff[i];
#else
        if (sint4__add(p + i, p1 -> coeff[i], -(p2 -> coeff[i])) < 0)
	  {
            resr__free_objects(p, sint4, d + 1);
	    resr__free_object(pr, poly);
	    lash_errno = LASH_ERR_OVERFLOW;
	    return NULL;    
	  }
#endif
      memcpy(p + (p2 -> degree) + 1, (p1 -> coeff) + 
          (p2 -> degree) + 1, ((p1 -> degree) - (p2 -> degree))
          * sizeof(sint4));
    }
  else
    {
      for (i = 0; i <= p1 -> degree; i++)
#if LASH_CHECK_LEVEL < 1
	p[i] = p1 -> coeff[i] - p2 -> coeff[i];
#else
        if (sint4__add(p + i, p1 -> coeff[i], -(p2 -> coeff[i])) < 0)
	  {
            resr__free_objects(p, sint4, d + 1);
	    resr__free_object(pr, poly);
	    lash_errno = LASH_ERR_OVERFLOW;
	    return NULL;    
	  }
#endif
	memcpy(p + (p1 -> degree) + 1, (p1 -> coeff) + 
	  (p1 -> degree) + 1, ((p2 -> degree) - (p1 -> degree))
          * sizeof(sint4));
    }

  return pr;
}

/**  poly *poly__uint4_mult(p, c)  :  Multiplies the coefficients of
                     the polynomial *p by the constant c. In the case
                     of success, the function returns a pointer to a
                     newly allocated polynomial. In the case of
                     insufficient memory or of arithmetic overflow,
                     the function returns NULL and sets lash_errno.
		                                                   **/
poly *poly__uint4_mult(p, c)
  poly  *p;
  uint4  c;
{
  register poly  *pr;
  register uint4  d, i;
  register sint4 *s;

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    return NULL;
#endif  /* >= 1 */

  pr = resr__new_object(poly);
  if (!pr)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  d = c ? (p -> degree) : ZERO_INT4;

  s = resr__new_objects(sint4, d + 1);
  if (!s)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pr -> degree = d;
  pr -> coeff = s;

  if (c)
    {
      for (i = 0; i <= d; i++, s++)
#if LASH_CHECK_LEVEL < 1
        *s = p -> coeff[i] * (sint4) c;
#else
        if (suint4__mult(s, p -> coeff[i], c) < 0)
	  {
	    resr__free_objects(pr -> coeff, sint4, d + 1);
	    resr__free_object(pr, poly);
	    return NULL;
  	  }
#endif
    }
  else
    s[0] = ZERO_INT4;

  return pr;
}

/**  poly *poly__uint4_div(p, c)  :  Divides the coefficients of
                     the polynomial *p by the constant c (rounding to
                     the result of the integer division). In the case
                     of success, the function returns a pointer to a
                     newly allocated polynomial. In the case of
                     insufficient memory, arithmetic overflow, or
                     arithmetic exception, the function returns NULL
                     and sets lash_errno.                          **/

poly *poly__uint4_div(p, c)
  poly  *p;
  uint4  c;
{
  register poly  *pr;
  register uint4  d, i;
  register sint4 *s;

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    return NULL;
#endif  /* >= 1 */

  if (!c)
    {
      lash_errno = LASH_ERR_EXCEPTION;
      return NULL;
    }

  pr = resr__new_object(poly);
  if (!pr)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  d = p -> degree;

  s = resr__new_objects(sint4, d + 1);
  if (!s)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pr -> degree = d;
  pr -> coeff  = s;

  for (i = 0; i <= d; i++, s++)
    *s = sint4__over_uint4(p -> coeff[i], c);

  return pr;
}

/**  poly *poly__mult(p1, p2)  :  Computes the product of the
                     polynomial *p1 by the polynomial *p2.  In the
                     case of success, the function returns a pointer
                     to a newly allocated polynomial. In the case of
                     insufficient memory or of arithmetic overflow,
                     the function returns NULL and sets lash_errno.  
		                                                   **/

poly *poly__mult(p1, p2)
  poly *p1, *p2;
{
  register uint4  i, j;
  register sint4 *p;
  register poly  *pr;
           uint4  d;
           sint4  v1;

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2)
    return NULL;
#endif  /* >= 1 */

  pr = resr__new_object(poly);
  if (!pr)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

#if LASH_CHECK_LEVEL < 1
  d = (p1 -> degree) + (p2 -> degree);
#else
  if (uint4__add(&d, p1 -> degree, p2 -> degree) < 0)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_OVERFLOW;
      return NULL;
    }
#endif

  p = resr__new_objects(sint4, d + 1);
  if (!p)
    {
      resr__free_object(pr, poly);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pr -> degree = d;
  pr -> coeff  = p;

  memset(p, 0, (d + 1) * sizeof(sint4));

  for (i = 0; i <= p1 -> degree; i++)
    for (j = 0; j <= p2 -> degree; j++)
#if LASH_CHECK_LEVEL < 1
      p[i + j] += (p1 -> coeff[i]) * (p2 -> coeff[j]);
#else
      if (sint4__mult(&v1, p1 -> coeff[i], p2 -> coeff[j]) < 0 ||
	  sint4__add(p + i + j, p[i + j], v1) < 0)
	{
	  resr__free_objects(p, sint4, d + 1);
	  resr__free_object(pr, poly);
          lash_errno = LASH_ERR_OVERFLOW;
	  return NULL; 
	}
#endif

  return pr;
}

/**  poly *poly__div(p1, p2, pden)  :  Computes the integer quotient
                     of the polynomial *p1 by the polynomial *p2.  In
                     the case of success, the function returns a
                     pointer to a newly allocated polynomial, as well
                     as a positive integer in the location pointed by
                     pden. Each coefficient of the returned polynomial
                     must be divided by that integer in order to get
                     the quotient (this denominator is introduced in
                     order to allow a result with rational
                     coefficients). In the case of insufficient
                     memory, arithmetic overflow or arithmetic
                     exception, the function returns NULL and sets
                     lash_errno.                                   **/

poly *poly__div(p1, p2, pden)
  poly  *p1, *p2;
  uint4 *pden;
{
  register uint4     i, dquo;
  register rational *p;
  register poly     *pol;
  register sint4    *s;
           sint4     zero[1] = { ZERO_INT4 };
           rational *rem, *quo;

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2 || !pden)
    return NULL;
#endif  /* >= 1 */

  if ((p1 -> degree) < (p2 -> degree))
    {
      *pden = 1;
      return poly__new(0, zero);
    }

  if (poly__is_zero(p2))
    {
      lash_errno = LASH_ERR_EXCEPTION;
      return NULL;
    }

  dquo = (p1 -> degree) - (p2 -> degree);

  if (poly_divide(p1, p2, &quo, &rem) < 0)
    return NULL;

  resr__free_objects(rem, rational, (p2 -> degree) + 1);

  for (i = 0, *pden = 1, p = quo; i <= dquo; i++, p++)
    if (uint4__lcm(pden, *pden, p -> den) < 0)
      {
	resr__free_objects(quo, rational, dquo + 1);
	lash_errno = LASH_ERR_OVERFLOW;
	return NULL;
      }

  pol = resr__new_object(poly);
  if (!pol)
    {
      resr__free_objects(quo, rational, dquo + 1);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  s = resr__new_objects(sint4, dquo + 1);
  if (!s)
    {
      resr__free_object(pol, poly);
      resr__free_objects(quo, rational, dquo + 1);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pol -> degree = dquo;
  pol -> coeff  = s;

  for (i = 0, p = quo; i <= dquo; i++, p++, s++)
    if (suint4__mult(s, p -> num, (*pden) / (p -> den)) < 0)
      {
	resr__free_objects(pol -> coeff, sint4, dquo + 1);
	resr__free_object(pol, poly);
	resr__free_objects(quo, rational, dquo + 1);
	lash_errno = LASH_ERR_OVERFLOW;
	return NULL;
      }

  resr__free_objects(quo, rational, dquo + 1);

  return pol;
}

/**  poly *poly__mod(p1, p2, pden)  :  Computes the remainder of the
                     division of the polynomial *p1 by the polynomial
		     *p2. In the case of success, the function returns
		     a pointer to a newly allocated polynomial, as
		     well as a positive integer in the location
		     pointed by pden. Each coefficient of the returned
		     polynomial must be divided by that integer in
		     order to get the remainder (this denominator is
		     introduced in order to allow a result with
		     rational coefficients). In the case of
		     insufficient memory, arithmetic overflow or
		     arithmetic exception, the function returns NULL
		     and sets lash_errno.                          **/

poly *poly__mod(p1, p2, pden)
  poly  *p1, *p2;
  uint4 *pden;
{
  register uint4     i, drem, dremred;
  register rational *p;
  register poly     *pol;
  register sint4    *s;
           rational *rem, *quo;
           sint4     zero[1] = { 0 };

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2 || !pden)
    return NULL;
#endif  /* >= 1 */

  if ((p1 -> degree) < (p2 -> degree))
    {
      *pden = 1;
      return poly__new(p1 -> degree, p1 -> coeff);
    }

  if (!(p2 -> degree))
    {
      if (p2 -> coeff[0])
	{
	  *pden = 1;
	  return poly__new(0, zero);
	}
      else
	{
	  lash_errno = LASH_ERR_EXCEPTION;
	  return NULL;
	}
    }

  drem = p2 -> degree - 1;
  if (poly_divide(p1, p2, &quo, &rem) < 0)
    return NULL;

  resr__free_objects(quo, rational, (p1 -> degree) - (p2 -> degree)
    + 1);

  for (i = 0, *pden = 1, p = rem, dremred = 0; i <= drem; i++, p++)
    if (p -> num)
      {
	dremred = i;
	if (uint4__lcm(pden, *pden, p -> den) < 0)
	  {
	    resr__free_objects(rem, rational, drem + 2);
	    lash_errno = LASH_ERR_OVERFLOW;
	    return NULL;
	  }
      }

  pol = resr__new_object(poly);
  if (!pol)
    {
      resr__free_objects(rem, rational, drem + 2);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  s = resr__new_objects(sint4, dremred + 1);
  if (!s)
    {
      resr__free_object(pol, poly);
      resr__free_objects(rem, rational, drem + 2);
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  pol -> degree = dremred;
  pol -> coeff  = s;

  for (i = 0, p = rem; i <= dremred; i++, p++, s++)
    if (suint4__mult(s, p -> num, (*pden) / (p -> den)) < 0)
      {
	resr__free_objects(pol -> coeff, sint4, dremred + 1);
	resr__free_object(pol, poly);
	resr__free_objects(rem, rational, drem + 2);
	lash_errno = LASH_ERR_OVERFLOW;
	return NULL;
      }

  resr__free_objects(rem, rational, drem + 2);

  return pol;
}

/**  poly *poly__gcd(p1, p2)  :  Computes the greatest common
                     divisor of the polynomials *p1 and *p2.  In the
                     case of success, the function returns a pointer
                     to a newly allocated polynomial. Since the gcd of
                     two polynomials is only defined up to a
                     multiplicative constant, the result is scaled
                     such that the gcd of its coefficients is 1.  In
                     the case of insufficient memory, arithmetic
                     overflow or arithmetic exception, the function
                     returns NULL and sets lash_errno.             **/

poly *poly__gcd(p1, p2)
  poly  *p1, *p2;
{
  register poly  *a1, *a2, *p3;
           uint4  v;

#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2)
    return NULL;
#endif  /* >= 1 */

  a1 = poly__new(p1 -> degree, p1 -> coeff);
  if (!a1)
    return NULL;

  a2 = poly__new(p2 -> degree, p2 -> coeff);
  if (!a2)
    {
      poly__free(a1);
      return NULL; 
    }

  if (a1 -> degree > a2 -> degree)
    {
      p3 = a1;
      a1 = a2;
      a2 = p3;
    }

  while (!poly__is_zero(a1))
    {
      p3 = poly__mod(a2, a1, &v);
      poly__free(a2);
      if (!p3)
	{
	  poly__free(a1);
	  return NULL;
	}
      poly__reduce(p3);
      a2 = a1;
      a1 = p3;
    }

  poly__free(a1);
  if (poly__is_zero(a2))
    {
      a2 -> coeff[0] = 1;
      return a2;
    }

  poly__reduce(a2);

  return a2;
}

/****  End of arithmetic.c  ****/
