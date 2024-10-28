/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    matrix.c  :  Operations over matrices.                      **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/14/99  :  Computation of characteristic polynomial. (BB) **/
/**    01/15/99  :  Power of matrices. (BB)                        **/
/**    01/18/99  :  Zero test. (BB)                                **/
/**    06/09/00  :  Minor correction. (BB)                         **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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
#include "arithmetic.h"
#include "matrix.h"
#include "lash-diag.h"

/****  Prototype(s) of private function(s).                      ****/

static int   poly_det_step(poly **, uint4, uint4, poly **, poly **,
                 int *);
static poly *poly_determinant(poly **, uint4);

/****  Private function(s).                                      ****/

/**  int  poly_det_step(m, n, ncur, pnum, pden, pneg)  :  This 
                     function is part of the computation of the
                     determinant of a matrix of polynomials. The
                     current value of the matrix is m, and its
                     dimension is n by n, with n > 1. The goal of this
                     function is to transform m using linear
                     operations so as to obtain a matrix whose column
                     of index (n - ncur) contains all zeros except for
                     the diagonal element which must be equal to
                     1. One may suppose that this function has already
                     been called with the current matrix for all the
                     columns left of the current one. The partially
                     computed determinant is the quotient of the
                     polynomials (*pnum) and (*pden), which are
                     relatively prime to each other. These polynomials
                     can be modified by this function.  The current
                     sign is *pneg (1 : negative, 0 positive) and can
                     also be modified. This function unallocates all
                     the elements of m whose line or column index is
                     equal to (n - ncur).  In the case of insufficient
                     memory or arithmetic overflow, this function
                     returns -1 and sets lash_errno. Otherwise, it
                     returns 0.                                    **/
   
static int  poly_det_step(m, n, ncur, pnum, pden, pneg)
  poly **m, **pnum, **pden;
  uint4  n, ncur;
  int   *pneg;
{
  register uint4  i, j, ccol;
  register poly  *p1, *p2, *p3, *pivot, *pivred, *base, *pg;
           uint4  denbase, denpivot;

  ccol = n - ncur;
  if (poly__is_zero(m[ccol * (n + 1)]))
    for (i = ccol + 1; i < n; i++)
      if (!poly__is_zero(m[i * n + ccol]))
	{
	  for (j = ccol; j < n; j++)
	    {
	      p1 = m[ccol * n + j];
              m[ccol * n + j] = m[i * n + j];
              m[i * n + j] = p1;
	    }
	  *pneg = 1 - *pneg; 
	  break;
	}

  pivot = m[ccol * (n + 1)];
  for (i = ccol + 1; i < n; i++)
    {
      if (poly__is_zero(m[i * n + ccol]))
	continue;

      pg = poly__gcd(pivot, m[i * n + ccol]);
      if (!pg)
	{
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}

      p1 = poly__div(m[i * n + ccol], pg, &denbase);
      if (!p1)
	{
	  poly__free(pg);
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}

      p2 = poly__div(pivot, pg, &denpivot);
      poly__free(pg);
      if (!p2)
	{
	  poly__free(p1);
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}

      base = poly__uint4_mult(p1, denpivot);
      poly__free(p1);
      if (!base)
	{
	  poly__free(p2);
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}

      pivred = poly__uint4_mult(p2, denbase);
      poly__free(p2);
      if (!pivred)
	{
	  poly__free(base);
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}

      for (j = ccol + 1; j < n; j++)
	{
	  p1 = poly__mult(m[i * n + j], pivred);
	  if (!p1)
	    {
	      poly__free(pivred);
	      poly__free(base);
	      for (i = ccol; i < n; i++)
		poly__free(m[i * n + ccol]);
	      for (i = ccol + 1; i < n; i++)
		poly__free(m[n * ccol + i]);
	      return -1;
	    }
	  
	  p3 = poly__mult(base, m[ccol * n + j]);
	  if (!p3)
	    {
	      poly__free(pivred);
	      poly__free(base);
	      poly__free(p1);
	      for (i = ccol; i < n; i++)
		poly__free(m[i * n + ccol]);
	      for (i = ccol + 1; i < n; i++)
		poly__free(m[n * ccol + i]);
	      return -1;
	    }
	  
	  p2 = poly__sub(p1, p3);
	  poly__free(p1);
	  poly__free(p3);
	  if (!p2)
	    {
	      poly__free(pivred);
	      poly__free(base);
	      for (i = ccol; i < n; i++)
		poly__free(m[i * n + ccol]);
	      for (i = ccol + 1; i < n; i++)
		poly__free(m[n * ccol + i]);
	      return -1;
	    }
	  
	  poly__free(m[i * n + j]);
	  m[i * n + j] = p2;
	}
      poly__free(base);
      p1 = poly__mult(*pden, pivred);
      poly__free(pivred);
      if (!p1)
	{
	  for (i = ccol; i < n; i++)
	    poly__free(m[i * n + ccol]);
	  for (i = ccol + 1; i < n; i++)
	    poly__free(m[n * ccol + i]);
	  return -1;
	}
      poly__free(*pden);
      *pden = p1;
    }

  for (i = ccol + 1; i < n; i++)
    poly__free(m[i * n + ccol]);
  for (i = ccol + 1; i < n; i++)
    poly__free(m[n * ccol + i]);

  p1 = poly__mult(pivot, *pnum);
  poly__free(pivot);
  if (!p1)
    return -1;

  pg = poly__gcd(p1, *pden);
  if (!pg)
    {
      poly__free(p1);
      return -1;
    }

  p2 = poly__div(p1, pg, &denpivot);
  poly__free(p1);
  if (!p2)
    {
      poly__free(pg);
      return -1;
    }

  p1 = poly__div(*pden, pg, &denbase);
  poly__free(pg);
  if (!p1)
    {
      poly__free(p2);
      return -1;
    }

  p3 = poly__uint4_mult(p2, denbase);
  poly__free(p2);
  if (!p3)
    {
      poly__free(p1);
      return -1;
    }
  
  p2 = poly__uint4_mult(p1, denpivot);
  poly__free(p1);
  if (!p2)
    {
      poly__free(p3);
      return -1;
    }

  poly__free(*pnum);
  poly__free(*pden);
  *pnum = p3;
  *pden = p2;

  return 0;
}

/**  poly *poly_determinant(m, n)  :  This function is called during
                     the computation of the characteristic polynomial
                     of a matrix. It computes the determinant of the
                     square matrix m, each component of which is a
                     pointer to a polynomial. In addition, this
                     function frees all the polynomials composing m.
                     The dimension of m is n by n, with n greater or
                     equal to 2. The function returns a pointer to a
                     newly allocated polynomial. In the case of
                     insufficient memory or arithmetic overflow, it
                     returns a NULL pointer and sets lash_errno.   **/

static poly *poly_determinant(m, n)
  poly **m;
  uint4  n;
{
  static   sint4  coeff[1] = { 1 };
  register uint4  i;
  register poly  *p1, *p2, *p3;
  register uint4  j, k;
           int    negative;
           uint4  dummy;
           poly  *den, *num;

  den = poly__new(0, coeff);
  if (!den)
    {
      for (i = 0; i < n * n; i++, m++)
	poly__free(*m);
      return NULL;
    }

  num = poly__new(0, coeff);
  if (!num)
    {
      poly__free(den);
      for (i = 0; i < n * n; i++, m++)
	poly__free(*m);
      return NULL;
    }
    
  for (i = n, negative = 0; i > 2; i--)
    if (poly_det_step(m, n, i, &num, &den, &negative) < 0)
      {
	for (j = n - i + 1; j < n; j++)
	  for (k = n - i + 1; k < n; k++)
	    poly__free(m[j * n + k]);
	poly__free(den);
        poly__free(num);
	return NULL;
      }

  p1 = poly__mult(m[(n - 2) * (n + 1)], m[n * n - 1]);
  poly__free(m[(n - 2) * (n + 1)]);
  poly__free(m[n * n - 1]);
  if (!p1)
    {
      poly__free(den);
      poly__free(m[n * n - n - 1]);
      poly__free(m[n * n - 2]);
      poly__free(num);
      return NULL;
    }
  
  p2 = poly__mult(m[n * n - n - 1], m[n * n - 2]);
  poly__free(m[n * n - n - 1]);
  poly__free(m[n * n - 2]);
  if (!p2)
    {
      poly__free(den);
      poly__free(p1);
      poly__free(num);
      return NULL;
    }

  p3 = negative ? poly__sub(p2, p1) : poly__sub(p1, p2);
  poly__free(p1);
  poly__free(p2);
  if (!p3)
    {
      poly__free(den);
      poly__free(num);
      return NULL;
    }

  p1 = poly__mult(p3, num);
  poly__free(num);
  poly__free(p3);
  if (!p1)
    {
      poly__free(den);
      return NULL;
    }

  p2 = poly__div(p1, den, &dummy);
  poly__free(p1);
  poly__free(den);

  return p2;
}

/****  Public invisible functions.                               ****/

/**  int  matrix__add(mr, m1, m2, n, m)  :  Adds the two matrices m1 
                     and m2 and stores the result in mr. The two
                     matrices being added as well as the result are
                     given as pointers to arrays of sint4 values
                     organized as a series of n lines, each containing
                     m values. The pointers mr, m1 and m2 are allowed
                     to coincide.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  matrix__add(mr, m1, m2, n, m)
  sint4 *mr, *m1, *m2;
  uint4  n, m;
{
  register uint4  i, j;

  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++, mr++, m1++, m2++)
#if LASH_CHECK_LEVEL < 1
      *mr = (*m1) + (*m2);
#else
      if (sint4__add(mr, *m1, *m2) < 0)
	return -1;
#endif

  return 0;
}

/**  int  matrix__mult(mr, m1, m2, n, m, p)  :  Multiplies the matrix 
                     m1 by the matrix m2, and stores the result in
                     mr. The dimension of m1 is n lines by m rows, the
                     one of m2 is m lines by p rows. The matrices m1,
                     m2 and mr are given as arrays of sint4 values
                     organized as a sequence of lines. The array
                     pointed by mr cannot overlap with those pointed
                     by m1 and m2.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/
int  matrix__mult(mr, m1, m2, n, m, p)
  sint4  *mr, *m1, *m2;
  uint4   n, m, p;
{
  register uint4  i, j, k;
  register sint4  a;
           sint4  b;

  for (i = 0; i < n; i++)
    for (j = 0; j < p; j++, mr++)
      {
	a = ZERO_INT4;
	for (k = 0; k < m; k++)
	  {
#if LASH_CHECK_LEVEL < 1
            a += m1[i * m + k] * m2[k * p + j];
#else
            if (sint4__mult(&b, m1[i * m + k], m2[k * p + j]) < 0)
              return -1;
            a += b;
#endif
	  }
	*mr = a;
      }
  return 0;
}

/**  int  matrix__power(mr, m1, n, k)  :  Computes the k-th power of
                     the matrix m1, and stores the result in mr. The
		     dimensions of m1 and mr is n lines by n rows.
                     Each matrix is organized as a sequence of lines.
		     The arrays pointed by m1 and mr cannot overlap.
		     
		     In the case of success, the function returns 0.
                     If the case of arithmetic overflow or of 
                     insufficient memory, it returns -1 ans sets
                     lash_errno.                                   **/

int  matrix__power(mr, m1, n, k)
  sint4 *mr, *m1;
  uint4  n, k;
{
  register sint4 *m, *mt;
  register uint4  i, j, l; 

  m = resr__new_objects(sint4, n * n);
  if (!m)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  mt = resr__new_objects(sint4, n * n);
  if (!mt)
    {
      resr__free_objects(m, sint4, n * n);
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  for (i = l = 0; i < n; i++)
    for (j = 0; j < n; j++, l++)
      {
	m[l]  = m1[l];
	mr[l] = (i == j) ? 1 : ZERO_INT4;
      }
  
  for (; k; k /= (uint4) 2)
    {
      if (k & 1)
	{
	  memcpy(mt, mr, n * n * sizeof(sint4));
	  if (matrix__mult(mr, mt, m, n, n, n) < 0)
	    {
	      resr__free_objects(m, sint4, n * n);
	      resr__free_objects(mt, sint4, n * n);
	      lash_errno = LASH_ERR_OVERFLOW;
	      return -1;
	    }
	}
      memcpy(mt, m, n * n * sizeof(sint4));
      if (matrix__mult(m, mt, mt, n, n, n) < 0)
	{
	  resr__free_objects(m, sint4, n * n);
	  resr__free_objects(mt, sint4, n * n);
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}
    }
  resr__free_objects(m, sint4, n * n);
  resr__free_objects(mt, sint4, n * n);

  return 0;
}

/**  int  matrix__const_mult(m1, n, m, c)  :  Multiplies the elements
                     of the matrix m1 by the constant c. The matrix
                     is given as an array of values organized as a
                     sequence of n lines, each containing m values.

                     In the case of success, the function returns 0.
                     If an arithmetic overflow occurs, it returns -1.
                                                                   **/

int  matrix__const_mult(m1, n, m, c)
  sint4 *m1, c;
  uint4  n, m;
{
  register uint4  i, j;

  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++, m1++)
#if LASH_CHECK_LEVEL < 1
      *m1 *= c;
#else
      if (sint4__mult(m1, *m1, c) < 0)
	return -1;
#endif

  return 0;
}

/**  int  matrix__is_zero(m1, n, m)  :  Tests whether all the 
                     coefficients of the matrix m1 (which has n lines
                     and m columns) are equal to zero. Returns 1 if
                     the test is positive, and 0 otherwise.        **/

int  matrix__is_zero(m1, n, m)
  sint4  *m1;
  uint4   n, m;
{
  register uint4  i, j;

  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++, m1++)
      if (*m1)
	return 0;

  return 1;
}

/**  poly *matrix__characteristic_poly(m1, n)  :  Computes the 
                     characteristic polynomial of the square matrix
                     m1. The matrix is given as an array of values
                     organized as a sequence of n lines, each
                     containing n values.

                     In the case of success, the function returns
                     a pointer to a newly allocated polynomial. In the
                     case of an error, it returns a NULL pointer. The
                     global variable lash_errno is set to 
                     LASH_ERR_NO_MEM if the cause of the error is
                     insufficient memory, or to LASH_ERR_OVERFLOW if
                     an arithmetic overflow occurred.              **/

poly *matrix__characteristic_poly(m1, n)
  sint4 *m1;
  uint4  n;
{
  static   sint4  coeff[2];
  register uint4  i, j;
  register poly **m, **mp, *pr;

  switch(n)
    {
    case 0 :
      coeff[0] = 1;
      return poly__new(0, coeff);
      
    case 1 :
      coeff[0] = m1[0];
      coeff[1] = -1;
      return poly__new(1, coeff);

    default :
      break;
    }

  m = resr__new_objects(poly *, n * n);
  if (!m)
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }

  coeff[1] = -1;

  for (i = 0, mp = m; i < n; i++)
    for (j = 0; j < n; j++, m1++, mp++)
      {
	coeff[0] = *m1;
	*mp = poly__new(!(i != j), coeff);
	if (!*mp)
	  {
	    resr__free_objects(m, poly *, n * n);
	    return NULL;
	  }
      }

  pr = poly_determinant(m, n);
  resr__free_objects(m, poly *, n * n);

  return pr;
}

/****  End of matrix.c  ****/
