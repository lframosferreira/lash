/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-transformations.c  :  Linear transformations over sets  **/
/**                 represented as Number Decision Diagrams.       **/
/**                                                                **/
/**    01/05/99  :  Creation. (BB)                                 **/
/**    01/12/99  :  Continued. (BB)                                **/
/**    01/26/99  :  Minor corrections. (BB)                        **/
/**    01/28/99  :  Power of transformations. (BB)                 **/
/**    02/02/99  :  Improved linear transformation info. (BB)      **/
/**    02/16/99  :  Improved projection. (BB)                      **/
/**    02/19/99  :  Minor corrections. (BB)                        **/
/**    08/17/01  :  Minor corrections. (BB)                        **/
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
#include "ndd.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "matrix.h"
#include "lash-auto-operations.h"

/****  Public visible functions.                                 ****/

/**  linear_transf *ndd_create_identity_transf(n)  :  Creates a linear
                     transformation corresponding to the operation
                     x : = x. The dimension of the vector x is n.

		     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid dimension.  **/
 
linear_transf *ndd_create_identity_transf(n)
  uint4  n;
{
  register uint4          i, j;
  register linear_transf *tr;
  register sint4         *s;

  diag__enter("ndd_create_identity_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  tr -> dim  = n;
  tr -> gdim = 0;
  tr -> gcoeff = tr -> goffset = NULL;

  s = resr__new_objects(sint4, n * n);
  if (!s)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> coeff = s;

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++, s++)
      *s = (i == j) ? 1 : ZERO_INT4;

  s = resr__new_objects(sint4, n);
  if (!s)
    {
      resr__free_objects(tr -> coeff, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> offset = s;
  memset(s, 0, n * sizeof(sint4));

  diag__return(tr);
}

/**  linear_transf *ndd_create_transf(n, m, a, b, p, q)  :  Creates
                     a linear transformation corresponding to the
                     operation  p x <= q  --> x := a x + b.
                     The matrices p and a are respectively of sizes
                     m x n and n x n; they are given as sequences
                     of coefficients organized line by line. The
                     vectors q and b are respectively of sizes m and
                     n.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/
 
linear_transf *ndd_create_transf(n, m, a, b, p, q)
  uint4  n, m;
  sint4 *a, *b, *p, *q;
{
  register linear_transf *tr;
  register sint4         *sc, *so, *gc, *go;

  diag__enter("ndd_create_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !b || (m && (!p || !q)))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (m)
    {
      gc = resr__new_objects(sint4, m * n);
      if (!gc)
	{
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_object(tr, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      go = resr__new_objects(sint4, m);
      if (!go)
	{
	  resr__free_objects(gc, sint4, m * n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_object(tr, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      memcpy(gc, p, n * m * sizeof(sint4));
      memcpy(go, q, m * sizeof(sint4));
    }
  else
    gc = go = NULL;

  memcpy(sc, a, n * n * sizeof(sint4));
  memcpy(so, b, n * sizeof(sint4));

  tr -> dim        = n;
  tr -> gdim       = m;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  return tr;
}

/**  linear_transf *ndd_create_assign_transf(n, p, a, b)  :  Creates 
                     a linear transformation corresponding to the
                     assignment x(p) := a[0] x(0) + a[1] x(1) + ...  +
                     a[n-1] x(n-1) + b.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

linear_transf *ndd_create_assign_transf(n, p, a, b)
  uint4  n, p;
  sint4 *a, b;
{
  register linear_transf *tr;
  register sint4         *sc, *so;
  register uint4          i, j;

  diag__enter("ndd_create_assign_transf", NULL);

  if (!n || (n > 0xffff) || (p >= n))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  tr -> dim        = n;
  tr -> gdim       = ZERO_INT4;
  tr -> gcoeff     = tr -> goffset = NULL;
  tr -> coeff      = sc;
  tr -> offset     = so;

  for (i = 0; i < n; i++, so++)
    {
      for (j = 0; j < n; j++, sc++)
	*sc = (i == p) ? a[j] : ((i == j) ? 1 : ZERO_INT4);
      *so = (i == p) ? b : ZERO_INT4;
    } 

  diag__return(tr);
}

/**  linear_transf *ndd_create_inequ_transf(n, p, q)  :  Creates a
                     linear transformation corresponding to the
                     inequality  p[0] x(0) + p[1] x(1) + ...
                     + p[n-1] x(n-1) <= q.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

linear_transf *ndd_create_inequ_transf(n, p, q)
  uint4  n;
  sint4 *p, q;
{
  register linear_transf *tr;
  register sint4         *sc, *so, *gc, *go;
  register uint4          i, j;

  diag__enter("ndd_create_inequ_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  gc = resr__new_objects(sint4, n);
  if (!gc)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  go = resr__new_object(sint4);
  if (!go)
    {
      resr__free_objects(gc, sint4, n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> dim        = n;
  tr -> gdim       = 1;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  for (i = 0; i < n; i++, so++, gc++)
    {
      for (j = 0; j < n; j++, sc++)
	*sc = (i == j) ? 1 : ZERO_INT4;
      *so = ZERO_INT4;
      *gc = p[i];
    }

  *go = q;

  diag__return(tr);
}

/**  linear_transf *ndd_create_equ_transf(n, p, q)  :  Creates a
                     linear transformation corresponding to the
                     equality p[0] x(0) + p[1] x(1) + ... + p[n] x(n)
                     = q.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

linear_transf *ndd_create_equ_transf(n, p, q)
  uint4  n;
  sint4 *p, q;
{
  register linear_transf *tr;
  register sint4         *sc, *so, *gc, *go;
  register uint4          i, j;

  diag__enter("ndd_create_equ_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  gc = resr__new_objects(sint4, 2 * n);
  if (!gc)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  go = resr__new_objects(sint4, 2);
  if (!go)
    {
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> dim        = n;
  tr -> gdim       = 2;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  for (i = 0; i < n; i++, so++, gc++)
    {
      for (j = 0; j < n; j++, sc++)
	*sc = (i == j) ? 1 : ZERO_INT4;
      *so = ZERO_INT4;
      *gc = p[i];
      *(gc + n) = -p[i];
    }

  go[0] =  q;
  go[1] = -q;

  diag__return(tr);
}

/**  linear_transf  *ndd_transf_compose(t1, t2)  :  Composes the two
                     linear transformations t1 and t2 into a linear 
                     transformation f such that f(x) = t2(t1(x)) for
                     every x.

                     In the case of success, this function returns a
                     pointer to a newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt 
                                               transformation(s).
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_DIMENSION  : Dimension mismatch. **/

linear_transf *ndd_transf_compose(t1, t2)
  linear_transf *t1, *t2;
{
  register uint4          n, m;
  register linear_transf *tr;
  register sint4         *sc, *so, *gc, *go;

  diag__enter("ndd_transf_compose", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!t1 || !t2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (t1 -> dim != t2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  tr = resr__new_object(linear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  n = tr -> dim  = t1 -> dim;  
  m = tr -> gdim = t1 -> gdim + t2 -> gdim;

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (m)
    {
      gc = resr__new_objects(sint4, m * n);
      if (!gc)
	{
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_objects(so, sint4, n);
	  resr__free_object(tr, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      go = resr__new_objects(sint4, m);
      if (!go)
	{
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(gc, sint4, m * n);
	  resr__free_object(tr, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
    }
  else
    gc = go = NULL;

  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  if (t1 -> gdim)
    {
      memcpy(gc, t1 -> gcoeff,  (t1 -> gdim) * n * sizeof(sint4));
      memcpy(go, t1 -> goffset, (t1 -> gdim) * sizeof(sint4));
    }

  if (matrix__mult(sc, t2 -> coeff, t1 -> coeff,  n, n, n) < 0 ||
      matrix__mult(so, t2 -> coeff, t1 -> offset, n, n, 1) < 0 ||
      matrix__add(so, so, t2 -> offset, n, 1) < 0 ||
      (t2 -> gdim && 
          (matrix__mult(gc + (t1 -> gdim) * n, t2 -> gcoeff, 
           t1 -> coeff, (t2 -> gdim), n, n) < 0 ||
           matrix__mult(go + (t1 -> gdim), t2 -> gcoeff, 
           t1 -> offset, (t2 -> gdim), n, 1) < 0 ||
           matrix__const_mult(go + (t1 -> gdim), (t2 -> gdim), 1,
           -1) < 0 ||
           matrix__add(go + (t1 -> gdim), go + (t1 -> gdim), 
           t2 -> goffset, (t2 -> gdim), 1) < 0)))
    {
      if (m)
	{
	  resr__free_objects(go, sint4, m);
	  resr__free_objects(gc, sint4, m * n);
	}
      resr__free_objects(so, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, linear_transf);
      diag__fail(LASH_ERR_OVERFLOW, NULL);
    }

  diag__return(tr);
}

/**  linear_transf *ndd_transf_power(tr, k)  :  Computes the k-th
                     power of the linear transformation *tr.

                     In the case of success, this function returns
		     a pointer to a newly allocated transformation.
		     In the case of an error, it returns a NULL
		     pointer.

		     Possible error codes:

                         LASH_ERR_NOT_INIT  : Not initialized.
			 LASH_ERR_NO_MEM    : Not enough memory.
                         LASH_ERR_OVERFLOW  : Arithmetic overflow.
                         LASH_ERR_CORRUPT   : Corrupt transformation.
                                                                   **/

linear_transf *ndd_transf_power(tr, k)
  linear_transf *tr;
  uint4          k;
{
  register linear_transf *t, *tm, *tt;

  diag__enter("ndd_transf_power", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  tm = ndd_transf_copy(tr);
  if (!tm)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  t = ndd_create_identity_transf(tr -> dim);
  if (!t)
    {
      ndd_transf_free(tm);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  while (k)
    {
      if (k & 1)
        {
	  tt = ndd_transf_compose(t, tm);
	  ndd_transf_free(t);
	  if (!tt)
	    {
	      ndd_transf_free(tm);
	      diag__fail(lash_errno, NULL);
	    }
	  t = tt;
        }
      
      k /= (uint4) 2;
      if (!k)
	break;

      tt = ndd_transf_compose(tm, tm);
      ndd_transf_free(tm);
      if (!tt)
	{
	  ndd_transf_free(t);
	  diag__fail(lash_errno, NULL);
	}
      tm = tt;
    }

  ndd_transf_free(tm);
  diag__return(t);
}

/**  int  ndd_transf_free(t)  :  Frees the linear transformation *t.

                     If successful, returns 0. In the case of an 
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt transformation.
                                                                   **/
int ndd_transf_free(t)
  linear_transf *t;
{
  register uint4  n, m;

  diag__enter("ndd_transf_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!t)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  n = t -> dim;
  m = t -> gdim;

  resr__free_objects(t -> coeff, sint4, n * n);
  resr__free_objects(t -> offset, sint4, n);

  if (m)
    {
      resr__free_objects(t -> gcoeff, sint4, n * m);
      resr__free_objects(t -> goffset, sint4, m);
    }
     
  resr__free_object(t, linear_transf);

  diag__return(0);  
}

/**  linear_transf *ndd_transf_copy(tr)  :  Creates a new linear 
                     transformation identical to *tr. In the case of
                     success, this function returns a pointer to a
		     newly allocated transformation. Otherwise, it
		     returns a NULL pointer.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt transformation.
                                                                   **/

linear_transf *ndd_transf_copy(tr)
  linear_transf *tr;
{
  register linear_transf *t;
  register uint4          n, m;
  register sint4         *s;

  diag__enter("ndd_transf_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */  

  t = resr__new_object(linear_transf);
  if (!t)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  t -> dim  = n = tr -> dim;
  t -> gdim = m = tr -> gdim;

  s = resr__new_objects(sint4, n * n);
  if (!s)
    {
      resr__free_object(t, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(s, tr -> coeff, n * n * sizeof(sint4));
  t -> coeff = s;

  s = resr__new_objects(sint4, n);
  if (!s)
    {
      resr__free_objects(t -> coeff, sint4, n * n);
      resr__free_object(t, linear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(s, tr -> offset, n * sizeof(sint4));
  t -> offset = s;

  if (m)
    {
      s = resr__new_objects(sint4, n * m);
      if (!s)
	{
	  resr__free_objects(t -> offset, sint4, n);
	  resr__free_objects(t -> coeff, sint4, n * n);
	  resr__free_object(t, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      
      memcpy(s, tr -> gcoeff, n * m * sizeof(sint4));
      t -> gcoeff = s;

      s = resr__new_objects(sint4, m);
      if (!s)
	{
	  resr__free_objects(t -> gcoeff, sint4, n * m);
	  resr__free_objects(t -> offset, sint4, n);
	  resr__free_objects(t -> coeff, sint4, n * n);
	  resr__free_object(t, linear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      
      memcpy(s, tr -> goffset, m * sizeof(sint4));
      t -> goffset = s;
    }
  else
    t -> gcoeff = t -> goffset = NULL;

  diag__return(t);
}

/**  ndd *ndd_image_by_transf(nd, t)  :  Computes the image of the set
                     represented by the NDD *nd by the linear 
                     transformation *t. This function is currently
                     only implemented for NDDs that operate serially.

                     This function does not modify *nd, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_CORRUPT    : Corrupt NDD or 
                                               transformation.     **/

ndd *ndd_image_by_transf(nd, t)
  ndd           *nd;
  linear_transf *t;
{
  register linear_tr_info *lti;
  register ndd            *ndr;

  diag__enter("ndd_image_by_transf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !t)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (nd -> dim != t -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(nd -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  lti = ndd_create_transf_info(t, nd -> base, 
      !!(nd -> properties & NDD_PROP_MSDF));
  if (!lti)
    diag__fail(lash_errno, NULL);

  ndr = ndd_image_by_transf_info(nd, lti);
    ndd_transf_info_free(lti);
  if (!ndr)
    diag__fail(lash_errno, NULL);
  
  diag__return(ndr);
}

/**  linear_tr_info *ndd_create_transf_info(tr, r, msdf)  :  Creates
                     a transducer associated to the linear
                     transformation *tr, whose purpose is to speed up
                     the computation of the image of sets by *tr. The
                     transducer is an NDD accepting all the vectors of
                     the form [x0 x0' x1 x1' ... x(n-1) x(n-1)'], 
                     where [x0' x1' ... x(n-1)'] = tr([x0 x1 ... 
                     x(n-1)]). The numeration base used for creating
                     that NDD is r. Numbers are accepted most 
                     significant digit first if msdf has a nonzero
                     value, and least significant digit first 
                     otherwise.

                     This function does not modify *tr, and returns
                     (in the case of success) a pointer to a newly
                     allocated linear transformation information. In
                     the case of an error, it returns a NULL pointer
                     and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid base. 
                         LASH_ERR_CORRUPT    : Corrupt transformation.
                                                                   **/

linear_tr_info *ndd_create_transf_info(tr, r, msdf)
  linear_transf *tr;
  uint1          r;
  int            msdf;
{
  register uint4  n, m, i, j;
  register sint4 *s;
  register ndd   *nd1, *nd2, *nd3, *nd4;
  register ndd   *(*f)(uint1, uint4, sint4 *, sint4);

  diag__enter("ndd_create_transf_info", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  n = tr -> dim;
  m = tr -> gdim;

  s = resr__new_objects(sint4, 2 * n);
  if (!s)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  for (i = 0; i < n; i++)
    {
      s[2 * i] = tr -> coeff[i];
      s[2 * i + 1] = ZERO_INT4;
    }

  s[1] = -1;

  f = msdf ? ndd_create_equation_msdf : 
       ndd_create_equation_lsdf;
  nd1 = f(r, 2 * n, s, -(tr -> offset[0]));
  if (!nd1)
    {
      resr__free_objects(s, sint4, 2 * n);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (j = 1; j < n; j++)
    {
      for (i = 0; i < n; i++)
        s[2 * i] = tr -> coeff[j * n + i];
      s[2 * j - 1] =  ZERO_INT4;
      s[2 * j + 1] = -1;
      nd2 = f(r, 2 * n, s, -(tr -> offset[j]));
      if (!nd2)
	{
	  ndd_free(nd1);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      nd3 = ndd_intersection(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);
      if (!nd3)
	{
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      nd1 = nd3;
    }

  if (!m)
    {
      resr__free_objects(s, sint4, 2 * n);
      diag__return((linear_tr_info *) nd1);
    }

  for (i = 0; i < n; i++)
    {
      s[2 * i] = tr -> gcoeff[i];
      s[2 * i + 1] = ZERO_INT4;
    }

  f  = msdf ? ndd_create_inequation_msdf : 
    ndd_create_inequation_lsdf;
  nd4 = f(r, 2 * n, s, tr -> goffset[0]);
  if (!nd4)
    {
      ndd_free(nd1);
      resr__free_objects(s, sint4, 2 * n);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  for (j = 1; j < m; j++)
    {
      for (i = 0; i < n; i++)
	s[2 * i] = tr -> gcoeff[j * n + i];
      
      nd2 = f(r, 2 * n, s, tr -> goffset[j]);
      if (!nd2)
	{
	  ndd_free(nd1);
	  ndd_free(nd4);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      nd3 = ndd_intersection(nd4, nd2);
      ndd_free(nd4);
      ndd_free(nd2);
      if (!nd3)
	{
	  ndd_free(nd1);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      nd4 = nd3;
    }

  resr__free_objects(s, sint4, 2 * n);
  nd2 = ndd_intersection(nd1, nd4);
  ndd_free(nd1);
  ndd_free(nd4);
  if (!nd2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return((linear_tr_info *) nd2);
}

/**  ndd *ndd_image_by_transf_info(nd, ti)  :  Computes the image of
                     the set represented by the ndd *nd by the linear
                     transformation associated with the information
                     *ti. This function is currently only implemented
                     for NDDs that operate serially.

                     This function does not modify *nd or *ti, and
                     returns (in the case of success) a pointer to a
                     newly allocated NDD. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BASE       : Base mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_CORRUPT    : Corrupt NDD or 
                                               transformation info. 
                                                                   **/

ndd *ndd_image_by_transf_info(nd, ti)
  ndd            *nd;
  linear_tr_info *ti;
{
  register uint4  n;
  register ndd   *nd1, *nd2;

  diag__enter("ndd_image_by_transf_info", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nd || !ti)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(nd -> dim) || (2 * (nd -> dim)) != ((ndd *) ti) -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (nd -> base != ((ndd *) ti) -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((nd -> properties & NDD_PROP_MSDF) !=
      (((ndd *) ti) -> properties & NDD_PROP_MSDF))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  if (!(nd -> properties & NDD_PROP_SERIAL) ||
      !(((ndd *) ti) -> properties & NDD_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  n   = nd -> dim;
  nd2 = ndd_interleave_z(nd, 1, 0);
  if (!nd2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  nd1 = ndd_intersection(nd2, ti);
  ndd_free(nd2);
  if (!nd1)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  nd2 = ndd_multi_projection(nd1, 2, 0);
  ndd_free(nd1);
  if (!nd2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return(nd2);
}

/**  int  ndd_transf_info_free(ti)  :  Frees the linear
                     transformation information *ti.

                     If successful, returns 0. In the case of an
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt transformation
                                             information           **/

int  ndd_transf_info_free(ti)
  linear_tr_info *ti;
{
  diag__enter("ndd_transf_info_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!ti)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  ndd_free((ndd *) ti);

  diag__return(0);
}

/****  End of ndd-transformations.c  ****/
