/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-transformations.c  :  Linear transformations over sets  **/
/**                 represented as Real Vector Automata.           **/
/**                                                                **/
/**    03/26/01  :  Creation. (SJ)                                 **/
/**    03/28/01  :  Continued. (SJ)                                **/
/**    03/29/01  :  Continued. (SJ)                                **/
/**    05/16/01  :  Usage of the restricted RVA flag. (SJ)         **/
/**    08/13/01  :  Small adaptation. (BB)                         **/
/**    08/20/01  :  Minor corrections. (SJ)                        **/
/**    04/18/02  :  Added 'rva_transf_info_copy'. (SJ)             **/
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
#include "datastruct.h"
#include "resource.h"
#include "matrix.h"
#include "lash-auto-operations.h"

/****  Prototypes of private functions.                          ****/

static int  normalize_linear_vector(uint4, sint4 *, sint4 *, sint4,
                  sint4, sint4 *, sint4 *, uint4 *);
static int  normalize_linear_matrix(uint4, uint4, sint4 *, sint4 *, 
                  sint4 *, sint4 *, sint4 *, sint4 *, uint4 *);
static int  transf_common_ratio(uint4, uint4, sint4 *, sint4 *, 
				uint4 *);
static void transf_minimize(uint4, uint4, sint4 *, sint4 *, uint4 *);

/****  Private functions.                                        ****/

/**  int  normalize_linear_vector(n, an, ad, bn, bd, a, b, denom)  :
                     This function reduces to the same denominator
		     the linear constraint
		     (an[0] / ad[0]) x(1) + (an[1] / ad[1]) x(2) +
		     ... + (an[n-1] / ad[n-1]) x(n) = (bn / bd). If
		     ad == NULL, all the denominators are taken equal
		     to 1. At the end of the execution, the linear
		     constraint a[0] x(1) + ... + a[n-1] x(n-1) = *b
		     is equivalent to the initial
		     constraint and the integer *denom contains
		     the greatest common denominator. Vectors an,
		     ad (if not NULL) and a must be of size n and
		     must have been allocated. *denom can be NULL, in
		     which case the calling function does not care
		     about the denominator.

		     In the case of success, this function returns 0.
		     In the case of an error, it returns -1 and sets
		     lash_errno.

                     Possible error codes:

		         LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : One of the 
			                       denominators equals 
					       zero.               **/

static int  normalize_linear_vector(n, an, ad, bn, bd, a, b, denom)
     uint4   n;
     sint4  *an, *ad, bn, bd, *a, *b;
     uint4  *denom;
{
  register uint4 lcm, i, den;
           uint4 tmp;
	   sint4 stmp;
  
  diag__enter("normalize_linear_vector", -1);

  if (!an || !a || !b || !bd)
    diag__fail(LASH_ERR_BAD_VALUE, -1);

  if (!ad && bd == 1)
    {
      memcpy(a, an, n * sizeof(sint4));
      *b = bn;
      if (denom)
	*denom = 1;
      diag__return(0);
    }

  if (bd<0)
    {
      bn = -bn;
      bd = -bd;
    }

  if (ad)
    {
      lcm = bd / uint4__gcd( bn >= 0 ? bn : -bn , bd);

      for (i = 0; i < n; i++)
	{
	  if (!ad[i])
	    diag__fail(LASH_ERR_BAD_VALUE, -1);
    
	  den = ad[i]>=0 ? ad[i] : -ad[i];
	  den = den / uint4__gcd( an[i] >= 0 ? an[i] : -an[i] , den);
	  if (uint4__mult(&tmp, lcm, den) < 0)
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  lcm = tmp / uint4__gcd(lcm, den);
	}

      if (sint4__mult(&stmp, bn, (sint4) lcm) < 0)
	diag__fail(LASH_ERR_OVERFLOW, -1);
      *b = stmp / bd;

      for (i = 0; i < n; i++)
	{
	  if (sint4__mult(&stmp, an[i], (sint4) lcm) < 0)
	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  a[i] = stmp / ad[i];
	}

      if (denom)
	*denom = lcm;
    }

  else
    {
      lcm = uint4__gcd( bn >= 0 ? bn : -bn , bd);
      *b = bn / (sint4) lcm;
      bd = bd / lcm;
      for (i = 0; i < n; i++)
	if (sint4__mult(a+i, an[i], bd) < 0)
	  diag__fail(LASH_ERR_OVERFLOW, -1);

      if (denom)
	*denom = bd;
    }

  diag__return(0);
}

/**  int  normalize_linear_matrix(n, m, an, ad, bn, bd, a, b, denom) :
		     This function reduces independently to the same
		     denominator a set of m linear constraints by
		     applying to each of them the function
		     normalize_linear_vector.  The set of constraints
		     is represented by the matricial equation (an /
		     ad) . x = (bn / bd), where we have allowed the
		     division term by term of two matrices of same
		     dimensions. The matrices an, ad, bn and bd are
		     respectively of sizes m x n, m x n, m x 1 and m x
		     1; they are given as sequences of coefficients
		     organized line by line. If ad == NULL or bd ==
		     NULL, the corresponding denominators are taken
		     equal to 1.

		     At the end of the execution, the matrix
		     equation a . x = b is equivalent to the initial
		     constraints and the vector denom contains the
		     greatest common denominator used for each of the
		     m constraints.  a must have been previously
		     allocated as a matrix of size m x n, and b and
		     denom as vectors of size m.  denom can be NULL,
		     in which case the calling function does not care
		     about the denominator.

		     In the case of success, this function returns 0.
		     In the case of an error, it returns -1 and sets
		     lash_errno.

                     Possible error codes:

		         LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : One of the denominators
                                               equals zero.       **/ 

static int  normalize_linear_matrix(n, m, an, ad, bn, bd, a, b, denom)
     uint4   n, m;
     sint4  *an, *ad, *bn, *bd, *a, *b;
     uint4  *denom;
{
  register uint4  j;

  diag__enter("normalize_linear_matrix", -1);

  if (!an || !bn || !a || !b)
    diag__fail(LASH_ERR_BAD_VALUE, -1);

  if (!bd && !bn)
    {
      memcpy(a, an, n * m * sizeof(sint4));
      memcpy(b, bn, m * sizeof(sint4));

      if (denom)
	for (j=0 ; j<m ; j++)
	  denom[j] = 1;

      diag__return(0);
    }

  for (j = 0; j < m; j++)
    if (normalize_linear_vector(n, an + j * n, ad ? ad + j * n : 
				NULL, bn[j],
				bd ? bd[j] : 1, a + j * n, b + j, 
				denom ? denom + j : NULL) < 0)
      diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  int  transf_common_ratio(n, m, a, b, d)  :  This function is used
		     when composing two linear transformations. It
		     turns the matricial relation 
		     (a X + b) / d (where we have allowed the
		     division term by term of two vectors)
		     into the matricial relation (a' X + b') / d' in
		     which all the elements of d' are equal. The 
		     matrix a is of size m x n and is given as a
		     sequence of coefficients organized line by
		     line. The vectors b and d are of size n. The
		     changes are made in place : a, b and d may be
		     modified by this function.

		     In case of success, the function returns 0.  In
                     case of error, it returns -1, sets lash_errno,
                     and leaves in a, b and d a relation that
		     is equivalent to the original one but whose
		     elements are not necessarily equal to the
		     original ones.

                     Possible error codes:

		         LASH_ERR_OVERFLOW : Arithmetic overflow. **/ 

static int  transf_common_ratio(n, m, a, b, d)
     uint4   n, m;
     sint4  *a, *b;
     uint4  *d;
{
  register uint4  i, j, f;
           uint4  lcm;

  diag__enter("transf_common_ratio", -1);

  lcm = d[0];
  for (i=1 ; i<m ; i++)
    if (uint4__lcm(&lcm, lcm, d[i]) < 0)
      diag__fail(LASH_ERR_OVERFLOW, -1);

  for (i=0 ; i<m ; i++)
    {
      f = lcm/d[i];

      if (sint4__mult(b+i, b[i], f) < 0)
	  diag__fail(LASH_ERR_OVERFLOW, -1);

      for (j=0 ; j<n ; j++)
	if (sint4__mult(a+i*n+j, a[i*n+j], f) < 0)
	  {
	    /* Removes the changes made to the current row */
	    while (j--)
	      a[i*n+j] /= (sint4) f;

	    b[i] /= (sint4) f;

	    diag__fail(LASH_ERR_OVERFLOW, -1);
	  }

      d[i] = lcm;
    }

  diag__return(0);
}

/**  void  transf_minimize(n, m, a, b, d)  :  This function is
		     used when composing two linear transformations.
		     It minimizes all the coefficients of the
		     matricial relation (a X + b) / d (where we have
		     allowed the division term by term of two
		     vectors).  The matrix a is of size m x n and is
		     given as a sequence of coefficients organized
		     line by line.  The vectors b and d are of size
		     m. d can be equal to NULL, in which case its
		     elements are assumed to be one and no change is
		     carried on d.  The changes are made in place : a,
		     b and d may be modified by this function.
		     
		     This function cannot incur an error.         **/ 

static void transf_minimize(n, m, a, b, d)
     uint4   n, m;
     sint4  *a, *b;
     uint4  *d;
{
  register uint4  i, j;
           uint4  gcd;

  for (i=0 ; i<m ; i++)
    {
      gcd = sint4__multi_gcd(a+i*n, n);
      gcd = uint4__gcd (gcd , b[i]>=0 ? b[i] : -b[i] );

      if (d)
	d[i] /= gcd;

      b[i] /= (sint4) gcd;

      if (gcd > 1)
	for (j=0 ; j<n ; j++)
	  a[i*n+j] /= (sint4) gcd;
    }
}


/****  Public visible functions.                                 ****/

/**  rlinear_transf *rva_create_identity_transf(n)  :  Creates a 
                     linear transformation corresponding to the
                     operation x : = x. The dimension of the vector x
                     is n.

		     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid dimension.  **/

rlinear_transf *rva_create_identity_transf(n)
  uint4      n;
{
  register uint4           i, j;
  register rlinear_transf *tr;
  register sint4          *s;

  diag__enter("rva_create_identity_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  tr -> dim  = n;
  tr -> gdim = 0;
  tr -> gcoeff = tr -> goffset = NULL;

  s = resr__new_objects(sint4, n * n);
  if (!s)
    {
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> coeff = s;

  tr -> ratio = resr__new_objects(uint4, n);
  if (!(tr -> ratio))
    {
      resr__free_objects(tr -> coeff, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < n; i++)
    {
      for (j = 0; j < n; j++, s++)
	*s = (i == j) ? 1 : ZERO_INT4;
      tr -> ratio[i] = 1;
    }

  s = resr__new_objects(sint4, n);
  if (!s)
    {
      resr__free_objects(tr -> ratio, uint4, n);
      resr__free_objects(tr -> coeff, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> offset = s;
  memset(s, 0, n * sizeof(sint4));

  diag__return(tr);
}

/**  rlinear_transf *rva_create_transf(n, m , a, aden, b, bden, p, 
                     pden,q,qden)  :
                     Creates a linear transformation corresponding to
                     the operation (p / pden) . x <= (q / qden) --> x
                     := (a / aden) . x + (b / bden) (where we have
                     allowed the division term by term of two matrices
                     or vectors). The matrices p, pden, a and aden are
                     respectively of sizes m x n, m x n, n x n and n x
                     n; they are given as sequences of coefficients
                     organized line by line. The vectors q, qden, b
                     and bden are respectively of sizes m, m, n and
                     n. If pden, qden or aden equals NULL, the
                     corresponding denominators are taken equal to
                     one.  If m equals zero, the guard is assumed to
                     be empty.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

rlinear_transf *rva_create_transf(n, m, a, aden, b, bden, p, pden, q,
				  qden)
  uint4       n, m;
  sint4      *a, *aden, *b, *bden, *p, *pden, *q, *qden;
{
  register rlinear_transf *tr;
  register sint4          *sc, *so, *gc, *go;
  register uint4          *sden;

  diag__enter("rva_create_transf", NULL);

  if (!n || (n > 0xffff))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !b || (m && (!p || !q)))
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sden = resr__new_objects(uint4, n);
  if (!sden)
    {
      resr__free_objects(so, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(lash_errno, NULL);
    }

  if (normalize_linear_matrix(n, n, a, aden, b, bden, sc, so, sden)
      < 0)
    {
      resr__free_objects(sden, uint4, n);
      resr__free_objects(so, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(lash_errno, NULL);
    }

  if (m)
    {
      gc = resr__new_objects(sint4, m * n);
      if (!gc)
	{
	  resr__free_objects(sden, uint4, n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_object(tr, rlinear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      go = resr__new_objects(sint4, m);
      if (!go)
	{
	  resr__free_objects(gc, sint4, m * n);
	  resr__free_objects(sden, uint4, n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_object(tr, rlinear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      if (normalize_linear_matrix(n, m, p, pden, q, qden, gc, go, 
				  NULL) < 0)
	{
	  resr__free_objects(go, sint4, m);
	  resr__free_objects(gc, sint4, m * n);
	  resr__free_objects(sden, uint4, n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_object(tr, rlinear_transf);
	  diag__fail(lash_errno, NULL);
	}
    }
  else
    gc = go = NULL;

  tr -> dim        = n;
  tr -> gdim       = m;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> ratio      = sden;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  return tr;
}

/** rlinear_transf *rva_create_assign_transf(n, p, a, aden, b, bden) :
                     Creates a linear transformation corresponding to
                     the assignment x(p) := (a[0] / aden[0]) . x(0) +
                     (a[1] / aden[1]) x(1) + ...  + (a[n-1] /
                     aden[n-1]) x(n-1) + (b / bden). If aden equals
                     NULL, the denominators aden[0], ..., aden[n-1]
                     are taken equal to one.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

rlinear_transf *rva_create_assign_transf(n, p, a, aden, b, bden)
  uint4       n, p;
  sint4      *a, *aden, b, bden;
{
  register rlinear_transf *tr;
  register sint4          *sc, *so;
  register uint4           i, j, *sden;

  diag__enter("rva_create_assign_transf", NULL);

  if (!n || (n > 0xffff) || (p >= n) || !bden)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sden = resr__new_objects(uint4, n);
  if (!sden)
    {
      resr__free_objects(so, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> dim        = n;
  tr -> gdim       = ZERO_INT4;
  tr -> gcoeff     = tr -> goffset = NULL;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> ratio      = sden;

  for (i = 0; i < n; i++, so++, sden++)
    if (i == p)
      {
	if (normalize_linear_vector(n, a, aden, b, bden, sc, so, 
				    sden) < 0)
	  {
	    resr__free_objects(sden, uint4, n);
	    resr__free_objects(so, sint4, n);
	    resr__free_objects(sc, sint4, n * n);
	    resr__free_object(tr, rlinear_transf);
	    diag__fail(lash_errno, NULL);
	  }
	sc += n;
      }

    else
      {
	for (j = 0; j < n; j++, sc++)
	  *sc = (i == j) ? 1 : ZERO_INT4;
	*so = ZERO_INT4;
	*sden = 1;
      }

  diag__return(tr);
}

/**  rlinear_transf *rva_create_inequ_transf(n, p, pden, q, qden)  :
                     Creates a linear transformation corresponding to
		     the inequality  (p[0] / pden[0]) . x(0) + 
		     (p[1] / pden[1]) . x(1) + ... +
		     (p[n-1] / pden[n-1]) . x(n-1) <= (q / qden). If 
		     pden equals NULL, the denominators pden[0], ...,
		     pden[n-1] are taken equal to one.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

rlinear_transf *rva_create_inequ_transf(n, p, pden, q, qden)
  uint4       n;
  sint4      *p, *pden, q, qden;
{
  register rlinear_transf *tr;
  register sint4         *sc, *so, *gc, *go, *sden;
  register uint4          i, j;

  diag__enter("rva_create_inequ_transf", NULL);

  if (!n || (n > 0xffff) || !qden)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  gc = resr__new_objects(sint4, n);
  if (!gc)
    {
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  go = resr__new_object(sint4);
  if (!go)
    {
      resr__free_objects(gc, sint4, n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sden = resr__new_objects(uint4, n);
  if (!sden)
    {
      resr__free_objects(so, sint4, n);
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> dim        = n;
  tr -> gdim       = 1;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> ratio      = sden;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  for (i = 0; i < n; i++, so++, sden++)
    {
      for (j = 0; j < n; j++, sc++)
	*sc = (i == j) ? 1 : ZERO_INT4;
      *so = ZERO_INT4;
      *sden = 1;
    }

  if (normalize_linear_vector(n, p, pden, q, qden, gc, go, NULL) < 0)
    {
      resr__free_objects(sden, uint4, n);
      resr__free_objects(so, sint4, n);
      resr__free_object(go, sint4);
      resr__free_objects(gc, sint4, n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_BAD_VALUE, NULL);
    }

  diag__return(tr);
}

/**  rlinear_transf *rva_create_equ_transf(n, p, pden, q, qden)  :
                     Creates a linear transformation corresponding to
		     the equality (p[0] / pden[0]) x(0) +
		     (p[1] / pden[1]) x(1) + ... +
		     (p[n-1] / pden[n-1]) x(n) = (q / qden). If pden
		     equals NULL, the denominators pden[0], ...,
		     pden[n-1] are taken equal to one.

                     In the case of success, this function returns a
                     pointer to the newly created transformation. In 
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/

rlinear_transf *rva_create_equ_transf(n, p, pden, q, qden)
  uint4       n;
  sint4      *p, *pden, q, qden;
{
  register rlinear_transf *tr;
  register sint4         *sc, *so, *gc, *go;
  register uint4          i, j, *sden;

  diag__enter("rva_create_equ_transf", NULL);

  if (!n || (n > 0xffff) || !qden)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!p)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);
#endif  /* >= 1 */

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  gc = resr__new_objects(sint4, 2 * n);
  if (!gc)
    {
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  go = resr__new_objects(sint4, 2);
  if (!go)
    {
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(so, sint4, n);
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sden = resr__new_objects(uint4, n);
  if (!sden)
    {
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  tr -> dim        = n;
  tr -> gdim       = 2;
  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> ratio      = sden;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  if (normalize_linear_vector(n, p, pden, q, qden, gc, go, NULL) < 0)
    {
      resr__free_objects(sden, uint4, n);
      resr__free_objects(so, sint4, n);
      resr__free_objects(go, sint4, 2);
      resr__free_objects(gc, sint4, 2 * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (i = 0; i < n; i++, so++, gc++, sden++)
    {
      for (j = 0; j < n; j++, sc++)
	*sc = (i == j) ? 1 : ZERO_INT4;
      *so = ZERO_INT4;
      *(gc + n) = - *gc;
      *sden = 1;
    }

  go[1] = -go[0];

  diag__return(tr);
}

/**  rlinear_transf  *rva_transf_compose(t1, t2)  :  Composes the two
                     linear transformations t1 and t2 into a linear 
                     transformation f such that f(x) = t2(t1(x)) for
                     every x. The coefficients of f are made minimal.

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

rlinear_transf *rva_transf_compose(t1, t2)
  rlinear_transf *t1, *t2;
{
  register uint4          n, m, *sden, i, den;
  register rlinear_transf *tr;
  register sint4         *sc, *so, *gc, *go, *tmp;

  diag__enter("rva_transf_compose", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!t1 || !t2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif

  if (t1 -> dim != t2 -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  tr = resr__new_object(rlinear_transf);
  if (!tr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  n = tr -> dim  = t1 -> dim;  
  m = tr -> gdim = t1 -> gdim + t2 -> gdim;

  if (transf_common_ratio(n, n, t1 -> coeff, t1 -> offset, 
			  t1 -> ratio) < 0)
    {
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_OVERFLOW, NULL);
    }

  den = (t1 -> ratio)[0];

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    {
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  sden = resr__new_objects(uint4, n);
  if (!sden)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (m)
    {
      gc = resr__new_objects(sint4, m * n);
      if (!gc)
	{
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sden, uint4, n);
	  resr__free_object(tr, rlinear_transf);
	  transf_minimize(n, n, t1 -> coeff, t1 -> offset, 
			  t1 -> ratio);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}

      go = resr__new_objects(sint4, m);
      if (!go)
	{
	  resr__free_objects(sc, sint4, n * n);
	  resr__free_objects(so, sint4, n);
	  resr__free_objects(sden, uint4, n);
	  resr__free_objects(gc, sint4, m * n);
	  resr__free_object(tr, rlinear_transf);
	  transf_minimize(n, n, t1 -> coeff, t1 -> offset, 
			  t1 -> ratio);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
    }
  else
    gc = go = NULL;

  tr -> coeff      = sc;
  tr -> offset     = so;
  tr -> ratio      = sden;
  tr -> gcoeff     = gc;
  tr -> goffset    = go;

  if (t1 -> gdim)
    {
      memcpy(gc, t1 -> gcoeff,  (t1 -> gdim) * n * sizeof(sint4));
      memcpy(go, t1 -> goffset, (t1 -> gdim) * sizeof(sint4));
    }

  tmp = resr__new_objects(sint4, t2 -> gdim);

  if (!tmp)
    {
      if (m)
	{
	  resr__free_objects(go, sint4, m);
	  resr__free_objects(gc, sint4, m * n);
	}
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      resr__free_objects(sden, uint4, n);
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_OVERFLOW, NULL);
    }

  for (i=0 ; i < t2 -> dim ; i++)
    tmp[i] = den * (t2 -> offset)[i];

  if (matrix__mult(sc, t2 -> coeff, t1 -> coeff,  n, n, n) < 0 ||
      matrix__mult(so, t2 -> coeff, t1 -> offset, n, n, 1) < 0 ||
      matrix__add(so, so, tmp, n, 1) < 0)
    {
      resr__free_objects(tmp, sint4, t2 -> gdim);
      if (m)
	{
	  resr__free_objects(go, sint4, m);
	  resr__free_objects(gc, sint4, m * n);
	}
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      resr__free_objects(sden, uint4, n);
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_OVERFLOW, NULL);
    }

  for (i=0 ; i < t2 -> gdim ; i++)
    tmp[i] = den * (t2 -> goffset)[i];

  if (t2 -> gdim && 
      (matrix__mult(gc + (t1 -> gdim) * n, t2 -> gcoeff, 
		    t1 -> coeff, (t2 -> gdim), n, n) < 0 ||
       matrix__mult(go + (t1 -> gdim), t2 -> gcoeff, 
		    t1 -> offset, (t2 -> gdim), n, 1) < 0 ||
       matrix__const_mult(go + (t1 -> gdim), (t2 -> gdim), 1,
			  -1) < 0 ||
       matrix__add(go + (t1 -> gdim), go + (t1 -> gdim), 
		   tmp, (t2 -> gdim), 1) < 0))
    {
      resr__free_objects(tmp, sint4, t2 -> gdim);
      if (m)
	{
	  resr__free_objects(go, sint4, m);
	  resr__free_objects(gc, sint4, m * n);
	}
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      resr__free_objects(sden, uint4, n);
      resr__free_object(tr, rlinear_transf);
      transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
      diag__fail(LASH_ERR_OVERFLOW, NULL);
    }

  resr__free_objects(tmp, sint4, t2 -> gdim);

  for (i=0 ; i<n ; i++)
    sden[i] = den * (t2 -> ratio)[i];

  transf_minimize(n, n, t1 -> coeff, t1 -> offset, t1 -> ratio);
  transf_minimize(n, n, tr -> coeff, tr -> offset, tr -> ratio);
  if (m)
    transf_minimize(n, m, tr -> gcoeff,tr -> goffset, NULL);

  diag__return(tr);
}

/**  rlinear_transf *rva_transf_power(tr, k)  :  Computes the k-th
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

rlinear_transf *rva_transf_power(tr, k)
  rlinear_transf *tr;
  uint4          k;
{
  register rlinear_transf *t, *tm, *tt;

  diag__enter("rva_transf_power", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif

  tm = rva_transf_copy(tr);
  if (!tm)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  t = rva_create_identity_transf(tr -> dim);
  if (!t)
    {
      rva_transf_free(tm);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  while (k)
    {
      if (k & 1)
        {
	  tt = rva_transf_compose(t, tm);
	  rva_transf_free(t);
	  if (!tt)
	    {
	      rva_transf_free(tm);
	      diag__fail(lash_errno, NULL);
	    }
	  t = tt;
        }
      
      k /= (uint4) 2;
      if (!k)
	break;

      tt = rva_transf_compose(tm, tm);
      rva_transf_free(tm);
      if (!tt)
	{
	  rva_transf_free(t);
	  diag__fail(lash_errno, NULL);
	}
      tm = tt;
    }

  rva_transf_free(tm);
  diag__return(t);
}

/**  int  rva_transf_free(t)  :  Frees the linear transformation *t.

                     If successful, returns 0. In the case of an 
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt transformation.
                                                                   **/
int rva_transf_free(t)
  rlinear_transf *t;
{
  register uint4  n, m;

  diag__enter("rva_transf_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!t)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  n = t -> dim;
  m = t -> gdim;

  resr__free_objects(t -> coeff, sint4, n * n);
  resr__free_objects(t -> offset, sint4, n);
  resr__free_objects(t -> ratio, uint4, n);

  if (m)
    {
      resr__free_objects(t -> gcoeff, sint4, n * m);
      resr__free_objects(t -> goffset, sint4, m);
    }

  resr__free_object(t, rlinear_transf);

  diag__return(0);  
}

/**  rlinear_transf *rva_transf_copy(tr)  :  Creates a new linear 
                     transformation identical to *tr. In the case of
                     success, this function returns a pointer to a
		     newly allocated transformation. Otherwise, it
		     returns a NULL pointer.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt transformation.
                                                                   **/

rlinear_transf *rva_transf_copy(tr)
  rlinear_transf *tr;
{
  register rlinear_transf *t;
  register uint4          n, m;
  register sint4         *s;

  diag__enter("rva_transf_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!tr)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */  

  t = resr__new_object(rlinear_transf);
  if (!t)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  t -> dim  = n = tr -> dim;
  t -> gdim = m = tr -> gdim;

  s = resr__new_objects(sint4, n * n);
  if (!s)
    {
      resr__free_object(t, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(s, tr -> coeff, n * n * sizeof(sint4));
  t -> coeff = s;

  s = resr__new_objects(sint4, n);
  if (!s)
    {
      resr__free_objects(t -> coeff, sint4, n * n);
      resr__free_object(t, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(s, tr -> offset, n * sizeof(sint4));
  t -> offset = s;

  t -> ratio = resr__new_objects(uint4, n);
  if (!(t -> ratio))
    {
      resr__free_objects(t -> offset, sint4, n);
      resr__free_objects(t -> coeff, sint4, n * n);
      resr__free_object(t, rlinear_transf);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(t -> ratio, tr -> ratio, n * sizeof(uint4));

  if (m)
    {
      s = resr__new_objects(sint4, n * m);
      if (!s)
	{
	  resr__free_objects(t -> ratio, uint4, n);
	  resr__free_objects(t -> offset, sint4, n);
	  resr__free_objects(t -> coeff, sint4, n * n);
	  resr__free_object(t, rlinear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      
      memcpy(s, tr -> gcoeff, n * m * sizeof(sint4));
      t -> gcoeff = s;

      s = resr__new_objects(sint4, m);
      if (!s)
	{
	  resr__free_objects(t -> gcoeff, sint4, n * m);
	  resr__free_objects(t -> ratio, uint4, n);
	  resr__free_objects(t -> offset, sint4, n);
	  resr__free_objects(t -> coeff, sint4, n * n);
	  resr__free_object(t, rlinear_transf);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      
      memcpy(s, tr -> goffset, m * sizeof(sint4));
      t -> goffset = s;
    }
  else
    t -> gcoeff = t -> goffset = NULL;

  diag__return(t);
}

/**  rva *rva_image_by_transf(rv, t)  :  Computes the image of the set
                     represented by the RVA *rv by the linear 
                     transformation *t. This function is currently
                     only implemented for RVAs restricted to the
		     additive theory of reals and integers, and that
		     operate serially.

                     This function does not modify *rv, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
			 LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_CORRUPT    : Corrupt RVA or 
                                               transformation.     **/

rva *rva_image_by_transf(rv, t)
  rva           *rv;
  rlinear_transf *t;
{
  register rlinear_tr_info *lti;
  register rva             *rvr;

  diag__enter("rva_image_by_transf", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || !t)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (rv -> dim != t -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL) ||
      !(rv -> properties & RVA_PROP_RESTRICTED))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  lti = rva_create_transf_info(t, rv -> base);
  if (!lti)
    diag__fail(lash_errno, NULL);

  rvr = rva_image_by_transf_info(rv, lti);
    rva_transf_info_free(lti);
  if (!rvr)
    diag__fail(lash_errno, NULL);
  
  diag__return(rvr);
}

/**  rlinear_tr_info *rva_create_transf_info(tr, r)  :  Creates
                     a transducer associated to the linear
                     transformation *tr, whose purpose is to speed up
                     the computation of the image of sets by *tr. The
                     transducer is a RVA accepting all the vectors of
                     the form [x0 x0' x1 x1' ... x(n-1) x(n-1)'], 
		     where [x0' x1' ... x(n-1)'] = tr([x0 x1 ... 
		     x(n-1)]). The numeration base used for creating
		     that RVA is r. 

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

rlinear_tr_info *rva_create_transf_info(tr, r)
  rlinear_transf *tr;
  uint1           r;
{
  register uint4  n, m, i, j;
  register sint4 *s;
  register rva   *rv1, *rv2, *rv3, *rv4;

  diag__enter("rva_create_transf_info", NULL);

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

  s[1] = - (tr -> ratio)[0];

  rv1 = rva_create_equation(r, 2 * n, s, -(tr -> offset[0]));
  if (!rv1)
    {
      resr__free_objects(s, sint4, 2 * n);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  for (j = 1; j < n; j++)
    {
      for (i = 0; i < n; i++)
        s[2 * i] = tr -> coeff[j * n + i];
      s[2 * j - 1] =  ZERO_INT4;
      s[2 * j + 1] = - (tr -> ratio)[j];
      rv2 = rva_create_equation(r, 2 * n, s, -(tr -> offset[j]));
      if (!rv2)
	{
	  rva_free(rv1);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      rv3 = rva_intersection(rv1, rv2);
      rva_free(rv1);
      rva_free(rv2);
      if (!rv3)
	{
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      rv1 = rv3;
    }

  if (!m)
    {
      resr__free_objects(s, sint4, 2 * n);
      diag__return((rlinear_tr_info *) rv1);
    }

  for (i = 0; i < n; i++)
    {
      s[2 * i] = tr -> gcoeff[i];
      s[2 * i + 1] = ZERO_INT4;
    }

  rv4 = rva_create_inequation(r, 2 * n, s, tr -> goffset[0]);
  if (!rv4)
    {
      rva_free(rv1);
      resr__free_objects(s, sint4, 2 * n);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  for (j = 1; j < m; j++)
    {
      for (i = 0; i < n; i++)
	s[2 * i] = tr -> gcoeff[j * n + i];
      
      rv2 = rva_create_inequation(r, 2 * n, s, tr -> goffset[j]);
      if (!rv2)
	{
	  rva_free(rv1);
	  rva_free(rv4);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      rv3 = rva_intersection(rv4, rv2);
      rva_free(rv4);
      rva_free(rv2);
      if (!rv3)
	{
	  rva_free(rv1);
	  resr__free_objects(s, sint4, 2 * n);
	  diag__fail(LASH_ERR_NO_MEM, NULL);
	}
      rv4 = rv3;
    }

  resr__free_objects(s, sint4, 2 * n);
  rv2 = rva_intersection(rv1, rv4);
  rva_free(rv1);
  rva_free(rv4);
  if (!rv2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return((rlinear_tr_info *) rv2);
}

/**  rva *rva_image_by_transf_info(rv, ti)  :  Computes the image of
                     the set represented by the rva *rv by the linear
                     transformation associated with the information
                     *ti. This function is currently only implemented
                     for RVAs restricted to the additive theory of
		     reals and integers, and that operate serially.

                     This function does not modify *rv or *ti, and
                     returns (in the case of success) a pointer to a
                     newly allocated RVA. In the case of an error, it
                     returns a NULL pointer and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BASE       : Base mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_NOT_IMPL   : Not (yet) implemented.
                         LASH_ERR_CORRUPT    : Corrupt RVA or 
                                               transformation info. 
                                                                   **/

rva *rva_image_by_transf_info(rv, ti)
  rva             *rv;
  rlinear_tr_info *ti;
{
  register uint4  n;
  register rva   *rv1, *rv2;

  diag__enter("rva_image_by_transf_info", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!rv || !ti)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(rv -> dim) || (2 * (rv -> dim)) != ((rva *) ti) -> dim)
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if (rv -> base != ((rva *) ti) -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if (!(rv -> properties & RVA_PROP_SERIAL) ||
      !(rv -> properties & RVA_PROP_RESTRICTED) ||
      !(((rva *) ti) -> properties & RVA_PROP_SERIAL))
    diag__fail(LASH_ERR_NOT_IMPL, NULL);

  if (auto_accept_type(rv -> automaton) != AUTO_ACCEPT_WEAK ||
      !auto_test_property(rv -> automaton, AUTO_PROP_DETERM))
    diag__fail(LASH_ERR_CORRUPT, NULL);

  n   = rv -> dim;
  rv2 = rva_interleave_rn(rv, 1, 0);
  if (!rv2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rv1 = rva_intersection(rv2, ti);
  rva_free(rv2);
  if (!rv1)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  rv2 = rva_multi_projection(rv1, 2, 0);
  rva_free(rv1);
  if (!rv2)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  diag__return(rv2);
}

/**  int  rva_transf_info_free(ti)  :  Frees the linear
                     transformation information *ti.

                     If successful, returns 0. In the case of an
                     error, returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt transformation
                                             information           **/

int  rva_transf_info_free(ti)
  rlinear_tr_info *ti;
{
  diag__enter("rva_transf_info_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!ti)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  rva_free((rva *) ti);

  diag__return(0);
}

/** rlinear_tr_info *rva_transf_info_copy(ti)  :  Returns a copy of
                     the linear transformation information *ti or a
                     NULL pointer in the case of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_NO_MEM   : No enough memory.
                         LASH_ERR_CORRUPT  : Corrupt RVA.          **/

rlinear_tr_info  *rva_transf_info_copy(ti)
  rlinear_tr_info *ti;
{
  register rlinear_tr_info *p;

  diag__enter("rva_transf_info_copy", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!ti)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  p = rva_copy((rva *) ti);
  if (!p)
    diag__fail(lash_errno, NULL);

  diag__return(p);
}

/****  End of rva-transformations.c  ****/
