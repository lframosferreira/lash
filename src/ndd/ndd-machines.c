/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    ndd-machines.c  :  State machines operating over unbounded  **/
/**                 integer variables.                             **/
/**                                                                **/
/**    02/22/99  :  Creation. (BB)                                 **/
/**    02/25/99  :  Continued. (BB)                                **/
/**    03/10/99  :  Reorganized. (BB)                              **/
/**    03/15/99  :  Minor correction. (BB)                         **/
/**    06/03/99  :  Support for atomic control locations. (BB)     **/
/**    02/04/00  :  Modified structure for the transition          **/
/**                 relation. (BB)                                 **/
/**    02/10/00  :  Efficient handling of meta-transitions. (BB)   **/
/**    01/19/01  :  Modified arg. type of ndd_states_get_data (LL) **/
/**    02/12/01  :  New function (LL+BB).                          **/
/**    07/09/02  :  Reorganization. (BB)                           **/
/**    09/13/02  :  Correction. (LL)                               **/
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
#include "lash-auto-operations.h"
#include "ndd.h"
#include "diag.h"
#include "resource.h"

/****  Prototypes of private functions.                          ****/

static ndd  *identity_transducer(uint1, uint4, int);
static int   insert_control(ndd **, ndd *);
static int   insert_translation_control(translation_info *, uint4);

/****  Private functions.                                        ****/

/**  ndd *identity_transducer(r, n, msdf)  :  Creates an NDD accepting
                     all the vectors of the form (x1, x1, x2, x2, ...,
                     xn, xn), where (x1, x2, ..., xn) is an
                     n-dimensional vector. The NDD to be constructed
                     operates in base r. It reads numbers least
                     significant digit first if the value of msdf is
                     zero, and most significant digit first otherwise.
                     
                     In the case of success, this function returns a
                     pointer to a newly allocated NDD. In the case of
                     insufficient memory, it returns a NULL pointer.
                                                                   **/
static ndd *identity_transducer(r, n, msdf)
  uint1  r;
  uint4  n;
  int    msdf;
{
  register sint4 *s;
  register uint4  i;
  register ndd   *nd1, *nd2, *nd3;
  register ndd   *(*f)(uint1, uint4, sint4 *, sint4);        

#if LASH_CHECK_LEVEL >= 1
  if (r <= 1 || !n)
    return NULL;
#endif

  s = resr__new_objects(sint4, 2 * n);
  if (!s)
    return NULL;

  memset(s + 2, 0, (2 * n - 2) * sizeof(sint4));
  s[0] = 1;
  s[1] = -1;

  f = msdf ? ndd_create_equation_msdf : 
       ndd_create_equation_lsdf;

  nd1 = f(r, 2 * n, s, ZERO_INT4);
  if (!nd1)
    {
      resr__free_objects(s, sint4, 2 * n);
      return NULL;
    }

  for (i = 1; i < n; i++)
    {
      s[2 * i - 2] = s[2 * i - 1] = ZERO_INT4;
      s[2 * i] = 1;
      s[2 * i + 1] = -1;
    
      nd2 = f(r, 2 * n, s, ZERO_INT4);
      if (!nd2)
        {
          ndd_free(nd1);
          resr__free_objects(s, sint4, 2 * n);
          return NULL;
        }

      nd3 = ndd_intersection(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);
      if (!nd3)
        {
          resr__free_objects(s, sint4, 2 * n);
          return NULL;
        }
      nd1 = nd3;
    }
   
  resr__free_objects(s, sint4, 2 * n);  
  
  return nd1;
}

/**  int  insert_control(nd, ndc)  :  Replaces the NDD **nd by the
                     product of the NDD *ndc (which contains control
                     information) by itself. In the case of success,
                     this function returns 0. In the case of 
                     insufficient memory, it returns -1.           **/

static int  insert_control(nd, ndc)
  ndd **nd, *ndc;
{
  register ndd *nd1;

  nd1 = ndd_product(ndc, *nd);
  if (!nd1)
    return -1;

  ndd_free(*nd);
  *nd = nd1;

  return 0;
}

/**  int  insert_translation_control(ti, m)  :  Inserts into each
                     NDD (but the first) in the translation 
                     information structure *ti m additional control
                     variables. In the case of success, this function
                     returns 0. In the case of insufficient memory, it
                     returns -1.                                   **/

static int  insert_translation_control(ti, m)
  translation_info *ti;
  uint4             m;
{
  register uint4  i;
  register ndd   *ndt;

  if (!m)
    return 0;

  ndt = identity_transducer(ti -> ndd[0] -> base, m,
      (ti -> ndd[0] -> properties & NDD_PROP_MSDF) ? 1 : 0);
  if (!ndt)
    return -1;
 
  for (i = 1; i < ti -> dim; i++)
    if (insert_control(ti -> ndd + i, ndt) < 0)
      {
	ndd_free(ndt);
	return -1;
      }

  ndd_free(ndt);

  return 0;
}

/****  Public visible functions.                                 ****/

/**  ndd_states *ndd_states_new_empty(r, lc, nv, msdf)  :  Creates a
                     new empty set of states for a machine whose
		     control states are described by lc integers and
		     whose data is composed of nv unbounded integer
		     variables. The numeration base used for
		     representing sets of states (as NDDs) is r. The
		     underlying NDD operates most significant digit
		     first if msdf is different from zero, and least
		     significant digit first otherwise. The NDD
		     operates serially.

                     In the case of success, this function returns a
                     pointer to the newly created structure. In the
                     case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/
 
ndd_states *ndd_states_new_empty(r, lc, nv, msdf)
  uint1  r;
  uint4  lc, nv;
  int    msdf;
{
  register ndd_states *ns;
  register ndd        *nd;

  diag__enter("ndd_states_new_empty", NULL);

  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  nd = ndd_create_empty(r, lc + nv, msdf);
  if (!nd)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ns = resr__new_object(ndd_states);
  if (!ns)
    {
      ndd_free(nd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }
  
  ns -> ndd      = nd; 
  ns -> ctrl_len = lc;

  diag__return(ns);
}

/**  int  ndd_states_add_data(ns, pc, nd)  :  Adds to the set of 
                     states represented by the structure *ns all the
                     states whose control part is described by the
                     array of integers *pc and whose data is
                     represented by an element of the set represented
                     by the NDD *nd.

                     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
			 LASH_ERR_BAD_TYPE   : Type mismatch.
			 LASH_ERR_BASE       : Base mismatch.      **/

int  ndd_states_add_data(ns, pc, nd)
  ndd_states  *ns;
  sint4       *pc;
  ndd         *nd;
{
  register uint4  i;
  register ndd   *nd1, *nd2, *nd3;
  register ndd   *(*f)(uint1, uint4, sint4 *, sint4);
  register int    rc;
  static   sint4  one[1] = { 1 };

  diag__enter("ndd_states_add_data", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!ns || !(ns -> ndd) || (ns -> ctrl_len && !pc) || !nd)
     diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  if (ns -> ndd -> dim != (nd -> dim  + ns -> ctrl_len))
    diag__fail(LASH_ERR_DIMENSION, -1);

  if (ns -> ndd -> base != nd -> base)
    diag__fail(LASH_ERR_BASE, -1);

  if (((ns -> ndd -> properties & NDD_PROP_MSDF) !=
      (nd -> properties & NDD_PROP_MSDF)) ||
      ((ns -> ndd -> properties & NDD_PROP_SERIAL) !=
      (nd -> properties & NDD_PROP_SERIAL)))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  f = (nd -> properties & NDD_PROP_MSDF) ?
      ndd_create_equation_msdf : ndd_create_equation_lsdf;

  nd1 = ndd_copy(nd);
  if (!nd1)
    diag__fail(LASH_ERR_NO_MEM, -1);

  for (i = 0, pc += ns -> ctrl_len - 1; i < ns -> ctrl_len; i++, pc--)
    {
      nd2 = f(nd -> base, 1, one, *pc);
      if (!nd2)
	{
	  ndd_free(nd1);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      nd3 = ndd_product(nd2, nd1);
      ndd_free(nd2);
      ndd_free(nd1);
      if (!nd3)
	diag__fail(LASH_ERR_NO_MEM, -1);
      nd1 = nd3;
    }

  rc = ndd_merge(ns -> ndd, nd1);
  ndd_free(nd1);
  if (rc < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);
  
  diag__return(0);
}

/**  ndd *ndd_states_get_data(ns, pc, pm)  :  Computes a 
                     representation of the set of data values
                     associated to at least one control state
                     described by the array of integers *pc associated
                     with the array of masks *pm (the value of a
                     component pc[i] is only considered if pm[i] != 0,
                     otherwise it is ignored).

                     In the case of success, this function returns a
                     pointer to a newly allocated NDD. In the case of
                     an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

ndd *ndd_states_get_data(ns, pc, pm)
  const ndd_states  *ns;
  sint4       *pc;
  int         *pm;
{
  register ndd   *nd, *nd1, *nd2, *nd3;
  register ndd   *(*f)(uint1, uint4, sint4 *, sint4);
  register ndd   *(*fz)(uint1, uint4);
  register uint4  i;
  static   sint4  one[1] = { 1 };

  diag__enter("ndd_states_get_data", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!ns || !(ns -> ndd) || (ns -> ctrl_len && (!pc || !pm)))
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  nd = ndd_copy(ns -> ndd);
  if (!nd)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  f  = (ns -> ndd -> properties & NDD_PROP_MSDF) ?
      ndd_create_equation_msdf : ndd_create_equation_lsdf;
  fz = (ns -> ndd -> properties & NDD_PROP_MSDF) ?
      ndd_create_zn_msdf : ndd_create_zn_lsdf;

  for (i = 0; i < ns -> ctrl_len; i++, pc++)
    {
      if (pm[i])
	{
	  nd1 = f(nd -> base, 1, one, *pc);
	  if (!nd1)
	    {
	      ndd_free(nd);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	  nd2 = fz(nd -> base, nd -> dim - 1);
          if (!nd2)
	    {
	      ndd_free(nd1);
	      ndd_free(nd);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	  nd3 = ndd_product(nd1, nd2);
	  ndd_free(nd1);
	  ndd_free(nd2);
	  if (!nd3)
	    {
	      ndd_free(nd);
	      diag__fail(LASH_ERR_NO_MEM, NULL);
	    }
	  nd1 = ndd_intersection(nd, nd3);
	  ndd_free(nd);
	  ndd_free(nd3);
	  if (!nd1)
	    diag__fail(LASH_ERR_NO_MEM, NULL);
	  nd = nd1;
	}
      nd1 = ndd_projection(nd, 0);
      ndd_free(nd);
      if (!nd1)
	diag__fail(LASH_ERR_NO_MEM, NULL);
      nd = nd1;
    }

  diag__return(nd);
}

/**  int  ndd_states_free(ns)  :  Frees the set of states *ns. In the 
                     case of success, this function returns 0.  In the
		     case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

int  ndd_states_free(ns)
  ndd_states  *ns;
{
  diag__enter("ndd_states_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!ns || !(ns -> ndd))
     diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  ndd_free(ns -> ndd);
  resr__free_object(ns, ndd_states);

  diag__return(0);
}

/**  ndd_relation *ndd_relation_new_empty(r, lc, nv, msdf)  :  Creates
                     a new empty representation of the transition
		     relation of a machine whose control states are
		     described by lc integers and whose data is
		     composed of nv unbounded integer variables. The
		     numeration base used for representing sets of
		     states (as NDDs) is r. The underlying NDDs
		     operate most significant digit first if msdf is
		     different from zero, and least significant digit
		     first otherwise. The NDDs operate serially.

                     In the case of success, this function returns a
                     pointer to the newly created structure. In the
                     case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_BAD_VALUE  : Invalid parameter.  **/
 
ndd_relation *ndd_relation_new_empty(r, lc, nv, msdf)
  uint1  r;
  uint4  lc, nv;
  int    msdf;
{
  register ndd_relation *nr;

  diag__enter("ndd_relation_new_empty", NULL);

  if (r <= 1)
    diag__fail(LASH_ERR_BAD_VALUE, NULL);

  nr = (ndd_relation *) resr__new_object(ndd_relation);
  if (!nr)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  nr -> list = NULL;
  nr -> ctrl_len = lc;
  nr -> nb_vars  = nv;
  nr -> base     = r;
  nr -> msdf     = msdf ? 1 : 0;

  diag__return(nr);
}

/**  int  ndd_relation_add_transition(nr, np, no, nd, tr)  :  Adds to
                     the transition relation represented by *nr a
                     transition linking the control states such that
                     the (np+1)-th component of their control
                     descriptor is equal to no to those for which this
                     component is equal to nd (the value of the other
                     components of the control descriptor does not
                     change). The transition is labeled by the linear
                     transformation *tr.

                     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
			 LASH_ERR_BAD_VALUE  : Bad value of np.
			 LASH_ERR_OVERFLOW   : Overflow.
                         LASH_ERR_DIMENSION  : Dimension mismatch. **/

int  ndd_relation_add_transition(nr, np, no, nd, tr)
  ndd_relation  *nr;
  uint4          np;
  sint4          no, nd;
  linear_transf *tr;
{
  register uint4            n, i, j;
  register sint4           *sc, *so, *gc, *go;
  register linear_transf   *tr1;
  register ndd             *nd1, *nd2, *nd3;
  register ndd_relation_el *el;

  diag__enter("ndd_relation_add_transition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nr || !tr || (nr -> base <= 1))
    diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  if ((tr -> dim) != (nr -> nb_vars))
    diag__fail(LASH_ERR_DIMENSION, -1);   

  n = nr -> ctrl_len;

  if (np >= n)
    diag__fail(LASH_ERR_BAD_VALUE, -1);   

  if (n > 0xffff)
    diag__fail(LASH_ERR_OVERFLOW, -1);   

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    diag__fail(LASH_ERR_NO_MEM, -1);

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  gc = resr__new_objects(sint4, (1 + n) * n);
  if (!gc)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  go = resr__new_objects(sint4, 1 + n);
  if (!go)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
     resr__free_objects(gc, sint4, (1 + n) * n); 
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  memset(gc, 0, n * (n + 1) * sizeof(sint4));

  for (i = 0; i < n; i++)
    {
      so[i] = (i == np) ? nd : ZERO_INT4;
      gc[i + n] = -(gc[i] = (i == np) ? 1 : ZERO_INT4);
      for (j = 0; j < n; j++)
        sc[i * n + j] = ((i == j) && (i != np)) ? 1 : ZERO_INT4;

      if (i < np && i < (n - 1))
	gc[2 * n + i * (n + 1)] = -1;
      else
	if (i > np)
	  gc[n + i * n + i] = -1;
    }

  go[1] = -(go[0] = no);
  if (n > 1) 
      memset(go + 2, 0, (n - 1) * sizeof(sint4));

  tr1 = ndd_create_transf(n, n + 1, sc, so, gc, go);

  resr__free_objects(gc, sint4, (1 + n) * n);
  resr__free_objects(sc, sint4, n * n);
  resr__free_objects(so, sint4, n);
  resr__free_objects(go, sint4, 1 + n);
  
  if (!tr1)
    diag__fail(lash_errno, -1);

  nd1 = (ndd *) ndd_create_transf_info(tr1, nr -> base, nr -> msdf);
  ndd_transf_free(tr1);
  if (!nd1)
    diag__fail(lash_errno, -1);

  nd2 = (ndd *) ndd_create_transf_info(tr, nr -> base, nr -> msdf);
  if (!nd2)
    {
      ndd_free(nd1);
      diag__fail(lash_errno, -1);
    }
  
  nd3 = ndd_product(nd1, nd2);
  ndd_free(nd1);
  ndd_free(nd2);
  if (!nd3)
    diag__fail(lash_errno, -1);

  el = (ndd_relation_el *) resr__new_object(ndd_relation_el);
  if (!el)
    {
      ndd_free(nd3);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  el -> type     = NDD_RELATION_NDD;
  el -> inf.ndd  = nd3;
  el -> next     = nr -> list;
  nr -> list     = el; 

  diag__return(0);
}

/**  int  ndd_relation_add_metatransition(nr, pc, pm, tr)  :  Adds to
                     the transition relation represented by *nr a
		     metatransition characterized by the array of
		     control descriptors *pc, the array of masks *pm
		     and the linear transformation *tr (an entry pc[i]
		     of *pc is only considered if pm[i] != 0, and
		     ignored otherwise).

                     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_NOT_IMPL   : Not yet implemented.
                         LASH_ERR_ITERATION  : No iterable.        **/

int  ndd_relation_add_metatransition(nr, pc, pm, tr)
  ndd_relation  *nr;
  sint4         *pc;
  int           *pm;
  linear_transf *tr;
{
  register uint4                 n, m, i, j;
  register sint4                *sc, *so, *gc, *go;
  register linear_transf        *tr1;
  register linear_star_info     *lsi;
  register ndd                  *nd1;
  register ndd_relation_el      *el;
  register presburger_star_info *psi;
  register int                   rc;

  diag__enter("ndd_relation_add_metatransition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nr || (nr -> ctrl_len && (!pc || !pm)) || !tr)
     diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  if ((tr -> dim) != (nr -> nb_vars))
    diag__fail(LASH_ERR_DIMENSION, -1);   

  n = nr -> ctrl_len;

  for (i = m = 0; i < n; i++)
    if (pm[i])
      m++;

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    diag__fail(LASH_ERR_NO_MEM, -1);

  memset(sc, 0, n * n * sizeof(sint4));

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  memset(so, 0, n * sizeof(sint4));

  gc = resr__new_objects(sint4, (n + m) * n);
  if (!gc)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  memset(gc, 0, (n + m) * n * sizeof(sint4));

  go = resr__new_objects(sint4, n + m);
  if (!go)
    {
      resr__free_objects(gc, sint4, (n + m) * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  memset(go, 0, (n + m) * sizeof(sint4));

  for (i = 0; i < n; i++)
    sc[(n + 1) * i] = 1;

  for (i = j = ZERO_INT4; i < n; i++, j++)
    if (pm[i])
      {
	gc[j * n + i]     = 1;
        go[j]             = pc[i];
        gc[(++j) * n + i] = -1;
        go[j]             = -pc[i];
      }
    else
      gc[j * n + i] = -1;

  tr1 = ndd_create_transf(n, n + m, sc, so, gc, go);

  resr__free_objects(go, sint4, n + m);
  resr__free_objects(gc, sint4, (n + m) * n);
  resr__free_objects(sc, sint4, n * n);
  resr__free_objects(so, sint4, n);

  if (!tr1)
    diag__fail(lash_errno, -1);

  nd1 = (ndd *) ndd_create_transf_info(tr1, nr -> base, nr -> msdf);
  ndd_transf_free(tr1);
  if (!nd1)
    diag__fail(lash_errno, -1);

  lsi = ndd_create_star_info(tr, nr -> base, nr -> msdf);
  if (!lsi)
    {
      ndd_free(nd1);
      diag__fail(lash_errno, -1);
    }

  if (lsi -> type != NDD_ITERATE_PRESBURGER)
    {
      ndd_star_info_free(lsi);
      ndd_free(nd1);
      diag__fail(LASH_ERR_NOT_IMPL, -1);
    }

  psi = lsi -> val.psi;

  rc = (insert_control(&psi -> tr, nd1) < 0  ||
        insert_control(&psi -> trp, nd1) < 0 ||
        insert_control(&psi -> tr2p, nd1) < 0 ||
        insert_translation_control(psi -> ti, n) < 0);

  ndd_free(nd1);
  if (rc)
    {
      ndd_star_info_free(lsi);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  el = (ndd_relation_el *) resr__new_object(ndd_relation_el);
  if (!el)
    {
      ndd_star_info_free(lsi);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  el -> type = NDD_RELATION_STAR;
  el -> inf.lsi = lsi;
  el -> next = nr -> list;
  nr -> list = el; 

  diag__return(0);
}

/**  int  ndd_relation_free(ns)  :  Frees the transition relation
                     structure *ns. In the case of success, this
                     function returns 0.  In the case of an error, it
                     returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

int  ndd_relation_free(ns)
  ndd_relation *ns;
{
  register ndd_relation_el *p, *p2;

  diag__enter("ndd_relation_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!ns)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  for (p = ns -> list; p; p = p2)
    {
      p2 = p -> next;
      switch(p -> type)
	{
	case NDD_RELATION_NDD:
	  ndd_free(p -> inf.ndd);
	  break;
        case NDD_RELATION_STAR:
	  ndd_star_info_free(p -> inf.lsi);
	  break;
        default:
	  diag__fail(LASH_ERR_CORRUPT, -1);
	}

      resr__free_object(p, ndd_relation_el);
    }

  resr__free_object(ns, ndd_relation);

  diag__return(0);
}

/**  sint8  ndd_relation_size(ns)  :  Returns the size of the
                     transition relation structure *ns, expressed
                     in individual NDD states. In the case of an
		     error, this function returns -1.

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_CORRUPT    : Corrupt structure.  **/

sint8  ndd_relation_size(ns)
  ndd_relation *ns;
{
  register ndd_relation_el *p;
  register sint8            r;

  diag__enter("ndd_relation_size", ((sint8) -1));

#if LASH_CHECK_LEVEL >= 1
  if (!ns)
    diag__fail(LASH_ERR_CORRUPT, ((sint8) -1));
#endif  /* >= 1 */

  for (p = ns -> list, r = ZERO_INT8; p; p = p -> next)
    switch(p -> type)
      {
      case NDD_RELATION_NDD:
	r += (sint8) auto_nb_states(p -> inf.ndd -> automaton);
	break;
      case NDD_RELATION_STAR:
	r += ndd_star_info_size(p -> inf.lsi);
	break;
      default:
	diag__fail(LASH_ERR_CORRUPT, ((sint8) -1));
      }

  diag__return(r);
}

/**  ndd_states *ndd_relation_succ(nr, ns)  :  Computes the set of
                     immediate successors of the states belonging to
                     the set *ns by the transition relation
                     represented by *nr.

		     In the case of success, this function returns a
                     pointer to a newly allocated set of states. In
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
                         LASH_ERR_DIMENSION  : Dimension mismatch. 
                         LASH_ERR_BASE       : Base mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch.      **/

ndd_states *ndd_relation_succ(nr, ns)
  ndd_relation *nr;
  ndd_states   *ns;
{
  register ndd             *ndr, *nd1;
  register ndd_states      *nsr;
  register ndd_relation_el *p;
  register int              rc;

  diag__enter("ndd_relation_succ", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nr || !ns || !(ns -> ndd))
     diag__fail(LASH_ERR_CORRUPT, NULL);   
#endif  /* >= 1 */

  if (nr -> base != ns -> ndd -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((nr -> ctrl_len  != ns -> ctrl_len) ||
      (nr -> nb_vars != ns -> ndd -> dim - ns -> ctrl_len)) 
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if ((nr -> msdf) !=
      ((ns -> ndd -> properties & NDD_PROP_MSDF) ? 1 : 0))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  ndr = ndd_create_empty(nr -> base, nr -> ctrl_len + nr -> nb_vars,
            nr -> msdf);
  if (!ndr)
    diag__fail(lash_errno, NULL);

  for (p = nr -> list; p; p = p -> next)
    {
      switch (p -> type)
	{
	case NDD_RELATION_NDD:
	  nd1 = ndd_image_by_transf_info(ns -> ndd, (linear_tr_info *)
	      p -> inf.ndd);
	  if (!nd1)
	    {
	      ndd_free(ndr);
	      diag__fail(lash_errno, NULL);
	    }
	  break;
	  
	case NDD_RELATION_STAR:
	  nd1 = ndd_image_by_star_info(ns -> ndd, p -> inf.lsi);
	  if (!nd1)
	    {
	      ndd_free(ndr);
	      diag__fail(lash_errno, NULL);
	    }
	  break;

	default:
	  diag__fail(LASH_ERR_CORRUPT, NULL);   
	}

      rc = ndd_is_empty(nd1);
      if (rc < 0)
	{
	  ndd_free(nd1);
	  ndd_free(ndr);
	  diag__fail(lash_errno, NULL);
	}

      if (!rc && ndd_merge(ndr, nd1) < 0)
	{
	  ndd_free(nd1);
	  ndd_free(ndr);
	  diag__fail(lash_errno, NULL);
	}
      ndd_free(nd1);
    }

  nsr = resr__new_object(ndd_states);
  if (!nsr)
    {
      ndd_free(ndr);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  nsr -> ndd      = ndr;
  nsr -> ctrl_len = ns -> ctrl_len;

  return nsr;
}

/**  ndd_states *ndd_relation_star_succ(nr, ns, callback)  :  
                     Computes transitively the set of successors of
		     the states belonging to the set *ns by the
		     transition relation represented by *nr. If the
		     pointer callback is not NULL, then the function
		     *callback is called at each step of the fixpoint
		     computation with the current computed set of
		     reachable states (this set cannot be modified by
		     the callback function). If the callback function
		     returns0, then then computation is
		     resumed. Otherwise, it is immediately
		     interrupted. If the value of the parameter
		     callback is NULL, then no callback function is
		     used.

		     In the case of success, this function returns a
                     pointer to a newly allocated set of states. In
                     the case of an error, it returns a NULL pointer.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
                         LASH_ERR_DIMENSION  : Dimension mismatch. 
                         LASH_ERR_BASE       : Base mismatch.
                         LASH_ERR_BAD_TYPE   : Type mismatch. 
                         LASH_ERR_INTERRUPT  : User interruption.  **/

ndd_states *ndd_relation_star_succ(nr, ns, callback)
  ndd_relation *nr;
  ndd_states   *ns;
  int         (*callback)(const ndd_states *);

{
  register ndd            *nd1;
  register ndd_states     *nsr, *nsr2, *nsc;

  diag__enter("ndd_relation_star_succ", NULL);

#if LASH_CHECK_LEVEL >= 1
  if (!nr || !ns || !(ns -> ndd))
     diag__fail(LASH_ERR_CORRUPT, NULL);   
#endif  /* >= 1 */

  if (nr  -> base != ns -> ndd -> base)
    diag__fail(LASH_ERR_BASE, NULL);

  if ((nr -> ctrl_len  != ns -> ctrl_len) ||
      (nr -> nb_vars != ns -> ndd -> dim - ns -> ctrl_len)) 
    diag__fail(LASH_ERR_DIMENSION, NULL);

  if ((nr -> msdf) !=
      ((ns -> ndd -> properties & NDD_PROP_MSDF) ? 1 : 0))
    diag__fail(LASH_ERR_BAD_TYPE, NULL);

  nd1 = ndd_copy(ns -> ndd);
  if (!nd1)
    diag__fail(lash_errno, NULL);

  nsr = resr__new_object(ndd_states);
  if (!nsr)
    {
      ndd_free(nd1);
      diag__fail(lash_errno, NULL);
    }

  nsr -> ctrl_len = ns -> ctrl_len;
  nsr -> ndd = nd1;

  nd1 = ndd_copy(nsr -> ndd);
  if (!nd1)
    {
      ndd_states_free(nsr);
      diag__fail(lash_errno, NULL);
    }

  nsc = resr__new_object(ndd_states);
  if (!nsc)
    {
      ndd_free(nd1);
      ndd_states_free(nsr);
      diag__fail(lash_errno, NULL);
    }

  nsc -> ctrl_len = ns -> ctrl_len;
  nsc -> ndd = nd1;

  for (;;) 
    {
      nsr2 = ndd_relation_succ(nr, nsc);
      if (!nsr2)
	{
	  ndd_states_free(nsc);
	  ndd_states_free(nsr);
	  diag__fail(lash_errno, NULL);
	}

      nd1 = ndd_difference(nsr2 -> ndd, nsr -> ndd);
      ndd_states_free(nsr2);
      if (!nd1)
	{
	  ndd_states_free(nsc);
	  ndd_states_free(nsr);
	  diag__fail(lash_errno, NULL);
	}

      ndd_free(nsc -> ndd);
      nsc -> ndd = nd1;

      if (ndd_is_empty(nd1))
        break;

      if (ndd_merge(nsr -> ndd, nd1) < 0)
	{
	  ndd_states_free(nsc);
	  ndd_states_free(nsr);
	  diag__fail(lash_errno, NULL);
	}

      if (callback && callback(nsr))
        {
	  ndd_states_free(nsc);
          ndd_states_free(nsr);
          diag__fail(LASH_ERR_INTERRUPT, NULL);
        }
     }

  ndd_states_free(nsc);

  diag__return(nsr); 
}

/**  int  ndd_relation_add_sync_transition(nr, orig_c, dest_c,  pm, 
                     tr)  :  Adds to the transition relation 
                     represented by *nr a synchronized transition
		     characterized by the arrays of control
		     descriptors *orig_c and *dest_c, the array of
		     masks *pm, and the linear transformation *tr (an
		     entry pc[i] of *orig_c and *dest_c are only
		     considered if pm[i] != 0, and ignored otherwise).

                     In the case of success, this function returns 0.
		     In the case of an error, it returns -1.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt structure.
                         LASH_ERR_DIMENSION  : Dimension mismatch.
                         LASH_ERR_OVERFLOW   : Arithmetic overflow.
                         LASH_ERR_NOT_IMPL   : Not yet implemented.
                         LASH_ERR_ITERATION  : No iterable.        **/

int  ndd_relation_add_sync_transition(nr, orig_c, dest_c,  pm, tr)
  ndd_relation  *nr;
  sint4         *orig_c, *dest_c;
  int           *pm;
  linear_transf *tr;
{
  register uint4                 n, m, i, j;
  register sint4                *sc, *so, *gc, *go;
  register linear_transf        *tr1;
  register ndd                  *nd1, *nd2, *nd3;
  register ndd_relation_el      *el;

  diag__enter("ndd_relation_add_sync_transition", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!nr || (nr -> ctrl_len && (!orig_c || !dest_c || !pm)) || !tr)
     diag__fail(LASH_ERR_CORRUPT, -1);   
#endif  /* >= 1 */

  if ((tr -> dim) != (nr -> nb_vars))
    diag__fail(LASH_ERR_DIMENSION, -1);   

  n = nr -> ctrl_len;

  for (i = m = 0; i < n; i++)
    if (pm[i])
      m++;

  sc = resr__new_objects(sint4, n * n);
  if (!sc)
    diag__fail(LASH_ERR_NO_MEM, -1);

  memset(sc, 0, n * n * sizeof(sint4));

  so = resr__new_objects(sint4, n);
  if (!so)
    {
      resr__free_objects(sc, sint4, n * n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  memset(so, 0, n * sizeof(sint4));

  gc = resr__new_objects(sint4, (n + m) * n);
  if (!gc)
    {
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  memset(gc, 0, (n + m) * n * sizeof(sint4));

  go = resr__new_objects(sint4, n + m);
  if (!go)
    {
      resr__free_objects(gc, sint4, (n + m) * n);
      resr__free_objects(sc, sint4, n * n);
      resr__free_objects(so, sint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  memset(go, 0, (n + m) * sizeof(sint4));

  for (i = 0; i < n; i++)
    if (pm[i])
      so[i] = dest_c[i];
    else
      sc[(n + 1) * i] = 1;
  
  for (i = j = ZERO_INT4; i < n; i++, j++)
    if (pm[i])
      {
	gc[j * n + i]     = 1;
        go[j]             = orig_c[i];
        gc[(++j) * n + i] = -1;
        go[j]             = -orig_c[i];
      }
    else
      gc[j * n + i] = -1;

  tr1 = ndd_create_transf(n, n + m, sc, so, gc, go);

  resr__free_objects(go, sint4, n + m);
  resr__free_objects(gc, sint4, (n + m) * n);
  resr__free_objects(sc, sint4, n * n);
  resr__free_objects(so, sint4, n);

  if (!tr1)
    diag__fail(lash_errno, -1);

  nd1 = (ndd *) ndd_create_transf_info(tr1, nr -> base, nr -> msdf);
  ndd_transf_free(tr1);
  if (!nd1)
    diag__fail(lash_errno, -1);

  nd2 = (ndd *) ndd_create_transf_info(tr, nr -> base, nr -> msdf);
  if (!nd2)
    {
      ndd_free(nd1);
      diag__fail(lash_errno, -1);
    }

  nd3 = ndd_product(nd1, nd2);
  ndd_free(nd1);
  ndd_free(nd2);
  
  el = (ndd_relation_el *) resr__new_object(ndd_relation_el);
  if (!el)
    {
      ndd_free(nd3);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  el -> type = NDD_RELATION_NDD;
  el -> inf.ndd = nd3;
  el -> next = nr -> list;
  nr -> list = el; 

  diag__return(0);
}

/****  End of ndd-machines.c  ****/
