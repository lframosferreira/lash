/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**     ndd-io.c  :  I/O operations over NDDs.                     **/
/**                                                                **/
/**     10/17/00  :  Creation. (JMF)                               **/
/**     02/12/01  :  Minor correction. (BB)                        **/
/**     07/09/02  :  Reorganization. (BB)                          **/
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
#include "lash-io.h"
#include "lash-auto-io.h"
#include "lash-ndd-io.h"
#include "lash-ndd.h"
#include "ndd.h"
#include "ndd-io.h"
#include "io-ops.h"
#include "diag.h"
#include "resource.h"

/****  Datatypes.                                                ****/

/**  Infos needed to serialize a NDD.                              **/

typedef struct {
  ndd        *nd;
  ndd_labels *nlabel;
} ndd_infos;


/****  Prototypes of private functions.                          ****/

static int  serialize_header(ndd_infos *, int (*)(uint4, void *,
						  uint1 *), void *);
static int  serialize_header_dim(ndd_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_header_base(ndd_infos *, int (*)(uint4, void *,
						 uint1 *), void *);
static int  serialize_body(ndd_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_body_automaton(ndd_infos *,
				     int (*)(uint4, void *, uint1 *),
				     void *);
static int  serialize_info(ndd_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_info_properties(ndd_infos *,
				      int (*)(uint4, void *, uint1 *),
				      void *);
static int  serialize_info_labels(ndd_infos *,
				  int (*)(uint4, void *, uint1 *),
				  void *);
static int  read_header(ndd_infos *, int (*)(uint4, void *, uint1 *),
			void *);
static int  read_header_dim(ndd_infos *,
			    int (*)(uint4, void *, uint1 *), void *);
static int  read_header_base(ndd_infos *,
			     int (*)(uint4, void *, uint1 *), void *);
static int  read_body(ndd_infos *, int (*)(uint4, void *, uint1 *),
		      void *);
static int  read_info(ndd_infos *, int (*)(uint4, void *, uint1 *),
		      void *);
static int  read_info_properties(ndd_infos *,
				 int (*)(uint4, void *, uint1 *),
				 void *);
static int  read_info_properties_serial(ndd_infos *,
					int (*)(uint4, void *,
						uint1 *), void *);
static int  read_info_properties_msdf(ndd_infos *,
				      int (*)(uint4, void *, uint1 *),
				      void *);
static int  read_info_labels(ndd_infos *,
			     int (*)(uint4, void *, uint1 *), void *);


/****  Private functions.                                        ****/

/**  int  serialize_header(n, f, arg)  :  Outputs the header
                     associated to the NDD held in *n using the
		     function *f with arg as its second argument (see
		     ndd_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_HEADER) ||
      serialize_header_dim(n, f, arg) ||
      serialize_header_base(n, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_header_dim(n, f, arg)  :  Outputs the dimension
                     of the NDD held in *n using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header_dim(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_HEADER_DIM) ||
      io__w_num(f, arg, n -> nd -> dim))
    return -1;

  return 0;
}

/**  int  serialize_header_base(n, f, arg)  :  Outputs the base
                     of the NDD held in *n using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header_base(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_HEADER_BASE) ||
      io__w_num(f, arg, n -> nd -> base))
    return -1;

  return 0;
}

/**  int  serialize_body(n, f, arg)  :  Outputs the header
                     associated to the NDD held *n using the function
		     *f with arg as its second argument (see
		     ndd_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_body(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_BODY) ||
      serialize_body_automaton(n, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_body_automaton(n, f, arg)  :  Outputs the
                     automaton associated to the NDD held in *n using
		     the function *f with arg as its second argument
		     (see ndd_serialize for a complete description of
		     *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_body_automaton(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_BODY_AUTOMATON) ||
      auto_serialize_write(n -> nd -> automaton, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_info(n, f, arg)  :  Outputs the informations
                     associated to the NDD held in *n using the
		     function *f with arg as its second argument (see
		     ndd_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_INFO) ||
      serialize_info_properties(n, f, arg) ||
      serialize_info_labels(n, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_info_properties(n, f, arg)  :  Outputs the known
                     properties associated to the NDD held in *n using
		     the function *f with arg as its second argument
		     (see ndd_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info_properties(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__NDD_INFO_PROPERTIES))
    return -1;
  
  if ((n -> nd -> properties & NDD_PROP_SERIAL) &&
      io__w_tag(f, arg, IO_TAG__NDD_INFO_PROPERTIES_SERIAL))
    return -1;
  
  if ((n -> nd -> properties & NDD_PROP_MSDF) &&
      io__w_tag(f, arg, IO_TAG__NDD_INFO_PROPERTIES_MSDF))
    return -1;
  
  if (io__w_tag(f, arg, IO_TAG__END))
    return -1;
  
  return 0;
}

/**  int  serialize_info_labels(n, f, arg)  :  Outputs the labels
                     of the NDD held in *n using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info_labels(n, f, arg)
     ndd_infos  *n;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (n -> nlabel)
    {
      uint4  i = ZERO_INT4;
      
      if (io__w_tag(f, arg, IO_TAG__NDD_INFO_LABELS))
	return -1;

      for (; i < n -> nlabel -> n; i++)
	{
	  char *label = (n -> nlabel -> labels)[i];
	  
#if LASH_CHECK_LEVEL >= 1
	  if (label && ((sint4)strlen(label)) != strlen(label))
	    {
	      lash_errno = LASH_ERR_NOT_IMPL;
	      return -1;
	    }
#endif  /* >= 1 */
	  
	  if (io__w_snum(f, arg,
			 label ? strlen(label) : (ZERO_INT4 - 1)) ||
	      (label && io__w_text(f, arg, label)))
	    return -1;
	}
    }
  
  return 0;
}

/**  int  read_header(pn, f, arg)  :  Reads the header section
                     of the serial encoding of a NDD using the
		     function *f with arg as its second argument
		     (see ndd_serialize_read for a complete
		     description of *f).

		     The informations held in pn are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  return(io__r_set(pn, f, arg, 2,
		   IO_TAG__NDD_HEADER_DIM,
		   read_header_dim,
		   IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE,
		   IO_TAG__NDD_HEADER_BASE,
		   read_header_base,
		   IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE));
}

/**  int  read_header_dim(pn, f, arg)  :  Reads the dimension
                     of the serial encoding of a NDD using the
		     function *f with arg as its second argument
		     (see ndd_serialize_read for a complete
		     description of *f).

		     The informations held in pn are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header_dim(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__r_num(f, arg, &(pn -> nd -> dim)))
    return -1;

  return 0;
}

/**  int  read_header_base(pn, f, arg)  :  Reads the base
                     of the serial encoding of a NDD using the
		     function *f with arg as its second argument
		     (see ndd_serialize_read for a complete
		     description of *f).

		     The informations held in pn are updated
		     according to the data read.
		     
		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header_base(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  base;

  if (io__r_num(f, arg, &base) || base > 0xFFU)
    return -1;

  pn -> nd -> base = (uint1)base;

  return 0;
}

/**  int  read_body(pn, f, arg)  :  Reads the body of the encoding
                     of a NDD using the function *f with arg as its
		     second argument (see ndd_serialize_read for a
		     complete description of *f).

		     The informations held in pn are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_body(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint1  tag;

  lash_errno = LASH_ERR_FILE;

  if (io__r_tag(f, arg, &tag) ||
      (tag != IO_TAG__NDD_BODY_AUTOMATON) ||
      !(pn -> nd -> automaton = auto_serialize_read(f, arg)) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__END))
    return -1;

  return 0;
}

/**  int  read_info(pn, f, arg)  :  Reads the information included in
                     the encoding of a NDD using the function *f with
		     arg as its second argument (see
		     ndd_serialize_read for a complete description of
		     *f).

		     The informations held in pn are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_info(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  lash_errno = LASH_ERR_FILE;

  if (io__r_set(pn, f, arg, 2,
		IO_TAG__NDD_INFO_PROPERTIES,
		read_info_properties,
		IO_SET_PROP_UNIQUE,
		IO_TAG__NDD_INFO_LABELS,
		read_info_labels,
		IO_SET_PROP_UNIQUE))
    return -1;
 
  return 0;
}

/**  int  read_info_properties(pn, f, arg)  :  Reads the properties
                     included in the encoding of a NDD using the
                     function *f with arg as its second argument (see
		     ndd_serialize_read for a complete description of
		     *f).

		     The informations held in pn are updated
		     according to the data read.
		     
		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_info_properties(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  return(io__r_set(pn, f, arg, 2,
		   IO_TAG__NDD_INFO_PROPERTIES_SERIAL,
		   read_info_properties_serial,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__NDD_INFO_PROPERTIES_MSDF,
		   read_info_properties_msdf,
		   IO_SET_PROP_UNIQUE));
}


/* int  read_info_properties_serial(pn, f, arg)  :  Sets the 'serial'
                     property of a NDD.

		     The NDD **pn is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_properties_serial(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  pn -> nd -> properties |= NDD_PROP_SERIAL;
  return 0;
}

/* int  read_info_properties_msdf(pn, f, arg)  :  Sets the 'msdf'
                     property of a NDD.

		     The NDD **pn is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_properties_msdf(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  pn -> nd -> properties |= NDD_PROP_MSDF;
  return 0;
}

/* int  read_info_labels(pn, f, arg)  :  Sets the labels associated
                     to the variables of a NDD.

		     The NDD **pn is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_labels(pn, f, arg)
     ndd_infos *pn;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  uint4  i = ZERO_INT4;
  sint4  l;
    
  if (!(pn -> nlabel = ndd_labels_new(pn -> nd -> dim)))
      return -1;
  
  for (; i < pn -> nlabel -> n; i++)
    {
      if (io__r_snum(f, arg, &l))
	return -1;

      if (l >= ZERO_INT4)
	{
	  char *label;
	  
	  if (!(label = resr__new_objects(char, (uint4)l + 1)))
	    {
	      lash_errno = LASH_ERR_NO_MEM;
	      return -1;
	    }
	  
	  if (io__r_text(f, arg, (uint4)l, &label) ||
	      ndd_labels_set(pn -> nlabel, i, label))
	    {
	      resr__free_objects(label, char, (uint4)l);
	      return -1;
	    }
	  
	  resr__free_objects(label, char, (uint4)l + 1);
	}
      else
	(pn -> nlabel -> labels)[i] = NULL;
    }
 
  return 0;
}


/****  Public visible function.                                  ****/

/**  ndd_labels *ndd_labels_new(n)  :  Creates a new array of n
                     labels.

		     Returns a pointer to the newly allocated array
		     in the case of success, or NULL in the case of
		     an error.

		     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt data.
			 LASH_ERR_NO_MEM  : Not enough memory.     **/

ndd_labels *ndd_labels_new(n)
     uint4  n;
{
  ndd_labels *rv;
 
  if (!(rv = resr__new_object(ndd_labels)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }
  
  if (n)
    {
      if (!(rv -> labels = resr__new_objects(char *, n)))
	{
	  resr__free_object(rv, ndd_labels);
	  lash_errno = LASH_ERR_NO_MEM;
	  return NULL;
	}
    }
  else
    rv -> labels = NULL;

  rv -> n = n;

  while(n)
    (rv -> labels)[--n] = NULL;

  return(rv);
}

/**  void  ndd_labels_free(nl)  :  Frees the memory allocated for
                     the array of labels *nl.                      **/

void  ndd_labels_free(nl)
     ndd_labels *nl;
{
  uint4  i = ZERO_INT4;

#if LASH_CHECK_LEVEL >= 1
  if (!nl)
    return;
#endif  /* >= 2 */
  
  for (; i < nl -> n; i++)
    if (nl -> labels[i])
      resr__free_objects(nl -> labels[i], char,
			 strlen(nl -> labels[i]) + 1);
  
  if (nl -> n)
    resr__free_objects(nl -> labels, char *, nl -> n);
  resr__free_object(nl, ndd_labels);
  
  return;
}

/**  int  ndd_labels_set(nl, n, l)  :  Sets the (n+1)-th label of the
                     array of labels *nl to l.

                     If l is NULL, the label is undefined.

		     Returns 0 in the case of success, -1 (and sets
		     lash_errno) in the case of an error.

		     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt data.
			 LASH_ERR_NO_MEM  : Not enough memory.     **/

int  ndd_labels_set(nl, n, l)
     ndd_labels *nl;
     uint4       n;
     char       *l;
{ 
#if LASH_CHECK_LEVEL >= 1
  if (!nl || n >= nl -> n)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 1 */
  
  if ((nl -> labels)[n])
    resr__free_objects((nl -> labels)[n], char,
		       strlen((nl -> labels)[n] + 1));
  
  if (l)
    {
      if (!((nl -> labels)[n] =
	    resr__new_objects(char, strlen(l) + 1)))
	{
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
      
      memcpy((nl -> labels)[n], l, strlen(l) + 1);
    }
  else
    (nl -> labels)[n] = NULL;
  
  return 0;
}

/**  int  ndd_serialize_write_labeled(n, l, f, arg)  :  Outputs the
                     NDD *n using the function *f.

		     *f takes 3 arguments. The first is the length
		     (in bytes) of the data to output; the second is
		     arg; the third is a pointer to the data to
		     output.

		     l is an array of (n -> dim) labels, or NULL if
		     no label is defined.
	
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         + error codes reported by *f.             **/

int  ndd_serialize_write_labeled(n, l, f, arg)
     ndd        *n;
     ndd_labels *l;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  ndd_infos  ni;

  diag__enter("ndd_serialize_write_labeled", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!n || !f || (l && (l -> n != n -> dim)))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  ni.nd = n;
  ni.nlabel = l;
  
  if (io__w_tag(f, arg, IO_TAG__NDD) ||
      serialize_header(&ni, f, arg) ||
      serialize_body(&ni, f, arg) ||
      serialize_info(&ni, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  ndd *ndd_serialize_read_labeled(f, arg, l)  :  Reads the NDD
                     written in the stream *arg using the function *f.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the NDD read.
		     If l is NULL, the (eventual) labels will be
		     left out.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.
			 + error codes reported by *f.             **/

ndd *ndd_serialize_read_labeled(f, arg, l)
     int        (*f)(uint4, void *, uint1 *);
     ndd_labels **l;
     void        *arg;
{
  uint1      tag;
  ndd_infos  ni;

  diag__enter("ndd_serialize_read_labeled", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(ni.nd = resr__new_object(ndd)))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ni.nlabel = NULL;
  
  ni.nd -> properties = ZERO_INT1;
  lash_errno = LASH_ERR_FILE;
  
  if (io__r_tag(f, arg, &tag) || (tag != IO_TAG__NDD) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__NDD_HEADER) ||
      read_header(&ni, f, arg) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__NDD_BODY) ||
      read_body(&ni, f, arg) ||
      io__r_tag(f, arg, &tag) ||
      ((tag != IO_TAG__NDD_INFO) && (tag != IO_TAG__END)))
    {
      resr__free_object(ni.nd, ndd);
      diag__fail(lash_errno, NULL);
    }
  
  if ((tag == IO_TAG__NDD_INFO) &&
      (read_info(&ni, f, arg) ||
       io__r_tag(f, arg, &tag) || (tag != IO_TAG__END)))
    {
      resr__free_object(ni.nd, ndd);
      diag__fail(lash_errno, NULL);
    }

  if (l)
    *l = ni.nlabel;
  else
    if (ni.nlabel)
      ndd_labels_free(ni.nlabel);
  
  diag__return(ni.nd);
}

/**  int  ndd_serialize_write_mem_labeled(n, l, mb)  :  Outputs the
                     NDD *n in the block of memory *mb.
		     
		     l is an array of (n -> dim) labels, or NULL if
		     no label is defined.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno (in this case, the content
		     of mb is not modified).
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  ndd_serialize_write_mem_labeled(n, l, mb)
     ndd        *n;
     ndd_labels *l;
     mem_block  *mb;
{
  mem_descr *m;
  uint1     *p;
  uint4      lr;
  
  diag__enter("ndd_serialize_write_mem_labeled", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!n || !mb)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
  
  if (((mb -> size != ZERO_INT4) && mem_block__content_free(mb)) ||
      !(m = io__to_mem_init()))
    diag__fail(lash_errno, -1);
  
  if (ndd_serialize_write(n,
			  (int (*)(uint4, void *, uint1 *))
			  io__to_mem,
			  (void *)m))
    {
      io__to_mem_end(m, &p, &lr);
      resr__free_objects(p, char, lr);
      diag__fail(lash_errno, -1);
    }
  
  io__to_mem_end(m, &p, &lr);
  
  if (mem_block__content_set(mb, p, lr))
    diag__fail(lash_errno, -1);
  
  resr__free_objects(p, char, lr);
  
  diag__return(0);
}

/**  ndd *ndd_serialize_read_mem_labeled(mb, l)  :  Reads the block 
                     of memory *mb and outputs the NDD read.

		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the NDD read.
		     If l is NULL, the (eventual) labels will be
		     left out.

		     If successful, this functions returns the
		     newly allocated NDD. In the case of an error, it
		     returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_FILE     : Bad file format.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

ndd *ndd_serialize_read_mem_labeled(mb, l)
     mem_block   *mb;
     ndd_labels **l;
{
  ndd       *n;
  mem_descr *md;

  diag__enter("ndd_serialize_read_mem_labeled", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!mb)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(md = io__from_mem_init(mb -> beginning, mb -> size)))
    diag__fail(lash_errno, NULL);

  if (!(n = ndd_serialize_read((int (*)(uint4, void *, uint1 *))
			       io__from_mem, (void *)md)))
    {
      io__from_mem_end(md);
      diag__fail(lash_errno, NULL);
    }
  
  io__from_mem_end(md);
  diag__return(n);
}

/**  int  ndd_serialize_write_file_labeled(n, l, f)  :  Outputs the
                     NDD *n to the file *f.  If this file already
		     exists, its content is destroyed.
		     
		     l is an array of (n -> dim) labels, or NULL if
		     no label is defined.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  ndd_serialize_write_file_labeled(n, l, f)
     ndd        *n;
     ndd_labels *l;
     char       *f;
{
  FILE *file;

  diag__enter("ndd_serialize_write_file_labeled", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!n || !f)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!(file = io__to_file_init(f)))
    diag__fail(lash_errno, -1);
 
  if (ndd_serialize_write_labeled(n, l,
				   (int (*)(uint4, void *, uint1 *))
				   io__to_file, (void *)file))
    {
      io__to_file_end(file);
      diag__fail(lash_errno, -1);
    }
 
  if (io__to_file_end(file))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  ndd *ndd_serialize_read_file_labeled(f, l)  :  Reads the file *f
                     and outputs the NDD read.

		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the NDD read.
		     If l is NULL, the (eventual) labels will be
		     left out.

		     If successful, this functions returns the newly
		     allocated NDD. In the case of an error, it
		     returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_FILE     : Bad file format.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

ndd *ndd_serialize_read_file_labeled(f, l)
     char        *f;
     ndd_labels **l;
{
  ndd  *n;
  FILE *file;

  diag__enter("ndd_serialize_read_file_labeled", NULL);
 
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(file = io__from_file_init(f)))
    diag__fail(lash_errno, NULL);

  if (!(n = ndd_serialize_read_labeled((int (*)(uint4, void *,
						 uint1 *))
					io__from_file, (void *)file,
					l)))
    {
      io__from_file_end(file);
      diag__fail(lash_errno, NULL);
    }

  if (io__from_file_end(file))
    diag__fail(lash_errno, NULL);
  
  diag__return(n);
}

/****  End of auto-io.c  ****/
