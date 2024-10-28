/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**     rva-io.c  :  I/O operations over RVAs.                     **/
/**                                                                **/
/**     08/06/01  :  Creation. (SJ)                                **/
/**     08/20/01  :  0-dimensional RVAs. (SJ)                      **/
/**     07/10/02  :  Reorganization. (BB)                          **/
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
#include "lash-rva-io.h"
#include "lash-rva.h"
#include "rva.h"
#include "rva-io.h"
#include "io-ops.h"
#include "diag.h"
#include "resource.h"

/****  Datatypes.                                                ****/

/**  Infos needed to serialize a RVA.                              **/

typedef struct {
  rva        *rv;
  rva_labels *nlabel;
} rva_infos;

/****  Prototypes of private functions.                          ****/

static int  serialize_header(rva_infos *, int (*)(uint4, void *,
						  uint1 *), void *);
static int  serialize_header_dim(rva_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_header_base(rva_infos *, int (*)(uint4, void *,
						 uint1 *), void *);
static int  serialize_header_universal(rva_infos *, 
				       int (*)(uint4, void *,
					       uint1 *), void *);
static int  serialize_body(rva_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_body_automaton(rva_infos *,
				     int (*)(uint4, void *, uint1 *),
				     void *);
static int  serialize_info(rva_infos *, int (*)(uint4, void *,
						uint1 *), void *);
static int  serialize_info_properties(rva_infos *,
				      int (*)(uint4, void *, uint1 *),
				      void *);
static int  serialize_info_labels(rva_infos *,
				  int (*)(uint4, void *, uint1 *),
				  void *);
static int  read_header(rva_infos *, int (*)(uint4, void *, uint1 *),
			void *);
static int  read_header_dim(rva_infos *,
			    int (*)(uint4, void *, uint1 *), void *);
static int  read_header_base(rva_infos *,
			     int (*)(uint4, void *, uint1 *), void *);
static int  read_header_universal(rva_infos *, 
				  int (*)(uint4, void *, uint1 *), 
				  void *);
static int  read_body(rva_infos *, int (*)(uint4, void *, uint1 *),
		      void *);
static int  read_info(rva_infos *, int (*)(uint4, void *, uint1 *),
		      void *);
static int  read_info_properties(rva_infos *,
				 int (*)(uint4, void *, uint1 *),
				 void *);
static int  read_info_properties_serial(rva_infos *,
					int (*)(uint4, void *,
						uint1 *), void *);
static int  read_info_properties_restricted(rva_infos *,
					    int (*)(uint4, void *, 
						    uint1 *), void *);
static int  read_info_labels(rva_infos *,
			     int (*)(uint4, void *, uint1 *), void *);


/****  Private functions.                                        ****/

/**  int  serialize_header(r, f, arg)  :  Outputs the header
                     associated to the RVA held in *r using the
		     function *f with arg as its second argument (see
		     rva_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_HEADER) ||
      serialize_header_dim(r, f, arg) ||
      serialize_header_base(r, f, arg) ||
      serialize_header_universal(r, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_header_dim(r, f, arg)  :  Outputs the dimension
                     of the RVA held in *r using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header_dim(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_HEADER_DIM) ||
      io__w_num(f, arg, r -> rv -> dim))
    return -1;

  return 0;
}

/**  int  serialize_header_base(r, f, arg)  :  Outputs the base
                     of the RVA held in *r using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header_base(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_HEADER_BASE) ||
      io__w_num(f, arg, r -> rv -> base))
    return -1;

  return 0;
}

/**  int  serialize_header_universal(r, f, arg)  :  Outputs the fact
                     whether the RVA held in *r represents the 
		     universal set. This information is only relevant
		     if *r is 0-dimensional. This function uses the
		     function *f with arg as its second argument 
		     (see auto_serialize for a complete description 
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_header_universal(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_HEADER_UNIVERSAL) ||
      io__w_num(f, arg, r -> rv -> universal))
    return -1;

  return 0;
}

/**  int  serialize_body(r, f, arg)  :  When the RVA held in *r is not
                     0-dimensional, outputs its associated automaton
		     using the function *f with arg as its second
		     argument (see rva_serialize for a complete 
		     description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_body(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (!(r -> rv -> dim))
    return 0;

  if (io__w_tag(f, arg, IO_TAG__RVA_BODY) ||
      serialize_body_automaton(r, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_body_automaton(r, f, arg)  :  Outputs the
                     automaton associated to the RVA held in *r using
		     the function *f with arg as its second argument
		     (see rva_serialize for a complete description of
		     *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_body_automaton(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_BODY_AUTOMATON) ||
      auto_serialize_write(r -> rv -> automaton, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_info(r, f, arg)  :  Outputs the informations
                     associated to the RVA held in *r using the
		     function *f with arg as its second argument (see
		     rva_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_INFO) ||
      serialize_info_properties(r, f, arg) ||
      serialize_info_labels(r, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_info_properties(r, f, arg)  :  Outputs the known
                     properties associated to the RVA held in *r using
		     the function *f with arg as its second argument
		     (see rva_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info_properties(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (io__w_tag(f, arg, IO_TAG__RVA_INFO_PROPERTIES))
    return -1;
  
  if ((r -> rv -> properties & RVA_PROP_SERIAL) &&
      io__w_tag(f, arg, IO_TAG__RVA_INFO_PROPERTIES_SERIAL))
    return -1;
  
  if ((r -> rv -> properties & RVA_PROP_RESTRICTED) &&
      io__w_tag(f, arg, IO_TAG__RVA_INFO_PROPERTIES_RESTRICTED))
    return -1;
  
  if (io__w_tag(f, arg, IO_TAG__END))
    return -1;
  
  return 0;
}

/**  int  serialize_info_labels(r, f, arg)  :  Outputs the labels
                     of the RVA held in *r using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_info_labels(r, f, arg)
     rva_infos  *r;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  if (r -> nlabel)
    {
      uint4  i = ZERO_INT4;
      
      if (io__w_tag(f, arg, IO_TAG__RVA_INFO_LABELS))
	return -1;

      for (; i < r -> nlabel -> n; i++)
	{
	  char *label = (r -> nlabel -> labels)[i];
	  
#if LASH_CHECK_LEVEL >= 1
	  if (label && ((sint4) strlen(label)) != strlen(label))
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

/**  int  read_header(pr, f, arg)  :  Reads the header section
                     of the serial encoding of a RVA using the
		     function *f with arg as its second argument
		     (see rva_serialize_read for a complete
		     description of *f).

		     The informations held in pr are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  return(io__r_set(pr, f, arg, 3,
		   IO_TAG__RVA_HEADER_DIM,
		   read_header_dim,
		   IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE,
		   IO_TAG__RVA_HEADER_BASE,
		   read_header_base,
		   IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE,
		   IO_TAG__RVA_HEADER_UNIVERSAL,
		   read_header_universal,
		   IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE));
}

/**  int  read_header_dim(pr, f, arg)  :  Reads the dimension
                     of the serial encoding of a RVA using the
		     function *f with arg as its second argument
		     (see rva_serialize_read for a complete
		     description of *f).

		     The informations held in pr are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header_dim(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__r_num(f, arg, &(pr -> rv -> dim)))
    return -1;

  return 0;
}

/**  int  read_header_base(pr, f, arg)  :  Reads the base
                     of the serial encoding of a RVA using the
		     function *f with arg as its second argument
		     (see rva_serialize_read for a complete
		     description of *f).

		     The informations held in pr are updated
		     according to the data read.
		     
		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header_base(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  base;

  if (io__r_num(f, arg, &base) || base > 0xFFU)
    return -1;

  pr -> rv -> base = (uint1) base;

  return 0;
}

/**  int  read_header_universal(pr, f, arg)  :  Reads the fact
                     whether the RVA represents the universal set.
		     This information is only relevant for 
		     0-dimensional RVAs. This function uses the
		     function *f with arg as its second argument
		     (see rva_serialize_read for a complete
		     description of *f).

		     The informations held in pr are updated
		     according to the data read.
		     
		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_header_universal(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  universal;

  if (io__r_num(f, arg, &universal) || 
      (universal != 0 && universal != 1))
    return -1;

  pr -> rv -> universal = (uint1) universal;

  return 0;
}

/**  int  read_body(pr, f, arg)  :  Reads the body of the encoding
                     of a RVA using the function *f with arg as its
		     second argument (see rva_serialize_read for a
		     complete description of *f).

		     The informations held in pr are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_body(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint1  tag;

  lash_errno = LASH_ERR_FILE;

  if (io__r_tag(f, arg, &tag) ||
      tag != IO_TAG__RVA_BODY_AUTOMATON ||
      !(pr -> rv -> automaton = auto_serialize_read(f, arg)))
    return -1;

  if (io__r_tag(f, arg, &tag) || 
      (tag != IO_TAG__END))
    {
      auto_free(pr -> rv -> automaton);
      return -1;
    }

  return 0;
}

/**  int  read_info(pr, f, arg)  :  Reads the information included in
                     the encoding of a RVA using the function *f with
		     arg as its second argument (see
		     rva_serialize_read for a complete description of
		     *f).

		     The informations held in pr are updated
		     according to the data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_info(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  lash_errno = LASH_ERR_FILE;

  if (io__r_set(pr, f, arg, 2,
		IO_TAG__RVA_INFO_PROPERTIES,
		read_info_properties,
		IO_SET_PROP_UNIQUE,
		IO_TAG__RVA_INFO_LABELS,
		read_info_labels,
		IO_SET_PROP_UNIQUE))
    return -1;
 
  return 0;
}

/**  int  read_info_properties(pr, f, arg)  :  Reads the properties
                     included in the encoding of a RVA using the
                     function *f with arg as its second argument (see
		     rva_serialize_read for a complete description of
		     *f).

		     The informations held in pr are updated
		     according to the data read.
		     
		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_FILE     : File format error.
			 + error codes given by *f.                **/

static int  read_info_properties(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  return(io__r_set(pr, f, arg, 2,
		   IO_TAG__RVA_INFO_PROPERTIES_SERIAL,
		   read_info_properties_serial,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__RVA_INFO_PROPERTIES_RESTRICTED,
		   read_info_properties_restricted,
		   IO_SET_PROP_UNIQUE));
}

/* int  read_info_properties_serial(pr, f, arg)  :  Sets the 'serial'
                     property of a RVA.

		     The RVA **pr is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_properties_serial(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  pr -> rv -> properties |= RVA_PROP_SERIAL;
  return 0;
}

/* int  read_info_properties_restricted(pr, f, arg)  :  Sets the
                     'restricted' property of a RVA.

		     The RVA **pr is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_properties_restricted(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  pr -> rv -> properties |= RVA_PROP_RESTRICTED;
  return 0;
}

/* int  read_info_labels(pr, f, arg)  :  Sets the labels associated
                     to the variables of a RVA.

		     The RVA **pr is updated according to the
		     data read.

		     If successful, this functions 0.  In the case
		     of an error, it returns -1 and sets lash_errno.

                     Possible error codes:                         **/

static int  read_info_labels(pr, f, arg)
     rva_infos *pr;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{ 
  uint4  i = ZERO_INT4;
  sint4  l;
    
  if (!(pr -> nlabel = rva_labels_new(pr -> rv -> dim)))
      return -1;
  
  for (; i < pr -> nlabel -> n; i++)
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
	      rva_labels_set(pr -> nlabel, i, label))
	    {
	      resr__free_objects(label, char, (uint4)l);
	      return -1;
	    }
	  
	  resr__free_objects(label, char, (uint4)l + 1);
	}
      else
	(pr -> nlabel -> labels)[i] = NULL;
    }
 
  return 0;
}

/****  Public visible function.                                  ****/

/**  rva_labels *rva_labels_new(n)  :  Creates a new array of n
                     labels.

		     Returns a pointer to the newly allocated array
		     in the case of success, or NULL in the case of
		     an error.

		     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt data.
			 LASH_ERR_NO_MEM  : Not enough memory.     **/

rva_labels *rva_labels_new(n)
     uint4  n;
{
  rva_labels *rl;
 
  if (!(rl = resr__new_object(rva_labels)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }
  
  if (n)
    {
      if (!(rl -> labels = resr__new_objects(char *, n)))
	{
	  resr__free_object(rl, rva_labels);
	  lash_errno = LASH_ERR_NO_MEM;
	  return NULL;
	}
    }
  else
    rl -> labels = NULL;

  rl -> n = n;

  while(n)
    (rl -> labels)[--n] = NULL;

  return(rl);
}

/**  void  rva_labels_free(rl)  :  Frees the memory allocated for
                     the array of labels *rl.                      **/

void  rva_labels_free(rl)
     rva_labels *rl;
{
  uint4  i = ZERO_INT4;

#if LASH_CHECK_LEVEL >= 1
  if (!rl)
    return;
#endif  /* >= 2 */
  
  for (; i < rl -> n; i++)
    if (rl -> labels[i])
      resr__free_objects(rl -> labels[i], char,
			 strlen(rl -> labels[i]) + 1);
  
  if (rl -> n)
    resr__free_objects(rl -> labels, char *, rl -> n);
  resr__free_object(rl, rva_labels);
  
  return;
}

/**  int  rva_labels_set(rl, n, l)  :  Sets the (n+1)-th label of the
                     array of labels *rl to l.

                     If l is NULL, the label is undefined.

		     Returns 0 in the case of success, -1 (and sets
		     lash_errno) in the case of an error.

		     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt data.
			 LASH_ERR_NO_MEM  : Not enough memory.     **/

int  rva_labels_set(rl, n, l)
     rva_labels *rl;
     uint4       n;
     char       *l;
{ 
#if LASH_CHECK_LEVEL >= 1
  if (!rl || n >= rl -> n)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 1 */
  
  if ((rl -> labels)[n])
    resr__free_objects((rl -> labels)[n], char,
		       strlen((rl -> labels)[n] + 1));
  
  if (l)
    {
      if (!((rl -> labels)[n] =
	    resr__new_objects(char, strlen(l) + 1)))
	{
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
      
      memcpy((rl -> labels)[n], l, strlen(l) + 1);
    }
  else
    (rl -> labels)[n] = NULL;
  
  return 0;
}

/**  int  rva_serialize_write_labeled(r, l, f, arg)  :  Outputs the
                     RVA *r using the function *f.

		     *f takes 3 arguments. The first is the length
		     (in bytes) of the data to output; the second is
		     arg; the third is a pointer to the data to
		     output.

		     l is an array of (r -> dim) labels, or NULL if
		     no label is defined.
	
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         + error codes reported by *f.             **/

int  rva_serialize_write_labeled(r, l, f, arg)
     rva        *r;
     rva_labels *l;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  rva_infos  ri;

  diag__enter("rva_serialize_write_labeled", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!r || !f || (l && (l -> n != r -> dim)))
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  ri.rv = r;
  ri.nlabel = l;
  
  if (io__w_tag(f, arg, IO_TAG__RVA) ||
      serialize_header(&ri, f, arg) ||
      serialize_body(&ri, f, arg) ||
      serialize_info(&ri, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  rva *rva_serialize_read_labeled(f, arg, l)  :  Reads the RVA
                     written in the stream *arg using the function *f.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the RVA read.
		     If l is NULL, the (eventual) labels will be
		     left out.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.
                         LASH_ERR_FILE     : File format error.
			 + error codes reported by *f.             **/

rva *rva_serialize_read_labeled(f, arg, l)
     int        (*f)(uint4, void *, uint1 *);
     rva_labels **l;
     void        *arg;
{
  uint1      tag;
  rva_infos  ri;

  diag__enter("rva_serialize_read_labeled", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(ri.rv = resr__new_object(rva)))
    diag__fail(LASH_ERR_NO_MEM, NULL);

  ri.nlabel = NULL;
  
  ri.rv -> properties = RVA_PROP_NOTHING;
  ri.rv -> automaton = NULL;
  lash_errno = LASH_ERR_FILE;
  
  if (io__r_tag(f, arg, &tag) || (tag != IO_TAG__RVA) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__RVA_HEADER) ||
      read_header(&ri, f, arg) ||
      io__r_tag(f, arg, &tag))
    {
      resr__free_object(ri.rv, rva);
      diag__fail(lash_errno ? lash_errno : LASH_ERR_FILE, NULL);
    }

  if ((tag == IO_TAG__RVA_BODY && !(ri.rv -> dim)) ||
      (tag != IO_TAG__RVA_BODY && (ri.rv -> dim)))
    {
      resr__free_object(ri.rv, rva);
      diag__fail(LASH_ERR_FILE, NULL);
    }

  if (tag == IO_TAG__RVA_BODY)
    {
      if (read_body(&ri, f, arg) ||
	  io__r_tag(f, arg, &tag))
	{
	  
	  resr__free_object(ri.rv, rva);
	  diag__fail(lash_errno ? lash_errno : LASH_ERR_FILE, NULL);
	}
    }
 
  if ((tag != IO_TAG__RVA_INFO) && (tag != IO_TAG__END))
    {
      if (ri.rv -> dim)
	auto_free(ri.rv -> automaton);
      resr__free_object(ri.rv, rva);
      diag__fail(lash_errno ? lash_errno : LASH_ERR_FILE, NULL);
    }
  
  if ((tag == IO_TAG__RVA_INFO) &&
      (read_info(&ri, f, arg) ||
       io__r_tag(f, arg, &tag) || (tag != IO_TAG__END)))
    {
      if (ri.rv -> dim)
	auto_free(ri.rv -> automaton);
      if (ri.nlabel)
	rva_labels_free(ri.nlabel);
      resr__free_object(ri.rv, rva);
      diag__fail(lash_errno ? lash_errno : LASH_ERR_FILE, NULL);
    }

  if (l)
    *l = ri.nlabel;
  else
    if (ri.nlabel)
      rva_labels_free(ri.nlabel);
  
  diag__return(ri.rv);
}

/**  int  rva_serialize_write_mem_labeled(r, l, mb)  :  Outputs the
                     RVA *r in the block of memory *mb.
		     
		     l is an array of (r -> dim) labels, or NULL if
		     no label is defined.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno (in this case, the content
		     of mb is not modified).
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  rva_serialize_write_mem_labeled(r, l, mb)
     rva        *r;
     rva_labels *l;
     mem_block  *mb;
{
  mem_descr *m;
  uint1     *p;
  uint4      lr;
  
  diag__enter("rva_serialize_write_mem_labeled", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!r || !mb)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
  
  if (((mb -> size != ZERO_INT4) && mem_block__content_free(mb)) ||
      !(m = io__to_mem_init()))
    diag__fail(lash_errno, -1);
  
  if (rva_serialize_write(r,
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

/**  rva *rva_serialize_read_mem_labeled(mb, l)  :  Reads the block 
                     of memory *mb and outputs the RVA read.

		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the RVA read.
		     If l is NULL, the (eventual) labels will be
		     left out.

		     If successful, this functions returns the
		     newly allocated RVA. In the case of an error, it
		     returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_FILE     : Bad file format.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

rva *rva_serialize_read_mem_labeled(mb, l)
     mem_block   *mb;
     rva_labels **l;
{
  rva       *r;
  mem_descr *md;

  diag__enter("rva_serialize_read_mem_labeled", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!mb)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(md = io__from_mem_init(mb -> beginning, mb -> size)))
    diag__fail(lash_errno, NULL);

  if (!(r = rva_serialize_read((int (*)(uint4, void *, uint1 *))
			       io__from_mem, (void *)md)))
    {
      io__from_mem_end(md);
      diag__fail(lash_errno, NULL);
    }
  
  io__from_mem_end(md);
  diag__return(r);
}

/**  int  rva_serialize_write_file_labeled(r, l, f)  :  Outputs the
                     RVA *r to the file *f.  If this file already
		     exists, its content is destroyed.
		     
		     l is an array of (r -> dim) labels, or NULL if
		     no label is defined.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  rva_serialize_write_file_labeled(r, l, f)
     rva        *r;
     rva_labels *l;
     char       *f;
{
  FILE *file;

  diag__enter("rva_serialize_write_file_labeled", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!r || !f)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!(file = io__to_file_init(f)))
    diag__fail(lash_errno, -1);
 
  if (rva_serialize_write_labeled(r, l,
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

/**  rva *rva_serialize_read_file_labeled(f, l)  :  Reads the file *f
                     and outputs the RVA read.

		     l is a pointer to a newly allocated array of
		     labels.  If *l is set to NULL, no label has
		     been defined in the RVA read.
		     If l is NULL, the (eventual) labels will be
		     left out.

		     If successful, this functions returns the newly
		     allocated RVA. In the case of an error, it
		     returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_FILE     : Bad file format.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

rva *rva_serialize_read_file_labeled(f, l)
     char        *f;
     rva_labels **l;
{
  rva  *r;
  FILE *file;

  diag__enter("rva_serialize_read_file_labeled", NULL);
 
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(file = io__from_file_init(f)))
    diag__fail(lash_errno, NULL);

  if (!(r = rva_serialize_read_labeled((int (*)(uint4, void *,
						 uint1 *))
					io__from_file, (void *)file,
					l)))
    {
      io__from_file_end(file);
      diag__fail(lash_errno, NULL);
    }

  if (io__from_file_end(file))
    diag__fail(lash_errno, NULL);
  
  diag__return(r);
}

/****  End of rva-io.c  ****/
