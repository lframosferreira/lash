/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-io.c  :  I/O operations over finite-state automata.    **/
/**                                                                **/
/**     10/04/00  :  Creation. (JMF)                               **/
/**     10/24/00  :  Reorganisation. (JMF)                         **/
/**     12/02/01  :  Minor correction. (BB)                        **/
/**     08/02/01  :  Support for automata on infinite words. (SJ)  **/
/**     08/10/01  :  Minor correction. (SJ)                        **/
/**     07/02/02  :  Reorganization. (BB)                          **/
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
#include "datastruct-io.h"
#include "lash-io.h"
#include "lash-auto-io.h"
#include "auto.h"
#include "io-ops.h"
#include "diag.h"
#include "resource.h"
#include "auto-io.h"

/****  Private type definitions.                                 ****/

typedef  struct {
  uint1  nbits;
  uint4  nstates;
  uint1  word_type;
  uint1  accept_type;
} header_info;

typedef  struct {
  automaton *a;
  uint4     *states;
  uint1      nbits;
} corr_auto;


/****  Prototypes of private functions.                          ****/

static int   serialize_auto_header(automaton *,
				   int (*)(uint4, void *, uint1 *),
				   void *);
static int   serialize_auto_header_type(automaton *,
					int (*)(uint4, void *,
						uint1 *), void *);
static int   serialize_auto_header_nbits(automaton *,
					 int (*)(uint4, void *,
						 uint1 *),
					 void *);
static int   serialize_auto_header_nstates(automaton *,
					   int (*)(uint4, void *,
						   uint1 *), void *);
static int   serialize_auto_body(automaton *,
				 int (*)(uint4, void *, uint1 *),
				 void *);
static int   serialize_auto_body_i_states(automaton *,
					  int (*)(uint4, void *,
						  uint1 *), void *);
static int   serialize_auto_body_f_states(automaton *,
					  int (*)(uint4, void *,
						  uint1 *),
					  void *);
static int   serialize_auto_body_tran(automaton *,
				      int (*)(uint4, void *, uint1 *),
				      void *);
static int   serialize_auto_info(automaton *,
				 int (*)(uint4, void *, uint1 *),
				 void *);
static int   serialize_auto_info_properties(automaton *,
					    int (*)(uint4, void *,
						    uint1 *), void *);
static int   read_auto_header(corr_auto *,
			      int (*)(uint4, void *, uint1 *),
			      void *);
static int   read_auto_header_wordtype(header_info *,
				       int (*f)(uint4, void *,
						uint1 *), void *);
static int   read_auto_header_accepttype(header_info *,
					 int (*f)(uint4, void *,
						  uint1 *),
					 void *);
static int   read_auto_header_nbits(header_info *,
				    int (*f)(uint4, void *, uint1 *),
				    void *);
static int   read_auto_header_nstates(header_info *,
				      int (*f)(uint4, void *,
					       uint1 *), void *);
static int   read_auto_body(corr_auto *,
			    int (*f)(uint4, void *, uint1 *), void *);
static int   read_auto_body_i_states(corr_auto *,
				     int (*f)(uint4, void *, uint1 *),
				     void *);
static int   read_auto_body_f_states(corr_auto *,
				     int (*f)(uint4, void *, uint1 *),
				     void *);
static int   read_auto_body_tran(corr_auto *,
				 int (*f)(uint4, void *, uint1 *),
				 void *);
static int   read_auto_body_tran_base(corr_auto *,
				      int (*f)(uint4, void *,
					       uint1 *), void *);
static int   read_auto_info(automaton *a,
			    int (*f)(uint4, void *, uint1 *), void *);
static int   read_auto_info_properties(automaton *a,
				       int (*f)(uint4, void *,
						uint1 *), void *);
static int   read_auto_info_prop_det(automaton *a,
				     int (*f)(uint4, void *,
					      uint1 *), void *);
static int   read_auto_info_prop_min(automaton *a,
				     int (*f)(uint4, void *,
					      uint1 *), void *);
static int   read_auto_info_prop_str(automaton *a,
				     int (*f)(uint4, void *,
					      uint1 *), void *);
static int   read_auto_info_prop_nor(automaton *a,
				     int (*f)(uint4, void *,
					      uint1 *), void *);
static int   read_auto_info_prop_weak_nor(automaton *a,
					  int (*f)(uint4, void *,
						   uint1 *), void *);

/****  Private functions.                                        ****/

/**  int  serialize_auto_header(a, f, arg)  :  Outputs the header
                     associated to the automaton *a using the function
		     *f with arg as its second argument (see
		     auto_serialize for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_header(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__w_tag(f, arg, IO_TAG__AUTO_HEADER) ||
      serialize_auto_header_type(a, f, arg) ||
      serialize_auto_header_nbits(a, f, arg) ||
      serialize_auto_header_nstates(a, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_auto_header_type(a, f, arg)  :  Outputs the type
                     of the automaton *a using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_header_type(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint1  tag;

  if (io__w_tag(f, arg, IO_TAG__AUTO_HEADER_WORDTYPE))
    return -1;

  switch(auto_word_type(a))
    {
    case AUTO_WORDS_FINITE : 
      tag = IO_TAGVAL__AUTO_HEADER_WORDTYPE_FINITE;
      if (io__w_tagval(f, arg, 1, &tag))
	return -1;
      break;

    case AUTO_WORDS_INFINITE :
      {
	tag = IO_TAGVAL__AUTO_HEADER_WORDTYPE_INFINITE;
	if (io__w_tagval(f, arg, 1, &tag))
	  return -1;

	switch(auto_accept_type(a))
	  {
	  case AUTO_ACCEPT_BUCHI :
	    tag = IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_BUCHI;
	    break;

	  case AUTO_ACCEPT_COBUCHI :
	    tag = IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_COBUCHI;
	    break;

	  case AUTO_ACCEPT_WEAK :
	    tag = IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_WEAK;
	    break;

	  default :
	    lash_errno = LASH_ERR_CORRUPT;
	    return -1;
	  }
	
	if (io__w_tag(f, arg, IO_TAG__AUTO_HEADER_ACCEPTTYPE) ||
	    io__w_tagval(f, arg, 1, &tag))
	  return -1;
	break;
      }

    default :
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }

  return 0;
}

/**  int  serialize_auto_header_nbits(a, f, arg)  :  Outputs the
                     number of bits required to store one symbol of
		     the automaton *a using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_header_nbits(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__w_tag(f, arg, IO_TAG__AUTO_HEADER_NBITS) ||
      io__w_num(f, arg, auto_alphabet_nbytes(a) * 8))
    return -1;
  
  return 0;
}

/**  int  serialize_auto_header_nstates(a, f, arg)  :  Outputs the
                     number of states of the automaton *a using
		     the function *f with arg as its second
		     argument (see auto_serialize for a complete
		     description of *f).
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_header_nstates(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__w_tag(f, arg, IO_TAG__AUTO_HEADER_NSTATES) ||
      io__w_num(f, arg, auto_nb_states(a)))
    return -1;

  return 0;
}

/**  int  serialize_auto_body(a, f, arg)  :  Outputs the body
                     associated to the automaton *a using the function
		     *f with arg as its second argument (see
		     auto_serialize for a complete description of *f).
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_body(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__w_tag(f, arg, IO_TAG__AUTO_BODY) ||
      serialize_auto_body_i_states(a, f, arg) ||
      serialize_auto_body_f_states(a, f, arg) ||
      serialize_auto_body_tran(a, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;
  
  return 0;
}

/**  int  serialize_auto_body_i_states(a, f, arg)  :  Outputs the
                     initial states of the automaton *a using the
		     function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_BAD_STATE  : No such state.
                         + error codes given by *f.                **/

static int  serialize_auto_body_i_states(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  n = auto_nb_i_states(a), i;

  if (io__w_tag(f, arg, IO_TAG__AUTO_BODY_I_STATES) ||
      io__w_num(f, arg, n))
    return -1;
  
  for (i = ZERO_INT4; i < n; i++)
    {
      uint4  s;

      if (auto_i_state(a, i, &s) ||
	  io__w_num(f, arg, s))
	return -1;
    }
  
  return 0;
}

/**  int  serialize_auto_body_f_states(a, f, arg)  :  Outputs the
                     final states of the automaton *a using the
		     function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_BAD_STATE  : No such state.
                         + error codes given by *f.                **/

static int  serialize_auto_body_f_states(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  n = ZERO_INT4, m, i, s;

  for (m = auto_nb_states(a), i = ZERO_INT4; i < m; i++)
    {
      uint4  s;

      if (auto_state(a, i, &s))
	return -1;

      if (auto_accepting_state(a, s))
	n++;
    }

  if (io__w_tag(f, arg, IO_TAG__AUTO_BODY_F_STATES) ||
      io__w_num(f, arg, n))
    return -1;

  for (i = ZERO_INT4; i < m; i++)
    if (auto_state(a, i, &s) ||
	(auto_accepting_state(a, s) && io__w_num(f, arg, s)))
      return -1;
  
  return 0;
}

/**  int  serialize_auto_body_tran(a, f, arg)  :  Outputs the
                     transition relation of the automaton *a using
		     the function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_body_tran(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  i, n = auto_nb_states(a);

  if (io__w_tag(f, arg, IO_TAG__AUTO_BODY_TRAN) ||
      io__w_tagval1(f, arg,
		    IO_TAGVAL__AUTO_BODY_TRAN_ENC_TYPE_BASE))
    return -1;

  for (i = ZERO_INT4; i < n; i++)
    {
      uint4  s, nb_tran;

      if (auto_state(a, i, &s) ||
	  auto_nb_out_transitions(a, s, &nb_tran))
	return -1;

      if (nb_tran)
	{
	  uint4  j;
	  
	  if (io__w_num(f, arg, s) ||
	      io__w_num(f, arg, nb_tran))
	    return -1;
	  
	  for (j = ZERO_INT4; j < nb_tran; j++)
	    {
	      tran  *t = auto_transition(a, i, j);
	      uint4  l;
	      uint1  nb = auto_alphabet_nbytes(a);

	      if (!t ||
		  io__w_num(f, arg, auto_transition_dest(t)) ||
		  io__w_num(f, arg, l = auto_transition_length(t)) ||
		  io__w_bytes(f, arg, l * nb,
			      auto_transition_label_ptr(t, nb)))
		return -1;
	    }
	}
    }

  if (io__w_num(f, arg, ZERO_INT4) ||
      io__w_num(f, arg, ZERO_INT4))
    return -1;

  return 0;
}

/**  int  serialize_auto_info(a, f, arg)  :  Outputs the infos
                     associated to the automaton *a using the function
		     *f with arg as its second argument (see
		     auto_serialize for a complete description of *f).
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_info(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__w_tag(f, arg, IO_TAG__AUTO_INFO) ||
      serialize_auto_info_properties(a, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    return -1;

  return 0;
}

/**  int  serialize_auto_info_properties(a, f, arg)  :  Outputs the
                     known properties of the automaton *a using the
		     function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_CORRUPT : Corrupt automaton.
                         + error codes given by *f.                **/

static int  serialize_auto_info_properties(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (auto_known_properties(a) != AUTO_PROP_NOTHING)
    {
      if (io__w_tag(f, arg, IO_TAG__AUTO_INFO_PROPERTIES))
	return -1;

      if (auto_test_property(a, AUTO_PROP_DETERM) &&
	  io__w_tag(f, arg,
		    IO_TAG__AUTO_INFO_PROPERTIES_DETERM))
	  return -1;

      if (auto_test_property(a, AUTO_PROP_MINIMAL) &&
	  io__w_tag(f, arg,
		    IO_TAG__AUTO_INFO_PROPERTIES_MINIMAL))
	  return -1;

      if (auto_test_property(a, AUTO_PROP_STRONG) &&
	  io__w_tag(f, arg,
		    IO_TAG__AUTO_INFO_PROPERTIES_STRONG))
	return -1;
      
      if (auto_test_property(a, AUTO_PROP_NORMAL) &&
	  io__w_tag(f, arg,
		    IO_TAG__AUTO_INFO_PROPERTIES_NORMAL))
	return -1;
      
      if (auto_test_property(a, AUTO_PROP_WEAK_NORMAL) &&
	  io__w_tag(f, arg,
		    IO_TAG__AUTO_INFO_PROPERTIES_WEAK_NORMAL))
	return -1;
      
      if (!(auto_test_property(a, AUTO_PROP_DETERM |
			       AUTO_PROP_MINIMAL |
			       AUTO_PROP_STRONG |
			       AUTO_PROP_NORMAL |
			       AUTO_PROP_WEAK_NORMAL)))
	{
	  lash_errno = LASH_ERR_CORRUPT;
	  return -1;
	}
    
      if (io__w_tag(f, arg, IO_TAG__END))
	return -1;
    }

  return 0;
}

/**  int  read_auto_header(pa, f, arg)  :  Reads the header section
                     of the serial encoding of an automaton using
		     the function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     Allocate the new automaton **pa if no error
		     occurs.

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT : Corrupt automaton.     **/

static int  read_auto_header(ca, f, arg)
     corr_auto  *ca;
     int       (*f)(uint4, void *, uint1 *);
     void       *arg;
{
  header_info  hi;
  uint4        i;

  if (io__r_set(&hi, f, arg, 4,
		IO_TAG__AUTO_HEADER_WORDTYPE,
		read_auto_header_wordtype,
		IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE,
		IO_TAG__AUTO_HEADER_ACCEPTTYPE,
		read_auto_header_accepttype,
		IO_SET_PROP_UNIQUE,
		IO_TAG__AUTO_HEADER_NBITS,
		read_auto_header_nbits,
		IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE,
		IO_TAG__AUTO_HEADER_NSTATES,
		read_auto_header_nstates,
		IO_SET_PROP_MANDATORY | IO_SET_PROP_UNIQUE))
    return -1;

  if (!(ca -> a = auto_new_empty((hi.nbits + 7) / 8)))
    return -1;

  if (!(ca -> states = resr__new_objects(uint4, hi.nstates)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }

  if (hi.word_type == IO_TAGVAL__AUTO_HEADER_WORDTYPE_INFINITE)
    {
      auto_word_type(ca -> a) = AUTO_WORDS_INFINITE;
      switch (hi.accept_type)
	{
	case IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_BUCHI :
	  auto_accept_type(ca -> a) = AUTO_ACCEPT_BUCHI;
	  break;

	case IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_COBUCHI :
	  auto_accept_type(ca -> a) = AUTO_ACCEPT_COBUCHI;
	  break;

	case IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_WEAK :
	  auto_accept_type(ca -> a) = AUTO_ACCEPT_WEAK;
	  break;

	default :
	  lash_errno = LASH_ERR_CORRUPT;
	  return -1;
	}
    }

  for (i = ZERO_INT4; i < hi.nstates; i++)
    if (auto_add_new_state(ca -> a, ca -> states + i))
      return -1;

  return 0;
}

/**  int  read_auto_header_wordtype(hi, f, arg)  :  Reads the
                     type of words read by an automaton described by
		     its serial encoding using the function *f with
		     arg as its second argument (see auto_serialize
		     for a complete description of *f).

		     The informations read are put in the structure
		     *hi.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         LASH_ERR_BAD_TYPE  :  Bad type of object.
                         Error codes given by *f.                  **/

static int  read_auto_header_wordtype(hi, f, arg)
     header_info *hi;
     int        (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  uint1  wt;

  if (io__r_bytes(f, arg, 1, &wt))
    return -1;

  if (wt != IO_TAGVAL__AUTO_HEADER_WORDTYPE_FINITE &&
      wt != IO_TAGVAL__AUTO_HEADER_WORDTYPE_INFINITE)
    {
      lash_errno = LASH_ERR_BAD_TYPE;
      return -1;
    }

  hi -> word_type = wt;
  return 0;
}

/**  int  read_auto_header_accepttype(hi, f, arg)  :  Reads the
                     type of accepting conditions of an automaton
		     described by its serial encoding using the
		     function *f with arg as its second argument (see
		     auto_serialize for a complete description of *f).

		     The informations read are put in the structure
		     *hi.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                        LASH_ERR_BAD_TYPE  :  Bad type of object.
                        Error codes given by *f.                  **/

static int  read_auto_header_accepttype(hi, f, arg)
     header_info *hi;
     int        (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  uint1  at;

  if (hi -> word_type == IO_TAGVAL__AUTO_HEADER_WORDTYPE_INFINITE)
    {
      if (io__r_bytes(f, arg, 1, &at))
	return -1;

      if (at != IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_BUCHI &&
	  at != IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_COBUCHI &&
	  at != IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_WEAK)
	{
	  lash_errno = LASH_ERR_BAD_TYPE;
	  return -1;
	}

      hi -> accept_type = at;
      return 0;
    }

  else
    return -1;
}

/**  int  read_auto_header_nbits(hi, f, arg)  :  Reads the
                     number of bits required to store one symbol
		     of an automaton described by its serial encoding
		     using the function *f with arg as its second
		     argument (see auto_serialize for a complete
		     description of *f).

		     The informations read are put in the structure
		     *hi.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         Error codes given by *f.                  **/

static int  read_auto_header_nbits(hi, f, arg)
     header_info *hi;
     int        (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  uint4  n;

  if (io__r_num(f, arg, &n))
    return -1;

  hi -> nbits = n;
  return 0;
}

/**  int  read_auto_header_nstates(hi, f, arg)  :  Reads the
                     number of states of an automaton described
		     by its serial encoding using the function *f
		     with arg as its second argument (see
		     auto_serialize for a complete description of *f).

		     The informations read are put in the structure
		     *hi.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         Error codes given by *f.                  **/

static int  read_auto_header_nstates(hi, f, arg)
     header_info *hi;
     int         (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  uint4  n;

  if (io__r_num(f, arg, &n))
    return -1;

  hi -> nstates = n;
  return 0;
}

/**  int  read_auto_body(ca, f, arg)  :  Reads the body section
                     of the serial encoding of an automaton using
		     the function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

static int  read_auto_body(ca, f, arg)
     corr_auto *ca;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__r_set(ca, f, arg, 3,
		IO_TAG__AUTO_BODY_I_STATES,
		read_auto_body_i_states,
		IO_SET_PROP_UNIQUE,
		IO_TAG__AUTO_BODY_F_STATES,
		read_auto_body_f_states,
		IO_SET_PROP_UNIQUE,
		IO_TAG__AUTO_BODY_TRAN,
		read_auto_body_tran,
		IO_SET_PROP_UNIQUE))
    return -1;

  return 0;
}

/**  int  read_auto_body_i_states(ca, f, arg)  :  Reads the info.
                     related to the initial states of the serial
		     encoding of of the serial encoding of an
		     automaton using the function *f with arg as its
		     second argument (see auto_serialize for a
		     complete description of *f).

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

static int  read_auto_body_i_states(ca, f, arg)
     corr_auto   *ca;
     int        (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  uint4  n, i;

  if (io__r_num(f, arg, &n))
    return -1;
  
  for (i = ZERO_INT4; i < n; i++)
    {
      uint4  s;
      
      if (io__r_num(f, arg, &s) || 
	  auto_add_new_i_state(ca -> a, ca -> states[s]))
	return -1;
    }
  
  return 0;
}

/**  int  read_auto_body_f_states(ca, f, arg)  :  Reads the infos
                     related to the final states of the serial
		     encoding of of the serial encoding of an
		     automaton using the function *f with arg as its
		     second argument (see auto_serialize for a
		     complete description of *f).

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
                         LASH_ERR_BAD_STATE  : No such state.
                         LASH_ERR_NOT_INIT   : Not initialized.    **/

static int  read_auto_body_f_states(ca, f, arg)
     corr_auto *ca;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  n, i, s;
  
  if (io__r_num(f, arg, &n))
    return -1;
  
  for (i = ZERO_INT4; i < n; i++)
    {
      if (io__r_num(f, arg, &s))
	return -1;
      
      auto_mark_accepting_state(ca -> a, ca -> states[s]);
    }
  
  return 0;
}

/**  int  read_auto_body_tran(ca, f, arg)  :  Reads the transition
                     relation of an automaton given by its serial     
		     encoding using the function *f with arg as
		     its second argument (see auto_serialize for a
		     complete description of *f).

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

static int  read_auto_body_tran(ca, f, arg)
     corr_auto *ca;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint1  type;
 
  if (io__r_bytes(f, arg, 1, &type))
    return -1;

  switch (type)
    {
    case IO_TAGVAL__AUTO_BODY_TRAN_ENC_TYPE_BASE:
      if (read_auto_body_tran_base(ca, f, arg))
	return -1;
      break;

    default:
      lash_errno = LASH_ERR_FILE;
      return -1;
    }

  return 0;
}

/**  int  read_auto_body_tran_base(ca, f, arg)  :  Reads the
                     transition relation of an automaton given by its
		     serial encoding using the function *f with arg
		     as its second argument (see auto_serialize for
		     a complete description of *f).  The encoding
		     of the transition relation is flagged with
		     the tag IO_TAGVAL__AUTO_BODY_TRAN_ENC_TYPE_BASE.

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

static int  read_auto_body_tran_base(ca, f, arg)
     corr_auto *ca;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  uint4  n = auto_nb_states(ca -> a);
  uint1  na = auto_alphabet_nbytes(ca -> a);

  for (;;)
    {
      uint4           orig, nb_tran;
      register uint1  i;

      if (io__r_num(f, arg, &orig) ||
	  io__r_num(f, arg, &nb_tran))
	return -1;
      
      if (nb_tran == ZERO_INT4)
	break;
 
      if (orig >= n)
	{
	  lash_errno = LASH_ERR_FILE;
	  return -1;
	}	

      for (orig = ca -> states[orig], i = nb_tran; i; i--)
	{
	  uint4  dest, length;
	  uint1 *label;

	  if (io__r_num(f, arg, &dest) ||
	      io__r_num(f, arg, &length) ||
	      !(label = resr__new_objects(uint1, length * na)) ||
	      io__r_bytes(f, arg, length * na, label) ||
	      auto_add_new_transition(ca -> a, orig, dest,
				      length, label))
	    return -1;

	  resr__free_objects(label, uint1, length * na);
	}

    }

  return 0;
}

/**  int  read_auto_info(a, f, arg)  :  Reads the info section
                     of the serial encoding of an automaton using
		     the function *f with arg as its second argument
		     (see auto_serialize for a complete description
		     of *f).

		     If successful, this functions returns a pointer
		     to the newly allocated automaton.  In the case
		     of an error, it returns NULL and sets
		     lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

static int  read_auto_info(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  if (io__r_set(a, f, arg, 1,
		IO_TAG__AUTO_INFO_PROPERTIES,
		read_auto_info_properties,
		IO_SET_PROP_UNIQUE))
    return -1;

  return 0;
}

/**  int  read_auto_info_properties(a, f, arg)  :  Reads the
                     known properties of an automaton described
		     by its serial encoding using the function *f
		     with arg as its second argument (see
		     auto_serialize for a complete description of *f).

		     The properties are set to the automaton *a.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
	
                     Possible error codes:

                         Error codes given by *f.                  **/

static int  read_auto_info_properties(a, f, arg)
     automaton   *a;
     int        (*f)(uint4, void *, uint1 *);
     void        *arg;
{
  return(io__r_set(a, f, arg, 5,
		   IO_TAG__AUTO_INFO_PROPERTIES_DETERM,
		   read_auto_info_prop_det,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__AUTO_INFO_PROPERTIES_MINIMAL,
		   read_auto_info_prop_min,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__AUTO_INFO_PROPERTIES_STRONG,
		   read_auto_info_prop_str,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__AUTO_INFO_PROPERTIES_NORMAL,
		   read_auto_info_prop_nor,
		   IO_SET_PROP_UNIQUE,
		   IO_TAG__AUTO_INFO_PROPERTIES_WEAK_NORMAL,
		   read_auto_info_prop_weak_nor,
		   IO_SET_PROP_UNIQUE));
}

/**  int  read_auto_info_prop_det(a, f, arg)  :  Sets the
                     'determinist' property of the automaton *a.   **/

static int  read_auto_info_prop_det(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  auto_set_property(a, AUTO_PROP_DETERM);
  return 0;
}

/**  int  read_auto_info_prop_min(hi, f, arg)  :  Sets the
                     'minimal' property of the automaton *a.       **/

static int  read_auto_info_prop_min(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  auto_set_property(a, AUTO_PROP_MINIMAL);
  return 0;
}

/**  int  read_auto_info_prop_str(hi, f, arg)  :  Sets the
                     'strong' property of the automaton *a.        **/

static int  read_auto_info_prop_str(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  auto_set_property(a, AUTO_PROP_STRONG);
  return 0;
}

/**  int  read_auto_info_prop_nor(hi, f, arg)  :  Sets the
                     'normal' property of the automaton *a.        **/

static int  read_auto_info_prop_nor(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  auto_set_property(a, AUTO_PROP_NORMAL);

  return 0;
}

/**  int  read_auto_info_prop_weak_nor(hi, f, arg)  :  Sets the
                     'weak_normal' property of the automaton *a.   **/

static int  read_auto_info_prop_weak_nor(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  auto_set_property(a, AUTO_PROP_WEAK_NORMAL);

  return 0;
}


/****  Public visible function.                                  ****/

/**  int  auto_serialize_write(a, f, arg)  :  Outputs the automaton *a
                     using the function *f.

		     *f takes 3 arguments. The first is the length
		     (in bytes) of the data to output; the second is
		     arg; the third is a pointer to the data to
		     output.
	
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.
                         + error codes reported by *f.             **/

int  auto_serialize_write(a, f, arg)
     automaton *a;
     int      (*f)(uint4, void *, uint1 *);
     void      *arg;
{
  diag__enter("auto_serialize_write", -1);
 
#if LASH_CHECK_LEVEL >= 1
  if (!a || !f)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (io__w_tag(f, arg, IO_TAG__AUTOMATON) ||
      serialize_auto_header(a, f, arg) ||
      serialize_auto_body(a, f, arg) ||
      serialize_auto_info(a, f, arg) ||
      io__w_tag(f, arg, IO_TAG__END))
    diag__fail(lash_errno, -1);
  
  diag__return(0);
}

/**  automaton *auto_serialize_read(f, arg)  :  Reads the automaton
                     written in the stream *arg using the function
		     *f.

		     *f takes 3 arguments. The first is the length
		     (in bytes) of the data to read; the second is
		     arg; the third is where to put the read data.

                     If successful, this functions returns the
		     newly allocated automaton.  In the case of an
		     error, it returns NULL and sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.      **/

automaton *auto_serialize_read(f, arg)
     int (*f)(uint4, void *, uint1 *);
     void *arg;
{
  corr_auto  ca;
  uint1      tag;
  
  diag__enter("auto_serialize_read", NULL);
 
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  ca.a = NULL;
  ca.states = NULL;

  if (io__r_tag(f, arg, &tag) || (tag != IO_TAG__AUTOMATON) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__AUTO_HEADER) ||
      read_auto_header(&ca, f, arg) ||
      io__r_tag(f, arg, &tag) || (tag != IO_TAG__AUTO_BODY) ||
      read_auto_body(&ca, f, arg) ||
      io__r_tag(f, arg, &tag) ||
      (tag != IO_TAG__AUTO_INFO && tag != IO_TAG__END))
    {
      if (ca.states)
	resr__free_objects(ca.states, uint4, auto_nb_states(ca.a));

      if (ca.a)
	auto_free(ca.a);

      diag__fail(lash_errno, NULL);
    }

  if (ca.states)
    resr__free_objects(ca.states, uint4, auto_nb_states(ca.a)); 
  
  if ((tag == IO_TAG__AUTO_INFO) &&
      (read_auto_info(ca.a, f, arg) ||
       io__r_tag(f, arg, &tag) || (tag != IO_TAG__END)))
    {
      auto_free(ca.a);
      diag__fail(lash_errno, NULL);
    }

  io__r_tag(f, arg, &tag);
  diag__return(ca.a);
}

/**  int  auto_serialize_write_mem(a, mb)  :  Outputs the automaton
                     *a in the block of memory *mb.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno (in this case, the content
		     of mb is not modified).
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  auto_serialize_write_mem(a, mb)
     automaton *a;
     mem_block *mb;
{
  mem_descr *m;
  uint1     *p;
  uint4      l;

  diag__enter("auto_serialize_write_mem", -1);
 
#if LASH_CHECK_LEVEL >= 1
  if (!a || !mb)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (((mb -> size != ZERO_INT4) && mem_block__content_free(mb)) ||
      !(m = io__to_mem_init()))
    diag__fail(lash_errno, -1);
  
  if (auto_serialize_write(a,
			   (int (*)(uint4, void *, uint1 *))
			   io__to_mem,
			   (void *)m))
    {
      io__to_mem_end(m, &p, &l);
      resr__free(p, l);
      diag__fail(lash_errno, -1);
    }
  
  io__to_mem_end(m, &p, &l);
  
  if (mem_block__content_set(mb, p, l))
    diag__fail(lash_errno, -1);
  
  resr__free(p, l);
  
  diag__return(0);
}

/**  automaton *auto_serialize_read_mem(mb)  :  Reads the block
                     of memory *mb and outputs the automaton read
		     in this block.

		     If successful, this functions returns the
		     newly allocated automaton. In the case of an
		     error, it returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_FILE     : Bad file format.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

automaton *auto_serialize_read_mem(mb)
     mem_block *mb;
{
  automaton *a;
  mem_descr *md;

  diag__enter("auto_serialize_read_mem", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!mb)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(md = io__from_mem_init(mb -> beginning, mb -> size)))
    diag__fail(lash_errno, NULL);

  if (!(a = auto_serialize_read((int (*)(uint4, void *, uint1 *))
				io__from_mem, (void *)md)))
    {
      io__from_mem_end(md);
      diag__fail(lash_errno, NULL);
    }
  
  io__from_mem_end(md);
  diag__return(a);
}

/**  int  auto_serialize_write_file(a, f)  :  Outputs the automaton
                     *a to the file *f.  If this file already exists,
		     its content is destroyed.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

int  auto_serialize_write_file(a, f)
     automaton *a;
     char      *f;
{
  FILE *file;

  diag__enter("auto_serialize_write_file", -1);
 
#if LASH_CHECK_LEVEL >= 1
  if (!a || !f)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (!(file = io__to_file_init(f)))
    diag__fail(lash_errno, -1);

  if (auto_serialize_write(a, (int (*)(uint4, void *, uint1 *))
			   io__to_file, (void *)file))
    {
      io__to_file_end(file);
      diag__fail(lash_errno, -1);
    }
  
  if (io__to_file_end(file))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  automaton *auto_serialize_read_file(f)  :  Reads the file *f
                     and outputs the automaton read.

		     If successful, this functions returns the
		     newly allocated automaton. In the case of an
		     error, it returns NULL and sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_FILE     : Bad file format.
			 LASH_ERR_IO       : IO error.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

automaton *auto_serialize_read_file(f)
     char *f;
{
  automaton *a;
  FILE      *file;

  diag__enter("auto_serialize_read_file", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!f)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  if (!(file = io__from_file_init(f)))
    diag__fail(lash_errno, NULL);
  
  if (!(a = auto_serialize_read((int (*)(uint4, void *, uint1 *))
				io__from_file, (void *)file)))
    {
      io__from_file_end(file);
      diag__fail(lash_errno, NULL);
    }
  
  if (io__from_file_end(file))
    diag__fail(lash_errno, NULL);
  
  diag__return(a);
}

/****  End of auto-io.c  ****/
