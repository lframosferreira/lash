/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/** auto-io-print.c  :  I/O operations over finite-state automata. **/
/**                                                                **/
/**        06/11/98  :  Creation. (BB)                             **/
/**        07/29/98  :  Continued. (BB)                            **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        10/24/00  :  Reorganisation. (JMF)                      **/
/**        08/06/01  :  Support for automata on infinite           **/
/**                     words. (SJ)                                **/
/**        07/02/02  :  Reorganization. (BB)                       **/
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
#include "diag.h"
#include "auto.h"
#include "lash-auto.h"
#include "auto-io-print.h"

/****  Prototypes of private functions.                          ****/

static void  print_auto_type(automaton *);
static void  print_auto_properties(automaton *);
static void  print_auto_nbytes(automaton *);
static void  print_auto_i_states(automaton *);
static void  print_auto_states(automaton *);
static void  print_auto_state(automaton *, uint4);
static void  print_transition(tran *, uint1);
static void  print_bytes(uint1 *, uint4);

/****  Private functions.                                        ****/

/**  void  print_auto_type(a)  :  Prints the type of the      
                     automaton *a.                                 **/

static void  print_auto_type(a)
  automaton *a;
{
  printf("automaton on ");

  switch(auto_word_type(a))
    {
    case AUTO_WORDS_FINITE : 
      printf("finite words,\n");
      break;
    case AUTO_WORDS_INFINITE : 
      printf("infinite words, ");
      switch(auto_accept_type(a))
	{
	case AUTO_ACCEPT_BUCHI :
	  printf("Buchi");
	  break;
	case AUTO_ACCEPT_COBUCHI :
	  printf("co-Buchi");
	  break;
	case AUTO_ACCEPT_WEAK :
	  printf("weak");
	  break;
	default :
	  printf("unknown");
	}
      printf(" acceptance condition,\n");
      break;
    default :
      printf("unknown type of words,\n");
    }
}

/**  void  print_auto_properties(a)  :  Prints the known properties 
                     of the automaton *a.                          **/

static void  print_auto_properties(a)
  automaton *a;
{
  register uint1  v;

  v = auto_known_properties(a);

  if (v != AUTO_PROP_NOTHING)
    {
      printf("known to be");

      if (v & AUTO_PROP_DETERM)
	printf(" deterministic,");

      if (v & AUTO_PROP_MINIMAL)
	printf(" minimal,");

      if (v & AUTO_PROP_STRONG)
	printf(" strongly");

      if (v & AUTO_PROP_NORMAL)
	printf(" normal,");

      if (v & AUTO_PROP_WEAK_NORMAL)
	printf(" weak normal,");

      if (!(v & (AUTO_PROP_DETERM | AUTO_PROP_MINIMAL | 
          AUTO_PROP_STRONG | AUTO_PROP_NORMAL |
	  AUTO_PROP_WEAK_NORMAL)))
	printf(" ???,");

      printf("\n");
    } 
}

/**  void  print_auto_nbytes(a)  :  Prints the numbers of bytes
                     required to store the symbols and the
                     accepting conditions of the automaton *a.     **/

static void  print_auto_nbytes(a)
  automaton *a;
{
  printf(FMT_UINT1, auto_alphabet_nbytes(a));
  printf(" byte(s)/symbol, ");
  printf(FMT_UINT1, auto_accept_nbytes(a));
  printf(" byte(s)/accepting status,\n");
}

/**  void  print_auto_i_states(a)  :  Prints the list of initial
                     states of the automaton *a.                   **/

static void  print_auto_i_states(a)
  automaton *a;
{
  register uint4  i, n;
           uint4  m;

  n = auto_nb_i_states(a);

  printf(FMT_UINT4, n);
  printf(" initial state(s)");

  if (n)
    {
      for (i = 0; i < n; i++)
	{ 
          printf(i ? ", " : ": ");
	  auto_i_state(a, i, &m);
          printf(FMT_UINT4, m);
	}
    }
  printf(".\n");
}

/**  void  print_auto_states(a)  :  Prints the list of states of the
                     automaton *a, as well as the outgoing
                     transitions from these states.                **/

static void  print_auto_states(a)
  automaton *a;
{
  register uint4  i, n;

  n = auto_nb_states(a);

  printf(FMT_UINT4, n);
  printf(" state(s)");

  if (n)
    {
      printf(":\n");
      for (i = 0; i < n; i++)
	print_auto_state(a, i);
    }
  else
    printf(".\n");
}

/**  void  print_auto_state(a, n)  :  Prints the state number n of
                     the automaton *a, as well as the outgoing
                     transitions from this state.                  **/

static void  print_auto_state(a, n)
  automaton *a;
  uint4      n;
{
  register uint4  i;
           uint4  m;

  printf("  state #");
  printf(FMT_UINT4, n);
  printf(":\n");
  printf("    accepting info: [");
  print_bytes(auto_accepting_info_ptr(a, n), auto_accept_nbytes(a));
  printf("].\n    ");

  if (auto_nb_out_transitions(a, n, &m) < 0)
    printf("  invalid outgoing transition(s)\n");
  else
    {
      printf(FMT_UINT4, m);
      printf(" outgoing transition(s)");

      if (m)
	{
	  printf(":\n");
	  for (i = 0; i < m; i++)
	    print_transition(auto_transition(a, n, i), 
			     auto_alphabet_nbytes(a));
	}
      else
	printf(".\n");
    }
}

/**  void  print_transition(t, n)  :  Prints the number of the
                     destination state as well as the label of
                     the transition *t. The number of bytes needed
                     for representing one symbol of the alphabet
                     is given by n.                                **/

static void  print_transition(t, n)
  tran  *t;
  uint1  n;
{
  register uint4  l, i;

  printf("      --> #");
  printf(FMT_UINT4, auto_transition_dest(t)); 
  printf(" [");
  
  l = auto_transition_length(t);

  for (i = 0; i < l; i++)
    {
      printf("<");
      print_bytes(auto_transition_label_ptr(t, n) + i * n, n); 
      printf(">");
    }

  printf("].\n");
}

/**  void  print_bytes(p, n)  :  Prints in hexadecimal n bytes
                     of information, starting at the address
                     given by p.                                   **/

static void  print_bytes(p, n)
  uint1 *p;
  uint4  n;
{
  register uint4 i;

  for (i = 0; i < n; i++)
    {
      if (i)
        printf(" ");
      printf("%.2x", p[i]);
    }
}

/****  Public visible function.                                  ****/

/**  int  auto_print(a)  :  Prints a description of the
                     finite-state automaton *a on stdout.          

                     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  auto_print(a)
  automaton *a;
{
  diag__enter("auto_print", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  print_auto_type(a);
  print_auto_properties(a);
  print_auto_nbytes(a);
  print_auto_i_states(a);
  print_auto_states(a);

  diag__return(0);
}

/****  End of auto-io-print.c  ****/
