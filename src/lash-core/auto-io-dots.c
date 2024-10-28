/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**   auto-io-dots.c :  Exports LASH automata in order to read     **/
/**                     them with the Graphviz package (versions   **/
/**                     1.5 and above).                            **/
/**                                                                **/
/**        08/05/00  :  Creation. (SJ)                             **/
/**        09/12/00  :  Improvements. (SJ)                         **/
/**        18/10/00  :  Inclusion in the LASH package. (JMF)       **/
/**        05/15/01  :  Minor correction. (BB)                     **/
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
/**  Graphviz is the Graph Drawing Programs from AT&T Research     **/
/**  and Lucent Bell Labs. You can find informations, sources and  **/
/**  binaries at the following URL :                               **/
/**                                                                **/
/**    http://www.research.att.com/sw/tools/graphviz               **/
/**                                                                **/
/********************************************************************/

#include <stdio.h>
#include <string.h>
#include "lash-auto.h"
#include "lash-auto-io.h"
#include "diag.h"
#include "resource.h"
#include "io-ops.h"
#include "auto-io-dots.h"

/**  Definition of constants.                                      **/

#define STR_MAX_LENGTH  20

/****  Public visible functions.                                 ****/

/**  int  auto_serialize_write_dot_file(a, f, p)  :  Exports to the
                     file named *f the visual description of the
		     automaton *a. p defines the output of the
		     labels.

		     This description can be read and
		     converted to a PostScript file by the "dot" tool
		     included in the Graphviz package.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
		     Possible error codes:

		         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_IO       : IO error.
			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  auto_serialize_write_dot_file(a, f, p)
     char          *f;
     automaton     *a;
     io_dots_param  p;
{
  FILE   *file;
  int    (*out_fn)(uint4, void *, uint1 *);
  uint4  i, j, l, s, nb_out_tran;
  tran  *t;
  char  *label, symbol;
  char   str_style[] = "\t" FMT_UINT4 " [style=filled];\n";
  char   str_term[] = "\t" FMT_UINT4 " [shape=doublecircle];\n";
  char   str_label[]= "\t" FMT_UINT4 " -> " FMT_UINT4 " [label=\"";

  out_fn = (int (*)(uint4, void *, uint1 *)) io__to_file;

  diag__enter("auto_serialize_write_dot_file", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!a || !f)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */
 
  if (!(file = io__to_file_init(f)))
    diag__fail(lash_errno, -1);

  if (io__w_text(out_fn, (void *)file, "digraph G {\n"
		 "\trankdir=LR;\n\tsize=\"8,5\"\n"
		 "\torientation=landscape;\n"))
    {
      io__to_file_end(file);
      diag__fail(lash_errno, -1);
    }
  
  for (i = ZERO_INT4 ; i < auto_nb_i_states(a) ; i++)
    {
      char  *str;
      uint4  i_state;

      if (auto_i_state(a, i, &i_state))
	{
	  io__to_file_end(file);
	  diag__fail(lash_errno, -1);
	}
      
      if (!(str = resr__new_objects(char,
				    strlen(str_style) +
				    STR_MAX_LENGTH)))
	{
	  io__to_file_end(file);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      
      sprintf(str, str_style, i_state);
      if (io__w_text(out_fn, (void *)file, str))
	{
	  resr__free_objects(str, char, strlen(str_style) +
			     STR_MAX_LENGTH);
	  io__to_file_end(file);
	  diag__fail(LASH_ERR_NO_MEM, -1);
	}
      
      resr__free_objects(str, char, strlen(str_style) +
			 STR_MAX_LENGTH);
    }
  
  for (i = ZERO_INT4 ; i < auto_nb_states(a) ; i++)
    {
    
      if (auto_state(a, i, &s))
	{
	  io__to_file_end(file);
	  diag__fail(lash_errno, -1);
	}

      if (auto_accepting_state(a, s))
	{
	  char *str;
	  
	  if (!(str = resr__new_objects(char,
					strlen(str_term) +
					STR_MAX_LENGTH)))
	    {
	      io__to_file_end(file);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  sprintf(str, str_term, s);
	  if (io__w_text(out_fn, (void *)file, str))
	    {
	      resr__free_objects(str, char, strlen(str_term) +
				 STR_MAX_LENGTH);
	      io__to_file_end(file);
	      diag__fail(LASH_ERR_NO_MEM, -1);
	    }
	  
	  resr__free_objects(str, char, strlen(str_term) +
			     STR_MAX_LENGTH);
	}
          
      if (auto_nb_out_transitions(a, s, &nb_out_tran))
	{
	  io__to_file_end(file);
	  diag__fail(lash_errno, -1);
	}
        
      for (j = ZERO_INT4 ; j < nb_out_tran; j++)
	{
	  if (!(t = auto_transition(a, s, j)))
	    {
	      io__to_file_end(file);
	      diag__fail(lash_errno, -1);
	    }
 
	  {
	    char  *str;
	    uint4  dest = auto_transition_dest(t);
	    
	    if (!(str =
		  resr__new_objects(char,
				    strlen(str_label) +
				    2 * STR_MAX_LENGTH)))
	      {
		io__to_file_end(file);
		diag__fail(LASH_ERR_NO_MEM, -1);
	      }
	    
	    sprintf(str, str_label, i, dest);
	    if (io__w_text(out_fn, (void *)file, str))
	      {
		resr__free_objects(str, char, strlen(str_label) +
				   2 * STR_MAX_LENGTH);
		io__to_file_end(file);
		diag__fail(LASH_ERR_NO_MEM, -1);
	      }
	    
	    resr__free_objects(str, char, strlen(str_label) +
			       2 * STR_MAX_LENGTH);
	  }
	  
	  if (auto_transition_length(t))
	    {
	      uint4  k;

	      label =
		auto_transition_label_ptr(t, auto_alphabet_nbytes(a));
	      
	      for (k = ZERO_INT4; k < auto_transition_length(t); k++)
		{
		  if (k != ZERO_INT4 && auto_alphabet_nbytes(a) > 1 &&
		      io__w_char(out_fn, (void *)file, ' '))
		    {
		      io__to_file_end(file);
		      diag__fail(lash_errno, -1);
		    }

		  for (l = ZERO_INT4; l < auto_alphabet_nbytes(a);
		       l++)
		    {
		      if (l > ZERO_INT4 &&
			  auto_alphabet_nbytes(a) > 1 &&
			  io__w_char(out_fn, (void *)file, ','))
			{
			  io__to_file_end(file);
			  diag__fail(lash_errno, -1);
			}
		      
		      symbol = label[l + k * auto_alphabet_nbytes(a)];
		      switch (p)
			{
			case LASH_EXP_ASIS :
			  break;
			case LASH_EXP_ALPHA :
			  symbol += 'a';
			  break;
			case LASH_EXP_DIGIT :
			  symbol += '0';
			  break;
			default:
			  io__to_file_end(file);
			  diag__fail(LASH_ERR_CORRUPT, -1);
			}
		      
		      if (io__w_char(out_fn, (void *)file, symbol))
			{
			  io__to_file_end(file);
			  diag__fail(lash_errno, -1); 
			}
		    }
		}

	      if (io__w_char(out_fn, (void *)file, '\"'))
		{
		  io__to_file_end(file);
		  diag__fail(lash_errno, -1); 
		}
	    } 
	  else /* Transition on the empty word */
	    if (io__w_text(out_fn, (void *)file,
			   "e\",fontname=Symbol"))
	      {
		io__to_file_end(file);
		diag__fail(lash_errno, -1); 
	      }
	  
	  if (io__w_text(out_fn, (void *)file, "];\n"))
	      {
		io__to_file_end(file);
		diag__fail(lash_errno, -1); 
	      }
	}
    }
  
  if (io__w_text(out_fn, (void *)file, "}\n"))
    {
      io__to_file_end(file);
      diag__fail(lash_errno, -1); 
    }
 
  if (io__to_file_end(file))
    diag__fail(lash_errno, -1); 
  
  diag__return(0);
}

/****  End of auto-io-dots.c  ****/
