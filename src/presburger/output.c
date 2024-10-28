/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   output.c  :  Presburger expressions output.                  **/
/**                                                                **/
/**     12/14/00  :  Creation. (LL)                                **/
/**     08/29/02  :  Reorganization. (BB)                          **/
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
#include "lash.h"
#include "lash-types.h"
#include "resource.h"
#include "datastruct.h"
#include "presburger.h"
#include "expression.h"
#include "output.h"
#include "auto-io-print.h"
#include "lash-ndd-io.h"

/****  Prototypes of private functions.                          ****/

static int    generate_output(exp_gen_condition *, char *);
static int    output_condition(FILE *, exp_condition *); 
static int    output_gen_condition(FILE *, exp_gen_condition *);

/****  Private functions.                                        ****/

/**  int  generate_output(gc, fn)  :  Outputs the condition pointed
                    to by gc to the file named *fn. This function
                    returns -1 and displays a message in the case of
                    an error, and returns 0 in the case of success.
                                                                   **/
static int  generate_output(gc, fn)
  exp_gen_condition *gc;
  char        *fn;
{
  FILE *f;
  
  f = fopen(fn, "w");
  if (!f)
    {
      report_presb_error(
        "Compiler error: Unable to create output file");
      return -1;
    }

  if (output_gen_condition(f, gc) < 0)
    {
      report_presb_error(
        "Compiler error: Write error in generating output file");
      fclose(f);
      return -1;
    }

  fclose(f);
  return 0;
}
  
/**  int  output_condition(f, p)  :  Outputs the parameters of the
                    condition instruction *p into the file *f.
                    Returns -1 in case of a write error, and 0
                    otherwise.                                     **/

static int  output_condition(f, p)
  FILE          *f;
  exp_condition *p;
{
  register uint4     i;
  register exp_atom_term *pt;

  for (i = ZERO_INT4, pt = p -> el; i < p -> nb_el; i++, pt++)
    {
      if (i)
	{
	  if (pt -> v < ZERO_INT4)
	    {
	      if (fprintf(f, " -") < 0)
		return -1;
	      if (pt -> v != -1 && fprintf(f, " %u", -(pt -> v)) < 0)
		return -1;
	    }
	  else
	    {
	      if (fprintf(f, " +") < 0)
		return -1;
	      if (pt -> v != 1 && fprintf(f, " %u", pt -> v) < 0)
		return -1;
	    }
	}
      else
	if (pt -> v < ZERO_INT4)
	  {
	    if (fprintf(f, "-") < 0)
	      return -1;
	    if (pt -> v != -1 && fprintf(f, "%u", -(pt -> v)) < 0)
	      return -1; 
	  }
	else
	  if (pt -> v != 1 && fprintf(f, "%u", pt -> v) < 0)
	    return -1;

      if (fprintf(f, (!i && (pt -> v == -1 || pt -> v == 1)) ?
          "x%u" : " x%u", pt -> no_var) < 0)
	return -1;
    }

  if (p -> cond_type == eq)
    {
      if (fprintf(f, " ==") < 0)
	return -1;
    }
  else if (p -> cond_type == cmp) 
    {
      if (fprintf(f, " <=") < 0)
	return -1;
    }
  else 
    {
      if (fprintf(f, " ?=") < 0)
	return -1;
    }

  if (fprintf(f, " %d.", p -> b) < 0) 
    return -1;

  return 0;
}

/**  int  output_gen_condition(f, p)  :  Outputs the parameters of the
                    condition instruction *p into the file *f.
                    Returns -1 in case of a write error, and 0
                    otherwise.                                     **/

static int  output_gen_condition(f, p)
     FILE          *f;
     exp_gen_condition *p;
{
  switch (p -> type) 
    {
    case EXP_COND_ATOM:
      return output_condition(f,  p -> u.atom.cond);
      break;
  
    case  EXP_COND_EXISTS:
    
      fprintf(f, "( EXISTS(x%d : ", p -> u.exists.no);
      if (output_gen_condition(f, p -> u.exists.cond) < 0)
	return -1;
      fprintf(f, ")");
      break;
      
    case  EXP_COND_NOT:
      fprintf(f, "( NOT (");
      if (output_gen_condition(f, p -> u.not.cond) < 0)
	return -1;
      fprintf(f, "))");
      break;

    case  EXP_COND_OR:
      if (fprintf(f, "(") < 0)
	return -1;	
      if (output_gen_condition(f, p -> u.or.cond1) < 0)
	return -1;
      if (fprintf(f, ") OR (") < 0)
	return -1;
      if (output_gen_condition(f, p -> u.or.cond2) < 0)
	return -1;
      if (fprintf(f, ")") < 0)
	return -1;	
      break;

    case  EXP_COND_AND:
      if (fprintf(f, "(") < 0)
	return -1;	
      if (output_gen_condition(f, p -> u.and.cond1) < 0)
	return -1;
      if (fprintf(f, ") AND (") < 0)
	return -1;
      if (output_gen_condition(f, p -> u.and.cond2) < 0)
	return -1;
      if (fprintf(f, ")") < 0)
	return -1;	
      break;
  
    default :
      return -1;
    }
   
  return 0;    
}

/****  Public functions.                                         ****/

/**  int  out_expression_string(gc)  :  Optionally outputs the 
                   expression to a file.
                   In the case of an error, this
                   function returns -1 and displays a message. In the
                   case of success, it returns 0.                  **/

int  out_expression_string(gc)
  exp_gen_condition *gc;
{

  if (presb_se_name && (generate_output(gc, presb_se_name) < 0))
    return -1;

  
  return 0;
}

/**  int  out_expression_ndd(nd, variables , nb_var, nb_free) :

                    Optionally outputs the expression to a file. *nd
		    is the NDD corresponding to the Presburger
		    formula, variables are all the variables met in
		    the expression, nb_var is the number of variables
		    and nb_free is the number of free variables.  In
		    the case of an error, this function returns -1 and
		    displays a message. In the case of success, it
		    returns 0.                                     **/

int  out_expression_ndd(nd, variables, nb_vars, nb_free)
  ndd *nd;
  exp_variable *variables;
  uint4 nb_vars, nb_free;
{
  register uint4 i;
  ndd_labels *labels;
  char filename[256];

  if (!(labels = ndd_labels_new(nb_free)))
    {
      report_presb_memory_error();
      return -1;
    }    
  
  for (i = 0 ; (i < nb_vars) ; i++)
    {
      if (variables[i].is_free == 1)
	{
	  if (ndd_labels_set(labels, variables[i].no_free,
			     variables[i].name) < 0)
	    {
	      ndd_labels_free(labels);
	      return -1;
	    }
	  
	}
    }

  if (presb_sndd_name) {
    sprintf(filename,"%s.ndd",presb_sndd_name);
    
    if (ndd_serialize_write_file_labeled(nd, labels, filename) < 0)
      {
	char msg[256];
	
	sprintf(msg, "Can't write ndd in %s", filename);
	report_presb_error(msg);
	return -1;
      }
  }

  ndd_labels_free(labels);

 return 0;
}

/**  int  out_expression_dot(ndd)  :  Optionally outputs the 
                   expression to a file.  In the case of an error,
                   this function returns -1 and displays a message. In
                   the case of success, it returns 0.              **/

int  out_expression_dot(nd)
  ndd *nd;
{
  char filename[256];

  if (presb_sdot_name)
    {
      sprintf(filename,"%s.dot",presb_sdot_name);
      
      if (auto_serialize_write_dot_file(nd -> automaton, 
					filename,
					LASH_EXP_DIGIT) < 0)
	{
	  char msg[256];
	  
	  sprintf(msg, "Can't write automaton in %s", 
		  presb_sdot_name);
	  report_presb_error(msg);
	  return -1;
	}
    }
  
  return 0;
}

/****  End of output.c  ****/
