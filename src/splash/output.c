/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**     output.c  :  Test description of state machines.           **/
/**                                                                **/
/**     06/02/99  :  Creation. (BB)                                **/
/**     06/03/99  :  Continued. (BB)                               **/
/**     07/17/02  :  Reorganization. (BB)                          **/
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
#include "splash.h"
#include "program.h"
#include "output.h"

/****  Prototypes of private functions.                          ****/

static void   display_statistics(pgm_program *);
static uint4  state_id(pgm_process *, uint4);
static int    generate_output(pgm_program *, char *);
static int    output_program(FILE *, pgm_program *);
static int    output_process(FILE *, pgm_process *);
static int    output_state(FILE *, pgm_process *, uint4);
static int    output_transition(FILE *, pgm_transition *);
static int    output_assignment(FILE *, pgm_operation *);
static int    output_condition(FILE *, pgm_condition *);
static int    output_meta(FILE *, pgm_program *, pgm_meta *);
static int    output_var_init(FILE *, pgm_variable *);

/****  Private functions.                                        ****/

/**  void  display_statistics()  :  Displays the compilation 
                    statistics for the program *p. This function does
                    not incur errors.                              **/

static void  display_statistics(p)
  pgm_program *p;
{
  register uint4    i, n;
  register pgm_meta *pm;

  printf("Compilation statistics:\n");
  printf("  number of processes               : %u.\n", p -> nb_proc);
  printf("  number of variables               : %u.\n", p -> nb_vars);

  for (i = n = ZERO_INT4; i < p -> nb_proc; i++)
    n += p -> processes[i].nb_ids;

  printf("  total number of control locations : %u.\n", n);

  for (n = ZERO_INT4, pm = p -> meta; pm; pm = pm -> next)
    if (pm -> trans)
      n++;

  printf("  number of meta-transitions        : %u.\n", n);
}

/**  uint4  state_id(p, n)  :  Returns the identifier of the state
                    of index n of the process *p. This function does
                    not report errors.                             **/

static uint4  state_id(p, n)
  pgm_process *p;
  uint4        n;
{
  while (p -> states[n].fusionable)
    n = p -> states[n].trans[0].dest;

  return p -> states[n].id;
}

/**  int  generate_output(p, fn)  :  Outputs the concurrent program
                    pointed by p to the file named *fn. This function
                    returns -1 and displays a message in the case of
                    an error, and returns 0 in the case of success.
                                                                   **/

static int  generate_output(p, fn)
  pgm_program *p;
  char        *fn;
{
  FILE *f;

  f = fopen(fn, "w");
  if (!f)
    {
      report_splash_error(
        "Compiler error: Unable to create output file");
      return -1;
    }

  if (output_program(f, p) < 0)
    {
      report_splash_error(
        "Compiler error: Write error in generating output file");
      fclose(f);
      return -1;
    }

  fclose(f);
  return 0;
}

/**  int  output_program(f, p)  :  Outputs the program *p into the
                    file *f. Returns -1 in case of a write error,
                    and 0 otherwise.                               **/

static int  output_program(f, p)
  FILE        *f;
  pgm_program *p;
{
  register uint4         i;
  register pgm_meta     *pm;

  if (fprintf(f, "%u variable(s).\n", p -> nb_vars) < 0)
    return -1;

  if (p -> nb_vars)
    {
      if (fprintf(f, "   initial value(s):\n") < 0)
	return -1;
  
      if (output_var_init(f, p -> global_vars) < 0)
	return -1;

      for (i = ZERO_INT4; i < p -> nb_proc; i++)
	if (output_var_init(f, p -> processes[i].local_vars) < 0)
	  return -1;

      if (fprintf(f, "\n") < 0)
	return -1;
    }

  if (fprintf(f, "%u process(es).\n", p -> nb_proc) < 0)
    return -1;

  for (i = ZERO_INT4; i < p -> nb_proc; i++)
    if (fprintf(f, "\nprocess #%u:\n", i) < 0 ||
        output_process(f, p -> processes + i) < 0)
      return -1;

  for (pm = p -> meta; pm; pm = pm -> next)
    if (pm -> trans && output_meta(f, p, pm) < 0)
      return -1;

  if (fprintf(f, "\nend of program.\n") < 0)
    return -1;

  return 0;
}

/**  int  output_process(f, p)  :  Outputs the process *p into the
                    file *f. Returns -1 in case of a write error,
                    and 0 otherwise.                               **/

static int  output_process(f, p)
  FILE        *f;
  pgm_process *p;
{
  register uint4      i;
  register pgm_state *ps;

  if (fprintf(f, "   %u states(s).\n", p -> nb_ids) < 0 ||
      fprintf(f, "   initial state is #%u.\n", state_id(p, 0)) < 0)
    return -1;
  
  for (i = ZERO_INT4, ps = p -> states; i < p -> nb_states; i++, ps++)
    if (ps -> reachable && !(ps -> fusionable) &&
        (output_state(f, p, i) < 0))
      return -1;

  return 0;
}

/**  int  output_state(f, p, n)  :  Outputs the state of index n of
                    the process *p into the file *f. Returns -1 in 
                    case of a write error, and 0 otherwise.        **/

static int  output_state(f, p, n)
  FILE        *f;
  pgm_process *p; 
  uint4        n;
{
  register pgm_state *ps;
  register uint4      i;

  ps = p -> states + n;

  if (fprintf(f, "\n   %sstate #%u:\n", 
          (ps -> atomic ? "atomic " : ""), ps -> id) < 0 ||
      fprintf(f, "      %u outgoing transition(s):\n", ps -> nb_trans)
          < 0)
    return -1;

  for (i = ZERO_INT4; i < ps -> nb_trans; i++)
    if (fprintf(f, "         ---> #%u: ", 
        state_id(p, ps -> trans[i].dest)) < 0 || 
        output_transition(f, ps -> trans + i) < 0)
      return -1;

  return 0;
}

/**  int  output_transition(f, t)  :  Outputs the operation labeling
                    the transition *t into the file *f.  Returns -1 in
                    case of a write error, and 0 otherwise.        **/

static int  output_transition(f, t)
  FILE           *f;
  pgm_transition *t;
{
  switch(t -> type)
    {
    case PGM_TRANS_ASGN:
      if (output_assignment(f, &t -> param.asgn) < 0)
	return -1;
      break;
    case PGM_TRANS_ASSERT:
      if (fprintf(f, "assert ") < 0 ||
	  output_condition(f, &t -> param.cond) < 0)
	return -1;
      break;
    case PGM_TRANS_COND:
      if (output_condition(f, &t -> param.cond) < 0)
	return -1;
      break;
    default:
      if (fprintf(f, "skip") < 0)
	return -1;
    }
  return 0;
}

/**  int  output_assignment(f, p)  :  Outputs the parameters of the
                    assignment instruction *p into the file *f.
                    Returns -1 in case of a write error, and 0
                    otherwise.                                     **/

static int  output_assignment(f, p)
  FILE          *f;
  pgm_operation *p;
{
  register uint4     i;
  register pgm_term *pt;

  if (fprintf(f, "x%u :=", p -> lvalue) < 0)
    return -1;

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
	    if (fprintf(f, " -") < 0)
	      return -1;
	    if (pt -> v != -1 && fprintf(f, "%u", -(pt -> v)) < 0)
	      return -1; 
	  }
	else
	  {
	    if (fprintf(f, " ") < 0)
	      return -1;
	    if (pt -> v != 1 && fprintf(f, "%u", pt -> v) < 0)
	      return -1;
	  }

      if (fprintf(f, (!i && (pt -> v == -1 || pt -> v == 1)) ?
          "x%u" : " x%u", pt -> no) < 0)
	return -1;
    }

  if (p -> nb_el && p -> c)
    {
      if (p -> c < ZERO_INT4)
	{
	  if (fprintf(f, " - %u", -(p -> c)) < 0)
	    return -1;
	}
      else
	{
	  if (fprintf(f, " + %u", p -> c) < 0)
	    return -1;
	}
    }
  else
    if (!(p -> nb_el) && fprintf(f, " %d", p -> c) < 0)
      return -1;

  if (fprintf(f, ".\n") < 0)
    return -1;

  return 0;
}

/**  int  output_condition(f, p)  :  Outputs the parameters of the
                    condition instruction *p into the file *f.
                    Returns -1 in case of a write error, and 0
                    otherwise.                                     **/

static int  output_condition(f, p)
  FILE          *f;
  pgm_condition *p;
{
  register uint4     i;
  register pgm_term *pt;

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
          "x%u" : " x%u", pt -> no) < 0)
	return -1;
    }

  switch(p -> cond_type)
    {
    case PGM_COND_EQU :
      if (fprintf(f, " ==") < 0)
	return -1;
      break;

    case PGM_COND_CMP :
      if (fprintf(f, " <=") < 0)
	return -1;
      break;

    case PGM_COND_INE :
      if (fprintf(f, " !=") < 0)
	return -1;
      break;

    default:
      if (fprintf(f, " ?=") < 0)
	return -1;
    }

  if (fprintf(f, " %d.\n", p -> b) < 0)
    return -1;

  return 0;
}

/**  int  output_meta(f, p, m)  :  Outputs the meta-transition *m of
                    the program *p into the file *f. Returns -1 in
                    case of a write error, and 0 otherwise.        **/

static int  output_meta(f, p, m)
  FILE        *f;
  pgm_program *p;
  pgm_meta    *m;
{
  register pgm_control *pc;
  register pgm_tr_ref  *pt;
  register int          first = 0;
  register uint4        n;

  if (fprintf(f, "\nmeta-transition:\n") < 0 ||
      fprintf(f, "   head: (") < 0)
    return -1;
  
  for (first = 1, pc = m -> head; pc; pc = pc -> next)
    {
      if (!first && fprintf(f, ", ") < 0)
	return -1;

      n = state_id(p -> processes + (pc -> process_no),
          pc -> location_no);

      if (fprintf(f, "#%u.%u", pc -> process_no, n) < 0)
	return -1;

      first = 0;
    }

  if (fprintf(f, ").\n   operations:\n") < 0)
    return -1;

  for (pt = m -> trans; pt; pt = pt -> next)
    if (fprintf(f, "      ") < 0 ||
	output_transition(f, pt -> tr) < 0)
      return -1;

  return 0;
}

/**  int  output_var_init(f, p)  :  Outputs the initial values of the
                    variables belonging to the linked list *p into the
                    file *f. Returns -1 in case of a write error, and
                    0 otherwise.                                   **/

static int  output_var_init(f, p)
  FILE        *f;
  pgm_variable *p;
{
  for (; p; p = p -> next)
    if (fprintf(f, "      x%u = %d.\n", p -> no, p -> init) < 0)
      return -1;

  return 0;
}

/****  Public function.                                          ****/

/**  int  out_init(p)  :  Optionally ouputs the compiled state
                    machines to a file. In the case of an error, this
                    function returns -1 and displays a message. In the
                    case of success, it returns 0.                 **/

int  out_init(p)
  pgm_program *p;
{
  if (splash_verbose)
    display_statistics(p);

  if (splash_sm_name && (generate_output(p, splash_sm_name) < 0))
    return -1;
  
  return 0;
}

/****  End of output.c  ****/
