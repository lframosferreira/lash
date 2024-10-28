/********************************************************************/
/**                                                                **/
/**  Simple IF LASH (Siflash) compiler -- v0.9                     **/
/**  =================================                             **/
/**                                                                **/
/**    program.c  :  Generation of concurrent programs.            **/
/**                                                                **/
/**     11/16/99  :  Creation. (LL)                                **/
/**     11/25/99  :  Added variable type. (LL)                     **/
/**     12/01/99  :  Added the functions for conditions. (LL)      **/
/**     04/03/01  :  Cleaning. (LL)                                **/
/**     07/06/01  :  Minor corrections. (LL)                       **/
/**     02/06/02  :  Corrections. (LL)                             **/
/**     04/16/02  :  Corrections. (LL)                             **/
/**     04/29/02  :  Modifications. (LL)                           **/
/**     05/13/02  :  Correction. (LL)                              **/
/**     05/31/02  :  Correction.  (LL)                             **/
/**     06/21/02  :  Minor corrections.  (LL)                      **/
/**     07/02/02  :  Modifications for synchronisation. (LL)       **/
/**     07/25/02  :  Modification for IF2.0. (LL)                  **/
/**     09/05/02  :  Corrections. (LL)                             **/
/**     09/06/02  :  Reorganization. (BB)                          **/
/**     09/13/02  :  Reorganization. (LL)                          **/
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
#include "siflash.h"
#include "program.h"
#include "output.h"
#include "arithmetic.h"
#include "explore.h" 

/****  Definitions.                                              ****/

#define  PGM_GROWTH      0x10  /*  Growth quota for arrays.         */
#define  PGM_HASH_SIZE  0x400  /*  Size of identifier hash tables.  */

/****  Global variables.                                         ****/

static uint4          current_process;
static uint4          next_state ;
static int            meta_local;
pgm_program           the_program;
static hash_table    *local_vars, *global_vars, *id_table, 
                     *local_const, *global_const, *local_types, 
                     *global_types;
static uint4_set     *parameter_set;
static pgm_fpar      *fpar;
static pgm_control  **current_control_p;
static uint8          nb_ins, nb_colls;
static pgm_gen_condition  *pre_cond = NULL;
static pgm_gen_operation  *asgn = NULL;   
static char          *label_trans = NULL;

/****  Prototypes of private functions.                          ****/

static void  free_uint1(uint1 *);
static void  free_constant(pgm_constant *);
static void *grow_table(void **, uint4 *, uint4 *, unsigned);
static void  free_the_program(void);
static void  free_process(pgm_process *);
static void  free_variable(pgm_variable *);
static void  free_meta(pgm_meta *);
static void  free_control(pgm_control *);
static void  free_gen_cond_content(pgm_gen_condition *);
static void  free_transition_elt(pgm_transition *tr);
static int   reference_metas(void); 
static int   reference_control(pgm_control *, pgm_meta *);
static int   mark_states(pgm_process *);  
static int   check_fusion_loops(pgm_process *); 
static int   set_init_flags(pgm_process *);
static int   finalize_transitions(pgm_process *);
static void  compute_leaf_no(pgm_process *, uint4);
static void  compute_process_leaf_no(pgm_process *);
static int   get_leaf_no(pgm_process *, char *, uint4 *);
static int   spread_transitions_downward(pgm_process *, uint4);
static pgm_transition *transition_copy(pgm_transition *);
static int   translate_meta(pgm_meta *);
static int   translate_meta_head(pgm_meta *, uint1 *, uint4 *);
static pgm_tr_ref *translate_meta_body(pgm_meta *, uint1 *, uint4 *);
static int   translate_meta_check_sequence(pgm_meta *, pgm_tr_ref *,
					   uint4 *);
static int   follow_meta(pgm_process *, pgm_meta *, uint4, uint4, 
  pgm_tr_ref ***); 
static char  *meta_transition_error_msg(pgm_process *, pgm_meta *, 
				       uint4, uint4, char *);
static pgm_gen_condition *gen_condition_copy(pgm_gen_condition *); 
static pgm_gen_operation *gen_operation_copy(pgm_gen_operation *); 
static int   operation_copy(pgm_operation *, pgm_operation *);
static int   compose_gop_1(pgm_gen_operation *, pgm_operation *);
static int   cond_add_arg(pgm_gen_condition *, pgm_gen_condition *);
static int   cond_merge_AND(pgm_gen_condition **, pgm_gen_condition *,
			    pgm_gen_condition *); 
static int   pgm_cond_norm_OR(pgm_gen_condition **);
static int   pgm_cond_norm_AND(pgm_gen_condition **);
static int   check_cond4meta(pgm_gen_condition *);  
static int   insert_term_in_list(pgm_term_list **, pgm_term *);  
static void  free_term_list(pgm_term_list *);
static int   check_init_states( void );
static int   generate_process_instance(uint4, uint4);
static int   generate_local_vars_copy(pgm_process *, pgm_variable **,
				      uint4 **);
static int   generate_states_copy(pgm_process *, pgm_state **, 
				  uint4 *, uint4 *, uint4 *);
static int   generate_transitions_copy(pgm_state *, pgm_transition **,
				       uint4 *, uint4 *, uint4 *);
static void  convert_gen_condition_vars_no(pgm_gen_condition *, 
					   uint4 *);
static void  convert_gen_operation_vars_no(pgm_gen_operation *, 
					   uint4 *);

/****  Private functions.                                        ****/

/** void  free_uint1(p) : Frees the memory allocated to the
                    uint1 *p. Does not report errors.              **/

static void  free_uint1(p)
     uint1 *p;
{
  resr__free_object(p, uint1);
}

/** void  free_constant(c) : Frees the memory allocated to the
                    pgm_constant *c. Does not report errors.       **/

static void  free_constant(c)
     pgm_constant *c;
{
  pgm_free_string(c -> name);
  resr__free_object(c, pgm_constant);
}

/**  void *grow_table(tp, np, ap, n)  :  Adds a new element to the
                    array *tp, whose entries are each of size n
                    (expressed in bytes). The current number of
                    entries in the array is *np, and the number of
                    allocated elements is *ap. These two values are
                    updated by the function. In the case of success,
                    the function returns a pointer to the newly
                    added element. In the case of insufficient
                    memory, it returns a NULL pointer.             **/

static void *grow_table(tp, np, ap, n)
  void **tp;
  uint4 *np, *ap, n;
{
  register void *r;

  if (!*ap)
    {
      r = (void *) resr__malloc(PGM_GROWTH * n);
      if (!r)
	return NULL;
      *tp = r;
      *ap = PGM_GROWTH;
      *np = 1;
      return r;
    }

  if ((*np) >= (*ap))
    {     
      r = (void *) resr__realloc((uint1 *) (*tp), 
          (PGM_GROWTH + *ap) * n, (*ap) * n);
      if (!r)
	return NULL;
      *tp  = r;
      *ap += PGM_GROWTH;
    }

  return (void *) (((uint1 *) (*tp)) + ((*np)++ * n));
}

/**  void  free_the_program()  :  Frees the dynamic data structures
                    allocated during the compilation. This function
                    can be called at any stage of the compilation,
                    and does not report errors.                    **/

static void  free_the_program()
{
  register uint4         i;
  register pgm_variable *pv, *pvnext;
  register pgm_meta     *pm, *pmnext;

  for (i = ZERO_INT4; i < the_program.nb_proc; i++)
    free_process(the_program.processes + i);

  if ((the_program.nb_proc > 0)
      || (the_program.processes))
    resr__free_objects(the_program.processes, pgm_process,
		       the_program.nb_proc);

  for (pv = the_program.global_vars; pv; pv = pvnext)
    {
      pvnext = pv -> next;
      free_variable(pv);
    }

  for (pm = the_program.meta; pm; pm = pmnext)
    {
      pmnext = pm -> next;
      free_meta(pm);
    }
}

/**  void  free_process(p)  :  Frees the process information
                    structure *p. Does not report errors.          **/

static void  free_process(p)
  pgm_process *p;
{
  register uint4         i;
  register pgm_variable *pv, *pvnext;

  if (p -> name)
    pgm_free_string(p -> name);

  for (i = ZERO_INT4; i < p -> nb_states; i++)
    pgm_free_state_elt(p -> states + i); 

  if ((p -> al_states) || ( p -> states))
    resr__free_objects(p -> states, pgm_state, p -> al_states);

  if ((p -> al_root_states) || ( p -> root_states))
    resr__free_objects(p -> root_states, uint4, p -> al_root_states);

  for (pv = p -> local_vars; pv; pv = pvnext)
    {
      pvnext = pv -> next;
      free_variable(pv);
    }  
}

/**  void  free_variable(v)  :  Frees the variable information
                    structure *v. Does not report errors.          **/

static void  free_variable(v)
  pgm_variable *v;
{
  if (!v)
    return;

  if (v -> name)
    pgm_free_string(v -> name);

  resr__free_object(v, pgm_variable);
}

/**  void  free_meta(m)  :  Frees the meta-transition information
                    structure *m. Does not report errors.          **/

static void  free_meta(m)
  pgm_meta *m;
{
  register pgm_control *pc, *pcnext;
  register pgm_tr_ref  *pt, *ptnext;  

  if (!m)
    return;

  for (pc = m -> head; pc; pc = pcnext)
    {
      pcnext = pc -> next;
      free_control(pc);
    }

  for (pc = m -> body; pc; pc = pcnext)
    {
      pcnext = pc -> next;
      free_control(pc);
    }

  for (pt = m -> trans; pt; pt = ptnext)
    {
      ptnext = pt -> next;
      resr__free_object(pt, pgm_tr_ref);
    }

  resr__free_object(m, pgm_meta);
}

/**  void  free_control(c)  :  Frees the control location information
                    structure *c. Does not report errors.          **/

static void  free_control(c)
  pgm_control *c;
{
  if (!c)
    return;

  if (c -> process_name)
    pgm_free_string(c -> process_name);

  if (c -> spec_name)
    pgm_free_string(c -> spec_name); 

  resr__free_object(c, pgm_control);
}

/** static void  free_gen_cond_content(c) : Frees the content of the
                     pgm_gen_condition *c, either the subconditions
		     in the case of type different from PGM_COND_ATOM
		     or the elements in the case of PGM_COND_ATOM.
		     Does not report errors.                       **/
static void  free_gen_cond_content(c)
     pgm_gen_condition *c;
{
  register uint4 i;

  if (c -> type == PGM_COND_ATOM)
    {
      resr__free_objects(((pgm_condition *) (c -> args)) -> el, 
			 pgm_term, 
			 ((pgm_condition *) (c -> args)) -> nb_el);
      resr__free_object((pgm_condition *)(c -> args), pgm_condition);
    }
  else 
    {
      for (i = ZERO_INT4 ; i < c -> nb_args ; i++)
	free_gen_cond_content((pgm_gen_condition *) (c -> args) + i);
      resr__free_objects(c -> args, pgm_gen_condition, c -> al_args);
    }
}

/** void  free_transition_elt(tr) : Frees the elements of the 
                   transition tr. Does not report errors.          **/

static void  free_transition_elt(tr)
     pgm_transition *tr;
{
  if (tr -> label)
    pgm_free_string(tr -> label);

  if (tr -> dest_name)
    pgm_free_string(tr -> dest_name);

  if (tr -> asgn)
    pgm_free_gen_operation(tr -> asgn);
  
  if (tr -> pre_cdt) 
      pgm_free_gen_condition(tr -> pre_cdt);
}

/**  int  reference_metas()  :  Scans all the meta-transitions of
                    the program that has been generated, replaces the
                    process or control location identifiers by the
                    corresponding index, and computes the equivalent
                    sequences of transitions.  In the case of an
                    error, this function returns -1 and displays a
                    message. Otherwise, it returns 0.              **/

static int  reference_metas()
{
  register pgm_meta    *pm;
  register pgm_control *pc;

  for (pm = the_program.meta; pm; pm = pm -> next)
    { 
      for (pc = pm -> head; pc; pc = pc -> next)
        if (reference_control(pc, pm) < 0)
	  return -1;
      for (pc = pm -> body; pc; pc = pc -> next)
        if (reference_control(pc, pm) < 0)
	  return -1;
      if (translate_meta(pm) < 0)
	return -1;
    }
  
  return 0;
}

/**  int  reference_control(c, m)  :  Replaces the process and/or
                    control location or transition identifiers
                    belonging to the control information structure *c
                    by their corresponding indices. The
                    meta-transition to which *c belongs is *m. In the
                    case of an error, this function returns -1 and
                    displays a message. Otherwise, it returns 0.   **/

static int  reference_control(c, m)
  pgm_control *c;
  pgm_meta    *m;
{
  register id_table_payload *pu;
           char   tag[256];

  if (c -> process_name)
    {
      pu = (id_table_payload *) hash__lookup_string(id_table, 
          c -> process_name);
      if ((!pu) || (pu -> process_defined == 0))
	{
	  char line[256];

          sprintf(line,
"Compiler error in definition at (L%u, C%u): Unknown process '%.32s'",
              m -> lno, m -> cno, c -> process_name);
	  report_siflash_error(line);
	  return -1;
	}
      c -> process_no = pu -> process;
    }

  if (c -> spec_name)
    {
      sprintf(tag, "%.127s@%.127s",
          the_program.processes[c -> process_no].name,
          c -> spec_name);

      pu = (id_table_payload *) hash__lookup_string(id_table, tag);
      if (!pu)
	{
	  char line[256];

          sprintf(line,
"Compiler error in definition at (L%u, C%u): Invalid label '%.32s'",
              m -> lno, m -> cno, c -> spec_name);
	  report_siflash_error(line);
	  return -1;
	}
      if (pu -> state_defined)
	c -> location_no = pu -> state;
      if (pu -> trans_defined)
	{
	  c -> transition_no = pu -> trans;
	  c -> trans_defined = 1;
	}
      else
	c -> trans_defined = 0;
      if (!((pu -> state_defined) || (pu -> trans_defined)))
	{
	  char line[256];
	  
          sprintf(line,
"Compiler error in definition at (L%u, C%u): Invalid label '%.32s'",
              m -> lno, m -> cno, c -> spec_name);
	  report_siflash_error(line);
	  return -1;  
	}
    }
  
  return 0;
}

/**  typedef mark_el  :  Information stored on the exploration stack
                    of the function mask_states.                   **/

typedef struct {
  uint4  st_no, tr_nb;
} mark_el;

/**  int  mark_states(p)  :  Computes the reachable and the fusionable
                    control locations of the process *p, settings the
                    appropriate flags (a fusionable control location
                    is one that can be fusioned with its successor.)
                    In the case of insufficient memory, this function
                    returns -1. Otherwise, it returns 0.  Does not
                    report error.                                  **/
static int  mark_states(p)
  pgm_process *p; 
{
  register stack     *st;
  register mark_el   *mt;
  register pgm_state *ps;
  register uint4      n;
           mark_el    me;

  if (!(p -> nb_states))
    return 0;

  st = stack__new_empty(mark_el);
  if (!st)
    return -1;

  if (!p -> has_leaf_no)
    compute_process_leaf_no(p);

  me.st_no = p -> leaf_no;
  me.tr_nb = ZERO_INT4;

  if (stack__push(st, &me) < 0)
    {
      stack__free(st);
      return -1;
    }

  for (n = ZERO_INT4; !(stack__is_empty(st));)
    {
      mt = (mark_el *) stack__top(st);
      ps = p -> states + mt -> st_no;

      if (!(mt -> tr_nb))
	{
	  ps -> reachable = 1;
	   if (ps -> nb_trans == 1 &&
	       (ps -> trans[ZERO_INT4].dest_type !=
		PGM_TRANS_TYPE_ASSERT) &&
	       (!ps -> trans[ZERO_INT4].asgn) &&
	       (!ps -> trans[ZERO_INT4].pre_cdt) &&
	       (((ps -> atomic) ||
		 (!(p -> states[
		        ps -> trans[ZERO_INT4].dest_no].atomic)))))
	     { 
	       ps -> fusionable = 1;
	     }
	  else 
	    ps -> id = n++;
	}

      if (mt -> tr_nb >= ps -> nb_trans)
	{
	  stack__pop(st, &me);
	  continue;
	}
      if (ps -> trans[mt -> tr_nb].dest_type == PGM_TRANS_TYPE_ASSERT)
	{
	  mt -> tr_nb++;
	  continue;
	}
      me.st_no = ps -> trans[mt -> tr_nb++].dest_no;
      
      if (p -> states[me.st_no].reachable)
	continue;    /* state already visited */

      me.tr_nb = ZERO_INT4;
      if (stack__push(st, &me) < 0)
	{
	  stack__free(st);
	  return -1;
	}

    }

  p -> nb_ids = n;

  stack__free(st);
  return 0;
}
		
/**  int  check_fusion_loops(p)  :  Checks whether there are loops
                    of fusionable states in the process *p, and,
                    if any, break such loops. In the case of 
                    insufficient memory, this function returns -1.
                    Otherwise, it returns 0.

                    This function does currently not produce an
                    optimal result, but runs in linear time. It
                    should be replaced by one based on a strongly
                    connected components analysis.                 **/

static int  check_fusion_loops(p)
  pgm_process *p;
{
  register bit_table *bt1, *bt2;
  register uint4      i, j;
  register pgm_state *ps;

  bt1 = bit__new_empty(p -> nb_states);
  if (!bt1)
    return -1;

  for (i = ZERO_INT4, ps = p -> states; i < p -> nb_states;
       i++, ps++)
    if (ps -> reachable && ps -> fusionable && !bit__member(bt1, i))
      {
	bit__add(bt1, i);
	
	bt2 = bit__new_empty(p -> nb_states);
	if (!bt2)
	  {
	    bit__free(bt1);
	    return -1;
	  }

	bit__add(bt2, i);

	for (j = ps -> trans[ZERO_INT4].dest_no; 
	     p -> states[j].fusionable;
             j = p -> states[j].trans[ZERO_INT4].dest_no)
	  if (bit__member(bt2, j))
	    {
	      p -> states[j].fusionable = 0;
	      p -> states[j].id = p -> nb_ids++;
	      break;
	    }
	  else
	    {
	      bit__add(bt1, j);
	      bit__add(bt2, j);
	    }
	bit__free(bt2);
      }
  bit__free(bt1);
  
  return 0;
}

/** int   set_init_flags(pr) : Determines the initial
                 state of the process pr and of compound state if not
                 already specified.  Return 0 in the case of success
                 and -1 otherwise.  Reports errors.                **/

static int  set_init_flags(pr)
     pgm_process *pr;
{
  register pgm_state *st, *init_st, *st2;
  register uint4      j, k;
           char       line[256];
  
  if (pr -> nb_root_states == ZERO_INT4)
    {
      sprintf(line, "Process %s has no state", pr -> name); 
      report_siflash_error(line);
      return -1;
    }
  
  for (j = ZERO_INT4, init_st = NULL; j < pr -> nb_root_states;
       j++)
    {
      st = pr -> states + *(pr -> root_states + j);
      if (st -> init > 0)
	{
	  if (init_st)
	    {
	      sprintf(line,
"Duplicate init state: %s and %s for process %s", 
		      init_st -> name, st -> name, pr -> name); 
	      report_siflash_error(line);
	      return -1;
	    }
	  else
	    {
	      init_st = st;
	    }
	}
    }
  if (!init_st)
    {
      init_st = pr -> states + pr -> root_states[0];
      init_st -> init = 1;
    }
  
  for (j = ZERO_INT4 ; j < pr -> nb_states ; j++)
    {
      st2 = pr -> states + j;
      if (st2 -> nb_substates == 0)
	continue;
      
      for (k = ZERO_INT4, init_st = NULL; k < st2 -> nb_substates;
	   k++)
	{
	  st = pr -> states + st2 -> substates[k];
	  if (st -> init > 0)
	    {
	      if (init_st)
		{
		  sprintf(line,
"Duplicate init state: %s and %s for state %s in process %s",
			  init_st -> name, st -> name, 
			  st2 -> name, pr -> name);
		  report_siflash_error(line);
		  return -1;
		}
	      else
		init_st = st;
	    }
	}
      if (!init_st)
	{
	  init_st  = pr -> states + st2 -> substates[0];
	  init_st -> init = 1;
	}
    }
  
  return 0;
}

/** int   finalize_transitions(pr) : Redirects transitions of the
               process pr to leaf state (i.e. not compound state), and
               spreads downward the transitions of compound state. 
               For each transition, frees dest_name and sets the index
               instead.  Returns 0 in the case of success and -1 in
               the case of error.  Reports errors.                 **/

static int   finalize_transitions(pr)
     pgm_process *pr;
{
  register pgm_state      *st;
  register pgm_transition *tr;
  register uint4           j, k;
           uint4           no;


  for (j = ZERO_INT4 ; j < pr -> nb_states ; j++)
    {
      st = pr -> states + j;

      /* Redirects transition and associate index. */
      for (k = ZERO_INT4 ; k < st -> nb_trans; k++)
	{
	  tr = st -> trans + k;
	  if (tr -> dest_type == PGM_TRANS_TYPE_ASSERT)
	    continue;
	  if (tr -> dest_type == PGM_TRANS_TYPE_STOP)
	    no = pr -> dummy_no;
	  else
	    if (get_leaf_no(pr, tr -> dest_name, &no) < 0)
	      return -1;
	  tr -> dest_no = no;
	  pgm_free_string(tr -> dest_name);
	  tr -> dest_name = NULL;
	}
    }
      
  /* Spreads downward transitions of compound states. */
  for (j = ZERO_INT4 ; j < pr -> nb_root_states ; j++)
    {
      st = pr -> states + pr -> root_states[j];
      if ((st -> nb_substates > 0) &&
	  (spread_transitions_downward(pr, pr -> root_states[j]) < 0))
	return -1;
    }
  
  return 0;
}
  
/** void   compute_leaf_no(p, s): Computes the index of the leaf
                   state corresponding to s if s is a compound
		   state. The leaf state is found by following
		   downward the init substates.  The function is
		   applied recursively to all substates of s.  Does
		   not report error.                               **/

static void   compute_leaf_no(pr, s)
     pgm_process *pr;
     uint4        s;
{
  register pgm_state *st, *next;
  register uint4 i;

  st = pr -> states + s;
  if (st -> nb_substates == ZERO_INT4)
    {
      st -> leaf_no = s;
      st -> has_leaf_no = 1;
      return;
    }
  
  for (i = ZERO_INT4; i < st -> nb_substates ; i++)
    {
      next = pr -> states + st -> substates[i];
      compute_leaf_no(pr, next -> no);
      if (next -> init > 0)
	{
	  st -> leaf_no = next -> leaf_no;
	  st -> has_leaf_no = 1;
	}	     
    }
}

/** void   compute_process_leaf_no(pr): Computes the index of the leaf
                   state corresponding to the process pr and for all
		   states of pr.  The leaf state is found by following
		   downward the init substates.  The function is
		   applied recursively to all substates of pr.  Does
		   not report errors.                              **/

static void   compute_process_leaf_no(pr)
     pgm_process *pr;
{
  register pgm_state *st;
  register uint4 i;
  
  for (i = ZERO_INT4; i < pr -> nb_root_states ; i++)
    {
      st = pr -> states + pr -> root_states[i];
      compute_leaf_no(pr, st -> no);
      if (st -> init > 0)
	{
	  if (st -> nb_substates == 0)
	    pr -> leaf_no = st -> no;
	  else
	    pr -> leaf_no = st -> leaf_no;
	  pr -> has_leaf_no = 1;
	}	     
    }
}

/** int   get_leaf_no(pr, name, no) : Looks up the leaf state
                 corresponding to the state of name name in the
		 process pr. Stores the index in *no.  Returns 0 in
		 the case of success and -1 in the case of
		 failure. Reports errors.                          **/

static int   get_leaf_no(pr, name, no)
     pgm_process *pr;
     char *name;
     uint4 *no;
{
  register   id_table_payload *pu;
  register   pgm_state *st;
  register   uint4      l;
             char      tag[256];
	     char      msg[512];

  sprintf(tag, "%.127s@", pr -> name);
  l = strlen(tag);
  sprintf(tag + l, "%.127s", name);
  pu = (id_table_payload *) hash__lookup_string(id_table, tag);
  
  if ((!pu) || (pu -> state_defined == 0))
    {
      sprintf(msg, "Unknown state:  %s in process %s", name, 
	      pr -> name);
      report_siflash_error(msg); 
      return -1;
    }
  
  st = pr -> states + pu -> state;
  if (st -> nb_substates == ZERO_INT4)
    *no =  st -> no;
  else
    {
      if (st -> has_leaf_no == ZERO_INT4)
	compute_leaf_no(pr, st -> no);
      
      *no = st -> leaf_no;
    }
  return 0;
}
 
/** int  spread_transitions_downward(pr, s) : Spreads the transitions
               (if any) of the state of index s of the process pr to
               its substates if any.  Returns 0 in the case of success
               and -1 in the case of failure. Reports errors.      **/

static int   spread_transitions_downward(pr, s)
     pgm_process *pr;
     uint4 s;
{
  register pgm_state   *st;
  register pgm_transition *tr;
  register uint4        i,j;

  st = pr -> states + s;
  if (st -> nb_substates == ZERO_INT4)
    return 0;
  if (st -> nb_trans == ZERO_INT4)
    return 0;
  for (i = ZERO_INT4 ; i < st -> nb_substates ; i++)
    {
      for (j = ZERO_INT4 ; j < st -> nb_trans ; j++)
	{
	  tr = transition_copy(st -> trans + j);
	  if (!tr)
	    {
	      report_siflash_memory_error();
	      return -1;
	    }
	  if (pgm_add_transition(st -> substates[i], tr) < 0)
	    return -1;
	}
      
      if (spread_transitions_downward(pr,st -> substates[i]) < 0)
	return -1;
    }
  return 0;
}

/** pgm_transition *transition_copy(tr) : Creates a  copy of the 
                    transition tr and returns a pointer to the newly
                    allocated transition in the case of success or
                    NULL in the case of insufficient memory.  Does not
                    report errors.                                 **/

static pgm_transition *transition_copy(tr)
     pgm_transition *tr;
{
  char              *dest_copy;
  pgm_transition    *tr_new;
  pgm_gen_operation *gop;
  pgm_gen_condition *gc;
  
  tr_new = resr__new_object(pgm_transition);
  if (!tr_new)
    return NULL;
  
  tr_new -> dest_type  = tr -> dest_type;
  tr_new -> dest_no    = tr -> dest_no; 
  tr_new -> process_no = tr -> process_no; 
  tr_new -> label      = NULL;
  tr_new -> asgn       = NULL;
  tr_new -> pre_cdt    = NULL;
  tr_new -> dest_name  = NULL;
  
  if (tr -> dest_name)
    {
      dest_copy = 
	resr__new_objects(char, strlen(tr -> dest_name) + 1);
      if (!dest_copy)
	{
	  resr__free_object(tr_new, pgm_transition);
	  return NULL;
	}
      strcpy(dest_copy, tr -> dest_name);
      tr_new -> dest_name = dest_copy;
    }
  
  tr_new -> label = NULL;
  if (tr -> asgn)
    {
      gop = gen_operation_copy(tr -> asgn);
      if (!gop)
	{
	  free_transition_elt(tr_new);
	  resr__free_object(tr_new, pgm_transition);
	  return NULL;
	}
      tr_new -> asgn = gop;
    }
  
  if (tr -> pre_cdt)
    {
      gc = gen_condition_copy(tr -> pre_cdt);
      if (!gc)
	{
	  free_transition_elt(tr_new);
	  resr__free_object(tr_new, pgm_transition);
	  return NULL;
	}
      tr_new -> pre_cdt = gc;
    }
  return tr_new;
}

/**  int  translate_meta(p)  :  Computes the list of transitions
                    corresponding to the meta-transition specified
                    by the information structure *p, and stores that
                    list in this structure. In the case of an error,
                    this function returns -1 and displays a
                    message. Otherwise, it returns 0.              **/

static int  translate_meta(p)
  pgm_meta *p;
{
  register uint1       *present;
  register uint4       *current_locations, i, n;
           pgm_tr_ref  *seq_ref, *cur, *next;

  n = the_program.nb_proc;
  present = resr__new_objects(uint1, n); /* indicates if there is a 
					  location in the head for 
					  process i */
  if (!present)
    {
      report_siflash_memory_error();
      return -1;
    }
  
  current_locations = resr__new_objects(uint4, n);
  if (!current_locations)
    {
      resr__free_objects(present, uint1, n);
      report_siflash_memory_error();
      return -1;
    }
  
  for (i = ZERO_INT4; i < n; i++)
    present[i] = 0;
  
  /* checks control states of the head */

  if (translate_meta_head(p, present, current_locations) < 0)
    {
      resr__free_objects(present, uint1, n);
      resr__free_objects(current_locations, uint4, n);
      return -1;	  
    }

  /* checks & processes the body */

  seq_ref = translate_meta_body(p, present, current_locations) ;
  if (!seq_ref)
    {
      resr__free_objects(present, uint1, n);
      resr__free_objects(current_locations, uint4, n);
      return -1;	  
    }
  
  resr__free_objects(present, uint1, n);  
   
  /* checks the sequence  */ 

  if (translate_meta_check_sequence(p, seq_ref, current_locations) 
      < 0)
    {
      resr__free_objects(current_locations, uint4, n);
      next = seq_ref;
      while(next) 
	{
	  cur = next;
	  next = cur -> next;
	  resr__free_object(cur, pgm_tr_ref);
	} 
      return -1;
    }
  
  p -> trans = seq_ref;
  resr__free_objects(current_locations, uint4, n);

  return 0;
}

/** int translate_meta_head(p, present, current_locations) :
              Checks the head of the meta-transition p and initializes
	      the variables present and current_location.  Returns 0
	      in the case of success and -1 otherwise.  Reports
	      errors.                                              **/

static int translate_meta_head(p, present, current_locations)
  pgm_meta *p;
  uint1    *present;
  uint4    *current_locations;
{
  register pgm_process *pr;
  register pgm_state   *st;
  register pgm_control *pc;
	   char line[256];
  
  for (pc = p -> head; pc; pc = pc -> next)
    {
      pr = the_program . processes + pc -> process_no;
      if (pr -> nb_instances != 1)
	{
	  sprintf(line, 
"Compiler error in Meta-transition definition at (L%u, C%u): %u instance(s) of process '%.32s'",
		  p -> lno, p -> cno, pr -> nb_instances, pr -> name);
	  report_siflash_error(line);
	  return -1;	  
	}

      if (present[pc -> process_no])
	{
	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Duplicate head process '%.32s'",
		  p -> lno, p -> cno, pr -> name);
	  report_siflash_error(line);
	  return -1;
	}
      st = pr -> states + pc -> location_no;
      if (! st -> reachable)
	{
	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Unreachable meta-transition in process '%.32s' (state %s)",
		  p -> lno, p -> cno, pr -> name, st -> name);
	  report_siflash_error(line);
	  return -1;
	}

      if ( pr -> states[pc -> location_no]. nb_substates > 0)
	{
	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Ambiguous meta-transition in process '%.32s' (state %s is a compound state)",
              p -> lno, p -> cno, pr -> name, st -> name);
	  report_siflash_error(line);
	  return -1;
	}
      
      present[pc -> process_no] = 1;
      current_locations[pc -> process_no] = pc -> location_no;
    }
  
  return 0;
}


/** pgm_tr_ref *translate_meta_body(p, present, current_locations) :
              Checks the body of the meta-transition p and initializes
	      the variables present and current_location.  Constructs
	      a linked list of pgm_tr_ref corresponding to the
	      sequence of transitions of the meta-transition.  Returns
	      a pointer to the first element of the linked list in the
	      case of success in the case of success and NULL
	      otherwise.  Reports errors.                          **/
static pgm_tr_ref *translate_meta_body(p, present, current_locations)

     pgm_meta *p;
     uint1    *present;
     uint4    *current_locations;
{
  register pgm_process *pr;
  register pgm_control *pc;
	   char line[256], *msg;
	   pgm_tr_ref  *tr_ref, *first_ref, **last_ref;
	   pgm_transition *tr;
	   pgm_state *st;

  first_ref = NULL;
  last_ref = &first_ref;
  for (pc = p -> body; pc; pc = pc -> next)
    {
      pr = the_program.processes + pc -> process_no;
      if (!present[pc -> process_no])
	{
	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Process '%.32s' has no head",
		  p -> lno, p -> cno, pr -> name);
	  report_siflash_error(line);
	  return NULL;
	}
      
      if (pc -> trans_defined == 1)
	{
	  if ( current_locations[pc -> process_no] != 
	       pc -> location_no)
	    {
	      if (follow_meta(pr, p, current_locations[pc -> 
						      process_no],
			      pc -> location_no, &last_ref) < 0)
		{
		  while (first_ref) 
		    {
		      tr_ref = first_ref;
		      first_ref = tr_ref -> next;
		      resr__free_object(tr_ref, pgm_tr_ref);
		    }
		  return NULL;
		}
	    }
	  
	  st = pr -> states + pc -> location_no; 
	  tr = st -> trans + pc -> transition_no;
	  
	  if ((tr -> pre_cdt)
	      && (check_cond4meta(tr -> pre_cdt) < 0))
	    {
	      msg = meta_transition_error_msg(the_program.processes +
					(pc -> process_no), p,
					pc -> location_no, 
					tr -> dest_no,
					"transition not valid") ;
	      if (msg) 
		{
		  report_siflash_error(msg);
		  pgm_free_string(msg);
		}
	      return NULL;
	    }
	  
	  tr_ref = resr__new_object(pgm_tr_ref);
	  if (!tr_ref)
	    {
	      report_siflash_memory_error();
	      return NULL;
	    }
	  
	  tr_ref -> tr = tr;
	  tr_ref -> next = NULL;
	  *last_ref = tr_ref;  /* linked list constructed from 
			       left to right                */
	  last_ref = &tr_ref -> next;
	  current_locations[pc -> process_no] = tr -> dest_no;
	}
      else  
	{
	  if (follow_meta(pr, p, current_locations[pc -> process_no],
			  pc -> location_no, &last_ref) < 0)
	    {
	      return NULL;
	    }
	  current_locations[pc -> process_no] = pc -> location_no;
	}
    }
  return first_ref;
}

/* translate_meta_check_sequence(p, seq_ref, current_locations) :
               Checks that the sequence of transitions corresponding
               to the linked list of pgm_tr_ref seq_ref is valid.
	       Returns 0 if the sequence is valid and if an error
               occured or if the sequence is not valid.
	       Reports the error.                                  **/
static int translate_meta_check_sequence(p, seq_ref, 
					 current_locations)
     pgm_meta *p;
     pgm_tr_ref *seq_ref;
     uint4 *current_locations;
{
  register pgm_process *pr;
  register pgm_control *pc;
	   char line[256];
	   pgm_tr_ref  *next;
  register pgm_state   *s;

 /* checks that the metatransition forms a loop */
 
  for (pc = p -> head; pc; pc = pc -> next)
    if (current_locations[pc -> process_no] != pc -> location_no)
      {
	sprintf(line, 
"Compiler error in definition at (L%u, C%u): Open loop in process '%.32s'",
		p -> lno, p -> cno,
		the_program.processes[pc -> process_no].name);
	report_siflash_error(line);
	return -1;
      }

  /* Checks that each visited state has no substate. */
  for (next = seq_ref; next ; next = next -> next)
    {
      pr =the_program.processes + (next -> tr) -> process_no;
      s = pr -> states + (next -> tr) -> dest_no;
      if (s -> nb_substates > ZERO_INT4)
	{
	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Ambiguous meta-transition in process '%.32s' (state %s is a compound state)",
		  p -> lno, p -> cno, pr -> name, s -> name);
	  report_siflash_error(line);
	  return -1;
	}
    }
  
  return 0;
}

/**  typedef follow_info  :  Information stored on the exploration
                   stack of the function follow_meta.              **/

typedef struct {
  uint4  st_no, tr_nb;
}  follow_info;

/**  int  follow_meta(pr, mt, no, nd, pl)  : 
                   Explores the transitions of the process *pr,
                   starting from the state of index no and heading to
                   the state of index nd. The sequence of explored
                   transitions is stored in the linked list **pl. The
                   current meta-transition being analyzed is *mt.  All
                   transitions (labelled or not) are considered while
                   searching a path from n0 to nd.  The loops are not
                   considered when attempting to find a path between
                   two control states.  If no path is found or if the
                   path is not valid (there are multiple paths, or one
                   transition is not valid), then an error message is
                   generated and it returns -1.  In the case of an
                   error, it returns -1. In the case of success, it
                   returns 0.                                      **/

static int  follow_meta(pr, mt, no, nd, pl)
  pgm_process  *pr;
  pgm_meta     *mt; /* used for identification in the case of an
                                                              error */
  uint4         no, nd;
  pgm_tr_ref ***pl;
{
  register bit_table      *bt;
  register stack          *st;
  register follow_info    *ft, *fp;
  register pgm_state      *ps;
  register pgm_transition *tr;
  register pgm_tr_ref     *ref;
  register uint4           i, n;
  register uint1           found;
           follow_info     fi;
           char           *msg;

  found = 0;
  
  bt = bit__new_empty(pr -> nb_states);
  if (!bt)
    {
      report_siflash_memory_error();
      return -1;
    }
      
  st = stack__new_empty(follow_info);
  if (!st)
    {
      bit__free(bt);
      report_siflash_memory_error();
      return -1;
    }
  fi.st_no = no;
  fi.tr_nb = ZERO_INT4;
      
  if (stack__push(st, &fi) < 0)
    {
      bit__free(bt);
      stack__free(st);
      report_siflash_memory_error();
      return -1;
    }

  while (!(stack__is_empty(st)))
    {
      ft = (follow_info *) stack__top(st);
      ps = pr -> states + ft -> st_no;
	  
      if (!(ft -> tr_nb)) /* first visit */
	{
	  if ((found == 1) && (ft -> st_no == nd)) 
	    {
	      msg = meta_transition_error_msg(pr, mt, no, nd,
					"ambiguous path");
	      if (msg){
		report_siflash_error(msg);
		pgm_free_string(msg);
	      }
	      stack__free(st);
	      bit__free(bt);
	      return -1;
	    }
	  
	  if ((stack__size(st) > 1) && (ft -> st_no == nd))
	    { /* nd reached */
	      for (n = stack__size(st), i = n - 1; i; i--)
		{
		  /* there is in the stack all the transitions
		     needed to reach nd */
		  fp = (follow_info *) stack__pick(st, i);
		  
		  tr = pr -> states[fp -> st_no].trans +
		    fp -> tr_nb - 1;
		  if ((tr -> pre_cdt)
		      && (check_cond4meta(tr -> pre_cdt) < 0)) 
		    {
		      msg = meta_transition_error_msg(pr, mt, no, nd,
						    "Transition not valid");
		      if (msg) 
			{
			  report_siflash_error(msg);
			  pgm_free_string(msg);
			}
		      stack__free(st);
		      bit__free(bt);
		      return 0;
		    }      
		  
		  if ((tr -> asgn) 
		      || (tr -> pre_cdt))
		    {
		      ref = resr__new_object(pgm_tr_ref);
		      if (!ref)
			{
			  report_siflash_memory_error();
			  return -1;
			}
		      ref -> tr   = tr;
		      ref -> next = NULL;
		      **pl = ref;  /* linked list constructed from 
				      left to right             */
		      *pl = &ref -> next;
		    }
		} 
	      found = 1;
	      stack__pop(st, &fi);
	      continue; /* continue to check in the case of  
			   multiple access to nd           */
	    } 
	  
	  if (!(stack__size(st) == 1 && no == nd)) 
	    bit__add(bt, ft -> st_no);
	} /* first visit */
      
      if (ft -> tr_nb >= ps -> nb_trans)
	{
	  /* no more transition to explore for this state... */
	  stack__pop(st, &fi);
	  continue;
	}
      
      /* prepare the next fi */
      tr = ps -> trans + (ft -> tr_nb++);
      if (tr -> dest_type == PGM_TRANS_TYPE_ASSERT)
	continue;
      fi.st_no = tr -> dest_no;
	  
      if (bit__member(bt, fi.st_no))
	continue;
      
      fi.tr_nb = ZERO_INT4;
	  
      if (stack__push(st, &fi) < 0)
	{
	  stack__free(st);
	  bit__free(bt);
	  report_siflash_memory_error();
	  return -1;
	}
    } 
  bit__free(bt);
  stack__free(st);
      
  if (found == 0)
    {
      msg = meta_transition_error_msg(pr, mt, no, nd, "No path ");
      if (msg)
	{       
	  report_siflash_error(msg);
	  pgm_free_string(msg);
	}
      return -1;
    }
  
  return 0;
}

/** char *meta_transition_error_msg(pr, mt, no, nd, st) : Generates
		  the error message regarding the meta-transition mt,
		  between the states no and nd.
		  The error type is initially
		  stored in the string st.  Return a pointer to a
		  newly allocated string in the case of
		  success and NULL in the case of failure and report
		  error.                                             **/

static char  *meta_transition_error_msg(pr, mt, no, nd, st)
  pgm_process  *pr;
  pgm_meta     *mt; /* used only for identification 
		       in the case of error */
  uint4         no, nd;
  char         *st;
{
  char  *name_o, *name_d, line[512], *msg;
  uint4 process_no, i, l;

  for (i = ZERO_INT4; i < the_program.nb_proc ; i++)
    if (the_program.processes + i == pr)
      {
	process_no = i;
	break;
      }
  
  if (i == the_program.nb_proc)
    {
      report_siflash_error("INTERNAL: corrupted data while generating error message");
      return NULL;
    }

  name_o = ( pr -> states + no ) -> name;
  name_d = ( pr -> states + nd ) -> name;
  
  
  if ((!name_o) || (!name_d))
    {
      sprintf(line, 
"Compiler error at (L%u, C%u): metatransition - %s.",
	      mt -> lno, mt -> cno, st);
    }
  else 
    { 
      sprintf(line, 
"Compiler error at (L%u, C%u): metatransition - %s from  %s.%s to %s.%s " , mt -> lno, mt -> cno, st, pr -> name, name_o,
	      pr -> name, name_d);
    }
  
  l = strlen(line);
  msg = resr__new_objects(char, l + 1);
  if (!msg)
    {
      report_siflash_memory_error();
      return NULL;
    }
  
  strcpy(msg, line);
  
  return msg;
}

/** pgm_gen_condition *gen_condition_copy(c) : Returns a pointer to a
                             copy of c in the case of success, and a
                             null pointer in the case of insufficient
                             memory.                               **/

static pgm_gen_condition *gen_condition_copy(c)
     pgm_gen_condition * c;
{
  pgm_gen_condition *new_c;
  pgm_term *el;

  new_c = resr__new_object(pgm_gen_condition);
  if (!new_c)
    return NULL;
  
  if (c -> type == PGM_COND_ATOM)
    {
      pgm_condition *new_sub_c, *old_sub_c;
      
      new_c -> type = PGM_COND_ATOM;
      new_c -> nb_args = 1;
      new_c -> al_args = 1;
      
      new_sub_c = resr__new_object(pgm_condition);
      if (!new_sub_c)
	{
	  resr__free_object(new_c, pgm_condition);
	  return NULL;
	}

      new_c -> args = new_sub_c;
      
      old_sub_c = (pgm_condition *) (c -> args);
      el = resr__new_objects(pgm_term, old_sub_c -> nb_el);
      if (!el)
	{
	  resr__free_object(new_sub_c, pgm_condition);
	  resr__free_object(new_c, pgm_condition);
	  return NULL;
	}
      
      memcpy(el , old_sub_c -> el, 
	     old_sub_c -> nb_el * sizeof(pgm_term));
      new_sub_c -> cond_type = old_sub_c -> cond_type;
      new_sub_c -> nb_el = old_sub_c -> nb_el;
      new_sub_c -> b = old_sub_c -> b;
      new_sub_c -> el = el;
 
      return new_c;
   }
  else
    {
      pgm_gen_condition *new_sub_c, *tmp, *sub_c;
      int i;

      new_sub_c = resr__new_objects(pgm_gen_condition, c -> nb_args);
      if (!new_sub_c)
	{
	  resr__free_object(new_c, pgm_condition);
	  return NULL;
	}

      new_c -> type =  c -> type ;
      new_c -> nb_args = c -> nb_args;
      new_c -> al_args = c -> nb_args;
      new_c -> args = new_sub_c;
      
      for (i = ZERO_INT4 , sub_c = (pgm_gen_condition *) (c -> args); 
	   i < c -> nb_args ; i++, sub_c++)
	{
	  tmp = gen_condition_copy(sub_c);
	  if (!tmp)
	    {
	      resr__free_object(new_c, pgm_condition);
	      resr__free_objects(new_sub_c, pgm_condition, 
				 c -> nb_args);
	      new_sub_c -> al_args = ZERO_INT4;
	      return NULL;
	    }
	  *(new_sub_c + i) = *tmp;
	  resr__free_object(tmp, pgm_gen_condition);
	}
      return new_c;
    }
}
    
/** int operation_copy(target, op) : Copies the content of the 
                   pgm_operation *op into pgm_operation *target.
		   Returns 0 in the case of success and -1 in
		   the case of insufficient memory.                **/

static int operation_copy(target, op)
     pgm_operation *op, *target;
{
  register uint4         i;
           pgm_term     *term;
           pgm_lin_expr *expr;

  expr = resr__new_object(pgm_lin_expr);
  if (!expr)
    return -1;

  target -> lvalue =  op -> lvalue;
  expr -> c = (op -> expr) -> c;
  expr -> nb_el = (op -> expr) -> nb_el;

  if (expr -> nb_el > ZERO_INT4)
    {
      term = resr__new_objects(pgm_term, expr -> nb_el);
      if (!term)
	{
	  resr__free_object(expr, pgm_lin_expr);
	  return -1;
	}
      for (i=ZERO_INT4; i < expr -> nb_el; i++) 
	term[i] = (op -> expr) -> el[i];
      expr -> el = term;
    }
  else
    expr -> el = NULL;
 
  target -> expr = expr;
  return 0;
}

/** pgm_gen_operation *gen_operation_copy(gop) : Returns a pointer to
		     a newly allocated pgm_gen_operation identical to
		     pgm_gen_operation *gop; Returns a null pointer in
		     the case of insuffient memory.                **/

static pgm_gen_operation *gen_operation_copy(gop)
     pgm_gen_operation *gop;
{
           pgm_gen_operation *new_gop;
           pgm_operation     *op;
  register uint4              i, j;
  
  new_gop = resr__new_object(pgm_gen_operation);
  if (!new_gop)
    return NULL;
  op = resr__new_objects(pgm_operation, gop -> nb_op);
  if (!new_gop)
    {
      resr__free_object(new_gop, pgm_gen_operation);
      return NULL;
    }
  
  for (i=ZERO_INT4; i < gop -> nb_op ; i++)
    {
      if (operation_copy(op + i, gop -> op + i) < 0)
	{
	  for (j=ZERO_INT4; j < i; j++)
	    pgm_free_lin_expr(op[i].expr);
	  resr__free_object(new_gop, pgm_gen_operation);
	  resr__free_objects(op, pgm_operation, gop -> nb_op);
	  return NULL;
	}
    }
  new_gop -> op = op;
  new_gop -> nb_op = gop -> nb_op;
  return new_gop;
} 
	
/** int   compose_gop_1(gop, op); Adds the operation op to the
                      set of operations gop such that op is executed
		      after all operations of gop.
		      Does not free op.
		      Returns 0 in the case of success and -1 in the
		      case of insufficient memeory.                **/
static  int  compose_gop_1(gop, op)
     pgm_gen_operation *gop;
     pgm_operation      *op;
{
  register uint4          i, j, k, nb_el;
  register sint4          new_c;
  register int            incr;
  register pgm_operation *op1, *new_op;
  register pgm_lin_expr  *expr, *expr1, *new_expr;
           pgm_term_list *t_list, *t_last;
           pgm_term      *el, term, *new_el;

  expr   = op -> expr; 
  new_c  = expr -> c;
  t_list = NULL;

  for (el = expr -> el, j = ZERO_INT4, nb_el = ZERO_INT4; 
       j < expr -> nb_el ; el++, j++)
    {
      for (i = ZERO_INT4, op1 = gop -> op; i < gop -> nb_op;
	   i++, op1++)
	if (op1 -> lvalue == el -> no) 
	  break;
	  
      if (i == gop -> nb_op)  	    
	{          /* no substitution */
	  incr = insert_term_in_list(&t_list, el);
	  if (incr < 0)
	    {
	      free_term_list(t_list);
	      return -1;
	    }
	  nb_el += incr;
	}
      else
	{ 	    /* substitution */
	  expr1 = op1 -> expr;
	  for (k = ZERO_INT4 ; k < expr1 -> nb_el ; k++)
	    {
	      term.v  = expr1 -> el[k].v * el -> v;
	      term.no = expr1 -> el[k].no;
	      incr = insert_term_in_list(&t_list, &term);
	      if (incr < 0)
		{
		  free_term_list(t_list);
		  return -1;
		}
	      nb_el += incr;	
	    } 
	  new_c = new_c + expr1 -> c * el -> v;
	}
    }

  if (nb_el > 0)
    {
      new_el = resr__new_objects(pgm_term, nb_el);
      if (!new_el)
	{
	  free_term_list(t_list);
	  return -1;
	}
      
      for ( k = ZERO_INT4 ; k < nb_el ; k++)
	{
	  new_el[k] = t_list -> term;
	  t_last = t_list; 
	  t_list = t_list -> next;
	  resr__free_object(t_last, pgm_term_list);
	  t_last = NULL;
	}
    }
  else
    new_el = NULL;

  new_expr = resr__new_object(pgm_lin_expr);
  if (!new_expr)
    {
      resr__free_objects(new_el, pgm_term, nb_el);
      return -1;
    }

  new_expr -> el    = new_el;
  new_expr -> nb_el = nb_el;
  new_expr -> c     = new_c;
      
  for (i = ZERO_INT4, op1 = gop -> op; i < gop -> nb_op; i++, op1++)
    if (op1 -> lvalue == op -> lvalue) 
      break;

  if (i < gop -> nb_op)
    {
      pgm_free_lin_expr(op1 -> expr);
      op1 -> expr = new_expr;
    }
  else
    {
      new_op = resr__resize_objects(gop -> op, pgm_operation, 
				    gop -> nb_op + 1, gop -> nb_op);
      if (!new_op)
	{
	  pgm_free_lin_expr(new_expr);
	  return -1;
	}
      gop -> op = new_op;  
      gop -> nb_op += 1;
      new_op[gop -> nb_op - 1].expr   = new_expr;
      new_op[gop -> nb_op - 1].lvalue = op -> lvalue;
    }
  
  return 0;
}

/** int cond_add_arg(c, cdt) : Adds the subcondition cdt to the
			condition c. Returns 0 in the case of success
			and -1 in the case of insufficient memory or
			bad condition type.  Reports errors.  This
			function does not free cdt!  c must be
			allocated and its type must differ from
			PGM_COND_ATOM.                             **/

static int cond_add_arg(c , cdt)
     pgm_gen_condition *c, *cdt;
{
  pgm_gen_condition *sc;
  pgm_gen_condition *nc;
  
  if (c -> type == PGM_COND_ATOM)
    {
      report_siflash_error(
          "Internal: Bad cond type in cond_add_arg"); 
      return -1;
    }

  if (c -> al_args == ZERO_INT4){
    c -> args = resr__new_objects(pgm_gen_condition, ARGS_INC);
    if (!(c -> args))
      {
	report_siflash_memory_error();
	return -1;
      }
    c -> al_args = ARGS_INC;
    c -> nb_args = ZERO_INT4;
  }

  sc = (pgm_gen_condition *) (c -> args);  
  
  if (c -> nb_args == c -> al_args)
    {
      sc = resr__resize_objects( sc,  pgm_gen_condition, 
			     c -> al_args + ARGS_INC, c -> al_args);
      if (!sc)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      c -> al_args += ARGS_INC;
    }
  
  nc = gen_condition_copy(cdt) ;
  memcpy(sc + c -> nb_args++ , nc, sizeof(pgm_gen_condition));
  resr__free_object(nc, pgm_gen_condition);
  c -> args = sc ;
  return 0;
}

/** int cond_merge_AND(c, c1, c2) : Stores in c a pointer to a newly
			 allocated pgm_gen_condition equivalent to (c1
			 AND c2). c1's and c2's type must be
			 PGM_COND_ATOM or PGM_COND_AND.  Returns 0 in
			 the case of success and -1 in the case of
			 failure.  Does not free c1 nor c2         **/

static int cond_merge_AND(c , c1, c2)
     pgm_gen_condition **c, *c1, *c2;
{
  register int i; 
  pgm_gen_condition *sc_c;
  pgm_gen_condition *nc;

  if ((c1 -> type == PGM_COND_OR) || (c2 -> type == PGM_COND_OR ))
    return -1;
  if (!c)
    return -1;
  
  nc = resr__new_object(pgm_gen_condition);
  if (!nc)
    {
      report_siflash_memory_error();
      return -1;
    }
  nc -> type = PGM_COND_AND; 
  nc -> al_args = ZERO_INT4;
  nc -> nb_args = ZERO_INT4;
  nc -> args = NULL;

  if ( c1 -> type == PGM_COND_ATOM )
    {
      if (cond_add_arg(nc, c1) == -1)
	{
	  pgm_free_gen_condition(nc);
	  return -1;
	}
    }  
  else  /*   ( c1 -> type == PGM_COND_AND ) */ 
    {
      sc_c = (pgm_gen_condition *)  c1 -> args;
      for (i = ZERO_INT4 ; i < c1 -> nb_args ; i++)
	{
	  if (cond_add_arg(nc, sc_c + i) == -1)
	    {
	      pgm_free_gen_condition(nc);
	      return -1;
	    }
	}
    }

    
  if  ( c2 -> type == PGM_COND_ATOM )
    {
      if (cond_add_arg(nc, c2) == -1)
	{
	  pgm_free_gen_condition(nc);
	  return -1;
	}
    }
  else  /*   ( c2 -> type == PGM_COND_AND ) */
    {
      sc_c = (pgm_gen_condition *) c2 -> args;
      for (i = ZERO_INT4 ; i < c2 -> nb_args ; i++)
	{
	  if ( cond_add_arg(nc, sc_c + i) == -1)
	    {
	      pgm_free_gen_condition(nc);
	      return -1;
	    }
	}
    }
    
  *c = nc;
  return 0;
}
		     
/** int pgm_cond_norm_OR(c): Normalize (in Disjunctive Form) the
                        condition c. c's sub-conditions are supposed
                        to be already in the normal form (without any
                        negation) and the type of c is PGM_COND_OR.
                        Returns 0 in the case of success and -1 in the
                        case of failure.                           **/

static int    pgm_cond_norm_OR(c)
     pgm_gen_condition **c;
{
  register int i, j;
  pgm_gen_condition *sc;  /* array of previous subconditions */
  pgm_gen_condition *tmp;  
  pgm_gen_condition *nc;  /* new condition */
  pgm_gen_condition *oc;  /* old condition */

  oc = *c;
  
  nc = resr__new_object(pgm_gen_condition);
  if (!nc)
    return -1;

  nc -> type = PGM_COND_OR;
  nc -> nb_args = ZERO_INT4;
  nc -> al_args = ZERO_INT4;
  nc -> args = NULL;
	
  sc = (pgm_gen_condition *) oc -> args;
      
  for (i = ZERO_INT4; i < oc -> nb_args ; i++, sc++ )
    {      
      if (sc -> type == PGM_COND_OR)
	{
	  /* Adds the sub_conditions of the old subcondition
	     as sub-conditions of the DNF condition */ 

	  for (j = ZERO_INT4; j < sc -> nb_args ; j++)
	    {
	      tmp = (pgm_gen_condition *) (sc -> args) + j;
	      if (cond_add_arg(nc, tmp) == -1)
		{
		  pgm_free_gen_condition(oc);
		  return -1;
		}
	    }
	}
      else if ((sc -> type == PGM_COND_AND) ||
	       (sc -> type == PGM_COND_ATOM)) 
	{
	  /* Adds directly the old subcondition
	     as a sub-condition of the DNF condition */ 
	  if (cond_add_arg(nc, sc) == -1)
	    {
	      pgm_free_gen_condition(oc);
	      return -1;
	    }
	}
    }
    
  pgm_free_gen_condition(oc);
  *c = nc;
      
  return 0;	
}

/** int pgm_cond_norm_AND(c): Normalize (in Disjunctive Form) the
                        condition c. c's sub-conditions are supposed
                        to be already in the normal form (without any
                        negation) and the type of c is PGM_COND_AND.
                        Returns 0 in the case of success and -1 in the
                        case of failure.  The function is called
                        recursively. At each step, all subconditions
                        of type PGM_COND_ATOM and PGM_COND_AND are
                        merged and subconditions of type PGM_COND_OR
                        are merged by pair.  For each pair, one
                        applies de Morgan's law : ex: if A = x OR y, B
                        = z, C = u AND v, D = w, and the condition is:
                        A AND B AND C AND D) => (A AND B) AND (C AND
                        D) => nc = [ (x AND z) OR (y AND z) ] AND [ u
                        AND v AND w ] => pgm_cond_normalise(nc) => nc
                        = (x AND z AND u AND v AND w) OR (y AND z AND
                        u AND v AND w).                            **/

static int    pgm_cond_norm_AND(c)
     pgm_gen_condition **c;
{
  register int       i, j, k; 
  pgm_gen_condition *c1, *c2;
  pgm_gen_condition *sc;       /* array of previous subconditions */
  pgm_gen_condition *nc;       /* new general condition */
  pgm_gen_condition *oc;       /* old condition */
  pgm_gen_condition *merged_sc, *tmp; 
  pgm_gen_condition *new_sc ;  /* new sub_condition */
  pgm_gen_condition *sc_c1 ;   /* sub_conditions of c1 */
  pgm_gen_condition *sc_c2 ;   /* sub_conditions of c2 */   
  
  merged_sc = NULL;
  c2 = NULL;
  oc = *c;
  sc = oc -> args;
  nc = resr__new_object(pgm_gen_condition);    
  if (!nc)
    return -1;
  nc -> type = PGM_COND_AND;
  nc -> al_args = ZERO_INT4;
  nc -> nb_args = ZERO_INT4;
  nc -> args = NULL;
   
  for (i = ZERO_INT4; i < oc -> nb_args; i++ )
    { 
      c1 = gen_condition_copy((pgm_gen_condition *) (sc + i));
      if (!c1)
	{
	  if (c2)
	    pgm_free_gen_condition(c2);
	  pgm_free_gen_condition(nc);
	  pgm_free_gen_condition(oc);
	  return -1;
	}
      
      if ( c1 -> type == PGM_COND_OR )
	{
	  if (!c2)
	   {
	     c2 = c1;
	     continue;
	   }

	  new_sc = resr__new_object(pgm_gen_condition);
	  if (!new_sc)
	    {
	    pgm_free_gen_condition(c2);
	    pgm_free_gen_condition(c1);
	    pgm_free_gen_condition(oc);
	    pgm_free_gen_condition(nc);
	    report_siflash_memory_error();
	    return -1;
	    }
	  new_sc -> type = PGM_COND_OR;
	  new_sc -> al_args = ZERO_INT4;
	  new_sc -> nb_args = ZERO_INT4;
	  new_sc -> args = NULL;
	  
	  sc_c1 = (pgm_gen_condition *) c1 -> args; 
	  sc_c2 = (pgm_gen_condition *) c2 -> args; 
	  for (j = ZERO_INT4 ; j < c1 -> nb_args ; j++)
	    for (k = ZERO_INT4 ; k < c2 -> nb_args ; k++)
	      {
		if ((cond_merge_AND(&tmp, sc_c1 + j, sc_c2 + k) < 0)
		    || (cond_add_arg(new_sc, tmp) < 0 ))
		  {
		    pgm_free_gen_condition(c2);
		    pgm_free_gen_condition(c1);
		    pgm_free_gen_condition(oc);
		    pgm_free_gen_condition(nc);
		    pgm_free_gen_condition(new_sc);
		    return -1;
		  }
		pgm_free_gen_condition(tmp);
	      }

	  pgm_free_gen_condition(c1); 
	  pgm_free_gen_condition(c2); c2 = NULL;

	  if (cond_add_arg(nc, new_sc) < 0 )
	    {
	      pgm_free_gen_condition(oc);
	      pgm_free_gen_condition(nc);
	      pgm_free_gen_condition(new_sc);
	      return -1;
	    }
	  pgm_free_gen_condition(new_sc);
	  
	} /* if PGM_COND_OR */      
      
      else  /* PGM_COND_AND or PGM_COND_ATOM */
    	{
	  if (!merged_sc)
	    {
	      merged_sc = c1;
	      continue;
	    }
	  
	  if (cond_merge_AND(&tmp, merged_sc, c1) < 0)
		{
		  pgm_free_gen_condition(merged_sc);
		  if (c2)
		    pgm_free_gen_condition(c2);
		  pgm_free_gen_condition(c1);
		  pgm_free_gen_condition(oc);
		  pgm_free_gen_condition(nc);
		  return -1;
		}
	  pgm_free_gen_condition(merged_sc);
	  pgm_free_gen_condition(c1);
	  merged_sc = tmp;
	}	    
    } /* for */
 
 if (c2)  /* number of OR-subconditions is odd. */
    {
      if (merged_sc)
	{
	  new_sc = resr__new_object(pgm_gen_condition);
	  if (!new_sc)
	    {
	      pgm_free_gen_condition(c2);
	      pgm_free_gen_condition(merged_sc);
	      pgm_free_gen_condition(oc);
	      pgm_free_gen_condition(nc);	 		   
	      report_siflash_memory_error();
	      return -1;
	    }
	  new_sc -> type = PGM_COND_OR;
	  new_sc -> al_args = ZERO_INT4;
	  new_sc -> nb_args = ZERO_INT4;
	  new_sc -> args = NULL;
	
	  for ( i = ZERO_INT4 ; i < c2 -> nb_args ; i++)
	    {
	      if ((cond_merge_AND(&tmp, (pgm_gen_condition *) 
				 (c2 -> args) + i, merged_sc) < 0)
		  ||  (cond_add_arg(new_sc, tmp) < 0 ))
		{
		  pgm_free_gen_condition(merged_sc);
		  pgm_free_gen_condition(c2);
		  pgm_free_gen_condition(new_sc);
		  pgm_free_gen_condition(oc);
		  pgm_free_gen_condition(nc);	 		   
		  return -1;
		}
	      pgm_free_gen_condition(tmp);
	    }
	  pgm_free_gen_condition(merged_sc);
	  pgm_free_gen_condition(c2);
	  if (cond_add_arg(nc, new_sc) < 0 )
	    {
	      pgm_free_gen_condition(new_sc);
	      pgm_free_gen_condition(oc);
	      pgm_free_gen_condition(nc);	
	      return -1;
	    }
	  pgm_free_gen_condition(new_sc);
	}
      else
	{
	  if (cond_add_arg(nc, c2) < 0 )
	    {
	      pgm_free_gen_condition(c2);
	      pgm_free_gen_condition(oc);
	      pgm_free_gen_condition(nc);	
	      return -1;
	    }
	  pgm_free_gen_condition(c2);
	}
    }
  else if (merged_sc)
    {
      if (cond_add_arg(nc, merged_sc) < 0 )
	return -1;
      pgm_free_gen_condition(merged_sc);
    }  
 
 pgm_free_gen_condition(oc);
 
 if ( nc -> nb_args == 1) /* only 1 subcondition */
    {
      *c = gen_condition_copy(nc -> args) ;
      if (!(*c))
	{
	  pgm_free_gen_condition(nc);
	  report_siflash_memory_error();
	}
      
      pgm_free_gen_condition(nc);
      return 0;
    }
 else
   *c = nc ;
   
  return  pgm_cond_norm_AND(c);
}

/** int check_cond4meta(c) : Checks that the condition c is a
                  condition that can be part of a transition included
                  in a meta-transition.  Returns -1 if c is rejected
                  and 0 if c is accepted.                          **/

static int check_cond4meta(c)
     pgm_gen_condition *c;
{
  if (c -> type == PGM_COND_OR)
    return -1;
       
  return 0;
}

/** int insert_term_in_list(tl, t) : Inserts the term t in the list tl.
                  If the variable of the term t is already present in
                  one term of the list tl, then the coefficient is add
                  to the term found. If it is not the case, a new
                  element of the list is created and the term t is
                  inserted in the new element.  It returns 0 if the
                  variable of t is found in the list, -1 if there is
                  not enough memory to create a new element, and 1 if
                  a new element is created successfully.           **/
		  
static int insert_term_in_list(tl, term)
     pgm_term_list **tl;
     pgm_term *term;
{
  pgm_term_list *t_init, *t_current, *t_last;
  t_last = NULL;

  for (t_current = t_init = *tl ; t_current ;
       t_current = t_current -> next)
    {
      if (t_current -> term.no == term -> no)
	{
	  t_current -> term.v += term -> v;
	  return 0;
	}
      t_last = t_current;
    }
  
  if (!t_current)
    {
      t_current  = resr__new_object(pgm_term_list);
      if (!t_current)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      
      t_current -> term = *term;
      t_current -> next = *tl;
      *tl = t_current;
    }
  
  return 1;
}

/** void free_term_list(tl) : Frees the list of terms tl.
    Does not report error.                           **/
static void free_term_list(tl)
     pgm_term_list *tl;
{
  register pgm_term_list *t_last;
  while(tl)
    {
      t_last = tl; 
      tl = tl -> next;
      resr__free_object(t_last, pgm_term_list);
      t_last = NULL;
    }
}

/** int  check_init_states() : Checks whether there are more than
                  one processes having an atomic initial state.
		  Returns -1 in this case and 0 otherwise.         **/

static int    check_init_states()
{
  register uint4 i, s, nb_atomic_init;
  register pgm_process *pr;

  nb_atomic_init = 0;
  for (i = ZERO_INT4; i < the_program.nb_proc; i++)
    {
      pr = the_program . processes + i;
      s = pr -> leaf_no;
      if (pr -> states[s] . atomic)
	nb_atomic_init++;
    }

  return (nb_atomic_init > 1 ? -1 : 0);
}

/** int generate_process_instance(pr, i) : Generates a new process
                  identical to the process of index pr and
                  corresponding to the ith instance of this process.
                  Returns 0 in the case of success and -1 in the case
                  of insufficient memory.                          **/

static int generate_process_instance(pr, i)
     uint4 pr;
     uint4 i;
{
  register pgm_process *p, *p_source;
  register uint4        n, *root_states,  nb_vars;
	   uint4       *convert_table;
  register char        *name;
           char         name_copy[256];

  n = the_program.nb_proc; 
  p = resr__resize_objects(the_program.processes, pgm_process, 
			   n + 1, n);
  if (!p)
    return -1;
  
  the_program.processes = p;
  the_program.nb_proc = n + 1;

  p_source = the_program.processes + pr;

  p = the_program.processes + n;
  p -> name         =  NULL;
  p -> nb_instances = 1;
  p -> nb_states    = ZERO_INT4;
  p -> al_states    = ZERO_INT4;
  p -> leaf_no      = p_source -> leaf_no;
  p -> dummy_no     = p_source -> dummy_no;
  p -> nb_ids       = p_source -> nb_ids;
  p -> states       = NULL;
  p -> has_leaf_no  = p_source -> has_leaf_no;
  p -> nb_root_states = ZERO_INT4;
  p -> root_states  = NULL;
  p -> al_root_states = 0;
  p -> local_vars   = NULL;

  sprintf(name_copy,"%s_%d", p_source -> name,i);
  name = resr__new_objects(char, strlen(name_copy) + 1);
  if (!name)
    return -1;
  strcpy(name, name_copy); 
  p -> name = name;

  nb_vars = the_program.nb_vars;

  if (generate_local_vars_copy(p_source, &p -> local_vars, 
			       &convert_table) < 0)
    return -1;

  if (generate_states_copy(p_source, &p -> states, &p -> nb_states,
			   &p -> al_states, convert_table) < 0)
    {
      resr__free_objects(convert_table, uint4, nb_vars);
      return -1;
    }
  
  resr__free_objects(convert_table, uint4, nb_vars);

  if (p_source -> nb_root_states > 0)
    {
      root_states = resr__new_objects(uint4, 
				      p_source -> nb_root_states);
      if (!root_states)
	return -1;

      memcpy(root_states, p_source -> root_states, 
	     sizeof(uint4) * p_source -> nb_root_states);
      p -> root_states  = root_states;
    } 
  else
    p -> root_states = NULL;
  
  p -> nb_root_states = p_source -> nb_root_states;  
  p -> al_root_states = p_source -> nb_root_states;

  return 0;
}

/** int generate_local_vars_copy(pr, vars, convert_table) : Generates
                  a new variable for each local variable of process pr
                  and assemble them into a linked list stored in
                  vars. Also geenrates a conversion table such that at
                  index i corresponds either the value i, if the
                  variable i is not a local variable of the process
                  pr, or the index of the new variable corresponding
                  to variable i.  Returns 0 in the case of success and
                  -1 in the case of insufficient memory.           **/

static int generate_local_vars_copy(pr, vars, convert_table)
     pgm_process *pr;
     pgm_variable **vars;
     uint4 **convert_table;
{
  register uint4 i, *table, nb_vars;
  register pgm_variable *next, *copy, *v, *previous, *first;
           char *name;

  nb_vars = the_program . nb_vars;
  table = resr__new_objects(uint4, nb_vars);
  if (!table)
    return -1;

  for (i = ZERO_INT4 ; i < nb_vars ; i++)
    table[i] = i;

  next = pr -> local_vars;
  if (!next)
    {
      *convert_table = table;
      *vars = NULL;
      return 0;
    }

  previous = first  = NULL;
  while (next)
    {
      v = next;
      next = v -> next;
      if (v -> is_ignored > 0)
	continue;
      copy = resr__new_object(pgm_variable);
      if (!copy)
	{
	  next = first;
	  while (next)
	    {
	      v = next;
	      next = v -> next;
	      free_variable(v);
	    }
	  resr__free_objects(table, uint4, nb_vars);
	  return -1;
	}

      if (v -> name)
	{
	  name = resr__new_objects(char, strlen(v -> name) +1);
	  if (!name)
	    {
	      next = first;
	      while (next)
		{
		  v = next;
		  next = v -> next;
		  free_variable(v);
		}
	      resr__free_object(copy, pgm_variable);
	      resr__free_objects(table, uint4, nb_vars);
	      return -1;
	    }
	  strcpy(name, v -> name);
	  copy -> name = name;
	}
      else
	copy -> name = NULL;

      copy -> no   = the_program.nb_vars++;
      table[v -> no] = copy -> no;
      copy -> init = v -> init;
      copy -> is_ignored = v -> is_ignored;
      copy -> is_parameter = v -> is_parameter;
      if (v -> is_parameter){
	if (set__add(parameter_set, copy -> no) < 0)
	  {
	    next = first;
	    while (next)
	      {
		v = next;
		next = v -> next;
		free_variable(v);
	      }
	    free_variable(copy);
	    resr__free_objects(table, uint4, nb_vars);
	    return -1;
	  }
      }
      copy -> next = NULL;
      if (!previous)
	first = previous = copy;
      else
	{
	  previous -> next = copy;
	  previous = copy;
	}
    }
  *vars = first;
  *convert_table = table;
  return 0;
}

/** int generate_states_copy(pr, states, nb_states, al_states,
                 convert_table): Generates a copy of the states of
		 process pr in states. Stores the number of states
		 and the number of states allocated in respectively
		 nb_states and al_states. convert_table contains the
		 conversion table for the variable numbers. This
		 table is used for the generation of the
		 pre-condition and assignment of the transitions. 
		 Returns 0 in the case of success and -1 in the case
		 of insufficient memmory.                          **/

static int generate_states_copy(pr, states, nb_states, al_states,
				convert_table)
     pgm_process  *pr;
     pgm_state   **states;
     uint4        *nb_states, *al_states, *convert_table;
{
  register pgm_state  *new_states, *st_cpy, *st;
  register uint4       i, j, *subs;
           char       *name;

  new_states = resr__new_objects(pgm_state, pr -> nb_states);
  if (!new_states)
    return -1;

  for (i = ZERO_INT4 ; i < pr -> nb_states ; i++)
    {
      st_cpy = new_states + i;
      st = pr -> states + i;
      if (st -> name)
	{
	  name = resr__new_objects(char, strlen(st -> name) + 1);
	  if (!name)
	    {
	      for (j = ZERO_INT4 ; j < i ; j++)
		  pgm_free_state_elt(new_states + j); 
	      resr__free_objects(new_states, pgm_state, 
				 pr -> nb_states);
	      return -1;
	    }
	  strcpy(name, st -> name);
	  st_cpy -> name = name;
	}
      else
	st_cpy -> name = NULL;
      st_cpy -> no = st -> no; 
      st_cpy -> leaf_no = st -> leaf_no; 
      st_cpy -> has_leaf_no = st -> has_leaf_no; 
      st_cpy -> id = st -> id;
      st_cpy -> atomic = st -> atomic;
      st_cpy -> init = st -> init;
      st_cpy -> reachable = st -> reachable;
      st_cpy -> fusionable = st -> fusionable;
      if (st -> nb_substates > 0)
	{
	  subs = resr__new_objects(uint4, st -> nb_substates);
	  if (!subs)
	    {
	      pgm_free_string(st_cpy -> name);
	      for (j = ZERO_INT4 ; j < i ; j++)
		pgm_free_state_elt(new_states + j); 
	      
	      resr__free_objects(new_states, pgm_state, pr -> nb_states);
	      return -1;
	    }
	  memcpy(subs, st -> substates, 
		 sizeof(uint4) * st -> nb_substates);
	  st_cpy -> substates = subs;
	}
      else
	st_cpy -> substates  = NULL;

      st_cpy -> nb_substates = st -> nb_substates;
      st_cpy -> al_substates = st -> nb_substates;
      st_cpy -> trans        = NULL;
      st_cpy -> nb_trans     = ZERO_INT4;
      st_cpy -> al_trans     = ZERO_INT4;
      
      if (generate_transitions_copy(st, &st_cpy -> trans, &st_cpy ->
				    nb_trans, &st_cpy -> al_trans,
				    convert_table) < 0)
	{
	  for (j = ZERO_INT4 ; j <= i ; j++)
	      pgm_free_state_elt(new_states + j); 
	  resr__free_objects(states, pgm_state, pr -> nb_states);
	  return -1;
	}
    }
  *states    = new_states;
  *nb_states = pr -> nb_states;
  *al_states = pr -> nb_states;
  return 0;
}

/** int generate_transitions_copy(st, trans, nb_trans, al_trans,
		 convert_table) : Generates a copy of the transitions
		 of the state st in trans. Stored the number of
		 transitions and the number of transitions allocated
		 in respectively nb_trans and al_trans. convert_table
		 contains the conversion table for the variable
		 numbers. This table is used for the generation of the
		 pre-condition and assignment of the transitions.
		 Returns 0 in the case of success and -1 in the case
		 of insufficient memmory.                          **/

static int generate_transitions_copy(st, trans, nb_trans, al_trans,
                 convert_table)
     pgm_state   *st;
     pgm_transition **trans;
     uint4       *nb_trans, *al_trans, *convert_table;
{
  register uint4  i;
  pgm_transition *new_trans, *tr_cpy;

  new_trans = resr__new_objects(pgm_transition, st -> nb_trans);
  if (!new_trans)
    return -1;

  for (i = ZERO_INT4 ; i < st -> nb_trans ; i++)
    {
      tr_cpy = transition_copy(st -> trans + i);
      if (!tr_cpy)
	{
	  resr__free_objects(new_trans, pgm_transition,
			      st -> nb_trans); 
	  return -1;
	}
	
      if (tr_cpy -> pre_cdt)
	convert_gen_condition_vars_no(tr_cpy -> pre_cdt, 
				      convert_table);
      if (tr_cpy -> asgn)
	convert_gen_operation_vars_no(tr_cpy -> asgn,  
				      convert_table);
      new_trans[i] = *tr_cpy;
      resr__free_object(tr_cpy, pgm_transition);
    }
  
  *trans = new_trans;
  *nb_trans = st -> nb_trans;
  *al_trans = st -> nb_trans;
  return 0;
}

/** void convert_gen_condition_vars_no(gc, convert_table) : Converts
                 the variable nos appearing in the pgm_gen_condition
                 gc according to the conversion table convert_table.
                 Does not report errors.                           **/

static void convert_gen_condition_vars_no(gc, convert_table)   
     pgm_gen_condition *gc;
     uint4 *convert_table;
{
  register pgm_condition *c;
  register uint4 i;
  register pgm_term *el;
  register pgm_gen_condition *subc;

  if (gc -> type == PGM_COND_ATOM)
    {
      c =  (pgm_condition *) (gc -> args);
      for (i = ZERO_INT4 ; i < c -> nb_el ; i++)
	{
	  el = c -> el + i;
	  el -> no = convert_table[el -> no];
	}
    }
  else
    {
      for (i = ZERO_INT4; i < gc -> nb_args; i++)
	{
	  subc = (pgm_gen_condition *) gc -> args + i;
	  convert_gen_condition_vars_no(subc, convert_table);
	}
    }
} 

/** void convert_gen_operation_vars_no(gop, convert_table) : Converts
                 the variable nos appearing in the pgm_gen_operation
                 gop according to the conversion table convert_table.
                 Does not report errors.                           **/

static void convert_gen_operation_vars_no(gop, convert_table)   
     pgm_gen_operation *gop;
     uint4 *convert_table;
{
  register pgm_operation *op;
  register uint4 i, j;
  register pgm_term *el;
  register pgm_lin_expr *lexp;

  for (i = ZERO_INT4; i < gop -> nb_op; i++)
    {
      op = gop -> op + i;
      op -> lvalue = convert_table[op -> lvalue];
      lexp = op -> expr;
      for (j = ZERO_INT4; j < lexp -> nb_el; j++)
	{
	  el = lexp -> el + j;
	  el -> no = convert_table[el -> no];
	}
    }
}

/****  Public functions.                                         ****/

/**  int  pgm_init()  :  Initializes the data structures used by this
                    module and creates an empty initial program. This
                    function returns 0 in the case of success, and -1
                    if there is not enough memory.
		    Reports the error.                             **/

int  pgm_init()
{
  global_vars = hash__new_empty(PGM_HASH_SIZE);
  if (!global_vars)
    {
      report_siflash_memory_error();
      return -1;
    }
  
  global_const = hash__new_empty(PGM_HASH_SIZE);
  if (!global_const)
    {
      report_siflash_memory_error();
      hash__free(global_vars, NULL, NULL);
      return -1;
    }
  
  id_table = hash__new_empty(PGM_HASH_SIZE);
  if (!id_table)
    {
      report_siflash_memory_error();
      hash__free(global_const, NULL, NULL);
      hash__free(global_vars, NULL, NULL);
      return -1;
    }
  
  parameter_set = set__new_empty();
  if (!parameter_set)
    {
      report_siflash_memory_error();
      hash__free(global_vars, NULL, NULL);
      hash__free(global_const, NULL, NULL); 
      hash__free(id_table, NULL, NULL);
      return -1;
    }

  global_types = hash__new_empty(PGM_HASH_SIZE);
  if (!global_types)
    {
      report_siflash_memory_error();
      hash__free(global_vars, NULL, NULL);
      hash__free(global_const, NULL, NULL); 
      hash__free(id_table, NULL, NULL);
      set__free(parameter_set);
      return -1;
    }

  local_vars              = NULL;
  local_types             = NULL;
  local_const             = NULL;
  current_process         = ZERO_INT4;
  nb_colls                = ZERO_INT8;
  nb_ins                  = ZERO_INT8;
  meta_local              = 0;
  current_control_p       = NULL;
  the_program.nb_proc     = ZERO_INT4;
  the_program.nb_vars     = ZERO_INT4;
  the_program.processes   = NULL;
  the_program.global_vars = NULL;

  return 0;
}

/**  int  pgm_finish()  :  Unallocates the data structures used by 
                    this module and optimizes the program that has
                    been generated. This function returns 0 in the
                    case of success, and -1 in the case of an error
                    (an error message is then output).             **/

int  pgm_finish()
{
  if (global_vars)
    hash__free(global_vars, (void (*)(void *)) pgm_free_string, NULL);

  if (global_const)
    hash__free(global_const, (void (*)(void *)) pgm_free_string,
	       (void (*)(void *)) free_constant);

  if (global_types)
    hash__free(global_types, (void (*)(void *)) pgm_free_string, 
	       (void (*) (void *)) free_uint1);
 
  if (local_vars)
    hash__free(local_vars, (void (*)(void *)) pgm_free_string, NULL);

  if (local_const)
    hash__free(local_const, (void (*)(void *)) pgm_free_string,
	       (void (*)(void *)) free_constant);

  if (local_types)
    hash__free(local_types, (void (*)(void *)) pgm_free_string, 
	       (void (*) (void *)) free_uint1);

  bytes__prepare_free(sizeof(id_table_payload));
  if (id_table)
    hash__free(id_table, (void (*)(void *)) pgm_free_string,
        (void (*)(void *)) bytes__free); 
  
  if (parameter_set)
    set__free(parameter_set);

  if (label_trans)
    pgm_free_string(label_trans);
  if (asgn)
    pgm_free_gen_operation(asgn);
  if (pre_cond)
    pgm_free_gen_condition(pre_cond);

  pgm_free_fpar();

  free_the_program();

  return 0;
}

/**  int  pgm_end()  :  Signals the end of the syntatic parsing. The
                    purpose of this function is to optimize the
                    generated program as well as to start the end
                    application. The function returns 0 in the case of
                    success, and -1 in the case of an error (a message
                    is then output).                               **/

int  pgm_end()
{
  if (!(the_program.nb_vars))
    {
      report_siflash_error(
"Translator error: The program has no variable");
      return -1;
    }

  if (reference_metas() < 0)
    return -1;

  if (out_init(&the_program) < 0)
    return -1;

  if (check_init_states () < 0)
    {
      report_siflash_error(
"Translator error: Two or more processes have an atomic initial state");
      return -1;
    }

  if (expl_init() < 0)
    return -1;

  return 0;
}

/** int pgm_new_state(name, s) : Creates a new pgm_state.  The name
		    name is associated to the state. The index of the
		    state is stored in *s if s is not null.  
		    The state is marked as non atomic and not
		    initial. 		    
		    The function returns 0 in the case of success, and
		     -1 if there is not enough memory or if there is
		    already a label of name name in the current
		    process, and reports the error.  Frees the name
		    name.                                          **/
int  pgm_new_state(name, s)
  char   *name; 
  uint4  *s;
{
  register uint4    l;
  register pgm_state *st;
  register pgm_process *pr;
  id_table_payload *u;
  char tag[256];
  void **r;

  pr = the_program.processes + current_process;
  st = grow_table((void **) (&pr -> states), &pr -> nb_states, 
		  &pr -> al_states, sizeof(pgm_state));
  if (!st)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
      return -1;
    }

  st -> nb_substates = ZERO_INT4;
  st -> al_substates = ZERO_INT4;
  st -> nb_trans     = ZERO_INT4;
  st -> al_trans     = ZERO_INT4;
  st -> substates    = NULL;
  st -> trans        = NULL;
  st -> atomic       = 0;
  st -> init         = 0;
  st -> name         = name;
  st -> reachable    = 0;
  st -> fusionable   = 0;
  st -> no           = pr -> nb_states -1;
  st -> has_leaf_no  = 0; 

  if (s)
    *s = pr -> nb_states - 1;
  if (name)
    {
      u = resr__new_object(id_table_payload);
      if (!u)
	{
	  resr__free_object(s, pgm_state);
	  report_siflash_memory_error();
	  return -1;
	}
      
      u -> trans_defined = 0;
      u -> state_defined = 1;
      u -> state         = pr -> nb_states -1;
      u -> process_defined = 1;
      u -> process       = current_process;
      
      sprintf(tag, "%.127s@", pr -> name); 
      l = strlen(tag);
      sprintf(tag + l, "%.127s", name);
      
#if LASH_CHECK_LEVEL >= 1
      if (hash__insert_string(id_table, tag, &r, &nb_colls, 
			      &nb_ins) < 0)
#else
	if (hash__insert_string(id_table, tag, &r) < 0)
#endif
	  {
	    report_siflash_memory_error();
	    return -1;
	  }
      
      if (!r)
	{
	  char line[256];
	  
	  resr__free_object(u, uint4);
	  sprintf(line, 
"Compiler error: Duplicate label '%.32s' in process '%.32s'",
		  name, pr -> name);
	  report_siflash_error(line);
	  return -1;
	} 
      
      *r = u;
    }
  return 0;
}

/** pgm_transition *pgm_create_transition(name, type): Creates a new 
                    transition. The destination is specified by the
		    parameters name and type.  Either the destination
		    name is given in name or type indicates the
		    special type of the transition.
		    The pre-condition and assignment  were stored in
		    global variables. 
		    Frees the corresponding global variables.  Returns
		    a pointer to the newly allocated transition
		    structure in the case of success and NULL in the
		    case of error.  Reports errors.                **/
pgm_transition *pgm_create_transition(name, type)
     char *name;
     uint1 type;
{
  pgm_transition *pt;

  if (name && type != PGM_TRANS_TYPE_NAME)
    {
      report_siflash_error("Conflicting information for transition");
      return NULL;      
    }

  pt = resr__new_object(pgm_transition);
  if (!pt)
    {
      report_siflash_memory_error();
      return NULL;
    }

  pt -> dest_type    = type;
  pt -> dest_name    = name;
  pt -> label        = label_trans;
  label_trans        = NULL;
  pt -> pre_cdt      = pre_cond;
  pre_cond           = NULL;
  pt -> asgn         = asgn;
  asgn               = NULL;
  pt -> process_no   = current_process;
  return pt;
}

/** int pgm_add_transition(s, tr): Adds to the state s the transition
		    tr.  The transition tr is freed.  The function
		    returns 0 in the case of success, and -1 if there
		    is not enough memory and reports the error.    **/

int  pgm_add_transition(s, tr)
     uint4 s;
     pgm_transition *tr;
{
  char *name_cpy;
  pgm_transition *pt;
  pgm_process *pr;
  pgm_state *st;
  id_table_payload *u;
  char tag[256];
  void **r;
  uint4 l;

  pr =  the_program . processes + current_process;
  st = pr -> states + s;
  pt = grow_table((void **) (&st -> trans), &st -> nb_trans, 
      &st -> al_trans, sizeof(pgm_transition));
  if (!pt)
    {
      free_transition_elt(tr);
      resr__free_object(tr, pgm_transition);
      report_siflash_memory_error();
      return -1;
    }
  *pt = *tr;
  resr__free_object(tr, pgm_transition);

  if ((pt -> dest_name == NULL) && 
      (pt -> dest_type == PGM_TRANS_TYPE_SELF))
    {
      name_cpy = resr__new_objects(char, strlen(st -> name) + 1);
      if (!name_cpy) {
	report_siflash_memory_error();
	return -1;
      }
      strcpy(name_cpy, st -> name);
      pt -> dest_name = name_cpy;
    }

  if (pt -> label)
    {
      u = resr__new_object(id_table_payload);
      if (!u)
	{
	  report_siflash_memory_error();
	  return -1;
	}
  
      u -> trans_defined = 1;
      u -> trans         = st -> nb_trans - 1; 
      u -> state_defined = 1;
      u -> state         = s;
      u -> process_defined = 1;
      u -> process       = current_process;
  
      sprintf(tag, "%.127s@", pr -> name); 
      l = strlen(tag);
      sprintf(tag + l, "%.127s", pt -> label);
      
#if LASH_CHECK_LEVEL >= 1
      if (hash__insert_string(id_table, tag, &r, &nb_colls, 
			      &nb_ins) < 0)
#else
	if (hash__insert_string(id_table, tag, &r) < 0)
#endif
	  {
	    report_siflash_memory_error();
	    return -1;
	  }
      
      if (!r)
	{
	  char line[256];
	  
	  sprintf(line, 
"Compiler error: Duplicate label '%.32s' in process '%.32s'",
		  pt -> label, pr -> name);
	  report_siflash_error(line);
	  return -1;
	} 
      
      *r = u;  
    }
  return 0;
}

/** int pgm_add_substate(s, subs): Add the substate subs to the state
		    s.  Returns 0 in the case of success and -1 in the
		    case of insufficient memory.  Reports errors.  **/

int    pgm_add_substate(s, subs)
     uint4 s, subs;
{
  pgm_state *st;
  pgm_process *pr;
  uint4 *sst;

  pr = the_program . processes + current_process;
  st = pr -> states + s;
  
  sst = grow_table((void **) (&st -> substates), &st -> nb_substates, 
      &st -> al_substates, sizeof(uint4));
  if (!sst)
    {
      report_siflash_memory_error();
      return -1;
    }
  *sst = subs;

  return 0;
}

/** int pgm_add_state(s) : Adds the state s to the current process.
		    Returns 0 in the case of success and -1 in the
		    case of insufficient memory.  Reports error.   **/

int    pgm_add_state(s)
     uint4 s;
{
  uint4 *st;
  pgm_process *pr;

  pr = the_program.processes + current_process;
  st = grow_table((void **) (&pr -> root_states), 
		  &pr -> nb_root_states, 
		  &pr -> al_root_states, sizeof(uint4));
  if (!st)
    {
      report_siflash_memory_error();
      return -1;
    }
  *st = s;
  return 0;
}

/** void pgm_set_state_options(s, init, atomic) : Sets the state
		    options init and atomic of state s to init and
		    atomic respectively.  Does not report errors.  **/

void pgm_set_state_options(s, init, atomic)
     uint4 s;
     uint1 init, atomic;
{
  register pgm_process *pr;
  register pgm_state *st;

  pr = the_program . processes + current_process;
  st = pr -> states + s;
  st -> atomic = atomic;
  st -> init = init;
}

/** int pgm_new_process(name, nb) : Creates a new process, which
		    becomes the current one.  Frees the string name.
		    nb is the number of instances of this process at
		    start time.  The function returns 0 in the case of
		    success, and -1 if there is not enough memory.
		    Reports the error.                             **/

int  pgm_new_process(name, nb)
     char *name;
     uint4 nb;
{
  register pgm_process *p;
  register uint4        n;

  n = the_program.nb_proc; 
  p = resr__resize_objects(the_program.processes, pgm_process, n + 1, 
          n);
  if (!p)
    {
      report_siflash_memory_error();
      return -1;
    }
  
  the_program.processes = p;
  the_program.nb_proc = n + 1;
  current_process = n;

  p += n;
  p -> name       = name;
  p -> nb_instances = nb;
  p -> nb_states  = ZERO_INT4;
  p -> al_states  = ZERO_INT4;
  p -> nb_root_states  = ZERO_INT4;
  p -> al_root_states  = ZERO_INT4;
  p -> root_states     = NULL;
  p -> nb_ids     = ZERO_INT4;
  p -> states     = NULL;
  p -> local_vars = NULL;
  
  local_vars = hash__new_empty(PGM_HASH_SIZE);
  if (!local_vars)
    {
      report_siflash_memory_error();
      return -1;
    }

  local_const = hash__new_empty(PGM_HASH_SIZE);
  if (!local_const)
    {
      report_siflash_memory_error();
      hash__free(local_vars, (void (*)(void *)) pgm_free_string, 
		 NULL);
      return -1;
    }
  
  local_types = hash__new_empty(PGM_HASH_SIZE);
  if (!local_types)
    {
      hash__free(local_vars, (void (*)(void *)) pgm_free_string, 
		 NULL);
      hash__free(local_const, (void (*)(void *)) pgm_free_string,
		 NULL);
      return -1;
    }

  /* Creates a dummy state for the STOP transition. */

  if (pgm_new_state(NULL, & p -> dummy_no) < 0)
    {
      hash__free(local_vars, (void (*)(void *)) pgm_free_string,
		 NULL);
      hash__free(local_const, (void (*)(void *)) pgm_free_string,
		 NULL);
      return -1;
    }

  return 0;
}

/** int pgm_declare_local_variable(name, is_parameter, ignored, val) :
		    Declares a new local variable whose identifier is
		    name, and unallocates the memory in which name is
		    stored.  This variable is a free parameter
		    (non-deterministic value) if is_parameter > 0.
		    The intial value is val (if not a parameter) and
		    the variable is ignored (unaccepted type) if
		    ignored > 0.  Returns 0 in the case of success,
		    and -1 in the case of an error (a message is then
		    output).                                       **/

int  pgm_declare_local_variable(name, is_parameter, ignored, val)
  char *name;
  uint1 is_parameter, ignored;
  sint4 val;
{
  register pgm_variable *pv;
           void        **r;
	 
  pv = resr__new_object(pgm_variable);
  if (!pv)
    {
      report_siflash_memory_error();
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(local_vars, name, &r, &nb_colls, &nb_ins) 
      < 0)
#else
  if (hash__insert_string(local_vars, name, &r) < 0)
#endif
    {
      resr__free_object(pv, pgm_variable);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pv, pgm_variable);
      sprintf(line, 
          "Compiler error: Duplicate variable name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = (void *) pv;

  pv -> name = name;
  if (!ignored)
    pv -> no   = the_program.nb_vars++;
  pv -> init = val;
  pv -> is_ignored = ignored;
  pv -> is_parameter = is_parameter;
  pv -> next = the_program.processes[current_process].local_vars;

  if (is_parameter){
    if (set__add(parameter_set, pv -> no) < 0)
      {
       resr__free_object(pv, pgm_variable);
       resr__free_objects(name, char, strlen(name) + 1);
       report_siflash_memory_error();
       return -1;
      }
  }

  the_program.processes[current_process].local_vars =  pv;  

  return 0;
}

/** int pgm_declare_global_variable(name, is_parameter, ignored, val)
		    : Declares a new global variable whose identifier
		    is name, and unallocates the memory in which name
		    is stored.  This variable is a free parameter
		    (non-deterministic value) if is_parameter > 0.
		    The intial value is val (if not a parameter) and
		    the variable is ignored (unaccepted type) if
		    ignored > 0.  Returns 0 in the case of success,
		    and -1 in the case of an error (a message is then
		    output).                                       **/

int  pgm_declare_global_variable(name, is_parameter, ignored, val)
  char *name;
  uint1 is_parameter, ignored;
  sint4 val;
{
  register pgm_variable *pv;
           void        **r;
	
  pv = resr__new_object(pgm_variable);
  if (!pv)
    {
      report_siflash_memory_error();
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(global_vars, name, &r, &nb_colls, &nb_ins)
      < 0)
#else
  if (hash__insert_string(global_vars, name, &r)
      < 0)
#endif
    {
      resr__free_object(pv, pgm_variable);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pv, pgm_variable);
      sprintf(line, 
          "Compiler error: Duplicate variable name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = (void *) pv;

  pv -> name = name;
  if (!ignored)
    pv -> no   = the_program.nb_vars++;
  pv -> init = val;
  pv -> is_ignored = ignored;
  pv -> is_parameter = is_parameter;
  pv -> next = the_program.global_vars;

  the_program.global_vars = pv;
 
  return 0;
}

/** int pgm_declare_local_type(name, ignored) : Declares the new local
		   type called name. If ignored = 0, then the type is
		   equivalent to the integer type.  If ignored > 0,
		   then the type is not equivalent to the integer type
		   and must be ignored.  Frees the string name.
		   Returns 0 in the case of success and -1 in the case
		   of failure.  Reports errors.                    **/

int  pgm_declare_local_type(name, ignored) 
     char *name;
     uint1 ignored;
{
  void  **r;
  uint1  *pay; 

  pay = resr__new_object(uint1);
  if (!pay)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
      return -1;
    }
 
#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(local_types, name, &r, &nb_colls, &nb_ins) 
      < 0)
#else
  if (hash__insert_string(local_types, name, &r) < 0)
#endif
    {
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];
      
      sprintf(line, 
	      "Compiler error: Duplicate type '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = pay;
  *pay = ignored;
  resr__free_objects(name, char, strlen(name) + 1);
  return 0;
}

/** int pgm_declare_global_type(name, ignored) : Declares the new
                   global type called name.  If ignored = 0, then the
                   type is equivalent to the integer type.  If ignored
                   > 0, then the type is not equivalent to the integer
                   type and must be ignored.  Frees the string name.
                   Returns 0 in the case of success and -1 in the case
                   of failure.  Reports error.                     **/

int  pgm_declare_global_type(name, ignored) 
     char *name;
     uint1 ignored;
{
  void        **r;
  uint1       *pay;
  pay = resr__new_object(uint1);
  if (!pay)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
      return -1;
    }
  
#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(global_types, name, &r, &nb_colls, &nb_ins) 
      < 0)
#else
  if (hash__insert_string(local_types, name, &r) < 0)
#endif
    {
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      sprintf(line, 
          "Compiler error: Duplicate type '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = pay;
  *pay = ignored;      
  resr__free_objects(name, char, strlen(name) + 1);
  return 0;
}

/** int pgm_declare_local_constant(name, val) : Declares a new local
                    constant whose identifier is name and whose value
                    is val, and  unallocates the memory in which name
                    is stored.  Returns 0 in the case of success, and
                    -1 in the case of an error (a message is then
                    output).                                       **/

int  pgm_declare_local_constant(name, val)
  char *name;
  sint4 val;
{
  register pgm_constant *pc;
           void        **r;
	 
  pc = resr__new_object(pgm_constant);
  if (!pc)
    {
      report_siflash_memory_error();
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(local_const, name, &r, &nb_colls, &nb_ins) 
      < 0)
#else
  if (hash__insert_string(local_const, name, &r) < 0)
#endif
    {
      resr__free_object(pc, pgm_constant);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pc, pgm_constant);
      sprintf(line, 
          "Compiler error: Duplicate constant name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = (void *) pc;

  pc -> name = name;
  pc -> val = val;

  return 0;
}

/** int pgm_declare_global_constant(name, val) : Declares a new global
                    variable whose identifier is name, whose value is
                    val, and unallocates the memory in which name is
                    stored.  Returns 0 in the case of success, and -1
                    in the case of an error (a message is then
                    output).                                       **/

int  pgm_declare_global_constant(name, val)
  char *name;
{
  register pgm_constant *pc;
           void        **r;
	
  pc = resr__new_object(pgm_constant);
  if (!pc)
    {
      report_siflash_memory_error();
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(global_const, name, &r, &nb_colls, &nb_ins)
      < 0)
#else
  if (hash__insert_string(global_const, name, &r)
      < 0)
#endif
    {
      resr__free_object(pc, pgm_constant);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pc, pgm_constant);
      sprintf(line, 
          "Compiler error: Duplicate constant name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = (void *) pc;

  pc -> name = name;
  pc -> val  = val;
 
  return 0;
}

/** int pgm_declare_parameter_alias(name, no, is_local): Declares the
		   parameter of name name as being an alias to the
		   variable no.  The alias is local if is_local > 0
		   and global otherwise.  Returns 0 in the case of
		   success and -1 in the case of insufficient memory.
		   Reports errors.                                 **/

int  pgm_declare_parameter_alias(name, no, is_local)
     char *name; 
     uint4 no;
     uint1 is_local;
{
  register pgm_variable *pv;
           void        **r;
	   hash_table   *h;

  pv = resr__new_object(pgm_variable);
  if (!pv)
    {
      report_siflash_memory_error();
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

  h = (is_local) ? local_vars : global_vars ;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(h, name, &r, &nb_colls, &nb_ins)
      < 0)
#else
  if (hash__insert_string(h, name, &r)
      < 0)
#endif
    {
      resr__free_object(pv, pgm_variable);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pv, pgm_variable);
      sprintf(line, 
          "Compiler error: Duplicate variable name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_siflash_error(line);
      return -1;
    }

  *r = (void *) pv;

  pv -> name = name;
  pv -> no   = no;
  pv -> init = 0;
  pv -> is_ignored = 0;
  pv -> is_parameter = 1;
  pv -> next = NULL;

  return 0;

}

/**  int  pgm_cleanup_current_process()  :  Unallocates the temporary
                    data structures associated to the current process.
                    Returns -1 and displays a message in the case of
                    an error, and returns 0 otherwise.             **/

int  pgm_cleanup_current_process()
{
  register id_table_payload *p;
  register pgm_process *pr;
  register uint4        n, nb;
           void **r;
   
  pr = the_program.processes + current_process ;
  if (pr -> nb_instances > 0)
    {
      p = resr__new_object(id_table_payload);
      if (!p)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      
      p -> process_defined = 0;
      p -> state_defined = 0;
      p -> trans_defined = 0;
      
#if LASH_CHECK_LEVEL >= 1
      if (hash__insert_string(id_table, 
      the_program.processes[current_process].name, &r, &nb_colls, 
			      &nb_ins) < 0)
#else
	if (hash__insert_string(id_table, 
	        the_program.processes[current_process].name, &r) < 0)
#endif
	  {
	    resr__free_object(p, uint4);
	    report_siflash_memory_error();
	    return -1;
	  }
      
      if (!r)
	{
	  char line[256];
	  resr__free_object(p, uint4);
	  sprintf(line, 
		  "Compiler error: Duplicate process name '%.32s'",
		  the_program.processes[current_process].name);
	  report_siflash_error(line);
	  return -1;
	}
      
      *r = p;
      p -> process_defined = 1;
      p -> process = current_process;
 
      if (set_init_flags(the_program.processes + current_process) < 0)
	return -1;

      compute_process_leaf_no(pr);  
      
      if (finalize_transitions(pr) < 0)
	return -1;
      
      if (mark_states(pr) < 0 ||
	  check_fusion_loops(pr) < 0)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      
    }

  hash__free(local_vars, (void (*)(void *)) pgm_free_string, NULL);
  local_vars = NULL;
  hash__free(local_const, (void (*)(void *)) pgm_free_string, 
             (void (*)(void *)) free_constant);
  local_const = NULL;
  hash__free(local_types, (void (*)(void *)) pgm_free_string, 
             (void (*)(void *)) free_uint1);
  local_types = NULL;

  if (pr -> nb_instances == 0)
    {
      free_process(pr);
      n = the_program.nb_proc; 
      if (n > 1)
	{
	  pr = resr__resize_objects(the_program.processes, 
				  pgm_process, n - 1, n);
	  the_program.processes = pr;
	}
      else
	{
	  resr__free_objects(the_program.processes, pgm_process, 1);
	  the_program.processes = NULL;
	}

      the_program.nb_proc   = n -1;
      return 0;
    }

  if (pr -> nb_instances > 1)
    {
      nb = pr -> nb_instances;
      for (n = 1 ; n < nb ; n++)
	if (generate_process_instance(current_process, n) < 0)
	  {
	    report_siflash_memory_error();
	    return -1;
	  }
    }
 
  return 0;
}


/**  int  pgm_lookup_constant(val, n)  :  Searches for a constant of
                    name n in the current constant table. If such a
                    constant is found, then its value is returned in
                    *val, and the function returns 0. 
		    Otherwise, the function returns -1.            **/

int  pgm_lookup_constant(val, n)
  sint4 *val;
  char  *n;
{
  register pgm_constant *pc = NULL;

  if (local_const)
    pc = (pgm_constant *) hash__lookup_string(local_const, n);

  if (!pc && global_const)
    pc = (pgm_constant *) hash__lookup_string(global_const, n);

  if (!pc)
    return -1;

  if (val){
    *val = pc -> val;
  }
    
  return 0;
}

/**  int  pgm_lookup_type(n, ignored)  :  Searches for a type of
                    name n in the current type table. If such a
                    type is found, then the function stores 0 in 
		    ignored if the type corresponds to the integer
		    type and 1 otherwise. 
		    If the type is not found, the function returns
                    -1.                                            **/

int  pgm_lookup_type(n, ignored)
  char  *n;
  uint1 *ignored;
{
  uint1 *pay;
  pay = NULL;
  if (local_types)
    pay = (uint1 *) hash__lookup_string(local_types, n);

  if (!pay && global_types)
    pay = (uint1 *) hash__lookup_string(global_types, n);

  if (!pay)
    return -1;

  *ignored = (*pay > 0) ? 1 : 0 ; 
  return 0;
}

/** int pgm_lookup_variable(p, is_parameter,is_ignored, n) : Searches
                    for a variable of name n in the current variable
                    table. If such a variable is found, then its index
                    is returned in *p and the function returns 0. If
                    is_ignored is not NULL, the flag is_ignored is
                    stored in *is_ignored and similarly, the flag
                    is_parameter is stored in *is_parameter.
                    Otherwise, the function returns -1.            **/

int  pgm_lookup_variable(p, is_parameter, is_ignored, n)
  uint4 *p;
  uint1 *is_parameter, *is_ignored;
  char  *n;
{
  register pgm_variable *pv = NULL;

  if (local_vars)
    pv = (pgm_variable *) hash__lookup_string(local_vars, n);

  if (!pv && global_vars)
    pv = (pgm_variable *) hash__lookup_string(global_vars, n);

  if (!pv)
    return -1;

  if (p)
    *p = pv -> no;
  if (is_parameter)
    *is_parameter = pv -> is_parameter;
  if (is_ignored)
    *is_ignored = pv -> is_ignored;

  return 0;
}

/** int pgm_lookup_process_variable(p, pr, v) : Searches for a
		    variable of name v in the process named pr.  If pr
		    is "global", then it searches the global variables
		    otherwise, it looks for v in the given process. If
		    such a variable is found, then its index is
		    returned in *p and the function returns 0.
		    Otherwise, the function returns -1.            **/

int  pgm_lookup_process_variable(p, pr, v)
  uint4 *p;
  char  *v, *pr;
{
  register pgm_variable *pv = NULL;

  if (strcmp(pr, "global") == 0)
    {
      if (global_vars)
	{
	  pv = (pgm_variable *) hash__lookup_string(global_vars, v);
	  if (!pv) 
	    return  -1;
	  if (p){
	    *p = pv -> no;
	  }
	  return 0;
	}
      return -1;
    }
  
  else
    {
      pgm_process *proc;
      uint4 p_no;
      id_table_payload *pu;

      pu = (id_table_payload *) hash__lookup_string(id_table, pr);
      if ((pu) && (pu -> process_defined != 0))
	{
	  p_no = pu -> process;
	  proc = the_program.processes + p_no ;  
	  for (pv = proc -> local_vars ; pv ; pv = pv -> next)
	    if (strcmp(pv -> name, v) == 0)
	 	{
		  if (p) 
		    {
		      *p = pv -> no;
		    }
		  return 0;
	 	} 
	}
      return -1;
    }     
}

/** int pgm_is_parameter(v) : Returns 1 if v is a parameter and 0
		    otherwise.  Does not report errors.            **/

int pgm_is_parameter(v)
     uint4 v;
{
  return set__member(parameter_set, v);
}

/** pgm_gen_operation *pgm_create_asgn(p, expr, n) : Returns a pointer
		    to a newly allocated pgm_gen_operation structure
		    containing one pgm_operation corresponding to the
		    assignement n := expr.  Returns NULL in the case
		    of insufficient memory.  Does not report errors.
                                                                   **/

pgm_gen_operation  *pgm_create_asgn(expr, n)
  pgm_lin_expr  *expr;
  uint4          n;
{
  pgm_operation *op;
  pgm_gen_operation *gop;

  op = resr__new_object(pgm_operation);
  if (!op)
    return NULL;
  gop = resr__new_object(pgm_gen_operation);
  if (!gop)
    {
      resr__free_object(op, pgm_operation);
      return NULL;
    }

  op -> lvalue = n;
  op -> expr = expr;
  gop -> nb_op = 1;
  gop -> op = op;
  return gop;
}

/** int pgm_cond_create_from_expr(p, lexpr, op) : Stores into the
		    newly allocated structure *p the expression lexpr
		    op 0 as a general condition Returns 0 in the case
		    of success, and -1 in the case of insufficient
		    memory.  Reports errors.  Does not free lexpr. **/

int  pgm_cond_create_from_expr(p, lexpr, op)
  pgm_gen_condition **p;
  pgm_lin_expr *lexpr;
  uint1 op;
{
  /* One has to convert into a condition of type a * x + b * y <= b */

  register pgm_term *pt;
  register sint4     offset;
  register int       compl;
  register uint4     i;

  pgm_condition *pc;
  pgm_gen_condition *pgc, *pgc1, *pgc2;

  pgc = resr__new_object(pgm_gen_condition);
  if (!pgc)
    {
      report_siflash_memory_error();
      return -1;
    }
  
  switch(op)
    {
    case PGM_OP_EQ:
     /* One needs to convert a == b into a >= b AND a <= b */

      if (pgm_cond_create_from_expr(&pgc1, lexpr, PGM_OP_GE) < 0)
	{
 	  resr__free_object(pgc, pgm_gen_condition);
	  return -1;
	}
      if (pgm_cond_create_from_expr(&pgc2, lexpr, PGM_OP_LE) < 0)
	{
 	  resr__free_object(pgc, pgm_gen_condition);
	  pgm_free_gen_condition(pgc1);
	  return -1;
	}
      pgc -> type = PGM_COND_AND;
      pgc -> nb_args = ZERO_INT4;
      pgc -> al_args = ZERO_INT4;
      if ((cond_add_arg(pgc, pgc1) < 0) 
	|| (cond_add_arg(pgc, pgc2) < 0))
	{
	  pgm_free_gen_condition(pgc1);
	  pgm_free_gen_condition(pgc2);
 	  resr__free_object(pgc, pgm_gen_condition);
	  return -1;
	}
      pgm_free_gen_condition(pgc1);
      pgm_free_gen_condition(pgc2);
      *p = pgc;
      return 0;

    case PGM_OP_NE:
      /* One needs to convert a != b into a > b OR a < b */
      if (pgm_cond_create_from_expr(&pgc1, lexpr, PGM_OP_GT) < 0)
	{
 	  resr__free_object(pgc, pgm_gen_condition);
	  return -1;
	}
      if (pgm_cond_create_from_expr(&pgc2, lexpr, PGM_OP_LT) < 0)
	{
 	  resr__free_object(pgc, pgm_gen_condition);
	  pgm_free_gen_condition(pgc1);
	  return -1;
	}
      pgc -> type = PGM_COND_OR;
      pgc -> nb_args = ZERO_INT4;
      pgc -> al_args = ZERO_INT4;
      if ((cond_add_arg(pgc, pgc1) < 0) 
	|| (cond_add_arg(pgc, pgc2) < 0))
	{
 	  resr__free_object(pgc, pgm_gen_condition);
	  pgm_free_gen_condition(pgc1);
	  pgm_free_gen_condition(pgc2);
	  return -1;
	}
      pgm_free_gen_condition(pgc1);
      pgm_free_gen_condition(pgc2);
      *p = pgc;
      return 0;

    case PGM_OP_GE:
      offset         = ZERO_INT4;
      compl          = 1;
      break;
    case PGM_OP_GT:
      offset         = -1;
      compl          = 1;
      break;
    case PGM_OP_LE:
      offset         = ZERO_INT4;
      compl          = 0;
      break;
    case PGM_OP_LT:
      offset         = -1;
      compl          = 0;
      break;
    default:
      report_siflash_error(
"Internal: Bad operator type in pgm_cond_create_from_expr");  
      resr__free_object(pgc, pgm_gen_condition);      
      return -1;
    }

  pc = resr__new_object(pgm_condition);
  if (!pc)
    {
      report_siflash_memory_error();
      resr__free_object(pgc, pgm_gen_condition);
      return -1;
    }
  pc -> cond_type = PGM_COND_CMP;
  pt = resr__new_objects(pgm_term, lexpr -> nb_el);
  if (!pt)
    {
      resr__free_object(pc, pgm_condition);
      resr__free_object(pgc, pgm_gen_condition);      
      report_siflash_memory_error();
    }
  pc -> nb_el = lexpr -> nb_el;
  for (i = ZERO_INT4 ; i < lexpr -> nb_el ; i++)
    {
      pt[i] . no = lexpr -> el[i] . no;
      pt[i] . v  = (compl) ? - lexpr -> el[i] . v : lexpr -> el[i] . v;
    }
  pc -> b     = (compl ? lexpr -> c : -lexpr -> c) + offset;
  pc -> el    = pt;
  pgc -> type = PGM_COND_ATOM;
  pgc -> args = (void *) pc;
  pgc -> nb_args = 1;
  pgc -> al_args = 1;
  *p = pgc;
  return 0;
}
     
/** pgm_gen_operation *pgm_composed_gop(gop1, gop2) : Composes the
		      operations gop1 and gop2 into a new operation
		      gop such that gop = gop2 o gop1.  In the case of
		      success, this function returns a pointer to a
		      newly created generalisezed operation.  In the
		      case of an error, it returns a null pointer.
		      Does not free gop1 nor gop2.  Reports errors.
		                                                   **/

pgm_gen_operation  *pgm_composed_gop(gop1, gop2)
     pgm_gen_operation *gop1, *gop2;
{
  register uint4 i;
  register pgm_operation *op;
  pgm_gen_operation *new_gop;

  if (!(new_gop = gen_operation_copy(gop1)))
    {
      report_siflash_memory_error();
      return NULL;
    }

  for (i = ZERO_INT4, op = gop2 -> op; i < gop2 -> nb_op;
       i++, op++)
    if (compose_gop_1(new_gop, op) < 0)
      {
	report_siflash_memory_error();
	pgm_free_gen_operation(new_gop);
	return NULL;
      }

  return new_gop;
}


/** int pgm_cond_create(gc, c1, c2, t) : Creates a new condition gc of
		    type t. ( with t = PGM_COND_OR or PGM_COND_AND ).
		    The new condition is the equivalent Disjonctive
		    Normal form of (c1 t c2).  c1 and c2 are supposed
		    to be in Disjunctive Normal Form.  Does not free
		    c1 nor c2.  Returns -1 in the case of insufficient
		    memory and 0 in the case of success.  Does not
		    report errors.                                 **/

int pgm_cond_create(gc, c1, c2, t)
     pgm_gen_condition **gc;
     pgm_gen_condition *c1, *c2;
     uint1 t;
{
  pgm_gen_condition *c;

  c = resr__new_object(pgm_gen_condition);
  if (!c)
    return -1;
  c -> al_args = ZERO_INT4; 
  c -> type = t;
  c -> nb_args = ZERO_INT4;
  c -> args = NULL;
	
  if (cond_add_arg(c, c1) < 0)
    return -1;
	
  if (cond_add_arg(c, c2) < 0)
    return -1;
	
  *gc = c;

  return pgm_cond_normalise(gc);
   }

/** void pgm_negate_lin_expr(expr) : Negates all the coefficients of
                      the linear expression *expr.  Does not report
                      errors.                                      **/
void    pgm_negate_lin_expr(expr)
     pgm_lin_expr *expr;
{
  register uint4 i;
  
  for (i=ZERO_INT4; i < expr -> nb_el ; i++)
    (expr -> el[i]). v = - (expr -> el[i]). v;
  expr -> c = - expr -> c;
}

/* pgm_lin_expr *pgm_add_lin_expr(expr1, expr2): adds the linear
                      expression expr2 to the linear expression
                      expr1. It returns a pointer to the newly
                      allocated linear expression in the case of
                      success and NULL in the case of insufficient
                      memory or arithmetic overflow.  Reports the
                      error.  Does not free expr1 nor expr2.       **/

pgm_lin_expr   *pgm_add_lin_expr(expr1, expr2)
     pgm_lin_expr *expr1, *expr2;
{
  register uint4 i,j;
  uint4 nb_el;
  sint4 v;
  pgm_term *terms, *t;
  pgm_lin_expr *expr;

  expr = resr__new_object(pgm_lin_expr);
  if (!expr)
    {
      report_siflash_memory_error();
      return NULL;
    }

  if (sint4__add(&(expr -> c), expr1 -> c, expr2 -> c) < 0)
    {
       report_siflash_error("Arithmetic overflow");
       resr__free_object(expr, pgm_lin_expr);
       return NULL;
    }

  if (uint4__add(&nb_el, expr1 -> nb_el, expr2 -> nb_el) < 0)
    {
      report_siflash_error("Arithmetic overflow");
      resr__free_object(expr, pgm_lin_expr);
      return NULL;
    }

  expr -> nb_el = nb_el;
  expr -> el = NULL;

  if (nb_el > 0)
    {
      terms = resr__new_objects(pgm_term, nb_el);
      if (!terms)
	{
	  report_siflash_memory_error();
	  return NULL;
	}
      
      if (expr1 -> nb_el > 0) 
	  memcpy(terms, expr1 -> el, expr1 -> nb_el * sizeof(pgm_term));
      expr -> nb_el = expr1 -> nb_el;

      for (i = ZERO_INT4; i < expr2 -> nb_el ; i++)
	{
	  t = expr2 -> el + i;
	  for (j = ZERO_INT4; j < expr1 -> nb_el ; j++)
	    {
	      if (terms[j].no == t -> no)
		{
		  if (sint4__add(&v, terms[j].v , t -> v) < 0)
		    {
		      report_siflash_error("Arithmetic overflow");
		      resr__free_objects(terms, pgm_term,  nb_el);
		      resr__free_object(expr, pgm_lin_expr);
		      return NULL;
		    }
		  break;
		}
	    }
	  if (j == expr1 -> nb_el)
	    {
	      terms[expr -> nb_el] = *t;  
	      expr -> nb_el++;
	    }
	}
  
      terms = resr__resize_objects(terms, pgm_term, expr -> nb_el,
				   expr1 -> nb_el + expr2 -> nb_el);
      if (!terms)
	{
	  report_siflash_memory_error();
	  resr__free_object(expr, pgm_lin_expr);
	  return NULL;
	}
      
      expr -> el = terms;
    }
  return expr;
}

/* int pgm_multiply_lin_expr(expr, c) : Multiplies all the
		    coeeficients in the linear expression expr by the
		    constant c.  Returns 0 in the case of success and
		    -1 in the case of arithmetic overflow.         **/

int    pgm_multiply_lin_expr(expr, c)
     pgm_lin_expr *expr;
     sint4 c;
{
  register uint4 i;
  sint4 v;

  if (c == 0)
    {
      if ((expr -> nb_el > 0) || (expr -> el))
	resr__free_objects(expr -> el, pgm_term, expr -> nb_el);
      expr -> c = 0;
      expr -> nb_el = ZERO_INT4;
      expr -> el = NULL;
      return 0;
    }
  
   for (i=ZERO_INT4; i < expr -> nb_el ; i++)
    {
      if (sint4__mult(&v, (expr -> el[i]).v, c)  < 0)
	return -1;
      (expr -> el[i]).v = v;
    }
   return 0;
}

/* pgm_lin_expr *pgm_create_lin_expr(c, no) : Creates a new linear
		  expression of one term whose coefficient is c and
		  whose variable number is no.  Returns a pointer to
		  the newly allocated linear expression in the case of
		  success and NULL in the case of insufficient memory.
		                                                   **/
pgm_lin_expr  *pgm_create_lin_expr(c, no)
     sint4 c;
     uint4 no;
{
  pgm_lin_expr *expr;
  pgm_term *t;

  t = resr__new_object(pgm_term);
  if (!t)
    return NULL;

  expr = resr__new_object(pgm_lin_expr);
  if (!expr)
    {
      resr__free_object(t, pgm_term);
      return NULL;
    }
  
  t -> v = c;
  t -> no = no;
  expr -> nb_el = 1;
  expr -> el = t;
  expr -> c = ZERO_INT4;

  return expr;
}

/* pgm_lin_expr *pgm_create_lin_expr_const(c) : Creates a new linear
		  expression of one constant term whose coefficient is
		  c.  Returns a pointer to the newly allocated linear
		  expression in the case of success and NULL in the
		  case of insufficient memory.                     **/

pgm_lin_expr   *pgm_create_lin_expr_const(c)
     sint4 c;
{
  pgm_lin_expr *expr;

  expr = resr__new_object(pgm_lin_expr);
  if (!expr)
    return NULL;
  
  expr -> nb_el = ZERO_INT4;
  expr -> el = NULL;
  expr -> c = c;

  return expr;
}

/* int pgm_extract_variable(expr, no) : Extracts the number of the
		 variables in the linear expression expr and stores
		 this value in *no.  Returns 0 in the case of success
		 and -1 if there are zero or more than one variable.
		                                                   **/
int  pgm_extract_variable(expr, no)
     pgm_lin_expr   *expr;
     uint4 *no;
{
  if (expr -> c != ZERO_INT4)
    return -1;
  if (expr -> nb_el != 1)
    return -1;
  if (expr -> el[0].v != 1)
    return -1;

  *no = expr -> el[0].no;
  return 0;
}

/* int pgm_extract_constant(expr, c) : Extracts the value of the
		 constant in the linear expression expr and stores
		 this value in *c.  Returns 0 in the case of success
		 and -1 if there are at least one variable in the
		 linear expression expr.                           **/
int  pgm_extract_constant(expr, c)
     pgm_lin_expr   *expr;
     sint4 *c;
{
  if (expr -> nb_el > ZERO_INT4)
    return -1;

  *c = expr -> c;
  return 0;
}

/** int pgm_cond_normalise(c) : Normalizes (in Disjunctive Form) the
			condition c. c's sub-conditions are supposed
			to be already in the normal form (without any
			negation).  Returns 0 in the case of success
			and -1 in the case of failure.             **/

int  pgm_cond_normalise(c)
     pgm_gen_condition **c;
{
 
  pgm_gen_condition *oc; /* old condition */
 
  oc = *c;

  /* oc is an atomic condition */
  if (oc -> type == PGM_COND_ATOM)
    return 0;
  
  /* oc is a disjunction */
  if (oc -> type == PGM_COND_OR)
    return pgm_cond_norm_OR(c);
  
  /* oc is a conjunction */
  if (oc -> type == PGM_COND_AND)
      return  pgm_cond_norm_AND(c);

  /* incorrect type */
  else 
    return -1;
}

/** int pgm_cond_neg(c) : Takes the negation of the condition c.
                       c is assumed to be in disjunctive normal form.
                       Returns 0 in case of success and -1 in case of 
		       failure and reports the error.              **/
int pgm_cond_neg(c)
  pgm_gen_condition **c;
{
  pgm_gen_condition *sc;
  pgm_condition *ac;
  pgm_term *terms;
  int i;
  if ((*c) -> type == PGM_COND_ATOM)
    {
      ac = (pgm_condition *) (*c) -> args;

      if ((!ac) || (ac  -> cond_type  != PGM_COND_CMP))
	{
	  report_siflash_error("Internal: data corrupted.");
	  return -1;
	}
      terms = ac -> el;
      ac -> b =  - (ac -> b) - 1;
      for (i = ZERO_INT4 ; i < ac -> nb_el ; i++, terms++)
	terms -> v = - (terms -> v);
      return 0;
    }
  else if ((*c) -> type == PGM_COND_OR)
    {
      pgm_gen_condition *args, *tmp;
      args = resr__new_objects(pgm_gen_condition, (*c) -> nb_args);
      if (!args)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      sc = (pgm_gen_condition *) (*c) -> args;
      (*c) -> type = PGM_COND_AND;   
  
      tmp = resr__new_object(pgm_gen_condition);
      if (!tmp)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      
      for (i = ZERO_INT4 ; i < (*c) -> nb_args ; i++)
	{
	  *tmp = *(sc + i); 
	  if (pgm_cond_neg(&tmp) < 0)
	    return -1;
	  args[i] = *tmp;
	}
      
      resr__free_object(tmp, pgm_gen_condition);
      resr__free_objects(sc, pgm_gen_condition, (*c) -> al_args);
      (*c) -> args = args;
      (*c) -> al_args =  (*c) -> nb_args;
	
      /* only for this case, normalisation might be needed. */
      if (pgm_cond_normalise(c) < 0)
	return -1;
      return 0;
    }
  
  else if ((*c) -> type == PGM_COND_AND)
    {
      pgm_gen_condition *args, *tmp;
      args = resr__new_objects(pgm_gen_condition, (*c) -> nb_args);
      if (!args)
	{
	  report_siflash_memory_error();
	  return -1;
	}

      sc = (pgm_gen_condition *) (*c) -> args;
      (*c) -> type = PGM_COND_OR;
    
      tmp = resr__new_object(pgm_gen_condition);
      if (!tmp)
	{
	  report_siflash_memory_error();
	  return -1;
	}
  
      for (i = ZERO_INT4 ; i < (*c) -> nb_args ; i++)
	{
	  *tmp = *(sc + i);
	  if (pgm_cond_neg(&tmp) < 0)
	    return -1;
	  args[i] = *tmp;
	}

      resr__free_object(tmp, pgm_gen_condition);
      resr__free_objects(sc, pgm_gen_condition, (*c) -> al_args);
      (*c) -> args = args;
      (*c) -> al_args =  (*c) -> nb_args;
	  
      return 0;
    }
  
  else
    {
      report_siflash_error("INTERNAL: data corrupted.");
      return -1;
    }
} 
  

/** int pgm_meta_local(lno, cno) : Initializes the meta-transition
                    generator for a local meta-transition whose
                    definition begins at the line lno and character
                    cno in the source file. Returns 0 in the case of
                    success, and -1 in the case of insufficient
                    memory. Reports errors.                        **/

int  pgm_meta_local(lno, cno)
  uint4  lno, cno;
{
  register pgm_meta *pm;

  pm = resr__new_object(pgm_meta);
  if (!pm)
    {
      report_siflash_memory_error();
      return -1;
    }

  pm -> head  = NULL;
  pm -> body  = NULL;
  pm -> trans = NULL;
  pm -> next  = the_program.meta;
  pm -> lno   = lno;
  pm -> cno   = cno;

  the_program.meta  = pm;
  current_control_p = &pm -> head;
  meta_local        = 1;

  return 0;
}

/**  int  pgm_meta_global(lno, cno)  :  Initializes the meta-
                    transition generator for a global meta-transition
                    whose definition begins at the line lno and
                    character cno in the source file. Returns 0 in the
                    case of success, and -1 in the case of
                    insufficient memory. Reports errors.           **/

int  pgm_meta_global(lno, cno)
  uint4  lno, cno;
{
  register pgm_meta *pm;

  pm = resr__new_object(pgm_meta);
  if (!pm)
    {
      report_siflash_memory_error();
      return -1;
    }

  pm -> head  = NULL;
  pm -> body  = NULL;
  pm -> trans = NULL;
  pm -> next  = the_program.meta;
  pm -> lno   = lno;
  pm -> cno   = cno;

  the_program.meta  = pm;
  current_control_p = &pm -> head;
  meta_local        = 0;

  return 0;
}

/**  void  pgm_meta_sequence_begin()  :  Informs the meta-transition
                    generator that the origin of the meta-transition
                    has been parsed, and that the sequence of control
                    locations is about to be read. This function does
                    not report errors.                             **/

void  pgm_meta_sequence_begin()
{
  current_control_p = &the_program.meta -> body;
}

/**  int  pgm_meta_process(name)  :  Provides the process identifier
                    name to the meta-transition generator (which must
                    unallocate this name). Returns 0 in the case of
                    success, and -1 in the case of insufficient
                    memory.                                        **/

int  pgm_meta_process(name)
  char *name;
{
  register pgm_control *pc;

  pc = resr__new_object(pgm_control);
  if (!pc)
    {
      resr__free_objects(name, char, strlen(name) + 1);
      return -1;
    }

  pc -> process_name  = name;
  pc -> spec_name = NULL;
  pc -> process_no    = pc -> location_no = ZERO_INT4;
  pc -> next          = NULL;
  pc -> trans_defined = 0;

  *current_control_p = pc;

  return 0;
}

/**  int  pgm_meta_label(name)  :  Provides the control location
                     identifier name to the meta-transition generator
                    (which must unallocate this name). Returns 0 in
                    the case of success, and -1 in the case of
                    insufficient memory.                           **/

int  pgm_meta_label(name)
  char *name;
{
  register pgm_control *pc;

  if (meta_local)
    {
      pc = resr__new_object(pgm_control);
      if (!pc)
	{
	  resr__free_objects(name, char, strlen(name) + 1);
	  return -1;
	}

      pc -> spec_name      = name;
      pc -> process_name   = NULL;
      pc -> process_no     = current_process;
      pc -> location_no    = ZERO_INT4;
      pc -> next           = NULL;

      *current_control_p   = pc;
    }
  else
    (*current_control_p) -> spec_name = name;

  current_control_p = &(*current_control_p) -> next;

  return 0;
}

/**  void  pgm_free_string(str)  :  Frees the null-terminated string 
                    str.                                           **/

void  pgm_free_string(str)
  char *str;
{
  register uint4  n;

  if (!str)
    return;

  n = strlen(str) + 1;

  resr__free_objects(str, char, n);
  str = NULL;
}

/**  void  pgm_free_lin_expr(lexp)  :  Frees the pgm_lin_expr *lexp.
		    Does not report errors.                        **/
void  pgm_free_lin_expr(lexp)
  pgm_lin_expr *lexp;
{
  if ((lexp -> nb_el > ZERO_INT4) || (lexp -> el))
    resr__free_objects(lexp -> el, pgm_term, lexp -> nb_el);
  resr__free_object(lexp, pgm_lin_expr); 
}

/** void free_gen_condition(c) : Frees recursively the subconditions 
                of the general condition structure *c, or the elements
		of c, if c is an atomic condition.
		Does not report errors.                            **/

void pgm_free_gen_condition(c)
  pgm_gen_condition *c;
{
  free_gen_cond_content(c);
  resr__free_object(c, pgm_gen_condition);
}

/** void  pgm_free_expression(expr): Frees the pgm_expression *expr.
		     Reports an error in case of wrong expression 
		     type.                                         **/
void  pgm_free_expression(expr)
  pgm_expression *expr;
{
  switch(expr -> type) 
    {
    case PGM_EXPR_LIN:
      pgm_free_lin_expr((pgm_lin_expr *) expr -> expr);
      break;
    case PGM_EXPR_COND:
      pgm_free_gen_condition((pgm_gen_condition *) expr -> expr);
      break;
    default:
      report_siflash_error("Internal error: pgm_expression type");
    }
  resr__free_object(expr, pgm_expression); 
}

/** void pgm_free_gen_operation(gop) : Frees the general operation
                structure *gop.  Does not report errors.           **/
void   pgm_free_gen_operation(gop)
  pgm_gen_operation *gop;
{
  uint4 i;
  pgm_operation *op;

  for (i=ZERO_INT4; i < gop -> nb_op; i++) 
    {
      op = gop -> op + i;
      pgm_free_lin_expr(op -> expr);
    }

  if ((gop -> nb_op) || (gop -> op))
    resr__free_objects(gop -> op, pgm_operation, gop -> nb_op); 
  resr__free_object(gop, pgm_gen_operation);
} 

/** void pgm_free_state_elt(s) : Frees the elements of the program
                state information structure *s.  Does not report
                errors.                                            **/

void  pgm_free_state_elt(s)
  pgm_state *s;
{
  register uint4           i;
  register pgm_transition *pt;

  for (i = ZERO_INT4, pt = s -> trans; i < s -> nb_trans; i++, pt++)
    free_transition_elt(pt);
  
  if ((s -> al_trans) || (s -> trans))
    resr__free_objects(s -> trans, pgm_transition, s -> al_trans);

  if ((s -> al_substates) || (s -> substates))
    resr__free_objects(s -> substates, uint4, s -> al_substates);

  if (s -> name)
    pgm_free_string(s -> name); 
}

/** void pgm_store_pre_cond(gc) : Stores the gen_condition
		gc into the private variable pre_cond. 
		Does not report errors.  Frees gc.         **/
int  pgm_store_pre_cond(gc)
     pgm_gen_condition *gc;
{
  pre_cond = gc;
  return 0;
}

/** void pgm_store_trans_label(name): stores the label name into the
                private variable label_trans. Frees name.  Does not
                report errors.                                     **/
void  pgm_store_trans_label(name)
     char *name;
{
  label_trans = name;
}

/** int   pgm_store_fpar_elt(name, ignored) : Stores the fpar
                of name name and of type integer if ignored
		is set to 0 or of ignored type otherwise.
		Returns 0 in the case of success and -1 in the case of
                insufficient memory and reports errors.            **/

int  pgm_store_fpar_elt(name, ignored)
     char *name;
     uint1 ignored;
{
  pgm_fpar *par;
  par = resr__new_object(pgm_fpar);
  if (!par)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
      return -1;
    } 
  par -> name = name;
  par -> ignored = ignored;
  par -> next = fpar;
  fpar = par;
  return 0;
}

/** int pgm_declare_fpar_as_variable() : Sets all the stored fpar
		elements as local variable of the current process.
		Return 0 in the case of success and -1 in the case of
		failure.  Reports errors.                          **/

int  pgm_declare_fpar_as_variable()
{
  pgm_fpar *next, *cur;

  next = fpar;
  fpar = NULL;
  while (next)
    {
      cur = next;
      next = cur -> next;
      if (pgm_declare_local_variable(cur -> name, 0,
				     cur -> ignored, 0) < 0)
	{
	  resr__free_object(cur, pgm_fpar);
	  while (next)
	    {
	      cur = next;
	      next = cur -> next;
	      pgm_free_string(cur -> name);
	      resr__free_object(cur, pgm_fpar);
	    }
	  return -1;
	}
      resr__free_object(cur, pgm_fpar);      
    }
  return 0;
}

/** void pgm_free_fpar() : Frees the fpar elements stored previously.
                Does not report errors.                          **/

void  pgm_free_fpar()
{
  pgm_fpar *next, *cur;

  next = fpar;
  fpar = NULL;
  while (next)
    {
      cur = next;
      next = cur -> next;
      pgm_free_string(cur -> name);
      resr__free_object(cur, pgm_fpar);
    }
}

/** void pgm_store_asgn(gop) : Stores the gen_operation gop into asgn.
                Does not report errors.                            **/

void   pgm_store_asgn(gop)
     pgm_gen_operation *gop;
{
  asgn = gop;
}

/* void pgm_set_next_state(s) : Sets the sucessor state for the next
                transition created as s.  Does not report errors. */

void pgm_set_next_state(s) 
     uint4 s;
{
  next_state = s;
  return ;
}

/****  End of program.c  ****/
