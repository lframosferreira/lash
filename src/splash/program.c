/********************************************************************/
/**                                                                **/
/**  Simple Promela for LASH (SPLASH) compiler -- v0.9             **/
/**  =========================================                     **/
/**                                                                **/
/**    program.c  :  Generation of concurrent programs.            **/
/**                                                                **/
/**     05/21/99  :  Creation. (BB)                                **/
/**     06/03/99  :  Continued. (BB)                               **/
/**     07/20/99  :  Minor corrections. (BB)                       **/
/**     05/15/01  :  Minor correction. (BB)                        **/
/**     08/14/01  :  Small adaptation. (BB)                        **/
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
#include "explore.h"

/****  Definitions.                                              ****/

#define  PGM_GROWTH      0x10  /*  Growth quota for arrays.         */
#define  PGM_HASH_SIZE  0x400  /*  Size of identifier hash tables.  */

/****  Global variables.                                         ****/

static uint4          current_state, current_process, nb_terms, 
                      al_terms;
static sint4          constant_term;
static uint1          cond_op;
static int            negative, second_hand, meta_local;
static pgm_program    the_program;
static hash_table    *local_vars, *global_vars, *id_table;
static pgm_variable  *current_var;
static pgm_term      *terms;
static pgm_control  **current_control_p;
static uint8          nb_ins, nb_colls;

/****  Prototypes of private functions.                          ****/

static void *grow_table(void **, uint4 *, uint4 *, unsigned);
static void  free_string(char *);
static void  free_the_program(void);
static void  free_process(pgm_process *);
static void  free_variable(pgm_variable *);
static void  free_meta(pgm_meta *);
static void  free_state(pgm_state *);
static void  free_label(pgm_label *);
static void  free_control(pgm_control *);
static int   resolve_labels(pgm_process *);
static int   resolve_tmp_transitions(pgm_process *);
static int   reference_metas(void);
static int   reference_control(pgm_control *, pgm_meta *);
static int   mark_states(pgm_process *);
static int   check_fusion_loops(pgm_process *);
static int   translate_meta(pgm_meta *);
static int   follow_meta(pgm_process *, pgm_meta *, uint4, uint4, 
                 pgm_tr_ref ***);

/****  Private functions.                                        ****/

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

/**  void  free_string(str)  :  Frees the null-terminated string str.
                                                                   **/

static void  free_string(str)
  char *str;
{
  register uint4  n;

  n = strlen(str) + 1;

  resr__free_objects(str, char, n);
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
  register pgm_label    *pl, *plnext;

  if (p -> name)
    free_string(p -> name);

  for (i = ZERO_INT4; i < p -> nb_states; i++)
    free_state(p -> states + i); 

  if (p -> al_states)
    resr__free_objects(p -> states, pgm_state, p -> al_states);

  for (pv = p -> local_vars; pv; pv = pvnext)
    {
      pvnext = pv -> next;
      free_variable(pv);
    }  

  for (pl = p -> labels; pl; pl = plnext)
    {
      plnext = pl -> next;
      free_label(pl);
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
    free_string(v -> name);

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

/**  void  free_state(s)  :  Frees the program state information
                    structure *s. Does not report errors.          **/

static void  free_state(s)
  pgm_state *s;
{
  register uint4           i;
  register pgm_transition *pt;

  for (i = ZERO_INT4, pt = s -> trans; i < s -> nb_trans; i++, pt++)
    switch(pt -> type)
      {
      case PGM_TRANS_ASGN:
	if (pt -> param.asgn.nb_el)
	  resr__free_objects(pt -> param.asgn.el, pgm_term, 
              pt -> param.asgn.nb_el);
	break;

      case PGM_TRANS_ASSERT:
      case PGM_TRANS_COND:
	if (pt -> param.cond.nb_el)
	  resr__free_objects(pt -> param.cond.el, pgm_term, 
              pt -> param.cond.nb_el);
	break;

      case PGM_TRANS_TMP:
	if (pt -> param.tmp_name)
	  free_string(pt -> param.tmp_name);  /*  fallthough.       */

      default:
	break;
      }

  if (s -> al_trans)
    resr__free_objects(s -> trans, pgm_transition, s -> al_trans);
}

/**  void  free_label(l)  :  Frees the program label information
                    structure *l. Does not report errors.          **/

static void  free_label(l)
  pgm_label *l;
{
  if (!l)
    return;

  if (l -> name)
    free_string(l -> name);

  resr__free_object(l, pgm_label);
}

/**  void  free_control(c)  :  Frees the control location information
                    structure *c. Does not report errors.          **/

static void  free_control(c)
  pgm_control *c;
{
  if (!c)
    return;

  if (c -> process_name)
    free_string(c -> process_name);

  if (c -> location_name)
    free_string(c -> location_name); 

  resr__free_object(c, pgm_control);
}

/**  int  resolve_labels(p)  :  Hashes the labels defined in the
                    process *p, and computes the destination of all
                    the transitions of type PGM_TRANS_TMP. In the
                    case of an error, the function displays a message
                    and returns -1. Otherwise, it returns 0.       **/

static int  resolve_labels(p)
  pgm_process *p;
{
  register uint4     *u, l;
  register pgm_label *pl;
           void     **r;
           char       tag[256];

  sprintf(tag, "%.127s@", p -> name);
  l = strlen(tag);

  for (pl = p -> labels; pl; pl = pl -> next)
    {
      u = resr__new_object(uint4);
      if (!u)
	{
	  report_splash_memory_error();
	  return -1;
	}

      sprintf(tag + l, "%.127s", pl -> name);

#if LASH_CHECK_LEVEL >= 1
      if (hash__insert_string(id_table, tag, &r, &nb_colls, 
          &nb_ins) < 0)
#else
      if (hash__insert_string(id_table, tag, &r) < 0)
#endif
	{
	  resr__free_object(u, uint4);
	  report_splash_memory_error();
	  return -1;
	}

     if (!r)
       {
	 char line[256];

	 resr__free_object(u, uint4);
         sprintf(line, 
      "Compiler error: Duplicate label '%.32s' in process '%.32s'",
              pl -> name, p -> name);
         report_splash_error(line);
	 return -1;
       } 
     
     *r = u;
     *u = pl -> state;
    }

  if (resolve_tmp_transitions(p) < 0)
     return -1;

  return 0;
}

/**  int  resolve_tmp_transitions(p)  :  Computes the destination of
                    the transitions of type PGM_TRANS_TMP. In the
                    case of an error, the function displays a message
                    and returns -1. Otherwise, it returns 0.       **/

static int  resolve_tmp_transitions(p)
  pgm_process *p;
{
  register uint4           i, j, l, *pu;
  register pgm_state      *ps;
  register pgm_transition *pt;
           char            tag[256];

  sprintf(tag, "%.127s@", p -> name);
  l = strlen(tag);

  for (i = ZERO_INT4, ps = p -> states; i < p -> nb_states;
       i++, ps++)
    for (j = ZERO_INT4, pt = ps -> trans; j < ps -> nb_trans;
         j++, pt++)
      if (pt -> type == PGM_TRANS_TMP)
	{
          sprintf(tag + l, "%.127s", pt -> param.tmp_name);
	  pu = (uint4 *) hash__lookup_string(id_table, tag);
	  if (!pu)
	    {
              char line[256];

	      sprintf(line, 
      "Compiler error: Unknown reference '%.32s' in process '%.32s'",
                  pt -> param.tmp_name, p -> name);
	      report_splash_error(line);
	      return -1;
	    }
	  pt -> type = PGM_TRANS_SKIP;
	  pt -> dest = *pu;
          free_string(pt -> param.tmp_name);
	}

  return 0;
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

/**  int  reference_control(c, m)  :  Replaces the the process and/or
                    control location identifiers belonging to the
                    control information structure *c by their
                    corresponding indices. The meta-transition to
                    which *c belongs is *m. In the case of an error,
                    this function returns -1 and displays a
                    message. Otherwise, it returns 0.              **/
 
static int  reference_control(c, m)
  pgm_control *c;
  pgm_meta    *m;
{
  register uint4 *pu;
           char   tag[256];

  if (c -> process_name)
    {
      pu = (uint4 *) hash__lookup_string(id_table, c -> process_name);
      if (!pu)
	{
	  char line[256];

          sprintf(line,
"Compiler error in definition at (L%u, C%u): Unknown process '%.32s'",
              m -> lno, m -> cno, c -> process_name);
	  report_splash_error(line);
	  return -1;
	}
      free_string(c -> process_name);
      c -> process_name = NULL;
      c -> process_no = *pu;
    }

  if (c -> location_name)
    {
      sprintf(tag, "%.127s@%.127s",
          the_program.processes[c -> process_no].name,
          c -> location_name);

      pu = (uint4 *) hash__lookup_string(id_table, tag);
      if (!pu)
	{
	  char line[256];

          sprintf(line,
"Compiler error in definition at (L%u, C%u): Invalid label '%.32s'",
              m -> lno, m -> cno, c -> location_name);
	  report_splash_error(line);
	  return -1;
	}
      free_string(c -> location_name);
      c -> location_name = NULL;
      c -> location_no = *pu;
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
                    returns -1. Otherwise, it returns 0.           **/

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

  me.st_no = ZERO_INT4;
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
	      ps -> trans[0].type == PGM_TRANS_SKIP &&
	      !((!(ps -> atomic)) &&
                 p -> states[ps -> trans[0].dest].atomic))
	    {
	      ps -> fusionable = 1;
	      ps -> id = ZERO_INT4;
	    }
	  else
	    ps -> id = n++;
	}

      if (mt -> tr_nb >= ps -> nb_trans)
	{
	  stack__pop(st, &me);
	  continue;
	}

      me.st_no = ps -> trans[mt -> tr_nb++].dest;

      if (p -> states[me.st_no].reachable)
	continue;

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

	for (j = ps -> trans[0].dest; p -> states[j].fusionable;
             j = p -> states[j].trans[0].dest)
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

/**  int  translate_meta(p)  :  Computes the list of transitions
                    corresponding to the meta-transition specified
                    by the information structure *p, and stores that
                    list in this structure. In the case of an error,
                    this function returns -1 and displays a
                    message. Otherwise, it returns 0.              **/

static int  translate_meta(p)
  pgm_meta *p;
{
  register int         *present;
  register uint4       *current_locations, i, n;
  register pgm_control *pc;
           pgm_tr_ref **ref;

  n = the_program.nb_proc;

  present = resr__new_objects(int, n);
  if (!present)
    {
      report_splash_memory_error();
      return -1;
    }
  
  current_locations = resr__new_objects(uint4, n);
  if (!current_locations)
    {
      resr__free_objects(present, int, n);
      report_splash_memory_error();
      return -1;
    }

  for (i = ZERO_INT4; i < n; i++)
    present[i] = 0;

  for (pc = p -> head; pc; pc = pc -> next)
    {
      if (present[pc -> process_no])
	{
	  char line[256];

	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Duplicate head process '%.32s'",
              p -> lno, p -> cno,
              the_program.processes[pc -> process_no].name);
	  report_splash_error(line);
	  resr__free_objects(present, int, n);
	  resr__free_objects(current_locations, uint4, n);
	  return -1;
	}

      if (!the_program.processes[pc -> process_no].states[pc -> 
          location_no].reachable)
	{
	  char line[256];

	  sprintf(line, 
"Compiler warning in definition at (L%u, C%u): Unreachable meta-transition in process '%.32s'",
              p -> lno, p -> cno,
              the_program.processes[pc -> process_no].name);
	  report_splash_warning(line);
	  resr__free_objects(present, int, n);
	  resr__free_objects(current_locations, uint4, n);
	  return 0;
	}

      present[pc -> process_no] = 1;
      current_locations[pc -> process_no] = pc -> location_no;
    }

  for (pc = p -> body, ref = &p -> trans; pc; pc = pc -> next)
    {
      if (!present[pc -> process_no])
	{
	  char line[256];

	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Process '%.32s' has no head",
              p -> lno, p -> cno,
              the_program.processes[pc -> process_no].name);
	  report_splash_error(line);
	  resr__free_objects(present, int, n);
	  resr__free_objects(current_locations, uint4, n);
	  return -1;
	}

      if (follow_meta(the_program.processes + (pc -> process_no),
          p, current_locations[pc -> process_no],
          pc -> location_no, &ref) < 0)
	{
	  resr__free_objects(present, int, n);
	  resr__free_objects(current_locations, uint4, n);
	  return -1;
	}

      current_locations[pc -> process_no] = pc -> location_no;
    }

  resr__free_objects(present, int, n);  

  for (pc = p -> head; pc; pc = pc -> next)
    if (current_locations[pc -> process_no] != pc -> location_no)
      {
	char line[256];

	  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Open loop in process '%.32s'",
              p -> lno, p -> cno,
              the_program.processes[pc -> process_no].name);
	  report_splash_error(line);
	  resr__free_objects(current_locations, uint4, n);
	  return -1;
      }
 
  resr__free_objects(current_locations, uint4, n);
  return 0;
}

/**  typedef follow_info  :  Information stored on the exploration
                   stack of the function follow_meta.              **/

typedef struct {
  uint4  st_no, tr_nb;
}  follow_info;

/**  int  follow_meta(pr, no, nd, pl)  :  Explores the transitions of
                   the process *pr, starting from the state of index
                   no and heading to the state of index nd. The
                   sequence of explored transitions is stored in the
                   linked list **pl. The current meta-transition
                   being analyzed is *mt. In the case of an error, the
                   function displays a message and returns -1. In
                   the case of success, it returns 0.              **/

static int  follow_meta(pr, mt, no, nd, pl)
  pgm_process  *pr;
  pgm_meta     *mt;
  uint4         no, nd;
  pgm_tr_ref ***pl;
{
  register bit_table      *bt;
  register stack          *st;
  register int             found;
  register follow_info    *ft, *fp;
  register pgm_state      *ps;
  register pgm_transition *tr;
  register pgm_tr_ref     *ref;
  register uint4           i, n;
           follow_info     fi;

  bt = bit__new_empty(pr -> nb_states);
  if (!bt)
    {
      report_splash_memory_error();
      return -1;
    }

  st = stack__new_empty(follow_info);
  if (!st)
    {
      bit__free(bt);
      report_splash_memory_error();
      return -1;
    }

  fi.st_no = no;
  fi.tr_nb = ZERO_INT4;

  if (stack__push(st, &fi) < 0)
    {
      bit__free(bt);
      stack__free(st);
      report_splash_memory_error();
      return -1;
    }

  for (found = 0; !(stack__is_empty(st));)
    {
      ft = (follow_info *) stack__top(st);
      ps = pr -> states + ft -> st_no;

      if (!(ft -> tr_nb))
	{
	  if ((stack__nb_elements(st) > 1) && ps -> labeled)
	    {
	      if (found && ft -> st_no == nd)
		{
		  char line[256];

		  sprintf(line, 
"Compiler error in definition at (L%u, C%u): Ambiguous (multiple) meta-transition",
                      mt -> lno, mt -> cno);
		  report_splash_error(line);
		  stack__free(st);
		  bit__free(bt);
		  return -1;
		}

	      if (ft -> st_no == nd)
		{
		  for (n = stack__nb_elements(st), i = n - 1; i; i--)
		    {
		      fp = (follow_info *) stack__pick(st, i);
		      tr = pr -> states[fp -> st_no].trans +
                          fp -> tr_nb - 1;
                      if (tr -> type != PGM_TRANS_SKIP)
			{
			  ref = resr__new_object(pgm_tr_ref);
			  ref -> tr   = tr;
			  ref -> next = NULL;
                        **pl = ref;
                         *pl = &ref -> next;
			}
		    }
		  found = 1;
		}

	      stack__pop(st, &fi);
	      continue;
	    }
	  bit__add(bt, ft -> st_no);
	}

      if (ft -> tr_nb >= ps -> nb_trans)
	{
	  stack__pop(st, &fi);
          bit__remove(bt, ft -> st_no);
	  continue;
	}

      tr = ps -> trans + (ft -> tr_nb++);

      fi.st_no = tr -> dest;

      if (bit__member(bt, fi.st_no) && (fi.st_no != no))
	{
	  char line[256];

	  sprintf(line, 
 "Compiler error in definition at (L%u, C%u): Ambiguous (with internal loop) meta-transition",
	      mt -> lno, mt -> cno);
	  report_splash_error(line);
	  stack__free(st);
	  bit__free(bt);
	  return -1;
	}

      fi.tr_nb = ZERO_INT4;
     
      if (stack__push(st, &fi) < 0)
	{
	  stack__free(st);
	  bit__free(bt);
	  report_splash_memory_error();
	  return -1;
	}
    }

  bit__free(bt);
  stack__free(st);

  if (!found)
    {
      char line[256];

      sprintf(line, 
 "Compiler error in definition at (L%u, C%u): Invalid meta-transition",
          mt -> lno, mt -> cno);

      report_splash_error(line);      
      return -1;
    }

  return 0;
}

/****  Public functions.                                         ****/

/**  int  pgm_init()  :  Initializes the data structures used by this
                    module and creates an empty initial program. This
                    function returns 0 in the case of success, and -1
                    if there is not enough memory.                 **/

int  pgm_init()
{
  global_vars = hash__new_empty(PGM_HASH_SIZE);
  if (!global_vars)
    return -1;

  id_table = hash__new_empty(PGM_HASH_SIZE);
  if (!id_table)
    {
      hash__free(global_vars, NULL, NULL);
      return -1;
    }

  local_vars              = NULL;
  current_state           = ZERO_INT4;
  current_process         = ZERO_INT4;
  current_var             = NULL;
  nb_colls                = ZERO_INT8;
  nb_ins                  = ZERO_INT8;
  nb_terms                = ZERO_INT4;
  al_terms                = ZERO_INT4;
  constant_term           = ZERO_INT4;
  cond_op                 = PGM_OP_EQ;
  negative                = 0;
  second_hand             = 0;
  meta_local              = 0;
  terms                   = NULL;
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
    hash__free(global_vars, (void (*)(void *)) free_string, NULL);

  if (local_vars)
    hash__free(local_vars, (void (*)(void *)) free_string, NULL);

  bytes__prepare_free(sizeof(uint4));

  if (id_table)
    hash__free(id_table, (void (*)(void *)) free_string,
        (void (*)(void *)) bytes__free); 

  if (al_terms)
    resr__free_objects(terms, pgm_term, al_terms);

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
      report_splash_error(
"Translator error: The program has no variable");
      return -1;
    }

  if (reference_metas() < 0)
    return -1;

  if (out_init(&the_program) < 0)
    return -1;

  if (expl_init(&the_program) < 0)
    return -1;

  return 0;
}

/**  int  pgm_new_state(p)  :  Creates a new control location in the
                    current process, which becomes the new current
                    state. If p is not null, then the index of the new
                    state is returned in *p.  The function returns 0
                    in the case of success, and -1 if there is not
                    enough memory.                                 **/

int  pgm_new_state(p)
  uint4  *p;
{
  register uint4        n;
  register pgm_state   *ps;
  register pgm_process *pr;

  if (!the_program.processes)
    return 0;

  pr = the_program.processes + current_process;
  n  = pr -> nb_states;
  ps = (pgm_state *) grow_table((void **) &pr -> states, 
      &pr -> nb_states, &pr -> al_states, sizeof(pgm_state));

  if (!ps)
    return -1;

  if (p)
    *p = n;

  current_state    = n;
  ps -> nb_trans   = ZERO_INT4;
  ps -> al_trans   = ZERO_INT4;
  ps -> trans      = NULL;
  ps -> atomic     = 0;
  ps -> labeled    = 0;
  ps -> reachable  = 0;
  ps -> fusionable = 0;
  ps -> id         = ZERO_INT4;

  return 0;
}

/**  int  pgm_new_transition(t, d, p)  :  Adds to the current process
                    a new instruction of type t, originating from the
                    current control location and going to that of
                    index d. The parameters of the new instruction are
                    given by *p. The function returns 0 in the case of
                    success, and -1 if there is not enough memory. **/

int  pgm_new_transition(t, d, p)
  uint1          t;
  uint4          d;
  pgm_parameter *p;
{
  register pgm_state      *ps;
  register pgm_transition *pt;

  if (!the_program.processes)
    return 0;

  ps = the_program.processes[current_process].states + current_state;
  pt = grow_table((void **) &ps -> trans, &ps -> nb_trans, 
      &ps -> al_trans, sizeof(pgm_transition));
  if (!pt)
    return -1;
  
  pt -> type  =  t;
  pt -> dest  =  d;

  if (p)
    pt -> param = *p;

  return 0;
}

/**  int  pgm_new_process()  :  Creates a new process, which becomes
                    the current one. The function returns 0 in the
                    case of success, and -1 if there is not enough
                    memory.                                        **/

int  pgm_new_process()
{
  register pgm_process *p;
  register uint4        n;

  n = the_program.nb_proc; 
  p = resr__resize_objects(the_program.processes, pgm_process, n + 1, 
          n);
  if (!p)
    return -1;

  the_program.processes = p;
  the_program.nb_proc = n + 1;

  current_process = n;
  current_state   = ZERO_INT4;

  p += n;
  p -> name       = NULL;
  p -> nb_states  = ZERO_INT4;
  p -> al_states  = ZERO_INT4;
  p -> nb_ids     = ZERO_INT4;
  p -> states     = NULL;
  p -> local_vars = NULL;
  p -> labels     = NULL;

  local_vars = hash__new_empty(PGM_HASH_SIZE);
  if (!local_vars)
    return -1;

  return 0;
}

/**  uint4  pgm_get_current_state()  :  Returns the index of the
                    current control location. This function does not
                    report errors.                                 **/

uint4  pgm_get_current_state()
{
  return  current_state;
}

/**  void  pgm_set_atomic()  :  Marks the current control location as
                    being atomic (which means that the next executable
                    instruction must be outgoing from that location).
                    This function does not report errors.          **/

void  pgm_set_atomic()
{
  the_program.processes[current_process].states[current_state].atomic 
      = 1;
}

/**  void  pgm_set_current_state(s)  :  Sets the current control
                    location to the one of index s. This function does
                    not report errors.                             **/

void  pgm_set_current_state(s)
  uint4  s;
{
  current_state = s;
}

/**  int  pgm_declare_label(name)  :  Associates the label name to the
                    current control location, and unallocates the
                    memory in which name is stored.  Returns 0 in the
                    case of success, and -1 in the case of an error
                    (a message is then output).                    **/

int  pgm_declare_label(name)
  char *name;
{
  register pgm_label   *pl;
  register pgm_process *pp;

  pl = resr__new_object(pgm_label);
  if (!pl)
    {
      resr__free_objects(name, char, strlen(name) + 1);
      report_splash_memory_error();
      return -1;
    }

  pp = the_program.processes + current_process;

  pl -> name  = name;
  pl -> state = current_state; 
  pl -> next  = pp -> labels;
  pp -> labels = pl;
  pp -> states[current_state].labeled = 1;

  return 0;
}

/**  int  pgm_declare_local_variable(name)  :  Declares a new local
                    variable whose identifier is name, and unallocates
                    the memory in which name is stored.  Returns 0 in
                    the case of success, and -1 in the case of an
                    error (a message is then output).              **/

int  pgm_declare_local_variable(name)
  char *name;
{
  register pgm_variable *pv;
           void        **r;
	 
  pv = resr__new_object(pgm_variable);
  if (!pv)
    {
      report_splash_memory_error();
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
      report_splash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pv, pgm_variable);
      sprintf(line, 
          "Compiler error: Duplicate variable name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_splash_error(line);
      return -1;
    }

  *r = (void *) pv;

  pv -> name = name;
  pv -> no   = the_program.nb_vars++;
  pv -> init = ZERO_INT4;
  pv -> next = the_program.processes[current_process].local_vars;

  the_program.processes[current_process].local_vars = current_var 
      = pv;

  return 0;
}

/**  int  pgm_declare_global_variable(name)  :  Declares a new global
                    variable whose identifier is name, and unallocates
                    the memory in which name is stored.  Returns 0 in
                    the case of success, and -1 in the case of an
                    error (a message is then output).              **/

int  pgm_declare_global_variable(name)
  char *name;
{
  register pgm_variable *pv;
           void        **r;

  pv = resr__new_object(pgm_variable);
  if (!pv)
    {
      report_splash_memory_error();
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
      report_splash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];

      resr__free_object(pv, pgm_variable);
      sprintf(line, 
          "Compiler error: Duplicate variable name '%.32s'", name);
      resr__free_objects(name, char, strlen(name) + 1);
      report_splash_error(line);
      return -1;
    }

  *r = (void *) pv;

  pv -> name = name;
  pv -> no   = the_program.nb_vars++;
  pv -> init = ZERO_INT4;
  pv -> next = the_program.global_vars;

  the_program.global_vars = current_var = pv;
 
  return 0;
}

/**  void  pgm_init_current_variable(v)  :  Assigns the initial value
                    v to the most recently declared variable. This
                    function does not report errors.               **/

void  pgm_init_current_variable(v)
  sint4  v;
{
  if (current_var)
    current_var -> init = v;
}

/**  void  pgm_name_current_process(name)  :  Assigns the identifier
                    name to the current process, and unallocates the
                    memory in which name is stored. This function does
                    not report errors.                             **/

void  pgm_name_current_process(name)
  char *name;
{
  the_program.processes[current_process].name = name;
}

/**  int  pgm_cleanup_current_process()  :  Unallocates the temporary
                    data structures associated to the current process.
                    Returns -1 and displays a message in the case of
                    an error, and returns 0 otherwise.             **/

int  pgm_cleanup_current_process()
{
  register uint4 *p;
           void **r;

  p = resr__new_object(uint4);
  if (!p)
    {
      report_splash_memory_error();
      return -1;
    }

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
      report_splash_memory_error();
      return -1;
    }

  if (!r)
    {
      char line[256];
      resr__free_object(p, uint4);
      sprintf(line, 
          "Compiler error: Duplicate process name '%.32s'",
          the_program.processes[current_process].name);
      report_splash_error(line);
      return -1;
    }

  *r = p;
  *p = current_process;

  hash__free(local_vars, (void (*)(void *)) free_string, NULL);
  local_vars = NULL;

  if (resolve_labels(the_program.processes + current_process) < 0)
    return -1;

  if (mark_states(the_program.processes + current_process) < 0 ||
      check_fusion_loops(the_program.processes + current_process)
      < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  return 0;
}

/**  int  pgm_lookup_variable(p, n)  :  Searches for a variable of
                    name n in the current variable table. If such a
                    variable is found, then its index is returned in
                    *p and the function returns 0. Otherwise, the
                    function returns -1.                           **/

int  pgm_lookup_variable(p, n)
  uint4 *p;
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

  return 0;
}

/**  void  pgm_expr_reset()  :  Resets the expression generator. This 
                    function does not report errors.               **/

void  pgm_expr_reset()
{
  if (al_terms)
    resr__free_objects(terms, pgm_term, al_terms);

  nb_terms                = ZERO_INT4;
  al_terms                = ZERO_INT4;
  constant_term           = ZERO_INT4;
  cond_op                 = PGM_OP_EQ;
  negative                = 0;
  second_hand             = 0;
  terms                   = NULL;
}

/**  void  pgm_expr_cond_op(c)  :  Registers the current condition 
                    operator to be used by the expression
                    generator. This function does not report errors.
                                                                   **/
void  pgm_expr_cond_op(c)
  uint1  c;
{
  cond_op = c;
  second_hand = 1;
}

/**  void  pgm_expr_negative_next()  :  Signals to the expression 
                    generator that the next term should be
                    complemented.  This function does not report
                    errors.                                        **/

void  pgm_expr_negative_next()
{
  negative = 1;
}

/**  void  pgm_expr_positive_next()  :  Signals to the expression 
                    generator that the next term should not be
                    complemented.  This function does not report
                    errors.                                        **/

void  pgm_expr_positive_next()
{
  negative = 0;
}

/**  int  pgm_expr_constant_term(v)  :  Provides the constant term
                    v to the expression generator. Returns 0 in the
                    case of success, and -1 in the case of
                    insufficient memory.                           **/

int  pgm_expr_constant_term(v)
  sint4  v;
{
  if (negative ^ second_hand)
    constant_term -= v;
  else
    constant_term += v; 

  return 0;
}

/**  int  pgm_expr_variable_term(c, n)  :  Provides to the expression
                    generator a term composed of the coefficient c and
                    the variable index n. Returns 0 in the case of
                    success, and -1 in the case of insufficient
                    memory.                                        **/

int  pgm_expr_variable_term(c, n)
  sint4  c;
  uint4  n;
{
  register pgm_term *pt;

  pt = (pgm_term *) grow_table((void **) &terms, &nb_terms, &al_terms,
       sizeof(pgm_term));
  if (!pt)
    return -1;

  pt -> no = n;
  pt -> v  = (negative ^ second_hand) ? (-c) : c;

  return 0;
}

/**  int  pgm_expr_store_asgn(p, n)  :  Stores into the structure *p 
                    the current expression as the right member of an
                    assignment whose left value is the variable of
                    index n. Returns 0 in the case of success, and -1
                    in the case of insufficient memory.            **/

int  pgm_expr_store_asgn(p, n)
  pgm_operation *p;
  uint4          n;
{
  register pgm_term *pt;

  if (nb_terms)
    {
      pt = resr__new_objects(pgm_term, nb_terms);
      if (!pt)
	return -1;
      memcpy((char *) pt, (char *) terms, nb_terms * 
          sizeof(pgm_term));
    }
  else
    pt = NULL;

  p -> lvalue = n;
  p -> nb_el  = nb_terms;
  p -> c      = constant_term;
  p -> el     = pt;

  return 0;
}

/**  int  pgm_expr_store_cond(p)  :  Stores into the structure *p
                    the current expression as a condition. Returns 0
                    in the case of success, and -1 in the case of
                    insufficient memory.                           **/

int  pgm_expr_store_cond(p)
  pgm_condition *p;
{
  register pgm_term *pt;
  register sint4     offset;
  register int       compl;
  register uint4     i;

  if (nb_terms)
    {
      pt = resr__new_objects(pgm_term, nb_terms);
      if (!pt)
	return -1;
      memcpy((char *) pt, (char *) terms, nb_terms * 
          sizeof(pgm_term));
    }
  else
    pt = NULL;

  switch(cond_op)
    {
    case PGM_OP_EQ:
      p -> cond_type = PGM_COND_EQU;
      offset         = ZERO_INT4;
      compl          = 0;
      break;
    case PGM_OP_GE:
      p -> cond_type = PGM_COND_CMP;
      offset         = ZERO_INT4;
      compl          = 1;
      break;
    case PGM_OP_GT:
      p -> cond_type = PGM_COND_CMP;
      offset         = -1;
      compl          = 1;
      break;
    case PGM_OP_LE:
      p -> cond_type = PGM_COND_CMP;
      offset         = ZERO_INT4;
      compl          = 0;
      break;
    case PGM_OP_LT:
      p -> cond_type = PGM_COND_CMP;
      offset         = -1;
      compl          = 0;
      break;
    case PGM_OP_NE:
    default:
      p -> cond_type = PGM_COND_INE;
      offset         = ZERO_INT4;
      compl          = 0;
    }

  p -> nb_el = nb_terms;
  p -> b     = (compl ? constant_term : -constant_term) + offset;
  p -> el    = pt;

  if (compl)
    for (i = ZERO_INT4; i < nb_terms; i++, pt++)
      pt -> v = -(pt -> v);

  return 0;
}

/**  int  pgm_meta_local(lno, cno)  :  Initializes the meta-transition
                    generator for a local meta-transition whose
                    definition begins at the line lno and character
                    cno in the source file. Returns 0 in the case of
                    success, and -1 in the case of insufficient
                    memory.                                        **/

int  pgm_meta_local(lno, cno)
  uint4  lno, cno;
{
  register pgm_meta *pm;

  pm = resr__new_object(pgm_meta);
  if (!pm)
    return -1;

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
                    insufficient memory.                           **/

int  pgm_meta_global(lno, cno)
  uint4  lno, cno;
{
  register pgm_meta *pm;

  pm = resr__new_object(pgm_meta);
  if (!pm)
    return -1;

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
  pc -> location_name = NULL;
  pc -> process_no    = pc -> location_no = ZERO_INT4;
  pc -> next          = NULL;

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

      pc -> location_name  = name;
      pc -> process_name   = NULL;
      pc -> process_no     = current_process;
      pc -> location_no    = ZERO_INT4;
      pc -> next           = NULL;

      *current_control_p   = pc;
    }
  else
    (*current_control_p) -> location_name = name;

  current_control_p = &(*current_control_p) -> next;

  return 0;
}

/**  int  pgm_meta_end()  :  Signals to the meta-transition generator
                    that the current meta-transition has been entirely
                    read. In the event of an error, the function
		    displays a message and returns -1. In the case of
		    success, it returns 0.                         **/

int  pgm_meta_end()
{
  return 0;
}

/****  End of program.c  ****/
