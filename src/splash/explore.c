/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**    explore.c  :  Interface with state-space exploration tool.  **/
/**                                                                **/
/**     06/04/99  :  Creation. (BB)                                **/
/**     02/03/00  :  Minor modifications in order to use improved  **/
/**                  state-machine handler. (BB)                   **/
/**     06/09/00  :  Minor correction. (BB)                        **/
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
#include "lash-diag.h"
#include "lash-auto.h"
#include "lash-ndd.h"
#include "resource.h"
#include "datastruct.h"
#include "splash.h"
#include "program.h"
#include "explore.h"

/****  Definitions.                                              ****/

#define  SPLASH_NDD_BASE  2        /*  Numeration base for NDDs.    */
#define  SPLASH_NDD_MSDF  1        /*  Most significant digit first */
                                   /*  in NDDs ?                    */

/****  Global variable.                                          ****/

static uint8          count;

/****  Prototypes of private functions.                          ****/

static sint4          state_index(pgm_process *, uint4);
static ndd_relation  *build_relation(pgm_program *);
static ndd_states    *build_initial_states(pgm_program *);
static int            add_transition(ndd_relation *, pgm_process *, 
                          uint4, sint4, pgm_transition *, uint4);
static int            add_meta_transition(ndd_relation *, 
                          pgm_program *, pgm_meta *);
static ndd           *initial_ndd(pgm_program *);
static void           init_variables(sint4 *, pgm_variable *);
static linear_transf *assign_transformation(pgm_operation *, uint4);
static linear_transf *condition_transformation(pgm_condition *,
                          uint4);
static int            negated_condition_transformation(
                          pgm_condition *, uint4, linear_transf **,
                          linear_transf **);
static int            callback(const ndd_states *);
static int            check_states(ndd_states *, pgm_program *);

/****  Private functions.                                        ****/

/**  sint4  state_index(pr, n)  :  Returns a number identifying the
                    state of index n of the process *pr. This number
                    is negative if the state is atomic, and strictly
                    positive otherwise. The number 0 is reserved for
                    the so-called error state.                     **/

static sint4  state_index(pr, n)
  pgm_process *pr;
  uint4        n;
{
  while (pr -> states[n].fusionable)
    n = pr -> states[n].trans[0].dest;

  if (pr -> states[n].atomic)
    return -(pr -> states[n].id + 1);
  else
    return (pr -> states[n].id + 1);
}

/**  ndd_relation *build_relation(p)  :  Constructs a transition and
                    meta-transition relation corresponding to the
                    program *p. Returns a pointer to the constructed
                    relation structure in the case of success. If an
                    error occurs, the function displays a message and
                    returns a NULL pointer.                        **/

static ndd_relation *build_relation(p)
  pgm_program *p;
{
  register ndd_relation   *nr;
  register uint4           i, j, k;
  register pgm_process    *pr;
  register pgm_state      *ps;
  register pgm_transition *pt;
  register pgm_meta       *pm;

  nr = ndd_relation_new_empty(SPLASH_NDD_BASE, p -> nb_proc,
           p -> nb_vars, SPLASH_NDD_MSDF); 
  if (!nr)
    {
      report_lash_error("initial transition relation");
      return NULL;
    }

  for (i = ZERO_INT4, pr = p -> processes; i < p -> nb_proc;
       i++, pr++)
    for (j = ZERO_INT4, ps = pr -> states; j < pr -> nb_states;
         j++, ps++)
      if (ps -> reachable && !(ps -> fusionable))
	for (k = ZERO_INT4, pt = ps -> trans; k < ps -> nb_trans;
	     k++, pt++)
	  if (add_transition(nr, pr, i, state_index(pr, j), pt,
              p -> nb_vars) < 0)
	    return NULL;
	    
  if (splash_verbose)
    printf(
"   with transitions                    : %lld NDD state(s).\n", 
        ndd_relation_size(nr));

  for (pm = p -> meta; pm; pm = pm -> next)
    if (add_meta_transition(nr, p, pm) < 0)
      return NULL;

  if (splash_verbose)
    printf(
"   with transitions & meta-transitions : %lld NDD state(s).\n", 
        ndd_relation_size(nr));

  return nr;
}

/**  ndd_states *build_initial_states(p)  :  Constructs a set of
                    initial states  corresponding to the program *p.
                    Returns a pointer to the constructed set
                    structure in the case of success. If an error
                    occurs, the function displays a message and
                    returns a NULL pointer.                        **/

static ndd_states *build_initial_states(p)
  pgm_program *p;
{
  register ndd_states *ns;
  register sint4      *pc;
  register ndd        *nd;
  register uint4       i;
  register int         rc;

  ns = ndd_states_new_empty(SPLASH_NDD_BASE, p -> nb_proc,
           p -> nb_vars, SPLASH_NDD_MSDF);
  if (!ns)
    {
      report_lash_error("set of initial states");
      return NULL;
    }

  pc = resr__new_objects(sint4, p -> nb_proc);
  if (!pc)
    {
      report_splash_memory_error();
      ndd_states_free(ns);
      return NULL;
    }

  for (i = ZERO_INT4; i < p -> nb_proc; i++)
    pc[i] = state_index(p -> processes + i, ZERO_INT4);

  nd = initial_ndd(p);
  if (!nd)
    {
      report_splash_memory_error();
      resr__free_objects(pc, sint4, p -> nb_proc);
      ndd_states_free(ns);
      return NULL;
    }

  rc = ndd_states_add_data(ns, pc, nd);
    
  resr__free_objects(pc, sint4, p -> nb_proc);
  ndd_free(nd);

  if (rc < 0)
    {
      report_lash_error("set of initial values");
      ndd_states_free(ns);
      return NULL;
    }

  if (splash_verbose)
    printf("   initial set : %u NDD state(s).\n", 
        auto_nb_states(ns -> ndd -> automaton));

  return ns;
}

/**  int  add_transition(nr, pr, pn, ns, tr, n)  :  Adds to the 
                    transition relation information structure *nr the
                    transition *tr of the process *pr (whose index is
                    pn), outgoing from the state of number ns. The
                    number of variables is n.  In the case of an
                    error, this function displays a message and
                    returns -1. In the case of success, it returns 0.
                                                                   **/

static int  add_transition(nr, pr, pn, ns, tr, n)
  ndd_relation   *nr;
  pgm_process    *pr;
  uint4           pn, n;
  sint4           ns;
  pgm_transition *tr;
{
  register linear_transf *lt;
  register int            rc;
           linear_transf *lt1, *lt2;

  switch (tr -> type)
    {
    case PGM_TRANS_ASGN:
      lt = assign_transformation(&tr -> param.asgn, n);
      if (!lt)
	{
	  report_splash_memory_error();
	  return -1;
	}
      rc = ndd_relation_add_transition(nr, pn, ns,
           state_index(pr, tr -> dest), lt);
      ndd_transf_free(lt);
      if (rc < 0)
	{
	  report_lash_error("assignment transition");
	  return -1;
	}
      break;

    case PGM_TRANS_COND:
      lt = condition_transformation(&tr -> param.cond, n);
      if (!lt)
	{
	  report_splash_memory_error();
	  return -1;
	}
      rc = ndd_relation_add_transition(nr, pn, ns,
           state_index(pr, tr -> dest), lt);
      ndd_transf_free(lt);
      if (rc < 0)
	{
	  report_lash_error("guard transition");
	  return -1;
	}
      break;

    case PGM_TRANS_ASSERT:
      lt = condition_transformation(&tr -> param.cond, n);
      if (!lt)
	{
	  report_splash_memory_error();
	  return -1;
	}
      rc = ndd_relation_add_transition(nr, pn, ns,
           state_index(pr, tr -> dest), lt);
      ndd_transf_free(lt);
      if (rc < 0)
	{
	  report_lash_error("assert transition");
	  return -1;
	}
      if (negated_condition_transformation(&tr -> param.cond, n,
          &lt1, &lt2) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
      rc = ndd_relation_add_transition(nr, pn, ns, 0, lt1);
      ndd_transf_free(lt1);
      if (rc < 0)
	{
	  report_lash_error("assert transition");
	  return -1;
	}

      if (lt2)
	{
	  rc = ndd_relation_add_transition(nr, pn, ns, 0, lt2);
	  ndd_transf_free(lt2);
	  if (rc < 0)
	    {
	      report_lash_error("assert transition");
	      return -1;
	    }
	}

      break;

    case PGM_TRANS_SKIP:
    default:
      lt = ndd_create_identity_transf(n);
      if (!lt)
	{
	  report_splash_memory_error();
	  return -1;
	}
      rc = ndd_relation_add_transition(nr, pn, ns,
           state_index(pr, tr -> dest), lt);
      ndd_transf_free(lt);
      if (rc < 0)
	{
	  report_lash_error("skip transition");
	  return -1;
	}
    }

  return 0;
}

/**  int  add_meta_transition(nr, p, pm)  :  Adds to the transition
                    relation information structure *nr the meta-
                    transition pm of the program *p. In the case of an
                    error, this function displays a message and
                    returns -1. In the case of success, it returns 0.
                                                                   **/
static int  add_meta_transition(nr, p, pm)
  ndd_relation *nr;
  pgm_program  *p;
  pgm_meta     *pm;
{
  register sint4          *ppc;
  register int            *ppm;
  register uint4           n;
  register pgm_control    *pc;
  register pgm_tr_ref     *pt;
  register linear_transf  *lt1, *lt2, *lt3;
  register pgm_transition *tr;
  register int             rc;

  n = p -> nb_proc;

  ppc = resr__new_objects(sint4, n);
  if (!ppc)
    {
      report_splash_memory_error();
      return -1;
    }

  ppm = resr__new_objects(int, n);
  if (!ppm)
    {
      resr__free_objects(ppc, sint4, n);
      report_splash_memory_error();
      return -1;
    }

  memset(ppc, 0, n * sizeof(sint4));
  memset(ppm, 0, n * sizeof(int));

  for (pc = pm -> head; pc; pc = pc -> next)
    {
      ppm[pc -> process_no] = 1;
      ppc[pc -> process_no] = state_index(p -> processes +
          (pc -> process_no), pc -> location_no);
    }

  lt1 = ndd_create_identity_transf(p -> nb_vars);
  if (!lt1)
    {
      resr__free_objects(ppc, sint4, n);
      resr__free_objects(ppm, int, n);
      report_splash_memory_error();
      return -1;
    }
  
  for (pt = pm -> trans; pt; pt = pt -> next)
    {
      tr = pt -> tr;
      switch(tr -> type)
	{
	case PGM_TRANS_ASGN:
	  lt2 = assign_transformation(&tr -> param.asgn,
              p -> nb_vars);
	  break;
	case PGM_TRANS_COND:
	case PGM_TRANS_ASSERT:
	  lt2 = condition_transformation(&tr -> param.cond,
              p -> nb_vars);
	  break;
	default:
	  continue;
	}
      lt3 = ndd_transf_compose(lt1, lt2);
      ndd_transf_free(lt1);
      ndd_transf_free(lt2);
      if (!lt3)
	{
	  ndd_transf_free(lt1);
	  resr__free_objects(ppc, sint4, n);
	  resr__free_objects(ppm, int, n);
	  report_lash_error("composition of transformations");
	  return -1;
	}
      lt1 = lt3;
    }

  rc = ndd_relation_add_metatransition(nr, ppc, ppm, lt1);

  ndd_transf_free(lt1);
  resr__free_objects(ppc, sint4, n);
  resr__free_objects(ppm, int, n);

  if (rc < 0)
    switch(lash_errno)
      {
      case LASH_ERR_ITERATION:
      case LASH_ERR_NOT_IMPL:
	{
          char line[256];

          sprintf(line,
"Translator warning in definition at (L%u, C%u): Non-iterable meta-transition (ignored)",
              pm -> lno, pm -> cno);
          report_splash_warning(line);
          return 0;
        }
      
      default:
	report_lash_error("iteration of meta-transition");
	return -1;
      }

  return 0;
}

/**  ndd *initial_ndd(p)  :  Constructs an NDD accepting the set of
                    initial variable values of the program *p. 
                    Returns a pointer to a newly allocated NDD in
                    the case of success, and a NULL pointer if there
                    is not enough memory.                          **/

static ndd *initial_ndd(p)
  pgm_program *p;
{
  register sint4 *pv;
  register uint4  i;
  register ndd   *nd1, *nd2, *nd3;
  static   sint4  one[1] = { 1 };

  pv = resr__new_objects(sint4, p -> nb_vars);
  if (!pv)
    return NULL;

  memset(pv, 0, (p -> nb_vars) * sizeof(sint4));

  init_variables(pv, p -> global_vars);

  for (i = ZERO_INT4; i < p -> nb_proc; i++)
    init_variables(pv, p -> processes[i].local_vars);

#if SPLASH_NDD_MSDF != 0
  nd1 = ndd_create_equation_msdf(SPLASH_NDD_BASE, 1, one, pv[0]);
#else
  nd1 = ndd_create_equation_lsdf(SPLASH_NDD_BASE, 1, one, pv[0]);
#endif

  if (!nd1)
    {
      resr__free_objects(pv, sint4, p -> nb_vars);
      return NULL;
    }

  for (i = 1; i < p -> nb_vars; i++)
    {
#if SPLASH_NDD_MSDF != 0
      nd2 = ndd_create_equation_msdf(SPLASH_NDD_BASE, 1, one, pv[i]);
#else
      nd2 = ndd_create_equation_lsdf(SPLASH_NDD_BASE, 1, one, pv[i]);
#endif
      if (!nd2)
	{
	  ndd_free(nd1);
	  resr__free_objects(pv, sint4, p -> nb_vars);
	  return NULL;
	}

      nd3 = ndd_product(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);

      if (!nd3)
	{
	  resr__free_objects(pv, sint4, p -> nb_vars);
	  return NULL;
	}

      nd1 = nd3;
    }

  resr__free_objects(pv, sint4, p -> nb_vars);

  return nd1;
}

/**  void  init_variables(pv, pl)  :  Initializes the initial variable
                    values in the array pv with the information
                    contained in the linked list of variables pl.
                    This function does not report errors.          **/

static void  init_variables(pv, pl)
  sint4        *pv;
  pgm_variable *pl;
{
  for (; pl; pl = pl -> next)
    pv[pl -> no] = pl -> init;
}

/**  linear_transf *assign_transformation(op, n)  :  Returns a linear
                    transformation equivalent to the assignment 
                    operation *op. The number of variables is n. In
                    the case of success, this function returns a
                    pointer to a newly allocated transformation. In
                    the case of insufficient memory, it returns a
                    NULL pointer.                                  **/

static linear_transf *assign_transformation(op, n)
  pgm_operation *op;
  uint4          n;
{
  register sint4         *sc;
  register uint4          i;  
  register pgm_term      *pt;
  register linear_transf *lt;

  sc = resr__new_objects(sint4, n);
  if (!sc)
    return NULL;

  memset(sc, 0, n * sizeof(sint4));

  for (i = ZERO_INT4, pt = op -> el; i < op -> nb_el; i++, pt++)
    sc[pt -> no] += pt -> v;

  lt = ndd_create_assign_transf(n, op -> lvalue, sc, op -> c);
  
  resr__free_objects(sc, sint4, n);

  return lt;
}

/**  linear_transf *condition_transformation(op, n)  :  Returns a 
                    linear transformation equivalent to the condition
                    operation *op. The number of variables is n. In
                    the case of success, this function returns a
                    pointer to a newly allocated transformation. In
                    the case of insufficient memory, it returns a NULL
                    pointer.                                       **/

static linear_transf *condition_transformation(op, n)
  pgm_condition *op;
  uint4          n;
{
  register sint4         *sc;
  register uint4          i;  
  register pgm_term      *pt;
  register linear_transf *lt;

  sc = resr__new_objects(sint4, n);
  if (!sc)
    return NULL; 

  memset(sc, 0, n * sizeof(sint4));

  for (i = ZERO_INT4, pt = op -> el; i < op -> nb_el; i++, pt++)
    sc[pt -> no] += pt -> v;

  if (op -> cond_type == PGM_COND_EQU)
    lt = ndd_create_equ_transf(n, sc, op -> b);
  else
    lt = ndd_create_inequ_transf(n, sc, op -> b);

  resr__free_objects(sc, sint4, n);

  return lt;
}

/**  int  negated_condition_transformation(op, n, plt1, plt2)  :  
                    Returns in *plt1 and *plt2 two linear
                    transformations whose union is equivalent to the
                    negation of the condition operation *op. If only
                    one transformation is sufficient, then a NULL
                    pointer is returned into *plt2.  The number of
                    variables is n. In the case of success, this
                    function returns 0. In the case of insufficient
                    memory, it returns -1.                         **/

static int  negated_condition_transformation(op, n, plt1, plt2)
  pgm_condition  *op;
  uint4           n;
  linear_transf **plt1, **plt2;
{
  register sint4         *sc;
  register uint4          i;  
  register pgm_term      *pt;

  sc = resr__new_objects(sint4, n);
  if (!sc)
    return -1; 

  memset(sc, 0, n * sizeof(sint4));

  for (i = ZERO_INT4, pt = op -> el; i < op -> nb_el; i++, pt++)
    sc[pt -> no] -= pt -> v;

  if (plt1)
    {
      *plt1 = ndd_create_inequ_transf(n, sc, -(op -> b) - 1);
      if (!*plt1)
	{
	  resr__free_objects(sc, sint4, n);
	  return -1;
	}
    }

  if (plt2)
    {
      if (op -> cond_type == PGM_COND_EQU)
	{
	  for (i = ZERO_INT4; i < n; i++)
	    sc[i] = -sc[i];
	  *plt2 = ndd_create_inequ_transf(n, sc, (op -> b) - 1);
	  if (!*plt2)
	    {
	      resr__free_objects(sc, sint4, n);
	      return -1;
	    }
	}
      else
	*plt2 = NULL;
    }

  resr__free_objects(sc, sint4, n);

  return 0;
}

/**  int  callback(ns)  :  Callback function called after each step
                    of the state-space exploration, if the compiler
                    is in verbose mode. This function displays the
                    number of NDD states of the current set of
                    reachable states, and returns 0.               **/

static int  callback(ns)
  const ndd_states *ns;
{
  printf("   intermediate result : %u NDD state(s).\n",
      auto_nb_states(ns -> ndd -> automaton));

  count++;

  return 0;
}

/**  int  check_states(ns, p)  :  Checks whether the set of
                    reachable states *ns contains an invalid state or
		    not, and displays an appropriate message.  The
		    program that has been explored is *p.  In the case
		    of success, this function returns 0.  In the case
		    of insufficient memory, it returns -1.         **/

static int  check_states(ns, p)
  ndd_states  *ns;
  pgm_program *p;
{
  register sint4 *pc;
  register int   *pm, err;
  register uint4  n, i;
  register ndd   *nd;

  n = p -> nb_proc;  

  pc = resr__new_objects(sint4, n);
  if (!pc)
    return -1;

  pm = resr__new_objects(int, n);
  if (!pc)
    {
      resr__free_objects(pc, sint4, n);
      return -1;
    }

  memset(pc, 0, n * sizeof(sint4));
  memset(pm, 0, n * sizeof(int));

  for (err = 0, i = ZERO_INT4; i < n; i++)
    {
      if (i)
	pm[i - 1] = 0;
      pm[i] = 1;

      nd = ndd_states_get_data(ns, pc, pm);
      if (!nd)
	{
	  resr__free_objects(pc, sint4, n);
	  resr__free_objects(pm, int, n);
	  return -1;
	}
      if (!(ndd_is_empty(nd)))
	{
	  printf("*** Assertion violation in process '%.32s'.\n",
              p -> processes[i].name);
	  err = 1;
	}
      ndd_free(nd);
    }

  resr__free_objects(pc, sint4, n);
  resr__free_objects(pm, int, n);

  if (!err)
    printf("*** Program validated.\n");

  return 0;
}

/****  Public functions.                                         ****/

/**  int  expl_init(p)  :  Optionally transmits the compiled state
                    machines to the state-space exploration tool. In
                    the case of an error, this function returns -1
                    and displays a message. In the  case of success,
                    it returns 0.                                  **/

int  expl_init(p)
  pgm_program *p;
{
  register ndd_relation *nr;
  register ndd_states   *ns, *nsr;

  if (!splash_explore)
    return 0;
   
  if (splash_verbose)
    printf("Translating the transition relation...\n");

  nr = build_relation(p);
  if (!nr)
    return -1;

  if (splash_verbose)
    printf("Translating the set of initial states...\n");

  ns = build_initial_states(p);
  if (!ns)
    {
      ndd_relation_free(nr);
      return -1;
    }

  if (splash_verbose)
    printf("Starting state-space exploration...\n");

  count = ZERO_INT8;

  nsr = ndd_relation_star_succ(nr, ns, 
        (splash_verbose ? callback : NULL));

  ndd_relation_free(nr);
  ndd_states_free(ns);
 
  if (!nsr)
    {
      report_lash_error("state-space exploration");
      return -1;
    }

  if (splash_verbose)
    {
      printf("Fixpoint reached in ");
      printf(FMT_UINT8, count);
      printf(" step(s).\n");
    }

  if (check_states(nsr, p) < 0)
    {
      ndd_states_free(nsr);
      report_splash_memory_error();
      return -1;
    }

  ndd_states_free(nsr);

  return 0;
}

/****  End of explore.c  ****/
