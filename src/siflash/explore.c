/********************************************************************/
/**                                                                **/
/**  Simple IF LASH (Siflash) compiler -- v0.9                     **/
/**  =================================                             **/
/**                                                                **/
/**    explore.c  :  Interface with state-space exploration tool.  **/
/**                                                                **/
/**     06/04/99  :  Creation. (BB)                                **/
/**     01/03/00  :  Added the generalised types. (LL)             **/
/**     03/02/00  :  Adjusted for ndd_relation_size(ns). (LL)      **/
/**     02/12/01  :  Minor corrections. (BB)                       **/
/**     05/07/01  :  New comment. (LL)                             **/
/**     04/17/02  :  Modification. (LL)                            **/
/**     09/06/02  :  Reorganization. (BB)                          **/
/**     09/12/02  :  Added parameters + Modification (LL)          **/
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
#include "siflash.h"
#include "program.h"
#include "arithmetic.h"
#include "explore.h"
#include "biguint.h"
#include "lash-auto-io.h"
#include "ndd.h"
#include "diag.h"

/****  Definitions.                                              ****/

#define  SIFLASH_NDD_BASE  2       /*  Numeration base for NDDs.    */
#define  SIFLASH_NDD_MSDF  1       /*  Most significant digit first */
                                   /*  in NDDs ?                    */

/****  Global variable.                                          ****/

static uint8          count;

/****  Prototypes of private functions.                          ****/
static sint4          state_index(pgm_process *, uint4);
static int            add_init_transition(ndd_relation *);
static int            add_parameter(ndd_relation *, uint4);
static int            add_global_parameters(ndd_relation *);
static int            add_local_parameters(ndd_relation *,
					    pgm_process *);
static ndd_relation  *build_relation(void);
static ndd_states    *build_initial_states(void);
static int            add_transition(ndd_relation *, pgm_process *, 
				     uint4, sint4, pgm_transition *,
				     uint4);
static int            add_meta_transition(ndd_relation *, 
					  pgm_meta *);
static ndd           *initial_ndd(void);
static void           init_variables(sint4 *, pgm_variable *);
static linear_transf *assign_transformation(pgm_gen_operation *, 
					    uint4); 

static linear_transf *condition_transformation(pgm_condition *,
					       uint4);
static linear_transf *gen_condition_transformation(
                                                pgm_gen_condition *,
						uint4); 
static linear_transf *generate_linear_transf(pgm_gen_condition *, 
					     uint1,
					     pgm_gen_operation *,  
					     uint1,
					     pgm_gen_condition *, 
					     uint1, uint4);
static int            callback(const ndd_states *);
static int            check_states(ndd_states *);
static int            generate_output_ndd(const ndd_states *);

/****  Private functions.                                        ****/

/**  sint4  state_index(pr, n)  :  Returns a number identifying the
                    state of index n of the process *pr. This number
                    is negative if the state is atomic, and strictly
                    positive otherwise. The number 0 is reserved for
                    the so-called error state and the number 1 (and -1)
		    is reserved for the initial state during which
		    the parameters are initialized.                **/

static sint4  state_index(pr, n)
  pgm_process *pr;
  uint4        n;
{
  while (pr -> states[n].fusionable)
    n = pr -> states[n].trans[0].dest_no;

  if (pr -> states[n].atomic)
    return -(pr -> states[n].id + 2);
  else
    return (pr -> states[n].id + 2);
}

/**  int   add_init_transition(nr)  :  Adds to the transition
                    relation nr the transition towards
		    the initial state (of the program) of each
		    process. Returns 0 in the case of success. If an
                    error occurs, the function displays a message and
                    returns -1.                                   **/

static int add_init_transition(nr)
     ndd_relation *nr;
{
  register uint4           i;
  register pgm_process    *pr;
  register sint4          *dest, *orig;
  register linear_transf  *lt;
  register int            *mask;

  
  dest = resr__new_objects(sint4, the_program.nb_proc);
  orig = resr__new_objects(sint4, the_program.nb_proc);
  mask = resr__new_objects(int, the_program.nb_proc);
  lt   = ndd_create_identity_transf(the_program.nb_vars);

  if ((!dest) || (!orig) || (!mask) || (!lt))
   {
     if (dest)
       resr__free_objects(dest, sint4, the_program.nb_proc);
     if (orig)
       resr__free_objects(orig, sint4, the_program.nb_proc);
     if (mask)
       resr__free_objects(mask, int, the_program.nb_proc);
     if (lt)
      ndd_transf_free(lt);
     report_siflash_memory_error();
     return -1;
   }

  for (i = ZERO_INT4, pr = the_program.processes; i < 
	 the_program.nb_proc;
       i++, pr++)
    {
      dest[i] = state_index(pr, pr -> leaf_no);
      orig[i] = 1;
      mask[i] = 1;
    }

  if (ndd_relation_add_sync_transition(nr,orig, dest, mask, lt) < 0)
    {
      resr__free_objects(dest, sint4, the_program.nb_proc);
      resr__free_objects(orig, sint4, the_program.nb_proc);
      resr__free_objects(mask, int, the_program.nb_proc);
      ndd_transf_free(lt);
      report_lash_error("ndd_relation_add_sync_transition");
      return -1;
    }

  resr__free_objects(dest, sint4, the_program.nb_proc);
  resr__free_objects(orig, sint4, the_program.nb_proc);
  resr__free_objects(mask, int, the_program.nb_proc);
  ndd_transf_free(lt);
  return 0;
}

/** int add_parameter(nr, no): Adds the
              meta-transitions corresponding to the initialization of
	      the parameter of index no to the transition
	      relation nr.
	      Return 0 in the case of success and -1 in the case of
	      failure. Reports a message in the case of failure.   **/
static int add_parameter(nr, no)
     ndd_relation *nr;
     uint4 no;
{
  register sint4         *a, *b, *loc;
  register uint4          i, loop;
  register int           *mask, rc; 
  register linear_transf *lt;
           uint4          nb;
  
  if (uint4__mult(&nb, the_program.nb_vars, the_program.nb_vars) < 0)
    {
      lash_errno = LASH_ERR_OVERFLOW;
      report_lash_error("add_parameter");
      return -1;
   }

  a    = resr__new_objects(sint4, nb);
  b    = resr__new_objects(sint4, the_program.nb_vars);
  loc  = resr__new_objects(sint4, the_program.nb_proc);
  mask = resr__new_objects(int, the_program.nb_proc);

  if ((!a) || (!b) || (!loc) || (!mask))
    {
      if (a)
      resr__free_objects(a, sint4, nb);
      if (b)
	resr__free_objects(b, sint4, the_program.nb_vars);
      if (loc)
	resr__free_objects(loc, sint4, the_program.nb_proc );
      if (mask)
	resr__free_objects(mask, int, the_program.nb_proc);
      report_siflash_memory_error();
      return -1;
    }

  for (i = ZERO_INT4; i < the_program.nb_proc; i++)
    {
      loc[i] = 1;
      mask[i] = 1;
    }


  memset(a, 0, nb * sizeof(sint4));
  memset(b, 0, the_program.nb_vars * sizeof(sint4));

  for (i = ZERO_INT4; i < the_program.nb_vars; i++)
    a[i + the_program.nb_vars * i] = 1;


  loop = ZERO_INT4;
  rc    = 0;

  while (loop < 2 && rc == 0)
    {
      if (loop == 0)
	b[no] = 1;
      else
	b[no] = -1;
      lt = ndd_create_transf(the_program.nb_vars, 
			     ZERO_INT4, a, b, NULL, NULL);
      if (lt)
	rc =  ndd_relation_add_metatransition(nr, loc, mask, lt);

      if ((!lt) || (rc < 0))
	{
	  resr__free_objects(a, sint4, nb);
	  resr__free_objects(b, sint4, the_program.nb_vars);
	  resr__free_objects(loc, sint4, the_program.nb_proc );
	  resr__free_objects(mask, int, the_program.nb_proc);
	  if (!lt)
	    report_lash_error("ndd_create_transf");
	  else
	    report_lash_error("ndd_relation_add_metatransition");
	  return -1;
	}
      ndd_transf_free(lt);
      loop++;
    }
  resr__free_objects(a, sint4, nb);
  resr__free_objects(b, sint4, the_program.nb_vars);
  resr__free_objects(loc, sint4, the_program.nb_proc );
  resr__free_objects(mask, int, the_program.nb_proc);
  return 0;
}

/** int add_global_parameters(nr): Adds the
              meta-transitions corresponding to the initialization of
	      the global parameters to the transition relation nr.
	      Return 0 in the case of success and -1 in the case of
	      failure. Reports a message in the case of failure.   **/
static int add_global_parameters(nr)
     ndd_relation *nr;
{
  register pgm_variable *pv;

  for (pv = the_program.global_vars; pv; pv = pv -> next)
    {
      if ((pv -> is_parameter > 0) &&
	  (pv -> is_ignored == 0)  &&
	  (add_parameter(nr,  pv -> no)  < 0))
	return -1;
    }
  return 0;
}

/** int add_local_parameters(nr, pr): Adds the
              meta-transitions corresponding to the initialization of
	      the local parameters of process pr to the transition
	      relation nr.
	      Return 0 in the case of success and -1 in the case of
	      failure. Reports a message in the case of failure.   **/
static int add_local_parameters(nr, pr)
     ndd_relation *nr;
     pgm_process  *pr;
{
  register pgm_variable *pv;

  for (pv = pr -> local_vars; pv; pv = pv -> next)
    {
      if ((pv -> is_parameter > 0) &&
	  (pv -> is_ignored == 0)  &&
	  (add_parameter(nr,  pv -> no)  < 0))
	return -1;
    }
  return 0;
}

/**  ndd_relation *build_relation()  :  Constructs a transition and
                    meta-transition relation corresponding to the
                    program *p. Returns a pointer to the constructed
                    relation structure in the case of success. If an
                    error occurs, the function displays a message and
                    returns a NULL pointer.                        **/

static ndd_relation *build_relation()
{
  register ndd_relation   *nr;
  register uint4           i, j, k;
  register pgm_process    *pr;
  register pgm_state      *ps;
  register pgm_transition *pt;
  register pgm_meta       *pm;

  if (the_program.nb_proc == 0)
    {
      report_siflash_error("Can not build relation: No instance of processes");
      return NULL;
    }


  nr = ndd_relation_new_empty(SIFLASH_NDD_BASE, the_program.nb_proc,
			      the_program.nb_vars, SIFLASH_NDD_MSDF); ;
  if (!nr)
      return NULL;

 for (i = ZERO_INT4, pr = the_program.processes; i < 
         the_program.nb_proc;
       i++, pr++)
    for (j = ZERO_INT4, ps = pr -> states; j < pr -> nb_states;
         j++, ps++)
      if (ps -> reachable && !(ps -> fusionable))
        for (k = ZERO_INT4, pt = ps -> trans; k < ps -> nb_trans;
             k++, pt++)
          {
             if (add_transition(nr, pr, i, state_index(pr, j), pt,
                                the_program. nb_vars) < 0)
               {
		 ndd_relation_free(nr);
		 return NULL;
	       }
          }
 
 if (add_init_transition(nr) < 0) 
   {
     ndd_relation_free(nr);
     return NULL;
   }
 
 if (siflash_verbose)
   printf(
"   with transitions                    : %lld NDD state(s).\n", 
       ndd_relation_size(nr)); 
 
 if (add_global_parameters(nr) < 0)
   {
     ndd_relation_free(nr);
     return NULL;
   }
 
 for (i = ZERO_INT4; i < the_program.nb_proc; i++)
   if (add_local_parameters(nr, the_program.processes + i) < 0)
     {
       ndd_relation_free(nr);
       return NULL;
     }
 
 if (siflash_verbose)
   printf(
"   with transitions & parameters       : %lld NDD state(s).\n", 
          ndd_relation_size(nr));

 for (pm = the_program.meta; pm; pm = pm -> next)
   if (add_meta_transition(nr, pm) < 0)
     return NULL;

 if (siflash_verbose)
   printf(
"   with transitions & meta-transitions : %lld NDD state(s).\n", 
          ndd_relation_size(nr));

 return nr;
}

/** ndd_states *build_initial_states() : Constructs a set of initial
                    states corresponding to the program the_program.
                    Returns a pointer to the constructed set structure
                    in the case of success. If an error occurs, the
                    function displays a message and returns a NULL
                    pointer.                                       **/

static ndd_states *build_initial_states()
{
  register ndd_states *ns;
  register sint4      *pc;
  register ndd        *nd;
  register uint4       i;
  register int         rc;

  ns = ndd_states_new_empty(SIFLASH_NDD_BASE, the_program. nb_proc,
           the_program. nb_vars, SIFLASH_NDD_MSDF);
  if (!ns)
    {
      report_lash_error("set of initial states");
      return NULL;
    }

  pc = resr__new_objects(sint4, the_program. nb_proc);
  if (!pc)
    {
      report_siflash_memory_error();
      ndd_states_free(ns);
      return NULL;
    }

  
  for (i = ZERO_INT4; i < the_program.nb_proc; i++)
    pc[i] = 1;
    

  nd = initial_ndd();
  if (!nd)
    {
      report_siflash_memory_error();
      resr__free_objects(pc, sint4, the_program. nb_proc);
      ndd_states_free(ns);
      return NULL;
    }

  rc = ndd_states_add_data(ns, pc, nd);
    
  resr__free_objects(pc, sint4, the_program. nb_proc);
  ndd_free(nd);

  if (rc < 0)
    {
      report_lash_error("set of initial values");
      ndd_states_free(ns);
      return NULL;
    }

  if (siflash_verbose)
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
  register int            i,rc;
           linear_transf *lt1, *lt2;
	   pgm_transition new_tr;

    lt = NULL;
       
    if (tr -> pre_cdt)
      {
	if ((tr -> pre_cdt) -> type == PGM_COND_OR)
	  {
	    for (i = 0; i < (tr -> pre_cdt) ->nb_args; i++)
	      {
		       
		new_tr = *tr ;
		new_tr.pre_cdt = (pgm_gen_condition *) 
		  (tr -> pre_cdt) -> args + i;
		if (add_transition(nr, pr, pn, ns, &new_tr, n) < 0)
		  return -1;
	      }
	    return 0;
	  }
	else
	  {
	    lt = gen_condition_transformation(tr -> pre_cdt, n);
	    if (!lt)
	      return -1;
	  }
	       
	if (tr -> dest_type == PGM_TRANS_TYPE_ASSERT)
	  {
	    rc = ndd_relation_add_transition(nr, pn, ns, 0, lt);
	    if (rc < 0)
	      {
		ndd_transf_free(lt);
		report_lash_error("assert transition");
		return -1;
	      }
	    ndd_transf_free(lt);
	    return 0;
	  }
	       
      } 
      
    if (tr -> asgn)
      {
	lt1 = assign_transformation(tr -> asgn, n);
	if (!lt1)
	  {
	    report_siflash_memory_error();
	    return -1;
	  }

	if (lt)
	  {	      
	    lt2 =  ndd_transf_compose(lt, lt1);
	    ndd_transf_free(lt1);
	    ndd_transf_free(lt);
	      
	    if (!lt2)
	      {
		report_siflash_memory_error();
		return -1;
	      }  
	    lt = lt2;
	  }
	else
	  lt = lt1;
	   
      } 

    if (!(tr -> asgn) && !(tr -> pre_cdt))
      {
	lt = ndd_create_identity_transf(n);
	if (!lt)
	  {
	    report_siflash_memory_error();
	    return -1;
	  }
   
      }
	       
    rc = ndd_relation_add_transition(nr, pn, ns,
				     state_index(pr, tr -> dest_no),
				     lt);
    if (rc < 0)
      {
	report_lash_error("adding a transition");
	return -1;
      }
	   
    ndd_transf_free(lt);
	   
    return 0;
}

/**  int  add_meta_transition(nr, pm)  :  Adds to the transition
                    relation information structure *nr the meta-
                    transition pm of the program the_program. In the 
		    case of an
                    error, this function displays a message and
                    returns -1. In the case of success, it returns 0.
                                                                   **/
static int  add_meta_transition(nr,  pm)
  ndd_relation *nr;
  pgm_meta     *pm;
{
  register sint4          *ppc;
  register int            *ppm,rc ;
  register uint4           nv,np,  *orig_c, *dest_c;
  register pgm_control    *pc;
  register pgm_tr_ref     *pt;
  register linear_transf  *ltg, *lt, *lt1;
  register pgm_transition *tr;
	   
  nv = the_program. nb_vars;
  np = the_program. nb_proc;
  
  ppc = resr__new_objects(sint4, np);
  if (!ppc)
    {
      report_siflash_memory_error();
      return -1;
    }

  ppm = resr__new_objects(int, np);
  if (!ppm)
    {
      resr__free_objects(ppc, sint4, np);
      report_siflash_memory_error();
      return -1;
    }

  memset(ppc, 0, np * sizeof(sint4));
  memset(ppm, 0, np * sizeof(int));

  orig_c = resr__new_objects(sint4, np);
  if (!orig_c)
    {
      resr__free_objects(ppc, sint4, np);
      resr__free_objects(ppm, int, np);
      report_siflash_memory_error();
      return -1;
    }
  
  dest_c = resr__new_objects(sint4, np );
  if (!dest_c)
    {
      resr__free_objects(ppc, sint4, np);
      resr__free_objects(ppm, int, np);
      resr__free_objects(orig_c, sint4, np);
      report_siflash_memory_error();
      return -1;
    }
 
  for (pc = pm -> head; pc; pc = pc -> next)
    {
      ppm[pc -> process_no] = 1;
      ppc[pc -> process_no] = state_index(the_program. processes +
					  (pc -> process_no), 
					  pc -> location_no);
    }

  ltg = ndd_create_identity_transf(nv);
  if (!ltg)
    {
      resr__free_objects(ppc, sint4, np);
      resr__free_objects(ppm, int, np);
      report_siflash_memory_error();
      return -1;
    }

  for (pt = pm -> trans; pt; pt = pt -> next)
    {
      lt = NULL;
      tr = pt -> tr;
      lt = generate_linear_transf(tr -> pre_cdt, 
				  (uint1)(tr -> pre_cdt ? 1 : 0),
				  tr -> asgn, 
				  (uint1)(tr -> asgn ? 1 : 0),
				  NULL, 0, nv);
      if (!lt) {
	resr__free_objects(orig_c, uint4 *, np);
	resr__free_objects(dest_c, uint4 *, np);
	resr__free_objects(ppc, sint4, np);
	resr__free_objects(ppm, int, np);
	ndd_transf_free(ltg);
	return -1;
      }
      
      lt1 = ndd_transf_compose(ltg, lt);
      ndd_transf_free(lt);
      ndd_transf_free(ltg);
      if (!lt1)
	{
	  resr__free_objects(ppc, sint4, np);
	  resr__free_objects(ppm, int, np);
	  resr__free_objects(orig_c, uint4 *, np);
	  resr__free_objects(dest_c, uint4 *, np);
	  report_lash_error("composition of transformations");
	  return -1;
	}
      ltg = lt1;
    } /* for (pt = pm -> trans; pt; pt = pt -> next) */
  
  /*
  printf("meta: l.");
  printf(FMT_UINT4,pm -> lno);
  printf(", c.");
  printf(FMT_UINT4,pm -> cno);
  printf("\n"); 
  print_linear_transf(ltg);
  */
  rc = ndd_relation_add_metatransition(nr, ppc, ppm, ltg);
  
  ndd_transf_free(ltg);
  resr__free_objects(ppc, sint4, np);
  resr__free_objects(ppm, int, np);
  resr__free_objects(orig_c, uint4 *, np);
  resr__free_objects(dest_c, uint4 *, np);
  
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
          report_siflash_warning(line);
          return 0;
        }
	
      default:
	report_lash_error("iteration of meta-transition");
	return -1;
      }
  
  return 0;
}

/**  ndd *initial_ndd()  :  Constructs an NDD accepting the set of
                    initial variable values of the program
		    the_program. ( the initial values are either
		    mentioned in the IF specification or taken as 0
		    by default.
		    Returns a pointer to a newly allocated NDD in
                    the case of success, and a NULL pointer if there
                    is not enough memory.                          **/

static ndd *initial_ndd()
{
  register sint4 *pv;
  register uint4  i;
  register ndd   *nd1, *nd2, *nd3;
  static   sint4  one[1] = { 1 };

  pv = resr__new_objects(sint4, the_program. nb_vars);
  if (!pv)
    return NULL;

  memset(pv, 0, (the_program. nb_vars) * sizeof(sint4)); 
  init_variables(pv, the_program. global_vars);

  for (i = ZERO_INT4; i < the_program. nb_proc; i++)
    init_variables(pv, the_program. processes[i].local_vars);

#if SIFLASH_NDD_MSDF != 0
  nd1 = ndd_create_equation_msdf(SIFLASH_NDD_BASE, 1, one, pv[0]);
#else
  nd1 = ndd_create_equation_lsdf(SIFLASH_NDD_BASE, 1, one, pv[0]);
#endif

  if (!nd1)
    {
      resr__free_objects(pv, sint4, the_program. nb_vars);
      return NULL;
    }

  for (i = 1; i < the_program. nb_vars; i++)
    {
#if SIFLASH_NDD_MSDF != 0
      nd2 = ndd_create_equation_msdf(SIFLASH_NDD_BASE, 1, one, pv[i]);
#else
      nd2 = ndd_create_equation_lsdf(SIFLASH_NDD_BASE, 1, one, pv[i]);
#endif
      if (!nd2)
	{
	  ndd_free(nd1);
	  resr__free_objects(pv, sint4, the_program. nb_vars);
	  return NULL;
	}

      nd3 = ndd_product(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);

      if (!nd3)
	{
	  resr__free_objects(pv, sint4, the_program. nb_vars);
	  return NULL;
	}

      nd1 = nd3;
    }

  resr__free_objects(pv, sint4, the_program. nb_vars);

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
    if (pl -> is_ignored == 0)
      pv[pl -> no] = pl -> init;
}


/**  linear_transf *assign_transformation(gop, n)  :  Returns a linear
                    transformation equivalent to the gen. assignment 
                    operation *gop. The number of variables is n. In
                    the case of success, this function returns a
                    pointer to a newly allocated transformation. In
                    the case of insufficient memory, it returns a
                    NULL pointer.                                  **/

static linear_transf *assign_transformation(gop, n)
  pgm_gen_operation *gop;
  uint4          n;
{
  register sint4         *a, *b;
  register uint4          i, j;  
  register pgm_term      *pt;
  register linear_transf *lt;
  register pgm_operation *op;
  register uint1         *flag;
  
  a = resr__new_objects(sint4, n * n);
  if (!a)
    return NULL;

  memset(a, 0, n * n * sizeof(sint4));
  
  b = resr__new_objects(sint4, n);
  if (!b)
    return NULL;

  memset(b, 0, n * sizeof(sint4));
  
  flag =  resr__new_objects(uint1, n);
  if (!flag)
    return NULL;
 
  memset(flag, 1, n * sizeof(uint1));


  for (j = ZERO_INT4, op = gop -> op; j < gop -> nb_op ; op++, j++)
    {
      for (i = ZERO_INT4, pt = (op -> expr) -> el; 
	   i < (op -> expr) -> nb_el; 
	   i++, pt++)
	a[op -> lvalue * n + pt -> no] += pt -> v;
      b[op -> lvalue] = (op -> expr) -> c ;
      flag[op -> lvalue] = 0;
    }

  for (i = 0; i < n ; i++)
    if   (flag[i] == 1)
      a[i * n + i] = 1;

  lt = ndd_create_transf( n, ZERO_INT4, a, b, NULL, NULL);

  resr__free_objects(b, sint4, n);
  resr__free_objects(a, sint4, n * n);
  resr__free_objects(flag, uint1, n);

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

/** linear_transf *gen_condition_transformation(op, n) : Returns a
                    linear transformation equivalent to the
                    generalised condition *gc. The number of variables
                    is n. In the case of success, this function
                    returns a pointer to a newly allocated
                    transformation.  The type of the generalized
                    condition has to be different from PGM_COND_OR.
                    In the case of failure, it returns a NULL pointer.
                                                                   **/

static linear_transf *gen_condition_transformation(gc, n)
  pgm_gen_condition *gc;
  uint4          n;
{
  register pgm_gen_condition *gec;
  register linear_transf *lt1, *lt2, *lt;
  register uint4 i;

  if (!gc)
    return NULL;
  
  lt = NULL;
   switch(gc ->type)
     {
     case PGM_COND_AND:
       for (i = 0; i < gc -> nb_args; i++)
	 {
	   gec = (pgm_gen_condition *) gc -> args + i;
	   lt1 = condition_transformation((pgm_condition *) 
					  gec -> args, n);
	   if (!lt1)
	     {
	       report_siflash_memory_error();
	       return NULL;
	     }  
	   
	   if (!lt)
	     lt2 = lt1;
	   else
	     {
	       lt2 =  ndd_transf_compose(lt, lt1);
	       ndd_transf_free(lt1);
	       ndd_transf_free(lt);
	     }
	   
	   if (!lt2)
	     {
	       report_siflash_memory_error();
	       return NULL;
	     }  
	   lt = lt2;
	 }  
	      
       if (!lt)
	 {
	   report_siflash_memory_error();
	   return NULL;
	 }
       break;

     case PGM_COND_ATOM:
       lt = condition_transformation((pgm_condition *)
				     gc -> args, n);
       
       if (!lt)
	 {
	   report_siflash_memory_error();
	   return NULL;
	 }
       break;

     default: 
       report_lash_error("condition type not valid");
       return NULL;
     }
   
   return lt;

}

/* linear_transf *generate_linear_transf(pre, has_pre, asgn,
                    has_asgn, post, has_post, nb):
                    Generates the linear transformation corresponding
		    to a transition whose precondition, assignment
		    and postcondition are respectively given by the
		    pgm_gen_condition *pre, the pgm_gen_operation
		    asgn and the pgm_gen_consition *post if there
		    are available. The availabilities are
		    respectively given by the uint1 has_pre,
		    has_asgn and has_post, which are > 0 in the case
		    of availability. 
		    nb is the number of variables in the system.
		    
		    Return a pointer to the newly allocated
		    linear_transf generated in the case of success
		    and NULL in the case of failure. */
 
static linear_transf *generate_linear_transf(pre, has_pre, asgn,
					     has_asgn, post, has_post,
					     nb)
     pgm_gen_condition *pre, *post;
     pgm_gen_operation *asgn;
     uint1 has_pre, has_asgn, has_post;
     uint4 nb;
{
  linear_transf *lt, *lt1, *lt2;
  lt = NULL;

  if (has_pre > 0)
    {
      lt = gen_condition_transformation(pre, nb);
      if (!lt) {
	return NULL;
      }  
    }  /* (has_pre > 0) */ 
  if (has_asgn > 0)
    {
      lt1 = assign_transformation(asgn, nb);
      if (!lt1)	{
	if (lt)
	  ndd_transf_free(lt);
	return NULL;
      }  
      
      if (!lt)
	lt = lt1;
      else
	{		  
	  lt2 =  ndd_transf_compose(lt, lt1);
	  ndd_transf_free(lt1);
	  ndd_transf_free(lt);
	  
	  if (!lt2) {
	    report_lash_error("composition of transformations");
	    return NULL;
	  }  
	  lt = lt2; 
	}
    }  /* (has_asgn > 0) */    
		  
  if (has_post > 0)
    {
      lt1 = gen_condition_transformation(post, nb);
      if (!lt1)	{
	if (lt)
	  ndd_transf_free(lt);
	return NULL;
      }  
	      
      if (!lt)
	lt = lt1;
      else
	{		  
	  lt2 =  ndd_transf_compose(lt, lt1);
	  ndd_transf_free(lt1);
	  ndd_transf_free(lt);
	  
	  if (!lt2) {
	    report_lash_error("composition of transformations");
	    return NULL;
	  }  
	  lt = lt2;
	}
    }/* (has_post > 0) */
  if ((has_asgn == 0) && (has_pre == 0) && (has_post == 0))
    {
      lt = ndd_create_identity_transf(nb);
      if (!lt)	{
	report_lash_error("identity transformation");
	return NULL;
      }
    } /*((has_asgn == 0) && (has_pre == 0) && (has_post == 0)) */     
 
  return lt;
}

/**  int  callback(ns)  :  Callback function called after each step
                    of the state-space exploration, if the compiler
                    is in verbose mode. This function displays the
                    number of NDD states of the current set of
                    reachable states, and returns 0.              
		    In addition, if siflash_save_steps is set,
		    then the ns is saved.                          **/

static int  callback(ns)
  const ndd_states *ns;
{
  biguint *tot_count;
  char *s_tot_count;

  if (siflash_save_steps == 1)
    {
      if (!(siflash_sndd_name || siflash_sdot_name))
	    {
	      report_lash_error("Can't save: no filename specified");
	      return -1;
	    }
      if (generate_output_ndd(ns) < 0)
	return -1;
    }
      
  if (!(tot_count = biguint__new_zero()))
    {
      report_siflash_memory_error();
      return -1;
    }
  
  
  if( ndd_count(ns -> ndd,  tot_count) < 0)
    {
      report_lash_error("Can't count ndd elements");
      return -1;
    }
  
  if (!(s_tot_count = biguint__tostring(tot_count)))
	{
	  report_lash_error("Can't convert to string");
	  return -1;
	}
   
  printf("   intermediate result : %u NDD state(s),\t %s state(s). \n",
	 auto_nb_states(ns -> ndd -> automaton), s_tot_count);
  
  resr__free_objects(s_tot_count, char, strlen(s_tot_count) + 1);
   
  count++;

  biguint__free(tot_count);

  return 0;
}

/** int check_states(ns) : Checks whether the set of reachable states
		    *ns contains an invalid state or not, and displays
		    an appropriate message.  The program that has been
		    explored is the_program. In the case of success,
		    this function returns 0.  In the case of
		    insufficient memory, it returns -1.            **/

static int  check_states(ns)
  ndd_states  *ns;
{
  register sint4 *pc;
  register int   *pm, err;
  register uint4  n, i;
  register ndd   *nd;

  n = the_program. nb_proc;  

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
              the_program. processes[i].name);
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

/** int generate_output_ndd(ns) : Outputs the ndd corresponding to the
		    data part of the state space, pointed by the
		    ndd_states ns, in the files and formats specified
		    in the command line arguments.  The name of the
		    variables are stored in the pgm_program
		    the_program.  The labels of the variables have the
		    type "name_p.name_var", with name_p being the name
		    of the process where the variable name_var
		    appears, or "global" in the case of a global
		    variable.  This function returns -1 and displays a
		    message in the case of an error, and returns 0 in
		    the case of success.
                                                                   **/

static int   generate_output_ndd(ns)
const ndd_states *ns;
{
  ndd_labels *labels;
  char text[256];
  ndd *nd;
  sint4 *pc;
  int *pm;
  register int i;
  pgm_variable *var;
  static int fileno = 0;
  char tmp_ndd_name[256], tmp_dot_name[256];

 if (!ns)
   {
     report_lash_error("generate_output_ndd:bad argument");
     return -1;
   }

  if (!(labels = ndd_labels_new(the_program. nb_vars)))
    {
      report_siflash_memory_error();
      return -1;
    }
    
  for (var = the_program. global_vars; var; var = var -> next)
    {
      if (var -> is_ignored > 0)
	continue;
      sprintf(text, "global.%.127s", var -> name);
      if (ndd_labels_set(labels, var -> no, text) < 0)
	{
	  ndd_labels_free(labels);
	  return -1;
	}
    }

  for (i=0; i < the_program. nb_proc ; i++)
    {
      for (var = the_program. processes[i].local_vars; var; 
	   var = var -> next)
	{
	  if (var -> is_ignored > 0)
	    continue;
	  sprintf(text, "%.127s.%.127s", 
		  the_program.processes[i].name, var -> name);
	  if (ndd_labels_set(labels, var -> no, text) < 0)
	    {
	      ndd_labels_free(labels);
	      return -1;
	    }
	}
    }

 if (!(pm = resr__new_objects(int, ns ->  ctrl_len)))
   {
     report_siflash_memory_error();
     return -1;
   }
 
 memset(pm, 0, ns -> ctrl_len * sizeof(int));
 
 if (!(pc = resr__new_objects(sint4, ns ->  ctrl_len)))
   {
     report_siflash_memory_error();
     return -1;
   }

     nd = ndd_states_get_data(ns, pc, pm);
     
     resr__free_objects(pm, int, ns -> ctrl_len);
     resr__free_objects(pc, sint4, ns -> ctrl_len);

 if (!nd) 
   return -1;

 if (siflash_sdot_name)
   {
     if (siflash_save_steps == 1)
       sprintf(tmp_dot_name,"%s%d.dot",siflash_sdot_name,fileno);
     else
      sprintf(tmp_dot_name,"%s.dot",siflash_sdot_name);
        
     if (auto_serialize_write_dot_file(nd -> automaton, 
					tmp_dot_name,
				       LASH_EXP_DIGIT) < 0)
       {
	 char msg[256];
      
	 sprintf(msg, "Can't write automaton in %s", tmp_dot_name);
	 report_siflash_error(msg);
	 ndd_free(nd);
	 ndd_labels_free(labels);   
	 return -1;
       }
   }

 if (siflash_sndd_name)
   {
     if (siflash_save_steps == 1)
       sprintf(tmp_ndd_name,"%s%d.ndd",siflash_sndd_name,fileno);
     else
       sprintf(tmp_ndd_name,"%s.ndd",siflash_sndd_name);
 
     if (ndd_serialize_write_file_labeled(nd, labels, tmp_ndd_name) 
	 < 0)
       {
	 char msg[256];
	 
	 sprintf(msg, "Can't save ndd in %s", tmp_ndd_name);
	 report_siflash_error(msg);
	 ndd_free(nd);
	 ndd_labels_free(labels);   
	 return -1;
       }
   }

 ndd_free(nd);
 ndd_labels_free(labels);
 fileno++;
 return 0;
}

/****  Public functions.                                         ****/

/**  int  expl_init()  :  Optionally transmits the compiled state
                    machines to the state-space exploration tool. In
                    the case of an error, this function returns -1
                    and displays a message. In the  case of success,
                    it returns 0.                                  **/

int  expl_init()
{
  register ndd_relation *nr;
  register ndd_states   *ns, *nsr;
 

  if (!(siflash_explore) &&
      (siflash_sndd_name || siflash_sdot_name))
     {
       ns = build_initial_states();
       if ((!ns) ||  (generate_output_ndd(ns) < 0))
	 return -1;
        ndd_states_free(ns);
       return 0;
     }
  
  if (!(siflash_explore))
      return 0;

  if (siflash_verbose)
    printf("Translating the transition relation...\n");

  nr = build_relation();
  if (!nr)
    return -1;

  if (siflash_verbose)
    printf("Translating the set of initial states...\n");

  ns = build_initial_states();
  if (!ns)
    {
      ndd_relation_free(nr);
      return -1;
    }

  if (siflash_verbose)
    printf("Starting state-space exploration...\n");

  count = ZERO_INT8;

  if (siflash_sndd_name && (siflash_save_steps == 1) &&
      (generate_output_ndd(ns) < 0))
    return -1;


  nsr = ndd_relation_star_succ(nr, ns, 
        (siflash_verbose || siflash_save_steps ? callback : NULL));

  ndd_relation_free(nr);
  ndd_states_free(ns);
 
  if (!nsr)
    {
      report_lash_error("state-space exploration");
      return -1;
    }

  if (siflash_verbose)
    {
      printf("Fixpoint reached in ");
      printf(FMT_UINT8, count);
      printf(" step(s).\n");
    }

  if (check_states(nsr) < 0)
    {
      ndd_states_free(nsr);
      report_siflash_memory_error();
      return -1;
    }

  if ((siflash_sndd_name || siflash_sdot_name) &&
      (generate_output_ndd(nsr) < 0))
    return -1;

   ndd_states_free(nsr);

  return 0;
}

/****  End of explore.c  ****/
