/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   expression.c                                                 **/
/**                                                                **/
/**     12/11/00  : Creation. (LL)                                 **/
/**     02/12/01  : Reorganisation. (BB)                           **/
/**     04/10/01  : Reorganisation (variables).  (LL)              **/
/**     05/28/01  : Added exp_gen_terms. (LL)                      **/
/**     03/27/02  : msdf and base global variables. (LL)           **/
/**     06/07/02  : Minor corrections. (LL)                        **/
/**     08/29/02  : Reorganization. (BB)                           **/
/**     11/01/02  : Corrections. (LL)                              **/
/**     05/27/03  : Reoganisation. (LL)                            **/
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
#include "ndd-count.h"
#include "ndd-congruence.h"
#include "auto-minimize.h"

/****  Definitions.                                              ****/

#define  EXP_GROWTH     0x10  /*  Growth quota for arrays.         */
#define  EXP_HASH_SIZE  0x400  /*  Size of identifier hash tables.  */

/****  Global variables.                                         ****/

static uint4         nb_vars, al_vars;
static uint4         nb_free_vars, nb_dummy;
static uint1          cond_op;
static hash_table    *id_table;
static exp_variable  *current_var;
static exp_variable  *variables ;
static uint8          nb_ins, nb_colls;
static exp_gen_condition *the_expression;

/****  Prototypes of private functions.                          ****/

static void *grow_table(void **, uint4 *, uint4 *, unsigned);
static void  free_string(char *);
static int   var_classify_atom( exp_condition *, exp_quant_info *);
static ndd  *generate_ndd_atom(exp_condition *, uint4);

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
      r = (void *) resr__malloc(EXP_GROWTH * n);
      if (!r)
	return NULL;
      *tp = r;
      *ap = EXP_GROWTH;
      *np = 1;
      return r;
    }

  if ((*np) >= (*ap))
    {     
      r = (void *) resr__realloc((uint1 *) (*tp), 
          (EXP_GROWTH + *ap) * n, (*ap) * n);
      if (!r)
	return NULL;
      *tp  = r;
      *ap += EXP_GROWTH;
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
  str = NULL;
}

/** var_classify_atom(c, qi):  classifies the variables appearing 
                    in the  condition c according to the 
		    quantification information of qi.
		    If the variable of a term is quantified, then
		    the flag is_quantified of the term is set to 1
		    and the quantification depth is set.
		    If the variable is not quantified, the flag is
		    left unchanged. If the variable is identified
		    as free for the first time, then its attibute
		    are altered in the table of variables variables
		    and the nb_free_variables is incremented.
		    Returns 0 in the case of success and -1 in the
		    case of failure.                               **/
static int   var_classify_atom(c, qi)
     exp_condition *c;
     exp_quant_info *qi;
{
  register uint4 i;
  exp_quant_info *qi_2;
  for (i= 0; i < c -> nb_el ; i++)
    {
      qi_2 = qi;
      while (qi_2)
	{
	  if ( (c -> el + i) -> no_var == qi_2 -> no_var )
	    {
	      (c -> el + i) -> quant_depth =  qi_2 -> quant_depth ;
	      (c -> el + i) -> is_quantified = 1;
	      break;
	    }
	  
	  qi_2 =qi_2 -> next;
	}
      
      if ((!qi_2)
	  && ( variables[(c -> el + i) -> no_var ].is_free == 0))
	{
	  variables[(c -> el + i) -> no_var ].is_free = 1;
	  variables[(c -> el + i) -> no_var ].no_free = 
	    nb_free_vars++;
	}
    }
  
  return 0;
}

/** generate_ndd_atom(c, nb_quant) : Creates an NDD corresponding to
                the condition c. The variables order in the ndd
                corresponds to the no_free field of the variables in
                the table variable.  The characteristics of the ndd
                (msdf , base) are set via global variables.  Return a
                pointer to the construct ndd in the case of success
                and NULL in the case of failure.                   **/

static ndd  *generate_ndd_atom(c, nb_quant)
     exp_condition *c;
     uint4 nb_quant;
{
  
  sint4 coef[nb_free_vars + nb_dummy + nb_quant], b = c -> b;
  register uint4 i, index;
  exp_atom_term *t;

  for (i = 0 ; i < nb_free_vars + nb_dummy + nb_quant ; i++)
    coef[i] = 0;
  
  for (i = 0 ; i < c -> nb_el ; i++)
    {
      t = c -> el + i;
      if (t -> is_quantified == 1) 
	index = nb_free_vars + nb_dummy -1 + t -> quant_depth;
      else
	index = (variables[t -> no_var]).no_free;
      
      coef[index] += t -> v ; 
    }
  
  if (c -> cond_type == mod)
    {
      if (msdf == 1)
	return ndd_create_congruence_msdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b, c->m);  
      else
	return ndd_create_congruence_lsdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b, c->m);  
    }
  if (c -> cond_type == cmp)
    {
      if (msdf == 1)
	return ndd_create_inequation_msdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b); 
      else
	return ndd_create_inequation_lsdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b);     
    }
  else if (c -> cond_type == eq)
    {
      if (msdf == 1)
	return ndd_create_equation_msdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b); 
      else
	return ndd_create_equation_lsdf(base, nb_free_vars + nb_dummy +
					  nb_quant, coef, b);    
    }

  else
    return NULL;
}

/****  Public functions.                                         ****/

/**  int  exp_init()  :  Initializes the data structures used by this
                    module. This function returns 0 in the case of
                    success, and -1 if there is not enough memory.
                                                                   **/
int  exp_init()
{
  id_table = hash__new_empty(EXP_HASH_SIZE);
  if (!id_table)
    {
      report_presb_memory_error();
      return -1;
    }

  current_var   = NULL;
  nb_colls      = ZERO_INT8;
  nb_ins        = ZERO_INT8;
  cond_op       = EXP_OP_EQ;
  nb_free_vars  = ZERO_INT4;
  nb_dummy      = ZERO_INT4;
  nb_vars       = ZERO_INT4;
  al_vars       = ZERO_INT4;
  variables     = NULL; 

  return 0;
}

/**  int  exp_finish()  :  Unallocates the data structures used by 
                    this module and optimizes the program that has
                    been generated. This function returns 0 in the
                    case of success, and -1 in the case of an error
                    (an error message is then output).             **/

int  exp_finish()
{
  register uint4 i;
  exp_variable *v;

  if (id_table)
    hash__free(id_table, (void (*)(void *)) free_string, 
	       (void (*) (void *)) uint4__free);

  if (variables)
    {
      for (i = 0 ; i < nb_vars ; i++)
	{	
	  v = variables + i;
	  if (v -> name)
	    free_string(v -> name);
	}
      resr__free_objects(variables, exp_variable, al_vars);
    }
  
  return 0;
}

/**  int  exp_end()  :  Signals the end of the syntatic parsing. The
                    purpose of this function is to optimize the
                    generated program as well as to start the end
                    application. The function returns 0 in the case of
                    success, and -1 in the case of an error (a message
                    is then output).                               **/

int  exp_end()
{
  ndd *nd;
  char *num_eval;
  biguint *num;

  if (out_expression_string(the_expression) < 0)
    return -1;

  if (exp_var_classify(the_expression, NULL) < 0)
    return -1;

 
  if (nb_free_vars == ZERO_INT4)
    nb_dummy = 1;

  nd = exp_generate_ndd(the_expression, 0);
  if (!nd)
    {
      exp_free_gen_condition(the_expression);
      return -1;
    }

  num = exp_count_sol(nd);
  if (!num)
    {
      exp_free_gen_condition(the_expression);
      ndd_free(nd);
      return -1;
    }

  if (nb_free_vars > 0)
    {
      if (!!(num_eval = biguint__tostring(num)))
	{
	  printf("Number of free variables: %u\n", nb_free_vars); 
	  printf("Number of solutions  :");
	  printf(" %s", num_eval);
	  printf(".\n");
	  resr__free_objects(num_eval, char, strlen(num_eval) + 1);
	}

      printf("Number of NDD states : %u.\n", 
	     auto_nb_states(nd -> automaton) );
 
      if (out_expression_ndd(nd, variables, nb_vars, nb_free_vars) < 0)
	return -1;
      
      if (out_expression_dot(nd) < 0)
	return -1;
    }
  else
    {
      printf("*** No free variables. *** \n");
      if (biguint__is_inf(num))
	printf("=>\t True.\n");
      else
	printf("=>\t False. \n");
    }

  exp_free_gen_condition(the_expression);
  ndd_free(nd);
 
  biguint__free(num);
 
  return 0;
}

/**  int  exp_declare_variable(name, t, no_var)  :  Tests whether name
                    is a new variable, in which case, the variable
		    is inserted in the hash table id_table as well 
		    as in the table variables. The newly allocated
		    variable number is stored in no_var.
		    If the variable has already been met, its number
		    is simply stored in no_var.
                    It unallocates the memory in which name is stored.
		    Returns 0 in the case of success, and -1 in the 
		    case of an error (a message is then output).   **/

int  exp_declare_variable(name, t, no_var)
  char *name;
  uint4 *no_var;
  uint1 t;
{
  register exp_variable *pv;
           void        **r;
	   uint1        t2;
	   uint4       num, *payload;

    if (!no_var)
      return -1;

#if LASH_CHECK_LEVEL >= 1
  if (hash__insert_string(id_table, name, &r, &nb_colls, &nb_ins) < 0)
#else
    if (hash__insert_string(id_table, name, &r) < 0)
	
#endif
    {
      resr__free_objects(name, char, strlen(name) + 1);
      report_presb_memory_error();
      return -1;
    }

  if (!r) /* variable already met */ 
    {
 
      if ((exp_lookup_variable(&num, &t2, name) < 0) || (t != t2))
	{
	  char line[256];
	  
	  sprintf(line, 
		  "Compiler error: variable name '%.32s'", name);
	  resr__free_objects(name, char, strlen(name) + 1);
	  report_presb_error(line);
	  return -1;
	}

      resr__free_objects(name, char, strlen(name) + 1);      
      *no_var = num;
      return 0;
    }

  else /* variable met for the first time */ 
    {
      pv = (exp_variable *) grow_table((void **) &variables, 
				       &nb_vars, &al_vars,
				       sizeof(exp_variable));
      if (!pv)
	{
	  report_presb_memory_error();
	  resr__free_objects(name, char, strlen(name) + 1);
	  return -1;
	}
      
      pv -> name = name;
      pv -> type = t;
      pv -> no_var = nb_vars -1;
      pv -> is_free = 0;
      *no_var = nb_vars -1;
      payload = resr__new_object(uint4);
      if (!payload)
	{
	  report_presb_memory_error();
	  return -1;
	}
      *payload = nb_vars -1;
      *r = (void *) payload;
      return 0;
    }
}
  
/**  int  exp_lookup_variable(p, t, n)  :  Searches for a variable of
                    name n in the current variable table. If such a
                    variable is found, then its index is returned in
                    *p and its type in *t, and the function returns 0.
		    Otherwise, the function returns -1.            **/
int  exp_lookup_variable(p, t, n)
  uint4 *p;
  uint1 *t;
  char  *n;
{
  register uint4 *index;

  index = (uint4 *) hash__lookup_string(id_table, n);
  
  if (!index)
    return -1;
  
  if (p){
    *p = *index;
    }
  
  if (t) {
    *t = variables[*index].type;
  }
    
  return 0;
}

/**  void  exp_expr_cond_op(c)  :  Registers the current condition 
                    operator to be used by the expression
                    generator. This function does not report errors.
                                                                   **/
void  exp_expr_cond_op(c)
  uint1  c;
{
  cond_op = c;
}

/** int exp_negative_next(t) : Stores in t the indication that
                    the next term should be
                    complemented.  
		    Returns -1 if t is corrupted, 0 otherwise. **/
int  exp_negative_next(t)
     exp_gen_term *t ;
{
  if ((!t) || (t -> next_sign_set))
    return -1 ;

  t -> next_sign_set = 1;
  t -> next_negative = 1;
  
  return 0 ;
}

/** int exp_positive_next(t) : Stores in t the indication that
                    the next term should not be complemented.  

		    Returns -1 if t is corrupted, 0 otherwise.     **/

int  exp_positive_next(t)
     exp_gen_term *t ;
{
  if ((!t) || (t -> next_sign_set))
    return -1;

  t -> next_sign_set = 1;
  t -> next_negative = 0;
  
  return 0;
}

/**  exp_gen_term  *exp_constant_term(v)  :  Generates the structure 
                    exp_gen_term corresponding to a constant term.
		    Returns a pointer to the newly allocated
		    structure in the case of success and NULL
		    in the case of insufficient memory.            **/

exp_gen_term  *exp_constant_term(v)
  sint4  v;
{
  exp_gen_term *t;
  if  (!(t = resr__new_object(exp_gen_term)))
    return NULL;

  t -> next_sign_set = 0;
  t -> nb_terms = ZERO_INT4;
  t -> al_terms = ZERO_INT4;
  t -> terms = NULL;
  t -> constant_term  = v;

  return t;
}

/**  exp_gen_term  *exp_variable_term(c, n)  : Generates
                    a term composed of the coefficient c and
                    the variable index n. Returns a pointer to the
		    allocated strcture in the case of
                    success, and NULL in the case of insufficient
                    memory.                                        **/

exp_gen_term  *exp_variable_term(c, n)
  sint4  c;
  uint4  n;
{
           exp_gen_term  *t;
  register exp_atom_term *pt;

  t = resr__new_object(exp_gen_term);
  
  if (!t)
    return NULL;

  t -> terms = NULL;
  t -> al_terms = ZERO_INT4;
  t -> nb_terms = ZERO_INT4;
  t -> constant_term = 0;
  t -> next_sign_set = 0;

  pt = (exp_atom_term *) grow_table((void **) &(t -> terms), 
				    &( t -> nb_terms), 
				    &( t -> al_terms),
				    sizeof(exp_atom_term));
  if (!pt)
    return NULL;

  pt -> no_var = n;
  pt -> v  = c;
  pt -> type = 0;        
  pt -> is_quantified = 0;
  pt -> quant_depth = 0;

  return t;
}

/** exp_gen_term   *exp_merge_terms(t1, t2) : Creates a new
               exp_gen_term corresponding to t1 + t2 or t1 - t2,
	       depending on the flag next_negative of t1.
	       In the case of success, it returns a pointer to the
	       newly allocated structure and frees t1 and t2. 
	       In the case of failure, it prints an appropriate
	       message and returns NULL.                          **/
    
exp_gen_term   *exp_merge_terms(t1, t2)
     exp_gen_term *t1, *t2;
{
  register int            i;
  register exp_atom_term *at;
           exp_gen_term  *t;

  if ((!t1) || (!t2) 
      || (t1 -> next_sign_set == 0)
      || (t2 -> next_sign_set != 0))
    { 
      report_presb_corrupted();
      return NULL; 
    } 
 
  if (!( t = resr__new_object(exp_gen_term)))
    {
      report_presb_memory_error();
      return NULL;
    }

 if  (t1 -> next_negative)
   {
     t2 -> constant_term *= -1;
     for (i = 0; i < t2 -> nb_terms; i++ )
       {
	 at = t2 -> terms + i;
	 at -> v *= (-1);
       }
   }

 at = resr__new_objects(exp_atom_term,
			t1 -> nb_terms + t2 -> nb_terms);
 if (!at)
   {
     report_presb_memory_error();
     return NULL ;
   }
 
 memcpy(at, t1 -> terms, t1 -> nb_terms * sizeof(exp_atom_term));
 memcpy(at + t1 -> nb_terms, t2 -> terms, 
	t2 -> nb_terms * sizeof(exp_atom_term));

 t -> al_terms = t1 -> nb_terms + t2 -> nb_terms;
 t -> nb_terms = t1 -> nb_terms + t2 -> nb_terms;
 t -> terms = at;
 t -> constant_term =  t1 -> constant_term + t2 -> constant_term;
 t -> next_sign_set = 0;
 
 if (t1 -> al_terms)
   resr__free_objects(t1 -> terms, exp_atom_term, t1 -> al_terms);
 resr__free_object(t1, exp_gen_term);

 if (t2 -> al_terms)
   resr__free_objects(t2 -> terms, exp_atom_term, t2 -> al_terms);
 resr__free_object(t2, exp_gen_term);

 return t;
}

/**  int  exp_expr_store_gen_cond(p, t)  :  Stores into the 
                    exp_gen_condition  *p 
                    the expression of the form t1 + .. + tn + c OP 0,
		    where the ti are the terms of t, c is the
		    constant term of t, and OP is the operator stored 
		    in the global variable cond_op.
		    Returns 0 and frees t in the case of
		    success, and -1 in the case of insufficient 
		    memory or in the case of corrupted argument(s)
		    and prints the appropriate message.            **/

int  exp_expr_store_gen_cond(p, t)
  exp_gen_condition *p;
  exp_gen_term *t;
{
  register exp_atom_term     *pt, *pt2;
  register sint4              offset;
  register int                compl;
  register uint4              i;

           exp_condition     *ac, *ac2;
           exp_gen_condition *p_ac, *p_ac2;

  if ((!p) || (!t))
    {
      report_presb_corrupted();
      return -1;
    }
  
  ac = resr__new_object(exp_condition);
  if (!ac)
    {
      report_presb_memory_error();
      return -1;
    }

  if (t -> nb_terms)
    {
      pt = resr__new_objects(exp_atom_term, t -> nb_terms);
      if (!pt)
	return -1;
      memcpy((char *) pt, (char *) (t -> terms),
	     t -> nb_terms * sizeof(exp_atom_term));
    }
  else
  {
    report_presb_error("No variable in condition");
    return -1;
  }

  switch(cond_op)
    {
    case EXP_OP_EQ:
      /* One needs to convert a == b into a >= b AND a <= b */
      ac -> cond_type = eq;
      offset         = ZERO_INT4;
      compl          = 0;
      break;
    case EXP_OP_GE:
      ac -> cond_type = cmp;
      offset         = ZERO_INT4;
      compl          = 1;
      break;
    case EXP_OP_GT:
      ac -> cond_type = cmp;
      offset         = -1;
      compl          = 1;
      break;
    case EXP_OP_LE:
      ac -> cond_type = cmp;
      offset         = ZERO_INT4;
      compl          = 0;
      break;
    case EXP_OP_LT:
      ac -> cond_type = cmp;
      offset         = -1;
      compl          = 0;
      break;
    case EXP_OP_MOD:
      ac -> cond_type = mod;
      ac -> m         = t->modulo_term;
      offset          = ZERO_INT4;
      compl           = 0;
      break;
    case EXP_OP_NE:
    default:     /* One needs to convert a != b into a > b OR a < b */
      p -> type = EXP_COND_OR;
      ac -> cond_type = ineq;
      offset         = -1;
      compl          = 0;
    }

  if (ac -> cond_type == ineq) 
                             /* a != b is equiv. to  a < b or a > b */
    { 
      p -> type = EXP_COND_OR;

      /* first condition */
      ac -> nb_el = t -> nb_terms;
      ac -> b     = - t -> constant_term + offset;
      ac -> el    = pt;
      ac -> cond_type = cmp;
  
      p_ac = resr__new_object(exp_gen_condition);
      if (!p_ac)
	{
	  report_presb_memory_error();
	  return -1;
	}
      
      p_ac -> type = EXP_COND_ATOM ;
      p_ac -> u.atom.cond =  ac;


      /* second condition */
       pt2 = resr__new_objects(exp_atom_term, t -> nb_terms);
      if (!pt2)
	{
	  report_presb_memory_error();
	  return -1;
	}
      
      memcpy((char *) pt2, (char *) (t -> terms), t -> nb_terms * 
	     sizeof(exp_atom_term)); 

      ac2 = resr__new_object(exp_condition);
      if (!ac2)
	return -1;
      ac2 -> nb_el = t -> nb_terms;
      ac2 -> b     = t -> constant_term + offset;
      ac2 -> el    = pt2;
      for (i = ZERO_INT4; i < t -> nb_terms; i++, pt2++)
	pt2 -> v = -(pt2 -> v);
      ac2 -> cond_type = cmp;
   
      p_ac2 = resr__new_object(exp_gen_condition);
      if (!p_ac2)
	return -1;
      p_ac2 -> type = EXP_COND_ATOM;
      p_ac2 -> u.atom.cond  =  ac2;

      p -> u.or.cond1 = p_ac;
      p -> u.or.cond2 = p_ac2;
    }
  else
    {
      ac -> nb_el = t -> nb_terms;
      ac -> b     = (compl ? t -> constant_term : - t ->constant_term)
		      + offset; 
      ac -> el    = pt;
      
      if (compl)
	for (i = ZERO_INT4; i < t -> nb_terms; i++, pt++)
	  pt -> v = -(pt -> v);
      
      p -> type = EXP_COND_ATOM;
      p -> u.atom.cond = ac;
    }

  if ( t -> al_terms )
    resr__free_objects( t -> terms , exp_atom_term, t -> al_terms) ;
  resr__free_object(t, exp_gen_term);

  return 0;
}

/** void free_condition(c) : Frees recursively the condition *c. 
                         Does not report errors.                   **/

void exp_free_condition(c)
exp_condition *c;
{
  resr__free_objects(c -> el, exp_atom_term, c -> nb_el);
  resr__free_object(c, exp_condition);
  return;
}

/** void free_gen_condition(c) : Frees recursively the subconditions 
                    of the general condition structure *c, or the 
		    elements of c, if c is an atomic condition.
                    Does not report errors.                        **/

void exp_free_gen_condition(c)
     exp_gen_condition *c;
{
  switch (c -> type) 
    {
    case EXP_COND_ATOM:
      exp_free_condition(c -> u.atom.cond); 
      break;
      
    case EXP_COND_NOT :
      exp_free_gen_condition(c -> u.not.cond);  
      break;
      
    case EXP_COND_OR :
      exp_free_gen_condition(c -> u.or.cond1);  
      exp_free_gen_condition(c -> u.or.cond2);  
      break;
      
    case EXP_COND_AND:
      exp_free_gen_condition(c -> u.and.cond1);  
      exp_free_gen_condition(c -> u.and.cond2);  
      break;

    case EXP_COND_EXISTS:
      exp_free_gen_condition(c -> u.exists.cond);  
      if (c -> u.exists.name)
	free_string(c -> u.exists.name);
      break;

    default:
      return;
    }
  
  resr__free_object(c, exp_gen_condition);
}

/** exp_var_classify(gc, qi):  modifies the variables appearing 

                    in the general condition gc according to the
		    quantification information of the linked list qi.
		    In addition, if gc is an existencial
		    quantification, then, a new exp_quant_info is
		    added that take into account the current
		    quantified variable.  This function also
		    identifies the free variables and modifies their
		    attributes in the table of variables variables.
		    Returns 0 in the case of success and -1 in the
		    case of failure.                               
**/
int  exp_var_classify(gc, qi) 
     exp_gen_condition *gc;
     exp_quant_info    *qi;
{
  register exp_quant_info *new_qi;

  switch (gc -> type)
    {
      case EXP_COND_ATOM:
	if (var_classify_atom(gc -> u.atom.cond, qi) < 0)
	  return -1;
	break;
	
    case EXP_COND_EXISTS:
      new_qi = resr__new_object(exp_quant_info);
      if (!new_qi)
	{
	  report_presb_memory_error();
	  return -1;
	}
      
      new_qi -> no_var = gc -> u.exists.no;
      if (qi)
	new_qi -> quant_depth = qi -> quant_depth + 1;
      else
	new_qi -> quant_depth = 1;

      new_qi -> next = qi;	

      gc -> u.exists.no = new_qi  -> quant_depth;
      
      if (exp_var_classify(gc -> u.exists.cond, new_qi) < 0)
	return -1;
      else
	  resr__free_object(new_qi, exp_quant_info);
      break;

    case EXP_COND_AND:
      if ((exp_var_classify(gc -> u.and.cond1, qi) < 0)
	  || (exp_var_classify(gc -> u.and.cond2, qi) < 0))
	return -1;
      break;
 
    case EXP_COND_OR:
      if ((exp_var_classify(gc -> u.or.cond1, qi) < 0)
	  || (exp_var_classify(gc -> u.or.cond2, qi) < 0))
	return -1;
      break;

    case EXP_COND_NOT:
      if (exp_var_classify(gc -> u.not.cond, qi) < 0)
	return -1;
      break;

    default: 
      return -1;
    }
      
  return 0;
}

/**  exp_store_the_expression(gc) : Stores the expression gc in the
                    variable the_expression. Frees gc. Does not report
                    errors.                                        **/
     
void   exp_store_the_expression(gc)
exp_gen_condition *gc;
{
  the_expression = gc;
  return;
}

/** exp_generate_ndd(gc, nb_quant) : Creates an NDD corresponding to
                    the general condition gc. The variables order in
                    the ndd corresponds to the no_free field of the
                    variables in the table variable.  The
                    characteristics of the NDD (msdf , base) are set
                    via global variables.  Return a pointer to the
                    construct ndd in the case of success and NULL in
                    the case of failure.                           **/
    
ndd *exp_generate_ndd(gc, nb_quant)
exp_gen_condition *gc;
uint4 nb_quant; 
{
  ndd *nd, *nd1, *nd2;

  switch (gc -> type)
    {
    case EXP_COND_ATOM:
      nd = generate_ndd_atom(gc -> u.atom.cond, nb_quant);
      break;
      
    case EXP_COND_NOT:
      nd1 = exp_generate_ndd(gc -> u.not.cond, nb_quant);
      if (!nd1)
	return NULL;
      nd2 = (msdf == 1) ? ndd_create_zn_msdf(base, nd1 -> dim) 
	: ndd_create_zn_lsdf(base, nd1 -> dim);
      
      if (!nd2)
	{
	  ndd_free(nd1);
	  return NULL;
	}
      
      nd = ndd_difference(nd2, nd1);
      ndd_free(nd1);
      ndd_free(nd2);
      break;
      
    case EXP_COND_AND:
      nd1 = exp_generate_ndd(gc -> u.and.cond1, nb_quant);
      if (!nd1)
	return NULL;

      nd2 = exp_generate_ndd(gc -> u.and.cond2, nb_quant);
      if (!nd2)
	{
	  ndd_free(nd1);
	  return NULL;
	}
      nd = ndd_intersection(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);
      break;

    case EXP_COND_OR:
      nd1 = exp_generate_ndd(gc -> u.or.cond1, nb_quant);
      if (!nd1)
	return NULL;

      nd2 = exp_generate_ndd(gc -> u.or.cond2, nb_quant);
      if (!nd2)
	{
	  ndd_free(nd1);
	  return NULL;
	}
      nd = ndd_union(nd1, nd2);
      ndd_free(nd1);
      ndd_free(nd2);
      break;

    case EXP_COND_EXISTS:
      nd1 = exp_generate_ndd(gc -> u.exists.cond, nb_quant + 1);
      if (!nd1)
	return NULL;
      
      if (nd1 -> dim != nb_free_vars + nb_dummy + nb_quant + 1)
	{
	  ndd_free(nd1);
	  return NULL;
	}

      nd = ndd_projection(nd1, nd1 -> dim - 1);
      ndd_free(nd1);
      break;
      
    default:
      return NULL;
    }
  
  if (auto_minimize(nd -> automaton) < 0)
    {
      ndd_free(nd);
      return NULL;
    }

  return nd;
}

/**  exp_count_sol(nd) :  Returns the number of elements in the
     NDD *nd.                                                      **/

biguint    *exp_count_sol(nd)
     ndd *nd;
{
  biguint *num;

  num = biguint__new_zero();
  if (!num)
    return NULL;
  
  if (ndd_count(nd, num) < 0)
    {
      biguint__free(num);
      return NULL;
    }
  
  return num;
}

/****  End of expression.c  ****/
