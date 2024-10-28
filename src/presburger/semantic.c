/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   semantic.c  :  Semantic routines.                            **/
/**                                                                **/
/**     12/14/00  :  Creation. (LL)                                **/
/**     05/23/01  :  New rule. (LL)                                **/
/**     05/28/01  :  Added exp_gen_terms. (LL)                     **/
/**     08/29/02  :  Reorganization. (BB)                          **/
/**     04/02/03  :  New rule. (LL)                                **/
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
#include "lash-types.h"
#include "resource.h"
#include "datastruct.h"
#include "presburger.h"
#include "lexical.h"
#include "semantic.h"
#include "expression.h"

/****  Prototypes of private functions.                          ****/

static void  semantic_error(lex_unit *, char *);

/****  Prototypes of private semantic functions.                 ****/

static int  sem0(stack *,  void **), 
 sem1(stack *, void **), 	 sem2(stack *, void **), 	
 sem3(stack *, void **), 	 sem4(stack *, void **), 	
 sem5(stack *, void **), 	 sem6(stack *, void **), 	
 sem7(stack *, void **), 	 sem8(stack *, void **), 	
 sem9(stack *, void **), 	 sem10(stack *, void **), 	
 sem11(stack *, void **), 	 sem12(stack *, void **), 	
 sem13(stack *, void **), 	 sem14(stack *, void **), 	
 sem15(stack *, void **), 	 sem16(stack *, void **), 	
 sem17(stack *, void **), 	 sem18(stack *, void **), 	
 sem19(stack *, void **), 	 sem20(stack *, void **), 	
 sem21(stack *, void **), 	 sem22(stack *, void **), 	
 sem23(stack *, void **), 	 sem24(stack *, void **), 	
 sem25(stack *, void **), 	 sem26(stack *, void **), 	
 sem27(stack *, void **), 	 sem28(stack *, void **), 	
 sem29(stack *, void **), 	 sem30(stack *, void **), 	
 sem31(stack *, void **), 	 sem32(stack *, void **), 	
 sem33(stack *, void **), 	 sem34(stack *, void **), 	
 sem35(stack *, void **), 	 sem36(stack *, void **), 	
 sem37(stack *, void **), 	 sem38(stack *, void **), 	
 sem39(stack *, void **), 	 sem40(stack *, void **), 	
 sem41(stack *, void **), 	sem42(stack *, void **); 

/****  Public variable.                                          ****/

int  (*sem_function[])(stack *, void **) = 
{
 sem0,   
 sem1,    sem2,    sem3,    sem4,    sem5,    sem6,   
 sem7,    sem8,    sem9,    sem10,   sem11,   sem12,  
 sem13,   sem14,   sem15,   sem16,   sem17,   sem18,  
 sem19,   sem20,   sem21,   sem22,   sem23,   sem24,  
 sem25,   sem26,   sem27,   sem28,   sem29,   sem30,  
 sem31,   sem32,   sem33,   sem34,   sem35,   sem36,  
 sem37,   sem38,   sem39,   sem40,   sem41,  sem42 };

/****  Private functions.                                        ****/

/**  void  semantic_error(p, msg)  :  Reports a semantic error that
                    has been detected during the parsing of the
                    lexical unit *p. A message to be included in
                    the report is given by *msg.                   **/

static void  semantic_error(p, msg)
  lex_unit *p;
  char     *msg;
{
  char line[256];

  sprintf(line, 
      "Compilation error at (L%u, C%u): %s", p -> line_no, 
      p -> char_no, msg);

  report_presb_error(line);
}


/****  Public functions.                                         ****/

/**  int  sem_init()  :  Initializes the semantic analyzer. Returns
                    0 in the case of success, and -1 if there is not
                    enough memory.                                 **/

int  sem_init()
{
  if (exp_init() < 0)
    {
      report_presb_memory_error();
      return -1;
    }

  return 0;
}

/**  int  sem_end()  :  Signals the end of the syntactic parsing.
                    Returns 0 in the case of success, and -1 in the 
                    case of an error.                              **/

int  sem_end()
{
  return  exp_end();
}

/**  int  sem_finish()  :  Shuts down the semantic analyzer. Returns
                    0 in the case of success, and -1 in the case of
                    an error.                                      **/

int  sem_finish()
{
  if (exp_finish() < 0)
    return -1;
  return 0;
}

/****  Private semantic functions.                               ****/

/*  <vars>          ::= NAME ':' */

static int sem0(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <atom_cond1_eq>    ::= <single_expr> EQ */

static int sem1(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_EQ);
  return 0;
}


/*  <atom_cond1>    ::= <single_expr> GE */

static int sem2(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_GE);
  return 0;
}

/*  <atom_cond1>    ::= <single_expr> '>' */

static int sem3(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_GT);
  return 0;
}

/*  <atom_cond1>    ::= <single_expr> LE */

static int sem4(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_LE);
  return 0;
}

/*  <single_{>      ::= '{'  */

static int sem5(st, r)
   stack *st;
   void **r;
{
  return 0;
}

/*  <single_(>      ::= '(' */

static int sem6(st, r)
   stack *st;
   void **r;
{
  return 0;
}

/*  <atom_cond1>    ::= <single_expr> '<' */

static int sem7(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_LT);
  return 0;
}

/*  <single_minus>  ::= '-' */

static int sem8(st, r)
   stack *st;
   void **r;
{
  return 0;
}

/*  <expr_1>        ::= <expr> '-'  */

static int sem9(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  if (exp_negative_next((exp_gen_term *) *r) < 0 )
    {
      report_presb_corrupted();
      return -1;
    }
  return 0;
}

/*  <single_name>   ::= NAME */

static int sem10(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <term>          ::= <const> '*' NAME */

static int sem11(st, r)
   stack *st;
   void **r;
{
           uint4 v;
  register sint4 c; 
           char *name;
  exp_gen_term  *t;

  name = (char *) (((lex_unit *) stack__top(st)) -> param);
  if (exp_declare_variable(name, 0, &v) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st,2));
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }
    
  c =  *((sint4 *) (((lex_unit *) stack__pick(st, 2)) -> param));
  t = exp_variable_term(c, v);

  lex_free_token((lex_unit *) stack__pick(st, 2));

  if (!t)
    return -1;

  *r = (void *) t ;
  
  return 0;
}

/* <term>          ::= <single_minus> NAME */ 

static int sem12(st, r)
   stack *st;
   void **r;
{
           uint4 v;
           char *name;
  exp_gen_term  *t;

  name = (char *) (((lex_unit *) stack__top(st)) -> param);
  if (exp_declare_variable(name, 0, &v) < 0)
    {
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }
    
  t = exp_variable_term(-1, v);

  if (!t)
    return -1;

  *r = (void *) t ;
  return 0;
}


/*  <atom_cond1>    ::= <single_expr> NE */

static int sem13(st, r)
   stack *st;
   void **r;
{
  *r =  ((lex_unit *) stack__pick(st, 1)) -> param;
  exp_expr_cond_op(EXP_OP_NE);
  return 0;
}

/*  <expr_1>        ::= <expr> '+'  */

static int sem14(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return exp_positive_next((exp_gen_term *) *r);
}

/*  <quantif>       ::= <quantif_2> ')' */

static int sem15(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <cond_(_)>      ::= <cond_(> ')' */

static int sem16(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/* <term>          ::= <single_(> <single_expr> ')' */

static int sem17(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <const>         ::= CONST  */

static int sem18(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <const>         ::= <single_minus> CONST  */

static int sem19(st, r)
   stack *st;
   void **r;
{
  register sint4 *v;
  
  v = (sint4 *) (((lex_unit *) stack__top(st)) -> param);   
  *v = - *v;  
  *r =  v; /* ( void *) */
  
  return 0;
}


/*  <atom_cond>     ::= <atom_cond_mod> CONST */
static int sem20(st, r)
   stack *st;
   void **r;
{
  sint4        *v;
  exp_gen_term *t;
  exp_gen_condition *gc;

  t = (exp_gen_term *) ((lex_unit *) stack__pick(st, 1)) -> param;

  v = (sint4 *) (((lex_unit *) stack__top(st)) -> param);   
  if (*v <= 0)
    {
      resr__free_object(v, sint4);
      semantic_error((lex_unit *) stack__top(st), 
		     "Value should be > 0");
      return -1;
    }
  t -> modulo_term = *v;
  resr__free_object(v, sint4);
  
  if (!( gc = resr__new_object(exp_gen_condition)))
    {
      report_presb_memory_error();
      return -1;
    }

  if (exp_expr_store_gen_cond(gc, t) < 0)
     return -1;

  *r = (void *) gc;
  return 0;
}

/*  <atom_cond_mod> ::= <atom_cond1_eq> <expr> MOD */
static int sem21(st, r)
   stack *st;
   void **r;
{
  register exp_gen_term *t1, *t2, *t;

  t2 = (exp_gen_term *) ((lex_unit *) stack__pick(st, 1)) -> param;
  t1 = (exp_gen_term *) ((lex_unit *) stack__pick(st, 2)) -> param;

  if (exp_negative_next(t1))
    {
      report_presb_corrupted();
      return -1;
    }

  exp_expr_cond_op(EXP_OP_MOD);
  
  t = (void *) exp_merge_terms(t1, t2);
  if (!t)
    return -1;

  *r = (void *) t;
  
  return 0;

}

/*  <presburger>    ::= <condition> */
static int sem22(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *gc;
  
  gc  = (exp_gen_condition *) (((lex_unit *) stack__top(st)) 
			       -> param);
  exp_store_the_expression(gc);
  
  return 0;
}

/*  <quantif_2>     ::= <quantif_1> <condition>  */

static int sem23(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *gc, *gc_new, *gc_tmp;
  register exp_quantifier    *q;
  
  gc  = (exp_gen_condition *) 
    (((lex_unit *) stack__top(st)) -> param);
  q   = (exp_quantifier *) 
    (((lex_unit *) stack__pick(st, 1)) -> param);
  
  if ((!gc) || (!q))
    {
      report_presb_corrupted();
      return -1;
    } 
  
  if (q -> type == Qforall) 
    {
      gc_tmp = resr__new_object(exp_gen_condition);
      gc_new = resr__new_object(exp_gen_condition);
      if ((!gc_tmp) || (!gc_new))
	{
	  exp_free_gen_condition(gc);
	  resr__free_object(gc_new, exp_gen_condition);
	  report_presb_memory_error();
	}
      gc_tmp -> type = EXP_COND_NOT;
      gc_tmp -> u.not.cond = gc;
      
      gc_new -> type = EXP_COND_EXISTS;
      gc_new -> u.exists.cond = gc_tmp;
      gc_new -> u.exists.name = q -> var.name;
      gc_new -> u.exists.no = q -> var.no_var;
      
      gc_tmp = gc_new;
      
      gc_new = resr__new_object(exp_gen_condition);
      if (!gc_new)
	{
	  exp_free_gen_condition(gc_tmp);
	  exp_free_gen_condition(gc);
	  report_presb_memory_error();
	}
      
      gc_new -> type = EXP_COND_NOT;
      gc_new -> u.not.cond = gc_tmp; 
    }
  else 
    {
      gc_new = resr__new_object(exp_gen_condition);
      if(!gc_new)
	{
	  exp_free_gen_condition(gc);
	  report_presb_memory_error();
	}
      
      gc_new -> type = EXP_COND_EXISTS;
      gc_new -> u.exists.cond = gc;
      gc_new -> u.exists.name = q -> var.name;
      gc_new -> u.exists.no = q -> var.no_var;
    }
  
  resr__free_object(q , exp_quantifier);
  *r = gc_new;
  
  return 0;
}

/*  <cond_(>        ::= <single_(> <condition>  */

static int sem24(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <condition>     ::= <cond1>  */

static int sem25(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <condition>     ::= <condition> OR <cond1>  */

static int sem26(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *c1, *c2, *c;
  
  c1 = (exp_gen_condition *) 
    (((lex_unit *) stack__top(st)) -> param) ;
  c2 = (exp_gen_condition *) 
    (((lex_unit *) stack__pick(st,2)) -> param) ;
  if ((!c1) ||  (!c1))
    {
      report_presb_corrupted();
      return -1;
    }
  
  c = resr__new_object(exp_gen_condition);
  if (!c)
    {
      exp_free_gen_condition(c1);
      exp_free_gen_condition(c2);
      report_presb_memory_error();
      return -1;
    }
  
  c -> type = EXP_COND_OR;
  c -> u.or.cond1 = c1;
  c -> u.or.cond2 = c2;
  
  *r = c;
  
  return 0;
}

/*  <condition>     ::= <quantif>  */

static int sem27(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <vars>          ::= NAME ',' <vars>  */

static int sem28(st, r)
   stack *st;
   void **r;
{
  semantic_error((lex_unit *) stack__top(st),
		 "Only one variable can be quantified at a time");
  return -1;
  
}

/*  <quantif_1>     ::= FORALL <single_(> <vars>  */

static int sem29(st, r)
   stack *st;
   void **r;
{
  register exp_quantifier *q;
  register char           *name;
           uint4           v;

  name = (char *) (((lex_unit *) stack__top(st)) -> param) ;
	   
  if (!name)
    {
      report_presb_corrupted();
      return -1;
    }
  
  name = (char *) (((lex_unit *) stack__top(st)) -> param);
  
  if (exp_declare_variable(name, 0, &v) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st,2));
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  q = resr__new_object(exp_quantifier);
  if (!q)
    {
      report_presb_memory_error();
      return -1;
    }
  
  q -> type = Qforall;
  q -> var.no_var = v;
  q -> var.name = NULL;
  
  *r = q;
  
  return 0;
}

/*  <quantif_1>     ::= EXISTS <single_(> <vars>  */

static int sem30(st, r)
   stack *st;
   void **r;
{
  register exp_quantifier *q;
  register char           *name;
           uint4           v;
 
  name = (char *) (((lex_unit *) stack__top(st)) -> param);
   
  if (!name)
    {
      report_presb_corrupted();
      return -1;
    }
  
  if (exp_declare_variable(name, 0, &v) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st,2));
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }
  
  q = resr__new_object(exp_quantifier);
  if (!q)
    {
      report_presb_memory_error();
      return -1;
    }
  
  q -> type = Qexists;
  q -> var.name = NULL;
  q -> var.no_var = v;
  
  *r  = q;
  
  return 0;
}

/*  <cond1>         ::= <cond2>  */

static int sem31(st, r)
     stack *st;
     void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <cond1>         ::= <cond1> AND <cond2>  */

static int sem32(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *c1, *c2, *c;

  c2 = (exp_gen_condition *) 
    (((lex_unit *) stack__top(st)) -> param);
  c1 = (exp_gen_condition *) 
    (((lex_unit *) stack__pick(st,2)) -> param);
  if ((!c1) ||  (!c1))
    {
      report_presb_corrupted();
      return -1;
    }
  
  c = resr__new_object(exp_gen_condition);
  if (!c)
    {
      exp_free_gen_condition(c1);
      exp_free_gen_condition(c2);
      report_presb_memory_error();
      return -1;
    }  
  
  c -> type = EXP_COND_AND;
  c -> u.and.cond1 = c1;
  c -> u.and.cond2 = c2;   
  *r = c;
  return 0;
}

/*  <cond2>         ::= <atom_cond>  */

static int sem33(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *p;
  
  p =  ((lex_unit *) stack__top(st)) -> param;   
  if (!p)
    {
      report_presb_corrupted();
      return -1;
    }
  
  *r = p;
  return 0;
}

/*  <cond2>         ::= NOT <atom_cond>  */

static int sem34(st, r)
   stack *st;
   void **r;
{
  register exp_gen_condition *p, *p_new;
   
  p =  ((lex_unit *) stack__top(st)) -> param;   
  if (!p)
    {  
      report_presb_corrupted();
      return -1;      
    }
  
  p_new = resr__new_object(exp_gen_condition);
  if (!p_new)
    {
      report_presb_memory_error();
      return -1;
    }
  
  p_new -> type = EXP_COND_NOT;
  p_new -> u.not.cond = p;
  
  *r = p_new;
  return 0;
}

/*  <atom_cond>     ::= <cond_(_)>  */

static int sem35(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <term>          ::= <single_name>  */

static int sem36(st, r)
   stack *st;
   void **r;
{
  register char         *name;
           uint4         v;
  register exp_gen_term *t ;
   
  name = (char *) (((lex_unit *) stack__top(st)) -> param);

  if (exp_declare_variable(name, 0, &v) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st,2));
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }
  
  if (!( t =  exp_variable_term( 1 , v )))
    {
      report_presb_memory_error();
      return -1;
    }
  
  *r = (void *) t;
  
  return 0;
}

/*  <term>          ::= <const>  */

static int sem37(st, r)
   stack *st;
   void **r;
{
  register exp_gen_term *t;
  
  t = exp_constant_term( 
		*((sint4 *) 
		  (((lex_unit *) stack__top(st)) -> param)));
  lex_free_token((lex_unit *) stack__top(st));
  
  if (!t)
    {
      report_presb_memory_error();
      return -1;
    }
  
  *r = t;

  return 0;
}

/*  <expr>          ::= <term>  */

static int sem38(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param ;
  return 0;
}

/*  <expr>          ::= <expr_1> <term>  */

static int sem39(st, r)
   stack *st;
   void **r;
{
  register  exp_gen_term *t1, *t2 ;

  t2 = (exp_gen_term *) ((lex_unit *) stack__top(st)) -> param;
  t1 = (exp_gen_term *) ((lex_unit *) stack__pick(st, 1)) -> param;
  
  *r = (void *) exp_merge_terms(t1, t2);

  if (!(*r))
    return -1;

  return 0;
}

/*  <single_expr>   ::= <expr>  */

static int sem40(st, r)
   stack *st;
   void **r;
{
  *r = ((lex_unit *) stack__top(st)) -> param ;
  return 0;
}

/*   <atom_cond>     ::= <atom_cond1_eq> <expr> */
static int sem41(st, r)
   stack *st;
   void **r;
{
  register exp_gen_term *t1, *t2, *t;
  register exp_gen_condition *gc;

  t2 = (exp_gen_term *) ((lex_unit *) stack__top(st)) -> param;
  t1 = (exp_gen_term *) ((lex_unit *) stack__pick(st, 1)) -> param;

  if (exp_negative_next(t1))
    {
      report_presb_corrupted();
      return -1;
    }

  t = (void *) exp_merge_terms(t1, t2);
  if (!t)
    return -1;

  if (!( gc = resr__new_object(exp_gen_condition)))
    {
      report_presb_memory_error();
      return -1;
    }

  if (exp_expr_store_gen_cond(gc, t) < 0)
     return -1;

  *r = (void *) gc;
  
  return 0;
}


/*   <atom_cond>     ::= <atom_cond1> <expr>  */

static int sem42(st, r)
   stack *st;
   void **r;
{
  register exp_gen_term *t1, *t2, *t;
  register exp_gen_condition *gc;

  t2 = (exp_gen_term *) ((lex_unit *) stack__top(st)) -> param;
  t1 = (exp_gen_term *) ((lex_unit *) stack__pick(st, 1)) -> param;

  if (exp_negative_next(t1))
    {
      report_presb_corrupted();
      return -1;
    }

  t = (void *) exp_merge_terms(t1, t2);
  if (!t)
    return -1;

  if (!( gc = resr__new_object(exp_gen_condition)))
    {
      report_presb_memory_error();
      return -1;
    }

  if (exp_expr_store_gen_cond(gc, t) < 0)
     return -1;

  *r = (void *) gc;
  
  return 0;
}

/****  End of semantic.c  ****/
