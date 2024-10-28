/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   expression.h                                                 **/
/**                                                                **/
/**     12/11/00  : Creation. (LL)                                 **/
/**     03/27/01  : Corrections. (BB)                              **/
/**     05/28/01  : Added exp_gen_terms. (LL)                      **/
/**     08/29/02  :  Reorganization. (BB)                          **/
/**     11/01/02  : Corrections. (LL)                              **/
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

/**
  Note on conditions:

  An atomic condition is either an equality or an inequality.
  A general condition is either an atomic condition, a negation
  of a general condition, a disjunction or a conjunction of 2 
  general conditions, or an existential quantification. (The 
  universal quantification are directly converted: 
  Forall (...) == not Exists not (...) 
**/

#ifndef PRESB_EXPRESSION_H
#define PRESB_EXPRESSION_H

#include "lash-types.h"
#include "datastruct.h"
#include "ndd.h"
#include "biguint.h"

/**  Term type definition.                                         **/

typedef struct {
  uint4  no_var;             /*  Variable reference.                */
  sint4  v;                  /*  Coefficient.                       */
  uint1  type;        
  uint1 is_quantified;
  uint4 quant_depth;
} exp_atom_term;

typedef struct {
  uint4         nb_terms, al_terms; 
  exp_atom_term *terms;
  int next_negative;
  int next_sign_set;
  sint4 constant_term;
  sint4 modulo_term;
} exp_gen_term;


/** Type of term definition */

#define PRESB_TERM_QUANTIFIED 1
#define PRESB_TERM_FREE 0

/**  Atomic Condition type definition.                             **/

typedef struct {
  enum { eq, cmp, ineq, mod }  cond_type;       
                             /*  Type of atomic condition.          */
  uint4     nb_el;           /*  Number of terms.                   */
  sint4     b;               /*  Second member.                     */
  sint4     m;               /*  Modulo (if appropriate)            */
  exp_atom_term *el;         /*  Array of terms.                    */
} exp_condition;

#define EXP_COND_ATOM 0
#define EXP_COND_AND 1
#define EXP_COND_OR 2
#define EXP_COND_NOT 3
#define EXP_COND_EXISTS 4

/** Condition type definition                                      **/

typedef struct _exp_gen_condition {
  uint1  type;                /* Type of condition                  */
  union {
    struct {
      exp_condition *cond;
    } atom;
    struct {
      struct _exp_gen_condition   *cond1, *cond2; 
    } or;
    struct {
      struct _exp_gen_condition   *cond1, *cond2; 
    } and;
    struct {
      struct _exp_gen_condition   *cond; 
    } not;
    struct {
      struct _exp_gen_condition   *cond; 
      char *name;
      uint4  no;
    } exists;
  } u;
} exp_gen_condition;

/**  Variable type definition.                                     **/

typedef struct _exp_var {
  char            *name;     /*  Name of variable.                  */
  uint1            is_free;  /*  Status of the variable             */
  uint4            no_free;
  uint4            no_var;
  uint1            type;     /*  type  (not used yet).              */
} exp_variable;

/** Quantifier definition **/

typedef struct {
  enum q_type { Qforall, Qexists } type;
  exp_variable var;
}  exp_quantifier ;

/** Alteration definition **/

typedef struct _exp_quant_info {
  struct _exp_quant_info *next;
  uint4 quant_depth;
  uint4 no_var;
} exp_quant_info;

/**  Comparison operators.                                          */

#define  EXP_OP_EQ       0   /*  Operator '=='.                     */
#define  EXP_OP_GE       1   /*  Operator '>='.                     */
#define  EXP_OP_GT       2   /*  Operator '>'.                      */
#define  EXP_OP_LE       3   /*  Operator '<='.                     */
#define  EXP_OP_LT       4   /*  Operator '<'.                      */
#define  EXP_OP_NE       5   /*  Operator '!='.                     */
#define  EXP_OP_MOD      6   /*  Operator '= .. mod ..'.            */

/**  Prototypes of public functions.                               **/

int            exp_init(void);
int            exp_finish(void);
int            exp_end(void);
int            exp_declare_variable(char *, uint1, uint4 *);
int            exp_lookup_variable(uint4 *, uint1 *, char *);
void           exp_expr_cond_op(uint1);
int            exp_negative_next(exp_gen_term *);
int            exp_positive_next(exp_gen_term *);
exp_gen_term  *exp_constant_term(sint4);
exp_gen_term  *exp_variable_term(sint4, uint4);
exp_gen_term  *exp_merge_terms(exp_gen_term *, exp_gen_term *);
int            exp_expr_store_gen_cond(exp_gen_condition *, exp_gen_term *); 
void           exp_free_condition(exp_condition *);
void           exp_free_gen_condition(exp_gen_condition *);
int            exp_var_classify(exp_gen_condition *, exp_quant_info *);
void           exp_store_the_expression(exp_gen_condition *);
ndd           *exp_generate_ndd(exp_gen_condition *, uint4);
biguint       *exp_count_sol(ndd *);

#endif  /* PRESB_EXPRESSION_H */

/****  End of expression.h  ****/
