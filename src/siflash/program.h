/********************************************************************/
/**                                                                **/
/**  Simple IF  LASH (Siflash) compiler -- v0.9                    **/
/**  ==================================                            **/
/**                                                                **/
/**    program.h  :  Extended state machines.                      **/
/**                                                                **/
/**     11/16/99  :  Creation. (LL)                                **/
/**     11/30/99  :  Modifications of conditions. (LL)             **/
/**     08/02/00  :  Added gates & synchronisations. (LL)          **/
/**     08/02/00  :  Changed transition parameters. (LL)           **/
/**     08/02/00  :  Removed transition types. (LL)                **/
/**     04/17/02  :  Modification. (LL)                            **/
/**     04/29/02  :  Modification. (LL)                            **/
/**     07/02/02  :  Modification for synchronisation. (LL)        **/
/**     09/06/02  :  Reorganization. (BB)                          **/
/**     09/13/02  :  Minor Modifications. (LL)                     **/
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

#ifndef SIFLASH_PROGRAM_H
#define SIFLASH_PROGRAM_H

#include "lash-types.h"
#include "datastruct.h"
#include "lash-ndd-io.h"

/**  Term type definition.                                         **/

typedef struct {
  uint4  no;                 /*  Variable reference.                */
  sint4  v;                  /*  Coefficient.                       */
} pgm_term;

/**  Structure used to store the  payload in the hash table 
     id_table.                                                     **/

typedef struct {
  uint4 process;
  uint4 state;
  uint4 trans;
  char process_defined;
  char state_defined;
  char trans_defined;
} id_table_payload;

typedef struct _fpar{
  char *name;
  uint1 ignored;
  struct _fpar *next;
} pgm_fpar; 

/**  term_list type definition.                                    **/

typedef struct _pgm_term_list {
  pgm_term term;
  struct _pgm_term_list *next;
}  pgm_term_list;

/**  Linear expression type definition                             **/

typedef struct {
  uint4      nb_el;          /*  Number of terms.                  **/
  pgm_term  *el;             /*  Array of terms.                   **/
  sint4      c;              /*  Additive constant.                **/
} pgm_lin_expr;

/**  Operation type definition.                                    **/

typedef struct {
  uint4         lvalue;      /*  Left value (variable reference).  **/
  pgm_lin_expr *expr;
} pgm_operation;

/**  General Operation type definition.                            **/

typedef struct {
  uint4 nb_op;               /*  Number of operations.             **/
  pgm_operation *op;         /*  Array of operations.              **/

} pgm_gen_operation;

/**  Atomic Condition type definition.                             **/

typedef struct {
  uint1     cond_type;       /*  Type of atomic condition.         **/
  uint4     nb_el;           /*  Number of terms.                  **/
  sint4     b;               /*  Second member.                    **/
  pgm_term *el;              /*  Array of terms.                   **/
} pgm_condition;

/**  Atomic Condition types.                                       **/

#define  PGM_COND_EQU     0  /*  Equality ( ... = ...).             */
#define  PGM_COND_CMP     1  /*  Linear comparison ( ... <= ...).   */
#define  PGM_COND_INE     2  /*  Inequality ( ... != ...).          */

/** Condition type definition                                      **/

typedef struct {
uint1 type;                   /* Type of condition                 **/
uint4 nb_args;                /* number of sub-conditions          **/
uint4 al_args;   /* ! */      /* number of slots allocated         **/
void  *args;                  /* Pointer towards the (atomic)
				 sub-conditions                    **/
} pgm_gen_condition;

#define ARGS_INC 6

/**  Condition types.                                              **/

#define  PGM_COND_OR     0  /* Disjunction of conditions (.. or ..) */
#define  PGM_COND_AND    1  /* Conjunction of conditions (.. and ..)*/
#define  PGM_COND_NOT    2  /* Negation of condition (not ..)       */
#define  PGM_COND_ATOM   3  /* Atomic condition                     */

#define PGM_EXPR_LIN     1  /* Linear expression: Sum of terms, 
			       where a term is the product of 
			       a constant by a parameter or a 
                               variable                             */
#define PGM_EXPR_COND    2  /* General condition                    */

/** Expression type definition?                                    **/

typedef struct {
  uint1 type;
  void *expr;
} pgm_expression;
 
/**  Transition type definition.                                   **/

typedef struct _pgm_transition {
  uint1          dest_type;    /* Indicates the destination type    */
  uint4          dest_no;      /* Destination state index,          */
                               /* if known                          */
  uint4          process_no;   /* Process index                     */
  char           *label, *dest_name;
  pgm_gen_operation *asgn;
  pgm_gen_condition *pre_cdt;
} pgm_transition;

#define PGM_TRANS_TYPE_NAME   0
#define PGM_TRANS_TYPE_SELF   1
#define PGM_TRANS_TYPE_ASSERT 2
#define PGM_TRANS_TYPE_STOP   3

/**  State type definition.                                        **/

typedef struct _pgm_state{
  char           *name;        /* Name of the state                 */
  uint4           nb_trans,    /* Number of outgoing transitions    */
                  al_trans,    /* Number of allocated  transitions  */
                  no,          /* Index before simplification       */
                  id,          /* Index after simplification        */
                  al_substates,/* Number of allocated sub-states    */
                  nb_substates;/* Number of sub-states              */
    
  pgm_transition *trans;       /* Outgoing transitions              */
  uint4          *substates;
  uint4           leaf_no;
  uint1           has_leaf_no;
  unsigned        atomic  : 1,
                               /* The immediate successors of this  */
			       /* state must belong to the same     */
			       /* process                           */
                  init : 1,    /* Is an 'init' substate ?           */
                  reachable : 1,
                               /* Potentially reachable?            */
                  fusionable : 1;
                               /* Can be replaced by its successor? */
} pgm_state;

/**  Constant type definition.                                     **/

typedef struct {
  char            *name;       /* Name of variable                  */
  sint4            val;        /* Value                             */
} pgm_constant;

/**  Variable type definition.                                     **/

typedef struct _pgm_var {
  char            *name;       /* Name of variable                  */
  uint4            no;         /* Index                             */
  uint1            is_parameter; /* 0 is variable, 1 if parameter   */
  uint1            is_ignored;   /* 1 if ignored variable           */
  sint4            init;       /* Initial value                     */
  struct _pgm_var *next;       /* Linked list pointer               */
} pgm_variable;

/**  Process type definition.                                      **/

typedef struct {
  char          *name;         /* Name of process                   */
  uint4          nb_instances; /* Number of instances               */
  uint4          nb_states,    /* Number of states                  */
                 al_states,    /* Number of allocated states        */
                 leaf_no,
                 dummy_no,
                 nb_ids;       /* Number of simplified states       */
  pgm_state     *states;       /* Array of states                   */
  uint1          has_leaf_no;
  uint4          nb_root_states;
  uint4         *root_states;
  uint4          al_root_states;
  pgm_variable  *local_vars;   /* Linked list of local variables    */
} pgm_process;

/**  Control location type definition.                             **/

typedef struct _pgm_control {
  uint4          process_no, location_no, transition_no;
                               /* Indices (if known)                */
  char          *process_name, *spec_name; 
  char           trans_defined;                    
                             
  struct _pgm_control *next;   /* Linked list pointer               */
} pgm_control;

/**  Transition reference type definition.                         **/

typedef struct _pgm_tr_ref { 
  pgm_transition     *tr;      /* Pointer to transition             */
  struct _pgm_tr_ref *next;    /* Linked list pointer               */
} pgm_tr_ref;

/**  Meta-transition type definition.                              **/

typedef struct _pgm_meta {
  pgm_control      *head,      /* Linked list of starting control   */
                               /* locations                         */
                   *body;      /* Linked list of control locations  */
                               /* identifying the loop              */
  pgm_tr_ref       *trans;     /* List of corresponding transitions */
  struct _pgm_meta *next;      /* Linked list pointer               */
  uint4             lno, cno;
                               /* Position of the declaration in    */
                               /* the source file.                  */
} pgm_meta;

/**  Program type definition.                                      **/

typedef struct {
  uint4          nb_proc,      /* Number of processes               */
                 nb_vars;      /* Total number of variables         */
  pgm_process   *processes;    /* Array of processes                */
  pgm_variable  *global_vars;
                               /* Linked list of global variables   */
  pgm_meta      *meta;         /* Linked list of meta-transitions   */
  ndd_labels    *labels_variables; 
                               /* Used when saving the result
				  of the state space exploration    */
				      
} pgm_program;

/**  External variable.                                            **/

extern pgm_program  the_program;

/**  Comparison operators.                                          */

#define  PGM_OP_EQ       0   /*  Operator '=='.                     */
#define  PGM_OP_GE       1   /*  Operator '>='.                     */
#define  PGM_OP_GT       2   /*  Operator '>'.                      */
#define  PGM_OP_LE       3   /*  Operator '<='.                     */
#define  PGM_OP_LT       4   /*  Operator '<'.                      */
#define  PGM_OP_NE       5   /*  Operator '!='.                     */

/**  Prototypes of public functions.                               **/

int                 pgm_init(void);
int                 pgm_finish(void);
int                 pgm_end(void);
int                 pgm_new_state(char *, uint4 *); 
pgm_transition     *pgm_create_transition(char *, uint1);
int                 pgm_add_transition(uint4, pgm_transition *); 
int                 pgm_add_substate(uint4, uint4); 
int                 pgm_add_state(uint4); 
void                pgm_set_state_options(uint4, uint1, uint1);
int                 pgm_new_process(char *name, uint4); 
int                 pgm_declare_local_variable(char *, uint1, uint1, 
					       sint4);
int                 pgm_declare_global_variable(char *, uint1, uint1,
						sint4);
int                 pgm_declare_local_type(char *, uint1);
int                 pgm_declare_global_type(char *, uint1);
int                 pgm_declare_local_constant(char *, sint4);
int                 pgm_declare_global_constant(char *, sint4);
int                 pgm_declare_parameter_alias(char *, uint4, uint1);
int                 pgm_cleanup_current_process(void); 
int                 pgm_lookup_constant(sint4 *,  char *);
int                 pgm_lookup_type(char *, uint1 *);
int                 pgm_lookup_variable(uint4 *, uint1 *, uint1 *,
					char *);
int                 pgm_is_parameter(uint4);
int                 pgm_lookup_process_variable(uint4 *, char *,
						char *);
pgm_gen_operation  *pgm_create_asgn(pgm_lin_expr  *, uint4);
int                 pgm_cond_create_from_expr(pgm_gen_condition **,
					      pgm_lin_expr *, uint1);
pgm_gen_operation  *pgm_composed_gop(pgm_gen_operation *, 
				     pgm_gen_operation *);
int                 pgm_cond_create(pgm_gen_condition **, 
				    pgm_gen_condition *, 
				    pgm_gen_condition *, uint1); 
int                 pgm_cond_neg( pgm_gen_condition **); 
void                pgm_negate_lin_expr(pgm_lin_expr *);
pgm_lin_expr       *pgm_add_lin_expr(pgm_lin_expr *, pgm_lin_expr *);
int                 pgm_multiply_lin_expr(pgm_lin_expr *, sint4);
pgm_lin_expr       *pgm_create_lin_expr(sint4, uint4);
pgm_lin_expr       *pgm_create_lin_expr_const(sint4);
int                 pgm_extract_variable(pgm_lin_expr *, uint4 *);
int                 pgm_extract_constant(pgm_lin_expr *, sint4 *);
int                 pgm_cond_normalise(pgm_gen_condition **); 
int                 pgm_meta_local(uint4, uint4);
int                 pgm_meta_global(uint4, uint4);
void                pgm_meta_sequence_begin(void);
int                 pgm_meta_process(char *);
int                 pgm_meta_label(char *);
int                 pgm_meta_end(void);
int                 pgm_is_process(char *);   
int                 pgm_resolve_labels(void);  
void                pgm_free_string(char *);
void                pgm_free_lin_expr(pgm_lin_expr *);
void                pgm_free_gen_condition(pgm_gen_condition *); 
void                pgm_free_expression(pgm_expression *); 
void                pgm_free_gen_operation(pgm_gen_operation *); 
void                pgm_free_state_elt(pgm_state *);
int                 pgm_store_pre_cond(pgm_gen_condition *);
void                pgm_store_trans_label(char *);
int                 pgm_store_fpar_elt(char *, uint1);
int                 pgm_declare_fpar_as_variable(void);
void                pgm_free_fpar(void);
void                pgm_store_asgn(pgm_gen_operation *); 
void                pgm_set_next_state(uint4);  

#endif  /* SIFLASH_PROGRAM_H */

/****  End of program.h  ****/
