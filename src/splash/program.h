/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**    program.h  :  Extended state machines.                      **/
/**                                                                **/
/**     05/19/99  :  Creation. (BB)                                **/
/**     06/02/99  :  Continued. (BB)                               **/
/**     07/19/99  :  Minor corrections. (BB)                       **/
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

#ifndef SPLASH_PROGRAM_H
#define SPLASH_PROGRAM_H

#include "lash-types.h"

/**  Term type definition.                                         **/

typedef struct {
  uint4  no;                 /*  Variable reference.                */
  sint4  v;                  /*  Coefficient.                       */
} pgm_term;

/**  Operation type definition.                                    **/

typedef struct {
  uint4     lvalue,          /*  Left value (variable reference).  **/
            nb_el;           /*  Number of terms.                  **/
  sint4     c;               /*  Additive constant.                **/
  pgm_term *el;              /*  Array of terms.                   **/
   
} pgm_operation;

/**  Condition type definition.                                    **/

typedef struct {
  uint1     cond_type;       /*  Type of condition.                **/
  uint4     nb_el;           /*  Number of terms.                  **/
  sint4     b;               /*  Second member.                    **/
  pgm_term *el;              /*  Array of terms.                   **/
} pgm_condition;

/**  Condition types.                                              **/

#define  PGM_COND_EQU     0  /*  Equality ( ... = ...).             */
#define  PGM_COND_CMP     1  /*  Linear comparison ( ... <= ...).   */
#define  PGM_COND_INE     2  /*  Inequality ( ... != ...).          */

/**  Transition parameter type definition.                         **/

typedef union {
  pgm_operation  asgn;       /*  Parameters of assignment.          */
  pgm_condition  cond;       /*  Parameters of condition.           */
  char          *tmp_name;   /*  Name of unresolved label.          */
} pgm_parameter;

/**  Transition type definition.                                   **/

typedef struct {
  uint1          type;       /*  Type of transition.                */
  uint4          dest;       /*  Destination state index,           */
                             /*    if known.                        */
  pgm_parameter  param;      /*  Parameters.                        */
} pgm_transition;

/**  Transition types.                                             **/

#define  PGM_TRANS_ASGN   0  /*  Assignment.                        */
#define  PGM_TRANS_ASSERT 1  /*  Assertion.                         */
#define  PGM_TRANS_COND   2  /*  Condition.                         */
#define  PGM_TRANS_SKIP   3  /*  Jump.                              */
#define  PGM_TRANS_TMP    4  /*  Jump to unresolved label.          */

/**  State type definition.                                        **/

typedef struct {
  uint4           nb_trans,  /*  Number of outgoing transitions.    */
                  al_trans,  /*  Number of allocated  transitions.  */
                  id;        /*  Index after simplification.        */
  pgm_transition *trans;     /*  Outgoing transitions.              */
  unsigned        atomic  : 1,
                             /*  The immediate successors of this   */
			     /*  state must belong to the same      */
			     /*  process.                           */
                  labeled : 1,    
                             /*  At least one associated label?     */
                  reachable : 1,
                             /*  Potentially reachable?             */
                  fusionable : 1;
                             /*  Can be replaced by its successor?  */
} pgm_state;

/**  Variable type definition.                                     **/

typedef struct _pgm_var {
  char            *name;     /*  Name of variable.                  */
  uint4            no;       /*  Index.                             */
  sint4            init;     /*  Initial value.                     */
  struct _pgm_var *next;     /*  Linked list pointer.               */
} pgm_variable;

/**  Label type definition.                                        **/

typedef struct _pgm_label {
  char              *name;   /*  Name of label.                     */
  uint4              state;  /*  Index of corresponding state.      */
  struct _pgm_label *next;   /*  Linked list pointer.               */
} pgm_label;

/**  Process type definition.                                      **/

typedef struct {
  char          *name;       /*  Name of process.                   */
  uint4          nb_states,  /*  Number of states.                  */
                 al_states,  /*  Number of allocated states.        */
                 nb_ids;     /*  Number of simplified states.       */
  pgm_state     *states;     /*  Array of states.                   */
  pgm_variable  *local_vars; /*  Linked list of local variables.    */
  pgm_label     *labels;     /*  Linked list of labels.             */
} pgm_process;

/**  Control location type definition.                             **/

typedef struct _pgm_control {
  uint4                process_no, location_no;
                             /*  Indices (if known).                */
  char                *process_name, *location_name;
                             /*  Names (NULL if the corresponding   */
                             /*  index is known).                   */
  struct _pgm_control *next; /*  Linked list pointer.               */
} pgm_control;

/**  Transition reference type definition.                         **/

typedef struct _pgm_tr_ref { 
  pgm_transition     *tr;    /*  Pointer to transition.             */
  struct _pgm_tr_ref *next;  /*  Linked list pointer.               */
} pgm_tr_ref;

/**  Meta-transition type definition.                              **/

typedef struct _pgm_meta {
  pgm_control      *head,    /*  Linked list of starting control    */
                             /*  locations.                         */
                   *body;    /*  Linked list of control locations   */
                             /*  identifying the loop.              */
  pgm_tr_ref       *trans;   /*  List of corresponding transitions. */
  struct _pgm_meta *next;    /*  Linked list pointer.               */
  uint4             lno, cno;
                             /*  Position of the declaration in the */
                             /*  source file.                       */
} pgm_meta;

/**  Program type definition.                                      **/

typedef struct {
  uint4          nb_proc,    /*  Number of processes.               */
                 nb_vars;    /*  Total number of variables.         */
  pgm_process   *processes;  /*  Array of processes.                */
  pgm_variable  *global_vars;
                             /*  Linked list of global variables.   */
  pgm_meta      *meta;       /*  Linked list of meta-transitions.   */
} pgm_program;

/**  Comparison operators.                                          */

#define  PGM_OP_EQ       0   /*  Operator '=='.                     */
#define  PGM_OP_GE       1   /*  Operator '>='.                     */
#define  PGM_OP_GT       2   /*  Operator '>'.                      */
#define  PGM_OP_LE       3   /*  Operator '<='.                     */
#define  PGM_OP_LT       4   /*  Operator '<'.                      */
#define  PGM_OP_NE       5   /*  Operator '!='.                     */

/**  Prototypes of public functions.                               **/

int    pgm_init(void);
int    pgm_finish(void);
int    pgm_end(void);
int    pgm_new_state(uint4 *);
int    pgm_new_transition(uint1, uint4, pgm_parameter *);
int    pgm_new_process(void);
uint4  pgm_get_current_state(void);
void   pgm_set_atomic(void);
void   pgm_set_current_state(uint4);
int    pgm_declare_label(char *);
int    pgm_declare_local_variable(char *);
int    pgm_declare_global_variable(char *);
void   pgm_init_current_variable(sint4);
void   pgm_name_current_process(char *);
int    pgm_cleanup_current_process(void);
int    pgm_lookup_variable(uint4 *, char *);
void   pgm_expr_reset(void);
void   pgm_expr_cond_op(uint1);
void   pgm_expr_negative_next(void);
void   pgm_expr_positive_next(void);
int    pgm_expr_constant_term(sint4);
int    pgm_expr_variable_term(sint4, uint4);
int    pgm_expr_store_asgn(pgm_operation *, uint4);
int    pgm_expr_store_cond(pgm_condition *);
int    pgm_meta_local(uint4, uint4);
int    pgm_meta_global(uint4, uint4);
void   pgm_meta_sequence_begin(void);
int    pgm_meta_process(char *);
int    pgm_meta_label(char *);
int    pgm_meta_end(void);

#endif  /* SPLASH_PROGRAM_H */

/****  End of program.h  ****/
