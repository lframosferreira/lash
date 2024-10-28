/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**   semantic.c  :  Semantic routines.                            **/
/**                                                                **/
/**     05/14/99  :  Creation. (BB)                                **/
/**     05/27/99  :  Continued. (BB)                               **/
/**     07/19/99  :  Minor correction (BB).                        **/
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
#include "lash-types.h"
#include "resource.h"
#include "datastruct.h"
#include "splash.h"
#include "lexical.h"
#include "semantic.h"
#include "program.h"

/****  Type definition for do - od and if - fi information 
       structures.                                               ****/

typedef struct {
  uint4  in, out;
} in_out;

/****  Global variables.                                         ****/

static stack          *do_od, *if_fi;    
                             /*  Do - od and if - fi input and      */
                             /*    output states.                   */
static uint4          nb_nested_bodies; 
                             /*  Nesting depth of current block.    */
static int            state_ok,  
                             /*  Is there a current state?          */
                      is_atomic, 
                             /*  Inside of atomic statements?       */
                      is_near_sep,      
                             /*  After SEP lexical unit?            */
                      is_local;
                             /*  Are variable declarations local?   */

/****  Prototypes of private functions.                          ****/

static void  semantic_error(lex_unit *, char *);
static void  useless_statement_warning(lex_unit *);
static int   create_new_state(uint4 *);
static int   create_new_transition(uint1, uint4, pgm_parameter *);
static int   create_next_transition(uint1, pgm_parameter *);
static int   create_new_process(void);
static int   declare_variable(char *);

/****  Prototypes of private semantic functions.                 ****/

static int  sem0(stack *,  void **), sem1(stack *,  void **),  
            sem2(stack *,  void **), sem3(stack *,  void **),  
            sem4(stack *,  void **), sem5(stack *,  void **),
            sem6(stack *,  void **), sem7(stack *,  void **),  
            sem8(stack *,  void **), sem9(stack *,  void **),  
            sem10(stack *, void **), sem11(stack *, void **),
            sem12(stack *, void **), sem13(stack *, void **), 
            sem14(stack *, void **), sem15(stack *, void **), 
            sem16(stack *, void **), sem17(stack *, void **),
            sem18(stack *, void **), sem19(stack *, void **), 
            sem20(stack *, void **), sem21(stack *, void **), 
            sem22(stack *, void **), sem23(stack *, void **),
            sem24(stack *, void **), sem25(stack *, void **), 
            sem26(stack *, void **), sem27(stack *, void **), 
            sem28(stack *, void **), sem29(stack *, void **),
            sem30(stack *, void **), sem31(stack *, void **), 
            sem32(stack *, void **), sem33(stack *, void **), 
            sem34(stack *, void **), sem35(stack *, void **),
            sem36(stack *, void **), sem37(stack *, void **), 
            sem38(stack *, void **), sem39(stack *, void **), 
            sem40(stack *, void **), sem41(stack *, void **),
            sem42(stack *, void **), sem43(stack *, void **), 
            sem44(stack *, void **), sem45(stack *, void **), 
            sem46(stack *, void **), sem47(stack *, void **),
            sem48(stack *, void **), sem49(stack *, void **), 
            sem50(stack *, void **), sem51(stack *, void **), 
            sem52(stack *, void **), sem53(stack *, void **),
            sem54(stack *, void **), sem55(stack *, void **), 
            sem56(stack *, void **), sem57(stack *, void **), 
            sem58(stack *, void **), sem59(stack *, void **),
            sem60(stack *, void **), sem61(stack *, void **), 
            sem62(stack *, void **), sem63(stack *, void **), 
            sem64(stack *, void **), sem65(stack *, void **),
            sem66(stack *, void **), sem67(stack *, void **), 
            sem68(stack *, void **), sem69(stack *, void **), 
            sem70(stack *, void **), sem71(stack *, void **),
            sem72(stack *, void **), sem73(stack *, void **), 
            sem74(stack *, void **);

/****  Public variable.                                          ****/

int  (*sem_function[])(stack *, void **) = 
{
    sem0,    sem1,    sem2,    sem3,    sem4,    sem5,    sem6,
    sem7,    sem8,    sem9,    sem10,   sem11,   sem12,   sem13,
    sem14,   sem15,   sem16,   sem17,   sem18,   sem19,   sem20,
    sem21,   sem22,   sem23,   sem24,   sem25,   sem26,   sem27,
    sem28,   sem29,   sem30,   sem31,   sem32,   sem33,   sem34,
    sem35,   sem36,   sem37,   sem38,   sem39,   sem40,   sem41,
    sem42,   sem43,   sem44,   sem45,   sem46,   sem47,   sem48,
    sem49,   sem50,   sem51,   sem52,   sem53,   sem54,   sem55,
    sem56,   sem57,   sem58,   sem59,   sem60,   sem61,   sem62,
    sem63,   sem64,   sem65,   sem66,   sem67,   sem68,   sem69,
    sem70,   sem71,   sem72,   sem73,   sem74
};   

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

  report_splash_error(line);
}

/**  void  useless_statement_warning(p)  :  Reports a statement with
                    no effect, detected during the parsing of the
                    lexical unit *p.                               **/

static void  useless_statement_warning(p)
  lex_unit *p;
{
  char line[256];

  sprintf(line, 
      "Compilation warning at (L%u, C%u): Statement has no effect",
      p -> line_no,  p -> char_no);

  report_splash_warning(line);
}

/**  int  create_new_state(p)  :  Creates a new state in the current
                    process, which becomes the new current state. If
                    the pointer p is not null, the index of the newly
                    created state is returned in *p. The function
                    returns 0 in the case of success, and -1 if there
                    is not enough memory.                          **/

static int  create_new_state(p)
  uint4 *p;
{
  if (state_ok && is_atomic)
    pgm_set_atomic();

  if (pgm_new_state(p) < 0)
    return -1;

  state_ok = 1;
  if (nb_nested_bodies > 1)
    is_atomic = 1;

  return 0;
}

/**  int  create_new_transition(t, d, p)  :  Creates a new transition
                    outgoing from the current state. The type of this
                    transition, the index of its destination state
                    and its parameters are respectively given by t, d
                    and *p. The function returns 0 in the case of 
                    success, and -1 if there is not enough memory. **/

static int  create_new_transition(t, d, p)
  uint1          t;
  uint4          d;
  pgm_parameter *p;
{
  is_near_sep = 0;

  if (!state_ok)
    return 0;
    
  return pgm_new_transition(t, d, p);
}

/**  int  create_next_transition(t, p)  :  Creates a new transition
                    outgoing from the current state. The destination
                    of this transition is a new state in the current
                    process that becomes the new current state. The
                    type of the new transition and its parameters are
                    respectively given by t and *p. The function
                    returns 0 in the case of success, and -1 if there
                    is not enough memory.                          **/

static int  create_next_transition(t, p)
  uint1          t;
  pgm_parameter *p;
{
  register uint4  origin;
           uint4  dest;

  is_near_sep = 0;

  if (!state_ok)
    return 0;

  origin = pgm_get_current_state();

  if (create_new_state(&dest) < 0)
    return -1;

  pgm_set_current_state(origin);

  if (pgm_new_transition(t, dest, p) < 0)
    return -1;

  pgm_set_current_state(dest);

  return 0;
}

/**  int  create_new_process()  :  Creates a new process. Returns 0
                    in the case of success, and -1 if there is not
                    enough memory.                                 **/

static int  create_new_process()
{
  if (pgm_new_process() < 0)
    return -1;

  is_atomic = is_near_sep = state_ok = 0;
  pgm_expr_reset();

  if (create_new_state(NULL) < 0)
    return -1;

  is_local = 1;

  return 0;
}

/**  int  declare_variable(n)  :  Declares a variable with the name
                    n. Returns 0 in the case of success, and -1 if 
                    there is not enough memory.                    **/

static int  declare_variable(n)
  char *n;
{
  return is_local ? pgm_declare_local_variable(n) :
         pgm_declare_global_variable(n);
}

/****  Public functions.                                         ****/

/**  int  sem_init()  :  Initializes the semantic analyzer. Returns
                    0 in the case of success, and -1 if there is not
                    enough memory.                                 **/

int  sem_init()
{
  nb_nested_bodies = ZERO_INT4;
  is_local         = 0;
  
  do_od = stack__new_empty(in_out);
  if (!do_od)
    return -1;

  if_fi = stack__new_empty(in_out);
  if (!if_fi)
    {
      stack__free(do_od);
      return -1;
    }

  if (pgm_init() < 0)
    {
      report_splash_memory_error();
      stack__free(do_od);
      stack__free(if_fi);
      return -1;
    }

  return 0;
}

/**  int  sem_end()  :  Signals the end of the syntactic parsing.
                    Returns 0 in the case of success, and -1 in the 
                    case of an error.                              **/

int  sem_end()
{
  return  pgm_end();
}

/**  int  sem_finish()  :  Shuts down the semantic analyzer. Returns
                    0 in the case of success, and -1 in the case of
                    an error.                                      **/

int  sem_finish()
{
  stack__free(do_od);
  stack__free(if_fi);

  if (pgm_finish() < 0)
    return -1;
  return 0;
}

/****  Private semantic functions.                               ****/

/**  <vdecl_2> ::= <vdecl_1> ASGN                                  **/

static int  sem0(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <asgn_stmnt> ::= <var_ref> ASGN                               **/

static int  sem1(st, r)
  stack *st;
  void **r; 
{
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/**  <stmnt> ::= BREAK                                             **/

static int  sem2(st, r)
  stack *st;
  void **r; 
{
  if (stack__is_empty(do_od))
    {
      semantic_error((lex_unit *) stack__top(st),
          "Misplaced break statement");
      return -1;
    }

  if (state_ok)
    {
       if (!is_near_sep)
	 pgm_set_atomic();

       if (create_new_transition(PGM_TRANS_SKIP, 
           ((in_out *) (stack__top(do_od))) -> out, NULL) < 0)
	 {
	   report_splash_memory_error();
	   return -1;
	 }

      state_ok = 0;
    }
  else
    useless_statement_warning((lex_unit *) stack__top(st));

  return 0;
}

/**  <body_2> ::= <body_2> NAME ':'                                **/

static int  sem3(st, r)
  stack *st;
  void **r; 
{
  if (!state_ok && create_new_state(NULL) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      report_splash_memory_error();
      return -1;
    }

  if (pgm_declare_label((char *) 
      (((lex_unit *) stack__pick(st, 1)) -> param)) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      return -1;
    }

  return 0;
}

/**  <if_stmnt_2> ::= <if_stmnt_2> NAME ':'                        **/

static int  sem4(st, r)
  stack *st;
  void **r; 
{
  if (!state_ok && create_new_state(NULL) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      report_splash_memory_error();
      return -1;
    }

  if (pgm_declare_label((char *) 
      (((lex_unit *) stack__pick(st, 1)) -> param)) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      return -1;
    }

  return 0;
}

/**  <do_stmnt_2> ::= <do_stmnt_2> NAME ':'                        **/

static int  sem5(st, r)
  stack *st;
  void **r; 
{
  if (!state_ok && create_new_state(NULL) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      report_splash_memory_error();
      return -1;
    }

  if (pgm_declare_label((char *) 
      (((lex_unit *) stack__pick(st, 1)) -> param)) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      return -1;
    }

  return 0;
}

/**  <vdecl_1b> ::= <vdecl> ','                                    **/

static int  sem6(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <mdecl_1> ::= <mdecl_2> ','                                   **/

static int  sem7(st, r)
  stack *st;
  void **r;
{
  return 0;
}

/**  <mdecl_3b> ::= <mdecl_3> ','                                  **/

static int  sem8(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <mdecl_3b> ::= <mdecl_4> ','                                  **/

static int  sem9(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <const> ::= CONST                                             **/

static int  sem10(st, r)
  stack *st;
  void **r; 
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/**  <const> ::= <single_minus> CONST                              **/

static int  sem11(st, r)
  stack *st;
  void **r; 
{
  register sint4 *v;

   v = (sint4 *) (((lex_unit *) stack__top(st)) -> param);
  *v = -*v;  
  *r = (void *) v;

  return 0;
}

/**  <cond_1> ::= <single_expr> EQ                                 **/

static int  sem12(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_cond_op(PGM_OP_EQ);
  return 0;
}

/**  <stmnt> ::= <if_stmnt_3> FI                                   **/

static int  sem13(st, r)
  stack *st;
  void **r; 
{
  in_out  p;

  if (state_ok)
    {
      pgm_set_atomic();
      if (create_new_transition(PGM_TRANS_SKIP,
          ((in_out *) (stack__top(if_fi))) -> out, NULL) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
    }

  stack__pop(if_fi, &p);
  pgm_set_current_state(p.out);
  state_ok = 1; 

  return 0;
}

/**  <cond_1> ::= <single_expr> GE                                 **/

static int  sem14(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_cond_op(PGM_OP_GE);
  return 0;
}

/**  <cond_1> ::= <single_expr> '>'                                **/

static int  sem15(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_cond_op(PGM_OP_GT);
  return 0;
}

/**  <cond_1> ::= <single_expr> LE                                 **/

static int  sem16(st, r)
  stack *st;
  void **r;
{
  pgm_expr_cond_op(PGM_OP_LE);
  return 0;
}

/**  <body_1> ::= '{'                                              **/

static int  sem17(st, r)
  stack *st;
  void **r; 
{
  switch(nb_nested_bodies)
    {
    case 0x0000:
      if (create_new_process() < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
      break;
    case 0xffff:
      semantic_error((lex_unit *) stack__top(st),
          "Too many nested atomic statements");
      return -1;
    }

  nb_nested_bodies++;

  return 0;
}

/**  <mdecl_1> ::= META '('                                        **/

static int  sem18(st, r)
  stack *st;
  void **r; 
{
  register int      (*f)(uint4, uint4);
  register lex_unit  *l;

  f = is_local ? pgm_meta_local : pgm_meta_global;
  l = (lex_unit *) stack__pick(st, 1);

  if (f(l -> line_no, l -> char_no) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  return 0;
}

/**  <cond_1> ::= <single_expr> '<'                                **/

static int  sem19(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_cond_op(PGM_OP_LT);
  return 0;
}

/**  <single_minus> ::= '-'                                        **/

static int  sem20(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <expr_1> ::= <term> '-'                                       **/

static int  sem21(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_negative_next();
  return 0;
}

/**  <single_name> ::= NAME                                        **/

static int  sem22(st, r)
  stack *st;
  void **r; 
{
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/**  <control> ::= NAME '.' NAME                                   **/

static int  sem23(st, r)
  stack *st;
  void **r; 
{
  if (is_local)
    {
      semantic_error((lex_unit *) stack__pick(st, 2),
          "Meta-transition control point not local");
      lex_free_token((lex_unit *) stack__top(st));
      lex_free_token((lex_unit *) stack__pick(st, 2));
      return -1;
    }

  if (pgm_meta_process(
    (char *) ((lex_unit *) stack__pick(st, 2)) -> param) < 0)
    {
      report_splash_memory_error();
      lex_free_token((lex_unit *) stack__top(st));
      lex_free_token((lex_unit *) stack__pick(st, 2));
      return -1;
    }

  if (pgm_meta_label(
    (char *) ((lex_unit *) stack__top(st)) -> param) < 0)
    {
      report_splash_memory_error();
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  return 0;
}

/**  <stmnt> ::= GOTO NAME                                         **/

static int  sem24(st, r)
  stack *st;
  void **r; 
{
  register int            f;
           pgm_parameter  p;

  f = !is_near_sep;

  if (state_ok)
    {
      p.tmp_name = (char *) (((lex_unit *) stack__top(st)) -> param);
      
      if (create_new_transition(PGM_TRANS_TMP, 0, &p) < 0)
	{
	  lex_free_token((lex_unit *) stack__top(st));
	  report_splash_memory_error();
	  return -1;
	}

      if (f)
	pgm_set_atomic();

      state_ok = 0;
    }
  else
    {
      useless_statement_warning((lex_unit *) stack__pick(st, 1));
      lex_free_token((lex_unit *) stack__top(st));
    }

  return 0;
}

/**  <vdecl_1> ::= TYPE NAME                                       **/

static int  sem25(st, r)
  stack *st;
  void **r; 
{
  if (declare_variable((char *) 
      (((lex_unit *) stack__top(st)) -> param)) < 0)
    {
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  return 0;
}

/**  <vdecl_1> ::= <vdecl_1b> NAME                                 **/

static int  sem26(st, r)
  stack *st;
  void **r; 
{
  if (declare_variable((char *) 
      (((lex_unit *) stack__top(st)) -> param)) < 0)
    {
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  return 0;
}

/**  <mdecl_2> ::= <mdecl_1> NAME                                  **/

static int  sem27(st, r)
  stack *st;
  void **r; 
{
  if (!is_local)
    {
      semantic_error((lex_unit *) stack__top(st),
          "Meta-transition start point without process id");
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  if (pgm_meta_label(
      (char *) ((lex_unit *) stack__top(st)) -> param) < 0)
    {
      report_splash_memory_error();
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  return 0;
}

/**  <mdecl_4> ::= <mdecl_3b> NAME                                 **/

static int  sem28(st, r)
  stack *st;
  void **r; 
{
  if (!is_local)
    {
      semantic_error((lex_unit *) stack__top(st),
          "Meta-transition control point without process id");
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  if (pgm_meta_label(
      (char *) ((lex_unit *) stack__top(st)) -> param) < 0)
    {
      report_splash_memory_error();
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  return 0;
}

/**  <cond_1> ::= <single_expr> NE                                 **/

static int  sem29(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_cond_op(PGM_OP_NE);
  return 0;
}

/**  <stmnt> ::= <do_stmnt_3> OD                                   **/

static int  sem30(st, r)
  stack *st;
  void **r; 
{
  in_out  p;

  if (state_ok)
    {
      pgm_set_atomic();
      if (create_new_transition(PGM_TRANS_SKIP,
          ((in_out *) (stack__top(do_od))) -> in , NULL) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
    }

  stack__pop(do_od, &p);
  pgm_set_current_state(p.out);
  state_ok = 1; 

  return 0;
}

/**  <expr_1> ::= <term> '+'                                       **/

static int  sem31(st, r)
  stack *st;
  void **r; 
{
  pgm_expr_positive_next();
  return 0;
}

/**  <body> ::= <body_3> '}'                                       **/

static int  sem32(st, r)
  stack *st;
  void **r; 
{
  if (!--nb_nested_bodies)
    is_local = 0;

  if (nb_nested_bodies < 2)
    is_atomic = 0;

  return 0;
}

/**  <mdecl_3> ::= <mdecl_2> ')'                                   **/

static int  sem33(st, r)
  stack *st;
  void **r; 
{
  pgm_meta_sequence_begin();
  return 0;
}

/**  <unit> ::= ';'                                                **/

static int  sem34(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <body_1> ::= <body_3> ';'                                     **/

static int  sem35(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <if_stmnt_1> ::= <if_stmnt_3> ';'                             **/

static int  sem36(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <do_stmnt_1> ::= <do_stmnt_3> ';'                             **/

static int  sem37(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <do_stmnt_1> ::= DO SEP                                       **/

static int  sem38(st, r)
  stack *st;
  void **r; 
{
  register uint4   s;
           in_out  p;

  if (is_near_sep)
    {
      semantic_error((lex_unit *) stack__pick(st, 1),
          "Invalid guard");
      return -1;	
    }

  if ((!state_ok && create_new_state(NULL) < 0) ||
      create_next_transition(PGM_TRANS_SKIP, NULL) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  s = pgm_get_current_state();

  if (create_new_state(NULL) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  p.in  = s;
  p.out = pgm_get_current_state();

  if (stack__push(do_od, &p) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  pgm_set_current_state(s);
  is_near_sep = 1;

  return 0;
}

/**  <if_stmnt_1> ::= IF SEP                                       **/

static int  sem39(st, r)
  stack *st;
  void **r; 
{
  register uint4   s;
           in_out  p;

  if (is_near_sep)
    {
      semantic_error((lex_unit *) stack__pick(st, 1),
          "Invalid guard");
      return -1;	
    }

  if ((!state_ok && create_new_state(NULL) < 0) ||
      create_next_transition(PGM_TRANS_SKIP, NULL) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  s = pgm_get_current_state();

  if (create_new_state(NULL) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  p.in  = s;
  p.out = pgm_get_current_state();

  if (stack__push(if_fi, &p) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  pgm_set_current_state(s);
  is_near_sep = 1;

  return 0;
}

/**  <if_stmnt_1> ::= <if_stmnt_3> SEP                             **/

static int  sem40(st, r)
  stack *st;
  void **r; 
{
  if (state_ok)
    {
      pgm_set_atomic();
      if (create_new_transition(PGM_TRANS_SKIP,
          ((in_out *) (stack__top(if_fi))) -> out, NULL) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
    }

  pgm_set_current_state(((in_out *) (stack__top(if_fi))) -> in);
  state_ok = is_near_sep = 1;

  return 0;
}

/**  <do_stmnt_1> ::= <do_stmnt_3> SEP                             **/

static int  sem41(st, r)
  stack *st;
  void **r; 
{
  if (state_ok)
    {
      pgm_set_atomic();
     if (create_new_transition(PGM_TRANS_SKIP,
         ((in_out *) (stack__top(do_od))) -> in, NULL) < 0)
       {
	 report_splash_memory_error();
	 return -1;
       }
    }

  pgm_set_current_state(((in_out *) (stack__top(do_od))) -> in);
  state_ok = is_near_sep = 1;

  return 0;
}

/**  <stmnt> ::= SKIP                                              **/

static int  sem42(st, r)
  stack *st;
  void **r; 
{
  if (state_ok)
    {
      if (create_next_transition(PGM_TRANS_SKIP, NULL) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
    }
  else
    useless_statement_warning((lex_unit *) stack__top(st));

  return 0;
}

/**  <program> ::= <unit>                                          **/

static int  sem43(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <program> ::= <program> <unit>                                **/

static int  sem44(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <stmnt> ::= ATOMIC <body>                                     **/

static int  sem45(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <unit> ::= PROCESS NAME <body>                                **/

static int  sem46(st, r)
  stack *st;
  void **r; 
{
  pgm_name_current_process( 
      (char *) (((lex_unit *) stack__pick(st, 1)) -> param));

  if (pgm_cleanup_current_process() < 0)
    return -1;

  return 0;
}

/**  <unit> ::= <decl>                                             **/

static int  sem47(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <body_3> ::= <body_1> <decl>                                  **/

static int  sem48(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <if_stmnt_3> ::= <if_stmnt_1> <decl>                          **/

static int  sem49(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <do_stmnt_3> ::= <do_stmnt_1> <decl>                          **/

static int  sem50(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <body_2> ::= <body_1>                                         **/

static int  sem51(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <body_3> ::= <body_2> <stmnt>                                 **/

static int  sem52(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <if_stmnt_3> ::= <if_stmnt_2> <stmnt>                         **/

static int  sem53(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <do_stmnt_3> ::= <do_stmnt_2> <stmnt>                         **/

static int  sem54(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <vdecl> ::= <vdecl_1>                                         **/

static int  sem55(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <decl> ::= <vdecl>                                            **/

static int  sem56(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <term> ::= <const>                                            **/

static int  sem57(st, r)
  stack *st;
  void **r; 
{
  register int  rc;

  rc = pgm_expr_constant_term( 
      *((sint4 *) (((lex_unit *) stack__top(st)) -> param))) < 0;
  lex_free_token((lex_unit *) stack__top(st));

  if (rc)
    {
      report_splash_memory_error();
      return -1;
    }

  return 0;
}

/**  <vdecl> ::= <vdecl_2> <const>                                 **/

static int  sem58(st, r)
  stack *st;
  void **r; 
{
  pgm_init_current_variable(
     *((sint4 *) (((lex_unit *) stack__top(st)) -> param)));
  lex_free_token((lex_unit *) stack__top(st));

  return 0;
}

/**  <mdecl_2> ::= <mdecl_1> <control>                             **/

static int  sem59(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <mdecl_4> ::= <mdecl_3b> <control>                            **/

static int  sem60(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <mdecl> ::= <mdecl_4>                                         **/

static int  sem61(st, r)
  stack *st;
  void **r; 
{
  if (pgm_meta_end() < 0)
    return -1;

  return 0;
}

/**  <decl> ::= <mdecl>                                            **/

static int  sem62(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <single_expr> ::= <expr>                                      **/

static int  sem63(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <stmnt> ::= <asgn_stmnt> <expr>                               **/

static int  sem64(st, r)
  stack *st;
  void **r; 
{
  pgm_parameter  p;

  if (pgm_expr_store_asgn(&p.asgn, 
      *((uint4 *) (((lex_unit *) stack__pick(st, 1)) -> param))) < 0)
    {
      lex_free_token((lex_unit *) stack__pick(st, 1));
      report_splash_memory_error();
      return -1;
    }

  if (state_ok && create_next_transition(PGM_TRANS_ASGN, &p) < 0)
    {
      report_splash_memory_error();
      return -1;
    }

  pgm_expr_reset();
  lex_free_token((lex_unit *) stack__pick(st, 1));

  return 0;
}

/**  <expr> ::= <expr_1> <expr>                                    **/

static int  sem65(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <cond> ::= <cond_1> <expr>                                    **/

static int  sem66(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <stmnt> ::= <cond>                                            **/

static int  sem67(st, r)
  stack *st;
  void **r; 
{
  pgm_parameter  p;

  if (pgm_expr_store_cond(&p.cond) < 0 || (state_ok && 
      create_next_transition(PGM_TRANS_COND, &p) < 0))
    {
      report_splash_memory_error();
      return -1;
    }
  
  pgm_expr_reset();
 
  return 0;
}

/**  <stmnt> ::= ASSERT <cond>                                     **/

static int  sem68(st, r)
  stack *st;
  void **r; 
{
  pgm_parameter  p;

  if (state_ok)
    {
      if (pgm_expr_store_cond(&p.cond) < 0 ||
          create_next_transition(PGM_TRANS_ASSERT, &p) < 0)
	{
	  report_splash_memory_error();
	  return -1;
	}
    }
  else
    useless_statement_warning((lex_unit *) stack__pick(st, 1));

  pgm_expr_reset();

  return 0;
}

/**  <term> ::= <var_ref>                                          **/

static int  sem69(st, r)
  stack *st;
  void **r; 
{
  register int  rc;

  rc = pgm_expr_variable_term(1, 
       *((uint4 *) (((lex_unit *) stack__top(st)) -> param))) < 0;
  lex_free_token((lex_unit *) stack__top(st));

  if (rc)
    {
       report_splash_memory_error();
       return -1;
    }

  return 0;
}

/**  <term> ::= <const> '*' <var_ref>                              **/

static int  sem70(st, r)
  stack *st;
  void **r; 
{
  register int  rc;

  rc = pgm_expr_variable_term( 
       *((sint4 *) (((lex_unit *) stack__pick(st, 2)) -> param)),
       *((uint4 *) (((lex_unit *) stack__top(st)) -> param))) < 0;

  lex_free_token((lex_unit *) stack__top(st));
  lex_free_token((lex_unit *) stack__pick(st, 2));

  if (rc)
    {
       report_splash_memory_error();
       return -1;
    }

  return 0;
}

/**  <var_ref> ::= <single_name>                                   **/

static int  sem71(st, r)
  stack *st;
  void **r; 
{
  register uint4 *p;

  p = resr__new_object(uint4);
  if (!p)
    {
      lex_free_token((lex_unit *) stack__top(st));
      report_splash_memory_error();
      return -1;
    }

  if (pgm_lookup_variable(p, 
           (char *) (((lex_unit *) stack__top(st)) -> param)) < 0)
    {
      semantic_error((lex_unit *) stack__top(st),
          "Reference to an undeclared variable");
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  lex_free_token((lex_unit *) stack__top(st));
  *r = (void *) p;

  return 0;
}

/**  <if_stmnt_2> ::= <if_stmnt_1>                                 **/

static int  sem72(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <do_stmnt_2> ::= <do_stmnt_1>                                 **/

static int  sem73(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/**  <expr> ::= <term>                                             **/

static int  sem74(st, r)
  stack *st;
  void **r; 
{
  return 0;
}

/****  End of semantic.c  ****/
