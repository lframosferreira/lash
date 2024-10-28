/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   lexical.h  :  Lexical analyser routines.                     **/
/**                                                                **/
/**     12/14/00  :  Creation. (LL)                                **/
/**     08/29/02  :  Reorganization. (BB)                          **/
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

#ifndef PRESB_LEXICAL_H
#define PRESB_LEXICAL_H

#include <stdio.h>
#include "lash-types.h"
#include "expression.h"

/**  Terminal symbols of the grammar.                              **/
   
#define DOLLAR	      0	 /*  End-of-file  or  empty  stack.  */ 
#define AND	      1	 /*  Keyword  'and'.  */ 
#define COLON	      2	 /*  ':'.  */ 
#define COMMA	      3	 /*  ','.  */ 
#define EQ	      4	 /*  '='.  */ 
#define EXCLAM_M      5	 /*  '!'  */ 
#define GE	      6	 /*  '>='.  */ 
#define GREATER	      7	 /*  '>'.  */ 
#define LE	      8	 /*  '<='.  */ 
#define LEFT_BRC      9	 /*  '{'.  */ 
#define LEFT_PAR     10	 /*  '('.  */ 
#define LOWER	     11	 /*  '<'.  */ 
#define MINUS	     12	 /*  '-'.  */ 
#define NAME         13	 /*  Identifier.  */ 
#define NE	     14	 /*  '<>'.  */ 
#define NOT	     15	 /*  Keyword  'not'.  */ 
#define OR	     16	 /*  Keyword  'or'.  */ 
#define PLUS	     17	 /*  '+'.  */ 
#define RIGHT_BRC    18	 /*  '}'.  */ 
#define RIGHT_PAR    19	 /*  ')'.  */ 
#define TIMES	     20	 /*  '*'.  */ 
#define FORALL	     21	 /*  Keyword  'forall'.  */ 
#define EXISTS	     22	 /*  Keyword  'exists'.  */ 
#define CONST	     23	 /*  Integer  constant.  */ 
#define MOD          24  /*  Keyword  'mod'.     */

/**  Distinguished symbol.                                         **/

#define D_SYMBOL     25	

/**  Non-terminal symbols that have to be considered (indices are
     reversed-engineered from the grammar generator).              **/

#define CONDITION    26  /* <condition>     */
#define COND1        27  /* <cond1>         */
#define COND2        33  /* <cond2>         */
#define ATOM_COND    34  /* <atom_cond>     */
#define SINGLE_NAME  38  /*  <single_name>. */
#define CONST_C      40  /*  <const>.       */

/**  Lexical unit type definition.                                 **/

typedef struct {
  uint1  symbol;
  void  *param;
  uint4  line_no, char_no;
}  lex_unit;

/**  Prototypes of public functions.                               **/

int  lex_open(char *);
void lex_close(void);
int  lex_next_token(lex_unit *);
void lex_print_token(FILE *, lex_unit *);
void lex_free_token(lex_unit *);

#endif  /* PRESB_LEXICAL_H */

/****  End of lexical.h  ****/
