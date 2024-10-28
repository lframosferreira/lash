/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**    lexical.h  :  Lexical analyzer.                             **/
/**                                                                **/
/**     05/05/99  :  Creation. (BB)                                **/
/**     05/14/99  :  Continued. (BB)                               **/
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

#ifndef SPLASH_LEXICAL_H
#define SPLASH_LEXICAL_H

#include <stdio.h>
#include "lash-types.h"

/**  Terminal symbols of the grammar.                              **/

#define  DOLLAR       0      /*  End-of-file or empty stack.        */

#define  ASGN         1      /*  '='.                               */
#define  ASSERT       2      /*  Keyword 'assert'.                  */
#define  ATOMIC       3      /*  Keyword 'atomic'.                  */
#define  BREAK        4      /*  Keyword 'break'.                   */
#define  COLON        5      /*  ':'.                               */
#define  COMMA        6      /*  ','.                               */
#define  CONST        7      /*  Integer constant.                  */
#define  DO           8      /*  Keyword 'do'.                      */
#define  DOT          9      /*  '.'.                               */
#define  EQ          10      /*  '=='.                              */
#define  FI          11      /*  Keyword 'fi'.                      */
#define  GE          12      /*  '>='.                              */
#define  GOTO        13      /*  Keyword 'goto'.                    */
#define  GREATER     14      /*  '>'.                               */
#define  IF          15      /*  Keyword 'if'.                      */
#define  LE          16      /*  '<='.                              */
#define  LEFT_BRC    17      /*  '{'.                               */
#define  LEFT_PAR    18      /*  '('.                               */
#define  LOWER       19      /*  '<'.                               */
#define  META        20      /*  Keyword 'meta'.                    */
#define  MINUS       21      /*  '-'.                               */
#define  NAME        22      /*  Identifier.                        */
#define  NE          23      /*  '!='.                              */
#define  OD          24      /*  Keyword 'od'.                      */
#define  PLUS        25      /*  '+'.                               */
#define  PROCESS     26      /*  Keyword 'process'.                 */
#define  RIGHT_BRC   27      /*  '}'.                               */
#define  RIGHT_PAR   28      /*  ')'.                               */
#define  SEMICOLON   29      /*  ';' or '->'                        */
#define  SEP         30      /*  '::'.                              */
#define  SKIP        31      /*  Keyword 'skip'.                    */
#define  TIMES       32      /*  '*'.                               */
#define  TYPE        33      /*  Keyword 'int'.                     */

/**  Distinguished symbol.                                         **/

#define  D_SYMBOL    34

/**  Non-terminal symbols that have to be considered (indices are
     reversed-engineered from the grammar generator).              **/

#define  VAR_REF     59      /*  <var_ref>.                         */
#define  ASGN_STMNT  54      /*  <asgn_stmnt>.                      */
#define  CONST_NT    46      /*  <const>.                           */
#define  SINGLE_NAME 60      /*  <single_name>.                     */

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

#endif  /* SPLASH_LEXICAL_H */

/****  End of lexical.h  ****/
