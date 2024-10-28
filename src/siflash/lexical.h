/********************************************************************/
/**                                                                **/
/**  Simple IF LASH (Siflash) compiler -- v0.9                     **/
/**  =================================                             **/
/**                                                                **/
/**    lexical.h  :  Lexical analyzer.                             **/
/**                                                                **/
/**     07/10/02  :  Creation. (LL)                                **/
/**     07/26/02  :  Modifications for IF2.0. (LL)                 **/
/**     09/05/02  :  Reorganization. (BB)                          **/
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

#ifndef SIFLASH_LEXICAL_H
#define SIFLASH_LEXICAL_H

#include <stdio.h>
#include "lash-types.h"

/**  Terminal symbols of the grammar.                              **/

#define DOLLAR		0	 /*  End-of-file  or  empty  stack. */
#define ABSTRACT	1	 /*  Keyword  'abstract'            */
#define ACTIVE		2	 /*  Keyword  'active'              */
#define AND		3	 /*  Keyword  'and'                 */
#define ARRAY		4	 /*  Keyword  'array'               */
#define ASGN		5	 /*  ':='                           */
#define ASSERT		6	 /*  Keyword  'assert'              */
#define BOOLEAN		7	 /*  Keyword  'boolean'             */
#define BY		8	 /*  Keyword  'by'                  */
#define CALL		9	 /*  Keyword  'call'                */
#define CLOCK		10	 /*  Keyword  'clock'               */
#define CODE_EXT	11	 /*  External  code                 */
#define COEFF		12	 /*  Constant  coefficient          */
#define COLON		13	 /*  ':'                            */
#define COMMA		14	 /*  ','                            */
#define CONST		15	 /*  Keyword  'const'               */
#define DEADLINE	16	 /*  Keyword  'deadline'            */
#define DELAY		17	 /*  Keyword  'delay'               */
#define DELAYABLE	18	 /*  Keyword  'delayable'           */
#define DO		19	 /*  Keyword  'do'                  */
#define DOT		20	 /*  '.'                            */
#define DOTDOT		21	 /*  '..'                           */
#define EAGER		22	 /*  Keyword  'eager'               */
#define ELSE		23	 /*  Keyword  'else'                */
#define ENDABSTRACT	24	 /*  Keyword  'endabstract'         */
#define ENDASSERT	25	 /*  Keyword  'endabstract'         */
#define ENDENUM		26	 /*  Keyword  'endenum'             */
#define ENDIF		27	 /*  Keyword  'endif'               */
#define ENDPROCEDURE	28	 /*  Keyword  'endprocedure'        */
#define ENDPROCESS	29	 /*  Keyword  'endprocess'          */
#define ENDRECORD	30	 /*  Keyword  'endrecord'           */
#define ENDSTATE	31	 /*  Keyword  'endstate'            */
#define ENDSYSTEM	32	 /*  Keyword  'endsystem'           */
#define ENDWHILE	33	 /*  Keyword  'endwhile'            */
#define ENUM		34	 /*  keyword  'enum'                */
#define ENV		35	 /*  Keyword  'env'                 */
#define EQ		36	 /*  '='                            */
#define FALSE		37	 /*  Literal  'false'               */
#define FIFO		38	 /*  Keyword  'fifo  '              */
#define FLOAT		39	 /*  Keyword  'float'               */
#define FORK		40	 /*  Keyword  'fork'                */
#define FPAR		41	 /*  Keyword  'fpar'                */
#define FROM		42	 /*  Keyword  'from'                */
#define GE		43	 /*  '>='                           */
#define GREATER		44	 /*  '>'                            */
#define IF		45	 /*  Keyword  'if'                  */
#define IN		46	 /*  Keyword  'in'                  */
#define INFORMAL	47	 /*  Keyword  'informal'            */
#define INOUT		48	 /*  Keyword  'inout'               */
#define INPUT		49	 /*  Keyword  'input'               */
#define INTEGER		50	 /*  Keyword  'integer'             */
#define KILL		51	 /*  Keyword  'kill'                */
#define LABEL		52	 /*  Keyword  'label'               */
#define LAZY		53	 /*  Keyword  'lazy'                */
#define LE		54	 /*  '<='                           */
#define LEFT_BRC	55	 /*  '{'                            */
#define LEFT_PAR	56	 /*  '('                            */
#define LEFT_SQ_BRC	57	 /*  '['                            */
#define LOSSY		58	 /*  Keyword  'lossy'               */
#define LOWER		59	 /*  '<'                            */
#define META		60	 /*  Keyword  'meta'                */
#define MINUS		61	 /*  '-'                            */
#define MULTICAST	62	 /*  Keyword  'multicast'           */
#define MULTISET	63	 /*  Keyword  'multiset'            */
#define NAME		64	 /*  Identifier                     */
#define NE		65	 /*  '<>'                           */
#define NEXTSTATE	66	 /*  Keyword  'nextstate'           */
#define NIL		67	 /*  Keyword  'nil'                 */
#define NOT		68	 /*  Keyword  'not'                 */
#define OF		69	 /*  Keyword  'of'                  */
#define OR		70	 /*  Keyword  'or'                  */
#define OUT		71	 /*  Keyword  'out'                 */
#define OUTPUT		72	 /*  Keyword  'output'              */
#define PEER		73	 /*  Keyword  'peer'                */
#define PERCENT		74	 /*  '%'                            */
#define PID		75	 /*  Keyword  'pid'                 */
#define PLUS		76	 /*  '+'                            */
#define PRIVATE		77	 /*  Keyword  'private'             */
#define PUBLIC		78	 /*  Keyword  'public'              */
#define PROCEDURE	79	 /*  Keyword  'procedure'           */
#define PROCESS		80	 /*  Keyword  'process'             */
#define PROVIDED	81	 /*  Keyword  'provided'            */
#define QUESTION_M	82	 /*  '?'                            */
#define RANGE		83	 /*  Keyword  'range'               */
#define RATE		84	 /*  Keyword  'rate'                */
#define RECORD		85	 /*  Keyword  'record'              */
#define RELIABLE	86	 /*  Keyword  'reliable'            */
#define RESET		87	 /*  Keyword  'reset'               */
#define RETURNS		88	 /*  Keyword  'returns'             */
#define RIGHT_BRC	89	 /*  '}'                            */
#define RIGHT_PAR	90	 /*  ')'                            */
#define RIGHT_SQ_BRC	91	 /*  ']'                            */
#define SAVE		92	 /*  Keyword  'save'                */
#define SELF		93	 /*  Keyword  'self'                */
#define SEMICOLON	94	 /*  ';'                            */
#define SET		95	 /*  Keyword  'set'                 */
#define SIGNAL		96	 /*  Keyword  'signal'              */
#define SIGNALROUTE	97	 /*  Keyword  'signalroute'         */
#define SKIP		98	 /*  Keyword  'skip'                */
#define SLASH		99	 /*  '/'                            */
#define STABLE		100	 /*  keyword  'stable'              */
#define START		101	 /*  keyword  'start'               */
#define STATE		102	 /*  Keyword  'state'               */
#define STOP		103	 /*  Keyword  'stop'                */
#define STRING		104	 /*  Keyword  'string'              */
#define STRING_EXPR	105	 /*  String literal                 */
#define SYSTEM		106	 /*  Keyword  'system'              */
#define TASK		107	 /*  Keyword  'task'                */
#define THEN		108	 /*  Keyword  'then'                */
#define TIMER		109	 /*  Keyword  'timer'               */
#define TIMES		110	 /*  '*'                            */
#define TO		111	 /*  Keyword  'to'                  */
#define TPC		112	 /*  Keyword  'tpc'                 */
#define TRUE		113	 /*  Literal  'true'                */
#define TYPE		114	 /*  Keyword  'type'                */
#define UNICAST		115	 /*  keyword  'unicast'             */
#define UNSTABLE	116	 /*  keyword  'unstable'            */
#define URGENT		117	 /*  keyword  'urgent'              */
#define VAR		118	 /*  Keyword  'var'                 */
#define VIA		119	 /*  Keyword  'via'                 */
#define WHEN		120	 /*  Keyword  'when'                */
#define WHILE		121	 /*  Keyword  'while'               */
#define WITH		122	 /*  Keyword  'with'                */

/**  Distinguished symbol.                                         **/

#define D_SYMBOL	123	

/**  Non-terminal symbols that have to be considered (indices are
     reversed-engineered from the grammar generator).              **/

#define NAME_BR         144
#define EXPRESSION      170
#define STATEMENT       183
#define SINGLE_NAME_BR  285

/**  Lexical unit type definition.                                 **/

typedef struct {
  uint4  symbol;
  void  *param;
  uint1 ignored;
  uint4  line_no, char_no;
}  lex_unit;

/**  Prototypes of public functions.                               **/

int  lex_open(char *);
void lex_close(void);
int  lex_next_token(lex_unit *);
void lex_print_token(FILE *, lex_unit *);
void lex_free_token(lex_unit *);

#endif  /* SIFLASH_LEXICAL_H */

/****  End of lexical.h  ****/

