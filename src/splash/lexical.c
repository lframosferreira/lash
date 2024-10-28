/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**    lexical.c  :  Lexical analyzer.                             **/
/**                                                                **/
/**     05/05/99  :  Creation. (BB)                                **/
/**     05/20/99  :  Continued. (BB)                               **/
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
#include <stdlib.h>
#include <string.h>
#include "lash-types.h"
#include "lash-diag.h"
#include "resource.h"
#include "splash.h"
#include "lexical.h"

/****  Maximum length of identifiers.                            ****/

#define  MAX_ID_LENGTH  128

/****  Type definition.                                          ****/

typedef struct {
  char  *name;
  uint1  symbol;
} keyword;

/****  Macros.                                                   ****/

#define  is_printable(c) ((c) > 31 && (c) < 127)
#define  is_alpha(c) (((c) >= 'a' && (c) <= 'z') || \
                      ((c) >= 'A' && (c) <= 'Z') || (c) == '_')
#define  is_digit(c) ((c) >= '0' && (c) <= '9')

/****  Global variable(s).                                       ****/

static FILE  *source_file = NULL;   
                             /*  File being compiled.               */
static char   current_char = 0;     
                             /*  Current character in file.         */
static int    next_char;     /*  Next character (or EOF).           */
static uint4  line_no;       /*  Line number of current character.  */
static sint4  char_no;       /*  Position of current character in its
                                 line (first character is at 1).    */

/**  Keywords                                                      **/

keyword  keywords[] =
{
  { "assert", ASSERT },  { "atomic", ATOMIC },    { "break", BREAK },
  { "do", DO },          { "fi", FI },            { "goto", GOTO },
  { "if", IF },          { "int", TYPE },         { "meta", META },
  { "od", OD },          { "process", PROCESS },  { "skip", SKIP } 
};

#define  NB_KEYWORDS  12

/****  Prototypes of private functions.                          ****/

static int   key_cmp(char *, keyword *);
static void  io_error(void);
static void  lexical_error(void);
static void  overflow_error(void);
static int   skip_blanks(void);
static int   read_const(lex_unit *);
static int   read_word(lex_unit *);

/****  Private functions.                                        ****/

/**  int  key_cmp(w, k)  :  Compares the string w with the keyword
                    *k, and returns an integer consistent with
                    strcmp.                                        **/

static int  key_cmp(w, k)
  char    *w;
  keyword *k;
{
  return (strcmp(w, k -> name));
}

/**  void  io_error()  :  Reports an input/output error that occurred
                    while reading the source file.                 **/

static void io_error()
{
  report_splash_error(
        "Compiler error: I/O error reading source file");
}

/**  void  lexical_error()  :  Reports a lexical error (which occurs
                    when the current character in the source file is
                    unexpected).                                   **/

static void  lexical_error()
{
  char line[256];

  if (is_printable(current_char))
    sprintf(line, 
    "Lexical error at (L%u, C%u): Unexpected character '%c' (0x%2x)",
      line_no, char_no, current_char, current_char);
  else
    sprintf(line, 
      "Lexical error at (L%u, C%u): Unexpected character 0x%2x",
      line_no, char_no, current_char);

  report_splash_error(line);
}

/**  void  overflow_error()  :  Reports an overflow error (which
                    occurs when a numerical constant in the source
                    file is too large).                            **/

static void  overflow_error()
{
  char line[256];

  sprintf(line, 
      "Lexical error at (L%u, C%u): Numerical constant too large",
      line_no, char_no);

   report_splash_error(line);
} 

/**  int  skip_blanks()  :  Skips the next contiguous whitespace
                    characters in the input file. Returns -1 and
                    displays a message in the case of an error,
                    and returns 0 otherwise.                       **/

static int  skip_blanks()
{
  for (;;)
    switch(next_char)
      {
      case '\n':
	line_no++;
	char_no = -1;        /*  Fallthrough!                       */
      case ' ':
      case '\t':
      case '\r':
	next_char = getc(source_file);
	if (ferror(source_file))
	  {
	    io_error();
	    return -1;
	  }
	char_no++;
	continue;
      default:
	return 0;
      }
}

/**  int  read_const(p)  :  Reads an integer constant from the source
                    file and fills the fields 'symbol' and 'param'
                    of the token *p according to the value of this
                    constant. Returns -1 in the case of an error,
                    and 0 otherwise.                               **/

static int  read_const(p)
  lex_unit *p;
{
  register uint8  n;
  register uint4 *par;

  for (n = (uint8) (current_char - '0');; char_no++)
    {
      if (is_digit(next_char))
	{
	  n = n * ((uint8) 10) + (uint8) (next_char - '0');
	  if (n > (uint8) (((uint4) -1) / 2))
	    {
	      overflow_error();
	      return -1;
	    }
	}
      else
	{
	  p -> symbol = CONST;
	  par = (uint4 *) resr__new_object(uint4);
	  if (!par)
	    {
	      report_splash_memory_error();
	      return -1;
	    }
	  *par = (uint4) n;
          p -> param = (void *) par;
	  return 0;
	}
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	}
    }
}

/**  int  read_word(p)  :  Reads an identifier or a keyword from the
                    source file and fills the fields 'symbol' and 
                    'param' of the token *p according to the result.
                    Returns -1 in the case of an error, and 0 
                    otherwise.                                     **/

static int  read_word(lex_unit *p)
{
  register char    *q;
  register uint4    i;
  register keyword *k;
  static   char     word[MAX_ID_LENGTH + 1];

  (q = word)[0] = current_char;
  i = 1;
  q[MAX_ID_LENGTH] = 0;

  for (q++;; i++, q++, char_no++)
    {
      if (is_alpha(next_char) || is_digit(next_char))
	{
	  if (i < MAX_ID_LENGTH)
	    *q = next_char;
	}
      else
	break;
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	} 
    }

  if (i < MAX_ID_LENGTH)
    *q = 0;


  if ((k = (keyword *) bsearch((const void *) word,
      (const void *) keywords, NB_KEYWORDS, sizeof(keyword),
      (int (*)(const void *, const void *)) key_cmp)))
    p -> symbol = k -> symbol;
  else
    {
      p -> symbol = NAME;
      q = (char *) resr__new_objects(char, i + 1);
      if (!q)
	{
	  report_splash_memory_error();
	  return -1;
	}
      strcpy(q, word);
      p -> param = (void *) q;
    }

  return 0;
}

/****  Public functions.                                         ****/

/**  int  lex_open(name)  :  Opens the source file for reading. 
                    Returns -1 and displays a message in the case of
                    an error. Returns 0 otherwise.                 **/

int  lex_open(name)
  char *name;
{
  if (!(source_file = fopen(name, "r")))
    {
      report_splash_error(
        "Compiler error: Unable to open source file");
      return -1;
    }

  next_char = getc(source_file);
  if (ferror(source_file))
    {
      io_error();
      fclose(source_file);
      return -1;
    }

  line_no = 1;
  char_no = 0;

  return 0;
}

/**  void  lex_close()  :  Closes the source file. This function
                    does not report errors.                        **/

void  lex_close()
{
  if (source_file)
    fclose(source_file);
}

/**  int  lex_next_token(p)  :  Reads the next lexical unit from the
                    source file, and copies that unit to the location
                    pointed by p. Returns -1 and displays a message
                    in the case of an error, and returns 0 otherwise.
		                                                   **/
int  lex_next_token(p)
  lex_unit *p;
{
  if (skip_blanks() < 0)
    return -1;

  if (next_char == EOF)
    {
      p -> symbol  = DOLLAR;
      p -> line_no = line_no;
      p -> char_no = char_no + 1;
      return 0;
    }

  current_char = (char) next_char;
  next_char    = getc(source_file);

  if (ferror(source_file))
    {
      io_error();
      return -1;
    }

  p -> line_no = line_no;
  p -> char_no = ++char_no;

  switch(current_char)
    {
    case '>':
      if (next_char != '=')
	{
	  p -> symbol = GREATER;
	  return 0;
	}
      p -> symbol = GE;
      break;
    case '<':
      if (next_char != '=')
	{
	  p -> symbol = LOWER;
	  return 0;
	}
      p -> symbol = LE;
      break;
    case ':':
      if (next_char != ':')
	{
	  p -> symbol = COLON;
	  return 0;
	}
      p -> symbol = SEP;
      break;    
    case '=':
      if (next_char != '=')
	{
	  p -> symbol = ASGN;
	  return 0;
	}
      p -> symbol = EQ;
      break;       
    case '-':
      if (next_char != '>')
	{
	  p -> symbol = MINUS;
	  return 0;
	}
      p -> symbol = SEMICOLON;
      break; 
      /*      We do not handle '!=' conditions yet.
    case '!':
      if (next_char != '=')
	{
	  lexical_error();
	  return -1;
	}
      p -> symbol = NE;
      break;
      */
    case ',':
      p -> symbol = COMMA;
      return 0;
    case '.':
      p -> symbol = DOT;
      return 0;
    case '{':
      p -> symbol = LEFT_BRC;
      return 0;  
    case '(':
      p -> symbol = LEFT_PAR;
      return 0;    
    case '+':
      p -> symbol = PLUS;
      return 0;
    case '}':
      p -> symbol = RIGHT_BRC;
      return 0;  
    case ')':
      p -> symbol = RIGHT_PAR;
      return 0;
    case ';':
      p -> symbol = SEMICOLON;
      return 0;
    case '*':
      p -> symbol = TIMES;
      return 0;
    default:
      if (is_digit(current_char))
	return read_const(p);

      if (is_alpha(current_char))
	return read_word(p);

      lexical_error();
      return -1;
    }

  next_char = getc(source_file);
  if (ferror(source_file))
    {
      io_error();
      return -1;
    }

  ++char_no;
  return 0;  
}

/**  void  lex_free_token(p)  :  Frees the space possibly allocated
                    for the parameter of the lexical unit *p. This
                    function cannot incur an error.                **/

void  lex_free_token(p)
  lex_unit *p;
{
  switch(p -> symbol)
    {
    case CONST:
    case VAR_REF:
    case ASGN_STMNT:
      resr__free_object((uint4 *) (p -> param), uint4);
      return;

    case NAME:
    case SINGLE_NAME:
      resr__free_objects((char *) (p -> param), char, 
          strlen((char *) (p -> param)) + 1);
      return;

    case CONST_NT:
      resr__free_object((uint4 *) (p -> param), sint4);
      return;

    default:
      return;
    }
}

/**  void  lex_print_token(s, p)  :  Displays a short description of 
                    the lexical unit *p on the stream *s. This 
                    function cannot incur an error.                **/

void  lex_print_token(s, p)
  FILE     *s;
  lex_unit *p;
{
  switch(p -> symbol)
    {
    case ASGN:      fprintf(s, "operator '='");           return;
    case ASSERT:    fprintf(s, "keyword 'assert'");       return;
    case ATOMIC:    fprintf(s, "keyword 'atomic'");       return;
    case BREAK:     fprintf(s, "keyword 'break'");        return;
    case COLON:     fprintf(s, "symbol ':'");             return;
    case COMMA:     fprintf(s, "symbol ','");             return;
    case CONST:     fprintf(s, "constant (%u)", 
                        *((uint4 *) (p -> param)));   return;
    case DO:        fprintf(s, "keyword 'do'");           return;
    case DOLLAR:    fprintf(s, "end of file");            return;
    case DOT:       fprintf(s, "symbol '.'");             return;
    case EQ:        fprintf(s, "operator '=='");          return;
    case FI:        fprintf(s, "keyword 'fi'");           return;
    case GE:        fprintf(s, "operator '>='");          return;    
    case GOTO:      fprintf(s, "keyword 'goto'");         return;
    case GREATER:   fprintf(s, "operator '>'");           return;     
    case IF:        fprintf(s, "keyword 'if'");           return;
    case LE:        fprintf(s, "operator '<='");          return;
    case LEFT_BRC:  fprintf(s, "symbol '{'");             return;
    case LEFT_PAR:  fprintf(s, "symbol '('");             return;
    case LOWER:     fprintf(s, "operator '<'");           return; 
    case META:      fprintf(s, "keyword 'meta'");         return;
    case MINUS:     fprintf(s, "operator '-'");           return;
    case NAME:      fprintf(s, "identifier (%s)",
                        (char *) (p -> param));           return;
    case NE:        fprintf(s, "operator '!='");          return;    
    case OD:        fprintf(s, "keyword 'od'");           return;
    case PLUS:      fprintf(s, "operator '+'");           return;
    case PROCESS:   fprintf(s, "keyword 'process'");      return;
    case RIGHT_BRC: fprintf(s, "symbol '}'");             return;
    case RIGHT_PAR: fprintf(s, "symbol ')'");             return;
    case SEMICOLON: fprintf(s, "symbol ';' or '->'");     return;
    case SEP:       fprintf(s, "symbol '::'");            return;
    case SKIP:      fprintf(s, "keyword 'skip'");         return;
    case TIMES:     fprintf(s, "operator '*'");           return;
    case TYPE:      fprintf(s, "keyword 'int'");          return;
    default:        fprintf(s, "unknown item");         
    }
}

/****  End of lexical.c  ****/
