/********************************************************************/
/**                                                                **/
/**  LASH Presburger compiler -- v0.9                              **/
/**  ========================                                      **/
/**                                                                **/
/**   lexical.c  :  Lexical analyser routines.                     **/
/**                                                                **/
/**     12/14/00  :  Creation. (LL)                                **/
/**     03/27/02  :  '.' added in symbol name [read_word]. (LL)    **/
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lash-types.h"
#include "lash-diag.h"
#include "resource.h"
#include "presburger.h"
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
                      ((c) >= 'A' && (c) <= 'Z') || \
		      (c) == '_')
#define  is_digit(c) ((c) >= '0' && (c) <= '9')

/****  Global variable(s).                                       ****/

static FILE  *source_file = NULL;   /*  File being compiled.        */
static char   current_char = 0;     /*  Current character in file.  */
static int    next_char;            /*  Next character (or EOF).    */
static uint4  line_no;              /*  Line number of current 
                                        character.                  */
static sint4  char_no;              /*  Position of current 
                                        character in its line (first
                                        character is at 1).         */

/**  Keywords                                                      **/

keyword  keywords[] =
{
  {"AND", AND}, {"EXISTS", EXISTS}, {"FORALL", FORALL} ,
  {"MOD", MOD}, {"NOT", NOT}, {"OR", OR}, {"and", AND}, 
  {"exists", EXISTS}, {"forall", FORALL}, {"mod", MOD}, 
  { "not", NOT },   { "or", OR}       
};

#define  NB_KEYWORDS  12

/****  Prototypes of private functions.                          ****/

static int   key_cmp(char *, keyword *);
static void  io_error(void);
static void  lexical_error(void);
static void  overflow_error(void);
static int   skip_blanks(void);
static int   skip_comment(void);
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
  report_presb_error("Compiler error: I/O error reading source file");
}

/**  void  lexical_error()  :  Reports a lexical error (which occurs
                    when the current character in the source file is
                    unexpected).                                   **/

static void  lexical_error()
{
  char line[256];

  if (is_printable(current_char))
    sprintf(line, 
"Lexical error at (L%u, C%u): Unexpected character '%c' (0x%2u)",
	    line_no, char_no, current_char, current_char);
  else
    sprintf(line, 
"Lexical error at (L%u, C%u): Unexpected character 0x%2u",
	    line_no, char_no, current_char);

  report_presb_error(line);
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

  report_presb_error(line);
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

/**  int  skip_comment()  :  Skips all the following characters 
                    in the input file until the characters
                    corresponding to the end of the comment are
                    reached. Returns -1 and displays a message in the
                    case of an error, and returns 0 otherwise.     **/

static int  skip_comment()
{
  int waiting = 0;
  for (;;)
    {
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	}
      char_no++;

      switch(next_char)
	{
	case '/':
	  if (waiting)
	    return 0;
	  break;
	case '*':
	  waiting = 1;
	  break;
	case '\n':
	  line_no++;
	  char_no = -1;        /*  Fallthrough!                     */
	  break;
	default:
	  waiting = 0; 
	}
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
	      report_presb_memory_error();
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
      if (is_alpha(next_char) || is_digit(next_char) || 
	  (next_char == '.'))
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
			       (const void *) keywords, NB_KEYWORDS, 
			       sizeof(keyword),
			       (int (*)(const void *, const void *)) 
			       key_cmp)))
    p -> symbol = k -> symbol;
  else
    {
      p -> symbol = NAME;
      q = (char *) resr__new_objects(char, i + 1);
      if (!q)
	{
	  report_presb_memory_error();
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
      report_presb_error(
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
  int comment ;  /*  comment is set to 1 if the characters read  
		     belong to a comment. In this case, the loop 
                     continues.                                    */ 
  for(;;)
    {
      comment = 0;
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
	  p -> symbol = COLON;
	  return 0;
	case '=':
	  p -> symbol = EQ;
	  return 0;
	case '-':
	  p -> symbol = MINUS;
	  return 0;
	case '!':
	  if (next_char != '=')
	    {
	      p -> symbol = EXCLAM_M;
	      return 0;
	    }
	  p -> symbol = NE;
	  break;

	case ',':
	  p -> symbol = COMMA;
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
	case '*':
	  p -> symbol = TIMES;
	  return 0;
      
	case '/' :
	  if (next_char != '*')
	    {
	      lexical_error();
	      return -1;	  
	    }

	  else 
	    {
	      skip_comment();
	      comment = 1;
	      break;
	    }
       
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
      if (comment == 0)
	return 0;  
    }
}

/**  void  lex_free_token(p)  :  Frees the space possibly allocated
                    for the parameter of the lexical unit *p. This
                    function cannot incur an error.                **/

void  lex_free_token(p)
     lex_unit *p;
{
   switch(p -> symbol)
    {

    case CONST_C:
      resr__free_object((uint4 *) (p -> param), uint4);
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
    case 0:   fprintf(s,  "keyword:  DOLLAR") ;      return; 
    case 1:   fprintf(s,  "keyword:  and");          return; 
    case 2:   fprintf(s,  "keyword:  ':'");          return; 
    case 3:   fprintf(s,  "keyword:  ','");          return; 
    case 4:   fprintf(s,  "keyword:  '='");          return; 
    case 5:   fprintf(s,  "keyword:  '!'");          return; 
    case 6:   fprintf(s,  "keyword:  '>='");         return; 
    case 7:   fprintf(s,  "keyword:  '>'");          return; 
    case 8:   fprintf(s,  "keyword:  '<='");         return; 
    case 9:   fprintf(s,  "keyword:  '{'");          return; 
    case 10:  fprintf(s,  "keyword:  '('");          return; 
    case 11:  fprintf(s,  "keyword:  '<'");          return; 
    case 12:  fprintf(s,  "keyword:  '-'");          return; 
    case 13:  fprintf(s,  "identifier  (%s)", 
			 (char *) (p -> param));     return;
    case 14:  fprintf(s,  "keyword:  !=");           return; 
    case 15:  fprintf(s,  "keyword:  not");          return; 
    case 16:  fprintf(s,  "keyword:  or");           return; 
    case 17:  fprintf(s,  "keyword:  '+'");          return; 
    case 18:  fprintf(s,  "keyword:  '}'");          return; 
    case 19:  fprintf(s,  "keyword:  ')'");          return; 
    case 20:  fprintf(s,  "keyword:  '*'");          return; 
    case 21:  fprintf(s,  "keyword:  'exists'");     return; 
    case 22:  fprintf(s,  "keyword:  'forall'");     return; 
    case 23:  fprintf(s,  "constante  (%s)", 
			 (char *) (p -> param));     return;
    case 24:  fprintf(s,  "keyword:  'mod'");     return; 
    case 25:  fprintf(s,  "class:  <presburger>");   return; 
    case 26:  fprintf(s,  "class:  <condition>");    return; 
    case 27:  fprintf(s,  "class:  <cond1>");        return; 
    case 28:  fprintf(s,  "class:  <quantif>");      return; 
    case 29:  fprintf(s,  "class:  <quantif_1>");    return; 
    case 30:  fprintf(s,  "class:  <single_(>");     return; 
    case 31:  fprintf(s,  "class:  <vars>");         return; 
    case 32:  fprintf(s,  "class:  <quantif_2>");    return; 
    case 33:  fprintf(s,  "class:  <cond2>");        return; 
    case 34:  fprintf(s,  "class:  <atom_cond>");    return; 
    case 35:  fprintf(s,  "class:  <cond_(>");       return; 
    case 36:  fprintf(s,  "class:  <cond_(_)>");     return; 
    case 37:  fprintf(s,  "class:  <single_minus>"); return; 
    case 38:  fprintf(s,  "class:  <single_name>");  return; 
    case 39:  fprintf(s,  "class:  <single_{>");     return; 
    case 40:  fprintf(s,  "class:  <const>");        return; 
    case 41:  fprintf(s,  "class:  <term>");         return;  
    case 42:  fprintf(s,  "class:  <single_expr>");  return; 
    case 43:  fprintf(s,  "class:  <expr>");         return; 
    case 44:  fprintf(s,  "class:  <expr_1>");       return; 
    case 45:  fprintf(s,  "class:  <atom_cond1_eq>");   return; 
    case 46:  fprintf(s,  "class:  <atom_cond1>");   return; 
    case 47:  fprintf(s,  "class:  <atom_cond_mod>"); return; 
    default:  fprintf(s, "unknown item");         
    }
}

/****  End of lexical.c  ****/
