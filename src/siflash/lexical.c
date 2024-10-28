/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (Siflash) compiler -- v0.9                **/
/**  ======================================                        **/
/**                                                                **/
/**    lexical.c  :  Lexical analyzer.                             **/
/**                                                                **/
/**     07/10/02  :  Creation. (LL)                                **/
/**     07/26/02  :  Modifications for IF2.0. (LL)                 **/
/**     09/05/02  :  Reorganization. (BB)                          **/
/**     10/14/02  :  Minor correction (LL)                         **/
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
#include "siflash.h"
#include "lexical.h"
#include "program.h"

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
#define C_COMMENT 1
#define CPLUSPLUS_COMMENT 2

/****  Global variables.                                         ****/

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
       { "abstract", ABSTRACT },  
       { "active", ACTIVE },
       { "and", AND }, 
       { "array", ARRAY }, 
       { "assert", ASSERT }, 
       { "boolean", BOOLEAN }, 
       { "by", BY },
       { "call", CALL }, 
       { "clock", CLOCK },
       { "const", CONST },  
       { "deadline", DEADLINE }, 
       { "delayable" , DELAYABLE }, 
       { "do", DO },  
       { "eager" , EAGER }, 
       { "else", ELSE }, 
       { "endabstract", ENDABSTRACT }, 
       { "endassert", ENDASSERT }, 
       { "endenum", ENDENUM },
       { "endif", ENDIF }, 
       { "endprocedure", ENDPROCEDURE }, 
       { "endprocess", ENDPROCESS }, 
       { "endrecord", ENDRECORD },  
       { "endstate", ENDSTATE }, 
       { "endsystem", ENDSYSTEM },
       { "endwhile", ENDWHILE },
       { "enum", ENUM },
       { "env", ENV }, 
       { "false", FALSE }, 
       { "float", FLOAT },
       { "fork", FORK }, 
       { "fpar", FPAR }, 
       { "from", FROM }, 
       { "if", IF }, 
       { "in", IN },
       { "informal", INFORMAL },
       { "inout", INOUT },
       { "input", INPUT },
       { "integer", INTEGER },
       { "kill", KILL },
       { "label", LABEL },
       { "lazy" , LAZY }, 
       { "meta", META },
       { "nextstate", NEXTSTATE }, 
       { "nil", NIL },
       { "not", NOT },
       { "of", OF }, 
       { "or", OR },
       { "out", OUT },
       { "output", OUTPUT }, 
       { "pid", PID },
       { "private", PRIVATE },
       { "procedure", PROCEDURE },
       { "process", PROCESS },
       { "provided", PROVIDED },
       { "public", PUBLIC },
       { "range", RANGE }, 
       { "record", RECORD },
       { "reset", RESET },  
       { "returns", RETURNS },
       { "save", SAVE },
       { "self", SELF },
       { "set", SET }, 
       { "signal", SIGNAL },
       { "signalroute", SIGNALROUTE },
       { "skip", SKIP },  
       { "state",  STATE },
       { "stop", STOP },
       { "string", STRING },
       { "system", SYSTEM },
       { "task", TASK },
       { "then", THEN },
       { "timer", TIMER }, 
       { "to", TO },
       { "tpc", TPC }, 
       { "true", TRUE },
       { "type", TYPE },         
       { "var",  VAR },
       { "via", VIA },
       { "when", WHEN },
       { "while", WHILE },
       { "with", WITH } };

#define  NB_KEYWORDS  80

keyword options[] = {
  { "fifo", FIFO }, 
  { "delay", DELAY },
  { "lossy", LOSSY }, 
  { "multicast", MULTICAST }, 
  { "multiset", MULTISET },
  { "peer", PEER },   
  { "rate", RATE },  
  { "reliable", RELIABLE },
  { "stable", STABLE }, 
  { "start", START },
  { "unicast", UNICAST }, 
  { "unstable", UNSTABLE },
  { "urgent", URGENT } };

#define  NB_OPTIONS  13

/****  Prototypes of private functions.                          ****/

static int   key_cmp(char *, keyword *);
static void  io_error(void);
static void  lexical_error(void);
static void  overflow_error(void);
static int   skip_blanks(void);
static int   skip_comment(char);
static int   read_const(lex_unit *);
static int   read_int(uint4 *);
static int   read_word(lex_unit *);
static int   read_string(lex_unit *);
static int   read_external_code(lex_unit *);
static int   read_option(lex_unit *);

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
  report_siflash_error(
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

  report_siflash_error(line);
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

  report_siflash_error(line);
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

/**  int  skip_comment(type)  :  Skips all the following characters 
                    in the input file until the characters
                    corresponding to the end of the comment are
                    reached (if type == C_COMMENT) or until it reaches
                    the end of the current line (if type ==
                    CPLUSPLUS_COMMENT). Returns -1 and displays a
                    message in the case of an error, and returns 0
                    otherwise.                                     **/

static int  skip_comment(type)
     char type;
{
  register int waiting = 0;

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
	  if (type == C_COMMENT)
	    waiting = 1;
	  break;
	case '\n':
	  line_no++;
	  char_no = -1;
	  if (type == CPLUSPLUS_COMMENT)
	    return 0;
	  break;
	case EOF:
	  lexical_error();
	  return -1;
	default:
	  waiting = 0; 
	}
    }
}

/**  int  read_const(p)  :  Reads a constant number from the source
                    file and fills the fields 'symbol' and 'param'
                    of the token *p according to the value of this
                    constant. Returns -1 in the case of an error,
                    and 0 otherwise.                               **/

static int read_const(p)
     lex_unit *p;
{
  uint4 uval;
  sint4 val;
  sint4 *par;

  if (read_int(&uval) < 0)
    return -1;

  if (next_char == '.') 
    {
      /*  Constant is a float, it gets rounded. */ 
      
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	}
      
      if (!(is_digit(next_char)))
	{
	  lexical_error();
	  return -1;
	}
      
      if (next_char > '4') 
	{
	  if (uval > uval + 1)
	    {
	      overflow_error();
	      return -1;
	    }
	  uval += 1;
	}
      
      while (is_digit(next_char)) 
	{
	  next_char = getc(source_file);
	  if (ferror(source_file))
	    {
	      io_error();
	      return -1;
	    }
	}
      printf(
"Lexical warning: Floating number rounded at (L%u, C%u)", 
             line_no, char_no ); 
    }
  
  val = (sint4) uval;
  
  if ((uint4) val != uval) 
    {
      overflow_error();
      return -1;
    }
  p -> symbol = COEFF;
  par = (sint4 *) resr__new_object(sint4);
  if (!par)
    {
      report_siflash_memory_error();
      return -1;
    }
  *par = val;
  p -> param = (void *) par;
  return 0;
}

/**  int  read_int(v)  :  Reads an integer number from the source
                    file and stores the value in the pointer *v.
		    Returns -1 in the case of an error,
                    and 0 otherwise.                               **/

static int  read_int(v)
     uint4 *v;
{
  register uint8  n;
  
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
	  *v = (uint4) n;
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
		    (const void *) keywords, NB_KEYWORDS, 
			       sizeof(keyword),
		    (int (*)(const void *, const void *)) key_cmp)))
    p -> symbol = k -> symbol;
  else
    {
      p -> symbol = NAME;
      q = (char *) resr__new_objects(char, i + 1);
      if (!q)
	{
	  report_siflash_memory_error();
	  return -1;
	}
      strcpy(q, word);
      p -> param = (void *) q;
    }
  
  return 0;
}

/**  int  read_string(p)  :  Reads a string literal from the
                    source file and fills the fields 'symbol' 
		    the token *p according to the result.
		    A string is a sequence of words starting with '"'
		    and terminating with '"' also.
                    Returns -1 in the case of an error, and 0 
                    otherwise.                                     **/

static int  read_string(lex_unit *p)
{
  for (;;)
    {
      current_char = next_char;
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	}
      char_no++;
      
      switch (current_char)
	{
	case '"':
	  p -> symbol = STRING_EXPR;
	  return 0;

	case '\n':
	  line_no++;
	  char_no = -1;
	  break;

	case EOF:
	  lexical_error();
	  return -1;
	default:
	  break;
	}
    }
}

/**  int  read_external_code(p)  :  Reads external code from the
                    source file and fills the field 'symbol' 
		    of the token *p according to the result.
		    External code starts with the sequence "{#" and
		    finishes with the sequence "#}".
                    Returns -1 in the case of an error, and 0 
                    otherwise.                                     **/

static int  read_external_code(lex_unit *p)
{
  int waiting;
 
  next_char = getc(source_file);
  if (ferror(source_file))
    {
      io_error();
      return -1;
    }
  char_no++;
  
  waiting = 0;
  for (;;)
    {
      current_char = next_char;
      next_char = getc(source_file);
      if (ferror(source_file))
	{
	  io_error();
	  return -1;
	}
      char_no++;

      switch(current_char)
	{
	case '}':
	  if (waiting) 
	    {
	      p -> symbol = CODE_EXT;	    
	      return 0;
	    }
	  break;
	case '#':
	  waiting = 1;
	  break;
	case '\n':
	  line_no++;
	  waiting = 0;
	  char_no = -1;
	  break;
	default:
	  waiting = 0; 
	}
    }
  return 0;
}

/**  int  read_option(p)  :  Reads an keyword associated to an option
                    from the
                    source file and fills the fields 'symbol' and 
                    'param' of the token *p according to the result.
                    Returns -1 in the case of an error, and 0 
                    otherwise.                                     **/

static int  read_option(lex_unit *p)
{
  register char    *q;
  register uint4    i;
  register keyword *k;
  static   char     word[MAX_ID_LENGTH + 1];

  q = word;
  i = 0;
  q[MAX_ID_LENGTH] = 0;

  for (;; i++, q++, char_no++)
    {
      if (is_alpha(next_char))
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

  if (i == 0) 
    {
      lexical_error();
      return -1; 
    }
  
  if (i < MAX_ID_LENGTH)
    *q = 0;
  
  if ((k = (keyword *) bsearch((const void *) word,
      (const void *) options, NB_OPTIONS, sizeof(keyword),
      (int (*)(const void *, const void *)) key_cmp)))
    p -> symbol = k -> symbol;
  else
    {
      char line[256];

      sprintf(line, 
	    "Lexical error at (L%u, C%u): Unexpected word \"#%s\"",
		line_no, char_no, word);
      report_siflash_error(line);
      return -1;
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
      report_siflash_error(
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
  int comment ; /* comment is set to 1 if the characters read  
		   are a comment. In this case, the loop continues. */ 
  p -> ignored = 0;
  p -> param = NULL;
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
	case ':':
	  if (next_char != '=')
	    {
	      p -> symbol = COLON;
	      return 0;
	    }
	  p -> symbol = ASGN;
	  break;
	case ',':
	  p -> symbol = COMMA;
	  return 0;
	case '.':
	  if (next_char != '.')
	    {
	      p -> symbol = DOT;
	      return 0;
	    }
	  else
	    p -> symbol = DOTDOT;
	  break;
	case '=':
	  p -> symbol = EQ;
	  return 0;
	case '>':
	  if (next_char != '=')
	    {
	      p -> symbol = GREATER;
	      return 0;
	    }
	  p -> symbol = GE;
	  break;
	case '<':
	  if ((next_char != '=') && (next_char != '>'))
	    {
	      p -> symbol = LOWER;
	      return 0;
	    }
	  else
	    { 
	      if (next_char == '=')
		p -> symbol = LE;
	      else
		p -> symbol = NE;
	    }
	  break;
	case '{':
	  if (next_char == '#')
	    return read_external_code(p);

	  p -> symbol = LEFT_BRC;
	  return 0;  
	case '(':
	  p -> symbol = LEFT_PAR;
	  return 0;    
	case '[':
	  p -> symbol = LEFT_SQ_BRC;
	  return 0;    
	case '-':
	  p -> symbol = MINUS;
	  return 0;
 	case '#':
	  return read_option(p);
	case '%':
	  p -> symbol = PERCENT;
	  return 0;
	case '+':
	  p -> symbol = PLUS;
	  return 0;
	case '?':
	  p -> symbol = QUESTION_M;
	  return 0;
	case '}':
	  p -> symbol = RIGHT_BRC;
	  return 0;  
	case ')':
	  p -> symbol = RIGHT_PAR;
	  return 0;
	case ']':
	  p -> symbol = RIGHT_SQ_BRC;
	  return 0;
	case ';':
	  p -> symbol = SEMICOLON;
	  return 0;
      	case '/' :
	  if ((next_char != '*') && (next_char != '/')) 
	    {
	      p -> symbol = SLASH;
	      return 0;	  
	    }
	  else
	    {
	      if (next_char == '*')
		{
		  if (skip_comment(C_COMMENT) < 0)
		    return -1;
		  comment = 1;
		  break;
		}
	      else
		{
		  if (skip_comment(CPLUSPLUS_COMMENT) < 0)
		    return -1;
		  comment = 1;
		  break;
		}
	    }
	  
	case '*':
	  p -> symbol = TIMES;
	  return 0;
	case '"':
	  return read_string(p);
       
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
  pgm_expression *e;
  if (p -> ignored > 0)
    return;

  switch(p -> symbol)
    {
    case NAME:
    case NAME_BR:
    case SINGLE_NAME_BR:
     resr__free_objects((char *) (p -> param), char, 
			 strlen((char *) (p -> param)) + 1);
     return;

    case COEFF:
      resr__free_object((sint4 *) (p -> param), sint4);
      return;

    case STATEMENT:
      pgm_free_gen_operation((pgm_gen_operation *) p -> param);
      return;

   case EXPRESSION:
      e = (pgm_expression *) p -> param;
      pgm_free_expression(e);
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
  fprintf(s, "Line %d Col %d: ", p -> line_no, p -> char_no);
  switch(p -> symbol)
    {
    case 0 	: fprintf(s, "token: DOLLAR"); return; 
    case 1 	: fprintf(s, "token: ABSTRACT"); return; 
    case 2 	: fprintf(s, "token: ACTIVE"); return; 
    case 3 	: fprintf(s, "token: AND"); return; 
    case 4 	: fprintf(s, "token: ARRAY"); return; 
    case 5 	: fprintf(s, "token: ':='"); return; 
    case 6 	: fprintf(s, "token: ASSERT"); return; 
    case 7 	: fprintf(s, "token: BOOLEAN"); return; 
    case 8 	: fprintf(s, "token: BY"); return; 
    case 9 	: fprintf(s, "token: CALL"); return; 
    case 10 	: fprintf(s, "token: CLOCK"); return; 
    case 11 	: fprintf(s, "token: CODE_EXT"); return; 
    case 12 	: fprintf(s, "token: COEFF"); return; 
    case 13 	: fprintf(s, "token: ':'"); return; 
    case 14 	: fprintf(s, "token: ','"); return; 
    case 15 	: fprintf(s, "token: CONST"); return; 
    case 16 	: fprintf(s, "token: DEADLINE"); return; 
    case 17 	: fprintf(s, "token: DELAY"); return; 
    case 18 	: fprintf(s, "token: DELAYABLE"); return; 
    case 19 	: fprintf(s, "token: DO"); return; 
    case 20 	: fprintf(s, "token: '.'"); return; 
    case 21 	: fprintf(s, "token: '..'"); return; 
    case 22 	: fprintf(s, "token: EAGER"); return; 
    case 23 	: fprintf(s, "token: ELSE"); return; 
    case 24 	: fprintf(s, "token: ENDABSTRACT"); return; 
    case 25 	: fprintf(s, "token: ENDASSERT"); return; 
    case 26 	: fprintf(s, "token: ENDENUM"); return; 
    case 27 	: fprintf(s, "token: ENDIF"); return; 
    case 28 	: fprintf(s, "token: ENDPROCEDURE"); return; 
    case 29 	: fprintf(s, "token: ENDPROCESS"); return; 
    case 30 	: fprintf(s, "token: ENDRECORD"); return; 
    case 31 	: fprintf(s, "token: ENDSTATE"); return; 
    case 32 	: fprintf(s, "token: ENDSYSTEM"); return; 
    case 33 	: fprintf(s, "token: ENDWHILE"); return; 
    case 34 	: fprintf(s, "token: ENUM"); return; 
    case 35 	: fprintf(s, "token: ENV"); return; 
    case 36 	: fprintf(s, "token: '='"); return; 
    case 37 	: fprintf(s, "token: FALSE"); return; 
    case 38 	: fprintf(s, "token: FIFO"); return; 
    case 39 	: fprintf(s, "token: FLOAT"); return; 
    case 40 	: fprintf(s, "token: FORK"); return; 
    case 41 	: fprintf(s, "token: FPAR"); return; 
    case 42 	: fprintf(s, "token: FROM"); return; 
    case 43 	: fprintf(s, "token: '>='"); return; 
    case 44 	: fprintf(s, "token: '>'"); return; 
    case 45 	: fprintf(s, "token: IF"); return; 
    case 46 	: fprintf(s, "token: IN"); return; 
    case 47 	: fprintf(s, "token: INFORMAL"); return; 
    case 48 	: fprintf(s, "token: INOUT"); return; 
    case 49 	: fprintf(s, "token: INPUT"); return; 
    case 50 	: fprintf(s, "token: INTEGER"); return; 
    case 51 	: fprintf(s, "token: KILL"); return; 
    case 52 	: fprintf(s, "token: LABEL"); return; 
    case 53 	: fprintf(s, "token: LAZY"); return; 
    case 54 	: fprintf(s, "token: '<='"); return; 
    case 55 	: fprintf(s, "token: '{'"); return; 
    case 56 	: fprintf(s, "token: '('"); return; 
    case 57 	: fprintf(s, "token: '['"); return; 
    case 58 	: fprintf(s, "token: LOSSY"); return; 
    case 59 	: fprintf(s, "token: '<'"); return; 
    case 60 	: fprintf(s, "token: META"); return; 
    case 61 	: fprintf(s, "token: '-'"); return; 
    case 62 	: fprintf(s, "token: MULTICAST"); return; 
    case 63 	: fprintf(s, "token: MULTISET"); return; 
    case 64 	: fprintf(s, "token: NAME"); return; 
    case 65 	: fprintf(s, "token: '<>'"); return; 
    case 66 	: fprintf(s, "token: NEXTSTATE"); return; 
    case 67 	: fprintf(s, "token: NIL"); return; 
    case 68 	: fprintf(s, "token: NOT"); return; 
    case 69 	: fprintf(s, "token: OF"); return; 
    case 70 	: fprintf(s, "token: OR"); return; 
    case 71 	: fprintf(s, "token: OUT"); return; 
    case 72 	: fprintf(s, "token: OUTPUT"); return; 
    case 73 	: fprintf(s, "token: PEER"); return; 
    case 74 	: fprintf(s, "token: '%%'"); return; 
    case 75 	: fprintf(s, "token: PID"); return; 
    case 76 	: fprintf(s, "token: '+'"); return; 
    case 77 	: fprintf(s, "token: PRIVATE"); return; 
    case 78 	: fprintf(s, "token: PUBLIC"); return; 
    case 79 	: fprintf(s, "token: PROCEDURE"); return; 
    case 80 	: fprintf(s, "token: PROCESS"); return; 
    case 81 	: fprintf(s, "token: PROVIDED"); return; 
    case 82 	: fprintf(s, "token: '?'"); return; 
    case 83 	: fprintf(s, "token: RANGE"); return; 
    case 84 	: fprintf(s, "token: RATE"); return; 
    case 85 	: fprintf(s, "token: RECORD"); return; 
    case 86 	: fprintf(s, "token: RELIABLE"); return; 
    case 87 	: fprintf(s, "token: RESET"); return; 
    case 88 	: fprintf(s, "token: RETURNS"); return; 
    case 89 	: fprintf(s, "token: '}'"); return; 
    case 90 	: fprintf(s, "token: ')'"); return; 
    case 91 	: fprintf(s, "token: ']'"); return; 
    case 92 	: fprintf(s, "token: SAVE"); return; 
    case 93 	: fprintf(s, "token: SELF"); return; 
    case 94 	: fprintf(s, "token: ';'"); return; 
    case 95 	: fprintf(s, "token: SET"); return; 
    case 96 	: fprintf(s, "token: SIGNAL"); return; 
    case 97 	: fprintf(s, "token: SIGNALROUTE"); return; 
    case 98 	: fprintf(s, "token: SKIP"); return; 
    case 99 	: fprintf(s, "token: '/'"); return; 
    case 100 	: fprintf(s, "token: STABLE"); return; 
    case 101 	: fprintf(s, "token: START"); return; 
    case 102 	: fprintf(s, "token: STATE"); return; 
    case 103 	: fprintf(s, "token: STOP"); return; 
    case 104 	: fprintf(s, "token: STRING"); return; 
    case 105 	: fprintf(s, "token: STRING_EXPR"); return; 
    case 106 	: fprintf(s, "token: SYSTEM"); return; 
    case 107 	: fprintf(s, "token: TASK"); return; 
    case 108 	: fprintf(s, "token: THEN"); return; 
    case 109 	: fprintf(s, "token: TIMER"); return; 
    case 110 	: fprintf(s, "token: '*'"); return; 
    case 111 	: fprintf(s, "token: TO"); return; 
    case 112 	: fprintf(s, "token: TPC"); return; 
    case 113 	: fprintf(s, "token: TRUE"); return; 
    case 114 	: fprintf(s, "token: TYPE"); return; 
    case 115 	: fprintf(s, "token: UNICAST"); return; 
    case 116 	: fprintf(s, "token: UNSTABLE"); return; 
    case 117 	: fprintf(s, "token: URGENT"); return; 
    case 118 	: fprintf(s, "token: VAR"); return; 
    case 119 	: fprintf(s, "token: VIA"); return; 
    case 120 	: fprintf(s, "token: WHEN"); return; 
    case 121 	: fprintf(s, "token: WHILE"); return; 
    case 122 	: fprintf(s, "token: WITH"); return; 
    case 123 	: fprintf(s, "token: <system>"); return; 
    case 124 	: fprintf(s, "token: <system_2>"); return; 
    case 125 	: fprintf(s, "token: <system_1a>"); return; 
    case 126 	: fprintf(s, "token: <system_1>"); return; 
    case 127 	: fprintf(s, "token: <process_decl>"); return; 
    case 128 	: fprintf(s, "token: <signalroute_decl>"); return; 
    case 129 	: fprintf(s, "token: <signal_decl>"); return; 
    case 130 	: fprintf(s, "token: <procedure_decl>"); return; 
    case 131 	: fprintf(s, "token: <var_decl>"); return; 
    case 132 	: fprintf(s, "token: <type_decl>"); return; 
    case 133 	: fprintf(s, "token: <const_decl>"); return; 
    case 134 	: fprintf(s, "token: <meta_decl>"); return; 
    case 135 	: fprintf(s, "token: <type_id>"); return; 
    case 136 	: fprintf(s, "token: <var_decl_1>"); return; 
    case 137 	: fprintf(s, "token: <var_decl_2>"); return; 
    case 138 	: fprintf(s, "token: <var_decl_3>"); return; 
    case 139 	: fprintf(s, "token: <var_decl_4>"); return; 
    case 140 	: fprintf(s, "token: <var_decl_4a>"); return; 
    case 141 	: fprintf(s, "token: <var_decl_4b>"); return; 
    case 142 	: fprintf(s, "token: <meta_trans_def>"); return; 
    case 143 	: fprintf(s, "token: <process_decl_1a>"); return; 
    case 144 	: fprintf(s, "token: <name_(>"); return; 
    case 145 	: fprintf(s, "token: <process_decl_1b>"); return; 
    case 146 	: fprintf(s, "token: <const>"); return; 
    case 147 	: fprintf(s, "token: <process_decl_1c>"); return; 
    case 148 	: fprintf(s, "token: <process_decl_1>"); return; 
    case 149 	: fprintf(s, "token: <fpar_1>"); return; 
    case 150 	: fprintf(s, "token: <fpar_opt>"); return; 
    case 151 	: fprintf(s, "token: <fpar_2>"); return; 
    case 152 	: fprintf(s, "token: <fpar>"); return; 
    case 153 	: fprintf(s, "token: <fpar_3>"); return; 
    case 154 	: fprintf(s, "token: <process_decl_2>"); return; 
    case 155 	: fprintf(s, "token: <process_decl_3>"); return; 
    case 156 	: fprintf(s, "token: <state_decl>"); return; 
    case 157 	: fprintf(s, "token: <state_decl_1>"); return; 
    case 158 	: fprintf(s, "token: <state_decl_2>"); return; 
    case 159 	: fprintf(s, "token: <state_decl_3>"); return; 
    case 160 	: fprintf(s, "token: <state_option>"); return; 
    case 161 	: fprintf(s, "token: <state_decl_4>"); return; 
    case 162 	: fprintf(s, "token: <assert>"); return; 
    case 163 	: fprintf(s, "token: <state_decl_5>"); return; 
    case 164 	: fprintf(s, "token: <tpc_cons>"); return; 
    case 165 	: fprintf(s, "token: <state_decl_6>"); return; 
    case 166 	: fprintf(s, "token: <save_signal>"); return; 
    case 167 	: fprintf(s, "token: <state_decl_7>"); return; 
    case 168 	: fprintf(s, "token: <trans_decl>"); return; 
    case 169 	: fprintf(s, "token: <assert_1>"); return; 
    case 170 	: fprintf(s, "token: <expression>"); return; 
    case 171 	: fprintf(s, "token: <save_signal_1>"); return; 
    case 172 	: fprintf(s, "token: <save_signal_2>"); return; 
    case 173 	: fprintf(s, "token: <trans_decl_0>"); return; 
    case 174 	: fprintf(s, "token: <label>"); return; 
    case 175 	: fprintf(s, "token: <deadline>"); return; 
    case 176 	: fprintf(s, "token: <trans_decl_1>"); return; 
    case 177 	: fprintf(s, "token: <provided>"); return; 
    case 178 	: fprintf(s, "token: <trans_decl_2>"); return; 
    case 179 	: fprintf(s, "token: <when>"); return; 
    case 180 	: fprintf(s, "token: <trans_decl_3>"); return; 
    case 181 	: fprintf(s, "token: <input>"); return; 
    case 182 	: fprintf(s, "token: <trans_decl_4>"); return; 
    case 183 	: fprintf(s, "token: <statement>"); return; 
    case 184 	: fprintf(s, "token: <terminator>"); return; 
    case 185 	: fprintf(s, "token: <label_1>"); return; 
    case 186 	: fprintf(s, "token: <single_expression>"); return; 
    case 187 	: fprintf(s, "token: <when_1>"); return; 
    case 188 	: fprintf(s, "token: <input_1>"); return; 
    case 189 	: fprintf(s, "token: <input_2>"); return; 
    case 190 	: fprintf(s, "token: <input_3>"); return; 
    case 191 	: fprintf(s, "token: <input_4>"); return; 
    case 192 	: fprintf(s, "token: <action>"); return; 
    case 193 	: fprintf(s, "token: <if>"); return; 
    case 194 	: fprintf(s, "token: <while>"); return; 
    case 195 	: fprintf(s, "token: <terminator_1>"); return; 
    case 196 	: fprintf(s, "token: <if_1>"); return; 
    case 197 	: fprintf(s, "token: <if_2>"); return; 
    case 198 	: fprintf(s, "token: <if_3>"); return; 
    case 199 	: fprintf(s, "token: <while_1>"); return; 
    case 200 	: fprintf(s, "token: <while_2>"); return; 
    case 201 	: fprintf(s, "token: <from_name>"); return; 
    case 202 	: fprintf(s, "token: <meta_trans_def_3>"); return; 
    case 203 	: fprintf(s, "token: <meta_trans_def_2>"); return; 
    case 204 	: fprintf(s, "token: <meta_trans_def_1>"); return; 
    case 205 	: fprintf(s, "token: <meta_trans_def_1b>"); return; 
    case 206 	: fprintf(s, "token: <meta_trans_def_4>"); return; 
    case 207 	: fprintf(s, "token: <meta_trans_def_5>"); return; 
    case 208 	: fprintf(s, "token: <meta_trans_def_6b>"); return; 
    case 209 	: fprintf(s, "token: <meta_trans_def_6>"); return; 
    case 210 	: fprintf(s, "token: <action_a1>"); return; 
    case 211 	: fprintf(s, "token: <action_b>"); return; 
    case 212 	: fprintf(s, "token: <left_asgn>"); return; 
    case 213 	: fprintf(s, "token: <output_1>"); return; 
    case 214 	: fprintf(s, "token: <output_2>"); return; 
    case 215 	: fprintf(s, "token: <output_3>"); return; 
    case 216 	: fprintf(s, "token: <output_4>"); return; 
    case 217 	: fprintf(s, "token: <output_5>"); return; 
    case 218 	: fprintf(s, "token: <output_6>"); return; 
    case 219 	: fprintf(s, "token: <expr_4>"); return; 
    case 220 	: fprintf(s, "token: <call_0>"); return; 
    case 221 	: fprintf(s, "token: <call_1>"); return; 
    case 222 	: fprintf(s, "token: <call_2>"); return; 
    case 223 	: fprintf(s, "token: <call_2b>"); return; 
    case 224 	: fprintf(s, "token: <call>"); return; 
    case 225 	: fprintf(s, "token: <fork_0>"); return; 
    case 226 	: fprintf(s, "token: <fork_1>"); return; 
    case 227 	: fprintf(s, "token: <fork_2>"); return; 
    case 228 	: fprintf(s, "token: <fork_2b>"); return; 
    case 229 	: fprintf(s, "token: <fork>"); return; 
    case 230 	: fprintf(s, "token: <signalroute_decl_1a>"); return; 
    case 231 	: fprintf(s, "token: <signalroute_decl_1b>"); return; 
    case 232 	: fprintf(s, "token: <signalroute_decl_1>"); return; 
    case 233 	: fprintf(s, "token: <signalroute_decl_2>"); return; 
    case 234 	: fprintf(s, "token: <signalroute_option>"); return; 
    case 235 	: fprintf(s, "token: <signalroute_decl_3>"); return; 
    case 236 	: fprintf(s, "token: <signalroute_decl_4a>"); return; 
    case 237 	: fprintf(s, "token: <signalroute_decl_4>"); return; 
    case 238 	: fprintf(s, "token: <signalroute_decl_5>"); return; 
    case 239 	: fprintf(s, "token: <signalroute_decl_6>"); return; 
    case 240 	: fprintf(s, "token: <delay_1>"); return; 
    case 241 	: fprintf(s, "token: <delay_2>"); return; 
    case 242 	: fprintf(s, "token: <delay_3>"); return; 
    case 243 	: fprintf(s, "token: <delay_4>"); return; 
    case 244 	: fprintf(s, "token: <delay>"); return; 
    case 245 	: fprintf(s, "token: <rate_1>"); return; 
    case 246 	: fprintf(s, "token: <rate_2>"); return; 
    case 247 	: fprintf(s, "token: <rate_3>"); return; 
    case 248 	: fprintf(s, "token: <rate_4>"); return; 
    case 249 	: fprintf(s, "token: <rate>"); return; 
    case 250 	: fprintf(s, "token: <signal_decl_1>"); return; 
    case 251 	: fprintf(s, "token: <signal_decl_2>"); return; 
    case 252 	: fprintf(s, "token: <signal_decl_2b>"); return; 
    case 253 	: fprintf(s, "token: <signal_decl_3>"); return; 
    case 254 	: fprintf(s, "token: <const_decl_1>"); return; 
    case 255 	: fprintf(s, "token: <const_decl_2>"); return; 
    case 256 	: fprintf(s, "token: <const_decl_3>"); return; 
    case 257 	: fprintf(s, "token: <const_decl_3a>"); return; 
    case 258 	: fprintf(s, "token: <type_decl_1>"); return; 
    case 259 	: fprintf(s, "token: <type_decl_2>"); return; 
    case 260 	: fprintf(s, "token: <type>"); return; 
    case 261 	: fprintf(s, "token: <enum_1>"); return; 
    case 262 	: fprintf(s, "token: <enum_1b>"); return; 
    case 263 	: fprintf(s, "token: <range_1>"); return; 
    case 264 	: fprintf(s, "token: <array_1>"); return; 
    case 265 	: fprintf(s, "token: <array_1b>"); return; 
    case 266 	: fprintf(s, "token: <array_2>"); return; 
    case 267 	: fprintf(s, "token: <array_3>"); return; 
    case 268 	: fprintf(s, "token: <record_1>"); return; 
    case 269 	: fprintf(s, "token: <record_2>"); return; 
    case 270 	: fprintf(s, "token: <record_3>"); return; 
    case 271 	: fprintf(s, "token: <string_1>"); return; 
    case 272 	: fprintf(s, "token: <string_2>"); return; 
    case 273 	: fprintf(s, "token: <string_3>"); return; 
    case 274 	: fprintf(s, "token: <abstract_1>"); return; 
    case 275 	: fprintf(s, "token: <abstract_2>"); return; 
    case 276 	: fprintf(s, "token: <abstract_3>"); return; 
    case 277 	: fprintf(s, "token: <abstract_4>"); return; 
    case 278 	: fprintf(s, "token: <abstract_5>"); return; 
    case 279 	: fprintf(s, "token: <abstract_6>"); return; 
    case 280 	: fprintf(s, "token: <expr_1>"); return; 
    case 281 	: fprintf(s, "token: <expr_2>"); return; 
    case 282 	: fprintf(s, "token: <expr_3>"); return; 
    case 283 	: fprintf(s, "token: <expr_3a>"); return; 
    case 284 	: fprintf(s, "token: <expr_3_b1>"); return; 
    case 285 	: fprintf(s, "token: <single_name_(>"); return; 
    case 286 	: fprintf(s, "token: <function_2>"); return; 
    case 287 	: fprintf(s, "token: <expr_5>"); return; 
    case 288 	: fprintf(s, "token: <expr_6>"); return; 
    case 289 	: fprintf(s, "token: <expr_7>"); return; 
    case 290 	: fprintf(s, "token: <single_+>"); return; 
    case 291 	: fprintf(s, "token: <expr_8>"); return; 
    case 292 	: fprintf(s, "token: <single_->"); return; 
    case 293 	: fprintf(s, "token: <expr_9>"); return; 
    case 294 	: fprintf(s, "token: <expr_10>"); return; 
    case 295 	: fprintf(s, "token: <expr_11>"); return; 
    case 296 	: fprintf(s, "token: <expr_12>"); return; 
    case 297 	: fprintf(s, "token: <expr_12_+>"); return; 
    case 298 	: fprintf(s, "token: <expr_12_->"); return; 
    case 299 	: fprintf(s, "token: <expr_13>"); return; 
    case 300 	: fprintf(s, "token: <single_=>"); return; 
    case 301 	: fprintf(s, "token: <expr_14>"); return; 
    case 302 	: fprintf(s, "token: <expr_15>"); return; 
    case 303 	: fprintf(s, "token: <expr_16>"); return; 
    case 304 	: fprintf(s, "token: <expr_16_1>"); return; 
    case 305 	: fprintf(s, "token: <expr_17_1>"); return; 
    case 306 	: fprintf(s, "token: <expr_17_2>"); return; 
    case 307 	: fprintf(s, "token: <expr_17_3>"); return; 
    case 308 	: fprintf(s, "token: <single_(>"); return; 
    case 309 	: fprintf(s, "token: <procedure_decl_1>"); return; 
    case 310 	: fprintf(s, "token: <procedure_decl_2>"); return; 
    case 311 	: fprintf(s, "token: <procedure_decl_3>"); return; 
    case 312 	: fprintf(s, "token: <procedure_decl_4>"); return; 
    case 313 	: fprintf(s, "token: <procedure_decl_4a>"); return; 
    case 314 	: fprintf(s, "token: <procedure_decl_5>"); return; 
    default     : fprintf(s, "unknown item");         
    }
}

/****  End of lexical.c  ****/
