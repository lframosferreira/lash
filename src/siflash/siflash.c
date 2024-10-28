/********************************************************************/
/**                                                                **/
/**  Simple IF  LASH (Siflash) compiler -- v0.9                    **/
/**  ==================================                            **/
/**                                                                **/
/**     siflash.c   :  Front-end.                                  **/
/**                                                                **/
/**     05/10/99    :  Creation. (LL)                              **/
/**     11/15/99    :  Conversion to  IF. (LL)                     **/
/**     02/23/01    :  Minor correction. (BB)                      **/
/**     07/25/02    :  Modification for IF2.0 (LL)                 **/
/**     09/05/02    :  Reorganization. (BB)                        **/
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
#include "lash.h"
#include "datastruct.h"
#include "siflash.h"
#include "lexical.h"
#include "grammar.h"
#include "semantic.h" 

/****  Definitions.                                              ****/

#define  DEBUG    0
#define  ERROR    0
#define  SHIFT    1
#define  REDUCE   2

/****  Public variables.                                         ****/

int   siflash_verbose = 0;       /*  Is the compiler verbose?       */
int   siflash_explore = 1;       /*  Do we explore the state-space? */
int   siflash_save_steps = 0;    /*  Do we save the NDD at each step
				     of the exploration?            */
char *siflash_sm_name = NULL;    /*  State machine output file name */
char *siflash_sndd_name = NULL;  /*  NDD output file name.          */
char *siflash_sdot_name = NULL;

/****  Global variable.                                          ****/

static char *source_file_name = NULL;

/****  Macro for extracting entries from the shift/reduce table. ****/

#define  get_entry(s1, s2)  \
    ((sr_table[l_table[s1]][(s2) >> 2] >> (2 * ((s2) % 4))) % 4)

/****  Prototypes of private functions.                          ****/

static stack *init_stack(void);
static int    search_rule(stack *, uint4 *);
static int    main_loop(void);
static int    compile_source(void);
static void   display_statistics(void);
static void   display_usage(char *);
static int    parse_args(int, char *[]);
static void   report_syntax_error(lex_unit *);

/****  Private functions.                                        ****/

/**  stack *init_stack()  :  Allocates a stack of lexical units, and
                    pushes an initial DOLLAR unit on that stack. In
		    the case of success, returns a pointer to the
		    new stack. In the case of insufficient memory,
                    returns a NULL pointer.                        **/

static stack *init_stack()
{
  register stack    *st;
           lex_unit  lu;

  st = stack__new_empty(lex_unit);
  if (!st)
    return NULL;

  lu.symbol  = DOLLAR;
  lu.param   = NULL;
  lu.line_no = lu.char_no = ZERO_INT4;

  if (stack__push(st, &lu) < 0)
    {
      lex_free_token(&lu);
      stack__free(st);
      return NULL;
    }

  return st;
}

/**  int  search_rule(st, p)  :  Scans efficiently the production
                    rules defining the language being compiled in
                    order to find a rule matching the top lexical
                    units on the stack *st. This function returns the
                    index of the rule that is found in *p. If no
                    suitable rule exists, the function returns -1.
                    Otherwise, it returns 0.                       **/

static int  search_rule(st, p)
  stack *st;
  uint4 *p;
{
  register uint4  s0, stack_symbol1, stack_symbol2, test_symbol;
  register uint4  first_pointer, start_pointer, end_pointer,
                  test_pointer;

  s0 = ((lex_unit *) stack__top(st)) -> symbol;
  first_pointer = start_rule[s0];

  stack_symbol1 = ((lex_unit *) stack__pick(st, 1)) -> symbol;
  if (stack_symbol1 == DOLLAR)
    {
      *p = first_pointer;
      return 0;
    }

  stack_symbol2 = ((lex_unit *) stack__pick(st, 2)) -> symbol;
  start_pointer = first_pointer;
  end_pointer   = end_rule[s0];

  do
    {
      test_pointer = (start_pointer + end_pointer + 1) / 2;

      switch(rule_len[test_pointer])
	{
	case 1 :
	  *p = first_pointer;
	  return 0;

	case 2 :
	  test_symbol = derivation[test_pointer][1];
          if (test_symbol == stack_symbol1)
	    {
	      *p = test_pointer;
	      return 0;
     	    }
	  if (test_symbol > stack_symbol1)
	    end_pointer = test_pointer - 1;
	  else
	    start_pointer = test_pointer + 1;
	  break;
	  
	case 3 :
	  test_symbol = derivation[test_pointer][1];
          if (test_symbol == stack_symbol1)
	    {
	      if (stack_symbol2 == DOLLAR)
		{
		  *p = first_pointer;
		  return 0;
		}

	      test_symbol = derivation[test_pointer][0];
	      if (test_symbol == stack_symbol2)
		{
		  *p = test_pointer;
		  return 0;
		}

	      if (test_symbol > stack_symbol2)
		end_pointer = test_pointer - 1;
	      else
		start_pointer = test_pointer + 1;
	    }
	  else
	    if (test_symbol > stack_symbol1)
	      end_pointer = test_pointer - 1;
	    else
	      start_pointer = test_pointer + 1;
	}
    } while (start_pointer <= end_pointer);

  if (rule_len[first_pointer] > 1 &&
      derivation[first_pointer][1] != stack_symbol1)
    return -1;

  if (rule_len[first_pointer] > 2 &&
      derivation[first_pointer][0] != stack_symbol2)
    return -1;
  *p = first_pointer;

  return 0;
}

/**  int  main_loop()  :  Repeatedly extracts a lexical unit from
                    the source file and calls the appropriate semantic
                    functions according to the syntax of the language
                    being compiled. Returns -1 and gives out a message
                    in the case of an error. Returns 0 otherwise.  **/

static int  main_loop()
{
  register uint4     strip_symbol = 0, stack_symbol;
  register int       forward;
  register stack    *st;
           uint4     rule;
           lex_unit  strip_lu, new_lu;
           void     *new_param;
	   uint1     ignored;
	   
  ignored = 0;
  st = init_stack();
  if (!st)
    {
      report_siflash_memory_error();
      return -1;
    }

  for (forward = 1;;)
    {
      if (forward)
	{
	  if (lex_next_token(&strip_lu) < 0)
	    {
	      stack__free_fn(st, (void (*)(void *)) lex_free_token);
	      return -1;
	    }
#if DEBUG == 1
	  fflush(stdout);
	  lex_print_token(stdout, &strip_lu);  
	 fprintf(stdout, ".\n");  
# endif
	  strip_symbol = strip_lu.symbol;
	}

      stack_symbol = ((lex_unit *) stack__top(st)) -> symbol;

      if ((strip_symbol == DOLLAR) && (stack__size(st) == 2) &&
          (stack_symbol == D_SYMBOL))
	break;

      switch(get_entry(stack_symbol, strip_symbol))
	{
	case ERROR :
	  report_syntax_error(&strip_lu);
          lex_free_token(&strip_lu);
          stack__free_fn(st, (void (*)(void *)) lex_free_token);
	  return -1;

	case SHIFT:
	  forward = 1;
	  new_lu  = strip_lu;
#if DEBUG == 1
	  fflush(stdout);
	  fprintf(stdout, "S : ");  
	 lex_print_token(stdout, &new_lu);  
	 fprintf(stdout, ".\n");  
# endif
          break;

        case REDUCE:
	  forward = 0;
	  if (search_rule(st, &rule) < 0)
	    {
              report_syntax_error(&strip_lu);
	      lex_free_token(&strip_lu);
	      stack__free_fn(st, (void (*)(void *)) lex_free_token);
	      return -1;
	    }
          new_lu.symbol = first_symbol[rule];
#if DEBUG == 1
	  fflush(stdout);
	  fprintf(stdout, "R : ");  
	  lex_print_token(stdout, &new_lu);  
	  fprintf(stdout, ".\n");  
# endif
          new_param = NULL;
	  ignored = 0;	  
#if DEBUG < 2
	  if (sem_function[rule](st, &new_param, &ignored) < 0)
	    {
	      lex_free_token(&strip_lu);
	      stack__free_fn(st, (void (*)(void *)) lex_free_token);
	      return -1;
	    }
# endif
          new_lu.param = new_param;
	  new_lu.ignored = ignored;
          stack__reduce_fn(st, rule_len[rule], NULL);
	}
      
      if (stack__push(st, &new_lu) < 0)
	{
	  lex_free_token(&strip_lu);
          lex_free_token(&new_lu);
          stack__free_fn(st, (void (*)(void *)) lex_free_token);
          report_siflash_memory_error();
	  return -1;
	}
    }
  
  lex_free_token(&strip_lu);
  stack__free_fn(st, (void (*)(void *)) lex_free_token);
  
  return 0;  
}

/**  int  compile_source()  :  Open, compiles, and closes the source
                    file. In the case of an error, returns -1.
		    Otherwise, sets the global variables according
		    to the compilation results, and returns 0.     **/

static int  compile_source()
{
  register int  rc;

  if (lex_open(source_file_name) < 0)
    return -1;

  if (sem_init() < 0)
    {
      lex_close();
      return -1;
    }

  rc = main_loop();

  if ((!rc) && sem_end() < 0)
    rc = -1;

  if (sem_finish() < 0)
    rc = -1;

  lex_close();

  return (rc < 0) ? -1 : 0;
}

/**  void  display_statistics()  :  Displays the memory statistics
                    reported by the LASH package. This function
                    does not incur errors.                         **/

static void  display_statistics()
{

#if LASH_CHECK_LEVEL >= 1

  printf("Runtime statistics:\n");
  printf("  residual memory : %llu byte(s).\n", lash_get_mem_usage());
  printf("  max memory      : %llu byte(s).\n", 
      lash_get_max_mem_usage());

#endif  /* >= 1 */
}

/**  void  display_usage(name)  :  Displays the command-line syntax
                    of the program. The name under which the compiler
                    has been invoked is given bt *name. This function
                    does not report errors.                        **/

static void  display_usage(name)
  char *name;
{
  fprintf(stderr, "Usage: %s [ options ] <source-file>\n", name);
  fprintf(stderr, 
  "    Options are  -c          : Does not explote state space.\n");
  fprintf(stderr, 
  "                 -m <name>   : Outputs state machines.\n");
  fprintf(stderr, 
  "                 -v          : Verbose output.\n");
  fprintf(stderr, 
  "                 -ndd <name> : Outputs NDD (variables) in LASH format.\n");
  fprintf(stderr, 
  "                 -dot <name> : Outputs NDD (variables) in dot format.\n");
  fprintf(stderr, 
  "                 -ss         : Outputs NDD at each exploration step.\n");
  fprintf(stderr, 
  "     (option -ss is active only if option(s) -ndd or/and -dot is set.)\n");
}

/**  int  parse_args(argc, argv)  :  Parses the arguments of the
                    main function and updates global variables
                    accordingly. In the case of an error, returns
                    -1. Otherwise, returns 0.                      **/

static int  parse_args(argc, argv)
  int   argc;
  char *argv[];
{
  register uint4  i, l;
  register char  *occur;
  
  siflash_verbose   = 0;
  source_file_name  = NULL;

  for (i = 1; i < argc; i++)
    {
      if (!strcmp(argv[i], "-v"))
	{
	  if (siflash_verbose)
	    return -1;

	  siflash_verbose = 1;
	  continue;
	}

      if (!strcmp(argv[i], "-c"))
	{
	  if (!siflash_explore)
	    return -1;
	  
	  siflash_explore = 0;
	  continue;
	}
 
      if (!strcmp(argv[i], "-ss"))   /* step saving */ 
	{
	  if (siflash_save_steps)
	    return -1;
	  
	  siflash_save_steps = 1;
	  continue;
	}

      if (!strcmp(argv[i], "-m"))
	{
	  if (siflash_sm_name || (i >= (argc - 1)))
	    return -1;

	  siflash_sm_name = argv[++i];
	  continue;
	}

      if (!strcmp(argv[i], "-ndd"))
	{
	  if (siflash_sndd_name || (i >= (argc - 1)))
	    return -1;

	  siflash_sndd_name = argv[++i];
	  l = strlen( siflash_sndd_name) ; 
	  occur = strstr( siflash_sndd_name, ".ndd");
	  if ((occur) && (strcmp(occur, ".ndd") == 0))
	    strcpy(occur, "");
	  continue;
	}

     if (!strcmp(argv[i], "-dot"))
	{
	  if (siflash_sdot_name || (i >= (argc - 1)))
	    return -1;

	  siflash_sdot_name = argv[++i];
	  l = strlen(siflash_sdot_name) ; 
	  occur = strstr( siflash_sdot_name, ".dot");
	  if ((occur) && (strcmp(occur, ".dot") == 0))
	    strcpy(occur, "");
	  continue;
	}

      if (source_file_name)
	  return -1;

      source_file_name = argv[i];
    }

  return  source_file_name ? 0 : -1;
}

/**  void  report_syntax_error(p)  :  Reports a syntactic error that 
                    occurred after reading the lexical unit *p.    **/

static void  report_syntax_error(p)
  lex_unit *p;
{
  fprintf(stderr, "Syntax error at (L%u, C%u): Unexpected ",
      p -> line_no, p -> char_no);
  lex_print_token(stderr, p);
  fprintf(stderr, ".\n");
}

/****  Public functions.                                         ****/

/**  void  report_lash_error(msg)  :  Reports an error that occurred
                    as a result of calling a LASH function. The error
                    message that will be printed includes the string
                    given by *msg.                                 **/

void  report_lash_error(msg)
  char *msg;
{
  char  line[80];

  sprintf(line, "LASH error: %.20s", msg);
  lash_perror(line);
}

/**  void  report_siflash_error(msg)  :  Reports an error encountered
                     in a function. A message expliciting the
                     error is given in *msg.                       **/

void  report_siflash_error(msg)
  char *msg;
{
  fprintf(stderr, "%s.\n", msg);
}

/**  main(argc, argv)  :  Entry point of the state-space explorer.
                     The syntax is the following:

                         siflash [ options ] <source-file>

                     For the available options, see Function
                     display_usage().                              **/

int  main(argc, argv)
  int   argc;
  char *argv[];
{ 
  if (parse_args(argc, argv) < 0)
    {
      display_usage(argv[0]);
      exit(-1);
    }

  if (lash_init() < 0)
    {
      report_lash_error("init");
      exit(-1);
    }

  if (compile_source() < 0)
    {
      lash_end();
      exit(-1);
    }

  if (siflash_verbose)
    display_statistics();

  if (lash_end() < 0)
    {
      report_lash_error("end");
      exit(-1);
    }
    
  exit(0);
}

/****  End of siflash.c  ****/