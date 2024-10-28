/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       diag.c  :  Runtime diagnoses and statistics.             **/
/**                                                                **/
/**     03/13/98  :  Creation. (BB)                                **/
/**     06/10/98  :  Minor corrections. (BB)                       **/
/**     07/24/98  :  Got rid of operations count. (BB)             **/
/**     07/29/98  :  New error codes. (BB)                         **/
/**     08/04/98  :  'log_message' added. (BB)                     **/
/**     08/26/98  :  New error codes. (BB)                         **/
/**     09/03/98  :  New error codes. (BB)                         **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/07/98  :  Minor corrections. (BB)                       **/
/**     09/24/98  :  New error codes. (BB)                         **/
/**     10/08/98  :  New error code. (BB)                          **/
/**     01/05/99  :  New error code. (BB)                          **/
/**     01/12/99  :  New error code. (BB)                          **/
/**     01/28/99  :  New error code. (BB)                          **/
/**     02/24/99  :  New error code. (BB)                          **/
/**     08/13/01  :  New error codes. (SJ)                         **/
/**     07/02/02  :  Reorganization. (BB)                          **/
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
#include "diag.h"

/****  Public variable.                                          ****/

uint4  lash_errno = LASH_ERR_NOERROR;
                                   /*  Code of the most recent      */
                                   /*  error.                       */

/****  Global variables.                                         ****/

#if LASH_CHECK_LEVEL >= 1

static uint8  mem_usage     = ZERO_INT8;
                                   /*  Number of currently          */
                                   /*  allocated bytes of memory.   */

static uint8  max_mem_usage = ZERO_INT8;
                                   /*  Maximum number of allocated  */
                                   /*  memory since last reset.     */

#endif  /* >= 1 */

#if LASH_CHECK_LEVEL >= 2

static FILE *logfile;              /*  Logfile.                     */

#endif  /* >= 2 */

/****  Prototype of private function.                            ****/

#if LASH_CHECK_LEVEL >= 2

static void  log_status(void);

#endif  /* >= 2 */

/****  Public visible functions.                                 ****/

/**  void lash_perror(char *s)  :  Produces a message on the 
                     standard error output, describing the last
                     error encountered during a call to the package.
                     The argument string s is printed first, followed
                     by a colon and a blank, and then by the message 
                     and a newline. (However, if s is a null pointer
                     or points to a null string, the colon is not
                     printed.) To be of most use, the argument string
                     should include the name of the program that 
                     incurred the error. The error number is
                     taken from the global variable lash_errno,
                     which is set when errors occur but not cleared
                     when non-erroneous calls are made.            **/

void  lash_perror(s)
  char *s;
{
  register char *msg;

  if (s && *s)
    fprintf(stderr, "%s: ", s);
  
  switch(lash_errno)
    {
    case LASH_ERR_NOERROR:
      msg = "no error";
      break;

    case LASH_ERR_ALREADY:
      msg = "operation already performed";
      break;

    case LASH_ERR_BAD_ARCH:
      msg = "bad architecture";
      break;

    case LASH_ERR_IN_USE:
      msg = "object(s) in use";
      break;

    case LASH_ERR_NOT_INIT:
      msg = "package not initialized";
      break;

    case LASH_ERR_NO_MEM:
      msg = "not enough memory";
      break;

    case LASH_ERR_LOGFILE:
      msg = "cannot create logfile";
      break;

    case LASH_ERR_BAD_STATE:
      msg = "no such state";
      break;

    case LASH_ERR_BAD_TRAN:
      msg = "no such transition";
      break;

    case LASH_ERR_CORRUPT:
      msg = "corrupt data";
      break;

    case LASH_ERR_PROP:
      msg = "inconsistent property";
      break;

    case LASH_ERR_BAD_TYPE:
      msg = "bad type of object";
      break;

    case LASH_ERR_ALPHABET:
      msg = "alphabet mismatch";
      break;

    case LASH_ERR_ALPH_SIZE:
      msg = "alphabet too large";
      break;

    case LASH_ERR_BAD_VALUE:
      msg = "bad value of parameter";
      break;

    case LASH_ERR_ABORT:
      msg = "operation aborted";
      break;

    case LASH_ERR_BASE:
      msg = "base mismatch";
      break;

    case LASH_ERR_DIMENSION:
      msg = "dimension mismatch";
      break;

    case LASH_ERR_NOT_IMPL:
      msg = "not (yet) implemented";
      break;

    case LASH_ERR_OVERFLOW:
      msg = "arithmetic overflow";
      break;

    case LASH_ERR_EXCEPTION:
      msg = "arithmetic exception";
      break;

    case LASH_ERR_ITERATION:
      msg = "not iterable";
      break;

    case LASH_ERR_INTERRUPT:
      msg = "user interrupt";
      break;

    case LASH_ERR_IO:
      msg = "IO error";
      break;

    case LASH_ERR_FILE:
      msg = "file format error";
      break;

    case LASH_ERR_TOO_BIG:
      msg = "object(s) too large";
      break;

    default:
      msg = "unknown error";
    }

  fprintf(stderr, "%s\n", msg);
}

#if LASH_CHECK_LEVEL >= 1

/**  uint8 lash_get_mem_usage() :  Returns the amount of memory
                     (in bytes) currently allocated. Cannot incur
                     an error.                                     **/

uint8  lash_get_mem_usage()
{
  return mem_usage;
}

/**  uint8 lash_get_max_mem_usage() :  Returns the maximum amount
                     of memory (in bytes) allocated since the first
                     use of the package or last call to
                     lash_reset_max_mem. Cannot incur an error.    **/

uint8  lash_get_max_mem_usage()
{
  return max_mem_usage;
}

/**  void lash_reset_max_mem_usage() :  Resets the current value of
                     the maximum amount of allocated memory.
                     Cannot incur an error.                        **/

void  lash_reset_max_mem_usage()
{
  max_mem_usage = ZERO_INT8;
}

#endif  /* >= 1 */

/****  Public invisible functions.                               ****/

#if LASH_CHECK_LEVEL >= 2

/**  int  diag__log_init()  :  Initializes the logging of diagnostic
                     messages. Returns 0 if successful, or -1 if
                     the logfile cannot be created.                **/

int  diag__log_init()
{
  if (!(logfile = fopen(LOG_FILE_NAME, "w")))
    return -1;

  fprintf(logfile, "begin\n");
  fflush(logfile);
  return 0;
}

/**  void  diag__log_entry(s)  :  Logs the entry point of the
                     function named s.                             **/

void  diag__log_entry(s)
  char *s;
{
  fprintf(logfile, "entry %s ", s);
  log_status();
}

/**  void  diag__log_return()  :  Logs the return point of the
                     current function.                             **/

void  diag__log_return()
{
  fprintf(logfile, "return ");

  log_status();
}

/**  void  diag__log_fail()  :  Logs the failure point of the   
                     current function.                             **/

void  diag__log_fail()
{
  fprintf(logfile, "fail ");
  log_status();
}

/**  void  diag__log_message(c)  :  Logs a comment c.              **/
  
void  diag__log_message(c)
  char *c;
{
  fprintf(logfile, "msg %s\n", c);
  fflush(logfile);
}

/**  void  diag__log_uint4(n)  :  Logs a number n.                 **/
  
void  diag__log_uint4(n)
  uint4 n;
{
  fprintf(logfile, "number ");
  fprintf(logfile, FMT_UINT4, n);
  fprintf(logfile, "\n");
  fflush(logfile);
}
                                             
/**  void  diag__log_end()  :  Terminates the logging of
                     diagnostic messages.                          **/

void  diag__log_end()
{
  fprintf(logfile, "end\n");
  fclose(logfile);
}

#endif  /* >= 2 */

/****  Private functions.                                        ****/

#if LASH_CHECK_LEVEL >= 2

/**  void  log_status()  :  Logs the current values of the number    
                     of operations performed and of the amount   
                     of memory allocated.                          **/

static void  log_status()
{
  fprintf(logfile, FMT_UINT8, lash_get_mem_usage());
  fprintf(logfile, " ");
  fprintf(logfile, FMT_UINT8, lash_get_max_mem_usage());
  fprintf(logfile, "\n");
  fflush(logfile);
}

#endif  /* >= 2 */

#if LASH_CHECK_LEVEL >= 1

/**  void  diag__log_alloc(n)  :  Adds n to the current number of
                     allocated bytes of memory.                    **/

void  diag__log_alloc(n)
  uint8 n;
{
  if ((mem_usage += n) > max_mem_usage)
    max_mem_usage = mem_usage;
}

/**  void  diag__log_unalloc(n)  :  Removes n from the current 
                     number of allocated bytes of memory.          **/

void  diag__log_unalloc(n)
  uint8 n;
{
  mem_usage -= n;
}

#endif  /* >= 1 */

/****  End of diag.c  ****/
