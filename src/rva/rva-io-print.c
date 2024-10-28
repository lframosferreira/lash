/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/** rva-io-print.c  :  Textual exportations of RVAs.               **/
/**                                                                **/
/**        08/20/01  :  Creation. (SJ)                             **/
/**        07/10/02  :  Reorganization. (BB)                       **/
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
#include "rva-io-print.h"
#include "lash-rva.h"
#include "diag.h"
#include "auto-io-print.h"

/****  Prototypes of private functions.                          ****/

static void  print_rva_characteristics(rva *);
static void  print_rva_properties(rva *);
static void  print_rva_automaton(rva *);

/****  Private functions.                                        ****/

/**  void  print_rva_characteristics(r)  :  Prints the 
                     characteristics of the automaton *a.          **/

static void  print_rva_characteristics(r)
  rva *r;
{
  printf("RVA of dimension %d, ", r -> dim);

  if (r -> dim)
    printf("operating in base %d,\n", r -> base);
  else
    {
      if (r -> universal)
	printf("representing the universal set { () }.\n");
      else
	printf("representing the empty set { }.\n");
    }
}

/**  void  print_rva_properties(r)  :  Prints the known properties 
                     of the RVA *r.                                **/

static void  print_rva_properties(r)
  rva *r;
{
  register uint1  v;

  if (!(r -> dim))
    return;

  v = r -> properties;

  if (v != RVA_PROP_NOTHING)
    {
      printf("known to operate\n");

      if (v & RVA_PROP_SERIAL)
	printf("   serially,\n");

      if (v & RVA_PROP_RESTRICTED)
	printf("   restricted to the additive theory of reals and integers,\n");

      if (!(v & (RVA_PROP_SERIAL | RVA_PROP_RESTRICTED)))
	printf(" ???,\n");
    } 
}

/**  void  print_rva_automaton(r)  :  Prints the automaton associated
                     to the RVA *r.                                **/

static void  print_rva_automaton(r)
  rva *r;
{
  if (r -> dim)
    {
      printf("associated to the following\n");
      auto_print(r -> automaton);
    }
}


/****  Public visible function.                                  ****/

/**  int  rva_print(a)  :  Prints a description of the RVA *r on
                     stdout.          

                     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  rva_print(r)
  rva *r;
{
  diag__enter("rva_print", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!r)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  print_rva_characteristics(r);
  print_rva_properties(r);
  print_rva_automaton(r);

  diag__return(0);
}

/****  End of rva-io-print.c  ****/
