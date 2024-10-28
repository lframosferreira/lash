/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-io.c  :  I/O operations over QDDs.                      **/
/**                                                                **/
/**    06/27/2000  :  Creation. (JMF)                              **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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
#include "lash-qdd.h"
#include "lash-auto-io.h"
#include "qdd.h"
#include "lash-qdd-io.h"

/****  Prototypes of private functions.                          ****/

static void  print_qdd_properties(qdd *);

/****  Private functions.                                        ****/

/**  void  print_qdd_properties(q)  :  Prints the known properties 
                     of the QDD *q.                                **/

static void  print_qdd_properties(q)
     qdd *q;
{
  uint1  v = qdd_known_properties(q);
  
  if (v != QDD_PROP_NOTHING)
    {
      if (v & QDD_PROP_ONE_QUEUE)
	printf("QDD known to operate on a single queue.\n");   
      
      if (!(v & (QDD_PROP_ONE_QUEUE | QDD_PROP_VALID_INIT_TUPLES)))
	printf(" Unknown property.\n");
    }
}

/****  Public visible function.                                  ****/

/**  int  qdd_print(q)  :  Prints a description of the QDD *q on
                     stdout.          

                     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  qdd_print(q)
     qdd *q;
{
#if LASH_CHECK_LEVEL >= 1
  if (!q)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */  

  print_qdd_properties(q);

  if (auto_print(qdd_automaton(q)))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/****  End of qdd-io.c  ****/
