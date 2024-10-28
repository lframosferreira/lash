/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-symbol.c  :  Function related to QDD symbols.           **/
/**                                                                **/
/**        12/08/99  :  Creation. (JMF)                            **/
/**        07/08/02  :  Reorganization. (BB)                       **/
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

#include "qdd-symbol.h"

/**  int qdd_qm_cmp(qm1, qm2)  :  Returns 0 if queue_symbol *qm1
                     and *qm2 are identical, 1 if they are not.    **/

int qdd_qm_cmp(qm1, qm2)
     queue_symbol *qm1, *qm2;
{
#if LASH_CHECK_LEVEL >= 2
  if (!qm1 || !qm2)
    return -1;
#endif   /* >= 2 */

  return ((queuesymbol_queue(qm1) != 
	   queuesymbol_queue(qm2))
	  || (queuesymbol_symbol(qm1) !=
	      queuesymbol_symbol(qm2)));
}

/**  int qdd_symbol_cmp(p1, p2)  :  Compares the two symbols
                     *p1 and *p2. Returns a negative value
		     if *p1 < *p2, 0 if *p1 = *p2, and a positive
		     value if *p1 > *p2.

	             In the case of error, returns -1.             **/

int qdd_symbol_cmp(p1, p2)
     symbol *p1, *p2;
{
#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2)
    return -1;
#endif   /* >= 1 */
  
  return (*p1) - (*p2);
}

/****  End of qdd-symbol.c  ****/
