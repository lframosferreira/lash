/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    graph-scc.h  :  Functions related to the search of          **/
/**                    strongly connected components in a graph.   **/
/**                                                                **/
/**        02/27/01  :  Creation. (SJ)                             **/
/**        03/16/01  :  Reorganization. (SJ)                       **/
/**        03/17/01  :  Integration into the LASH package. (SJ)    **/
/**        04/30/01  :  Reorganization. (SJ)                       **/
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

#ifndef LASH_GRAPH_SCC_H
#define LASH_GRAPH_SCC_H

#include "lash-auto.h"
#include "datastruct.h"

/**  Definitions.                                                  **/

/**  SCC_ONLY_REACHABLE  :  Parameter to limit Tarjan's algorithm
                     to reachable states.                          **/

#define SCC_ONLY_REACHABLE         0x01

/**  SCC_MARK_TRANSIENT  :  Parameter to tell Tarjan's algorithm to
                     number the transient components by the value
                     SCC_TRANSIENT_NODE instead of enumerating them
		     like other s.c.c.                             **/

#define SCC_MARK_TRANSIENT         0x02
#define SCC_TRANSIENT_NODE         (uint4) -2

/**  SCC_AUTOMATIC_POP  :  Parameter to tell Tarjan's algorithm that
                     the function dealing with the s.c.c. does not
		     manually remove the nodes from the top of the
		     active stack.                                 **/

#define SCC_AUTOMATIC_POP          0x04

/**  SCC_UNREACHABLE  :  Component number assigned to unreachable
		     states when the SCC_ONLY_REACHABLE parameter
		     is set.                                       **/

#define SCC_UNREACHABLE            (uint4) -1

/**  Function prototypes.                                          **/

int     scc_algorithm(automaton *, uint4 *, uint4 *, uint1,
		     int (*)(automaton *, stack *, uint4, uint4 *,
			     uint1, uint1, void *), void *);
uint4  *scc_compute(automaton *, uint4 *);

#endif  /* LASH_GRAPH_SCC_H */

/****  End of graph-scc.h ****/
