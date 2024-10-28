/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-sequential.h  :  Sequential operations over finite-    **/
/**                     state automata.                            **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        03/21/01  :  Operations with separator. (SJ)            **/
/**        04/17/02  :  'auto_seq_multi_projection' added.(SJ)     **/
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

#ifndef LASH_AUTO_SEQUENTIAL_H
#define LASH_AUTO_SEQUENTIAL_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_SEQ_PROD_HSIZE  :  Default size of the hash table
                     used by the sequential product algorithm.     **/
 
#define AUTO_DEFAULT_SEQ_PROD_HSIZE    0x1000

/**  AUTO_DEFAULT_SEQ_PROJ_HSIZE : Default size of the hash table
                     used by the sequential projection algorithm.  **/

#define AUTO_DEFAULT_SEQ_PROJ_HSIZE    0x1000

/**  Shortcuts.                                                    **/

#define auto_seq_product(a1, a2, p, u) \
        auto_seq_product_separ((a1), (a2), (p), (u), NULL)

#define auto_seq_projection(a, p, u) \
        auto_seq_projection_separ((a), (p), (u), NULL)

#define auto_seq_multi_projection(a, p, var) \
        auto_seq_projection_separ((a), (p), (var), NULL)

/**  Function prototypes.                                          **/

void        auto_set_seq_prod_hsize(uint4);
void        auto_set_seq_proj_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1

uint8       auto_get_seq_prod_ncolls(void);
void        auto_reset_seq_prod_ncolls(void);
uint8       auto_get_seq_prod_nins(void);
void        auto_reset_seq_prod_nins(void);

uint8       auto_get_seq_proj_ncolls(void);
void        auto_reset_seq_proj_ncolls(void);
uint8       auto_get_seq_proj_nins(void);
void        auto_reset_seq_proj_nins(void);

#endif  /* >= 1 */

automaton  *auto_seq_product_separ(automaton *, automaton *,
                      uint4, uint4, uint1 *);
automaton  *auto_seq_projection_separ(automaton *, uint4,
                      uint4, uint1 *);
automaton  *auto_seq_multi_projection_separ(automaton *, uint4,
                      int *, uint1 *);

#endif  /* LASH_AUTO_SEQUENTIAL_H */

/****  End of auto-sequential.h  ****/
