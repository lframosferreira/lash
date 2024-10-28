/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-product.h  :  Product-based operations over finite-    **/
/**                     state automata.                            **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        02/05/01  :  Product-based union. (SJ)                  **/
/**        05/01/01  :  Product with separator. (SJ)               **/
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

#ifndef LASH_AUTO_PRODUCT_H
#define LASH_AUTO_PRODUCT_H

#include "lash-auto-operations.h"

/**  AUTO_DEFAULT_PROD_HSIZE  :  Default size of the hash table used
                     by the product algorithms.                    **/
 
#define AUTO_DEFAULT_PROD_HSIZE    0x1000

/**  Shortcut.                                                     **/

#define auto_product(a1, a2) \
        auto_product_separ((a1), (a2), NULL, NULL)

/**  Function prototypes.                                          **/

void        auto_set_prod_hsize(uint4);

#if LASH_CHECK_LEVEL >= 1
uint8       auto_get_prod_ncolls(void);
void        auto_reset_prod_ncolls(void);
uint8       auto_get_prod_nins(void);
void        auto_reset_prod_nins(void);
#endif  /* >= 1 */

automaton  *auto_product_separ(automaton *, automaton *, uint1 *, 
			       uint1 *);
automaton  *auto_intersection(automaton *, automaton *);
int         auto_empty_intersection(automaton *, automaton *);
automaton  *auto_product_union(automaton *, automaton *);

#endif  /* LASH_AUTO_PRODUCT_H */

/****  End of auto-product.h  ****/
