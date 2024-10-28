/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       rva.h  :  Manipulation of Real Vector Automata.          **/
/**                                                                **/
/**    03/15/01  :  Creation. (SJ)                                 **/
/**    05/16/01  :  Flag for restricted RVA added. (SJ)            **/
/**    07/10/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_RVA_H
#define LASH_RVA_H

#include "lash-rva.h"
#include "auto.h"

/**  Flags for rva.properties (or-able).                           **/

#define RVA_PROP_NOTHING     0x00  /*  No known property.           */
#define RVA_PROP_SERIAL      0x01  /*  Bits related to the          */
				   /*  different vector components  */
				   /*  are read one by one rather   */
				   /*  than as a tuple.             */
#define RVA_PROP_RESTRICTED  0x02  /*  If this bit is set, the RVA  */
				   /*  is restricted to the linear  */
				   /*  arithmetic with real and     */
				   /*  integer values.              */

#endif  /* LASH_RVA_H */

/****  End of rva.h  ****/
