/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       ndd.h  :  Manipulation of Number Decision Diagrams.      **/
/**                                                                **/
/**    09/22/98  :  Creation. (BB)                                 **/
/**    09/28/98  :  Reorganization. (BB)                           **/
/**    07/09/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_NDD_H
#define LASH_NDD_H

#include "lash-ndd.h"
#include "auto.h"

/**  Flags for ndd.properties (or-able).                           **/

#define NDD_PROP_NOTHING     0x00  /*  No known property.           */
#define NDD_PROP_SERIAL      0x01  /*  Bits related to the          */
				   /*  different vector components  */
				   /*  are read one by one rather   */
				   /*  than as a tuple.             */
#define NDD_PROP_MSDF        0x02  /*  Most significant digit is    */
				   /*  read first.                  */

#endif  /* LASH_NDD_H */

/****  End of ndd.h  ****/
