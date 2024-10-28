/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    rva-shifting.h  :  Shifting of sets represented as RVAs.    **/
/**                                                                **/
/**    11/02/01  :  Creation. (SJ)                                 **/
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

#ifndef LASH_RVA_SHIFTING_H
#define LASH_RVA_SHIFTING_H

#include "rva.h"

/**  RVA_DEFAULT_SHIFT_HSIZE  :  Default size of the hash table used
                     by the shifting algorithms.                   **/

#define RVA_DEFAULT_SHIFT_HSIZE    0x1000

/**  Function prototypes.                                          **/

#if LASH_CHECK_LEVEL >= 1
uint8  rva_get_shift_ncolls(void);
void   rva_reset_shift_ncolls(void);
uint8  rva_get_shift_nins(void);
void   rva_reset_shift_nins(void);
#endif  /* >= 1 */

rva   *rva_shift_left(rva *, uint4);
rva   *rva_shift_right(rva *, uint4);
rva   *rva_shift(rva *, sint4);

#endif  /* LASH_RVA_SHIFTING_H */

/****  End of rva-shifting.h  ****/
