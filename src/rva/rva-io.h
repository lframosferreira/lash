/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**     rva-io.h  :  I/O operations over RVAs.                     **/
/**                                                                **/
/**     08/06/01  :  Creation. (SJ)                                **/
/**     07/10/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_RVA_IO_H
#define LASH_RVA_IO_H

#include "datastruct-io.h"

/** RVA related tags (must be positive and <= 0xFFU) **/

#define IO_TAG__RVA                            0x30

#define IO_TAG__RVA_HEADER                     0x10
#define IO_TAG__RVA_HEADER_DIM                 0x00
#define IO_TAG__RVA_HEADER_BASE                0x01
#define IO_TAG__RVA_HEADER_UNIVERSAL           0x02

#define IO_TAG__RVA_BODY                       0x20
#define IO_TAG__RVA_BODY_AUTOMATON             0x00

#define IO_TAG__RVA_INFO                       0x30
#define IO_TAG__RVA_INFO_PROPERTIES            0x00
#define IO_TAG__RVA_INFO_PROPERTIES_SERIAL     0x00
#define IO_TAG__RVA_INFO_PROPERTIES_RESTRICTED 0x01
#define IO_TAG__RVA_INFO_LABELS                0x01

#endif  /* LASH_RVA_IO_H */

/****  End of rva-io.h  ****/
