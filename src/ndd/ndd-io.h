/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**     ndd-io.h  :  I/O operations over NDDs.                     **/
/**                                                                **/
/**     10/17/00  :  Creation. (JMF)                               **/
/**     02/12/01  :  Minor correction. (BB)                        **/
/**     07/09/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_NDD_IO_H
#define LASH_NDD_IO_H

#include "datastruct-io.h"

/** NDD related tags (must be positive and <= 0xFFU) **/

#define IO_TAG__NDD                         0x20

#define IO_TAG__NDD_HEADER                  0x10
#define IO_TAG__NDD_HEADER_DIM              0x00
#define IO_TAG__NDD_HEADER_BASE             0x01

#define IO_TAG__NDD_BODY                    0x20
#define IO_TAG__NDD_BODY_AUTOMATON          0x00

#define IO_TAG__NDD_INFO                    0x30
#define IO_TAG__NDD_INFO_PROPERTIES         0x00
#define IO_TAG__NDD_INFO_PROPERTIES_SERIAL  0x00
#define IO_TAG__NDD_INFO_PROPERTIES_MSDF    0x01
#define IO_TAG__NDD_INFO_LABELS             0x01

#endif  /* LASH_NDD_IO_H */

/****  End of ndd-io.h  ****/
