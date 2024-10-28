/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-io.h  :  I/O operations over finite-state automata.    **/
/**                                                                **/
/**     10/17/00  :  Creation. (JMF)                               **/
/**     08/02/01  :  Support for automata on infinite words. (SJ)  **/
/**     07/08/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_AUTO_IO_H
#define LASH_AUTO_IO_H

/** Automaton related tags (must be positive and <= 0xFFU) **/

#define IO_TAG__AUTOMATON                          0x10

#define IO_TAG__AUTO_HEADER                        0x10
#define IO_TAG__AUTO_HEADER_WORDTYPE               0x00
#define IO_TAGVAL__AUTO_HEADER_WORDTYPE_FINITE     0x00
#define IO_TAGVAL__AUTO_HEADER_WORDTYPE_INFINITE   0x01
#define IO_TAG__AUTO_HEADER_ACCEPTTYPE             0x01
#define IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_BUCHI    0x00
#define IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_COBUCHI  0x01
#define IO_TAGVAL__AUTO_HEADER_ACCEPTTYPE_WEAK     0x02
#define IO_TAG__AUTO_HEADER_NBITS                  0x02
#define IO_TAG__AUTO_HEADER_NSTATES                0x03

#define IO_TAG__AUTO_BODY                          0x20
#define IO_TAG__AUTO_BODY_I_STATES                 0x00
#define IO_TAG__AUTO_BODY_F_STATES                 0x01
#define IO_TAG__AUTO_BODY_TRAN                     0x02
#define IO_TAGVAL__AUTO_BODY_TRAN_ENC_TYPE_BASE    0x01

#define IO_TAG__AUTO_INFO                          0x30
#define IO_TAG__AUTO_INFO_PROPERTIES               0x00
#define IO_TAG__AUTO_INFO_PROPERTIES_DETERM        0x00
#define IO_TAG__AUTO_INFO_PROPERTIES_MINIMAL       0x01
#define IO_TAG__AUTO_INFO_PROPERTIES_STRONG        0x02
#define IO_TAG__AUTO_INFO_PROPERTIES_NORMAL        0x03
#define IO_TAG__AUTO_INFO_PROPERTIES_WEAK_NORMAL   0x04

#endif  /* LASH_AUTO_IO_H */

/****  End of auto-io.h  ****/
