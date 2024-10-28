/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-serialize.h  :  Serialization and unserialization of   **/
/**                         automata (NDD and RVA).                **/
/**                                                                **/
/**        09/29/00  :  Creation. (SJ)                             **/
/**        04/12/01  :  Reorganization. (SJ)                       **/
/**        04/30/01  :  Integration in the LASH package. (SJ)      **/
/**        09/26/01  :  Serialization of automata. (SJ)            **/
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

#ifndef LASH_AUTO_SERIALIZE_H
#define LASH_AUTO_SERIALIZE_H

#include "lash-auto-operations.h"

/**  Function prototypes.                                          **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_unser_ncolls(void);
void   auto_reset_unser_ncolls(void);
uint8  auto_get_unser_nins(void);
void   auto_reset_unser_nins(void);
#endif  /* >= 1 */

automaton *auto_serialize(automaton *, uint1 *);
automaton *auto_unserialize(automaton *, uint4, uint1 *);

#endif  /* LASH_AUTO_SERIALIZE_H */

/****  End of auto-serialize.h  ****/
