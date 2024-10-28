/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  auto-io-dots.h  :  Exports LASH automata in order to read     **/
/**                     them with the Graphviz package (versions   **/
/**                     1.5 and above).                            **/
/**                                                                **/
/**        08/05/00  :  Creation. (SJ)                             **/
/**        10/18/00  :  Inclusion in the LASH package. (JMF)       **/
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
/**  Graphviz is the Graph Drawing Programs from AT&T Research     **/
/**  and Lucent Bell Labs. You can find informations, sources and  **/
/**  binaries at the following URL :                               **/
/**                                                                **/
/**    http://www.research.att.com/sw/tools/graphviz               **/
/**                                                                **/
/********************************************************************/

#ifndef LASH_AUTO_IO_DOTS_H
#define LASH_AUTO_IO_DOTS_H

#include "lash-auto.h"

/**  Type definitions.                                             **/

typedef enum {
  LASH_EXP_ASIS, LASH_EXP_ALPHA, LASH_EXP_DIGIT
} io_dots_param;

/****  Public visible function(s).                               ****/

int  auto_serialize_write_dot_file(automaton *, char *,
				   io_dots_param);

#endif  /* LASH_AUTO_IO_DOTS_H */

/****  End of auto-io-dots.h  ****/
