/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-auto-io.h  :  Visible prototypes for performing I/O      **/
/**                     operations on finite-state automata.       **/
/**                                                                **/
/**        10/04/00  :  Creation. (JMF)                            **/
/**        10/24/00  :  Reorganisation. (JMF)                      **/
/**        02/07/01  :  Minor correction. (BB)                     **/
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

#ifndef LASH_LASH_AUTO_IO_H
#define LASH_LASH_AUTO_IO_H

#include "lash-auto.h"
#include "lash-io.h"

/**  Function prototypes.                                          **/

int        auto_serialize_write(automaton *,
				int (*)(uint4, void *, uint1 *),
				void *);
automaton *auto_serialize_read(int (*)(uint4, void *, uint1 *),
			       void *);
int        auto_serialize_write_mem(automaton *, mem_block *);
automaton *auto_serialize_read_mem(mem_block *);
int        auto_serialize_write_file(automaton *, char *);
automaton *auto_serialize_read_file(char *);

#include "auto-io-print.h"
#include "auto-io-dots.h"

#endif  /* LASH_LASH_AUTO_IO_H */

/****  End of lash-auto-io.h  ****/
