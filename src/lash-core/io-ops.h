/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       io-ops.h  :  Low-level functions for writing/reading.    **/
/**                                                                **/
/**       07/05/00  :  Creation. (JMF)                             **/
/**       07/08/02  :  Reorganization. (BB)                        **/
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

#ifndef LASH_IO_OPS_H
#define LASH_IO_OPS_H

#include <stdio.h>
#include "lash-auto.h"
#include "lash-types.h"
#include "lash-auto-io.h"


/**  Size in bytes of the allocated blocks of memory.              **/

#define  MEM_GROWTH_QUANTUM  0x40


/**  Type definitions.                                             **/

typedef struct {
  uint1  *beginning;
  uint4   where;
  uint4   alloc_nbytes;
} mem_descr;


/**  Constants definitions.                                        **/

#define  IO_SET_PROP_NOTHING       (0)
#define  IO_SET_PROP_MANDATORY     (1 << 0)
#define  IO_SET_PROP_UNIQUE        (1 << 1)


/**  Function prototypes.                                          **/

mem_descr *io__to_mem_init(void);
int        io__to_mem(uint4, mem_descr *, uint1 *);
void       io__to_mem_end(mem_descr *, uint1 **, uint4 *);
mem_descr *io__from_mem_init(uint1 *, uint4);
int        io__from_mem(uint4, mem_descr *, uint1 *);
void       io__from_mem_end(mem_descr *);
FILE      *io__to_file_init(char *);
int        io__to_file(uint4, FILE *, uint1 *);
int        io__to_file_end(FILE *);
FILE      *io__from_file_init(char *);
int        io__from_file(uint4, FILE *, uint1 *);
int        io__from_file_end(FILE *);

int        io__w_tag(int (*)(uint4, void *, uint1 *),
		     void *, uint1);
int        io__w_tagval(int (*)(uint4, void *, uint1 *),
			void *, uint4, uint1 *);
int        io__w_tagval1(int (*)(uint4, void *, uint1 *),
			 void *, uint1);
int        io__w_num(int (*)(uint4, void *, uint1 *),
		     void *, uint4);
int        io__w_snum(int (*)(uint4, void *, uint1 *),
		      void *, sint4);
int        io__w_char(int (*)(uint4, void *, uint1 *),
		      void *, char);
int        io__w_text(int (*)(uint4, void *, uint1 *),
		      void *, char *);
int        io__w_bytes(int (*)(uint4, void *, uint1 *),
		       void *, uint4, uint1 *);
int        io__r_tag(int (*)(uint4, void *, uint1 *),
		     void *, uint1 *);
int        io__r_text(int (*)(uint4, void *, uint1 *),
		      void *, uint4, char **);
int        io__r_bytes(int (*)(uint4, void *, uint1 *),
		       void *, uint4, uint1 *);
int        io__r_num(int (*)(uint4, void *, uint1 *),
		     void *, uint4 *);
int        io__r_snum(int (*)(uint4, void *, uint1 *),
		      void *, sint4 *);
int        io__r_set(void *, int  (*f)(uint4, void *, uint1 *),
		     void *, uint1, ...);

/*  
    TODO:
    int  io__w_bits(int (*)(uint4, void *, uint1 *),
    void *, uint4, uint1 *);
    int  io__w_bits_flush(int (*)(uint4, void *, uint1 *),
    void *);
*/


/** General purpose tags (must be positive and <= 0xFFU) **/

#define IO_TAG__END                 0xFF

#endif  /* LASH_IO_OPS_H */

/****  End of io-ops.h  ****/
