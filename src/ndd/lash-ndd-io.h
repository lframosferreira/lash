/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-ndd-io.h   :  Visible prototypes for performing  I/O     **/
/**                     operations on NDDs.                        **/
/**                                                                **/
/**        10/12/00  :  Creation. (JMF)                            **/
/**        12/02/01  :  Minor correction. (BB)                     **/
/**        07/10/02  :  Reorganization. (BB)                       **/
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

#ifndef LASH_LASH_NDD_IO_H
#define LASH_LASH_NDD_IO_H

#include "datastruct-io.h"
#include "lash-io.h"
#include "lash-ndd.h"

/**  Datatypes.                                                    **/

/**  Labels associated to each variable of a NDD.                  **/

typedef struct {
  char  **labels;
  uint4   n;
} ndd_labels;


/**  Function prototype.                                           **/

int  ndd_serialize_write_labeled(ndd *, ndd_labels *,
				  int (*)(uint4, void *, uint1 *),
				  void *);
ndd *ndd_serialize_read_labeled(int (*)(uint4, void *, uint1 *),
				 void *, ndd_labels **);
int  ndd_serialize_write_mem_labeled(ndd *, ndd_labels *,
				      mem_block *);
ndd *ndd_serialize_read_mem_labeled(mem_block *, ndd_labels **);
int  ndd_serialize_write_file_labeled(ndd *, ndd_labels *, char *);
ndd *ndd_serialize_read_file_labeled(char *, ndd_labels **);
int  ndd_serialize_remove_file(char *);

ndd_labels *ndd_labels_new(uint4);
void        ndd_labels_free(ndd_labels *);
int         ndd_labels_set(ndd_labels *, uint4, char *);


/**  Macros.                                                       **/

/**  The following macros directly maps their corresponding
     fonctions without the variable labelling facility.            **/

#define  ndd_serialize_write(n, f, p) \
     ndd_serialize_write_labeled(n, NULL, f, p)

#define  ndd_serialize_read(f, p) \
     ndd_serialize_read_labeled(f, p, NULL)

#define  ndd_serialize_write_mem(n, b) \
     ndd_serialize_write_mem_labeled(n, NULL, b)

#define  ndd_serialize_read_mem(b) \
     ndd_serialize_read_mem_labeled(b, NULL)

#define  ndd_serialize_write_file(n, f) \
     ndd_serialize_write_file_labeled(n, NULL, f)

#define  ndd_serialize_read_file(f) \
     ndd_serialize_read_file_labeled(f, NULL)


/**  Macro  ndd_label_element(nl, i)  :  Returns the i-th label
     of the label array given by nl.                               **/

#define  ndd_labels_get(nl, i)  ((nl -> labels)[i])
#define  ndd_labels_new_ndd(nd) ndd_labels_new(nd -> dim)

#endif  /* LASH_LASH_NDD_IO_H */

/****  End of lash-ndd-io.h  ****/
