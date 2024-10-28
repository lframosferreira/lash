/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-rva-io.h   :  Visible prototypes for performing I/O      **/
/**                     operations on RVAs.                        **/
/**                                                                **/
/**        08/06/01  :  Creation. (SJ)                             **/
/**        08/14/01  :  Small adaptations. (BB)                    **/
/**        08/20/01  :  Small modification. (SJ)                   **/
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

#ifndef LASH_LASH_RVA_IO_H
#define LASH_LASH_RVA_IO_H

#include "datastruct-io.h"
#include "lash-io.h"
#include "lash-rva.h"
#include "rva-io-print.h"

/**  Datatypes.                                                    **/

/**  Labels associated to each variable of a RVA.                  **/

typedef struct {
  char  **labels;
  uint4   n;
} rva_labels;


/**  Function prototypes.                                          **/

int  rva_serialize_write_labeled(rva *, rva_labels *,
				  int (*)(uint4, void *, uint1 *),
				  void *);
rva *rva_serialize_read_labeled(int (*)(uint4, void *, uint1 *),
				 void *, rva_labels **);
int  rva_serialize_write_mem_labeled(rva *, rva_labels *,
				      mem_block *);
rva *rva_serialize_read_mem_labeled(mem_block *, rva_labels **);
int  rva_serialize_write_file_labeled(rva *, rva_labels *, char *);
rva *rva_serialize_read_file_labeled(char *, rva_labels **);
int  rva_serialize_remove_file(char *);

rva_labels *rva_labels_new(uint4);
void        rva_labels_free(rva_labels *);
int         rva_labels_set(rva_labels *, uint4, char *);


/**  Macros.                                                       **/

/**  The following macros directly map onto their corresponding
     functions without the variable labeling facility.             **/

#define  rva_serialize_write(r, f, p) \
     rva_serialize_write_labeled(r, NULL, f, p)

#define  rva_serialize_read(f, p) \
     rva_serialize_read_labeled(f, p, NULL)

#define  rva_serialize_write_mem(r, b) \
     rva_serialize_write_mem_labeled(r, NULL, b)

#define  rva_serialize_read_mem(b) \
     rva_serialize_read_mem_labeled(b, NULL)

#define  rva_serialize_write_file(r, f) \
     rva_serialize_write_file_labeled(r, NULL, f)

#define  rva_serialize_read_file(f) \
     rva_serialize_read_file_labeled(f, NULL)


/**  Macro  rva_label_element(nl, i)  :  Returns the i-th label
     of the label array given by nl.                               **/

#define  rva_labels_get(nl, i)  ((nl -> labels)[i])
#define  rva_labels_new_rva(nd) rva_labels_new(nd -> dim)

#endif  /* LASH_LASH_RVA_IO_H */

/****  End of lash-rva-io.h  ****/
