/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  datastruct-io.c  :  Data structures specific to I/O           **/
/**                      operations.                               **/
/**                                                                **/
/**         02/12/01  :  Creation. (JMF+BB)                        **/
/**         07/02/02  :  Reorganization. (BB)                      **/
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

#include <stdio.h>
#include <string.h>
#include "lash-io.h"
#include "diag.h"
#include "resource.h"

/****  Public visible function(s).                               ****/

/**  mem_block *mem_block__new()  :  Allocates a new block of memory
                     of size zero.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
		         LASH_ERR_NO_MEM   : Not enough memory.    **/

mem_block *mem_block__new(void)
{
  mem_block *mb;
  
  diag__enter("mem_block__new", NULL);
  
  if (!(mb = resr__new_object(mem_block)))
    diag__fail(LASH_ERR_NO_MEM, NULL);
 
  mb -> beginning = NULL;
  mb -> size = ZERO_INT4;
  
  diag__return(mb);
}

/**  int  mem_block__free(mb)  :  Frees the block of memory *mb.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_CORRUPT  : Corrupt data.         **/

int  mem_block__free(mb)
     mem_block *mb;
{
  diag__enter("mem_block__free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!mb)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  resr__free(mb -> beginning, mb -> size);
  resr__free_object(mb, mem_block);

  diag__return(0);
}

/**  int  mem_block__content free(mb)  :  Frees the content of the
                     block of memory *mb.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_CORRUPT  : Corrupt data.         **/

int  mem_block__content_free(mb)
     mem_block *mb;
{
  diag__enter("mem_block__content_free", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!mb)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (mb -> beginning != NULL)
    resr__free(mb -> beginning, mb -> size);
  
  mb -> beginning = NULL;
  mb -> size = ZERO_INT4;
 
  diag__return(0);
}

/**  int  mem_block__content set(mb, p, l)  :  Sets the content of
                     the block of memory *mb.  After the call to
		     this function, the content of the memory block
		     is the same as the content of the memory location
		     pointed by p and has a length of l bytes.  The
		     previous content is deleted.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.
		     
                     Possible error codes:

                         LASH_ERR_NOT_INIT : Not initialized.
			 LASH_ERR_NO_MEM   : Not enough memory.
			 LASH_ERR_CORRUPT  : Corrupt data.         **/

int  mem_block__content_set(mb, p, l)
     mem_block *mb;
     uint1     *p;
     uint4      l;
{
  diag__enter("mem_block__content_set", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!mb || !p)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  if (mem_block__content_free(mb))
    diag__fail(lash_errno, -1);
  
  if (!(mb -> beginning = resr__malloc(mb -> size = l)))
    {
      mb -> beginning = NULL;
      mb -> size = ZERO_INT4;
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  memcpy(mb -> beginning, p, l);

  diag__return(0);
}

/****  End of datastruct-io.c  ****/
