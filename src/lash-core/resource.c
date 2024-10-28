/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  resource.c  :  Resource management.                           **/
/**                                                                **/
/**    03/16/98  :  Creation. (BB)                                 **/
/**    06/09/98  :  Minor corrections. (BB)                        **/
/**    08/04/98  :  Continued. (BB)                                **/
/**    09/04/98  :  Reorganization. (BB)                           **/
/**    09/07/98  :  Minor corrections. (BB)                        **/
/**    09/30/98  :  Function 'sint4__free'. (BB)                   **/
/**    01/14/99  :  Improved memory reallocation routine. (BB)     **/
/**    02/23/01  :  Added warning to realloc function doc. (BB)    **/
/**    07/02/02  :  Reorganization. (BB)                           **/
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

#include <stdlib.h>
#include <string.h>
#include "resource.h"
#include "diag.h"

/****  Public invisible functions.                               ****/

/**  uint1 *resr__malloc(size)  :  Allocates size bytes of memory
                     and updates statistics. Returns a NULL           
                     pointer if not enough memory is available,
                     or a pointer to the newly allocated block
                     otherwise.                                    **/

uint1 *resr__malloc(size)
  uint4 size;
{
  register uint1 *ptr;

  if (!(ptr = (uint1 *) malloc(size)))
    return NULL;

  diag__log_alloc((uint8) size);

  return ptr;
}

/** uint1 *resr__realloc(ptr, size, oldsize) : Reallocates the memory
                     block to which ptr points in order to accomodate
                     size bytes. If ptr is NULL, then a new memory
                     block is allocated. This function updates the
                     statistics. It returns a NULL pointer if there is
                     not enough available memory, and a pointer to the
                     resized block otherwise. The argument oldsize
                     must contain the size of the already allocated
                     block of memory.

                     WARNING: Calling this function in order to make
                     the size of an allocated block become equal to
                     zero is strongly discouraged, as it may lead to
                     an unpredictable behavior on some systems (due
                     to differences in implementations of 'realloc').
                                                                   **/

uint1 *resr__realloc(ptr, size, oldsize)
  uint1 *ptr;
  uint4  size, oldsize;
{
  register uint1 *r;

  if (ptr)
    {
      if (!(r = (uint1 *) realloc(ptr, size)))
	{
	  if (size > oldsize)
	    return NULL;
	}
      else
	ptr = r;

#if LASH_CHECK_LEVEL >= 1

      if (size >= oldsize)  
        diag__log_alloc(size - oldsize);
      else
        diag__log_unalloc(oldsize - size);

#endif

      return ptr;
    }
  else
    return resr__malloc(size);
}

/**  void  resr__free(ptr, size)  :  Frees the block of size bytes
                     of memory pointed to by ptr, and updates 
                     statistics.                                   **/

void  resr__free(ptr, size)
  uint1 *ptr;
  uint4  size;
{
#if LASH_CHECK_LEVEL < 1
  free((char *) ptr);
#else
  if (ptr)  
    {
      free((char *) ptr);
      diag__log_unalloc((uint8) size);
    }
#endif
}

/** uint1 *resr__realloc_info(ptr, size, oldsize) : Reallocates the
                     memory block contained in the info structure to
                     which ptr points in order to accomodate size
                     bytes. This function updates the statistics. It
                     returns a NULL pointer if there is not enough
                     available memory, and a pointer to the resized
                     block otherwise. The argument oldsize must
                     contain the size of the already allocated block
                     of memory.                                    **/

uint1 *resr__realloc_info(ptr, size, oldsize)
  info *ptr;
  uint4 size, oldsize;  
{
  static   uint1  tmp[4];
  register uint1 *r;

  if (size <= 4)
    {
      if (oldsize <= 4)
	return ptr -> bytes;
      else
	{
	  memcpy(tmp, (uint1 *) (ptr -> ptr), size);
	  resr__free((uint1 *) ptr -> ptr, oldsize);
	  memcpy(ptr -> bytes, tmp, size);
	  return ptr -> bytes;
	}
    }
  else
    if (oldsize <= 4)
      {
        r = resr__malloc(size);
        if (!r)
	  return NULL;
	memcpy(r, ptr -> bytes, oldsize);
        ptr -> ptr = r;
	return r;
      }
    else
      {
	r = resr__realloc((uint1 *) (ptr -> ptr), size, oldsize);
        if (!r)
	  return NULL;      
        ptr -> ptr = r;
	return r;
      }
}

/**  void uint4__free(p)  :  Frees the uint4 number *p. The primary
                     purpose of this function is to be an argument
                     of a function freeing a more complex data
                     structure.                                    **/

void uint4__free(p)
  uint4 *p;
{
  if (p)
    resr__free_object(p, uint4);
}

/**  void sint4__free(p)  :  Frees the sint4 number *p. The primary
                     purpose of this function is to be an argument
                     of a function freeing a more complex data
                     structure.                                    **/

void sint4__free(p)
  sint4 *p;
{
  if (p)
    resr__free_object(p, sint4);
}

/****  End of resource.c  ****/
