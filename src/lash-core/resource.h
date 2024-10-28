/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  resource.h  :  Invisible definitions for resource             **/
/**                 management.                                    **/
/**                                                                **/
/**    03/16/98  :  Creation. (BB)                                 **/
/**    06/09/98  :  Minor corrections. (BB)                        **/
/**    07/28/98  :  Functions for handling the info type. (BB)     **/
/**    08/03/98  :  Continued. (BB)                                **/
/**    09/04/98  :  Reorganization. (BB)                           **/
/**    09/07/98  :  Minor corrections. (BB)                        **/
/**    09/30/98  :  Function 'sint4__free'. (BB)                   **/
/**    07/08/02  :  Reorganization. (BB)                           **/
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

#ifndef LASH_RESOURCE_H
#define LASH_RESOURCE_H

#include "lash-types.h"

/**  Prototypes of invisible functions.                            **/

uint1 *resr__malloc(uint4);
uint1 *resr__realloc(uint1 *, uint4, uint4);
void   resr__free(uint1 *, uint4);
uint1 *resr__realloc_info(info *, uint4, uint4);

void   uint4__free(uint4 *);
void   sint4__free(sint4 *);

/**  Macro resr__new_object(t)  :  Allocates the space for a new   **/
/**                  object of type t, and updates the statistics. **/
/**                  Returns a NULL pointer if there is not        **/
/**                  enough available memory, and a pointer to     **/
/**                  the newly allocated object otherwise.         **/

#define resr__new_object(t) (t *) resr__malloc(sizeof(t))

/**  Macro resr__new_objects(t, n)  :  Allocates the space for n   **/
/**                  new objects of type t, and updates the        **/
/**                  statistics. Returns a NULL pointer if there   **/
/**                  is not enough available memory, and a         **/
/**                  pointer to the newly allocated block          **/
/**                  otherwise.                                    **/

#define resr__new_objects(t, n) (t *) resr__malloc(sizeof(t) * (n))

/**  Macro resr__resize_objects(p, t, n, on)  :  Reallocates the   **/
/**                  memory block to which p points in order to    **/
/**                  accomodate n objects of type t. If p is NULL, **/
/**                  then a new memory block is allocated. This    **/
/**                  macro updates the statistics. It returns a    **/
/**                  NULL pointer if there is not enough           **/
/**                  available memory, and a pointer to the        **/
/**                  resized block otherwise. The argument on      **/
/**                  must contain the previous number of           **/
/**                  allocated objects.                            **/

#define resr__resize_objects(p, t, n, on) \
    (t *) resr__realloc((uint1 *) (p), sizeof(t) * (n), \
    sizeof(t) * (on))

/**  Macro resr__free_object(ptr, t)  :  Frees the space           **/
/**                  allocated for the object of type t to which   **/
/**                  ptr points. Update the statistics.            **/

#define resr__free_object(ptr, t) \
    resr__free((uint1 *) (ptr), sizeof(t))

/**  Macro resr__free_objects(ptr, t, n)  :  Frees the space       **/
/**                  allocated for an array of n objects of type   **/
/**                  t to which ptr points. Update the             **/
/**                  statistics.                                   **/

#define resr__free_objects(ptr, t, n) \
    resr__free((uint1 *) (ptr), sizeof(t) * (n))

/**  Macro resr__new_info_objects(p, t, n)  :  Allocates the       **/
/**                  space for n new objects of type t in the      **/
/**                  info structure given by *p, and updates the   **/
/**                  statistics. Returns a NULL pointer if there   **/
/**                  is not enough available memory, and a         **/
/**                  pointer to the newly allocated block          **/
/**                  otherwise.                                    **/

#define resr__new_info_objects(p, t, n) \
    ((t *) (sizeof(t) * (n) <= 4 ? (p) -> bytes \
    : ((p) -> ptr = (void *) resr__malloc(sizeof(t) * (n)))))

/**  Macro resr__free_info_objects(p, t, n)  :  Frees the space    **/
/**                  allocated for an array of n objects of type   **/
/**                  t contained in an info structure given by *p. **/
/**                  Updates the statistics.                       **/

#define resr__free_info_objects(p, t, n) \
    { if (sizeof(t) * (n) > 4) \
        resr__free((uint1 *) ((p) -> ptr), sizeof(t) * (n)); }

/**  Macro resr__resize_info_objects(p, t, n, m)  :  Resizes       **/
/**                  the array of m objects of type t contained    **/
/**                  in the info structure given by *p to the new  **/
/**                  size of n objects. Updates the statistics.    **/
/**                  Returns a pointer to the newly allocated      **/
/**                  array if successful, and NULL otherwise.      **/

#define resr__resize_info_objects(p, t, n, m) \
    ((t *) resr__realloc_info((p), sizeof(t) * (n), sizeof(t) * (m)))

#endif  /* LASH_RESOURCE_H */

/****  End of resource.h  ****/
