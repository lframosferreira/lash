/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-types.h  :  Visible multipurpose type definitions.       **/
/**                                                                **/
/**      03/11/98  :  Creation. (BB)                               **/
/**      06/09/98  :  Minor corrections. (BB)                      **/
/**      07/24/98  :  Accessor for the info datatype. (BB)         **/
/**      09/04/98  :  Reorganization. (BB)                         **/
/**      09/29/98  :  Signed integer types. (BB)                   **/
/**      02/24/01  :  Integer pointer type. (BB)                   **/
/**      02/28/01  :  Most significant bit mask. (SJ)              **/
/**      07/08/02  :  Reorganization. (BB)                         **/
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

#ifndef LASH_LASH_TYPES_H
#define LASH_LASH_TYPES_H

#include "lash.h"

/**  Integer data types. These type definitions are system-        **/
/**  dependent and must be updated if the package is moved         **/
/**  to a new architecture.                                        **/

typedef unsigned char uint1; /*  0x00 ... 0xff.               */
typedef unsigned int uint4;  /*  0x00000000 ... 0xffffffff.   */
typedef unsigned long long   /*  0x0000000000000000 ...       */
    uint8;                   /*         0xffffffffffffffff.   */

typedef signed char sint1; /*  -0x80 ... 0x7f.              */
typedef signed int sint4;  /*  -0x80000000 ... 0x7fffffff.  */
typedef signed long long   /*  -0x8000000000000000 ...      */
    sint8;                 /*         0x7fffffffffffffff.   */

typedef uint8 uintptr; /*  Same size as memory pointer. */

/**  Integer constants. These definitions are system-dependent     **/
/**  and must be updated if the package is moved to a new          **/
/**  architecture.                                                 **/

#define ZERO_INT1 0
#define ZERO_INT4 0
#define ZERO_INT8 0LL

/**  Integer print formats. These definitions are system-          **/
/**  dependent and must be updated if the package is moved to a    **/
/**  new architecture.                                             **/

#define FMT_UINT1 "%u"
#define FMT_UINT4 "%u"
#define FMT_UINT8 "%llu"

#define FMT_SINT1 "%d"
#define FMT_SINT4 "%d"
#define FMT_SINT8 "%lld"

/**  Masks for the most significant bit of integer data types.     **/

#define MSB_MASK_UINT1 (uint1)(1 << 7)
#define MSB_MASK_UINT4 (uint4)(1 << 31)
#define MSB_MASK_UINT8 (uint8)(1 << 63)

/**  Multipurpose data structure used for storing either 4 bytes   **/
/**  of information, or a pointer.                                 **/

typedef union {
  uint4 val;
  void *ptr;
  uint1 bytes[4];
} info;

/**  Accessor for info type. When provided with a pointer to the   **/
/**  structure and the length of the stored information, returns   **/
/**  a pointer to that information.                                **/

#define info_ptr(p, l) ((l) <= 4 ? (p)->bytes : (p)->ptr)

#endif /* LASH_LASH_TYPES_H */

/****  End of lash-types.h  ****/
