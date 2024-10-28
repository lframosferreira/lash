/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    datastruct.h  :  General-purpose data structures.           **/
/**                                                                **/
/**        08/03/98  :  Creation. (BB)                             **/
/**        08/21/98  :  Continued. (BB)                            **/
/**        08/26/98  :  Added "info__cpy". (BB)                    **/
/**        09/04/98  :  Reorganization.  (BB)                      **/
/**        09/07/98  :  Minor modifications. (BB)                  **/
/**        09/15/98  :  Stack datatype. (BB)                       **/
/**        10/23/98  :  Improved set operations. (BB)              **/
/**        10/26/98  :  Continued. (BB)                            **/
/**        03/15/99  :  New set function.  (GC+BB)                 **/
/**        05/14/99  :  New stack functions and macro. (BB)        **/
/**        05/25/99  :  New hash functions. (BB)                   **/
/**        03/24/00  :  New bit table function. (JMF)              **/
/**        02/12/01  :  Reorganization. (GC+JMF+BB)                **/
/**        06/19/01  :  New type: queue (LL)                       **/
/**        08/09/01  :  New bit table function. (SJ)               **/
/**        01/24/02  :  New set function. (LL)                     **/
/**        07/29/02  :  Reorganization. (BB)                       **/
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

#ifndef LASH_DATASTRUCT_H
#define LASH_DATASTRUCT_H

#include <string.h>
#include "lash-types.h"

/**  Set of 4-byte integers.                                       **/

typedef struct {
  uint4  nb, al;
  uint4 *ptr;
  uint1  sorted;
} uint4_set;

/**  Size in bytes of the allocated blocks of memory for sets.     **/

#define  SET_GROWTH_QUANTUM  0x40

/**  Hash table type definitions.                                  **/

typedef struct _hash_entry{
  struct _hash_entry *next;
  void               *key, *payload; 
} hash_entry;

typedef struct {
  uint4        size;
  hash_entry **table;
} hash_table;

/**  Bit table.                                                    **/

typedef struct {
  uint4  nbytes;
  uint1 *bits;
} bit_table;

/**  Stack.                                                        **/

typedef struct {
  uint4  element_nbytes, alloc_nbytes, nb_elements;
  uint1 *base;
} stack;

/**  Fifo.                                                         **/
 
typedef struct {
  uint4  element_nbytes, alloc_nbytes, nb_elements;
  uint4  front, rear;
  uint1 *base;
} fifo;

/**  Set of objects.                                               **/

typedef struct {
  uint4  object_nbytes, alloc_nbytes, nb_elements;
  uint1 *base;
  int  (*object_cmp) (const void *, const void *);
  uint1  sorted;
} object_set;

/*  object_cmp is a function that must return a negative value if
    arg 1 is considered to precede arg 2, zero if arg 1 is
    equivalent to arg 2 and a positive value if arg 1 is considered
    to follow arg 2.                                                */

/**  Size in bytes of the allocated blocks of memory for stacks.   **/

#define  STACK_GROWTH_QUANTUM       0x200

/** Size in bytes of the allocated blocks of memory                **/
/** for object sets.                                               **/

#define  OBJECT_SET_GROWTH_QUANTUM  0x200

/**  Size in bytes of the allocated blocks of memory for fifos.    **/

#define  FIFO_GROWTH_QUANTUM       0x200

/**  Maximum number of removed elements in fifos before freeing
     the corresponding memory.                                     **/

#define  FIFO_SHIFT      0x200

/**  Function prototypes.                                          **/

int         uint4_cmp(uint4 *, uint4 *);
void        info__link_ptr(info *, uint1 *, uint4);
void        bytes__prepare_free(uint4);
void        bytes__free(uint1 *);

uint4_set  *set__new_empty(void);
void        set__new_empty_content(uint4_set *);
void        set__free(uint4_set *);
void        set__free_content(uint4_set *);
uint4       set__nb_elements(uint4_set *);
int         set__add(uint4_set *, uint4);
uint4       set__element(uint4_set *, uint4);
int         set__member(uint4_set *, uint4);
void        set__remove(uint4_set *, uint4);
uint4_set  *set__new_copy(uint4_set *);
int         set__equal(uint4_set *, uint4_set *);
void        set__replace(uint4_set *, uint4_set *);

hash_table *hash__new_empty(uint4);
void        hash__free(hash_table *, void (*)(void *), 
                void (*)(void *));
void       *hash__lookup_set(hash_table *, uint4_set *);
void       *hash__lookup_bytes(hash_table *, uint1 *, uint4);
void       *hash__lookup_string(hash_table *, char *);

#if LASH_CHECK_LEVEL >= 1
int         hash__insert_set(hash_table *, uint4_set *, void ***,
                uint8 *, uint8 *);
int         hash__insert_bytes(hash_table *, uint1 *, uint4,
                void ***, uint8 *, uint8 *);
int         hash__insert_string(hash_table *, char *, void ***,
                uint8 *, uint8 *);
#else
int         hash__insert_set(hash_table *, uint4_set *, void ***);
int         hash__insert_bytes(hash_table *, uint1 *, uint4,
                void ***);
int         hash__insert_string(hash_table *, char *, void ***);
#endif  /* >= 1 */

bit_table  *bit__new_empty(uint4);
void        bit__free(bit_table *);
bit_table  *bit__new_copy(bit_table *);
void        bit__add(bit_table *, uint4);
void        bit__remove(bit_table *, uint4);
int         bit__member(bit_table *, uint4);
void        bit__empty_content(bit_table *);
uint4       bit__nb_storable(bit_table *);

stack      *stack__new_empty_from_size(uint4);
int         stack__push(stack *, void *);
void        stack__pop(stack *, void *);
void       *stack__top(stack *);
void       *stack__pick(stack *, uint4);
void        stack__reduce_fn(stack *, uint4, void (*)(void *));
void        stack__free(stack *);
void        stack__free_fn(stack *, void (*)(void *));

fifo       *fifo__new_empty_from_size(uint4);
int         fifo__add(fifo *, void *);
void        fifo__remove(fifo *, void *);
void       *fifo__front(fifo *);
void       *fifo__pick(fifo *, uint4);
void        fifo__free(fifo *);

object_set *object_set__new_empty_from_size(uint4, int (*) 
					    (const void *,
					     const void *));
void        object_set__free(object_set *);
uint4       object_set__nb_elements(object_set *);
int         object_set__add(object_set *, void *);
uint1      *object_set__element(object_set *, uint4);
int         object_set__member(object_set *, void *);
void        object_set__sort(object_set *);

/**  Macro  info__cpy(id, is, n)  :  Copies the content of the info 
                     structure *is to the content of the info 
                     structure *id. The number of bytes is n.      **/

#define  info__cpy(id, is, n)  (memcpy(info_ptr((id), (n)), \
    info_ptr((is), (n)), (n)))

/**  Macro  stack__new_empty(t)  :  Returns a pointer to a new empty
                     stack that can be used for storing elements of
                     type t, or returns a NULL pointer if there is
                     not enough memory.                            **/

#define  stack__new_empty(t)  stack__new_empty_from_size(sizeof(t))

/**  Macro  stack__is_empty(st)  :  Returns 1 if the stack *st is
                     empty, and 0 otherwise.                       **/

#define  stack__is_empty(st)  (!((st) -> nb_elements))

/**  Macro  stack__size(st)  :  Returns the number of elements in the
                     stack *st.                                    **/

#define  stack__size(st)  ((st) -> nb_elements)
 
/**  Macro  fifo__new_empty(t)  :  Returns a pointer to a new 
                     empty fifo that can be used for storing elements
		     of type t, or returns a NULL pointer if there is
		     not enough memory.                            **/

#define  fifo__new_empty(t)  fifo__new_empty_from_size(sizeof(t))

/**  Macro  fifo__is_empty(q)  :  Returns 1 if the queue *q is
                     empty, and 0 otherwise.                       **/

#define  fifo__is_empty(q)  (!((q) -> nb_elements))

/**  Macro  fifo__size(st)  :    Returns the number of elements
                     in the queue *q.                              **/

#define  fifo__size(q)  ((q) -> nb_elements)

/**  Macro  object_set__new_empty(t, cmp)  :  Returns a pointer to a
                     new empty object_set that can be used for storing
		     elements of type t, or returns a NULL pointer if
		     there is not enough memory. cmp is the function
		     used to compare two objects of type t.        **/

#define  object_set__new_empty(t, cmp) \
     object_set__new_empty_from_size(sizeof(t),(cmp))

/**  Macro  stack__nb_elements(st):  Returns the number of elements
                     in the stack *st                              **/

#define  stack__nb_elements(st)  ((st) -> nb_elements)

/**  Macro  stack__element(st, n):  Returns a pointer to the n th
                     (n = 0, 1, ...) element of the stack *st      **/

#define  stack__element(st, n)  \
      (((st) -> base) + (n) * ((st) -> element_nbytes))

#endif  /* LASH_DATASTRUCT_H */

/****  End of datastruct.h  ****/
