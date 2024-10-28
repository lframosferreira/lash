/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    datastruct.c  :  General-purpose data structures.           **/
/**                                                                **/
/**        08/03/98  :  Creation. (BB)                             **/
/**        08/27/98  :  Continued. (BB)                            **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/07/98  :  Minor corrections. (BB)                    **/
/**        09/15/98  :  Stack datatype. (BB)                       **/
/**        10/23/98  :  Improved set operations. (BB)              **/
/**        10/27/98  :  Minor correction. (BB)                     **/
/**        10/29/98  :  Improved set operations. (BB)              **/
/**        11/09/98  :  More efficient data structures. (BB)       **/
/**        02/11/99  :  Minor correction. (BB)                     **/
/**        03/10/99  :  Corrected bug in stack handler. (GC+BB)    **/
/**        03/15/99  :  New set function. (GC+BB)                  **/
/**        05/14/99  :  New stack functions. (BB)                  **/
/**        05/26/99  :  New hash functions. (BB)                   **/
/**        11/12/99  :  Improved hash functions. (BB)              **/
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "resource.h"
#include "sort.h"
#include "datastruct.h"

/****  Global variable.                                          ****/

static uint4  bytes_size = ZERO_INT4;

/****  Prototypes of private functions.                          ****/

static void   pack_set(uint4_set *);
static uint4  hash_set(uint4_set *, uint4);
static uint4  hash_bytes(uint1 *, uint4, uint4);
static uint4  hash_string(char *, uint4);
static void   pack_object_set(object_set *);

/****  Private functions.                                        ****/

/**  void  pack_set(s)  :  Sorts the elements of a set s in ascending
                     order, and eliminates duplicates.             **/

static void  pack_set(s)
  uint4_set *s;
{
  register uint4 *ps;
           uint4  n;  

#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  if (s -> sorted)
    return;

  n  = s -> nb;
  ps = s -> ptr;

  if (n > 1 && uint4__sort_and_pack(ps, &n) < 0)  
    {
      register uint4  n1, i, *pt;

      n1 = s -> nb;
      qsort((void *) ps, n1, sizeof(uint4), 
        (int (*) (const void *, const void *)) uint4_cmp);

      for (i = 0, pt = ps, n = 0; i < n1; i++, ps++)
	if (!i || (*ps != *(ps - 1)))
	  {
	    *(pt++) = *ps;
	    n++;
	  }
    }
    
  s -> nb = n;
  if (s -> al != n)
    {
      s -> ptr = resr__resize_objects(s -> ptr, uint4, n, s -> al);
      s -> al = n;
    }

  s -> sorted = 1;
}

/**  uint4  hash_set(s, m)  :  Hashes the set *s and returns a value
                     between 0 and m-1.                            **/

static uint4  hash_set(s, m)
  uint4_set *s;
  uint4      m;
{
  register uint4 i, n, v;

  pack_set(s);

  v = n = s -> nb;  
  v = (2654435769UL * v) >> 16;

  for (i = 0; i < n; i++)
    v = (v >> 31) + (v * 2U) + 
          ((2654435769UL * (s -> ptr[i])) >> 16);

  return (v % m);
}

/**  uint4  hash_bytes(p, n, m)  :  Hashes the sequence of n bytes
                     beginning at the address p, and returns a value
                     between 0 and m-1.                            **/

static uint4  hash_bytes(p, n, m)
  uint1 *p;
  uint4  n, m;
{
  register uint4 i, v;

  v = n;

  for (i = 0; i < n; i++)
    {
      v *= 0x4001;
      v = (v >> 31) + (v * 2);
      v = v + (uint4) p[i];
    }

  return (v % m);
}

/**  uint4  hash_string(s, m)  :  Hashes the null-terminated string
                     s, and returns a value between 0 and m-1.     **/

static uint4  hash_string(s, m)
  char  *s;
  uint4  m;
{
  return  hash_bytes((uint1 *) s, strlen(s) + 1, m);
}

/**  void  pack_object_set(s)  :  Sorts the elements of an object
                     set s in ascending order wrt the comparison
		     fonction given at the initialisation of *s , and
		     eliminates duplicates.                        **/

static void  pack_object_set(s)
  object_set *s;
{
   uint1 *ps;
   uint4  n;  

#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  if (s -> sorted)
    return;

  n  = s -> nb_elements;
  ps = s -> base;

  if (n > 1 && bytes__sort_and_pack(ps, &n, s -> object_nbytes,
				    s -> object_cmp, NULL))
    {
      register uint4  n1, i, nb;
      register uint1 *pt;
      int  (*cmp) (const void *, const void *);
      
      n1  = s -> nb_elements;
      nb  = s -> object_nbytes;
      cmp = s -> object_cmp;
      
      qsort(ps, n1, nb, cmp);

      for (i = 0, pt = ps, n = 1; i < n1; i++, ps += nb)
	if (i && cmp(ps - nb, ps))
	  {
	    pt += nb;
	    memcpy(pt, ps, nb);
	    n++;
	  }
    }
    
  s -> nb_elements = n;
  if (s -> alloc_nbytes != n * s -> object_nbytes)
    {
      s -> base =
	resr__resize_objects(s -> base, uint1,
			     n * s -> object_nbytes,
			     s -> alloc_nbytes);
      s -> alloc_nbytes = n * s -> object_nbytes;
    }

  s -> sorted = 1;
}

/****  Public functions.                                         ****/

/**  int  uint4_cmp(p1, p2)  :  Compares the two integers *p1 and
                     *p2. Returns a negative value if *p1 < *p2,
                     0 if *p1 = *p2, and a positive value if *p1 >
                     *p2.                                          **/

int  uint4_cmp(p1, p2)
  uint4 *p1, *p2;
{
  return (*p1) - (*p2);
}

/**  void  info__link_ptr(i, p, l)  :  Makes the content of the info
                     structure *i point to the array of l bytes
                     beginning with *p. The content of the array is
                     not copied, except if it fits entirely in the
                     fields of the info structure. The purpose of this
                     function is to provide a way of quickly referring
                     to an array by means of an info structure,
                     without copying the content of the
                     array. Warning: The previous content of the info
                     structure is lost but not freed!              **/

void  info__link_ptr(i, p, l)
  info  *i;
  uint1 *p;
  uint4  l;
{
  if (l <= 4)
    memcpy(i -> bytes, p, l);
  else
    i -> ptr = p;
}

/**  void  bytes__prepare_free(n)  :  Sets the global variable 
                     bytes_size to the value n. This means that all 
                     the subsequent calls to bytes__free will use n as
                     the current size of the array of bytes to be 
                     freed.                                        **/

void  bytes__prepare_free(n)
  uint4 n;
{
  bytes_size = n;
}

/**  void  bytes__free(p)  :  Frees the array of bytes *p. The number
                     of bytes stored in the array is given by the
                     global variable bytes_size, which can be modified
                     by a call to the function bytes__prepare_free.
                     This side effect is unavoidable, the primary
                     purpose of the function bytes__free being to
                     serve as an argument to routines that expect a
                     function with only one argument.              **/

void  bytes__free(p)
  uint1 *p;
{
  resr__free_objects(p, uint1, bytes_size);
} 

/**  uint4_set *set__new_empty()  :  Creates a new empty set. Returns
                     a pointer to this set if successful, and NULL
                     in the case of insufficient memory.           **/

uint4_set *set__new_empty()
{
  register uint4_set *r;

  r = resr__new_object(uint4_set);
  if (!r)
    return NULL;

  r -> nb = r -> al = ZERO_INT4;
  r -> ptr = NULL;
  r -> sorted = 1;

  return r;
}

/**  void  set__new_empty_content(s)  :  Fills the contents of the
                     set structure *s with a new empty set, without
                     performing any action with the previous content.
		                                                   **/

void  set__new_empty_content(s)
  uint4_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  s -> nb = s -> al = ZERO_INT4;
  s -> ptr = NULL;
  s -> sorted = 1;
}

/**  void  set__free(s)  :  Frees the set *s.                      **/

void  set__free(s)
  uint4_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  if (s -> al)
    resr__free_objects(s -> ptr, uint4, s -> al);

  resr__free_object(s, uint4_set);
}

/**  void  set__free_content(s)  :  Frees the content of the set *s,
                     but not the set itself.                       **/

void  set__free_content(s)
  uint4_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  if (s -> al)
    resr__free_objects(s -> ptr, uint4, s -> al);

  s -> nb = s -> al = ZERO_INT4;
  s -> ptr = NULL;
  s -> sorted = 1;
}

/**  uint4  set__nb_elements(s)  :  Returns the number of (distinct)
                     elements in the set *s.                       **/     

uint4  set__nb_elements(s)
  uint4_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return ZERO_INT4;
#endif

  pack_set(s);
  return s -> nb;
}

/**  int  set__add(s, n)  :  Inserts the number n into the set *s. 
                     Returns -1 in case of insufficient memory, and 0
                     otherwise.                                    **/

int  set__add(s, n)
  uint4_set *s;
  uint4  n;
{
  register uint4  m;
  register uint4 *r;

#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return -1;
#endif

  m = s -> al;
  
  if (s -> nb >= m)
    {
      r = resr__resize_objects(s -> ptr, uint4, 
          m + SET_GROWTH_QUANTUM, m);
      if (!r)
	return -1;
      
      s -> al  = m + SET_GROWTH_QUANTUM;
      s -> ptr = r;
    }

  m = s -> nb++;
  s -> sorted = 0;
  s -> ptr[m] = n;

  return 0;
}

/**  void  set__remove(s, n)  :  Removes n from the set *s if it 
                     belongs to it (otherwise, does nothing.)      **/

void set__remove(s, n)
     uint4_set *s;
     uint4 n;
{
  static   uint4  key;
  register uint4 *el;

#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  pack_set(s);
  key = n;
  if (s -> nb > 0)
    {
      el = bsearch((const void *) &key, (const void *) 
		   (s -> ptr), s -> nb, sizeof(uint4),
		   (int (*) (const void *, const void *)) 
		   uint4_cmp);    
      if (el)
	{
	  *el = s -> ptr[--(s -> nb)];
	  s -> sorted = 0;
	}
    }
  return;
}

/**  uint4  set__element(s, n)  :  Returns the n-th element (n = 0, 1,
                     2, ...) from the set *s.                      **/

uint4  set__element(s, n)
  uint4_set *s;
  uint4      n;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return ZERO_INT4;
#endif

  pack_set(s);

#if LASH_CHECK_LEVEL >= 2
  if (n >= s -> nb)
    return ZERO_INT4;
#endif

  return s -> ptr[n];
}

/**  int  set__member(s, n)  :  Returns 1 if n belongs to the set *s.
                     Returns 0 otherwise.                          **/
   
int  set__member(s, n)
  uint4_set *s;
  uint4      n;
{
  register uint4  j;
  static   uint4  key;

  if (!s)
    return 0;
  
  pack_set(s);
  j   = s -> nb;
  key = n;

  return ((j && bsearch((const void *) &key, (const void *) 
      (s -> ptr), j, sizeof(uint4), (int (*) (const void *, 
      const void *)) uint4_cmp)) ? 1 : 0);
}

/**  uint4_set *set__new_copy(s)  :  Returns a a pointer to a new set
                     equal to the set *s. Returns a NULL pointer in
                     the case of insufficient memory.              **/

uint4_set *set__new_copy(s)
  uint4_set *s;
{
  register uint4_set *s2;
  register uint4     *p, n;

#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return NULL;
#endif

  pack_set(s);
  
  s2 = resr__new_object(uint4_set);

  if (!s2)
    return NULL;

  n = s -> nb;

  p = resr__new_objects(uint4, n);
  if (!p)
    {
      resr__free_object(s2, uint4_set);
      return NULL;
    }

  s2 -> ptr = p;
  s2 -> nb = s2 -> al = n;
  s2 -> sorted = s -> sorted;

  memcpy(p, s -> ptr, n * sizeof(uint4));

  return s2;
}

/**  int  set__equal(s1, s2)  :  Returns 1 if the two sets *s1 and
                     *s2 are equal, and 0 otherwise.               **/

int  set__equal(s1, s2)
  uint4_set *s1, *s2;
{
  register uint4 i, n;

#if LASH_CHECK_LEVEL >= 2
  if (!s1 || !s2)
    return 0;
#endif

  pack_set(s1);
  pack_set(s2);

  n = s1 -> nb;

  if (s2 -> nb != n)
    return 0;
  
  for (i = 0; i < n; i++)
    if (s1 -> ptr[i] != s2 -> ptr[i])
      return 0;

  return 1;
}

/**  void  set__replace(s1, s2)  :  Frees the components of the 
                     set *s1 and replaces them by the components 
                     of *s2. Then, frees the set *a2.              **/

void  set__replace(s1, s2)
  uint4_set *s1, *s2;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s1 || !s2)
    return;
#endif

  set__free_content(s1);
  *s1 = *s2;
  resr__free_object(s2, uint4_set);
}

/**  hash_table *hash__new_empty(n)  :  Creates a new empty hash table
                     of size n. Returns a pointer to the table that
                     has been created in case of success, and a NULL
                     pointer if there is not enough memory.        **/

hash_table *hash__new_empty(n)
  uint4 n;
{
  register hash_table  *t;
  register hash_entry **e;

  t = resr__new_object(hash_table);
  if (!t)
    return NULL;

  e = resr__new_objects(hash_entry *, n);
  if (!e)
    {
      resr__free_object(t, hash_table);
      return NULL;
    }

  memset(e, 0, n * sizeof(hash_entry *));

  t -> size  = n;
  t -> table = e;

  return t;
}

/**  void  hash__free(t, fk, fp)  :  Frees the hash_table *t, invoking
                     the functions *fk and *fp for respectively each
                     key and each payload present in the table.    **/   

void  hash__free(t, fk, fp)
  hash_table *t;
  void (*fk)(void *);
  void (*fp)(void *);
{
  register uint4       i, n;
  register hash_entry *e, *f;

#if LASH_CHECK_LEVEL >= 2
  if (!t)
    return;
#endif

  n = t -> size;

  for (i = 0; i < n; i++)
    for (e = t -> table[i]; e; e = f)
      {
	if (fk)
	  fk(e -> key);
	if (fp)
	  fp(e -> payload);
	f = e -> next;
       resr__free_object(e, hash_entry);
      }
    
  resr__free_objects(t -> table, uint4, n);
  resr__free_object(t, hash_table);
}

/**  void *hash__lookup_set(t, s)  :  Searches for the set *s in the
                     hash table *t. Returns a pointer to the
                     corresponding payload if the set is found, and
                     a NULL pointer otherwise.                     **/  

void *hash__lookup_set(t, s)
  hash_table *t;
  uint4_set  *s;
{
  register hash_entry *e;

  for (e = t -> table[hash_set(s, t -> size)]; e; e = e -> next)
    if (set__equal(s, (uint4_set *)(e -> key)))
      return e -> payload;

  return NULL;
}

/**  int  hash__insert_set(t, s, r)  :  Inserts the set *s in the hash
                     table *t. Places in *r a pointer to the pointer
                     to the corresponding payload if the set has been
                     successfully inserted. The value placed in *r
                     is NULL if the set is already present in the
                     table. In both of these cases, the function
                     returns 0. In the case of an error, the function
                     returns -1.

                     If the consistency check level is at least equal
                     to 1, the function has two additional parameters
                     p1 and p2 which give the addresses of two 
                     counters that are incremented respectively at
                     each collision and at each insertion in the 
                     table.                                        **/

#if LASH_CHECK_LEVEL >= 1
int  hash__insert_set(t, s, r, p1, p2)
  uint8 *p1, *p2;
#else
int  hash__insert_set(t, s, r)
#endif  /* >= 1 */
  hash_table *t;
  uint4_set  *s;
  void     ***r;
{
  register uint4  i;
  register hash_entry *e;

  i = hash_set(s, t -> size);
  for (e = t -> table[i]; e; e = e -> next)
    if (set__equal(s, (uint4_set *)(e -> key)))
      {
	if (r)
	  *r = NULL;
        return 0;
      }

  e = resr__new_object(hash_entry);
  if (!e)
    return -1;

  e -> key = (void *) set__new_copy(s);
  if (!(e -> key))
    {
      resr__free_object(e, hash_entry);
      return -1;
    }

  e -> next = t -> table[i];
  t -> table[i] = e;
  e -> payload = NULL;

#if LASH_CHECK_LEVEL >= 1
  if (e -> next)
    ++*p1;

  ++*p2;
#endif  /* >= 1 */

  if (r)
    *r = &e -> payload;  

  return 0;
}

/**  void *hash__lookup_bytes(t, p, n)  :  Searches for the sequence
                     of n bytes beginning at the address p in the hash
                     table *t. All the entries in this table are
                     supposed to be sequences of length n. Returns a
                     pointer to the corresponding payload if the
                     sequence is found, and a NULL pointer otherwise. 
                                                                   **/

void *hash__lookup_bytes(t, p, n)
  hash_table *t;
  uint1      *p;
  uint4       n;
{
  register hash_entry *e;

  for (e = t -> table[hash_bytes(p, n, t -> size)]; e; e = e -> next)
    if (!n || !memcmp(p, (uint1 *)(e -> key), n))
      return e -> payload;

  return NULL;
}

/**  int  hash__insert_bytes(t, p, n, r)  :  Inserts the sequence of
                     n bytes beginning at the address p in the hash
                     table *t. All the entries of this table must be
                     sequences of exactly n bytes. Places in *r a
                     pointer to the pointer to the corresponding
                     payload if the sequence has been successfully
                     inserted. The value placed in r is NULL if the
                     sequence is already present in the table. In both
                     of these cases, the function returns 0. In the
                     case of an error, the function returns -1.

                     If the consistency check level is at least equal
                     to 1, the function has two additional parameters
                     p1 and p2 which give the addresses of two 
                     counters that are incremented respectively at
                     each collision and at each insertion in the 
                     table.                                        **/

#if LASH_CHECK_LEVEL >= 1
int  hash__insert_bytes(t, p, n, r, p1, p2)
  uint8 *p1, *p2;
#else
int  hash__insert_bytes(t, p, n, r)
#endif  /* >= 1 */
  hash_table *t;
  uint1      *p;
  uint4       n;
  void     ***r;
{
  register uint4       i;
  register hash_entry *e;

  i = hash_bytes(p, n, t -> size);
  for (e = t -> table[i]; e; e = e -> next)
    if (!n || !memcmp(p, (uint1 *)(e -> key), n))
      {
	if (r)
	  *r = NULL;
        return 0;
      }

  e = resr__new_object(hash_entry);
  if (!e)
    return -1;

  e -> key = (void *) resr__new_objects(uint1, n);
  if (!(e -> key))
    {
      resr__free_object(e, hash_entry);
      return -1;
    }

  if (n)
    memcpy((uint1 *) e -> key, p, n);

  e -> next = t -> table[i];
  t -> table[i] = e;
  e -> payload = NULL;

#if LASH_CHECK_LEVEL >= 1
  if (e -> next)
    ++*p1;

  ++*p2;
#endif  /* >= 1 */

  if (r)
    *r = &e -> payload;  

  return 0;
}

/**  void *hash_lookup_string(t, s)  :  Searches for the null-
                     terminated string s in the hash table *t.
                     Returns a pointer to the corresponding payload if
                     the sequence is found, and a NULL pointer
                     otherwise.                                    **/

void *hash__lookup_string(t, s)
  hash_table *t;
  char       *s;
{
  register hash_entry *e;

  for (e = t -> table[hash_string(s, t -> size)]; e; e = e -> next)
    if (!strcmp(s, (uint1 *)(e -> key)))
      return e -> payload;

  return NULL;
} 

/**  int  hash__insert_string(t, s, r)  :  Inserts the null-terminated
                     string s in the hash table *t.Places in *r a
                     pointer to the pointer to the corresponding
                     payload if the string has been successfully
                     inserted. The value placed in r is NULL if the
                     string is already present in the table. In both
                     of these cases, the function returns 0. In the
                     case of an error, the function returns -1.

                     If the consistency check level is at least equal
                     to 1, the function has two additional parameters
                     p1 and p2 which give the addresses of two 
                     counters that are incremented respectively at
                     each collision and at each insertion in the 
                     table.                                        **/

#if LASH_CHECK_LEVEL >= 1
int  hash__insert_string(t, s, r, p1, p2)
  uint8 *p1, *p2;
#else
int  hash__insert_string(t, s, r)
#endif  /* >= 1 */
  hash_table *t;
  char       *s;
  void     ***r;
{
  register uint4       i;
  register hash_entry *e;

  i = hash_string(s, t -> size);
  for (e = t -> table[i]; e; e = e -> next)
    if (!strcmp(s, (char *)(e -> key)))
      {
	if (r)
	  *r = NULL;
        return 0;
      }

  e = resr__new_object(hash_entry);
  if (!e)
    return -1;

  e -> key = (void *) resr__new_objects(char, strlen(s) + 1);
  if (!(e -> key))
    {
      resr__free_object(e, hash_entry);
      return -1;
    }

  strcpy((char *) e -> key, s);

  e -> next = t -> table[i];
  t -> table[i] = e;
  e -> payload = NULL;

#if LASH_CHECK_LEVEL >= 1
  if (e -> next)
    ++*p1;

  ++*p2;
#endif  /* >= 1 */

  if (r)
    *r = &e -> payload;  

  return 0;
}

/**  bit_table *bit__new_empty(n)  :  Creates a new empty bit table
                     capable of holding n entries. Returns a pointer
                     to the new table, or a NULL pointer if there is
                     not enough memory.                            **/

bit_table *bit__new_empty(n) 
  uint4  n;
{
  register bit_table *t;
  register uint1     *p;
  register uint4      m;

  t = resr__new_object(bit_table);
  if (!t)
    return NULL;

  m = (n + 7) / 8;

  p = resr__new_objects(uint1, m);
  if (!p)
    {
      resr__free_object(t, bit_table);
      return NULL;
    }

  t -> nbytes = m;
  t -> bits = p;

  memset(p, 0, m);

  return t;
}

/**  void  bit__free(t)  :  Frees the bit table *t.                **/

void  bit__free(t)
  bit_table *t;
{
#if LASH_CHECK_LEVEL >= 2
  if (!t)
    return;
#endif

  resr__free_objects(t -> bits, uint1, t -> nbytes);
  resr__free_object(t, bit_table);
}

/**  bit_table *bit__new_copy(t)  :  Creates a copy of the bit table
                     *t.   Returns a pointer to the new table, or a
                     NULL pointer if there is not enough memory.   **/

bit_table *bit__new_copy(bt)
     bit_table *bt;
{  
  register bit_table *t;
  register uint1     *p;

#if LASH_CHECK_LEVEL >= 2
  if (!bt)
    return NULL;
#endif  /* >= 2 */
  
  t = resr__new_object(bit_table);
  if (!t)
    return NULL;

  p = resr__new_objects(uint1, bt -> nbytes);
  if (!p)
    {
      resr__free_object(t, bit_table);
      return NULL;
    }

  t -> nbytes = bt -> nbytes;
  t -> bits = p;

  memcpy(p, bt -> bits, bt -> nbytes);

  return t; 
}

/**  void  bit__add(t, n)  :  Sets the bit number n (n = 0, 1, 2, ...)
                     in the bit table *t.                          **/

void  bit__add(t, n)
  bit_table *t;
  uint4      n;
{
  register uint4 i;
  register uint1 j;
  
  i = n / 8;
  j = 1 << (n % 8);

#if LASH_CHECK_LEVEL >= 2
  if (t && i < (t -> nbytes))
#endif  /* >= 1 */
    t -> bits[i] |= j;
}

/**  void  bit__remove(t, n)  :  Resets the bit number n (n = 0, 1,
                     2, ...) in the bit table *t.                  **/

void  bit__remove(t, n)
  bit_table *t;
  uint4      n;
{
  register uint4 i;
  register uint1 j;
  
  i = n / 8;
  j = 1 << (n % 8);

#if LASH_CHECK_LEVEL >= 2
  if (t && i < (t -> nbytes))
#endif  /* >= 1 */
    t -> bits[i] &= ~j;
}

/** int bit__member(t, n)  :  Returns 1 if the bit number n (n = 0, 1,
                     2, ...) is set in the bit table *t, and 0
                     otherwise.                                    **/

int  bit__member(t, n)
  bit_table *t;
  uint4 n;
{
  register uint4 i;
  register uint1 j;
  
  i = n / 8;
  j = 1 << (n % 8);

#if LASH_CHECK_LEVEL >= 2
  return t && (i < (t -> nbytes)) && !!((t -> bits[i]) & j);
#else
  return !!((t -> bits[i]) & j);
#endif  /* >= 1 */
}

/**  void  bit__empty_content(t)  :  Resets all the bits in the
                     bit table *t.                                 **/

void  bit__empty_content(t)
  bit_table *t;
{
#if LASH_CHECK_LEVEL >= 2
  if (!t)
    return;
#endif

  memset(t -> bits, 0, t -> nbytes);
}

/**  stack *stack__new_empty_from_size(n)  :  Returns a pointer to a
                     new empty stack that can be used for storing
                     elements of size n (in bytes), or a NULL pointer
                     if there is not enough memory.                **/

stack *stack__new_empty_from_size(n)
  uint4  n;
{
  register stack *st;

  st = resr__new_object(stack);
  if (!st)
    return NULL;

  st -> element_nbytes = n;
  st -> alloc_nbytes = ZERO_INT4;
  st -> nb_elements  = ZERO_INT4;
  st -> base = NULL;

  return st;
}

/**  int  stack__push(st, p)  :  Pushes the element *p onto the stack
                     *st. Returns -1 in case of insufficient memory,
                     and 0 otherwise.                              **/

int  stack__push(st, p)
  stack *st;
  void  *p;
{
  register uint4   na, ne, nb, n, increment;
  register uint1  *new_base;

  na = st -> alloc_nbytes;
  ne = st -> nb_elements;
  nb = st -> element_nbytes;

  n = (ne + 1) * nb;
  if (n > na)
    {
      increment = (nb > STACK_GROWTH_QUANTUM) ? 
          nb : STACK_GROWTH_QUANTUM;
      new_base = resr__resize_objects(st -> base, uint1, 
          na + increment, na);
      if (!new_base)
	return -1;
      st -> alloc_nbytes = na + increment;
      st -> base = new_base;
    }
 
  st -> nb_elements = ne + 1;
  memcpy(st -> base + (ne * nb), (uint1 *) p, nb); 
  
  return 0;
}

/**  void  stack__pop(st, p)  :  Pops the element *p from the stack
                     *st.                                          **/

void  stack__pop(st, p)
  stack *st;
  void  *p;
{
#if LASH_CHECK_LEVEL >= 2
  if (!st || !(st -> nb_elements))
    return;
#endif

  if (p)
    memcpy((uint1 *) p, st -> base + (st -> nb_elements - 1) *
        (st -> element_nbytes), st -> element_nbytes);

   st -> nb_elements--;
}

/**  void *stack__top(st)  :  Returns a pointer to the topmost element
                     of the stack *st.                             **/

void *stack__top(st)
  stack *st;
{
#if LASH_CHECK_LEVEL >= 1
  if (!st || !(st -> nb_elements))
    return NULL;
#endif

  return (st -> base) + (st -> element_nbytes) * 
      (st -> nb_elements - 1);
}

/**  void *stack__pick(st, n)  :  Returns a pointer to the n-th
                     topmost element of the stack *st (n = 0
                     corresponding to the top element).            **/

void *stack__pick(st, n)
  stack *st;
  uint4  n;
{
#if LASH_CHECK_LEVEL >= 1
  if (!st || (st -> nb_elements <= n))
    return NULL;
#endif

  return (st -> base) + (st -> element_nbytes) * 
      (st -> nb_elements - n - 1);
}

/**  void  stack__reduce_fn(st, n, f)  :  Removes n elements from the
                     top of the stack *st, and calls *f for each
                     removed element.                              **/

void  stack__reduce_fn(st, n, f)
  stack *st;
  uint4  n;
  void (*f)(void *);
{
  register uint4  i;
  register uint1 *p;

#if LASH_CHECK_LEVEL >= 1
  if (!st || (st -> nb_elements < n))
    return;
#endif

  if (f)
    for (i = 0, p = (st -> base) + (st -> element_nbytes) * 
        (st -> nb_elements - 1); i < n; i++, 
        p -= st -> element_nbytes)
      f((void *) p);

  st -> nb_elements -= n; 
}

/**  void  stack__free(st)  :  Frees the stack *st.                **/

void  stack__free(st)
  stack *st;
{
#if LASH_CHECK_LEVEL >= 1
  if (!st)
    return;
#endif

  resr__free_objects(st -> base, uint1, st -> alloc_nbytes);
  resr__free_object(st, stack);
}

/**  void  stack_free_fn(st, f)  :  Frees the stack *st, calling the
                     function *f for each unallocated element.     **/

void  stack__free_fn(st, f)
  stack  *st;
  void  (*f)(void *);
{
  register uint4  i;
  register uint1 *p;

#if LASH_CHECK_LEVEL >= 1
  if (!st)
    return;
#endif 

  if (f)
    for (i = 0, p = st -> base; i < st -> nb_elements;
        i++, p += st -> element_nbytes)
      f((void *) p);

  resr__free_objects(st -> base, uint1, st -> alloc_nbytes);
  resr__free_object(st, stack);
}

/**  fifo *fifo__new_empty_from_size(n)  :  Returns a pointer to a
                     new empty queue that can be used for storing
		     elements of size n (in  bytes), or a NULL
		     pointer if there is not enough memory.        **/

fifo *fifo__new_empty_from_size(n)
     uint4 n;
{
  register fifo *q;

#if LASH_CHECK_LEVEL >= 1
  if (!n)
    return NULL;
#endif 

  q = resr__new_object(fifo);
  if (!q)
    return NULL;

  q -> element_nbytes = n;
  q -> alloc_nbytes = ZERO_INT4;
  q -> nb_elements  = ZERO_INT4;
  q -> base = NULL;
  q -> front = ZERO_INT4;
  q -> rear =  ZERO_INT4;
  return q;
}

/**  int  fifo__add(q, p)  :  Appends the element *p to the tail of
                     the queue *q. Returns -1 in case of insufficient
		     memory, and 0 otherwise.                      **/

int  fifo__add(q, p)
  fifo *q;
  void  *p;
{
  register uint4   na, nu, nb, n, increment;
  register uint1  *new_base;

#if LASH_CHECK_LEVEL >= 1
  if (!p || !q)
    return -1;
#endif 

  na = q -> alloc_nbytes;
  nu = q -> rear;
  nb = q -> element_nbytes;

  n = (nu + 1) * nb;
  if (n > na)
    {
      increment = (nb > FIFO_GROWTH_QUANTUM) ? 
          nb : FIFO_GROWTH_QUANTUM;
      new_base = resr__resize_objects(q -> base, uint1, 
          na + increment, na);
      if (!new_base)
	return -1;
      q -> alloc_nbytes = na + increment;
      q -> base = new_base;
    }
 
  memcpy(q -> base + (q -> rear * nb), (uint1 *) p, nb); 
  q -> rear++;
  q -> nb_elements++;
  
  return 0;
}

/** void  fifo__remove(q, p): Remove the element *p from the 
                     head of the queue *q. (p must point to an
		     allocated memory block of the appropriate
		     size, in which the removed element will be
		     copied.)                                      **/

void  fifo__remove(q,p)
     fifo *q;
     void *p;
{
  register uint4  nb, new_a;
  register uint1 *base; 

#if LASH_CHECK_LEVEL >= 1
  if (!q || !(q -> nb_elements) || !p)
    return;
#endif

  nb = q -> element_nbytes;

  if (p)
    memcpy((uint1 *) p, q -> base + (q -> front * nb), nb);

  q -> nb_elements--;
  q -> front++;

  if  (q -> front > FIFO_SHIFT)
    {
      new_a = nb * q -> nb_elements ;
      base = resr__new_objects(uint1, new_a);
      if (base)
	{
	  memcpy(base, q -> base + (q -> front * nb), new_a); 
	  resr__free_objects(q -> base, uint1, q -> alloc_nbytes);
	  q -> base = base;
	  q -> alloc_nbytes = new_a;
	  q -> front =  ZERO_INT4;
	  q -> rear = q -> nb_elements;
	}
    }
}

/** void *fifo__front(q);  Returns a pointer to the 
                     frontmost element of the queue *q             **/

void *fifo__front(q)
     fifo *q;
{
#if LASH_CHECK_LEVEL >= 1
  if (!q)
    return NULL;
#endif

  return (q -> base) + ( q -> front * q -> element_nbytes);
}

/**  void *fifo__pick(q, n)  :  Returns a pointer to the n-th
                     element of the queue *q (n = 0 corresponding to
		     the front element).                           **/

void *fifo__pick(q, n)
     fifo *q;
     uint4  n;
{
  register uint4 index;

#if LASH_CHECK_LEVEL >= 1
  if (!q || (q -> nb_elements <= n))
    return NULL;
#endif

  index = (q -> front + n) * q -> element_nbytes;

  return (q -> base) + index;
}

/**  void  fifo__free(q)  :  Frees the queue *q.                   **/

void  fifo__free(q)
     fifo *q;
{
#if LASH_CHECK_LEVEL >= 1
  if (!q)
    return;
#endif

  resr__free_objects(q -> base, uint1, q -> alloc_nbytes);
  resr__free_object(q, fifo);
}

/** object_set *object_set__new_empty_from_size(n, cmp) : Creates a
                     new empty set that can be used for storing
                     objects of size n (in bytes). The function used
                     to compare two objects is cmp. Returns a pointer
                     to the newly created set if successful, and NULL
                     in the case of insufficient memory.           **/

object_set *object_set__new_empty_from_size(n, cmp)
     uint4 n;
     int  (*cmp) (const void *, const void *);
{
  register object_set *r;

  r = resr__new_object(object_set);
  if (!r)
    return NULL;

  r -> object_nbytes = n;
  r -> alloc_nbytes = r -> nb_elements = ZERO_INT4;
  r -> base = NULL;
  r -> object_cmp = cmp;
  r -> sorted = 1;

  return r;
}

/**  void  object_set__free(s)  :  Frees the object_set *s.        **/

void  object_set__free(s)
  object_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return;
#endif

  if (s -> alloc_nbytes)
    resr__free_objects(s -> base, uint1, s -> alloc_nbytes);

  resr__free_object(s, object_set);
}

/**  uint4  object_set__nb_elements(s)  :  Returns the number of
                     (distinct) elements in the object set *s.     **/

uint4  object_set__nb_elements(s)
  object_set *s;
{
#if LASH_CHECK_LEVEL >= 2
  if (!s)
    return ZERO_INT4;
#endif

  pack_object_set(s);
  return s -> nb_elements;
}

/**  int  object_set__add(s, p)  :  Inserts the object pointed by *p
                     into the set *s. Returns -1 in case of
		     insufficient memory, and 0 otherwise.         **/

int  object_set__add(s, p)
  object_set *s;
  void       *p;
{
  register uint4  na, ne, nb, n, new_na;
  register uint1 *new_base;

  nb = s -> object_nbytes;
  na = s -> alloc_nbytes;
  ne = s -> nb_elements;

  n  = (ne + 1) * nb;
  if (n > na)
    {
      if (OBJECT_SET_GROWTH_QUANTUM > nb)
	new_na = na + OBJECT_SET_GROWTH_QUANTUM;
      else
	new_na = na + nb;
      
      new_base = resr__resize_objects(s -> base, uint1, new_na, na);
      if (!new_base)
	return -1;
      
      s -> alloc_nbytes  = new_na;
      s -> base = new_base;
    }
  
  s -> nb_elements++;
  memcpy(s -> base + ne * nb,  p, nb);
  
  s -> sorted = 0;

  return 0;
}

/**  uint1* object_set__element(s, n)  :  Returns a pointer to
                     the n-th element (n = 0, 1, 2, ...) of the
		     object_set *s.                                **/

uint1  *object_set__element(s, n)
  object_set *s;
  uint4       n;
{
  pack_object_set(s);

#if LASH_CHECK_LEVEL >= 2
  if (n >= s -> nb_elements)
    return NULL;
#endif

  return (uint1 *) (s -> base + n * s -> object_nbytes);
}

/**  int object_set__member(s, p)  :  Returns 1 if the object element
                     *p belongs to the set *s. Returns 0 otherwise.
                                                                   **/
   
int object_set__member(s, p)
     object_set *s;
     void       *p;
{
  uint4  i, j, m, nb;
  uint1 *base;
  int    t;
  int  (*cmp) (const void *, const void *);

  if (!(s && p))
    return 0;
  
  pack_object_set(s);
  j = s -> nb_elements;

  if (!j)
    return 0;

  i = 0;
  j--;
  cmp = s -> object_cmp;
  base = s -> base;
  nb = s -> object_nbytes;

  do
    {
      m = (i + j) / 2;
      t = cmp(base + m * nb, p);

      if (!t)
	return 1;

      if (t < 0)
	i = m + 1;
      else
	{
	  if (m)        /* test needed by the fact that we use an   */
	    j = m - 1;  /* unsigned integral type (uint4)           */
	  else
	    return 0;
	}
    }
  while (i <= j);
  
  return 0;
}

/**  void  object_set__sort(s)  :  Sorts the elements of an object
                     set s in ascending order w.r.t. the comparison
		     fonction given at the initialisation of *s.   **/

void  object_set__sort(s)
  object_set *s;
{
  pack_object_set(s);
}

/****  End of datastruct.c  ****/
