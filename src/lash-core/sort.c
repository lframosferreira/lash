/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**          sort.c  :  General-purpose sorting algorithms.        **/
/**                                                                **/
/**        10/27/98  :  Creation. (BB)                             **/
/**        10/29/98  :  Continued. (BB)                            **/
/**        08/12/99  :  Improved sorting function for arbitrary    **/
/**                     values. (BB)                               **/
/**        07/02/02  :  Reorganization. (BB)                       **/
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
#include "datastruct.h"
#include "sort.h"

/****  Private types.                                            ****/

typedef struct _uint4_sort_info {
  uint4  value;
  struct _uint4_sort_info *next, *prev;
} uint4_sort_info;

typedef struct {
  uint4_sort_info *left, *right;
} uint4_stack_info;

typedef struct _bytes_sort_info {
  uint1  *value;
  struct _bytes_sort_info *next, *prev;
} bytes_sort_info;

typedef struct {
  bytes_sort_info *left, *right;
} bytes_stack_info;

/****  Prototypes of private functions.                          ****/

static uint4  pack_interval_uint4(uint4_sort_info *, 
                  uint4_sort_info *);
static int    sort_and_pack_uint4(uint4_sort_info *, uint4, stack *);
static uint4  interval_bytes(bytes_sort_info *, 
                  bytes_sort_info *, int (*) (const void *, 
                  const void *));
static int    sort_bytes(bytes_sort_info *, uint4, 
                  int (*) (const void *, const void *), stack *);
static uint4  pack_interval_bytes(bytes_sort_info *, 
                  bytes_sort_info *, int (*) (const void *, 
                  const void *), void (*) (const void *));
static int    sort_and_pack_bytes(bytes_sort_info *, uint4, 
                  int (*) (const void *, const void *), 
                  void (*) (const void *), stack *);

/****  Private functions.                                        ****/

/**  The following parameter is used by the function 
     sort_and_pack_uint4. A discussion of its value can be found in
     [Knuth73].                                                    **/

#define  QSORT_M  9

/**  uint4  pack_interval_uint4(p1, p2)  :  This function is part
                     of the uint4 sort-and-pack algorithm. It removes
                     from the doubly linked list starting at *p1 and
                     ending just before *p2 all the items whose value
                     is equal to that of the first item (this first
                     item being not removed). The function returns the
                     number of distinct items that have been found if
                     the values present in the interval do not appear
                     in increasing order, and 0 otherwise.         **/

static uint4  pack_interval_uint4(p1, p2)
  uint4_sort_info *p1, *p2;
{
  register uint4            n, k;
  register uint4_sort_info *p;
  register int              s;

  if (p1 == p2)
    return 0;

  for (n = 1, k = p1 -> value, p = p1 -> next, s = 1; p != p2;
       p = p -> next)
    {
      if (s && p -> value < p -> prev -> value)
	s = 0;
      if (p -> value == k)
	{
	  p -> prev -> next = p -> next;
	  p -> next -> prev = p -> prev;
	}
      else
	n++;
    }
 
  return s ? 0 : n;
}

/**  int  sort_and_pack_uint4(t, n, st)  :  This function is part
                     of the uint4 sort-and-pack algorithm. It sorts
                     the array *t, arranged as a doubly linked list,
                     and supposed to contain n + 2 entries. The
                     entries of index 0 and n + 1 are respectively
                     supposed to contain the entries with the lowest
		     and the highest value. The parameter st points
		     to a stack that can be used by this function
		     for storing intermediate values. 

                     This function is based on the quicksort 
                     algorithm described in [Knuth73].

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

static int  sort_and_pack_uint4(t, n, st)
  uint4_sort_info *t;
  uint4            n;
  stack           *st;
{
  register uint4_sort_info  *pl, *pr, *pi, *pj;
  register uint4             k, tmp, nr, nl;
  static   uint4_stack_info  si;

  if (pack_interval_uint4(t + 1, t + n + 1) > QSORT_M)
    {
      pl = t + 1;
      pr = t[n + 1].prev;

      for (;;)
	{
	  k = (pi = pl) -> value;
          pj = pr -> next;

	  for (;;)
	    {
	      for (pi = pi -> next; pi -> value < k; 
		   pi = pi -> next);

	      for (pj = pj -> prev; pj -> value > k; 
		   pj = pj -> prev);

	      if (pj > pi)
		{
		  tmp = pi -> value; pi -> value = pj -> value;
                  pj -> value = tmp;
		  continue;
		}
	      break;
	    } 

	  tmp = pl -> value; pl -> value = pj -> value;
	  pj -> value = tmp;
          nl = pack_interval_uint4(pl, pj);
	  nr = pack_interval_uint4(pj -> next, pr -> next);
	  if (nr >= nl && nl > QSORT_M)
	    {
	      si.left  = pj -> next;
	      si.right = pr -> next -> prev;
              pr = pj -> prev;
	      if (stack__push(st, (void *) &si) < 0)
		return -1;
	      continue;
	    }
	  else
	    if (nl > nr && nr > QSORT_M)
	      {
		si.left  = pl;
		si.right = pj -> prev;
                pl = pj -> next;
		if (stack__push(st, (void *) &si) < 0)
		  return -1;
		continue;
	      }
	  else
	    if (nr > QSORT_M && nl <= QSORT_M)
	      {
		pl = pj -> next;
		pr = pr -> next -> prev;
		continue;
	      }
	    else
	      if (nl > QSORT_M && nr <= QSORT_M)
		{
		  pr = pj -> prev;
		  continue;
		}
 
	  if (stack__is_empty(st))
	    break;

	  stack__pop(st, (void *) &si);
	  pl = si.left;
	  pr = si.right;
	}
    }

  for (pj = t -> next -> next; pj -> next; pj = pr)
    {
      pr = pj -> next;
      k = pj -> value;
      if (pj -> prev -> value < k)
	continue;

      pj -> prev -> next = pj -> next;
      pj -> next -> prev = pj -> prev;

      for (pi = pj -> prev;; pi = pi -> prev)
	{
	  if (pi -> value == k && pi -> prev)
	    break;
	  
	  if (pi -> value <= k)
	    {
	      pi -> next -> prev = pj;
	      pj -> next = pi -> next;
	      pi -> next = pj;
	      pj -> prev = pi;
	      break;
	    }
	}
    }

  return 0;
}

/**  uint4  pack_interval_bytes(p1, p2, f, fd)  :  This function is
                     part of the sort-and-pack algorithm for arbitrary
                     values. It removes from the doubly linked list
                     starting at *p1 and ending just before *p2 all
                     the items whose value is equal to that of the
                     first item (this first item being not
                     removed). The comparison function is *f. The
                     function returns the number of distinct items
                     that have been found if the values present in the
                     interval do not appear in increasing order, and 0
                     otherwise. If fd is not null, the function *fd
                     is called for every element that is deleted from
                     the list.                                     **/

static uint4  pack_interval_bytes(p1, p2, f, fd)
  bytes_sort_info *p1, *p2;
  int            (*f) (const void *, const void *);
  void           (*fd) (const void *);
{
  register uint4            n;
  register bytes_sort_info *p;
  register uint1           *k;
  register int              s;

  if (p1 == p2)
    return 0;

  for (n = 1, k = p1 -> value, p = p1 -> next, s = 1; p != p2;
       p = p -> next)
    {
      if (s && f((const void *) p -> value, 
          (const void *) p -> prev -> value) < 0)
	s = 0;
      if (!f((const void *) p -> value, (const void *) k))
	{
	  p -> prev -> next = p -> next;
	  p -> next -> prev = p -> prev;
          if (fd)
            fd((const void *) (p -> value));
	}
      else
	n++;
    }
 
  return s ? 0 : n;
}

/**  int  sort_and_pack_bytes(t, n, f, fd, st)  :  This function is 
                     part of the sort-and-pack algorithm for arbitrary
                     values. It sorts the array *t, arranged as a
                     doubly linked list and supposed to contain n + 2
                     entries, with respect to the comparison function
                     *f. The entries of index 0 and n + 1 must
                     respectively contain the entries with the lowest
                     and the highest value. The parameter st points to
                     a stack that can be used by this function for
                     storing intermediate values. If fd is not null,
                     the function *fd  is called for every element 
                     that is deleted from the array.   

                     This function is based on the quicksort 
                     algorithm described in [Knuth73].

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

static int  sort_and_pack_bytes(t, n, f, fd, st)
  bytes_sort_info *t;
  uint4            n;
  int            (*f) (const void *, const void *);
  void           (*fd) (const void *);
  stack           *st;
{
  register bytes_sort_info  *pl, *pr, *pi, *pj;
  register uint1            *k, *tmp;
  register uint4             nr, nl;
  static   bytes_stack_info  si;
  register int               r, deleted;

  if (pack_interval_bytes(t + 1, t + n + 1, f, fd) > QSORT_M)
    {
      pl = t + 1;
      pr = t[n + 1].prev;

      for (;;)
	{
	  k = (pi = pl) -> value;
          pj = pr -> next;

	  for (;;)
	    {
	      for (pi = pi -> next; f((const void *) pi -> value,
                   (const void *) k) < 0; pi = pi -> next);

	      for (pj = pj -> prev; f((const void *) pj -> value,
                   (const void *) k) > 0; pj = pj -> prev);

	      if (pj > pi)
		{
		  tmp = pi -> value; pi -> value = pj -> value;
                  pj -> value = tmp;
		  continue;
		}
	      break;
	    } 

	  tmp = pl -> value; pl -> value = pj -> value;
	  pj -> value = tmp;
          nl = pack_interval_bytes(pl, pj, f, fd);
	  nr = pack_interval_bytes(pj -> next, pr -> next, f, fd);
	  if (nr >= nl && nl > QSORT_M)
	    {
	      si.left  = pj -> next;
	      si.right = pr -> next -> prev;
              pr = pj -> prev;
	      if (stack__push(st, (void *) &si) < 0)
		return -1;
	      continue;
	    }
	  else
	    if (nl > nr && nr > QSORT_M)
	      {
		si.left  = pl;
		si.right = pj -> prev;
                pl = pj -> next;
		if (stack__push(st, (void *) &si) < 0)
		  return -1;
		continue;
	      }
	  else
	    if (nr > QSORT_M && nl <= QSORT_M)
	      {
		pl = pj -> next;
		pr = pr -> next -> prev;
		continue;
	      }
	    else
	      if (nl > QSORT_M && nr <= QSORT_M)
		{
		  pr = pj -> prev;
		  continue;
		}
 
	  if (stack__is_empty(st))
	    break;

	  stack__pop(st, (void *) &si);
	  pl = si.left;
	  pr = si.right;
	}
    }

  for (pj = t -> next -> next; pj -> next; pj = pr)
    {
      pr = pj -> next;
      k = pj -> value;
      if (f((const void *) pj -> prev -> value, 
            (const void *) k) < 0)
	continue;

      pj -> prev -> next = pj -> next;
      pj -> next -> prev = pj -> prev;
      deleted = 1;

      for (pi = pj -> prev;; pi = pi -> prev)
	{
	  r = f((const void *) pi -> value, (const void *) k);
	  if (!r && pi -> prev)
	    break;
	  
	  if (r <= 0)
	    {
	      pi -> next -> prev = pj;
	      pj -> next = pi -> next;
	      pi -> next = pj;
	      pj -> prev = pi;
              deleted = 0;
	      break;
	    }
	}

      if (deleted && fd)
	fd((const void *) (pj -> value));
    }

  return 0;
}
	     
/****  Public functions.                                         ****/

/**  int  uint4__sort_and_pack(t, np)  :  Sorts the array of uint4
                     numbers *t in ascending order, and eliminates
                     duplicate values. The number of values that
                     are present in the array is given by *np, and
                     can be modified by this function if values are
                     suppressed.   

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

int  uint4__sort_and_pack(t, np)
  uint4 *t, *np;
{
  register uint4             n, i, max, min, tmp;
  register uint4_sort_info  *si, *pi;
  register stack            *st;

  switch(n = *np)
    {
    case 0:
    case 1:
      return 0;
    case 2:
      if (t[0] == t[1])
	*np = 1;
      else
	if (t[0] > t[1])
	  {
	    tmp = t[0]; t[0] = t[1]; t[1] = tmp;
	  }
      return 0;
    default:
      break;
    }

  st = stack__new_empty(uint4_stack_info);
  if (!st)
    return -1;

  si = resr__new_objects(uint4_sort_info, n + 2);
  if (!si)
    {
      stack__free(st);
      return -1;
    }

  for (i = 0, pi = si + 1, max = min = t[0]; i < n; i++, pi++)
    if ((pi -> value = t[i]) > max)
      max = pi -> value;
    else
      if (pi -> value < min)
	min = pi -> value;

  for (i = 0, pi = si; i <= n; i++, pi++)
    pi -> next = pi + 1;

  for (i = 0, pi = si + 1; i <= n; i++, pi++)
    pi -> prev = pi - 1;

  si[0].value = min;
  si[n + 1].value = max;
  si[0].prev = si[n + 1].next = NULL;
  
  if (sort_and_pack_uint4(si, n, st) < 0)
    {
      resr__free_objects(si, uint4_sort_info, n + 2);
      stack__free(st);
      return -1;
    }

  for (pi = si -> next, *np = ZERO_INT4; pi && pi -> next;
       pi = pi -> next)
    {
      *(t++) = pi -> value;
      ++*np;
    }
  
  resr__free_objects(si, uint4_sort_info, n + 2);
  stack__free(st);

  return 0;
}

/**  int  bytes__sort_and_pack(t, np, w, f, fd)  :  Sorts the
                     array of values t in ascending order with respect
                     to the comparison function *f, and eliminates
                     duplicate values. The number of values that are
                     present in the array is given by *np, and can be
                     modified by this function if values are 
                     suppressed. The width in bytes of each value is
                     given by w. If fd is not null, the function *fd
                     is called for every element that is deleted from
                     the array.  

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

int  bytes__sort_and_pack(t, np, w, f, fd)
  void  *t;
  uint4 *np, w;
  int  (*f) (const void *, const void *);
  void (*fd) (const void *);
{
  register uint4             n, i;
  register bytes_sort_info  *si, *pi;
  register stack            *st;
  register uint1            *copy, *max, *min;
  register int               r;

  switch(n = *np)
    {
    case 0:
    case 1:
      return 0;
    case 2:
      r = f((const void *) t,
            ((const void *) (((uint1 *) t) + w)));
      if (!r)
	{
	  *np = 1;
	  if (fd)
	    fd((const void *) ((((uint1 *) t) + w)));
	}
      else
	if (r > 0)
	  {
	    copy = resr__new_objects(uint1, w);
	    if (!copy)
	      return -1;
	    memcpy(copy, (uint1 *) t, w);
	    memcpy((uint1 *) t, ((void *) (((uint1 *) t) + w)), w);
	    memcpy(((void *) (((uint1 *) t) + w)), copy, w);
	    resr__free_objects(copy, uint1, w);
	  }
      return 0;
    default:
      break;
    }

  copy = resr__new_objects(uint1, n * w);
  if (!copy)
    return -1;

  memcpy(copy, (uint1 *) t, n * w);

  st = stack__new_empty(bytes_stack_info);
  if (!st)
    {
      resr__free_objects(copy, uint1, n * w);
      return -1;
    }

  si = resr__new_objects(bytes_sort_info, n + 2);
  if (!si)
    {
      resr__free_objects(copy, uint1, n * w);
      stack__free(st);
      return -1;
    }

  for (i = 0, pi = si + 1, max = min = copy; i < n; i++, pi++)
    {
      pi -> value = copy + i * w;
      if (f((const void *) pi -> value, (const void *) max) > 0)
	max = pi -> value;
      else
	if (f((const void *) pi -> value, (const void *) min) < 0)
	  min = pi -> value;
    }

  for (i = 0, pi = si; i <= n; i++, pi++)
    pi -> next = pi + 1;

  for (i = 0, pi = si + 1; i <= n; i++, pi++)
    pi -> prev = pi - 1;

  si[0].value = min;
  si[n + 1].value = max;
  si[0].prev = si[n + 1].next = NULL;
  
  if (sort_and_pack_bytes(si, n, f, fd, st) < 0)
    {
      resr__free_objects(copy, uint1, n * w);
      resr__free_objects(si, bytes_sort_info, n + 2);
      stack__free(st);
      return -1;
    }

  for (pi = si -> next, *np = ZERO_INT4, min = (uint1 *) t;
       pi && pi -> next; pi = pi -> next, min += w)
    {
      memcpy(min, pi -> value, w);
      ++*np;
    }
  
  resr__free_objects(copy, uint1, n * w);
  resr__free_objects(si, bytes_sort_info, n + 2);
  stack__free(st);

  return 0;
}

/**  uint4  interval_bytes(p1, p2, f)  :  This function is

                     part of the sort algorithm for arbitrary
                     values. It tests whether the items belonging to
                     the doubly linked list starting at *p1 and ending
                     just before *p2 appear in increasing order with
                     respect to the comparison function f. If yes, the
                     function returns 0. If no, it returns the number
                     of elements that belong to the list.          **/

static uint4  interval_bytes(p1, p2, f)
  bytes_sort_info *p1, *p2;
  int            (*f) (const void *, const void *);
{
  register uint4            n;
  register bytes_sort_info *p;
  register int              s;

  if (p1 == p2)
    return 0;

  for (n = 1, p = p1 -> next, s = 1; p != p2; p = p -> next)
    {
      if (s && f((const void *) p -> value, 
          (const void *) p -> prev -> value) < 0)
	s = 0;
      n++;
    }
 
  return s ? 0 : n;
}

/**  int  sort_bytes(t, n, f, st)  :  This function is part of the 
                     sort algorithm for arbitrary values. It sorts the
                     array *t, arranged as a doubly linked list and
                     supposed to contain n + 2 entries, with respect
                     to the comparison function *f. The entries of
                     index 0 and n + 1 must respectively contain the
                     entries with the lowest and the highest
                     value. The parameter st points to a stack that
                     can be used by this function for storing
                     intermediate values.

                     This function is based on the quicksort 
                     algorithm described in [Knuth73].

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

static int  sort_bytes(t, n, f, st)
  bytes_sort_info *t;
  uint4            n;
  int            (*f) (const void *, const void *);
  stack           *st;
{
  register bytes_sort_info  *pl, *pr, *pi, *pj;
  register uint1            *k, *tmp;
  register uint4             nr, nl;
  static   bytes_stack_info  si;

  if (interval_bytes(t + 1, t + n + 1, f) > QSORT_M)
    {
      pl = t + 1;
      pr = t[n + 1].prev;

      for (;;)
	{
	  k = (pi = pl) -> value;
          pj = pr -> next;

	  for (;;)
	    {
	      for (pi = pi -> next; f((const void *) pi -> value,
                   (const void *) k) < 0; pi = pi -> next);

	      for (pj = pj -> prev; f((const void *) pj -> value,
                   (const void *) k) > 0; pj = pj -> prev);

	      if (pj > pi)
		{
		  tmp = pi -> value; pi -> value = pj -> value;
                  pj -> value = tmp;
		  continue;
		}
	      break;
	    } 

	  tmp = pl -> value; pl -> value = pj -> value;
	  pj -> value = tmp;
          nl = interval_bytes(pl, pj, f);
	  nr = interval_bytes(pj -> next, pr -> next, f);
	  if (nr >= nl && nl > QSORT_M)
	    {
	      si.left  = pj -> next;
	      si.right = pr -> next -> prev;
              pr = pj -> prev;
	      if (stack__push(st, (void *) &si) < 0)
		return -1;
	      continue;
	    }
	  else
	    if (nl > nr && nr > QSORT_M)
	      {
		si.left  = pl;
		si.right = pj -> prev;
                pl = pj -> next;
		if (stack__push(st, (void *) &si) < 0)
		  return -1;
		continue;
	      }
	  else
	    if (nr > QSORT_M && nl <= QSORT_M)
	      {
		pl = pj -> next;
		pr = pr -> next -> prev;
		continue;
	      }
	    else
	      if (nl > QSORT_M && nr <= QSORT_M)
		{
		  pr = pj -> prev;
		  continue;
		}
 
	  if (stack__is_empty(st))
	    break;

	  stack__pop(st, (void *) &si);
	  pl = si.left;
	  pr = si.right;
	}
    }

  for (pj = t -> next -> next; pj -> next; pj = pr)
    {
      pr = pj -> next;
      k = pj -> value;
      if (f((const void *) pj -> prev -> value, 
            (const void *) k) < 0)
	continue;

      pj -> prev -> next = pj -> next;
      pj -> next -> prev = pj -> prev;

      for (pi = pj -> prev;; pi = pi -> prev)
	if (f((const void *) pi -> value, (const void *) k) <= 0)
	  {
	    pi -> next -> prev = pj;
	    pj -> next = pi -> next;
	    pi -> next = pj;
	    pj -> prev = pi;
	    break;
	  }
    }

  return 0;
}

/**  int  bytes__sort(t, n, w, f)  :  Sorts the array of values t in
                     ascending order with respect to the comparison
                     function *f. The number of values that are
                     present in the array is given by n. The width in
                     bytes of each value is given by w.

                     In the case of insufficient memory, this 
                     function returns -1. Otherwise, it returns 0. **/

int  bytes__sort(t, n, w, f)
  void  *t;
  uint4  n, w;
  int  (*f) (const void *, const void *);
{
  register uint4             i;
  register bytes_sort_info  *si, *pi;
  register stack            *st;
  register uint1            *copy, *max, *min;

  switch(n)
    {
    case 0:
    case 1:
      return 0;
    case 2:
      if (f((const void *) t,
            ((const void *) (((uint1 *) t) + w))) > 0)
	{
	  copy = resr__new_objects(uint1, w);
	  if (!copy)
	    return -1;
	  memcpy(copy, (uint1 *) t, w);
	  memcpy((uint1 *) t, ((void *) (((uint1 *) t) + w)), w);
	  memcpy(((void *) (((uint1 *) t) + w)), copy, w);
	  resr__free_objects(copy, uint1, w);
	}
      return 0;
    default:
      break;
    }
  
  copy = resr__new_objects(uint1, n * w);
  if (!copy)
    return -1;

  memcpy(copy, (uint1 *) t, n * w);

  st = stack__new_empty(bytes_stack_info);
  if (!st)
    {
      resr__free_objects(copy, uint1, n * w);
      return -1;
    }

  si = resr__new_objects(bytes_sort_info, n + 2);
  if (!si)
    {
      resr__free_objects(copy, uint1, n * w);
      stack__free(st);
      return -1;
    }

  for (i = 0, pi = si + 1, max = min = copy; i < n; i++, pi++)
    {
      pi -> value = copy + i * w;
      if (f((const void *) pi -> value, (const void *) max) > 0)
	max = pi -> value;
      else
	if (f((const void *) pi -> value, (const void *) min) < 0)
	  min = pi -> value;
    }

  for (i = 0, pi = si; i <= n; i++, pi++)
    pi -> next = pi + 1;

  for (i = 0, pi = si + 1; i <= n; i++, pi++)
    pi -> prev = pi - 1;

  si[0].value = min;
  si[n + 1].value = max;
  si[0].prev = si[n + 1].next = NULL;
  
  if (sort_bytes(si, n, f, st) < 0)
    {
      resr__free_objects(copy, uint1, n * w);
      resr__free_objects(si, bytes_sort_info, n + 2);
      stack__free(st);
      return -1;
    }

  for (pi = si -> next, min = (uint1 *) t;
       pi && pi -> next; pi = pi -> next, min += w)
    memcpy(min, pi -> value, w);
  
  resr__free_objects(copy, uint1, n * w);
  resr__free_objects(si, bytes_sort_info, n + 2);
  stack__free(st);

  return 0;
}

/****  End of sort.c  ****/
