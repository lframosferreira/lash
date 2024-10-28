/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       io-ops.c  :  Low-level functions for writing/reading.    **/
/**                                                                **/
/**       07/05/00  :  Creation. (JMF)                             **/
/**       07/02/02  :  Reorganization. (BB)                        **/
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
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "diag.h"
#include "resource.h"
#include "io-ops.h"

/****  Private definitions                                       ****/

typedef int (*set_pfn)(void *, int (*)(uint4, void *, uint1 *),
		       void *);
typedef struct {
  uint1    tag, type, prop;
  set_pfn  fnct;
} set_entry;

#define io_setentry_set_property(pp, p)  (*pp |= (p))
#define io_setentry_test_property(pp, p) (!!(*pp & (p)))
#define IO_SET_PROP_FOUND    (1<<0)


/****  Public visible function(s).                               ****/

/**  mem_descr *io__to_mem_init()  :  Allocates and initialize a new
                     memory descriptor.

		     If successful, this functions returns a pointer
		     to the newly allocated descriptor.
                     In the case of an error, it returns NULL and
                     sets lash_errno.

                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.    **/

mem_descr *io__to_mem_init(void)
{
  mem_descr *mem = resr__new_object(mem_descr);
  
  if (mem)
    {
      mem -> where = ZERO_INT4;
      mem -> alloc_nbytes = MEM_GROWTH_QUANTUM;

      if ((mem -> beginning =
	   resr__malloc(MEM_GROWTH_QUANTUM)))
	return mem;
      else
	{
	  resr__free_object(mem, mem_descr);

	  lash_errno = LASH_ERR_NO_MEM;
	  return NULL;
	}
    }
  else
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }
}

/**  int  io__to_mem(length, mem, data)  :  Outputs data to the block
                     of memory given by *mem.

		     length is the length (in bytes) of the data to
		     output; s is a pointer to the data.
	
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt automaton.    **/

int  io__to_mem(length, mem, data)
     uint4      length;
     mem_descr *mem;
     uint1     *data;
{
  if (length > (mem -> alloc_nbytes - mem -> where))
    {
      uint4  increment =
	(length - (mem -> alloc_nbytes - mem -> where)) ?
	(length - (mem -> alloc_nbytes - mem -> where)) :
	MEM_GROWTH_QUANTUM;
      
      if (!(mem -> beginning =
	    resr__realloc(mem -> beginning, mem -> alloc_nbytes,
			  mem -> alloc_nbytes + increment)))
	{
	  lash_errno = LASH_ERR_NO_MEM;
	  return -1;
	}
      
      mem -> alloc_nbytes += increment;
    }
  
  memcpy((void *)(mem -> beginning + mem -> where),
	 (void *)data, length);
  mem -> where += length;

  return 0;
}

/**  int  io__to_mem_end(mem, p, l)  :  Deallocates properly the
                     memory descriptor *mem.  A pointer to the
		     content of the block of memory described by
		     *mem is put in *p (if p is not NULL) and its
		     length is put in *l (if l is not NULL).
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.                             **/

void  io__to_mem_end(mem, p, l)
     mem_descr  *mem;
     uint1     **p;
     uint4      *l;
{
  if (p)
    *p = mem -> beginning;
  
  if (l)
    *l = mem -> alloc_nbytes;

  resr__free_object(mem, mem_descr);
}

/** mem_descr *io__from_mem_init(p, l) : Allocates and initialize a
                     new memory descriptor.  p is a pointer to the
                     memory block and l is its length (in bytes).

                     Returns a pointer to the newly allocated memory
                     descriptor if successful, or NULL if there is not
		     enough memory (and sets lash_errno).          **/

mem_descr *io__from_mem_init(p, l)
     uint1 *p;
     uint4  l;
{
  mem_descr *md = resr__new_object(mem_descr);

  if (md)
    {
      md -> beginning = p;
      md -> where = ZERO_INT4;
      md -> alloc_nbytes = l;
      return md;
    }
  else
    {
      lash_errno = LASH_ERR_NO_MEM;
      return NULL;
    }
}

/**  int  io__from_mem(l, md, p)  :  Reads l bytes from the memory
                     descriptor *md and put them at p.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

		         LASH_ERR_FILE  : Bad file format.         **/

int  io__from_mem(l, md, p)
     uint4      l;
     mem_descr *md;
     uint1     *p;
{
  if (md -> where + l >= md -> alloc_nbytes)
    {
      lash_errno = LASH_ERR_FILE;
      return -1;
    }

  memcpy(p, md -> beginning + md -> where, l);
  md -> where += l;

  return 0;
}

/**  void  io__from_mem_end(md)  :  Terminates the reading operations
                     on the memory block described by md.          **/

void  io__from_mem_end(md)
     mem_descr *md;
{
  resr__free_object(md, mem_descr);
}

/**  FILE *io__to_file_init(f)  :  Allocates and initialize a new
                     file descriptor for the file *f.  If the file
		     *f already exists, its content is destroyed.

		     If successful, this functions returns a pointer
		     to the newly file descriptor. In the case of an
		     error, it returns NULL and sets lash_errno.

                     Possible error codes:

		         LASH_ERR_IO      : IO error.
		         LASH_ERR_NO_MEM  : Not enough memory.     **/

FILE *io__to_file_init(f)
     char *f;
{
  FILE *file = fopen(f, "wb");

  if (!file)
    {
      lash_errno = LASH_ERR_IO;
      return NULL;
    }
  
  /* Unbuffered read/write, to avoid unpredictable results with
     the fwrite function. */
  setbuf(file, NULL);

  return file;
}

/**  int  io__to_file(length, mem, data)  :  Outputs data to the block
                     of memory given by *mem.

		     length is the length (in bytes) of the data to
		     output; s is a pointer to the data.
	
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

		         LASH_ERR_IO       : IO error.             **/

int  io__to_file(length, file, data)
     uint4  length;
     FILE  *file;
     uint1 *data;
{
#if LASH_CHECK_LEVEL >= 2
  if (!file)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (fwrite((void *)data, sizeof(uint1), length, file) < length)
    {
      lash_errno = LASH_ERR_IO;
      return -1;
    }

  return 0;
}

/**  int  io__to_file_end(file)  :  Closes the file *file.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.  

                     Possible error codes:

		         LASH_ERR_IO       : IO error.             **/

int  io__to_file_end(file)
     FILE *file;
{
#if LASH_CHECK_LEVEL >= 2
  if (!file)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (fclose(file))
    {
      lash_errno = LASH_ERR_IO;
      return -1;
    }

  return 0;
}

/** FILE *io__from_mem_init(f) : Initialize a new file descriptor
                     for the file f.

                     Returns a pointer to the new file descriptor
                     if successful, or NULL if there is an error
		     (and sets lash_errno).

                     Possible error codes:

		         LASH_ERR_IO  : IO error.                  **/

FILE *io__from_file_init(f)
     char *f;
{
  FILE *file = fopen(f, "rb");

  if (!file)
    {
      lash_errno = LASH_ERR_IO;
      return NULL;
    }
  
  return file;
}

/**  int  io__from_file(l, md, p)  :  Reads l bytes from the memory
                     descriptor *md and put them at p.

		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.

                     Possible error codes:

		         LASH_ERR_IO  : IO error.                  **/

int  io__from_file(length, file, data)
     uint4  length;
     FILE  *file;
     uint1 *data;
{
#if LASH_CHECK_LEVEL >= 2
  if (!file || (!data && length))
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (fread((void *)data, 1, length, file) < length)
    {
      lash_errno = LASH_ERR_IO;
      return -1;
    }

  return 0;
}

/**  int  io__from_file_end(file)  :  Closes the file *file.
		     
		     If successful, this functions returns 0.
                     In the case of an error, it returns -1 and
                     sets lash_errno.  

                     Possible error codes:

		     LASH_ERR_IO       : IO error.                 **/

int  io__from_file_end(file)
     FILE *file;
{
#if LASH_CHECK_LEVEL >= 2
  if (!file)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (fclose(file))
    {
      lash_errno = LASH_ERR_IO;
      return -1;
    }
  
  return 0;
}

int  io__w_tag(f, arg, t)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint1  t;
{
  return f(1, arg, &t);
}

int  io__w_tagval(f, arg, n, t)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4  n;
     uint1 *t;
{
  return f(n, arg, t);
}

int  io__w_tagval1(f, arg, t)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint1  t;
{
  return io__w_tagval(f, arg, 1, &t);
}

int  io__w_num(f, arg, n)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4  n;
{
  uint1  b, l = 1;
  uint4  n2 = n;

  if (n < ((uint4)0xFE))
    return f(1, arg, (b = n % 0x100, &b));
  
  /* The following assumes that the encoding of the
     length (in bytes) of the encoding of n is smaller
     than 0xFE (should be true because n is a uint4). */
  for (; n2 >>= 8; l++) ;
  
  {
    uint1  ff = 0xFF;
    
    if (f(1, arg, &ff) ||
	f(1, arg, &l))
      return -1;
  }
  
  for (; l; l--, n >>= 8)
    if (f(1, arg, (b = n % 0x100, &b)))
      return -1;
  
  return 0;
}

int  io__w_snum(f, arg, n)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     sint4  n;
{
  uint1  b, l = 1;
  uint4  n2;

  if (n >= 0 && n < ((sint4)0xFE))
    return f(1, arg, (b = n % 0x100, &b));

  if (n < ZERO_INT4)
    n2 = -n;
  else
    n2 = n;
  
  /* The following assumes that the encoding of the
     length (in bytes) of the encoding of n is smaller
     than 0xFE (should be true because n is a sint4). */
  for (; n2 >>= 8; l++) ;
  
  {
    uint1  ffe;
    
    if (n < ZERO_INT4)
      ffe = 0xFE;
    else
      ffe = 0xFF;

    if (f(1, arg, &ffe) ||
	f(1, arg, &l))
      return -1;
  }
  
  if (n < ZERO_INT4)
    n2 = -n;
  else
    n2 = n;
  
  for (; l; l--, n >>= 8)
    if (f(1, arg, (b = n % 0x100, &b)))
      return -1;
  
  return 0;
}

int  io__w_char(f, arg, c)
     int  (*f)(uint4, void *, uint1 *);
     void *arg;
     char  c;
{
  return (f(1, arg, (uint1 *)&c));
}

int  io__w_text(f, arg, text)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     char  *text;
{
  if (text && f((uint4)strlen(text), arg, (uint1 *)text))
    return -1;
  
  return 0;
}

int  io__w_bytes(f, arg, l, p)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4  l;
     uint1 *p;
{
  return f(l, arg, p);
}

int  io__r_tag(f, arg, t)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint1 *t;
{
  if (f(1, arg, t))
    return -1;
  
  return 0;
}

int  io__r_text(f, arg, length, text)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4  length;
     char **text;
{
  if (text && f(length, arg, (uint1 *)*text))
    return -1;
  
  (*text)[length] = '\0';

  return 0;
}

int  io__r_bytes(f, arg, n, t)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4  n;
     uint1 *t;
{
  if (f(n, arg, t))
    return -1;
  
  return 0;
}

int  io__r_num(f, arg, n)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     uint4 *n;
{
  uint1  b, l;

  if (f(1, arg, &b))
    return -1;
  
  if (b != 0xFF)
    {
      *n = b;
      return 0;
    }
  else
    {
      uint1  l2 = ZERO_INT4;
      uint4  base = 1;

      *n = ZERO_INT4;

      if (f(1, arg, &l))
	return -1;

      if (!l)
	{
	  lash_errno = LASH_ERR_FILE;
	  return -1;
	}
      
      if (l > 4)
	{
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}

      for (; l2 < l; l2++)
	{
	  if (f(1, arg, &b))
	    return -1;
	  *n += (b * base);
	  base *= 256;
	}

      return 0;
    }
}

int  io__r_snum(f, arg, n)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg;
     sint4 *n;
{
  uint1  b, l;
 
  if (f(1, arg, &b))
    return -1;
  
  if (b < 0xFE)
    {
      *n = b;
      return 0;
    }
  else
    {
      unsigned short int  sign_neg;
      uint1               l2 = ZERO_INT4;
      uint4               base = 1;

      sign_neg = (b == 0xFE) ? 1 : 0;
      *n = ZERO_INT4;

      if (f(1, arg, &l))
	return -1;

      if (!l)
	{
	  lash_errno = LASH_ERR_FILE;
	  return -1;
	}

      /* The following assumes that a sint4 is max. 4 bytes long */
      if (l == 0 || l > 4)
	{
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}

      for (; l2 < l; l2++)
	{
	  if (f(1, arg, &b))
	    return -1;
	  *n += (b * base);
	  base *= 256;
	}

      if (sign_neg && l == 4 && b & (0x01 << 7))
	{
	  lash_errno = LASH_ERR_OVERFLOW;
	  return -1;
	}
      
      if (sign_neg)
	*n *= -1;

      return 0;
    }
}

int  io__r_set(arg, f, farg, n)
     int  (*f)(uint4, void *, uint1 *);
     void  *arg, *farg;
     uint1  n;
{
  uint1      n2 = n;
  set_entry *entries;
  va_list    ap;

  va_start(ap, n);
  
  if (!(entries = resr__new_objects(set_entry, n)))
    {
      lash_errno = LASH_ERR_NO_MEM;
      return -1;
    }
  
  while (n2--)
    {
      entries[n2].tag = (uint1) va_arg(ap, int);
      entries[n2].fnct = va_arg(ap, set_pfn);
      entries[n2].type = (uint1) va_arg(ap, int);
      entries[n2].prop = ZERO_INT1;
    }
  
  va_end(ap);

  for (;;)
    {
      uint1  tag;
      
      if (io__r_tag(f, farg, &tag))
	{
	  resr__free_objects(entries, set_entry, n);
	  return -1;
	}

      if (tag == IO_TAG__END)
	break;
     
      for (n2 = ZERO_INT1; n2 < n; n2++)
	{
	  if (tag == entries[n2].tag)
	    {
	      if (io_setentry_test_property(&entries[n2].type,
					    IO_SET_PROP_UNIQUE) &&
		  io_setentry_test_property(&entries[n2].prop,
					    IO_SET_PROP_FOUND))
		{
		  resr__free_objects(entries, set_entry, n);
		  lash_errno = LASH_ERR_FILE;
		  return -1;
		}
	      else
		{
		  if (entries[n2].fnct(arg, f, farg))
		    {
		      resr__free_objects(entries, set_entry, n);
		      return -1;
		    }
		  
		  io_setentry_set_property(&entries[n2].prop,
					   IO_SET_PROP_FOUND);
		  break;
		}
	    }
	}
      
      /* Unknown tag */
      if (n2 == n)
	{
	  resr__free_objects(entries, set_entry, n);
	  lash_errno = LASH_ERR_FILE;
	  return -1;
	}
    }
  
  for (n2 = ZERO_INT1; n2 < n; n2++)
    {
      if (io_setentry_test_property(&entries[n2].type,
				    IO_SET_PROP_MANDATORY) &&
	  !io_setentry_test_property(&entries[n2].prop,
				     IO_SET_PROP_FOUND))
	{
	  resr__free_objects(entries, set_entry, n);
	  lash_errno = LASH_ERR_FILE;
	  return -1;
	}
    }

  resr__free_objects(entries, set_entry, n); 
  return 0;
}

/****  End of io-ops.c  ****/
