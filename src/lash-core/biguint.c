/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       biguint.c  :  Functions manipulating large unsigned      **/
/**              	integer.                                   **/
/**                                                                **/
/**        07/07/00  :  Creation. (LL)                             **/
/**        08/16/01  :  Corrections. (BB)                          **/
/**        07/02/02  :  Reorganization. (BB)                       **/
/**        05/01/05  :  Add biguint__new. (LL)                     **/
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
#include "biguint.h"
#include "arithmetic.h"
#include "diag.h"

/** biguint *biguint__new_zero() :  Creates a new biguint initialized 
                     to 0. Returns a pointer to this biguint if 
                     successful, and NULL in the case of insufficient 
                     memory.  
                  
                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.    **/
   
biguint *biguint__new_zero()
{
  register biguint *r;

  diag__enter("biguint__new_zero", NULL);

  r = resr__new_object(biguint);
  if (!r)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  r -> alloc_uint4 = r -> nb_uint4 = ZERO_INT4;
  r -> array = NULL;
  r -> is_inf = 0;

  diag__return(r);

}

/** biguint *biguint__new(val) :  Creates a new biguint initialized 
                     to val. Returns a pointer to this biguint if 
                     successful, and NULL in the case of insufficient 
                     memory.  
                  
                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.    **/
   
biguint *biguint__new(val)
     uint4 val;
{
  register biguint *r;
  register uint4   *array;

  diag__enter("biguint__new", NULL);

  r = resr__new_object(biguint);
  if (!r)
    diag__fail(LASH_ERR_NO_MEM, NULL);

  array = resr__new_object(uint4);
  if (!array)
    {
      resr__free_object(r, biguint);
      diag__fail(LASH_ERR_NO_MEM, NULL);      
    }

  *array = val;

  r -> alloc_uint4 = r -> nb_uint4 = 1;
  r -> array = array;
  r -> is_inf = 0;

  diag__return(r);
}


/** int biguint__add(biguint *x, biguint *y, biguint *z) :
                     Adds the two biguint numbers pointed by y and z
                     and stores the result in x. The arguments x, y
                     and z must point to already allocated biguint 
                     objects.

		     Returns 0 in the case of success and -1
		     otherwise (in which case lash_errno is set).
                    
                     Possible error codes:

		         LASH_ERR_NO_MEM   : Not enough memory.
 		         LASH_ERR_CORRUPT  : Corrupt data.         **/

int biguint__add(biguint *x, biguint *y, biguint *z)
{
  register uint4 shared, i, max;
  register uint8 sum, carry;
  register uint1 is_z_bigger;

  diag__enter("biguint__add", -1);

  if (!(x) || !(y) || !(z))
    diag__fail(LASH_ERR_CORRUPT, -1);
  
  if (biguint__is_inf(z) || biguint__is_inf(y))
    diag__return(biguint__setinf(x));
     
  is_z_bigger = z -> nb_uint4 > y -> nb_uint4;
  if (is_z_bigger)
    {
      shared = y -> nb_uint4;      
      max = z -> nb_uint4;
    }
  else
    {
      shared = z -> nb_uint4;
      max = y -> nb_uint4;
    }
  
  if ( x -> alloc_uint4 < max)
    {
      if  (!(x -> array = 
	     (uint4 *) resr__realloc((char *) (x -> array),
				     (max + 1) * sizeof(uint4), 
				     x -> alloc_uint4 * 
                                     sizeof(uint4))))
	    diag__fail(LASH_ERR_NO_MEM, -1);
      
      x -> alloc_uint4 = max + 1;
    }
  
  carry = (uint8) ZERO_INT4;
  for (i = ZERO_INT4; i < shared ; i++)
    {
      sum = (uint8) (y -> array[i]) + (uint8) (z -> array[i]);
      sum = sum + carry;
      x -> array[i] = (uint4) sum; 
      carry = sum >> 32;
    } 
  
  for (i = shared; i < max; i++)
    {
      if (is_z_bigger)
	sum = (uint8) z -> array[i] + carry;
      else
	sum = (uint8) y -> array[i] + carry;
      x -> array[i] = (uint4) sum; 
      carry = sum >> 32;
    } 
      
  if (carry)
    {
      if ( x -> alloc_uint4 < max + 1)
	{ 
	  if (!(x -> array = 
		(uint4 *) resr__realloc((char *) (x -> array),
					(max + 1) * sizeof(uint4), 
					x -> alloc_uint4 * 
					sizeof(uint4))))
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  
	  x -> alloc_uint4 = max + 1;
	}
      
      x -> array[max] = (uint4) carry;
      x -> nb_uint4 = max + 1;
    }
  else
    x -> nb_uint4 = max;
  
  diag__return(0);
}

/** int biguint__add1(biguint *x, biguint *y)  :  Adds 1 to the 
                     biguint number pointed by y and stores the 
                     result in x. The arguments must point to already
                     allocated biguint numbers. Returns 0 in case of
                     success and -1 otherwise (in which case 
                     lash_errno is set).

                     Possible error codes:

 			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/
	
int biguint__add1(biguint *x, biguint *y)
{
  register uint4 i, max;
  register uint8 sum, carry;

  diag__enter("biguint__add1", -1);

  if (!(x) || !(y))
    diag__fail(LASH_ERR_CORRUPT, -1);
  
  if (biguint__is_inf(y))
    diag__return(biguint__setinf(x));
       
  max = y -> nb_uint4 ;
  
  
  if ( x -> alloc_uint4 < max )
    {
      if  (!(x -> array = 
	     (uint4 *) resr__realloc((char *) (x -> array),
				     (max) * sizeof(uint4), 
				     x -> alloc_uint4 * 
                                     sizeof(uint4))))
	diag__fail(LASH_ERR_NO_MEM, -1);
      
      x -> alloc_uint4 = max + 1 ;
    }
  
  carry = (uint8) 1;
  for (i = ZERO_INT4 ; i < max ; i++)
    {
      sum = (uint8) (y -> array[i]); 
      sum = sum + carry;
      x -> array[i] = (uint4) sum; 
      carry = sum >> 32;
    } 
  
  if (carry) 
    {
      if ( x -> alloc_uint4 < max + 1)
	{ 
	  if  (!(x -> array = 
		 (uint4 *) resr__realloc((char *) (x -> array),
					 (max + 1) * sizeof(uint4), 
					 x -> alloc_uint4 * 
                                         sizeof(uint4))))
	    diag__fail(LASH_ERR_NO_MEM, -1);
	  
	  x -> alloc_uint4 = max + 1 ;
	}
      
      x -> array[max] = (uint4) carry;
      x -> nb_uint4 = max + 1;
    }
  else
    x -> nb_uint4 = max;
  
  diag__return(0);
}

/** String denoting infinite biguint number.                       **/

#define BIGUINT__INF_STR  "INF"
#define BIGUINT__INF_LEN  (strlen(BIGUINT__INF_STR))

/** char  *biguint__tostring(biguint *x) : Creates a string denoting
                     the value of the biguint number x expressed in 
                     base 10. Returns a pointer to this string in the
                     case of success and NULL in the case of failure.
                     The memory allocated to the
                     returned string is its length + 1.  
                    
                     Possible error codes:

 			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/

char  *biguint__tostring(biguint *x)
{
  char *s, *out;
  uint4 size_s, index, i;
  biguint *big_tmp;
  uint8 remain;

  diag__enter("biguint__tostring", NULL);

  if (!x)
    diag__fail(LASH_ERR_CORRUPT, NULL);

  if (biguint__is_inf(x))
    {
      if (!( out =  resr__new_objects(char, BIGUINT__INF_LEN + 1)))
	diag__fail(LASH_ERR_NO_MEM, NULL);
      strncpy(out, BIGUINT__INF_STR, BIGUINT__INF_LEN + 1);
      diag__return(out);
    }

  if (biguint__is_zero(x))
    {
      if (!( out =  resr__new_objects(char, 2)))
	diag__fail(LASH_ERR_NO_MEM, NULL);
      strcpy(out, "0");
      diag__return(out);
    }
  
  if (!(big_tmp = biguint__new_zero()))
    diag__return(NULL);
  
  if (biguint__copy(big_tmp, x) < 0)
    diag__return(NULL);

  size_s = (uint4) (32 * x -> nb_uint4 / 3) + 1 ;

  if (!(s = resr__new_objects(char, size_s)))
     diag__fail(LASH_ERR_NO_MEM, NULL);

  index = size_s - 1;
  s[index] = '\0';
  index--;

  for (; !(biguint__is_zero(big_tmp)); index--)
    {
      remain = ZERO_INT4;
      for (i =  big_tmp -> nb_uint4 - 1;  i >= ZERO_INT4;)
	{
	  remain = remain << 32 ;
	  remain = remain  +  (uint8) big_tmp -> array[i]; 
	  big_tmp -> array[i] = (uint4) (remain / (uint8) 10);
	  if ((i == big_tmp -> nb_uint4 -1)
	      && (big_tmp -> array[i] == ZERO_INT4))
	    big_tmp -> nb_uint4--;
	  remain =  remain % 10; 
	  if (i > ZERO_INT4)
	    i--;
	  else
	    break;
	}
      s[index] = '0' + (char) remain;
    }
  
  biguint__free(big_tmp);
  
  if (!(out = resr__new_objects(char,  size_s - index - 1)))
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  strncpy(out, s + index + 1, size_s - index - 1);
  resr__free_objects(s, char, size_s);
  
  diag__return(out);
}

/** biguint__equal(x, y) : Tests whether the biguint object x has
                     the same value as the one pointed by y. 
                     If x = y, then it returns 1. Otherwise, it
                     returns 0.  
		     
                     Does not report errors.                       **/

int biguint__equal(biguint *x, biguint *y)
{
  register uint4 i;

  diag__enter("biguint__equal", -1);

  if ((x -> is_inf) && ( y -> is_inf))
    diag__return(1);
  
  if ((x -> is_inf) || ( y -> is_inf))
    diag__return(0);
		     
  if (x -> nb_uint4 != y -> nb_uint4)
    diag__return(0);
  
  for ( i = ZERO_INT4 ; i < x -> nb_uint4 ; i++)
    if (x -> array[i] != y -> array[i])
      diag__return(0);
 
  diag__return(1);
}

/** biguint__is_zero(x) : Tests whether x is equal to 0.
			  If x = 0, then it returns 1. Otherwise,
			  it returns 0.          

			  Does not report errors.                  **/

int biguint__is_zero(biguint *x)
{
  diag__enter("biguint__is_zero", -1);

  if (x -> is_inf)
    {
      
      diag__return(0);
    }
  diag__return(x -> nb_uint4  == ZERO_INT4);
}

/** int biguint__setzero(x) : Sets the value of x  to 0.
			  Returns 0 in the case of success, and -1
			  in the case of failure.                 
			  Possible error codes:

			  LASH_ERR_CORRUPT : Corrupt biguint.      **/

int biguint__setzero(biguint *x)
{
  diag__enter("biguint__setzero", -1);

  if (!x)
    diag__fail(LASH_ERR_CORRUPT, -1);

  x -> nb_uint4 = ZERO_INT4;
  x -> is_inf = 0; 
  diag__return(0);
}

/** biguint__is_inf(x) : Tests whether x represents an infinite 
                     value. If x is  infinite, then it returns 1. 
                     Otherwise, it returns 0.                      **/

int biguint__is_inf(biguint *x)
{

  diag__enter("biguint__is_inf", -1);

  diag__return(x -> is_inf);
}

/** int biguint__setinf(x) : Sets the value of x to infinity.
		     Returns 0 in the case of success, and -1
		     in the case of failure.
		     
                     Possible error codes:

		         LASH_ERR_CORRUPT : Corrupt biguint.       **/
                 

int biguint__setinf(biguint *x)
{
  diag__enter("biguint__setinf", -1);

  if (!x)
    diag__fail(LASH_ERR_CORRUPT, -1);

  x -> is_inf = 1;

  diag__return(0);
}

/** int biguint__copy(x, y)  : Sets x equal to y.
                     x and y have to point to valid biguint objects
		     before the call. In the case of success, 
		     the function returns 0. In the case of failure,
		     it returns -1.                            

		     Possible error codes:
			 
			 LASH_ERR_NO_MEM   : Not enough memory.
                         LASH_ERR_CORRUPT  : Corrupt data.         **/
	      
int biguint__copy(biguint *x, biguint *y)
{
  diag__enter("biguint__copy", -1);

  if ((!x) || (!y))
    diag__fail(LASH_ERR_CORRUPT , -1);

  if (biguint__is_inf(y))
    {
      biguint__setinf(x);
      diag__return(0);
    }

  if (biguint__is_inf(x))
    biguint__setzero(x);
    
  if (x -> alloc_uint4 < y -> nb_uint4)
    {
      if  (!(x -> array = 
	     (uint4 *) resr__realloc((char *) (x -> array),
				     y -> nb_uint4  * sizeof(uint4), 
				     x -> alloc_uint4 * 
				     sizeof(uint4))))
	diag__fail(LASH_ERR_NO_MEM, -1);

      x -> alloc_uint4 = y -> nb_uint4;
    }
  
  x -> nb_uint4 = y -> nb_uint4 ;
  memcpy(x -> array, y -> array, y -> nb_uint4 * sizeof(uint4));

  diag__return(0);
}

/** void biguint__free(x)  : Frees the memory allocated for x.     **/

void biguint__free(biguint *x)
{
  if (!x)
    return ;
  
  resr__free_objects(x -> array, uint4, x -> alloc_uint4); 
  resr__free_object(x, biguint);
  
  return;
}

/****  End of biguint.c  ****/
