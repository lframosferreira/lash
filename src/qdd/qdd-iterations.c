/********************************************************************/
/**                                                                **/
/**  Queue Decision Diagrams -- v0.9                               **/
/**  =======================                                       **/
/**                                                                **/
/**    qdd-iterations.c  :  Computation of the effect of the       **/
/**                 closure of a sequence of operations on a QDD.  **/
/**                                                                **/
/**    12/17/99  :  Creation. (JMF)                                **/
/**    02/14/00  :  Modification of qdd_meta. (JMF)                **/
/**    02/16/00  :  Continued. (JMF)                               **/
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

#include "lash-types.h"
#include "diag.h"
#include "resource.h"
#include "arithmetic.h"
#include "qdd.h"
#include "queue-operations.h"
#include "qdd-iterations-utils.h"
#include "qdd-iterations.h"

/****  Private data.                                             ****/

/* Used in epsilon_queue.  Use of border effects is unavoidable
   because of the way qdd_apply_homomorphism is defined.            */

static queue         eps_queue; 
static queue_symbol *newlabel;
static uint4         newlabel_size;       
#define  NEWLABEL_GROWTH_QUANTUM  (0x10)

/****  Prototypes of private functions.                          ****/

static int  erase_queue(qdd *, queue);
static int  epsilon_queue(uint4, uint1 *, uint4 *, uint1 **, uint1,
			  uint1);
/****  Private functions.                                        ****/

/**  int  epsilon_queue(n, ptr, rn, rptr, na1, na2)  :  Modifies
                     the transition label *ptr (which holds
		     n symbols of size na1) to another label
		     **rptr (which holds *rn symbols of 
		     size na2).

		     The new label is the same as the one given
		     as an argument, except that every symbol
		     involving the queue eps_queue is removed.

		     Returns 0 if successful, -1 and sets lash_errno
		     in the case of an error.

		     Possible error code:
 
                         LASH_ERR_NO_MEM     : Not enough memory.  **/

static int  epsilon_queue(n, ptr, rn, rptr, na1, na2)
     uint4  n, *rn;
     uint1 *ptr, **rptr, na1, na2;
{
  uint4  cur_symb, rnb_symb = ZERO_INT4;

#if LASH_CHECK_LEVEL >= 2
  if (na2 != qdd_alphabet_nbytes)
    return -1;
#endif  /* >= 2 */

  for (cur_symb = ZERO_INT4; cur_symb < n; cur_symb++)
    {
      queue_symbol *cur_qm = ((queue_symbol *) ptr) + cur_symb;
      queue_symbol *newlabel_tmp;

      if (queuesymbol_queue(cur_qm) != eps_queue)
	{
	  if (++rnb_symb > newlabel_size)
	    {
	      if (!(newlabel_tmp =
		    resr__resize_objects(newlabel, queue_symbol,
					 newlabel_size +
					 NEWLABEL_GROWTH_QUANTUM,
					 newlabel_size)))
		{
		  lash_errno = LASH_ERR_NO_MEM;
		  return -1;
		}

	      newlabel = newlabel_tmp;
	      newlabel_size += NEWLABEL_GROWTH_QUANTUM;
	    }
	  
	  newlabel[rnb_symb-1] = *cur_qm;
	}
    }

  *rn = rnb_symb;
  *rptr = (uint1 *) newlabel;

  return 0;
}

/**  int  erase_queue(q, i)  :  Removes the symbols labelling the
                     transitions of the QDD *q and involving the
		     queue i.

		     If i == QUEUE_UNK_QUEUE, *q is not modified.

		     In the case of error, returns -1 and sets
		     lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt data.       **/

static int  erase_queue(q, i)
     qdd     *q;
     queue    i;
{
#if LASH_CHECK_LEVEL >= 2
  if (!q)
    {
      lash_errno = LASH_ERR_CORRUPT;
      return -1;
    }
#endif  /* >= 2 */

  if (i == QUEUE_UNK_QUEUE)
    return 0;

  eps_queue = i;

  if (qdd_apply_homomorphism(q, epsilon_queue, qdd_alphabet_nbytes) ||
      qdd_minimize(q))
    return -1;

  return 0;
}

/****  Public functions.                                         ****/

/**  int  qdd_meta(q, s, c)  :  Returns 1 if the closure of the
                     sequence s can always be applied to arbitrary set
		     of queue-set contents represented by the QDD q,
		     0 if it cannot.

		     If qdd_meta returns 1 and c is not NULL, sets *c
		     to the queue number such that *s|*c (the
		     projection of *s on the *c-th queue) is counting
		     (if such a queue exists) or to QUEUE_UNK_QUEUE
		     (if such a sequence doesn't exist).

		     In the case of error, returns -1 and sets
		     lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_NO_MEM     : Not enough memory.
			 LASH_ERR_CORRUPT    : Corrupt data.       **/

int  qdd_meta(q, s, c)
     qdd               *q;
     queue_op_sequence *s;
     queue             *c;
{
  uint1               queue_nb;
  unsigned short int  j = 0;
  uint4              *nb_send, *nb_receive, op_nb;
  register uint1      nb_queues;
  queue               counting = QUEUE_UNK_QUEUE;

  diag__enter("qdd_meta", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !s)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  nb_queues = qdd_nb_queues(q);

  if (!(nb_send = resr__new_objects(uint4, nb_queues)))
      diag__fail(LASH_ERR_NO_MEM, -1);
 
  if (!(nb_receive = resr__new_objects(uint4, nb_queues)))
    {
      resr__free_objects(nb_send, uint4, nb_queues);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }

  for (queue_nb = ZERO_INT1; queue_nb < nb_queues; queue_nb++)
      nb_send[queue_nb] = nb_receive[queue_nb] = ZERO_INT4;
 
  for (op_nb = ZERO_INT1; op_nb < nb_queues; op_nb++)
    {
      queue_operation *cur_op = queue_op_sequence_element(s, op_nb);

      if (queue_operation_issend(cur_op))
	nb_send[queue_operation_queue(cur_op)]++;
      else
	if (queue_operation_isreceive(cur_op))
	  nb_receive[queue_operation_queue(cur_op)]++;
    }
  
  for (queue_nb = ZERO_INT1; queue_nb < nb_queues; queue_nb++)
    if ((qdd_queue_alphabet_size(q, queue_nb) > 1
	 && nb_send[queue_nb])
	|| (qdd_queue_alphabet_size(q, queue_nb) == 1
	    && nb_send[queue_nb] > nb_receive[queue_nb]))
      {
	if (!j)
	  {
	    counting = queue_nb;
	    j++;
	  }
	else
	  diag__return(0);
      }
  
  resr__free_objects(nb_send, uint4, nb_queues);
  resr__free_objects(nb_receive, uint4, nb_queues);

  if (c)
    *c = counting;

  diag__return(1);
}

/**  int  qdd_closure(q, seq)  :  Computes the effect of the closure
                     of the operation sequence *seq on the QDD *q.

		     Returns 0 if successful; else returns -1 and
		     sets lash_errno.

		     Possible error codes:
		     
		        LASH_ERR_NOT_INIT   : Not initialized.
		        LASH_ERR_CORRUPT    : Corrupt data.
			LASH_ERR_OVERFLOW   : Arithmetic overflow.
			LASH_ERR_PROP       : Automaton with wrong
			                      known properties.   **/

int  qdd_closure(q, seq)
     qdd               *q;
     queue_op_sequence *seq;
{ 
  queue    i;
  p_table *qddtable;
  uint4    b, j = ZERO_INT4;
  qdd     *a_prime;

  diag__enter("qdd_closure", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q || !seq)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  switch(qdd_meta(q, seq, &i))
    {
    case -1:
      diag__fail(lash_errno, -1);
      
    case 0:
      diag__fail(LASH_ERR_ITERATION, -1);
    }

  if (!sequence_iscommunicating(seq))
    diag__return(0);

  if (!(newlabel =
	resr__new_objects(queue_symbol,
			  newlabel_size = NEWLABEL_GROWTH_QUANTUM)))
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (!(qddtable = p_table__new_empty()))
    {
      resr__free_objects(newlabel, queue_symbol, newlabel_size);
      diag__fail(LASH_ERR_NO_MEM, -1);
    }
  
  if (!(a_prime = qdd_copy(q)))
    {
      resr__free_objects(newlabel, queue_symbol, newlabel_size);
      p_table__free(qddtable, (int (*)(void *))qdd_free);
      diag__fail(lash_errno, -1);
    }

  {
    qdd *qcopy;
    
    if (!(qcopy = qdd_copy(q)))
      {
	qdd_free(a_prime);
	resr__free_objects(newlabel, queue_symbol, newlabel_size);
	p_table__free(qddtable, (int (*)(void *))qdd_free);
	diag__fail(lash_errno, -1);
      }
   
    if (p_table__add(qddtable, qcopy) ||
	erase_queue(qcopy, i))
      {
	qdd_free(a_prime);
	resr__free_objects(newlabel, queue_symbol, newlabel_size);
	p_table__free(qddtable, (int (*)(void *))qdd_free);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
  }

  for(;;)
    {
      qdd *next_qdd;
      int  f;

      if (uint4__add(&j, j, 1))
	{
	  qdd_free(a_prime);
	  resr__free_objects(newlabel, queue_symbol, newlabel_size);
	  p_table__free(qddtable, (int (*)(void *))qdd_free);
	  diag__fail(LASH_ERR_OVERFLOW, -1);
	}

      if (qdd_apply(a_prime, seq))
	{
	  qdd_free(a_prime);
	  resr__free_objects(newlabel, queue_symbol, newlabel_size);
	  p_table__free(qddtable, (int (*)(void *))qdd_free);
	  diag__fail(lash_errno, -1);
	}

      if (!(next_qdd = qdd_copy(a_prime)))
	{
	  qdd_free(a_prime);
	  resr__free_objects(newlabel, queue_symbol, newlabel_size);
	  p_table__free(qddtable, (int (*)(void *))qdd_free);
	  diag__fail(lash_errno, -1);
	}

      if (erase_queue(next_qdd, i))
	{
	  qdd_free(next_qdd);
	  qdd_free(a_prime);
	  resr__free_objects(newlabel, queue_symbol, newlabel_size);
	  p_table__free(qddtable, (int (*)(void *))qdd_free);
	  diag__fail(lash_errno, -1);
	}

      if ((f = p_table__find(qddtable, next_qdd,
			     (int (*)(void *, void *))qdd_equality, 
			     &b)))
	{
	  if (f == 1)
	    {
	      if (qdd_free(next_qdd))
		{
		  qdd_free(a_prime);
		  resr__free_objects(newlabel, queue_symbol, 
				     newlabel_size);
		  p_table__free(qddtable, (int (*)(void *))qdd_free);
		  diag__fail(lash_errno, -1);
		}

	      break;
	    }
	  else
	    {
	      qdd_free(next_qdd);
	      qdd_free(a_prime);
	      resr__free_objects(newlabel, queue_symbol, 
				 newlabel_size);
	      p_table__free(qddtable, (int (*)(void *))qdd_free);
	      diag__fail(lash_errno, -1);
	    }
	}

      if (p_table__add(qddtable, next_qdd))
	{
	  qdd_free(next_qdd);
	  qdd_free(a_prime);
	  resr__free_objects(newlabel, queue_symbol, newlabel_size);
	  p_table__free(qddtable, (int (*)(void *))qdd_free);
	  diag__fail(lash_errno, -1);
	}
    }

  resr__free_objects(newlabel, queue_symbol, newlabel_size);

  if (qdd_free(a_prime))
    {
      p_table__free(qddtable, (int (*)(void *))qdd_free);
      diag__fail(lash_errno, -1);
    }
  
  if (i == QUEUE_UNK_QUEUE)
    { /* No counting projection of *seq found */
      register uint4  n = p_table__nb_elements(qddtable), k;
      
      for (k = 1; k < n; k++)
	if (qdd_merge(q, (qdd *)p_table__element(qddtable, k)))
	  {
	    p_table__free(qddtable, (int (*)(void *))qdd_free);
	    diag__fail(lash_errno, -1);
	  }
	
      if (p_table__free(qddtable, (int (*)(void *))qdd_free))
	diag__fail(lash_errno, -1);

      diag__return(0);
    }

  if (p_table__free(qddtable, (int (*)(void *))qdd_free))
    diag__fail(lash_errno, -1);

  { /* Counting projection of *seq found */
    uint4              p = j - b;
    queue_op_sequence *seqproj;

    if (!(a_prime = qdd_copy(q)))
      diag__fail(LASH_ERR_NO_MEM, -1);

    if (!(seqproj = queue_op_sequence_copy(seq)))
      {
	qdd_free(a_prime);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
    
    queue_op_sequence_repeat(seqproj, b);
    
    if (qdd_apply(a_prime, seq))
      {
	qdd_free(a_prime);
	queue_op_sequence_free(seqproj);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
    
    queue_op_sequence_free(seqproj);
    
    if (!(seqproj = queue_op_sequence_new()))
      {
	qdd_free(a_prime);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
    
    if (sequence_project_queue(seq, i, seqproj))
      {
	queue_op_sequence_free(seqproj);
	qdd_free(a_prime);
	diag__fail(LASH_ERR_NO_MEM, -1);
      }
    
    queue_op_sequence_repeat(seqproj, p);

    if (qdd_perform_fnct(a_prime, i,
			 (int (*)(qdd *, void *))qdd_one_closure,
			 (void *)seqproj))
      {
	queue_op_sequence_free(seqproj);
	qdd_free(a_prime);
	diag__fail(lash_errno, -1);
      }

    queue_op_sequence_free(seqproj);

    if (repeated_union_seq(q, seq, b, 0) ||
	repeated_union_seq(a_prime, seq, p, 0) ||
	qdd_merge(q, a_prime))
      {
	qdd_free(a_prime);
	diag__fail(lash_errno, -1);
      }

    if (qdd_free(a_prime))
      diag__fail(lash_errno, -1);
    
    diag__return(0);
  }
}

/****  End of qdd-iterations.c  ****/
