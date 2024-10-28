/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**     queue-operations.c : Visible data structures for           **/
/**	             symbols and queue operations.                 **/
/**                                                                **/
/**     04/22/98  :  Creation. (GC)                                **/
/**     10/15/99  :  Upgraded. (JMF)                               **/
/**     12/07/99  :  Reorganisation (creation of symbol.*). (JMF)  **/
/**     12/15/99  :  Creation of sequences of operations. (JMF)    **/
/**     05/15/01  :  Minor correction. (BB)                        **/
/**     07/08/02  :  Reorganization. (BB)                          **/
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
#include <stdarg.h>
#include <string.h>
#include "diag.h"
#include "resource.h"
#include "arithmetic.h"
#include "queue-operations.h"

/****  Public functions.                                         ****/

/**  queue_operation *queue_op_new(t, ...)  :  Returns a
                     new queue operation of type t.  If t is not
		     'QUEUE_OPERATION_COMMUNICATING', then 2 other
		     arguments must be given; the first is the queue
		     number of the operation (0, 1, ...) and the
		     second and the second is the symbol involved in
		     the operation.  Returns NULL in the case of
		     insufficient memory.                          **/

queue_operation *queue_op_new(uint1 t, ...)
{
  queue_operation *op;
  va_list          ap;

  va_start(ap, t);
  
  if (!(op = resr__new_object(queue_operation)))
    return NULL;

  if ((op -> type = t) != QUEUE_OPERATION_COMMUNICATING)
    {
      op -> qm.queue = va_arg(ap, int);
      op -> qm.symbol = va_arg(ap, int);
    }
 
  va_end(ap);

  return(op);
}

/**  void queue_op_free(op)  :  Frees the operation *op.           **/

void queue_op_free(op)
     queue_operation *op;
{
  resr__free_object(op, queue_operation);

  return;
}

/**  int queue_operation_cmp(p1, p2)  :  Returns 0 if queue 
                     operations *p1 and *p2 are identical, 1 if they 
		     are not.                                      **/

int queue_operation_cmp(p1, p2)
     queue_operation *p1, *p2;
{
#if LASH_CHECK_LEVEL >= 1
  if (!p1 || !p2)
    return -1;
#endif   /* >= 1 */

  return (p1 -> type != p2 -> type) ? 1 : qdd_qm_cmp(&(p1 -> qm), 
						     &(p2 -> qm));
}

/** queue_op_sequence *queue_op_sequence_new()  :  Returns a new,
                     empty queue_op_sequence.

		     Returns a pointer to the newly allocated sequence
		     if successful, NULL if there is not enough 
		     available memory.                             **/

queue_op_sequence *queue_op_sequence_new()
{
  queue_op_sequence *op;

  if (!(op = resr__new_object(queue_op_sequence)))
    return NULL;

  op -> length = ZERO_INT4;
  op -> rep    = 1;

  return(op);
}

/** void queue_op_sequence_free(op)  :  Frees the sequence *op.    **/

void queue_op_sequence_free(op)
     queue_op_sequence *op;
{
#if LASH_CHECK_LEVEL >= 2
  if (!op)
    return;
#endif   /* >= 2 */

  if (op -> length)
    resr__free_objects(op -> sequence, queue_operation,
		       op -> length / op -> rep);

  resr__free_object(op, queue_op_sequence);

  return;
}

/**  queue_op_sequence *queue_op_sequence_copy(seq)  :  Copies the
                     queue operation sequence *seq.

		     Returns the new sequence if successful, or NULL
		     in the case of an insufficiant memory.        **/

queue_op_sequence *queue_op_sequence_copy(seq)
     queue_op_sequence *seq;
{
  queue_op_sequence *rv;

#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return NULL;
#endif   /* >= 2 */

  if (!(rv = resr__new_object(queue_op_sequence)))
    return NULL;
  
  if (seq -> sequence)
    {    
      if (!(rv -> sequence =
	    resr__new_objects(queue_operation,
			      seq -> length / seq -> rep)))
	{
	  resr__free_object(rv, queue_op_sequence);
	  return NULL;
	}

      memcpy(rv -> sequence, seq -> sequence,
	     (seq -> length / seq -> rep) * sizeof(queue_operation));
    }
  else
    rv -> sequence = NULL;

  rv -> length = seq -> length;
  rv -> rep = seq -> rep;

  return rv;
}

/** int queue_op_sequence_set(seq, nb_ops, ops)  :  Initialize the
                     sequence *seq with the nb_ops operations of the
		     array ops.  The previous content of seq (if any)
		     if freed.

		     Returns 0 if successful, -1 if there is not
		     enough memory.                                **/

int queue_op_sequence_set(seq, nb_ops, ops)
     queue_op_sequence *seq;
     uint4              nb_ops;
     queue_operation   *ops;
{
#if LASH_CHECK_LEVEL >= 1
  if (!seq || !ops)
    return -1;
#endif   /* >= 1 */
  
  if (seq -> length)
    {
      if (nb_ops)
	seq -> sequence =
	  resr__resize_objects(seq -> sequence, queue_operation,
			       nb_ops, seq -> length / seq -> rep);
      else
	resr__free_objects(seq -> sequence, queue_operation,
			   seq -> length / seq -> rep);
    }
  else
    if (nb_ops)
      seq -> sequence = resr__new_objects(queue_operation, nb_ops);
  
  seq -> rep = 1;

  if (nb_ops && !seq -> sequence)
    {
      seq -> length = ZERO_INT4;
      return -1;
    }
  
  seq -> length = nb_ops;
  memcpy(seq -> sequence, ops, nb_ops * sizeof(queue_operation));
  
  return 0;
}

/** int queue_op_sequence_add(seq, op)  :  Appends the operation *op
                     to the sequence *seq.
		     
		     Return 0 if successful, -1 if there is not
		     enough memory.                                **/

int queue_op_sequence_add(seq, op)
     queue_op_sequence *seq;
     queue_operation   *op;
{
#if LASH_CHECK_LEVEL >= 1
  if (!seq || !op)
    return -1;
#endif   /* >= 1 */
  
  if (!(seq -> sequence = (seq -> length) ?
	resr__resize_objects(seq -> sequence, queue_operation,
			     seq -> length + 1,
			     seq -> length / seq -> rep) :
	resr__new_objects(queue_operation, seq -> length + 1)))
    return -1;

  if (seq -> rep > 1 && seq -> length)
    {
      uint4  replength = seq -> length / seq -> rep;
      
      for (; seq -> rep > 1; seq -> rep--)
	memcpy((seq -> sequence) + (seq -> rep - 1) * replength,
	       seq -> sequence,
	       sizeof(queue_operation) * replength);
    }
  else
    seq -> rep = 1;

  memcpy((seq -> sequence) + (seq -> length++), op,
	 sizeof(queue_operation));
  
  return 0;
}

/**  void queue_op_sequence_repeat(seq, r)  :  Repeats the sequence
                     *seq r times.                                 **/

void queue_op_sequence_repeat(seq, r)
     queue_op_sequence *seq;
     uint4              r;
{
#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return;
#endif   /* >= 2 */

  if (!r)
    {
      if (seq -> length)
	{
	  resr__free_objects(seq -> sequence, queue_operation,
			     seq -> length / seq -> rep);
	  seq -> sequence = NULL;
	  seq -> length = ZERO_INT4;
	}
      
      seq -> rep = 1;

      return;
    }

  seq -> rep *= r;
  seq -> length *= seq -> rep;

  return;
}

/**  int  sequence_iscommunicating(seq)  :  Returns 1 if the sequence
                     *seq is communicating, else returns 0.        **/

int  sequence_iscommunicating(seq)
     queue_op_sequence *seq;
{
  uint4  nb_send, nb_receive;

#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return 0;
#endif   /* >= 2 */

  sequence_count_type(seq, &nb_send, &nb_receive);

  return (nb_send || nb_receive);
}

/**  void  sequence_count(seq, nb_send, nb_receive)  :  Counts the
                     number of send and receive operations in the
		     sequence of queue operations *seq.  The results
		     are put in nb_send and nb_receive, respectively.
		     If seq is NULL, the function does nothing.    **/

void  sequence_count_type(seq, nb_send, nb_receive)
     queue_op_sequence *seq;
     uint4             *nb_send, *nb_receive;
{
  register uint4  cur_op_nb, op_seq_length;

#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return;
#endif   /* >= 2 */
  
  op_seq_length = seq -> length / seq -> rep;

  if (nb_send)
    (*nb_send) = ZERO_INT4;

  if (nb_receive)
    (*nb_receive) = ZERO_INT4;

  for (cur_op_nb = ZERO_INT4; cur_op_nb < op_seq_length; cur_op_nb++)
    {
      register queue_operation *cur_op = seq -> sequence + cur_op_nb;

      if (nb_send && queue_operation_issend(cur_op))
	(*nb_send) += seq -> rep;
      else
	if (nb_receive && queue_operation_isreceive(cur_op))
	  (*nb_receive) += seq -> rep;
    }

  return;
}

/**  uint4 sequence_count_queue(seq, qu)  :  Returns the number of
                     operations in the sequence *seq involving the
		     qu-th queue.                                  **/

uint4 sequence_count_queue(seq, qu)
     queue_op_sequence *seq;
     uint1              qu;
{
  register uint4  cur_op, op_seq_length, retval = ZERO_INT4;

#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return ZERO_INT4;
#endif   /* >= 2 */

  op_seq_length = seq -> length / seq -> rep;

  for (cur_op = ZERO_INT4; cur_op < op_seq_length; cur_op++)
    if (!queue_operation_isinternal(seq -> sequence + cur_op) &&
	queue_operation_queue(seq -> sequence + cur_op) == qu)
      retval += seq -> rep;

  return retval;
}

/**  void  sequence_project(seq, w_all, w_send, w_receive)  :  Copies
                     the queue_symbol of sequence *seq's (non-
		     internal) components to the array w_all (if not
		     NULL), the sent one to w_send (if not NULL),
		     the received one to w_receive (if not NULL).  **/

void  sequence_project_type(seq, w_all, w_send, w_receive)
     queue_op_sequence *seq;
     queue_symbol      *w_all, *w_send, *w_receive;
{
  register uint4  cur_op_nb, op_seq_length;

#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return;
#endif   /* >= 2 */
  
  op_seq_length = seq -> length;
  
  for (cur_op_nb = ZERO_INT4; cur_op_nb < op_seq_length; cur_op_nb++)
    {
      register queue_operation *cur_op = 
	queue_op_sequence_element(seq, cur_op_nb);
      
      if (w_all && !queue_operation_isinternal(cur_op))
	*w_all++ = queue_operation_qm(cur_op);

      if (w_send && queue_operation_issend(cur_op))
	*w_send++ = queue_operation_qm(cur_op);
      else
	if (w_receive && queue_operation_isreceive(cur_op))
	  *w_receive++ = queue_operation_qm(cur_op);
    }
  
  return;
}

/**  void  sequence_project_queue(seq, qu, seqproj)  :  Appends
                     to the empty sequence *seqproj the operations of
		     *seq involving the qu-th queue.

		     If qu == QUEUE_UNK_QUEUE, the entire sequence
		     is appended.

		     Returns 0 if successful, -1 if there is not
		     enough available memory.                      **/

int sequence_project_queue(seq, qu, seqproj)
     queue_op_sequence *seq, *seqproj;
     queue              qu;
{
  register uint4  cur_op_nb, op_seq_length;

#if LASH_CHECK_LEVEL >= 2
  if (!seq || !seqproj || seqproj -> length)
    return -1;
#endif  /* >= 2 */
  
  seqproj -> rep = 1;

  op_seq_length = seq -> length / seq -> rep;
  
  for (cur_op_nb = ZERO_INT4; cur_op_nb < op_seq_length; cur_op_nb++)
    {
      register queue_operation *cur_op = 
	seq -> sequence + cur_op_nb;
      
      if ((queue_operation_queue(cur_op) == qu ||
	   qu == QUEUE_UNK_QUEUE) &&
	  queue_op_sequence_add(seqproj, cur_op))
	return -1;
    }

  if (!seqproj -> length)
    seqproj -> rep = 1;
  else
    queue_op_sequence_repeat(seqproj, seq -> rep);
  
  return 0;
}

/**  int  queue_op_sequence_divide(seq, d)  :  Frees the end of the
                     sequence *seq; the remaining sequence holds
		     |*seq| / d operations (the greatest integer
		     smaller than the length of *seq divided by d).

		     Returns 0 if successful, -1 if there is not
		     enough available memory.                      **/

int  queue_op_sequence_divide(seq, d)
     queue_op_sequence *seq;
     uint4              d;
{
  uint4            newlength, cur_op;
  queue_operation *newseq = NULL;
  
#if LASH_CHECK_LEVEL >= 2
  if (!seq)
    return -1;
#endif   /* >= 2 */

  if (!seq -> length)
    return 0;

  if (d && !(seq -> rep % d))
    {
      seq -> rep /= d;
      seq -> length /= d;
      
      return 0;
    }

  if (d)
    newlength = seq -> length / d;
  else
    newlength = ZERO_INT4;

  if (newlength)
    {
      if (!(newseq = resr__new_objects(queue_operation, newlength)))
	return -1;
      
      for (cur_op = ZERO_INT4; cur_op < newlength; cur_op++)
	newseq[cur_op] =
	  seq -> sequence[cur_op % (seq -> length / seq -> rep)];
    }
  
  resr__free_objects(seq -> sequence, queue_operation,
		     seq -> length / seq -> rep);

  seq -> sequence = newseq;
  seq -> length = newlength;
  seq -> rep = 1;

  return 0;
}

/****  End of queue-operations.c  ****/
