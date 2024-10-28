/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**     queue-operations.h : Visible data structures for           **/
/**	             symbols and queue operations                  **/
/**                                                                **/
/**     04/22/98  :  Creation. (GC)                                **/
/**     10/15/99  :  Upgraded. (JMF)                               **/
/**     12/07/99  :  Reorganisation (creation of symbol.*). (JMF)  **/
/**     12/15/99  :  Creation of sequences of operations. (JMF)    **/
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

#ifndef LASH_QUEUE_OPERATIONS_H
#define LASH_QUEUE_OPERATIONS_H

#include "qdd-symbol.h"

/**  Queue operation & sequences of such operations                **/

typedef struct {
  uint1         type;
  queue_symbol  qm;
} queue_operation;

typedef struct {
  uint4            length;          /* Total length of the sequence */
  queue_operation *sequence;
  uint4            rep;         /* The sequence is repeated rep times
				   This field can't be ZERO_INT4ed. */
} queue_op_sequence;


/**  Definitions for queue_operation.type.                         **/

#define QUEUE_OPERATION_COMMUNICATING 0x01  /* a communicating op.  */
#define QUEUE_OPERATION_SEND          0x03  /* a send op.           */
#define QUEUE_OPERATION_RECEIVE       0x05  /* a receive op.        */
#define QUEUE_OPERATION_INTERNAL      0x08  /* an internal op.      */


/**  Function prototypes.                                          **/

queue_operation   *queue_op_new(uint1, ...);
void               queue_op_free(queue_operation *);
queue_op_sequence *queue_op_sequence_new(void);
void               queue_op_sequence_free(queue_op_sequence *);
queue_op_sequence *queue_op_sequence_copy(queue_op_sequence *);
int                queue_operation_cmp(queue_operation *, 
				       queue_operation *);
int                queue_op_sequence_set(queue_op_sequence *, uint4,
					 queue_operation *);
int                queue_op_sequence_add(queue_op_sequence *,
					 queue_operation *);
void               queue_op_sequence_repeat(queue_op_sequence *,
					    uint4);
int                queue_op_sequence_divide(queue_op_sequence *,
					    uint4);
int                sequence_iscommunicating(queue_op_sequence *);
void               sequence_count_type(queue_op_sequence *, uint4 *,
				       uint4 *);
uint4              sequence_count_queue(queue_op_sequence *, uint1);
void               sequence_project_type(queue_op_sequence *,
					 queue_symbol *,
					 queue_symbol *,
					 queue_symbol *);
int                sequence_project_queue(queue_op_sequence *,
					  queue,
					  queue_op_sequence *);

/**  Macro queue_op_sequence_firstelement(seq)  :  Returns a pointer
                     to the first operation of the operation sequence
		     seq                                           **/

#define  queue_op_sequence_firstelement(seq) \
                     ((queue_op_sequence_element(seq, ZERO_INT4)))

/**  Macro queue_op_sequence_element(seq, n)  :  Returns a pointer to
                     the n-th operation of the operation sequence
	             seq (n = 0, ...)                              **/

#define  queue_op_sequence_element(seq, n) \
     ((((seq) -> sequence) + \
       (n % (queue_op_sequence_length(seq) / (seq) -> rep))))

/**  Macro queue_op_sequence_length(seq)  :  Returns the length of a
                     the given queue_op_sequence *qop              **/

#define  queue_op_sequence_length(seq) ((seq) -> length)

/**  Macro  queue_operation_set_internal(op)  :  Sets the type of the
                     queue operation *op to
		     QUEUE_OPERATION_INTERNAL                      **/

#define  queue_operation_set_internal(op) \
     ((op) -> type = QUEUE_OPERATION_INTERNAL)

/**  Macro  queue_operation_set_send(op)  :  Sets the type of the
                     queue operation *op to QUEUE_OPERATION_SEND   **/

#define  queue_operation_set_send(op) ((op) -> type = \
				       QUEUE_OPERATION_SEND)


/**  Macro  queue_operation_set_receive(op)  :  Sets the type of the
                     queue operation *op to QUEUE_OPERATION_RECEIVE
                                                                   **/

#define  queue_operation_set_receive(op) \
     ((op) -> type = QUEUE_OPERATION_RECEIVE)


/**  Macro  queue_operation_iscommunicating(op)  :  Returns 1 if *op
                     is a communicating operation (i.e. a send or a
		     receive operation). Returns 0 otherwise.      **/

#define  queue_operation_iscommunicating(op) \
     (!!((op) -> type & QUEUE_OPERATION_COMMUNICATING))


/**  Macro  queue_operation_isinternal(op)  :  Returns 1 if *op is an
                     internal operation. Returns 0 otherwise.      **/

#define  queue_operation_isinternal(op) \
     (!((op) -> type & QUEUE_OPERATION_COMMUNICATING))

/**  Macro  queue_operation_issend(op)  :  Returns 1 if *op is a send 
                     communicating operation. Returns 0 otherwise. **/

#define  queue_operation_issend(op) \
     (!!((op) -> type == QUEUE_OPERATION_SEND))


/**  Macro  queue_operation_isreceive(op)  :  Returns 1 if *op is a
                     receive communicating operation. Returns 0
		     otherwise.                                    **/

#define  queue_operation_isreceive(op) \
     (!!((op) -> type == QUEUE_OPERATION_RECEIVE))

/**  Macro  queue_operation_qm(op)  :  Returns the queue_symbol
                     corresponding to the operation *op.           **/

#define  queue_operation_qm(op)  ((op) -> qm)


/**  Macro  queue_operation_symbol(op)  :  Returns the symbol stored
                     in the operation *op.                         **/

#define  queue_operation_symbol(op)  (((op) -> qm).symbol)


/**  Macro  queue_operation_queue(op)  :  Returns the queue
                     involved by the operation *op.                **/

#define  queue_operation_queue(op)  (((op) -> qm).queue)



/**  Macro  queue_operation_type(op)  :  Returns the type of operation
                     of the operation *op.                         **/

#define  queue_operation_type(op)  ((op) -> type)


#endif  /* LASH_QUEUE_OPERATIONS_H */

/****  End of queue-operations.h  ****/
