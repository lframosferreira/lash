/********************************************************************/
/**                                                                **/
/**   Queue Decision Diagrams -- v0.9                              **/
/**   =======================                                      **/
/**                                                                **/
/**    qdd-operations.c  :  Elementary operations and tests over   **/
/**                 sets represented as QDDs.                      **/
/**                                                                **/
/**    01/17/2000  :  Creation. (JMF)                              **/
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

#include "diag.h"
#include "resource.h"
#include "qdd.h"
#include "qdd-operations.h"

/****  Public functions.                                         ****/

/**  int  qdd_merge(q1, q2)  :  Computes a QDD representing the union
                     of the sets of queue-set contents represented by
		     the QDDs *q1 and *q2, and replaces *q1 with the
		     resulting QDD. These two QDDs *q1 qnd *q2 must
		     have the same number of queues, and the same
		     queue alphabet's sizes.

		     This function does not modify *q2, and
                     returns 0 in the case of success, -1 in the case
                     of an error.

                     Possible error codes:

                         LASH_ERR_NOT_INIT  : Not initialized.
			 LASH_ERR_NO_MEM    : Not enough memory.
			 LASH_ERR_ALPHABET  : Alphabet mismatch.
			 LASH_ERR_BAD_TYPE  : Bad type of QDD(s) or
			                      automaton.
			 LASH_ERR_CORRUPT   : Corrupt QDD/automaton.
			 LASH_ERR_BAD_STATE : Automaton contains
                                               reference to an
                                               invalid state.      **/

int  qdd_merge(q1, q2)
     qdd *q1, *q2;
{
  diag__enter("qdd_merge", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!q1 || !q2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  switch(qdd_same_alphabet(q1, q2))
    {
    case 0:
      diag__fail(LASH_ERR_ALPHABET, -1);

    case 1:
      break;

    default:
      diag__fail(lash_errno, -1);
    }
  
  if (qdd_test_property(q1, QDD_PROP_ONE_QUEUE) &&
      qdd_test_property(q2, QDD_PROP_ONE_QUEUE))
    {
      qdd_reset_property(q1, qdd_known_properties(q1));
      qdd_set_property(q1, QDD_PROP_ONE_QUEUE);
    }
  else
     qdd_reset_property(q1, qdd_known_properties(q1));
  
  if (auto_merge(qdd_automaton(q1), qdd_automaton(q2)) ||
      auto_minimize(qdd_automaton(q1)) ||
      qdd_free_init_tuples(q1))
    diag__fail(lash_errno, -1);

  diag__return(0);
}

/**  int qdd_union(q1, q2)  :  Computes a QDD representing the union
                     of the sets of queue-set contents represented by
		     the QDDs *q1 and *q2.  These two QDDs must have
		     the same number of queues, and the same queue
		     alphabet's sizes.

		     This function does not modify *q1 or *q2, and
		     returns (in the case of success) a pointer to a
		     newly allocated QDD. In the case of an error, it
		     returns a NULL pointer and sets lash_errno.

		     Possible error codes:

		         LASH_ERR_NOT_INIT  : Not initialized.
			 LASH_ERR_NO_MEM    : Not enough memory.
                         LASH_ERR_BAD_TYPE  : Bad type of automaton.
			 LASH_ERR_ALPHABET  : Alphabet mismatch.
			 LASH_ERR_CORRUPT   : Corrupt QDD.         **/

qdd *qdd_union(q1, q2)
     qdd *q1, *q2;
{
  register qdd *q;

  diag__enter("qdd_union", NULL);
  
#if LASH_CHECK_LEVEL >= 1
  if (!q1 || !q2)
    diag__fail(LASH_ERR_CORRUPT, NULL);
#endif  /* >= 1 */

  switch(qdd_same_alphabet(q1, q2))
    {
    case 0:
      diag__fail(LASH_ERR_ALPHABET, NULL);

    case 1:
      break;

    default:
      diag__fail(lash_errno, NULL);
    }   

  if (!(q = resr__new_object(qdd)))
    diag__fail(LASH_ERR_NO_MEM, NULL);
  
  q -> properties = QDD_PROP_NOTHING;
  if (qdd_test_property(q1, QDD_PROP_ONE_QUEUE) &&
      qdd_test_property(q2, QDD_PROP_ONE_QUEUE))
    qdd_set_property(q, QDD_PROP_ONE_QUEUE);

  q -> nb_queues = q1 -> nb_queues;
  
  if (!(q -> queue_alphabet_size =
	resr__new_objects(uint1, q -> nb_queues)))
    {
      resr__free_objects(q -> queue_alphabet_size, uint1,
			 q -> nb_queues);
      resr__free_object(q, qdd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  memcpy(q -> queue_alphabet_size, q1 -> queue_alphabet_size,
	 q -> nb_queues);
  
  if (!(q -> automaton =
	auto_union(qdd_automaton(q1), qdd_automaton(q2))))
    {
      resr__free_object(q, qdd);
      diag__fail(LASH_ERR_NO_MEM, NULL);
    }

  if (auto_minimize(qdd_automaton(q)))
    {
      auto_free(qdd_automaton(q));
      resr__free_objects(q -> queue_alphabet_size, uint1,
			 qdd_nb_queues(q));
      resr__free_object(q, qdd);
      diag__fail(lash_errno, NULL);
    }

  q -> nb_it = ZERO_INT4;
  q -> init = NULL;

  diag__return(q);
}

/** int qdd_equality(q1, q2)  :  Tests if the QDDs *q1 and *q2 are
                     equal (i.e. accept the same language).
  
                     Returns 0 in the case of success, -1 in the case
                     of an error.

		     Possible error codes:

		         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_ALPHABET   : Alphabet mismatch.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton/QDD.
			                                           **/

int qdd_equality(q1, q2)
     qdd *q1, *q2;
{
  diag__enter("qdd_equal", -1);
  
#if LASH_CHECK_LEVEL >= 1
  if (!q1 || !q2)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  switch(auto_equality(qdd_automaton(q1), qdd_automaton(q2)))
    {
    case 0:
      diag__return(0);

    case 1:
      diag__return(1);

    default:
      diag__fail(lash_errno, -1);
    }
}


/**  int  qdd_empty(q)  :  Returns 1 if the QDD *q accepts the empty
                     language, or 0 if it doesn't.

		     The function returns -1 if an error occurs,
		     and sets lash_errno.

		     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton. **/

int qdd_empty(q)
     qdd *q;
{
  diag__enter("qdd_empty", -1);

#if LASH_CHECK_LEVEL >= 1
  if (!q)
    diag__fail(LASH_ERR_CORRUPT, -1);
#endif  /* >= 1 */

  switch(auto_empty_language(qdd_automaton(q)))
    {
    case 0:
      diag__return(0);

    case 1:
      diag__return(1);

    default:
      diag__fail(lash_errno, -1);
    }
}

/****  End of qdd-operations.c  ****/
