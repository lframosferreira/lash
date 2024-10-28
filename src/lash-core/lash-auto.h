/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**  lash-auto.h  :  Visible data structures and prototypes        **/
/**                  for manipulating finite-state automata.       **/
/**                                                                **/
/**     03/10/98  :  Creation. (BB)                                **/
/**     06/11/98  :  Additional functions and minor corrections.   **/
/**                  (BB)                                          **/
/**     08/26/98  :  Minor modifications. (BB)                     **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/14/98  :  Minor corrections. (BB)                       **/
/**     09/18/98  :  New function. (BB)                            **/
/**     03/10/99  :  New function 'auto_state'. (BB+GC)            **/
/**     08/12/99  :  Improved sorting functions. (BB)              **/
/**     02/03/01  :  Support for weak and co-Buchi aut. (SJ)       **/
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

#ifndef LASH_LASH_AUTO_H
#define LASH_LASH_AUTO_H

#include <stdlib.h>
#include "lash-types.h"
#include "datastruct.h"

/**  Individual transition of an automaton.                        **/

typedef struct {
  uint4    end_state;              /*  Index of destination state.  */
  uint4    nb_symbols;             /*  Number of symbols labeling   */
                                   /*  the transition.              */
  info     label;                  /*  Label.                       */

} tran;

/**  Individual state of an automaton.                             **/

typedef struct {
  uint4    nb_trans;               /*  Number of outgoing           */
                                   /*  transitions.                 */
  tran    *trans;                  /*  Array of transitions.        */
  info     accepting;              /*  Accepting status.            */

} state;

/**  General data structure associated to any type of finite-      **/
/**  state automaton.                                              **/

typedef struct {
  uint1    word_type;              /*  Does the automaton accept    */
                                   /*  finite or infinite words?    */

  uint1    accept_type;            /*  If word_type ==              */
				   /*      AUTO_WORDS_INFINITE,     */
                                   /*  defines the accepting        */
                                   /*  condition (undefined         */
                                   /*  otherwise).                  */

  uint1    properties;             /*  Known properties of the      */
                                   /*  automaton (bit field).       */

  uint1    alphabet_nbytes;        /*  Number of bytes required to  */
                                   /*  store one symbol of the      */
                                   /*  alphabet.                    */

  uint1    accept_nbytes;          /*  Number of bytes required to  */
                                   /*  store the accepting status   */
                                   /*  of a state.                  */

  uint4    nb_i_states;            /*  Number of initial states.    */
  uint4   *i_states;               /*  Array of initial state       */
                                   /*  indices.                     */
  
  uint4    nb_states;              /*  Number of states.            */
  state   *states;                 /*  Array of states.             */ 

} automaton;

/**  Definitions for automaton.word_type.                          **/

#define AUTO_WORDS_FINITE    0x66
#define AUTO_WORDS_INFINITE  0x69

/**  Definitions for automaton.accept_type.                        **/

#define AUTO_ACCEPT_BUCHI    0x62  /** Buchi automaton    **/
#define AUTO_ACCEPT_COBUCHI  0x63  /** co-Buchi automaton **/
#define AUTO_ACCEPT_WEAK     0x64  /** Weak automaton     **/

/**  Function prototypes.                                          **/

automaton *auto_new_empty(uint1);
int        auto_free(automaton *);
int        auto_add_new_state(automaton *, uint4 *);
int        auto_add_new_transition(automaton *, uint4, uint4, uint4,
               uint1 *);
int        auto_add_new_i_state(automaton *, uint4);
int        auto_nb_out_transitions(automaton *, uint4, uint4 *);
tran      *auto_transition(automaton *, uint4, uint4);
int        auto_state(automaton *, uint4, uint4 *);
int        auto_i_state(automaton *, uint4, uint4 *);
void       auto_transition_cmp_prepare(automaton *);
void       auto_transition_free(tran *);
int        auto_transition_cmp(tran *, tran *);
int        auto_transition_full_cmp(tran *, tran *);
void       auto_mark_accepting_state(automaton *, uint4);
void       auto_unmark_accepting_state(automaton *, uint4);
int        auto_replace(automaton *, automaton *);
automaton *auto_copy(automaton *);
void       auto_remove_i_states(automaton *);

/**  Macro  auto_accepting_info_ptr(a, p)  :  Returns a pointer to the
		     accepting information for the state number p of
		     the automaton *a.

		     This pointer cannot be used for modifying the
		     accepting information!.                       **/

#define  auto_accepting_info_ptr(a, p) ((uint1 *)(info_ptr(&(a) -> \
    states[p].accepting, (a) -> accept_nbytes)))

/**  Macro  auto_accepting_state(a, p)  :  Returns 1 if the first byte
                     of the accepting status of the state p of the
                     automaton *a is nonzero, and 0 otherwise.     **/

#define  auto_accepting_state(a, p) \
    (!!(auto_accepting_info_ptr((a), (p))[0]))

/**  Macro  auto_transition_dest(t)  :  Returns the number of the
                     endstate of the transition *t.                **/

#define  auto_transition_dest(t) ((t) -> end_state)

/**  Macro  auto_transition_length(t)  :  Returns the length of the
                     label of the transition *t (expressed in
                     number of symbols).                           **/

#define  auto_transition_length(t) ((t) -> nb_symbols)

/**  Macro  auto_transition_label_ptr(t, n) : Returns a pointer to
                     the label of the transition given by *t. The
                     number of bytes required for storing one symbol
                     of the alphabet is given by n.       

                     This pointer cannot be used for modifying the
		     content of the label!                         **/

#define  auto_transition_label_ptr(t, n)  ((uint1 *) \
    (info_ptr(&(t) -> label, (t) -> nb_symbols * (n))))

/**  Macro  auto_redirect_transition(t, s)  :  Redirects the
                     destination of the transition *t to the
                     state index s.                                **/

#define  auto_redirect_transition(t, s) ((t) -> end_state = (s))

/**  Macro  auto_nb_states(a)  :  Returns the number of states of
                     the automaton *a.                             **/

#define  auto_nb_states(a) ((a) -> nb_states)

/**  Macro  auto_nb_i_states(a)  :  Returns the number of initial 
                     states of the automaton *a.                   **/

#define  auto_nb_i_states(a) ((a) -> nb_i_states)

/**  Macro  auto_alphabet_nbytes(a)  :  Returns the number of bytes
                     needed for storing one symbol of the alphabet
                     of the automaton *a.                          **/

#define  auto_alphabet_nbytes(a) ((a) -> alphabet_nbytes)

/**  Macro  auto_accept_nbytes(a)  :  Returns the number of bytes
                     needed for storing the accepting status of a
                     state of the automaton *a.                    **/

#define  auto_accept_nbytes(a) ((a) -> accept_nbytes)

/**  Macro  auto_word_type(a)  :  Returns the nature (finite or
                     infinite) of the words accepted by the
                     automaton *a (see the beginning of the file
                     for the definition of the corresponding
                     symbolic constants).                          **/

#define  auto_word_type(a) ((a) -> word_type)    

/**  Macro  auto_accept_type(a)  :  Returns the type of accepting
                     condition of the automaton a (see the beginning 
                     of the file for the definition of the 
                     corresponding symbolic constants).            **/

#define  auto_accept_type(a) ((a) -> accept_type)    

#endif  /* LASH_LASH_AUTO_H */

/****  End of lash-auto.h  ****/
