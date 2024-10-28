/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**       auto.h  :  Invisible data structures and prototypes      **/
/**                  for manipulating finite-state automata.       **/
/**                                                                **/
/**     03/10/98  :  Creation. (BB)                                **/
/**     06/11/98  :  Additional functions and minor corrections.   **/
/**                  (BB)                                          **/
/**     08/26/98  :  Minor modifications. (BB)                     **/
/**     09/04/98  :  Reorganization. (BB)                          **/
/**     09/08/98  :  Minor corrections. (BB)                       **/
/**     08/12/99  :  Improved sorting functions. (BB)              **/
/**     03/14/01  :  Normal form property for weak automata. (SJ)  **/
/**     08/14/01  :  Small adaptation. (BB)                        **/
/**     07/11/02  :  Reorganization. (BB)                          **/
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

#ifndef LASH_AUTO_H
#define LASH_AUTO_H

#include "lash-auto.h"

/**  Flags for automaton.properties (or-able).                     **/

#define AUTO_PROP_NOTHING    0x00  /*  No known property.           */
#define AUTO_PROP_DETERM     0x01  /*  Deterministic automaton.     */
#define AUTO_PROP_MINIMAL    0x02  /*  Minimal automaton.           */
#define AUTO_PROP_NORMAL     0x04  /*  Normal automaton (length     */
                                   /*  of transition labels <= 1).  */
#define AUTO_PROP_STRONG     0x08  /*  Strongly normal automaton    */
                                   /*  (all labels of length 1).    */
#define AUTO_PROP_WEAK_NORMAL 0x10 /*  Weak automata is in normal   */
				   /*  form, as defined in [Lod01]. */

/**  Function prototypes.                                          **/

uint1 *auto_resize_transition_label(automaton *, tran *, uint4,
           uint4);
int    auto_search_out_transition(automaton *, uint4, uint4, uint1 *,
           uint4 *);
void   auto_pack_out_transitions(automaton *, uint4);
void   auto_sort_transitions(automaton *);
void   auto_remove_trans(automaton *, uint4);

/**  Macro  auto_known_properties(a)  :  Returns the known properties
                     (deterministic, normal, strongly normal, ...) of
                     the automaton *a (see the file "lash-auto.h" for
                     the definition of the corresponding symbolic
                     constants). It should be noted that there is no
                     guarantee that a true property will be reported
                     by this function. However, any property that is
                     reported is known to be true.

		     This macro is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

#define  auto_known_properties(a) ((a) -> properties)

/**  Macro  auto_reset_property(a, p)  :  Marks the property p as
                     being invalid for the automaton *a.

		     This macro is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

#define  auto_reset_property(a, p) ((a) -> properties &= ~(p))

/**  Macro  auto_set_property(a, p)  :  Marks the property p as
                     being valid for the automaton *a.

		     This macro is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

#define  auto_set_property(a, p) ((a) -> properties |= (p))

/**  Macro  auto_test_property(a, p)  :  Returns 1 if the property p
                     is known to be valid for the automaton *a, and 0
                     otherwise.

		     This macro is for internal use of the LASH
                     package, and should not be called by external
		     applications!                                 **/

#define  auto_test_property(a, p) (!!((a) -> properties & (p)))

#endif  /* LASH_AUTO_H */

/****  End of auto.h  ****/
