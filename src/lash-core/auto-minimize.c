/********************************************************************/
/**                                                                **/
/**  LASH package -- v0.92                                         **/
/**  ============                                                  **/
/**                                                                **/
/**    auto-minimize.c  :  Minimization of finite-state automata.  **/
/**                                                                **/
/**        07/28/98  :  Creation. (BB)                             **/
/**        09/04/98  :  Reorganization. (BB)                       **/
/**        09/08/98  :  Minor corrections. (BB)                    **/
/**        09/14/98  :  Got rid of recursion. (BB)                 **/
/**        11/09/98  :  More efficient data structures. (BB)       **/
/**        08/12/99  :  Improved sorting function. (BB)            **/
/**        11/12/99  :  Bug corrected in hash table size           **/
/**                     computation. (BB)                          **/
/**        02/27/01  :  Minimization of weak det. automata. (SJ)   **/
/**        03/17/01  :  Integration in the LASH package. (SJ)      **/
/**        08/14/01  :  Minor corrections. (SJ+BB)                 **/
/**        08/23/01  :  Minor corrections. (SJ)                    **/
/**        07/02/02  :  Reorganization. (BB)                       **/
/**        05/27/03  :  Minor correction. (LL)                     **/
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
#include "auto.h"
#include "auto-minimize.h"
#include "diag.h"
#include "resource.h"
#include "datastruct.h"
#include "sort.h"

/****  Private types.                                            ****/

typedef struct {
  uint4   symbol; 
  uint4   origin;
} tran_info;

typedef struct _s_block_info {
  struct _state_info *next, *prev; 
} s_block_info;

typedef struct {
  struct _state_info *first, *last;
  uint4               nb;
} s_block;

typedef struct _state_info {
  uint4               nb_in_trans, indirect_block_num;
  tran_info          *in_trans;
  struct _state_info *next, *prev, *next_decomp;
  s_block_info       *in_s_block;
} state_info;

typedef struct _block {
  uint4          real_num, next_decomp;
  state_info    *first, *last, *first_decomp;
  s_block       *s_blocks;
} block;

typedef struct {
  uint4     *block, first, nb;
  bit_table *bits;
} split_entry;

/****  Global variables.                                         ****/

/**  auto_mini_hsize  :  Size of the hash table used by the     
                     minimization algorithm.                       **/

static uint4  auto_mini_hsize = AUTO_DEFAULT_MINI_HSIZE;

/**  auto_mini_ncolls  :  Number of collisions observed in the   
                     hash table used by the minimization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_mini_ncolls = ZERO_INT8;
#endif  /* >= 1 */

/**  auto_mini_nins  :  Number of insertions performed in the   
                     hash table used by the minimization
                     algorithm.                                    **/

#if LASH_CHECK_LEVEL >= 1
static uint8  auto_mini_nins = ZERO_INT8;
#endif  /* >= 1 */

/****  Prototypes of private functions.                          ****/

static void        pack_transitions(automaton *);
static int         explore_reachable_states(automaton *, bit_table *,
                       uint4);
static void        mini_free_state_table(state_info *, uint4, uint4);
static void        mini_free_blocks(block *, uint4, uint4);
static void        mini_free_split_list(split_entry *, uint4, uint4);
static int         tran_index_cmp(tran_info *, tran_info *);
static uint4       compute_mini_hsize(automaton *);
static int         mini_part_initialize(automaton *, uint4 *,
                       state_info **, uint4 *, block **,
                       split_entry **, bit_table **, bit_table **);
static int         mini_part_initialize_1(automaton *, hash_table **,
                       uint4 *, state_info **, bit_table **,
                       bit_table **);
static int         mini_part_initialize_2(automaton *, hash_table *,
                       uint4, state_info *);
static int         mini_part_initialize_3(automaton *, uint4,
                       state_info *, uint4 *, block **);
static int         mini_part_initialize_4(automaton *, uint4,
                       block *, split_entry **);
static void        mini_fill_s_blocks(block *, state_info *, uint4);
static int         mini_select(split_entry *, uint4, uint4 *,
                       uint4 *);
static int         mini_enumerate(uint4, state_info *, uint4 *,
                       block **, split_entry *, bit_table *,
                       bit_table *, uint4, uint4);
static int         mini_mark_decomposition(state_info *, uint4,
                       state_info *, block *, uint4 *, uint4 *, 
                       bit_table *, bit_table *);
static int         mini_perform_decomposition(uint4, uint4,
                       state_info *, uint4 *, block **,
                       split_entry *, bit_table *);
static void        mini_simplify_blocks(uint4 **, uint4, uint4);
static int         mini_partition(automaton *, uint4 *, uint4 **);
static automaton  *mini_build(automaton *, uint4, uint4 *);

/****  Private functions.                                        ****/

/**  void  pack_transitions(a)  :  Eliminates duplicate transitions
                     at each state of the automaton *a.            **/

static void  pack_transitions(a)
  automaton *a;
{
  register uint4  i, n;

  n = auto_nb_states(a);

  for (i = 0; i < n; i++)
    auto_pack_out_transitions(a, i);
}

/**  typedef  ers_info  :  Type of the data placed of the exploration
                     stack of the function explore_reachable_states.
                     The first field is the index of a state; the 
                     second field is the number of outgoing 
                     transitions that have already been followed 
                     from that state.                              **/

typedef struct {
  uint4  state, nb_transitions;
} ers_info;

/**  int  explore_reachable_states(a, bt, s)  :  Explores all the
                     reachable states of the finite-state automaton
                     *a, starting at the state of index s. The bit
                     table *bt contains the indices of the states 
                     that have already been visited. This function
                     returns -1 if there is not enough memory, and
                     0 otherwise.                                  **/

static int  explore_reachable_states(a, bt, s)
  automaton *a;
  bit_table *bt;
  uint4      s;
{
           uint4     m;
  register tran     *t;
  register stack    *st;
           ers_info  e;
  
  st = stack__new_empty(ers_info);
  if (!st)
    return -1;

  e.state = s;
  e.nb_transitions = ZERO_INT4;

  if (stack__push(st, (void *) &e) < 0)
    {
      stack__free(st);
      return -1;
    }

  while (!stack__is_empty(st))
    {
      stack__pop(st, (void *) &e);
      if (!e.nb_transitions)
        {
          if (bit__member(bt, e.state))
            continue;

          bit__add(bt, e.state);
        }
      if (auto_nb_out_transitions(a, e.state, &m) < 0)
        {
          stack__free(st);
          return -1;
        }
      if (e.nb_transitions >= m)
        continue;
     
      t = auto_transition(a, e.state, e.nb_transitions);

      e.nb_transitions++;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }

      e.state = auto_transition_dest(t);
      e.nb_transitions = ZERO_INT4;
      if (stack__push(st, &e) < 0)
        {
          stack__free(st);
          return -1;
        }
    }
  
  stack__free(st);
  return 0;
}

/**  void  mini_free_state_table(p, n, ns)  :  This routine is part of
                     the  minimization algorithm. It frees the state
                     table *p, given its number of entries n and the
                     number of symbols ns.                         **/

static void  mini_free_state_table(p, n, ns)
  state_info *p;
  uint4       n, ns;
{
  register uint4  i;

  if (!p)
    return;

  for (i = 0; i < n; i++)
    {
      resr__free_objects(p[i].in_trans, tran_info, p[i].nb_in_trans);
      resr__free_objects(p[i].in_s_block, s_block_info, ns);
    }

  resr__free_objects(p, state_info, n);
}

/**  void  mini_free_blocks(p, n, ns)  :  This routine is part of the
                     minimization algorithm. It frees the array
                     of blocks *p, given the number n of blocks
                     in that array, and the number ns of symbols.  **/

static void mini_free_blocks(p, n, ns)
  block *p;
  uint4  n, ns;
{
  register uint4  i;

  if (!p)
    return;

  for (i = 0; i < n; i++)
    resr__free_objects(p[i].s_blocks, s_block, ns);
 
  resr__free_objects(p, block, n);
}

/**  void  mini_free_split_list(p, n, m)  :  This routine is part of
                     the minimization algorithm. It frees the content
                     of the split list *p (which corresponds to the
                     variable L of [Hop71]), given the number n of
                     states and the number m of distinct symbols.  **/

static void  mini_free_split_list(p, n, m)
  split_entry *p;
  uint4        n, m;
{
  register uint4  i;

  if (!p)
    return;

  for (i = 0; i < m; i++)
    {
      resr__free_objects(p[i].block, uint4, n);
      bit__free(p[i].bits);
    }
  
  resr__free_objects(p, split_entry, m);
}

/**  int tran_index_cmp(p1, p2)  :  This routine is used by the
                     minimization algorithm. It compares the two
                     incoming transitions *p1 and *p2 with respect to
                     the index of their labeling symbol. The function
                     returns a negative value if *p1 < *p2, 0 if *p1 =
                     *p2, and a positive value if *p1 > *p2.       **/

static int  tran_index_cmp(p1, p2)
  tran_info *p1, *p2;
{
  register sint4  d;

  return (d = (p1 -> symbol) - (p2 -> symbol)) ? 
             ((d < 0) ? -1 : 1) : 0;
}

/**  uint4  compute_mini_hsize(a)  :  Adjusts (heuristically) the
                     size of the hash table needed for minimizing
                     the automaton *a.                             **/

static uint4  compute_mini_hsize(a)
  automaton *a;
{
  register uint4 n;

  n = 8 * auto_alphabet_nbytes(a);

  if (!n)
    n = 1;

  if (n < 32 && auto_mini_hsize >> n)
    return (1 << n);

  if (auto_mini_hsize < n)
    return n;

  return auto_mini_hsize;
}

/**  int  mini_part_initialize(a, nb_symbols_p, state_table_p,
        nb_blocks_p, blocks_p, split_list_p, decomp_blocks_check_p,
        decomp_states_check_p)  :  Initializes the variables used
                     during the partitioning stage of the minimization
                     algorithm. The parameters a points to the
                     automaton being minimized. The other parameters
                     correspond each to the address of a variable.
                     The values of these variables are set as follows:

			*nb_symbols_p: Number of distinct symbols
			     labeling the transitions of *a
                             (supposed to be strongly normal).

			*state_table_p: Array associating to each
                             state of *a the set of its ingoing
                             transitions, its block number, pointers
                             to its successor and to its predecessor
                             in its block, and a pointer to an
                             array of superblock information.

                             The block number of the state is
                             given indirectly. This means that the
                             field indirect_block_num contains the
                             number of the block whose field real_num
                             contains the actual block number.

			     There is one additional entry in the
                             array of states, corresponding to a
                             non-accepting state introduced in order
                             to make *a complete.

		        *nb_blocks_p: Number of classes of the
                             partition.

			*blocks_p: Partition of the set of states.
                             Each element of this array contains
                             pointers to the beginning and to the
                             end of a doubly-linked list of states
                             belonging to the block, as well as
                             a pointer to an array of superblocks.
 
                        *split_list_p: List of pairs (symbol, block
                             to be split).

			*decomp_blocks_p:
                        *decomp_states_p: Bit tables used temporarily
                             by the block decomposition algorithm.
                             The capacity of both of them is equal to
                             the number of states of the automaton.
                             They are initialized with empty arrays.

                     In case of success, the function returns 0.
                     In case of insufficient memory, it returns -1.
                                                                   **/

static int  mini_part_initialize(a, nb_symbols_p, state_table_p,
      nb_blocks_p, blocks_p, split_list_p, decomp_blocks_p,
      decomp_states_p)
  automaton    *a;
  uint4        *nb_symbols_p;
  state_info  **state_table_p;
  uint4        *nb_blocks_p;
  block       **blocks_p;
  split_entry **split_list_p;
  bit_table   **decomp_blocks_p, **decomp_states_p;
{
           hash_table   *h_symbols;
  register uint4         i;  

  if (mini_part_initialize_1(a, &h_symbols, nb_symbols_p,
      state_table_p, decomp_blocks_p, decomp_states_p) < 0)
    return -1;

  if (mini_part_initialize_2(a, h_symbols, *nb_symbols_p,
      *state_table_p) < 0)
    {
      bytes__prepare_free(auto_alphabet_nbytes(a));
      for (i = 0; i <= auto_nb_states(a); i++)
	resr__free_objects((*state_table_p)[i].in_s_block,
            s_block_info, *nb_symbols_p);
        resr__free_objects(*state_table_p, state_info, 
            auto_nb_states(a) + 1);
      hash__free(h_symbols, (void (*)(void *)) bytes__free,
          (void (*)(void *)) uint4__free);
      bit__free(*decomp_blocks_p);
      bit__free(*decomp_states_p);
      return -1;
    }

  bytes__prepare_free(auto_alphabet_nbytes(a));  
  hash__free(h_symbols, (void (*)(void *)) bytes__free,
     (void (*)(void *)) uint4__free);

  if (mini_part_initialize_3(a, *nb_symbols_p, *state_table_p,
      nb_blocks_p, blocks_p) < 0)
    {
      mini_free_state_table(*state_table_p, auto_nb_states(a) + 1,
          *nb_symbols_p);
      bit__free(*decomp_blocks_p);
      bit__free(*decomp_states_p);
      return -1;
    }

  if (mini_part_initialize_4(a, *nb_symbols_p, *blocks_p, 
      split_list_p) < 0)
    {
      mini_free_state_table(*state_table_p, auto_nb_states(a) + 1,
          *nb_symbols_p);
      mini_free_blocks(*blocks_p, *nb_blocks_p, *nb_symbols_p);
      bit__free(*decomp_blocks_p);
      bit__free(*decomp_states_p);
      return -1;
    }

  return 0;
}

/**  int  mini_part_initialize_1(a, h_symbols_p, nb_symbols_p,
                     state_table_p, decomp_blocks_p, decomp_states_p)
                     :  Performs the first stage of the 
                     initializations to be carried out by the function
                     mini_part_initialize. Specifically, this function
                     creates and fills the hash table *h_symbols_p,
                     computes the value of *nb_symbols_p, and
                     allocates *state_table_p as well as the two bit
                     tables *decomp_blocks_p and *decomp_states_p.

                     In case of success, the function returns 0.
                     In case of insufficient memory, it returns -1.
                                                                   **/

static int  mini_part_initialize_1(a, h_symbols_p, nb_symbols_p,
      state_table_p, decomp_blocks_p, decomp_states_p)
  automaton    *a;
  hash_table  **h_symbols_p;
  uint4        *nb_symbols_p;
  state_info  **state_table_p;
  bit_table   **decomp_blocks_p, **decomp_states_p;
{
  register uint4          nb_states, i, j, total_nb_trans;
  register uint1          alph_nbytes;
  register tran          *t;
  register uint4         *v;
  register s_block_info  *sb;
           void         **r;
           uint4          m;

  nb_states = auto_nb_states(a);
  alph_nbytes = auto_alphabet_nbytes(a);

  *decomp_blocks_p = bit__new_empty(nb_states + 1);
  if (!*decomp_blocks_p)
    return -1;

  *decomp_states_p = bit__new_empty(nb_states + 1);
  if (!*decomp_states_p)
    {
      bit__free(*decomp_blocks_p);
      return -1;
    }

  *state_table_p = resr__new_objects(state_info, nb_states + 1);
  if (!*state_table_p)
    {
      bit__free(*decomp_blocks_p);
      bit__free(*decomp_states_p);
      return -1;
    }

  memset(*state_table_p, 0, (nb_states + 1) * sizeof(state_info));

  *h_symbols_p = hash__new_empty(compute_mini_hsize(a));
  if (!*h_symbols_p)
    {
      resr__free_objects(*state_table_p, state_info, nb_states + 1);
      bit__free(*decomp_blocks_p);
      bit__free(*decomp_states_p);
      return -1;
    }

  *nb_symbols_p  = ZERO_INT4;
  total_nb_trans = ZERO_INT4; 

  bytes__prepare_free(alph_nbytes);

  for (i = 0; i < nb_states; i++)
    {
      if (auto_nb_out_transitions(a, i, &m) < 0)
	{
	  resr__free_objects(*state_table_p, state_info, 
              nb_states + 1);
	  hash__free(*h_symbols_p,
              (void (*)(void *)) bytes__free,
              (void (*)(void *)) uint4__free);
	  bit__free(*decomp_blocks_p);
	  bit__free(*decomp_states_p);
	  return -1;
	}

      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
	  (*state_table_p)[auto_transition_dest(t)].nb_in_trans++;
          total_nb_trans++;
#if LASH_CHECK_LEVEL >= 1
	  if (hash__insert_bytes(*h_symbols_p,
              auto_transition_label_ptr(t, alph_nbytes),
              alph_nbytes, &r, &auto_mini_ncolls,
              &auto_mini_nins) < 0)
#else
	  if (hash__insert_bytes(*h_symbols_p,
              auto_transition_label_ptr(t, alph_nbytes),
              alph_nbytes, &r) < 0)
#endif  /* >= 1 */
	    {
	      resr__free_objects(*state_table_p, state_info,
                  nb_states + 1);
	      hash__free(*h_symbols_p,
                  (void (*)(void *)) bytes__free,
                  (void (*)(void *)) uint4__free);
	      bit__free(*decomp_blocks_p);
	      bit__free(*decomp_states_p);
	      return -1;
	    }
	  if (r)
	    {
	      v = resr__new_object(uint4);
	      if (!v)
		{
		  resr__free_objects(*state_table_p, state_info,
                      nb_states + 1);
		  hash__free(*h_symbols_p,
                      (void (*)(void *)) bytes__free,
		      (void (*)(void *)) uint4__free);
		  bit__free(*decomp_blocks_p);
		  bit__free(*decomp_states_p);
		  return -1;
		}

              *r = v;
              *v = (*nb_symbols_p)++;    
	    }
	}
    }

  (*state_table_p)[nb_states].nb_in_trans = (nb_states + 1) * 
      (*nb_symbols_p) - total_nb_trans;

  for (i = 0; i <= nb_states; i++)
    {
      sb = resr__new_objects(s_block_info, *nb_symbols_p);
      if (!sb)
	{
	  for (j = 0; j < i; i++)
	    resr__free_objects((*state_table_p)[j].in_s_block,
                s_block_info, *nb_symbols_p);
	  resr__free_objects(*state_table_p, state_info,
              nb_states + 1);
	  hash__free(*h_symbols_p,
	      (void (*)(void *)) bytes__free,
	      (void (*)(void *)) uint4__free);
	  bit__free(*decomp_blocks_p);
	  bit__free(*decomp_states_p);
	  return -1;
	}
      (*state_table_p)[i].in_s_block = sb;
      memset(sb, 0, (*nb_symbols_p) * sizeof(s_block_info));
    }

  return 0;
}

/**  int  mini_part_initialize_2(a, h_symbols, nb_symbols, 
        state_table)  :  Performs the second stage of initializations
                     to be carried out by the function
                     mini_part_initialize.  Specifically, this
                     function fills state_table with the ingoing
                     transitions from each state of *a.

                     In case of success, the function returns 0.
                     In case of insufficient memory, it returns -1.
                                                                   **/

static int  mini_part_initialize_2(a, h_symbols, nb_symbols, 
    state_table)
  automaton   *a;
  hash_table  *h_symbols;
  uint4        nb_symbols;
  state_info  *state_table;
{
  register uint4       i, j, nb_states;
  register uint1       alph_nbytes;
  register tran_info  *ti;
  register state_info *si;
  register tran       *t;
  register uint4      *p;
  register bit_table  *symbols;
           uint4       m;

  nb_states   = auto_nb_states(a) + 1;
  alph_nbytes = auto_alphabet_nbytes(a); 
  
  symbols = bit__new_empty(nb_symbols);
  if (!symbols)
    return -1;

  for (i = 0, si = state_table; i < nb_states; i++, si++)
    {
      if (si -> nb_in_trans > 0)
	{
	  ti = resr__new_objects(tran_info, si -> nb_in_trans);
	  if (!ti)
	    {
	      for (j = 0; j < i; j++)
		resr__free_objects(state_table[j].in_trans,
				   tran_info, state_table[j].nb_in_trans);
	      bit__free(symbols);
	      return -1;
	    }
	  si -> in_trans = ti;
	}
      else
	si -> in_trans = NULL; 
    }

  for (i = 0, si = state_table; i < nb_states; i++, si++)
    si -> nb_in_trans = 0;

  for (i = 0; i < nb_states - 1; i++)
    {
      auto_nb_out_transitions(a, i, &m);
      bit__empty_content(symbols);

      for (j = 0; j < m; j++)
	{
	  t = auto_transition(a, i, j);
          si = state_table + auto_transition_dest(t);

	  p = (uint4 *) hash__lookup_bytes(h_symbols,
	      auto_transition_label_ptr(t, alph_nbytes), 
              alph_nbytes);

          ti = (si -> in_trans) + (si -> nb_in_trans++);
          ti -> symbol = *p;
          ti -> origin = i;
	  bit__add(symbols, *p);
	}

      for (j = 0; j < nb_symbols; j++)
	if (!bit__member(symbols, j))
	  {
	    si = state_table + nb_states - 1;
	    ti = (si -> in_trans) + (si -> nb_in_trans++);
	    ti -> symbol = j;
	    ti -> origin = i;
	  }
    }

  for (j = 0; j < nb_symbols; j++)
	{
	  si = state_table + nb_states - 1;
	  ti = (si -> in_trans) + (si -> nb_in_trans++);
	  ti -> symbol = j;
	  ti -> origin = nb_states - 1;
	}

  bit__free(symbols);

  for (i = 0, si = state_table; i < nb_states; i++, si++)
    if (si -> nb_in_trans > 1 && 
        bytes__sort((void *) si -> in_trans, si -> nb_in_trans,
            sizeof(tran_info), (int (*) (const void *, const void *))
            tran_index_cmp) < 0)
      qsort((void *) si -> in_trans, si -> nb_in_trans,
          sizeof(tran_info), 
          (int (*) (const void *, const void *)) tran_index_cmp); 

  return 0;
}

/**  int  mini_part_initialize_3(a, nb_symbols, state_table,
                     nb_blocks_p, blocks_p)  :  Performs the third
                     stage of initializations to be carried out by the
                     function mini_part_initialize.  Specifically,
                     this function creates the two initial blocks of
                     the state partition, builds the corresponding
                     superblocks (i.e., the sets $\hat{B}$ of
                     [Hop71]), and fills the block field of each entry
                     in the state table.

                     In case of success, the function returns 0.
                     In case of insufficient memory, it returns -1.
                                                                   **/

static int  mini_part_initialize_3(a, nb_symbols, state_table,
    nb_blocks_p, blocks_p)
  automaton   *a;
  uint4        nb_symbols;
  state_info  *state_table;
  uint4       *nb_blocks_p;
  block      **blocks_p; 
{
  register uint4       nb_states, i, j;
  register s_block    *sb;
  register state_info *si;
  register block      *bl;

  *blocks_p = resr__new_objects(block, 2);
  if (!*blocks_p)
    return -1;

  memset(*blocks_p, 0, 2 * sizeof(block));

  for (i = 0; i < 2; i++)
    {
      sb = resr__new_objects(s_block, nb_symbols);
      if (!sb)
	{
	  for (j = 0; j < i; j++)
	    resr__free_objects((*blocks_p)[j].s_blocks, s_block,
                nb_symbols);
	  resr__free_objects(*blocks_p, block, 2);
	  return -1;
	}
      (*blocks_p)[i].s_blocks = sb;
      (*blocks_p)[i].real_num = i;
      memset(sb, 0, nb_symbols * sizeof(s_block));
    }

  *nb_blocks_p = 2;
  nb_states = auto_nb_states(a) + 1;

  for (i = 0, si = state_table; i < nb_states; i++, si++)
    {
      if (i < (nb_states - 1) && auto_accepting_state(a, i))
	{
	  si -> indirect_block_num = 0;
	  bl = *blocks_p;
	}
      else
	{
	  si -> indirect_block_num = 1;
          bl = (*blocks_p) + 1;
	}
      if ((si -> prev = bl -> last))
	si -> prev -> next = si;
      else
	bl -> first = si;
      si -> next = NULL;
      bl -> last = si;
    }

  for (i = 0; i < 2; i++)
    mini_fill_s_blocks((*blocks_p) + i, state_table, nb_symbols);

  return 0;
}

/** int mini_part_initialize_4(a, nb_symbols, blocks, split_list_p)  :
                     Performs the fourth and last stage of
                     initializations to be carried out by the function
                     mini_part_initialize.  Specifically, this
                     function creates and initializes the split list
                     (i.e., the variable L of [Hop71]).
      
                     In case of success, the function returns 0.
                     In case of insufficient memory, it returns -1.
                                                                   **/

static int  mini_part_initialize_4(a, nb_symbols, blocks, 
    split_list_p)
  automaton    *a;
  uint4         nb_symbols;
  block        *blocks;
  split_entry **split_list_p;  
{
  register uint4        i, j, nb_states;
  register split_entry *e;

  e = resr__new_objects(split_entry, nb_symbols);
  if (!e)
    return -1;

  *split_list_p = e;
  nb_states = auto_nb_states(a) + 1;

  for (i = 0; i < nb_symbols; i++, e++)
    {
      e -> block = resr__new_objects(uint4, nb_states);
      if (!(e -> block))
	{
	  for (j = 0, e = *split_list_p; j < i; j++, e++)
	    resr__free_objects(e -> block, uint4, nb_states);
          resr__free_objects(*split_list_p, split_entry, nb_symbols);
          return -1;
	}
      e -> bits = bit__new_empty(nb_states);
      if (!(e -> bits))
	{
	  for (j = 0, e = *split_list_p; j <= i; j++, e++)
	    resr__free_objects(e -> block, uint4, nb_states);
	  for (j = 0, e = *split_list_p; j < i; j++, e++)
	    bit__free(e -> bits);
          resr__free_objects(*split_list_p, split_entry, nb_symbols);
          return -1;
	}
      e -> first = 0;
      e -> nb = 1;
      e -> block[0] = (blocks[0].s_blocks[i].nb <=
		       blocks[1].s_blocks[i].nb) ? 0 : 1;
      bit__add(e -> bits, e -> block[0]);
    }

  return 0;
}

/**  void  mini_fill_s_blocks(bl, st, nb)  :  This routine is used
                     by the minimization algorithm. It fills the
                     superblocks associated to the block *bl with
                     the information gathered from the state table
                     *st. The number of distinct symbols is nb.

                     In the case of an error, this function returns
                     -1. In the case of success, it returns 0.     **/

static void  mini_fill_s_blocks(bl, st, nb)
  block      *bl;
  state_info *st;
  uint4       nb;
{
  register uint4       j, m;
  register state_info *si;
  register tran_info  *ti;
  register s_block    *sb;

  for (si = bl -> first; si; si = si -> next)
    for (j = 0, ti = si -> in_trans; j < si -> nb_in_trans;
          j++, ti++)
      {
        m  = ti -> symbol;
	sb = bl -> s_blocks + m;
        if (si -> in_s_block[m].prev || sb -> first == si)
	  continue;
	if ((si -> in_s_block[m].prev = sb -> last))
	  sb -> last -> in_s_block[m].next = si;
	else
	  sb -> first = si;
	sb -> last = si;
	si -> in_s_block[m].next = NULL;
	sb -> nb++;
      }

  return;  
}

/**  int  mini_select(sl, nb, pa, pi)  :  This function is part of
                     the minimization algorithm. It selects a symbol
                     and a block index belonging to the split list
                     *sl, and removes them from the list. If the split
                     list *sl is empty, the function returns 0.
                     Otherwise, the function returns 1 and writes the
                     symbol and the block index at the locations pa
                     and pi. The number of symbols is nb.          **/

static int  mini_select(sl, nb, pa, pi)
  split_entry *sl;
  uint4 nb,   *pa, *pi;
{
  register uint4        i, v;
  register split_entry *se;

  for (i = 0, se = sl; i < nb; i++, se++)
    if (se -> nb)
      {
        v = se -> block[se -> first];

	if (pa)
	  *pa = i;
	if (pi)
	  *pi = v;

        se -> first++;
        se -> nb--;

        bit__remove(se -> bits, v);

	return 1;
      }

  return 0;
}

/**  int  mini_enumerate(nb_symbols, state_table, nb_blocks_p, 
         blocks_p, split_list, decomp_blocks_check, 
         decomp_states_check, aa, ii)  :  Performs the inner part of
                     the minimization algorithm described in
                     [Hop71]. The first seven parameters are the
                     variables of this algorithm (see the comments of
                     mini_part_initialize for a description of these
                     variables). The argument aa and ii give the
                     symbol index and the block index that are to be
                     considered.

                     This function returns 0 in case of success,
                     and -1 otherwise.                             **/

static int  mini_enumerate(nb_symbols, state_table, nb_blocks_p,
    blocks_p, split_list, decomp_blocks_check, decomp_states_check,
    aa, ii)
  uint4        nb_symbols, *nb_blocks_p, aa, ii;
  state_info  *state_table;
  block      **blocks_p;
  split_entry *split_list;
  bit_table   *decomp_blocks_check, *decomp_states_check;
{
  register state_info *si;
  register uint4       i, j;
           uint4       decomp_blocks, nb_decomp_blocks;

  nb_decomp_blocks = decomp_blocks = 0;
  for (si = (*blocks_p)[ii].s_blocks[aa].first; si; 
       si = si -> in_s_block[aa].next)
    mini_mark_decomposition(si, aa, state_table, *blocks_p,
        &decomp_blocks, &nb_decomp_blocks, decomp_blocks_check,
        decomp_states_check);

  for (i = 0, j = decomp_blocks; i < nb_decomp_blocks; 
       i++, j = (*blocks_p)[j].next_decomp)
    {
      bit__remove(decomp_blocks_check, j);
      if (mini_perform_decomposition(j, nb_symbols, state_table,
	  nb_blocks_p, blocks_p, split_list, decomp_states_check) 
          < 0)
	return -1;
    }

  return 0;
}

/**  void  mini_mark_decomposition(si, aa, state_table, blocks,
         decomp_blocks_p, nb_decomp_blocks_p, decomp_blocks_check, 
         decomp_states_check)  :  This routine is part of the inner 
                     core of the minimization algorithm. Its purpose
                     is to scan all the ingoing transitions labeled by
                     the symbol of index aa to the state whose entry
                     in the state table state_table is si.  For each
                     transition found, the block to which its origin
                     belongs is added to the linked list
                     *decomp_blocks_p (whose number of elements is
                     *nb_decomp_blocks_p) and is marked in the bit
                     table decomp_blocks_check, and the origin itself
                     is added to the linked list associated to the
                     block and is marked in the bit table
                     decomp_states_check.  The array of blocks is
                     given by the argument blocks.

                     This routine does not report errors.          **/

static int  mini_mark_decomposition(si, aa, state_table, blocks,
    decomp_blocks_p, nb_decomp_blocks_p, decomp_blocks_check, 
    decomp_states_check)
  uint4       aa, *decomp_blocks_p, *nb_decomp_blocks_p;
  state_info *si, *state_table;
  block      *blocks;
  bit_table  *decomp_blocks_check, *decomp_states_check;
{
  register uint4       k, q, n, m;
  register tran_info  *ti, *tq;
  register block      *bl;
           tran_info   dummy_tran_info;  

  n  = si -> nb_in_trans;
  if (!n)
    return 0;

  dummy_tran_info.symbol = aa;

  tq = (tran_info *) bsearch ((const void *) &dummy_tran_info,
      (const void *) (si -> in_trans), n, sizeof(tran_info),
      (int (*)(const void *, const void *)) tran_index_cmp);

  if (!tq)
    return 0;

  q = tq - (si -> in_trans);

  for (k = q, ti = tq - 1; k; k--, ti--)
    if (!tran_index_cmp(ti, tq))
      {
	m = blocks[state_table[ti -> origin].indirect_block_num].\
real_num;
        bl = blocks + m;
        if (!bit__member(decomp_blocks_check, m))
	  {
	    bit__add(decomp_blocks_check, m);
	    bl -> next_decomp = *decomp_blocks_p;
	    *decomp_blocks_p = m;
            ++*nb_decomp_blocks_p;
            bl -> first_decomp = NULL;
	  }
	if (!bit__member(decomp_states_check, ti -> origin))
	  {
	    bit__add(decomp_states_check, ti -> origin);
	    state_table[ti -> origin].next_decomp =
                bl -> first_decomp;
            bl -> first_decomp = state_table + ti -> origin;
	  }
      }

  for (k = q, ti = tq; k < n; k++, ti++)
    if (!tran_index_cmp(ti, tq))
      {
        m = blocks[state_table[ti -> origin].indirect_block_num].\
real_num;
        bl = blocks + m;
        if (!bit__member(decomp_blocks_check, m))
	  {
	    bit__add(decomp_blocks_check, m);
	    bl -> next_decomp = *decomp_blocks_p;
	    *decomp_blocks_p = m;
            ++*nb_decomp_blocks_p;
            bl -> first_decomp = NULL;
	  }
	if (!bit__member(decomp_states_check, ti -> origin))
	  {
	    bit__add(decomp_states_check, ti -> origin);
	    state_table[ti -> origin].next_decomp =
                bl -> first_decomp;
            bl -> first_decomp = state_table + ti -> origin;
	  }
      }

  return 0;
}

/**  int  mini_perform_decomposition(bl, nb_symbols, state_table, 
         nb_blocks_p, blocks_p, split_list, decomp_states_check)  :
                     This function is part of the inner core of the
                     minimization algorithm. It splits the block of
                     index bl according to the information contained
                     in the linked list of decomposable states
                     associated to that block.  It then updates the
                     variables of the minimization algorithm in order
                     to reflect the new partition.

                     In the case of success, this function returns 0.
                     In the case of an error, it returns -1.       **/

static int  mini_perform_decomposition(bl, nb_symbols, state_table, 
    nb_blocks_p, blocks_p, split_list, decomp_states_check)
  uint4        bl, nb_symbols, *nb_blocks_p;
  state_info  *state_table;
  block      **blocks_p;
  split_entry *split_list;
  bit_table   *decomp_states_check;
{
  register state_info   *si, *split_first, *split_last, *rem_first,
                        *rem_last;
  register block        *blocks;
  register s_block      *s_blocks, *sb, *oldsb;
  register uint4         i, nb_blocks, n1;
  register s_block_info *sbi;
  register split_entry  *se;

  split_first = split_last = NULL;

  for (si = (*blocks_p)[bl].first_decomp; si; si = si -> next_decomp)
    {
      bit__remove(decomp_states_check, (si - state_table));

      if (si -> prev)
	si -> prev -> next = si -> next;
      else
	(*blocks_p)[bl].first = si -> next;
      if (si -> next)
	si -> next -> prev = si -> prev;
      else
	(*blocks_p)[bl].last = si -> prev;
      si -> next = NULL;
      if ((si -> prev = split_last))
	split_last -> next = si;
      else
	split_first = si;
      split_last = si;
    }

  rem_first = (*blocks_p)[bl].first;
  rem_last  = (*blocks_p)[bl].last;
  (*blocks_p)[bl].first = split_first;
  (*blocks_p)[bl].last  = split_last;

  if (!rem_first)
    return 0;

  nb_blocks= *nb_blocks_p;
  blocks = resr__resize_objects(*blocks_p, block, nb_blocks + 1,
      nb_blocks);
  if (!blocks)
    return -1;

  s_blocks = resr__new_objects(s_block, nb_symbols);
  if (!s_blocks)
    {
      *blocks_p = resr__resize_objects(blocks, block, nb_blocks,
      nb_blocks + 1);
      return -1;
    }
  *blocks_p    = blocks;
  *nb_blocks_p = nb_blocks + 1;
  blocks[nb_blocks].s_blocks = blocks[bl].s_blocks;
  blocks[bl].s_blocks = s_blocks;
  memset(s_blocks, 0, nb_symbols * sizeof(s_block));
 
  n1 = split_first -> indirect_block_num;

  blocks[nb_blocks].real_num = bl;
  blocks[n1].real_num = nb_blocks;
  blocks[nb_blocks].first = rem_first;
  blocks[nb_blocks].last  = rem_last;

  for (si = split_first; si; si = si -> next_decomp)
    {
      si -> indirect_block_num = nb_blocks;

      for (i = 0, sbi = si -> in_s_block, sb = s_blocks,
           oldsb = blocks[nb_blocks].s_blocks; i < nb_symbols;
           i++, sbi++, sb++, oldsb++)
	{
          if (!(sbi -> prev || si == oldsb -> first))
	    continue;

	  if (sbi -> prev)
	    sbi -> prev -> in_s_block[i].next = sbi -> next;
	  else
	    oldsb -> first = sbi -> next;

	  if (sbi -> next)
	    sbi -> next -> in_s_block[i].prev = sbi -> prev;
	  else
	    oldsb -> last = sbi -> prev;

	  sbi -> next = NULL;
	  if ((sbi -> prev = sb -> last))
	    sb -> last -> in_s_block[i].next = si;
	  else
	    sb -> first = si;

	  sb -> last = si;
	  sb -> nb++;
          oldsb -> nb--;
	}
    }

  for (i = 0, se = split_list; i < nb_symbols; i++, se++)
    if ((!bit__member(se -> bits, bl)) && 
        (*blocks_p)[bl].s_blocks[i].nb <= s_blocks[i].nb)
      {
	bit__add(se -> bits, bl);
	se -> block[se -> first + se -> nb++] = bl;
      }
    else
      {
	bit__add(se -> bits, nb_blocks);
	se -> block[se -> first + se -> nb++] = nb_blocks;
      }

  return 0;
}

/**  void  mini_simplify_blocks(bl, nb_blocks, nb_states)  :  The
                     purpose of this routine is to clean up the result
                     of the state partition obtained during the
                     minimization of a finite-state automaton.  The
                     parameters nb_blocks and bl are respectively the
                     number of blocks of the partition and a pointer
                     to an array associating to each state the index
                     of the block to which it belongs. The parameter
                     nb_states gives the number of entries in *bl. The
                     function simply renumbers the elements of the
                     partition so as to assign the number 0 to the
                     block containing the state having the index
                     nb_states - 1. Then, it resizes the array *bl so
                     as to get rid of that state.                  **/

static void  mini_simplify_blocks(bl, nb_blocks, nb_states)
  uint4 **bl, nb_blocks, nb_states;
{
  register uint4  n1, i;

  n1 = (*bl)[nb_states - 1];

  *bl = resr__resize_objects(*bl, uint4, nb_states - 1, nb_states);
  
  for (i = 0; i < nb_states - 1; i++)
    if ((*bl)[i] == n1)
      (*bl)[i] = 0;
    else
      if ((*bl)[i] == 0)
	(*bl)[i] = n1;
}

/**  int  mini_partition(a, nb_bl, bl)  :  Partitions the states
                     of the automaton *a according to the algorithm
                     of Hopcroft [Hop71]. In case of success, returns
                     0, puts in *nb_bl the number of elements in
                     the partition, and puts in *bl an array of
                     integers associating to each state of *a the
                     number of the block (between 0 and *nb_bl - 1)
                     to which it belongs. The block that has the
                     number 0 contains all the states that can be
                     removed from the automaton without affecting its
                     accepted language. This block is the only one
                     that may be empty.

                     In the case of an error, this function returns -1
                     and does not set *nb_blocks and *blocks.      **/

static int  mini_partition(a, nb_bl, bl)
  automaton *a;
  uint4     *nb_bl;
  uint4    **bl;
{
  register uint4        i, *p;
           uint4        nb_symbols, nb_states, aa, ii;
           state_info  *state_table;
           uint4        nb_blocks;
           block       *blocks;
           split_entry *split_list;
           bit_table   *decomp_blocks_check, *decomp_states_check;

  if (mini_part_initialize(a, &nb_symbols, &state_table, &nb_blocks,
      &blocks, &split_list, &decomp_blocks_check,
      &decomp_states_check) < 0) 
    return -1;

  nb_states = auto_nb_states(a);
  
  while (mini_select(split_list, nb_symbols, &aa, &ii))
    if (mini_enumerate(nb_symbols, state_table, &nb_blocks, &blocks,
        split_list, decomp_blocks_check, decomp_states_check, aa, ii)
        < 0)
      {
	mini_free_state_table(state_table, nb_states + 1, nb_symbols);
	mini_free_blocks(blocks, nb_blocks, nb_symbols);
        mini_free_split_list(split_list, nb_states + 1, nb_symbols);
        bit__free(decomp_blocks_check);
        bit__free(decomp_states_check);
        return -1;
      }
      
  if (nb_bl)
    *nb_bl = nb_blocks;

  if (bl)
    {
      *bl = p = resr__new_objects(uint4, nb_states + 1);
      for (i = 0; i <= nb_states; i++, p++)
	*p = blocks[state_table[i].indirect_block_num].real_num;
    }

  mini_simplify_blocks(bl, nb_blocks, nb_states + 1);
  mini_free_state_table(state_table, nb_states + 1, nb_symbols);
  mini_free_blocks(blocks, nb_blocks, nb_symbols);
  mini_free_split_list(split_list, nb_states + 1, nb_symbols);
  bit__free(decomp_blocks_check);
  bit__free(decomp_states_check);

  return 0;
}              

/**  automaton *mini_build(a, nb_blocks, blocks)  :  Constructs an
                     automaton whose states each correspond to a
                     equivalence class between the states of the
                     automaton *a. The number of equivalence classes
                     is given by nb_blocks. The argument block
                     is an array that associates to each state of
                     *a the index (between 0 and nb_blocks - 1)
                     of the class to which this state belongs.
                     The equivalence class that has the number 0
                     contains all the states that can be deleted
                     without influencing the language accepted by
                     the automaton. This equivalence class is the
                     only one that may be empty.

                     In case of error, this function returns a NULL
                     pointer.                                      **/

static automaton *mini_build(a, nb_blocks, blocks)
  automaton *a;
  uint4      nb_blocks, *blocks;
{
  register uint4      nb_states, alph_nbytes, i, j, *st_to_num;
  register automaton *ar;
  register tran       *t;
           uint4      p;

  nb_states   = auto_nb_states(a);
  alph_nbytes = auto_alphabet_nbytes(a); 

  ar = auto_new_empty(alph_nbytes);
  if (!ar)
    return NULL;
  
  st_to_num = resr__new_objects(uint4, nb_blocks - 1);
  if (!st_to_num)
    {
      auto_free(ar);
      return NULL;
    }

  for (i = 0; i < nb_blocks - 1; i++)
    if (auto_add_new_state(ar, st_to_num + i) < 0)
      {
	resr__free_objects(st_to_num, uint4, nb_blocks - 1);
	auto_free(ar);
	return NULL;
      }

  for (i = 0; i < nb_states; i++)
    if (blocks[i])
      {
	if (auto_nb_out_transitions(a, i, &p) == 0)
	  for (j = 0; j < p; j++)
	    {
	      t = auto_transition(a, i, j);
	      if (blocks[auto_transition_dest(t)] &&
	          auto_add_new_transition(ar, 
                  st_to_num[blocks[i] - 1], 
                  st_to_num[blocks[auto_transition_dest(t)] - 1],
                  auto_transition_length(t),
                  auto_transition_label_ptr(t, alph_nbytes)) < 0)
		{
		  resr__free_objects(st_to_num, uint4, nb_blocks - 1);
		  auto_free(ar);
		  return NULL;
		}
	    }
	if (auto_accepting_state(a, i))
	  auto_mark_accepting_state(ar, 
	      st_to_num[blocks[i] - 1]);
      }

  j = auto_nb_i_states(a);
  for (i = 0; i < j; i++)
    {
      auto_i_state(a, i, &p);
      if (blocks[p] && auto_add_new_i_state(ar, 
          st_to_num[blocks[p] - 1]) < 0)
	{
	  resr__free_objects(st_to_num, uint4, nb_blocks - 1);
	  auto_free(ar);
	  return NULL;
	}
    }

  resr__free_objects(st_to_num, uint4, nb_blocks - 1);
  pack_transitions(ar);

  return ar;
}

/****  Public visible functions.                                 ****/

/**  void  auto_set_mini_hsize(s)  :  Sets the size of the hash table
                     used by the minimization algorithm to s.
                     This function does not report errors.         **/

void  auto_set_mini_hsize(s)
  uint4 s;
{
  if (s)
    auto_mini_hsize = s;
}

/**  uint8  auto_get_mini_ncolls()  :  Returns the number of
                     collisions observed in the hash table used by the
                     minimization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8  auto_get_mini_ncolls()
{
  return auto_mini_ncolls;
}
#endif  /* >= 1 */

/**  void  auto_reset_mini_ncolls()  :  Resets the number of
                     collisions observed in the hash table used by the
                     minimization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_mini_ncolls()
{
  auto_mini_ncolls = ZERO_INT8;
}
#endif  /* >= 1 */

/**  uint8  auto_get_mini_nins()  :  Returns the number of insertions
                     performed in the hash table used by the
                     minimization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
uint8 auto_get_mini_nins()
{
  return auto_mini_nins;
}
#endif  /* >= 1 */

/**  void  auto_reset_mini_nins()  :  Resets the number of insertions
                     performed in the hash table used by the
                     minimization algorithm.  This function does
                     not report errors.                            **/

#if LASH_CHECK_LEVEL >= 1
void  auto_reset_mini_nins()
{
  auto_mini_nins = ZERO_INT8;
}
#endif  /* >= 1 */

/**  int  auto_prune(a)  :  Simplifies the finite-state automaton on
                     (in)finite words *a by removing all the states
		     that cannot be reached from an initial state, as
		     well as all the transitions leading to the
		     suppressed states.

                     In case of success, the function returns 0.
                     In case of error, it returns -1 and does not
                     modify *a.
 
                    Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
			 LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.  **/

int  auto_prune(a)
  automaton *a;
{
  register bit_table  *bt;
  register uint4       i, j, n, m, *st_to_new, *st_to_num;
  register automaton  *ar;
  register tran       *t;
  register int         is_determ;
           uint4       p;

  diag__enter("auto_prune", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE &&
      auto_word_type(a) != AUTO_WORDS_INFINITE)
    diag__fail(LASH_ERR_BAD_TYPE, -1);
  
  if (auto_test_property(a, AUTO_PROP_MINIMAL))
    diag__return(0);

  is_determ = auto_test_property(a, AUTO_PROP_DETERM);

  n = auto_nb_states(a);

  bt = bit__new_empty(n);
  if (!bt)
    diag__fail(LASH_ERR_NO_MEM, -1);

  m = auto_nb_i_states(a);
  for (i = 0; i < m; i++)
    {
       auto_i_state(a, i, &p);
       if (explore_reachable_states(a, bt, p) < 0)
	 {
	   bit__free(bt);
	   diag__fail(LASH_ERR_NO_MEM, -1); 
	 }
    }

  st_to_new = resr__new_objects(uint4, n);
  if (!st_to_new)
    {
      bit__free(bt);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  for (i = m = 0; i < n; i++)
    if (bit__member(bt, i))
      st_to_new[i] = m++;

  ar = auto_new_empty(auto_alphabet_nbytes(a));
  if (!ar)
    {
      bit__free(bt);
      resr__free_objects(st_to_new, uint4, n);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  st_to_num = resr__new_objects(uint4, m);
  if (!st_to_num)
    {
      bit__free(bt);
      resr__free_objects(st_to_new, uint4, n);
      auto_free(ar);
      diag__fail(LASH_ERR_NO_MEM, -1); 
    }

  for (i = 0; i < m; i++)
    if (auto_add_new_state(ar, st_to_num + i) < 0)
      {
	bit__free(bt);
	resr__free_objects(st_to_new, uint4, n);
	resr__free_objects(st_to_num, uint4, m);
	auto_free(ar);
        diag__fail(LASH_ERR_NO_MEM, -1); 
      }

  for (i = 0; i < n; i++)
    if (bit__member(bt, i))
      {
	if (auto_nb_out_transitions(a, i, &p) < 0)
	  {
	    bit__free(bt);
	    resr__free_objects(st_to_new, uint4, n);
	    resr__free_objects(st_to_num, uint4, m);
	    auto_free(ar);
	    diag__fail(LASH_ERR_CORRUPT, -1); 
	  }
	for (j = 0; j < p; j++)
	  {
	    t = auto_transition(a, i, j);
	    if (bit__member(bt, auto_transition_dest(t)) &&
	        auto_add_new_transition(ar,
                st_to_num[st_to_new[i]], 
                st_to_num[st_to_new[auto_transition_dest(t)]],
                auto_transition_length(t),
                auto_transition_label_ptr(t,
                auto_alphabet_nbytes(a))) < 0)
	      {
		bit__free(bt);
		resr__free_objects(st_to_new, uint4, n);
		resr__free_objects(st_to_num, uint4, m);
		auto_free(ar);
		diag__fail(LASH_ERR_NO_MEM, -1); 
	      }
	  }
	if (auto_accepting_state(a, i))
	  auto_mark_accepting_state(ar, 
	      st_to_num[st_to_new[i]]);
      }

  j = auto_nb_i_states(a);
  for (i = 0; i < j; i++)
    {
      auto_i_state(a, i, &p);
      if (bit__member(bt, p) && auto_add_new_i_state(ar,
          st_to_num[st_to_new[p]]) < 0)
	{
	  bit__free(bt);
	  resr__free_objects(st_to_new, uint4, n);
	  resr__free_objects(st_to_num, uint4, m);
	  auto_free(ar);
	  diag__fail(LASH_ERR_NO_MEM, -1); 
	}
    }

  bit__free(bt);
  resr__free_objects(st_to_new, uint4, n);
  resr__free_objects(st_to_num, uint4, m);

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = auto_accept_type(a);
      if (auto_test_property(a, AUTO_PROP_WEAK_NORMAL))
	auto_set_property(ar, AUTO_PROP_WEAK_NORMAL);
    }

  auto_replace(a, ar);

  if (is_determ)
    auto_set_property(a, AUTO_PROP_DETERM);

  diag__return(0);
}

/**  int  auto_minimize(a)  :  Minimizes the finite-state automaton on
                     finite or infinite words *a, i.e., transforms it
		     into the smallest deterministic automaton (in
		     terms of number of states) accepting the same
		     language. The minimization algorithm is
		     applicable to an automaton on infinite words if
		     and only if it is known to be deterministic 
                     weak. For that purpose, it uses the weak 
                     automata normalization algorithm proposed in
                     [Lod01].

                     In case of success, the function returns 0.  In
                     case of error, it returns -1, sets lash_errno,
                     and leaves in *a an automaton that accepts the
                     same language as the original one, but not
                     necessarily deterministic, minimal, or normal,
                     and possibly containing useless components.

                     Possible error codes:

                         LASH_ERR_NOT_INIT   : Not initialized.
                         LASH_ERR_BAD_TYPE   : Bad type of automaton.
                         LASH_ERR_NO_MEM     : Not enough memory.
                         LASH_ERR_CORRUPT    : Corrupt automaton.
			 LASH_ERR_TOO_BIG    : Automaton with too 
			                       many states.        **/

int  auto_minimize(a)
  automaton *a;
{
  register automaton *ar;
  register uint4      i, n;
           uint4      nb_blocks, *blocks;

  diag__enter("auto_minimize", -1);

  if (auto_word_type(a) != AUTO_WORDS_FINITE &&
      (auto_word_type(a) != AUTO_WORDS_INFINITE ||
       auto_accept_type(a) != AUTO_ACCEPT_WEAK ||
       !auto_test_property(a, AUTO_PROP_DETERM)))
    diag__fail(LASH_ERR_BAD_TYPE, -1);

  if (auto_test_property(a, AUTO_PROP_MINIMAL))
    diag__return(0);

  if (auto_prune(a) < 0 || auto_determinize(a) < 0)
    diag__fail(lash_errno, -1);
  
  n = auto_nb_states(a);
  for (i = 0; i < n; i++)
    if (auto_accepting_state(a, i))
      break;
  
  if (!n || (i >= n) || auto_nb_i_states(a) == 0)
    {
      ar = auto_new_empty(auto_alphabet_nbytes(a));
      if (!ar)
	diag__fail(LASH_ERR_NO_MEM, -1);

      if (auto_word_type(a) == AUTO_WORDS_INFINITE)
	{
  	  auto_word_type(ar) = AUTO_WORDS_INFINITE;
  	  auto_accept_type(ar) = auto_accept_type(a);
 	}

      auto_replace(a, ar);
      diag__return(0);
    }

  if ( (auto_word_type(a) == AUTO_WORDS_FINITE ?
	auto_normalize(a) : auto_weak_normalize(a)) < 0)
    diag__fail(lash_errno, -1);

  if (mini_partition(a, &nb_blocks, &blocks) < 0)
    diag__fail(LASH_ERR_NO_MEM, -1);

  ar = mini_build(a, nb_blocks, blocks);

  resr__free_objects(blocks, uint4, auto_nb_states(a));

  if (!ar)
    diag__fail(LASH_ERR_NO_MEM, -1);

  if (auto_word_type(a) == AUTO_WORDS_INFINITE)
    {
      auto_word_type(ar) = AUTO_WORDS_INFINITE;
      auto_accept_type(ar) = AUTO_ACCEPT_WEAK;
      auto_set_property(ar, AUTO_PROP_WEAK_NORMAL);
    }

  auto_replace(a, ar);

  auto_set_property(a, AUTO_PROP_DETERM);
  auto_set_property(a, AUTO_PROP_MINIMAL);

  diag__return(0);
}

/****  End of auto-minimize.c  ***/
