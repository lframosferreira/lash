/********************************************************************/
/**                                                                **/
/**  Simple Promela LASH (SPLASH) compiler -- v0.9                 **/
/**  =====================================                         **/
/**                                                                **/
/**   grammar.h  :  Tables defining the syntax of the language     **/
/**                 being compiled. This file has been generated   **/
/**                 by the program 'grammar-generate'.             **/
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

#define NB_SYMB     70
#define NB_TERM     34
#define NB_PROD     75

static uint1  l_table[NB_SYMB] = {
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   1,   4,   1,   9,   1, 
  8,   1,  10,   6,   1,  11,   1,  12,   1,   4,   1,   9,  13,  14,  15, 
 10,   4,   9,   9,  16,  17,  13,  13,  18,  19,  20,   4,  21,   9,  22, 
 23,  24,   9,  25,  26,  27,   9,  22,  13,   2,  28,   4,  29,  30,  31, 
 32,  18,  19,  18,  19,  33,  34,   2,  35,   2 };

static uint1  sr_table[][(NB_TERM + 3) / 4] = {
{   0,   0,   0,   0,   0,   1,  16,   4,  84 },
{   0, 128,   0,   0,   0,  40,   0,   0,   0 },
{   0,  64,   0,   0,   0,  20,   0,   0,   0 },
{   0,   0,   0,   0,   4,   0,   0,   0,   0 },
{   0,   0, 128,   0,   0,   0, 130,  40,   0 },
{ 160, 130,   2, 136,   0,  40,   0, 128,   0 },
{   0,   0,   0,   0,   0,  32,   0,   0,   0 },
{   2,  32, 160,  34, 130, 138, 170,  40, 138 },
{   0,   0,   0,   0,   0,   0,   0,  16,   0 },
{   0,   0,   0,   0,   0,  16,   0,   0,   0 },
{ 160, 130,   2, 136,   0,  42,   0, 128,   8 },
{   0,   0,   0,   0,  16,   0,   0,   0,   0 },
{  10,  36, 164,  34, 134, 138, 170,  42, 136 },
{   2,   0, 128,   0,   0,   2, 162,  40, 136 },
{   0,  32,   0,   0,   0,   0,   0,   0,   0 },
{ 162, 130,   2, 136,   0,  42,  32, 136, 136 },
{   2,   0,   0,   0,   0,   1,  16,   4,  68 },
{   2,   0,   0,   0,   0,   2,  32,   8, 136 },
{ 160, 130,   2, 136,   0,  41,   0, 128,   4 },
{  80,  65,   1,  68,   0,  20,   0,  64,   0 },
{   0,   0,   0,   0,   0,   0,  64,   4,   0 },
{   6,  32, 128,   0,   0,   2, 162,  40, 136 },
{   2,  16, 128,   0,   0,   2, 162,  40, 136 },
{   0,  64,   0,   0,   0,   4,   0,   0,   0 },
{   2,  32, 160,  34, 130, 138, 170,  40, 137 },
{   0,  16,   0,   0,   0,   0,   0,   1,   0 },
{   2,  32, 128,   0,   0,   2, 162,  42, 136 },
{   0,  16,   0,   0,   0,   0,   0,   0,   0 },
{   0,   0, 160,  34, 130, 128, 130,  40,   0 },
{   0,   0,  64,   0,   0,   0,   0,  20,   0 },
{   0,   0,   0,   0,   0,   0,   1,  20,   0 },
{   4,   0, 160,  34, 130, 136, 138,  40,   0 },
{   8,   0, 160,  34, 130, 136, 138,  40,   0 },
{   0,  64,   0,   0,   0,   0,   0,   0,   0 },
{   0,   0, 160,  34, 130, 132, 134,  40,   0 },
{   0,   0,  16,  17,  65,  64,   0,   0,   0 }};

static uint1  start_rule[NB_SYMB] = {
  0 ,   0 ,   0 ,   0 ,   2 ,   3 ,   6 ,  10 ,   0 ,   0 ,  12 ,  13 , 
 14 ,   0 ,  15 ,   0 ,  16 ,  17 ,  18 ,  19 ,   0 ,  20 ,  22 ,  29 , 
 30 ,  31 ,   0 ,  32 ,  33 ,  34 ,  38 ,  42 ,   0 ,   0 ,   0 ,  43 , 
 45 ,  47 ,  51 ,   0 ,   0 ,  52 ,  55 ,   0 ,  56 ,   0 ,  57 ,   0 , 
  0 ,  59 ,   0 ,   0 ,  61 ,  62 ,   0 ,  63 ,  67 ,   0 ,   0 ,  69 , 
 71 ,  72 ,   0 ,  73 ,   0 ,   0 ,  74 ,   0 ,   0 ,   0  };

static uint1  end_rule[NB_SYMB] = {
  0 ,   1 ,   0 ,   0 ,   2 ,   5 ,   9 ,  11 ,   0 ,   0 ,  12 ,  13 , 
 14 ,   0 ,  15 ,   0 ,  16 ,  17 ,  18 ,  19 ,   0 ,  21 ,  28 ,  29 , 
 30 ,  31 ,   0 ,  32 ,  33 ,  37 ,  41 ,  42 ,   0 ,   0 ,   0 ,  44 , 
 46 ,  50 ,  51 ,   0 ,   0 ,  54 ,  55 ,   0 ,  56 ,   0 ,  58 ,   0 , 
  0 ,  60 ,   0 ,   0 ,  61 ,  62 ,   0 ,  66 ,  68 ,   0 ,   0 ,  70 , 
 71 ,  72 ,   0 ,  73 ,   0 ,   0 ,  74 ,   0 ,   0 ,   0  };

static uint1  derivation[NB_PROD][2] = {
{  0,  42} , {  0,  59} , {  0,   0} , { 39,  22} , { 62,  22} , 
{ 64,  22} , {  0,  44} , {  0,  48} , {  0,  50} , {  0,  52} , 
{  0,   0} , {  0,  65} , {  0,  68} , {  0,  57} , {  0,  68} , 
{  0,  68} , {  0,  68} , {  0,   0} , {  0,  20} , {  0,  68} , 
{  0,   0} , {  0,  66} , {  0,   0} , { 22,   9} , {  0,  13} , 
{  0,  33} , {  0,  43} , {  0,  47} , {  0,  51} , {  0,  68} , 
{  0,  58} , {  0,  66} , {  0,  40} , {  0,  48} , {  0,   0} , 
{  0,  40} , {  0,  57} , {  0,  58} , {  0,   8} , {  0,  15} , 
{  0,  57} , {  0,  58} , {  0,   0} , {  0,   0} , {  0,  34} , 
{  0,   3} , { 26,  22} , {  0,   0} , {  0,  38} , {  0,  61} , 
{  0,  63} , {  0,   0} , {  0,  39} , {  0,  62} , {  0,  64} , 
{  0,   0} , {  0,   0} , {  0,   0} , {  0,  45} , {  0,  47} , 
{  0,  51} , {  0,   0} , {  0,   0} , {  0,   0} , {  0,  54} , 
{  0,  67} , {  0,  69} , {  0,   0} , {  0,   2} , {  0,   0} , 
{ 46,  32} , {  0,   0} , {  0,   0} , {  0,   0} , {  0,   0} };


static uint1  first_symbol[NB_PROD] = {
 45 ,  54 ,  41 ,  39 ,  62 ,  64 ,  43 ,  47 ,  51 ,  51 ,  46 ,  46 , 
 69 ,  41 ,  69 ,  69 ,  69 ,  38 ,  47 ,  69 ,  65 ,  67 ,  60 ,  49 , 
 41 ,  42 ,  42 ,  48 ,  52 ,  69 ,  41 ,  67 ,  36 ,  50 ,  35 ,  38 , 
 61 ,  63 ,  63 ,  61 ,  61 ,  63 ,  41 ,  34 ,  34 ,  41 ,  35 ,  35 , 
 40 ,  57 ,  58 ,  39 ,  40 ,  57 ,  58 ,  44 ,  37 ,  66 ,  44 ,  48 , 
 52 ,  53 ,  37 ,  68 ,  41 ,  55 ,  56 ,  41 ,  41 ,  66 ,  66 ,  59 , 
 62 ,  64 ,  55  };

static uint1  rule_len[NB_PROD] = {
  2 ,   2 ,   1 ,   3 ,   3 ,   3 ,   2 ,   2 ,   2 ,   2 ,   1 ,   2 , 
  2 ,   2 ,   2 ,   2 ,   2 ,   1 ,   2 ,   2 ,   1 ,   2 ,   1 ,   3 , 
  2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   1 ,   2 , 
  2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   1 ,   1 ,   2 ,   2 ,   3 ,   1 , 
  2 ,   2 ,   2 ,   1 ,   2 ,   2 ,   2 ,   1 ,   1 ,   1 ,   2 ,   2 , 
  2 ,   1 ,   1 ,   1 ,   2 ,   2 ,   2 ,   1 ,   2 ,   1 ,   3 ,   1 , 
  1 ,   1 ,   1  };

/*

  0)  <vdecl_2>       ::= <vdecl_1> ASGN
  1)  <asgn_stmnt>    ::= <var_ref> ASGN
  2)  <stmnt>         ::= BREAK
  3)  <body_2>        ::= <body_2> NAME ':'
  4)  <if_stmnt_2>    ::= <if_stmnt_2> NAME ':'
  5)  <do_stmnt_2>    ::= <do_stmnt_2> NAME ':'
  6)  <vdecl_1b>      ::= <vdecl> ','
  7)  <mdecl_1>       ::= <mdecl_2> ','
  8)  <mdecl_3b>      ::= <mdecl_3> ','
  9)  <mdecl_3b>      ::= <mdecl_4> ','
 10)  <const>         ::= CONST
 11)  <const>         ::= <single_minus> CONST
 12)  <cond_1>        ::= <single_expr> EQ
 13)  <stmnt>         ::= <if_stmnt_3> FI
 14)  <cond_1>        ::= <single_expr> GE
 15)  <cond_1>        ::= <single_expr> '>'
 16)  <cond_1>        ::= <single_expr> LE
 17)  <body_1>        ::= '{'
 18)  <mdecl_1>       ::= META '('
 19)  <cond_1>        ::= <single_expr> '<'
 20)  <single_minus>  ::= '-'
 21)  <expr_1>        ::= <term> '-'
 22)  <single_name>   ::= NAME
 23)  <control>       ::= NAME '.' NAME
 24)  <stmnt>         ::= GOTO NAME
 25)  <vdecl_1>       ::= TYPE NAME
 26)  <vdecl_1>       ::= <vdecl_1b> NAME
 27)  <mdecl_2>       ::= <mdecl_1> NAME
 28)  <mdecl_4>       ::= <mdecl_3b> NAME
 29)  <cond_1>        ::= <single_expr> NE
 30)  <stmnt>         ::= <do_stmnt_3> OD
 31)  <expr_1>        ::= <term> '+'
 32)  <body>          ::= <body_3> '}'
 33)  <mdecl_3>       ::= <mdecl_2> ')'
 34)  <unit>          ::= ';'
 35)  <body_1>        ::= <body_3> ';'
 36)  <if_stmnt_1>    ::= <if_stmnt_3> ';'
 37)  <do_stmnt_1>    ::= <do_stmnt_3> ';'
 38)  <do_stmnt_1>    ::= DO SEP
 39)  <if_stmnt_1>    ::= IF SEP
 40)  <if_stmnt_1>    ::= <if_stmnt_3> SEP
 41)  <do_stmnt_1>    ::= <do_stmnt_3> SEP
 42)  <stmnt>         ::= SKIP
 43)  <program>       ::= <unit>
 44)  <program>       ::= <program> <unit>
 45)  <stmnt>         ::= ATOMIC <body>
 46)  <unit>          ::= PROCESS NAME <body>
 47)  <unit>          ::= <decl>
 48)  <body_3>        ::= <body_1> <decl>
 49)  <if_stmnt_3>    ::= <if_stmnt_1> <decl>
 50)  <do_stmnt_3>    ::= <do_stmnt_1> <decl>
 51)  <body_2>        ::= <body_1>
 52)  <body_3>        ::= <body_2> <stmnt>
 53)  <if_stmnt_3>    ::= <if_stmnt_2> <stmnt>
 54)  <do_stmnt_3>    ::= <do_stmnt_2> <stmnt>
 55)  <vdecl>         ::= <vdecl_1>
 56)  <decl>          ::= <vdecl>
 57)  <term>          ::= <const>
 58)  <vdecl>         ::= <vdecl_2> <const>
 59)  <mdecl_2>       ::= <mdecl_1> <control>
 60)  <mdecl_4>       ::= <mdecl_3b> <control>
 61)  <mdecl>         ::= <mdecl_4>
 62)  <decl>          ::= <mdecl>
 63)  <single_expr>   ::= <expr>
 64)  <stmnt>         ::= <asgn_stmnt> <expr>
 65)  <expr>          ::= <expr_1> <expr>
 66)  <cond>          ::= <cond_1> <expr>
 67)  <stmnt>         ::= <cond>
 68)  <stmnt>         ::= ASSERT <cond>
 69)  <term>          ::= <var_ref>
 70)  <term>          ::= <const> '*' <var_ref>
 71)  <var_ref>       ::= <single_name>
 72)  <if_stmnt_2>    ::= <if_stmnt_1>
 73)  <do_stmnt_2>    ::= <do_stmnt_1>
 74)  <expr>          ::= <term>

*/

/****  End of grammar.h  ****/
