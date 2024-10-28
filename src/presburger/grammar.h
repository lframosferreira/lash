/********************************************************************/
/**                                                                **/
/**  Presburger  compiler -- v0.9                                  **/
/**  ============================                                  **/
/**                                                                **/
/**   grammar.h  :  Tables defining the syntax of the language     **/
/**                 being compiled. This file has been generated   **/
/**                 by the program 'grammar-generate'.             **/
/**  Copyright (c) 1998-2002, Universite de Liege (ULg). All       **/
/**  rights reserved. This package is provided for evaluation      **/
/**  purposes and educational use only. No guarantee is expressed  **/
/**  or implied by the distribution of this software. The          **/
/**  commercial use and the redistribution of this product or of   **/
/**  any part of it are not permitted without our prior, express,  **/
/**  and written consent.                                          **/
/**                                                                **/
/********************************************************************/

#define NB_SYMB     48
#define NB_TERM     25
#define NB_PROD     43

static uint1  l_table[NB_SYMB] = {
  0,   1,   2,   3,   4,   5,   4,   4,   4,   5,   2,   4,   4,   6,   4, 
  7,   1,   4,   5,   8,   3,   9,   9,  10,  11,  12,  13,  14,  15,   0, 
  0,   2,  16,  17,  17,  16,  17,  18,   8,   5,  19,   8,  20,  21,   7, 
  7,   7,  22 };

static uint1  sr_table[][(NB_TERM + 3) / 4] = {
{   0,   0,  16,  69,   0,  84,  84 },
{   0,   0,  16,  69,   0,  64,   0 },
{   0,   0,  32, 138,   0, 168, 160 },
{   0,   0,   0,   4,   0,   0,   0 },
{   0,   0,  32,  10,   0, 128,   0 },
{   0,   0,   0,   0,   0,   0,   0 },
{  90, 162, 130,  34, 138,   0,   2 },
{   0,   0,  16,   5,   0,  64,   0 },
{  10, 162, 130,  34, 138,   0,   2 },
{   0,   0,  16,   0,   0,   0,   0 },
{  10, 162, 130,  34, 138,   2,   2 },
{   0,   0,   0,   0,   0, 128,   0 },
{   2,   0,   0,   0,   0,   0,   0 },
{   2,   0,   0,   0, 129,   0,   0 },
{   6,   0,   0,   0, 130,   0,   0 },
{   2,   0,   0,   0, 130,   0,   0 },
{   0,   0,   0,   0,  64,   0,   0 },
{  10,   0,   0,   0, 130,   0,   0 },
{   0,   0,   0,   4,   0,  64,   0 },
{  10, 162, 130,  34, 138,   1,   2 },
{   0,  81,  65,  16,  64,   0,   0 },
{  10, 162, 130,  33, 134,   0,   1 },
{   0,   0,   0,   0,   0,  64,   0 }};

static uint1  start_rule[NB_SYMB] = {
  0 ,   0 ,   0 ,   0 ,   1 ,   0 ,   2 ,   3 ,   4 ,   5 ,   6 ,   7 , 
  8 ,  10 ,  13 ,   0 ,   0 ,  14 ,   0 ,  15 ,   0 ,   0 ,   0 ,  18 , 
 21 ,   0 ,  22 ,  25 ,  27 ,   0 ,   0 ,  28 ,   0 ,  31 ,  33 ,   0 , 
 35 ,   0 ,  36 ,   0 ,  37 ,  38 ,   0 ,  40 ,   0 ,   0 ,   0 ,   0  };


static uint1  end_rule[NB_SYMB] = {
  0 ,   0 ,   0 ,   0 ,   1 ,   0 ,   2 ,   3 ,   4 ,   5 ,   6 ,   7 , 
  9 ,  12 ,  13 ,   0 ,   0 ,  14 ,   0 ,  17 ,   0 ,   0 ,   0 ,  20 , 
 21 ,   0 ,  24 ,  26 ,  27 ,   0 ,   0 ,  30 ,   0 ,  32 ,  34 ,   0 , 
 35 ,   0 ,  36 ,   0 ,  37 ,  39 ,   0 ,  42 ,   0 ,   0 ,   0 ,   0  };


static uint1  derivation[NB_PROD][2] = {
{  0,  13} , {  0,  42} , {  0,  42} , {  0,  42} , {  0,  42} , 
{  0,   0} , {  0,   0} , {  0,  42} , {  0,   0} , {  0,  43} , 
{  0,   0} , { 40,  20} , {  0,  37} , {  0,  42} , {  0,  43} , 
{  0,  32} , {  0,  35} , { 30,  42} , {  0,   0} , {  0,  37} , 
{  0,  47} , { 45,  43} , {  0,   0} , {  0,  29} , {  0,  30} , 
{  0,   0} , { 26,  16} , {  0,   0} , { 13,   3} , { 21,  30} , 
{ 22,  30} , {  0,   0} , { 27,   1} , {  0,   0} , {  0,  15} , 
{  0,   0} , {  0,   0} , {  0,   0} , {  0,   0} , {  0,  44} , 
{  0,   0} , {  0,  45} , {  0,  46} };

static uint1  first_symbol[NB_PROD] = {
 31 ,  45 ,  46 ,  46 ,  46 ,  39 ,  30 ,  46 ,  37 ,  44 ,  38 ,  41 , 
 41 ,  46 ,  44 ,  28 ,  36 ,  41 ,  40 ,  40 ,  34 ,  47 ,  25 ,  32 , 
 35 ,  26 ,  26 ,  26 ,  31 ,  29 ,  29 ,  27 ,  27 ,  33 ,  33 ,  34 , 
 41 ,  41 ,  43 ,  43 ,  42 ,  34 ,  34  };

static uint1  rule_len[NB_PROD] = {
  2 ,   2 ,   2 ,   2 ,   2 ,   1 ,   1 ,   2 ,   1 ,   2 ,   1 ,   3 , 
  2 ,   2 ,   2 ,   2 ,   2 ,   3 ,   1 ,   2 ,   2 ,   3 ,   1 ,   2 , 
  2 ,   1 ,   3 ,   1 ,   3 ,   3 ,   3 ,   1 ,   3 ,   1 ,   2 ,   1 , 
  1 ,   1 ,   1 ,   2 ,   1 ,   2 ,   2  };


 /**** Semantic Rules *****/

/*

  0)  <vars>          ::= NAME ':'
  1)  <atom_cond1_eq> ::= <single_expr> EQ
  2)  <atom_cond1>    ::= <single_expr> GE
  3)  <atom_cond1>    ::= <single_expr> '>'
  4)  <atom_cond1>    ::= <single_expr> LE
  5)  <single_{>      ::= '{'
  6)  <single_(>      ::= '('
  7)  <atom_cond1>    ::= <single_expr> '<'
  8)  <single_minus>  ::= '-'
  9)  <expr_1>        ::= <expr> '-'
 10)  <single_name>   ::= NAME
 11)  <term>          ::= <const> '*' NAME
 12)  <term>          ::= <single_minus> NAME
 13)  <atom_cond1>    ::= <single_expr> NE
 14)  <expr_1>        ::= <expr> '+'
 15)  <quantif>       ::= <quantif_2> ')'
 16)  <cond_(_)>      ::= <cond_(> ')'
 17)  <term>          ::= <single_(> <single_expr> ')'
 18)  <const>         ::= CONST
 19)  <const>         ::= <single_minus> CONST
 20)  <atom_cond>     ::= <atom_cond_mod> CONST
 21)  <atom_cond_mod> ::= <atom_cond1_eq> <expr> MOD
 22)  <presburger>    ::= <condition>
 23)  <quantif_2>     ::= <quantif_1> <condition>
 24)  <cond_(>        ::= <single_(> <condition>
 25)  <condition>     ::= <cond1>
 26)  <condition>     ::= <condition> OR <cond1>
 27)  <condition>     ::= <quantif>
 28)  <vars>          ::= NAME ',' <vars>
 29)  <quantif_1>     ::= FORALL <single_(> <vars>
 30)  <quantif_1>     ::= EXISTS <single_(> <vars>
 31)  <cond1>         ::= <cond2>
 32)  <cond1>         ::= <cond1> AND <cond2>
 33)  <cond2>         ::= <atom_cond>
 34)  <cond2>         ::= NOT <atom_cond>
 35)  <atom_cond>     ::= <cond_(_)>
 36)  <term>          ::= <single_name>
 37)  <term>          ::= <const>
 38)  <expr>          ::= <term>
 39)  <expr>          ::= <expr_1> <term>
 40)  <single_expr>   ::= <expr>
 41)  <atom_cond>     ::= <atom_cond1_eq> <expr>
 42)  <atom_cond>     ::= <atom_cond1> <expr>

*/


 /**** List of SYMBOLS *****/


/*
 0 	 : DOLLAR 
 1 	 : AND 
 2 	 : ':' 
 3 	 : ',' 
 4 	 : EQ 
 5 	 : '!' 
 6 	 : GE 
 7 	 : '>' 
 8 	 : LE 
 9 	 : '{' 
 10 	 : '(' 
 11 	 : '<' 
 12 	 : '-' 
 13 	 : NAME 
 14 	 : NE 
 15 	 : NOT 
 16 	 : OR 
 17 	 : '+' 
 18 	 : '}' 
 19 	 : ')' 
 20 	 : '*' 
 21 	 : FORALL 
 22 	 : EXISTS 
 23 	 : CONST 
 24 	 : MOD 
 25 	 : <presburger> 
 26 	 : <condition> 
 27 	 : <cond1> 
 28 	 : <quantif> 
 29 	 : <quantif_1> 
 30 	 : <single_(> 
 31 	 : <vars> 
 32 	 : <quantif_2> 
 33 	 : <cond2> 
 34 	 : <atom_cond> 
 35 	 : <cond_(> 
 36 	 : <cond_(_)> 
 37 	 : <single_minus> 
 38 	 : <single_name> 
 39 	 : <single_{> 
 40 	 : <const> 
 41 	 : <term> 
 42 	 : <single_expr> 
 43 	 : <expr> 
 44 	 : <expr_1> 
 45 	 : <atom_cond1_eq> 
 46 	 : <atom_cond1> 
 47 	 : <atom_cond_mod> 
*/

/****  End of grammar.h  ****/
