/********************************************************************/
/**                                                                **/
/**  Simple IF LASH (Siflash) compiler -- v0.9                     **/
/**  =================================                             **/
/**                                                                **/
/**   semantic.c  :  Semantic routines.                            **/
/**                                                                **/
/**     11/15/99  :  Creation. (LL)                                **/
/**     12/15/00  :  Minor correction (LL).                        **/
/**     02/12/01  :  Minor correction (BB).                        **/
/**     12/13/01  :  New rules. (LL)                               **/
/**     04/29/02  :  Minor corrections. (LL)                       **/
/**     07/02/02  :  Modification: synchronisation. (LL)           **/
/**     07/25/02  :  Modification for IF2.0. (LL)                  **/
/**     09/06/02  :  Reorganization. (BB)                          **/
/**     09/13/02  :  Reorganization. (LL)                          **/
/**     10/17/02  :  Minor Correction. (LL)                        **/
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
#include "lash-types.h"
#include "resource.h"
#include "datastruct.h"
#include "siflash.h"
#include "lexical.h"
#include "semantic.h"
#include "program.h"

typedef struct {
  uint1 atomic;
  uint1 init;
} state_option;

typedef struct {
  char *name;
  uint1 type;
} trans_dest_info;

typedef struct {
  char *name;
  sint4 val;
  uint1 ignored;
} sem_variable ;

typedef struct {
  char *name;
  uint1 ignored;
} sem_type ; 

/****  Global variables.                                         ****/

static uint1            is_local;
                             /*  Are variable declarations local?   */
static uint4            nb_process_instances;

/****  Prototypes of private functions.                          ****/

static void  semantic_error(lex_unit *, char *);
static void  semantic_warning(lex_unit *, char *);
static int   create_new_process(char *, uint4);
static int   declare_new_variable(char *, uint1 , uint1, sint4);
static int   declare_new_constant(char *, sint4);
static int   declare_new_type(char *, uint1);

/****  Prototypes of private semantic functions.                 ****/

static int  sem0(stack *,  void **, uint1 *), 
 sem1(stack *, void **, uint1 *), 	 sem2(stack *, void **, uint1 *), 	
 sem3(stack *, void **, uint1 *), 	 sem4(stack *, void **, uint1 *), 	
 sem5(stack *, void **, uint1 *), 	 sem6(stack *, void **, uint1 *), 	
 sem7(stack *, void **, uint1 *), 	 sem8(stack *, void **, uint1 *), 	
 sem9(stack *, void **, uint1 *), 	 sem10(stack *, void **, uint1 *), 	
 sem11(stack *, void **, uint1 *), 	 sem12(stack *, void **, uint1 *), 	
 sem13(stack *, void **, uint1 *), 	 sem14(stack *, void **, uint1 *), 	
 sem15(stack *, void **, uint1 *), 	 sem16(stack *, void **, uint1 *), 	
 sem17(stack *, void **, uint1 *), 	 sem18(stack *, void **, uint1 *), 	
 sem19(stack *, void **, uint1 *), 	 sem20(stack *, void **, uint1 *), 	
 sem21(stack *, void **, uint1 *), 	 sem22(stack *, void **, uint1 *), 	
 sem23(stack *, void **, uint1 *), 	 sem24(stack *, void **, uint1 *), 	
 sem25(stack *, void **, uint1 *), 	 sem26(stack *, void **, uint1 *), 	
 sem27(stack *, void **, uint1 *), 	 sem28(stack *, void **, uint1 *), 	
 sem29(stack *, void **, uint1 *), 	 sem30(stack *, void **, uint1 *), 	
 sem31(stack *, void **, uint1 *), 	 sem32(stack *, void **, uint1 *), 	
 sem33(stack *, void **, uint1 *), 	 sem34(stack *, void **, uint1 *), 	
 sem35(stack *, void **, uint1 *), 	 sem36(stack *, void **, uint1 *), 	
 sem37(stack *, void **, uint1 *), 	 sem38(stack *, void **, uint1 *), 	
 sem39(stack *, void **, uint1 *), 	 sem40(stack *, void **, uint1 *), 	
 sem41(stack *, void **, uint1 *), 	 sem42(stack *, void **, uint1 *), 	
 sem43(stack *, void **, uint1 *), 	 sem44(stack *, void **, uint1 *), 	
 sem45(stack *, void **, uint1 *), 	 sem46(stack *, void **, uint1 *), 	
 sem47(stack *, void **, uint1 *), 	 sem48(stack *, void **, uint1 *), 	
 sem49(stack *, void **, uint1 *), 	 sem50(stack *, void **, uint1 *), 	
 sem51(stack *, void **, uint1 *), 	 sem52(stack *, void **, uint1 *), 	
 sem53(stack *, void **, uint1 *), 	 sem54(stack *, void **, uint1 *), 	
 sem55(stack *, void **, uint1 *), 	 sem56(stack *, void **, uint1 *), 	
 sem57(stack *, void **, uint1 *), 	 sem58(stack *, void **, uint1 *), 	
 sem59(stack *, void **, uint1 *), 	 sem60(stack *, void **, uint1 *), 	
 sem61(stack *, void **, uint1 *), 	 sem62(stack *, void **, uint1 *), 	
 sem63(stack *, void **, uint1 *), 	 sem64(stack *, void **, uint1 *), 	
 sem65(stack *, void **, uint1 *), 	 sem66(stack *, void **, uint1 *), 	
 sem67(stack *, void **, uint1 *), 	 sem68(stack *, void **, uint1 *), 	
 sem69(stack *, void **, uint1 *), 	 sem70(stack *, void **, uint1 *), 	
 sem71(stack *, void **, uint1 *), 	 sem72(stack *, void **, uint1 *), 	
 sem73(stack *, void **, uint1 *), 	 sem74(stack *, void **, uint1 *), 	
 sem75(stack *, void **, uint1 *), 	 sem76(stack *, void **, uint1 *), 	
 sem77(stack *, void **, uint1 *), 	 sem78(stack *, void **, uint1 *), 	
 sem79(stack *, void **, uint1 *), 	 sem80(stack *, void **, uint1 *), 	
 sem81(stack *, void **, uint1 *), 	 sem82(stack *, void **, uint1 *), 	
 sem83(stack *, void **, uint1 *), 	 sem84(stack *, void **, uint1 *), 	
 sem85(stack *, void **, uint1 *), 	 sem86(stack *, void **, uint1 *), 	
 sem87(stack *, void **, uint1 *), 	 sem88(stack *, void **, uint1 *), 	
 sem89(stack *, void **, uint1 *), 	 sem90(stack *, void **, uint1 *), 	
 sem91(stack *, void **, uint1 *), 	 sem92(stack *, void **, uint1 *), 	
 sem93(stack *, void **, uint1 *), 	 sem94(stack *, void **, uint1 *), 	
 sem95(stack *, void **, uint1 *), 	 sem96(stack *, void **, uint1 *), 	
 sem97(stack *, void **, uint1 *), 	 sem98(stack *, void **, uint1 *), 	
 sem99(stack *, void **, uint1 *), 	 sem100(stack *, void **, uint1 *), 	
 sem101(stack *, void **, uint1 *), 	 sem102(stack *, void **, uint1 *), 	
 sem103(stack *, void **, uint1 *), 	 sem104(stack *, void **, uint1 *), 	
 sem105(stack *, void **, uint1 *), 	 sem106(stack *, void **, uint1 *), 	
 sem107(stack *, void **, uint1 *), 	 sem108(stack *, void **, uint1 *), 	
 sem109(stack *, void **, uint1 *), 	 sem110(stack *, void **, uint1 *), 	
 sem111(stack *, void **, uint1 *), 	 sem112(stack *, void **, uint1 *), 	
 sem113(stack *, void **, uint1 *), 	 sem114(stack *, void **, uint1 *), 	
 sem115(stack *, void **, uint1 *), 	 sem116(stack *, void **, uint1 *), 	
 sem117(stack *, void **, uint1 *), 	 sem118(stack *, void **, uint1 *), 	
 sem119(stack *, void **, uint1 *), 	 sem120(stack *, void **, uint1 *), 	
 sem121(stack *, void **, uint1 *), 	 sem122(stack *, void **, uint1 *), 	
 sem123(stack *, void **, uint1 *), 	 sem124(stack *, void **, uint1 *), 	
 sem125(stack *, void **, uint1 *), 	 sem126(stack *, void **, uint1 *), 	
 sem127(stack *, void **, uint1 *), 	 sem128(stack *, void **, uint1 *), 	
 sem129(stack *, void **, uint1 *), 	 sem130(stack *, void **, uint1 *), 	
 sem131(stack *, void **, uint1 *), 	 sem132(stack *, void **, uint1 *), 	
 sem133(stack *, void **, uint1 *), 	 sem134(stack *, void **, uint1 *), 	
 sem135(stack *, void **, uint1 *), 	 sem136(stack *, void **, uint1 *), 	
 sem137(stack *, void **, uint1 *), 	 sem138(stack *, void **, uint1 *), 	
 sem139(stack *, void **, uint1 *), 	 sem140(stack *, void **, uint1 *), 	
 sem141(stack *, void **, uint1 *), 	 sem142(stack *, void **, uint1 *), 	
 sem143(stack *, void **, uint1 *), 	 sem144(stack *, void **, uint1 *), 	
 sem145(stack *, void **, uint1 *), 	 sem146(stack *, void **, uint1 *), 	
 sem147(stack *, void **, uint1 *), 	 sem148(stack *, void **, uint1 *), 	
 sem149(stack *, void **, uint1 *), 	 sem150(stack *, void **, uint1 *), 	
 sem151(stack *, void **, uint1 *), 	 sem152(stack *, void **, uint1 *), 	
 sem153(stack *, void **, uint1 *), 	 sem154(stack *, void **, uint1 *), 	
 sem155(stack *, void **, uint1 *), 	 sem156(stack *, void **, uint1 *), 	
 sem157(stack *, void **, uint1 *), 	 sem158(stack *, void **, uint1 *), 	
 sem159(stack *, void **, uint1 *), 	 sem160(stack *, void **, uint1 *), 	
 sem161(stack *, void **, uint1 *), 	 sem162(stack *, void **, uint1 *), 	
 sem163(stack *, void **, uint1 *), 	 sem164(stack *, void **, uint1 *), 	
 sem165(stack *, void **, uint1 *), 	 sem166(stack *, void **, uint1 *), 	
 sem167(stack *, void **, uint1 *), 	 sem168(stack *, void **, uint1 *), 	
 sem169(stack *, void **, uint1 *), 	 sem170(stack *, void **, uint1 *), 	
 sem171(stack *, void **, uint1 *), 	 sem172(stack *, void **, uint1 *), 	
 sem173(stack *, void **, uint1 *), 	 sem174(stack *, void **, uint1 *), 	
 sem175(stack *, void **, uint1 *), 	 sem176(stack *, void **, uint1 *), 	
 sem177(stack *, void **, uint1 *), 	 sem178(stack *, void **, uint1 *), 	
 sem179(stack *, void **, uint1 *), 	 sem180(stack *, void **, uint1 *), 	
 sem181(stack *, void **, uint1 *), 	 sem182(stack *, void **, uint1 *), 	
 sem183(stack *, void **, uint1 *), 	 sem184(stack *, void **, uint1 *), 	
 sem185(stack *, void **, uint1 *), 	 sem186(stack *, void **, uint1 *), 	
 sem187(stack *, void **, uint1 *), 	 sem188(stack *, void **, uint1 *), 	
 sem189(stack *, void **, uint1 *), 	 sem190(stack *, void **, uint1 *), 	
 sem191(stack *, void **, uint1 *), 	 sem192(stack *, void **, uint1 *), 	
 sem193(stack *, void **, uint1 *), 	 sem194(stack *, void **, uint1 *), 	
 sem195(stack *, void **, uint1 *), 	 sem196(stack *, void **, uint1 *), 	
 sem197(stack *, void **, uint1 *), 	 sem198(stack *, void **, uint1 *), 	
 sem199(stack *, void **, uint1 *), 	 sem200(stack *, void **, uint1 *), 	
 sem201(stack *, void **, uint1 *), 	 sem202(stack *, void **, uint1 *), 	
 sem203(stack *, void **, uint1 *), 	 sem204(stack *, void **, uint1 *), 	
 sem205(stack *, void **, uint1 *), 	 sem206(stack *, void **, uint1 *), 	
 sem207(stack *, void **, uint1 *), 	 sem208(stack *, void **, uint1 *), 	
 sem209(stack *, void **, uint1 *), 	 sem210(stack *, void **, uint1 *), 	
 sem211(stack *, void **, uint1 *), 	 sem212(stack *, void **, uint1 *), 	
 sem213(stack *, void **, uint1 *), 	 sem214(stack *, void **, uint1 *), 	
 sem215(stack *, void **, uint1 *), 	 sem216(stack *, void **, uint1 *), 	
 sem217(stack *, void **, uint1 *), 	 sem218(stack *, void **, uint1 *), 	
 sem219(stack *, void **, uint1 *), 	 sem220(stack *, void **, uint1 *), 	
 sem221(stack *, void **, uint1 *), 	 sem222(stack *, void **, uint1 *), 	
 sem223(stack *, void **, uint1 *), 	 sem224(stack *, void **, uint1 *), 	
 sem225(stack *, void **, uint1 *), 	 sem226(stack *, void **, uint1 *), 	
 sem227(stack *, void **, uint1 *), 	 sem228(stack *, void **, uint1 *), 	
 sem229(stack *, void **, uint1 *), 	 sem230(stack *, void **, uint1 *), 	
 sem231(stack *, void **, uint1 *), 	 sem232(stack *, void **, uint1 *), 	
 sem233(stack *, void **, uint1 *), 	 sem234(stack *, void **, uint1 *), 	
 sem235(stack *, void **, uint1 *), 	 sem236(stack *, void **, uint1 *), 	
 sem237(stack *, void **, uint1 *), 	 sem238(stack *, void **, uint1 *), 	
 sem239(stack *, void **, uint1 *), 	 sem240(stack *, void **, uint1 *), 	
 sem241(stack *, void **, uint1 *), 	 sem242(stack *, void **, uint1 *), 	
 sem243(stack *, void **, uint1 *), 	 sem244(stack *, void **, uint1 *), 	
 sem245(stack *, void **, uint1 *), 	 sem246(stack *, void **, uint1 *), 	
 sem247(stack *, void **, uint1 *), 	 sem248(stack *, void **, uint1 *), 	
 sem249(stack *, void **, uint1 *), 	 sem250(stack *, void **, uint1 *), 	
 sem251(stack *, void **, uint1 *), 	 sem252(stack *, void **, uint1 *), 	
 sem253(stack *, void **, uint1 *), 	 sem254(stack *, void **, uint1 *), 	
 sem255(stack *, void **, uint1 *), 	 sem256(stack *, void **, uint1 *), 	
 sem257(stack *, void **, uint1 *), 	 sem258(stack *, void **, uint1 *), 	
 sem259(stack *, void **, uint1 *), 	 sem260(stack *, void **, uint1 *), 	
 sem261(stack *, void **, uint1 *), 	 sem262(stack *, void **, uint1 *), 	
 sem263(stack *, void **, uint1 *), 	 sem264(stack *, void **, uint1 *), 	
 sem265(stack *, void **, uint1 *), 	 sem266(stack *, void **, uint1 *), 	
 sem267(stack *, void **, uint1 *), 	 sem268(stack *, void **, uint1 *), 	
 sem269(stack *, void **, uint1 *), 	 sem270(stack *, void **, uint1 *), 	
 sem271(stack *, void **, uint1 *), 	 sem272(stack *, void **, uint1 *), 	
 sem273(stack *, void **, uint1 *), 	 sem274(stack *, void **, uint1 *), 	
 sem275(stack *, void **, uint1 *), 	 sem276(stack *, void **, uint1 *), 	
 sem277(stack *, void **, uint1 *), 	 sem278(stack *, void **, uint1 *), 	
 sem279(stack *, void **, uint1 *), 	 sem280(stack *, void **, uint1 *), 	
 sem281(stack *, void **, uint1 *), 	 sem282(stack *, void **, uint1 *), 	
 sem283(stack *, void **, uint1 *), 	 sem284(stack *, void **, uint1 *), 	
 sem285(stack *, void **, uint1 *), 	 sem286(stack *, void **, uint1 *), 	
 sem287(stack *, void **, uint1 *), 	 sem288(stack *, void **, uint1 *), 	
 sem289(stack *, void **, uint1 *), 	 sem290(stack *, void **, uint1 *), 	
 sem291(stack *, void **, uint1 *), 	 sem292(stack *, void **, uint1 *), 	
 sem293(stack *, void **, uint1 *), 	 sem294(stack *, void **, uint1 *), 	
 sem295(stack *, void **, uint1 *), 	 sem296(stack *, void **, uint1 *), 	
 sem297(stack *, void **, uint1 *), 	 sem298(stack *, void **, uint1 *), 	
 sem299(stack *, void **, uint1 *), 	 sem300(stack *, void **, uint1 *), 	
 sem301(stack *, void **, uint1 *), 	 sem302(stack *, void **, uint1 *), 	
 sem303(stack *, void **, uint1 *), 	 sem304(stack *, void **, uint1 *), 	
 sem305(stack *, void **, uint1 *), 	 sem306(stack *, void **, uint1 *), 	
 sem307(stack *, void **, uint1 *), 	 sem308(stack *, void **, uint1 *), 	
 sem309(stack *, void **, uint1 *), 	 sem310(stack *, void **, uint1 *), 	
 sem311(stack *, void **, uint1 *), 	 sem312(stack *, void **, uint1 *), 	
 sem313(stack *, void **, uint1 *), 	 sem314(stack *, void **, uint1 *), 	
 sem315(stack *, void **, uint1 *), 	 sem316(stack *, void **, uint1 *), 	
 sem317(stack *, void **, uint1 *), 	 sem318(stack *, void **, uint1 *), 	
 sem319(stack *, void **, uint1 *), 	 sem320(stack *, void **, uint1 *), 	
 sem321(stack *, void **, uint1 *), 	 sem322(stack *, void **, uint1 *), 	
 sem323(stack *, void **, uint1 *), 	 sem324(stack *, void **, uint1 *), 	
 sem325(stack *, void **, uint1 *), 	 sem326(stack *, void **, uint1 *), 	
 sem327(stack *, void **, uint1 *), 	 sem328(stack *, void **, uint1 *), 	
 sem329(stack *, void **, uint1 *), 	 sem330(stack *, void **, uint1 *), 	
 sem331(stack *, void **, uint1 *), 	 sem332(stack *, void **, uint1 *), 	
 sem333(stack *, void **, uint1 *), 	 sem334(stack *, void **, uint1 *), 	
 sem335(stack *, void **, uint1 *), 	 sem336(stack *, void **, uint1 *), 	
 sem337(stack *, void **, uint1 *), 	 sem338(stack *, void **, uint1 *), 	
 sem339(stack *, void **, uint1 *), 	 sem340(stack *, void **, uint1 *), 	
 sem341(stack *, void **, uint1 *), 	 sem342(stack *, void **, uint1 *), 	
 sem343(stack *, void **, uint1 *), 	 sem344(stack *, void **, uint1 *), 	
 sem345(stack *, void **, uint1 *), 	 sem346(stack *, void **, uint1 *), 	
 sem347(stack *, void **, uint1 *), 	 sem348(stack *, void **, uint1 *), 	
 sem349(stack *, void **, uint1 *), 	 sem350(stack *, void **, uint1 *), 	
 sem351(stack *, void **, uint1 *), 	 sem352(stack *, void **, uint1 *), 	
 sem353(stack *, void **, uint1 *), 	 sem354(stack *, void **, uint1 *), 	
 sem355(stack *, void **, uint1 *), 	 sem356(stack *, void **, uint1 *), 	
 sem357(stack *, void **, uint1 *), 	 sem358(stack *, void **, uint1 *), 	
 sem359(stack *, void **, uint1 *), 	 sem360(stack *, void **, uint1 *), 	
 sem361(stack *, void **, uint1 *), 	 sem362(stack *, void **, uint1 *), 	
 sem363(stack *, void **, uint1 *), 	 sem364(stack *, void **, uint1 *), 	
 sem365(stack *, void **, uint1 *), 	 sem366(stack *, void **, uint1 *), 	
 sem367(stack *, void **, uint1 *), 	 sem368(stack *, void **, uint1 *), 	
 sem369(stack *, void **, uint1 *), 	 sem370(stack *, void **, uint1 *), 	
 sem371(stack *, void **, uint1 *), 	 sem372(stack *, void **, uint1 *), 	
 sem373(stack *, void **, uint1 *), 	 sem374(stack *, void **, uint1 *), 	
sem375(stack *, void ** , uint1 *); 

/****  Public variable.                                          ****/

int  (*sem_function[])(stack *, void **,uint1 *) = 
{
 sem0,   
 sem1,    sem2,    sem3,    sem4,    sem5,    sem6,   
 sem7,    sem8,    sem9,    sem10,   sem11,   sem12,  
 sem13,   sem14,   sem15,   sem16,   sem17,   sem18,  
 sem19,   sem20,   sem21,   sem22,   sem23,   sem24,  
 sem25,   sem26,   sem27,   sem28,   sem29,   sem30,  
 sem31,   sem32,   sem33,   sem34,   sem35,   sem36,  
 sem37,   sem38,   sem39,   sem40,   sem41,   sem42,  
 sem43,   sem44,   sem45,   sem46,   sem47,   sem48,  
 sem49,   sem50,   sem51,   sem52,   sem53,   sem54,  
 sem55,   sem56,   sem57,   sem58,   sem59,   sem60,  
 sem61,   sem62,   sem63,   sem64,   sem65,   sem66,  
 sem67,   sem68,   sem69,   sem70,   sem71,   sem72,  
 sem73,   sem74,   sem75,   sem76,   sem77,   sem78,  
 sem79,   sem80,   sem81,   sem82,   sem83,   sem84,  
 sem85,   sem86,   sem87,   sem88,   sem89,   sem90,  
 sem91,   sem92,   sem93,   sem94,   sem95,   sem96,  
 sem97,   sem98,   sem99,   sem100,  sem101,  sem102, 
 sem103,  sem104,  sem105,  sem106,  sem107,  sem108, 
 sem109,  sem110,  sem111,  sem112,  sem113,  sem114, 
 sem115,  sem116,  sem117,  sem118,  sem119,  sem120, 
 sem121,  sem122,  sem123,  sem124,  sem125,  sem126, 
 sem127,  sem128,  sem129,  sem130,  sem131,  sem132, 
 sem133,  sem134,  sem135,  sem136,  sem137,  sem138, 
 sem139,  sem140,  sem141,  sem142,  sem143,  sem144, 
 sem145,  sem146,  sem147,  sem148,  sem149,  sem150, 
 sem151,  sem152,  sem153,  sem154,  sem155,  sem156, 
 sem157,  sem158,  sem159,  sem160,  sem161,  sem162, 
 sem163,  sem164,  sem165,  sem166,  sem167,  sem168, 
 sem169,  sem170,  sem171,  sem172,  sem173,  sem174, 
 sem175,  sem176,  sem177,  sem178,  sem179,  sem180, 
 sem181,  sem182,  sem183,  sem184,  sem185,  sem186, 
 sem187,  sem188,  sem189,  sem190,  sem191,  sem192, 
 sem193,  sem194,  sem195,  sem196,  sem197,  sem198, 
 sem199,  sem200,  sem201,  sem202,  sem203,  sem204, 
 sem205,  sem206,  sem207,  sem208,  sem209,  sem210, 
 sem211,  sem212,  sem213,  sem214,  sem215,  sem216, 
 sem217,  sem218,  sem219,  sem220,  sem221,  sem222, 
 sem223,  sem224,  sem225,  sem226,  sem227,  sem228, 
 sem229,  sem230,  sem231,  sem232,  sem233,  sem234, 
 sem235,  sem236,  sem237,  sem238,  sem239,  sem240, 
 sem241,  sem242,  sem243,  sem244,  sem245,  sem246, 
 sem247,  sem248,  sem249,  sem250,  sem251,  sem252, 
 sem253,  sem254,  sem255,  sem256,  sem257,  sem258, 
 sem259,  sem260,  sem261,  sem262,  sem263,  sem264, 
 sem265,  sem266,  sem267,  sem268,  sem269,  sem270, 
 sem271,  sem272,  sem273,  sem274,  sem275,  sem276, 
 sem277,  sem278,  sem279,  sem280,  sem281,  sem282, 
 sem283,  sem284,  sem285,  sem286,  sem287,  sem288, 
 sem289,  sem290,  sem291,  sem292,  sem293,  sem294, 
 sem295,  sem296,  sem297,  sem298,  sem299,  sem300, 
 sem301,  sem302,  sem303,  sem304,  sem305,  sem306, 
 sem307,  sem308,  sem309,  sem310,  sem311,  sem312, 
 sem313,  sem314,  sem315,  sem316,  sem317,  sem318, 
 sem319,  sem320,  sem321,  sem322,  sem323,  sem324, 
 sem325,  sem326,  sem327,  sem328,  sem329,  sem330, 
 sem331,  sem332,  sem333,  sem334,  sem335,  sem336, 
 sem337,  sem338,  sem339,  sem340,  sem341,  sem342, 
 sem343,  sem344,  sem345,  sem346,  sem347,  sem348, 
 sem349,  sem350,  sem351,  sem352,  sem353,  sem354, 
 sem355,  sem356,  sem357,  sem358,  sem359,  sem360, 
 sem361,  sem362,  sem363,  sem364,  sem365,  sem366, 
 sem367,  sem368,  sem369,  sem370,  sem371,  sem372, 
 sem373,  sem374, sem375 };

/****  Private functions.                                        ****/

/**  void  semantic_error(p, msg)  :  Reports a semantic error that
                    has been detected during the parsing of the
                    lexical unit *p. A message to be included in
                    the report is given by *msg.                   **/

static void  semantic_error(p, msg)
  lex_unit *p;
  char     *msg;
{
  char line[256];

  sprintf(line, 
      "Compilation error at (L%u, C%u): %s", p -> line_no, 
      p -> char_no, msg);

  report_siflash_error(line);
}

/**  void  semantic_warning(p, msg)  :  Reports a semantic warning 
                    that has been detected during the parsing of the
                    lexical unit *p. A message to be included in
                    the report is given by *msg.
		    A warning does not stop execution.
		    This function does not report errors.          **/

static void  semantic_warning(p, msg)
  lex_unit *p;
  char     *msg;
{
  fprintf(stderr, 
      "Compilation warning at (L%u, C%u): %s.\n", p -> line_no, 
      p -> char_no, msg);
}

/**  int  create_new_process(name, nb) : Creates a new process.
                    of name name and having nb instances at run time.
                    Returns 0 in the case of success, and -1 if there
                    is not enough memory.                          **/

static int  create_new_process(name, nb)
char *name;
uint4 nb;
{
  nb_process_instances = nb;
  
  if (pgm_new_process(name, nb) < 0)
    return -1;

  is_local = 1;

  return 0;
}

/** int   declare_new_variable(name, is_parameter, ignored, val) :
                    Declares a new variable whose identifier is name,
		    and unallocates the memory in which name is
		    stored.  This variable is a free parameter
		    (non-deterministic value) if is_parameter > 0.
		    The initial value is val (if not a parameter) and
		    the variable is ignored (unaccepted type) if
		    ignored > 0.  Returns 0 in the case of success,
		    and -1 in the case of an error (a message is then
		    output).                                       **/

static int   declare_new_variable(name, is_parameter, ignored, val)
  char *name;
  uint1 ignored, is_parameter;
  sint4 val;
{
  register uint1   flag;
  register int   (*f)(char *, uint1, uint1, sint4);

  f = (is_local) ? pgm_declare_local_variable : 
    pgm_declare_global_variable;
  flag = ignored || (is_local && (nb_process_instances == 0));
 
  return f(name, is_parameter, flag , val);
}

/** int   declare_new_constant(char *, sint4) :
                    Declares a new constant whose identifier is name
                    and whose value is val, and unallocates the memory
                    in which name is stored.  Returns 0 in the case of
                    success, and -1 in the case of an error (a message
                    is then output).                               **/

static int  declare_new_constant(name, val)
  char *name;
  sint4 val;
{
  int (*f)(char *, sint4);

  f = (is_local) ? pgm_declare_local_constant :
    pgm_declare_global_constant;

  return f(name, val);
}

/** int   declare_new_type(name, ignored) :
                   Declares the new type of name name.  If ignored =
                   0, then the type is equivalent to the integer type.
                   If ignored > 0, then the type is not equivalent to
                   the integer type and must be ignored.  Frees the
                   string name.  Return 0 in the case of success and
                   -1 in the case of failure.  Reports errors.     **/

static int   declare_new_type(name, ignored)
  char *name; 
  uint1 ignored;
{
  register int (*f)(char *, uint1);
   
  f = (is_local) ? pgm_declare_local_type:
    pgm_declare_global_type;

  return f(name, ignored);
}

/****  Public functions.                                         ****/

/**  int  sem_init()  :  Initializes the semantic analyzer. Returns
                    0 in the case of success, and -1 if there is not
                    enough memory.                                 **/

int  sem_init()
{
   is_local = 0;
  
  if (pgm_init() < 0)
    {
      report_siflash_memory_error();
      return -1;
    }

  return 0;
}

/**  int  sem_end()  :  Signals the end of the syntactic parsing.
                    Returns 0 in the case of success, and -1 in the 
                    case of an error.                              **/

int  sem_end()
{
  return  pgm_end();
}

/**  int  sem_finish()  :  Shuts down the semantic analyzer. Returns
                    0 in the case of success, and -1 in the case of
                    an error.                                      **/

int  sem_finish()
{
  if (pgm_finish() < 0)
    return -1;
  return 0;
}

/****  Private semantic functions.                               ****/

/*  <abstract_1>    ::= ABSTRACT */
static int sem0(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Abstract type not accepted. Ignored");
  *b =1;
  return 0;
}

/*  <left_asgn>     ::= <expr_4> ':=' */
static int sem1(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  uint4    v, *v2;
  register pgm_expression *expr;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  expr = (pgm_expression *) lex -> param;
  if ((expr -> type != PGM_EXPR_LIN)
      || (pgm_extract_variable((pgm_lin_expr *) expr -> expr, &v)
	  < 0))
    {
      semantic_error(lex, "Expression is not a valid left value");
      pgm_free_expression(expr);
      return -1;
    }

  if (pgm_is_parameter(v) > 0)
    {
      semantic_error(lex, "Left value is a parameter");
      pgm_free_expression(expr);
      return -1;
    }

  pgm_free_expression(expr);

  v2 = resr__new_object(uint4);
  if (!v2)
    {
      report_siflash_memory_error();
      pgm_free_expression(expr);
      return -1;
    }

  *v2 = v;
  *r = v2;
  return 0;
}

/*  <type_id>       ::= BOOLEAN */
static int sem2(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning(((lex_unit *) stack__top(st)),
		   "Boolean type not handled. Ignored");
  *b = 1;
  return 0;
 }

/*  <meta_trans_def_4> ::= <meta_trans_def_1> BY */
static int sem3(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  pgm_meta_sequence_begin();
  return 0;
}

/*  <call_0>        ::= CALL */
static int sem4(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
		   "Procedure calls not handled. Ignored");
  return 0;
}

/*  <call_0>        ::= <left_asgn> CALL */
static int sem5(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      resr__free_object((uint4 *) lex -> param, uint4);
    }

  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
		   "Procedure calls not handled. Ignored");
  return 0;
}

/*  <type_id>       ::= CLOCK */

static int sem6(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
		   "Clock type not handled. Ignored");
  return 0;
}

/*  <procedure_decl_5> ::= <procedure_decl_4> CODE_EXT */

static int sem7(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_1>        ::= COEFF */
static int sem8(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_lin_expr *lexpr;
  register pgm_expression *expr;
  register sint4 *val;
 
  val = (sint4 *) ((lex_unit *) stack__top(st)) -> param;

  lexpr = pgm_create_lin_expr_const(*val);
  resr__free_object(val, sint4);
  if (!lexpr) 
    {
      report_siflash_memory_error();
      return -1;
    }

  expr = resr__new_object(pgm_expression);
  if (!expr) 
    {
      pgm_free_lin_expr(lexpr);
      report_siflash_memory_error();
      return -1;
    }

  expr -> type = PGM_EXPR_LIN;
  expr -> expr = lexpr;
  *r = expr;

  return 0;
}

/*  <type>          ::= <range_1> '..' COEFF */
static int sem9(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <range_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <array_2>       ::= <array_1b> '..' COEFF */
static int sem10(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_1b> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <range_1>       ::= RANGE COEFF */
static int sem11(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  lex_free_token((lex_unit *) stack__top(st));
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Range type not handled. Ignored");
  return 0;
}

/*  <var_decl_4>    ::= <var_decl_4a> COEFF */
static int sem12(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register sem_variable *v;

  v = (sem_variable *) ((lex_unit *)stack__pick(st,1)) -> param;
  v -> val =  * (sint4 *) ((lex_unit *) stack__top(st)) -> param;

  lex_free_token((lex_unit *) stack__top(st));
  *r = v;
  return 0;
}

/*  <var_decl_4>    ::= <var_decl_4b> COEFF */
static int sem13(st, r, b)
stack *st;
  void **r;
 uint1 *b;
{ 
  register sem_variable *v;

  v = (sem_variable *) ((lex_unit *)stack__pick(st,1)) -> param;
  v -> val =  * (sint4 *) ((lex_unit *) stack__top(st)) -> param;
  v -> val *= (-1) ; 
  lex_free_token((lex_unit *) stack__top(st));
  *r = v;
   return 0;
 }


/*  <process_decl_1b> ::= <process_decl_1a> COEFF */

static int sem14(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char *name;
  register sint4 *coeff;
  register uint4 nb;

  name = (char *) ((lex_unit *) stack__pick(st,1)) -> param;
  coeff = (sint4 *) ((lex_unit *) stack__top(st)) -> param;
  if (*coeff < 0)
    {
      semantic_error((lex_unit *) stack__top(st),
		     "Unexpected negative number");
      pgm_free_string(name);
      lex_free_token((lex_unit *) stack__top(st));
    }

  nb = (uint4) *coeff;
  resr__free_object(coeff, sint4);
  if (create_new_process(name, nb) < 0)
      return -1;

  if (nb == 0)
    semantic_warning((lex_unit *) stack__top(st),
		     "Zero instance. Process ignored");
  return 0;
}

/*  <signalroute_decl_1b> ::= <signalroute_decl_1a> COEFF */
static int sem15(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0)
    {
      semantic_error(lex, 
		     "Internal: <signal_route_decl_1a> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <delay_2>       ::= <delay_1> COEFF */
static int sem16(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;


  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <delay_4>       ::= <delay_3> COEFF */
static int sem17(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay_3> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <rate_2>        ::= <rate_1> COEFF */
static int sem18(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <rate_4>        ::= <rate_3> COEFF */
static int sem19(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <const_decl_3>  ::= <const_decl_2> COEFF */
static int sem20(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char *name;
  register sint4 val, *v;

  name = (char *) ((lex_unit *)stack__pick(st,1)) -> param;
  v    = (sint4 *) ((lex_unit *) stack__top(st)) -> param;
  val  =  *v;

  resr__free_object(v, sint4);
  return declare_new_constant(name, val);
}

/*  <const_decl_3>  ::= <const_decl_3a> COEFF */
static int sem21(st, r, b)
stack *st;
  void **r;
 uint1 *b;
{ 
  register char *name;
  register sint4 val, *v;

  name = (char *) ((lex_unit *)stack__pick(st,1)) -> param;
  v    = (sint4 *) ((lex_unit *) stack__top(st)) -> param;
  val  =  *v;
  val  =  val * (-1);

  resr__free_object(v, sint4);
  return declare_new_constant(name, val);
 }


/*  <array_1b>      ::= <array_1> COEFF */
static int sem22(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <string_2>      ::= <string_1> COEFF */
static int sem23(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <string_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_17_3>     ::= <expr_17_2> ':' */
static int sem24(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <expr_17_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <fpar_3>        ::= <fpar_2> ',' */
static int sem25(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <save_signal_2> ::= <save_signal> ',' */
static int sem26(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <save_signal> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <function_2>    ::= <single_name_(> <single_expression> ',' */
static int sem27(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  lex = (lex_unit *) stack__pick(st, 2);
  lex_free_token(lex);

  semantic_warning(lex, "Fonction calls not handled. Ignored");

  *b = 1;
  return 0;
}

/*  <function_2>    ::= <function_2> <single_expression> ',' */
static int sem28(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  lex = (lex_unit *) stack__pick(st, 2);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <function_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <input_3>       ::= <input_2> ',' */
static int sem29(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <meta_trans_def_2> ::= <meta_trans_def_1> ',' */
static int sem30(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <meta_trans_def_4> ::= <meta_trans_def_5> ',' */
static int sem31(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <meta_trans_def_4> ::= <meta_trans_def_6> ',' */
static int sem32(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <output_3>      ::= <output_2> ',' */
static int sem33(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <call_2b>       ::= <call_2> ',' */
static int sem34(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call_2> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <fork_2b>       ::= <fork_2> ',' */
static int sem35(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_2> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <signalroute_decl_6> ::= <signalroute_decl_5> ',' */
static int sem36(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_5> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <delay_3>       ::= <delay_2> ',' */
static int sem37(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay_2> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <rate_3>        ::= <rate_2> ',' */
static int sem38(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signal_decl_2b> ::= <signal_decl_2> ',' */
static int sem39(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signalroute_decl_5> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <enum_1b>       ::= <enum_1> ',' */
static int sem40(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <enum_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_6>    ::= <abstract_5> ',' */
static int sem41(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_5> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <meta_trans_def_1b> ::= <meta_trans_def_3> '.' */
static int sem42(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (is_local)
    {
      semantic_error((lex_unit *) stack__top(st),
		     "Meta-transition label not local");
      return -1;
    }
  
  return 0;
}

/*  <meta_trans_def_6b> ::= <meta_trans_def_5> '.' */
static int sem43(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (is_local)
    {
      semantic_error((lex_unit *) stack__top(st),
		     "Meta-transition label not local");
      return -1;
    }
  
  return 0;
}

/*  <expr_3a>       ::= <expr_3> '.' */
static int sem44(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  semantic_warning((lex_unit *) stack__top(st),
		   "'.' operator  not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <type>          ::= <abstract_1> ENDABSTRACT */
static int sem45(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <abstract_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <assert>        ::= <assert_1> ENDASSERT */
static int sem46(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored > 0)
    {
      *b = 1;
      return 0;
    }
  *r = lex -> param;
  return 0;
}

/*  <type>          ::= <enum_1> ENDENUM */
static int sem47(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <enum_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <if>            ::= <if_2> ELSE ENDIF */
static int sem48(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <if_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <if>            ::= <if_2> ENDIF */
static int sem49(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <if>            ::= <if_3> ENDIF */
static int sem50(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <type>          ::= <record_1> ENDRECORD */
static int sem51(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <record_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <while>         ::= <while_2> ENDWHILE */
static int sem52(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
    semantic_error(lex, "Internal: <while_2> not ignored");
    return -1;
  }

  *b = 1;
  return 0;
}

/*  <signalroute_decl_3> ::= <signalroute_decl_2> FROM ENV */
static int sem53(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_route_decl_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_decl_4> ::= <signalroute_decl_3> TO ENV */
static int sem54(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 2);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_route_decl_3> not ignored");
      return -1;
    }

  *b = 1;

  return 0;
}

/*  <single_=>      ::= '=' */
static int sem55(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <var_decl_4a>   ::= <var_decl_3> '=' */
static int sem56(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;

  return 0;
}

/*  <const_decl_2>  ::= <const_decl_1> '=' */
static int sem57(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;

  return 0;
}

/*  <type_decl_2>   ::= <type_decl_1> '=' */
static int sem58(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st,1)) -> param;

  return 0;
}

/*  <const>         ::= FALSE */
static int sem59(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
                   "Boolean literal not handled. Ignored");
  return 0;
}

/*  <signalroute_option> ::= FIFO */
static int sem60(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> FIFO */
static int sem61(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
  }

  *b = 1;

  return 0;
}

/*  <type_id>       ::= FLOAT */
static int sem62(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning(((lex_unit *) stack__top(st)),
		   "Float type not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <fork_0>        ::= FORK */
static int sem63(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning(((lex_unit *) stack__top(st)),
     "Dynamic creation of processes not handled. Ignored");

  *b = 1;
  return 0;
}

/*  <fork_0>        ::= <left_asgn> FORK */
static int sem64(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    resr__free_object((uint4 *) lex -> param, uint4);
  
  semantic_warning(((lex_unit *) stack__top(st)),
    "Dynamic creation of processes not handled. Ignored");

  *b = 1;
  return 0;
}

/*  <fpar_opt>      ::= IN */
static int sem65(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Parameter option In ignored");
  *b = 1;
   return 0;
}

/*  <fpar_opt>      ::= INOUT */
static int sem66(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Parameter option InOut ignored");
  *b = 1;
  return 0;
}

/*  <type_id>       ::= INTEGER */
static int sem67(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <single_(>      ::= '(' */
static int sem68(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <name_(>        ::= NAME '(' */
static int sem69(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);

  *r = lex -> param;
  return 0;
}

/*  <array_1>       ::= ARRAY '[' */
static int sem70(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Array type not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <delay_1>       ::= DELAY '[' */
static int sem71(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Delay feature not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <rate_1>        ::= RATE '[' */
static int sem72(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Rate feature not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <string_1>      ::= STRING '[' */
static int sem73(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "String literals not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <expr_3_b1>     ::= <expr_3> '[' */
static int sem74(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  semantic_warning((lex_unit *) stack__top(st),
		   "Arrays not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= LOSSY */
static int sem75(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> LOSSY */
static int sem76(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <single_->      ::= '-' */
static int sem77(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <terminator_1>  ::= NEXTSTATE '-' */
static int sem78(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register trans_dest_info *info;

  info = resr__new_object(trans_dest_info);
  if (!info)
    report_siflash_memory_error();

  info -> type = PGM_TRANS_TYPE_SELF;
  info -> name = NULL;
  *r = info;
  return 0;
}


/*  <var_decl_4b>   ::= <var_decl_4a> '-' */
static int sem79(st, r, b)
stack *st;
  void **r;
 uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st,1)) -> param;
   return 0;
 }

/*  <const_decl_3a> ::= <const_decl_2> '-' */
static int sem80(st, r, b)
stack *st;
  void **r;
 uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st,1)) -> param;  
  return 0;
 }

/*  <expr_12_->     ::= <expr_12> '-' */
static int sem81(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__pick(st, 1)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__pick(st, 1)) -> param; 

  return 0;
}


/*  <signalroute_option> ::= MULTICAST */
static int sem82(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b =1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> MULTICAST */
static int sem83(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
  }

  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= MULTISET */
static int sem84(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> MULTISET */
static int sem85(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex,
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_2>        ::= NAME */
static int sem86(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
           uint4          p;
           uint1          is_ignored;
           sint4          c;
           char           *name;
  register pgm_lin_expr   *lexpr;
  register pgm_expression *expr;
  register lex_unit       *lex;

  expr = resr__new_object(pgm_expression);

  if (!expr) 
    {
      report_siflash_memory_error();
      lex_free_token((lex_unit *) stack__top(st));
      return -1;
    }

  lex = (lex_unit *) stack__top(st);
  name = (char *) lex -> param;
  if (pgm_lookup_variable(&p, NULL, &is_ignored, name) < 0)
    {
      if (pgm_lookup_constant(&c, name) < 0)
	{
	  char line[256];
	  sprintf(line, 
		  "%s is neither a variable nor a constant. Ignored", 
		  name);
	  semantic_warning((lex_unit *) stack__top(st),line);
	  lex_free_token((lex_unit *) stack__top(st));
	  resr__free_object(expr, pgm_expression);
	  *b = 1;
	  return 0;
	}
      else 
	{
	  lexpr = pgm_create_lin_expr_const(c);
	  if (!lexpr) 
	    {
	      report_siflash_memory_error();
	      lex_free_token((lex_unit *) stack__top(st));
	      resr__free_object(expr, pgm_expression);
	      return -1;
	    }
	}
    }
  else
    {
      if (is_ignored)
	{
	  lex_free_token((lex_unit *) stack__top(st));
	  resr__free_object(expr, pgm_expression);
	  *b = 1;
	  return 0;
	}
      
      lexpr = pgm_create_lin_expr(1, p);
      if (!lexpr) 
	{
	  report_siflash_memory_error();
	  lex_free_token((lex_unit *) stack__top(st));
	  resr__free_object(expr, pgm_expression);
	  return -1;
	}
    }
  
  expr -> type = PGM_EXPR_LIN;
  expr -> expr = lexpr;
  
  lex_free_token((lex_unit *) stack__top(st));
  *r = expr;

  return 0;
}

/*  <const_decl_1>  ::= CONST NAME */
static int sem87(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <type>          ::= <range_1> '..' NAME */
static int sem88(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <range_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <array_2>       ::= <array_1b> '..' NAME */
static int sem89(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_1b> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <enum_1>        ::= ENUM NAME */
static int sem90(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  lex_free_token((lex_unit *) stack__top(st));
  semantic_warning((lex_unit *) stack__pick(st,1),
		   "Enumeration types not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <fpar_1>        ::= FPAR NAME */
static int sem91(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <from_name>     ::= FROM NAME */
static int sem92(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <label_1>       ::= LABEL NAME */
static int sem93(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <terminator_1>  ::= NEXTSTATE NAME */
static int sem94(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  register char *name;
  register trans_dest_info *info;
  

  lex = (lex_unit *) stack__top(st);
  name = (char *) lex -> param;
  info = resr__new_object(trans_dest_info);
  if (!info)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
    }

  info -> type = PGM_TRANS_TYPE_NAME;
  info -> name = name;
  *r = info;
  return 0;
}

/*  <type>          ::= <array_3> OF NAME */
static int sem95(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <type>          ::= <string_3> OF NAME */
static int sem96(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <string_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <procedure_decl_1> ::= PROCEDURE NAME */
static int sem97(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Procedures not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <range_1>       ::= RANGE NAME */
static int sem98(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  lex_free_token((lex_unit *) stack__top(st));
  semantic_warning((lex_unit *) stack__pick(st,1),
		   "Range types not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <procedure_decl_4a> ::= <procedure_decl_3> RETURNS NAME */
static int sem99(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <save_signal_1> ::= SAVE NAME */
static int sem100(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Signals not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  return 0;
}

/*  <system_1a>     ::= SYSTEM NAME */
static int sem101(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  lex_free_token((lex_unit *) stack__top(st));     
  return 0;
}

/*  <type_decl_1>   ::= TYPE NAME */
static int sem102(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
   return 0;
}

/*  <signalroute_decl_5> ::= <signalroute_decl_4> WITH NAME */
static int sem103(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <var_decl_2>    ::= <var_decl_1> NAME */
static int sem104(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register sem_variable *v;
  v = resr__new_object(sem_variable);
  if (!v)
    {
      report_siflash_memory_error();
      return -1;
    }
  v -> name = (char *) ((lex_unit *) stack__top(st)) -> param;
  v -> ignored = 0;
  v -> val = 0;
  *r = v;

  return 0;
}

/*  <var_decl_3>    ::= <var_decl_2> NAME */
static int sem105(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
           uint1         ignored;
  register sem_variable *v;
  register char         *name;
  register lex_unit     *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  v = (sem_variable *) lex -> param;
  lex = (lex_unit *) stack__top(st);
  name = (char *) lex -> param;

  if (pgm_lookup_type(name, &ignored) < 0)
    {
      char line[256];
      sprintf(line, "Unknown type: %s", name);
      pgm_free_string(name);
      resr__free_object(v, sem_variable);
      semantic_error(lex,line);
      return -1;
    }

  pgm_free_string(name);
  if (ignored > 0)
    {
      semantic_warning(lex, "Variable of ignored type. Ignored");
      v -> ignored = 1;
    }

  *r = v;

  return 0;
}

/*  <var_decl_4>    ::= <var_decl_4a> NAME */
static int sem106(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{
  sem_variable *v;
  sint4 val;
  char *name;

  name = (char *) ((lex_unit *) stack__top(st)) -> param;
  v    = (sem_variable *) ((lex_unit *)stack__pick(st,1)) -> param;

  if (pgm_lookup_constant(&val, name) < 0)
    {
      char line[256];
      sprintf(line, "%s is not a constant", name);
      pgm_free_string(name);
      pgm_free_string(v -> name);
      resr__free_object(v, sem_variable);
      semantic_error((lex_unit *) stack__top(st), line);
      return -1;
    }
  v -> val =  val;
  lex_free_token((lex_unit *) stack__top(st));
  *r = v;
  return 0;

}

/*  <process_decl_1b> ::= <process_decl_1a> NAME */
static int sem107(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  sint4 val;
  char *name1, *name0;
  name0 = (char *) ((lex_unit *) stack__top(st)) -> param;
  name1 = (char *) ((lex_unit *) stack__pick(st,1)) -> param;

  if (pgm_lookup_constant(&val, name0) < 0)
    {
      char line[256];
      sprintf(line, "%s is not a constant", name0);
      pgm_free_string(name0);
      pgm_free_string(name1);
      semantic_error((lex_unit *) stack__top(st), line);
      return -1;
    }

  if (val < 0)
    {
      semantic_error((lex_unit *) stack__top(st),
		     "Unexpected negative number");
      pgm_free_string(name0);
      pgm_free_string(name1);
      return -1;
    }

  if (create_new_process(name1, val) < 0)
    {
      pgm_free_string(name0);
      pgm_free_string(name1);
      return -1;
    }

  pgm_free_string(name0);
  
  return 0;
}

/*  <fpar_2>        ::= <fpar_1> NAME */
static int sem108(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{
  uint1 ignored;
  register char *name1, *name0;
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  name1 = (char *) lex -> param; 
  lex = (lex_unit *) stack__top(st); 
  name0 = (char *) lex -> param; 
  if (pgm_lookup_type(name0, &ignored) < 0)
    { 
      char line[256];
      sprintf(line, "Unknown type: %s", name0);
      pgm_free_string(name0);
      pgm_free_string(name1);
      semantic_error(lex,line);
      return -1;
    }

  pgm_free_string(name0);
  if (ignored > 0)
      semantic_warning(lex, "Parameter of ignored type. Ignored");
 
  if (pgm_store_fpar_elt(name1, ignored) < 0)
    return -1;

  return 0;
}

/*  <fpar_1>        ::= FPAR <fpar_opt> NAME */
static int sem109(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st,1);
  if (lex -> ignored == 0) 
    {
      lex_free_token((lex_unit *) stack__top(st));
      semantic_error(lex, "Internal: <fpar_opt> not ignored");
      return -1;
    }

  *r =  ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <fpar_1>        ::= <fpar_3> <fpar_opt> NAME */
static int sem110(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st,1);
  if (lex -> ignored == 0) 
    {
      lex_free_token((lex_unit *) stack__top(st));
      semantic_error(lex, "Internal: <fpar_opt> not ignored");
      return -1;
    }
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <fpar_1>        ::= <fpar_3> NAME */
static int sem111(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r =  ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <state_decl_2>  ::= <state_decl_1> NAME */
static int sem112(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register uint4 *s;
  register char  *name;

  name = (char *) ((lex_unit *) stack__top(st)) -> param;

  s = resr__new_object(uint4);
  if (!s)
    {
      pgm_free_string(name);
      report_siflash_memory_error();
      return -1;
    }
  if  (pgm_new_state(name, s) < 0)
    return -1;

  *r = s;

  return 0;
}

/*  <save_signal>   ::= <save_signal_2> NAME */
static int sem113(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <save_signal_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <meta_trans_def_3> ::= <meta_trans_def_2> NAME */
static int sem114(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st); 
  if ((!is_local) && (pgm_meta_process((char *) lex -> param) < 0))
    return -1;

  if  ((is_local) && (pgm_meta_label((char *) lex -> param) < 0))
    return -1;
  
  return 0;
}

/*  <meta_trans_def_1> ::= <meta_trans_def_1b> NAME */
static int sem115(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  lex = (lex_unit *) stack__top(st) ;
  if (pgm_meta_label((char *) lex -> param) < 0)
      return -1;

  return 0;
}

/*  <meta_trans_def_5> ::= <meta_trans_def_4> NAME */
static int sem116(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (is_local && (pgm_meta_label((char *) lex -> param) < 0))
      return -1;
   
  if (!is_local && (pgm_meta_process((char *) lex -> param) < 0))
      return -1;

  return 0;
}

/*  <meta_trans_def_6> ::= <meta_trans_def_6b> NAME */
static int sem117(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (pgm_meta_label((char *) lex -> param) < 0)
    return -1;

  return 0;
}

/*  <signalroute_decl_1b> ::= <signalroute_decl_1a> NAME */
static int sem118(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_1a> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_decl_4> ::= <signalroute_decl_4a> NAME */
static int sem119(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_4a> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <signalroute_decl_5> ::= <signalroute_decl_6> NAME */
static int sem120(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_6> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signal_decl_2> ::= <signal_decl_1> NAME */
static int sem121(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signal_decl_2> ::= <signal_decl_2b> NAME */
static int sem122(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_2b> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <const_decl_3>  ::= <const_decl_2> NAME */
static int sem123(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char *name1, *name0;
           sint4 val;
           uint1 is_parameter;
           uint4 no;

  name1 = (char *) ((lex_unit *) stack__pick(st, 1)) -> param;
  name0 = (char *) ((lex_unit *) stack__top(st)) -> param;

  if (pgm_lookup_constant(&val, name0) < 0) 
    {
      if (pgm_lookup_variable(&no, &is_parameter,NULL, name0) < 0)
	{
	  semantic_error((lex_unit *) stack__top(st),
			 "Invalid constant declaration");
	  pgm_free_string(name1);
	  pgm_free_string(name0);
	  return -1;
	}
      else
	{
	  if (!is_parameter)
	    {
	      semantic_error((lex_unit *) stack__top(st),
			     "Invalid constant declaration");
	      pgm_free_string(name1);
	      pgm_free_string(name0);
	      return -1;
	    }
	  pgm_free_string(name0);
	  return pgm_declare_parameter_alias(name1, no, is_local);
	}
    }
  else
    {
      pgm_free_string(name0);
      return declare_new_constant(name1, val);
    } 
}

/*  <enum_1>        ::= <enum_1b> NAME */
static int sem124(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex,
		     "Internal: <enum_1b> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <array_1b>      ::= <array_1> NAME */
static int sem125(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <array_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <record_2>      ::= <record_1> NAME */
static int sem126(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <record_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <record_3>      ::= <record_2> NAME */
static int sem127(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <record_2> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <string_2>      ::= <string_1> NAME */
static int sem128(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <string_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_2>    ::= <abstract_1> NAME */
static int sem129(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_1> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <abstract_5>    ::= <abstract_3> NAME */
static int sem130(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_5>    ::= <abstract_6> NAME */
static int sem131(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_6> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <const>         ::= NIL */
static int sem132(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
                   "Pid type not handled. Ignored");

  return 0;
}

/*  <fpar_opt>      ::= OUT */
static int sem133(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Parameter option Out ignored");
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= PEER */
static int sem134(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> PEER */
static int sem135(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <type_id>       ::= PID */
static int sem136(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning(((lex_unit *) stack__top(st)),
		   "Pid type not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <single_+>      ::= '+' */
static int sem137(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

   return 0;
}

/*  <expr_12_+>     ::= <expr_12> '+' */
static int sem138(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__pick(st, 1)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__pick(st, 1)) -> param; 

  return 0;
}

/*  <expr_17_1>     ::= <expr_16> '?' */
static int sem139(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  semantic_warning((lex_unit *) stack__top(st),
		   "Conditional expressions not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <record_1>      ::= RECORD */
static int sem140(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  semantic_warning(lex, "Record type not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= RELIABLE */
static int sem141(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> RELIABLE */
static int sem142(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_16_1>     ::= '{' NAME '}' */
static int sem143(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  lex_free_token((lex_unit *) stack__pick(st, 1));
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 2),
		   "Casting not supported. Ignored");
  return 0;
}

/*  <expr_16_1>     ::= '{' <type_id> '}' */
static int sem144(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 2),
		   "Casting not supported. Ignored");
  *b = 1;
  return 0;
}

/*  <process_decl_1c> ::= <process_decl_1b> ')' */
static int sem145(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <expr_4>        ::= <single_name_(> <single_expression> ')' */
static int sem146(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  *b = 1;
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  lex = (lex_unit *) stack__pick(st, 2);

  pgm_free_string((char *) lex -> param);
  semantic_warning(lex, "Fonction calls not handled. Ignored");

  return 0;
}

/*  <expr_4>        ::= <function_2> <single_expression> ')' */
static int sem147(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      pgm_free_expression((pgm_expression *) lex -> param);
    }

 lex = (lex_unit *) stack__pick(st, 2);
 if (lex -> ignored == 0) 
   {
     semantic_error(lex, "Internal: <function_2> not ignored");
     return -1;
   }

 *b = 1;
 return 0;
}

/*  <expr_5>        ::= <single_(> <single_expression> ')' */
static int sem148(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    *r =  lex -> param;
  else
    *b = 1;
  return 0;
}

/*  <input_4>       ::= <input_1> ')' */
static int sem149(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <input_4>       ::= <input_2> ')' */
static int sem150(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <output_4>      ::= <output_1> ')' */
static int sem151(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <output_4>      ::= <output_2> ')' */
static int sem152(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <call>          ::= <call_1> ')' */
static int sem153(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <call>          ::= <call_2> ')' */
static int sem154(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call_2> not ignored");
      return -1;
    }

  *b = 1;
   return 0;
}

/*  <fork>          ::= <fork_1> ')' */
static int sem155(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <fork>          ::= <fork_2> ')' */
static int sem156(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_2> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <signalroute_decl_1> ::= <signalroute_decl_1b> ')' */
static int sem157(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_1b> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signal_decl_3> ::= <signal_decl_1> ')' */
static int sem158(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signal_decl_3> ::= <signal_decl_2> ')' */
static int sem159(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_4>    ::= <abstract_3> ')' */
static int sem160(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_4>    ::= <abstract_5> ')' */
static int sem161(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
 register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_5> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_4>        ::= <single_name_(> ')' */
static int sem162(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  semantic_warning(lex, "Function calls not handled. Ignored");
  pgm_free_string((char *) lex -> param);

  *b = 1;
  return 0;
}

/*  <expr_3>        ::= <expr_3_b1> <single_expression> ']' */
static int sem163(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      pgm_free_expression((pgm_expression *) lex -> param);
    }

  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <expr_3_b1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <delay>         ::= <delay_4> ']' */
static int sem164(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <rate>          ::= <rate_4> ']' */
static int sem165(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate_4> not ignored");
      return -1;
    }
 
 *b = 1;
  return 0;
}

/*  <array_3>       ::= <array_2> ']' */
static int sem166(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <string_3>      ::= <string_2> ']' */
static int sem167(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <string_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <const>         ::= SELF */
static int sem168(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__top(st),
                   "Pid type not handled. Ignored");

  return 0;
}

/*  <deadline>      ::= DEADLINE DELAYABLE ';' */
static int sem169(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 2),
		   "Deadline option not handled. Ignored");
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Delayable option not handled. Ignored");
  return 0;
}

/*  <deadline>      ::= DEADLINE EAGER ';' */
static int sem170(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 2),
		   "Deadline option not accepted. Ignored");
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Eager option not accepted. Ignored");
  return 0;
}

/*  <procedure_decl> ::= <procedure_decl_5> ENDPROCEDURE ';' */
static int sem171(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_5> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <process_decl>  ::= <process_decl_3> ENDPROCESS ';' */
static int sem172(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  is_local = 0;

  return pgm_cleanup_current_process();
}

/*  <state_decl>    ::= <state_decl_7> ENDSTATE ';' */
static int sem173(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st, 2)) -> param;

  return 0;
}

/*  <system>        ::= <system_2> ENDSYSTEM ';' */
static int sem174(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <deadline>      ::= DEADLINE LAZY ';' */
static int sem175(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 2),
		   "Deadline option not accepted. Ignored");
  
  return 0;
}

/*  <var_decl>      ::= <var_decl_4> PRIVATE ';' */
static int sem176(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register sem_variable *v;
  register lex_unit     *lex;

  semantic_warning((lex_unit *) stack__pick(st,1),
		   "Private option ignored");

  lex = (lex_unit *) stack__pick(st, 2);
  v = (sem_variable *) lex -> param;

  if (declare_new_variable(v -> name, 0, v -> ignored, v -> val) < 0)
    {
      resr__free_object(v, sem_variable);
      return -1;
    }

  resr__free_object(v, sem_variable);
  
  return 0;
}

/*  <var_decl>      ::= <var_decl_4> PUBLIC ';' */
static int sem177(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register sem_variable *v;
  register lex_unit *lex;

  semantic_warning((lex_unit *) stack__pick(st,1),
		   "Public option ignored");

  lex = (lex_unit *) stack__pick(st, 2);
  v = (sem_variable *) lex -> param;

  if (declare_new_variable(v -> name, 0, v -> ignored, v -> val) < 0)
    {
      resr__free_object(v, sem_variable);
      return -1;
    }
  resr__free_object(v, sem_variable);

  return 0;
}

/*  <system_1>      ::= <system_1a> ';' */
static int sem178(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <var_decl>      ::= <var_decl_4> ';' */
static int sem179(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register sem_variable *v;
  register lex_unit     *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  v = (sem_variable *) lex -> param;

  if (declare_new_variable(v -> name, 0, v -> ignored, v -> val) < 0)
    {
      resr__free_object(v, sem_variable);
      return -1;
    }
  resr__free_object(v, sem_variable);

  return 0;
}

/*  <process_decl_1> ::= <process_decl_1c> ';' */
static int sem180(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <fpar>          ::= <fpar_2> ';' */
static int sem181(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <state_decl_3>  ::= <state_decl_2> ';' */
static int sem182(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;

  return 0;
}

/*  <state_decl_3>  ::= <state_decl_2> <state_option> ';' */
static int sem183(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit     *lex;
  register uint4        *s;
  register state_option *opt;

  lex = (lex_unit *) stack__pick(st, 1);
  opt = (state_option *) lex -> param;
  lex  = (lex_unit *) stack__pick(st, 2);
  s = (uint4 *) lex -> param;

  pgm_set_state_options(*s, opt -> init, opt -> atomic);
  resr__free_object(opt, state_option);
  *r = s;

  return 0;
}

/*  <label>         ::= <label_1> ';' */
static int sem184(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <provided>      ::= PROVIDED <single_expression> ';' */
static int sem185(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register  pgm_expression *expr;
  register lex_unit        *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  expr = (pgm_expression *) lex -> param;
  if (expr -> type != PGM_EXPR_COND)
    {
      semantic_warning(lex, "Expression is not a valid condition");
      pgm_free_expression(expr);
      *b = 1;
      return 0;
    }

  pgm_store_pre_cond((pgm_gen_condition *) expr -> expr);
  resr__free_object(expr, pgm_expression);

  return 0;
}

/*  <when>          ::= <when_1> ';' */
static int sem186(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <when_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <input>         ::= <input_4> ';' */
static int sem187(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <terminator>    ::= <terminator_1> ';' */
static int sem188(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{
  *r = ((lex_unit *) stack__pick(st,1)) -> param;
  return 0;
}

/*  <meta_trans_def> ::= <meta_trans_def_5> ';' */
static int sem189(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <meta_trans_def> ::= <meta_trans_def_6> ';' */
static int sem190(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <action>        ::= <action_a1> ';' */
static int sem191(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 

  /* Note that the only non-ignored action is an assignment,
     stored as an pgm_operation structure. */

  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored > 0) 
    {
      *b = 1;
      return 0;
    }

  *r = lex -> param;
  return 0;
}

/*  <action>        ::= <call> ';' */
static int sem192(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <action>        ::= <fork> ';' */
static int sem193(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_decl> ::= <signalroute_decl_4> ';' */
static int sem194(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_decl> ::= <signalroute_decl_5> ';' */
static int sem195(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_5> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <signal_decl>   ::= <signal_decl_3> ';' */
static int sem196(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <const_decl>    ::= <const_decl_1> ';' */
static int sem197(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char *name;

  name = (char *) ((lex_unit *) stack__pick(st, 1)) -> param;
  return declare_new_variable(name, 1, 0, 0);
}

/*  <const_decl>    ::= <const_decl_3> ';' */
static int sem198(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <type_decl>     ::= <type_decl_2> <type> ';' */
static int sem199(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  char              *name;
  register lex_unit *lex;
  register uint1     ignored;

  lex = (lex_unit *) stack__pick(st, 2);
  name = (char *) lex -> param;
  lex = (lex_unit *) stack__pick(st, 1);
  ignored = (lex -> ignored > 0) ? 1 : 0;

  if (declare_new_type(name, ignored) < 0)
    return -1;

  return 0;
}

/*  <record_1>      ::= <record_3> ';' */
static int sem200(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <record_3> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <abstract_1>    ::= <abstract_4> ';' */
static int sem201(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_4> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <procedure_decl_2> ::= <procedure_decl_1> ';' */
static int sem202(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <procedure_decl_4> ::= <procedure_decl_4a> ';' */
static int sem203(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <procedure_decl_4a> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <action_a1>     ::= SKIP */
static int sem204(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  /* SKIP is dealt with by ignoring the action. */

  *b = 1;
  return 0;
}

/*  <state_option>  ::= STABLE */
static int sem205(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register state_option *opt;

  opt = resr__new_object(state_option);
  if (!opt)
    {
      report_siflash_memory_error();
      return -1;
    }

  opt -> init = 0;
  opt -> atomic = 0;
  *r = opt;

  return 0;
}

/*  <state_option>  ::= <state_option> STABLE */
static int sem206(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  register state_option *opt;

  lex = (lex_unit *) stack__pick(st, 1);
  opt = (state_option *) lex -> param;
  opt -> atomic = 0;
  *r = opt;

  return 0;
}

/*  <state_option>  ::= START */
static int sem207(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register state_option *opt;

  opt = resr__new_object(state_option);
  if (!opt)
    {
      report_siflash_memory_error();
      return -1;
    }

  opt -> init = 1;
  opt -> atomic = 0;
  *r = opt;

  return 0;
}

/*  <state_option>  ::= <state_option> START */
static int sem208(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit     *lex;
  register state_option *opt;

  lex = (lex_unit *) stack__pick(st, 1);
  opt = (state_option *) lex -> param;
  opt -> init = 1;
  *r = opt;

  return 0;
}

/*  <state_decl_1>  ::= STATE */
static int sem209(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <terminator_1>  ::= STOP */
static int sem210(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register trans_dest_info *info;

  info = resr__new_object(trans_dest_info);
  if (!info)
    report_siflash_memory_error();

  info -> type = PGM_TRANS_TYPE_STOP;
  info -> name = NULL;
  *r = info;
  return 0;
}

/*  <action_a1>     ::= INFORMAL STRING_EXPR */
static int sem211(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st,1),
		   "Informal actions not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <type_id>       ::= TIMER */
static int sem212(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning(((lex_unit *) stack__top(st)),
		   "Timer type not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <output_6>      ::= <output_5> TO */
static int sem213(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_5> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <signalroute_decl_4a> ::= <signalroute_decl_3> TO */
static int sem214(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_3> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <const>         ::= TRUE */
static int sem215(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;

  semantic_warning((lex_unit *) stack__top(st),
                   "Boolean literal not handled. Ignored");
  return 0;
}

/*  <signalroute_option> ::= UNICAST */
static int sem216(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> UNICAST */
static int sem217(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/*  <state_option>  ::= UNSTABLE */
static int sem218(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register state_option *opt;

  opt = resr__new_object(state_option);
  if (!opt)
    {
      report_siflash_memory_error();
      return -1;
    }

  opt -> init = 0;
  opt -> atomic = 1;
  *r = opt;

  return 0;
}

/*  <state_option>  ::= <state_option> UNSTABLE */
static int sem219(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  register state_option *opt;

  lex = (lex_unit *) stack__pick(st, 1);
  opt = (state_option *) lex -> param;
  opt -> atomic = 1;
  *r = opt;
  
  return 0;
}

/*  <signalroute_option> ::= URGENT */
static int sem220(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> URGENT */
static int sem221(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <var_decl_1>    ::= VAR */
static int sem222(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <system_2>      ::= <system_1> */
static int sem223(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <system_2>      ::= <system_2> <process_decl> */
static int sem224(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <system_2>      ::= <system_2> <signalroute_decl> */
static int sem225(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signalroute_decl> not ignored");
      return -1;
    }

  return 0;
}

/*  <system_2>      ::= <system_2> <signal_decl> */
static int sem226(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl> not ignored");
      return -1;
    }

  return 0;
}

/*  <system_2>      ::= <system_2> <procedure_decl> */
static int sem227(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl> not ignored");
      return -1;
    }

  return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <procedure_decl> */
static int sem228(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl> not ignored");
      return -1;
    }

  return 0;
}

/*  <system_2>      ::= <system_2> <var_decl> */
static int sem229(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <var_decl> */
static int sem230(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <system_2>      ::= <system_2> <type_decl> */
static int sem231(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <type_decl> */
static int sem232(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <system_2>      ::= <system_2> <const_decl> */
static int sem233(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <const_decl> */
static int sem234(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <system_2>      ::= <system_2> <meta_decl> */
static int sem235(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <meta_decl> */
static int sem236(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <procedure_decl_4a> ::= <procedure_decl_3> RETURNS <type_id> */
static int sem237(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <var_decl_3>    ::= <var_decl_2> <type_id> */
static int sem238(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
 register lex_unit     *lex;
 register sem_variable *v;

  lex = (lex_unit *) stack__pick(st, 1);
  v = (sem_variable *) lex -> param;
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored > 0)
    v -> ignored = 1;
  else
    v -> ignored = 0;

  *r = v;
  return 0;
}

/*  <fpar_2>        ::= <fpar_1> <type_id> */
static int sem239(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  register char     *name;

  lex = (lex_unit *) stack__pick(st, 1);
  name = (char *) lex -> param;
  lex = (lex_unit *) stack__top(st);
  if (pgm_store_fpar_elt(name, lex -> ignored) < 0)
    return -1;

  return 0;
}

/*  <signal_decl_2> ::= <signal_decl_1> <type_id> */
static int sem240(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl_1> not ignored");
      return -1;
    }  

  *b = 1;
  return 0;
}

/*  <signal_decl_2> ::= <signal_decl_2b> <type_id> */
static int sem241(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <signal_decl_2b> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <record_3>      ::= <record_2> <type_id> */
static int sem242(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <record_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <var_decl_4>    ::= <var_decl_3> */
static int sem243(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;  
  return 0;
}

/*  <meta_decl>     ::= META <meta_trans_def>  */
static int sem244(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <single_name_(> ::= <name_(> */
static int sem245(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <input_1>       ::= INPUT <name_(> */
static int sem246(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__top(st),
		   "Signals not handled. Ignored");

  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;

  return 0;
}

/*  <output_1>      ::= OUTPUT <name_(> */
static int sem247(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  lex_free_token(lex);
  *b = 1;

  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Signals not handled. Ignored");
  return 0;
}

/*  <process_decl_1a> ::= PROCESS <name_(> */
static int sem248(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <signal_decl_1> ::= SIGNAL <name_(> */
static int sem249(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Signals not handled. Ignored");

  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;

  return 0;
}

/*  <signalroute_decl_1a> ::= SIGNALROUTE <name_(> */
static int sem250(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Signalroutes not handled. Ignored");

  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;

  return 0;
}

/*  <call_1>        ::= <call_0> <name_(> */
static int sem251(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  lex_free_token(lex);
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <call_0> not ignored"); 
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <fork_1>        ::= <fork_0> <name_(> */
static int sem252(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  lex_free_token(lex);
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_0> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <abstract_3>    ::= <abstract_2> <name_(> */
static int sem253(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  lex_free_token(lex);
  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <abstract_2> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <expr_1>        ::= <const> */
static int sem254(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <type>          ::= <range_1> '..' <const> */
static int sem255(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 2);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <range_1> not ignored");
      return -1;
    }
  *b = 1;

  return 0;
}

/*  <array_2>       ::= <array_1b> '..' <const> */
static int sem256(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 2);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_1b> not ignored");
      return -1;
    }
  *b = 1;

  return 0;
}

/*  <range_1>       ::= RANGE <const> */
static int sem257(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Range types not handled. Ignored"); 
  *b = 1;
  return 0;
}

/*  <process_decl_1b> ::= <process_decl_1a> <const> */
static int sem258(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  register char     *name;

  lex = (lex_unit *) stack__pick(st, 1);  
  name = (char *)((lex_unit *) stack__pick(st, 1)) -> param;
  lex = (lex_unit *) stack__top(st);

  if (lex -> ignored == 0) 
    {
      pgm_free_string(name);
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  semantic_warning(lex, "Process is not instanciated");
  is_local = 1;  

  return  create_new_process(name, 0);
}

/*  <signalroute_decl_1b> ::= <signalroute_decl_1a> <const> */
static int sem259(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_1a> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <const_decl_3>  ::= <const_decl_2> <const> */
static int sem260(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    pgm_free_string((char *) lex -> param);
  lex = (lex_unit *) stack__top(st);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <array_1b>      ::= <array_1> <const> */
static int sem261(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <array_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <string_2>      ::= <string_1> <const> */
static int sem262(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <const> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <string_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <process_decl_2> ::= <process_decl_1> */
static int sem263(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <process_decl_2> ::= <process_decl_1> <fpar> */
static int sem264(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return  pgm_declare_fpar_as_variable();
}

/*  <procedure_decl_3> ::= <procedure_decl_2> <fpar> */
static int sem265(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  pgm_free_fpar();
  lex = (lex_unit *) stack__pick(st,1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_2> not ignored");
      return -1;
    }
  *b = 1;

  return 0;
}

/*  <process_decl_3> ::= <process_decl_2> */
static int sem266(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <process_decl_3> ::= <process_decl_3> <state_decl> */
static int sem267(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register uint4 *s;
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__top(st);
  s = (uint4 *) lex -> param;
  if (pgm_add_state(*s) < 0)
    {
      resr__free_object(s, uint4);
      return -1;
    }
  resr__free_object(s, uint4);
  return 0;
}

/*  <state_decl_7>  ::= <state_decl_7> <state_decl> */
static int sem268(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register uint4    *s, *subs;
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__top(st);
  subs = (uint4 *) lex -> param;
  lex = (lex_unit *) stack__pick(st, 1);
  s = (uint4 *) lex -> param;
  if (pgm_add_substate(*s, *subs) < 0)
    {
      resr__free_object(s, uint4);
      resr__free_object(subs, uint4);
      return -1;
    }
  resr__free_object(subs, uint4);
  *r = s;
  return 0;
}

/*  <state_decl_4>  ::= <state_decl_3> */
static int sem269(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
   return 0;
}

/*  <state_decl_5>  ::= <state_decl_4> */
static int sem270(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <state_decl_4>  ::= <state_decl_3> <assert> */
static int sem271(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit       *lex;
  register uint4          *s;
  register pgm_transition *t;

  lex = (lex_unit *) stack__pick(st, 1);
  s = (uint4 *) lex -> param;
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored > 0)
    {
      *r = s;
      return 0;
    }
  t = (pgm_transition *) lex -> param;

  if (pgm_add_transition(*s,t) < 0)
    {
      resr__free_object(s, uint4);
    }

  *r = s;
  return 0;
}

/*  <state_decl_6>  ::= <state_decl_5> */
static int sem272(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <state_decl_5>  ::= <state_decl_4> <tpc_cons> */
static int sem273(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <tpc_cons> not ignored");
      lex = (lex_unit *) stack__pick(st, 1);
      resr__free_object(lex -> param,  uint4);
      return -1;
    }
  
  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <state_decl_7>  ::= <state_decl_6> */
static int sem274(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <state_decl_6>  ::= <state_decl_5> <save_signal> */
static int sem275(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <save_signal> not ignored");
      lex = (lex_unit *) stack__pick(st, 1);
      resr__free_object(lex -> param,  uint4);
      return -1;
    }

  *r = ((lex_unit *) stack__pick(st, 1)) -> param;
  return 0;
}

/*  <state_decl_7>  ::= <state_decl_7> <trans_decl> */
static int sem276(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register  uint4          *s;
  register  pgm_transition *tr;

  s = (uint4 *) ((lex_unit *) stack__pick(st, 1)) -> param;
  tr = (pgm_transition *) ((lex_unit *) stack__top(st)) -> param;
  if (pgm_add_transition(*s, tr) < 0)
    {
      resr__free_object(s, uint4);
      return -1;
    }

  *r = s;
  return 0;
}

/*  <single_expression> ::= <expression> */
static int sem277(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  *r = ((lex_unit *) stack__top(st)) -> param;
  *b = ((lex_unit *) stack__top(st)) -> ignored;
  return 0;
}

/*  <assert_1>      ::= ASSERT <expression> */
static int sem278(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression    *expr;
  register   pgm_transition    *tr;
             pgm_gen_condition *gc;

  register lex_unit *lex;
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored > 0)
    {
      *b = 1;
      return 0;
    }
  expr = (pgm_expression *) lex -> param;
  if (expr -> type != PGM_EXPR_COND)
    {
      semantic_error(lex, "Expression is not a valid condition");
      lex_free_token(lex);
      return -1;
    }
  gc = expr -> expr;
  resr__free_object(expr, pgm_expression);
  if (pgm_cond_neg(&gc) <0)
    return -1;
  pgm_store_pre_cond(gc);
  tr = pgm_create_transition(NULL, PGM_TRANS_TYPE_ASSERT);
  if (!tr)
    return -1;

  *r = tr;
  return 0;
}

/*  <if_1>          ::= IF <expression> */
static int sem279(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "If structures not handled. Ignored");
   lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <action_a1>     ::= KILL <expression> */
static int sem280(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
      "Dynamic process destruction not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <action_a1>     ::= RESET <expression> */
static int sem281(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Clocks not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <tpc_cons>      ::= TPC <expression> */
static int sem282(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Clocks not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <output_5>      ::= <output_4> VIA <expression> */
static int sem283(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_4> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <when_1>        ::= WHEN <expression> */
static int sem284(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Timing constraints not handled. Ignored");
  *b = 1;
  return 0;
}

/*  <while_1>       ::= WHILE <expression> */
static int sem285(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "While structures not handled. Ignored");
  lex_free_token((lex_unit *) stack__top(st));
  *b = 1;
  return 0;
}

/*  <input_2>       ::= <input_1> <expression> */
static int sem286(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <input_2>       ::= <input_3> <expression> */
static int sem287(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <action_a1>     ::= <action_b> <expression> */
static int sem288(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   lex_unit *lex1, *lex0;
  register   pgm_expression *expr;
  register   pgm_gen_operation *gop;
  register   uint4 v;

  lex1 = (lex_unit *) stack__pick(st, 1);
  lex0 = (lex_unit *) stack__top(st);
  if (lex1 -> ignored > 0)
    {
      lex_free_token(lex0);
      *b = 1;
      return 0;
    }

  v = *(uint4 *) lex1 -> param;
  resr__free_object((uint4 *) lex1 -> param, uint4);
 	
  if (lex0 -> ignored > 0)
    {
      *b = 1;
      return 0;
    }
 
  expr = (pgm_expression *) lex0 -> param;
  if (expr -> type != PGM_EXPR_LIN)
    {
      semantic_warning(lex0, 
        "Right member is not a linear expression. Ignored");
      *b = 1;
      lex_free_token(lex0);
      return 0;
    }
  
  gop = pgm_create_asgn((pgm_lin_expr *) expr -> expr, v);
  resr__free_object(expr, pgm_expression);
  if (!gop) 
    {
      report_siflash_memory_error();
      lex_free_token(lex0);
    }
  
  *r = gop;

  return 0;
}

/*  <output_2>      ::= <output_1> <expression> */
static int sem289(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
 
  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <output_2>      ::= <output_3> <expression> */
static int sem290(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_3> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <action_a1>     ::= <output_6> <expression> */
static int sem291(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_6> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <call_2>        ::= <call_1> <expression> */
static int sem292(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <call_2>        ::= <call_2b> <expression> */
static int sem293(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <call_2b> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <fork_2>        ::= <fork_1> <expression> */
static int sem294(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <fork_2>        ::= <fork_2b> <expression> */
static int sem295(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <fork_2b> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <save_signal>   ::= <save_signal_1> */
static int sem296(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <save_signal_1> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <trans_decl_1>  ::= <trans_decl_0> */
static int sem297(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
   return 0;
}

/*  <trans_decl_0>  ::= <label> */
static int sem298(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char *name;

  name = (char *) ((lex_unit *) stack__top(st)) -> param;
  pgm_store_trans_label(name);
  return 0;
}

/*  <trans_decl_0>  ::= <deadline> */
static int sem299(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <deadline> not ignored");
      return -1;
    }
  
  return 0;
}

/*  <trans_decl_0>  ::= <label> <deadline> */
static int sem300(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register char     *name;
  register lex_unit *lex;

  name = (char *) ((lex_unit *) stack__pick(st, 1)) -> param;
  pgm_store_trans_label(name);
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <deadline> not ignored");
      return -1;
    }
  
  return 0;
}

/*  <trans_decl_2>  ::= <trans_decl_1> */
static int sem301(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <trans_decl_1>  ::= <provided> */
static int sem302(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <trans_decl_1>  ::= <trans_decl_0> <provided> */
static int sem303(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <trans_decl_3>  ::= <trans_decl_2> */
static int sem304(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <trans_decl_2>  ::= <when> */
static int sem305(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <when> not ignored");
      return -1;
    }
  
   return 0;
}

/*  <trans_decl_2>  ::= <trans_decl_1> <when> */
static int sem306(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <when> not ignored");
      return -1;
    }

  return 0;
}

/*  <trans_decl_4>  ::= <trans_decl_3> */
static int sem307(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  return 0;
}

/*  <trans_decl_3>  ::= <input> */
static int sem308(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input> not ignored");
      return -1;
    }

  return 0;
}

/*  <trans_decl_3>  ::= <trans_decl_2> <input> */
static int sem309(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <input> not ignored");
      return -1;
    }

  return 0;
}

/*  <trans_decl_4>  ::= <statement> */
static int sem310(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }
  
  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <while_2>       ::= <while_1> DO <statement> */
static int sem311(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);

  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <while_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <if_3>          ::= <if_2> ELSE <statement> */
static int sem312(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);

  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <if_2>          ::= <if_1> THEN <statement> */
static int sem313(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);

  lex = (lex_unit *) stack__pick(st, 2);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <trans_decl_4>  ::= <trans_decl_4> <statement> */
static int sem314(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_gen_operation *gop;
  register lex_unit          *lex1, *lex0;

  lex0 = (lex_unit *) stack__top(st);
  lex1 = (lex_unit *) stack__pick(st, 1);

  if (lex0 -> ignored > 0)
    {
      if (lex1 -> ignored > 0)
	{
	  *b = 1;
	  return 0;
	}
      else
	{
	  *r = lex1 -> param;
	  return 0;
	}
    }

  if ((lex1 -> ignored > 0) || (lex1 -> param == NULL))
    {
      *r = lex0 -> param;
      return 0;
    }

  if (lex0 -> param == NULL)
    {
      *r = lex1 -> param;
      return  0;
    }


  gop = pgm_composed_gop((pgm_gen_operation *) lex1 -> param,
			 (pgm_gen_operation *) lex0 -> param);
  pgm_free_gen_operation((pgm_gen_operation *) lex1 -> param);
  pgm_free_gen_operation((pgm_gen_operation *) lex0 -> param);

  if (!gop)
    return -1;

  *r = gop;

  return 0;
}

/*  <if_2>          ::= <if_2> <statement> */
static int sem315(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);
  
  lex = (lex_unit *) stack__pick(st,1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <if_3>          ::= <if_3> <statement> */
static int sem316(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);

  lex = (lex_unit *) stack__pick(st, 1);

  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <while_2>       ::= <while_2> <statement> */
static int sem317(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    lex_free_token(lex);

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <while_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <trans_decl>    ::= <terminator> */
static int sem318(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_transition  *tr;
  register   trans_dest_info *info;

  info = (trans_dest_info *) ((lex_unit *) stack__top(st)) -> param;
  tr = pgm_create_transition(info -> name, info -> type);
  resr__free_object(info, trans_dest_info);
  if (!tr)
    return -1;
  *r = tr;
  return 0;
}

/*  <trans_decl>    ::= <trans_decl_4> <terminator> */
static int sem319(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit        *lex;
  register pgm_transition  *tr;
  register trans_dest_info *info;

  lex = (lex_unit *) stack__pick(st,1);
  if ((lex -> ignored == 0) && (lex -> param != NULL))
    pgm_store_asgn((pgm_gen_operation *) lex -> param);
  lex = (lex_unit *) stack__top(st);
  info = (trans_dest_info *) lex -> param;
  tr = pgm_create_transition(info -> name, info -> type);
  resr__free_object(info, trans_dest_info);
  if (!tr)
    return -1;

  *r = tr;
  return 0;
}

/*  <statement>     ::= <action> */
static int sem320(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  /* Note that the only non-ignored statement is the assignment,
     stored as an pgm_gen_operation structure. */

  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored > 0) 
    {
      *b = 1;
      return 0;
    }

  *r = lex -> param;
  return 0;
}

/*  <statement>     ::= <if> */
static int sem321(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <if> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <statement>     ::= <while> */
static int sem322(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <while> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <meta_trans_def_3> ::= <from_name> */
static int sem323(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register int      (*f)(uint4, uint4);
  register lex_unit  *l;

  f = is_local ? pgm_meta_local : pgm_meta_global;
  l = (lex_unit *) stack__top(st);

  if (is_local && nb_process_instances == 0)
    {
      pgm_free_string((char *) l -> param);
      semantic_error(l, "definition of local meta-transition in process with no instance");
      return -1;      
    }

  if (f(l -> line_no, l -> char_no) < 0)
      return -1;

  if ((!is_local) && (pgm_meta_process((char *) l -> param) < 0))
      return -1;

  if  ((is_local) && (pgm_meta_label((char *) l -> param) < 0))
      return -1;
  
  return 0;
}

/*  <signalroute_decl_3> ::= <signalroute_decl_2> <from_name> */
static int sem324(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex_free_token((lex_unit *) stack__top(st));
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      lex_free_token((lex_unit *) stack__top(st));
      semantic_error(lex, 
		     "Internal: <signal_route_decl_2> not ignored");
    return -1;
  }

  *b = 1;
  return 0;
}

/*  <meta_trans_def_1> ::= <meta_trans_def_3> */
static int sem325(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (is_local == 0)
    {
      semantic_error((lex_unit *) stack__top(st),
		     "Meta-transition label not global");
      return -1;
    }

  return 0;
}

/*  <action_b>      ::= SET <left_asgn> */
static int sem326(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      resr__free_object((uint4 *) lex -> param, uint4);
    }
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Clocks not handled. Ignored");
  return 0;
}

/*  <action_b>      ::= TASK <left_asgn> */
static int sem327(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = lex -> param;

  return 0;
}

/*  <output_5>      ::= <output_4> */
static int sem328(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_4> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <action_a1>     ::= <output_5> */
static int sem329(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <output_5> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_5>        ::= <expr_4> */
static int sem330(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param; 

  return 0;
}

/*  <signalroute_decl_2> ::= <signalroute_decl_1> */
static int sem331(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
          "Internal: <signalroute_decl_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_decl_2> ::= <signalroute_decl_1> <signalroute_option>*/
static int sem332(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_decl_1> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <delay> */
static int sem333(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay> not ignored");
      return -1;
    }
  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> <delay> */
static int sem334(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <delay> not ignored");
      return -1;
    }

  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <rate> */
static int sem335(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <signalroute_option> ::= <signalroute_option> <rate> */
static int sem336(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <rate> not ignored");
      return -1;
    }
  lex = (lex_unit *) stack__pick(st, 1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, 
		     "Internal: <signalroute_option> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <expr_2>        ::= <expr_1> */
static int sem337(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param; 

  return 0;
}

/*  <expr_3>        ::= <expr_2> */
static int sem338(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <expr_3>        ::= <expr_3a> <expr_2> */
static int sem339(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  *b = 1;
  lex = (lex_unit *) stack__top(st);

  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  lex = (lex_unit *) stack__pick(st,1);
  if (lex -> ignored == 0)
    {
      semantic_error(lex, "Internal: <expr_3a> not ignored");
      return -1;
    }
  
  return 0;
}

/*  <expr_4>        ::= <expr_3> */
static int sem340(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <expr_6>        ::= <expr_5> */
static int sem341(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_5>        ::= NOT <expr_5> */
static int sem342(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr;
             pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }
  
  expr = (pgm_expression *) ((lex_unit *) stack__top(st)) -> param;
  if (expr -> type != PGM_EXPR_COND)
    {
      *b = 1;
      semantic_warning((lex_unit *) stack__pick(st, 1),
		       "Argument of not cannot be handled. Ignored");
      pgm_free_expression(expr);
      return 0;
    }
  
  gc = (pgm_gen_condition *) expr -> expr;
  if (pgm_cond_neg(&gc) < 0)
    {
      return -1;
    }
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_7>        ::= <expr_6> */
static int sem343(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_6>        ::= ACTIVE <expr_6> */
static int sem344(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  expr = (pgm_expression *) ((lex_unit *) stack__top(st)) -> param;
  pgm_free_expression(expr);
  *b = 1;  

  semantic_warning((lex_unit *) stack__top(st),
		   "Active operator not handled. Ignored");
  return 0;
}

/*  <expr_8>        ::= <expr_7> */
static int sem345(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <expr_8>        ::= <single_+> <expr_7> */
static int sem346(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <expr_8>        ::= <single_-> <expr_7> */
static int sem347(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  expr = (pgm_expression *) ((lex_unit *) stack__top(st)) -> param;
  if (expr -> type != PGM_EXPR_LIN)
    {
      pgm_free_expression(expr);
      semantic_error((lex_unit *) stack__pick(st, 1), 
		     "Cannot apply negation");
      return -1;
    }

  pgm_negate_lin_expr((pgm_lin_expr *) expr -> expr);

  *r = expr;
  return 0;
}

/*  <expr_9>        ::= <expr_8>    */
static int sem348(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_9>        ::= <expr_9> '*' <expr_8> */
static int sem349(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  sint4 c;

  pgm_expression *expr1, *expr2;
  pgm_lin_expr   *lexpr1, *lexpr2;
 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }
  
  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;

  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Operator cannot be applied");
      return -1;
    }

  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;

  if (pgm_extract_constant(lexpr1, &c) < 0) 
    {
      if (pgm_extract_constant(lexpr2, &c) < 0) 
	{
	  semantic_warning((lex_unit *) stack__top(st),
			   "Not in Presburger arithmetic. Ignored");
	  *b = 1;
	  pgm_free_expression(expr1);
	  pgm_free_expression(expr2);
	  return 0;
	}
      else
	{
	  if (pgm_multiply_lin_expr(lexpr1, c) < 0)
	    {
	      pgm_free_expression(expr1);
	      pgm_free_expression(expr2);
	      semantic_error((lex_unit *) stack__top(st),
			     "Arithmetic overflow");
	      return -1;
	    }
	  pgm_free_expression(expr2);
	  *b = 0;
	  *r = expr1;
	  return 0;
	}
    }
  else
    {
      if (pgm_multiply_lin_expr(lexpr2, c) < 0)
	{
	  pgm_free_expression(expr1);
	  pgm_free_expression(expr2);
	  semantic_error((lex_unit *) stack__top(st),
			 "Arithmetic overflow");
	  return -1;
	}
      
      pgm_free_expression(expr1);
      
      *r = expr2;
      return 0;
    }
}

/*  <expr_10>       ::= <expr_9> */
static int sem350(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_10>       ::= <expr_10> '/' <expr_9> */
static int sem351(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr;

  if (((lex_unit *) stack__top(st)) -> ignored == 0)
    {
      expr = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr);
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
    {
      expr = (pgm_expression *) 
	((lex_unit *) stack__pick(st, 2)) -> param;
      pgm_free_expression(expr);
    }
  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Not in Presburger arithmetic. Ignored");
  return 0;
}

/*  <expr_11>       ::= <expr_10> */
static int sem352(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_11>       ::= <expr_11> '%' <expr_10> */
static int sem353(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr;

  if (((lex_unit *) stack__top(st)) -> ignored == 0)
    {
      expr = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr);
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
    {
      expr = (pgm_expression *) 
	((lex_unit *) stack__pick(st, 2)) -> param;
      pgm_free_expression(expr);
    }

  *b = 1;
  semantic_warning((lex_unit *) stack__pick(st, 1),
		   "Not in Presburger arithmetic. Ignored");
  return 0;
}

/*  <expr_12>       ::= <expr_11> */
static int sem354(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_12>       ::= <expr_12_+> <expr_11> */
static int sem355(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register  pgm_expression *expr1, *expr2, *expr;
  register  pgm_lin_expr   *lexpr1, *lexpr2, *lexpr;
 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,1)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 1)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,1)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 1)) -> param;

  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Plus operator cannot be applied");
      return -1;
    }

  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;

  lexpr = pgm_add_lin_expr(lexpr1, lexpr2);
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
   
  if (!lexpr)
    return -1;

  expr = resr__new_object(pgm_expression);
  if (!expr){
    report_siflash_memory_error();
    pgm_free_lin_expr(lexpr);
    return -1;
  }

  expr -> type = PGM_EXPR_LIN;
  expr -> expr = lexpr;

  *r = expr;
  return 0;
}

/*  <expr_12>       ::= <expr_12_-> <expr_11> */
static int sem356(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression *expr1, *expr2, *expr;
  register pgm_lin_expr   *lexpr1, *lexpr2, *lexpr;
 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,1)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 1)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }
  if (((lex_unit *) stack__pick(st,1)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 1)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Minus operator can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2);
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
   
  if (!lexpr)
    return -1;
   
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_LIN;
  expr -> expr = lexpr;

  *r = expr;
  return 0;
}


/*  <expr_13>       ::= <expr_12> */
static int sem357(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_13>       ::= <expr_13> '>=' <expr_12> */
static int sem358(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression *expr1, *expr2, *expr;
  register pgm_lin_expr   *lexpr1, *lexpr2, *lexpr;
            pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Comparison operator cannot be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_GE) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_13>       ::= <expr_13> '>' <expr_12> */
static int sem359(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression    *expr1, *expr2, *expr;
  register pgm_lin_expr      *lexpr1, *lexpr2, *lexpr;
           pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Operator '>' can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_GT) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_13>       ::= <expr_13> '<=' <expr_12> */
static int sem360(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression    *expr1, *expr2, *expr;
  register pgm_lin_expr      *lexpr1, *lexpr2, *lexpr;
           pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Comparison operator can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_LE) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_13>       ::= <expr_13> '<' <expr_12> */
static int sem361(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression    *expr1, *expr2, *expr;
  register pgm_lin_expr      *lexpr1, *lexpr2, *lexpr;
           pgm_gen_condition *gc;
  
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Comparison operator can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_LT) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_13>       ::= <expr_13> '<>' <expr_12> */
static int sem362(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression    *expr1, *expr2, *expr;
  register pgm_lin_expr      *lexpr1, *lexpr2, *lexpr;
           pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Comparison operator can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_NE) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_13>       ::= <expr_13> <single_=> <expr_12> */
static int sem363(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register   pgm_expression *expr1, *expr2, *expr;
  register   pgm_lin_expr *lexpr1, *lexpr2, *lexpr;
             pgm_gen_condition *gc;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;
   
  if ((expr1 -> type != PGM_EXPR_LIN)
      || (expr2 -> type != PGM_EXPR_LIN))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__top(st),
		     "Equality operator can not be applied");
      return -1;
    }
   
  lexpr1 = expr1 -> expr;
  lexpr2 = expr2 -> expr;
  pgm_negate_lin_expr(lexpr1);
   
  lexpr = pgm_add_lin_expr(lexpr1, lexpr2); 
  pgm_free_expression(expr1);
  pgm_free_expression(expr2);
  if (!lexpr)
    return -1;

  if (pgm_cond_create_from_expr(&gc, lexpr, PGM_OP_EQ) < 0)
    {
      pgm_free_lin_expr(lexpr);
      return -1;
    }   
  pgm_free_lin_expr(lexpr);
  expr = resr__new_object(pgm_expression);
  if (!expr)
    {
      report_siflash_memory_error();
      pgm_free_lin_expr(lexpr);
      return -1;
    }
   
  expr -> type = PGM_EXPR_COND;
  expr -> expr = gc;

  *r = expr;
  return 0;
}

/*  <expr_14>       ::= <expr_13> */
static int sem364(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_14>       ::= <expr_14> AND <expr_13> */
static int sem365(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression    *expr1, *expr2;
           pgm_gen_condition *gc, *gc1, *gc2;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;

  if ((expr1 -> type != PGM_EXPR_COND)
      || (expr2 -> type != PGM_EXPR_COND))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__pick(st,1),
		     "And operator can not be applied");
      return -1;
    }

  gc1 = expr1 -> expr;
  gc2 = expr2 -> expr;

  if (pgm_cond_create(&gc, gc1, gc2, PGM_COND_AND) < 0)
    {
      report_siflash_memory_error();
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      return -1;
    }

  pgm_free_expression(expr1); 
  pgm_free_expression(expr2);

  expr1 = resr__new_object(pgm_expression);
  if (!expr1)
    {
        pgm_free_gen_condition(gc); 
	report_siflash_memory_error();
	return -1;
    }

  expr1 -> expr = gc;
  expr1 -> type = PGM_EXPR_COND;
  *r = expr1;

  return 0;
}

/*  <expr_15>       ::= <expr_14> */
static int sem366(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_15>       ::= <expr_15> OR <expr_14> */
static int sem367(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register pgm_expression *expr1, *expr2;
           pgm_gen_condition *gc, *gc1, *gc2;

  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      if (((lex_unit *) stack__pick(st,2)) -> ignored == 0)
	{
	  expr1 = (pgm_expression *) 
	    ((lex_unit *) stack__pick(st, 2)) -> param;
	  pgm_free_expression(expr1);
	}
      *b = 1;
      return 0;
    }

  if (((lex_unit *) stack__pick(st,2)) -> ignored > 0)
    {
      expr1 = (pgm_expression *) 
	((lex_unit *) stack__top(st)) -> param;
      pgm_free_expression(expr1);
      *b = 1;
      return 0;
    }

  expr1 = ((lex_unit *) stack__top(st)) -> param;
  expr2 = ((lex_unit *) stack__pick(st, 2)) -> param;

  if ((expr1 -> type != PGM_EXPR_COND)
      || (expr2 -> type != PGM_EXPR_COND))
    {
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      semantic_error((lex_unit *) stack__pick(st,1),
		     "Or operator can not be applied");
      return -1;
    }

  gc1 = expr1 -> expr;
  gc2 = expr2 -> expr;

  if (pgm_cond_create(&gc, gc1, gc2, PGM_COND_OR) < 0)
    {
      report_siflash_memory_error();
      pgm_free_expression(expr1);
      pgm_free_expression(expr2);
      return -1;
    }

  pgm_free_expression(expr1); 
  pgm_free_expression(expr2);

  expr1 = resr__new_object(pgm_expression);
  if (!expr1)
    {
        pgm_free_gen_condition(gc); 
	report_siflash_memory_error();
	return -1;
    }

  expr1 -> expr = gc;
  expr1 -> type = PGM_EXPR_COND;
  *r = expr1;

  return 0;
}

/*  <expr_16>       ::= <expr_15> */
static int sem368(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;

  return 0;
}

/*  <expr_16>       ::= <expr_16_1> <expr_15> */
static int sem369(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  *b = 1;
  return 0;
}

/*  <expression>    ::= <expr_16> */
static int sem370(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  if (((lex_unit *) stack__top(st)) -> ignored > 0)
    {
      *b = 1;
      return 0;
    }

  *r = ((lex_unit *) stack__top(st)) -> param;
  return 0;
}

/*  <expr_17_2>     ::= <expr_17_1> <expr_16> */
static int sem371(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);

  *b = 1;
  return 0;
}

/*  <expression>    ::= <expr_17_3> <expr_16> */
static int sem372(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;
  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0)
    pgm_free_expression((pgm_expression *) lex -> param);
  
  *b = 1;
  return 0;
}

/*  <procedure_decl_3> ::= <procedure_decl_2> */
static int sem373(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_2> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <procedure_decl_4> ::= <procedure_decl_3> */
static int sem374(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_3> not ignored");
      return -1;
    }

  *b = 1;
  return 0;
}

/*  <procedure_decl_5> ::= <procedure_decl_4> */
static int sem375(st, r, b)
     stack *st;
     void **r;
     uint1 *b;
{ 
  register lex_unit *lex;

  lex = (lex_unit *) stack__top(st);
  if (lex -> ignored == 0) 
    {
      semantic_error(lex, "Internal: <procedure_decl_4> not ignored");
      return -1;
    }
  
  *b = 1;
  return 0;
}

/****  End of semantic.c  ****/
