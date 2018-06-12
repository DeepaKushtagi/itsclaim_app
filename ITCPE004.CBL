       IDENTIFICATION DIVISION.                                         00010024
       PROGRAM-ID.    ITCPE004.                                         00020024
       AUTHOR.        EMPIRE ITS HOME TEAM.                             00030024
       INSTALLATION.  EMPIRE BLUE CROSS BLUE SHIELD.                    00040024
       DATE-WRITTEN.  APRIL 15, 1994.                                   00050024
       DATE-COMPILED.                                                   00060024
      ******************************************************************00070024
      *                                                                *00080024
      *             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *00090024
      *                                                                *00100024
      *   MODULE:   ITCPE004                                           *00110024
      *   NAME:     INSTITUTIONAL PLAN SPECIFIC EDIT MODULE            *00120024
REL101*   VERSION:  02.5                                               *00130006
REL101*   DATE:     NOVEMBER 16, 2007                                  *00140006
      *                                                                *00150024
      *   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *00160024
      *   TYPE:         COMMON                                         *00170024
      *   FUNCTION:     THIS MODULE IS THE INSTITUTIONAL PLAN SPECIFIC *00180024
      *                 EDIT MODULE FOR BATCH OR ON-LINE.              *00190024
      *                                                                *00200024
      *   MODIFICATION                                                 *00210024
      *        HISTORY: 06/01/90 01.0 INITIAL RELEASE                  *00220024
      *                 05/08/97 02.0 MODIFIED FOR 8.0 RELEASE         *00230000
      *                 05/13/97 02.0 ADD B1800-EDIT-SELECT-NON-PPO    *00231002
      *                               AND B1850-EDIT-ALL-PPO           *00235002
      *                 10/24/97 02.0 UN-COMMENT THE HOSPICE CLAIM EDIT*00236006
      *                 12/12/97 02.0 ADD FACILITY TYPE OVERRIDE FOR   *
      *                               ROCHESTER                        *
      *                 12/16/97 02.0 REMOVE FACILITY TYPE OVERRIDE FOR*
      *                               ROCHESTER                        *
      *                 12/19/97 02.0 ADD PREFIX 'YLE' TO EDIT NON-PPO *00240311
      *                 12/19/97 02.0 REMOVE HOSPICE CLAIM EDIT ZI003  *00240413
      *                               AND ZI004                        *00241013
      *                 01/14/98 02.0 REMOVE ZI029 AND ZI031 FROM LOCAL*00240413
      *                               EDIT                             *00241013
      *                 03/09/98 02.0 CR#98010068                      *00253028
      *                               REMOVE NEW HAMPSHIRE, BUFFALO AND*00253028
      *                               SYRACUSE PLANS FROM NON-PPO EDIT *00230003
003900*                 03/13/98 02.0 TECH MEMO ITS-TW-98-03      -CSC *00253028
003901*                        - REMOVED "IGZEDT4" CALL FOR YEAR 2000  *ITCPE004
003902*                          CHANGES (DATE AND TIME ARE AVAILABLE  *ITCPE004
003910*                          FROM ITSIAPC LINKAGE AREA).           *ITCPE004
003920*                 11/06/98 02.0 CR#1998100778                    *00253028
003930*                               CHANGE MAINE PLAN CODE FROM      *00253028
003940*                               180/680 TO 221/721               *00230003
003950*                 12/04/97 02.0 ADD PREFIX 'YLK' TO EDIT NON-PPO *00240311
003960*                 01/07/99 02.0 CR#1998120906                    *00253028
003970*                               REMOVE VERMONT PLAN FROM NON-PPO *00253028
003980*                               EDIT                             *00230003
003990*                 01/21/99 02.0 CR#1999010016                    *00253028
003991*                               ADDED COND CODE '26' AND '29' TO *00253028
003992*                               ZI027/ZP027 EDIT                 *00230003
002546*                 03/19/99 02.0 CR#19990XXXXX                    *00254614
002547*                               EDIT PRICE MTD 01 FOR PRODUCTION *00254714
002548*                               ONLY                             *00254814
002450*                 07/16/99 02.0 CR#1999040211                    *00245023
003997*                               CHANGE EDIT FOR 'ZI035'=>100000  *00254714
003998*                               AND 'YLN' PREFIX ONLY.           *00254814
002462*                 09/10/99 02.0 CR#1999090536                    *00246222
002463*                               ZI027/ZP027 EDIT BYPASS PREFIX   *00246323
002464*                               'CKI'.                           *00246423
002555*                 10/15/99 02.0 CR#1999100613                    *00255502
002556*                               FLORIDA PPO EDIT                 *00255600
002557*                 11/05/99 02.0 CR#1999100619                    *00255708
002558*                               ENHANCE SF LOCAL EDIT FOR ZI027  *00255805
002559*                               CR#1999100646                    *00255908
002560*                               FLORIDA PPO EDIT                 *00256008
002561*                 11/24/99 02.0 CR#1999110702                    *00256112
002562*                               NATIONAL PPO/EPO PREFIX EDIT     *00256209
002564*                               ADDED MAINE PLAN CODE 181/681.   *00256412
002565*                 02/11/00 02.0 CR#2000010855                    *00256514
002566*                               NATIONAL EPO EDIT FOR UTAH/OREGON*00256616
002567*                               REMOVE INTERNATIONAL LOGIC.      *00256713
002568*                 03/24/00 02.0 CR#00047317                      *00256817
002569*                               BYPASS EDIT ZERO RATE FOR TEST.  *00256917
002570*                               CR#00046944                      *00257018
002571*                               EMPIRE PAR PROVIDER FOR NJ.      *00257118
002572*                               CR#00047656                      *00257221
002573*                               DISPLAY NON-PAR ENGLEWOOD MESSAGE*00257321
002573*                 04/06/00 02.0 CR#********                      *00257327
002574*                               BYPASS THE FOLLOWING EDITS FOR   *00257427
002575*                               ECRP CLAIMS: ZI002, ZI005,ZI007, *00257527
002576*                               ZI008,ZI017,ZI026, ZI027,ZI028,  *00257627
002577*                               ZI033,ZI036,ZI090, ZI094,ZI095,  *00257727
002578*                               ZI096,ZI097.                     *00257827
002579*                               ELIMINATE THE FOLLOWING EDITS FOR*00257927
002580*                               ALL CLAIMS: ZI001,ZI035.         *00258027
002581*                 04/06/00 02.0 CR#00048786                      *00258131
002583*                               EDIT ZI099 FOR LOC-PLAN-PAYER AND*00258300
002584*                               PAY-SUB.                         *00258401
002584*                 04/06/00 02.0 CR#********                      *00258404
002585*                               ADD INTL-CD NOT = 'I' FOR ERROR  *00258504
002586*                               CODE ZI026, ZI027, AND ZI036.    *00258604
002587*                 06/29/00 02.0 CR#00050512                      *00258709
002588*                               ADD PPO PREFIX EDIT FOR 'SWH'    *00258811
002592*                               CR#00050513                      *00259211
002593*                               ADD PPO PREFIX EDIT FOR 'YLX/YLY'*00259311
002591*                 07/14/00 02.0 CR#00050280                      *00259113
002592*                               DENIAL OF ALL CLAIMS THAT ARE    *00259213
002593*                               EMPIRE PAR PROVIDER FOR NJ BASED *00259313
002594*                               ON TAX-ID AND/OR NJ PROVIDER #.  *00259413
002595*                 08/11/00 02.0 CR#00051848                      *00259514
002596*                               ENHANCE UTAH/OREGON PROV FLIP    *00259614
002597*                 09/22/00 02.0 CR#00052998                      *00259715
002598*                               UPDATE OF APPLICABLE NJ PROVIDERS*00259815
002599*                               DENIED IN CR#00050280.           *00259915
002600*                 09/22/00 02.0 CR#00052747                      *00260016
010800*                               DELETE FLIPPING PROVIDER CODE FOR*01080017
002602*                               PUERTO RICO PLAN CODE.           *00260216
011100*                 11/30/00 02.0 CR#00053762                      *01110018
011200*                               ADD PREFIXES AND PPO PROV IND TO *01120018
011300*                               FLIP LOGIC                       *01130018
011301*                 03/08/01 02.0 CR#00052324                      *01130102
011302*                               ADD DENIAL CLAIMS FOR EMPIRE PAR *01130201
011303*                               NJ PROVIDER                      *01130301
011304*                               CR#00056429                      *01130401
011305*                               ADD LOGIC TO CHECK HACKENSACK    *01130501
011306*                               PRIOR 3/1/99                     *01130601
011307*                 05/14/01 02.0 CR#00056444                      *00380005
011308*                               ITS RELEASE 8.3                  *00390005
011309*                               CHANGES TO 'FM105-PAY-SUB' AND   *00400005
011310*                               'FM105-PAY-REP-PAYEE' DUE TO     *00410005
011311*                               COPYLIB CHANGES.                 *00420005
      *                 05/14/01 02.0 CR#00055386                      *00430005
      *                               ITS RELEASE 8.3                  *00440005
      *                               LIFT LOCAL EDIT FOR $0 PRICE ALL *00450005
      *                               INCLUSIVE.                       *00460005
011316*                 11/13/01 02.0 IR#284357                        *01131601
011317*                               REMOVE CPT4/ICD9 EDIT            *01131700
011318*                 01/02/02 02.0 CR#63716                         *01131801
011319*                               ADD BNY & FUE, REMOVE MAINE &    *01131902
011320*                               MOVE ESE & HTF TO BLUECARD PPO   *01132002
011321*                               FOR NON-PPO LOGIC.               *01132102
011322*                 02/28/02 02.0 CR#65363                         *01132200
011323*                               ADD RFB PREFIX                   *01132300
004610*                 08/03/01 -  2.2  TECH MEMO ITS-TM-01-08-R8.4:  *00461006
004620*                          COMPLETED BY:  B. PEPPERS             *00462006
004630*                          - HIPAA - ADJUSTED ITCIASC REDEFINES  *00463006
004640*                            FOR NEW RECORD TYPES D2,F6,32,66.   *00464006
004650*                 03/17/02 02.2 CR#61813 - ITS 8.4               *00465009
004700*                               ADDED 'L' & 'Q' FOR SRC-PMT-CD   *00470008
004710*                               REMOVED 6,7,A,H AND ADD M TO     *00471008
004720*                                  WS-CERT-1-PROV                *00472008
004721*                               REMOVED 8,B TO WS-CERT-2-PROV    *00472108
004700*                               ADDED 'R' & 'S' FOR SRC-PMT-CD   *00472212
024310*                 05/23/02 02.2 CR#65241                         *02431002
024320*                               REMOVED PREFIX 'CKI' ZI027/ZP027 *02432002
024330*                               BYPASS.                          *02433008
024340*                               CR#66839                         *02434005
024350*                               CHG NJ PROVIDER WHEN STATEMENT   *02435004
024360*                               FROM AN 'OR' TO 'AND'.           *02436007
024400*                               UPDATED NJ PROVIDER TABLES.      *02440007
024401*                 06/20/02 02.2 CR#67767                         *02440111
024410*                               ADD PPO PREFIX EDIT FOR 32BJ     *02441011
015500*                 08/01/02 02.2 CR#68615                         *01550012
015600*                               NO EITSZI027 FOR ADJ SF          *01560014
015610*                 08/09/02 01.36 CR#70439                        *01561023
015620*                               U602 NON 0 PRICE EDIT EITSZI100  *01562023
015630*                 10/24/02 01.37 CR#71966                        *01563029
015640*                               UPDATE NJ PROVIDER TABLES.       *01564024
015650*                 10/31/02 02.3  CR#73519                        *01565032
015660*                               BYPASS ZI100 & ZP100 ON ECRPS.   *01566031
024419*                 03/27/03 02.3  CR#76679                        *02441903
024420*                               INCLUDE A NEW EDIT CODE ZI101 TO *02442002
024421*                               CHECK INVALID SEMI-PVT RM RATE   *02442102
024422*                               CAUSING DF ERROR.                *02442202
015665*                 04/13/03 02.3  CR#75564                        *01566504
015666*                               ITS RELEASE 8.6                  *01566604
015667*                               ADDED LS-ITCDBP20-RECORD TO      *01566704
015668*                               LINKAGE SECTION.                 *01566804
015669*                                CR#75322                        *01566904
015670*                               RELEASE 8.6 ALLOWS MEDICARE CLAIM*01567004
015680*                               TO BE PROCESS THRU BLUECARD. SF  *01568004
015690*                               EDITS ZI026 AND ZI027 LIFTED.    *01569004
015665*                 05/05/03      CR#77528                         *01566504
015665*                               DENY SF'S WITH ERROR ZI008 &     *01566504
015665*                               CREAT NF                         *01566504
015665*                 06/20/03      IR#297588  INITIALIZING          *01566504
015665*                                          WS-PAYSUB-IND         *01566504
024436*                 07/03/03      CR#78570                         *02443601
024437*                               ADDED PREFIX FUJ TO PPO FLIP LOGIC02443702
024438*                 09/25/03      CR#82974                         *02443800
024439*                               NEW NASCO MEDICARE EDITS ZI102   *02443900
024442*                 10/13/03      CR#84911                         *02444205
024443*                               INTERCEPT SF FOR LEASED NETWORK  *02444305
024444*                 10/23/03      CR#84481                         *02444404
024445*                               CORRECT TAX ID FOR ST BARNABAS   *02444503
024445*                 12/04/03      CR#78885                         *02444503
024445*                               EDIT ZI099 FOR LOC-PLAN-PAYER AND*02444503
024445*                               PAY-SUB FOR NASCO  INTERNATIONAL *02444503
      *                               CLAIM                            *00250024
024450*                 12/18/03      CR#86159                         *02445003
024451*                               LIFT ZI099/ZP099 EDIT FOR ALL    *02445105
024452*                               INTERNATIONAL CLAIMS             *02445205
024453*                 02/26/04      CR#89643                         *02445307
024454*                               ADDED PREFIX PNW TO PPO FLIP LOGIC02445407
024455*                 05/20/04      CR#89907                         *02445503
024456*                               LIFT EITS EDIT FOR ZI092         *02445603
024457*                               CR#89908                         *02445703
024458*                               AUTOMATE SINF FOR ZI002 FOR CS90 *02445803
024459*                               CR#89930                         *02445903
024460*                               SEND AUTO-SINF ON ZI033 ERROR    *02446004
024455*                 06/03/04      CR#92916                         *02445503
024456*                               MODIFY MEDICARE EDIT ON          *02445603
024457*                               INSTITUTIONAL CLAIMS             *02445703
      *                 09/09/04      CR#96104                         *02445503
      *                               REJECT EITS ERROR ZI033,ZI002    *02445603
      *                               CR#96584                         *
      *                               REMOVE MONTANA FROM NON PPO      *
024468*                 09/23/04      CR#96657                         *02445503
024469*                               INCLUDE ECRP+ SF'S IN THE LAST   *02445603
024470*                               ACTION MAPPING DATE              *
024480*                 10/19/04      IR#302693                        *02448010
024481*                               REMOVE B1350-EDIT-ADMIT-HR EDIT  *02448109
024482*                 11/05/04      CR#99899                         *02448211
024483*                               ADD ECRP PLUS TO                 *02448311
024484*                               B1230-EDIT-EMPIRE-PAR-PROV       *02448411
024485*                 03/10/05      CR#98645                         *02448501
024486*                               CREATE UPFRONT EDIT(ZI109) FOR   *02448601
024487*                               TROPICANNA                       *02448701
024488*                 04/16/05      CR#103663(REL9.0)                *02448800
024489*                               EITS TO SEND AUTO SINF ON        *02448900
024490*                               1015.SET EDIT ZI108              *02449000
024491*                 06/02/05      CR#106203                        *02449101
024492*                               REMOVE THE EDIT ZI036.           *02449201
024488*                 06/02/05      CR#94179                         *02448800
024489*                               LIFT EDIT ZI008                  *02448900
024495*                 06/30/05      CR#106466                        *02449500
024496*                               REMOVE EITS WEAK NETWORK LOGIC   *02449600
024497*                               EXCEPT UTAH AND OREGON           *02449703
023040*                 07/28/05      CR#108613                        *02304006
023050*                               REMOVE FLIP LOGIC FOR 'ENP'      *02305004
023060*                               PREFIX                           *02306004
024501*                 08/25/05      CR#108694                        *02450101
024502*                               DENY CONTIG CTY INSTITUTIONAL    *02450202
024503*                               PROVIDERS                        *02450302
024501*                 12/01/05      CR#112936                        *02450101
024502*                               DENY ENGLEWOOD HOSPITAL          *02450202
024503*                               CONTIGUOUS COUNTY PROVIDER       *02450302
024000*                 12/29/05      CR#114000                        *02400004
024100*                               DENY CONTIG CTY INSTITUTIONAL    *02410004
024200*                               PROVIDERS                        *02420004
024000*                 02/21/06      CR#117810                        *02400004
024100*                               UPDATE CONTIG CTY INSTITUTIONAL  *02410004
024200*                               PROVIDERS                        *02420004
024000*                 04/06/06      CR#117967                        *02400004
024100*                               DENY ECRP+ CLAIMS FROM PLAN 800  *02410004
024200*                               AS 1069                          *02420004
024000*                 05/04/06      CR#118546                        *02400004
024100*                               REMOVE COMMENTED OUT CODES AND   *02410004
024200*                               UNUSED ERROR CODES               *02420004
024000*                 09/07/06      CR#122982                        *02400004
024100*                               LIFTED THE ZI005 EDIT            *02410004
      *                 08/09/07      CR#134584                        *
      *                               REMOVED MEDICARE ID FROM         *
      *                               NJ CONTIG CTY LOGIC              *
      *                 11/15/07      CR#136539                        *
      *                               CREATE EDIT(ZI111) FOR REJECTING *
      *                               CLAIMS WITH NEGATIVE PRICING     *
      *                 11/15/07      CR#138137                        *
      *                               UPDATE WS-NJ-TAX-ID 88 LEVEL LIST*
      *                 12/13/07      CR#137498                        *
      *                               REMOVED ILLEGAL CHARACTERS IN THE*
      *                               SUBSCRIBER/PATIENT NAME          *
      *                 28/12/07      CR#140358                        *
      *                               REMOVED 2 TAX IDS FROM EITSZI097 *
      *                               DENIAL.                          *
      *                 01/02/08      CR#138938                        *
      *                               CREATE EDIT FOR NEGATIVE CLAIM   *
      *                               LEVEL PRICING
REL101* 11/16/2007 RELEASE 10.1 - 999 LINE EXPANSION PROJECT           *
REL101*            PROGRAM VERSION 02.5                                *
REL101*          RIC CASE                                              *
REL101*                                                                *00068
      *                 06/05/08      CR#142804                        *
      *                               ADDED 2 TAX IDS FOR EITSZI097    *
      *                               DENIAL IN WS-NJ-TAX-ID.          *
      *                 08/08/08      CR#145185                        *
      *                               DELETE 2 TAX IDS FOR EITSZI097   *
      *                               THAT WERE ADDED FOR CR#142804.   *
      *                 08/14/08      CR#144348                        *
      *                               CHANGE THE FILLER OF LS-STACK    *
      *                               DUE TO CHANGE OF ITCIASC FOR 10.2*
      *                 01/08/09      CR#147829                        *
      *                               PLEASE LIFT THE ZI023 LOCAL EDIT *
      *                               AND ZI022 EDIT ALSO.             *
      *                 01/22/09      CR#145948                        *
      *                               LOCAL EDITS SH NOT OVERLAY OTHER *
      *                               LOCAL EDITS                      *00250024
      *                 05/11/09      CR#148645                        *
      *                               SUSPEND THE SF WITH LOCAL ERROR  *
      *                               CODE ZI113 IF LINE TOTAL CHARGE  *00250024
      *                               IS GREATER THAN $999999.99.      *00250024
      *                 07/17/09      CR#153059                        *
      *                               REMOVED 1 TAX IDS FROM EITSZI097 *
      *                               DENIAL.                          *
      *                 09/17/09      CR#154027                        *
      *                               UPDATE TO EITSZI097 NJ EDIT      *
      *                               '226019101'.                     *00250024
      *                 11/19/09      CR#154698                        *
      *                               PLEASE ADD FACILITY TO THE       *
      *                               EITSZI097 NJ EDIT '221487243'    *00250024
      *                               '221487330'.                     *00250024
      *                 12/03/09      CR#154112                        *
      *                               BYPASS THE LOCAL EDIT ZI094 FOR  *
      *                               ADJUSTMENT SFS WHERE THE         *00250024
      *                               PROCESSING SITE CONTROL NUMBER   *00250024
      *                               EQUALS EITSUNKN.                 *00250024
      *                 12/17/09      CR#155472                        *
      *                               CREATE EDIT(ZI114) TO PREVENT    *
      *                               INPATIENT DME CLAIMS, REJECT THE *00250024
      *                               SF AND CREATE A B2 INFO MESSAGE  *00250024
      *                 06/18/10      CR#160685                        *
      *                               UPDATE NJ INSTITUTIONAL LOGIC    *
      *                               MUHLENBERG HCP                   *00250024
      *                 09/17/10      CR#140137                        *
      *                               MODIFY SF DATE LOGIC CHANGES     *
      *                 11/04/10      CR#163462                        *
      *                               ADD FACILITIES TO EITSZI097 NJ   *
      *                               EDIT                             *00250024
      *                 07/01/11      CR#171041                        *
      *                               REMOVE HACKENSACK FROM PROV LIST *
      *                               EDIT                             *00250024
      *                 07/15/11      CR#171387                        *
      *                               UPDATE THE EITSZI097 EDIT        *
      *                               FOR NJ HOSPITALS                 *00250024
      *                 04/20/12      ITS 11.4                         *
      *                               REMOVE LOCAL EDITS ZI025 AND     *
      *                               ZI096 TO AVOID OVERLAYING SPACE  *00250024
      *                               FOR CERT IND.                    *00250024
      *                 06/15/12      CR#138141                        *
      *                               REMOVE 88 LEVEL LOCAL EDITS ZI097*
REL120*                 10/14/12      ITS 12.0 ADD NEW RECORD TYPES    *
REL120*                               LOCAL EDITS MODIFICATIONS FOR    *
REL120*                               ITS 12.0. ADDED FOLLOWING        *
REL120*                               ERROR CODES TO PROC-SITE-CNTL-NUM*
REL120*                               ZI007, ZI017, ZI099, ZI113,      *
REL120*                               ZI999.                           *
REL120*                               ELIMINATE EDITS :                *00103400
REL120*                               ZI002, ZI102, ZI104, ZI105,      *00103400
REL120*                               ZI108, ZI110.                    *00103400
REL135*                 10/11/13      ADDED GY MODIFIER EDIT:          *00103400
REL135*                               ZI135                            *00103400
REL140*                 04/11/14      GY LOGIC BYPASSED FOR PREFIXES   *
REL140*                               YLV, YLR, XGH, XGK               *
REL145*                 10/17/14      SERVICE DATE AND STATEMENT COVER *00260024
REL145*                               DATE CHANGED                     *
CQ8919*                 06/12/15      CR#1368919                       *
CQ8919*                               CREATE EDIT(ZI116) TO DENY SF    *
CQ8919*                               IF THE DOS IS GREATER THAN OR    *00250024
CQ8919*                               EQUAL TO 10/01/2015 AND THE      *00250024
CQ8919*                               PROVIDER SUBMITTED VERSION 9 OR  *00250024
CQ8919*                               SPACES                           *00250024
ICDFIX*                 10/01/2015    CQ2046457                        *00250024
ICDFIX*                               MODIFIED EDIT ZP116 TO ACCOMODATE*00250024
ICDFIX*                               ICD IND = SPACES FOR ICD-10      *00250024
ZI115F*                 11/10/2015    IN# INC0755544 EMERCH# CHG0025173*00250024
ZI115F*                               REMOVED EDIT ZI115 AS ITS NO     *00250024
ZI115F*                               LONGER NEEDED & LINE LEVEL       *00250024
ZI115F*                               DATES WERE BLANK WHICH CAUSED    *00250024
ZI115F*                               SF TO HIT LOCAL EDITS ZI115      *00250024
J33055*                 11/10/2017    JIRA STORY# NYCS90-650           *00250024
J33055*                               SSCR 33055                       *00250024
J33055*                               COMMENTED THE LOGIC FOR GY MOD   *00250024
J33055*                               MADE IN REL135, REL140 WITH      *00250024
J33055*                               J33055                           *00260024
      *   NOTES:                                                       *00250024
      *                                                                *00260024
      ******************************************************************00270024
                                                                        00280024
       ENVIRONMENT DIVISION.                                            00290024
                                                                        00300024
       CONFIGURATION SECTION.                                           00310024
                                                                        00320024
       INPUT-OUTPUT SECTION.                                            00330024
                                                                        00340024
       FILE-CONTROL.                                                    00350024
                                                                        00360024
       DATA DIVISION.                                                   00370024
                                                                        00380024
       FILE SECTION.                                                    00390024
       EJECT                                                            00400024
      ********* W O R K I N G   S T O R A G E   S E C T I O N ********* 00410024
      *---------------------------------------------------------------* 00420024
       WORKING-STORAGE SECTION.                                         00430024
      *---------------------------------------------------------------* 00440024
       01  FILLER                            PIC X(45) VALUE            00450024
           '*** ITCPE004 WORKING-STORAGE STARTS HERE ***'.              00460024
                                                                        00470024
       01  WS-DISPLAY-MODULE.                                           00480024
           05  WS-MODULE-ID                  PIC X(08) VALUE 'ITCPE004'.00490024
           05  FILLER                        PIC X(03) VALUE ' - '.     00500024
           05  WS-MODULE-DESCRIPTION         PIC X(50) VALUE            00510024
               'INSTITUTIONAL PLAN SPECIFIC EDIT MODULE           '.    00520024
           05  FILLER                        PIC X(09) VALUE            00530024
               'VERSION #'.                                             00540024
REL101     05  WS-VERSION-NUMBER             PIC X(04) VALUE '02.5'.    01870032
           05  FILLER                        PIC X(13) VALUE            00560024
               ' RELEASED ON '.                                         00570024
REL010     05  WS-RELEASE-DATE               PIC X(08) VALUE '11/16/07'.01900004
                                                                        00590024
005910 01  WS-NON-PAR-ENGLEWOOD-MSG          PIC X(50) VALUE            00591021
005920     'ENGLEWOOD HOSPITAL NON-PAR TODAY                  '.        00592024
005930                                                                  00593021
007700*01  WS-CURRENT-TIME.                                             00600024
007800*    05  WS-CURRENT-HOUR               PIC 9(02).                 00610024
007900*    05  WS-CURRENT-MINUTE             PIC 9(02).                 00620024
008000*    05  WS-CURRENT-SECOND             PIC 9(04).                 00630024
008100*                                                                 00640024
008200*01  WS-CURRENT-DATE.                                             00650024
008300*    05  WS-CURRENT-CC                 PIC 9(02).                 00660024
008400*    05  WS-CURRENT-YY                 PIC 9(02).                 00670024
008500*    05  WS-CURRENT-MM                 PIC 9(02).                 00680024
008600*    05  WS-CURRENT-DD                 PIC 9(02).                 00690024
                                                                        00700024
020600***********U602************************************************** 02060015
020700 01  WS-AVG-SEMI-PRIV-RM-RATE          PIC S9(8)V99 COMP-3.       02070016
020800***********U602************************************************** 02080015
020900                                                                  02090015
007010 01  WS-SUB-ID-PFX-1                   PIC X(03).                 00701009
005400     88  WS-BLUECARD-PPO-PFX-1         VALUE 'YLD' 'YLE' 'YLK'    00540009
016600                                             'IAT' 'NYY' 'NYB'    01660018
016601                                             'BNY' 'FUE' 'RFB'    01660100
029800                                             'BJF' 'BJT' 'FUJ'    02980000
029801                                             'ESE' 'HTF' 'PNW'    02980106
005500                                             'YLX' 'YLY'.         00550009
005600     88  WS-BLUCD-DS-PPO-PFX-1         VALUE 'SWH'.               00560009
016900     88  WS-NATL-PPO-EPO-PFX-1         VALUE 'ENP' 'EDC' 'EXP'    01690018
017000                                             'MYC'.               01700002
007022     88  WS-NATL-EPO-UTAH-PFX-1        VALUE 'ENP'.               00702213
007022                                                                  00702209
007023 01  WS-SUB-ID-PFX-2                   PIC X(03).                 00702309
006100     88  WS-BLUECARD-PPO-PFX-2         VALUE 'YLD' 'YLE' 'YLK'    00610009
017500                                             'IAT' 'NYY' 'NYB'    01750018
017501                                             'BNY' 'FUE' 'RFB'    01750100
030810                                             'BJF' 'BJT' 'FUJ'    03081000
030811                                             'ESE' 'HTF' 'PNW'    03081106
006200                                             'YLX' 'YLY'.         00620009
006300     88  WS-BLUCD-DS-PPO-PFX-2         VALUE 'SWH'.               00630009
017800     88  WS-NATL-PPO-EPO-PFX-2         VALUE 'ENP' 'EDC' 'EXP'    01780018
017900                                             'MYC'.               01790002
007027     88  WS-NATL-EPO-UTAH-PFX-2        VALUE 'ENP'.               00702713
007026                                                                  00702609
007027 01  WS-SUB-ID-PFX-3                   PIC X(03).                 00702709
006800     88  WS-BLUECARD-PPO-PFX-3         VALUE 'YLD' 'YLE' 'YLK'    00680009
018400                                             'IAT' 'NYY' 'NYB'    01840018
018401                                             'BNY' 'FUE' 'RFB'    01840100
031910                                             'BJF' 'BJT' 'FUJ'    03191000
031911                                             'ESE' 'HTF' 'PNW'    03191106
006900                                             'YLX' 'YLY'.         00690009
007000     88  WS-BLUCD-DS-PPO-PFX-3         VALUE 'SWH'.               00700009
018700     88  WS-NATL-PPO-EPO-PFX-3         VALUE 'ENP' 'EDC' 'EXP'    01870018
018800                                             'MYC'.               01880002
007032     88  WS-NATL-EPO-UTAH-PFX-3        VALUE 'ENP'.               00703213
007030                                                                  00703009
007010 01  WS-SUB-ID-PFX-1-3                 PIC X(03).                 00701000
031919     88  WS-IBM                        VALUE 'IBM'.               03191900
031920     88  WS-MLC                        VALUE 'MLC'.               03192000
031921     88  WS-INA                        VALUE 'INA'.               03192100
031922     88  WS-PBG                        VALUE 'PBG'.               03192200
007020     88  WS-CIRCUIT-CITY               VALUE 'CCX'.               00702000
007030     88  WS-MERRILL-LYNCH              VALUE 'MLA'.               00703000
007040     88  WS-MARRIOTT-INTL              VALUE 'MTI' 'SDH' 'HMS'.   00704000
007050     88  WS-SONY                       VALUE 'SME'.               00705000
007070     88  WS-COLUMBIA-HOUSE             VALUE 'COH'.               00707000
007080     88  WS-SEARS                      VALUE 'SER'.               00708000
007090     88  WS-THE-HARTFORD               VALUE 'THF'.               00709000
007095     88  WS-FL-NATL-ACCT               VALUE                      00709500
007099         'CCX' 'MLA' 'MTI' 'SDH' 'HMS' 'SME' 'COH' 'SER' 'THF'.   00709901
031935     88  WS-UNS                        VALUE 'UNS'.               03193504
031936     88  WS-UNT                        VALUE 'UNT'.               03193604
REL120*    88  WS-LEASED-NET                 VALUE 'UNS' 'UNT'.         03193704
007105                                                                  00710500
       01  WS-PLAN-CD                        PIC X(03).                 00710046
007120     88  WS-UTAH                       VALUE '410' '910'.         00712013
007130     88  WS-OREGON                     VALUE '851'.               00713016
018817*    88  WS-MAINE                      VALUE '181' '681'          01881700
018818*                                            '221' '721'.         01881800
      *    88  WS-MONTANA                    VALUE '251' '751'.         00730046
      *    88  WS-NEW-HAMPSHIRE              VALUE '270' '770'.         00740046
      *    88  WS-BUFFALO                    VALUE '301' '801' '800'.   00750046
      *    88  WS-SYRACUSE                   VALUE '305' '805'.         00760046
009400*    88  WS-VERMONT                    VALUE '415' '915'.         00770046
007030*    88  WS-SAN-JUAN-PR                VALUE '470'.               00703016
018826*    88  WS-NON-PPO-PLAN               VALUE '251' '751'.         01882600
018827*    88  WS-NON-PPO-PLAN               VALUE '221' '721'          01882700
018828*                                            '181' '681'.         01882800
009710*        '751' '415' '915' '470'.                                 00810046
      *        '751' '270' '770' '301' '801' '800' '305' '805' '415'    00810046
      *****************************EHIMS******************************* 00820000
007040*01  WS-TAX-ID-NUM             PIC X(09).                         00704013
171041*    88  WS-NJ-HACKENSACK-TAX-ID                                  01883201
171041*                              VALUE '221487576'.                 01883301
035800*    88  WS-CONTIG-CTY-TAX-ID  VALUE '135596796'                  03580002
035900*                                    '141338465'                  03590001
036000*                                    '141338471'                  03600001
036100*                                    '141347719'                  03610001
036200*                                    '141364467'                  03620001
036300*                                    '141364513'                  03630001
036400*                                    '141372647'                  03640001
036500*                                    '150524324'                  03650001
036600*                                    '150552726'                  03660001
036700*                                    '156001246'                  03670001
036701*                                    '161540394'                  03670102
038100*                                    '221487173'                  03810006
038300*                                    '311737300'.                 03830005
038400*    88  WS-CONTIG-CTY-YLN-TAX-ID  VALUE '030181058'              03840006
038500*                                        '030219309'              03850005
038200*                                        '060646659'.             03820006
035800*    88  WS-NJ-TAX-ID          VALUE '208489105'                  03580005
035830*                                    '210744668'                  03583005
007039*                                    '220820545'                  00703915
007040*                                    '221413947'                  00704015
007060*                                    '221487126'                  00706015
      *                                    '221487173'
154698*                                    '221487243'
      *                                    '221487246'
007092*                                    '221487278'                  00709215
007093*                                    '221487305'                  00709315
007094*                                    '221487307'                  00709415
007095*                                    '221487322'                  00709515
007096*                                    '221487324'                  00709615
154698*                                    '221487330'
171041*                                    '221487576'                  00709815
      *                                    '221494440'
      *                                    '221494446'
007102*                                    '221500556'                  00710215
007105*                                    '221500650'                  00710515
007108*                                    '221658917'                  00710815
007109*                                    '221750190'                  00710915
007110*                                    '221775306'                  00711015
      *                                    '221944440'
018863*                                    '222783298'                  01886301
007112*                                    '223014175'                  00711215
007114*                                    '223452311'                  00711415
007116*                                    '223452412'                  00711615
      *                                    '223601678'
      *                                    '226019101'
      *                                    '221487148'.
      *                                    '221487148'.
      *****************************EHIMS*******************************
007095                                                                  00709513
      *****************************EHIMS*******************************
007096*01  WS-PROVDR-NUM             PIC X(13).                         00709613
171041*    88  WS-NJ-HACKENSACK-PROVDR                                  01887601
171041*                              VALUE '310001       '.             01887701
171041*    88  WS-NJ-PROVDR          VALUE '310001       '              00712415
171041*    88  WS-NJ-PROVDR          VALUE '310002       '              00712415
171041*                                    '310002       '              00712515
007126*                                    '310003       '              00712615
007127*                                    '310008       '              00712715
007128*                                    '310009       '              00712815
007129*                                    '310012       '              00712915
007130*                                    '310013       '              00713015
007140*                                    '310016       '              00714015
007150*                                    '310018       '              00715015
007151*                                    '310019       '              00715115
007152*                                    '310020       '              00715215
007153*                                    '310024       '              00715315
007154*                                    '310025       '              00715415
007155*                                    '310026       '              00715515
007156*                                    '310028       '              00715615
007157*                                    '310034       '              00715715
007159*                                    '310037       '              00715915
007160*                                    '310038       '              00716015
007161*                                    '310039       '              00716115
007162*                                    '310040       '              00716215
007163*                                    '310042       '              00716315
036100*                                    '310051       '              03610027
042420*                                    '310054       '              04242006
007165*                                    '310063       '              00716515
007166*                                    '310070       '              00716615
007167*                                    '310072       '              00716715
007168*                                    '310073       '              00716815
018905*                                    '310074       '              01890501
007169*                                    '310075       '              00716915
007170*                                    '310076       '              00717015
036910*                                    '310077       '              03691027
007171*                                    '310078       '              00717115
007172*                                    '310090       '              00717215
007173*                                    '310093       '              00717315
007174*                                    '310096       '              00717415
037600*                                    '310105       '              03760027
007175*                                    '310108       '              00717515
007176*                                    '310111       '              00717615
007177*                                    '310112       '              00717715
007178*                                    '310116       '              00717815
007179*                                    '310118       '              00717915
007180*                                    '310119       '              00718015
007181*                                    '310120       '              00718115
038410*                                    '313025       '              03841027
007182*                                    '313300       '.             00718215
042443*    88  WS-CONTIG-CTY-PROVDR  VALUE '310045       '              04244302
042443*                                    '330010       '              04244302
042444*                                    '330016       '              04244401
042445*                                    '330047       '              04244501
042446*                                    '330092       '              04244601
042447*                                    '330116       '              04244701
042448*                                    '330121       '              04244801
042449*                                    '330136       '              04244903
042450*                                    '330250       '              04245003
042460*                                    '330276       '              04246003
042470*                                    '331302       '              04247003
042471*                                    '331305       '.             04247103
007170*                                                                 00717013
      *****************************EHIMS*******************************
       01  WS-PROD-TYPE                      PIC X(01).                 00830000
           88  WS-INTERNATIONAL              VALUE 'I'.                 00840000
           88  WS-NATIONAL                   VALUE 'N'.                 00850000
           88  SELECT-PROD-TYPE              VALUE 'I' 'N'.             00860000
                                                                        00870000
       01  WS-FACIL-TYPE                     PIC X(04).                 00880000
           88  WS-SHORT-TERM                 VALUE '0001'.              00890000
           88  WS-CHILD-HOSP                 VALUE '3300'.              00900000
           88  WS-MULT-HOSP                  VALUE '0900'.              00910000
           88  SELECT-FACIL-TYPE             VALUE '0001' '3300'        00920000
                                                   '0900'.              00930000
                                                                        00830024
       01  WS-SCCF-EDIT-ERROR-CD             PIC X(05) VALUE SPACES.    00710024
                                                                        00720024
REL120*01  WS-NF02-MSG.                                                 04248003
REL120*    10  FILLER                        PIC X(04) VALUE 'NF02'.    04249003
REL120*    10  WS-NF02-ERR                   PIC X(05) VALUE SPACES.    04249103
042492                                                                  04249203
       01  WS-ANC-SERV-DT                    PIC X(06) VALUE SPACES.    00730024
                                                                        00740024
       01  WS-PROC-SITE-CNTL-NUM.                                       00750024
           05  WS-PROC-SITE-CNTL-NUM-1-4.                               00760024
               10  WS-PROC-SITE-CNTL-NUM-1-2 PIC X(02).                 00770024
               10  FILLER                        PIC X(02).             00780024
           05  WS-PROC-SITE-CNTL-NUM-5-8.                               00790024
               10  WS-PROC-SITE-CNTL-NUM-5       PIC X(01).             00800024
               10  FILLER                        PIC X(03).             00810024
           05  FILLER                        PIC X(09).                 00820024
                                                                        00830024
       01  WS-ERROR-CODE-TABLE.                                         00840024
           05  FILLER                        PIC X(02).                 00850024
           05  WS-ERROR-CODES.                                          00860024
               10  WS-ERROR-CODE OCCURS 5 TIMES  PIC X(03).             00870024
                                                                        00880024
       01  WS-PAT-STATUS                     PIC X(02).                 00890024
           88  WS-PAT-STATUS-DISCHARGED      VALUE '01' THRU '07',      00900024
                                                   '10' THRU '19',      00910024
                                                   '20' THRU '29',      00920024
                                                   '40' THRU '42'.      00930024
                                                                        00940024
       01  WS-REV-CD.                                                   00950024
           05  WS-REV-CD-1                       PIC X(01).             01090039
           05  WS-REV-CD-2-4                     PIC X(03).             01100039
               88  WS-ACCOM-CD               VALUE '100' THRU '219'.    00970024
042488         88  WS-10X                    VALUE '110' THRU '119'.    04248800
               88  WS-14X                    VALUE '140' THRU '149'.    00980024
               88  WS-25X                    VALUE '250' THRU '259'.    00990024
                                                                        01010024
042523*01  WS-TYPE-ADM                            PIC X(1).             04252309
042524*    88  WS-EMERGENCY                       VALUE '1'.            04252409
042525*    88  WS-URGENT                          VALUE '2'.            04252509
042526*    88  WS-ELECTIVE                        VALUE '3'.            04252609
042527*    88  WS-NEWBORN                         VALUE '4'.            04252709
042528*    88  WS-ADM-INFO-NOT-AVAIL              VALUE '9'.            04252809
                                                                        01080026
       01  WS-TYPE-OF-BILL.                                             01090024
           05  WS-CLAIM-TYPE                       PIC X(1).            01100024
               88  WS-HOSPITAL                     VALUE '1'.           01110024
               88  WS-SNF                          VALUE '2'.           01120024
               88  WS-HOME-HEALTH                  VALUE '3'.           01130024
               88  WS-CHRI-SCI-HOSP                VALUE '4'.           01140024
               88  WS-CHRI-SCI-EXT-CARE            VALUE '5'.           01150024
               88  WS-INTERMEDIATE-CARE            VALUE '6'.           01160024
               88  WS-CLINIC                       VALUE '7'.           01170024
               88  WS-SPECIAL-FACILITY             VALUE '8'.           01180024
           05  WS-TYPE-OF-STAY                     PIC X(1).            01190024
               88  WS-INPATIENT-MEDICARE-A         VALUE '1'.           01200024
               88  WS-INPATIENT-MEDICARE-B         VALUE '2'.           01210024
               88  WS-OUTPATIENT                   VALUE '3'.           01220024
               88  WS-OTHER-PART-B                 VALUE '4'.           01230024
               88  WS-INTERMED-CARE-LEV-I          VALUE '5'.           01240024
               88  WS-INTERMED-CARE-LEV-II         VALUE '6'.           01250024
               88  WS-INTERMED-CARE-LEV-III        VALUE '7'.           01260024
               88  WS-SWING-BEDS                   VALUE '8'.           01270024
               88  WS-RURAL-HEALTH                 VALUE '1'.           01280024
               88  WS-FREE-STANDING-REN-DIAL       VALUE '2'.           01290024
               88  WS-FREE-STANDING                VALUE '3'.           01300024
               88  WS-CORF                         VALUE '4'.           01310024
               88  WS-OTHER-CLINIC                 VALUE '9'.           01320024
               88  WS-HOSPICE-NON-HOSP-BASED       VALUE '1'.           01330024
               88  WS-HOSPICE-HOSP-BASED           VALUE '2'.           01340024
               88  WS-AMBULATORY-SURG-CENTER       VALUE '3'.           01350024
               88  WS-FREE-STAND-BIRTH-CENTER      VALUE '4'.           01360024
           05  WS-FREQUENCY                        PIC X(1).            01370024
               88  WS-NON-PAYMENT                  VALUE '0'.           01380024
               88  WS-ADMIT-DISCHARGE              VALUE '1'.           01390024
               88  WS-INTERIM-FIRST-CLAIM          VALUE '2'.           01400024
               88  WS-INTERIM-CONT-CLAIM           VALUE '3'.           01410024
               88  WS-INTERIM-LAST-CLAIM           VALUE '4'.           01420024
               88  WS-LATE-CHRGS-CLAIM-ONLY        VALUE '5'.           01430024
               88  WS-ADJUSTMENT                   VALUE '6'.           01440024
               88  WS-REPLACEMENT                  VALUE '7'.           01450024
               88  WS-VOID-CANCEL                  VALUE '8'.           01460024
                                                                        01490024
       01  WS-SWITCHES.                                                 01500024
           05  WS-EMPTY-ERROR-CD-SLOT-SW     PIC X(01) VALUE 'N'.       01510025
               88  FOUND-EMPTY-ERROR-CD-SLOT VALUE 'Y'.                 01520025
               88  NO-EMPTY-ERROR-CD-SLOT    VALUE 'N'.                 01521031
           05  WS-FIRST-TIME-SW              PIC X(01) VALUE 'Y'.       01530024
               88  FIRST-TIME                VALUE 'Y'.                 01540024
           05  WS-PRICE-01-10-MTD-SW         PIC X(01) VALUE 'N'.       01580024
               88  FOUND-01-10-PRICE-MTD     VALUE 'Y'.                 01590024
               88  NOT-FOUND-01-10-PRICE-MTD VALUE 'N'.                 01600024
           05  WS-ANC-001-SW                 PIC X(01) VALUE 'N'.       01610024
               88  FOUND-ANC-001             VALUE 'Y'.                 01620024
           05  WS-ANC-SERV-DT-SW             PIC X(01) VALUE 'S'.       01630024
               88  ALL-ANC-SERV-DT-SAME      VALUE 'S'.                 01640024
               88  ANC-SERV-DT-DIFFERENT     VALUE 'D'.                 01650024
           05  WS-RATE-SW                    PIC X(01) VALUE 'N'.       01690024
               88  BOTH-RATES-FURNISHED      VALUE 'Y'.                 01700024
042561     05  WS-RATE-SW-NEW                PIC X(01) VALUE 'N'.       04256100
042562         88  SEMI-PRIV-RM-RATE-0       VALUE 'Y'.                 04256200
           05  WS-PRICE-MTD-RULE-SW          PIC X(01) VALUE 'N'.       01780024
               88  FOUND-PRICE-MTD-RULE-ERROR                           01790024
                                             VALUE 'Y'.                 01800024
           05  WS-STMT-COV-DT-SW             PIC X(01) VALUE 'N'.       01850025
               88  FOUND-STMT-COV-DT-ERROR                              01860025
                                             VALUE 'Y'.                 01870024
               88  NO-STMT-COV-DT-ERROR      VALUE 'N'.                 01880025
           05  WS-STMT-TO-DT-SW              PIC X(01) VALUE ' '.       01890024
               88  FOUND-STMT-TO-DT          VALUE 'Y'.                 01900024
               88  NO-STMT-TO-DT             VALUE 'N'.                 01910024
      *    05  WS-ANC-RECEIPT-DT-SW          PIC X(01) VALUE 'N'.       01920024
      *        88  FOUND-ANC-RECEIPT-DT-ERROR                           01930024
      *                                      VALUE 'Y'.                 01940024
      *        88  NO-ANC-RECEIPT-DT-ERROR   VALUE 'N'.                 01950024
           05  WS-PRIV-ROOM-RATE-SW          PIC X(01) VALUE 'N'.       02020024
               88  FOUND-PRIV-ROOM-RATE-ERROR                           02030024
                                             VALUE 'Y'.                 02040024
               88  NO-PRIV-ROOM-RATE-ERROR   VALUE 'N'.                 02050024
           05  WS-ITS-EDIT-ERROR-CD-SW       PIC X(01) VALUE 'N'.       02090024
               88  FOUND-ITS-EDIT-ERROR-CD   VALUE 'Y'.                 02100024
               88  NO-ITS-EDIT-ERROR-CD      VALUE 'N'.                 02110024
           05  WS-MEDICARE-ERROR-SW          PIC X(01) VALUE 'N'.       02120024
               88  FOUND-MEDICARE-ERROR      VALUE 'Y'.                 02130024
               88  NO-MEDICARE-ERROR         VALUE 'N'.                 02140024
           05  WS-FIRST42-SW                 PIC X(01) VALUE 'Y'.       02261037
               88  FIRST42                   VALUE 'Y'.                 02262037
               88  NOT-FIRST42               VALUE 'N'.                 02263037
           05  WS-PPO-EDIT-BYP-SW            PIC X(01) VALUE 'N'.       02551001
               88  BYPASS-PPO-EDIT           VALUE 'Y'.                 02552001
               88  NO-BYPASS-PPO-EDIT        VALUE 'N'.                 02553001
025534     05  WS-EMPIRE-PAR-PROV-SW         PIC X(01) VALUE 'N'.       02553418
025535         88  EMPIRE-PAR-PROV           VALUE 'Y'.                 02553518
025536         88  NOT-EMPIRE-PAR-PROV       VALUE 'N'.                 02553618
      ***CR117967 START
REL120*    05  WS-LOC-PLAN-800-SW            PIC X(01) VALUE 'N'.       02553418
REL120*        88  LOC-PLAN-800              VALUE 'Y'.                 02553518
REL120*        88  NOT-LOC-PLAN-800          VALUE 'N'.                 02553618
      ***CR117967 END
025537     05  WS-FIRST-ENGLEWOOD-SW         PIC X(01) VALUE 'Y'.       02553721
025538         88  FIRST-ENGLEWOOD           VALUE 'Y'.                 02553821
023200     05  WS-ECRP-SWITCH                PIC X(01).                 02320028
023300         88  WS-ECRP-YES               VALUE 'Y'.                 02330027
023400         88  WS-ECRP-NO                VALUE 'N' ' '.             02340028
042641         88  WS-ECRP-PLUS              VALUE 'H'.                 04264104
022800     05  WS-BLCD-COV-TO-DT             PIC X(01).                 02280009
022900         88  WS-BLCD-COV-DATE-Y        VALUE 'Y'.                 02290009
023000         88  WS-BLCD-COV-DATE-N        VALUE 'N' ' '.             02300009
058300***********U602************************************************** 05830015
058400     05  WS-U602-ZERO-PRICE-IND        PIC X(01).                 05840015
058500         88  U602-ZERO-PRICED-CLAIM       VALUE 'N'.              05850015
058600         88  U602-NOT-A-ZERO-PRICED-CLAIM VALUE 'Y'.              05860015
058700     05  WS-U602-CLAIM-LVL-IND         PIC X(01).                 05870020
058800         88  U602-CLAIM-LVL            VALUE 'Y'.                 05880020
058900***********U602************************************************** 05890015
REL120*    05  WS-LEASED-NET-IND             PIC X(01) VALUE 'N'.       05890404
REL120*        88  NO-LEASED-NET             VALUE 'N'.                 05890504
REL120*        88  FOUND-LEASED-NET          VALUE 'Y'.                 05890604
REL120*    05  WS-EITSZI002-SW               PIC X(01) VALUE 'N'.       05890703
REL120*        88  FOUND-EITSZI002           VALUE 'Y'.                 05890803
REL120*        88  NO-EITSZI002              VALUE 'N'.                 05890903
058910     05  WS-EITSZI033-SW               PIC X(01) VALUE 'N'.       05891003
058920         88  FOUND-EITSZI033           VALUE 'Y'.                 05892003
058921         88  NO-EITSZI033              VALUE 'N'.                 05892103
ZI115F*    05  WS-EITSZI115-SW               PIC X(01) VALUE 'N'.       05891003
ZI115F*        88  FOUND-EITSZI115           VALUE 'Y'.                 05892003
ZI115F*        88  NO-EITSZI115              VALUE 'N'.                 05892103
REL120     05  WS-EITSZI007-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZI007              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZI007           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZI017-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZI017              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZI017           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZI099-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZI099              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZI099           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZI113-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZI113              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZI113           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZI999-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZI999              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZI999           VALUE 'Y'.                 01730010
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135***CQ#420369 START
REL135*    05  WS-EITSZI135-SW               PIC X(01) VALUE 'N'.       01710010
REL135*        88  NO-EITSZI135              VALUE 'N'.                 01720010
REL135*        88  FOUND-EITSZI135           VALUE 'Y'.                 01730010
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919     05  WS-EITSZI116-SW               PIC X(01) VALUE 'N'.
CQ8919         88  NO-EITSZI116              VALUE 'N'.
CQ8919         88  FOUND-EITSZI116           VALUE 'Y'.
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135*    05  WS-GY-IDEN-SW                 PIC X(01) VALUE 'N'.       01710010
REL135*        88  WS-GY-NOT-FOUND           VALUE 'N'.                 01730010
REL135*        88  WS-GY-FOUND               VALUE 'Y'.                 01720010
REL135***CQ#420369 START
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
058929*REL9.0 START                                                     05892900
REL120*    05 WS-1015-BUNDLED-CLAIM          PIC X(01).                 05893000
REL120*        88 1015-BUNDLED-CLAIM         VALUE 'Y'.                 05893100
REL120*        88 1015-NOT-BUNDLED-CLAIM     VALUE 'N'.                 05893200
058933*REL9.0 END                                                       05893300
      ***CR136539 START
           05  WS-NEG-PRC-SW                 PIC X(01) VALUE 'N'.       02553418
               88  NEG-PRC                   VALUE 'Y'.                 02553518
               88  NOT-NEG-PRC               VALUE 'N'.                 02553618
      ***CR136539 END
      ***CR145948 START
           05  WS-RESUBMISSION-SW            PIC X(01) VALUE 'N'.       02553418
               88  RESUBMISSION-YES          VALUE 'Y'.                 02553518
               88  RESUBMISSION-NO           VALUE 'N'.                 02553618
      ***CR145948 END
      ***CR#148645 START
           05  WS-HIGH-DOLLAR-SW             PIC X(01) VALUE 'N'.       02553418
               88  FOUND-HIGH-DOLLAR         VALUE 'Y'.                 02553518
               88  NO-HIGH-DOLLAR            VALUE 'N'.                 02553618
      ***CR#148645 END
      ***CR#155472 START
           05  WS-INPAT-DME-MSG-SW           PIC X(01) VALUE 'N'.       02553418
               88  FOUND-INPAT-DME-MSG       VALUE 'Y'.                 02553518
               88  NO-INPAT-DME-MSG          VALUE 'N'.                 02553618
      ***CR#155472 END
       01  WS-CONSTANTS.                                                02280024
           05  WS-I                          PIC X(01) VALUE 'I'.       02290024
           05  WS-V                          PIC X(01) VALUE 'V'.       02300024
           05  WS-EITS-TEXT.                                            02310024
               10  FILLER                    PIC X(04) VALUE 'EITS'.    02320024
               10  WS-EITS-ERR               PIC X(05) VALUE SPACES.    02330024
      *** CR#148645 START
           05  WS-MAX-SERV-CHRG              PIC S9(8)V99 COMP-3
                                             VALUE +999999.99.
      *** CR#148645 END
      ***CR#155472 START
           05  WS-DME-MSG-CD-1               PIC X(04) VALUE SPACES.    02553418
               88  VALID-DME-MSG-CD-1        VALUE                      02553518
                 'U290','U291','U292',
                 'U293','U294','U295',
                 'U296'.
           05  WS-DME-MSG-CD-2               PIC X(04) VALUE SPACES.    02553418
               88  VALID-DME-MSG-CD-2        VALUE                      02553518
                 'U290','U291','U292',
                 'U293','U294','U295',
                 'U296'.
           05  WS-DME-MSG-CD-3               PIC X(04) VALUE SPACES.    02553418
               88  VALID-DME-MSG-CD-3        VALUE                      02553518
                 'U290','U291','U292',
                 'U293','U294','U295',
                 'U296'.
           05  WS-DME-MSG-CD-4               PIC X(04) VALUE SPACES.    02553418
               88  VALID-DME-MSG-CD-4        VALUE                      02553518
                 'U290','U291','U292',
                 'U293','U294','U295',
                 'U296'.
           05  WS-DME-MSG-CD-5               PIC X(04) VALUE SPACES.    02553418
               88  VALID-DME-MSG-CD-5        VALUE                      02553518
                 'U290','U291','U292',
                 'U293','U294','U295',
                 'U296'.
      ***CR#155472 END
                                                                        02340024
       01  WS-SUBSCRIPTS.                                               02350024
           05  CD-SUB                        PIC S9(04) COMP.           02360024
           05  30-SUB                        PIC S9(04) COMP.           02370024
           05  60-SUB                        PIC S9(04) COMP.           02380024
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135***CQ#420369 START                                                02380024
REL135*    05  GY-SUB                        PIC S9(04) COMP.           02380024
REL135***CQ#420369 START                                                02380024
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
           05  65-SUB                        PIC S9(04) COMP.           02390024
           05  VSUB                          PIC S9(04) COMP.           02400024
           05  WS-SUB                        PIC S9(04) COMP.           02410024
058919     05  OPL-SUB                       PIC S9(04) COMP.           05891900
           05  ERR-SUB                       PIC S9(04) COMP.           02420024
           05  ILG-SUB                       PIC S9(04) COMP.           02420024
060600***********U602************************************************** 06060015
060700     05  MSG-SUB                       PIC S9(04) COMP.           06070015
060800***********U602************************************************** 06080015
           05  COND-SUB                      PIC S9(04) COMP.           02430024
           05  VALUE-SUB                     PIC S9(04) COMP.           02440024
           05  ERR-SLOT                      PIC S9(04) COMP.           02450025
           05  WS-ACCOM-CTR                  PIC S9(04) COMP.           02460024
028810     05  SUBX                     COMP PIC S9(04) VALUE +0.             24
                                                                        02470024
       01  WS-PRICE-MTD-RULE-TABLE.                                     02480024
           05  WS-PRICING-MTD                PIC X(02).                 02490024
               88  WS-NO-PRICING-MTD         VALUE '  '.                02500024
               88  WS-CHRGS                  VALUE '01'.                02510024
               88  WS-PCT-OF-CHRGS           VALUE '10'.                02520024
               88  WS-PCT-ALLOW-AMT-PER-COS  VALUE '14'.                02530024
               88  WS-PCT-ALLOW-AMT-PER-UOS  VALUE '15'.                02540024
               88  WS-PER-DIEM               VALUE '20'.                02550024
               88  WS-DRG                    VALUE '30'.                02560024
               88  WS-CASE-ALLOW-PCT-OF-CHRGS                           02570024
                                             VALUE '33'.                02580024
               88  WS-FLAT-FEE-ALLOW-PER-COS VALUE '40'.                02590024
               88  WS-FLAT-FEE-ALLOW-PER-UOS VALUE '41'.                02600024
               88  WS-MULT-SRV-ALLOW         VALUE '42'.                02610024
               88  WS-MULT-SRV-ALLOW-PCT-OF-CHRGS                       02620024
                                             VALUE '43'.                02630024
               88  WS-ALL-INCL-PRICE-MTD     VALUE '20' '30'.           02640024
           05  WS-RULE-NUM.                                             02650024
               10  WS-RULE-NUM-1-2           PIC X(02).                 02660024
               10  WS-RULE-NUM-3-5           PIC X(03).                 02670024
                                                                        02680024
       01  WS-CLASS-PROV                     PIC X(01).                 02690024
031325     88  WS-CERT-1-PROV             VALUE '1' '4' '5'             03132508
031326                                          'D' 'E' 'F' 'G' 'M'.    03132608
031327     88  WS-CERT-2-PROV             VALUE '3' 'N'.                03132708
027201                                                                  02720105
027210 01  WS-VAL-CD                         PIC X(02).                 02721005
027220     88  VALID-VAL-CD               VALUE 'A1' 'B1' 'C1' 'D1' 'E1'02722005
027230                                          'F1' 'G1'.              02723005
027240 01  WS-VAL-AMT               COMP-3   PIC S9(08)V99.             02724005
027250     88  VALID-VAL-AMT              VALUE +736.00 +760.00 +764.00 02725005
027260                                          +768.00.                02726005
027270 01  WS-FED-TAX-ID-NUM                 PIC X(09).                 02727008
027280     88  WS-PPO-FL-TAX-ID           VALUE '930926631'             02728008
027281                                          '593337028'             02728108
027290                                          '411506440'.            02729008
060843**CR#82974                                                        06084300
060845 01  WS-MEDICARE-EDIT-FLAGS.                                      06084500
REL120*    05  WS-NASCO-REJ-FLAG             PIC X(01)    VALUE 'N'.    06084600
REL120*        88 NASCO-REJECT                            VALUE 'Y'.    06084700
REL120*        88 NO-NASCO-REJECT                         VALUE 'N'.    06084800
060850     05  WS-A1-IND                     PIC X(01)    VALUE 'N'.    06085000
060851         88 FOUND-A1                                VALUE 'Y'.    06085100
060852         88 NO-A1                                   VALUE 'N' ' '.06085200
060853     05  WS-A2-IND                     PIC X(01)    VALUE 'N'.    06085300
060854         88 FOUND-A2                                VALUE 'Y'.    06085400
060855         88 NO-A2                                   VALUE 'N' ' '.06085500
060856     05  WS-A3-IND                     PIC X(01)    VALUE 'N'.    06085600
060857         88 FOUND-A3                                VALUE 'Y'.    06085700
060858         88 NO-A3                                   VALUE 'N' ' '.06085800
060859     05  WS-A4-IND                     PIC X(01)    VALUE 'N'.    06085900
060860         88 FOUND-A4                                VALUE 'Y'.    06086000
060861         88 NO-A4                                   VALUE 'N' ' '.06086100
060862     05  WS-A5-IND                     PIC X(01)    VALUE 'N'.    06086200
060863         88 FOUND-A5                                VALUE 'Y'.    06086300
060864         88 NO-A5                                   VALUE 'N' ' '.06086400
060865     05  WS-OTH-IND                    PIC X(01)    VALUE 'N'.    06086500
060866         88 FOUND-OTH                               VALUE 'Y'.    06086600
060867         88 NO-OTH                                  VALUE 'N' ' '.06086700
060874     05  WS-A3-AMT-CLM                 PIC S9(8)V99 VALUE 0.      06087400
060875     05  WS-A4-AMT-CLM                 PIC S9(8)V99 VALUE 0.      06087500
060876     05  WS-A3-AMT-LN                  PIC S9(8)V99 VALUE 0.      06087600
060877     05  WS-A4-AMT-LN                  PIC S9(8)V99 VALUE 0.      06087700
060878     05  WS-A1-A3-AMT-CLM              PIC S9(8)V99 VALUE 0.      06087800
060879     05  WS-A2-A3-AMT-CLM              PIC S9(8)V99 VALUE 0.      06087900
060880     05  WS-A1-A3-AMT-LN               PIC S9(8)V99 VALUE 0.      06088000
060881     05  WS-A2-A3-AMT-LN               PIC S9(8)V99 VALUE 0.      06088100
060882     05  WS-TOT-CHRG                   PIC S9(8)V99 VALUE 0.      06088200
060886**                                                                06088600
      *** CR138938 START
           05  WS-CLAIM-PRICING              PIC S9(8)V99 COMP-3.
      *** CR138938 END
       EJECT                                                            02730024
027500 COPY EHPPOINS.                                                   02750003
027510 EJECT                                                            02751002
       01  ITCWSCC-RECORD.                                              02740024
           COPY ITCWSCC.                                                02750024
       EJECT                                                            02760024
       01  ITCWSMC-RECORD.                                              02770024
           COPY ITCWSMC.                                                02780024
       EJECT                                                            02790024
       01  ITCWSMCD-RECORD.                                             02800024
           COPY ITCWSMCD.                                               02810024
                                                                        02820024
       01  FILLER                            PIC X(45) VALUE            02830024
           '*** ITCPE004 WORKING STORAGE ENDS HERE ***'.                02840024
       EJECT                                                            02850024
      ****************  L I N K A G E    S E C T I O N  *************** 02860024
      *---------------------------------------------------------------* 02870024
       LINKAGE SECTION.                                                 02880024
      *---------------------------------------------------------------* 02890024
       01  LS-PROCESS-CONTROL.                                          02900024
           COPY ITSIAPC.                                                02910024
           15 WORK-AREA REDEFINES                                       02920024
              SIAPC-EIB-AREA.                                           02930024
              20 SUB         PIC 9(4).                                  02940024
              20 SUB1        PIC 9(4).                                  02950024
              20 SUB2        PIC 9(4).                                  02960024
              20             PIC X(88).                                 02970024
       EJECT                                                            02980024
       01  LS-BATCH-PROCESS.                                            02990024
           COPY ITCIABP.                                                03000024
       EJECT                                                            03010024
       01  LS-INST-ERROR-INDICATORS.                                    03020024
           COPY ITCIAEI.                                                03030024
       EJECT                                                            03040024
       01  LS-WORK-AREA.                                                03050024
           COPY ITCIAWA.                                                03060024
       EJECT                                                            03070024
       01  LS-INST-WORK-AREA1 REDEFINES LS-WORK-AREA.                   03080024
           05 FILLER                     PIC X(1275).                   03090025
       EJECT                                                            03100024
       01  LS-INST-WORK-AREA2 REDEFINES LS-WORK-AREA.                   03110024
REL101*    COPY ITCFM146.                                               03120024
REL101     05 FILLER                     PIC X(1275).                   03130024
       EJECT                                                            03140024
       01  LS-STACKED-CLAIM.                                            03150024
           COPY ITCIASC.                                                03160024
       EJECT                                                            03170024
       01  LS-INST-REC-TYPE-DATA REDEFINES LS-STACKED-CLAIM.            03180024
REL102*    02  FILLER              PIC X(4).                            03190025
REL102     02  FILLER              PIC X(10).                           03190025
           02  CIASC-INST-RECORDS.                                      03200025
               03  CIASC-05-RECORD.                                     03210025
                COPY ITCFM105.                                          03220025
               03  CIASC-10-RECORD.                                     03230025
                COPY ITCFM110.                                          03240025
               03  CIASC-15-RECORD.                                     03250025
                COPY ITCFM115.                                          03260025
               03  CIASC-20-RECORD.                                     03270025
                COPY ITCFM120.                                          03280025
               03  CIASC-30-RECORD OCCURS 3 TIMES.                      03290025
                COPY ITCFM130.                                          03300025
               03  CIASC-31-RECORD OCCURS 3 TIMES.                      03310025
                COPY ITCFM131.                                          03320025
033300         03  CIASC-32-RECORD.                                     03330006
033400          COPY ITCFM132.                                          03340006
REL120         03  CIASC-33-RECORD.                                     03330006
REL120          COPY ITCFM133.                                          03340006
               03  CIASC-40-RECORD.                                     03330025
                COPY ITCFM140.                                          03340025
               03  CIASC-41-RECORD.                                     03350025
                 COPY ITCFM141.                                         03360025
REL120         03  CIASC-42-RECORD.                                     03350025
REL120           COPY ITCFM142.                                         03360025
REL120         03  CIASC-43-RECORD.                                     03350025
REL120           COPY ITCFM143.                                         03360025
REL120         03  CIASC-44-RECORD.                                     03350025
REL120           COPY ITCFM144.                                         03360025
REL120         03  CIASC-45-RECORD.                                     03350025
REL120           COPY ITCFM145.                                         03360025
               03  CIASC-46-RECORD OCCURS 999 TIMES.                    03370025
REL101           COPY ITCFM146.                                         03380025
REL120         03  CIASC-47-RECORD.                                     03350025
REL120           COPY ITCFM147.                                         03360025
               03  CIASC-50-RECORD.                                     03390025
                COPY ITCFM150.                                          03400025
               03  CIASC-60-RECORD OCCURS 999 TIMES.                    03410025
                COPY ITCFM160.                                          03420025
               03  CIASC-65-RECORD OCCURS 999 TIMES.                    03430025
                COPY ITCFM165.                                          03440025
034410         03  CIASC-66-RECORD OCCURS 999 TIMES.                    03441006
034420          COPY ITCFM166.                                          03442006
               03  CIASC-70-RECORD.                                     03450025
                COPY ITCFM170.                                          03460025
REL120         03  CIASC-71-RECORD.                                     03350025
REL120           COPY ITCFM171.                                         03360025
REL120         03  CIASC-72-RECORD.                                     03350025
REL120           COPY ITCFM172.                                         03360025
REL120         03  CIASC-73-RECORD.                                     03350025
REL120           COPY ITCFM173.                                         03360025
REL120         03  CIASC-74-RECORD.                                     03350025
REL120           COPY ITCFM174.                                         03360025
REL120         03  CIASC-75-RECORD.                                     03350025
REL120           COPY ITCFM175.                                         03360025
               03  CIASC-80-RECORD.                                     03470025
                COPY ITCFM180.                                          03480025
               03  CIASC-90-RECORD.                                     03490025
                COPY ITCFM190.                                          03500025
           02  FILLER              PIC X(255).                          03510025
072600****** REL8.6 *********                                           07260004
072700/                                                                 07270004
072800 01  LS-ITCDBP20-RECORD.                                          07280004
072900     COPY ITCDBP20.                                               07290004
073000***********************                                           07300004
                                                                        03520024
       EJECT                                                            03530024
      ******************************************************************03540024
      ******************************************************************03550024
      *              P R O C E D U R E   D I V I S I O N               *03560024
      ******************************************************************03570024
      ******************************************************************03580024
                                                                        03590024
       PROCEDURE DIVISION USING LS-PROCESS-CONTROL                      03600024
                                LS-BATCH-PROCESS                        03610024
                                LS-INST-ERROR-INDICATORS                03620024
                                LS-WORK-AREA                            03630024
074300                          LS-STACKED-CLAIM                        07430004
074400******* REL8.6 *************                                      07440004
074500                          LS-ITCDBP20-RECORD.                     07450004
074600****************************                                      07460004
                                                                        03650024
      ******************************************************************03660024
       00000-MAINLINE.                                                  03670024
      ******************************************************************03680024
                                                                        03690024
           PERFORM A0000-INITIALIZATION                                 03700024
                                                                        03710024
           IF SIAPC-FUNCTION-CD-MAJOR = CWSCC-EDIT-DATA                 03720024
              PERFORM B0000-EDIT-PROCESS                                03730024
           ELSE                                                         03740024
              PERFORM Z9999-ERROR-ABORT                                 03750024
           END-IF                                                       03760024
                                                                        03770024
           GOBACK                                                       03780024
           .                                                            03790024
                                                                        03800024
       EJECT                                                            03810024
      ******************************************************************03820024
       A0000-INITIALIZATION.                                            03830024
      ******************************************************************03840024
                                                                        03850024
           IF CIABP-PLAN-INST-FIRST-TIME                                03860024
043600*       CALL 'IGZEDT4' USING BY REFERENCE WS-CURRENT-DATE         03870024
043700*       ACCEPT WS-CURRENT-TIME FROM TIME                          03880024
              DISPLAY WS-DISPLAY-MODULE                                 03890024
              MOVE 'N' TO CIABP-PLAN-INST-EDIT-SW                       03900024
           END-IF                                                       03910024
                                                                        03920024
           MOVE CWSCC-SUCCESSFUL      TO SIAPC-RETURN-IND               03930024
           MOVE CWSCC-ACTION-COMPLETE TO SIAPC-RETURN-STATUS            03940024
                                         SIAPC-ABEND-CODE               03950024
                                                                        03960024
           MOVE SPACES TO WS-SCCF-EDIT-ERROR-CD                         03970024
038510*                   WS-TAX-ID-NUM                                 03851013
038520*                   WS-PROVDR-NUM                                 03852013
REL120*                   WS-NF02-ERR                                   07463803
                                                                        04000024
           MOVE 'N'    TO WS-EMPTY-ERROR-CD-SLOT-SW                     04010025
                          WS-ITS-EDIT-ERROR-CD-SW                       04020024
                          WS-PRICE-01-10-MTD-SW                         04040024
                          WS-ANC-001-SW                                 04050024
                          WS-PRICE-MTD-RULE-SW                          04060024
      *                   WS-ANC-RECEIPT-DT-SW                          04070024
                          WS-STMT-COV-DT-SW                             04080025
                          WS-PRIV-ROOM-RATE-SW                          04120024
                          WS-MEDICARE-ERROR-SW                          04130024
044501                    WS-EMPIRE-PAR-PROV-SW                         04450118
      ***CR117967 START
REL120*                   WS-LOC-PLAN-800-SW
      ***CR117967 END
041300                    WS-ECRP-SWITCH                                04130027
041000                    WS-BLCD-COV-TO-DT                             04100009
080600***********U602************************************************** 08060015
080700                    WS-U602-ZERO-PRICE-IND                        08070015
080800                    WS-U602-CLAIM-LVL-IND                         08080020
080900***********U602************************************************** 08090015
REL120*                   WS-LEASED-NET-IND                             08090204
REL120*                   WS-EITSZI002-SW                               08090303
080904                    WS-EITSZI033-SW                               08090403
ZI115F*                   WS-EITSZI115-SW                               08090403
REL120                    WS-EITSZI007-SW                               03360010
REL120                    WS-EITSZI017-SW                               03360010
REL120                    WS-EITSZI099-SW                               03360010
REL120                    WS-EITSZI113-SW                               03360010
REL120                    WS-EITSZI999-SW                               03360010
080908*REL9.0 START                                                     08090800
REL120*                   WS-1015-BUNDLED-CLAIM                         08090900
080910*REL9.0 END                                                       08091000
      ***CR136539 START
                          WS-NEG-PRC-SW
      ***CR136539 END
      ***CR145948 START
                          WS-RESUBMISSION-SW
      ***CR145948 END
      ***CR#148645 START
                          WS-HIGH-DOLLAR-SW
      ***CR#148645 END
      ***CR#155472 START
                          WS-INPAT-DME-MSG-SW
      ***CR#155472 END
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135***CQ#420369 START
REL135*                   WS-EITSZI135-SW
REL135*                   WS-GY-IDEN-SW
REL135***CQ#420369 END
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919                    WS-EITSZI116-SW
                                                                        04450101
080903     INITIALIZE WS-MEDICARE-EDIT-FLAGS                            08090302
080904                                                                  08090402
           SET NO-BYPASS-PPO-EDIT     TO  TRUE                          04451001
                                                                        04160024
           IF FM105-ERR-CDS = SPACES                                    04170024
              MOVE 'N' TO WS-ITS-EDIT-ERROR-CD-SW                       04180024
              MOVE 1   TO ERR-SLOT                                      04190025
           ELSE                                                         04200024
              MOVE 'Y' TO WS-ITS-EDIT-ERROR-CD-SW                       04210024
              PERFORM A1000-CHECK-EDIT-ERROR-CDS                        04220025
                 VARYING ERR-SUB FROM 1 BY 1                            04230024
                 UNTIL   ERR-SUB > 5 OR                                 04240024
                         FOUND-EMPTY-ERROR-CD-SLOT                      04250025
              IF NO-EMPTY-ERROR-CD-SLOT                                 04250131
                 MOVE 6   TO ERR-SLOT                                   04251031
              END-IF                                                    04260031
           END-IF                                                       04261031
           .                                                            04270024
                                                                        04280024
      ******************************************************************04290024
       A1000-CHECK-EDIT-ERROR-CDS.                                      04300025
      ******************************************************************04310024
                                                                        04320024
           IF FM105-ERR-CD (ERR-SUB) = SPACES                           04330024
              MOVE ERR-SUB TO ERR-SLOT                                  04340025
              MOVE 'Y'     TO WS-EMPTY-ERROR-CD-SLOT-SW                 04350025
           ELSE                                                         04360024
              MOVE 'N'     TO WS-EMPTY-ERROR-CD-SLOT-SW                 04370025
           END-IF                                                       04380024
           .                                                            04390024
                                                                        04400024
       EJECT                                                            04410024
      ******************************************************************04420024
       B0000-EDIT-PROCESS.                                              04430024
      ******************************************************************04440024
                                                                        04450024
044540     MOVE FM105-ECR-IND   TO  WS-ECRP-SWITCH                      04454028
044545                                                                  04454527
           MOVE FM105-PROC-SITE-CNTL-NUM TO                             04460024
                      WS-PROC-SITE-CNTL-NUM                             04470024
                                                                        04480024
044549     IF (WS-PROC-SITE-CNTL-NUM-1-4 = 'EITS') AND                  04454927
044550         WS-ECRP-NO                                               04455027
              MOVE WS-PROC-SITE-CNTL-NUM-5-8 TO CWSMCD-MESSAGE-CD       04500024
044570           IF CWSMCD-VALID-MESSAGE-CD   OR                        04457027
044580              WS-PROC-SITE-CNTL-NUM-5 = 'Z'                       04458027
044590               PERFORM B3000-DETERMINE-CLAIM-STATUS               04459027
REL120*          ELSE                                                   04460027
      ***CR#154112 START
REL120*             IF (WS-PROC-SITE-CNTL-NUM-5-8 = 'UNKN') AND
REL120*                 FM105-ADJUSTMENT
REL120*                CONTINUE
REL120*             ELSE
      ***CR#154112 END
REL120*                MOVE 'ZI094' TO WS-SCCF-EDIT-ERROR-CD            04470027
REL120*                PERFORM B1999-INSERT-ERROR-CODE                  04480027
REL120*                PERFORM B3000-DETERMINE-CLAIM-STATUS             04490027
      ***CR#154112 START
REL120*             END-IF
      ***CR#154112 END
045000           END-IF                                                 04500027
           ELSE                                                         04590024
098264*CR#141076 BEGIN                                                  09826414
REL120*       IF WS-ECRP-YES                                            09826515
REL120*          MOVE 'ZI112' TO WS-SCCF-EDIT-ERROR-CD                  09826615
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        09826715
REL120*          PERFORM B3000-DETERMINE-CLAIM-STATUS                   09826815
REL120*       END-IF                                                    09826915
098270*CR#141076 END                                                    09827014
              PERFORM B1000-EDIT-CLAIM                                  04600024
              PERFORM B2000-EITSXXXXX-CHK                               04610024
              PERFORM B3000-DETERMINE-CLAIM-STATUS                      04620024
           END-IF                                                       04630024
           .                                                            04640024
                                                                        04650024
      ******************************************************************04660024
       B1000-EDIT-CLAIM.                                                04670024
      ******************************************************************04680024
                                                                        04690024
045500     PERFORM B1001-ITS-PAYER-IND                                  04550022
080968     MOVE FM130-SUB-ID-PFX-1-3 (SUBX) TO WS-SUB-ID-PFX-1-3        08096804
045600                                                                  04560022
           IF FM105-REC-TYPE NOT = SPACES AND                           04700035
REL101*        FM105-SEQ-NUM NUMERIC      AND                           04701035
083197         FM105-SEQ-NUM NOT = ZEROES                               04710028
047300        PERFORM B1140-MAP-LAST-ACT-DT                             04730028
      ***CR117967 START
REL120*       PERFORM B1180-EDIT-LOC-PLAN-800                           04740000
      ***CR117967 END
047500     END-IF                                                       04750028
047600                                                                  04760028
047700     IF FM105-REC-TYPE NOT = SPACES AND                           04770028
REL101*        FM105-SEQ-NUM NUMERIC      AND                           04780028
047900         FM105-SEQ-NUM NOT = ZEROES AND                           04790028
048000         WS-ECRP-NO                                               04800028
              PERFORM B1100-EDIT-PLAN-PAYER-CD                          04710024
              PERFORM B1120-EDIT-RESUBMISSION-IND                       04730024
088701*       PERFORM B1140-MAP-LAST-ACT-DT                             04750024
           END-IF                                                       04770024
                                                                        04780024
           IF FM110-REC-TYPE NOT = SPACES AND                           04791035
REL101*        FM110-SEQ-NUM NUMERIC      AND                           04792035
               FM110-SEQ-NUM NOT = ZEROES                               04793035
REL120*       PERFORM B1210-EDIT-FEDERAL-TAX-ID-NUM                     04810024
              PERFORM B1220-EDIT-TYPE-OF-BILL                           04820024
048537        PERFORM B1230-EDIT-EMPIRE-PAR-PROV                        04853718
           END-IF                                                       04830024
                                                                        04840024
REL114*************************EHIMS************************************
REL114*    IF FM115-REC-TYPE NOT = SPACES AND                           04851035
REL101*        FM115-SEQ-NUM NUMERIC      AND                           04852035
REL114*        FM115-SEQ-NUM NOT = ZEROES                               04853035
REL114*       PERFORM B1310-EDIT-CERTIFICATION-IND                      04870024
REL114*    END-IF                                                       04880024
REL114*************************EHIMS************************************
                                                                        04890024
      ***CR#148645 START
           PERFORM B1330-CHK-HIGH-DOLLAR VARYING 60-SUB FROM 1 BY 1     11125800
           UNTIL FOUND-HIGH-DOLLAR OR 60-SUB > 999 OR                   11125900
                 ( FM160-REC-TYPE(60-SUB) = SPACES AND                  11126100
                   FM160-SEQ-NUM(60-SUB) NOT = ZEROS )                  11125900
      ***CR#148645 END

140137*    IF FM120-REC-TYPE NOT = SPACES AND                           04901035
REL101*        FM120-SEQ-NUM NUMERIC      AND                           04902035
140137*        FM120-SEQ-NUM NOT = ZEROES                               04903035
088727********IF FM105-REC-TYPE NOT = SPACES AND                        08872709
088728********    FM105-SEQ-NUM NUMERIC      AND                        08872809
088729********    FM105-SEQ-NUM NOT = ZEROES                            08872909
088730********   PERFORM B1350-EDIT-ADMIT-HR                            08873009
088731********END-IF                                                    08873109
140137*       PERFORM B1360-EDIT-STMT-COV-DT                            04940025
140137*    END-IF                                                       04960024
                                                                        04970024
REL120*    IF WS-LEASED-NET AND (WS-ECRP-NO OR WS-ECRP-PLUS)            08874304
REL120*       PERFORM B1450-LEASED-NET                                  08874405
REL120*    END-IF                                                       08874504
                                                                        05010024
REL120*    PERFORM B1500-EDIT-CS90-DATE-CHECK                           05020024
                                                                        05030024
                                                                        05050024
           PERFORM B1600-EDIT-PRIV-ROOM-CLAIM                           05060024
                                                                        05070024
      ***CR155472 START
           IF FM105-REC-TYPE NOT = SPACES AND
              FM105-SEQ-NUM NOT = ZEROES AND
              FM105-INS-INPATIENT
                PERFORM B1650-EDIT-INPAT-DME VARYING 65-SUB FROM 1 BY 1
                UNTIL 65-SUB > 999 OR FOUND-INPAT-DME-MSG
           END-IF                                                       05591001
      ***CR155472 END
                                                                        05090024
098206                                                                  09820603
      *****PERFORM B1700-EDIT-ACCOM-CODES                               05100024
                                                                        05110024
                                                                        05470048
098216*    PERFORM B1820-EDIT-FLORIDA-FOR-PPO                           09821602
      ***CR138938 START
           PERFORM B1860-EDIT-NEG-CLM
           IF NEG-PRC
              CONTINUE
           ELSE
      ***CR138938 END
      ***CR136539 START
              PERFORM B1970-EDIT-NEG-PRC
              VARYING 65-SUB  FROM 1 BY 1
                 UNTIL 65-SUB > 999 OR NEG-PRC
      ***CR136539 END
      ***CR138938 START
           END-IF
      ***CR138938 END
054900                                                                  05490000
098223                                                                  09822301
REL114*************************EHIMS************************************
REL114*    IF NO-BYPASS-PPO-EDIT                                        05581001
REL114*       PERFORM B1850-EDIT-ALL-PPO                                05590001
REL114*    END-IF                                                       05591001
REL114*************************EHIMS************************************
095800***********U602************************************************** 09580015
095810     IF WS-ECRP-NO                                                09581031
095900        PERFORM B1900-EDIT-U602                                   09590031
096000        IF U602-NOT-A-ZERO-PRICED-CLAIM                           09600031
096100           MOVE 'ZI100' TO WS-SCCF-EDIT-ERROR-CD                  09610031
096200           PERFORM B1999-INSERT-ERROR-CODE                        09620031
096300        END-IF                                                    09630031
096300     END-IF                                                       09630015
096400***********U602************************************************** 09640015
098229**CR#82974 NASCO MEDICARE CLAIMS VALIDATIONS********************* 09822900
098231                                                                  09823100
098232     IF FM105-LINE-BUS = '9'                    AND               09823200
098233        ( FM105-CNTL-PLAN-CD = '243' OR '803' ) AND               09823300
098234          WS-ECRP-NO                            AND               09823400
098235        ( NOT WS-INTERNATIONAL )                AND               09823500
098236        ( WS-MERRILL-LYNCH OR WS-IBM OR                           09823600
098237          WS-MLC OR WS-INA OR WS-PBG )                            09823700
098238                                                                  09823800
098239        IF FM130-MED-CLM-REJ-CD-CLM(SUBX) = SPACES                09823900
098240           PERFORM B1200-NASCO-MEDICARE-EDIT                      09824000
098241        END-IF                                                    09824100
098242                                                                  09824200
REL120*       IF NASCO-REJECT                                           09824300
REL120*          MOVE 'ZI102' TO WS-SCCF-EDIT-ERROR-CD                  09824400
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        09824500
REL120*       END-IF                                                    09824600
098247     END-IF                                                       09824700
098254*REL9.0 START                                                     09825400
REL120*    IF WS-ECRP-NO                                                09825500
REL120*       PERFORM B1960-EDIT-M1015                                  09825600
REL120*       IF 1015-BUNDLED-CLAIM                                     09825700
REL120*          MOVE 'ZI108' TO WS-SCCF-EDIT-ERROR-CD                  09825800
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        09825900
REL120*       END-IF                                                    09826000
REL120*    END-IF                                                       09826100
098262*REL9.0 END                                                       09826200
      ***CR137498 START
           PERFORM B1980-REMV-ILG-SYB
                   VARYING ILG-SUB  FROM 1 BY 1
                      UNTIL ILG-SUB > 3
      ***CR137498 END
ZI115F*    IF FM105-REC-TYPE = '05' AND
ZI115F*       FM105-TRANS-IND = 'P' AND
ZI115F*       FM105-ICD-PROV-SUB-VER-IND = '0' AND
REL120*       (FM120-STMT-COV-FROM-DT  < '20141001' OR
REL120*        FM160-DT-SERV (1) < '20141001')
ZI115F*       (FM120-STMT-COV-FROM-DT  < '20151001' OR
ZI115F*        FM160-DT-SERV (1) < '20151001')
ZI115F*       MOVE 'ZI115' TO WS-SCCF-EDIT-ERROR-CD                     09824400
ZI115F*       PERFORM B1999-INSERT-ERROR-CODE                           09824500
ZI115F*       SET FOUND-EITSZI115    TO TRUE                            15481603
ZI115F*    END-IF
ICDFIX*    IF FM105-TRANS-IND = 'P' AND
ICDFIX*       FM105-ICD-PROV-SUB-VER-IND NOT = '0'
ICDFIX*       IF FM105-CLM-TYPE = '11' AND
ICDFIX*            FM120-STMT-COV-TO-DT  >= '20151001'
ICDFIX*            MOVE 'ZI116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX*            PERFORM B1999-INSERT-ERROR-CODE
ICDFIX*            SET FOUND-EITSZI116    TO TRUE
ICDFIX*       ELSE
ICDFIX*            IF FM105-CLM-TYPE = '12' AND
ICDFIX*            FM120-STMT-COV-FROM-DT  >= '20151001'
ICDFIX*             MOVE 'ZI116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX*             PERFORM B1999-INSERT-ERROR-CODE
ICDFIX*             SET FOUND-EITSZI116    TO TRUE
ICDFIX*            END-IF
ICDFIX*       END-IF
ICDFIX*    END-IF
ICDFIX*
CQ8919     IF FM105-TRANS-IND = 'P' AND
ICDFIX        ((FM105-CLM-TYPE = '11' AND
ICDFIX                   FM120-STMT-COV-TO-DT  >= '20151001') OR
ICDFIX         (FM105-CLM-TYPE = '12' AND
ICDFIX                   FM120-STMT-COV-FROM-DT >= '20151001'))
ICDFIX        EVALUATE TRUE
ICDFIX         WHEN FM105-ICD-PROV-SUB-VER-IND = '9'
CQ8919              MOVE 'ZI116' TO WS-SCCF-EDIT-ERROR-CD
CQ8919              PERFORM B1999-INSERT-ERROR-CODE
CQ8919              SET FOUND-EITSZI116    TO TRUE
ICDFIX         WHEN FM105-ICD-PROV-SUB-VER-IND = (SPACES OR
ICDFIX                LOW-VALUES)
ICDFIX              IF FM171-ICD-10-DIAG-CDS = (SPACES OR
ICDFIX                 LOW-VALUES) AND
ICDFIX                 FM171-ICD-10-ADMIT-DIAG-CD = (SPACES OR
ICDFIX                 LOW-VALUES) AND
ICDFIX                 FM171-ICD-10-PRINC-DIAG-CD = (SPACES OR
ICDFIX                 LOW-VALUES)
ICDFIX                MOVE 'ZI116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX                PERFORM B1999-INSERT-ERROR-CODE
ICDFIX                SET FOUND-EITSZI116    TO TRUE
ICDFIX              ELSE
ICDFIX                 MOVE '0' TO FM105-ICD-PROV-SUB-VER-IND
ICDFIX                 DISPLAY "ICD-10 SF INST:" FM105-SCCF-SER-NUM
ICDFIX              END-IF
ICDFIX         WHEN FM105-ICD-PROV-SUB-VER-IND = '0'
ICDFIX              CONTINUE
ICDFIX        END-EVALUATE
CQ8919     END-IF
ICDFIX*
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL140*    IF FM130-SUB-ID-PFX-1-3(SUBX) = 'YLV' OR
REL140*       FM130-SUB-ID-PFX-1-3(SUBX) = 'YLR' OR
REL140*       FM130-SUB-ID-PFX-1-3(SUBX) = 'XGH' OR
REL140*       FM130-SUB-ID-PFX-1-3(SUBX) = 'XGK'
REL140*       CONTINUE
REL140*    ELSE
REL135***CQ#420369 START
REL135*       PERFORM B1335-SET-GY-IDENTIFER VARYING GY-SUB FROM 1 BY 1 11125800
REL135*       UNTIL   GY-SUB > 999 OR                                   11125900
REL135*               WS-GY-FOUND                                       11125900
REL135*
REL135*       IF WS-GY-FOUND
REL135*       PERFORM B1335-CHK-GY-IDENTIFER VARYING GY-SUB FROM 1 BY 1 11125800
REL135*       UNTIL   GY-SUB > 999    OR                                11125900
REL135*               FOUND-EITSZI135 OR                                11125900
REL135*              (FM160-REV-CD (GY-SUB) = 0001) OR                  11125900
REL135*              (FM160-REC-TYPE(GY-SUB) = SPACES AND               11125900
REL135*               FM160-SEQ-NUM(GY-SUB) NOT = ZEROS)                11125900
REL135*       END-IF                                                    11125900
REL135***CQ#420369 END
REL135*    END-IF                                                       11125900
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START

           .                                                            05150024
      ******************************************************************05170024
051400 B1001-ITS-PAYER-IND.                                             05140022
051500******************************************************************05150022
051501                                                                  05150122
051502     EVALUATE TRUE                                                05150222
051503        WHEN FM130-ITS-PAYER-IND (1) = 'Y'                        05150322
051504           MOVE +1 TO SUBX                                        05150422
051505        WHEN FM130-ITS-PAYER-IND (2) = 'Y'                        05150522
051506           MOVE +2 TO SUBX                                        05150622
051507        WHEN FM130-ITS-PAYER-IND (3) = 'Y'                        05150722
051508           MOVE +3 TO SUBX                                        05150822
051509        WHEN OTHER                                                05150922
051510           MOVE +1 TO SUBX                                        05151022
051511     END-EVALUATE                                                 05151122
051512     .                                                            05151222
051513                                                                  05151322
051514******************************************************************05151422
       B1100-EDIT-PLAN-PAYER-CD.                                        05180024
      ******************************************************************05190024
                                                                        05200024
           IF FM105-PROCESSING-PLAN-PAYER                               05210024
055934*       IF FM105-PAY-PROV OR FM105-PAY-REP-PAYEE                  05570005
055935        IF FM105-PAY-PROV OR                                      05580005
055936         ((FM105-PMT-DISP-CD = '4') AND                           05590005
055937                (FM105-SER-NUM-JULIAN-DATE < '2001134'))          05591005
                 MOVE 'ZI007' TO WS-SCCF-EDIT-ERROR-CD                  05230024
                 PERFORM B1999-INSERT-ERROR-CODE                        05240024
REL120           SET FOUND-EITSZI007    TO TRUE                         15481603
              END-IF                                                    05250024
           END-IF                                                       05260024
055939                                                                  05593901
154413     IF FM105-INTL-CD NOT = 'I'                                   15441303
055943        IF FM105-LOC-PLAN-PAYER AND                               05594301
055945           FM105-PAY-SUB-REP                                      05594505
055945           MOVE 'ZI099' TO WS-SCCF-EDIT-ERROR-CD                  05594501
055946           PERFORM B1999-INSERT-ERROR-CODE                        05594601
REL120           SET FOUND-EITSZI099    TO TRUE                         15481603
055948        END-IF                                                    05594801
154419     END-IF                                                       15441903
           .                                                            05270024
                                                                        05280024
      ******************************************************************05290024
      ******************************************************************05750024
       B1120-EDIT-RESUBMISSION-IND.                                     05760024
      ******************************************************************05770024
                                                                        05780024
           IF FM105-ORIGINAL-RESUBMISSION    OR                         05790024
              FM105-ADJUSTMENT-RESUBMISSION OR                          05800024
              FM105-STORE-FRONT                                         05810024
              MOVE 'ZI017' TO WS-SCCF-EDIT-ERROR-CD                     05820024
      *CR 145948 BEGIN
              SET RESUBMISSION-YES        TO TRUE
      *CR 145948 END
              PERFORM B1999-INSERT-ERROR-CODE                           05830024
REL120        SET FOUND-EITSZI017    TO TRUE                            15481603
           END-IF                                                       05840024
           .                                                            05850024
                                                                        05860024
      ******************************************************************05870024
       B1140-MAP-LAST-ACT-DT.                                           06290024
      ******************************************************************06300024
                                                                        06310024
070900     MOVE SIAPC-GREG-CURRENT-CENTURY TO FM105-ONL-DT-LAST-ACT-CC  06320024
071000     MOVE SIAPC-GREG-CURRENT-YEAR    TO FM105-ONL-DT-LAST-ACT-YY  06330024
071100     MOVE SIAPC-GREG-CURRENT-MONTH   TO FM105-ONL-DT-LAST-ACT-MM  06340024
071200     MOVE SIAPC-GREG-CURRENT-DAY     TO FM105-ONL-DT-LAST-ACT-DD  06350024
           .                                                            06360024
      ******************************************************************06380024
      ***CR117967 START
      ******************************************************************06990024
REL120*B1180-EDIT-LOC-PLAN-800.                                         06990027
      ******************************************************************07010024
REL120*
REL120*    IF WS-ECRP-PLUS AND FM105-LOC-PLAN-CD = '800'
REL120*       SET LOC-PLAN-800 TO TRUE
REL120*       MOVE 'ZI110' TO WS-SCCF-EDIT-ERROR-CD
REL120*       PERFORM B1999-INSERT-ERROR-CODE
REL120*    END-IF
REL120*    .                                                            06970000
      ***CR117967 END
REL120*
      ******************************************************************07090024
111232 B1200-NASCO-MEDICARE-EDIT.                                       11123200
111233******************************************************************11123300
111234                                                                  11123400
111247     IF  FM132-REC-TYPE NOT = SPACES AND                          11124700
               FM132-SEQ-NUM  NOT = ZEROES                              11124800
REL101*        FM132-SEQ-NUM  NUMERIC                                   11124900
111251         PERFORM B1210-CHECK-OPL-CLM VARYING OPL-SUB FROM 1 BY 1  11125100
111252         UNTIL OPL-SUB > 16 OR                                    11125200
111253               FM132-OPL-VAL-CD-CLM(OPL-SUB) = SPACES             11125300
111256     END-IF                                                       11125600
111257                                                                  11125700
111258     PERFORM B1220-CHECK-OPL-LN VARYING 60-SUB FROM 1 BY 1        11125800
111259     UNTIL 60-SUB > 999 OR (FM160-REC-TYPE(60-SUB) = SPACES )     11125900
REL101*                           FM160-SEQ-NUM(60-SUB) NOT NUMERIC )   11126100
111264                                                                  11126400
REL120*    PERFORM B1230-EDIT-NASCO-REJECT                              11126500
111266     .                                                            11126600
111267                                                                  11126700
111268******************************************************************11126800
111269 B1210-CHECK-OPL-CLM.                                             11126900
111270******************************************************************11127000
111271                                                                  11127100
111272     EVALUATE FM132-OPL-VAL-CD-CLM(OPL-SUB)                       11127200
111273        WHEN 'A1'                                                 11127300
111274           SET FOUND-A1              TO TRUE                      11127400
111275           ADD FM132-OPL-VAL-AMT-CLM(OPL-SUB)                     11127500
111276                                     TO WS-A1-A3-AMT-CLM          11127600
111278        WHEN 'A2'                                                 11127800
111279           SET FOUND-A2              TO TRUE                      11127900
111280           ADD FM132-OPL-VAL-AMT-CLM(OPL-SUB)                     11128000
111281                                     TO WS-A2-A3-AMT-CLM          11128100
111283        WHEN 'A3'                                                 11128300
111284           SET FOUND-A3              TO TRUE                      11128400
111285           ADD FM132-OPL-VAL-AMT-CLM(OPL-SUB)                     11128500
111286                                     TO WS-A3-AMT-CLM             11128600
111287                                        WS-A1-A3-AMT-CLM          11128700
111288                                        WS-A2-A3-AMT-CLM          11128800
111289        WHEN 'A4'                                                 11128900
111290           SET FOUND-A4              TO TRUE                      11129000
111291           ADD FM132-OPL-VAL-AMT-CLM(OPL-SUB)                     11129100
111292                                     TO WS-A4-AMT-CLM             11129200
111293        WHEN 'A5'                                                 11129300
111294           SET FOUND-A5              TO TRUE                      11129400
111295        WHEN OTHER                                                11129500
111296           SET FOUND-OTH             TO TRUE                      11129600
111297     END-EVALUATE                                                 11129700
111298     .                                                            11129800
111341                                                                  11134100
111342******************************************************************11134200
111343 B1220-CHECK-OPL-LN.                                              11134300
111344******************************************************************11134400
111345                                                                  11134500
111346     IF FM160-REV-CD(60-SUB) = 0001                               11134600
111347        MOVE FM160-SERV-CHRG(60-SUB) TO WS-TOT-CHRG               11134700
111348     END-IF                                                       11134800
111349                                                                  11134900
111350     PERFORM VARYING OPL-SUB FROM 1 BY 1 UNTIL OPL-SUB > 16 OR    11135000
111352             FM166-OPL-VAL-CD-LN(60-SUB OPL-SUB) = SPACES         11135200
111353                                                                  11135300
111354       EVALUATE FM166-OPL-VAL-CD-LN(60-SUB OPL-SUB)               11135400
111355          WHEN 'A1'                                               11135500
111356             SET FOUND-A1            TO TRUE                      11135600
111357             ADD FM166-OPL-VAL-AMT-LN(60-SUB OPL-SUB)             11135700
111358                                     TO WS-A1-A3-AMT-LN           11135800
111360          WHEN 'A2'                                               11136000
111361             SET FOUND-A2            TO TRUE                      11136100
111362             ADD FM166-OPL-VAL-AMT-LN(60-SUB OPL-SUB)             11136200
111363                                     TO WS-A2-A3-AMT-LN           11136300
111365          WHEN 'A3'                                               11136500
111366             SET FOUND-A3            TO TRUE                      11136600
111367             ADD FM166-OPL-VAL-AMT-LN(60-SUB OPL-SUB)             11136700
111368                                     TO WS-A3-AMT-LN              11136800
111369                                        WS-A1-A3-AMT-LN           11136900
111370                                        WS-A2-A3-AMT-LN           11137000
111371          WHEN 'A4'                                               11137100
111372             SET FOUND-A4            TO TRUE                      11137200
111373             ADD FM166-OPL-VAL-AMT-LN(60-SUB OPL-SUB)             11137300
111374                                     TO WS-A4-AMT-LN              11137400
111375          WHEN 'A5'                                               11137500
111376             SET FOUND-A5            TO TRUE                      11137600
111377          WHEN OTHER                                              11137700
111378             SET FOUND-OTH           TO TRUE                      11137800
111379       END-EVALUATE                                               11137900
111380     END-PERFORM                                                  11138000
111381     .                                                            11138100
111382                                                                  11138200
111390******************************************************************11139000
REL120*B1230-EDIT-NASCO-REJECT.                                         11140000
111401******************************************************************11140100
REL120*                                                                 11140200
REL120*    EVALUATE TRUE                                                11140300
REL120*       WHEN FOUND-A5 AND FOUND-A3                                11140401
REL120*          CONTINUE                                               11140500
REL120*       WHEN FOUND-A1 AND NO-A3                                   11140600
REL120*       WHEN FOUND-A2 AND NO-A3                                   11140700
REL120*       WHEN FOUND-A1 AND NO-A2 AND NO-A3 AND NO-A4 AND NO-A5 AND 11140800
REL120*            NO-OTH                                               11140900
REL120*       WHEN FOUND-A2 AND NO-A1 AND NO-A3 AND NO-A4 AND NO-A5 AND 11141000
REL120*            NO-OTH                                               11141100
REL120*       WHEN FOUND-A3 AND NO-A1 AND NO-A2 AND NO-A4 AND NO-A5 AND 11141200
REL120*            NO-OTH                                               11141300
REL120*       WHEN FOUND-A1 AND NO-A2 AND NO-A3 AND FOUND-A4 AND NO-A5  11141400
REL120*            AND NO-OTH                                           11141500
REL120*       WHEN NO-A1 AND FOUND-A2 AND NO-A3 AND FOUND-A4 AND NO-A5  11141600
REL120*            AND NO-OTH                                           11141700
REL120*       WHEN NO-A1 AND NO-A2 AND FOUND-A3 AND FOUND-A4 AND NO-A5  11141800
REL120*            AND NO-OTH AND ( WS-A3-AMT-CLM NOT = WS-A4-AMT-CLM ) 11141900
REL120*       WHEN NO-A1 AND NO-A2 AND FOUND-A3 AND FOUND-A4 AND NO-A5  11142100
REL120*            AND NO-OTH AND ( WS-A3-AMT-LN  NOT = WS-A4-AMT-LN  ) 11142200
111423*       WHEN WS-A1-A3-AMT-CLM = WS-TOT-CHRG                       11142300
111424*       WHEN WS-A1-A3-AMT-LN  = WS-TOT-CHRG                       11142400
111425*       WHEN WS-A2-A3-AMT-CLM = WS-TOT-CHRG                       11142500
111426*       WHEN WS-A2-A3-AMT-LN  = WS-TOT-CHRG                       11142600
REL120*          SET NASCO-REJECT          TO TRUE                      11142700
REL120*    END-EVALUATE                                                 11142800
REL120*    .                                                            11142900
111430                                                                  11143000
REL120* REL 12.0 CHANGES BEGINS                                         11143000
111431******************************************************************11143100
REL120*B1210-EDIT-FEDERAL-TAX-ID-NUM.                                   07100024
      ******************************************************************07110024
REL120*                                                                 07120024
REL120*    IF (FM110-FED-TAX-ID-NUM = SPACES) AND                       07110027
REL120*        WS-ECRP-NO                                               07120027
REL120*       MOVE 'ZI002' TO WS-SCCF-EDIT-ERROR-CD                     07140024
REL120*       PERFORM B1999-INSERT-ERROR-CODE                           07150024
REL120*       SET FOUND-EITSZI002    TO TRUE                            15478603
REL120*    END-IF                                                       07160024
REL120*    .                                                            07170024
REL120* REL 12.0 CHANGES ENDS                                           11143000
                                                                        07180024
      ******************************************************************07190024
       B1220-EDIT-TYPE-OF-BILL.                                         07200024
      ******************************************************************07210024
                                                                        07220024
074100     IF (FM110-TYPE-BILL = '147') AND WS-ECRP-NO                  07410027
              MOVE 'ZI033' TO WS-SCCF-EDIT-ERROR-CD                     07420024
              PERFORM B1999-INSERT-ERROR-CODE                           07430024
154816        SET FOUND-EITSZI033    TO TRUE                            15481603
           END-IF                                                       07440024
           .                                                            07450024
                                                                        07460024
      ******************************************************************07470024
083819 B1230-EDIT-EMPIRE-PAR-PROV.                                      08381918
083820******************************************************************08382018
083821                                                                  08382118
083822     IF FM105-LOC-PLAN-CD = '780'                                 08382219
083824        IF FM115-CLASS-PROV-CLM = '3' AND FIRST-ENGLEWOOD         08382422
083826           IF FM110-FED-TAX-ID-NUM = '221487173'     OR           08382621
083827              FM110-MED-PROV-NUM   = '310045       '              08382721
083828              MOVE 'N' TO WS-FIRST-ENGLEWOOD-SW                   08382823
083829              DISPLAY ' '                                         08382921
083830              DISPLAY '************ HELEN **************'         08383021
083831              DISPLAY WS-NON-PAR-ENGLEWOOD-MSG                    08383121
083832              DISPLAY '*********************************'         08383221
083833              DISPLAY ' '                                         08383321
083834           END-IF                                                 08383421
083835        END-IF                                                    08383521
154837     END-IF                                                       15483702
154838                                                                  15483802
      *****************************EHIMS*******************************
154839*    IF WS-ECRP-NO OR WS-ECRP-PLUS                                15483902
154840*       MOVE  FM110-FED-TAX-ID-NUM TO WS-TAX-ID-NUM               15484002
154841*       MOVE  FM110-MED-PROV-NUM   TO WS-PROVDR-NUM               15484102
154842*       EVALUATE TRUE                                             15484202
      *****************************EHIMS*******************************
171041*          WHEN WS-NJ-HACKENSACK-TAX-ID AND                       15484302
171041*               FM120-STMT-COV-FROM-DT < '19990301' AND           15484402
171041*               FM105-LOC-PLAN-CD = '780'                         15484502
171041*          WHEN WS-NJ-HACKENSACK-PROVDR AND                       15484602
171041*               FM120-STMT-COV-FROM-DT < '19990301' AND           15484702
171041*               FM105-LOC-PLAN-CD = '780'                         15484802
171041*             CONTINUE                                            15484902
      *****************************EHIMS*******************************
154850*          WHEN WS-NJ-TAX-ID AND FM105-LOC-PLAN-CD = '780'        15485003
154851*****      WHEN WS-NJ-PROVDR AND FM105-LOC-PLAN-CD = '780'        15485103
154852*****      WHEN WS-NJ-PROVDR AND WS-NJ-TAX-ID AND                 15485202
154853*****           FM105-LOC-PLAN-CD = '780'                         15485302
169200*          WHEN WS-CONTIG-CTY-YLN-TAX-ID AND                      16920005
169300*               FM130-SUB-ID-PFX-1-3 (SUBX) = 'YLN'               16930005
154854*          WHEN WS-CONTIG-CTY-TAX-ID                              15485402
154855*****      WHEN WS-CONTIG-CTY-PROVDR                              15485502
154856*****      WHEN WS-CONTIG-CTY-PROVDR AND WS-CONTIG-CTY-TAX-ID     15485602
154858*              SET EMPIRE-PAR-PROV TO TRUE                        15485802
154859*              MOVE 'ZI097' TO WS-SCCF-EDIT-ERROR-CD              15485902
154860*              PERFORM B1999-INSERT-ERROR-CODE                    15486002
154861*       END-EVALUATE                                              15486102
083851*    END-IF                                                       08385121
      *****************************EHIMS*******************************
083852     .                                                            08385221
083853                                                                  08385321
REL114*************************EHIMS************************************
      ******************************************************************07610024
REL114*B1310-EDIT-CERTIFICATION-IND.                                    07620024
      ******************************************************************07630024
REL114*                                                                 07640024
REL114*    MOVE FM115-CLASS-PROV-CLM      TO WS-CLASS-PROV              07650024
REL114*                                                                 07660024
REL114*    IF FM115-CERT-IND = SPACE                                    07670024
REL114*       IF  WS-CERT-1-PROV                                        07680024
REL114*           MOVE 1  TO FM115-CERT-IND                             07690024
REL114*       ELSE                                                      07700024
REL114*           IF  WS-CERT-2-PROV                                    07710024
REL114*               MOVE 2  TO FM115-CERT-IND                         07720024
REL114*           ELSE                                                  07730024
REL114*               IF FM115-CLASS-PROV-CLM = SPACES                  07740024
REL114*                   PERFORM B1312-CHECK-01-10-PRICE-MTD           07750024
REL114*                   IF NOT-FOUND-01-10-PRICE-MTD                  07760024
REL114*                       MOVE 1  TO FM115-CERT-IND                 07770024
REL114*                   ELSE                                          07780024
REL114*                       MOVE 'ZI025' TO WS-SCCF-EDIT-ERROR-CD     07790024
REL114*                       PERFORM B1999-INSERT-ERROR-CODE           07800024
REL114*                   END-IF                                        07810024
REL114*               END-IF                                            07820024
REL114*           END-IF                                                07830024
REL114*       END-IF                                                    07840024
REL114*    END-IF                                                       07850024
REL114*                                                                 07860024
REL114*    MOVE 'N'   TO WS-PRICE-01-10-MTD-SW                          07870024
REL114*    .                                                            07880024
REL114*                                                                 07890024
      ******************************************************************07900024
REL114*B1312-CHECK-01-10-PRICE-MTD.                                     07910024
      ******************************************************************07920024
REL114*                                                                 07930024
REL114*    IF FM150-REC-TYPE NOT = SPACES AND                           07941035
REL101*        FM150-SEQ-NUM NUMERIC      AND                           07942035
REL114*        FM150-SEQ-NUM NOT = ZEROES                               07943035
REL114*       IF FM150-PRC-MTD-CLM = ('01' OR '10')                     07950024
REL114*          MOVE 'Y' TO WS-PRICE-01-10-MTD-SW                      07960024
REL114*       END-IF                                                    07970024
REL114*    END-IF                                                       07980024
REL114*                                                                 07990024
REL114*    PERFORM B1314-65-LINE-PRICE-MTD                              08000024
REL114*       VARYING 65-SUB FROM 1 BY 1                                08010024
REL114*       UNTIL   FOUND-01-10-PRICE-MTD                             08020024
REL114*            OR 65-SUB > 999                                      08040024
REL114*    .                                                            08050024
REL114*                                                                 08060024
      ******************************************************************08070024
REL114*B1314-65-LINE-PRICE-MTD.                                         08080024
      ******************************************************************08090024
REL114*                                                                 08100024
REL114*    IF FM165-REC-TYPE (65-SUB) NOT = SPACES AND                  08101035
REL101*        FM165-SEQ-NUM (65-SUB) NUMERIC      AND                  08102035
REL114*        FM165-SEQ-NUM (65-SUB) NOT = ZEROES                      08103035
REL114*       IF FM165-PRC-MTD-LN (65-SUB) = ('01' OR '10')             08110033
REL114*          MOVE 'Y' TO WS-PRICE-01-10-MTD-SW                      08120033
REL114*       END-IF                                                    08130033
REL114*    END-IF                                                       08131033
REL114*    .                                                            08140024
REL114*                                                                 08150024
      *** CR#148645 START
REL114*************************EHIMS************************************
      ******************************************************************20996404
       B1330-CHK-HIGH-DOLLAR.                                           16160048
      ******************************************************************16170048
           IF FM160-REV-CD(60-SUB) = 0001
              CONTINUE
           ELSE
              IF FM160-SERV-CHRG(60-SUB) > WS-MAX-SERV-CHRG AND
                 FM160-NUM-SERV(60-SUB) = 1
                 MOVE 'Y'     TO WS-HIGH-DOLLAR-SW                      13506700
                 MOVE 'ZI113' TO WS-SCCF-EDIT-ERROR-CD
                 PERFORM B1999-INSERT-ERROR-CODE
REL120           SET FOUND-EITSZI113    TO TRUE                         15481603
              END-IF
           END-IF
           .
      *** CR#148645 END
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
      ******************************************************************20996404
REL135*B1335-SET-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM160-HCPCS-PROC-CD-MOD-1(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-2(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-3(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-4(GY-SUB) = 'GY'
REL135*          SET WS-GY-FOUND TO TRUE                                13506700
REL135*    END-IF
REL135*    .
REL135******************************************************************20996404
REL135*B1335-CHK-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM160-HCPCS-PROC-CD-MOD-1(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-2(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-3(GY-SUB) = 'GY' OR
REL135*       FM160-HCPCS-PROC-CD-MOD-4(GY-SUB) = 'GY'
REL135**      CONTINUE
REL135**   ELSE
REL135*          MOVE 'ZI135' TO WS-SCCF-EDIT-ERROR-CD
REL135*          PERFORM B1999-INSERT-ERROR-CODE
REL135*          SET FOUND-EITSZI135    TO TRUE                         15481603
REL135*    END-IF
REL135*    .

      ******************************************************************08160024
154933*B1350-EDIT-ADMIT-HR.                                             15493309
      ******************************************************************08180024
154935*                                                                 15493509
154936*    MOVE FM120-TYPE-ADM  TO WS-TYPE-ADM                          15493609
154937*                                                                 15493709
154938*    IF FM105-INS-OUTPATIENT                                      15493809
154939*        IF WS-EMERGENCY                                          15493909
154940*         OR WS-URGENT                                            15494009
154941*          OR WS-ADM-INFO-NOT-AVAIL                               15494109
154942*            IF FM120-ADM-HH = ZEROES OR                          15494209
154943*               FM120-ADM-HH NOT NUMERIC                          15494309
154944*               MOVE +12    TO FM120-ADM-HH                       15494409
154945*            END-IF                                               15494509
154946*       END-IF                                                    15494609
154947*    END-IF                                                       15494709
154948*    .                                                            15494809
154949*                                                                 15494909
      ******************************************************************08340024
140137*B1360-EDIT-STMT-COV-DT.                                          08350025
      ******************************************************************08360024
                                                                        08370024
140137*    IF FM105-INS-OUTPATIENT                                      08380024
140137*       IF FM120-STMT-COV-FROM-DT NOT =                           08390025
140137*          FM120-STMT-COV-TO-DT                                   08400025
140137*          PERFORM B1362-DIFF-ANC-SERV-DT                         08410024
140137*       END-IF                                                    08420024
140137*    END-IF                                                       08430024
140137*    .                                                            08440024
                                                                        08450024
      ******************************************************************08460024
140137*B1362-DIFF-ANC-SERV-DT.                                          08470024
      ******************************************************************08480024
                                                                        08490024
140137*    MOVE 'Y' TO WS-FIRST-TIME-SW                                 08500024
140137*    MOVE 'S' TO WS-ANC-SERV-DT-SW                                08510024
140137*    MOVE 'N' TO WS-STMT-COV-DT-SW                                08520025
140137*    MOVE ' ' TO WS-STMT-TO-DT-SW                                 08530024
140137*    MOVE  0  TO WS-SUB                                           08540024
                                                                        08550024
140137*    PERFORM B1364-CHECK-ANC-SERV-DT                              08560024
140137*       VARYING 60-SUB  FROM 1 BY 1                               08570024
140137*       UNTIL   60-SUB  > 999                                     08590033
                                                                        08600024
140137*    IF ALL-ANC-SERV-DT-SAME OR FOUND-STMT-COV-DT-ERROR           08610025
140137*        PERFORM B1368-MOVE-STMT-TO-DATE                          08620024
140137*            UNTIL WS-STMT-TO-DT-SW NOT = SPACES                  08630024
140137*    END-IF                                                       08640024
140137*    .                                                            08650024
                                                                        08660024
      ******************************************************************08670024
140137*B1364-CHECK-ANC-SERV-DT.                                         08680024
      ******************************************************************08690024
                                                                        08700024
140137*    IF FM160-REC-TYPE (60-SUB) NOT = SPACES AND                  08701035
REL101*        FM160-SEQ-NUM (60-SUB) NUMERIC      AND                  08702035
140137*        FM160-SEQ-NUM (60-SUB) NOT = ZEROES                      08703035
140137*       IF FM160-REV-CD (60-SUB) (2:3) = '001'                    09660040
140137*          ADD  1  TO WS-SUB                                      08720033
140137*       ELSE                                                      08730033
140137*           IF FM160-DT-SERV (60-SUB) = SPACES                    08740033
140137*              PERFORM B1365-ANC-DT-SPACES                        08750033
140137*           ELSE                                                  08760033
140137*              PERFORM B1366-ANC-DT-SAME                          08770033
140137*           END-IF                                                08780033
140137*       END-IF                                                    08790033
140137*    END-IF                                                       08792033
140137*    .                                                            08800024
                                                                        08810024
      ******************************************************************08820024
140137*B1365-ANC-DT-SPACES.                                             08830024
      ******************************************************************08840024
                                                                        08850024
140137*    IF FIRST-TIME                                                08860024
140137*       MOVE 'N'    TO WS-FIRST-TIME-SW                           08870024
140137*       MOVE FM120-STMT-COV-FROM-DT                               08880025
140137*                   TO FM160-DT-SERV (60-SUB)                     08890024
140137*                      WS-ANC-SERV-DT                             08900024
140137*       MOVE 'Y'    TO WS-STMT-COV-DT-SW                          08910025
140137*       ADD  1      TO WS-SUB                                     08920024
140137*    ELSE                                                         08930024
140137*       ADD  1      TO WS-SUB                                     08940024
140137*       MOVE FM120-STMT-COV-FROM-DT                               08950025
140137*                   TO FM160-DT-SERV (60-SUB)                     08960024
140137*    END-IF                                                       08970024
140137*    .                                                            08980024
                                                                        08990024
      ******************************************************************09000024
140137*B1366-ANC-DT-SAME.                                               09010024
      ******************************************************************09020024
                                                                        09030024
140137*    IF FIRST-TIME                                                09040024
140137*       MOVE 'N'    TO WS-FIRST-TIME-SW                           09050024
140137*       MOVE FM160-DT-SERV (60-SUB) TO WS-ANC-SERV-DT             09060024
140137*       ADD   1     TO WS-SUB                                     09070024
140137*    ELSE                                                         09080024
140137*       IF FM160-DT-SERV (60-SUB) NOT = WS-ANC-SERV-DT            09090024
140137*          ADD   1     TO WS-SUB                                  09100024
140137*          MOVE 'D'    TO WS-ANC-SERV-DT-SW                       09110024
140137*       ELSE                                                      09120024
140137*          ADD   1     TO WS-SUB                                  09130024
140137*       END-IF                                                    09140024
140137*    END-IF                                                       09150024
140137*    .                                                            09160024
                                                                        09170024
      ******************************************************************09180024
140137*B1368-MOVE-STMT-TO-DATE.                                         09190024
      ******************************************************************09200024
                                                                        09210024
140137*    IF FM160-REV-CD (WS-SUB) (2:3) = '001'                       10180040
140137*       SUBTRACT +1 FROM WS-SUB                                   09230024
140137*       IF WS-SUB = 0                                             09240024
140137*          MOVE 'N' TO WS-STMT-TO-DT-SW                           09250024
140137*       ELSE                                                      09260024
140137*          IF FM160-REV-CD (WS-SUB) (2:3) = '001'                 10230040
140137*             CONTINUE                                            09280024
140137*          ELSE                                                   09290024
140137*             MOVE FM120-STMT-COV-TO-DT                           09300025
140137*              TO FM160-DT-SERV (WS-SUB)                          09310024
140137*             MOVE 'Y' TO WS-STMT-TO-DT-SW                        09320024
140137*         END-IF                                                  09330024
140137*       END-IF                                                    09340024
140137*    ELSE                                                         09350024
140137*       MOVE FM120-STMT-COV-TO-DT                                 09360025
140137*        TO FM160-DT-SERV (WS-SUB)                                09370024
140137*       MOVE 'Y' TO WS-STMT-TO-DT-SW                              09380024
140137*    END-IF                                                       09390024
140137*    .                                                            09400024
                                                                        09410024
      ******************************************************************09420024
REL120*B1450-LEASED-NET.                                                13484906
      ******************************************************************10590024
                                                                        10600024
REL120*    SET FOUND-LEASED-NET TO TRUE                                 13485206
REL120*    MOVE 'ZI104' TO WS-SCCF-EDIT-ERROR-CD                        13485306
REL120*    PERFORM B1999-INSERT-ERROR-CODE                              13485406
           .                                                            11290024
                                                                        11300024
      ******************************************************************11310024
REL120*B1500-EDIT-CS90-DATE-CHECK.                                      11320024
      ******************************************************************11330024
                                                                        11340024
      *    IF FM105-INS-INPATIENT                                       11350024
      *       PERFORM B1502-EDIT-ADM-RECEIPT-DT                         11360024
      *    END-IF                                                       11370024
                                                                        11380024
      *    MOVE 'N' TO WS-ANC-RECEIPT-DT-SW                             11390024
                                                                        11400024
      *    PERFORM B1504-EDIT-ANC-RECEIPT-DT                            11410024
      *       VARYING 60-SUB  FROM 1 BY 1                               11420024
      *        UNTIL  FOUND-ANC-RECEIPT-DT-ERROR                        11430024
      *            OR 60-SUB  > 999                                     11450024
                                                                        11460024
REL120*    PERFORM B1506-EDIT-PAT-DOB-STMT-FRM-DT                       11470025
           .                                                            11480024
                                                                        11490024
      ******************************************************************11500024
      *B1502-EDIT-ADM-RECEIPT-DT.                                       11510024
      ******************************************************************11520024
                                                                        11530024
      *    IF FM120-ADM-DT > FM105-LOC-PLAN-RCPT-DT                     11540024
      *       MOVE 'ZI022' TO WS-SCCF-EDIT-ERROR-CD                     11550024
      *       PERFORM B1999-INSERT-ERROR-CODE                           11560024
      *    END-IF                                                       11570024
      *    .                                                            11580024
                                                                        11590024
      ******************************************************************11600024
      *B1504-EDIT-ANC-RECEIPT-DT.                                       11610024
      ******************************************************************11620024
                                                                        11630024
      *    IF FM160-REC-TYPE (60-SUB) NOT = SPACES AND                  11631035
REL101*        FM160-SEQ-NUM (60-SUB) NUMERIC      AND                  11632035
      *        FM160-SEQ-NUM (60-SUB) NOT = ZEROES                      11633035
      *       IF FM160-REV-CD (60-SUB) (2:3) = '001'                    12790040
      *          CONTINUE                                               11650033
      *       ELSE                                                      11660033
      *          IF FM160-DT-SERV (60-SUB)                              11670033
      *                         > FM105-LOC-PLAN-RCPT-DT                11680033
      *             MOVE 'Y'    TO WS-ANC-RECEIPT-DT-SW                 11690033
      *             MOVE 'ZI023' TO WS-SCCF-EDIT-ERROR-CD               11700033
      *             PERFORM B1999-INSERT-ERROR-CODE                     11710033
      *          END-IF                                                 11720033
      *       END-IF                                                    11730033
      *    END-IF                                                       11732033
      *    .                                                            11740024
                                                                        11750024
REL120* ITS 12.0 CHANGES BEGIN                                          11750024
      ******************************************************************11760024
REL120*B1506-EDIT-PAT-DOB-STMT-FRM-DT.                                  11770025
      ******************************************************************11780024
REL120*                                                                 11790024
REL120*    IF FM120-PAT-BRT-DT > FM120-STMT-COV-FROM-DT                 11800024
REL120*       MOVE 'ZI024' TO WS-SCCF-EDIT-ERROR-CD                     11810024
REL120*       PERFORM B1999-INSERT-ERROR-CODE                           11820024
REL120*    END-IF                                                       11830024
REL120*    .                                                            11840024
REL120* ITS 12.0 CHANGES BEGIN                                          11750024
                                                                        11850024
      ******************************************************************11860024
       B1600-EDIT-PRIV-ROOM-CLAIM.                                      12650024
      ******************************************************************12660024
                                                                        12670024
                                                                        12670024
           IF FM150-REC-TYPE NOT = SPACES AND                           12681035
REL101*        FM150-SEQ-NUM NUMERIC      AND                           12682035
               FM150-SEQ-NUM NOT = ZEROES                               12683035
              IF (FM150-PRIV-RM-RATE IS NUMERIC AND                     12690024
                  FM150-PRIV-RM-RATE > ZEROES)                          12700024
                                    AND                                 12710024
                 (FM150-AVG-SEMI-PRIV-RM-RATE IS NUMERIC AND            12720024
                  FM150-AVG-SEMI-PRIV-RM-RATE > ZEROES)                 12730024
                 MOVE 'Y' TO WS-RATE-SW                                 12740024
              ELSE                                                      12750024
                 MOVE 'N' TO WS-RATE-SW                                 12760024
              END-IF                                                    12770024
135033                                                                  13503300
135035        IF (FM150-AVG-SEMI-PRIV-RM-RATE IS NUMERIC AND            13503500
135036            FM150-AVG-SEMI-PRIV-RM-RATE = ZEROES   AND            13503600
135037            FM105-INS-INPATIENT)                                  13503700
135038           MOVE 'Y' TO WS-RATE-SW-NEW                             13503800
135039        ELSE                                                      13503900
135040           MOVE 'N' TO WS-RATE-SW-NEW                             13504000
135041        END-IF                                                    13504100
           END-IF                                                       12780024
                                                                        12790024
           PERFORM B1602-PRIV-ROOM-RATE                                 12800024
              VARYING 60-SUB FROM 1 BY 1                                12810024
               UNTIL  FOUND-PRIV-ROOM-RATE-ERROR                        12820024
                   OR 60-SUB > 999                                      12840024
           .                                                            12850024
                                                                        12860024
      ******************************************************************12870024
       B1602-PRIV-ROOM-RATE.                                            12880024
      ******************************************************************12890024
                                                                        12900024
            IF WS-ECRP-NO                                               12840027
128500        IF FM160-REC-TYPE (60-SUB) NOT = SPACES AND               12850027
REL101*           FM160-SEQ-NUM (60-SUB) NUMERIC      AND               12860027
128700            FM160-SEQ-NUM (60-SUB) NOT = ZEROES                   12870027
128800           MOVE FM160-REV-CD (60-SUB) TO WS-REV-CD                12880027
128900           IF WS-14X AND BOTH-RATES-FURNISHED                     12890027
REL120              MOVE ZEROES TO FM150-PRIV-RM-RATE
REL120*             MOVE 'Y'    TO WS-PRIV-ROOM-RATE-SW                 12900027
REL120*             MOVE 'ZI028' TO WS-SCCF-EDIT-ERROR-CD               12910027
REL120*             PERFORM B1999-INSERT-ERROR-CODE                     12920027
129300           END-IF                                                 12930027
135065                                                                  13506501
135066           IF ( WS-14X OR WS-10X ) AND SEMI-PRIV-RM-RATE-0        13506600
                    MOVE FM160-ACCOM-RATE (60-SUB)
                                        TO FM150-AVG-SEMI-PRIV-RM-RATE
REL120*             MOVE 'Y'     TO WS-PRIV-ROOM-RATE-SW                13506700
REL120*             MOVE 'ZI101' TO WS-SCCF-EDIT-ERROR-CD               13506800
REL120*             PERFORM B1999-INSERT-ERROR-CODE                     13506900
135070           END-IF                                                 13507000
REL120*                                                                 13507101
              END-IF                                                    12970033
           END-IF                                                       12972033
           .                                                            12980024
                                                                        12990024
      ***CR155472 START
      ******************************************************************13310024
       B1650-EDIT-INPAT-DME.
      ******************************************************************
           IF FM165-REC-TYPE (65-SUB) NOT = SPACES AND
              FM165-SEQ-NUM (65-SUB)NOT = ZEROES
              MOVE FM165-SF-MSG-CD-LN (65-SUB 1) TO WS-DME-MSG-CD-1
              MOVE FM165-SF-MSG-CD-LN (65-SUB 2) TO WS-DME-MSG-CD-2
              MOVE FM165-SF-MSG-CD-LN (65-SUB 3) TO WS-DME-MSG-CD-3
              MOVE FM165-SF-MSG-CD-LN (65-SUB 4) TO WS-DME-MSG-CD-4
              MOVE FM165-SF-MSG-CD-LN (65-SUB 5) TO WS-DME-MSG-CD-5
             IF VALID-DME-MSG-CD-1 OR
                VALID-DME-MSG-CD-2 OR
                VALID-DME-MSG-CD-3 OR
                VALID-DME-MSG-CD-4 OR
                VALID-DME-MSG-CD-5
                   SET FOUND-INPAT-DME-MSG  TO TRUE
                   MOVE 'ZI114' TO WS-SCCF-EDIT-ERROR-CD
                   PERFORM B1999-INSERT-ERROR-CODE
             END-IF
           END-IF
           .
      ******************************************************************
      ***CR155472 END
      ******************************************************************13310024
      *B1700-EDIT-ACCOM-CODES.                                          13320024
      ******************************************************************13330024
                                                                        13340024
133600*    IF FM105-INS-INPATIENT AND WS-ECRP-NO                        13360027
      *       MOVE 0 TO WS-ACCOM-CTR                                    13360024
      *       PERFORM B1702-CHECK-ACCOM-CODES                           13370024
      *        VARYING 60-SUB  FROM 1 BY 1                              13380024
      *          UNTIL 60-SUB  > 999                                    13400033
      *       IF WS-ACCOM-CTR > 5                                       13410024
      *          MOVE 'ZI005' TO WS-SCCF-EDIT-ERROR-CD                  13420024
      *          PERFORM B1999-INSERT-ERROR-CODE                        13430024
      *       END-IF                                                    13440024
      *    END-IF                                                       13450024
      *    .                                                            13460024
      *                                                                 13470024
      ******************************************************************13480024
      *B1702-CHECK-ACCOM-CODES.                                         13490024
      ******************************************************************13500024
      *                                                                 13510024
      *    IF FM160-REC-TYPE (60-SUB) NOT = SPACES AND                  13511035
      *        FM160-SEQ-NUM (60-SUB) NUMERIC      AND                  13512035
      *        FM160-SEQ-NUM (60-SUB) NOT = ZEROES                      13513035
      *       MOVE FM160-REV-CD (60-SUB) TO WS-REV-CD                   13520033
      *       IF WS-ACCOM-CD                                            13540033
      *          ADD 1 TO WS-ACCOM-CTR                                  13550033
      *       END-IF                                                    13560033
      *    END-IF                                                       13562033
      *    .                                                            13570024
      *                                                                 13580024
      ******************************************************************16150048
196328*B1820-EDIT-FLORIDA-FOR-PPO.                                      19632800
161403******************************************************************16140300
196330*                                                                 19633000
208300*    IF FM105-LOC-PLAN-CD = '090' AND                             20830003
208310*       WS-ECRP-NO                                                20831003
208311*       MOVE FM130-SUB-ID-PFX-1-3 (SUBX) TO WS-SUB-ID-PFX-1-3     20831103
208312*       MOVE FM110-BCBS-PROV-NUM         TO WS-BCBS-PROV-NUM      20831202
208313*       MOVE FM110-FED-TAX-ID-NUM        TO WS-FED-TAX-ID-NUM     20831303
208314*       EVALUATE TRUE                                             20831403
208315*         WHEN WS-FL-NATL-ACCT  AND                               20831503
208316*              WS-PPO-FL-PROV                                     20831603
208317*         WHEN WS-MERRILL-LYNCH AND                               20831703
208318*              WS-PPO-FL-TAX-ID                                   20831803
208319*           EVALUATE TRUE                                         20831903
208320*             WHEN FM120-STMT-COV-FROM-DT-CC = '19' AND           20832003
208330*                  FM120-STMT-COV-FROM-DT-YY = '99' AND           20833003
209500*                 (FM120-STMT-COV-FROM-DT-MM = '10' OR            20950003
209510*                  FM120-STMT-COV-FROM-DT-MM = '11' OR            20951003
209520*                  FM120-STMT-COV-FROM-DT-MM = '12')              20952003
209521*             WHEN FM120-STMT-COV-TO-DT-CC   = '19' AND           20952103
209522*                  FM120-STMT-COV-TO-DT-YY   = '99' AND           20952203
209900*                 (FM120-STMT-COV-TO-DT-MM   = '10' OR            20990003
209910*                  FM120-STMT-COV-TO-DT-MM   = '11' OR            20991003
209920*                  FM120-STMT-COV-TO-DT-MM   = '12')              20992003
209921*               MOVE '5' TO FM115-CLASS-PROV-CLM                  20992103
209922*           END-EVALUATE                                          20992203
209923*       END-EVALUATE                                              20992303
209924*    END-IF                                                       20992402
209925*    .                                                            20992500
209926*                                                                 20992600
209927***CR#98645 ADDED BEGIN                                           20992701
209964******************************************************************20996404
REL114*B1850-EDIT-ALL-PPO.                                              16160048
      ******************************************************************16170048
REL114*                                                                 16180048
REL114*    IF WS-ECRP-NO                                                15520027
REL114*       IF (WS-BLUECARD-PPO-PFX-1       OR                        15530027
REL114*           WS-BLUECARD-PPO-PFX-2       OR                        15540027
REL114*           WS-BLUECARD-PPO-PFX-3       OR                        15170011
REL114*           WS-BLCD-COV-DATE-Y)         AND                       15180011
REL114*          (FM115-CLASS-PROV-CLM = '5') AND                       15560027
REL114*          (FM115-CERT-IND = '2')                                 15570027
REL114*           MOVE 'ZI096' TO WS-SCCF-EDIT-ERROR-CD                 15210009
REL114*           PERFORM B1999-INSERT-ERROR-CODE                       15220009
REL114*       END-IF                                                    15600027
REL114*    END-IF                                                       13770024
REL114*    .                                                            13780024
REL114*                                                                 13790024
      *** CR138938 START
209964******************************************************************20996404
       B1860-EDIT-NEG-CLM.                                              16160048
      ******************************************************************16170048
           IF FM150-REC-TYPE NOT = SPACES AND
REL101*       FM150-SEQ-NUM NUMERIC       AND
              FM150-SEQ-NUM NOT = ZEROES
                 PERFORM B1865-CHECK-PRC-METHOD
                 IF WS-CLAIM-PRICING IS NUMERIC AND
                    WS-CLAIM-PRICING < ZEROES
                       SET NEG-PRC          TO TRUE
                       MOVE 'ZI111' TO WS-SCCF-EDIT-ERROR-CD
                       PERFORM B1999-INSERT-ERROR-CODE
                 END-IF
           END-IF
           .
209964******************************************************************20996404
       B1865-CHECK-PRC-METHOD.                                          16160048
      ******************************************************************16170048
           EVALUATE FM150-PRC-MTD-CLM
               WHEN '20'
                    MOVE  FM150-PER-DIEM-RATE  TO WS-CLAIM-PRICING
               WHEN '10'
                    MOVE  FM150-PCT-FCTR-CLM   TO WS-CLAIM-PRICING
               WHEN '30'
                    MOVE  FM150-CASE-ALLW-AMT  TO WS-CLAIM-PRICING
               WHEN '01'
                    PERFORM VARYING 60-SUB FROM 1 BY 1
                    UNTIL 60-SUB > 999
                      IF FM160-REV-CD(60-SUB) = 0001
                         MOVE  FM160-SERV-CHRG(60-SUB)
                                               TO WS-CLAIM-PRICING
                      ELSE
                         CONTINUE
                      END-IF
                    END-PERFORM
           END-EVALUATE
           .
      *** CR138938 END
225700***********U602************************************************** 22570015
      ******************************************************************13800024
225900 B1900-EDIT-U602.                                                 22590015
226000******************************************************************22600012
226100                                                                  22610015
226200     IF FM105-INS-INPATIENT                                       22620015
226300        PERFORM B1910-II-U602-CLAIM                               22630015
226400                                                                  22640015
226500        IF U602-ZERO-PRICED-CLAIM                                 22650015
226600           PERFORM B1920-II-U602-LINE                             22660015
226700                   VARYING 65-SUB FROM 1 BY 1                     22670015
226800                   UNTIL 65-SUB > 999 OR                          22680015
226900                         U602-NOT-A-ZERO-PRICED-CLAIM             22690015
227000        END-IF                                                    22700015
227100     END-IF                                                       22710015
227200                                                                  22720015
227300     IF FM105-INS-OUTPATIENT                                      22730015
227400        PERFORM B1940-IP-U602-CLAIM                               22740015
227500                                                                  22750015
227600        IF U602-ZERO-PRICED-CLAIM                                 22760015
227700           PERFORM B1950-IP-U602-LINE                             22770015
227800                   VARYING 65-SUB FROM 1 BY 1                     22780015
227900                   UNTIL 65-SUB > 999 OR                          22790015
228000                         U602-NOT-A-ZERO-PRICED-CLAIM             22800015
228100        END-IF                                                    22810015
228200     END-IF                                                       22820015
228300     .                                                            22830015
228400                                                                  22840015
228500******************************************************************22850016
228600 B1910-II-U602-CLAIM.                                             22860015
228700******************************************************************22870016
228800     IF FM150-SF-MSG-CD-CLM (1) = 'U602' OR                       22880019
228900        FM150-SF-MSG-CD-CLM (2) = 'U602' OR                       22890019
229000        FM150-SF-MSG-CD-CLM (3) = 'U602' OR                       22900019
229100        FM150-SF-MSG-CD-CLM (4) = 'U602' OR                       22910019
229200        FM150-SF-MSG-CD-CLM (5) = 'U602'                          22920019
229300        MOVE 'Y' TO WS-U602-CLAIM-LVL-IND                         22930020
229400        PERFORM B1911-CHECK-ZERO-PRICED-CLM                       22940015
229500     END-IF                                                       22950015
229600     .                                                            22960015
229700                                                                  22970015
229800******************************************************************22980016
229900 B1911-CHECK-ZERO-PRICED-CLM.                                     22990016
230000******************************************************************23000016
230100     IF FM150-REC-TYPE NOT = SPACES AND                           23010015
REL101*       FM150-SEQ-NUM NUMERIC       AND                           23020015
230300        FM150-SEQ-NUM NOT = ZEROES  AND                           23030015
230400        FM150-PRC-MTD-CLM NOT = SPACES                            23040015
230500        EVALUATE FM150-PRC-MTD-CLM                                23050015
230600           WHEN '01'                                              23060015
230700              SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE            23070015
230800           WHEN '10'                                              23080015
230900              IF FM150-PCT-FCTR-CLM IS NUMERIC                    23090015
231000                 IF FM150-PCT-FCTR-CLM NOT = 00000                23100015
231100                    SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE      23110015
231200                 END-IF                                           23120015
231300              END-IF                                              23130015
231400           WHEN '20'                                              23140015
231500              IF FM150-PER-DIEM-RATE IS NUMERIC                   23150015
231600                 IF FM150-PER-DIEM-RATE > ZEROES                  23160015
231700                    SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE      23170015
231800                 END-IF                                           23180015
231900              END-IF                                              23190015
232000           WHEN '30'                                              23200015
232100              IF FM150-CASE-ALLW-AMT IS NUMERIC                   23210015
232200                 IF FM150-CASE-ALLW-AMT > ZEROES                  23220015
232300                    SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE      23230015
232400                 END-IF                                           23240015
232500              END-IF                                              23250015
232600           WHEN '33'                                              23260015
232700              IF FM150-CASE-ALLW-AMT IS NUMERIC                   23270015
232800                 IF FM150-CASE-ALLW-AMT > ZEROES                  23280015
232900                    IF FM150-PCT-FCTR-CLM IS NUMERIC              23290015
233000                       IF FM150-PCT-FCTR-CLM NOT = 00000          23300015
233100                          SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE23310015
233200                       END-IF                                     23320015
233300                    END-IF                                        23330015
233400                 END-IF                                           23340015
233500              END-IF                                              23350015
233600        END-EVALUATE                                              23360015
233700     END-IF                                                       23370015
233800     .                                                            23380015
233900                                                                  23390015
234000******************************************************************23400016
234100 B1920-II-U602-LINE.                                              23410015
234200******************************************************************23420016
234300     IF FM160-REC-TYPE (65-SUB) NOT = SPACES AND                  23430015
REL101*       FM160-SEQ-NUM (65-SUB) NUMERIC AND                        23440015
234500        FM160-SEQ-NUM (65-SUB) NOT = ZEROES AND                   23450015
234600        FM160-REV-CD (65-SUB) NUMERIC AND                         23460015
234700        FM160-REV-CD (65-SUB) NOT = 0001                          23470015
234800        IF FM165-REC-TYPE (65-SUB) NOT = SPACES AND               23480015
REL101*          FM165-SEQ-NUM (65-SUB) NUMERIC AND                     23490015
235000           FM165-SEQ-NUM (65-SUB) NOT = ZEROES                    23500015
235100           IF FM165-SF-MSG-CD-LN (65-SUB 1) = 'U602' OR           23510015
235200              FM165-SF-MSG-CD-LN (65-SUB 2) = 'U602' OR           23520015
235300              FM165-SF-MSG-CD-LN (65-SUB 3) = 'U602' OR           23530015
235400              FM165-SF-MSG-CD-LN (65-SUB 4) = 'U602' OR           23540015
235500              FM165-SF-MSG-CD-LN (65-SUB 5) = 'U602' OR           23550020
235600              U602-CLAIM-LVL                                      23560020
235700              PERFORM B1921-CHECK-ZERO-PRICED-LN                  23570015
235800              IF U602-ZERO-PRICED-CLAIM                           23580020
235900                 PERFORM B1930-II-U602-PRIVATE-RM                 23590020
236000              END-IF                                              23600020
236100           END-IF                                                 23610015
236200        END-IF                                                    23620015
236300     END-IF                                                       23630015
236400     .                                                            23640015
236500                                                                  23650015
236600******************************************************************23660016
236700 B1921-CHECK-ZERO-PRICED-LN.                                      23670015
236800******************************************************************23680016
236900     EVALUATE FM165-PRC-MTD-LN (65-SUB)                           23690015
237000        WHEN '01'                                                 23700015
237100           SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE               23710015
237200        WHEN '10'                                                 23720015
237300           IF FM165-PCT-FCTR-LN(65-SUB) IS NUMERIC                23730015
237400              IF FM165-PCT-FCTR-LN(65-SUB) NOT = 00000            23740015
237500                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         23750015
237600              END-IF                                              23760015
237700           END-IF                                                 23770015
237800        WHEN '14'                                                 23780015
237900        WHEN '15'                                                 23790018
238000           IF FM165-PCT-FCTR-LN(65-SUB) IS NUMERIC                23800015
238100              IF FM165-PCT-FCTR-LN(65-SUB) NOT = 00000            23810015
238200                 IF FM165-LOC-RATE (65-SUB) IS NUMERIC            23820015
238300                    IF FM165-LOC-RATE(65-SUB) > ZEROES            23830015
238400                       SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE   23840015
238500                    END-IF                                        23850015
238600                 END-IF                                           23860015
238700              END-IF                                              23870015
238800           END-IF                                                 23880015
238900        WHEN '20'                                                 23890015
239000           IF FM165-LOC-RATE (65-SUB) IS NUMERIC                  23900015
239100              IF FM165-LOC-RATE(65-SUB) > ZEROES                  23910015
239200                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         23920015
239300              END-IF                                              23930015
239400           END-IF                                                 23940015
239500        WHEN '40'                                                 23950015
239600        WHEN '41'                                                 23960018
239700           IF FM165-LOC-RATE (65-SUB) IS NUMERIC                  23970015
239800              IF FM165-LOC-RATE(65-SUB) > ZEROES                  23980015
239900                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         23990015
240000              END-IF                                              24000015
240100           END-IF                                                 24010015
240200     END-EVALUATE                                                 24020015
240300     .                                                            24030015
240400                                                                  24040015
240500******************************************************************24050016
240600 B1930-II-U602-PRIVATE-RM.                                        24060015
240700******************************************************************24070016
240800     IF FM160-REC-TYPE (65-SUB) NOT = SPACES AND                  24080020
REL101*       FM160-SEQ-NUM (65-SUB) NUMERIC      AND                   24090020
241000        FM160-SEQ-NUM (65-SUB) NOT = ZEROES                       24100020
241100        IF FM160-REV-CD (65-SUB) (2:2) = '11' OR '14'             24110020
241200           MOVE 0 TO WS-AVG-SEMI-PRIV-RM-RATE                     24120015
241300           IF FM160-NUM-SERV (65-SUB) IS NUMERIC                  24130020
241400              IF FM160-SERV-CHRG (65-SUB) IS NUMERIC              24140020
241500                 COMPUTE WS-AVG-SEMI-PRIV-RM-RATE =               24150015
241600                         FM160-SERV-CHRG (65-SUB) /               24160020
241700                         FM160-NUM-SERV (65-SUB)                  24170020
241800              END-IF                                              24180015
241900           END-IF                                                 24190015
242000           PERFORM B1931-PRIVATE-RM-CLM                           24200015
242100        END-IF                                                    24210015
242200     END-IF                                                       24220015
242300     .                                                            24230015
242400                                                                  24240015
242500******************************************************************24250016
242600 B1931-PRIVATE-RM-CLM.                                            24260015
242700******************************************************************24270016
242800     IF FM150-REC-TYPE NOT = SPACES AND                           24280015
REL101*       FM150-SEQ-NUM NUMERIC       AND                           24290015
243000        FM150-SEQ-NUM NOT = ZEROES                                24300015
243100        IF FM150-PRIV-RM-RATE IS NUMERIC                          24310015
243200           IF FM150-PRIV-RM-RATE > 0                              24320015
243300              SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE            24330015
243400           END-IF                                                 24340015
243500        END-IF                                                    24350015
243600                                                                  24360015
243700        IF FM150-AVG-SEMI-PRIV-RM-RATE > WS-AVG-SEMI-PRIV-RM-RATE 24370015
243800           SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE               24380015
243900        END-IF                                                    24390015
244000     END-IF                                                       24400015
244100     .                                                            24410015
244200                                                                  24420016
244300******************************************************************24430016
244400 B1940-IP-U602-CLAIM.                                             24440015
244500******************************************************************24450016
244600     IF FM150-SF-MSG-CD-CLM (1) = 'U602' OR                       24460020
244700        FM150-SF-MSG-CD-CLM (2) = 'U602' OR                       24470020
244800        FM150-SF-MSG-CD-CLM (3) = 'U602' OR                       24480020
244900        FM150-SF-MSG-CD-CLM (4) = 'U602' OR                       24490020
245000        FM150-SF-MSG-CD-CLM (5) = 'U602'                          24500020
245100        MOVE 'Y' TO WS-U602-CLAIM-LVL-IND                         24510020
245200        PERFORM B1941-CHECK-ZERO-PRICED-CLM                       24520020
245300     END-IF                                                       24530015
245400     .                                                            24540015
245500                                                                  24550015
245600******************************************************************24560016
245700 B1941-CHECK-ZERO-PRICED-CLM.                                     24570016
245800******************************************************************24580016
245900     IF FM150-REC-TYPE NOT = SPACES AND                           24590015
REL101*       FM150-SEQ-NUM NUMERIC       AND                           24600015
246100        FM150-SEQ-NUM NOT = ZEROES  AND                           24610015
246200        FM150-PRC-MTD-CLM NOT = SPACES                            24620015
246300        EVALUATE FM150-PRC-MTD-CLM                                24630015
246400           WHEN '01'                                              24640015
246500              SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE            24650015
246600           WHEN '10'                                              24660015
246700              IF FM150-PCT-FCTR-CLM IS NUMERIC                    24670015
246800                 IF FM150-PCT-FCTR-CLM NOT = 00000                24680015
246900                    SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE      24690015
247000                 END-IF                                           24700015
247100              END-IF                                              24710015
247200        END-EVALUATE                                              24720015
247300     END-IF                                                       24730015
247400     .                                                            24740015
247500                                                                  24750015
247600******************************************************************24760016
247700 B1950-IP-U602-LINE.                                              24770015
247800******************************************************************24780016
247900     IF FM160-REC-TYPE (65-SUB) NOT = SPACES AND                  24790022
REL101*       FM160-SEQ-NUM (65-SUB) NUMERIC AND                        24800022
248100        FM160-SEQ-NUM (65-SUB) NOT = ZEROES AND                   24810022
248200        FM160-REV-CD (65-SUB) NUMERIC AND                         24820022
248300        FM160-REV-CD (65-SUB) NOT = 0001                          24830022
248400        IF FM165-REC-TYPE (65-SUB) NOT = SPACES AND               24840022
REL101*          FM165-SEQ-NUM (65-SUB) NUMERIC AND                     24850022
248600           FM165-SEQ-NUM (65-SUB) NOT = ZEROES                    24860022
248700           IF FM165-SF-MSG-CD-LN (65-SUB 1) = 'U602' OR           24870022
248800              FM165-SF-MSG-CD-LN (65-SUB 2) = 'U602' OR           24880022
248900              FM165-SF-MSG-CD-LN (65-SUB 3) = 'U602' OR           24890022
249000              FM165-SF-MSG-CD-LN (65-SUB 4) = 'U602' OR           24900022
249100              FM165-SF-MSG-CD-LN (65-SUB 5) = 'U602' OR           24910022
249200              U602-CLAIM-LVL                                      24920020
249300              PERFORM B1951-CHECK-ZERO-PRICED-LN                  24930020
249400           END-IF                                                 24940020
249500        END-IF                                                    24950020
249600     END-IF                                                       24960020
249700     .                                                            24970020
249800                                                                  24980015
249900******************************************************************24990016
250000 B1951-CHECK-ZERO-PRICED-LN.                                      25000015
250100******************************************************************25010016
250200     EVALUATE FM165-PRC-MTD-LN (65-SUB)                           25020015
250300        WHEN '01'                                                 25030015
250400           SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE               25040015
250500        WHEN '10'                                                 25050015
250600           IF FM165-PCT-FCTR-LN(65-SUB) IS NUMERIC                25060015
250700              IF FM165-PCT-FCTR-LN(65-SUB) NOT = 00000            25070015
250800                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         25080015
250900              END-IF                                              25090015
251000           END-IF                                                 25100015
251100        WHEN '14'                                                 25110015
251200        WHEN '15'                                                 25120018
251300           IF FM165-PCT-FCTR-LN(65-SUB) IS NUMERIC                25130015
251400              IF FM165-PCT-FCTR-LN(65-SUB) NOT = 00000            25140015
251500                 IF FM165-LOC-RATE (65-SUB) IS NUMERIC            25150015
251600                    IF FM165-LOC-RATE(65-SUB) > ZEROES            25160015
251700                       SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE   25170015
251800                    END-IF                                        25180015
251900                 END-IF                                           25190015
252000              END-IF                                              25200015
252100           END-IF                                                 25210015
252200        WHEN '40'                                                 25220015
252300        WHEN '41'                                                 25230018
252400        WHEN '42'                                                 25240018
252500        WHEN '43'                                                 25250018
252600           IF FM165-LOC-RATE (65-SUB) IS NUMERIC                  25260015
252700              IF FM165-LOC-RATE(65-SUB) > ZEROES                  25270015
252800                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         25280015
252900              END-IF                                              25290015
253000           END-IF                                                 25300015
253100     END-EVALUATE                                                 25310015
253200     .                                                            25320016
253300                                                                  25330016
253400***********U602************************************************** 25340015
253838*****REL9.0 START************************************************ 25383800
REL120*B1960-EDIT-M1015.                                                25383900
253840***************************************************************** 25384000
REL120*    IF FM150-REC-TYPE  NOT = SPACES AND                          25385000
REL101*       FM150-SEQ-NUM   NUMERIC AND                               25386000
REL120*       FM150-SEQ-NUM   NOT = ZEROES                              25387000
REL120*       IF FM150-SF-MSG-CD-CLM (1) = '1015' OR                    25388000
REL120*          FM150-SF-MSG-CD-CLM (2) = '1015' OR                    25389000
REL120*          FM150-SF-MSG-CD-CLM (3) = '1015' OR                    25390000
REL120*          FM150-SF-MSG-CD-CLM (4) = '1015' OR                    25391000
REL120*          FM150-SF-MSG-CD-CLM (5) = '1015'                       25392000
REL120*          MOVE 'Y' TO WS-1015-BUNDLED-CLAIM                      25393000
REL120*       END-IF                                                    25394000
REL120*    END-IF                                                       25395000
REL120*    .                                                            25396000
253970*****REL9.0 END***************************************************25397000

      ***CR136539 START
      ******************************************************************
       B1970-EDIT-NEG-PRC.
      ******************************************************************
           IF FM165-REC-TYPE (65-SUB) NOT = SPACES AND
REL101*       FM165-SEQ-NUM (65-SUB) NUMERIC      AND
              FM165-SEQ-NUM (65-SUB)NOT = ZEROES
             IF (FM165-LOC-RATE (65-SUB) IS NUMERIC AND
                 FM165-LOC-RATE (65-SUB) < ZEROES)
                   SET NEG-PRC          TO TRUE
                   MOVE 'ZI111' TO WS-SCCF-EDIT-ERROR-CD
                   PERFORM B1999-INSERT-ERROR-CODE
             END-IF
           END-IF
      ******************************************************************
      ***CR136539 END
           .
      ***CR137498 START
253500******************************************************************25350015
       B1980-REMV-ILG-SYB.
      ******************************************************************
             INSPECT FM130-SUB-NAME(ILG-SUB) REPLACING ALL '¬' BY ' '
           IF FM120-REC-TYPE NOT = SPACES AND
REL101*        FM120-SEQ-NUM NUMERIC      AND
               FM120-SEQ-NUM NOT = ZEROES
                 INSPECT FM120-PAT-NAME REPLACING ALL '¬' BY  ' '
      ******************************************************************
      ***CR137498 END
           .
253500******************************************************************25350015
       B1999-INSERT-ERROR-CODE.                                         13810024
      ******************************************************************13820024
                                                                        13830024
           IF ERR-SLOT < 6                                              13840025
              MOVE WS-SCCF-EDIT-ERROR-CD TO                             13850024
                   FM105-ERR-CD (ERR-SLOT)                              13860025
              ADD 1 TO ERR-SLOT                                         13870025
           ELSE                                                         13880024
              IF ERR-SLOT > 5                                           13890025
                 MOVE 'ZI999' TO FM105-ERR-CD (5)                       13900024
                 ADD 1 TO ERR-SLOT                                      13910025
REL120           SET FOUND-EITSZI999    TO TRUE                         15481603
              END-IF                                                    13920024
           END-IF                                                       13930024
                                                                        13940024
           MOVE SPACES TO WS-SCCF-EDIT-ERROR-CD                         13950024
           .                                                            13960024
                                                                        13970024
      ******************************************************************13980024
       B2000-EITSXXXXX-CHK.                                             13990024
      ******************************************************************14000024
                                                                        14010024
158600        EVALUATE TRUE                                             15860027
*****************************EHIMS*******************************
160300*          WHEN (EMPIRE-PAR-PROV       AND                        16030027
160400*                NO-ITS-EDIT-ERROR-CD  AND                        16040027
256415*                (WS-ECRP-NO OR WS-ECRP-PLUS))                    25641512
160600*             MOVE 'ZI097' TO WS-EITS-ERR                         16060027
160700*             PERFORM B2010-INSERT-EITSXXXXX                      16070027
*****************************EHIMS*******************************
      ***CR117967 START
REL120*          WHEN (LOC-PLAN-800          AND                        16030027
REL120*                NO-ITS-EDIT-ERROR-CD  AND                        16040027
REL120*                WS-ECRP-PLUS)                                    25641512
REL120*             MOVE 'ZI110' TO WS-EITS-ERR                         16060027
REL120*             PERFORM B2010-INSERT-EITSXXXXX                      16070027
      ***CR117967 END
258500***********U602************************************************** 25850015
258600           WHEN U602-NOT-A-ZERO-PRICED-CLAIM AND                  25860015
258700                NO-ITS-EDIT-ERROR-CD  AND                         25870015
258800                WS-ECRP-NO                                        25880015
258900              MOVE 'ZI100' TO WS-EITS-ERR                         25890015
259000              PERFORM B2010-INSERT-EITSXXXXX                      25900015
259100***********U602************************************************** 25910015
259106***********NASCO MEDICARE**************************************** 25910600
REL120*          WHEN NO-ITS-EDIT-ERROR-CD  AND                         25910700
REL120*               NASCO-REJECT          AND                         25910900
REL120*               WS-ECRP-NO                                        25911000
REL120*             MOVE 'ZI102' TO WS-EITS-ERR                         25911100
REL120*             PERFORM B2010-INSERT-EITSXXXXX                      25911200
REL120*          WHEN NO-ITS-EDIT-ERROR-CD  AND                         25911304
REL120*               FOUND-LEASED-NET      AND                         25911404
REL120*               (WS-ECRP-NO OR WS-ECRP-PLUS)                      25911504
REL120*             MOVE 'ZI104' TO WS-EITS-ERR                         25911604
REL120*             PERFORM B2010-INSERT-EITSXXXXX                      25911704
REL120*          WHEN FOUND-EITSZI002 AND NO-ITS-EDIT-ERROR-CD          25911803
REL120*             MOVE 'ZI002' TO WS-EITS-ERR                         25911903
REL120*             PERFORM B2010-INSERT-EITSXXXXX                      25912003
259130           WHEN FOUND-EITSZI033 AND NO-ITS-EDIT-ERROR-CD          25913003
259140              MOVE 'ZI033' TO WS-EITS-ERR                         25914003
259141              PERFORM B2010-INSERT-EITSXXXXX                      25914103
      ***CR136539 START
160300           WHEN (NEG-PRC               AND                        16030027
160400                 NO-ITS-EDIT-ERROR-CD)                            16040027
160600              MOVE 'ZI111' TO WS-EITS-ERR                         16060027
160700              PERFORM B2010-INSERT-EITSXXXXX                      16070027
      ***CR136539 END
259142*REL9.0 START                                                     25914200
REL120*          WHEN 1015-BUNDLED-CLAIM   AND                          25914300
REL120*               NO-ITS-EDIT-ERROR-CD AND                          25914400
REL120*               WS-ECRP-NO                                        25914500
      *CR 145948 BEGIN
REL120*               IF RESUBMISSION-YES
REL120*                  CONTINUE
REL120*               ELSE
      *CR 145948 END
REL120*                  MOVE 'ZI108' TO WS-NF02-ERR                    25914600
REL120*                  PERFORM B2020-INSERT-NF02XXXXX                 25914700
      *CR 145948 BEGIN
REL120*               END-IF
      *CR 145948 END
259148*REL9.0 END                                                       25914800
      ***CR155472 START
                 WHEN (FOUND-INPAT-DME-MSG   AND                        16030027
                       NO-ITS-EDIT-ERROR-CD)                            16040027
                    MOVE 'ZI114' TO WS-EITS-ERR                         16060027
                    PERFORM B2010-INSERT-EITSXXXXX                      16070027
      ***CR155472 END
ZI115F*          WHEN FOUND-EITSZI115 AND NO-ITS-EDIT-ERROR-CD          25913003
ZI115F*             MOVE 'ZI115' TO WS-EITS-ERR                         25914003
ZI115F*             PERFORM B2010-INSERT-EITSXXXXX                      25914103
259142                                                                  25914201
REL120           WHEN (FOUND-EITSZI007       AND
REL120                 NO-ITS-EDIT-ERROR-CD)
REL120               MOVE 'ZI007' TO WS-EITS-ERR
REL120               PERFORM B2010-INSERT-EITSXXXXX
REL120           WHEN (FOUND-EITSZI017       AND
REL120                 NO-ITS-EDIT-ERROR-CD)
REL120               MOVE 'ZI017' TO WS-EITS-ERR
REL120               PERFORM B2010-INSERT-EITSXXXXX
REL120           WHEN (FOUND-EITSZI099       AND
REL120                 NO-ITS-EDIT-ERROR-CD)
REL120               MOVE 'ZI099' TO WS-EITS-ERR
REL120               PERFORM B2010-INSERT-EITSXXXXX
REL120           WHEN (FOUND-EITSZI113       AND
REL120                 NO-ITS-EDIT-ERROR-CD)
REL120               MOVE 'ZI113' TO WS-EITS-ERR
REL120               PERFORM B2010-INSERT-EITSXXXXX
REL120           WHEN (FOUND-EITSZI999       AND
REL120                 NO-ITS-EDIT-ERROR-CD)
REL120               MOVE 'ZI999' TO WS-EITS-ERR
REL120               PERFORM B2010-INSERT-EITSXXXXX
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135*          WHEN (FOUND-EITSZI135       AND
REL135*                NO-ITS-EDIT-ERROR-CD)
REL135*              MOVE 'ZI135' TO WS-EITS-ERR
REL135*              PERFORM B2010-INSERT-EITSXXXXX
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919           WHEN (FOUND-EITSZI116 AND (FM105-TYPE-SUBM ='1' OR
CQ8919                 FM105-TYPE-SUBM ='2') AND
CQ8919                 NO-ITS-EDIT-ERROR-CD)
CQ8919               MOVE 'ZI116' TO WS-EITS-ERR
CQ8919               PERFORM B2010-INSERT-EITSXXXXX
160800        END-EVALUATE                                              16080027
           .                                                            14210024
                                                                        14220024
      ******************************************************************14230024
       B2010-INSERT-EITSXXXXX.                                          14240024
      ******************************************************************14250024
                                                                        14260024
           MOVE SPACES       TO FM105-ERR-CDS                           14270024
           MOVE SPACES       TO FM105-PROC-SITE-CNTL-NUM                14280024
           MOVE WS-EITS-TEXT TO FM105-PROC-SITE-CNTL-NUM                14290024
           .                                                            14300024
                                                                        14310024
      ******************************************************************14320024
REL120*B2020-INSERT-NF02XXXXX.                                          25915503
259156******************************************************************25915603
REL120*                                                                 25915703
REL120*    MOVE SPACES       TO FM105-ERR-CDS                           25915803
REL120*    MOVE SPACES       TO FM105-PROC-SITE-CNTL-NUM                25915903
REL120*    MOVE WS-NF02-MSG  TO FM105-PROC-SITE-CNTL-NUM                25916003
REL120*    .                                                            25916103
REL120*                                                                 25916203
259163******************************************************************25916303
       B3000-DETERMINE-CLAIM-STATUS.                                    14330024
      ******************************************************************14340024
                                                                        14350024
           IF FM105-ERR-CDS = SPACES                                    14360024
              MOVE WS-V             TO FM105-STAT-CD                    14370024
              MOVE WS-V             TO CIABP-STAT-CD                    14380025
              MOVE CWSMC-CLAIM-REL-FOR-EXTRACT                          14390024
                                    TO SIAPC-RETURN-MESSAGE-CD          14400024
           ELSE                                                         14410024
              MOVE WS-I             TO FM105-STAT-CD                    14420024
              MOVE WS-I             TO CIABP-STAT-CD                    14430025
              MOVE CWSMC-FAILED-REL TO SIAPC-RETURN-MESSAGE-CD          14440024
           END-IF                                                       14450024
           .                                                            14460024
                                                                        14470024
       EJECT                                                            14480024
      ******************************************************************14490024
       Z9999-ERROR-ABORT.                                               14500024
      ******************************************************************14510024
                                                                        14520024
           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND              14530024
           MOVE CWSCC-INVALID-FUNCTION TO SIAPC-RETURN-STATUS           14540024
           MOVE CWSMC-INVALID-FUNCTION TO SIAPC-RETURN-MESSAGE-CD       14550024
           .                                                            14560024
                                                                        14570024
      ******************************************************************14580024
      *       E N D   O F   P R O G R A M   I T C P E 0 0 4             14590024
      ******************************************************************14600024
