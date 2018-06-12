       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID.    ITCPE005.                                         00020001
       AUTHOR.        EMPIRE ITS HOME PLAN.                             00030001
       INSTALLATION.  EMPIRE BLUE CROSS BLUE SHIELD.                    00040001
       DATE-WRITTEN.  APRIL 15, 1994.                                   00050001
       DATE-COMPILED.                                                   00060001
      ******************************************************************00070001
      *                                                                *00080001
      *             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *00090001
      *                                                                *00100001
      *   MODULE:   ITCPE005                                           *00110001
      *   NAME:     PROFESSIONAL PLAN SPECIFIC EDIT MODULE             *00120001
003200*   VERSION:  03.5                                               *00320001
003300*   DATE:     NOVEMBER 16, 2007                                  *00330001
      *                                                                *00150001
      *   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *00160001
      *   TYPE:         COMMON                                         *00170001
      *   FUNCTION:     THIS MODULE IS THE PROFESSIONAL PLAN SPECIFIC  *00180001
      *                 EDIT MODULE FOR BATCH OR ON-LINE.              *00190001
      *                                                                *00200001
      *   MODIFICATION                                                 *00210001
      *        HISTORY: 06/01/90 01.0 INITIAL RELEASE                  *00220001
      *                 11/01/96 03.0 MODIFIED FOR 8.0 RELEASE         *00230007
      *                 03/13/97 03.0 ADD B1700-EDIT-PERF-PROV-SPEC    *00240022
      *                 05/13/97 03.0 ADD B1800-EDIT-NON-PPO           *00250028
      *                 10/06/97 03.0 UN-COMMENT                       *00250028
      *                               B1600-EDIT-SERV-ALLOWANCE-AMT    *04400024
      *                 12/12/97 03.0 ADD PREFIX 'YLE' TO EDIT NON-PPO *00250028
      *                 03/09/98 03.0 CR#98010068                      *00253028
      *                               REMOVE NEW HAMPSHIRE, BUFFALO AND*00253028
      *                               SYRACUSE PLANS FROM NON-PPO EDIT *00230003
003110*                 03/13/98 03.0 TECH MEMO ITS-TW-98-03      -CSC *00253028
003120*                        - REMOVED "IGZEDT4" CALL FOR YEAR 2000  *ITCPE004
003130*                          CHANGES (DATE AND TIME ARE AVAILABLE  *ITCPE004
003140*                          FROM ITSIAPC LINKAGE AREA).           *ITCPE004
003150*                 11/06/98 03.0 CR#1998100778                    *00253028
003160*                               CHANGE MAINE PLAN CODE FROM      *00253028
003170*                               180/680 TO 221/721               *00230003
003180*                 12/04/97 03.0 ADD PREFIX 'YLK' TO EDIT NON-PPO *00240311
003190*                 01/07/99 03.0 CR#1998120906                    *00253028
003191*                               REMOVE VERMONT PLAN FEOM NON-PPO *00253028
003192*                               EDIT                             *00230003
002617*                 03/19/99 03.0 CR#19990XXXXX                    *00261715
002618*                               EDIT PRICE MTD 01 FOR PRODUCTION *00261815
002619*                               ONLY                             *00261915
002620*                 09/10/99 03.0 CR#1999090536                    *00262000
002621*                               ZI027/ZP027 EDIT BYPASS PREFIX   *00262100
002622*                               'CKI'.                           *00262200
002623*                 10/15/99 03.0 CR#1999100613                    *00262320
002624*                               FLORIDA PPO EDIT                 *00262400
002625*                               ADD CHECKING PERF-PROV-TYPE-CLM  *00262503
002626*                               TO EDIT NON-PPO                  *00262603
002627*                 11/05/99 03.0 CR#1999100646                    *00262720
002628*                               FLORIDA PPO EDIT                 *00262807
002629*                 11/24/99 03.0 CR#1999110702                    *00262920
002630*                               NATIONAL PPO/EPO PREFIX EDIT     *00263008
002631*                               ADD CHECKING CLASS-PROV-CLM      *00263110
002632*                               TO EDIT NON-PPO                  *00263210
002633*                               ADDED MAINE PLAN CODE 181/681.   *00263313
002634*                 02/25/00 03.0 CR#2000010839                    *00263420
002635*                               EDIT PROC CODE FOR AUTO 02 SINF  *00263514
002636*                 03/24/00 03.0 CR#00047317                      *00263620
002637*                               BYPASS EDIT ZERO RATE FOR TEST.  *00263719
002638*                 06/26/00 03.0 CR#00046592                      *00263828
002639*                               BYPASS EDITS FOR ECRP:           *00263928
002640*                               ZP002,ZP006,ZP007,ZP009,ZP010,   *00264028
002641*                               ZP017,ZP026,ZP027,ZP030,ZP090,   *00264128
002642*                               ZP093,ZP094,ZP097.               *00264228
002643*                               REMOVE ZP001 EDITS.              *00264328
002646*                               BYPASS ZP026 AND ZP027 FOR       *00264628
002647*                               WORLDWIDE CLAIMS.                *00264728
002648*                               CONVERT ITS TO CS90 PROV SPEC CD.*00264828
002649*                               CR#00048786                      *00264928
002650*                               ADD LOC-PLAN-PAYER AND PAY SUB   *00265028
002651*                               COMB EDIT ZP099.                 *00265128
002650*                 06/29/00 03.0 CR#00050512                      *00265033
002651*                               ADD PPO PREFIX EDIT FOR 'SWH'    *00265135
002653*                 07/13/00 03.0 CR#00050783                      *00265338
002654*                               REMOVE 'CLASS-PROV' FLIP FOR     *00265438
002655*                               INTERNATIONAL CLAIMS.            *00265538
002656*                               CR#00051004                      *00265639
002657*                               DO NOT FLIP ANCILLARY PROV FOR   *00265739
002658*                               ECRP CLAIMS                      *00265839
002659*                 07/27/00 03.0 CR#000XXXXX                      *00265942
002660*                               REMOVE NF02 EDIT                 *00266042
002661*                 08/24/00 03.0 CR#00052048                      *00266143
002662*                               FLIP ANCILLARY PROV FOR ECRP CLAIM00266243
002663*                 11/30/00 03.0 CR#00053762                      *00266251
002665*                               ADD PREFIXES AND PPO PROV IND TO *00266544
002665*                               FLIP LOGIC                       *00266451
002666*                 05/14/01 03.0 CR#00056444                      *00266551
002667*                               ITS RELEASE 8.3                  *00266651
002669*                               CHANGES TO 'FM105-PAY-SUB' AND   *00266751
002670*                               'FM1A5-PAY-REP-PAYEE' DUE TO     *00266851
002671*                               COPYLIB CHANGES.                 *00266951
      *                 05/14/01 03.0 CR#00055386                      *00267051
      *                               ITS RELEASE 8.3                  *00267151
      *                               LIFT LOCAL EDIT FOR $0 PRICE ALL *00267251
      *                               INCLUSIVE.                       *00267351
002676*                 11/13/01 03.0 IR#284357                        *00267600
002677*                               REMOVE CPT4/ICD9 EDIT            *00267700
002678*                 01/02/02 03.0 CR#63716                         *00267800
002679*                               ADD BNY & FUE, REMOVE MAINE &    *00267901
002680*                               MOVE ESE & HTF TO BLUECARD PPO   *00268001
002681*                               FOR NON-PPO LOGIC.               *00268101
002682*                 02/28/02 03.0 CR#65363                         *00268200
002683*                               ADD RFB PREFIX                   *00268300
012500*                 08/03/01 -  3.2  TECH MEMO ITS-TM-01-08-R8.4:  *01250001
012600*                          COMPLETED BY:  B. PEPPERS             *01260001
012700*                          - HIPAA - ADJUSTED ITCIASC REDEFINES  *01270001
012800*                            FOR NEW RECORD TYPES D2,F6,32,66.   *01280001
012900*                 04/21/02 03.2 CR#61813 - ITS 8.4               *01290002
013000*                               ADDED 'L' & 'Q' FOR SRC-PMT-CD   *01300001
013100*                               REMOVED 6,7,A,H AND ADD M TO     *01310001
013200*                                  WS-CERT-1-PROV                *01320001
013300*                               REMOVED 8,B TO WS-CERT-2-PROV    *01330001
013400*                               ADDED 'R' & 'S' FOR SRC-PMT-CD   *01340001
013500*                               CHANGED PROV-SUB TABLE MAX TO 187*01350002
013501*                 05/23/02 03.2 CR#65241                         *01350103
013502*                               REMOVED PREFIX 'CKI' ZI027/ZP027 *01350203
013503*                               BYPASS                           *01350303
013504*                 06/20/02 03.2 CR#67767                         *01350406
013505*                               ADD PPO PREFIX EDIT FOR 32BJ     *01350506
012400*                 08/01/02 03.2 CR#68615                         *01240008
012500*                               NO EITSZP027 FRO ADJ SF.         *01250011
012600*                 08/09/02 01.32 CR#70439                        *01260016
012610*                               U602 NON 0 PRICE EDIT EITSZP100  *01261016
012620*                 10/31/02 03.3 CR#73519                         *01262018
012630*                               BYPASS ZI100 & ZP100 ON ECRPS.   *01263018
013530*                 02/05/03      CR#76035                         *01353002
013540*                               BYPASS ZP030 LOCAL EDIT.         *01354002
012633*                 04/13/03 02.3  CR#75564                        *01263301
012634*                               ITS RELEASE 8.6                  *01263401
012635*                               ADDED LS-ITCDBP20-RECORD TO      *01263501
012636*                               LINKAGE SECTION.                 *01263601
012637*                                CR#75322                        *01263701
012638*                               RELEASE 8.6 ALLOWS MEDICARE CLAIM*01263801
012639*                               TO BE PROCESS THRU BLUECARD. SF  *01263901
012640*                               EDITS ZP026 AND ZP027 LIFTED.    *01264001
012641*                                CR#76073                        *01264101
012642*                               DENY MEDICARE SF'S W/ONLY CLAIM  *01264201
012643*                               LEVEL A4 VALUE.                  *01264301
013600*                 05/05/03      CR#77528                         *01360001
013600*                               DENY SF'S WITH ERROR ZP009 &     *01360001
013600*                               CREAT NF                         *01360001
013600*                 06/20/03      IR#297588  INITIALIZING          *01360001
013600*                               WS-PMR-IND                       *01360001
013605*                 07/03/03      CR#78570                         *01360502
013606*                               ADDED PREFIX FUJ TO PPO FLIP LOGIC01360602
013607*                               CR#80637                         *01360703
013610*                               LIFT PROF EDIT ZP030             *01361002
013605*                 07/17/03      CR#80581                         *01360502
013605*                               MODIFY ZP102 EDIT                *01360502
013605*                 07/31/03      CR#80165                         *01360502
013605*                               EITS TO SEND AUTO SINF ON        *01360502
013605*                               HCPC 99070 AND SET EDIT ZP103    *01360502
013616*                 10/13/03      CR#84911                         *01361605
013617*                               INTERCEPT SF FOR LEASED NETWORK  *01361705
013617*                 12/04/03      CR#78885                         *01361705
013617*                               EDIT ZP099 FOR LOC-PLAN-PAYER AND*01361705
013617*                               PAY-SUB FOR NASCO INTERNATIONAL  *01361705
013617*                               CLAIM                            *01361705
013622*                 12/18/03      CR#86159                         *01362200
013623*                               LIFT ZI099/ZP099 EDIT FOR ALL    *01362300
013624*                               INTERNATIONAL CLAIMS             *01362400
013625*                 12/18/03      CR#85844                         *01362500
013626*                               MODIFY AUTO SINF FOR HCPCS 99070 *01362600
013627*                 02/26/04      CR#89643                         *01362702
013628*                               ADDED PREFIX PNW TO PPO FLIP LOGIC01362802
013629*                 05/06/04      CR#89726                         *01362902
013630*                               EITS TO SEND AUTO SINF ON HCPC   *01363002
013631*                               E1399& A4649                     *01363102
013632*                 05/20/04      CR#89907                         *01363209
013633*                               LIFT EITS EDITS ZP006            *01363310
013634*                 05/20/04      CR#89908                         *01363410
013635*                               AUTOMATE SINF FOR ZP002 FOR CS90 *01363510
013634*                 05/20/04      CR#88839                         *00951020
013635*                               MODIFY EDIT ZP102                *00952020
013634*                 06/03/04      CR#93028                         *00952122
013635*                               MODIFY EDIT ZP102                *00952222
013634*                 06/03/04      CR#92619                         *00952322
013635*                               EITS EDIT ZP103 OVERLAYS ZP009   *00952422
013634*                 07/14/04      CR#94376                         *00952528
013635*                               MODIFY EDIT ZP102                *00952628
      *                 09/09/04      CR#96104                         *01850033
      *                               REJECT EITS ERROR ZP002          *01860033
      *                               CR#96584                         *01870033
      *                               REMOVE MONTANA FROM NON PPO      *01880033
013648*                 09/23/04      CR#96657                         *02445503
013649*                               INCLUDE ECRP+ SF'S IN THE LAST   *02445603
013650*                               ACTION MAPPING DATE              *
013662*                 10/16/04      CR#95948                         *01881034
013663*                               ADDED SPECIALTY CODES FOR 8.9    *01882034
024460*                 03/10/05      CR#98645                         *02446001
024461*                               CREATE UPFRONT EDIT(ZP109) FOR   *02446105
024462*                               TROPICANNA                       *02446201
024463*                 04/16/05      CR#103663(REL9.0)                *02446300
024464*                               EITS TO SEND AUTO SINF ON        *02446400
024465*                               1015.SET EDIT ZP108              *02446500
024466*                 04/16/05      CR#103662(REL9.0)                *02446600
024467*                               LIFT THE EDIT ZP102.             *02446700
013662*                 04/16/05      CR#104819                        *01881034
013663*                               ADDED SPECIALTY CODES FOR 9.0    *01882034
013662*                 06/02/05      CR#94179                         *01881034
013663*                               LIFT EDIT ZP009                  *01882034
024488*                 06/30/05      CR#106466                        *02448800
024489*                               REMOVE EITS WEAK NETWORK LOGIC   *02448900
024488*                 04/06/06      CR#117967                        *02448800
024489*                               DENY ECRP+ CLAIMS FROM PLAN 800  *02448900
      *                               AS 1069                          *
024488*                 05/04/06      CR#118546                        *02448800
024489*                               REMOVE COMMENTED OUT CODES AND   *02448900
      *                               UNUSED ERROR CODES               *
021400*                 07/13/06      CR#120565                        *02140000
021500*                               MODIFY AUTO SINF FROM CR # 89726 *02150000
021400*                 11/15/07      CR#136539                        *02140000
021500*                               CREATE EDIT(ZP111) FOR REJECTING *02150000
      *                               CLAIMS WITH NEGATIVE PRICING     *
      *                 12/13/07      CR#137498                        *
      *                               REMOVED ILLEGAL CHARACTERS IN THE*
      *                               SUBSCRIBER/PATIENT NAME          *
021600*                                                                *02160000
095700* 11/16/2007 RELEASE 10.1 - 999 LINE EXPANSION PROJECT           *
099400*            PROGRAM VERSION 03.5                                *
099400*          RIC CASE                                              *
      *                 08/14/08      CR#144348                        *
      *                               CHANGE THE FILLER OF LS-STACK    *
      *                               DUE TO CHANGE OF ITCIASC FOR 10.2*
      *                 01/22/09      CR#145948                        *
      *                               LOCAL EDITS SH NOT OVERLAY OTHER *
      *                               LOCAL EDITS                      *00250024
      *                 05/12/09      CR#148645                        *
      *                               SUSPEND THE SF WITH LOCAL ERROR  *
      *                               CODE ZP113 IF LINE TOTAL CHARGE  *00250024
      *                               IS GREATER THAN $999999.99.      *00250024
      *                 12/03/09      CR#154112                        *
      *                               BYPASS THE LOCAL EDIT ZP094 FOR  *
      *                               ADJUSTMENT SFS WHERE THE         *00250024
      *                               PROCESSING SITE CONTROL NUMBER   *00250024
      *                               EQUALS EITSUNKN.                 *00250024
REL120*                 10/14/12      ITS 12.0 ADD NEW RECORD TYPES    *
REL120*                               LOCAL EDITS MODIFICATIONS FOR    *
REL120*                               ITS 12.0. ADDED FOLLOWING        *
REL120*                               ERROR CODES TO PROC-SITE-CNTL-NUM*
REL120*                               ZP007, ZP017, ZP099, ZP113,      *
REL120*                               ZP999.                           *
REL120*                               ELIMINATE EDITS :                *00103400
REL120*                               ZP002, ZP102, ZP104, ZP105,      *00103400
REL120*                               ZP108, ZP110.                    *00103400
REL135*                 10/11/13      ADDED GY MODIFIER EDIT:          *00103400
REL135*                               ZP135                            *00103400
REL140*                 04/11/14      GY LOGIC BYPASSED FOR PREFIXES   *
REL140*                               YLV, YLR, XGH, XGK               *
REL145*                 10/17/14      STATEMENT COVER DATE CHANGED     *00260024
CQ8919*                 06/12/15      CR#1368919                       *
CQ8919*                               CREATE EDIT(ZP116) TO DENY SF    *
CQ8919*                               IF THE DOS IS GREATER THAN OR    *00250024
CQ8919*                               EQUAL TO 10/01/2015 AND THE      *00250024
CQ8919*                               PROVIDER SUBMITTED VERSION 9 OR  *00250024
CQ8919*                               SPACES                           *00250024
ICDFIX*                 10/01/2015    CQ2046457                        *00250024
ICDFIX*                               MODIFIED EDIT ZP116 TO ACCOMODATE*00250024
ICDFIX*                               ICD IND = SPACES FOR ICD-10      *00250024
ZI115F*                 11/10/2015    IN# INC0755544 EMERCH# CHG0025173*00250024
ZP115F*                               REMOVED EDIT ZP115 AS ITS NO     *00250024
ZP115F*                               LONGER NEEDED                    *00250024
J33055*                 11/10/2017    JIRA STORY# NYCS90-650           *00250024
J33055*                               SSCR 33055                       *00250024
J33055*                               COMMENTED THE LOGIC FOR GY MOD   *00250024
J33055*                               MADE IN REL135, REL140 WITH      *00250024
J33055*                               J33055                           *00260024
021600*                                                                *02160000
002665*     NOTES:                                                     *00266543
      *                                                                *00260001
      ******************************************************************00270001
                                                                        00280001
       ENVIRONMENT DIVISION.                                            00290001
                                                                        00300001
       CONFIGURATION SECTION.                                           00310001
                                                                        00320001
       INPUT-OUTPUT SECTION.                                            00330001
                                                                        00340001
       FILE-CONTROL.                                                    00350001
                                                                        00360001
       DATA DIVISION.                                                   00370001
                                                                        00380001
       FILE SECTION.                                                    00390001
       EJECT                                                            00400001
      ********* W O R K I N G   S T O R A G E   S E C T I O N ********* 00410001
      *---------------------------------------------------------------* 00420001
       WORKING-STORAGE SECTION.                                         00430001
      *---------------------------------------------------------------* 00440001
       01  FILLER                            PIC X(45) VALUE            00450001
           '*** ITCPE005 WORKING-STORAGE STARTS HERE ***'.              00460001
                                                                        00470001
       01  WS-DISPLAY-MODULE.                                           00480001
           05  WS-MODULE-ID                  PIC X(08) VALUE 'ITCPE005'.00490001
           05  FILLER                        PIC X(03) VALUE ' - '.     00500001
           05  WS-MODULE-DESCRIPTION         PIC X(50) VALUE            00510001
               'PROFESSIONAL PLAN SPECIFIC EDIT MODULE            '.    00520001
           05  FILLER                        PIC X(09) VALUE            00530001
               'VERSION #'.                                             00540001
015800     05  WS-VERSION-NUMBER             PIC X(04) VALUE '03.5'.    01580018
           05  FILLER                        PIC X(13) VALUE            00560001
               ' RELEASED ON '.                                         00570001
016100     05  WS-RELEASE-DATE               PIC X(08) VALUE '11/16/07'.01610018
                                                                        00590001
006800*01  WS-CURRENT-TIME.                                             00600001
006900*    05  WS-CURRENT-HOUR               PIC 9(02).                 00610001
007000*    05  WS-CURRENT-MINUTE             PIC 9(02).                 00620001
007100*    05  WS-CURRENT-SECOND             PIC 9(04).                 00630001
007200*                                                                 00640001
007300*01  WS-CURRENT-DATE.                                             00650001
007400*    05  WS-CURRENT-CC                 PIC 9(02).                 00660002
007500*    05  WS-CURRENT-YY                 PIC 9(02).                 00670002
007600*    05  WS-CURRENT-MM                 PIC 9(02).                 00680001
007700*    05  WS-CURRENT-DD                 PIC 9(02).                 00690001
                                                                        00640001
005901 01  WS-SUB-ID-PFX-1-3                 PIC X(03).                 00590100
004500     88  WS-BLUECARD-PPO-PFX           VALUE 'YLD' 'YLE' 'YLK'    00450044
006904                                             'BNY' 'FUE' 'RFB'    00690400
018600                                             'BJF' 'BJT' 'FUJ'    01860001
018601                                             'ESE' 'HTF' 'PNW'    01860101
004510                                             'IAT' 'NYY' 'NYB'.   00451044
004900     88  WS-BLUCD-DS-PPO-PFX           VALUE 'SWH'.               00490033
004700     88  WS-NATL-PPO-EPO-PFX           VALUE 'ENP' 'EDC' 'EXP'    00470044
006909                                             'MYC'.               00690901
005902     88  WS-CIRCUIT-CITY               VALUE 'CCX'.               00590200
005903     88  WS-MERRILL-LYNCH              VALUE 'MLA'.               00590300
005904     88  WS-MARRIOTT-INTL              VALUE 'MTI' 'SDH' 'HMS'.   00590400
005905     88  WS-SONY                       VALUE 'SME'.               00590500
005906     88  WS-COLUMBIA-HOUSE             VALUE 'COH'.               00590600
005907     88  WS-SEARS                      VALUE 'SER'.               00590700
005908     88  WS-THE-HARTFORD               VALUE 'THF'.               00590800
005913     88  WS-FL-NATL-ACCT               VALUE                      00591300
005914         'CCX' 'MLA' 'MTI' 'SDH' 'HMS' 'SME' 'COH' 'SER' 'THF'.   00591400
018615     88  WS-UNS                        VALUE 'UNS'.               01861505
018616     88  WS-UNT                        VALUE 'UNT'.               01861605
REL120*    88  WS-LEASED-NET                 VALUE 'UNS' 'UNT'.         01861705
005920                                                                  00592000
       01  WS-SCCF-EDIT-ERROR-CD             PIC X(05) VALUE SPACES.    00710001
                                                                        00720001
REL120*01  WS-DT-SERV-START-CCYY.                                       00730002
REL120*    05  WS-DT-SERV-START-CC           PIC X(02).                 00740002
REL120*    05  WS-DT-SERV-START-YY           PIC X(02).                 00750002
                                                                        00760002
       01  WS-PROC-SITE-CNTL-NUM.                                       00770001
           05  WS-PROC-SITE-CNTL-NUM-1-4.                               00780001
006800         10  WS-PROC-SITE-CNTL-NUM-1-2     PIC X(02).             00680014
               10  FILLER                        PIC X(02).             00800001
           05  WS-PROC-SITE-CNTL-NUM-5-8.                               00810001
               10  WS-PROC-SITE-CNTL-NUM-5       PIC X(01).             00820001
               10  FILLER                        PIC X(03).             00830001
           05  FILLER                            PIC X(09).             00840001
                                                                        00850001
       01  WS-SWITCHES.                                                 01470001
010020     05  WS-PPO-AVAILABILITY           PIC X(01).                 01002045
010030         88  PPO-AVAILABLE             VALUE 'Y'.                 01003045
010040         88  PPO-NOT-AVAILABLE         VALUE 'N'.                 01004045
           05  WS-EMPTY-ERROR-CD-SLOT-SW     PIC X(01) VALUE 'N'.       01480001
               88  FOUND-EMPTY-ERROR-CD-SLOT VALUE 'Y'.                 01490001
               88  NO-EMPTY-ERROR-CD-SLOT    VALUE 'N'.                 01491015
           05  WS-PRICE-MTD-RULE-SW          PIC X(01) VALUE 'N'.       01500001
               88  FOUND-PRICE-MTD-RULE-ERROR                           01510001
                                             VALUE 'Y'.                 01590001
           05  WS-PERF-PROV-SPEC-SW          PIC X(01) VALUE 'N'.       01490022
               88  PERF-PROV-SPEC-MISSING     VALUE 'Y'.                01500023
               88  PERF-PROV-SPEC-NOT-MISSING VALUE 'N'.                01510023
012610     05  WS-PROV-SPEC-SW               PIC X(01) VALUE 'N'.       01261028
012620         88  PROV-SPEC-FOUND           VALUE 'Y'.                 01262028
012630         88  PROV-SPEC-NOT-FOUND       VALUE 'N'.                 01263028
REL120*    05  WS-F0-F5-ERROR-SW             PIC X(01) VALUE 'N'.       01600001
REL120*        88  FOUND-F0-F5-ERROR         VALUE 'Y'.                 01610001
           05  WS-ITS-EDIT-ERROR-CD-SW       PIC X(01) VALUE 'N'.       01620001
               88  NO-ITS-EDIT-ERROR-CD      VALUE 'N'.                 01630001
               88  FOUND-ITS-EDIT-ERROR-CD   VALUE 'Y'.                 01640001
           05  WS-MEDICARE-ERROR-SW          PIC X(01) VALUE 'N'.       01650001
               88  NO-MEDICARE-ERROR         VALUE 'N'.                 01660001
               88  FOUND-MEDICARE-ERROR      VALUE 'Y'.                 01670001
REL120*    05  WS-EITSZP093-SW               PIC X(01) VALUE 'N'.       01710010
REL120*        88  NO-EITSZP093              VALUE 'N'.                 01720010
REL120*        88  FOUND-EITSZP093           VALUE 'Y'.                 01730010
ZP115F*    05  WS-EITSZP115-SW               PIC X(01) VALUE 'N'.       01710010
ZP115F*        88  NO-EITSZP115              VALUE 'N'.                 01720010
ZP115F*        88  FOUND-EITSZP115           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZP007-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZP007              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZP007           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZP017-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZP017              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZP017           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZP099-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZP099              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZP099           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZP113-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZP113              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZP113           VALUE 'Y'.                 01730010
REL120     05  WS-EITSZP999-SW               PIC X(01) VALUE 'N'.       01710010
REL120         88  NO-EITSZP999              VALUE 'N'.                 01720010
REL120         88  FOUND-EITSZP999           VALUE 'Y'.                 01730010
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135***CQ420369 START
REL135*    05  WS-EITSZP135-SW               PIC X(01) VALUE 'N'.       01710010
REL135*        88  NO-EITSZP135              VALUE 'N'.                 01720010
REL135*        88  FOUND-EITSZP135           VALUE 'Y'.                 01730010
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919     05  WS-EITSZP116-SW               PIC X(01) VALUE 'N'.
CQ8919         88  NO-EITSZP116              VALUE 'N'.
CQ8919         88  FOUND-EITSZP116           VALUE 'Y'.
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135*    05  WS-GY-IDEN-SW                 PIC X(01) VALUE 'N'.       01710010
REL135*        88  WS-GY-NOT-FOUND           VALUE 'N'.                 01730010
REL135*        88  WS-GY-FOUND               VALUE 'Y'.                 01720010
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
REL135*    05  WS-GY1-IDEN-SW                PIC X(01) VALUE 'N'.       01710010
REL135*        88  WS-GY1-FOUND              VALUE 'Y'.                 01720010
REL135*        88  WS-GY1-NOT-FOUND          VALUE 'N'.                 01730010
REL135***CQ420369 START
           05  WS-PPO-PROV-IND               PIC X(01) VALUE 'Y'.       01670026
               88  PPO-PROV                  VALUE 'Y'.                 01680026
               88  NON-PPO-PROV              VALUE 'N'.                 01690027
015610     05  WS-PPO-FL-PROV-IND            PIC X(01) VALUE 'N'.       01561002
015620         88  WS-PPO-FL-PROV            VALUE 'Y'.                 01562002
015630     05  WS-PROC-CD-SW                 PIC X(01) VALUE 'N'.       01563014
015640         88  FOUND-NF02                VALUE 'Y'.                 01564014
REL120*    05  WS-EITSZP002-SW               PIC X(01) VALUE 'N'.       03640033
REL120*        88  FOUND-EITSZP002           VALUE 'Y'.                 03650033
REL120*        88  NO-EITSZP002              VALUE 'N'.                 03660033
014900     05  WS-ECRP-SWITCH                PIC X(01).                 01490026
015000         88  WS-ECRP-YES               VALUE 'Y'.                 01500026
015100         88  WS-ECRP-NO                VALUE 'N' ' '.             01510026
018729         88  WS-ECRP-PLUS              VALUE 'H'.                 01872906
015000     05  WS-BLCD-SVC-DT-END            PIC X(01).                 01500033
015100         88  WS-BLCD-SVC-DT-END-Y      VALUE 'Y'.                 01510033
015110         88  WS-BLCD-SVC-DT-END-N      VALUE 'N' ' '.             01511033
030510***********U602************************************************** 03051012
030520     05  WS-U602-ZERO-PRICE-IND        PIC X(01).                 03052012
030530         88  U602-ZERO-PRICED-CLAIM       VALUE 'N'.              03053012
030540         88  U602-NOT-A-ZERO-PRICED-CLAIM VALUE 'Y'.              03054012
030541     05  WS-U602-CLAIM-LVL-IND         PIC X(01).                 03054115
030542         88  U602-CLAIM-LVL            VALUE 'Y'.                 03054215
030550***********U602************************************************** 03055012
030535     05  WS-NUM-F6-LN                  PIC 9(02) VALUE ZEROES.    03053505
030536         88  NO-F6-LN                  VALUE ZEROES.              03053605
REL120*    05  WS-LEASED-NET-IND             PIC X(01) VALUE 'N'.       03056405
REL120*        88  NO-LEASED-NET             VALUE 'N'.                 03056505
REL120*        88  FOUND-LEASED-NET          VALUE 'Y'.                 03056605
036645*REL9.0 START                                                     03664500
REL120*    05  WS-1015-BUNDLED-CLAIM         PIC X(01).                 03664600
REL120*        88 1015-BUNDLED-CLAIM         VALUE 'Y'.                 03664700
REL120*        88 1015-NOT-BUNDLED-CLAIM     VALUE 'N'.                 03664800
036649*REL9.0 END                                                       03664900
      ***CR117967 START
REL120*    05  WS-LOC-PLAN-800-SW            PIC X(01) VALUE 'N'.
REL120*        88  LOC-PLAN-800              VALUE 'Y'.
REL120*        88  NOT-LOC-PLAN-800          VALUE 'N'.
      ***CR117967 END
036600***CR120565 START                                                 03660004
036700     05  WS-SERV-CHG-GTEQ-500          PIC X(01) VALUE 'N'.       03670004
036800         88  SERV-CHG-GTEQ-500         VALUE 'Y'.                 03680004
036900         88  SERV-CHG-NOT-GTEQ-500     VALUE 'N'.                 03690004
037000***CR120565 END                                                   03700004
      ***CR136539 START
           05  WS-NEG-PRC-SW                 PIC X(01) VALUE 'N'.
               88  NEG-PRC                   VALUE 'Y'.
               88  NOT-NEG-PRC               VALUE 'N'.
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
                                                                        01740001
REL120*01  WS-RULE-NUMBER.                                              01710025
REL120*    05  FILLER                        PIC X(03).                 01720025
REL120*    05  WS-RULE-4-6                   PIC X(03).                 01730025
REL120*        88  WS-INV-RULE-NUM           VALUE                      01740025
REL120*                      '001' '002' '004' '006' '007' '008'.       01750025
                                                                        01760001
       01  WS-PLAN-CD                        PIC X(03).                 01770025
017701*    88  WS-MAINE                      VALUE '181' '681'          01770100
017702*                                            '221' '721'.         01770200
      *    88  WS-MONTANA                    VALUE '251' '751'.         04160033
      *    88  WS-NEW-HAMPSHIRE              VALUE '270' '770'.         01800025
      *    88  WS-BUFFALO                    VALUE '301' '801'.         01810025
      *    88  WS-SYRACUSE                   VALUE '305' '805'.         01820025
018900*    88  WS-VERMONT                    VALUE '415' '915'.         01830025
018400*    88  WS-SAN-JUAN-PR                VALUE '470'.               01840000
018501*    88  WS-VALID-PLAN-CD              VALUE '251' '751'.         04220033
018502*    88  WS-VALID-PLAN-CD              VALUE '221' '721' '470'    01850200
018503*                                            '181' '681'.         01850300
019210*        '751' '415' '915' '470'.                                 01860025
      *        '751' '270' '770' '301' '801' '305' '805' '415' '915'    01860025
                                                                        01880025
016500 01  WS-PERF-PROV-SPEC                 PIC X(02).                 01650028
016510                                                                  01651028
       01  WS-PROVIDER-TYPE                  PIC X(02).                 01890025
           88  WS-MD                      VALUE 'A1'.                   01900026
           88  WS-OSTEOPATH               VALUE 'A6'.                   01910026
           88  WS-OSTEOPATH-GROUP         VALUE 'BP'.                   01920026
           88  SELECT-PROV-TYPE           VALUE 'A1' 'A6' 'BP'.         01930026
                                                                        01940026
       01  WS-PROD-TYPE                      PIC X(01).                 01950026
           88  WS-INTERNATIONAL              VALUE 'I'.                 01960026
           88  WS-NATIONAL                   VALUE 'N'.                 01970026
           88  SELECT-PROD-TYPE              VALUE 'I' 'N'.             01980026
                                                                        01990025
       01  WS-CLASS-PROV                     PIC X(01).                 02000025
035000     88  WS-CERT-1-PROV             VALUE '1' '4' '5'             03500001
035100                                          'D' 'E' 'F' 'G' 'M'.    03510001
035200     88  WS-CERT-2-PROV             VALUE '3' 'N'.                03520001
035300     88  WS-NON-PPO-PROV            VALUE '1' '3' '4'             03530001
035400                                          'D' 'E' 'F' 'G'         03540001
035500                                          'M' 'N'.                03550001
                                                                        01800001
       01  WS-CONSTANTS.                                                01810001
           05  WS-I                          PIC X(01) VALUE 'I'.       01820001
           05  WS-V                          PIC X(01) VALUE 'V'.       01830001
           05  WS-EITS-TEXT.                                            01840001
               10  FILLER                    PIC X(04) VALUE 'EITS'.    01850001
               10  WS-EITS-ERR               PIC X(05) VALUE SPACES.    01860008
      ***CR80165
019309*    05  WS-NF02-MSG.                                             01930916
019310*        10  FILLER                    PIC X(04) VALUE 'NF02'.    01931016
019311*        10  WS-PROC-CD                PIC X(05).                 01931116
019312*            88  PROC-CD-99070          VALUE '99070'.            01931217
019309     05  WS-NF02-MSG.                                             01930916
019310         10  FILLER                    PIC X(04) VALUE 'NF02'.    01931016
               10  WS-NF02-ERR               PIC X(05) VALUE SPACES.    01860008
019311     05  WS-PROC-CD                    PIC X(05).                 01931116
019312         88  PROC-CD-99070               VALUE '99070'.           01931217
035518         88  PROC-CD-E1399               VALUE 'E1399'.           03551800
035519         88  PROC-CD-A4649               VALUE 'A4649'.           03551900
      ***CR80165
035519***CR85844                                                        03551900
035520     05  WS-HCPCS-FOUND-FLAG           PIC X(01) VALUE 'N'.       03552000
035521         88  HCPCS-FOUND               VALUE 'Y'.                 03552100
035522     05  WS-HCPCS-OTH-FLAG             PIC X(01) VALUE 'N'.       03552200
035523         88  HCPCS-OTH                 VALUE 'Y'.                 03552300
035526***CR89726                                                        03552602
035527     05  WS-HCPCS-FOUND-E1399-FLAG     PIC X(01) VALUE 'N'.       03552702
035528         88  HCPCS-E1399-FOUND         VALUE 'Y'.                 03552802
035529     05  WS-HCPCS-FOUND-A4649-FLAG     PIC X(01) VALUE 'N'.       03552902
035530         88  HCPCS-A4649-FOUND         VALUE 'Y'.                 03553002
      *** CR#148645 START
           05  WS-MAX-SERV-CHRG              PIC S9(8)V99 COMP-3
                                             VALUE +999999.99.
      *** CR#148645 END
035524**                                                                03552400
       01  WS-SUBSCRIPTS.                                               01880001
           05  CD-SUB                        PIC S9(04) COMP.           01890001
           05  T4-SUB                        PIC S9(04) COMP.           01900001
           05  D1-SUB                        PIC S9(04) COMP.           01910002
           05  D9-SUB                        PIC S9(04) COMP.           01920002
019315     05  DT-SUB                        PIC S9(04) COMP.           01931503
           05  F0-SUB                        PIC S9(04) COMP.           01930001
           05  F5-SUB                        PIC S9(04) COMP.           01940001
035410     05  F6-SUB                        PIC S9(04) COMP.           03541002
035420     05  VAL-SUB                       PIC S9(04) COMP.           03542002
019311     05  PROV-SUB                      PIC S9(04) COMP.           01931130
           05  ERR-SUB                       PIC S9(04) COMP.           01950001
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135*    05  GY-SUB                        PIC S9(04) COMP.           01950001
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
REL135*    05  GY-SU1                        PIC S9(04) COMP.           01950001
037010***********U602************************************************** 03701012
037020     05  MSG-SUB                       PIC S9(04) COMP.           03702012
037030***********U602************************************************** 03703012
           05  ERR-SLOT                      PIC S9(04) COMP.           01960001
                                                                        01970001
       01  WS-PMT-DISP-CD                    PIC X(01).                 01980001
           88  WS-PAY-SUB                    VALUE '2'.                 01990001
           88  WS-PAY-PROV                   VALUE '3'.                 02000001
           88  WS-PAY-REP-PAYEE              VALUE '4'.                 02010001
                                                                        02020001
       01  WS-PRICE-MTD-RULE-TABLE.                                     02030001
           05  WS-PRICING-MTD                PIC X(02).                 02040001
               88  WS-NO-PRICING-MTD         VALUE '  '.                02050001
               88  WS-CHRGS                  VALUE '01'.                02060001
               88  WS-PCT-OF-CHRGS           VALUE '10'.                02070001
               88  WS-PCT-ALLOW-AMT-PER-COS  VALUE '14'.                02080001
               88  WS-PCT-ALLOW-AMT-PER-UOS  VALUE '15'.                02090001
               88  WS-PER-DIEM               VALUE '20'.                02100001
               88  WS-DRG                    VALUE '30'.                02110001
               88  WS-CASE-ALLOW-PCT-OF-CHRGS                           02120001
                                             VALUE '33'.                02130001
               88  WS-FLAT-FEE-ALLOW-PER-COS VALUE '40'.                02140001
               88  WS-FLAT-FEE-ALLOW-PER-UOS VALUE '41'.                02150001
               88  WS-MULT-SRV-ALLOW         VALUE '42'.                02160001
               88  WS-MULT-SRV-ALLOW-PCT-OF-CHRGS                       02170001
                                             VALUE '43'.                02180001
       EJECT                                                            02190001
020200 COPY EHPRVSPC.                                                   02020028
020300 EJECT                                                            02030026
020900 COPY EHPPOMED.                                                   02090003
020910 EJECT                                                            02091001
       01  ITCWSCC-RECORD.                                              02200001
           COPY ITCWSCC.                                                02210001
       EJECT                                                            02220001
       01  ITCWSMC-RECORD.                                              02230001
           COPY ITCWSMC.                                                02240001
                                                                        02250001
       EJECT                                                            02260001
       01  ITCWSMCD-RECORD.                                             02270001
           COPY ITCWSMCD.                                               02280001
                                                                        02290001
       01  FILLER                            PIC X(45) VALUE            02300001
           '*** ITCPE005 WORKING STORAGE ENDS HERE ***'.                02310001
       EJECT                                                            02320001
      ****************  L I N K A G E    S E C T I O N  *************** 02330001
      *---------------------------------------------------------------* 02340001
       LINKAGE SECTION.                                                 02350001
      *---------------------------------------------------------------* 02360001
       01  LS-PROCESS-CONTROL.                                          02370001
           COPY ITSIAPC.                                                02380001
                                                                        02390001
       01  LS-BATCH-PROCESS.                                            02400001
           COPY ITCIABP.                                                02410001
                                                                        02420001
       01  LS-PROF-ERROR-INDICATORS.                                    02430001
           COPY ITCIAEP.                                                02440001
                                                                        02450001
       01  LS-WORK-AREA.                                                02460001
           COPY ITCIAWA.                                                02470001
                                                                        02480001
       01  LS-PROF-WORK-AREA REDEFINES LS-WORK-AREA.                    02490001
           05 FILLER                     PIC X(1275).                   02500012
                                                                        02510001
       01  LS-STACKED-CLAIM.                                            02520001
           COPY ITCIASC.                                                02530001
                                                                        02540001
       01  LS-PROF-REC-TYPE-DATA REDEFINES LS-STACKED-CLAIM.            02550001
REL102*    02  FILLER              PIC X(4).                            02560012
REL102     02  FILLER              PIC X(10).                           02560012
           02  CIASC-PROF-RECORDS.                                      02570012
               03  CIASC-A5-RECORD.                                     02580012
                COPY ITCFM1A5.                                          02590012
               03  CIASC-B0-RECORD.                                     02600012
                COPY ITCFM1B0.                                          02610012
               03  CIASC-B5-RECORD.                                     02620012
                COPY ITCFM1B5.                                          02630012
               03  CIASC-C0-RECORD.                                     02640012
                COPY ITCFM1C0.                                          02650012
               03  CIASC-D0-RECORD.                                     02660012
                COPY ITCFM1D0.                                          02670012
               03  CIASC-D1-RECORD OCCURS 2 TIMES.                      02680012
                COPY ITCFM1D1.                                          02690012
045700         03  CIASC-D2-RECORD.                                     04570001
045800          COPY ITCFM1D2.                                          04580001
REL120         03  CIASC-D3-RECORD.                                     04570001
REL120          COPY ITCFM1D3.                                          04580001
               03  CIASC-E0-RECORD.                                     02700012
                COPY ITCFM1E0.                                          02710012
               03  CIASC-E1-RECORD.                                     02720012
                COPY ITCFM1E1.                                          02730012
REL120         03  CIASC-E2-RECORD.                                     02720012
REL120          COPY ITCFM1E2.                                          02730012
REL101         03  CIASC-E6-RECORD OCCURS 999 TIMES.                    02740012
REL101          COPY ITCFM1E6.                                          02750012
REL101         03  CIASC-F0-RECORD OCCURS 999 TIMES.                    02760012
                COPY ITCFM1F0.                                          02770012
REL135*        03  CIASC-F1-RECORD OCCURS 999 TIMES.                    02760012
REL135*         COPY ITCFM1F1.                                          02770012
REL101         03  CIASC-F5-RECORD OCCURS 999 TIMES.                    02780012
                COPY ITCFM1F5.                                          02790012
REL101         03  CIASC-F6-RECORD OCCURS 999 TIMES.                    04690001
047000          COPY ITCFM1F6.                                          04700001
               03  CIASC-G0-RECORD.                                     02800012
                COPY ITCFM1G0.                                          02810012
               03  CIASC-X0-RECORD.                                     02820012
                COPY ITCFM1X0.                                          02830012
           02  FILLER                 PIC X(1530).                      02840012
045700****** REL8.6 *********                                           04570001
045800/                                                                 04580001
045900 01  LS-ITCDBP20-RECORD.                                          04590001
046000     COPY ITCDBP20.                                               04600001
046100***********************                                           04610001
                                                                        02850001
                                                                        02860001
       EJECT                                                            02870001
      ******************************************************************02880001
      ******************************************************************02890001
      *              P R O C E D U R E   D I V I S I O N               *02900001
      ******************************************************************02910001
      ******************************************************************02920001
       PROCEDURE DIVISION USING LS-PROCESS-CONTROL                      02930001
                                LS-BATCH-PROCESS                        02940001
                                LS-PROF-ERROR-INDICATORS                02950001
                                LS-WORK-AREA                            02960001
046860                          LS-STACKED-CLAIM                        04686001
046870******* REL8.6 *************                                      04687001
046880                          LS-ITCDBP20-RECORD.                     04688001
046890****************************                                      04689001
                                                                        02980001
      ******************************************************************02990001
       00000-MAINLINE.                                                  03000002
      ******************************************************************03010001
                                                                        03020001
           PERFORM A0000-INITIALIZATION                                 03030002
                                                                        03040001
           IF SIAPC-FUNCTION-CD-MAJOR = CWSCC-EDIT-DATA                 03050001
              PERFORM B0000-EDIT-PROCESS                                03060002
           ELSE                                                         03070001
              PERFORM Z9999-ERROR-ABORT                                 03080002
           END-IF                                                       03090001
                                                                        03100001
           GOBACK                                                       03110001
           .                                                            03120001
                                                                        03130001
       EJECT                                                            03140001
      ******************************************************************03150001
       A0000-INITIALIZATION.                                            03160002
      ******************************************************************03170001
                                                                        03180001
           IF CIABP-PLAN-PROF-FIRST-TIME                                03190001
035300*       CALL 'IGZEDT4' USING BY REFERENCE WS-CURRENT-DATE         03200002
035400*       ACCEPT WS-CURRENT-TIME FROM TIME                          03210001
              DISPLAY WS-DISPLAY-MODULE                                 03220001
              MOVE 'N' TO CIABP-PLAN-PROF-EDIT-SW                       03230001
           END-IF                                                       03240001
                                                                        03250001
           MOVE SPACES TO WS-SCCF-EDIT-ERROR-CD                         03260001
030210                    WS-PERF-PROV-SPEC                             03021028
                                                                        03280001
           MOVE 'N'    TO WS-EMPTY-ERROR-CD-SLOT-SW                     03290001
                          WS-ITS-EDIT-ERROR-CD-SW                       03300001
                          WS-PRICE-MTD-RULE-SW                          03310001
                          WS-PERF-PROV-SPEC-SW                          03260022
030910                    WS-PROV-SPEC-SW                               03091028
REL120*                   WS-F0-F5-ERROR-SW                             03330001
                          WS-MEDICARE-ERROR-SW                          03340001
REL120*                   WS-EITSZP093-SW                               03360010
ZP115F*                   WS-EITSZP115-SW                               03360010
REL120                    WS-EITSZP007-SW                               03360010
REL120                    WS-EITSZP017-SW                               03360010
REL120                    WS-EITSZP099-SW                               03360010
REL120                    WS-EITSZP113-SW                               03360010
REL120                    WS-EITSZP999-SW                               03360010
032610                    WS-PROC-CD-SW                                 03261018
031500                    WS-ECRP-SWITCH                                03150026
031500                    WS-BLCD-SVC-DT-END                            03150033
052610***********U602************************************************** 05261012
052620                    WS-U602-ZERO-PRICE-IND                        05262012
052621                    WS-U602-CLAIM-LVL-IND                         05262115
052630***********U602************************************************** 05263012
REL120*                   WS-LEASED-NET-IND                             05263205
052633                    WS-HCPCS-FOUND-FLAG                           05263300
052634                    WS-HCPCS-FOUND-E1399-FLAG                     05263402
052635                    WS-HCPCS-FOUND-A4649-FLAG                     05263502
052634                    WS-HCPCS-OTH-FLAG                             05263400
REL120*                   WS-EITSZP002-SW                               06680033
      ***CR117967 START
REL120*                   WS-LOC-PLAN-800-SW
      ***CR117967 END
066804*REL9.0 START                                                     06680400
REL120*                   WS-1015-BUNDLED-CLAIM                         06680500
066806*REL9.0 END                                                       06680600
063500***CR120565 START                                                 06350004
063600                    WS-SERV-CHG-GTEQ-500                          06360004
063700***CR120565 END                                                   06370004
      ***CR136539 START
                          WS-NEG-PRC-SW
      ***CR136539 END
      ***CR145948 START
                          WS-RESUBMISSION-SW
      ***CR145948 END
      ***CR#148645 START
                          WS-HIGH-DOLLAR-SW
      ***CR#148645 END
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135***CQ#420369 START
REL135*                   WS-EITSZP135-SW
REL135*                   WS-GY-IDEN-SW
REL135*                   WS-GY1-IDEN-SW
REL135***CQ#420369 END
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919                    WS-EITSZP116-SW
                                                                        03370001
           IF FM1A5-ERR-CDS = SPACES                                    03380001
              MOVE 'N' TO WS-ITS-EDIT-ERROR-CD-SW                       03390001
              MOVE 1   TO ERR-SLOT                                      03400001
           ELSE                                                         03410001
              MOVE 'Y' TO WS-ITS-EDIT-ERROR-CD-SW                       03420001
              PERFORM A1000-CHECK-EDIT-ERROR-CDS                        03430013
                 VARYING ERR-SUB FROM 1 BY 1                            03440001
                 UNTIL   ERR-SUB > 5 OR                                 03450001
                         FOUND-EMPTY-ERROR-CD-SLOT                      03460001
              IF NO-EMPTY-ERROR-CD-SLOT                                 03460115
                 MOVE 6   TO ERR-SLOT                                   03461015
              END-IF                                                    03470015
           END-IF                                                       03471015
           .                                                            03480001
                                                                        03490001
      ******************************************************************03500001
       A1000-CHECK-EDIT-ERROR-CDS.                                      03510013
      ******************************************************************03520001
                                                                        03530001
           IF FM1A5-ERR-CD (ERR-SUB) = SPACES                           03540001
              MOVE ERR-SUB TO ERR-SLOT                                  03550001
              MOVE 'Y'     TO WS-EMPTY-ERROR-CD-SLOT-SW                 03560001
           END-IF                                                       03660001
REL120*    IF FM1A5-ERR-CD (ERR-SUB) = 'R0F0 ' OR 'R0F5 ' OR            03580002
REL120*                                'PR06 ' OR 'PR07 ' OR            03590008
REL120*                                ' R0F0' OR ' R0F5' OR            03600008
REL120*                                ' PR06' OR ' PR07'               03610008
REL120*       MOVE 'Y'     TO WS-F0-F5-ERROR-SW                         03620001
REL120*       MOVE 'N'     TO WS-EMPTY-ERROR-CD-SLOT-SW                 03630001
REL120*    ELSE                                                         03640001
REL120*       MOVE 'N'     TO WS-EMPTY-ERROR-CD-SLOT-SW                 03650001
REL120*    END-IF                                                       03660001
REL120*    END-IF                                                       03670001
           .                                                            03680001
                                                                        03690001
       EJECT                                                            03700001
      ******************************************************************03710001
       B0000-EDIT-PROCESS.                                              03720002
      ******************************************************************03730001
                                                                        03740001
035700     MOVE FM1A5-ECR-IND   TO  WS-ECRP-SWITCH                      03570026
035800                                                                  03580026
           MOVE FM1A5-PROC-SITE-CNTL-NUM TO                             03750004
                      WS-PROC-SITE-CNTL-NUM                             03760004
                                                                        03770004
036200     IF (WS-PROC-SITE-CNTL-NUM-1-4 = 'EITS') AND                  03620026
036300         WS-ECRP-NO                                               03630026
              MOVE WS-PROC-SITE-CNTL-NUM-5-8 TO CWSMCD-MESSAGE-CD       03790004
              IF (CWSMCD-VALID-MESSAGE-CD      OR                       03800004
                  WS-PROC-SITE-CNTL-NUM-5      =  'Z')                  04130032
                 PERFORM B3000-DETERMINE-CLAIM-STATUS                   03820004
REL120*       ELSE                                                      03830004
      ***CR#154112 START
REL120*          IF (WS-PROC-SITE-CNTL-NUM-5-8 = 'UNKN') AND
REL120*              FM1A5-ADJUSTMENT
REL120*             CONTINUE
REL120*          ELSE
      ***CR#154112 END
REL120*             MOVE 'ZP094' TO WS-SCCF-EDIT-ERROR-CD               03840008
REL120*             PERFORM B1999-INSERT-ERROR-CODE                     03850004
REL120*             PERFORM B3000-DETERMINE-CLAIM-STATUS                03860004
      ***CR#154112 START
REL120*          END-IF
      ***CR#154112 END
              END-IF                                                    03870004
           ELSE                                                         03880004
067076*CR#141076 BEGIN                                                  06707612
REL120*       IF WS-ECRP-YES                                            06707712
REL120*          MOVE 'ZP112' TO WS-SCCF-EDIT-ERROR-CD                  06707812
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        06707912
REL120*          PERFORM B3000-DETERMINE-CLAIM-STATUS                   06708012
REL120*       END-IF                                                    06708112
067082*CR#141076 END                                                    06708212
              PERFORM B1000-EDIT-CLAIM                                  03890004
              PERFORM B2000-EITSXXXXX-CHK                               03900008
              PERFORM B3000-DETERMINE-CLAIM-STATUS                      03910004
           END-IF                                                       03920004
           .                                                            03930001
                                                                        03940001
      ******************************************************************03950001
       B1000-EDIT-CLAIM.                                                03960002
      ******************************************************************03970001
052700     MOVE FM1D0-SUB-ID-PFX-1-3     TO WS-SUB-ID-PFX-1-3           05270005
                                                                        03980002
           IF FM1A5-REC-TYPE NOT = SPACES AND                           03990017
REL101*        FM1A5-SEQ-NUM NUMERIC      AND                           03991017
056906         FM1A5-SEQ-NUM NOT = ZEROES                               03860026
038800        PERFORM B1050-MAP-LAST-ACT-DT                             03880026
      ***CR117967 START
REL120*       PERFORM B1180-EDIT-LOC-PLAN-800
      ***CR117967 END
039000     END-IF                                                       03900026
039100                                                                  03910026
039200     IF FM1A5-REC-TYPE NOT = SPACES AND                           03920026
REL101*        FM1A5-SEQ-NUM NUMERIC      AND                           03930026
039400         FM1A5-SEQ-NUM NOT = ZEROES AND                           03940026
039500         WS-ECRP-NO                                               03950026
              PERFORM B1020-EDIT-PLAN-PAYER-CD                          04010002
              PERFORM B1040-EDIT-RESUBMISSION-IND                       04030003
056919*       PERFORM B1050-MAP-LAST-ACT-DT                             04040002
           END-IF                                                       04060002
                                                                        04070001
      ***CR#148645 START
           PERFORM B1300-CHK-HIGH-DOLLAR VARYING F0-SUB FROM 1 BY 1     11125800
           UNTIL FOUND-HIGH-DOLLAR OR F0-SUB > 999 OR                   11125900
                 ( FM1F0-REC-TYPE (F0-SUB) = SPACES AND                 11126100
                   FM1F0-SEQ-NUM (F0-SUB) NOT = ZEROS )                 11125900
      ***CR#148645 END

REL120*    IF WS-ECRP-NO                                                04020226
REL120*       IF FM1B0-REC-TYPE NOT = SPACES AND                        04020326
REL101*           FM1B0-SEQ-NUM NUMERIC      AND                        04020426
REL120*           FM1B0-SEQ-NUM NOT = ZEROES                            04020526
REL120*          PERFORM B1110-EDIT-FEDERAL-TAX-ID-NUM                  04020726
REL120*       END-IF                                                    04020826
                                                                        04120002
REL120*       IF FM1A5-CNTL-PLAN-CD   = '243'   OR                      06050110
REL120*          FM1A5-CNTL-PLAN-CD   = '803'   OR                      06050210
REL120*          FM1D0-SUB-ID-PFX-1-3 = 'YLG'                           06050310
REL120*          CONTINUE                                               06050410
REL120*       END-IF                                                    06101010
                                                                        04240011
REL120*       PERFORM B1400-EDIT-DOS                                    04110026
REL120*          VARYING F0-SUB FROM 1 BY 1                             04120026
REL120*          UNTIL   FOUND-F0-F5-ERROR                 OR           04130026
REL120*                  FOUND-EITSZP093                   OR           04140026
REL120*                  F0-SUB > 999                                   04150026
                                                                        04310001
                                                                        04370001
REL120*    END-IF
                                                                        04450021
REL120*    IF WS-LEASED-NET AND (WS-ECRP-NO OR WS-ECRP-PLUS)            06364305
REL120*       PERFORM B1450-LEASED-NET                                  06364405
REL120*    END-IF                                                       06364505
063646                                                                  06364604
           PERFORM B1700-EDIT-PERF-PROV-SPEC                            04460021
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL140*    IF FM1D0-SUB-ID-PFX-1-3 = 'YLV' OR
REL140*       FM1D0-SUB-ID-PFX-1-3 = 'YLR' OR
REL140*       FM1D0-SUB-ID-PFX-1-3 = 'XGH' OR
REL140*       FM1D0-SUB-ID-PFX-1-3 = 'XGK'
REL140*       CONTINUE
REL140*    ELSE
REL135***CQ#420369 START
REL135*       PERFORM B1335-SET-GY-IDENTIFER VARYING GY-SUB FROM 1 BY 1 11125800
REL135*       UNTIL   GY-SUB > 999 OR
REL135*               WS-GY-FOUND
REL135*
REL135*       IF WS-GY-FOUND
REL135*        PERFORM B1335-CHK-GY-IDENTIFER VARYING GY-SUB FROM 1 BY 111125800
REL135*        UNTIL   GY-SUB > 999    OR                               11125900
REL135*                FOUND-EITSZP135 OR                               11125900
REL135*                ( FM1F0-REC-TYPE (GY-SUB) = SPACES AND           11125900
REL135*                  FM1F0-SEQ-NUM (GY-SUB) NOT = ZEROS )           11125900
REL135*       END-IF                                                    11125900
REL135*    END-IF                                                       11125900
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
REL135*    PERFORM B1340-SET-GY-IDENTIFER VARYING GY-SU1 FROM 1 BY 1    11125800
REL135*    UNTIL   GY-SU1 > 999 OR
REL135*            WS-GY1-FOUND
REL135*
REL135*    IF WS-GY1-FOUND
REL135*       PERFORM B1340-CHK-GY-IDENTIFER VARYING GY-SU1 FROM 1 BY 1 11125800
REL135*       UNTIL   GY-SU1 > 999    OR                                11125900
REL135*               FOUND-EITSZP135 OR                                11125900
REL135*               ( FM1F1-REC-TYPE (GY-SU1) = SPACES AND            11125900
REL135*                 FM1F1-SEQ-NUM (GY-SU1) NOT = ZEROS )            11125900
REL135*    END-IF                                                       11125900
REL135***CQ#420369 END

                                                                        04820025
      *    PERFORM B1800-EDIT-NON-PPO                                   04830025
048301                                                                  04830100
048307*    PERFORM B1820-EDIT-FLORIDA-FOR-PPO                           04830702
067200                                                                  06720001
067811     IF WS-ECRP-NO                                                06781118
043600        PERFORM B1900-EDIT-PROC-CD                                    0436
043700           VARYING F0-SUB FROM 1 BY 1                                 0437
067814*          UNTIL   FOUND-NF02         OR                          06781400
REL101           UNTIL   F0-SUB > 999                                   06781600
067860     END-IF                                                       06786012
067861                                                                  06786101
067867                                                                  06786701
067810***********U602************************************************** 06781012
067811     IF WS-ECRP-NO                                                06781118
067820        PERFORM B1900-EDIT-U602                                   06782018
067830        IF U602-NOT-A-ZERO-PRICED-CLAIM                           06783018
067840           MOVE 'ZP100' TO WS-SCCF-EDIT-ERROR-CD                  06784018
067850           PERFORM B1999-INSERT-ERROR-CODE                        06785018
067860        END-IF                                                    06786018
067860     END-IF                                                       06786012
067870***********U602************************************************** 06787012
067877*REL9.0 START                                                     06787700
REL120*    IF WS-ECRP-NO                                                06787800
REL120*       PERFORM B1950-EDIT-M1015                                  06787900
REL120*       IF 1015-BUNDLED-CLAIM                                     06788000
REL120*          MOVE 'ZP108' TO WS-SCCF-EDIT-ERROR-CD                  06788100
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        06788200
REL120*       END-IF                                                    06788300
REL120*    END-IF                                                       06788400
067885*REL9.0 END                                                       06788500
                                                                        04450008
      ***CR136539 START
           PERFORM B1960-EDIT-NEG-PRC
               VARYING F5-SUB FROM 1 BY 1
REL101         UNTIL   F5-SUB > 999
      ***CR136539 END
      ***CR137498 START
           PERFORM B1970-REMV-ILG-SYB
      ***CR137498 END
ZP115F*    IF FM1A5-REC-TYPE = 'A5' AND
ZP115F*       FM1A5-TRANS-IND = 'P' AND
ZP115F*       FM1A5-ICD-PROV-SUB-VER-IND = '0' AND
REL120*       FM1F0-DT-SERV-START (1) < '20141001'
ZP115F*       FM1F0-DT-SERV-START (1) < '20151001'
ZP115F*       MOVE 'ZP115' TO WS-SCCF-EDIT-ERROR-CD                     09824400
ZP115F*       PERFORM B1999-INSERT-ERROR-CODE                           09824500
ZP115F*       SET FOUND-EITSZP115    TO TRUE                            15481603
ZP115F*    END-IF
ICDFIX*    IF FM1A5-TRANS-IND = 'P' AND
ICDFIX*       FM1A5-ICD-PROV-SUB-VER-IND NOT = '0'  AND
ICDFIX*       FM1F0-DT-SERV-START (1) >= '20151001'
ICDFIX*       MOVE 'ZP116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX*       PERFORM B1999-INSERT-ERROR-CODE
ICDFIX*       SET FOUND-EITSZP116    TO TRUE
ICDFIX*    END-IF
      *
CQ8919     IF FM1A5-TRANS-IND = 'P' AND
CQ8919        FM1F0-DT-SERV-START (1) >= '20151001'
ICDFIX        EVALUATE TRUE
ICDFIX         WHEN FM1A5-ICD-PROV-SUB-VER-IND = '9'
ICDFIX              MOVE 'ZP116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX              PERFORM B1999-INSERT-ERROR-CODE
ICDFIX              SET FOUND-EITSZP116    TO TRUE
ICDFIX         WHEN FM1A5-ICD-PROV-SUB-VER-IND = (SPACES OR
ICDFIX                LOW-VALUES)
ICDFIX              IF FM1E2-ICD-10-DIAG-CDS = (SPACES OR
ICDFIX                LOW-VALUES)
ICDFIX                MOVE 'ZP116' TO WS-SCCF-EDIT-ERROR-CD
ICDFIX                PERFORM B1999-INSERT-ERROR-CODE
ICDFIX                SET FOUND-EITSZP116    TO TRUE
ICDFIX              ELSE
ICDFIX                 MOVE '0' TO FM1A5-ICD-PROV-SUB-VER-IND
ICDFIX                 DISPLAY "ICD-10 SF PROF:" FM1A5-SCCF-SER-NUM
ICDFIX              END-IF
ICDFIX         WHEN FM1A5-ICD-PROV-SUB-VER-IND = '0'
ICDFIX              CONTINUE
ICDFIX        END-EVALUATE
REL120     END-IF
           .                                                            04440008
      ******************************************************************04460002
       B1020-EDIT-PLAN-PAYER-CD.                                        04570002
      ******************************************************************04580002
                                                                        04590002
           MOVE FM1A5-PMT-DISP-CD TO WS-PMT-DISP-CD                     04600002
                                                                        04610002
           IF FM1A5-PROCESSING-PLAN-PAYER                               04620006
045700*       IF WS-PAY-PROV OR WS-PAY-REP-PAYEE                        04520051
045800        IF WS-PAY-PROV OR                                         04530051
045900         ((WS-PAY-REP-PAYEE) AND                                  04540051
046000                 (FM1A5-SER-NUM-JULIAN-DATE < '2001134'))         04550051
                 MOVE 'ZP007' TO WS-SCCF-EDIT-ERROR-CD                  04640008
                 PERFORM B1999-INSERT-ERROR-CODE                        04650006
REL120           SET FOUND-EITSZP007    TO TRUE                         15481603
              END-IF                                                    04660006
           END-IF                                                       04670006
046900                                                                  04690026
067908     IF FM1A5-INTL-CD NOT = 'I'                                   06790800
047100        IF FM1A5-LOC-PLAN-PAYER AND                               04710026
047200           WS-PAY-SUB                                             04720026
047300           MOVE 'ZP099' TO WS-SCCF-EDIT-ERROR-CD                  04730026
047400           PERFORM B1999-INSERT-ERROR-CODE                        04740026
REL120           SET FOUND-EITSZP099    TO TRUE                         15481603
047500        END-IF                                                    04750026
067914     END-IF                                                       06791400
           .                                                            04680002
                                                                        04690002
      ******************************************************************04700002
       B1040-EDIT-RESUBMISSION-IND.                                     04870002
      ******************************************************************04880002
                                                                        04890002
           IF FM1A5-ORIGINAL-RESUBMISSION   OR                          04900013
              FM1A5-ADJUSTMENT-RESUBMISSION OR                          04910002
              FM1A5-STORE-FRONT                                         04920002
              MOVE 'ZP017' TO WS-SCCF-EDIT-ERROR-CD                     04930008
      *CR 145948 BEGIN
              SET RESUBMISSION-YES        TO TRUE
      *CR 145948 END
              PERFORM B1999-INSERT-ERROR-CODE                           04940002
REL120        SET FOUND-EITSZP017    TO TRUE                            15481603
           END-IF                                                       04950002
           .                                                            04960002
                                                                        04970002
      ******************************************************************04980002
       B1050-MAP-LAST-ACT-DT.                                           04990002
      ******************************************************************05000002
                                                                        05010002
056401     MOVE SIAPC-GREG-CURRENT-CENTURY TO FM1A5-ONL-DT-LAST-ACT-CC  06320024
056402     MOVE SIAPC-GREG-CURRENT-YEAR    TO FM1A5-ONL-DT-LAST-ACT-YY  06330024
056403     MOVE SIAPC-GREG-CURRENT-MONTH   TO FM1A5-ONL-DT-LAST-ACT-MM  06340024
056410     MOVE SIAPC-GREG-CURRENT-DAY     TO FM1A5-ONL-DT-LAST-ACT-DD  06350024
           .                                                            05060002
                                                                        05070002
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
      ******************************************************************20996404
REL135*B1335-SET-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM1F0-HCPCS-PROC-CD-MOD-1(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-2(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-3(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-4(GY-SUB) = 'GY'
REL135*          SET WS-GY-FOUND TO TRUE                                13506700
REL135*    END-IF
REL135*    .
      ******************************************************************20996404
REL135*B1335-CHK-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM1F0-HCPCS-PROC-CD-MOD-1(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-2(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-3(GY-SUB) = 'GY' OR
REL135*       FM1F0-HCPCS-PROC-CD-MOD-4(GY-SUB) = 'GY'
REL135*       CONTINUE                                                  13506700
REL135*    ELSE                                                         13506700
REL135*          MOVE 'ZP135' TO WS-SCCF-EDIT-ERROR-CD
REL135*          PERFORM B1999-INSERT-ERROR-CODE
REL135*          SET FOUND-EITSZP135    TO TRUE                         15481603
REL135*    END-IF
REL135*    .
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START END
REL135******************************************************************16170048
REL135*B1340-SET-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM1F1-HCPCS-PROC-CD-MOD-1(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-2(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-3(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-4(GY-SU1) = 'GY'
REL135*          SET WS-GY1-FOUND TO TRUE                               13506700
REL135*    END-IF
REL135*    .
REL135******************************************************************16170048
REL135*B1340-CHK-GY-IDENTIFER.                                          16160048
REL135******************************************************************16170048
REL135*    IF FM1F1-HCPCS-PROC-CD-MOD-1(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-2(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-3(GY-SU1) = 'GY' OR
REL135*       FM1F1-HCPCS-PROC-CD-MOD-4(GY-SU1) = 'GY'
REL135*       CONTINUE                                                  13506700
REL135*    ELSE                                                         13506700
REL135*          MOVE 'ZP135' TO WS-SCCF-EDIT-ERROR-CD
REL135*          PERFORM B1999-INSERT-ERROR-CODE
REL135*          SET FOUND-EITSZP135    TO TRUE                         15481603
REL135*    END-IF
REL135*    .
      ******************************************************************05080002
REL120*B1110-EDIT-FEDERAL-TAX-ID-NUM.                                   05580002
      ******************************************************************05590001
REL120*                                                                 05600001
REL120*     IF FM1B0-FED-TAX-ID-NUM = (SPACES OR LOW-VALUES)            05500026
REL120*        SET  FOUND-EITSZP002  TO TRUE                            09910033
REL120*        MOVE 'ZP002' TO WS-SCCF-EDIT-ERROR-CD                    05510026
REL120*        PERFORM B1999-INSERT-ERROR-CODE                          05520026
REL120*     END-IF                                                      05530026
REL120*    .                                                            05650001
REL120*
      ***CR117967 START
      ******************************************************************05570001
REL120*B1180-EDIT-LOC-PLAN-800.                                         05580002
      ******************************************************************05590001
REL120*                                                                 05600001
REL120*     IF WS-ECRP-PLUS AND FM1A5-LOC-PLAN-CD = '800'               05500026
REL120*        SET LOC-PLAN-800 TO TRUE
REL120*        MOVE 'ZP110' TO WS-SCCF-EDIT-ERROR-CD                    09910033
REL120*        PERFORM B1999-INSERT-ERROR-CODE                          05510026
REL120*     END-IF                                                      05520026
REL120*    .                                                            05650001
      ***CR117967 END
      *** CR#148645 START
      ******************************************************************20996404
       B1300-CHK-HIGH-DOLLAR.                                           16160048
      ******************************************************************16170048
           IF FM1F0-SERV-CHRG (F0-SUB) > WS-MAX-SERV-CHRG AND
              FM1F0-NUM-SERV (F0-SUB) = 1
              MOVE 'Y'     TO WS-HIGH-DOLLAR-SW
              MOVE 'ZP113' TO WS-SCCF-EDIT-ERROR-CD
              PERFORM B1999-INSERT-ERROR-CODE
REL120        SET FOUND-EITSZP113    TO TRUE                            15481603
           END-IF                                                       13506700
           .
      *** CR#148645 END
      ******************************************************************06120001
REL120*B1400-EDIT-DOS.                                                  06130002
      ******************************************************************06140001
REL120*                                                                 06150001
REL120*    IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  06151017
REL101*        FM1F0-SEQ-NUM (F0-SUB) NUMERIC      AND                  06152017
REL120*        FM1F0-SEQ-NUM (F0-SUB) NOT = ZEROES                      06153017
REL120*       MOVE FM1F5-RULE-NUM-LN (F0-SUB) TO WS-RULE-NUMBER         06160016
REL120*       IF FM1F0-DT-SERV-START (F0-SUB) NOT NUMERIC               06180016
REL120*          CONTINUE                                               06190016
REL120*       ELSE                                                      06200016
REL120*          MOVE FM1F0-DT-SERV-START-CC (F0-SUB)                   06210016
REL120*                                   TO WS-DT-SERV-START-CC        06220016
REL120*          MOVE FM1F0-DT-SERV-START-YY (F0-SUB)                   06230016
REL120*                                   TO WS-DT-SERV-START-YY        06240016
REL120*          IF WS-DT-SERV-START-CCYY < '1996'    AND               06250016
REL120*             WS-INV-RULE-NUM                                     06260016
REL120*             MOVE 'Y'  TO   WS-EITSZP093-SW                      06270016
REL120*             MOVE 'ZP093' TO WS-SCCF-EDIT-ERROR-CD               06280016
REL120*             PERFORM B1999-INSERT-ERROR-CODE                     06290016
REL120*          END-IF                                                 06300016
REL120*       END-IF                                                    06310016
REL120*    END-IF                                                       06313016
REL120*    .                                                            06320001
                                                                        06330001
      ******************************************************************06340001
REL120*B1450-LEASED-NET.                                                08842705
088428******************************************************************08842805
REL120*                                                                 08842905
REL120*    SET FOUND-LEASED-NET TO TRUE                                 08843005
REL120*    MOVE 'ZP104' TO WS-SCCF-EDIT-ERROR-CD                        08843105
REL120*    PERFORM B1999-INSERT-ERROR-CODE                              08843205
REL120*    .                                                            08843305
REL120*                                                                 08843405
      ******************************************************************07250021
       B1700-EDIT-PERF-PROV-SPEC.                                       07260021
      ******************************************************************07270001
                                                                        07280021
           IF FM1E0-PERF-PROV-SPEC-CLM = (SPACES OR LOW-VALUES)         07290022
              PERFORM B1705-CHECK-PERF-PROV-SPEC-LN                     07300022
                VARYING F0-SUB FROM 1 BY 1                              07310022
                  UNTIL  PERF-PROV-SPEC-MISSING                         07320023
REL101               OR  F0-SUB > 999                                   07330023
REL120*       IF PERF-PROV-SPEC-MISSING AND WS-ECRP-NO                  06900028
REL120*          MOVE 'ZP010' TO WS-SCCF-EDIT-ERROR-CD                  07350022
REL120*          PERFORM B1999-INSERT-ERROR-CODE                        07360022
REL120*       END-IF                                                    07370022
069310     ELSE                                                         06931028
069320        MOVE FM1E0-PERF-PROV-SPEC-CLM TO WS-PERF-PROV-SPEC        06932028
069400     END-IF                                                       06940028
069510                                                                  06951028
069520     IF WS-ECRP-YES                                               06952028
069523        IF FM1A5-CNTL-PLAN-CD   = '243' OR                        06952328
069524           FM1A5-CNTL-PLAN-CD   = '803' OR                        06952428
069530           FM1D0-SUB-ID-PFX-1-3 = 'YLG'                           06953028
069531           CONTINUE                                               06953128
069532        ELSE                                                      06953228
069540           IF FM1A5-PMT-DISP-CD     = '2'    AND                  06954028
069550              WS-PERF-PROV-SPEC NOT = SPACES                      06955028
069552              MOVE 'SF000' TO FM1B0-BCBS-PROV-NUM                 06955231
069560              PERFORM B1710-CONVERT-PROV-SPEC                     06956028
069561                 VARYING PROV-SUB FROM 1 BY 1                     06956128
069562                  UNTIL  PROV-SPEC-FOUND                          06956228
101100                     OR  PROV-SUB > 436                           13390034
069570           END-IF                                                 06957028
069580        END-IF                                                    06958028
           END-IF                                                       07380022
           .                                                            07390021
                                                                        07400021
      ******************************************************************07410021
       B1705-CHECK-PERF-PROV-SPEC-LN.                                   07420022
      ******************************************************************07430001
                                                                        07440022
           IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  07450023
REL101*       FM1F0-SEQ-NUM  (F0-SUB) NUMERIC      AND                  07030028
070400        FM1F0-SEQ-NUM  (F0-SUB) NOT = ZEROES                      07040028
070500        IF FM1F0-PERF-PROV-SPEC-LN (F0-SUB)                       07050028
                                       = (SPACES OR LOW-VALUES)         07460022
070700           SET PERF-PROV-SPEC-MISSING TO TRUE                     07070028
070710        ELSE                                                      07071028
070720           MOVE FM1F0-PERF-PROV-SPEC-LN (1) TO WS-PERF-PROV-SPEC  07072028
070800        END-IF                                                    07080028
071894     END-IF                                                       07189428
071895     .                                                            07189528
071896                                                                  07189628
071897******************************************************************07189728
071898 B1710-CONVERT-PROV-SPEC.                                         07189828
071899******************************************************************07189928
071900                                                                  07190028
071904     IF WS-PERF-PROV-SPEC = WS-ITS-PROV-SPEC (PROV-SUB)           07190428
071905        MOVE WS-CS90-PROV-SPEC (PROV-SUB) TO                      07190528
071906             FM1B0-BCBS-PROV-NUM                                  07190628
071907        SET PROV-SPEC-FOUND TO TRUE                               07190728
           END-IF                                                       07910022
           .                                                            07920022
                                                                        07930022
      ******************************************************************07940025
      *B1800-EDIT-NON-PPO.                                              07950025
      ******************************************************************07960025
      *                                                                 07970025
071510*    SET PPO-AVAILABLE TO TRUE                                    07151045
071900*    MOVE 'N' TO WS-BLCD-SVC-DT-END                               07190033
      *    SET  PPO-PROV                 TO TRUE                        07980026
      *    MOVE FM1A5-SER-NUM-LOCAL-PLAN TO WS-PLAN-CD                  07990025
      *    MOVE FM1A5-BCP-PROD-TYPE      TO WS-PROD-TYPE                08000025
      *                                                                 08010025
071921*    IF FM1E0-PPO-AVBL = 'N'                                      07192145
071922*       SET PPO-NOT-AVAILABLE TO TRUE                             07192245
071923*    ELSE                                                         07192345
071924*       PERFORM B1830-CHECK-PPO-SPEC                              07192445
071925*          VARYING F0-SUB FROM 1 BY 1                             07192545
REL101*          UNTIL   F0-SUB > 999                                   07192645
071927*    END-IF                                                       07192745
071928*                                                                 07192845
071960*    IF WS-BLUCD-DS-PPO-PFX                                       07196033
071970*       PERFORM B1801-CHECK-DT-SERV-END                           07197033
071980*          VARYING DT-SUB FROM 1 BY 1                             07198033
REL101*          UNTIL   DT-SUB > 999                                   07199033
072000*    END-IF                                                       07200033
072100*                                                                 07210033
080110*    IF FM1E0-PERF-PROV-TYPE-CLM NOT = SPACES                     08011003
080111*       MOVE FM1E0-PERF-PROV-TYPE-CLM TO WS-PROVIDER-TYPE         08011103
080120*    ELSE                                                         08012003
080200*       PERFORM B1805-CHECK-PROVIDER-TYPE                         08020003
080300*           VARYING F0-SUB FROM 1 BY 1                            08030003
REL101*            UNTIL  F0-SUB > 999                                  08040010
080410*    END-IF                                                       08041003
      *                                                                 08050025
080600*    IF FM1E0-CLASS-PROV-CLM NOT = SPACES                         08060010
080601*       MOVE FM1E0-CLASS-PROV-CLM TO WS-CLASS-PROV                08060110
      *    ELSE                                                         08120025
080603*       PERFORM B1810-CHECK-CLASS-PROV                            08060310
080604*          VARYING F0-SUB FROM 1 BY 1                             08060410
REL101*          UNTIL   F0-SUB > 999                                   08060610
080607*    END-IF                                                       08060710
      *                                                                 08160025
080609*    EVALUATE TRUE                                                08060912
073700*       WHEN (WS-BLUECARD-PPO-PFX OR WS-BLCD-SVC-DT-END-Y)  AND   07370041
074100*            (NOT SELECT-PROD-TYPE)                         AND   07410035
073900*            (NOT SELECT-PROV-TYPE)                               07390043
080900*          SET NON-PPO-PROV TO TRUE                               08090010
074100*       WHEN (WS-BLUECARD-PPO-PFX OR WS-NATL-PPO-EPO-PFX    OR    14170033
074200*             WS-BLCD-SVC-DT-END-Y)                         AND   14180033
074300*            WS-VALID-PLAN-CD                               AND   14190033
074400*            WS-NON-PPO-PROV                                      14200033
089315*          PERFORM B1815-CHECK-PRICE-MTD                          14210033
089316*             VARYING F0-SUB FROM 1 BY 1                          14220033
REL101*               UNTIL F0-SUB > 999                                14230033
089318*    END-EVALUATE                                                 08931812
      *                                                                 08210025
074460*    IF NON-PPO-PROV AND                                          07446045
074461*       PPO-NOT-AVAILABLE                                         07446145
089321*       MOVE '5' TO FM1E0-CLASS-PROV-CLM                          08932110
089322*       PERFORM B1899-SPACE-ALL-CLASS-PROV-LN                     08932210
089323*          VARYING F0-SUB FROM 1 BY 1                             08932310
REL101*            UNTIL F0-SUB > 999                                   08932410
      *    END-IF                                                       08280026
      *    .                                                            08290026
      *                                                                 08300025
      ******************************************************************08310026
075300*B1801-CHECK-DT-SERV-END.                                         07530033
075400******************************************************************07540033
075500*                                                                 07550033
075600*    IF FM1F0-REC-TYPE (DT-SUB) NOT = SPACES AND                  07560033
075700*       FM1F0-SEQ-NUM  (DT-SUB) NUMERIC      AND                  07570033
075800*       FM1F0-SEQ-NUM  (DT-SUB) NOT = ZEROES                      07580033
075900*         IF FM1F0-DT-SERV-END (DT-SUB) > '20000630'              07590036
076000*            MOVE 'Y' TO WS-BLCD-SVC-DT-END                       07600033
076010*            MOVE 24 TO DT-SUB                                    07601033
076020*         ELSE                                                    07602033
076021*            MOVE 'N' TO WS-BLCD-SVC-DT-END                       07602133
076022*         END-IF                                                  07602233
076023*    END-IF                                                       07602333
076024*    .                                                            07602433
076025*                                                                 07602533
076026******************************************************************07602633
      *B1805-CHECK-PROVIDER-TYPE.                                       08320026
      ******************************************************************08330026
      *                                                                 08340026
      *    IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  08350026
089334*       FM1F0-SEQ-NUM (F0-SUB) NUMERIC       AND                  08933410
089335*       FM1F0-SEQ-NUM (F0-SUB) NOT = ZEROES                       08933510
089336*       MOVE FM1F0-PERF-PROV-TYPE-LN (F0-SUB) TO WS-PROVIDER-TYPE 08933610
089337*       IF NOT SELECT-PROV-TYPE                                   08933710
089338*          MOVE 24 TO F0-SUB                                      08933810
089339*       END-IF                                                    08933910
      *    ELSE                                                         08430026
089341*       MOVE 24 TO F0-SUB                                         08934110
      *    END-IF                                                       08450026
      *    .                                                            08460026
      *                                                                 08470026
      ******************************************************************08480025
      *B1810-CHECK-CLASS-PROV.                                          08490026
      ******************************************************************08500025
      *                                                                 08510026
      *    IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  08530026
089355*       FM1F0-SEQ-NUM (F0-SUB) NUMERIC       AND                  08935510
089356*       FM1F0-SEQ-NUM (F0-SUB) NOT = ZEROES                       08935610
089357*       MOVE FM1F0-CLASS-PROV-LN (F0-SUB) TO WS-CLASS-PROV        08935710
089359*       IF WS-NON-PPO-PROV                                        08935910
089360*          MOVE 24 TO F0-SUB                                      08936010
089362*       END-IF                                                    08936210
089363*    ELSE                                                         08936310
089364*       MOVE 24 TO F0-SUB                                         08936410
      *    END-IF                                                       08610026
      *    .                                                            08620026
      *                                                                 08630026
      ******************************************************************08640026
089369*B1815-CHECK-PRICE-MTD.                                           14840033
      ******************************************************************08660026
                                                                        08670026
      *    IF FM1F5-REC-TYPE (F0-SUB) NOT = SPACES AND                  14870033
089373*       FM1F5-SEQ-NUM (F0-SUB) NUMERIC       AND                  14880033
089374*       FM1F5-SEQ-NUM (F0-SUB) NOT = ZEROES                       14890033
089375*       MOVE FM1F5-PRC-MTD-LN (F0-SUB) TO WS-PRICING-MTD          14900033
      *       EVALUATE TRUE                                             14910033
089377*          WHEN WS-PCT-OF-CHRGS                                   14920033
089378*             IF FM1F5-PCT-FCTR-LN (F0-SUB) NOT = 10000           14930033
089379*                SET NON-PPO-PROV  TO TRUE                        14940033
089380*             END-IF                                              14950033
089381*          WHEN WS-CHRGS                                          14960033
080600*          WHEN WS-NO-PRICING-MTD                                 14970033
089382*             CONTINUE                                            14980033
089383*          WHEN OTHER                                             14990033
089384*             SET NON-PPO-PROV     TO TRUE                        15000033
      *       END-EVALUATE                                              15010033
089386*    ELSE                                                         15020033
089387*       MOVE 24 TO F0-SUB                                         15030033
      *    END-IF                                                       15040033
      *    .                                                            15050033
      *                                                                 08850026
      ******************************************************************08860025
089393*B1820-EDIT-FLORIDA-FOR-PPO.                                      08939300
      ******************************************************************08880025
      *                                                                 08890025
080700*    IF FM1A5-LOC-PLAN-CD = '090' AND                             08070046
080710*       WS-ECRP-NO                                                08071046
089399*       INITIALIZE WS-PPO-FL-PROV-IND                             08939902
089400*       MOVE FM1D0-SUB-ID-PFX-1-3 TO WS-SUB-ID-PFX-1-3            08940002
089402*       PERFORM B1821-BCBS-PROV-NUM                               08940202
089403*       IF WS-FL-NATL-ACCT AND                                    08940302
089404*          WS-PPO-FL-PROV                                         08940402
089405*          PERFORM B1822-CHECK-DT-SERV                            08940502
089406*             VARYING DT-SUB FROM 1 BY 1                          08940603
REL101*             UNTIL   DT-SUB > 999                                08940703
089408*       END-IF                                                    08940802
089409*    END-IF                                                       08940902
089427*    .                                                            08942700
089428*                                                                 08942800
089429******************************************************************08942902
089430*B1821-BCBS-PROV-NUM.                                             08943002
089431******************************************************************08943102
089432*                                                                 08943202
089433*    EVALUATE FM1B0-BCBS-PROV-NUM (1:1)                           08943302
089434*       WHEN '0'                                                  08943402
089435*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-0        08943502
089436*          IF WS-PPO-FL-PROV-0                                    08943602
089437*             SET WS-PPO-FL-PROV TO TRUE                          08943702
089438*          END-IF                                                 08943802
089439*       WHEN '1'                                                  08943902
089440*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-1        08944002
089441*          IF WS-PPO-FL-PROV-1                                    08944102
089442*             SET WS-PPO-FL-PROV TO TRUE                          08944202
089443*          END-IF                                                 08944302
089444*       WHEN '2'                                                  08944402
089445*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-2        08944502
089446*          IF WS-PPO-FL-PROV-2                                    08944602
089447*             SET WS-PPO-FL-PROV TO TRUE                          08944702
089448*          END-IF                                                 08944802
089449*       WHEN '3'                                                  08944902
089450*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-3        08945002
089451*          IF WS-PPO-FL-PROV-3                                    08945102
089452*             SET WS-PPO-FL-PROV TO TRUE                          08945202
089453*          END-IF                                                 08945302
089454*       WHEN '4'                                                  08945402
089455*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-4        08945502
089456*          IF WS-PPO-FL-PROV-4                                    08945602
089457*             SET WS-PPO-FL-PROV TO TRUE                          08945702
089458*          END-IF                                                 08945802
089459*       WHEN '5'                                                  08945902
089460*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-5        08946002
089461*          IF WS-PPO-FL-PROV-5                                    08946102
089462*             SET WS-PPO-FL-PROV TO TRUE                          08946202
089463*          END-IF                                                 08946302
089464*       WHEN '6'                                                  08946402
089465*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-6        08946502
089466*          IF WS-PPO-FL-PROV-6                                    08946602
089467*             SET WS-PPO-FL-PROV TO TRUE                          08946702
089468*          END-IF                                                 08946802
089469*       WHEN '7'                                                  08946902
089470*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-7        08947002
089471*          IF WS-PPO-FL-PROV-7                                    08947102
089472*             SET WS-PPO-FL-PROV TO TRUE                          08947202
089473*          END-IF                                                 08947302
089474*       WHEN '8'                                                  08947402
089475*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-8        08947502
089476*          IF WS-PPO-FL-PROV-8                                    08947602
089477*             SET WS-PPO-FL-PROV TO TRUE                          08947702
089478*          END-IF                                                 08947802
089479*       WHEN '9'                                                  08947902
089480*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-9        08948002
089481*          IF WS-PPO-FL-PROV-9                                    08948102
089482*             SET WS-PPO-FL-PROV TO TRUE                          08948202
089483*          END-IF                                                 08948302
089484*       WHEN OTHER                                                08948406
089485*          MOVE FM1B0-BCBS-PROV-NUM  TO WS-BCBS-PROV-NUM-X        08948506
089486*          IF WS-PPO-FL-PROV-X                                    08948606
089487*             SET WS-PPO-FL-PROV TO TRUE                          08948706
089488*          END-IF                                                 08948806
089484*    END-EVALUATE                                                 08948404
089485*    .                                                            08948502
089486*                                                                 08948602
089487******************************************************************08948702
089488*B1822-CHECK-DT-SERV.                                             08948802
089489******************************************************************08948902
089491*                                                                 08949102
089492*    IF FM1F0-REC-TYPE (DT-SUB) NOT = SPACES AND                  08949203
089493*       FM1F0-SEQ-NUM  (DT-SUB) NUMERIC      AND                  08949303
089494*       FM1F0-SEQ-NUM  (DT-SUB) NOT = ZEROES                      08949403
089495*       EVALUATE TRUE                                             08949502
089496*          WHEN FM1F0-DT-SERV-START-CC (DT-SUB) = '19' AND        08949603
089497*               FM1F0-DT-SERV-START-YY (DT-SUB) = '99' AND        08949703
089300*              (FM1F0-DT-SERV-START-MM (DT-SUB) = '10' OR         08930046
089310*               FM1F0-DT-SERV-START-MM (DT-SUB) = '11' OR         08931046
089320*               FM1F0-DT-SERV-START-MM (DT-SUB) = '12')           08932046
089500*          WHEN FM1F0-DT-SERV-END-CC   (DT-SUB) = '19' AND        08950003
089501*               FM1F0-DT-SERV-END-YY   (DT-SUB) = '99' AND        08950103
089430*              (FM1F0-DT-SERV-END-MM   (DT-SUB) = '10' OR         08943046
089431*               FM1F0-DT-SERV-END-MM   (DT-SUB) = '11' OR         08943146
089432*               FM1F0-DT-SERV-END-MM   (DT-SUB) = '12')           08943246
089504*             MOVE '5' TO FM1E0-CLASS-PROV-CLM                    08950402
089505*             PERFORM B1899-SPACE-ALL-CLASS-PROV-LN               08950503
089506*                 VARYING F0-SUB FROM 1 BY 1                      08950603
REL101*                     UNTIL F0-SUB > 999                          08950703
089508*             MOVE 24 TO DT-SUB                                   08950803
089509*       END-EVALUATE                                              08950902
089510*    ELSE                                                         08951002
089511*       MOVE 24 TO DT-SUB                                         08951103
089512*    END-IF                                                       08951202
089513*    .                                                            08951302
089514*                                                                 08951402
089515******************************************************************08951503
089507*B1830-CHECK-PPO-SPEC.                                            08950745
089508******************************************************************08950837
089509*    IF FM1F5-PPO-PRV-TYP-AVL-LN (F0-SUB) NOT = SPACES AND        08950945
089510*       FM1F5-PPO-PRV-TYP-AVL-LN (F0-SUB) NOT = LOW-VALUES        08951045
089511*       IF FM1F5-PPO-PRV-TYP-AVL-LN (F0-SUB) = 'N'                08951145
089512*          SET PPO-NOT-AVAILABLE TO TRUE                          08951245
089513*          MOVE 24 TO F0-SUB                                      08951345
089514*       END-IF                                                    08951445
089515*    END-IF                                                       08951545
089517*    .                                                            08951745
089518*                                                                 08951845
150624***CR#98645 ADDED BEGIN                                           15062401
089519******************************************************************08951945
089516*B1899-SPACE-ALL-CLASS-PROV-LN.                                   08951603
089517******************************************************************08951703
089518*                                                                 08951803
089518*    IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  08900025
089524*       FM1F0-SEQ-NUM (F0-SUB) NUMERIC       AND                  08952408
089521*       FM1F0-SEQ-NUM (F0-SUB) NOT = ZEROES                       08952105
089522*       MOVE ' '          TO FM1F0-CLASS-PROV-LN (F0-SUB)         08952205
089518*    ELSE                                                         07520022
089524*       MOVE 24 TO F0-SUB                                         08952405
089518*    END-IF                                                       06803016
089518*    .                                                            06810001
089518*                                                                 06820001
      ******************************************************************06830001
089520 B1900-EDIT-PROC-CD.                                              08952042
089534******************************************************************08953414
089522                                                                  08952242
089523     IF FM1F0-REC-TYPE (F0-SUB) NOT = SPACES AND                  08952342
REL101*       FM1F0-SEQ-NUM  (F0-SUB) NUMERIC      AND                  08952442
089525        FM1F0-SEQ-NUM  (F0-SUB) NOT = ZEROES                      08952542
089526        MOVE FM1F0-HCPCS-PROC-CD (F0-SUB) TO WS-PROC-CD           08952642
101411***CR85844,CR89726                                                10141102
101412        EVALUATE WS-PROC-CD                                       10141202
101413           WHEN  '99070'                                          10141310
101414              SET  HCPCS-FOUND        TO TRUE                     10141402
101415           WHEN  'E1399'                                          10141510
123410              PERFORM B1910-CHECK-SERV-CHRG                       12341009
101417           WHEN  'A4649'                                          10141710
101418              SET  HCPCS-A4649-FOUND  TO TRUE                     10141802
101419        END-EVALUATE                                              10141902
101420                                                                  10142002
101415        IF ( NOT PROC-CD-99070 ) AND ( WS-PROC-CD NOT = SPACES )  10141500
101416           SET  HCPCS-OTH    TO TRUE                              10141600
101417        END-IF                                                    10141700
101429                                                                  10142902
101421        IF HCPCS-FOUND AND HCPCS-OTH                              10142100
089528           SET FOUND-NF02 TO TRUE                                 08952842
                 MOVE 'ZP103' TO WS-SCCF-EDIT-ERROR-CD
101433           PERFORM B1999-INSERT-ERROR-CODE                        10143300
REL101           MOVE 999     TO   F0-SUB                               10143511
101436        END-IF                                                    10143600
101455                                                                  10145503
101456        IF HCPCS-A4649-FOUND                                      10145612
101457           MOVE 'ZP107' TO WS-SCCF-EDIT-ERROR-CD                  10145701
                 PERFORM B1999-INSERT-ERROR-CODE
REL101           MOVE 999     TO   F0-SUB                               10146011
089530        END-IF                                                    08953042
101462***CR89726                                                        10146201
089531     ELSE                                                         08953142
REL101        MOVE 999 TO F0-SUB                                        08953242
089533     END-IF                                                       08953342
089534     .                                                            08953442
089559                                                                  08955914
126901*****CR#120565 START                                              12690109
126902******************************************************************12690209
126903 B1910-CHECK-SERV-CHRG.                                           12690309
126904******************************************************************12690409
126920     IF FM1F0-SERV-CHRG (F0-SUB) > +499.99                        12692009
126921        SET HCPCS-E1399-FOUND TO TRUE                             12692109
126930        SET SERV-CHG-GTEQ-500 TO TRUE                             12693009
126950        MOVE 'ZP106' TO WS-SCCF-EDIT-ERROR-CD                     12695009
126960        PERFORM B1999-INSERT-ERROR-CODE                           12696009
REL101        MOVE 999     TO   F0-SUB                                  12697009
126980     END-IF                                                       12698009
126990     .                                                            12699009
126991*****CR#120565 END                                                12699109
126992                                                                  12699209
133110***********U602************************************************** 13311012
089560******************************************************************08956014
133130 B1900-EDIT-U602.                                                 13313012
133140******************************************************************13314012
133150                                                                  13315012
133160     IF FM1E1-SF-MSG-CD-CLM (1) = 'U602' OR                       13316015
133170        FM1E1-SF-MSG-CD-CLM (2) = 'U602' OR                       13317015
133180        FM1E1-SF-MSG-CD-CLM (3) = 'U602' OR                       13318015
133190        FM1E1-SF-MSG-CD-CLM (4) = 'U602' OR                       13319015
133191        FM1E1-SF-MSG-CD-CLM (5) = 'U602'                          13319115
133192        MOVE 'Y' TO WS-U602-CLAIM-LVL-IND                         13319215
133193     END-IF                                                       13319315
133194                                                                  13319415
133195     PERFORM B1920-U602-LINE                                      13319515
133196             VARYING F5-SUB FROM 1 BY 1                           13319615
REL101             UNTIL F5-SUB > 999 OR                                13319715
133198                   U602-NOT-A-ZERO-PRICED-CLAIM                   13319815
133221     .                                                            13322112
133222                                                                  13322212
133273******************************************************************13327312
133274 B1920-U602-LINE.                                                 13327412
133275******************************************************************13327512
133281     IF FM1F5-REC-TYPE (F5-SUB) NOT = SPACES AND                  13328112
REL101*       FM1F5-SEQ-NUM (F5-SUB) NUMERIC AND                        13328212
133283        FM1F5-SEQ-NUM (F5-SUB) NOT = ZEROES                       13328312
133284        IF FM1F5-SF-MSG-CD-LN (F5-SUB 1) = 'U602' OR              13328412
133285           FM1F5-SF-MSG-CD-LN (F5-SUB 2) = 'U602' OR              13328512
133286           FM1F5-SF-MSG-CD-LN (F5-SUB 3) = 'U602' OR              13328612
133287           FM1F5-SF-MSG-CD-LN (F5-SUB 4) = 'U602' OR              13328712
133288           FM1F5-SF-MSG-CD-LN (F5-SUB 5) = 'U602' OR              13328815
133289           U602-CLAIM-LVL                                         13328915
133290           PERFORM B1921-CHECK-ZERO-PRICED-LN                     13329012
133291        END-IF                                                    13329112
133292     END-IF                                                       13329212
133293     .                                                            13329312
133294                                                                  13329412
133295******************************************************************13329512
133296 B1921-CHECK-ZERO-PRICED-LN.                                      13329612
133297******************************************************************13329712
133298     EVALUATE FM1F5-PRC-MTD-LN (F5-SUB)                           13329812
133299        WHEN '01'                                                 13329912
133300           SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE               13330012
133301        WHEN '10'                                                 13330112
133302           IF FM1F5-PCT-FCTR-LN(F5-SUB) IS NUMERIC                13330212
133303              IF FM1F5-PCT-FCTR-LN(F5-SUB) NOT = 00000            13330312
133304                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         13330412
133305              END-IF                                              13330512
133306           END-IF                                                 13330612
133307        WHEN '14'                                                 13330712
133308        WHEN '15'                                                 13330814
133309           IF FM1F5-PCT-FCTR-LN(F5-SUB) IS NUMERIC                13330914
133310              IF FM1F5-PCT-FCTR-LN(F5-SUB) NOT = 00000            13331014
133311                 IF FM1F5-SERV-ALLW-AMT (F5-SUB) IS NUMERIC       13331114
133312                    IF FM1F5-SERV-ALLW-AMT (F5-SUB) > ZEROES      13331214
133313                       SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE   13331314
133314                    END-IF                                        13331414
133315                 END-IF                                           13331514
133316              END-IF                                              13331614
133317           END-IF                                                 13331714
133328        WHEN '40'                                                 13332814
133329        WHEN '41'                                                 13332914
133330        WHEN '42'                                                 13333014
133331        WHEN '43'                                                 13333114
133332           IF FM1F5-SERV-ALLW-AMT (F5-SUB) IS NUMERIC             13333214
133340              IF FM1F5-SERV-ALLW-AMT (F5-SUB) > ZEROES            13334013
133350                 SET U602-NOT-A-ZERO-PRICED-CLAIM TO TRUE         13335013
133351              END-IF                                              13335113
133352           END-IF                                                 13335213
133371     END-EVALUATE                                                 13337113
133372     .                                                            13337213
133373                                                                  13337313
133515***********U602************************************************** 13351513
150792                                                                  15079200
150793*****REL9.0 START*************************************************15079300
REL120*B1950-EDIT-M1015.                                                15079400
150795******************************************************************15079500
REL120*    IF FM1E1-REC-TYPE  NOT = SPACES AND                          15079600
REL101*       FM1E1-SEQ-NUM   NUMERIC AND                               15079700
REL120*       FM1E1-SEQ-NUM   NOT = ZEROES                              15079800
REL120*                                                                 15079900
REL120*       IF FM1E1-SF-MSG-CD-CLM (1) = '1015' OR                    15080000
REL120*          FM1E1-SF-MSG-CD-CLM (2) = '1015' OR                    15080100
REL120*          FM1E1-SF-MSG-CD-CLM (3) = '1015' OR                    15080200
REL120*          FM1E1-SF-MSG-CD-CLM (4) = '1015' OR                    15080300
REL120*          FM1E1-SF-MSG-CD-CLM (5) = '1015'                       15080400
REL120*          MOVE 'Y' TO WS-1015-BUNDLED-CLAIM                      15080500
REL120*       END-IF                                                    15080600
REL120*    END-IF                                                       15080700
REL120*    .                                                            15080800
150809*****REL9.0 END***************************************************15080900
      *
      ***CR136539 START
      ******************************************************************
       B1960-EDIT-NEG-PRC.
      ******************************************************************
           IF FM1F5-REC-TYPE (F5-SUB) NOT = SPACES AND
REL101*       FM1F5-SEQ-NUM (F5-SUB) NUMERIC      AND
              FM1F5-SEQ-NUM (F5-SUB) NOT = ZEROES
             IF (FM1F5-SERV-ALLW-AMT (F5-SUB)  IS NUMERIC AND
                 FM1F5-SERV-ALLW-AMT (F5-SUB) < ZEROES)
                   SET NEG-PRC           TO TRUE
                   MOVE 'ZP111' TO WS-SCCF-EDIT-ERROR-CD
                   PERFORM B1999-INSERT-ERROR-CODE
             END-IF
           END-IF
      *****************************************************************
      ***CR136539 END
           .
      ***CR137498 START
133516******************************************************************13351613
       B1970-REMV-ILG-SYB.
      ******************************************************************
              INSPECT FM1D0-SUB-NAME REPLACING ALL '¬' BY ' '
           IF FM1C0-REC-TYPE NOT = SPACES AND
REL101*        FM1C0-SEQ-NUM NUMERIC      AND
               FM1C0-SEQ-NUM NOT = ZEROES
                 INSPECT FM1C0-PAT-NAME REPLACING ALL '¬' BY  ' '
      ******************************************************************
      ***CR137498 END
           .
133516******************************************************************13351613
       B1999-INSERT-ERROR-CODE.                                         06840002
      ******************************************************************06850001
                                                                        06860001
           IF ERR-SLOT < 6                                              06870001
              MOVE WS-SCCF-EDIT-ERROR-CD TO                             06880001
                   FM1A5-ERR-CD (ERR-SLOT)                              06890001
              ADD 1 TO ERR-SLOT                                         06900001
           ELSE                                                         06910001
           IF ERR-SLOT > 5                                              06920001
              MOVE 'ZP999' TO FM1A5-ERR-CD (5)                          06930002
              ADD 1 TO ERR-SLOT                                         06940001
REL120        SET FOUND-EITSZP999    TO TRUE                            15481603
           END-IF                                                       06950001
           END-IF                                                       06960001
                                                                        06970001
           MOVE SPACES TO WS-SCCF-EDIT-ERROR-CD                         06980001
           .                                                            06990001
                                                                        07000001
      ******************************************************************07010001
       B2000-EITSXXXXX-CHK.                                             07020008
      ******************************************************************07030001
                                                                        07040001
089582     EVALUATE TRUE                                                08958216
REL120*       WHEN FOUND-EITSZP093       AND                            08958726
REL120*            NO-ITS-EDIT-ERROR-CD  AND                            08958826
REL120*            WS-ECRP-NO                                           08958926
REL120*          MOVE 'ZP093' TO WS-EITS-ERR                            08958816
REL120*          PERFORM B2010-INSERT-EITSXXXXX                         08958916
089590*       WHEN FOUND-NF02      AND NO-ITS-EDIT-ERROR-CD             08959016
089591*          PERFORM B2020-INSERT-NF02XXXXX                         08959116
              WHEN FOUND-NF02           AND
                   NO-ITS-EDIT-ERROR-CD AND
                   WS-ECRP-NO
                   IF RESUBMISSION-YES
                      CONTINUE
                   ELSE
                      MOVE 'ZP103' TO WS-NF02-ERR
089591                PERFORM B2020-INSERT-NF02XXXXX                    08959116
                   END-IF
136710***********U602************************************************** 13671012
136720        WHEN U602-NOT-A-ZERO-PRICED-CLAIM AND                     13672012
136730             NO-ITS-EDIT-ERROR-CD  AND                            13673012
136740             WS-ECRP-NO                                           13674012
136750           MOVE 'ZP100' TO WS-EITS-ERR                            13675012
136760           PERFORM B2010-INSERT-EITSXXXXX                         13676012
136770***********U602************************************************** 13677012
REL120*       WHEN NO-ITS-EDIT-ERROR-CD  AND                            13678105
REL120*            FOUND-LEASED-NET      AND                            13678205
REL120*            (WS-ECRP-NO OR WS-ECRP-PLUS)                         13678305
REL120*            MOVE 'ZP104' TO WS-EITS-ERR                          13678405
REL120*            PERFORM B2010-INSERT-EITSXXXXX                       13678505
136786        WHEN HCPCS-E1399-FOUND    AND                             13678602
136787             NO-ITS-EDIT-ERROR-CD AND                             13678702
140900             WS-ECRP-NO           AND                             14090004
141000*CR#120565 START                                                  14100004
141100             SERV-CHG-GTEQ-500                                    14110004
141200*CR#120565 END                                                    14120004
                 IF RESUBMISSION-YES
                    CONTINUE
                 ELSE
136789              MOVE 'ZP106' TO WS-NF02-ERR                         13678902
136790              PERFORM B2020-INSERT-NF02XXXXX                      13679002
                 END-IF
136791        WHEN HCPCS-A4649-FOUND    AND                             13679102
136792             NO-ITS-EDIT-ERROR-CD AND                             13679202
136793             WS-ECRP-NO                                           13679302
                  IF RESUBMISSION-YES
                     CONTINUE
                  ELSE
136794               MOVE 'ZP107' TO WS-NF02-ERR                        13679402
136795               PERFORM B2020-INSERT-NF02XXXXX                     13679502
                  END-IF
REL120*       WHEN FOUND-EITSZP002     AND                              18370033
REL120*            NO-ITS-EDIT-ERROR-CD                                 13679706
REL120*            MOVE 'ZP002' TO WS-EITS-ERR                          18390033
REL120*            PERFORM B2010-INSERT-EITSXXXXX                       18400033
136796***CR117967 START                                                 18370033
REL120*       WHEN (LOC-PLAN-800          AND                           13679706
REL120*             NO-ITS-EDIT-ERROR-CD  AND                           18390033
REL120*             WS-ECRP-PLUS)                                       18400033
REL120*            MOVE 'ZP110' TO WS-EITS-ERR
REL120*            PERFORM B2010-INSERT-EITSXXXXX
      ***CR117967 END
184001*REL9.0 START                                                     18400100
REL120*       WHEN 1015-BUNDLED-CLAIM    AND                            18400200
REL120*            NO-ITS-EDIT-ERROR-CD  AND                            18400300
REL120*            WS-ECRP-NO                                           18400400
REL120*            IF RESUBMISSION-YES
REL120*               CONTINUE
REL120*            ELSE
REL120*               MOVE 'ZP108' TO WS-NF02-ERR                       18400500
REL120*               PERFORM B2020-INSERT-NF02XXXXX                    18400600
REL120*            END-IF
184007*REL9.0 END                                                       18400700
      ***CR136539 START
              WHEN (NEG-PRC               AND
                    NO-ITS-EDIT-ERROR-CD)
                  MOVE 'ZP111' TO WS-EITS-ERR
                  PERFORM B2010-INSERT-EITSXXXXX
      ***CR136539 END
ZP115F*       WHEN (FOUND-EITSZP115       AND
ZP115F*             NO-ITS-EDIT-ERROR-CD)
ZP115F*           MOVE 'ZP115' TO WS-EITS-ERR
ZP115F*           PERFORM B2010-INSERT-EITSXXXXX
REL120        WHEN (FOUND-EITSZP007       AND
REL120              NO-ITS-EDIT-ERROR-CD)
REL120            MOVE 'ZP007' TO WS-EITS-ERR
REL120            PERFORM B2010-INSERT-EITSXXXXX
REL120        WHEN (FOUND-EITSZP017       AND
REL120              NO-ITS-EDIT-ERROR-CD)
REL120            MOVE 'ZP017' TO WS-EITS-ERR
REL120            PERFORM B2010-INSERT-EITSXXXXX
REL120        WHEN (FOUND-EITSZP099       AND
REL120              NO-ITS-EDIT-ERROR-CD)
REL120            MOVE 'ZP099' TO WS-EITS-ERR
REL120            PERFORM B2010-INSERT-EITSXXXXX
REL120        WHEN (FOUND-EITSZP113       AND
REL120              NO-ITS-EDIT-ERROR-CD)
REL120            MOVE 'ZP113' TO WS-EITS-ERR
REL120            PERFORM B2010-INSERT-EITSXXXXX
REL120        WHEN (FOUND-EITSZP999       AND
REL120              NO-ITS-EDIT-ERROR-CD)
REL120            MOVE 'ZP999' TO WS-EITS-ERR
REL120            PERFORM B2010-INSERT-EITSXXXXX
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 START
REL135*       WHEN (FOUND-EITSZP135       AND
REL135*             NO-ITS-EDIT-ERROR-CD)
REL135*           MOVE 'ZP135' TO WS-EITS-ERR
REL135*           PERFORM B2010-INSERT-EITSXXXXX
J33055* COMMENTED WITH RELEASE 11/10/17 FOR J33055 END
CQ8919        WHEN (FOUND-EITSZP116 AND (FM1A5-TYPE-SUBM ='1' OR
CQ8919                 FM1A5-TYPE-SUBM ='2') AND
CQ8919              NO-ITS-EDIT-ERROR-CD)
CQ8919            MOVE 'ZP116' TO WS-EITS-ERR
CQ8919            PERFORM B2010-INSERT-EITSXXXXX
089592     END-EVALUATE                                                 08959217
           .                                                            07140001
                                                                        07150001
      ******************************************************************07160001
       B2010-INSERT-EITSXXXXX.                                          07170008
      ******************************************************************07180001
                                                                        07190001
           MOVE SPACES       TO FM1A5-ERR-CDS                           07200001
           MOVE SPACES       TO FM1A5-PROC-SITE-CNTL-NUM                07210001
           MOVE WS-EITS-TEXT TO FM1A5-PROC-SITE-CNTL-NUM                07220001
           .                                                            07230001
                                                                        07240001
      ******************************************************************07250001
089607 B2020-INSERT-NF02XXXXX.                                          08960716
089608******************************************************************08960816
089609                                                                  08960916
           MOVE SPACES       TO FM1A5-ERR-CDS                           07200001
089611     MOVE SPACES       TO FM1A5-PROC-SITE-CNTL-NUM                08961116
089612     MOVE WS-NF02-MSG  TO FM1A5-PROC-SITE-CNTL-NUM                08961216
089614     .                                                            08961416
089615                                                                  08961516
089616******************************************************************08961616
       B3000-DETERMINE-CLAIM-STATUS.                                    07260002
      ******************************************************************07270001
                                                                        07280001
           IF FM1A5-ERR-CDS = SPACES                                    07290001
              MOVE WS-V             TO FM1A5-STAT-CD                    07300001
              MOVE WS-V             TO CIABP-STAT-CD                    07310012
              MOVE CWSMC-CLAIM-REL-FOR-EXTRACT                          07320001
                                    TO SIAPC-RETURN-MESSAGE-CD          07330001
           ELSE                                                         07340001
              MOVE WS-I             TO FM1A5-STAT-CD                    07350001
              MOVE WS-I             TO CIABP-STAT-CD                    07360012
              MOVE CWSMC-FAILED-REL TO SIAPC-RETURN-MESSAGE-CD          07370001
           END-IF                                                       07380001
           .                                                            07390001
                                                                        07400001
       EJECT                                                            07410001
      ******************************************************************07420001
       Z9999-ERROR-ABORT.                                               07430002
      ******************************************************************07440001
                                                                        07450001
           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND              07460001
           MOVE CWSCC-INVALID-FUNCTION TO SIAPC-RETURN-STATUS           07470001
           MOVE CWSMC-INVALID-FUNCTION TO SIAPC-RETURN-MESSAGE-CD       07480001
           .                                                            07490001
                                                                        07500001
      ******************************************************************07510001
      *       E N D   O F   P R O G R A M   I T C P E 0 0 5             07520001
      ******************************************************************07530001
