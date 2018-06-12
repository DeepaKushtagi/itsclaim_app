000100 IDENTIFICATION DIVISION.                                         ITCP9102
000200                                                                  ITCP9102
000300 PROGRAM-ID.  ITCP9102.                                           ITCP9102
000400 AUTHOR.  ITS PROJECT TEAM.                                       ITCP9102
000500 INSTALLATION. ITS CENTER.                                        ITCP9102
000600 DATE-WRITTEN. DEC  04, 2008.                                     00000600
000700 DATE-COMPILED.                                                   ITCP9102
000800 TITLE 'SF EDIT MAIN MODULE FOR RT/MLE AND BATCH'.                00080000
000900                                                                  00000900
001000******************************************************************ITCP9102
001100*                                                                *ITCP9102
001200*             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *ITCP9102
001300*                                                                *ITCP9102
001400*   MODULE:       ITCP9102                                       *ITCP9102
001500*   NAME:         SF EDIT MAIN MODULE                            *00150000
001600*   VERSION:      20.3                                           *00160000
001700*   DATE:         NOV  9, 2017                                   *00170000
001800*                                                                *ITCP9102
001900*   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *ITCP9102
002000*   TYPE:         COMMON                                         *ITCP9102
002100*   FUNCTION:     THIS MODULE IS THE REAL TIME SF MAIN MODULE    *00002100
080900*                                                                *00085100
002300* MODIFICATION  HISTORY:                                         *00002300
      *R11.0.2¦ 03/18/2010 ¦ 17.6 ¦ PD_53472 ¦ JK  ALT NET ACCESS FEE  *
      *         CHANGES                                                *
      *R11.0.2  03/18/2010   17.6   TD_55594   YAS: PASS RT/MLE IND TO *
      *         DETERMINE ENV FOR BOID EXIT ITPEX7S1.                  *
007100*R11.0.2  04/01/2010   17.7   TD_55219   EXL: USE DB2 SEQUENCE   *00710000
007200*         OBJECT TO GENERATE SCCF NUMBER FOR RT/MLE              *00720000
      *R11.0.3  04/26/2010   17.8   PD_57333   EXL: WHEN CHECK FOR DUP *00721000
002548*         VERIFY COUNTERS FOR SF 05 OR A5 RECORDS ON FPCT        *
007300*R11.1  ¦ 05/13/2010 ¦ 17.9 ¦ E_48276  ¦ EXL: SET INIT TRANSM CD *00730000
007400*R11.1  ¦ 05/13/2010 ¦ 17.9 ¦ E_46690  ¦ EXL: STOP PROCESS WHEN  *00740000
007500*         STRUCTURAL ERROR IN 1010 PARA IS FOUND                 *00750000
007510*R11.1  ¦ 05/13/2010 ¦ 17.9 ¦ TD_57006 ¦ DL:  DO NOT EXECUTE     *00751000
007520*         9990- WITHIN PARAGRAPH 1000- TO ISSUE THE ABEND        *00752000
007530*R11.2  ¦ 11/11/2010 ¦ 18.0 ¦ E_32688  ¦ DL:  5010 CHANGES       *00751000
007540*         SET FM170-PROC-DT-ADDL TO CURRENT DATE FOR MLE CLM     *00752000
007550*         IC047/PC047 - ADJUSTMENT MUST HAVE THE SAME CLM TYPE AS*00752000
007560*           ORIGINAL                                             *00752000
      *R11.2  ¦ 11/11/2010 ¦ 18.0 ¦ PD_61223 ¦ YAS: INITIALIZE PASSING *
      *         WHEN CALLING THE PLAN STATION TABLE.                   *
007590*R11.3  ¦ 05/19/2011 ¦ 18.1 ¦E_10097   ¦DSL: CODESET ALIGNMENT   *00180
007591*         PRELOAD PAYMENT SOURCE AND PAT RELATION INTO STORAGE   *00180
007592*R11.3  ¦ 05/19/2011 ¦ 18.1 ¦E_49671   ¦DSL: SUPER JUMBO         *00180
007593*         PROCESS CLAIMS WITH ACCOUNT TYPE C                     *00180
007594*R11.3  ¦ 05/19/2011 ¦ 18.1 ¦PD_59966  ¦DSL: USE DIFFERENT SUBSCR*00180
007595*         WHEN LOADING TYPE-SERV INTO TABLE                      *00180
009200*R11.4  ¦ 11/10/2011 ¦ 18.2 ¦E_38714   ¦EXL: LET CLAIMS ADMIN SF *00180
009300*       TO BE PROCESSED - CHECK CIART-RT-IND FOR 'C'             *00760000
009400*       THESE CHANGES ALSO COVER E_45272 AND E_27448             *00760000
009500*R11.4  ¦ 11/10/2011 ¦ 18.2 ¦E_09799   ¦ RCM: CALL ITCP9109 FOR  *
009600*         'SF' FIELD REMOVAL PROCESSING                          *
009700*R11.4  ¦ 11/10/2011 ¦ 18.2 ¦E_43280   ¦ RCM: TO PASS LOC PLAN   *
009800*         CODE WHILE GENERATING SCCF NUMBER IN REAL TIME PROCESS *
009810*R11.4.1¦ 03/01/2012 ¦ 18.3 ¦I_69092   ¦ DSL: REMOVE INST PLAN   *
009820*         EXIT MESSAGES FOR EVERY CLAIM                          *
009830*R11.4.1¦ 03/01/2012 ¦ 18.3 ¦TD_72974  ¦ DSL: MOVE SETTING OF    *
009840*         PT045/IT045 TO DIFFERENT PARAGRAPH                     *
009850*R11.4.1¦ 03/01/2012 ¦ 18.3 ¦TD_71425  ¦ DSL: SET IS05A/PS05A    *
009860*R11.4.1¦ 03/01/2012 ¦ 18.3 ¦TD_73292  ¦ DSL: UPDATE # OF E6/46  *
009870*         UPON RETURN FROM PLAN EXIT                             *
009880*R11.4.1¦ 03/01/2012 ¦ 18.3 ¦TD_75487  ¦ DSL: UPDATE # OF E0/60  *
009890*         RECORDS                                                *
009810*R11.4.P¦ 05/29/2012 ¦ 18.4 ¦PD_77362  ¦ DSL: CORRECT SPECIAL    *
009811*         NOTATION PROCESSING FOR REAL TIME                      *
009812*R11.4.P¦ 05/29/2012 ¦ 18.4 ¦PD_76900  ¦ DSL: CORRECT ISSUE      *
009813*         IMPACTING CLAIMS ADMIN SCREEN                          *
009814*R12.0  ¦ 05/17/2012 ¦ 18.5 ¦E_19137   ¦ DSL: ICD-10 CHANGES. IT *
009820*       ¦ INCLUDES THE FOLLOWING ENHANCEMENTS E_69631, E_49746,  *
009821*       ¦ E_61141, E_75096, E_75097                              *
009824*R12.0  ¦ 05/17/2012 ¦ 18.5 ¦E_59024   ¦ EXL: BURST CONTROL.     *
009825*         CHECK HOME PLAN REAL TIME CAPABILITY ACCESSING BCBS_STN*
009826*R12.0.1¦ 08/23/2012 ¦ 18.6 ¦N/A       ¦ DSL: RETROFIT 11.4.P    *
009827*         PD_77362 AND PD_76900 WITH 05/29/2012 RELEASE DATE.    *
009828*         (PATCH WAS RELEASED AFTER 12.0 AND WAS LISTED IN       *
009829*         CHRONOLOGICAL ORDER).                                  *
009830*R12.0.1¦ 08/23/2012 ¦ 18.6 ¦TD_79489  ¦ DSL: PREVENTING 11.4    *
009840*         UNCONVERTED RECORDS FROM GOING THROUGH SF EDITS        *
011800*R13.0  ¦ 11/08/2012 ¦ 18.7 ¦E_60193   ¦ DSL: MODIFY 1250- TO    *
011900*         ENSURE PLAY-PAYER-CD ON ADJUSTMENT IS THE SAME AS ORIG *
012000*R13.0  ¦ 11/08/2012 ¦ 18.7 ¦E_72746   ¦ DSL: HEALTHCARE XCHGS   *
012010*R13.0  ¦ 11/08/2012 ¦ 18.7 ¦PD_81924  ¦ EXL: IS05A/PS05A ERROR  *
012100*         IN CA PROCESS IS DISPLAYED AS GENERIC ERROR CA120      *00760000
012101*R13.0.1¦ 02/21/2013 ¦ 18.8 ¦PD_82220  ¦ DSL: BYPASS ISSUING     *
012102*         IXR53/PXR53 IF DF DATE ON FPCT IS 9999-12-31           *00760000
012103*R13.0.1¦ 02/21/2013 ¦ 18.8 ¦E_82574   ¦ DSL: INCREASE TABLE SIZE*
012104*         FOR REF DATA - TOS, POS AND ZIP                        *00760000
012105*R13.5  ¦ 05/09/2013 ¦ 18.9 ¦E_73004   ¦ DSL: SET FM120-PAT-STAT *
012106*         TO 01 IF EST-IND = E                                   *00760000
012107*R13.5  ¦ 05/09/2013 ¦ 18.9 ¦E_79779   ¦ DSL: PASS LOC PLAN RCPT *
012108*         DATE TO LEVEL 2/3 EDIT                                 *00760000
012109*R13.5  ¦ 05/09/2013 ¦ 18.9 ¦E_76693   ¦ DSL: ADD ACCESS FEE CODE*
012110*         OF G AND H                                             *00760000
012111*R13.5.P¦ 10/29/2013 ¦ 19.0 ¦PD_91147  ¦ DSL: REPLACE ACCESS FEE *
012112*         PCT OF ZERO WITH 21 RECORD                             *00760000
013800*R14.0  ¦ 11/07/2013 ¦ 19.1 ¦E_84725   ¦ DSL: VALIDATE PROVIDER  *
013900*         SANCTION DATE ON APPL PARM RECORD 20.  BLANK OUT U270  *00760000
013910*         IF CLAIM IS PRIOR TO SANCTION DATE                     *00760000
013920*R14.0.1¦ 02/20/2014 ¦ 19.2 ¦TD_93033  ¦ DSL: MOVE SPEC PRICING  *
013930*         DAYS ALONG WITH CD,AMT AND PCT                         *
014100*R14.5  ¦ 05/15/2014 ¦ 19.3 ¦ E_34072  ¦ EXL: ALLOW ADJUSTMENT SF*01450000
014200*         TO BE TRANSPORTED IN REAL TIME. TMC = 4                *01460000
014300*R14.5  ¦ 05/15/2014 ¦ 19.3 ¦ PD_57538 ¦ SKG: STOP PROCESS IF DB2*01470000
014400*         ABEND IN CALLING MODULES                               *01480000
014410*R14.5  ¦ 05/15/2014 ¦ 19.3 ¦ E_63022  ¦ DSL: DO NOT DEFAULT     *01490000
014420*         PAYER CODE/PAYMENT DISP FOR INTL CLAIMS                *01500000
014430*R14.5.1¦ 08/28/2014 ¦ 19.4 ¦ E_96652  ¦ DSL: REMOVE IRL8L/PRL8L *01490000
014430*R14.5.1¦ 08/28/2014 ¦ 19.4 ¦ TD_98602 ¦ EXL: SET CIABP STATUS   *01490000
014500*         FOR IA046 ERROR                                        *01510000
014501*R15.5  ¦ 05/14/2015 ¦ 19.5 ¦ E_90497  ¦ DSL: ISSUE IRL4V/PRL4V  *01490000
014502*         IF ADJUSTMENT SF HAS MSG U200/U201 AND LATEST DF HAS   *01510000
014503*         MSG 1323                                               *01510000
115210*R15.5.1¦ 08/20/2015 ¦ 19.6 ¦ E_103267 ¦PMP: MOVE STAT-CD = V    *
115220*         WHEN PRL5T.                                            *
000048*R16.0  ¦ 11/12/2015 ¦ 19.7 ¦ E_75449  ¦ SKG: DUE TO             *00180
014510*       SUBSCRIBER GROUP NUMBER, ADJUSTMENT EDIT INDICATOR,      *01510000
014510*       CLAIM DEVELOPMENT INDICATOR & TRANSMIT MODE CODE SUBMISSIO01510000
014510*       N BEING REMOVED FROM PLAN PROFILE, REFERENCES TO THESE   *01510000
014510*       FIELDS WILL BE REMOVED.                                  *01510000
000048*R16.0  ¦ 11/12/2015 ¦ 19.7 ¦ E_101251 ¦ PMP: ADDED NEW EDITS    *00180
014510*       IRL5V, PRL5V, PRL5W, IRL5X AND PRL5X.                    *01510000
000048*R16.0.1¦ 02/25/2016 ¦ 19.8 ¦ TD_111483¦ EXL: FIX TO PRL5W EDIT  *00180
014510*                                                                *01510000
      *R16.5  ¦ 05/12/2016 ¦ 19.9 ¦ E_73994  ¦ VKJ: ADDED FORMAT       *
      *                             CONVERSION INDICATOR               *
004530*R16.5  ¦ 05/12/2016 ¦ 19.9 ¦ E_104562 ¦ PMP: ADD NEW SF WARNING *00760000
004540*         PRL5Z.                                                 *00760000
004530*R16.5  ¦ 05/12/2016 ¦ 19.9 ¦ E_97628  ¦ EXL: REMOVE PRLA3 EDIT. *00760000
004530*R16.5.P1¦10/27/2016 ¦ 20.0 ¦ E_117120 ¦ EXL: PERFORM IRL5V EDIT *00760000
      *         FOR OUTPATIENT CLAIMS ONLY - BYPASS INPATIENT          *
      *R17.0  ¦ 11/10/2016 ¦ 20.1 ¦ E_93016  ¦ SKG: UPDATED II018 CODE.*01320000
      *                             TD_116228¦ EXL: CORRECT OCCURRENCE *01330000
      *       OF REC-30 S/B IN ITCFM130 COPYBOOK AFTER PARAGRAPH 9932- *01331000
      *R17.5  ¦ 05/11/2017 ¦ 20.2 ¦ E_104138 ¦ TGW: RENAMED ALPHA      *01350000
      *       PREFIX                                                   *01360000
      *R17.5  ¦ 05/11/2017 ¦ 20.2 ¦ E_115778 ¦ EXL: SHOWED DESCRIPTION *01370000
      *       OF IRL3C AND PRL2R - COMMENTS ONLY                       *01380000
      *R17.5  ¦ 05/11/2017 ¦ 20.2 ¦ E_112726 ¦ EXL: FLEXIBLE NETWORK   *01390000
      *       GET COST GROUPING AND POPULATE IT ON SF                  *01400000
      *R17.5  ¦ 05/11/2017 ¦ 20.2 ¦ E_118591 ¦ EXL: FLEXIBLE NETWORK   *01401000
      *       CHECK CIAPP2-FLXBL-NETW-IND INSTEAD OF CIABP-DELV-MTD-IND*01402000
      *R18.0  ¦ 11/09/2017 ¦ 20.3 ¦ E_114613 ¦ TGW: MOVE PROGRAM ID TO *01430000
      *       TO CSAPI-CALLING-PGM-ID                                  *01440000
      *R18.0  ¦ 11/09/2017 ¦ 20.3 ¦ E_115667 ¦ EXL: CCF PROCESS AND NEW*01450000
      *       EDITS PRLC1 - PRLC5                                      *01460000
002536*                                                                *00003620
002900******************************************************************ITCP9102
003000/                                                                 ITCP9102
003100 ENVIRONMENT DIVISION.                                            ITCP9102
003200                                                                  ITCP9102
003300 CONFIGURATION SECTION.                                           ITCP9102
003400                                                                  ITCP9102
003500 SOURCE-COMPUTER. IBM-370.                                        ITCP9102
003600 OBJECT-COMPUTER. IBM-370.                                        ITCP9102
003700                                                                  ITCP9102
003800 INPUT-OUTPUT SECTION.                                            ITCP9102
004300 DATA DIVISION.                                                   ITCP9102
004400                                                                  ITCP9102
005600 WORKING-STORAGE SECTION.                                         ITCP9102
005700                                                                  ITCP9102
005800 01  WS-START-LIT                     PIC X(46) VALUE             ITCP9102
005900     '**** ITCP9102 WORKING-STORAGE STARTS HERE ****'.            ITCP9102
006000                                                                  ITCP9102
006100 01  WS-PROGRAM-INFO.                                             ITCP9102
006200     05  WS-MODULE-ID                 PIC X(8)  VALUE 'ITCP9102'. ITCP9102
016500     05  WS-VERSION-NUMBER            PIC X(4)  VALUE '20.3'.     01670000
016600     05  WS-RELEASE-DATE              PIC X(8)  VALUE '11/09/17'. 01680000
006700     05  WS-PROGRAM-DISPLAY.                                      ITCP9102
085100        10  WS-COMMON-DISPLAY         PIC X(30) VALUE             00006100
012300            'ITCP9102-SF EDIT MAIN MODULE '.                      01000000
007000        10  WS-MODULE-DISPLAY         PIC X(8)  VALUE ' MODULE '. ITCP9102
007100        10  WS-RELEASE-DISPLAY        PIC X(34) VALUE             ITCP9102
017200            'VERSION #20.3 RELEASED ON 11/09/17'.                 01740000
007300                                                                  ITCP9102
007400 01  WS-BINARY-FIELDS      COMP.                                  ITCP9102
007700     05  WS-PLUS-ONE                  PIC S9(4) VALUE +1.         ITCP9102
007800     05  WS-SUB                       PIC S9(4) VALUE +0.         ITCP9102
007900     05  WS-SUB1                      PIC S9(4) VALUE +0.         ITCP9102
014900     05  WS-SUB2                      PIC S9(4) VALUE +0.         ITCP9102
086501     05  WS-SUB3                      PIC S9(4) VALUE +0.         00007200
086510     05  WS-SUB4                      PIC S9(4) VALUE +0.         00092400
086503     05  WS-SUB5                      PIC S9(4) VALUE +0.         00007400
011210     05  WS-SUB6                      PIC S9(4) VALUE +0.         01120000
066020     05  WS-SUBR                      PIC S9(4) VALUE +0.         00707
000936                                                                  00093600
000937 01  WS-PARM-SEQ-NUM-MAX.                                         00093700
086700     05  WS-PARM-INST-SEQ-NUM-MAX     PIC  9(4) VALUE 0.          00092900
086700     05  WS-PARM-PROF-SEQ-NUM-MAX     PIC  9(4) VALUE 0.          00093000
008000                                                                  ITCP9102
033200 01  WS-PACKED-FIELDS      COMP-3.                                ITCP9102
033300     05  WS-AEA-INTL-AMT              PIC S9(6)V99                ITCP9102
033400                                                VALUE +5.00.      ITCP9102
033500     05  WS-AEA-NON-INTL-AMT          PIC S9(6)V99                ITCP9102
033600                                                VALUE +1.00.      ITCP9102
008100 01  WS-CONSTANTS.                                                ITCP9102
008200     05  WS-0                         PIC 9(1)  VALUE ZEROS.      ITCP9102
008300     05  WS-500                       PIC 9(3)  VALUE 500.        ITCP9102
008600     05  WS-I                         PIC X(1)  VALUE 'I'.        ITCP9102
061210     05  WS-H                         PIC X(1)  VALUE 'H'.        00674
008700     05  WS-N                         PIC X(1)  VALUE 'N'.        ITCP9102
008900     05  WS-R                         PIC X(1)  VALUE 'R'.        ITCP9102
009000     05  WS-V                         PIC X(1)  VALUE 'V'.        ITCP9102
013900     05  WS-X                         PIC X(1)  VALUE 'X'.        ITCP9102
009100     05  WS-Y                         PIC X(1)  VALUE 'Y'.        ITCP9102
009200     05  WS-1                         PIC X(1)  VALUE '1'.        ITCP9102
009300     05  WS-2                         PIC X(1)  VALUE '2'.        ITCP9102
020200     05  WS-1008                      PIC X(4)  VALUE '1008'.     ITCP9102
020300                                                                  ITCP9102
009900 01  WS-INDICATORS.                                               ITCP9102
092800     05  WS-FIRST-TIME-SW             PIC X(1)  VALUE 'Y'.        00010200
012400     05  WS-SEMI-PRIV-RM-RT-IND       PIC X(1)  VALUE 'N'.        ITCP9102
014000     05  WS-BYPASS-EDITS-IND          PIC X(1)  VALUE 'N'.        ITCP9102
014100         88  WS-BYPASS-EDITS                    VALUE 'Y'.        ITCP9102
014200         88  WS-DONT-BYPASS-EDITS               VALUE 'N'.        ITCP9102
014300     05  WS-DUP-FILE-IND              PIC X(1)  VALUE 'N'.        ITCP9102
015000     05  WS-CLM-TYPE-IND              PIC X(1).                   ITCP9102
015100         88  WS-INSTITUTIONAL                   VALUE 'I'.        ITCP9102
015200         88  WS-PROFESSIONAL                    VALUE 'P'.        ITCP9102
015300     05  WS-SUB-PAYER-IND             PIC 9(1)  VALUE ZERO.       ITCP9102
015400         88  WS-NO-ITS-SUB                      VALUE 0.          ITCP9102
015500         88  WS-3001-ITS-SUB                    VALUE 1.          ITCP9102
015600         88  WS-3002-ITS-SUB                    VALUE 2.          ITCP9102
015700         88  WS-3003-ITS-SUB                    VALUE 4.          ITCP9102
042200     05  WS-BCP-POS-IND               PIC X(1)  VALUE 'N'.        ITCP9102
042300         88  WS-NOT-BCP-POS                     VALUE 'N'.        ITCP9102
042400         88  WS-BCP-POS                         VALUE 'Y'.        ITCP9102
042500     05  WS-INTL-PAY-SUB-IND          PIC X(1)  VALUE 'N'.        ITCP9102
042600         88  WS-NOT-INTL-PAY-SUB                VALUE 'N'.        ITCP9102
042700         88  WS-INTL-PAY-SUB                    VALUE 'Y'.        ITCP9102
042800     05  WS-ECR-NON-PAR-IND           PIC X(1)  VALUE 'N'.        ITCP9102
042900         88  WS-NOT-ECR-NON-PAR                 VALUE 'N'.        ITCP9102
043000         88  WS-ECR-NON-PAR                     VALUE 'Y'.        ITCP9102
043100     05  WS-ECR-IND                   PIC X(1)  VALUE SPACES.     ITCP9102
043200         88  WS-ECR-PROC                        VALUE 'Y'.        ITCP9102
068200         88  WS-ECR-HIPAA-PROC                  VALUE 'H'.        00682
043300         88  WS-HOST-OVRIDE-ECR-PROC            VALUE 'N'.        ITCP9102
043400         88  WS-STD-BCP-PROC                    VALUE SPACE.      ITCP9102
043500         88  WS-ECR-IND-VALID-VALUES            VALUE ' ' 'N' 'Y'.ITCP9102
068600         88  WS-ECR-IND-VALID-VALUES-R85        VALUE ' ' 'H' 'N' 00686
068700                                                      'Y'.        00687
098201     05  WS-PRL2J-SW                  PIC X(1)  VALUE SPACES.     00013400
098202     05  WS-PRL2L-SW                  PIC X(1)  VALUE SPACES.     00013500
098300     05  WS-CLASS-PROV-CLM-OK-SW      PIC X(1)  VALUE SPACE.      00013600
098700     05  WS-CLASS-PROV-LN-OK-SW       PIC X(1)  VALUE SPACES.     00013700
098800     05  WS-VALID-SCCF                PIC X(1)  VALUE SPACE.      00013710
044700     05  WS-F0-CLASS-PROV-PAR-IND     PIC X(1)  VALUE SPACES.     ITCP9102
045000     05  WS-F0-CLASS-PROV-NONPAR-IND  PIC X(1)  VALUE SPACES.     ITCP9102
099600     05  WS-CLASS-PROV-LN-BLANK-IND   PIC X(1)  VALUE SPACES.
045300     05  WS-CLASS-PROV-NONPAR-IND     PIC X(1)  VALUE SPACES.     ITCP9102
079112     05  WS-REMOVE-DECIMAL.                                       00978
079113         10  WS-RD-INPUT              PIC 9(02).                  00979
079114         10  WS-RD-OUTPUT             PIC 9(02).                  00980
079115         10  WS-REMOVE-DECIMAL-INPUT  PIC X(07).                  00981
079116         10  WS-REMOVE-DECIMAL-OUTPUT PIC X(07).                  00982
106501     05  WS-ZIP-FOUND-SW1             PIC X(1).                   00014900
106502     05  WS-ZIP-FOUND-SW2             PIC X(1).                   00015000
106503     05  WS-ZIP-FOUND-SW3             PIC X(1).                   00015100
018620     05  WS-END-REF-ITEM              PIC X(1) VALUE SPACES.      01860000
025920     05  WS-U200-1-IND                PIC X(1) VALUE SPACES.      02650000
025920*R17.0 E_93016 SKG : ADDED BELOW WS-PAYER-INDICATORS              02480000
025920     05  WS-PAYER-INDICATORS.                                     02490000
025920         10 WS-PAYER-IND OCCURS 3 TIMES PIC X(01).                02500000
025920                                                                  02510000
025920*R16.0 E_101251 PMP START                                         02650000
       01  WS-WORK-SWITCHES.
025920     05  WS-PERF-PROV-SPEC-LN-SW      PIC X(1).                   02650000
025920     05  WS-PROV-NPI-ALL-LN-BLANK     PIC X(1).                   02650000
025920     05  WS-PROV-NPI-ANY-LN-BLANK     PIC X(1).                   02650000
025920     05  WS-ATYPICAL-PROV-SW          PIC X(1).                   02650000
025920*R16.0 E_101251 PMP END                                           02650000
079140                                                                  00987
016000 01  WS-WORK-FIELDS.                                              ITCP9102
107101     05  WS-RT-BEG-MINUS1                  PIC 9(5).              00015400
107102     05  WS-OEC-BEG-MINUS1                 PIC 9(5).              00015500
107103     05  WS-AEA-AMT-X                      PIC X(5).              00015600
107104     05  WS-AEA-AMT REDEFINES WS-AEA-AMT-X PIC 9(3)V99.           00015700
016100     05  WS-30-REC-CNT                PIC 9(1)  VALUE ZERO.       ITCP9102
016200         88  WS-NO-30-REC                       VALUE 0.          ITCP9102
016300         88  WS-HAVE-3001-REC                   VALUE 1.          ITCP9102
016400         88  WS-HAVE-3002-REC                   VALUE 2.          ITCP9102
016500         88  WS-HAVE-3003-REC                   VALUE 4.          ITCP9102
016600         88  WS-ONE-30-REC                      VALUE 1 2 4.      ITCP9102
016700         88  WS-TWO-30-REC                      VALUE 3 5 6.      ITCP9102
016800         88  WS-THREE-30-REC                    VALUE 7.          ITCP9102
016900     05  WS-TBL-RESULT                PIC X(1)  VALUE SPACES.     ITCP9102
017000     05  WS-TYPE-SUBM                 PIC X(1)  VALUE SPACES.     ITCP9102
047000     05  WS-PGM-CD                    PIC X(1)  VALUE SPACES.     ITCP9102
047100         88  WS-BCP-PGM-CD                      VALUE 'A'.        ITCP9102
047200         88  WS-ECR-PGM-CD                      VALUE '9'.        ITCP9102
047300     05  WS-INTL-CD                   PIC X(1)  VALUE SPACES.     ITCP9102
047400         88  WS-INTERNATIONAL                   VALUE 'I'.        ITCP9102
047500         88  WS-NOT-INTERNATIONAL               VALUE SPACES.     ITCP9102
047600     05  WS-PMT-DISP-CD               PIC X(1)  VALUE SPACES.     ITCP9102
058000         88  WS-PAY-SUB-REP                     VALUE '2'.        00629
047900         88  WS-PAY-REP-PAYEE                   VALUE '4'.        ITCP9102
048100     05  WS-CLASS-PROV                PIC X(1)  VALUE SPACES.     ITCP9102
048200         88  WS-NON-PAR-PROV                    VALUE '3'.        ITCP9102
048600         88  WS-CLASS-PROV-VALID-VALUES         VALUE SPACES,     ITCP9102
058900               '1'  '3'  '4'  '5'  '6'  '7'  '8'  '9'             00677
059000               'A'  'B'  'C'  'D'  'E'  'F'  'G'  'H'  'M'  'N'.  00678
059100         88  WS-CLASS-PROV-VALID-VALUES-84      VALUE SPACES,     00681
059110               '1'  '3'  '4'  '5'  '9'                            00682
110600               'D'  'E'  'F'  'G'  'M' 'P'.                       01137
110700     05  WS-HOLD-SEQ                  PIC X(4)  VALUE SPACES.     00018500
           05  WS-PDR-CD                    PIC X(01).                  03010000
           05  WS-RL6F-SW                   PIC X(01).                  03020000
           05  WS-CST-ENTERED-SW            PIC X(01).                  03030000
           05  WS-CST-GRP-CLASS-LN          PIC X(01).                  03040000
           05  WS-PDR-SUB                   PIC S9(04) COMP.            03050000
           05  WS-CST-GRP-TBL.                                          03060000
               10 WS-CST-GRP-LN           OCCURS 10 TIMES.              03070000
                  15 WS-CST-GRP-PROV   PIC X(13).                       03080000
                  15 WS-CST-GRP-DT     PIC X(08).                       03090000
      ** R18.0 E_115667 EXL       START                                 03150000
           05 WS-CCF-LN-NUM            PIC S9(04) COMP.                 03160000
           05 WS-ALLOWED-AMT           PIC S9(08)V99 COMP-3.            03170000
      ** R18.0 E_115667 EXL       END                                   03180000
017300                                                                  ITCP9102
017400     05  WS-TABLE-KEY.                                            ITCP9102
017500         10  WS-TBL-KEY-SF-TYPE       PIC X(01).                  ITCP9102
017600         10  WS-TBL-KEY-FDB-TYPE      PIC X(01).                  ITCP9102
017700         10  WS-TBL-KEY-FDB-STAT      PIC X(01).                  ITCP9102
017800                                                                  ITCP9102
022900     05  WS-SF-REL-NUM                PIC X(3).                   ITCP9102
023000     05  WS-FDB-REL-NUM               PIC X(3).                   ITCP9102
049800     05  WS-LOC-PLAN-CD               PIC X(3).                   ITCP9102
049900         88  WS-INTL-PLAN-CDS                   VALUE '071',      ITCP9102
077900                          '081', '381', '478', '571', '581',      07790003
078000                          '881', '978'.                           07800003
050100     05  WS-PLAN-CD                   PIC X(3).                   ITCP9102
018000         88  WS-PLAN-CD-WAS-ENTERED             VALUE '001'       ITCP9102
018100                                                THRU  '999'.      ITCP9102
018400         88  WS-NON-STD-PLAN-CDS                VALUE '250',      ITCP9102
018500                          '332', '350', '430', '751', '834',      ITCP9102
018600                          '851', '934'.                           ITCP9102
018700         88  WS-VIRGINIA-PLAN-CD                VALUE '441',      ITCP9102
018800                          '443', '444', '941', '943', '944'.      ITCP9102
018900         88  WS-CROSS-PLAN                      VALUE '001'       ITCP9102
019000                                                THRU '499'.       ITCP9102
019100         88  WS-SHIELD-PLAN                     VALUE '500'       ITCP9102
019200                                                THRU  '999'.      ITCP9102
019300     05  WS-VIRGINIAN-PLAN-CDS        PIC X(03).                  ITCP9102
019400         88  WS-VIRGINIAN-PLAN-CD               VALUE '441',      ITCP9102
019500                          '443', '444', '941', '943', '944'.      ITCP9102
019600     05  WS-COMPUTED-PLAN-CD          PIC 9(3) VALUE 000.         ITCP9102
019700     05  WS-NUMERIC-PLAN-CD           PIC 9(3)  VALUE 000.        ITCP9102
019800                                                                  ITCP9102
019900     05  WS-REV-CD                    PIC 9(04).                  ITCP9102
020600     05  FILLER                            REDEFINES WS-REV-CD.   ITCP9102
113903         10  FILLER                   PIC 9(3).                   00021800
081000         88  WS-SEMI-PRIV-REV-CD                VALUE 011 014.    08100003
020700         10  FILLER                   PIC 9.                      ITCP9102
021200                                                                  ITCP9102
113907     05  WS-NON-TRANSM-CNT            PIC S9(04) COMP.            00021600
028500     05  WS-ICD-DIAG-TBL.                                         02580000
028600         10  WS-ICD-DIAG-IND          OCCURS 4 TIMES              02590000
028700                                      PIC XX.                     02600000
028900                                                                  ITCP9102
115700     05  WS-ERR-CD                   PIC X(05) VALUE SPACES.      00022600
115800         88 WS-END-EDIT-ERR-CD                 VALUE 'II018'      00022700
036800             'II999' 'IR007' 'IR021' 'IT040' 'IT041' 'IXR01'      ITCP9102
036900             'IXR02' 'IXR21' 'IXR22' 'IXR50' 'IXR51' 'IXR52'      ITCP9102
029400             'IXR56'                                              02650000
037000             'PP999' 'PR007' 'PR021' 'PT040' 'PT041' 'PXR01'      ITCP9102
064601             'PXR02' 'PXR03' 'PXR21' 'PXR22' 'PXR50' 'PXR51'      00740
029700             'PXR52' 'PXR56'.                                     02680000
116400         88  WS-DUP-FILE-ERR-CD                VALUE 'IS001'      00023300
021700             'IS042' 'IXR53' 'IXR54' 'PS001' 'PS042' 'PXR53'      ITCP9102
021800             'PXR54'.                                             ITCP9102
021900                                                                  ITCP9102
030300     05  WS-UPF-PRC-EDIT-CD           PIC X(01) VALUE SPACES.     02730000
030400     05  WS-ZIP                       PIC X(05) VALUE SPACES.     02730000
      ** R18.0 E_115667 EXL ADDED WS-ITDCCFS1                           03740000
022000     05  WS-CALL-MODULE               PIC X(08) VALUE SPACES.     ITCP9102
116900         88  WS-ITCPE004                        VALUE 'ITCPE004'. 00023800
117000         88  WS-ITCPE005                        VALUE 'ITCPE005'. 00023900
117100         88  WS-ITCPP002                        VALUE 'ITCPP002'. 00024000
117900         88  WS-ITCP9110                        VALUE 'ITCP9110'. 00024100
118100         88  WS-ITCP4152                        VALUE 'ITCP4152'. 00024200
118300         88  WS-ITCP9526                        VALUE 'ITCP9526'. 00024300
118400         88  WS-ITCP9528                        VALUE 'ITCP9528'. 00024400
118500         88  WS-ITCP9556                        VALUE 'ITCP9556'. 00024500
118600         88  WS-ITCP9558                        VALUE 'ITCP9558'. 00024600
118601         88  WS-ITCP9646                        VALUE 'ITCP9646'. 00024600
118602         88  WS-ITPP01U1                        VALUE 'ITPP01U1'. 00024700
118603         88  WS-ITDS01S1                        VALUE 'ITDS01S1'. 00024800
118604         88  WS-ITPFCTU1                        VALUE 'ITPFCTU1'. 00024900
521100         88  WS-ITDF01S1                        VALUE 'ITDF01S1'. 00025000
521101         88  WS-ITDF51S1                        VALUE 'ITDF51S1'. 00025100
521102         88  WS-ITDC01S1                        VALUE 'ITDC01S1'. 00025200
521103         88  WS-ITCQRQU0                        VALUE 'ITCQRQU0'. 00025300
029100         88  WS-ITDSCCF1                        VALUE 'ITDSCCF1'. 02910000
521104         88  WS-PARTKEY                         VALUE 'PARTKEY '. 00025400
521105         88  WS-ITDE02U1                        VALUE 'ITDE02U1'. 00025410
032800         88  WS-ITDB22S1                        VALUE 'ITDB22S1'.
033000         88  WS-ITCP9109                        VALUE 'ITCP9109'.
033002         88  WS-ITDS02S1                        VALUE 'ITDS02S1'.
037902         88  WS-ITDB27S1                        VALUE 'ITDB27S1'. 03850000
037903         88  WS-ITDCA1S1                        VALUE 'ITDCA1S1'. 03860000
037904         88  WS-ITDFCXS1                        VALUE 'ITDFCXS1'. 03870000
037904         88  WS-ITDD04S1                        VALUE 'ITDD04S1'. 03940000
037904         88  WS-ITDCCFS1                        VALUE 'ITDCCFS1'. 04030000
023500                                                                  ITCP9102
033200     05  WS-B2-ACTION               PIC X(03) VALUE SPACES.
033300     05  WS-B2-REQUEST              PIC X(01) VALUE SPACES.
033400     05  WS-B2-SUB-COMM             PIC 9(04) COMP VALUE 0.
033500     05  WS-B2-RESUB                PIC X(01) VALUE SPACES.
038600     05  WS-B2-MSG-TP-CD            PIC X(08) VALUE SPACES.       03940000
038700     05  WS-B2-MSG-STS-CD           PIC X(04) VALUE SPACES.       03950000
033600                                                                  02940000
521107     05  WS-GREG-DT                   PIC 9(08).                  00025600
521108     05  WS-JULIAN-DT                 PIC 9(07).                  00025700
036410     05  WS-LOC-RCPT-DT               PIC 9(8) VALUE 0.           05790000
036411     05  WS-LOC-RCPT-DT-SEP REDEFINES WS-LOC-RCPT-DT.             05790000
036420         10 WS-DATE-YY                PIC 9(4).                   05790000
036430         10 WS-DATE-MM                PIC 9(2).                   05790000
036440         10 WS-DATE-DD                PIC 9(2).                   05790000
521109     05  WS-END-OF-SF                 PIC X(01).                  00025800
521110     05  WS-EARLIEST-SERV-DT          PIC X(08).                  00025900
521111     05  WS-F0-PRESENT-SW             PIC X(01).                  00026000
521112     05  WS-F1-PRESENT-SW             PIC X(01).                  00026100
024100                                                                  ITCP9102
067820     05  WS-SHIFT-2.                                              00776
067830         10  WS-SHIFT-FIELD2          PIC X(2).                   00777
067840         10  FILLER                   PIC X(1).                   00778
067850     05  WS-SHIFT-3   REDEFINES WS-SHIFT-2.                       00779
067860         10  WS-SHIFT-FIELD3          PIC X(3).                   00780
067910     05  WS-SHIFT                     PIC 99    VALUE ZEROES.     00782
040200*R14.0 E_84725 DSL BEGIN
040210     05  WS-SANCTION-GREG-DT          PIC 9(08).                  02950000
040300     05  WS-SAVED-SF-MSGS-CLM.                                    05790000
040400         10  WS-SAVED-SF-MSG         OCCURS 5 TIMES               05790000
040500                                        PIC X(4) VALUE SPACES.    05790000
040600     05  WS-SAVED-SPEC-PRC-CC-CLM.                                05790000
040700         10  WS-SAVED-SPEC-PRC-CD    OCCURS 5 TIMES               05790000
040800                                        PIC X(3) VALUE SPACES.    05790000
040810     05  WS-SAVED-SPEC-PRC-AMT-CLM.                               05790000
040820         10  WS-SAVED-SPEC-PRC-AMT   OCCURS 5 TIMES               05790000
040830                                        PIC S9(8)V99 COMP-3.      05790000
040840     05  WS-SAVED-SPEC-PRC-PCT-CLM.                               05790000
040850         10  WS-SAVED-SPEC-PRC-PCT   OCCURS 5 TIMES               05790000
040860                                        PIC S9(3)V9(5) COMP-3.    05790000
040861*R14.0.1 TD_93033 DSL ADD DAYS
040870     05  WS-SAVED-SPEC-PRC-DAY-CLM.                               05790000
040880         10  WS-SAVED-SPEC-PRC-DAY   OCCURS 5 TIMES               05790000
040890                                        PIC S9(3).                05790000
040900*R14.0 E_84725 DSL END
026600                                                                  ITCP9102
035002*** THIS TABLE CONTAINS NON-REAL TIME ENABLED STATION CODES.
035003*** IT IS POPULATED IN 9090- PARAGRAPH. USED IN 1800-             03080000
035004 01  WS-STN-TBL.                                                  03080000
035005     05 WS-STN-CD   OCCURS 125 TIMES
035006                    INDEXED BY WS-STN-I PIC X(04).
035007
043300*R16.0 E_101251 PMP ADDED REV-FLAG2                               04450000
521130 01  WS-REF-DATA-TBL.                                             00027000
521131     05 WS-REF-INST-TBL.                                          00027100
043600        10 WS-REF-REV         OCCURS 999 TIMES PIC X(04).         04480000
043700        10 WS-REF-REV-TP      OCCURS 999 TIMES PIC X(01).         04490000
043800        10 WS-REF-REV-FLAG    OCCURS 999 TIMES PIC X(01).         04500000
043800        10 WS-REF-REV-FLAG2   OCCURS 999 TIMES PIC X(01).         04500000
521134     05 WS-REF-PROF-TBL REDEFINES WS-REF-INST-TBL.                00027400
039100        10 WS-REF-POS-LIST OCCURS 100 TIMES.                      03140000
035900           15 WS-REF-POS                    PIC X(02).            03140000
036000           15 WS-REF-POS-FLAG               PIC X(1).             03140000
039400        10 WS-REF-TOS-LIST OCCURS 150 TIMES.                      03150000
036200           15 WS-REF-TOS                    PIC X(03).            03140000
036300           15 WS-REF-TOS-FLAG               PIC X(1).             03140000
039700        10 WS-REF-ZIP-LIST OCCURS 800 TIMES.                      03160000
036500           15 WS-REF-ZIP                    PIC X(05).            03140000
036600           15 WS-REF-ZIP-FLAG               PIC X(01).            03140000
045000        10 FILLER                           PIC X(1293).          04620000
043900*R16.0 E_101251 PMP START                                         04510000
043500     05 WS-PROC-CD-TBL     OCCURS 999 TIMES.                      04470000
043600        10 WS-PROC-CD                       PIC X(05).            04480000
043800        10 WS-PROC-CD-FLAG                  PIC X(01).            04500000
043900*R16.0 E_101251 PMP END                                           04510000
027500                                                                  ITCP9102
031910 01  WS-REF-REL-CD-TBL.                                           03090000
031920     COPY ITCWRREL.                                               03100000
045610                                                                  04680000
031930 01  WS-REF-SRC-PMT-TBL.                                          03090000
031940     COPY ITCWRSRC.                                               03100000
031950                                                                  03210000
027600 01  WS-ERROR-MSGS.                                               ITCP9102
521151
045700     05  WS-PARM-FILE-MSG             PIC X(50) VALUE             ITCP9102
045800         'VALID PARAMETER/CONTROL FILE MISSING - JOB ABORTED'.    ITCP9102
030300     05  WS-PDB-CONTENTS-MSG.                                     ITCP9102
030400         10  FILLER                   PIC X(44) VALUE             ITCP9102
030500         'DATA IN THE PARAMETER DB SYSTEM FLOW RECORD '.          ITCP9102
030600         10  FILLER                   PIC X(23) VALUE             ITCP9102
030700         '(ITCDBP16) IS NOT VALID'.                               ITCP9102
093610     05  WS-PDB-MISSING-REC-MSG.                                  00999
093620         10  FILLER                   PIC X(34) VALUE             01000
093630         'PARAMETER DATABASE 20 RECORD ERROR'.                    01001
031800     05  WS-DB-READ-MSG.                                          ITCP9102
031900         10  FILLER                   PIC X(15) VALUE             ITCP9102
032000             'UNABLE TO READ '.                                   ITCP9102
032100         10  WS-READ-DB-NAME          PIC X(32) VALUE SPACES.     ITCP9102
032200             88  WS-READ-PDB-NAME             VALUE               ITCP9102
032300                 'PARAMETER (ITCDBP00) DATABASE   '.              ITCP9102
032400             88  WS-READ-PPF-NAME             VALUE               ITCP9102
032500                 'PLAN PROFILE (ITCDB600) DATABASE'.              ITCP9102
032600             88  WS-READ-FDB-NAME             VALUE               ITCP9102
032700                 'FORMATS (ITCDBKXX) DATABASES    '.              ITCP9102
102121             88  WS-READ-VALDBD-CD-NAME       VALUE               01253
102124                 'VALIDATION DATABASE CODE'.                      01255
102150     05  WS-PDB-REC21-INV-RATE-MSG.                               01117
102160         10  FILLER                   PIC X(16) VALUE             01118
102170             'INVALID RATE IN '.                                  01119
102180         10  WS-READ-DB-NAME          PIC X(32) VALUE SPACES.     01120
102190             88  WS-READ-PDB21-NAME             VALUE             01121
102191                 'PARAMETER (ITCDBP00) 21 RECORD  '.              01122
034200     05  WS-RETURN-IND-MSG.                                       ITCP9102
034300         10  FILLER                   PIC X(41) VALUE             ITCP9102
034400             'UNEXPECTED RETURN INDICATOR FROM CALL TO '.         ITCP9102
034500         10  WS-RETURN-IND-PGM        PIC X(08) VALUE SPACES.     ITCP9102
034600         10  FILLER                   PIC X(07) VALUE ' MODULE'.  ITCP9102
034700                                                                  ITCP9102
034800******************************************************************ITCP9102
034900****  THE FOLLOWING TABLE REPRESENTS A MODIFIED TABLE         ****ITCP9102
035000****  OUTLINED IN THE RELEASE 7.2 FUNCTIONAL SPECIFICATION.   ****ITCP9102
035100*** TYPE   1    1    2    2    3    3    4    4    A    A    NO  *ITCP9102
035200***  OF   REJ  INV  REJ  INV  REJ  INV  REJ  INV  REJ  INV   REC *ITCP9102
035300***  SUB  VAL  MIS  VAL  MIS  VAL  MIS  VAL  MIS  VAL  MIS  FOUND*ITCP9102
035400***--------------------------------------------------------------*ITCP9102
035500***  1     D    V    D    D    D    D    D    D    D    D     V  *ITCP9102
040700***  1*    X    V    D    D    D    D    D    D    D    D     V  *ITCP9102
035700***--------------------------------------------------------------*ITCP9102
035800***  2     V    V    V    V    I    I    I    I    I    I     V  *ITCP9102
035900***  2*    V    I    V    V    I    I    I    I    I    I     I  *ITCP9102
036000***--------------------------------------------------------------*ITCP9102
054400***  3     D    D    D    D    D    V    D    D    D    D     V  *ITCP9102
054500***  3*    D    D    D    D    X    V    D    D    D    D     V  *ITCP9102
036300***--------------------------------------------------------------*ITCP9102
036400***  4     I    I    I    I    V    V    V    V    I    I     V  *ITCP9102
036500***  4*    I    I    I    I    V    I    V    V    I    I     I  *ITCP9102
036600***--------------------------------------------------------------*ITCP9102
036700***  D=DUPLICATE                                                 *ITCP9102
036800***  I=INVALID RESUBMISSION                                      *ITCP9102
036900***  V=VALID FOR OVERLAY OR WRITING TO FDB                       *ITCP9102
042100***  X=DUPLICATE OR VALID DEPENDING ON RELEASE NUMBER            *ITCP9102
037000***  *=HOST RESULTS FOR SF TYPE OF SUBMISSION                    *ITCP9102
037100***--------------------------------------------------------------*ITCP9102
037200******************************************************************ITCP9102
037300 01  WS-FDB-LOOKUP-DATA.                                          ITCP9102
037400                                                                  ITCP9102
042700     05 FILLER                        PIC X(05) VALUE '11RDX'.    ITCP9102
042800     05 FILLER                        PIC X(05) VALUE '11VDX'.    ITCP9102
037700     05 FILLER                        PIC X(05) VALUE '11IVV'.    ITCP9102
037800     05 FILLER                        PIC X(05) VALUE '11MVV'.    ITCP9102
037900     05 FILLER                        PIC X(05) VALUE '12RDD'.    ITCP9102
038000     05 FILLER                        PIC X(05) VALUE '12VDD'.    ITCP9102
038100     05 FILLER                        PIC X(05) VALUE '12IDD'.    ITCP9102
038200     05 FILLER                        PIC X(05) VALUE '12MDD'.    ITCP9102
038300     05 FILLER                        PIC X(05) VALUE '13RDD'.    ITCP9102
038400     05 FILLER                        PIC X(05) VALUE '13VDD'.    ITCP9102
038500     05 FILLER                        PIC X(05) VALUE '13IDD'.    ITCP9102
038600     05 FILLER                        PIC X(05) VALUE '13MDD'.    ITCP9102
038700     05 FILLER                        PIC X(05) VALUE '14RDD'.    ITCP9102
038800     05 FILLER                        PIC X(05) VALUE '14VDD'.    ITCP9102
038900     05 FILLER                        PIC X(05) VALUE '14IDD'.    ITCP9102
039000     05 FILLER                        PIC X(05) VALUE '14MDD'.    ITCP9102
039100     05 FILLER                        PIC X(05) VALUE '1ARDD'.    ITCP9102
039200     05 FILLER                        PIC X(05) VALUE '1AVDD'.    ITCP9102
039300     05 FILLER                        PIC X(05) VALUE '1AIDD'.    ITCP9102
039400     05 FILLER                        PIC X(05) VALUE '1AMDD'.    ITCP9102
039500     05 FILLER                        PIC X(05) VALUE '1NNVV'.    ITCP9102
039600**************************************************************    ITCP9102
039700     05 FILLER                        PIC X(05) VALUE '21RVV'.    ITCP9102
039800     05 FILLER                        PIC X(05) VALUE '21VVV'.    ITCP9102
039900     05 FILLER                        PIC X(05) VALUE '21IVI'.    ITCP9102
040000     05 FILLER                        PIC X(05) VALUE '21MVI'.    ITCP9102
040100     05 FILLER                        PIC X(05) VALUE '22RVV'.    ITCP9102
040200     05 FILLER                        PIC X(05) VALUE '22VVV'.    ITCP9102
040300     05 FILLER                        PIC X(05) VALUE '22IVV'.    ITCP9102
040400     05 FILLER                        PIC X(05) VALUE '22MVV'.    ITCP9102
040500     05 FILLER                        PIC X(05) VALUE '23RII'.    ITCP9102
040600     05 FILLER                        PIC X(05) VALUE '23VII'.    ITCP9102
040700     05 FILLER                        PIC X(05) VALUE '23III'.    ITCP9102
040800     05 FILLER                        PIC X(05) VALUE '23MII'.    ITCP9102
040900     05 FILLER                        PIC X(05) VALUE '24RII'.    ITCP9102
041000     05 FILLER                        PIC X(05) VALUE '24VII'.    ITCP9102
041100     05 FILLER                        PIC X(05) VALUE '24III'.    ITCP9102
041200     05 FILLER                        PIC X(05) VALUE '24MII'.    ITCP9102
041300     05 FILLER                        PIC X(05) VALUE '2ARII'.    ITCP9102
041400     05 FILLER                        PIC X(05) VALUE '2AVII'.    ITCP9102
041500     05 FILLER                        PIC X(05) VALUE '2AIII'.    ITCP9102
041600     05 FILLER                        PIC X(05) VALUE '2AMII'.    ITCP9102
041700     05 FILLER                        PIC X(05) VALUE '2NNVI'.    ITCP9102
041800**************************************************************    ITCP9102
060300     05 FILLER                        PIC X(05) VALUE '31RDD'.    ITCP9102
060400     05 FILLER                        PIC X(05) VALUE '31VDD'.    ITCP9102
060500     05 FILLER                        PIC X(05) VALUE '31IDD'.    ITCP9102
060600     05 FILLER                        PIC X(05) VALUE '31MDD'.    ITCP9102
042300     05 FILLER                        PIC X(05) VALUE '32RDD'.    ITCP9102
042400     05 FILLER                        PIC X(05) VALUE '32VDD'.    ITCP9102
042500     05 FILLER                        PIC X(05) VALUE '32IDD'.    ITCP9102
042600     05 FILLER                        PIC X(05) VALUE '32MDD'.    ITCP9102
047900     05 FILLER                        PIC X(05) VALUE '33RDX'.    ITCP9102
048000     05 FILLER                        PIC X(05) VALUE '33VDX'.    ITCP9102
042900     05 FILLER                        PIC X(05) VALUE '33IVV'.    ITCP9102
043000     05 FILLER                        PIC X(05) VALUE '33MVV'.    ITCP9102
043100     05 FILLER                        PIC X(05) VALUE '34RDD'.    ITCP9102
043200     05 FILLER                        PIC X(05) VALUE '34VDD'.    ITCP9102
043300     05 FILLER                        PIC X(05) VALUE '34IDD'.    ITCP9102
043400     05 FILLER                        PIC X(05) VALUE '34MDD'.    ITCP9102
043500     05 FILLER                        PIC X(05) VALUE '3ARDD'.    ITCP9102
043600     05 FILLER                        PIC X(05) VALUE '3AVDD'.    ITCP9102
043700     05 FILLER                        PIC X(05) VALUE '3AIDD'.    ITCP9102
043800     05 FILLER                        PIC X(05) VALUE '3AMDD'.    ITCP9102
043900     05 FILLER                        PIC X(05) VALUE '3NNVV'.    ITCP9102
044000**************************************************************    ITCP9102
044100     05 FILLER                        PIC X(05) VALUE '41RII'.    ITCP9102
044200     05 FILLER                        PIC X(05) VALUE '41VII'.    ITCP9102
044300     05 FILLER                        PIC X(05) VALUE '41III'.    ITCP9102
044400     05 FILLER                        PIC X(05) VALUE '41MII'.    ITCP9102
044500     05 FILLER                        PIC X(05) VALUE '42RII'.    ITCP9102
044600     05 FILLER                        PIC X(05) VALUE '42VII'.    ITCP9102
044700     05 FILLER                        PIC X(05) VALUE '42III'.    ITCP9102
044800     05 FILLER                        PIC X(05) VALUE '42MII'.    ITCP9102
044900     05 FILLER                        PIC X(05) VALUE '43RVV'.    ITCP9102
045000     05 FILLER                        PIC X(05) VALUE '43VVV'.    ITCP9102
045100     05 FILLER                        PIC X(05) VALUE '43IVI'.    ITCP9102
045200     05 FILLER                        PIC X(05) VALUE '43MVI'.    ITCP9102
045300     05 FILLER                        PIC X(05) VALUE '44RVV'.    ITCP9102
045400     05 FILLER                        PIC X(05) VALUE '44VVV'.    ITCP9102
045500     05 FILLER                        PIC X(05) VALUE '44IVV'.    ITCP9102
045600     05 FILLER                        PIC X(05) VALUE '44MVV'.    ITCP9102
045700     05 FILLER                        PIC X(05) VALUE '4ARII'.    ITCP9102
045800     05 FILLER                        PIC X(05) VALUE '4AVII'.    ITCP9102
045900     05 FILLER                        PIC X(05) VALUE '4AIII'.    ITCP9102
046000     05 FILLER                        PIC X(05) VALUE '4AMII'.    ITCP9102
046100     05 FILLER                        PIC X(05) VALUE '4NNVI'.    ITCP9102
046200                                                                  ITCP9102
046300 01  WS-FDB-LOOKUP-TABLE REDEFINES WS-FDB-LOOKUP-DATA.            ITCP9102
046400     05  WS-FDB-ENTRY    OCCURS 84 TIMES                          ITCP9102
046500                         INDEXED BY WS-FDB-INX.                   ITCP9102
046600        10  WS-SF-TYPE-SUBM           PIC X(01).                  ITCP9102
046700        10  WS-FDB-TYPE-SUBM          PIC X(01).                  ITCP9102
046800        10  WS-FDB-STAT-CD            PIC X(01).                  ITCP9102
046900        10  WS-LOOKUP-RESULT.                                     ITCP9102
047000            15  WS-HOME-RESULT        PIC X(01).                  ITCP9102
047100            15  WS-HOST-RESULT        PIC X(01).                  ITCP9102
047200                                                                  ITCP9102
153507 01  WS-MISC-FIELDS.                                              01582
153508                                                                  01583
521314     05  WS-RCPT-DT.                                              00046700
521315         10 WS-RCPT-CCYY.                                         00046800
521316            15 WS-RCPT-CC                PIC X(02).               00046900
521317            15 WS-RCPT-YY                PIC X(02).               00047000
521318         10 FILLER                       PIC X VALUE '-'.         00047100
521319         10 WS-RCPT-MM                   PIC X(02).               00047200
521320         10 FILLER                       PIC X VALUE '-'.         00047300
521321         10 WS-RCPT-DD                   PIC X(02).               00047400
153509     05  WS-D09-LOC-PLAN-RCPT-DT.                                 01584
153510         10  WS-D09-LOC-PLAN-RCPT-DT-CC  PIC  X(02).              01585
153511         10  WS-D09-LOC-PLAN-RCPT-DT-YY  PIC  X(02).              01586
153512         10  WS-D09-LOC-PLAN-RCPT-DT-MM  PIC  X(02).              01587
153513         10  WS-D09-LOC-PLAN-RCPT-DT-DD  PIC  X(02).              01588
153514                                                                  01589
153515     05  WS-D09-ECR-IND                PIC X(01).                 01590
162100         88  WS-D09-ECR-Y                       VALUE 'Y'.        01653
162200         88  WS-D09-ECR-H                       VALUE 'H'.        01654
153516     05  WS-D09-BCP-PROD-TYPE          PIC X(01).                 01591
153517     05  WS-D09-PRODUCT-CODE           PIC X(01).                 01592
121080                                                                  01534
521334 01  WS-PASSING-AREA.                                             00048700
521335     05 WS-PLAN-CD2                    PIC X(3).                  00048800
521336     05 FILLER                         PIC X(7).                  00048900
521337                                                                  00049000
521338     COPY ITDFCTX1.                                               00049100
521339                                                                  00049200
521340     COPY ITDF01X1.                                               00049300
521341                                                                  00049400
521342     COPY ITDF51X1.                                               00049500
521343                                                                  00049600
521344     COPY ITDC01X1.                                               00049700
521345                                                                  00049800
065700** R17.5 E_112726 EXL ADD ITDD04X1 COPYBOOK                       06770000
065800***01  D04-DATA-AREA.                                             06780000
065500     COPY ITDD04X1.                                               06790000
065600                                                                  06800000
057302** R12.0 E_59024 EXL ADD ITDS02X1 COPYBOOK
057400***01  WS-BCBS-STN.                                               05150000
057401     COPY ITDS02X1.                                               05140000
057402                                                                  05150000
121020 01  WS-HOLD-21-KEY.                                              01332
121030     05  WS-REC-TYPE-21                PIC X(02) VALUE '21'.      01333
121040     05  FILLER                        PIC X    VALUE SPACES.     01334
121050     05  WS-ACCT-TYPE-21               PIC X    VALUE SPACES.     01335
121060     05  WS-INCUR-YR-21                PIC X(02) VALUE SPACES.    01336
521360                                                                  00050400
521370 01  PART-ALGO-PASS-AREA.                                         00050500
521380     05 PART-KEY-NUM                 PIC X(03).                   00050600
521390     05 PART-SCCF-ID                 PIC X(17).                   00050700
521400     05 FILLER                       PIC X(04).                   00050800
521500                                                                  00050900
521600 01  LK-RQST-RTRN-CD.                                             00051000
521700* 'C' - COMPRESS; 'D' - DECOMRESS                                 00051100
521800     05  LK-REQUEST-CODE            PIC X(01).                    00051200
522000     05  LK-TYPE                    PIC X(01).                    00051400
522100     05  LK-RETURN-CODE             PIC X(04).                    00051500
522200     05  LK-TOTL-RCD                PIC S9(04) COMP.              00051600
522300 01  LK-MESSAGE-AREA                PIC X(50).                    00051700
522400                                                                  00051800
522702 01  WS-ITCDBP14.                                                 00052300
522703     COPY ITCDBP14.                                               00052400
522704                                                                  00052500
121061 01  WS-ITCDBP21-RECORD.                                          01337
121062     COPY ITCDBP21.                                               01338
059900                                                                  05410000
059911     COPY ITCSFCNT.                                               05410000
522710                                                                  00053100
049500 01  ITCFM130-RECORD.                                             ITCP9102
049600 COPY ITCFM130.                                                   ITCP9102
069100                                                                  07190000
049800 01  ITCFM160-RECORD.                                             ITCP9102
049900 COPY ITCFM160.                                                   ITCP9102
069310                                                                  07220000
069320 01  ITCFM165-RECORD.                                             07030000
069330 COPY ITCFM165.                                                   07040000
069400                                                                  07250000
054800 01  ITCFM1D1-RECORD.                                             05480000
054900 COPY ITCFM1D1.                                                   05490000
069700                                                                  07280000
050100 01  ITCFM1F0-RECORD.                                             ITCP9102
050200 COPY ITCFM1F0.                                                   ITCP9102
070000                                                                  07310000
095910 01  ITCFM1F1-RECORD.                                             01071
095920 COPY ITCFM1F1.                                                   01072
070300                                                                  07340000
070310 01  ITCFM1F5-RECORD.                                             07120000
070320 COPY ITCFM1F5.                                                   07130000
070330                                                                  07370000
522723 01  WS-ITCWSFC.                                                  00054400
050500 COPY ITCWSFC.                                                    ITCP9102
070600                                                                  07400000
522726 01  WS-ITCWSCC.                                                  00054700
050800 COPY ITCWSCC.                                                    ITCP9102
070900                                                                  07430000
116220 01  WS-DATE-CONVERSION.                                          01231
116230 COPY ITCWSDT.                                                    01232
071200                                                                  07460000
522732 01  WS-ITCIAPP1.                                                 00055300
051100 COPY ITCIAPP1.                                                   ITCP9102
071500                                                                  07490000
522735 01  WS-ITCIAPP2.                                                 00055600
051400 COPY ITCIAPP2.                                                   ITCP9102
071800                                                                  07520000
063000** ERROR INDICATORS FOR PROFESSIONAL SF                           05730000
063100 01  WS-PROF-ERROR-INDICATORS.                                    05740000
063200 COPY ITCIAEP.                                                    05760000
063300                                                                  05690000
522738** ERROR INDICATORS FOR INSTITUTIONAL SF
063500 01  WS-INST-ERROR-INDICATORS REDEFINES WS-PROF-ERROR-INDICATORS. 05700000
051700 COPY ITCIAEI.                                                    ITCP9102
522750                                                                  00056100
052500 01  WS-WORK-AREA.                                                ITCP9102
052600 COPY ITCIAWA.                                                    ITCP9102
064600 01  WS-WORK-AREA-R  REDEFINES WS-WORK-AREA.                      05790000
064700     05  FILLER                            PIC X(1020).           05790000
068801     05  FILLER.                                                  05790000
068810         10 WS-LOC-RCPT-DT-JUL             PIC 9(7).              05790000
072500         10 WS-SANCTION-DT                 PIC 9(8).              05790000
074300     05  FILLER                            PIC X(20).             07540000
074400     05  WS-TOTL-CHRG                      PIC S9(8)V9(2) COMP-3. 07550000
074500     05  FILLER                            PIC X(59).             07560000
064900 COPY ITCIACAE.                                                   05790000
074800                                                                  07720000
522759 01  WS-ITCIADBP.                                                 00056800
522760     COPY ITCIADBP.                                               00056900
075100                                                                  07750000
522762 01  WS-PDB-FC-RECORD.                                            00057100
522763     COPY ITCDBP16.                                               00057200
075400                                                                  07780000
522765 01  WS-ITCDBP19-RECORD.                                          00057400
522766     COPY ITCDBP19.                                               00057500
075700                                                                  07810000
522768 01  WS-ITCDBP20-RECORD.                                          00057700
522769     COPY ITCDBP20.                                               00057800
076000                                                                  07840000
522771 01  WS-ITCCSAPI.                                                 00058000
522772     COPY ITCCSAPI.                                               00058100
076300                                                                  07870000
522774 01  WS-FDB-PASSING-AREA.                                         00058300
522775     COPY ITCIADBK.                                               00058400
522776                                                                  00058500
522777 01  WS-ITCWSPSC.                                                 00058600
522778 COPY ITCWSPSC.                                                   00058700
522779                                                                  00058800
522780 01  LS-PPF-ITCDB600-PCB.                                         00058900
522790     COPY ITCPC600.                                               00059000
522800                                                                  00059100
522900 01  WS-LINK-ITDE02U1.                                            00059110
523000     05  LK-ORIG-SOURCE         PIC  X(30).                       00059120
523100     05  LK-TRAN-ID             PIC  X(32).                       00059130
523200     05  LK-USER-ID             PIC  X(32).                       00059140
523300     05  LK-FUNCT-DESC          PIC  X(60).                       00059150
523400     05  LK-RETURN-STS-DESC     PIC  X(60).                       00059160
068100                                                                  06110000
078000*01  B22-DATA-AREA.                                               07910000
068400     COPY ITDB22X1.                                               06030000
078200                                                                  08060000
078300*01  B27-DATA-AREA.                                               07940000
078400     COPY ITDB27X1.                                               07950000
078500                                                                  07960000
078501*01  IPS-DATA-AREA.                                               07970000
078502     COPY ITCLMSRH.                                               07980000
078500                                                                  08120000
078500** R18.0 E_115667 EXL                                             08130000
078501*01  CCF-DATA-AREA.                                               08140000
078502     COPY ITDCCFX1.                                               08150000
523401                                                                  00059170
523402 01  WS-END-LIT                       PIC X(44) VALUE             00059210
523403     '**** ITCP9102 WORKING-STORAGE ENDS HERE ****'.              00059300
523404                                                                  00059400
523405 LINKAGE SECTION.                                                 00059500
523406                                                                  00059600
523407 01  WS-ITSIAPC.                                                  00059700
523408 COPY ITSIAPC.                                                    00059800
523409                                                                  00059900
523410 01  WS-ITCIABP.                                                  00060000
523411 COPY ITCIABP.                                                    00060100
523412/                                                                 00060200
052800 01  WS-STACKED-CLAIM.                                            ITCP9102
052900 COPY ITCIASC.                                                    ITCP9102
523415                                                                  00060500
523416*** INSTITUTIONAL CLAIM                                           00060600
523417 01  FILLER          REDEFINES WS-STACKED-CLAIM.                  00060700
523418     05  FILLER                       PIC X(10).                  00060800
053300     COPY ITCFM105.                                               ITCP9102
168300     COPY ITCFM110.                                               01715
523430                                                                  00061100
088600     COPY ITCFM115.                                               ITCP9102
053600     COPY ITCFM120.                                               ITCP9102
053700*    SPACE OCCUPIED FOR 3 - 30 RECORDS OF 255 BYTES EACH          ITCP9102
053800     05  FILLER                       PIC X(765).                 ITCP9102
053900*    SPACE OCCUPIED FOR 3 - 31 RECORDS OF 255 BYTES EACH          ITCP9102
054000     05  FILLER                       PIC X(765).                 ITCP9102
099711*    SPACE OCCUPIED FOR 1 - 32 RECORD                             01113
099712     05  FILLER                       PIC X(255).                 01114
071410*    SPACE OCCUPIED FOR 1 - 33 RECORD                             06380000
071420     05  FILLER                       PIC X(255).                 06390000
054100*    SPACE OCCUPIED FOR 40 & 41 RECORDS OF 255 BYTES EACH         ITCP9102
127112     05  FILLER                       PIC X(255).                 01605
127140     COPY ITCFM141.                                               01606
071710*    SPACE OCCUPIED FOR 42, 43, 44 AND 45 RECORDS-255 BYTES EACH  06380000
071720     05  FILLER                       PIC X(1020).                06390000
169800*    SPACE OCCUPIED FOR 999 - 46 RECORDS OF 255 BYTES EACH        00176600
169900     05  FILLER                       PIC X(254745).              00176700
071910*    SPACE OCCUPIED FOR 1 - 47 RECORD                             06380000
071920     05  FILLER                       PIC X(255).                 06390000
054500     COPY ITCFM150.                                               ITCP9102
170100*    SPACE OCCUPIED FOR 999 - 60 RECORDS OF 255 BYTES EACH        00176900
170200     05  FILLER                       PIC X(254745).              00177000
170300*    SPACE OCCUPIED FOR 999 - 65 RECORDS OF 255 BYTES EACH        00177100
170400     05  FILLER                       PIC X(254745).              00177200
170600*    SPACE OCCUPIED FOR 999 - 66 RECORDS OF 255 BYTES EACH        00177400
170700     05  FILLER                       PIC X(254745).              00177500
072700*    SPACE OCCUPIED FOR 1 - 70 RECORD                             06520000
128340     COPY ITCFM170.                                               01621
072810*    SPACE OCCUPIED FOR 1 - 71 RECORD                             06520000
072811     05  FILLER                       PIC X(255).                 06390000
072812*    SPACE OCCUPIED FOR 1 - 72 RECORD                             06520000
055200     05  FILLER                       PIC X(255).                 ITCP9102
072814*    SPACE OCCUPIED FOR 1 - 73 RECORD                             06520000
072815     COPY ITCFM173.                                               06530000
072816*    SPACE OCCUPIED FOR 74, 75, 80 AND 90 RECORDS - 255 EACH      06520000
072900     05  FILLER                       PIC X(1020).                06540000
525300*** PROFESSIONAL CLAIM                                            00063600
525400 01  FILLER        REDEFINES WS-STACKED-CLAIM.                    00063700
525500     05  FILLER                       PIC X(10).                  00063800
055600     COPY ITCFM1A5.                                               ITCP9102
168300     COPY ITCFM1B0.                                               01715
172001     COPY ITCFM1B5.                                               00179200
525900*    SPACE OCCUPIED BY C0 RECORD  OF 255 BYTES EACH               00064200
172002     05  FILLER                       PIC X(255).                 00179200
055900     COPY ITCFM1D0.                                               ITCP9102
056000*    SPACE OCCUPIED FOR 2 - D1 RECORDS OF 255 BYTES EACH          ITCP9102
056100     05   FILLER                      PIC X(510).                 ITCP9102
101820*    SPACE OCCUPIED FOR 1 - D2 RECORD                             01142
101830     05   FILLER                      PIC X(255).                 01143
074310*    SPACE OCCUPIED FOR 1 - D3 RECORD                             06670000
074320     05   FILLER                      PIC X(255).                 06680000
091300     COPY ITCFM1E0.                                               ITCP9102
067600     COPY ITCFM1E1.                                               ITCP9102
074510*    SPACE OCCUPIED FOR 1 - E2 RECORD                             06670000
074520     05   FILLER                      PIC X(255).                 06680000
173000*    SPACE OCCUPIED FOR 999 - E6 RECORDS OF 255 BYTES EACH        00180100
173100     05  FILLER                       PIC X(254745).              00180200
173200*    SPACE OCCUPIED FOR 999 - F0 RECORDS OF 255 BYTES EACH        00180300
173300     05  FILLER                       PIC X(254745).              00180400
173400*    SPACE OCCUPIED FOR 999 - F5 RECORDS OF 255 BYTES EACH        00180500
173500     05  FILLER                       PIC X(254745).              00180600
173700*    SPACE OCCUPIED FOR 999 - F6 RECORDS OF 255 BYTES EACH        00180800
173800     05  FILLER                       PIC X(254745).              00180900
057000*    SPACE OCCUPIED FOR G0 & X0 RECORDS OF 255 BYTES EACH         ITCP9102
057100     05  FILLER                       PIC X(510).                 ITCP9102
527800     05  FILLER                       PIC X(1530).                00066100
058400                                                                  ITCP9102
528000     COPY ITCIART.                                                00066300
060300                                                                  ITCP9102
528200 01  LS-ITCRT001.                                                 00066500
528300     COPY ITCRT001.                                               00066600
186200                                                                  01936
087800*** R14.5 E_34072  EXL     ADDED THE COPYBOOK TO LINKAGE          08920000
087900 01  WS-ITDCA1X1.                                                 08930000
088000     COPY ITDCA1X1.                                               08940000
088100                                                                  08950000
528401****************************************************************  00066800
088300*** R14.5 E_34072  EXL ADDED ITDCA1X1 TO PROCEDURE DIVISION USING 08970000
528402 PROCEDURE DIVISION USING WS-ITSIAPC                              00066900
528403                          WS-ITCIABP                              00067000
528404                          WS-STACKED-CLAIM                        00067100
528405                          ITCIART                                 00067200
088800                          LS-ITCRT001                             09020000
088900                          WS-ITDCA1X1.                            09030000
075000/                                                                 ITCP9102
075100 0000-MAINLINE.                                                   ITCP9102
075200                                                                  ITCP9102
528410     MOVE 0                           TO CSAPI-RETURN-CODE.       00067610
075300     IF SIAPC-FUNCTION-CD-MAJOR = CWSCC-NORMAL-PROCESSING         ITCP9102
528412        PERFORM 0100-HOUSEKEEPING               THRU 0100-EXIT    00067800
528413        IF CIABP-EOJ                                              00067900
528414           CONTINUE                                               00068000
528415        ELSE                                                      00068100
528416           PERFORM 1000-EDIT-PROCESSING         THRU 1000-EXIT    00068200
528417        END-IF                                                    00068300
075800     ELSE                                                         ITCP9102
528419        MOVE CWSCC-INVALID-FUNCTION   TO SIAPC-RETURN-STATUS      00068500
528420        MOVE 'INVALID FUNCTION MAJOR' TO SIAPC-ERROR-MESSAGE      00068600
070800        MOVE 'SF001'   TO SIAPC-RETURN-MESSAGE-CD                 07080000
070900                          CIASC-FIRST-SCCF-EDIT-ERR-CD(1)         07090000
076300        PERFORM 9990-ERROR-DISPLAY              THRU 9990-EXIT    ITCP9102
528423     END-IF.                                                      00069000
076800                                                                  ITCP9102
090900*** R14.5 E_34072  EXL START                                      09230000
091000** FOR ADJUSTMENT SF CHECK:                                       09240000
091100***1) IPP_SUSPENSE_SEARCH FOR DUPLICATE ADJUSTMENT SF      -1340  09250000
091101***2) IPP_CLM_SEARCH FOR DUPLICATE ADJUSTMENT SF ON FDB    -1350  09260000
091102***3) IPP_MSG_SEARCH TABLE FOR MATCHING ADJ REQUEST MESSAGE-1310  09270000
091200     IF  (WS-TYPE-SUBM             = '3' OR '4')                  09280000
091300     AND CIASC-CLAIM-STATUS-CD (1) = 'V'                          09290000
091400     AND CRT001-SERV-ID        NOT = 'MISMEDIT'                   09300000
094100        IF CIABP-HOST-HOME-SITE-IND  = '1'                        09310000
094200           PERFORM 1340-CHECK-DUP-SUSPENSE THROUGH 1340-EXIT      09320000
094201        END-IF                                                    09330000
094202        IF  CIABP-HOST-HOME-SITE-IND  = '1'                       09340000
094203        AND CIASC-CLAIM-STATUS-CD (1) = 'V'                       09350000
094204        AND WS-TYPE-SUBM              = '3'                       09351000
094205           PERFORM 1350-CHECK-DUP-FDB      THROUGH 1350-EXIT      09360000
094206        END-IF                                                    09370000
094207        IF CIASC-CLAIM-STATUS-CD (1) = 'V'                        09380000
094208           PERFORM 1310-FIND-ADJ-REQUEST   THROUGH 1310-EXIT      09390000
094209        END-IF                                                    09400000
094210        IF  CIART-RT-IND             = 'Y'                        09410000
094211        AND CIASC-CLAIM-STATUS-CD(1) = 'M'                        09420000
094212            MOVE LOW-VALUES            TO CA1-NULL-INDICATORS     09430000
094213            MOVE +0                    TO CA1-TOT-CHG-AMT-NI      09440000
094214            IF WS-PROFESSIONAL                                    09450000
094215               PERFORM 1320-MAP-CA1-PROF   THROUGH 1320-EXIT      09460000
094216            ELSE                                                  09470000
094217               PERFORM 1330-MAP-CA1-INST   THROUGH 1330-EXIT      09480000
094218            END-IF                                                09490000
094219        END-IF                                                    09500000
094220     END-IF.                                                      09510000
094221*** R14.5 E_34072  EXL END                                        09520000
094222                                                                  09530000
078900     IF (CIART-RT-IND = 'C' OR                                    55120000
079000                       SIAPC-MODULE-NAME (1) = 'ITCA9080')        55120000
079100        AND SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                   50610000
079210        MOVE CIACAE-RECORD TO SIAPC-ERROR-MESSAGE                 50610000
079300     END-IF.                                                      55120000
079500                                                                  07120000
094229*** R14.5 E_34072  EXL REMOVE CHECK FOR WS-TYPE-SUBM              09600000
094230*** R14.5 PD_57538 SKG BYPASS 1800- IF JOB ABENDED (CIABP-EOJ)    09610000
094231     IF  CIABP-EOJ                                                09620000
094232         NEXT SENTENCE                                            09630000
094233     ELSE                                                         09640000
094234        IF   CDBP16-SET-TMC-IND-SF     = 'Y'                      09650000
094235        AND  CIABP-HOST-HOME-SITE-IND  = '1'                      09660000
094236        AND (CIABP-STAT-CD             = 'V' OR 'R')              09670000
094300        AND  CIART-RT-IND              = 'N'                      09680000
094400***  AND (WS-TYPE-SUBM              = '1' OR '2')                 09690000
094500        AND  CRT001-CLM-EST-IND        = ' '                      09700000
094600        AND  CIABP-TRANSM-MODE-CD      < '7'                      09710000
094700             PERFORM 1800-VERIFY-TMC             THROUGH 1800-EXIT09720000
094800        END-IF                                                    09730000
185010     END-IF.                                                      15850000
185012                                                                  07120000
528425     GOBACK.                                                      00069400
076900                                                                  ITCP9102
528427 0100-HOUSEKEEPING.                                               00069600
077100                                                                  ITCP9102
528429     IF WS-FIRST-TIME-SW     = 'Y'
071800        IF CIART-RT-IND      = 'N'                                07180000
528431           DISPLAY WS-PROGRAM-DISPLAY                             00069700
528432           MOVE WS-Y          TO CIABP-LEVEL-ONE-EDIT-SW          00070300
528433                                 CIABP-LEVEL-2-INST-EDIT-SW       00070400
528434                                 CIABP-LEVEL-3-INST-EDIT-SW       00070500
528435                                 CIABP-LEVEL-2-PROF-EDIT-SW       00070600
528436                                 CIABP-LEVEL-3-PROF-EDIT-SW       00070700
528437        END-IF
528438     END-IF.
528439
096500** R17.5 E_112726 EXL  INIT TWO WS-CST- SWITCHES TO 'N'           10110000
528440     MOVE 'N'                 TO CIABP-CLAIM-TRAILER-RECAL-IND    00069800
528450                                 CIABP-SF-MSG-CD-IND              00070000
528460                                 CIART-SPEC-NOTE-IND
096900                                 WS-BYPASS-EDITS-IND              10150000
                                       WS-CST-GRP-CLASS-LN              10160000
                                       WS-CST-ENTERED-SW.               10170000
528471                                                                  00070200
081700     IF CIART-RT-IND = 'Y' OR 'C'                                 07330000
528473        MOVE 'Y'              TO CIABP-LEVEL-ONE-EDIT-SW          00070300
528474                                 CIABP-LEVEL-2-INST-EDIT-SW       00070400
528475                                 CIABP-LEVEL-3-INST-EDIT-SW       00070500
528476                                 CIABP-LEVEL-2-PROF-EDIT-SW       00070600
528477                                 CIABP-LEVEL-3-PROF-EDIT-SW       00070700
528478        MOVE 'N'              TO CIABP-EOJ-IND
528479                                 CIABP-PLAN-PROF-EDIT-SW          00070900
082410                                 CIABP-PLAN-INST-EDIT-SW          07400000
082500     ELSE                                                         07410000
082700        IF WS-FIRST-TIME-SW       = 'Y'                           07430000
082800            MOVE 'Y'          TO CIABP-PLAN-PROF-EDIT-SW          07440000
082810                                 CIABP-PLAN-INST-EDIT-SW          07440000
082900        END-IF                                                    07450000
528493     END-IF.
528494
083200     MOVE 'Y'                 TO WS-VALID-SCCF                    07480000
653800                                 WS-CLASS-PROV-CLM-OK-SW.         00071400
083400                                                                  07500000
083600     MOVE LOW-VALUES          TO CIACAE-RECORD.                   07510000
653801                                                                  00071200
096500** R17.5 E_112726 EXL  INIT WS-CST-GRP-TBL                        10410000
653802     MOVE SPACES              TO WS-CLM-TYPE-IND                  00071300
653803                                 WS-F0-CLASS-PROV-PAR-IND         00071000
653804                                 WS-F0-CLASS-PROV-NONPAR-IND      00071000
653805                                 WS-CLASS-PROV-LN-BLANK-IND
653806                                 WS-CLASS-PROV-LN-OK-SW           00071100
084300                                 WS-PROF-ERROR-INDICATORS         07560000
100200                                 WS-REF-DATA-TBL                  10270000
100200                                 WS-CST-GRP-TBL                   10490000
653809                                 CIABP-HEADER-CLAIM-TYPE          00071800
653810                                 CIABP-PROV-DATA-IND              00071900
653811                                 CIABP-PRC-DATA-IND               00072000
653812                                 CIABP-STAT-CD                    00072100
653813                                 CIABP-HOLD-DUP-ERR-CD            00072200
653814                                 CIABP-INST-CLM-REC-IND           00072300
653815                                 CIABP-PLAN-PROF-STND-IND         00072400
653816                                 CIABP-DF-LOC-PLAN-RCPT-DT        00072500
653817                                 CIABP-TRANS-ID-IND               00072600
653818                                 CIABP-REC-TYPE-IND               00072700
653819                                 CIABP-SEQ-NUM-IND                00072800
653820                                 CIABP-DUP-REC-IND                00072900
101500                                 CIABP-FOUND-F0-F1-IND            10400000
                                       WS-WORK-SWITCHES
101510                                 WS-U200-1-IND.                   10400000
123130                                                                  01371
526800*R16.0 E_101251 PMP SET WS-PROV-NPI-ALL-LN-BLANK                  54390000
           MOVE 'Y'                 TO WS-PROV-NPI-ALL-LN-BLANK.

086000     IF CIART-RT-IND = 'Y' OR 'C'                                 07720000
653842        MOVE SPACE            TO CIABP-SF-IO-RETURN-IND           00071700
653846                                 CIASC-CLAIM-STATUS-CD(1)         00073000
077410                                 WS-REF-REL-CD-TBL                07740000
077420                                 WS-REF-SRC-PMT-TBL               07740000
077421        SET  REF-REL-CD-I     TO 1                                07730000
077422        SET  REF-SRC-PMT-I    TO 1                                07730000
653843     END-IF.                                                      00073100
157628                                                                  02300
102800** R18.0 E_115667 EXL ADDED INIT FOR WS-CCF-LN-NUM                10750000
653849     MOVE ZERO                TO CIABP-INST-SUB-REC-IND           00073200
653850                                 CIABP-F0-CTR
653860                                 CIABP-F1-CTR
094500                                 WS-NON-TRANSM-CNT                07800000
                                       WS-CCF-LN-NUM                    10800000
094510                                 WS-LOC-RCPT-DT.                  07800000
157631                                                                  02303
656500     MOVE '99999999'          TO WS-EARLIEST-SERV-DT.             00073500
656600     MOVE 006.00              TO CIABP-CAPPED-ACCESS-FEE-PCT.     00073600
656601     MOVE CRT001-HOST-HOME-CD      TO CIABP-HOST-HOME-SITE-IND.   00073700
656602     MOVE CIASC-SCCF-SERIAL-NUM(1) TO CIABP-SCCF-SER-NUM.         00073800
157634                                                                  02306
656604     MOVE CIART-CURR-TS(1:2)     TO SIAPC-GREG-CURRENT-CENTURY.   00074000
656605     MOVE CIART-CURR-TS(3:2)     TO SIAPC-GREG-CURRENT-YEAR.      00074100
656606     MOVE CIART-CURR-TS(6:2)     TO SIAPC-GREG-CURRENT-MONTH.     00074200
656607     MOVE CIART-CURR-TS(9:2)     TO SIAPC-GREG-CURRENT-DAY.       00074300
656608     MOVE CIART-CURR-TS(12:2)    TO SIAPC-CURRENT-HOUR.           00074400
656609     MOVE CIART-CURR-TS(15:2)    TO SIAPC-CURRENT-MINUTE.         00074500
656610     MOVE CIART-CURR-TS(18:2)    TO SIAPC-CURRENT-SECOND.         00074600
157637                                                                  02309
656612     MOVE SIAPC-GREG-CURRENT-DT    TO WS-GREG-DT.                 00074800
656613     COMPUTE WS-JULIAN-DT = FUNCTION DAY-OF-INTEGER               00074900
656614           (FUNCTION INTEGER-OF-DATE(WS-GREG-DT)).                00075000
656615     MOVE WS-JULIAN-DT       TO SIAPC-JULIAN-CURRENT-DT.          00075100
157640                                                                  02312
656617     PERFORM 9900-RESET-SIAPC-RETURN-DATA.                        00075300
157643                                                                  02315
656619     PERFORM 0200-PROCESS-PARAMETER-DB        THRU 0200-EXIT.     00075500
157646                                                                  02318
656621     IF CIABP-EOJ                                                 00075400
656622        GO TO 0100-EXIT                                           00075400
656623     END-IF.                                                      00075400
656624                                                                  00075400
656625     IF  WS-FIRST-TIME-SW         = 'Y'                           00078400
080900     AND CIART-RT-IND             = 'N'                           08090000
656627        PERFORM 9060-CALL-TBL-LOAD-MODULE     THRU 9060-EXIT      00075400
081001*R11.3 E_10097 DSL BEGIN                                          05470000
081002        MOVE SPACE            TO WS-REF-REL-CD-TBL                07740000
081003                                 WS-REF-SRC-PMT-TBL               07740000
081004        SET  REF-REL-CD-I     TO 1                                07730000
081005        SET  REF-SRC-PMT-I    TO 1                                07730000
081010        PERFORM 9070-GET-REL-CD               THRU 9070-EXIT      08100000
081020        PERFORM 9080-GET-SRC-PMT              THRU 9080-EXIT      08100000
081030*R11.3 E_10097 END                                                05470000
185133
185134*R12.0 E_59024 EXL ADD CODE TO GET BATCH ONLY STATION CODES
185135        PERFORM 9090-GET-TMC1-STN-CD          THRU 9090-EXIT      08100000
656628     END-IF.
656629
656630     MOVE 'N'     TO WS-FIRST-TIME-SW.
656631                                                                  00075600
656632 0100-EXIT.  EXIT.                                                00075700
656633                                                                  00075800
656634 0200-PROCESS-PARAMETER-DB.                                       00075900
656635                                                                  00076000
656636     IF CIABP-HOST-HOME-SITE-IND = '1' OR '2'                     00076100
656637        NEXT SENTENCE                                             00076200
656638     ELSE                                                         00076300
656639        MOVE CWSCC-UNEXPECTED-SYS-PASS-DATA                       00076400
656640                                    TO SIAPC-RETURN-STATUS        00076500
656650        MOVE WS-PARM-FILE-MSG       TO SIAPC-ERROR-MESSAGE        00076600
082500        MOVE 'SF002'        TO SIAPC-RETURN-MESSAGE-CD            08250000
082600                               CIASC-FIRST-SCCF-EDIT-ERR-CD(1)    08260000
656670        PERFORM 9990-ERROR-DISPLAY            THRU 9990-EXIT      00076900
656680        GO TO 0200-EXIT.                                          00077000
656681                                                                  00077100
656682     PERFORM 0220-READ-PDB-16-REC             THRU 0220-EXIT.     00077200
656683                                                                  00077300
656684     IF CIABP-EOJ                                                 00077400
656685        GO TO 0200-EXIT                                           00077500
656686     END-IF.                                                      00077600
656687                                                                  00077700
656688     PERFORM 0240-READ-PDB-20-REC             THRU 0240-EXIT.     00077800
656689                                                                  00077900
656690     IF CIABP-EOJ                                                 00078000
656700        GO TO 0200-EXIT                                           00078100
656701     END-IF.                                                      00078200
656702                                                                  00078300
094400** FOR REAL TIME OR CLAIMS ADMIN IF SCCF IS BLANK, ACCESS PARM    08420000
094500** RECORD 22 TO GET NEXT AVAILABLE SCCF. IN BATCH IT SHOULD BE    08420000
094600** SET IN ITCP9002                                                08430000
094700**R11.4 E_38714 18.2  DSL BEGIN, ADDED CHECK FOR 'C'              07320000
094800     IF  CIART-RT-IND = ('Y' OR 'C')                              08450000
656707     AND CIABP-HOST-HOME-SITE-IND = '1'                           00078400
656708     AND CIASC-SCCF-SERIAL-NUM(1) = SPACE                         00078500
084800         PERFORM 0270-ASSIGN-SCCF    THRU 0270-EXIT               08480000
084900         IF CSAPI-SUCCESSFUL                                      08490000
085000            MOVE CSAPI-SCCF-SER-NUM  TO CIASC-SCCF-SERIAL-NUM(1)  08500000
085100                                        CIADBK-SCCF-SERIAL-NUMBER 08510000
085200                                        CIART-SCCF-ID             08520000
095600            IF WS-PROFESSIONAL                                    15850000
095700               MOVE 'R' TO FM1A5-SYS-CRT-CD                       08620000
095800                           FM1A5-SYS-UPDT-CD                      08620000
095900            ELSE                                                  15850000
096000               MOVE 'R' TO FM105-SYS-CRT-CD                       08620000
096100                           FM105-SYS-UPDT-CD                      08620000
096200            END-IF                                                15850000
096300**R11.4 E_38714 END                                               07320000
085300         END-IF                                                   08530000
656713     ELSE                                                         00079000
656714         MOVE CIASC-SCCF-SERIAL-NUM(1)                            00079100
656715                                  TO CIADBK-SCCF-SERIAL-NUMBER.   00079200
656716                                                                  00079300
656717     IF CIABP-EOJ                                                 00079400
656718        GO TO 0200-EXIT                                           00079500
656719     END-IF.                                                      00079600
656720                                                                  00079700
656715     MOVE CIADBK-SCCF-SERIAL-NUMBER TO PART-SCCF-ID.              00079200
656730     PERFORM 1205-CALL-PART-KEY-NUM.                              00079800
803795     MOVE PART-KEY-NUM              TO CIART-PART-KEY-NUM         00310400
803794                                       CIADBK-FORMATS-DB-NUMBER.  00310300
656740
097800** FOR REAL TIME OR CLAIMS ADMIN GET SPECIFIC PARM RECORD 19      08670000
097900** RELATED TO THIS SF.                                            08670000
656760** FOR BATCH LOAD SWSFC TABLE WITH RECORD 19 INFO WHEN CALING     00125101
656770** ITCP9102 FOR THE FIRST TIME                                    00125101
098200**R11.4 E_38714 18.2  ADDED CHECK FOR 'C'                         07320000
098300     IF CIART-RT-IND = 'Y' OR 'C'                                 08700000
656772        PERFORM 0290-READ-PDB-19-REC       THRU 0290-EXIT         00125400
656773     ELSE
656774        IF WS-FIRST-TIME-SW    = 'Y'
656775           PERFORM 0295-FETCH-PDB-19-REC   THRU 0295-EXIT         00125400
656776     END-IF.                                                      00125500
656777                                                                  00079900
656778 0200-EXIT.  EXIT.                                                00080000
656779                                                                  00080100
656780*********************************************************         00080200
656781 0220-READ-PDB-16-REC.                                            00080300
656782                                                                  00080400
656783     MOVE CWSCC-READ-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR.    00080500
656784     MOVE SPACES                   TO SIAPC-FUNCTION-CD-SUB.      00080600
656785                                                                  00080700
656786     MOVE SPACES                   TO CIADBP-DATA.                00080800
656787     MOVE '16'                     TO CIADBP-REC-TYPE.            00080900
656788     MOVE CIABP-ENVIRONMENT-IND    TO CIADBP-BATCH-ONLINE-IND.    00081000
656789                                                                  00081120
656790     MOVE SPACE                    TO CSAPI-COMMAND.              00081200
656800     MOVE 'ITCP9102'               TO CSAPI-CALLING-PGM-ID.       00081300
656801     IF CIABP-BATCH                                               00081400
656802        SET CSAPI-BATCH-ENV        TO TRUE                        00081410
656803     ELSE                                                         00081420
656804        SET CSAPI-ON-LINE-ENV      TO TRUE                        00081430
656805     END-IF.                                                      00081440
656806     SET WS-ITPP01U1               TO TRUE.                       00081500
656807                                                                  00081510
656808     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00081600
656809                                   WS-ITSIAPC                     00081700
656810                                   WS-ITCIADBP                    00081800
656820                                   WS-ITCWSCC                     00081900
656830                                   CIART-BUS-OWNER-ID-PARM.       00082000
656831                                                                  00082100
656832     IF CSAPI-RETURN-CODE  = 0                                    00082200
656833        MOVE CIADBP-RECORD            TO CDBP16-AREA              00082300
656834        MOVE CDBP16-TRANSACTION-IND   TO CIABP-TRANS-IND          00082400
656835        MOVE CDBP16-INSTALLATION-PLN-CD                           00082500
656836                                      TO CIABP-INSTALL-PLAN-CD    00082600
656837     ELSE                                                         00082700
656838        MOVE CWSCC-READ-ERROR         TO SIAPC-RETURN-STATUS      00082800
656839        MOVE '0220- BAD READ PARM_DATA RECORD 16'                 00082900
656840                                      TO SIAPC-ERROR-MESSAGE      00083000
091300        MOVE 'SF003'        TO SIAPC-RETURN-MESSAGE-CD            09130000
091400                               CIASC-FIRST-SCCF-EDIT-ERR-CD(1)    09140000
656842        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00083200
656843        GO TO 0220-EXIT                                           00083300
656844     END-IF.                                                      00083400
656845                                                                  00083500
656846     IF   CDBP16-NO-OF-FORMATS-DBS NUMERIC                        00083600
656847     AND  CDBP16-VALID-NO-OF-FORMATS                              00083700
656848     AND (CDBP16-CALL-PLN-EDIT-EXIT        OR                     00083800
656849          CDBP16-DONT-CALL-PLN-EDIT-EXIT)                         00083900
656850     AND (CDBP16-BATCH-CALL-PLN-MOD        OR                     00084000
656851          CDBP16-BATCH-DONT-CALL-PLN-MOD)                         00084100
656852     AND  CDBP16-INSTALLATION-PLN-CD NUMERIC                      00084200
656853     AND  CDBP16-PRODUCTION OR CDBP16-TEST                        00084300
656854          MOVE CDBP16-NO-OF-FORMATS-DBS  TO                       00084400
656855               CIADBK-NUMBER-OF-FORMATS-DBS                       00084500
656856     ELSE                                                         00084600
656857          MOVE CWSCC-UNEXPECTED-SYS-PASS-DATA                     00084700
656858                TO SIAPC-RETURN-STATUS                            00084800
656860          MOVE WS-PDB-CONTENTS-MSG  TO SIAPC-ERROR-MESSAGE        00085100
093300          MOVE 'SF004'         TO SIAPC-RETURN-MESSAGE-CD         09330000
093400                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 09340000
656870          PERFORM 9990-ERROR-DISPLAY        THRU 9990-EXIT.       00085200
656871                                                                  00085300
656872 0220-EXIT.  EXIT.                                                00085400
093800*********************************************************         09380000
657611 0240-READ-PDB-20-REC.                                            00093700
082000                                                                  ITCP9102
657613     MOVE CWSCC-READ-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR.    00093900
657614     MOVE SPACES                   TO SIAPC-FUNCTION-CD-SUB.      00094000
657615                                                                  00094100
657616     MOVE CWSCC-READ-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR.    00094200
657617     MOVE SPACES                   TO SIAPC-FUNCTION-CD-SUB.      00094300
083100                                                                  ITCP9102
657619     MOVE SPACES                   TO CIADBP-DATA.                00094500
657620     MOVE '20'                     TO CIADBP-REC-TYPE.            00094600
657630     MOVE CIABP-ENVIRONMENT-IND    TO CIADBP-BATCH-ONLINE-IND.    00094710
083300                                                                  ITCP9102
657650     MOVE SPACE                    TO CSAPI-COMMAND.              00094900
657660     MOVE 'ITCP9102'               TO CSAPI-CALLING-PGM-ID.       00095000
657661     IF CIABP-BATCH                                               00095010
657662        SET CSAPI-BATCH-ENV        TO TRUE                        00095020
083800     ELSE                                                         ITCP9102
657664        SET CSAPI-ON-LINE-ENV      TO TRUE                        00095040
657665     END-IF.                                                      00095050
657666     SET WS-ITPP01U1               TO TRUE.                       00095200
657667     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00095300
657668                                   WS-ITSIAPC                     00095400
657669                                   WS-ITCIADBP                    00095500
657670                                   WS-ITCWSCC                     00095600
657680                                   CIART-BUS-OWNER-ID-PARM.       00095700
085900                                                                  ITCP9102
657682     IF CSAPI-RETURN-CODE NOT = 0                                 00095900
657683        MOVE CWSCC-READ-ERROR         TO SIAPC-RETURN-STATUS      00096000
657684        MOVE '0240- BAD READ PARM_DATA RECORD 20'                 00096100
657685                                      TO SIAPC-ERROR-MESSAGE      00096200
096900        MOVE 'SF007'           TO SIAPC-RETURN-MESSAGE-CD         09690000
097000                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 09700000
657687        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00096400
657688        GO TO 0240-EXIT.                                          00096500
251197                                                                  00263800
657690     MOVE CIADBP-RECORD             TO WS-ITCDBP20-RECORD.        00096700
657691     MOVE CDBP20-835-MOD-YYYY (1:2) TO CWSDT-J-CENTURY.           00096800
657692     MOVE CDBP20-835-MOD-YYYY (3:2) TO CWSDT-J-YEAR.              00096900
657693     MOVE CDBP20-835-MOD-DDD        TO CWSDT-J-DAY.               00097000
657694     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.         00097100
086500                                                                  ITCP9102
657696     MOVE CDBP20-NPI-MOD-YYYY (1:2) TO CWSDT-J-CENTURY.           00097300
657697     MOVE CDBP20-NPI-MOD-YYYY (3:2) TO CWSDT-J-YEAR.              00097400
657698     MOVE CDBP20-NPI-MOD-DDD        TO CWSDT-J-DAY.               00097500
657699     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.         00097600
095400                                                                  ITCP9102
657800     MOVE CDBP20-AF-MOD-YYYY (1:2)  TO CWSDT-J-CENTURY.           00097800
657900     MOVE CDBP20-AF-MOD-YYYY (3:2)  TO CWSDT-J-YEAR.              00097900
658000     MOVE CDBP20-AF-MOD-DDD         TO CWSDT-J-DAY.               00098000
185324     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.         09880000
185325                                                                  09890000
185326*R12.0 E_19137 DSL BEGIN
185327     MOVE CDBP20-ICD-10-COMPLNCE-YYYY (1:2)  TO CWSDT-J-CENTURY.
185328     MOVE CDBP20-ICD-10-COMPLNCE-YYYY (3:2)  TO CWSDT-J-YEAR.
185329     MOVE CDBP20-ICD-10-COMPLNCE-DDD         TO CWSDT-J-DAY.
658001     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.         00098100
185331*R12.0 E_19137 DSL END
167336                                                                  01786
118220*R13.5 E_79779 DSL BEGIN
118230     MOVE CDBP20-TOB33-REMOVAL-YYYY   (1:2)  TO CWSDT-J-CENTURY.
118240     MOVE CDBP20-TOB33-REMOVAL-YYYY   (3:2)  TO CWSDT-J-YEAR.
118250     MOVE CDBP20-TOB33-REMOVAL-DDD           TO CWSDT-J-DAY.
118260     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.
118270*R13.5 E_79779 DSL END
118300                                                                  09890000
123100*R14.0 E_84725 DSL BEGIN
123200     MOVE CDBP20-PROVIDER-SANCTION-YYYY (1:2) TO CWSDT-J-CENTURY.
123300     MOVE CDBP20-PROVIDER-SANCTION-YYYY (3:2) TO CWSDT-J-YEAR.
123400     MOVE CDBP20-PROVIDER-SANCTION-DDD        TO CWSDT-J-DAY.
123500     PERFORM 0245-VALIDATE-PDB-20-DATES   THRU 0245-EXIT.
123510     PERFORM 0249-CONVERT-TO-GREG         THRU 0249-EXIT.
123600*R14.0 E_84725 DSL END
123700                                                                  09890000
167337 0240-EXIT.  EXIT.                                                01787
267000                                                                  26700000
267300 0245-VALIDATE-PDB-20-DATES.                                      26730000
267400                                                                  26740000
267500     MOVE 'TJ'                       TO   CWSDT-DC-FROM           26750000
267600     MOVE 'VL'                       TO   CWSDT-DC-TO             26760000
267700     MOVE '1'                        TO   CWSDT-DATE-IND          26770000
267800     PERFORM X100-ITCPDDT-DATECONV  THRU X100-ITCPDDT-DC-EXIT     26780000
267900                                                                  26790000
658012     IF CWSDT-G-DATE                  = '99999999'                00099200
268100        MOVE CWSCC-READ-ERROR        TO SIAPC-RETURN-STATUS       26810000
268200        MOVE WS-PDB-MISSING-REC-MSG  TO SIAPC-ERROR-MESSAGE       26820000
100200        MOVE 'SF008'           TO SIAPC-RETURN-MESSAGE-CD         10020000
100300                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 10030000
268500        PERFORM 9990-ERROR-DISPLAY              THRU 9990-EXIT    26850000
658017     END-IF.                                                      00099800
268800                                                                  26880000
268900 0245-EXIT.  EXIT.                                                26890000
125600                                                                  09910000
125700*R14.0 E_84725 DSL BEGIN                                          11730000
126520 0249-CONVERT-TO-GREG.                                            09920000
126530                                                                  09930000
126540     MOVE CDBP20-PROVIDER-SANCTION-DATE TO WS-JULIAN-DT.          09940000
126550     COMPUTE WS-GREG-DT = FUNCTION  DATE-OF-INTEGER               09950000
126560         (FUNCTION INTEGER-OF-DAY (WS-JULIAN-DT)).                09960000
126570     MOVE WS-GREG-DT TO WS-SANCTION-GREG-DT                       09970000
126571                        WS-SANCTION-DT.                           09970000
126580                                                                  09980000
126591 0249-EXIT.  EXIT.                                                10070000
126592*R14.0 E_84725 DSL END                                            11730000
658020************************************************************      00100100
100900 0270-ASSIGN-SCCF.                                                10090000
178820                                                                  01959
101100     INITIALIZE                       WS-ITCCSAPI.                10110000
101200     SET CSAPI-GET-NEXT-SEQ        TO TRUE.                       10120000
101300     MOVE CIART-BUS-OWNER-ID-PARM  TO CSAPI-BOID.                 10130000
112700*11.4 E_43280 RBG BEGIN TO PASS LOC PLAN CODE                     11730000
112800     IF WS-PROFESSIONAL
112900        MOVE FM1A5-LOC-PLAN-CD     TO WS-LOC-PLAN-CD
113000     ELSE
113100        MOVE FM105-LOC-PLAN-CD     TO WS-LOC-PLAN-CD
113200     END-IF.
113300*11.4 E_43280 RBG END                                             11730000
178880                                                                  01966
101500     SET WS-ITDSCCF1               TO TRUE.                       10150000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    13800000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   13810000
113600*11.4 E_43280 RBG -TO PASS LOC PLAN CODE                          11730000
113700     CALL WS-CALL-MODULE        USING WS-ITCCSAPI                 10160000
113800                                      WS-LOC-PLAN-CD.             10170000
658062     IF CSAPI-RETURN-CODE  = 0                                    00102100
101900        NEXT SENTENCE                                             10190000
658064     ELSE                                                         00102300
658065        MOVE CSAPI-RETURN-CODE        TO SIAPC-RETURN-STATUS      00102400
102200        MOVE '0270- BAD ASSIGN OF SCCF NUMBER'                    10220000
658067                                      TO SIAPC-ERROR-MESSAGE      00102600
658068        MOVE 'SF009'                  TO SIAPC-RETURN-MESSAGE-CD  00102700
658069        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00102800
658071     END-IF.                                                      00103000
658072                                                                  00103100
658137 0270-EXIT.  EXIT.                                                00108300
658139**************************************************************    00108500
658140***   GET ACCESS FEE                                              00108600
658141**************************************************************    00108700
178955 0280-ACCESS-PDB-21-FOR-RATE.                                     02003
178956                                                                  02004
           MOVE SPACES TO WS-ACCT-TYPE-21
           IF (CIAPP2-DELIVERY-METHOD = 'J'          AND
               CIAPP2-ACCESS-FEE-CD   = ('1' OR '4') AND
               CIAPP2-PROGRAM-CD      = 'A'          AND
               FM105-REL-NUM >= '110')

              IF CIAPP2-ACCT-TYPE-CD    = 'B'
                 MOVE 'B' TO WS-ACCT-TYPE-21
273968        ELSE
124600        IF CIAPP2-NATIONAL-OOA-CD = '3' AND                       10440000
124610           CIAPP2-ACCT-TYPE-CD NOT = ('D' AND 'E' AND 'F')        10410000
                  MOVE 'A' TO WS-ACCT-TYPE-21
              ELSE
              IF (CIAPP2-ACCT-TYPE-CD    = 'A' AND
                  CIAPP2-NATIONAL-OOA-CD = '1')
                  MOVE 'X' TO WS-ACCT-TYPE-21.

           IF WS-ACCT-TYPE-21 > SPACES
              NEXT SENTENCE
273980     ELSE                                                         00281800
125800*13.5 E_76693 DSL ADD TYPE CD D/E/G/H                             10540000
125900      IF CIAPP2-ACCT-TYPE-CD = ('B' OR 'C' OR 'D' OR 'E' OR 'G'   10550000
125910                                    OR 'H')                       10550000
126000      OR (CIAPP2-NATIONAL-OOA-CD = '3' AND                        10560000
126001          CIAPP2-ACCT-TYPE-CD NOT = 'F')                          10550000
126100          MOVE CIAPP2-ACCT-TYPE-CD   TO WS-ACCT-TYPE-21           10570000
126200      ELSE                                                        10580000
126300        IF CIAPP2-ACCT-TYPE-CD = 'F'                              10630000
126400           MOVE 'X' TO WS-ACCT-TYPE-21                            10630000
659500        ELSE
126600           IF CIAPP2-PROGRAM-CD   = '9'                           10590000
126700           AND CIAPP2-ACCESS-FEE-CD = '1'                         10600000
126800               EVALUATE CIAPP2-DELIVERY-METHOD                    10610000
126900                  WHEN '7'                                        10620000
127000                     MOVE 'M' TO WS-ACCT-TYPE-21                  10630000
127100                  WHEN 'A'                                        10640000
127200                     MOVE 'N' TO WS-ACCT-TYPE-21                  10650000
127300                  WHEN 'B'                                        10660000
127400                     MOVE 'O' TO WS-ACCT-TYPE-21                  10670000
127500                  WHEN 'C'                                        10680000
127600                     MOVE 'P' TO WS-ACCT-TYPE-21                  10690000
127700                  WHEN 'D'                                        10700000
127800                     MOVE 'Q' TO WS-ACCT-TYPE-21                  10710000
127900                  WHEN 'E'                                        10720000
128000                     MOVE 'R' TO WS-ACCT-TYPE-21                  10730000
128100                  WHEN 'F'                                        10740000
128200                     MOVE 'S' TO WS-ACCT-TYPE-21                  10750000
128300                  WHEN OTHER                                      10760000
128400                     MOVE 'X' TO WS-ACCT-TYPE-21                  10770000
128500               END-EVALUATE                                       10780000
128600           ELSE                                                   10790000
128700              MOVE 'X'     TO WS-ACCT-TYPE-21                     10800000
128800           END-IF                                                 10810000
128900      END-IF                                                      10810000
659602     END-IF.                                                      00110000
274900                                                                  27490000
275000     EVALUATE WS-ACCT-TYPE-21                                     27500000
129300*13.5 E_76693 DSL ADD D/E/G/H                                     11630000
108500     WHEN 'C'                                                     10850000
129500     WHEN 'D'                                                     10850000
129600     WHEN 'E'                                                     10850000
129610     WHEN 'G'                                                     10850000
129620     WHEN 'H'                                                     10850000
108600         MOVE CIABP-INCUR-DT-YY TO WS-INCUR-YR-21                 10990000
108601*11.3 E_49671 END                                                 11630000
275100     WHEN 'B'                                                     27510000
178964         IF CIABP-INCUR-DT (1:4) < '2004'                         02011
178965         AND CDBP16-PRODUCTION                                    02012
178966             MOVE '04' TO WS-INCUR-YR-21                          02013
178967         ELSE                                                     02014
178968             MOVE CIABP-INCUR-DT-YY TO WS-INCUR-YR-21             02015
178969         END-IF                                                   02016
275800                                                                  27580000
275900     WHEN 'A'                                                     27590000
178971         IF  FM105-REL-NUM >= '087'                               02018
178972             IF  CIABP-INCUR-DT (1:4) < '2003'                    02019
178973             AND CDBP16-PRODUCTION                                02020
178975                 MOVE '03' TO WS-INCUR-YR-21                      02021
178976             ELSE                                                 02022
178977                 MOVE CIABP-INCUR-DT-YY TO WS-INCUR-YR-21         02023
178978             END-IF                                               02024
178979         END-IF                                                   02025
276800                                                                  27680000
276900     WHEN 'X'                                                     27690000
277000         IF CIABP-INCUR-DT (1:4) < CDBP20-AF-MOD-YYYY             27700000
277100             MOVE CDBP20-AF-MOD-DDD-2-3 TO WS-INCUR-YR-21         27710000
277200         ELSE                                                     27720000
277300             MOVE CIABP-INCUR-DT-YY TO WS-INCUR-YR-21             27730000
277400         END-IF                                                   27740000
277612     WHEN  'M'                                                    00284903
277613     WHEN  'N'                                                    00284904
277614     WHEN  'O'                                                    00284905
277615     WHEN  'P'                                                    00284906
277616     WHEN  'Q'                                                    00284907
277617     WHEN  'R'                                                    00284908
277618     WHEN  'S'                                                    00284909
277620         IF CIABP-INCUR-DT (1:4) < '2009'                         00284910
277630         AND CDBP16-PRODUCTION                                    00284911
277640             MOVE '09' TO WS-INCUR-YR-21                          00284912
277650         ELSE                                                     00284913
277660             MOVE CIABP-INCUR-DT-YY TO WS-INCUR-YR-21             00284914
277670         END-IF                                                   00284915
660002     END-EVALUATE.                                                00112700
178982                                                                  02028
660004     PERFORM 9900-RESET-SIAPC-RETURN-DATA.                        00109000
660005     MOVE CWSCC-READ-FILES-REQUEST   TO SIAPC-FUNCTION-CD-MAJOR.  00109100
660006     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB.    00109200
660007     MOVE SPACES                     TO CIADBP-DATA.              00109300
660008     MOVE CIABP-ENVIRONMENT-IND      TO CIADBP-BATCH-ONLINE-IND.  00109310
178984                                                                  02030
660010     MOVE WS-HOLD-21-KEY             TO CIADBP-KEY.               00112900
178987                                                                  02033
660030     MOVE SPACE                    TO CSAPI-COMMAND.              00113100
660040     MOVE 'ITCP9102'               TO CSAPI-CALLING-PGM-ID.       00113200
660050
660051     IF CIABP-BATCH                                               00113310
660052        SET CSAPI-BATCH-ENV        TO TRUE                        00113320
660053     ELSE                                                         00113330
660054        SET CSAPI-ON-LINE-ENV      TO TRUE                        00113340
660055     END-IF.                                                      00113350
660056
660057     SET WS-ITPP01U1               TO TRUE.                       00113400
660058     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00113500
660059                                   WS-ITSIAPC                     00113600
660060                                   WS-ITCIADBP                    00113700
660070                                   WS-ITCWSCC                     00113800
660080                                   CIART-BUS-OWNER-ID-PARM.       00113900
660090                                                                  00114000
660100     IF CSAPI-SUCCESSFUL                                          00114000
660200        NEXT SENTENCE
660300     ELSE
660400        IF CSAPI-NOT-FOUND                                           00114
660500           PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT
660600           IF WS-SUB1 < 5
660700           OR FM105-ERR-CD (5) =  SPACES
660800              IF WS-INSTITUTIONAL
660900                 MOVE 'IRL1N' TO FM105-ERR-CD (WS-SUB1)
661000              ELSE
661100                 MOVE 'PRL11' TO FM1A5-ERR-CD (WS-SUB1)
661200              END-IF
661300           END-IF
661400           GO TO 0280-EXIT
661401        ELSE
661402***  BAD CSAPI-RETURN-CODE  ****                                  00114100
661403           MOVE CWSCC-READ-ERROR         TO SIAPC-RETURN-STATUS   00114200
661404           MOVE '0280- BAD READ PARM_DATA RECORD 21'              00114300
661405                                         TO SIAPC-ERROR-MESSAGE   00114400
116700           MOVE 'SF012'        TO SIAPC-RETURN-MESSAGE-CD         11670000
116800                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 11680000
661407           PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT    00114600
661408           GO TO 0280-EXIT                                        00114700
661409       END-IF.                                                    00114800
661410                                                                  00114800
661420*** CSAPI-RETURN-CODE WAS SUCCESSFUL  ****                        00114100
661430     MOVE CIADBP-RECORD  TO WS-ITCDBP21-RECORD.                   00114900
661440                                                                  00115000
661450     IF CDBP21-ACCT-TYPE-CD-RATE NOT NUMERIC                      00115100
661460        MOVE CWSCC-UNEXPECTED-SYS-PASS-DATA TO                    00115200
661470                                   SIAPC-RETURN-STATUS            00115300
661480        MOVE WS-PDB-REC21-INV-RATE-MSG TO                         00115400
661490                                   SIAPC-ERROR-MESSAGE            00115500
118100        MOVE 'SF012'            TO SIAPC-RETURN-MESSAGE-CD        11810000
118200                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)11820000
661500        PERFORM 9990-ERROR-DISPLAY THRU 9990-EXIT                 00115700
661600        GO TO 0280-EXIT
661700     END-IF.
131200                                                                  11860000
131300*11.4 E_38714 DSL BEGIN IF NO FM150/FM1E0 RECORD                  11730000
131400     IF WS-INSTITUTIONAL                                          12090000
131500        IF FM150-ACCESS-FEE-PCT NOT NUMERIC                       12100000
131600           MOVE 0 TO FM150-ACCESS-FEE-PCT                         12100000
131700        END-IF                                                    12100000
131800     ELSE                                                         12090000
131900        IF FM1E0-ACCESS-FEE-PCT NOT NUMERIC                       12100000
132000           MOVE 0 TO FM1E0-ACCESS-FEE-PCT                         12100000
132100        END-IF                                                    12100000
132200     END-IF                                                       12090000
132300*11.4 E_38714 DSL END                                             11730000
661800                                                                  00115800
141300*13.5 E_76693 DSL BEGIN                                           11730000
141400     IF CIAPP2-ACCESS-FEE-CD =  ('1' OR '2' OR '4' OR '5') AND    11870000
118800        CIABP-HOST-PAR-SITE                                       11880000
119003           IF WS-INSTITUTIONAL                                    12090000
142000              EVALUATE TRUE
142100              WHEN WS-ACCT-TYPE-21 = 'D' OR 'E' OR 'G' OR 'H'
142200              WHEN WS-ACCT-TYPE-21 = 'X' AND
142201                   CIAPP2-ACCT-TYPE-CD = 'F'
142202                 IF (FM150-ACCESS-FEE-PCT >                       12110000
142203                                    CDBP21-ACCT-TYPE-CD-RATE) OR  1212000
142204                    (FM150-ACCESS-FEE-PCT < ZEROES)               12110000
142205                     MOVE CDBP21-ACCT-TYPE-CD-RATE TO             12130000
142206                          FM150-ACCESS-FEE-PCT                    12140000
142207                 END-IF                                           12110000
154610*R13.5.P PD_91147 DSL - REPLACE ZERO ACCESS FEE                   15720000
142208              WHEN WS-ACCT-TYPE-21 = 'X'
142210                 IF (FM150-ACCESS-FEE-PCT = ZEROES) OR
142216                    ((FM150-ACCESS-FEE-PCT >                        090000
142217                                      CDBP21-ACCT-TYPE-CD-RATE)   0900  00
142218                    AND CIAPP2-DELIVERY-METHOD = 'J'                090000
142219                    AND FM105-REL-NUM >= '110')                     110000
142220                    MOVE CDBP21-ACCT-TYPE-CD-RATE TO                12000
142221                         FM150-ACCESS-FEE-PCT                       110000
142222                 END-IF                                             130000
142223              WHEN OTHER
142224                IF (FM150-ACCESS-FEE-PCT = ZEROES OR
142225                   (FM150-ACCESS-FEE-PCT >
142226                    CDBP21-ACCT-TYPE-CD-RATE))
142227                   MOVE CDBP21-ACCT-TYPE-CD-RATE TO
142228                        FM150-ACCESS-FEE-PCT
142229                END-IF
142230              END-EVALUATE
119010           ELSE                                                   12160000
142232              EVALUATE TRUE
142233              WHEN WS-ACCT-TYPE-21 = 'D' OR 'E' OR 'G' OR 'H'
142234              WHEN WS-ACCT-TYPE-21 = 'X' AND
142235                   CIAPP2-ACCT-TYPE-CD = 'F'
142236                 IF (FM1E0-ACCESS-FEE-PCT >                       12110000
142237                                    CDBP21-ACCT-TYPE-CD-RATE) OR  1212000
142238                    (FM1E0-ACCESS-FEE-PCT < ZEROES)               12110000
142239                     MOVE CDBP21-ACCT-TYPE-CD-RATE TO             12130000
142240                          FM1E0-ACCESS-FEE-PCT                    12140000
142241                 END-IF                                           12110000
157410*R13.5.P PD_91147 DSL - REPLACE ZERO ACCESS FEE                   16010000
142242              WHEN WS-ACCT-TYPE-21 = 'X'
142245                 IF (FM1E0-ACCESS-FEE-PCT = ZEROES) OR
142246                    ((FM1E0-ACCESS-FEE-PCT >                        090000
142247                                      CDBP21-ACCT-TYPE-CD-RATE)   0900  00
142246                    AND CIAPP2-DELIVERY-METHOD = 'J'                090000
142249                    AND FM1A5-REL-NUM >= '110')                     110000
142248                    MOVE CDBP21-ACCT-TYPE-CD-RATE TO                12000
142249                         FM1E0-ACCESS-FEE-PCT                       110000
142250                 END-IF                                             130000
142251              WHEN OTHER
142252                IF (FM1E0-ACCESS-FEE-PCT = ZEROES OR
142253                   (FM1E0-ACCESS-FEE-PCT >
142254                    CDBP21-ACCT-TYPE-CD-RATE))
142255                   MOVE CDBP21-ACCT-TYPE-CD-RATE TO
142256                        FM1E0-ACCESS-FEE-PCT
142257                END-IF
142258              END-EVALUATE
142259           END-IF.
151100*13.5 E_76693 END                                                 11730000
664800                                                                  00118700
664900     MOVE CDBP21-ACCT-TYPE-CD-RATE TO                             00118800
665000          CIABP-CAPPED-ACCESS-FEE-PCT.                            00118900
665001                                                                  00119000
126310*11.3 E_49671 END                                                 11730000
665002 0280-EXIT.  EXIT.                                                00119100
665003**********************************************************        00119200
665004*** GET RELEASE NUMBER AND MAX ALLOWED NUMBER OF LINES ***        00119300
665005*** FOR REAL TIME PROCESS - FOR THIS CLAIM ONLY        ***        00119300
665006**********************************************************        00119400
665007 0290-READ-PDB-19-REC.                                            00119500
665008                                                                  00119600
665009     MOVE CWSCC-READ-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR.    00119700
665010     MOVE CWSCC-RANDOM-READ-ACCESS TO SIAPC-FUNCTION-CD-SUB.      00119800
665011                                                                  00119900
665012     MOVE SPACES                   TO CIADBP-DATA.                00120000
665013     MOVE '19'                     TO CIADBP-REC-TYPE.            00120100
665014     MOVE CIABP-ENVIRONMENT-IND    TO CIADBP-BATCH-ONLINE-IND.    00120110
665015     MOVE FM105-SCCF-SER-NUM       TO CIADBP-CALC-SCCF-NO.        00120300
665016                                                                  00120400
665017     MOVE SPACE                    TO CSAPI-COMMAND.              00120500
665018     MOVE 'ITCP9102'               TO CSAPI-CALLING-PGM-ID.       00120600
665019     SET CSAPI-ON-LINE-ENV         TO TRUE.                       00120740
665020     SET WS-ITPP01U1               TO TRUE.                       00120800
665030     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00120900
665040                                   WS-ITSIAPC                     00121000
665050                                   WS-ITCIADBP                    00121100
665060                                   WS-ITCWSCC                     00121200
665070                                   CIART-BUS-OWNER-ID-PARM.       00121300
179006                                                                  02052
665072     IF CSAPI-RETURN-CODE  = 0                                    00121500
771700        MOVE CIADBP-RECORD         TO WS-ITCDBP19-RECORD          00121600
771800        MOVE CDBP19-REL-NUM-INFO   TO CWSFC-REL-NUM-TABLE(1)      00121700
179007     ELSE                                                         02053
771802        MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS         00121900
771803        MOVE '0290- BAD READ PARM_DATA RECORD 19'                 00122000
771804                                   TO SIAPC-ERROR-MESSAGE         00122100
129600        MOVE 'SF013'            TO SIAPC-RETURN-MESSAGE-CD        12960000
129700                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)12970000
771806        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00122300
771807        GO TO 0290-EXIT                                           00122400
771808     END-IF.                                                      00122500
179018                                                                  02064
771810 0290-EXIT. EXIT.                                                 00122700
771811************************************************************      00119400
771812*** LOAD RELEASE NUMBER AND MAX ALLOWED # OF LINES TABLE ***      00119300
771813*** FOR BATCH PROCESS - UP TO 19 OCCURRENCES             ***      00119300
771814************************************************************      00119400
771815 0295-FETCH-PDB-19-REC.                                           00119500
181296                                                                  02741
771817     MOVE SPACES                   TO CIADBP-DATA.                00120000
771818     MOVE CIABP-ENVIRONMENT-IND    TO CIADBP-BATCH-ONLINE-IND.    00120110
771819     MOVE '19'                     TO CIADBP-REC-TYPE.            00120100
771820     MOVE SPACE                    TO CIADBP-SUB-KEY.
181300                                                                  02135
771831     MOVE 'ITCP9102'               TO CSAPI-CALLING-PGM-ID.       00120600
771832     MOVE 'DECLARE'                TO CSAPI-COMMAND.              00119600
771833     MOVE  2                       TO CSAPI-SUB-COMMAND.          00119600
771834     SET CSAPI-BATCH-ENV           TO TRUE.                       00120720
103500                                                                  ITCP9102
771836     SET WS-ITPP01U1               TO TRUE.                       00120800
771837     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00120900
771838                                   WS-ITSIAPC                     00121000
771839                                   WS-ITCIADBP                    00121100
771840                                   WS-ITCWSCC                     00121200
771850                                   CIART-BUS-OWNER-ID-PARM.       00121300
771851                                                                  00121400
771852     IF CSAPI-RETURN-CODE  = 0                                    00121500
771853        CONTINUE                                                  00121600
771854     ELSE                                                         00121800
771855        MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS         00121900
771856        MOVE '0295- BAD OPEN CURSOR FOR RECORD 19'                00122000
771857                                   TO SIAPC-ERROR-MESSAGE         00122100
133200        MOVE 'SF013'            TO SIAPC-RETURN-MESSAGE-CD        13320000
133300                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)13330000
771859        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00122300
771860        GO TO 0295-EXIT                                           00122400
771861     END-IF.                                                      00122500
771862                                                                  00122600
133710*R11.3 E_10097 DSL INCREASE # ENTRIES FROM 19 TO 20               05470000
771863     PERFORM VARYING CWSFC-NT-IDX FROM 1 BY 1
133900               UNTIL CWSFC-NT-IDX > 20                            13390000
771865        MOVE 'FETCH'               TO CSAPI-COMMAND               00119600
771866        MOVE  2                    TO CSAPI-SUB-COMMAND           00119600
771867        MOVE SPACES                TO CIADBP-RECORD
771868        CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                 00120900
771869                                      WS-ITSIAPC                  00121000
771870                                      WS-ITCIADBP                 00121100
771880                                      WS-ITCWSCC                  00121200
771881                                      CIART-BUS-OWNER-ID-PARM     00121300
771882        IF CSAPI-RETURN-CODE  = 0                                 00121500
771883           MOVE CIADBP-RECORD        TO WS-ITCDBP19-RECORD        00121600
771884           MOVE CDBP19-REL-NUM-INFO  TO CWSFC-REL-NUM-TABLE       00121700
771885                                       (CWSFC-NT-IDX)
771886        ELSE
771887        IF CSAPI-RETURN-CODE  = +100                              00121500
771888           IF CWSFC-NT-IDX = 1
771889              MOVE CWSCC-READ-ERROR   TO SIAPC-RETURN-STATUS      00121900
771890              MOVE '0295-BAD SELECT APPL_PARM RECORD 19'          00122000
771900                                      TO SIAPC-ERROR-MESSAGE      00122100
135800              MOVE 'SF013'      TO SIAPC-RETURN-MESSAGE-CD        13580000
135900                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)13590000
771902              PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT       00122300
771903              SET CWSFC-NT-IDX       TO 20
771904           ELSE
771905              MOVE SPACE  TO CWSFC-REL-NUM-TABLE(CWSFC-NT-IDX)
771906              SET CWSFC-NT-IDX       TO 20
771907           END-IF                                                 00122500
771908        ELSE
771909           MOVE CWSCC-READ-ERROR     TO SIAPC-RETURN-STATUS       00121900
771910           MOVE '0295-BAD SELECT APPL_PARM RECORD 19'             00122000
771920                                     TO SIAPC-ERROR-MESSAGE       00122100
137000           MOVE 'SF013'         TO SIAPC-RETURN-MESSAGE-CD        13700000
137100                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)13710000
771922           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT          00122300
771923           GO TO 0295-EXIT                                        00122400
771924        END-IF                                                    00122500
771925        END-IF                                                    00122500
771926     END-PERFORM.                                                 00122600
771927                                                                  00122600
771928     MOVE 'CLOSE'               TO CSAPI-COMMAND.                 00119600
771929     MOVE  2                    TO CSAPI-SUB-COMMAND.             00119600
771930                                                                  00119900
771940     CALL WS-CALL-MODULE  USING    WS-ITCCSAPI                    00120900
771950                                   WS-ITSIAPC                     00121000
771960                                   WS-ITCIADBP                    00121100
771970                                   WS-ITCWSCC                     00121200
771971                                   CIART-BUS-OWNER-ID-PARM.       00121300
771972                                                                  00122600
771973 0295-EXIT. EXIT.                                                 00122700
771974/*******************************************************          00122800
771975                                                                  00122900
103700 1000-EDIT-PROCESSING.                                            ITCP9102
103800                                                                  ITCP9102
771978     PERFORM 1010-EDIT-INPUT     THRU 1010-EXIT.                  00123200
150830                                                                  01666
771980     IF WS-NON-TRANSM-CNT > 0                                     00123700
771981        MOVE 'Y'                   TO CIART-SPEC-NOTE-IND         00123700
771982     END-IF.                                                      00123700
105200                                                                  ITCP9102
771984     IF WS-EARLIEST-SERV-DT EQUAL '99999999'                      00123400
771985        MOVE SPACES                  TO WS-EARLIEST-SERV-DT       00123500
771986     END-IF.                                                      00123600
106200                                                                  ITCP9102
771988     MOVE WS-EARLIEST-SERV-DT        TO CIABP-INCUR-DT.           00123800
106400                                                                  ITCP9102
771990     IF WS-BYPASS-EDITS-IND = 'Y'                                 00124000
771991        MOVE 'I'                       TO CIABP-STAT-CD
771992        IF WS-INSTITUTIONAL                                       00124401
771993           MOVE 'D'                    TO FM105-STAT-CD           00124402
771994        ELSE                                                      00124403
771995           MOVE 'D'                    TO FM1A5-STAT-CD           00124404
771996        END-IF                                                    00124405
771997        IF WS-VALID-SCCF      = 'N'                               00124410
771998           IF WS-INSTITUTIONAL                                    00124411
771999              MOVE 'IS001'             TO FM105-ERR-CD (1)        00124420
141400                                          SIAPC-RETURN-MESSAGE-CD 14140000
772000           ELSE                                                   00124431
772001              MOVE 'PS001'             TO FM1A5-ERR-CD (1)        00124432
141700                                          SIAPC-RETURN-MESSAGE-CD 14170000
772002           END-IF                                                 00124440
772003        ELSE                                                      00124442
772004        IF CIABP-TRANS-ID-ERR                                     00124443
772005           IF WS-INSTITUTIONAL                                    00124444
772006              MOVE 'IT040'             TO FM105-ERR-CD (1)        00124445
142300                                          SIAPC-RETURN-MESSAGE-CD 14230000
772007           ELSE                                                   00124446
772008              MOVE 'PT040'             TO FM1A5-ERR-CD (1)        00124447
142600                                          SIAPC-RETURN-MESSAGE-CD 14260000
772009           END-IF                                                 00124449
772010        ELSE                                                      00124451
772011        IF CIABP-REC-TYPE-ERR                                     00124452
772012           IF WS-INSTITUTIONAL                                    00124453
772013              MOVE 'IR007'             TO FM105-ERR-CD (1)        00124454
143200                                          SIAPC-RETURN-MESSAGE-CD 14320000
772014           ELSE                                                   00124455
772015              MOVE 'PR007'             TO FM1A5-ERR-CD (1)        00124456
143500                                          SIAPC-RETURN-MESSAGE-CD 14350000
772016           END-IF                                                 00124459
772017        ELSE                                                      00124461
772018        IF CIABP-SEQ-NUM-ERR                                      00124462
772019           IF WS-INSTITUTIONAL                                    00124463
772020              MOVE 'IXR02'             TO FM105-ERR-CD (1)        00124464
144100                                          SIAPC-RETURN-MESSAGE-CD 14410000
772021           ELSE                                                   00124465
772022              MOVE 'PXR02'             TO FM1A5-ERR-CD (1)        00124466
144400                                          SIAPC-RETURN-MESSAGE-CD 14440000
772023           END-IF                                                 00124467
772024        ELSE                                                      00124468
155601*R11.4.1 TD_71425 DSL BEGIN                                       05470000
155610        IF CIABP-SEQ-NUM-IND = '4'                                14380000
155620           IF WS-INSTITUTIONAL                                    14390000
155630              MOVE 'IS05A'             TO FM105-ERR-CD (1)        14400000
155640                                          SIAPC-RETURN-MESSAGE-CD 14410000
155650           ELSE                                                   14420000
155660              MOVE 'PS05A'             TO FM1A5-ERR-CD (1)        14430000
155670                                          SIAPC-RETURN-MESSAGE-CD 14440000
155680           END-IF                                                 14450000
155681           SET CIABP-EOJ               TO TRUE                    55080000
171100*R13.0 PD_81924 EXL        START                                  05470000
175500           IF CIART-RT-IND = 'C'                                  14860100
175600              MOVE 'I'                 TO CIASC-CLAIM-STATUS-CD(1)14870000
175700              GO TO 1000-EXIT
175701           END-IF
155690        ELSE                                                      14460000
175703*R13.0 PD_81924 EXL        END                                    05470000
772025        IF CIABP-DUP-REC-ERR                                      00124469
772026           IF WS-INSTITUTIONAL                                    00124470
772027              MOVE 'IXR21'             TO FM105-ERR-CD (1)        00124480
145000                                          SIAPC-RETURN-MESSAGE-CD 14500000
772028           ELSE                                                   00124481
772029              MOVE 'PXR21'             TO FM1A5-ERR-CD (1)        00124482
145300                                          SIAPC-RETURN-MESSAGE-CD 14530000
772030           END-IF                                                 00124483
772031        ELSE                                                      00124484
772032        IF CIABP-MISS-90-OR-X0-ERR                                00124485
772033           IF WS-INSTITUTIONAL                                    00124486
772034              MOVE 'IXR22'             TO FM105-ERR-CD (1)        00124487
145900                                          SIAPC-RETURN-MESSAGE-CD 14590000
772035           ELSE                                                   00124488
772036              MOVE 'PXR22'             TO FM1A5-ERR-CD (1)        00124489
146200                                          SIAPC-RETURN-MESSAGE-CD 14620000
772037           END-IF                                                 00124490
772038        ELSE                                                      00124491
772039        IF CIABP-FOUND-F0-F1                                      00124500
772040           MOVE 'PXR03'                TO FM1A5-ERR-CD (1)        00124502
146700                                          SIAPC-RETURN-MESSAGE-CD 14670000
772041        ELSE                                                      00124503
772042           IF WS-INSTITUTIONAL                                    00124510
772043              IF FM105-ERR-CD(1) > SPACE
772044                 MOVE FM105-ERR-CD(1)  TO SIAPC-RETURN-MESSAGE-CD
772045              ELSE
772046                 MOVE 'SF020'          TO SIAPC-RETURN-MESSAGE-CD 00124600
147400                                          FM105-ERR-CD(1)         14740000
772047              END-IF                                              00124490
772048           ELSE                                                   00124800
772049              IF FM1A5-ERR-CD(1) > SPACE
772050                 MOVE FM1A5-ERR-CD(1)  TO SIAPC-RETURN-MESSAGE-CD
772051              ELSE
772052                 MOVE 'SF021'          TO SIAPC-RETURN-MESSAGE-CD 00124900
148100                                          FM1A5-ERR-CD(1)         14810000
772053              END-IF                                              00124490
772054           END-IF.                                                00125100
106700                                                                  ITCP9102
772062     IF WS-BYPASS-EDITS-IND = 'Y'                                 00124000
159600**R11.4 E_38714 18.2  ADDED CHECK FOR 'C'                         07320000
159700        IF CIART-RT-IND = 'Y' OR 'C'                              14860100
148610           PERFORM 9990-ERROR-DISPLAY                             14861000
              END-IF                                                    14861100
772063        GO TO 1000-EXIT.                                          00126100
107200                                                                  ITCP9102
772065     IF CIABP-EOJ                                                 00125600
772066        MOVE 'I'                       TO CIABP-STAT-CD           00125700
149100        MOVE '      ITCP9102 - INVALID RELEASE'                   14910000
149200                                       TO SIAPC-ERROR-MESSAGE     14920000
772067        MOVE CWSCC-ABORT               TO SIAPC-RETURN-IND        00125800
772070        GO TO 1000-EXIT.                                          00126100
149500                                                                  14950000
149600**R11.1,05/13/2010,17.7,E_46690, EXL -START                       14960000
149700     IF CIART-RT-IND       = 'N'                                  14970000
149800        IF  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) = 'PXR51'             14980000
149900        OR  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) = 'IXR51'             14990000
150000            MOVE 'D'   TO CIASC-CLAIM-STATUS-CD(1)                15000000
150100            MOVE 'I'                    TO CIABP-STAT-CD          15010000
150200            GO TO 1000-EXIT                                       15020000
150300        END-IF                                                    15030000
150400     END-IF.                                                      15040000
150500**R11.1,05/13/2010,17.7,E_46690, EXL -END                         15050000
107700                                                                  ITCP9102
107800     IF WS-INSTITUTIONAL                                          ITCP9102
772079        IF FM105-EST-IND = 'E'                                    00126400
772080           MOVE '7'                   TO FM105-TRANSM-MODE-CD     00126500
772090           MOVE SIAPC-GREG-CURRENT-DT TO FM120-ADM-DT             00126600
772091                                         FM120-STMT-COV-FROM-DT   00126700
772092                                         FM120-STMT-COV-TO-DT     00126800
185870*R12.0 E_19137 DSL BEGIN                                          15130000
185871           IF FM173-PRINC-PROC-DT > SPACE                         15130000
772094              MOVE SIAPC-GREG-CURRENT-DT TO                       00127000
185873                                     FM173-PRINC-PROC-DT          15150000
772096           END-IF                                                 00127200
185875           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 6    15170000
185876              IF FM173-PROC-DT(WS-SUB) > SPACE                    15180000
772099                 MOVE SIAPC-GREG-CURRENT-DT TO                    00127500
185878                                     FM173-PROC-DT(WS-SUB)        15200000
772200              END-IF                                              00127700
772201           END-PERFORM                                            00127800
179410*R13.5 E_73004 DSL BEGIN                                          15130000
179500           IF FM120-PAT-STAT-CD = (SPACES OR LOW-VALUES) AND      15130000
179640              FM105-TYPE-SUBM = ('1' OR '2')             AND      15140000
179650              FM105-REL-NUM >= '135'                              15140000
179660              MOVE '01' TO FM120-PAT-STAT-CD                      15140000
179801           END-IF                                                 15160000
179900*R13.5 E_73004 DSL END                                            15130000
772202        END-IF                                                    00127900
772203        IF FM105-TRANSM-MODE-CD <= SPACE                          00128000
772204           MOVE '1'                   TO FM105-TRANSM-MODE-CD     00128100
772205        END-IF                                                    00128200
152700** VER 17.7 REL 11.1 E_48276 START                                15270000
152800        IF FM105-INIT-TRANSM-MODE-CD NOT = '1' AND '4' AND '7'    15280000
152900                                               AND '2' AND '9'    15290000
153000          MOVE FM105-TRANSM-MODE-CD                               15300000
153100                                  TO FM105-INIT-TRANSM-MODE-CD    15310000
153200        END-IF                                                    15320000
153300** VER 17.7 REL 11.1 E_48276 END                                  15330000
165300**R11.4 E_38714 18.2  ADDED CHECK FOR 'C'                         07320000
165400        IF CIART-RT-IND = 'Y' OR 'C'                              15340000
772207           MOVE SIAPC-GREG-CURRENT-DT TO FM105-EDIT-DT            00128400
772208        END-IF
772209        MOVE FM105-TRANSM-MODE-CD     TO CIABP-TRANSM-MODE-CD     00128300
772210        MOVE FM105-LOC-PLAN-CD        TO                          00128600
108100             CIABP-CLAIM-HEADER-LOC-PLAN-CD                       ITCP9102
108200        MOVE FM105-CLM-TYPE            TO CIABP-CLM-TYPE          ITCP9102
108300        MOVE FM105-LINE-BUS            TO CIABP-LINE-BUS          ITCP9102
108400        MOVE FM105-TYPE-SUBM           TO WS-TYPE-SUBM            ITCP9102
108500        MOVE SPACES                    TO FM105-ERR-CDS           ITCP9102
108600                                          FM105-FMT-DB-POST-DT    ITCP9102
              IF FM105-STAT-CD  NOT = 'R'
108800           MOVE SPACES                 TO FM105-STAT-CD           ITCP9102
108900        END-IF                                                    ITCP9102
120700        PERFORM                                                   ITCP9102
120800        VARYING WS-SUB FROM 1 BY 1                                ITCP9102
120900        UNTIL WS-SUB > 5                                          ITCP9102
121000           IF FM150-SF-MSG-CD-CLM (WS-SUB) = WS-1008              ITCP9102
121100              SET CIABP-MSG-1008-FOUND TO TRUE                    ITCP9102
121200           END-IF                                                 ITCP9102
121300        END-PERFORM                                               ITCP9102
192100*R17.0 E_93016 SKG : BEGIN                                        19520000
                                                                        19530000
              IF FM105-REL-NUM      < '170'                             19540000
                 PERFORM 9930-FIND-ITS-30-REC      THRU 9930-EXIT       19550000
              ELSE                                                      19560000
                 PERFORM 9932-II018-EDIT           THRU 9932-EXIT       19570000
              END-IF                                                    19580000
                                                                        19590000
192000*       PERFORM 9930-FIND-ITS-30-REC            THRU 9930-EXIT    19600000
192100*R17.0 E_93016 SKG : END                                          19610000
187100        PERFORM 9940-INST-SF-MSG-U270        THRU 9940-EXIT       15550000
121500                                                                  ITCP9102
126700        EVALUATE TRUE                                             ITCP9102
772227        WHEN FM105-CLM-TYPE NOT = ('11' AND '12')                 00130600
109100           MOVE 'IXR01'                TO FM105-ERR-CD (1)        ITCP9102
772229           MOVE 'D'                    TO FM105-STAT-CD           00130800
127500                                                                  ITCP9102
127600        WHEN FM105-TRANS-IND NOT = ('P' AND 'T')                  ITCP9102
127700        WHEN (FM105-TRANS-IND = ('P' OR 'T')                      ITCP9102
127800             AND CDBP16-TRANSACTION-IND NOT = FM105-TRANS-IND)    ITCP9102
127900           MOVE 'IT041'                TO FM105-ERR-CD (1)        ITCP9102
772235           MOVE 'D'                    TO FM105-STAT-CD           00131800
128100                                                                  ITCP9102
128200        WHEN CIABP-NO-ITS-SUB-FOUND                               ITCP9102
128300           MOVE 'II018'                TO FM105-ERR-CD (1)        ITCP9102
772239           MOVE 'D'                    TO FM105-STAT-CD           00132200
128500                                                                  ITCP9102
128600        WHEN FM130-SUB-ID-NUM = SPACES OR LOW-VALUES              ITCP9102
128700           MOVE 'IS042'                TO FM105-ERR-CD (1)        ITCP9102
772243           MOVE 'D'                    TO FM105-STAT-CD           00132600
169410           MOVE WS-SUB6                TO CIACAE-ERR-LINE (1 1)   15740000
128900                                                                  ITCP9102
303900        WHEN  CIABP-46-CTR > WS-PARM-INST-SEQ-NUM-MAX             00311700
772246           MOVE 'IRL2T'                TO FM105-ERR-CD (1)        00133400
772247           MOVE 'I'                    TO FM105-STAT-CD           00130800
306000        WHEN  CIABP-60-CTR > WS-PARM-INST-SEQ-NUM-MAX             00312000
772249           MOVE 'IRL2T'                TO FM105-ERR-CD (1)        00133800
772250           MOVE 'I'                    TO FM105-STAT-CD           00130800
131600        END-EVALUATE                                              ITCP9102
772252     END-IF.                                                      00136400
111900                                                                  ITCP9102
112000     IF WS-PROFESSIONAL                                           ITCP9102
158600        IF FM1A5-EST-IND = 'E'                                    15860000
772256           MOVE '7'                   TO FM1A5-TRANSM-MODE-CD     00136800
772257        END-IF                                                    00136900
772258        IF FM1A5-TRANSM-MODE-CD   = SPACE                         00137000
772259           MOVE '1'                    TO FM1A5-TRANSM-MODE-CD    00137100
159100        END-IF                                                    15910000
159300        IF FM1A5-INIT-TRANSM-MODE-CD NOT = '1' AND '4' AND '7'    15930000
159400                                               AND '2' AND '9'    15940000
159500          MOVE FM1A5-TRANSM-MODE-CD                               15950000
159600                                  TO FM1A5-INIT-TRANSM-MODE-CD    15960000
772260        END-IF                                                    00137200
172000        IF CIART-RT-IND = 'Y' OR 'C'                              15990000
772262           MOVE SIAPC-GREG-CURRENT-DT  TO FM1A5-EDIT-DT           00137400
772263        END-IF
772264        MOVE FM1A5-TRANSM-MODE-CD      TO CIABP-TRANSM-MODE-CD    00137300
112200        MOVE FM1A5-LOC-PLAN-CD         TO                         ITCP9102
112300             CIABP-CLAIM-HEADER-LOC-PLAN-CD                       ITCP9102
112400        MOVE FM1A5-CLM-TYPE            TO CIABP-CLM-TYPE          ITCP9102
112500        MOVE FM1A5-LINE-BUS            TO CIABP-LINE-BUS          ITCP9102
112600        MOVE FM1A5-TYPE-SUBM           TO WS-TYPE-SUBM            ITCP9102
112700        MOVE SPACES                    TO FM1A5-ERR-CDS           ITCP9102
112800                                          FM1A5-FMT-DB-POST-DT    ITCP9102
              IF FM1A5-STAT-CD  NOT = 'R'
113000           MOVE SPACES                 TO FM1A5-STAT-CD           ITCP9102
113100        END-IF                                                    ITCP9102
126600        PERFORM                                                   ITCP9102
126700        VARYING WS-SUB FROM 1 BY 1                                ITCP9102
126800        UNTIL WS-SUB > 5                                          ITCP9102
126900           IF FM1E1-SF-MSG-CD-CLM (WS-SUB) = WS-1008              ITCP9102
127000              SET CIABP-MSG-1008-FOUND TO TRUE                    ITCP9102
127100           END-IF                                                 ITCP9102
127200        END-PERFORM                                               ITCP9102
193800*R14.0 E_84725 DSL BEGIN
193900        PERFORM 9950-PROF-SF-MSG-U270       THRU 9950-EXIT
127400                                                                  ITCP9102
134300        EVALUATE TRUE                                             ITCP9102
165210        WHEN FM1A5-CLM-TYPE NOT = ('20' AND '40')                 01839
113300           MOVE 'PXR01'                TO FM1A5-ERR-CD (1)        ITCP9102
772283           MOVE 'D'                    TO FM1A5-STAT-CD           00139600
135100                                                                  ITCP9102
135200        WHEN FM1A5-TRANS-IND NOT = ('P' AND 'T')                  ITCP9102
135300        WHEN (FM1A5-TRANS-IND = ('P' OR 'T')                      ITCP9102
135400             AND CDBP16-TRANSACTION-IND NOT = FM1A5-TRANS-IND)    ITCP9102
135500           MOVE 'PT041'                TO FM1A5-ERR-CD (1)        ITCP9102
772289           MOVE 'D'                    TO FM1A5-STAT-CD           00140600
135700                                                                  ITCP9102
135800        WHEN FM1D0-SUB-ID-NUM = SPACES OR LOW-VALUES              ITCP9102
135900           MOVE 'PS042'                TO FM1A5-ERR-CD (1)        ITCP9102
772293           MOVE 'D'                    TO FM1A5-STAT-CD           00141000
                                                                        00321100
              WHEN  CIABP-E6-CTR > WS-PARM-PROF-SEQ-NUM-MAX             00321300
772296           MOVE 'PRL2E'                TO FM1A5-ERR-CD (1)        00141800
772297           MOVE 'I'                    TO FM105-STAT-CD           00130800
              WHEN  CIABP-F0-CTR > WS-PARM-PROF-SEQ-NUM-MAX             00321600
772299           MOVE 'PRL2E'                TO FM1A5-ERR-CD (1)        00142200
772300           MOVE 'I'                    TO FM105-STAT-CD           00130800
              WHEN  CIABP-F1-CTR > WS-PARM-PROF-SEQ-NUM-MAX             00321900
772302           MOVE 'PRL2E'                TO FM1A5-ERR-CD (1)        00142600
772303           MOVE 'I'                    TO FM105-STAT-CD           00130800
138800        END-EVALUATE                                              ITCP9102
772305     END-IF.                                                      00145600
115500                                                                  ITCP9102
115600     EVALUATE CIASC-CLAIM-STATUS-CD (1)                           ITCP9102
115700        WHEN 'R'                                                  ITCP9102
115800           MOVE WS-R                   TO CIABP-STAT-CD           ITCP9102
772310           MOVE 'Y'                    TO WS-BYPASS-EDITS-IND     00146100
116200        WHEN 'D'                                                  ITCP9102
772312           MOVE 'I'                    TO CIABP-STAT-CD           00146300
772313           MOVE 'Y'                    TO WS-BYPASS-EDITS-IND     00146100
116600        WHEN OTHER                                                ITCP9102
116700           PERFORM 1100-PLAN-PROFILE            THRU 1100-EXIT    ITCP9102
772316     END-EVALUATE.                                                00146700
117000                                                                  ITCP9102
772318     IF CIABP-EOJ                                                 00146900
772319        OR WS-BYPASS-EDITS-IND = 'Y'
772320        GO TO 1000-EXIT.                                          00147000
117200                                                                  ITCP9102
772322     PERFORM 1180-ECR-EDITS                     THRU 1180-EXIT.   00147200
772323                                                                  00147300
772324     PERFORM 1111-REF-DATA-ACCESS               THRU 1111-EXIT.   00147400
772325                                                                  00147500
772326     IF CIABP-EOJ                                                 00147700
772327        GO TO 1000-EXIT.                                          00147800
772328                                                                  00147900
772329     IF  CIABP-STAT-CD     > SPACE                                00148000
772330     AND CIABP-STAT-CD NOT = 'V'                                  00148100
772340         MOVE 'Y'                    TO WS-BYPASS-EDITS-IND       00146100
772350         GO TO 1000-EXIT.                                         00148300
772351                                                                  00148400
772352     IF WS-INSTITUTIONAL                                          00148500
772353        PERFORM 1181-INST-AEA-AMT-PROCESS THRU 1181-EXIT          00148600
772354     ELSE                                                         00148700
772355        PERFORM 1182-PROF-AEA-AMT-PROCESS THRU 1182-EXIT          00148800
772356     END-IF.                                                      00148900
772357                                                                  00149000
772358     PERFORM 1200-SF-DUPLICATE-CHK  THRU 1200-EXIT.               00149100
772359                                                                  00149200
772360     EVALUATE CIASC-CLAIM-STATUS-CD (1)                           00145800
772361        WHEN 'R'                                                  00145900
772362           MOVE WS-R                      TO CIABP-STAT-CD        00146000
772363        WHEN 'D'                                                  00146200
772364           MOVE 'I'                       TO CIABP-STAT-CD        00146300
772365     END-EVALUATE.                                                00149400
772366                                                                  00149400
772367     IF CIABP-EOJ                                                 00149500
772368        GO TO 1000-EXIT.                                          00149600
772369                                                                  00149700
772370     IF  CIABP-STAT-CD     > SPACE                                00149800
772380     AND CIABP-STAT-CD NOT = 'V'                                  00149900
772390         GO TO 1000-EXIT.                                         00150100
772400                                                                  00150200
772401     PERFORM 1220-FDB-RESUB-ADJ-CHK             THRU 1220-EXIT.   00150300
772402                                                                  00150400
772403     IF  WS-BYPASS-EDITS-IND = 'N'                                00150500
772404     OR  NOT CIABP-EOJ                                            00150500
772405         PERFORM 1290-EDIT-MODULE-CHK           THRU 1290-EXIT    00150600
772406     END-IF.                                                      00150800
772407                                                                  00150900
182600     IF WS-PROFESSIONAL                                           15850000
182700        IF  (FM1A5-SYS-CRT-CD NOT = ('P' AND 'R' AND 'B' AND 'O'    850000
182800                                         AND 'C') AND             15850000
182900             FM1A5-REL-NUM >= '114')                              15850000
183000            MOVE 'P' TO FM1A5-SYS-CRT-CD                          15850000
183100        END-IF                                                    15850000
183200        IF  (FM1A5-SYS-UPDT-CD NOT = ('P' AND 'R' AND 'B' AND 'O' 15850000
183300                                          AND 'C') AND            15850000
183400             FM1A5-REL-NUM >= '114')                              15850000
183500            MOVE 'P' TO FM1A5-SYS-UPDT-CD                         15850000
183600        END-IF                                                    15850000
183700     ELSE                                                         15850000
183800        IF  (FM105-SYS-CRT-CD NOT = ('P' AND 'R' AND 'B' AND 'O'    850000
183900                                         AND 'C') AND             15850000
184000             FM105-REL-NUM >= '114')                              15850000
184100            MOVE 'P' TO FM105-SYS-CRT-CD                          15850000
184200        END-IF                                                    15850000
184300        IF  (FM105-SYS-UPDT-CD NOT = ('P' AND 'R' AND 'B' AND 'O' 15850000
184400                                          AND 'C') AND            15850000
184500             FM105-REL-NUM >= '114')                              15850000
184600            MOVE 'P' TO FM105-SYS-UPDT-CD                         15850000
184700        END-IF                                                    15850000
184800     END-IF.                                                      15850000
210000                                                                  21550000
210000** R17.5 E_112726 EXL  NEW PROCESS TO GET COST GROUPING           21560000
210000     IF  CIABP-HOST-HOME-SITE-IND = '1'                           21570000
210000     AND CIABP-STAT-CD            = 'V'                           21571000
              IF WS-PROFESSIONAL                                        21580000
248400           PERFORM 1960-SET-WS-CST-GRP-TABLE THRU 1960-EXIT       21590000
              END-IF                                                    21600000
210000        PERFORM 1900-COST-GROUP-PROCESS      THRU 1900-EXIT       21610000
210000     END-IF.                                                      21620000
185000                                                                  15850000
772408 1000-EXIT.  EXIT.                                                00151000
772409                                                                  00151100
772410******************************************************************00151200
772411* THE EDIT IN THIS PARAGRAPH WAS MOVED FROM ITCP9104 FOR REAL    *00151300
772412* TIME PROCESSING.                                               *00151400
772413******************************************************************00151500
772414 1010-EDIT-INPUT.                                                 00151600
772415                                                                  00151700
186105     MOVE 'N'                 TO WS-END-OF-SF.                    17250000
186106     MOVE SPACE               TO WS-HOLD-SEQ.                     17260000
186107                                                                  41360000
186108     MOVE 1                   TO WS-SUB2                          17270000
186109                                 WS-SUB4                          17280000
186110                                 WS-SUB5                          17290000
186111                                 WS-SUB6.                         17290000
186112     MOVE 0                   TO CIART-SERV-LN-CNT                17300000
186113                                 CIART-NOTE-LN-CNT                17310000
186114                                 CIASC-INPUT                      17320000
186115                                 CIASC-APPENDED                   17330000
186116                                 WS-NON-TRANSM-CNT.               17340000
186117                                                                  41360000
186118     IF CIASC-SCCF-SERIAL-NUM (1) NUMERIC AND                     17120000
186119        CIASC-SCCF-SERIAL-NUM (1) > 0     AND                     17120000
186120        CIASC-SCCF-SERIAL-NUM (1) (16:2) = '00'                   17120000
186130        CONTINUE                                                  17130000
186200     ELSE                                                         17150000
186201        MOVE 'N'          TO WS-VALID-SCCF                        17130000
186202        MOVE 'Y'          TO WS-BYPASS-EDITS-IND                  17140000
186203        GO TO 1010-EXIT                                           17140000
186204     END-IF.                                                      17140000
186206                                                                  17140000
772416     IF CIASC-REC-TYPE (1) = 'A5'                                 00151800
772417        SET WS-PROFESSIONAL         TO TRUE                       00151900
772418        SET CIABP-HDR-PROFESSIONAL  TO TRUE                       00152000
772419     ELSE                                                         00152100
772420        IF CIASC-REC-TYPE (1) = '05'                              00152200
772421           SET WS-INSTITUTIONAL        TO TRUE                    00152300
772422           SET CIABP-HDR-INSTITUTIONAL TO TRUE                    00152400
772423        ELSE                                                      00152500
772424           MOVE 'I'                    TO CIABP-STAT-CD           00152600
772425           SET CIABP-REC-TYPE-ERR      TO TRUE                    00152700
772426           MOVE 'Y'                    TO WS-BYPASS-EDITS-IND     00152800
772427           GO TO 1010-EXIT.                                       00152900
772439                                                                  00153810
772440     MOVE CIASC-STACKED-CLAIM-ITEM(1)(22:4)                       00153900
772450                               TO WS-HOLD-SEQ.                    00154000
772451                                                                  00154100
772452     IF WS-PROFESSIONAL                                           00154200
686800**R16.0 E_101251 PMP NEXT LINE                                    70550000
686300        MOVE 'X'              TO WS-PROC-CD-FLAG(1)               70500000
772453        MOVE 'N'              TO WS-F0-PRESENT-SW                 00154300
772454                                 WS-F1-PRESENT-SW                 00154400
772455        PERFORM 1020-EDIT-PROFESSIONAL  THRU 1020-EXIT            00154500
772456           VARYING WS-SUB FROM 2 BY 1                             00154600
772457           UNTIL WS-END-OF-SF  = 'Y'                              00154700
772458           OR    WS-BYPASS-EDITS-IND = 'Y'                        00154800
189500           OR    WS-SUB        > SFCNT-MAX-PROF-RECS              17460000
772460        MOVE CIART-NOTE-LN-CNT   TO CIABP-E6-CTR                  00155000
772470        IF  WS-F0-PRESENT-SW          = 'Y'                       00155100
772480            IF WS-F1-PRESENT-SW       = 'Y'                       00155200
772481               SET CIABP-FOUND-F0-F1     TO TRUE                  00155300
772482               MOVE 'Y'                  TO WS-BYPASS-EDITS-IND   00152800
772483            ELSE                                                  00155500
772484               MOVE CIART-SERV-LN-CNT  TO CIABP-F0-CTR            00155600
772485        ELSE                                                      00155700
772486           MOVE CIART-SERV-LN-CNT      TO CIABP-F1-CTR            00155800
772487        END-IF                                                    00155900
772488     ELSE                                                         00156000
772489        PERFORM 1030-EDIT-INSTITUTIONAL THRU 1030-EXIT            00156100
772490           VARYING WS-SUB FROM 2 BY 1                             00156200
772500           UNTIL WS-END-OF-SF  = 'Y'                              00156300
772501           OR    WS-BYPASS-EDITS-IND = 'Y'                        00156400
191100           OR    WS-SUB        > SFCNT-MAX-INST-RECS              17620000
772503        MOVE CIART-NOTE-LN-CNT   TO CIABP-46-CTR                  00156600
772504        MOVE CIART-SERV-LN-CNT   TO CIABP-60-CTR.                 00156700
217600                                                                  22370000
176600     MOVE CIASC-SCCF-SERIAL-NUM (1) TO CRT001-SCCF-ID             17660000
176700     MOVE CIART-SERV-LN-CNT         TO CRT001-SERV-LN-CNT         17670000
177700                                                                  17770000
772506     IF WS-BYPASS-EDITS-IND = 'Y'                                 00156900
772507        GO TO 1010-EXIT.                                          00157000
772508                                                                  00156800
193000     IF CIART-RT-IND       = 'N' AND                              17810000
193010        SIAPC-MODULE-NAME (1) NOT = 'ITCA9080'                    17810000
772510        SUBTRACT WS-NON-TRANSM-CNT FROM CIASC-INPUT               00156800
772520     END-IF.                                                      00156800
772700                                                                  00157120
772800     MOVE 'D'                     TO LK-REQUEST-CODE.             00157200
772900     MOVE 'S'                     TO LK-TYPE.                     00157300
773000     MOVE WS-SUB                  TO LK-TOTL-RCD.                 00157400
773100     SUBTRACT 1                 FROM LK-TOTL-RCD.                 00157500
773200                                                                  00157600
773300     SET WS-ITCQRQU0              TO TRUE.                        00157700
773400     CALL WS-CALL-MODULE  USING   LK-RQST-RTRN-CD                 00157800
773500                                  LK-MESSAGE-AREA                 00157900
773600                                  WS-STACKED-CLAIM                00158000
773700                                  ITCIART.                        00158100
773800                                                                  00158200
773900     IF LK-RETURN-CODE = '0000'                                   00158300
774000        MOVE '2'                    TO CIART-DATA-FORMAT-IND      00158400
774100     ELSE                                                         00158500
774200        MOVE LK-MESSAGE-AREA        TO SIAPC-ERROR-MESSAGE        00158600
774300        IF WS-INSTITUTIONAL                                       00158700
774400           MOVE 'IS001'             TO SIAPC-RETURN-MESSAGE-CD    00158800
774500                                       FM105-ERR-CD(1)            00158800
774600           MOVE 'D'                 TO FM105-STAT-CD              00158900
774700        ELSE                                                      00159000
774800           MOVE 'PS001'             TO SIAPC-RETURN-MESSAGE-CD    00159100
774900                                       FM1A5-ERR-CD(1)            00158800
775000           MOVE 'D'                 TO FM1A5-STAT-CD              00159200
775100        END-IF                                                    00159300
775200        MOVE 'I'                    TO CIABP-STAT-CD
775300        PERFORM 9990-ERROR-DISPLAY  THRU 9990-EXIT                00159400
775400        GO TO 1010-EXIT.                                          00159500
                                                                        22760000
197100     IF WS-PROFESSIONAL                                           17680000
197200        MOVE FM1A5-CLM-TYPE         TO CRT001-CLM-TP-CD           17690000
197300        MOVE FM1A5-EST-IND          TO CRT001-CLM-EST-IND         17700000
      *R17.5 E_104138 TGW RENAMED FIELD                                 23040000
              MOVE FM1D0-SUB-ID-PFX-1-3   TO CRT001-SUB-PFX-ID          23050000
223900*       MOVE FM1D0-SUB-ID-PFX-1-3   TO CRT001-ALPHA-PREFIX        23060000
197500     ELSE                                                         17720000
197600        MOVE FM105-CLM-TYPE         TO CRT001-CLM-TP-CD           17730000
197700        MOVE FM105-EST-IND          TO CRT001-CLM-EST-IND         17740000
      *R17.5 E_104138 TGW RENAMED FIELD                                 23100000
              MOVE FM130-SUB-ID-PFX-1-3   TO CRT001-SUB-PFX-ID          23110000
224300*       MOVE FM130-SUB-ID-PFX-1-3   TO CRT001-ALPHA-PREFIX        23120000
197900     END-IF.                                                      17760000
776601
198400     IF CIABP-HOST-HOME-SITE-IND = '1'
198500        MOVE 'I'        TO LK-REQUEST-CODE
198600        MOVE SPACES     TO LK-TYPE
198700        SET WS-ITCP9109 TO TRUE
198800        CALL WS-CALL-MODULE USING LK-RQST-RTRN-CD
198900                                  LK-MESSAGE-AREA
199000                                  WS-STACKED-CLAIM
199100                                  ITCIART
199200        END-CALL
199300     END-IF.
199500                                                                  18210000
199700     IF CIART-RT-IND = 'Y' OR 'C'                                 18220000
776603        IF WS-PROFESSIONAL
776604           MOVE CWSFC-REL-NUM (1) TO FM1A5-REL-NUM                00130200
776605           MOVE CWSFC-PROF-SEQ-NUM-MAX(1)                         00130300
776606                                     TO WS-PARM-PROF-SEQ-NUM-MAX  00130400
776607        ELSE
776608           MOVE CWSFC-REL-NUM (1) TO FM105-REL-NUM                00130200
776609           MOVE CWSFC-INST-SEQ-NUM-MAX(1)                         00130300
776610                                  TO WS-PARM-INST-SEQ-NUM-MAX     00130400
776620        END-IF
776630     ELSE
776640        IF WS-PROFESSIONAL
776650           MOVE FM1A5-SER-NUM-JULIAN-DATE TO CWSFC-SEARCH-DATE
776660           PERFORM 1040-GET-RELEASE-NUM   THRU 1040-EXIT
776670           IF CWSFC-SEARCH-MATCH
776680              MOVE CWSFC-REL-NUM (CWSFC-NT-IDX)
776690                                    TO FM1A5-REL-NUM
776700              MOVE CWSFC-PROF-SEQ-NUM-MAX(CWSFC-NT-IDX)
776800                                    TO WS-PARM-PROF-SEQ-NUM-MAX
776801           ELSE
776802              MOVE 'PR021'          TO FM1A5-ERR-CD (1)           00139900
776803              MOVE 'D'              TO FM1A5-STAT-CD              00140000
776804           END-IF
776805        ELSE
776806           MOVE FM105-SER-NUM-JULIAN-DATE TO CWSFC-SEARCH-DATE
776807           PERFORM 1040-GET-RELEASE-NUM   THRU 1040-EXIT
776808           IF CWSFC-SEARCH-MATCH
776809              MOVE CWSFC-REL-NUM (CWSFC-NT-IDX)
776810                                          TO FM105-REL-NUM
776820              MOVE CWSFC-INST-SEQ-NUM-MAX(CWSFC-NT-IDX)
776830                                   TO WS-PARM-INST-SEQ-NUM-MAX
776831           ELSE
776832              MOVE 'IR021'         TO FM105-ERR-CD (1)            00139900
776833              MOVE 'D'             TO FM105-STAT-CD               00140000
776834           END-IF
776835        END-IF
776836     END-IF.
776837                                                                  00159600
776838     IF  WS-PROFESSIONAL                                          00159700
      *R16.5 E_73994 REL-NUM CHANGED FROM 120 TO 165
      *    FMT-CONV-IND ADDED INSTEAD OF FIRST POS OF FILLER
230200        IF FM1A5-REL-NUM >= '165' OR                              23290000
230300           FM1A5-FMT-CONV-IND = 'B'                               23300000
203440           CONTINUE
203450        ELSE
      **
      ** PRLA4 - THE SF IS TO BE CONVERTED TO THE LATEST FORMAT - R16.5
      **
203460           MOVE 'PRLA4'            TO FM1A5-ERR-CD (1)
203470           MOVE 'I'                TO FM1A5-STAT-CD
203480           MOVE 'Y' TO WS-BYPASS-EDITS-IND
203481           GO TO 1010-EXIT                                        18200000
203490        END-IF
203491     ELSE
      *R16.5 E_73994 REL-NUM CHANGED FROM 120 TO 165
      *    FMT-CONV-IND ADDED INSTEAD OF FIRST POS OF FILLER
231200        IF FM105-REL-NUM >= '165' OR                              23390000
231300           FM105-FMT-CONV-IND = 'B'                               23400000
203494           CONTINUE
203495        ELSE
      **
      ** IRL5L - THE SF IS TO BE CONVERTED TO THE LATEST FORMAT - R16.5
      **
203496           MOVE 'IRL5L'            TO FM105-ERR-CD (1)
203497           MOVE 'I'                TO FM105-STAT-CD
203498           MOVE 'Y' TO WS-BYPASS-EDITS-IND
203499           GO TO 1010-EXIT                                        18200000
203500        END-IF
203501     END-IF .
203503
203510     IF  WS-PROFESSIONAL                                          18600000
776839     AND FM1E0-REC-TYPE         = 'E0'                            00159800
776840        IF FM1E0-CLASS-PROV-CLM > SPACES                          00159900
776850           MOVE FM1E0-CLASS-PROV-CLM TO WS-CLASS-PROV             00160100
776860           IF FM1A5-REL-NUM < '084'
776870              IF NOT WS-CLASS-PROV-VALID-VALUES
776880                 MOVE 'N'   TO WS-CLASS-PROV-CLM-OK-SW            00160300
776890              END-IF
776900           ELSE
777000              IF NOT WS-CLASS-PROV-VALID-VALUES-84                00160200
777100                 MOVE 'N'   TO WS-CLASS-PROV-CLM-OK-SW            00160300
777200              END-IF
777300           END-IF
777400        END-IF                                                    00160600
777500*** CLASS-PROV SHOULD BE PRESENT ON THE CLAIM OR ON THE LINE LEVEL
777600*** BUT NOT BOTH
777700*** WS-CLASS-PROV-LN-OK-SW - 'Y' IF PRESENT ON ANY LINE AND VALID
777800***                        - 'N' IF PRESENT ON ANY LINE AND INVAL
777801***                        - ' ' IF NOT PRESENT ON ANY LINE
777802        IF  FM1E0-CLASS-PROV-CLM      > SPACE                     00481100
777803        AND WS-CLASS-PROV-LN-OK-SW    = 'Y' OR 'N'                00481000
777804            MOVE 'N'       TO WS-CLASS-PROV-CLM-OK-SW             00481300
777805            MOVE 'N'       TO WS-CLASS-PROV-LN-OK-SW              00481400
777806        ELSE                                                      00481500
777807            IF  FM1E0-CLASS-PROV-CLM      <= SPACE                00481600
777808            AND WS-CLASS-PROV-LN-BLANK-IND = 'Y'                  00481700
777809                MOVE 'N'       TO WS-CLASS-PROV-CLM-OK-SW         00481800
777810                MOVE 'N'       TO WS-CLASS-PROV-LN-OK-SW          00481900
777811            END-IF                                                00482000
777812        END-IF                                                    00482100
777813        IF FM1E0-TOOTH-STAT  > SPACES                             00160800
777814           MOVE SPACES TO WS-SHIFT-3                              00160900
777815           MOVE FM1E0-TOOTH-STAT TO WS-SHIFT-2                    00161000
777816           MOVE 2 TO WS-SHIFT                                     00161100
777817           PERFORM 9980-SHIFT-FIELDS                              00161200
777818           MOVE WS-SHIFT-FIELD2 TO FM1E0-TOOTH-STAT               00161300
777819        END-IF                                                    00161400
777820        IF FM1E0-TOOTH-NUM-CLM > SPACES                           00161500
777830           MOVE SPACES TO WS-SHIFT-3                              00161600
777840           MOVE FM1E0-TOOTH-NUM-CLM TO WS-SHIFT-2                 00161700
777850           MOVE 2 TO WS-SHIFT                                     00161800
777860           PERFORM 9980-SHIFT-FIELDS                              00161900
777870           MOVE WS-SHIFT-FIELD2 TO FM1E0-TOOTH-NUM-CLM            00162000
777880        END-IF                                                    00162100
777890     END-IF.                                                      00162200
777900                                                                  00162300
236900** R18.0 E_115667 EXL     CCF LOGIC                               24180000
215000     IF  WS-CCF-LN-NUM               > ZERO                       24190000
236900     AND CIABP-HOST-HOME-SITE-IND    = '1'                        24200000
236900         PERFORM 1050-CCF-EDIT    THRU 1050-EXIT                  24210000
236900     END-IF.                                                      24220000
236900                                                                  24230000
778000 1010-EXIT.  EXIT.                                                00162400
778100**********************************************************        00162500
778200 1020-EDIT-PROFESSIONAL.                                          00162600
778300                                                                  00162700
778400     IF CIASC-REC-TYPE(WS-SUB) <=  SPACE                          00162800
778500        SET CIABP-MISS-90-OR-X0-ERR TO TRUE                       00162900
778600        MOVE 'Y'                    TO WS-BYPASS-EDITS-IND        00152800
778700        GO TO 1020-EXIT.                                          00163100
778800                                                                  00163200
209400     IF     CIABP-HOST-HOME-SITE-IND = '1'                        19190000
209500     AND CIASC-SCCF-SERIAL-NUM(WS-SUB) <= SPACE                   19200000
209600         MOVE CIASC-SCCF-SERIAL-NUM(1)                            19210000
209700           TO CIASC-SCCF-SERIAL-NUM(WS-SUB)                       19220000
779200     ELSE                                                         00163600
209810        IF CIASC-SCCF-SERIAL-NUM (WS-SUB) =                       19150000
209820                                    CIASC-SCCF-SERIAL-NUM (1)           00
209830           CONTINUE                                               19170000
779700        ELSE                                                      00164100
209900           MOVE 'N'             TO WS-VALID-SCCF                  19240000
210000           MOVE 'Y'             TO WS-BYPASS-EDITS-IND            19250000
210100           GO TO 1020-EXIT                                        19260000
210110        END-IF                                                    19230000
210120     END-IF.                                                      19230000
780100                                                                  00164500
780200     IF CIASC-TRANS-ID(WS-SUB)(2:1) = '1'                         00164600
780300        CONTINUE                                                  00164700
780400     ELSE                                                         00164800
780500        SET CIABP-TRANS-ID-ERR TO TRUE                            00164900
780600        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
780700        GO TO 1020-EXIT.                                          00165100
780800                                                                  00165200
780900     IF CIASC-SEQUENCE-NUM(WS-SUB) = 0                            00165300
781000     OR CIASC-SEQUENCE-NUM(WS-SUB) > 999                          00165400
781100        SET CIABP-SEQ-NUM-ERR  TO TRUE                            00165500
781200        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
211311        IF CIASC-REC-TYPE (WS-SUB) = 'E6'                         19380000
211320           MOVE '4'  TO CIABP-SEQ-NUM-IND                         19380000
211330        END-IF                                                    19380000
781300        GO TO 1020-EXIT.                                          00165700
781400                                                                  00165800
781500     IF  CIASC-SEQUENCE-NUM(WS-SUB) = 1                           00165900
781600     OR (CIASC-REC-TYPE(WS-SUB)     = 'E6' OR 'F0' OR 'F1' OR     00166000
781700                                      'F5' OR 'F6')               00166100
781800     OR (CIASC-REC-TYPE(WS-SUB)     = 'D1'   AND                  00166200
781900         CIASC-SEQUENCE-NUM(WS-SUB) = 2)                          00166300
782000        CONTINUE                                                  00166400
782100     ELSE                                                         00166500
782200        SET CIABP-SEQ-NUM-ERR  TO TRUE                            00166600
782300        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
782400        GO TO 1020-EXIT.                                          00166800
782500                                                                  00166900
782600     IF CIASC-REC-TYPE(WS-SUB) = 'F0' OR 'F5' OR 'F6' OR 'E6'     00167000
782700              OR 'A5' OR 'B0' OR 'B5' OR 'C0' OR 'D0' OR 'D1'     00167100
212900              OR 'D2' OR 'D3' OR 'E0' OR 'E1' OR 'E2' OR 'G0'     19540000
212910              OR 'X0' OR 'F1'                                     19540000
782900        CONTINUE                                                  00167300
783000     ELSE                                                         00167400
783100        SET CIABP-REC-TYPE-ERR TO TRUE                            00167500
783200        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
783300        GO TO 1020-EXIT                                           00167700
783400     END-IF.                                                      00167800
783500                                                                  00167900
213900     IF CIART-RT-IND   = 'Y' OR 'C'                               19620000
196200        IF CIASC-REC-TYPE(WS-SUB) = 'D0'                          19620000
214400           IF CIASC-STACKED-CLAIM-ITEM(WS-SUB)(26:2) > SPACE      19640000
214500              MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB)(26:2) TO      19640000
214600                                        REF-REL-CD (REF-REL-CD-I) 19630000
196204              SET REF-REL-CD-I UP BY 1                            19630000
196205           END-IF                                                 19640000
196206        END-IF                                                    19640000
196207                                                                  05470000
196209        IF CIASC-REC-TYPE(WS-SUB) = 'D1'                          19620000
196212           MOVE CIASC-STACKED-CLAIM-ITEM (WS-SUB) TO FM1D1-RECORD 19630000
196213           IF FM1D1-PAT-REL-OTH-INS > SPACE                       19640000
196214              MOVE FM1D1-PAT-REL-OTH-INS TO                       19630000
196215                                        REF-REL-CD (REF-REL-CD-I) 19630000
196216              SET REF-REL-CD-I UP BY 1                            19630000
196217           END-IF                                                 19640000
196218           IF FM1D1-SRC-PMT-CD   > SPACE                          19640000
196219              MOVE FM1D1-SRC-PMT-CD TO                            19630000
196220                                      REF-SRC-PMT (REF-SRC-PMT-I) 19630000
196221              SET REF-SRC-PMT-I UP BY 1                           19630000
196222           END-IF                                                 19640000
196223        END-IF                                                    19640000
196224     END-IF.                                                      19640000
196226                                                                  19640000
783600     IF CIASC-REC-TYPE(WS-SUB) = 'E6'                             00168000
783700        MOVE CIASC-SEQUENCE-NUM(WS-SUB) TO CIART-NOTE-LN-CNT      00168100
783800        IF CIASC-STACKED-CLAIM-ITEM(WS-SUB)(26:1) = 'N'
783900           ADD 1                   TO WS-NON-TRANSM-CNT
784000        END-IF                                                    00168200
784100     END-IF.                                                      00168200
784200                                                                  00168300
784300     IF CIASC-REC-TYPE(WS-SUB) = 'F0'                             00168400
784400        ADD 1               TO CIART-SERV-LN-CNT                  00168500
784500        MOVE 'Y'            TO WS-F0-PRESENT-SW                   00168600
784600        MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM1F0-RECORD     00168700
784700        PERFORM 9945-FIND-EARLIEST-F0-DT    THRU 9945-EXIT        00168800
784800        MOVE FM1F0-RECORD TO CIASC-STACKED-CLAIM-ITEM(WS-SUB)     00168900
785000     END-IF                                                       00169100
785100     IF CIASC-REC-TYPE(WS-SUB) = 'F1'                             00169200
785200        ADD 1               TO CIART-SERV-LN-CNT                  00169300
785300        MOVE 'Y'            TO WS-F1-PRESENT-SW                   00169400
785400        MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM1F1-RECORD     00169500
785500        PERFORM 9948-FIND-EARLIEST-F1-DT    THRU 9948-EXIT        00169600
785600        MOVE FM1F1-RECORD TO CIASC-STACKED-CLAIM-ITEM(WS-SUB)     00169700
785800     END-IF.                                                      00169900
785900                                                                  00170000
      ** R18.0 E_115667 EXL SET WS-CCF-LN-NUM                           25320000
249810     IF CIASC-REC-TYPE(WS-SUB) = 'F5'                             25160000
249811        MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM1F5-RECORD     25100000
249812        IF FM1F5-FLEX-NETWK-GRP-LN  > SPACE                       25580000
                 MOVE 'Y'                TO WS-CST-ENTERED-SW           25590000
              END-IF                                                    25600000
249812        IF WS-U200-1-IND = SPACE                                  25180000
249820           IF FM1F5-SF-MSG-CD-LN (1) = 'U201' OR                  25170000
249821              FM1F5-SF-MSG-CD-LN (2) = 'U201' OR                  25170000
249822              FM1F5-SF-MSG-CD-LN (3) = 'U201' OR                  25170000
249823              FM1F5-SF-MSG-CD-LN (4) = 'U201' OR                  25170000
249824              FM1F5-SF-MSG-CD-LN (5) = 'U201'                     25170000
249830              MOVE 'Y'      TO WS-U200-1-IND                      25180000
249831           END-IF                                                 25170000
249840        END-IF                                                    25170000
249820        IF (FM1F5-SF-MSG-CD-LN (1) = '1040' OR                    25470000
249820            FM1F5-SF-MSG-CD-LN (2) = '1040' OR                    25480000
249820            FM1F5-SF-MSG-CD-LN (3) = '1040' OR                    25490000
249820            FM1F5-SF-MSG-CD-LN (4) = '1040' OR                    25500000
249820            FM1F5-SF-MSG-CD-LN (5) = '1040')                      25510000
              AND WS-CCF-LN-NUM          = ZERO                         25520000
                  MOVE FM1F5-SEQ-NUM    TO WS-CCF-LN-NUM                25530000
              END-IF                                                    25540000
249850     END-IF.                                                      25550000
249860                                                                  25170000
786000     IF CIASC-STACKED-CLAIM-ITEM(WS-SUB)(22:4)                    00170100
786100                                          > WS-HOLD-SEQ           00170200
786200        MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB)(22:4)               00170300
786300                                         TO WS-HOLD-SEQ           00170400
786400     ELSE                                                         00170500
786500        SET CIABP-DUP-REC-ERR  TO TRUE                            00170600
786600        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
786700        GO TO 1020-EXIT.                                          00170800
786800                                                                  00170900
786900     IF CIASC-REC-TYPE(WS-SUB) = 'X0'                             00171000
787000        MOVE WS-SUB            TO CIASC-INPUT                     00171100
787100        MOVE 'Y'               TO WS-END-OF-SF                    00171400
787200        IF CIASC-REC-TYPE(WS-SUB + 1) > SPACE                     00171500
787300           SET CIABP-MISS-90-OR-X0-ERR TO TRUE                    00171600
787400           MOVE 'Y'            TO WS-BYPASS-EDITS-IND.            00152800
787500                                                                  00171800
787600 1020-EXIT. EXIT.                                                 00171900
787700**********************************************************        00172000
787800 1030-EDIT-INSTITUTIONAL.                                         00172100
787900                                                                  00172200
788000     IF CIASC-REC-TYPE(WS-SUB) <=  SPACE                          00172300
788100        SET CIABP-MISS-90-OR-X0-ERR TO TRUE                       00172400
788200        MOVE 'Y'                    TO WS-BYPASS-EDITS-IND        00152800
788300        GO TO 1030-EXIT.                                          00172600
788400                                                                  00172700
222300     IF     CIABP-HOST-HOME-SITE-IND = '1'                        20150000
222400     AND CIASC-SCCF-SERIAL-NUM(WS-SUB) <= SPACE                   20160000
222500         MOVE CIASC-SCCF-SERIAL-NUM(1)                            20170000
222600           TO CIASC-SCCF-SERIAL-NUM(WS-SUB)                       20180000
788800     ELSE                                                         00173100
222701        IF CIASC-SCCF-SERIAL-NUM(WS-SUB) =                        20160000
222702                                    CIASC-SCCF-SERIAL-NUM(1)      20160000
222703           CONTINUE                                               20130000
789300        ELSE                                                      00173600
222800           MOVE 'N'                TO WS-VALID-SCCF               20200000
222900           MOVE 'Y'                TO WS-BYPASS-EDITS-IND         20210000
223000           GO TO 1030-EXIT                                        20220000
223010        END-IF                                                    20190000
223020     END-IF.                                                      20190000
789700                                                                  00174000
789800     IF CIASC-TRANS-ID(WS-SUB)(2:1) = '1'                         00174100
789900        CONTINUE                                                  00174200
790000     ELSE                                                         00174300
790100        SET CIABP-TRANS-ID-ERR TO TRUE                            00174400
790200        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
790300        GO TO 1030-EXIT.                                          00174600
790400                                                                  00174700
790500     IF CIASC-SEQUENCE-NUM(WS-SUB) = 0                            00174800
790600     OR CIASC-SEQUENCE-NUM(WS-SUB) > 999                          00174900
790700        SET CIABP-SEQ-NUM-ERR  TO TRUE                            00175000
790800        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
255600                                                                  26080000
224220        IF CIASC-REC-TYPE (WS-SUB) = '46'                         19380000
224230           MOVE '4'  TO CIABP-SEQ-NUM-IND                         19380000
224240        END-IF                                                    19380000
790900        GO TO 1030-EXIT.                                          00175200
791000                                                                  00175300
791100     IF  CIASC-SEQUENCE-NUM(WS-SUB) = 1                           00175400
791200     OR (CIASC-REC-TYPE(WS-SUB)     = '46' OR '60' OR '65' OR     00175500
791300                                      '66')                       00175600
791400     OR ((CIASC-REC-TYPE(WS-SUB)     = '30' OR '31')   AND        00175700
791500          CIASC-SEQUENCE-NUM(WS-SUB) < 4)                         00175800
791600        CONTINUE                                                  00175900
791700     ELSE                                                         00176000
791800        SET CIABP-SEQ-NUM-ERR  TO TRUE                            00176100
791900        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
792000        GO TO 1030-EXIT.                                          00176300
792100                                                                  00176400
792200     IF CIASC-REC-TYPE(WS-SUB) = '60' OR '65' OR '66' OR '46'     00176500
792300                      OR '05' OR '10' OR '15' OR '20' OR '30'     00176600
225800                      OR '31' OR '32' OR '33' OR '40' OR '41'     20500000
225801                      OR '42' OR '43' OR '44' OR '45' OR '47'     20500000
225810                      OR '50' OR '70' OR '71' OR '72' OR '73'     20500000
225820                      OR '74' OR '75' OR '80' OR '90'             20500000
                                                                        26310000
226200        IF   CIASC-REC-TYPE(WS-SUB) = '30'                        20520000
226300        AND (CIART-RT-IND           = 'Y' OR 'C')                 20520000
205210           MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM130-RECORD  20600000
205400           IF FM130-PAT-REL-SUB > SPACE                           20540000
205410              MOVE FM130-PAT-REL-SUB TO REF-REL-CD (REF-REL-CD-I) 20600000
205500              SET REF-REL-CD-I    UP BY 1                         20550000
205600           END-IF                                                 20560000
205610           IF FM130-SRC-PMT-CD  > SPACE                           20540000
205620              MOVE FM130-SRC-PMT-CD TO REF-SRC-PMT (REF-SRC-PMT-I)20600000
205630              SET REF-SRC-PMT-I   UP BY 1                         20550000
205640           END-IF                                                 20560000
205700        END-IF                                                    20570000
205702                                                                  20570000
792600        IF CIASC-REC-TYPE(WS-SUB) = '46'                          00176900
792700           MOVE CIASC-SEQUENCE-NUM(WS-SUB) TO CIART-NOTE-LN-CNT   00177000
792800           IF CIASC-STACKED-CLAIM-ITEM(WS-SUB)(26:1) = 'N'
792900              ADD 1                   TO WS-NON-TRANSM-CNT
793000           END-IF
793100        END-IF                                                    00177100
793200        IF CIASC-REC-TYPE(WS-SUB) = '60'                          00177200
793300           ADD 1                           TO CIART-SERV-LN-CNT   00177300
793400           MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM160-RECORD  00177400
793600           IF FM105-EST-IND = 'E'                                 00177600
793700              MOVE SIAPC-GREG-CURRENT-DT   TO FM160-DT-SERV       00177700
793800              MOVE FM160-RECORD TO                                00177800
793900                   CIASC-STACKED-CLAIM-ITEM(WS-SUB)               00177900
261300           END-IF                                                 26400000
261400        END-IF                                                    26410000
261402                                                                  26340000
261450        IF CIASC-REC-TYPE(WS-SUB) = '65'                          26310000
261460           MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB) TO FM165-RECORD  26130000
261461           IF WS-U200-1-IND = SPACE                               25180000
261470              IF FM165-SF-MSG-CD-LN (1) = 'U201' OR               26310000
261480                 FM165-SF-MSG-CD-LN (2) = 'U201' OR               26310000
261490                 FM165-SF-MSG-CD-LN (3) = 'U201' OR               26310000
261491                 FM165-SF-MSG-CD-LN (4) = 'U201' OR               26310000
261492                 FM165-SF-MSG-CD-LN (5) = 'U201'                  26310000
261493                 MOVE 'Y' TO WS-U200-1-IND                        26320000
261494              END-IF                                              26330000
794000           END-IF                                                 00178000
794100        END-IF                                                    00178100
261497*R15.5 E_90497 DSL END                                            26340000
794200     ELSE                                                         00178200
794300        SET CIABP-REC-TYPE-ERR TO TRUE                            00178300
794400        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
794500        GO TO 1030-EXIT.                                          00178500
794600                                                                  00178600
794700     IF CIASC-STACKED-CLAIM-ITEM(WS-SUB)(22:4)                    00178700
794800                                          > WS-HOLD-SEQ           00178800
794900        MOVE CIASC-STACKED-CLAIM-ITEM(WS-SUB)(22:4)               00178900
795000                                         TO WS-HOLD-SEQ           00179000
795100     ELSE                                                         00179100
795200        SET CIABP-DUP-REC-ERR  TO TRUE                            00179200
795300        MOVE 'Y'               TO WS-BYPASS-EDITS-IND             00152800
795400        GO TO 1030-EXIT.                                          00179400
795500                                                                  00179500
795600     IF CIASC-REC-TYPE(WS-SUB) = '90'                             00179600
795700        MOVE WS-SUB            TO CIASC-INPUT                     00179700
795800        MOVE 'Y'               TO WS-END-OF-SF                    00180000
795900        IF CIASC-REC-TYPE(WS-SUB + 1) > SPACE                     00180100
796000           MOVE 'Y'                    TO WS-BYPASS-EDITS-IND     00152800
796100           SET CIABP-MISS-90-OR-X0-ERR TO TRUE.                   00180200
796200                                                                  00180400
796201 1030-EXIT. EXIT.                                                 00180500
796202*************************************************************     00180600
796203 1040-GET-RELEASE-NUM.
796204                                                                  00180400
796205      SET  CWSFC-NT-IDX TO 1.                                     00180400
796206                                                                  00180400
796207      SEARCH CWSFC-REL-NUM-TABLE VARYING  CWSFC-NT-IDX            00180400
796208          AT END                                                  00180400
796209               MOVE 'N' TO CWSFC-SEARCH-REL-NUM-IND               00180400
796210          WHEN CWSFC-SEARCH-DATE  >=                              00180400
796220               CWSFC-REL-NUM-EFF-DATE (CWSFC-NT-IDX)              00180400
796230           AND CWSFC-SEARCH-DATE  <=                              00180400
796240               CWSFC-REL-NUM-END-DATE (CWSFC-NT-IDX)              00180400
796250               MOVE 'Y' TO CWSFC-SEARCH-REL-NUM-IND               00180400
796260      END-SEARCH.                                                 00180400
796270                                                                  00180400
796271 1040-EXIT. EXIT.                                                 00180500
      ** R18.0 E_115667 EXL      PARAGRAPH 1050-                        27120000
265300*************************************************************     27130000
265100 1050-CCF-EDIT.                                                   27140000
265100                                                                  27150000
265100     IF CDBP16-CCF-START-DT    NUMERIC                            27160000
265100        MOVE CIASC-F0-1-999-SLOT(WS-CCF-LN-NUM)                   27170000
265100                       TO ITCFM1F0-RECORD                         27180000
265100        IF FM1F0-DT-SERV-START   < CDBP16-CCF-START-DT            27190000
265100        OR FM1F0-DT-SERV-START   > CDBP16-CCF-END-DT              27200000
      **                                                                27210002
265100** PRLC1 - HOST PLAN IS NOT ELIGIBLE TO SEND SFMC 1040            27220002
      **                                                                27230002
265100           MOVE 'C'  TO CIAEP-SF-MSG-CD-LN-IND (WS-CCF-LN-NUM, 1) 27240002
265100        GO TO 1050-EXIT                                           27250000
265100        END-IF                                                    27260000
265100     ELSE                                                         27270000
      **                                                                27280002
265100** PRLC1 - HOST PLAN IS NOT ELIGIBLE TO SEND SFMC 1040            27290002
      **                                                                27300002
265100        MOVE 'C'     TO CIAEP-SF-MSG-CD-LN-IND (WS-CCF-LN-NUM, 1) 27310002
265100        GO TO 1050-EXIT                                           27320000
265100     END-IF.                                                      27330000
265100                                                                  27340000
265100     MOVE FM1F0-HCPCS-PROC-CD      TO CCF-HCPCS-PROC-CD.          27350001
265100     MOVE FM1A5-BOID               TO CCF-BUS-OWNER-ID.           27360001
                                                                        27370000
265100     MOVE FM1F0-DT-SERV-START(1:4) TO CCF-HCPCS-EFF-START-DT(1:4).27380001
265100     MOVE FM1F0-DT-SERV-START(5:2) TO CCF-HCPCS-EFF-START-DT(6:2).27390001
265100     MOVE FM1F0-DT-SERV-START(7:2) TO CCF-HCPCS-EFF-START-DT(9:2).27400001
265100     MOVE '-'                      TO CCF-HCPCS-EFF-START-DT(5:1) 27410001
265100                                      CCF-HCPCS-EFF-START-DT(8:1).27420001
265100     MOVE WS-MODULE-ID             TO CSAPI-CALLING-PGM-ID.       27430001
265100     SET  CSAPI-SELECT             TO TRUE.                       27440001
265100     MOVE  1                       TO CSAPI-SUB-COMMAND.          27450001
265100     SET WS-ITDCCFS1               TO TRUE.                       27460001
265100     ADD 1                         TO SIAPC-MODULE-LEVEL.         27470001
265100     MOVE WS-CALL-MODULE TO SIAPC-MODULE-NAME(SIAPC-MODULE-LEVEL).27480001
265100                                                                  27490000
265100     CALL WS-CALL-MODULE        USING WS-ITCCSAPI,                27500001
265100                                      CCF-DATA-AREA.              27510001
265100                                                                  27520000
265100     EVALUATE CSAPI-RETURN-CODE                                   27530001
265100       WHEN 0                                                     27540003
265100            CONTINUE                                              27550001
265100       WHEN 100                                                   27560002
      **                                                                27570001
265100** PRLC2 - SFMC 1040 IS NOT VALID FOR PROCEDURE CODE              27580001
      **                                                                27590001
265100            MOVE 'E' TO CIAEP-SF-MSG-CD-LN-IND (WS-CCF-LN-NUM, 1) 27600002
265100            GO TO 1050-EXIT                                       27610001
265100       WHEN OTHER                                                 27620001
265100            MOVE 'SF028'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)     27630001
265100                              SIAPC-RETURN-MESSAGE-CD             27640001
265100            DISPLAY 'ITCP9102/1050 - BAD SELECT FROM CCF TABLE'   27650001
265100            DISPLAY CSAPI-RETURN-MESSAGE                          27660001
265100            PERFORM 9990-ERROR-DISPLAY         THRU 9990-EXIT     27670001
265100            GO TO 1050-EXIT                                       27680001
265100     END-EVALUATE.                                                27690001
265100                                                                  27700000
265100     MOVE SPACE TO SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).        27710001
265100     SUBTRACT 1                  FROM SIAPC-MODULE-LEVEL.         27720001
265100                                                                  27730001
265100     IF  CCF-HCPCS-INIT-VIS-IND  = 'M'                            27740002
265100     AND CIABP-F0-CTR            >  1                             27750002
      **                                                                27760001
265100** PRLC5 - CCF MAINTENANCE FEE CANNOT BE SUBMITTED WITH OTHER     27770004
      **         SERVICES                                               27780004
265100         MOVE '4'     TO CIAEP-HCPCS-PROC-CD-IND(WS-CCF-LN-NUM)   27790001
265100         GO TO 1050-EXIT                                          27800001
265100     END-IF.                                                      27810001
265100                                                                  27820001
265100     MOVE CIASC-F5-1-999-SLOT(WS-CCF-LN-NUM) TO ITCFM1F5-RECORD.  27830006
                                                                        27840006
265100     EVALUATE FM1F5-PRC-MTD-LN                                    27850001
265100       WHEN '01'                                                  27860001
265100          IF FM1F0-SERV-CHRG NUMERIC                              27870001
265100             MOVE FM1F0-SERV-CHRG          TO WS-ALLOWED-AMT      27880001
265100          END-IF                                                  27890001
265100       WHEN '10'                                                  27900001
265100          IF  FM1F0-SERV-CHRG    NUMERIC                          27910001
265100          AND FM1F5-PCT-FCTR-LN  NUMERIC                          27920001
265100              COMPUTE WS-ALLOWED-AMT = FM1F0-SERV-CHRG            27930001
265100                                     * FM1F5-PCT-FCTR-LN / 100    27940005
265100          END-IF                                                  27950001
265100       WHEN '14'                                                  27960001
265100          IF  FM1F5-SERV-ALLW-AMT NUMERIC                         27970001
265100          AND FM1F5-PCT-FCTR-LN   NUMERIC                         27980001
265100              COMPUTE WS-ALLOWED-AMT = FM1F5-SERV-ALLW-AMT        27990001
265100                                     * FM1F5-PCT-FCTR-LN / 100    28000005
265100          END-IF                                                  28010001
265100       WHEN '15'                                                  28020001
265100          IF  FM1F5-SERV-ALLW-AMT NUMERIC                         28030001
265100          AND FM1F5-PCT-FCTR-LN   NUMERIC                         28040001
265100          AND FM1F0-NUM-SERV      NUMERIC                         28050001
265100              COMPUTE WS-ALLOWED-AMT = FM1F5-SERV-ALLW-AMT        28060001
265100                    * FM1F0-NUM-SERV * FM1F5-PCT-FCTR-LN / 100    28070005
265100          END-IF                                                  28080001
265100       WHEN '40'                                                  28090001
265100          IF FM1F5-SERV-ALLW-AMT NUMERIC                          28100001
265100             MOVE FM1F5-SERV-ALLW-AMT TO WS-ALLOWED-AMT           28110002
265100          END-IF                                                  28120001
265100       WHEN '41'                                                  28130001
265100          IF  FM1F5-SERV-ALLW-AMT NUMERIC                         28140001
265100          AND FM1F0-NUM-SERV      NUMERIC                         28150001
265100              COMPUTE WS-ALLOWED-AMT = FM1F5-SERV-ALLW-AMT        28160001
265100                                     * FM1F0-NUM-SERV             28170001
265100          END-IF                                                  28180001
265100       WHEN '42'                                                  28190001
265100       WHEN '43'                                                  28200001
      **                                                                28210001
265100** PRLC3 - INCLUSIVE PRICING METHOD IS NOT VALID FOR CCF SERVICE  28220001
      **                                                                28230001
265100          MOVE 'G' TO CIAEP-SF-MSG-CD-LN-IND (WS-CCF-LN-NUM, 1)   28240002
265100          GO TO 1050-EXIT                                         28250001
265100       WHEN OTHER                                                 28260001
265100          GO TO 1050-EXIT                                         28270001
265100       END-EVALUATE.                                              28280001
                                                                        28290001
265100     IF WS-ALLOWED-AMT   < CCF-HCPCS-MIN-ALLWD-AMT                28300002
265100     OR WS-ALLOWED-AMT   > CCF-HCPCS-MAX-ALLWD-AMT                28310002
      **                                                                28320001
265100** PRLC4 - ALLOWED AMOUNT DOES NOT MATCH APPROVED CCF RATE        28330001
      **                                                                28340001
265100        MOVE '4'         TO CIAEP-SERV-CHRG-IND (WS-CCF-LN-NUM)   28350002
265100     END-IF.                                                      28360001
265100                                                                  28370001
265200 1050-EXIT. EXIT.                                                 28380001
796272*************************************************************     00180600
117400 1100-PLAN-PROFILE.                                               ITCP9102
117500                                                                  ITCP9102
117600     IF WS-INSTITUTIONAL                                          ITCP9102
117700        MOVE FM105-CNTL-PLAN-CD      TO CIAPP1-SCCF-CNTRL-PLAN-CD ITCP9102
117800                                        WS-PLAN-CD                ITCP9102
117900     ELSE                                                         ITCP9102
118000        IF WS-PROFESSIONAL                                        ITCP9102
118100           MOVE FM1A5-CNTL-PLAN-CD   TO CIAPP1-SCCF-CNTRL-PLAN-CD ITCP9102
118200                                        WS-PLAN-CD                ITCP9102
118300        END-IF                                                    ITCP9102
796283     END-IF.                                                      00181700
118500                                                                  ITCP9102
796285     PERFORM 1110-INITIALIZE-PPF-PARMS          THRU 1110-EXIT.   00181900
796286     PERFORM 9030-CALL-PPF-IO-MODULE            THRU 9030-EXIT.   00182000
118800                                                                  ITCP9102
118900****  SECOND READ WITH RECALCULATED PLAN CODE                 ****ITCP9102
119000                                                                  ITCP9102
119100     IF  CIABP-NOT-EOJ                                            ITCP9102
119200     AND WS-PLAN-CD-WAS-ENTERED                                   ITCP9102
119300     AND SIAPC-RETURN-STATUS = CWSCC-INPUT-CNTRL-PLAN-NE-PP       ITCP9102
119400        PERFORM 1120-COMPUTE-PLAN-CD            THRU 1120-EXIT    ITCP9102
119500        MOVE WS-COMPUTED-PLAN-CD     TO CIAPP1-SCCF-CNTRL-PLAN-CD ITCP9102
119600        PERFORM 1110-INITIALIZE-PPF-PARMS       THRU 1110-EXIT    ITCP9102
119700        PERFORM 9030-CALL-PPF-IO-MODULE         THRU 9030-EXIT    ITCP9102
796297     END-IF.                                                      00183100
119900                                                                  ITCP9102
120000     EVALUATE SIAPC-RETURN-IND                                    ITCP9102
120100        WHEN CWSCC-SUCCESSFUL                                     ITCP9102
120200           IF CIAPP2-SUBMISSION-PROCESS-NO                        ITCP9102
120300              PERFORM 1130-PPF-STATUS-CHECK     THRU 1130-EXIT    ITCP9102
796303              MOVE 'Y'                    TO WS-BYPASS-EDITS-IND  00152800
120400           ELSE                                                   ITCP9102
796305              IF WS-PROFESSIONAL                                  00183900
796306                 IF FM1A5-EST-IND  = 'E'                          00184000
268800*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
268800*                   MOVE '7'   TO CIAPP2-TRANSMIT-MODE-CD-S       27150000
796308                    MOVE '0'   TO CIAPP2-ACCESS-FEE-CD            00184200
796309                    MOVE  0    TO FM1E0-ACCESS-FEE-PCT            00184300
796310                    MOVE '0'   TO CIAPP2-ADMIN-EXPENSE-ALLOW-CD   00184400
796320                    MOVE '0'   TO CIAPP2-CFA-CD                   00184500
796330                 END-IF                                           00184600
796340              ELSE                                                00184700
796341                 IF FM105-EST-IND  = 'E'                          00184800
269600*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
269600*                   MOVE '7'   TO CIAPP2-TRANSMIT-MODE-CD-S       27230000
796343                    MOVE '0'   TO CIAPP2-ACCESS-FEE-CD            00185000
796344                    MOVE  0    TO FM150-ACCESS-FEE-PCT            00185100
796345                    MOVE '0'   TO CIAPP2-ADMIN-EXPENSE-ALLOW-CD   00185200
796346                    MOVE '0'   TO CIAPP2-CFA-CD                   00185300
796347                 END-IF                                           00185400
796348              END-IF                                              00185500
120500              PERFORM 1160-PPF-TO-RECORD        THRU 1160-EXIT    ITCP9102
120600              PERFORM 1170-PPF-TO-ITCIABP       THRU 1170-EXIT    ITCP9102
796360**** FOR BATCH PROCESS CWSPSC COPYBOOK WAS FILLED WITH THE
796370**** PLAN/STATION CODE INFO IN THE INITIALIZATION PARAGRAPH
796380**** PERFORM 9065 PARA FOR REAL TIME ONLY TO GET SPECIFIC PLAN
796390**** INFO
238700**R11.4 E_38714 18.2  ADDED CHECK FOR CIART-RT-IND = 'C'          07320000
238800              IF CIART-RT-IND  = 'Y' OR 'C'                       21620000
796601                 PERFORM 9065-GET-PLANCD-STACD-RT THRU 9065-EXIT  00185800
796602              END-IF                                              00185900
120800           END-IF                                                 ITCP9102
120900                                                                  ITCP9102
121000        WHEN CWSCC-UNSUCCESSFUL                                   ITCP9102
121100           PERFORM 1130-PPF-STATUS-CHECK        THRU 1130-EXIT    ITCP9102
796607           MOVE 'Y'                    TO WS-BYPASS-EDITS-IND     00152800
796608           PERFORM 9900-RESET-SIAPC-RETURN-DATA                   00186400
121200                                                                  ITCP9102
121300        WHEN CWSCC-SEVERE-LOG                                     ITCP9102
796611           SET CIABP-EOJ    TO TRUE                               00186700
796612     END-EVALUATE.                                                00186800
121700                                                                  ITCP9102
121800 1100-EXIT.  EXIT.                                                ITCP9102
121900                                                                  ITCP9102
122000/                                                                 ITCP9102
122100 1110-INITIALIZE-PPF-PARMS.                                       ITCP9102
122200                                                                  ITCP9102
122300     IF CIAPP1-SCCF-CNTRL-PLAN-CD = '000'                         ITCP9102
122400        MOVE SPACES                  TO CIAPP1-SCCF-CNTRL-PLAN-CD ITCP9102
796621     END-IF.                                                      00187700
122600                                                                  ITCP9102
122700     IF WS-INSTITUTIONAL                                          ITCP9102
122800        MOVE FM105-LOC-PLAN-CD       TO CIAPP1-SCCF-LOC-PLAN-CD   ITCP9102
122900        MOVE ZERO                    TO CIAPP1-LOC-PLAN-RECEIPT-C ITCP9102
123000        MOVE FM105-LOC-PLAN-RCPT-DT-MM                            ITCP9102
123100          TO CIAPP1-LOC-PLAN-RECEIPT-MM                           ITCP9102
123200        MOVE FM105-LOC-PLAN-RCPT-DT-DD                            ITCP9102
123300          TO CIAPP1-LOC-PLAN-RECEIPT-DD                           ITCP9102
123400        MOVE FM105-LOC-PLAN-RCPT-DT-YY                            ITCP9102
123500          TO CIAPP1-LOC-PLAN-RECEIPT-YY                           ITCP9102
123600        MOVE FM105-CLM-TYPE          TO CIAPP1-CLAIM-TYPE         ITCP9102
123700                                                                  ITCP9102
123800        IF FM105-INS-INPATIENT                                    ITCP9102
796635           MOVE FM120-ADM-DT            TO CIABP-INCUR-DT         00189100
124000        ELSE                                                      ITCP9102
796637           MOVE FM120-STMT-COV-FROM-DT  TO CIABP-INCUR-DT         00189300
124300        END-IF                                                    ITCP9102
124400                                                                  ITCP9102
124500        MOVE FM130-SUB-ID-PFX-1-3    TO CIAPP1-PREFIX-CD          ITCP9102
269600*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
275100*       MOVE FM130-SUB-GRP-NUM       TO CIAPP1-SCCF-SUB-GRP-NUM   27780000
124700     ELSE                                                         ITCP9102
124800     IF WS-PROFESSIONAL                                           ITCP9102
124900        MOVE FM1A5-LOC-PLAN-CD       TO CIAPP1-SCCF-LOC-PLAN-CD   ITCP9102
125000        MOVE ZERO                    TO CIAPP1-LOC-PLAN-RECEIPT-C ITCP9102
125100        MOVE FM1A5-LOC-PLAN-RCPT-DT-MM                            ITCP9102
125200          TO CIAPP1-LOC-PLAN-RECEIPT-MM                           ITCP9102
125300        MOVE FM1A5-LOC-PLAN-RCPT-DT-DD                            ITCP9102
125400          TO CIAPP1-LOC-PLAN-RECEIPT-DD                           ITCP9102
125500        MOVE FM1A5-LOC-PLAN-RCPT-DT-YY                            ITCP9102
125600          TO CIAPP1-LOC-PLAN-RECEIPT-YY                           ITCP9102
125700        MOVE FM1A5-CLM-TYPE          TO CIAPP1-CLAIM-TYPE         ITCP9102
125800        MOVE FM1D0-SUB-ID-PFX-1-3    TO CIAPP1-PREFIX-CD          ITCP9102
269600*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
276400*       MOVE FM1D0-SUB-GRP-NUM       TO CIAPP1-SCCF-SUB-GRP-NUM   27910000
126100     END-IF                                                       ITCP9102
796656     END-IF.                                                      00191200
126300                                                                  ITCP9102
796658     MOVE ZERO                       TO CIAPP1-INCUR-C.           00191400
796659     MOVE CIABP-INCUR-DT-MM          TO CIAPP1-INCUR-MM.          00191500
796660     MOVE CIABP-INCUR-DT-DD          TO CIAPP1-INCUR-DD.          00191600
796661     MOVE CIABP-INCUR-DT-YY          TO CIAPP1-INCUR-YY.          00191700
126800                                                                  ITCP9102
796663     MOVE WS-1                       TO CIAPP1-FUNC-CODE.         00191900
127000                                                                  ITCP9102
796665     MOVE CWSCC-NORMAL-PROCESSING    TO SIAPC-FUNCTION-CD-MAJOR.  00192100
796666     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB.    00192200
796667     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT.   00192300
127500                                                                  ITCP9102
127600 1110-EXIT.  EXIT.                                                ITCP9102
127700                                                                  ITCP9102
127800/                                                                 ITCP9102
127900 1120-COMPUTE-PLAN-CD.                                            ITCP9102
128000******************************************************************ITCP9102
128100****  A NEW PLAN CODE IS COMPUTED BY ADDING 500 IF A CROSS    ****ITCP9102
128200****  PLAN.  IF A SHIELD PLAN SUBTRACT 500.  FOR PLAN CODES   ****ITCP9102
128300****  LISTED, USE THE SPECIFIED VALUE.  THE COMPUTED PLAN     ****ITCP9102
128400****  CODE WILL BE USED AS THE NEW KEY TO SEARCH PLAN PROFILE.****ITCP9102
128500******************************************************************ITCP9102
128600                                                                  ITCP9102
128700     MOVE ZEROES                     TO WS-COMPUTED-PLAN-CD       ITCP9102
128800                                        WS-NUMERIC-PLAN-CD        ITCP9102
128900                                                                  ITCP9102
129000     IF WS-PLAN-CD NUMERIC                                        ITCP9102
129100        MOVE WS-PLAN-CD              TO WS-NUMERIC-PLAN-CD        ITCP9102
129200     END-IF                                                       ITCP9102
129300                                                                  ITCP9102
129400     EVALUATE TRUE                                                ITCP9102
129500        WHEN WS-NON-STD-PLAN-CDS                                  ITCP9102
129600           EVALUATE WS-PLAN-CD                                    ITCP9102
129700              WHEN '250'                                          ITCP9102
129800                 MOVE '751'          TO WS-COMPUTED-PLAN-CD       ITCP9102
129900                                                                  ITCP9102
130000              WHEN '332'                                          ITCP9102
130100                 MOVE '834'          TO WS-COMPUTED-PLAN-CD       ITCP9102
130200                                                                  ITCP9102
130300              WHEN '350'                                          ITCP9102
130400                 MOVE '851'          TO WS-COMPUTED-PLAN-CD       ITCP9102
130500                                                                  ITCP9102
130600              WHEN '430'                                          ITCP9102
130700                 MOVE '934'          TO WS-COMPUTED-PLAN-CD       ITCP9102
130800                                                                  ITCP9102
130900              WHEN '751'                                          ITCP9102
131000                 MOVE '250'          TO WS-COMPUTED-PLAN-CD       ITCP9102
131100                                                                  ITCP9102
131200              WHEN '834'                                          ITCP9102
131300                 MOVE '332'          TO WS-COMPUTED-PLAN-CD       ITCP9102
131400                                                                  ITCP9102
131500              WHEN '851'                                          ITCP9102
131600                 MOVE '350'          TO WS-COMPUTED-PLAN-CD       ITCP9102
131700                                                                  ITCP9102
131800              WHEN '934'                                          ITCP9102
131900                 MOVE '430'          TO WS-COMPUTED-PLAN-CD       ITCP9102
132000           END-EVALUATE                                           ITCP9102
132100                                                                  ITCP9102
132200        WHEN WS-VIRGINIA-PLAN-CD                                  ITCP9102
132300           MOVE CIAPP2-SCCF-CNTRL-PLAN-CD                         ITCP9102
132400             TO WS-VIRGINIAN-PLAN-CDS                             ITCP9102
132500                                                                  ITCP9102
132600           IF WS-VIRGINIAN-PLAN-CD                                ITCP9102
132700              MOVE CIAPP2-SCCF-CNTRL-PLAN-CD                      ITCP9102
132800                TO WS-COMPUTED-PLAN-CD                            ITCP9102
132900           END-IF                                                 ITCP9102
133000                                                                  ITCP9102
133100        WHEN WS-CROSS-PLAN                                        ITCP9102
133200           COMPUTE WS-COMPUTED-PLAN-CD =                          ITCP9102
133300              WS-NUMERIC-PLAN-CD + WS-500                         ITCP9102
133400                                                                  ITCP9102
133500        WHEN WS-SHIELD-PLAN                                       ITCP9102
133600           COMPUTE WS-COMPUTED-PLAN-CD =                          ITCP9102
133700              WS-NUMERIC-PLAN-CD - WS-500                         ITCP9102
796731     END-EVALUATE.                                                00198700
134000                                                                  ITCP9102
134100 1120-EXIT.  EXIT.                                                ITCP9102
134200                                                                  ITCP9102
796735***********************************************************       00199100
134400 1130-PPF-STATUS-CHECK.                                           ITCP9102
134500                                                                  ITCP9102
134600     MOVE WS-I                       TO CIABP-STAT-CD             ITCP9102
134700     PERFORM 9960-FIND-ERROR-SLOT               THRU 9960-EXIT    ITCP9102
134800                                                                  ITCP9102
134900     IF WS-INSTITUTIONAL                                          ITCP9102
135000        PERFORM 1140-PPF-INST-ERROR             THRU 1140-EXIT    ITCP9102
135100     ELSE                                                         ITCP9102
135200        PERFORM 1150-PPF-PROF-ERROR             THRU 1150-EXIT    ITCP9102
796745     END-IF.                                                      00200100
135700                                                                  ITCP9102
135800 1130-EXIT.  EXIT.                                                ITCP9102
135900                                                                  ITCP9102
227000/                                                                 03206
136100 1140-PPF-INST-ERROR.                                             ITCP9102
136200                                                                  ITCP9102
136300     MOVE WS-I                       TO FM105-STAT-CD             ITCP9102
136400                                                                  ITCP9102
136500     IF CIAPP2-SUBMISSION-PROCESS-NO                              ITCP9102
136600        MOVE 'IPP08'                 TO FM105-ERR-CD (WS-SUB1)    ITCP9102
136700     ELSE                                                         ITCP9102
136800        EVALUATE SIAPC-RETURN-STATUS                              ITCP9102
136900           WHEN CWSCC-RECORD-NOT-FOUND                            ITCP9102
137000              MOVE 'IPP01'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
137100                                                                  ITCP9102
137200           WHEN CWSCC-EDIT-ERROR                                  ITCP9102
137300              MOVE 'IPP02'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
137400                                                                  ITCP9102
137500           WHEN CWSCC-INPUT-CNTRL-PLAN-NE-PP                      ITCP9102
137600              MOVE 'IPP03'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
137700                                                                  ITCP9102
137800           WHEN CWSCC-PP-INVALID-CLAIM-TYPE                       ITCP9102
137900              MOVE 'IPP04'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
138000                                                                  ITCP9102
138100           WHEN CWSCC-NOT-IN-EFF-DATE-RANGE                       ITCP9102
138200              MOVE 'IPP05'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
138300                                                                  ITCP9102
138400           WHEN CWSCC-INCUR-DATE-NOT-IN-RANGE                     ITCP9102
138500              MOVE 'IPP06'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
138600                                                                  ITCP9102
138700           WHEN CWSCC-RECPT-DATE-NOT-IN-RANGE                     ITCP9102
138800              MOVE 'IPP07'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
139200        END-EVALUATE                                              ITCP9102
139300     END-IF                                                       ITCP9102
139400     .                                                            ITCP9102
139500                                                                  ITCP9102
139600 1140-EXIT.  EXIT.                                                ITCP9102
139700                                                                  ITCP9102
231000/                                                                 03246
139900 1150-PPF-PROF-ERROR.                                             ITCP9102
140000                                                                  ITCP9102
140100     MOVE WS-I                       TO FM1A5-STAT-CD             ITCP9102
140200                                                                  ITCP9102
140300     IF CIAPP2-SUBMISSION-PROCESS-NO                              ITCP9102
140400        MOVE 'PPP08'                 TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
140500     ELSE                                                         ITCP9102
140600        EVALUATE SIAPC-RETURN-STATUS                              ITCP9102
140700           WHEN CWSCC-RECORD-NOT-FOUND                            ITCP9102
140800              MOVE 'PPP01'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
140900                                                                  ITCP9102
141000           WHEN CWSCC-EDIT-ERROR                                  ITCP9102
141100              MOVE 'PPP02'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
141200                                                                  ITCP9102
141300           WHEN CWSCC-INPUT-CNTRL-PLAN-NE-PP                      ITCP9102
141400              MOVE 'PPP03'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
141500                                                                  ITCP9102
141600           WHEN CWSCC-PP-INVALID-CLAIM-TYPE                       ITCP9102
141700              MOVE 'PPP04'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
141800                                                                  ITCP9102
141900           WHEN CWSCC-NOT-IN-EFF-DATE-RANGE                       ITCP9102
142000              MOVE 'PPP05'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
142100                                                                  ITCP9102
142200           WHEN CWSCC-INCUR-DATE-NOT-IN-RANGE                     ITCP9102
142300              MOVE 'PPP06'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
142400                                                                  ITCP9102
142500           WHEN CWSCC-RECPT-DATE-NOT-IN-RANGE                     ITCP9102
142600              MOVE 'PPP07'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
143000        END-EVALUATE                                              ITCP9102
143100     END-IF                                                       ITCP9102
143200     .                                                            ITCP9102
143300                                                                  ITCP9102
143400 1150-EXIT.  EXIT.                                                ITCP9102
143500                                                                  ITCP9102
796819******************************************************            00207500
143700 1160-PPF-TO-RECORD.                                              ITCP9102
143800                                                                  ITCP9102
796822     IF (WS-INSTITUTIONAL AND CIABP-HOST-PAR-SITE)                00207800
144000        MOVE CIAPP2-CFA-CD           TO FM105-CFA-CD              ITCP9102
144100        MOVE CIAPP2-CP-CFA-ACCOUNT-CD                             ITCP9102
144200          TO FM105-CNTL-PLAN-CFA-ACCT-CD                          ITCP9102
168300        MOVE CIAPP2-CUST-PRIMY-NTWK TO FM110-CUST-PRIMY-NTWK      01715
171900        MOVE CIAPP2-CUST-SCNDY-NTWK TO FM110-CUST-SCNDY-NTWK      01751
144300        MOVE CIAPP2-ADMIN-EXPENSE-ALLOW-CD                        ITCP9102
144400          TO FM105-AEA-CD                                         ITCP9102
144500        MOVE CIAPP2-ACCESS-FEE-CD    TO FM105-ACCESS-FEE-CD       ITCP9102
269600*R16.0 E_75449 SKG BEGIN
294100        MOVE 'Y'                                                  29680000
144700          TO FM105-PLAN-PROF-ADJ-EDIT-IND                         ITCP9102
294100*       MOVE CIAPP2-ADJUSTMENT-EDIT-IND                           29680000
294200*         TO FM105-PLAN-PROF-ADJ-EDIT-IND                         29690000
269600*R16.0 E_75449 SKG END
144800        MOVE CIAPP2-RESUB-DF-IND     TO FM105-RESUB-DF-IND        ITCP9102
144900        MOVE CIAPP2-PROGRAM-CD       TO FM105-PGM-CD              ITCP9102
145000        MOVE CIAPP2-NATIONAL-OOA-CD  TO FM105-NAT-OOA-CD          ITCP9102
145100                                                                  ITCP9102
145200        MOVE CIAPP2-SCCF-CNTRL-PLAN-CD                            ITCP9102
145300          TO FM105-CNTL-PLAN-CD                                   ITCP9102
145400        MOVE CIAPP2-SCCF-PROCESS-PLAN-CD                          ITCP9102
145500          TO FM105-PROC-SITE-PLAN-CD                              ITCP9102
145800        MOVE CIAPP2-PROCESS-STATION-CD-S                          ITCP9102
145900          TO FM105-PROC-SITE-STA-CD                               ITCP9102
200720        MOVE CIAPP2-DELIVERY-METHOD                               02209
200730          TO FM105-DEL-MTD                                        02210
227500        IF FM105-REL-NUM >= '085'                                 02275
227600           MOVE CIAPP2-UPF-PRICING-EDIT-CD                        02276
227700          TO FM105-UPF-PRC-EDIT-CD                                02277
227800        END-IF                                                    02278
146000                                                                  ITCP9102
146100        IF CIAPP2-STANDARD-RULE                                   ITCP9102
146200           MOVE WS-2                 TO FM105-PLAN-PROF-STND-IND  ITCP9102
146300        ELSE                                                      ITCP9102
146400           MOVE CIAPP2-LOC-PLAN-STATION-CD-S                      ITCP9102
146500             TO FM105-LOC-PLAN-STA-CD                             ITCP9102
146600           MOVE WS-1                 TO FM105-PLAN-PROF-STND-IND  ITCP9102
269600*R16.0 E_75449 SKG BEGIN - COMMENTED BELOW IF CONDITION
296600*          IF CIAPP2-CLAIM-DEVELOPMENT-IND EQUAL WS-Y             29930000
296700*             MOVE CIAPP2-CLAIM-DEVELOPMENT-IND                   29940000
296800*               TO FM105-CLM-DEVL-IND                             29950000
296900*          ELSE                                                   29960000
297000*             IF CIAPP2-CLAIM-DEVELOPMENT-IND EQUAL WS-N          29970000
297100*                IF FM105-CLM-DEVL-IND EQUAL WS-Y                 29980000
297200*                   CONTINUE                                      29990000
297300*                ELSE                                             30000000
297400*                   MOVE WS-N        TO FM105-CLM-DEVL-IND        30010000
297500*                END-IF                                           30020000
297600*             END-IF                                              30030000
297700*          END-IF                                                 30040000
269600*R16.0 E_75449 SKG END
147900        END-IF                                                    ITCP9102
240220        MOVE CIAPP2-ACCT-TYPE-CD     TO FM105-ACCT-TYPE-CD        02742
148000     END-IF                                                       ITCP9102
148100                                                                  ITCP9102
796872     IF (WS-PROFESSIONAL AND CIABP-HOST-PAR-SITE)                 00212800
148300        MOVE CIAPP2-CFA-CD           TO FM1A5-CFA-CD              ITCP9102
148400        MOVE CIAPP2-CP-CFA-ACCOUNT-CD                             ITCP9102
148500          TO FM1A5-CNTL-PLAN-CFA-ACCT-CD                          ITCP9102
168300        MOVE CIAPP2-CUST-PRIMY-NTWK TO FM1B0-CUST-PRIMY-NTWK      01715
171900        MOVE CIAPP2-CUST-SCNDY-NTWK TO FM1B0-CUST-SCNDY-NTWK      01751
148600        MOVE CIAPP2-ADMIN-EXPENSE-ALLOW-CD                        ITCP9102
148700          TO FM1A5-AEA-CD                                         ITCP9102
148800        MOVE CIAPP2-ACCESS-FEE-CD    TO FM1A5-ACCESS-FEE-CD       ITCP9102
269600*R16.0 E_75449 SKG BEGIN
299100        MOVE 'Y'                                                  30180000
149000          TO FM1A5-PLAN-PROF-ADJ-EDIT-IND                         ITCP9102
299100*       MOVE CIAPP2-ADJUSTMENT-EDIT-IND                           30180000
299200*         TO FM1A5-PLAN-PROF-ADJ-EDIT-IND                         30190000
269600*R16.0 E_75449 SKG END
149100        MOVE CIAPP2-RESUB-DF-IND     TO FM1A5-RESUB-DF-IND        ITCP9102
149200        MOVE CIAPP2-PROGRAM-CD       TO FM1A5-PGM-CD              ITCP9102
149300        MOVE CIAPP2-NATIONAL-OOA-CD  TO FM1A5-NAT-OOA-CD          ITCP9102
149400                                                                  ITCP9102
149500        MOVE CIAPP2-SCCF-CNTRL-PLAN-CD                            ITCP9102
149600          TO FM1A5-CNTL-PLAN-CD                                   ITCP9102
149700        MOVE CIAPP2-SCCF-PROCESS-PLAN-CD                          ITCP9102
149800          TO FM1A5-PROC-SITE-PLAN-CD                              ITCP9102
150100        MOVE CIAPP2-PROCESS-STATION-CD-S                          ITCP9102
150200          TO FM1A5-PROC-SITE-STA-CD                               ITCP9102
205020        MOVE CIAPP2-DELIVERY-METHOD                               02255
205030          TO FM1A5-DEL-MTD                                        02256
232700        IF FM1A5-REL-NUM >= '085'                                 02327
232800           MOVE CIAPP2-UPF-PRICING-EDIT-CD                        02328
232900          TO FM1A5-UPF-PRC-EDIT-CD                                02329
233000        END-IF                                                    02330
150300                                                                  ITCP9102
150400        IF CIAPP2-STANDARD-RULE                                   ITCP9102
150500           MOVE WS-2                 TO FM1A5-PLAN-PROF-STND-IND  ITCP9102
150600        ELSE                                                      ITCP9102
150700           MOVE CIAPP2-LOC-PLAN-STATION-CD-S                      ITCP9102
150800             TO FM1A5-LOC-PLAN-STA-CD                             ITCP9102
150900           MOVE WS-1                 TO FM1A5-PLAN-PROF-STND-IND  ITCP9102
269600*R16.0 E_75449 SKG BEGIN - COMMENTED BELOW IF CONDITION
301600*          IF CIAPP2-CLAIM-DEVELOPMENT-IND EQUAL WS-Y             30430000
301700*             MOVE CIAPP2-CLAIM-DEVELOPMENT-IND                   30440000
301800*               TO FM1A5-CLM-DEVL-IND                             30450000
301900*          ELSE                                                   30460000
302000*             IF CIAPP2-CLAIM-DEVELOPMENT-IND EQUAL WS-N          30470000
302100*                IF FM1A5-CLM-DEVL-IND EQUAL WS-Y                 30480000
302200*                   CONTINUE                                      30490000
302300*                ELSE                                             30500000
302400*                   MOVE WS-N        TO FM1A5-CLM-DEVL-IND        30510000
302500*                END-IF                                           30520000
302600*             END-IF                                              30530000
302700*          END-IF                                                 30540000
269600*R16.0 E_75449 SKG END
152200        END-IF                                                    ITCP9102
245420        MOVE CIAPP2-ACCT-TYPE-CD     TO FM1A5-ACCT-TYPE-CD        02797
152300     END-IF                                                       ITCP9102
152400     .                                                            ITCP9102
152500                                                                  ITCP9102
152600 1160-EXIT.  EXIT.                                                ITCP9102
245900/                                                                 03401
152800 1170-PPF-TO-ITCIABP.                                             ITCP9102
152900                                                                  ITCP9102
271500**R11.4 E_38714 DSL BEGIN                                         07320000
271600     MOVE CIAPP2-UPF-PRICING-EDIT-CD TO WS-UPF-PRC-EDIT-CD.       25220000
271700     IF CIAPP2-PROGRAM-CD NOT = '6'                               25220000
271800        MOVE 'Y' TO CIAPP2-SUBMISS-EDIT-IND                       24900000
271900        MOVE '1' TO CIAPP2-UPF-PRICING-EDIT-CD                    24900000
272000     END-IF.                                                      25220000
272100**R11.4 E_38714 END                                               07320000
272200                                                                  24880000
153000****  PLAN PROFILE FIELDS NEEDED FOR HOME PROCESSING EDITS.   ****ITCP9102
153100     MOVE CIAPP2-LOC-PLAN-STATION-CD-S                            ITCP9102
796929       TO CIABP-LOC-PLAN-STA-CD.                                  00218500
153300     MOVE CIAPP2-SCCF-CNTRL-PLAN-CD  TO                           ITCP9102
796931          CIABP-PLAN-PROF-CNTL-PLAN-CD.                           00218700
269600*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
305000*    MOVE CIAPP2-CLAIM-DEVELOPMENT-IND                            30770000
305100*      TO CIABP-CLM-DEVL-IND.                                     30780000
796934     MOVE CIAPP2-ACCESS-FEE-CD       TO CIABP-ACCESS-FEE-CD.      00219000
796935     MOVE CIAPP2-CFA-CD              TO CIABP-CFA-CD.             00219100
153900     MOVE CIAPP2-CP-CFA-ACCOUNT-CD   TO                           ITCP9102
796937          CIABP-CNTL-PLAN-CFA-ACCT-CD.                            00219300
154100     MOVE CIAPP2-ADMIN-EXPENSE-ALLOW-CD                           ITCP9102
796939       TO CIABP-AEA-CD.                                           00219500
269600*R16.0 E_75449 SKG - COMMENTED BELOW MOVE STATMENT
305800*    MOVE CIAPP2-ADJUSTMENT-EDIT-IND TO                           30850000
305900*         CIABP-PLAN-PROF-ADJ-EDIT-IND.                           30860000
796942     MOVE CIAPP2-RESUB-DF-IND        TO CIABP-RESUB-DF-IND.       00219800
796943     MOVE CIAPP2-PROGRAM-CD          TO CIABP-PGM-CD.             00219900
796944     MOVE CIAPP2-NATIONAL-OOA-CD     TO CIABP-NAT-OOA-CD.         00220000
154800     MOVE CIAPP2-SCCF-PROCESS-PLAN-CD                             ITCP9102
796946       TO CIABP-PROC-SITE-PLAN-CD.                                00220200
155100     MOVE CIAPP2-PROCESS-STATION-CD-S                             ITCP9102
796948       TO CIABP-PROC-SITE-STA-CD.                                 00220400
155300                                                                  ITCP9102
155400****  PLAN PROFILE FIELDS NEEDED FOR GENERAL PROCESSING.      ****ITCP9102
796951     MOVE CIAPP2-PROVIDER-DATA-IND   TO CIABP-PROV-DATA-IND.      00220700
796952     MOVE CIAPP2-PRICING-DATA-IND    TO CIABP-PRC-DATA-IND.       00220800
155700     MOVE CIAPP2-SUBMISS-EDIT-IND    TO                           ITCP9102
796954          CIABP-CALL-LEVEL-2-EDIT-IND.                            00221000
796955     MOVE CIAPP2-PLAN-PAYER-CD       TO CIABP-PLAN-PAYER-CD.      00221100
796956     MOVE CIAPP2-DELIVERY-METHOD     TO CIABP-DELV-MTD-IND.       00221200
796957     MOVE CIAPP2-UPF-PRICING-EDIT-CD TO CIABP-UPF-PRC-EDIT-CD.    00221300
796958     MOVE CIAPP2-MANAGED-CARE-CD     TO CIABP-MNG-CARE-CD.        00221400
156200                                                                  ITCP9102
156300     IF CIAPP2-STANDARD-RULE                                      ITCP9102
156400        MOVE WS-2                    TO CIABP-PLAN-PROF-STND-IND  ITCP9102
156500     ELSE                                                         ITCP9102
156600        MOVE WS-1                    TO CIABP-PLAN-PROF-STND-IND  ITCP9102
796964     END-IF.                                                      00222000
156800                                                                  ITCP9102
156900     IF WS-PROFESSIONAL                                           ITCP9102
201300        MOVE FM1A5-PGM-CD            TO WS-PGM-CD                 ITCP9102
201400        MOVE FM1A5-PMT-DISP-CD       TO WS-PMT-DISP-CD            ITCP9102
201500        MOVE FM1A5-LOC-PLAN-CD       TO WS-LOC-PLAN-CD            ITCP9102
201600        MOVE FM1A5-INTL-CD           TO WS-INTL-CD                ITCP9102
201700        MOVE FM1A5-ECR-IND           TO WS-ECR-IND                ITCP9102
201800                                                                  ITCP9102
796973        IF  WS-CLASS-PROV-CLM-OK-SW   = 'Y'                       00222900
796974        AND WS-CLASS-PROV-LN-OK-SW    = 'Y' OR ' '                00222900
796975           IF FM1E0-CLASS-PROV-CLM   > SPACE                      00223100
796976              MOVE FM1E0-CLASS-PROV-CLM   TO WS-CLASS-PROV        00223200
796977              IF WS-NON-PAR-PROV                                  00223400
796978                 MOVE 'Y'     TO WS-CLASS-PROV-NONPAR-IND         00223500
202600              ELSE                                                ITCP9102
796980                 MOVE ' '     TO WS-CLASS-PROV-NONPAR-IND         00223500
202800              END-IF                                              ITCP9102
202900           ELSE                                                   ITCP9102
796983              IF  WS-F0-CLASS-PROV-PAR-IND    = ' '               00224000
796984              AND WS-F0-CLASS-PROV-NONPAR-IND = 'Y'               00224000
796985                  MOVE 'Y'     TO WS-CLASS-PROV-NONPAR-IND        00224100
796986              ELSE                                                00224200
796987                  MOVE ' '     TO WS-CLASS-PROV-NONPAR-IND        00224300
796988              END-IF                                              00224400
203800           END-IF                                                 ITCP9102
203900        ELSE                                                      ITCP9102
796991           MOVE ' '          TO WS-CLASS-PROV-NONPAR-IND          00224700
204100        END-IF                                                    ITCP9102
157100     ELSE                                                         ITCP9102
204400        MOVE FM105-PGM-CD            TO WS-PGM-CD                 ITCP9102
204500        MOVE FM105-PMT-DISP-CD       TO WS-PMT-DISP-CD            ITCP9102
204600        MOVE FM105-LOC-PLAN-CD       TO WS-LOC-PLAN-CD            ITCP9102
204700        MOVE FM105-INTL-CD           TO WS-INTL-CD                ITCP9102
204800        MOVE FM105-ECR-IND           TO WS-ECR-IND                ITCP9102
204900        MOVE FM115-CLASS-PROV-CLM    TO WS-CLASS-PROV             ITCP9102
205000                                                                  ITCP9102
797001        IF WS-NON-PAR-PROV                                        00225700
797002           MOVE 'Y'     TO WS-CLASS-PROV-NONPAR-IND               00225800
205300        ELSE                                                      ITCP9102
797004           MOVE ' '     TO WS-CLASS-PROV-NONPAR-IND               00225800
205500        END-IF                                                    ITCP9102
797006     END-IF.                                                      00226200
209800                                                                  ITCP9102
210000     IF  WS-BCP-PGM-CD                                            ITCP9102
210100     AND CIABP-DELIVERY-BCP-POS                                   ITCP9102
210300        SET WS-BCP-POS                          TO TRUE           ITCP9102
210400     ELSE                                                         ITCP9102
210500        SET WS-NOT-BCP-POS                      TO TRUE           ITCP9102
797013     END-IF.                                                      00226900
210700                                                                  ITCP9102
210800     IF  WS-BCP-PGM-CD                                            ITCP9102
210900     AND WS-NOT-BCP-POS                                           ITCP9102
797017     AND WS-CLASS-PROV-NONPAR-IND   = 'Y'                         00227300
211100        SET WS-ECR-NON-PAR                      TO TRUE           ITCP9102
211200     ELSE                                                         ITCP9102
211300        SET WS-NOT-ECR-NON-PAR                  TO TRUE           ITCP9102
797021     END-IF.                                                      00227700
211500                                                                  ITCP9102
211600     IF  WS-BCP-PGM-CD                                            ITCP9102
211700     AND WS-INTERNATIONAL                                         ITCP9102
211800     AND WS-INTL-PLAN-CDS                                         ITCP9102
222700     AND ((WS-PAY-SUB-REP)                                        02284
222701     OR  (CIABP-HOME-CNTL-SITE                                    02285
222702     AND  WS-PAY-REP-PAYEE                                        02286
797029     AND  FM105-SER-NUM-JULIAN-DATE   < '2001134'))               00228500
212000        SET WS-INTL-PAY-SUB                     TO TRUE           ITCP9102
212100     ELSE                                                         ITCP9102
212200        SET WS-NOT-INTL-PAY-SUB                 TO TRUE           ITCP9102
797033     END-IF.                                                      00228900
262420                                                                  02983
797035     MOVE CIAPP2-ACCT-TYPE-CD TO CIABP-ACCT-TYPE-CD.              00229100
797036     MOVE CIAPP2-CUST-PRIMY-NTWK TO CIABP-CUST-PRIMY-NTWK.        00229200
797037     MOVE CIAPP2-CUST-SCNDY-NTWK TO CIABP-CUST-SCNDY-NTWK.        00229300
358200                                                                  03706
161400 1170-EXIT.  EXIT.                                                ITCP9102
262801/                                                                 03585
797041 1111-REF-DATA-ACCESS.                                            00229700
262804                                                                  03588
283900*R11.4 E_38714 DSL BEGIN                                          05470000
260500     IF  WS-PROFESSIONAL                                          26050000
284100        IF WS-F0-PRESENT-SW = 'Y'                                 26300000
284200           PERFORM 2300-ACCESS-REF-F0       THRU 2300-EXIT        26300000
284300        ELSE                                                      26300000
284400           PERFORM 2305-ACCESS-REF-F1       THRU 2305-EXIT        26300000
284500        END-IF                                                    26300000
284600        IF   FM1A5-REL-NUM  > '102'                               26380000
284700        AND  FM1A5-PGM-CD   = '9'                                 26390000
284800        AND (FM1A5-DEL-MTD  = '7' OR 'A' OR 'B' OR 'C' OR         26400000
284900                              'D' OR 'E' OR 'F')                  26410000
285000            PERFORM 3300-REF-DATA-ZIP-PROF  THRU 3300-EXIT        26300000
285100        END-IF                                                    26380000
285200*R11.4 E_38714 DSL END                                            05470000
260502        IF CIABP-EOJ                                              26300000
260503           GO TO 1111-EXIT                                        26090000
260504        END-IF                                                    26300000
260505     END-IF.                                                      26300000
260506                                                                  05470000
285800**R11.4 E_38714 18.2  ADDED CHECK FOR CIART-RT-IND = 'C'          07320000
285900     IF CIART-RT-IND = 'Y' OR 'C'                                 05470000
260508        IF REF-REL-CD (1)  > SPACE                                26300000
260509           PERFORM 2510-GET-REF-DATA-REL THRU 2510-EXIT           26300000
260510           IF CIABP-EOJ                                           26300000
260511              GO TO 1111-EXIT                                     26090000
260512           END-IF                                                 26300000
260513        END-IF                                                    26300000
260514        IF REF-SRC-PMT (1) > SPACE                                26300000
260515           PERFORM 2520-GET-REF-SRC-PMT  THRU 2520-EXIT           26300000
260516           IF CIABP-EOJ                                           26300000
260517              GO TO 1111-EXIT                                     26090000
260518           END-IF                                                 26300000
260519        END-IF                                                    26300000
260520     END-IF.                                                      05470000
260521*R11.3 E_10097 END                                                05470000
260522                                                                  26300000
797043     IF (WS-PROFESSIONAL       AND                                00229900
797044         FM1A5-REL-NUM < '089')                                   00230000
797045     OR (WS-INSTITUTIONAL      AND                                00230100
797046         FM105-REL-NUM < '089')                                   00230200
797047         GO TO 1111-EXIT.                                         00230300
797048                                                                  00230400
262805     IF WS-PROFESSIONAL                                           03589
797050        PERFORM 1196-PROF-AEA-AMT THRU 1196-EXIT                  00230600
797051        IF WS-D09-PRODUCT-CODE > SPACES                           00230700
797052           PERFORM 2000-GET-AEA-AMT THRU 2000-EXIT                00230800
797053           IF CIABP-EOJ                                           00230900
797054              GO TO 1111-EXIT                                     00231000
797055           END-IF                                                 00231100
797056        END-IF                                                    00231200
797057     ELSE                                                         00231300
797058        PERFORM 1198-INST-AEA-AMT THRU 1198-EXIT                  00231400
797059        IF WS-D09-PRODUCT-CODE > SPACES                           00231500
797060           PERFORM 2000-GET-AEA-AMT THRU 2000-EXIT                00231600
797061           IF CIABP-EOJ                                           00231700
797062              GO TO 1111-EXIT                                     00231800
797063           END-IF                                                 00231900
797064        END-IF                                                    00232000
797065     END-IF.                                                      00232100
262808                                                                  03592
289900*R11.4 E_38714 DSL BEGIN - THESE ARE BEING DONE IN PARA 2300-     05470000
290000*    IF WS-PROFESSIONAL                                           26290000
262910*R11.3 E_10097 DSL 2300- MOVED TO TOP OF THE PARAGRAPH            05470000
263000*       PERFORM 2300-ACCESS-REF-DATA-POS      THRU 2300-EXIT      26300000
263100*       IF CIABP-EOJ                                              26310000
263200*          GO TO 1111-EXIT                                        26320000
263300*       END-IF                                                    26330000
263310*R11.3 E_10097 END                                                05470000
290700*                                                                 05470000
290800*       PERFORM 2400-ACCESS-REF-DATA-TOS      THRU 2400-EXIT      26340000
290900*       IF CIABP-EOJ                                              26350000
291000*          GO TO 1111-EXIT                                        26360000
291100*       END-IF                                                    26370000
291200*       IF   FM1A5-REL-NUM  > '102'                               26380000
291300*       AND  FM1A5-PGM-CD   = '9'                                 26390000
291400*       AND (FM1A5-DEL-MTD  = '7' OR 'A' OR 'B' OR 'C' OR         26400000
291500*                             'D' OR 'E' OR 'F')                  26410000
291600*          PERFORM 2700-REF-DATA-ZIP-PROF        THRU 2700-EXIT   26420000
291700*          IF CIABP-EOJ                                           26430000
291800*             GO TO 1111-EXIT                                     26440000
291900*          END-IF                                                 26450000
292000*       END-IF                                                    26460000
292100*    END-IF.                                                      26470000
292200*R11.4 E_38714 DSL END                                            05470000
359206                                                                  03735
797303     IF WS-INSTITUTIONAL                                          00234300
292500*R11.4 E_38714 DSL - THIS IS NOW DONE IN PARA 2600-               05470000
292600**      PERFORM 2500-ACCESS-REF-DATA-REV      THRU 2500-EXIT      26500000
797305        IF CIABP-EOJ                                              00234500
797306           GO TO 1111-EXIT                                        00234600
797307        ELSE                                                      00234700
797308           PERFORM 2600-PROCESS-REC-60        THRU 2600-EXIT      00234800
368840        END-IF                                                    00380200
293200        IF   FM105-REL-NUM  > '102'                               26560000
293300        AND  FM105-PGM-CD   = '9'                                 26570000
293400        AND (FM105-DEL-MTD  = '7' OR 'A' OR 'B' OR 'C' OR         26580000
797340                              'D' OR 'E' OR 'F')                  00235300
797350           PERFORM 2800-REF-DATA-ZIP-INST     THRU 2800-EXIT      00235400
360205        END-IF                                                    03752
797370     END-IF.                                                      00235600
262862                                                                  03631
262863 1111-EXIT.                                                       03632
262864     EXIT.                                                        03633
326400/                                                                 32910000
797374****************************************************              00236000
383522 1182-PROF-AEA-AMT-PROCESS.                                       04024
713201                                                                  04027
713202     IF  FM1A5-SER-NUM-JULIAN-DATE > '2000177'                    04028
713203         IF  FM1A5-ADMIN-EXP-ALLW NUMERIC  AND                    04029
713204             (FM1A5-ADMIN-EXP-ALLW = ZERO  OR                     04030
713205             FM1A5-ADMIN-EXP-ALLW > ZERO)                         04031
713206             CONTINUE                                             04032
713207         ELSE                                                     04033
713208*                                                                 04034
713209*.... PA019 (ADMIN EXPENSE ALLOWANCE MUST BE NUMERIC AND >= ZERO) 04035
713210*                                                                 04036
713211             PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT        04037
713213             MOVE 'PA019'        TO FM1A5-ERR-CD (WS-SUB1)        04039
797388             MOVE '2'            TO CIAEP-ADMIN-EXP-ALLW-IND
797389             GO TO 1182-EXIT                                      00237400
713214         END-IF                                                   04040
713215     END-IF.                                                      04041
713216                                                                  04042
797393     IF  CIABP-HOST-PAR-SITE                                      00237800
713236         IF WS-ECR-PROC  OR  WS-ECR-HIPAA-PROC                    04062
713237            EVALUATE TRUE                                         04063
713238                                                                  04064
713239            WHEN CIAPP2-AEA-NOT-APPLICABLE                        04065
713240            WHEN CIAPP2-AEA-WAIVED-PCPM                           04066
713241                 MOVE ZEROES           TO FM1A5-ADMIN-EXP-ALLW    04067
713242                                                                  04068
713243            WHEN CIAPP2-AEA-STANDARD-ALLOW                        04069
713244                IF FM1A5-REL-NUM < '089'                          04070
713245                   IF WS-INTERNATIONAL                            04071
713246                      MOVE WS-AEA-INTL-AMT                        04072
713247                                         TO FM1A5-ADMIN-EXP-ALLW  04073
713248                   ELSE                                           04074
713249                      MOVE WS-AEA-NON-INTL-AMT                    04075
713250                                         TO FM1A5-ADMIN-EXP-ALLW  04076
713251                   END-IF                                         04077
713252                ELSE                                              04078
797411                   IF WS-AEA-AMT-X  NUMERIC                       00239600
797412                      MOVE WS-AEA-AMT TO FM1A5-ADMIN-EXP-ALLW     00239700
713255                   END-IF                                         04081
713256                END-IF                                            04082
713257                                                                  04083
713258              WHEN CIAPP2-AEA-NON-STANDARD-ALLOW                  04084
713259                IF FM1A5-REL-NUM < '089'                          04085
713260                   IF WS-INTERNATIONAL                            04086
713261                      IF CIAPP2-AEA-NON-STD-AMT                   04087
713262                                        < WS-AEA-INTL-AMT         04088
713263                         MOVE CIAPP2-AEA-NON-STD-AMT              04089
713264                                         TO FM1A5-ADMIN-EXP-ALLW  04090
713265                      ELSE                                        04091
713266                         MOVE WS-AEA-INTL-AMT                     04092
713267                                         TO FM1A5-ADMIN-EXP-ALLW  04093
713268                      END-IF                                      04094
713269                   ELSE                                           04095
713270                      IF CIAPP2-AEA-NON-STD-AMT <                 04096
713271                                         WS-AEA-NON-INTL-AMT      04097
713272                         MOVE CIAPP2-AEA-NON-STD-AMT              04098
713273                                         TO FM1A5-ADMIN-EXP-ALLW  04099
713274                      ELSE                                        04100
713275                         MOVE WS-AEA-NON-INTL-AMT                 04101
713276                                         TO FM1A5-ADMIN-EXP-ALLW  04102
713277                      END-IF                                      04103
713278                   END-IF                                         04104
713279                ELSE                                              04105
797438                   IF WS-AEA-AMT-X  NUMERIC                       00242300
713281                      IF (CIAPP2-AEA-NON-STD-AMT                  04107
797440                                    > WS-AEA-AMT)                 00242500
797441                          MOVE WS-AEA-AMT                         00242600
713284                                         TO FM1A5-ADMIN-EXP-ALLW  04110
713285                      ELSE                                        04111
713286                          MOVE CIAPP2-AEA-NON-STD-AMT             04112
713287                                         TO FM1A5-ADMIN-EXP-ALLW  04113
713288                      END-IF                                      04114
713289                   END-IF                                         04115
713290                END-IF                                            04116
713291            END-EVALUATE                                          04117
713292         ELSE                                                     04118
713293            CONTINUE                                              04119
713294         END-IF                                                   04120
713295     END-IF.                                                      04121
713297                                                                  04123
713298 1182-EXIT.                                                       04124
713299     EXIT.                                                        04125
713300                                                                  04126
713302                                                                  04128
713304 1181-INST-AEA-AMT-PROCESS.                                       04130
713307                                                                  04133
713308     IF  FM105-SER-NUM-JULIAN-DATE > '2000177'                    04134
713309         IF  FM105-ADMIN-EXP-ALLW NUMERIC                         04135
713310             AND (FM105-ADMIN-EXP-ALLW = ZERO                     04136
713311             OR  FM105-ADMIN-EXP-ALLW > ZERO)                     04137
713312             CONTINUE                                             04138
713313         ELSE                                                     04139
713314*                                                                 04140
713315*.... IA019 (ADMIN EXPENSE ALLOWANCE MUST BE NUMERIC AND >= ZERO) 04141
713316*                                                                 04142
713317             PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT        04143
713319             MOVE 'IA019'        TO FM105-ERR-CD (WS-SUB1)        04145
797490             MOVE '2'            TO CIAEI-ADMIN-EXP-ALLW-IND
797491             GO TO 1181-EXIT                                      00245700
713320         END-IF                                                   04146
713321     END-IF.                                                      04147
713322                                                                  04148
797495     IF CIABP-HOST-PAR-SITE                                       00246100
797496        IF WS-ECR-PROC  OR  WS-ECR-HIPAA-PROC                     00246200
713344            EVALUATE TRUE                                         04170
713346            WHEN CIAPP2-AEA-NOT-APPLICABLE                        04172
713347            WHEN CIAPP2-AEA-WAIVED-PCPM                           04173
713348                 MOVE ZEROES           TO FM105-ADMIN-EXP-ALLW    04174
713349                                                                  04175
713350            WHEN CIAPP2-AEA-STANDARD-ALLOW                        04176
713351                IF FM105-REL-NUM < '089'                          04177
713352                   IF WS-INTERNATIONAL                            04178
713353                      MOVE WS-AEA-INTL-AMT                        04179
797506                                       TO FM105-ADMIN-EXP-ALLW    00247200
713355                   ELSE                                           04181
713356                      MOVE WS-AEA-NON-INTL-AMT                    04182
797509                                       TO FM105-ADMIN-EXP-ALLW    00247500
713358                   END-IF                                         04184
713359                ELSE                                              04185
797512                   MOVE WS-AEA-AMT     TO FM105-ADMIN-EXP-ALLW    00247800
713363                END-IF                                            04189
713364                                                                  04190
713365              WHEN CIAPP2-AEA-NON-STANDARD-ALLOW                  04191
713366                IF FM105-REL-NUM < '089'                          04192
713367                   IF WS-INTERNATIONAL                            04193
713368                      IF CIAPP2-AEA-NON-STD-AMT                   04194
713369                                        <  WS-AEA-INTL-AMT        04195
713370                         MOVE CIAPP2-AEA-NON-STD-AMT              04196
713371                                         TO FM105-ADMIN-EXP-ALLW  04197
713372                      ELSE                                        04198
713373                         MOVE WS-AEA-INTL-AMT                     04199
713374                                         TO FM105-ADMIN-EXP-ALLW  04200
713375                      END-IF                                      04201
713376                   ELSE                                           04202
713377                      IF CIAPP2-AEA-NON-STD-AMT <                 04203
713378                                         WS-AEA-NON-INTL-AMT      04204
713379                         MOVE CIAPP2-AEA-NON-STD-AMT              04205
713380                                         TO FM105-ADMIN-EXP-ALLW  04206
713381                      ELSE                                        04207
713382                         MOVE WS-AEA-NON-INTL-AMT                 04208
713383                                         TO FM105-ADMIN-EXP-ALLW  04209
713384                      END-IF                                      04210
713385                   END-IF                                         04211
713386                ELSE                                              04212
797537                   IF (CIAPP2-AEA-NON-STD-AMT                     00250300
797538                                      > WS-AEA-AMT)               00250400
797539                       MOVE WS-AEA-AMT                            00250500
797540                                      TO FM105-ADMIN-EXP-ALLW     00250600
797541                   ELSE                                           00250700
797542                       MOVE CIAPP2-AEA-NON-STD-AMT                00250800
797543                                      TO FM105-ADMIN-EXP-ALLW     00250900
713395                      END-IF                                      04221
713397                END-IF                                            04223
713398            END-EVALUATE                                          04224
713399         ELSE                                                     04225
713400            CONTINUE                                              04226
713401         END-IF                                                   04227
713402     END-IF.                                                      04228
713403                                                                  04229
713404 1181-EXIT.                                                       04230
713405     EXIT.                                                        04231
797554*******************************************************           00252000
797555 1196-PROF-AEA-AMT.                                               00252100
713406                                                                  04232
797557     MOVE SPACES                    TO WS-D09-PRODUCT-CODE.       00252300
797558                                                                  00252400
797559     MOVE FM1A5-LOC-PLAN-RCPT-DT    TO WS-D09-LOC-PLAN-RCPT-DT.   00252500
797560     MOVE FM1A5-ECR-IND             TO WS-D09-ECR-IND.            00252600
797561     MOVE FM1A5-BCP-PROD-TYPE       TO WS-D09-BCP-PROD-TYPE.      00252700
797562                                                                  00252800
797563     PERFORM 1197-SET-DATE-PROD-CD  THRU 1197-EXIT.               00252900
797564                                                                  00253000
797565 1196-EXIT.                                                       00253100
797566     EXIT.                                                        00253200
713409/                                                                 04235
713410 1197-SET-DATE-PROD-CD.                                           04236
713411                                                                  04237
713412     IF  WS-D09-LOC-PLAN-RCPT-DT                                  04238
713413                           NOT EQUAL SPACES AND LOW-VALUES        04239
713414         MOVE ZEROS                       TO CWSDT-G-CENTURY      04240
713415         MOVE WS-D09-LOC-PLAN-RCPT-DT-YY  TO CWSDT-G-YEAR         04241
713416         MOVE WS-D09-LOC-PLAN-RCPT-DT-MM  TO CWSDT-G-MONTH        04242
713417         MOVE WS-D09-LOC-PLAN-RCPT-DT-DD  TO CWSDT-G-DAY          04243
713418                                                                  04244
713419         MOVE 'GR'                        TO CWSDT-DC-FROM        04245
713420         MOVE 'TJ'                        TO CWSDT-DC-TO          04246
713421         MOVE '2'                         TO CWSDT-DATE-IND       04247
713422                                                                  04248
713423         PERFORM  X100-ITCPDDT-DATECONV   THRU                    04249
713424                                     X100-ITCPDDT-DC-EXIT         04250
713425                                                                  04251
713426         IF CWSDT-G-DATE = 99999999                               04252
797585            MOVE '9999'                  TO WS-RCPT-CCYY          00255100
797586            MOVE '12'                    TO WS-RCPT-MM            00255200
797587            MOVE '31'                    TO WS-RCPT-DD            00255300
713428         ELSE                                                     04254
797589            MOVE CWSDT-G-CENTURY         TO WS-RCPT-CC            00255500
797590            MOVE CWSDT-G-YEAR            TO WS-RCPT-YY            00255600
797591            MOVE CWSDT-G-MONTH           TO WS-RCPT-MM            00255700
797592            MOVE CWSDT-G-DAY             TO WS-RCPT-DD            00255800
713430         END-IF                                                   04256
713431     END-IF.                                                      04257
713432                                                                  04258
423800     EVALUATE TRUE                                                04212
423900     WHEN (WS-D09-ECR-Y            AND                            04213
424000           WS-INTERNATIONAL)                                      04214
424100        MOVE 'O'                 TO WS-D09-PRODUCT-CODE           04215
424200     WHEN  (WS-BCP-PGM-CD          AND                            04216
424300            WS-D09-ECR-Y           AND                            04217
797602            WS-CLASS-PROV-NONPAR-IND   = 'Y')                     00256800
424500        MOVE 'D'                 TO WS-D09-PRODUCT-CODE           04219
424600     WHEN (WS-ECR-PGM-CD           AND                            04220
424700           WS-D09-ECR-Y            AND                            04221
797606           CIABP-DELIVERY-NON-MA   AND                            00257200
797607           WS-CLASS-PROV-NONPAR-IND   = 'Y')                      00257300
425000        MOVE 'E'                 TO WS-D09-PRODUCT-CODE           04224
425100     WHEN (WS-ECR-PGM-CD           AND                            04225
425200           WS-D09-ECR-Y            AND                            04226
425300           CIABP-DELIVERY-MED-COMP)                               04227
425400        MOVE 'F'                 TO WS-D09-PRODUCT-CODE           04228
425500     WHEN (WS-ECR-PGM-CD           AND                            04229
425600           WS-D09-ECR-Y            AND                            04230
425700           CIABP-DELIVERY-MA)                                     04231
425800        MOVE 'G'                 TO WS-D09-PRODUCT-CODE           04232
425900     WHEN (WS-ECR-PGM-CD           AND                            04233
426000           WS-D09-ECR-Y            AND                            04234
426100           CIABP-DELIVERY-NON-MA   AND                            04235
797620           WS-CLASS-PROV-NONPAR-IND   = ' ')                      00258600
426300        MOVE 'H'                 TO WS-D09-PRODUCT-CODE           04237
426500     WHEN (WS-BCP-PGM-CD           AND                            04238
426600           WS-D09-ECR-H            AND                            04239
797624           WS-CLASS-PROV-NONPAR-IND   = 'Y')                      00259000
426800        MOVE 'I'                 TO WS-D09-PRODUCT-CODE           04241
427000     WHEN (WS-ECR-PGM-CD           AND                            04242
427100           WS-D09-ECR-H            AND                            04243
427200           CIABP-DELIVERY-NON-MA   AND                            04244
797629           WS-CLASS-PROV-NONPAR-IND   = 'Y')                      00259500
427400        MOVE 'J'                 TO WS-D09-PRODUCT-CODE           04246
427600     WHEN (WS-ECR-PGM-CD           AND                            04247
427700           WS-D09-ECR-H            AND                            04248
427800           CIABP-DELIVERY-MED-COMP)                               04249
427900        MOVE 'K'                 TO WS-D09-PRODUCT-CODE           04250
428100     WHEN (WS-ECR-PGM-CD           AND                            04251
428200           WS-D09-ECR-H            AND                            04252
428300           CIABP-DELIVERY-MA)                                     04253
428400        MOVE 'L'                 TO WS-D09-PRODUCT-CODE           04254
428600     WHEN (WS-ECR-PGM-CD           AND                            04255
428700           WS-D09-ECR-H            AND                            04256
428800           CIABP-DELIVERY-NON-MA   AND                            04257
797642           WS-CLASS-PROV-NONPAR-IND   = ' ')                      00260800
429000        MOVE 'M'                 TO WS-D09-PRODUCT-CODE           04259
429001     WHEN (CIABP-DELIVERY-MEDICAID AND                            04260
429002          (WS-D09-ECR-Y            OR                             04261
429003           WS-D09-ECR-H))                                         04262
429004        MOVE 'Q'                 TO WS-D09-PRODUCT-CODE           04263
429010     END-EVALUATE.                                                04264
713490                                                                  04316
713491 1197-EXIT.                                                       04317
713492     EXIT.                                                        04318
713493                                                                  04319
713494/                                                                 04320
713495 1198-INST-AEA-AMT.                                               04321
713496                                                                  04322
797656     MOVE SPACES                    TO WS-D09-PRODUCT-CODE.       00262200
713500     MOVE FM105-LOC-PLAN-RCPT-DT    TO WS-D09-LOC-PLAN-RCPT-DT.   04326
713501                                                                  04327
713502     MOVE FM105-ECR-IND             TO WS-D09-ECR-IND.            04328
713503     MOVE FM105-BCP-PROD-TYPE       TO WS-D09-BCP-PROD-TYPE.      04329
713504                                                                  04330
713505     PERFORM 1197-SET-DATE-PROD-CD  THRU 1197-EXIT.               04331
713506                                                                  04332
713521 1198-EXIT.                                                       04347
713522     EXIT.                                                        04348
713523                                                                  04349
263230***************************************************************** 03872
213100 1180-ECR-EDITS.                                                  ITCP9102
213200                                                                  ITCP9102
797670     IF WS-INSTITUTIONAL                                          00263600
797671        IF ((WS-HOST-OVRIDE-ECR-PROC  OR                          00263700
797672             WS-ECR-PROC              OR                          00263800
797673             WS-ECR-HIPAA-PROC)       AND                         00263900
797674             (FM105-REL-NUM < '094'))                             00264000
797675        OR  CIABP-HOME-CNTL-SITE                                  00264100
797676        OR  (FM105-SER-NUM-JULIAN-DATE < '2000178'   AND          00264110
797677             FM105-ORIGINAL-RESUBMISSION)                         00264200
797678        OR  (FM105-REF-NUM-JULIAN-DATE < '2000178'   AND          00264320
797679            (FM105-ADJUSTMENT OR FM105-ADJUSTMENT-RESUBMISSION))  00264330
797680             CONTINUE                                             00264400
797681        ELSE                                                      00264500
797682           IF WS-INTL-PAY-SUB                                     00264600
797683              IF  (FM105-REL-NUM < '094')                         00264700
797684                    MOVE WS-Y           TO FM105-ECR-IND          00264800
797685                                           WS-ECR-IND             00264900
797686              ELSE                                                00265000
797687                    MOVE SPACE          TO FM105-ECR-IND          00265100
797688                                           WS-ECR-IND             00265200
797689              END-IF                                              00265300
463800           ELSE                                                   04613
797691              IF (WS-ECR-PGM-CD OR WS-ECR-NON-PAR)                00265500
797692                  IF  FM105-SER-NUM-JULIAN-DATE >=                00265600
797693                      CDBP20-835-MOD-DATE                         00265700
797694                  AND FM105-835-IND = 'Y'                         00265800
466500                      IF  (FM105-REL-NUM < '094')                 04640
464000                           MOVE WS-H  TO FM105-ECR-IND            04615
464100                                         WS-ECR-IND               04616
465200                      END-IF                                      04627
797699                   ELSE                                           00266300
466500                      IF  (FM105-REL-NUM < '094')                 04640
464000                           MOVE WS-Y  TO FM105-ECR-IND            04615
464100                                         WS-ECR-IND               04616
466500                      ELSE                                        04640
464000                           MOVE SPACE TO FM105-ECR-IND            04615
464100                                         WS-ECR-IND               04616
465200                      END-IF                                      04627
797707                  END-IF                                          00267100
797708                  PERFORM 1183-REVIEW-NEW-CRITERIA-INST           00267200
465200                            THRU 1183-EXIT                        04627
465300                 ELSE                                             04628
465400                    CONTINUE                                      04629
465600              END-IF                                              04631
465700           END-IF                                                 04632
465800        END-IF                                                    04633
452900                                                                  04504
219100        IF  (FM105-ADJUSTMENT OR FM105-ADJUSTMENT-RESUBMISSION)   02191
219200        AND FM105-CROSS-REF-SCCF-NUM NOT NUMERIC                  02192
797718            MOVE '2'  TO  CIAEI-CROSS-REF-SCCF-NUM-IND
219500        END-IF                                                    02195
219600                                                                  02196
219700        IF (WS-HOST-OVRIDE-ECR-PROC OR WS-STD-BCP-PROC)           02197
213800           MOVE ZEROES               TO FM105-ADMIN-EXP-ALLW      ITCP9102
214100        END-IF                                                    ITCP9102
214300                                                                  ITCP9102
797725        IF FM105-SER-NUM-JULIAN-DATE > '2000177'                  00268400
220200           IF  WS-ECR-IND-VALID-VALUES                            02202
257000           OR  (FM105-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE  02652
257100              AND WS-ECR-IND-VALID-VALUES-R85)                    02653
220300              CONTINUE                                            02203
220400           ELSE                                                   02204
214700*                                                                 ITCP9102
220600*.... IE015 (ELECTRONIC CLAIM ROUTING INDICATOR IF PRESENT MUST   02206
257800*     BE 'Y', 'N' OR ('H' WHEN SCCF JULIAN DATE >= PDB 835        02658
257801*     MOD DATE))                                                  02659
255800***********  END ITS-TM-02-04 15.5 CR01-0046 CHANGES  *********** 02558
215000*                                                                 ITCP9102
220900              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02209
221100              MOVE 'IE015'           TO FM105-ERR-CD (WS-SUB1)    02211
797739              MOVE '2'               TO CIAEI-ECR-IND
221200           END-IF                                                 02212
258500        END-IF                                                    02667
258600                                                                  02668
258610        IF FM105-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE       02670
258611           IF FM105-835-IND = ('Y' OR 'N')                        02671
258612              CONTINUE                                            02672
258613           ELSE                                                   02673
258614*                                                                 02674
258615*.... IE019 (835 INDICATOR MUST BE 'Y' OR 'N' WHEN SCCF SERIAL    02675
258616*     NUMBER JULIAN DATE >= PDB 835 MOD DATE)                     02676
258620*                                                                 02677
258621              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02678
258623              MOVE 'IE019'           TO FM105-ERR-CD (WS-SUB1)    02680
797753              MOVE '2'               TO CIAEI-835-IND
258624           END-IF                                                 02681
258625        ELSE                                                      02682
258626           MOVE SPACES               TO FM105-835-IND             02683
215800        END-IF                                                    ITCP9102
216000                                                                  ITCP9102
221500        IF  WS-ECR-IND-VALID-VALUES                               02215
221600        AND WS-ECR-PROC                                           02216
221700           IF WS-ECR-PGM-CD                                       02217
221800           OR (WS-ECR-NON-PAR AND WS-NOT-INTERNATIONAL)           02218
221900           OR (WS-INTL-PAY-SUB AND WS-INTERNATIONAL)              02219
222000              CONTINUE                                            02220
222100           ELSE                                                   02221
216800*                                                                 ITCP9102
222300*.... IRL94 (ELECTRONIC CLAIM ROUTING PROCESS VALID FOR HANDLE    02223
222400*     DIRECT, NON PAR AND INTL PAY SUB CLAIMS ONLY)               02224
217100*                                                                 ITCP9102
222600              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02226
217500              MOVE 'IRL94'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
797781              MOVE '4'               TO CIAEI-ECR-IND
221600           END-IF                                                 ITCP9102
221700        END-IF                                                    ITCP9102
221800                                                                  ITCP9102
223200        IF  WS-ECR-IND-VALID-VALUES                               02232
223300        AND WS-HOST-OVRIDE-ECR-PROC                               02233
797787            IF WS-ECR-NON-PAR                                     00273400
797788            OR WS-INTL-PAY-SUB                                    00273500
797789               CONTINUE                                           00273600
797790            ELSE                                                  00273700
223800*                                                                 02238
223900*.... IRL95 (HOST CAN SET ECR INDICATOR TO 'N' ONLY FOR NON PAR   02239
224000*     AND INTL PAY SUB CLAIMS)                                    02240
224100*                                                                 02241
797795               PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT   00274200
797796               MOVE 'IRL95'           TO FM105-ERR-CD (WS-SUB1)   00274300
797797               MOVE '5'               TO CIAEI-ECR-IND
797798            END-IF                                                00274400
222100        END-IF                                                    ITCP9102
222200                                                                  ITCP9102
260000        IF  WS-ECR-IND-VALID-VALUES-R85                           02600
260100        AND WS-ECR-HIPAA-PROC                                     02601
466858        AND NOT WS-INTERNATIONAL                                  00472900
797804            IF WS-ECR-PGM-CD                                      00275000
797805            OR WS-ECR-NON-PAR                                     00275100
797806               CONTINUE                                           00275200
797807            ELSE                                                  00275300
260600*                                                                 02606
260700*.... IRL83 (HIPAA ELECTRONIC CLAIM ROUTING PROCESS VALID FOR     02607
260800*     HANDLE DIRECT AND NON PAR CLAIMS ONLY)                      02608
260900*                                                                 02609
797812               PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT   00275800
797813               MOVE 'IRL83'           TO FM105-ERR-CD (WS-SUB1)   00275900
797814               MOVE '6'               TO CIAEI-ECR-IND
797815            END-IF                                                00276000
261400        END-IF                                                    02614
261600                                                                  02616
797818        IF (WS-ECR-PROC)     AND                                  00276300
797819           (FM105-REL-NUM < '094')                                00276400
797820            SET FM105-PROCESSING-PLAN-PAYER   TO TRUE             00276500
797821        END-IF                                                    00276600
368410*R14.5 E_63022 DSL BEGIN                                          37120000
368500        IF FM105-INTL-CD NOT = 'I'                                37130000
368510           IF (CIABP-DELIVERY-MEDICAID) AND                       37140000
368600              (NOT WS-NON-PAR-PROV)   AND                         37150000
368700              (CIABP-HOST-PAR-SITE)                               37160000
368800               MOVE '1' TO FM105-PLAN-PAYER-CD                    37170000
368900               MOVE '3' TO FM105-PMT-DISP-CD                      37180000
369000           END-IF                                                 37190000
369100           IF (CIABP-DELIVERY-MEDICAID) AND                       37200000
369200              (WS-NON-PAR-PROV)       AND                         37210000
369300              (CIABP-HOST-PAR-SITE)                               37220000
369400               MOVE '3' TO FM105-PMT-DISP-CD                      37230000
369500           END-IF                                                 37240000
797832        END-IF                                                    00277700
369520*R14.5 E_63022 DSL END                                            37260000
797833        IF   FM105-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE     00277800
797834        AND (FM105-REL-NUM < '094')                               00277900
797835            IF (WS-ECR-PROC AND FM105-835-IND = 'Y')              00278000
797836            OR (WS-ECR-HIPAA-PROC AND FM105-835-IND = 'N')        00278100
278214*                                                                 02825
278215*.... IRL1I (HIPAA ELECTRONIC CLAIM ROUTING PROCESS MUST HAVE     02826
278216*     835 INDICATOR OF 'Y' AND ELECTRONIC CLAIM ROUTING PROCESS   02827
278217*     NOT HAVE 835 INDICATOR OF 'Y')                              02828
278218*                                                                 02829
797842              PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT       00278700
797843              MOVE 'IRL1I'        TO FM105-ERR-CD (WS-SUB1)       00278800
797844              MOVE '7'            TO CIAEI-ECR-IND
278223           END-IF                                                 02834
797846        END-IF                                                    00279000
371000*                                                                 37410000
371010*R14.5 E_63022 DSL RETIRE IRL1J BEGIN                             37420000
371100*       IF  FM105-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE      37430000
371200*       AND WS-ECR-HIPAA-PROC                                     37440000
371300*           IF (FM105-PLAN-PAYER-CD = '1'                         37450000
371400*             AND FM105-PMT-DISP-CD = '3')                        37460000
371500*           OR (FM105-PLAN-PAYER-CD = '2'                         37470000
371600*             AND FM105-PMT-DISP-CD = '2')                        37480000
371700*              CONTINUE                                           37490000
371800*           ELSE                                                  37500000
278233*                                                                 02844
278234*.... IRL1J (HIPAA ELECTRONIC CLAIM ROUTING PROCESS MUST HAVE     02845
278235*     HOST PLAN PAY PROVIDER OR HOME PLAN PAY SUBSCRIBER)         02846
278236*                                                                 02847
372300*                PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT    37550000
372400*                MOVE 'IRL1J'        TO FM105-ERR-CD (WS-SUB1)    37560000
372500*                MOVE '6'            TO CIAEI-PLAN-PAYER-CD-IND   37570000
372600*                                       CIAEI-PMT-DISP-CD-IND     37580000
372700*           END-IF                                                37590000
372800*       END-IF                                                    37600000
372810*R14.5 E_63022 DSL RETIRE IRL1J END                               37610000
797885        IF  FM105-REL-NUM >= '103'                                00280900
797886        AND ((FM105-PGM-CD = '9' AND FM105-INTL-CD = 'I' AND      00281000
797887              CIAPP2-WRLD-WIDE-BNFT-IND = SPACE)                  00281100
797888            OR                                                    00281200
797889             (FM105-PGM-CD = 'A' AND FM105-INTL-CD = 'I' AND      00281300
797890             (FM105-DEL-MTD = '6' OR '8')   AND                   00281400
797900              CIAPP2-WRLD-WIDE-BNFT-IND = SPACE))                 00281500
470060            PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT      00324400
      ** R17.5 E_115778 CHANGED DESCRIPTION OF IRL3C                    38500000
      **                                                                38510000
      ** BCBS GLOBAL CORE BENEFITS NOT AVAILABLE FOR THIS ARRANGEMENT   38520000
      **                                                                38530000
470070            MOVE 'IRL3C'             TO FM105-ERR-CD (WS-SUB1)    00324500
225700        END-IF                                                    ITCP9102
713973     END-IF.                                                      04799
225900                                                                  ITCP9102
798204     IF WS-PROFESSIONAL                                           00282200
798205        IF ((WS-HOST-OVRIDE-ECR-PROC  OR                          00282300
798206             WS-ECR-PROC              OR                          00282400
798207             WS-ECR-HIPAA-PROC)       AND                         00282500
798208            (FM1A5-REL-NUM < '094'))                              00282600
798209        OR  CIABP-HOME-CNTL-SITE                                  00282700
798210        OR  (FM1A5-SER-NUM-JULIAN-DATE < '2000178' AND            00282710
798211             FM1A5-ORIGINAL-RESUBMISSION)                         00282800
798212        OR  (FM1A5-REF-NUM-JULIAN-DATE < '2000178' AND            00282900
798213            (FM1A5-ADJUSTMENT OR FM1A5-ADJUSTMENT-RESUBMISSION))  00283000
798214             CONTINUE                                             00283300
798215        ELSE                                                      00283400
798216           IF WS-INTL-PAY-SUB                                     00283500
798217              IF FM1A5-REL-NUM < '094'                            00283600
798218                 MOVE WS-Y           TO FM1A5-ECR-IND             00283700
798219                                        WS-ECR-IND                00283800
798220              ELSE                                                00283900
798221                 MOVE SPACE          TO FM1A5-ECR-IND             00284000
798222                                        WS-ECR-IND                00284100
798223              END-IF                                              00284200
482000           ELSE                                                   04795
798225              IF (WS-ECR-PGM-CD OR WS-ECR-NON-PAR)                00284400
798226                 IF  FM1A5-SER-NUM-JULIAN-DATE >=                 00284500
798227                     CDBP20-835-MOD-DATE                          00284600
798228                 AND FM1A5-835-IND = 'Y'                          00284700
798229                     IF FM1A5-REL-NUM < '094'                     00284800
798230                        MOVE WS-H TO FM1A5-ECR-IND                00284900
798231                                     WS-ECR-IND                   00285000
798232                     END-IF                                       00285100
466500                 ELSE                                             04640
798234                    IF FM1A5-REL-NUM < '094'                      00285300
798235                       MOVE WS-Y     TO FM1A5-ECR-IND             00285400
798236                                        WS-ECR-IND                00285500
798237                    ELSE                                          00285600
798238                       MOVE SPACE    TO FM1A5-ECR-IND             00285700
798239                                        WS-ECR-IND                00285800
798240                    END-IF                                        00285900
466500                 END-IF                                           04640
798242                 PERFORM 1184-REVIEW-NEW-CRITERIA-PROF            00286100
798243                            THRU 1184-EXIT                        00286200
482500              ELSE                                                04800
798245                 CONTINUE                                         00286400
483900              END-IF                                              04814
484000           END-IF                                                 04815
484100        END-IF                                                    04816
484200                                                                  04817
233300        IF  (FM1A5-ADJUSTMENT OR FM1A5-ADJUSTMENT-RESUBMISSION)   02333
233400        AND FM1A5-CROSS-REF-SCCF-NUM NOT NUMERIC                  02334
798270           MOVE '2'  TO CIAEP-CROSS-REF-SCCF-NUM-IND
233700        END-IF                                                    02337
233800                                                                  02338
233900        IF (WS-HOST-OVRIDE-ECR-PROC OR WS-STD-BCP-PROC)           02339
234000           MOVE ZEROES               TO FM1A5-ADMIN-EXP-ALLW      02340
234100        END-IF                                                    02341
234200                                                                  02342
798286        IF FM1A5-SER-NUM-JULIAN-DATE > '2000177'                  00287900
234500           IF  WS-ECR-IND-VALID-VALUES                            02345
274500           OR  (FM1A5-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE  02863
274600              AND WS-ECR-IND-VALID-VALUES-R85)                    02864
234600              CONTINUE                                            02346
227000           ELSE                                                   ITCP9102
234800*                                                                 02348
234900*.... PE015 (ELECTRONIC CLAIM ROUTING INDICATOR IF PRESENT MUST   02349
275010*     BE 'Y', 'N' OR ('H' WHEN SCCF JULIAN DATE >= PDB 835        02869
275020*     MOD DATE))                                                  02870
235100*                                                                 02351
235200              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02352
235400              MOVE 'PE015'           TO FM1A5-ERR-CD (WS-SUB1)    02354
798299              MOVE '2'               TO CIAEP-ECR-IND
227200           END-IF                                                 ITCP9102
227300        END-IF                                                    ITCP9102
227400                                                                  ITCP9102
276030        IF FM1A5-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE       02881
276040           IF FM1A5-835-IND = ('Y' OR 'N')                        02882
276050              CONTINUE                                            02883
276060           ELSE                                                   02884
276070*                                                                 02885
276080*.... PE019 (835 INDICATOR MUST BE 'Y' OR 'N' WHEN SCCF SERIAL    02886
276090*     NUMBER JULIAN DATE >= PDB 835 MOD DATE)                     02887
276091*                                                                 02888
276092              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02889
276094              MOVE 'PE019'           TO FM1A5-ERR-CD (WS-SUB1)    02891
798313              MOVE '2'               TO CIAEP-835-IND
276095           END-IF                                                 02892
276096        ELSE                                                      02893
276097           MOVE SPACES               TO FM105-835-IND             02894
276098        END-IF                                                    02895
276100                                                                  02897
798319        IF WS-ECR-PROC                                            00291100
236000           IF WS-ECR-PGM-CD                                       02360
236100           OR (WS-ECR-NON-PAR AND WS-NOT-INTERNATIONAL)           02361
236200           OR (WS-INTL-PAY-SUB AND WS-INTERNATIONAL)              02362
236300              CONTINUE                                            02363
236400           ELSE                                                   02364
236500*                                                                 02365
236600*.... PRL69 (ELECTRONIC CLAIM ROUTING PROCESS VALID FOR HANDLE    02366
236700*     DIRECT, NON PAR AND INTL PAY SUB CLAIMS ONLY)               02367
236800*                                                                 02368
236900              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02369
237100              MOVE 'PRL69'           TO FM1A5-ERR-CD (WS-SUB1)    02371
798331              MOVE '4'               TO CIAEP-ECR-IND
237200           END-IF                                                 02372
227700        END-IF                                                    ITCP9102
227800                                                                  ITCP9102
237500        IF  WS-ECR-IND-VALID-VALUES                               02375
237600        AND WS-HOST-OVRIDE-ECR-PROC                               02376
237700           IF WS-ECR-NON-PAR                                      02377
237800           OR WS-INTL-PAY-SUB                                     02378
237900              CONTINUE                                            02379
238000           ELSE                                                   02380
238100*                                                                 02381
238200*.... PRL70 (HOST CAN SET ECR INDICATOR TO 'N' ONLY FOR NON PAR   02382
238300*     AND INTL PAY SUB CLAIMS)                                    02383
238400*                                                                 02384
238500              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02385
238700              MOVE 'PRL70'           TO FM1A5-ERR-CD (WS-SUB1)    02387
798347              MOVE '5'               TO CIAEP-ECR-IND
238800           END-IF                                                 02388
238900        END-IF                                                    02389
239000                                                                  02390
277500        IF  WS-ECR-IND-VALID-VALUES-R85                           02775
277600        AND WS-ECR-HIPAA-PROC                                     02776
484289        AND NOT WS-INTERNATIONAL                                  00496000
277700           IF WS-ECR-PGM-CD                                       02777
798355           OR WS-ECR-NON-PAR                                      00294500
277900              CONTINUE                                            02779
278000           ELSE                                                   02780
278100*                                                                 02781
278200*.... PRL79 (HIPAA ELECTRONIC CLAIM ROUTING PROCESS VALID FOR     02782
278300*     HANDLE DIRECT AND NON PAR CLAIMS ONLY)                      02783
278400*                                                                 02784
278500              PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT    02785
278700              MOVE 'PRL79'           TO FM1A5-ERR-CD (WS-SUB1)    02787
798364              MOVE '6'               TO CIAEP-ECR-IND
278800           END-IF                                                 02788
278900        END-IF                                                    02789
798367        IF (WS-ECR-PROC)     AND                                  00295600
798368           (FM1A5-REL-NUM < '094')                                00295700
798369            SET FM1A5-PROCESSING-PLAN-PAYER   TO TRUE             00295800
798370        END-IF                                                    00295900
388210*R14.5 E_63022 DSL BEGIN                                          39150000
388220        IF  FM1A5-INTL-CD NOT = 'I'                               39160000
388300           IF (CIABP-DELIVERY-MEDICAID) AND                       39170000
388400              (NOT WS-NON-PAR-PROV)   AND                         39180000
388500              (CIABP-HOST-PAR-SITE)                               39190000
388600               MOVE '1' TO FM1A5-PLAN-PAYER-CD                    39200000
388700               MOVE '3' TO FM1A5-PMT-DISP-CD                      39210000
388800           END-IF                                                 39220000
388900           IF (CIABP-DELIVERY-MEDICAID) AND                       39230000
389000              (WS-NON-PAR-PROV)       AND                         39240000
389100              (CIABP-HOST-PAR-SITE)                               39250000
389200               MOVE '3' TO FM1A5-PMT-DISP-CD                      39260000
389300           END-IF                                                 39270000
799200        END-IF                                                    00297000
389320*R14.5 E_63022 DSL END                                            39290000
241500                                                                  02415
799400        IF  FM1A5-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE      00297200
799500        AND (FM105-REL-NUM < '094')                               00297300
799600           IF (WS-ECR-PROC AND FM1A5-835-IND = 'Y')               00297400
799700           OR (WS-ECR-HIPAA-PROC AND FM1A5-835-IND = 'N')         00297500
299360*                                                                 03071
299370*.... PRL55 (HIPAA ELECTRONIC CLAIM ROUTING PROCESS MUST HAVE     03072
299380*     835 INDICATOR OF 'Y' AND ELECTRONIC CLAIM ROUTING PROCESS   03073
299390*     NOT HAVE 835 INDICATOR OF 'Y')                              03074
299391*                                                                 03075
800300              PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT       00298100
800400              MOVE 'PRL55'        TO FM1A5-ERR-CD (WS-SUB1)       00298200
800500              MOVE '7'               TO CIAEP-ECR-IND
299396           END-IF                                                 03080
800700        END-IF                                                    00298400
299397                                                                  03081
390910*R14.5 E_63022 DSL RETIRE PRL76 BEGIN                             39460000
391000*       IF  FM1A5-SER-NUM-JULIAN-DATE >= CDBP20-835-MOD-DATE      39470000
391100*       AND WS-ECR-HIPAA-PROC                                     39480000
391200*          IF (FM1A5-PLAN-PAYER-CD = '1'                          39490000
391300*             AND FM1A5-PMT-DISP-CD = '3')                        39500000
391400*          OR (FM1A5-PLAN-PAYER-CD = '2'                          39510000
391500*             AND FM1A5-PMT-DISP-CD = '2')                        39520000
391600*             CONTINUE                                            39530000
391700*          ELSE                                                   39540000
299406*                                                                 03090
299407*.... PRL76 (HIPAA ELECTRONIC CLAIM ROUTING PROCESS MUST HAVE     03091
299408*     HOST PLAN PAY PROVIDER OR HOME PLAN PAY SUBSCRIBER)         03092
299409*                                                                 03093
392200*             PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT       39590000
392300*             MOVE 'PRL76'        TO FM1A5-ERR-CD (WS-SUB1)       39600000
392400*             MOVE '6'            TO CIAEP-PLAN-PAYER-CD-IND      39610000
392500*                                    CIAEP-PMT-DISP-CD-IND        39620000
392600*          END-IF                                                 39630000
392700*       END-IF                                                    39640000
392710*R14.5 E_63022 DSL RETIRE PRL76 END                               39650000
802800        IF    FM1A5-REL-NUM >= '103'                              00300300
802900        AND ((FM1A5-PGM-CD = '9' AND FM1A5-INTL-CD = 'I' AND      00300400
803000              CIAPP2-WRLD-WIDE-BNFT-IND = SPACE)                  00300500
803100            OR                                                    00300600
803200             (FM1A5-PGM-CD = 'A' AND FM1A5-INTL-CD = 'I' AND      00300700
803300             (FM1A5-DEL-MTD = '6' OR '8')   AND                   00300800
803400              CIAPP2-WRLD-WIDE-BNFT-IND = SPACE))                 00300900
488370            PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT      00324400
      ** R17.5 E_115778 CHANGED DESCRIPTION OF PRL2R                    40570000
      **                                                                40580000
      ** BCBS GLOBAL CORE BENEFITS NOT AVAILABLE FOR THIS ARRANGEMENT   40590000
      **                                                                40600000
488380            MOVE 'PRL2R'             TO FM1A5-ERR-CD (WS-SUB1)    00324500
231300        END-IF                                                    ITCP9102
803702     END-IF.                                                      00301400
231600                                                                  ITCP9102
394200 1180-EXIT. EXIT.                                                 40650000
488800/                                                                 00503700
484800 1183-REVIEW-NEW-CRITERIA-INST.                                   04823
484800                                                                  04823
484800     IF  FM105-REL-NUM > '093'                                    04823
484800         IF    WS-BCP-PGM-CD                                      04823
803710         AND  (FM105-BCP-PROD-TYPE NOT  = 'P')                    00302200
803711         AND   WS-CLASS-PROV-NONPAR-IND = 'Y'                     00302300
484800               MOVE SPACES TO FM105-ECR-IND                       04823
484800                              WS-ECR-IND                          04823
484800         ELSE                                                     04823
484800              IF WS-ECR-PGM-CD                                    04823
484800                 MOVE 'H' TO FM105-ECR-IND                        04823
484800                             WS-ECR-IND                           04823
484800              END-IF                                              04823
803719         END-IF                                                   00303100
803720     END-IF.                                                      00303200
484800                                                                  04823
396000 1183-EXIT. EXIT.                                                 40830000
484800                                                                  04823
484800 1184-REVIEW-NEW-CRITERIA-PROF.                                   04823
484800                                                                  04823
484800     IF  FM1A5-REL-NUM > '093'                                    04823
484800         IF    WS-BCP-PGM-CD                                      04823
803728         AND  (FM1A5-BCP-PROD-TYPE  NOT = 'P')                    00304000
803729         AND   WS-CLASS-PROV-NONPAR-IND = 'Y'                     00304100
484800               MOVE SPACES TO FM1A5-ECR-IND                       04823
484800                              WS-ECR-IND                          04823
484800         ELSE                                                     04823
484800              IF WS-ECR-PGM-CD                                    04823
484800                 MOVE 'H' TO FM1A5-ECR-IND                        04823
484800                             WS-ECR-IND                           04823
484800              END-IF                                              04823
484800         END-IF                                                   04823
484800     END-IF.                                                      04823
161500                                                                  ITCP9102
397800 1184-EXIT. EXIT.                                                 41010000
803741                                                                  00305300
161600/                                                                 ITCP9102
161700 1200-SF-DUPLICATE-CHK.                                           ITCP9102
162500                                                                  ITCP9102
803745     MOVE CIASC-SCCF-SERIAL-NUM(1) TO CIADBK-SCCF-SERIAL-NUMBER.  00305700
803746                                                                  00305700
803747     PERFORM 1210-GET-FPCT-ROW.                                   00305800
803748                                                                  00305900
803749     IF CIABP-EOJ                                                 00306000
803750        GO TO 1200-EXIT.                                          00306100
803751                                                                  00306200
803752     MOVE WS-TYPE-SUBM            TO WS-TBL-KEY-SF-TYPE.          00306300
803753     IF CSAPI-NOT-FOUND                                           00306400
803754        MOVE WS-N                 TO WS-TBL-KEY-FDB-TYPE          00306500
803755                                     WS-TBL-KEY-FDB-STAT          00306600
803756        PERFORM 1215-FDB-TBL-LOOKUP    THRU 1215-EXIT             00306700
803757     ELSE                                                         00306800
803758        IF (WS-PROFESSIONAL   AND                                 00306900
803759            FM1A5-CLM-TYPE    =  FCT-CLM-TP-CD)                   00307000
803760        OR (WS-INSTITUTIONAL  AND                                 00307100
803761            FM105-CLM-TYPE    =  FCT-CLM-TP-CD)                   00307200
803762            PERFORM 1260-GET-SF-FIELDS     THRU 1260-EXIT         00307300
803763            IF CIABP-EOJ                                          00307700
803764               GO TO 1200-EXIT                                    00307800
803765            ELSE                                                  00307900
803766               MOVE F01-SUBM-TP-CD    TO WS-TBL-KEY-FDB-TYPE      00307400
803767               MOVE F01-STS-CD        TO WS-TBL-KEY-FDB-STAT      00307500
803768               MOVE F01-RLSE-NUM-ID   TO WS-FDB-REL-NUM           00307600
803769               PERFORM 1215-FDB-TBL-LOOKUP THRU 1215-EXIT         00308000
803770            END-IF                                                00308100
164000        ELSE                                                      ITCP9102
803772            PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT         00308300
803773            IF WS-INSTITUTIONAL                                   00308400
803774               MOVE 'IXR50'     TO FM105-ERR-CD (WS-SUB1)         00308500
803775               MOVE 'D'         TO FM105-STAT-CD                  00308600
803776               MOVE 'I'         TO CIABP-STAT-CD
803777            ELSE                                                  00308700
803778               MOVE 'PXR50'     TO FM1A5-ERR-CD (WS-SUB1)         00308800
803779               MOVE 'D'         TO FM1A5-STAT-CD                  00308900
803780               MOVE 'I'         TO CIABP-STAT-CD
803781            END-IF                                                00309000
164300        END-IF                                                    ITCP9102
803783     END-IF.                                                      00309200
164800                                                                  ITCP9102
164900 1200-EXIT.  EXIT.                                                ITCP9102
165000                                                                  ITCP9102
803787 1205-CALL-PART-KEY-NUM.                                          00309600
165300                                                                  ITCP9102
803790     SET WS-PARTKEY                 TO TRUE.                      00309900
803791     CALL WS-CALL-MODULE         USING PART-ALGO-PASS-AREA        00310000
803792                                       WS-FDB-PASSING-AREA.       00310100
165900                                                                  ITCP9102
803797 1205-EXIT.  EXIT.                                                00310600
166500                                                                  ITCP9102
803799 1210-GET-FPCT-ROW.                                               00310800
166800                                                                  ITCP9102
803801     INITIALIZE FCT-DATA-AREA.                                    00311000
803802     MOVE CIART-BUS-OWNER-ID-FMT    TO FCT-BUS-OWNER-ID.          00311100
803803     MOVE CIADBK-SCCF-SERIAL-NUMBER TO FCT-SCCF-ID.               00311200
803804     MOVE CIADBK-FORMATS-DB-NUMBER  TO FCT-PART-KEY-NUM.          00311300
803805     MOVE 'SELECT'                  TO CSAPI-COMMAND              00311500
803806     MOVE  1                        TO CSAPI-SUB-COMMAND          00311600
803807     SET WS-ITPFCTU1                TO TRUE.                      00311400
803808     CALL WS-CALL-MODULE         USING WS-ITCCSAPI                00311700
803809                                       FCT-DATA-AREA.             00311800
167300                                                                  ITCP9102
810520** PD_57333 REL 11.0.2 EXL 04/26/2010    START                    00338000
810530     IF CSAPI-RETURN-CODE = 100                                   00312000
803812        NEXT SENTENCE                                             00312100
803813     ELSE                                                         00312200
810542     IF CSAPI-RETURN-CODE =   0                                   00312000
810543        IF FCT-SF-ITCFM1A5-NUM  > 0                               00338000
810544        OR FCT-SF-ITCFM105-NUM  > 0                               00338000
810545           NEXT SENTENCE                                          00338000
810546        ELSE                                                      00338000
810547           MOVE 100      TO CSAPI-RETURN-CODE                     00312000
810548        END-IF
810549** PD_57333 REL 11.0.2 EXL 04/26/2010     END                     00338000
810551     ELSE                                                         00312200
803814        MOVE CWSCC-READ-ERROR         TO SIAPC-RETURN-STATUS      00312300
803815        MOVE '1210- BAD READ FPCT'    TO SIAPC-ERROR-MESSAGE      00312400
345200        MOVE 'SF014'            TO SIAPC-RETURN-MESSAGE-CD        34520000
345300                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)34530000
803817        PERFORM 9990-ERROR-DISPLAY.                               00312600
168900                                                                  ITCP9102
175800 1210-EXIT.  EXIT.                                                ITCP9102
176000/                                                                 ITCP9102
176100 1215-FDB-TBL-LOOKUP.                                             ITCP9102
170700                                                                  ITCP9102
803823     SET WS-FDB-INX                  TO 1.                        00313200
176400     SEARCH WS-FDB-ENTRY VARYING WS-FDB-INX                       ITCP9102
176500        AT END                                                    ITCP9102
176600           MOVE SPACE                TO WS-TBL-RESULT             ITCP9102
176700        WHEN WS-SF-TYPE-SUBM(WS-FDB-INX) = WS-TBL-KEY-SF-TYPE     ITCP9102
176800        AND WS-FDB-TYPE-SUBM(WS-FDB-INX) = WS-TBL-KEY-FDB-TYPE    ITCP9102
176900        AND WS-FDB-STAT-CD(WS-FDB-INX) = WS-TBL-KEY-FDB-STAT      ITCP9102
803830           IF CIABP-HOST-PAR-SITE                                 00313900
177100              MOVE WS-HOST-RESULT(WS-FDB-INX)                     ITCP9102
177200                TO WS-TBL-RESULT                                  ITCP9102
177300           ELSE                                                   ITCP9102
177400              MOVE WS-HOME-RESULT(WS-FDB-INX)                     ITCP9102
177500                TO WS-TBL-RESULT                                  ITCP9102
177600           END-IF                                                 ITCP9102
803837     END-SEARCH.                                                  00314600
180900                                                                  ITCP9102
181100     IF WS-TBL-RESULT = WS-X                                      ITCP9102
181200        IF  WS-SF-REL-NUM = '080'                                 ITCP9102
181300        AND WS-FDB-REL-NUM < '080'                                ITCP9102
181400           MOVE WS-V                 TO WS-TBL-RESULT             ITCP9102
181500        ELSE                                                      ITCP9102
803844           MOVE 'D'                  TO WS-TBL-RESULT             00315300
181700        END-IF                                                    ITCP9102
803846     END-IF.                                                      00315500
177800                                                                  ITCP9102
803848     IF WS-TBL-RESULT = 'D' OR WS-I                               00315700
803849        PERFORM 9960-FIND-ERROR-SLOT           THRU 9960-EXIT.    00315800
178000                                                                  ITCP9102
178100     IF WS-INSTITUTIONAL                                          ITCP9102
803852        IF WS-TBL-RESULT = 'D'                                    00316100
178300           MOVE 'IXR50'              TO FM105-ERR-CD (WS-SUB1)    ITCP9102
803854           MOVE 'D'                  TO FM105-STAT-CD             00316300
803855           MOVE 'I'                  TO CIABP-STAT-CD
178700        ELSE                                                      ITCP9102
178800           IF WS-TBL-RESULT = WS-I                                ITCP9102
178900              MOVE 'IXR52'           TO FM105-ERR-CD (WS-SUB1)    ITCP9102
803859              MOVE 'D'               TO FM105-STAT-CD             00316700
803860              MOVE 'I'               TO CIABP-STAT-CD
171400           ELSE                                                   ITCP9102
803862              CONTINUE                                            00316900
172300           END-IF                                                 ITCP9102
179600        END-IF                                                    ITCP9102
179700     ELSE                                                         ITCP9102
803866        IF WS-TBL-RESULT = 'D'                                    00317300
179900           MOVE 'PXR50'              TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
803868           MOVE 'D'                  TO FM1A5-STAT-CD             00317500
803869           MOVE 'I'                  TO CIABP-STAT-CD
172400        ELSE                                                      ITCP9102
180400           IF WS-TBL-RESULT = WS-I                                ITCP9102
180500              MOVE 'PXR52'           TO FM1A5-ERR-CD (WS-SUB1)    ITCP9102
803873              MOVE 'D'               TO FM1A5-STAT-CD             00317900
803874              MOVE 'I'               TO CIABP-STAT-CD
173000           ELSE                                                   ITCP9102
803876              CONTINUE                                            00318100
173900           END-IF                                                 ITCP9102
174000        END-IF                                                    ITCP9102
174100     END-IF                                                       ITCP9102
174200     .                                                            ITCP9102
174300                                                                  ITCP9102
181600 1215-EXIT.  EXIT.                                                ITCP9102
174500                                                                  ITCP9102
174600/                                                                 ITCP9102
174700 1220-FDB-RESUB-ADJ-CHK.                                          ITCP9102
175400                                                                  ITCP9102
803903     IF  WS-TYPE-SUBM  = '2' OR '4'                               00319200
803904         PERFORM 1230-FDB-RESUB-CHK             THRU 1230-EXIT    00319300
803905     END-IF.                                                      00319400
175900                                                                  ITCP9102
803907     IF  WS-BYPASS-EDITS-IND = 'N'                                00319600
803908     AND (WS-TYPE-SUBM = '3' OR '4')                              00319700
803909         PERFORM 1240-FDB-ADJ-CHK               THRU 1240-EXIT    00319800
803910     END-IF.                                                      00319900
176400                                                                  ITCP9102
176500 1220-EXIT.  EXIT.                                                ITCP9102
176600                                                                  ITCP9102
327500/                                                                 04726
176800 1230-FDB-RESUB-CHK.                                              ITCP9102
176900                                                                  ITCP9102
803917     IF WS-TYPE-SUBM  = '2'                                       00320600
714458        IF WS-INSTITUTIONAL                                       05332
803919           IF F01-ECRP-IND     = FM105-ECR-IND                    00320800
803920           OR F01-ECRP-IND     = 'N'                              00320900
513000              CONTINUE                                            05105
513000           ELSE                                                   05105
803923              IF  (F01-ECRP-IND = 'H')                            00321200
513000              AND (FM105-ECR-IND = SPACES)                        05105
513000              AND  WS-BCP-PGM-CD                                  05105
803926              AND  WS-CLASS-PROV-NONPAR-IND = 'Y'                 00321500
513000                   CONTINUE                                       05105
513000              ELSE                                                05105
803929                   PERFORM 9960-FIND-ERROR-SLOT THRU  9960-EXIT   00321800
803930                   MOVE 'IXR55'       TO FM105-ERR-CD (WS-SUB1)   00321900
803940                   MOVE 'D'           TO FM105-STAT-CD            00322000
803941                   MOVE 'I'           TO CIABP-STAT-CD
803942                   MOVE 'Y'           TO WS-BYPASS-EDITS-IND      00323900
803943                   GO TO 1230-EXIT                                00322200
510000              END-IF                                              05075
714463           END-IF                                                 05336
714464        ELSE                                                      05337
803947           IF F01-ECRP-IND  = FM1A5-ECR-IND                       00322600
803948           OR F01-ECRP-IND  = 'N'                                 00322700
511100              CONTINUE                                            05086
511100           ELSE                                                   05086
803951              IF  (F01-ECRP-IND  = 'H')                           00323000
511100              AND (FM1A5-ECR-IND = SPACES)                        05086
511100              AND  WS-BCP-PGM-CD                                  05086
803954              AND  WS-CLASS-PROV-NONPAR-IND = 'Y'                 00323300
511100                   CONTINUE                                       05086
511100              ELSE                                                05086
803957                   PERFORM 9960-FIND-ERROR-SLOT THRU  9960-EXIT   00323600
803958                   MOVE 'PXR55'       TO FM1A5-ERR-CD (WS-SUB1)   00323700
803959                   MOVE 'D'           TO FM105-STAT-CD            00323800
803960                   MOVE 'Y'           TO WS-BYPASS-EDITS-IND      00323900
803961                   GO TO 1230-EXIT                                00324000
510500              END-IF                                              05080
714468           END-IF                                                 05341
714469        END-IF                                                    05342
803965     END-IF.                                                      00324400
177200                                                                  ITCP9102
409000*R13.0.1 PD_82220 DSL ISSUE ERROR ONLY IF TIMESTAMP NOT = 9999    35950000
409001     IF FCT-DF-ITCFM21A-NUM > 0 AND                               35950000
409010        FCT-PTS-LDLA-DF-TS NOT = '9999-12-31-00.00.00.000000'     35950000
803968        SET CIABP-DUP-XR53-FOUND TO TRUE                          00324700
803969        PERFORM 9960-FIND-ERROR-SLOT      THRU 9960-EXIT          00324800
803970        IF WS-INSTITUTIONAL                                       00324900
803971           MOVE 'IXR53'          TO FM105-ERR-CD (WS-SUB1)        00325000
803972           MOVE 'D'              TO FM105-STAT-CD                 00325100
177600        ELSE                                                      ITCP9102
803974           MOVE 'PXR53'          TO FM1A5-ERR-CD (WS-SUB1)        00325300
803975           MOVE 'D'              TO FM1A5-STAT-CD                 00325400
178800        END-IF                                                    ITCP9102
803977        MOVE 'I'                 TO CIABP-STAT-CD
803978        MOVE 'Y'                 TO WS-BYPASS-EDITS-IND           00323900
389300        GO TO 1230-EXIT                                           01410000
803979     END-IF.                                                      00325700
389500                                                                  36080000
389600*R11.4 E_38714 DSL BEGIN                                          44520000
389700     MOVE 2     TO WS-B2-SUB-COMM.                                44230000
389800     MOVE 'Q'   TO WS-B2-REQUEST.                                 44230000
389900     MOVE 'Y'   TO WS-B2-RESUB.                                   44230000
390000     MOVE SPACE TO WS-B2-ACTION.                                  44230000
422700                                                                  42650000
422800*** R14.5 E_34072  EXL    START                                   42660000
422900     MOVE 'GENINQ'                 TO WS-B2-MSG-TP-CD.            42670000
423000     MOVE 'PRSD'                   TO WS-B2-MSG-STS-CD.           42680000
423100     MOVE CIASC-SCCF-SERIAL-NUM(1) TO CSAPI-SCCF-SER-NUM.         42690000
423200*** R14.5 E_34072  EXL    END                                     42700000
423300                                                                  42710000
390100     PERFORM 2900-READ-B2-MSG       THRU 2900-EXIT.               48250000
390200                                                                  44300000
390300     EVALUATE CSAPI-RETURN-CODE                                   44300000
390400     WHEN 0                                                       44300000
390500        GO TO 1230-EXIT                                           44300000
390600                                                                  44300000
390700     WHEN +100                                                    44300000
390800       MOVE 3              TO WS-B2-SUB-COMM                      44230000
390900       MOVE 'R'            TO WS-B2-REQUEST                       44230000
391000       MOVE SPACE          TO WS-B2-RESUB                         44230000
391100       MOVE '124'          TO WS-B2-ACTION                        44230000
424500*** R14.5 E_34072  EXL    START                                   42830000
424600       MOVE 'GENINQ'       TO WS-B2-MSG-TP-CD                     42840000
424700       MOVE 'PRSD'         TO WS-B2-MSG-STS-CD                    42850000
424800       MOVE CIASC-SCCF-SERIAL-NUM(1) TO CSAPI-SCCF-SER-NUM        42860000
424900*** R14.5 E_34072  EXL    END                                     42870000
391200       PERFORM 2900-READ-B2-MSG     THRU 2900-EXIT                37010000
391300       EVALUATE CSAPI-RETURN-CODE                                 44300000
391400         WHEN +0                                                  44300000
391500           CONTINUE                                               44300000
391600         WHEN +100                                                44300000
391610            PERFORM 9960-FIND-ERROR-SLOT THRU         9960-EXIT   35660000
391700            IF WS-INSTITUTIONAL                                   34130000
391800               MOVE 'IXR56'     TO FM105-ERR-CD (WS-SUB1)         34140000
391900               MOVE 'I'         TO FM105-STAT-CD                  34150000
392000            ELSE                                                  34130000
392100               MOVE 'PXR56'     TO FM1A5-ERR-CD (WS-SUB1)         34140000
392200               MOVE 'I'         TO FM1A5-STAT-CD                  34150000
392300            END-IF                                                34130000
392400            MOVE 'I'            TO CIABP-STAT-CD                  34160000
392500            MOVE 'Y'            TO WS-BYPASS-EDITS-IND            01410000
392600         WHEN OTHER                                               37020000
392700           PERFORM 2902-INSERT-IO-ERROR-MSG                       44300000
392800       END-EVALUATE                                               37030000
392900                                                                  36080000
393000     WHEN OTHER                                                   44300000
393100       PERFORM 2902-INSERT-IO-ERROR-MSG                           44300000
393200     END-EVALUATE.                                                44300000
179100                                                                  ITCP9102
179200 1230-EXIT.  EXIT.                                                ITCP9102
803982*                                                                 00326000
803983/**************************************************************   00326100
179500 1240-FDB-ADJ-CHK.                                                ITCP9102
179600                                                                  ITCP9102
414900*R13.0 E_60193 DSL BEGIN                                          36100000
415000     MOVE SPACE                        TO F51-DATA-AREA           37000000
415100                                          F01-DATA-AREA.          37000000
415200     IF WS-INSTITUTIONAL                                          37010000
415300        MOVE FM105-CROSS-REF-SCCF-NUM  TO F51-SCCF-ID             37020000
415400                                          F01-SCCF-ID             37020000
415500     ELSE                                                         37030000
415600        MOVE FM1A5-CROSS-REF-SCCF-NUM  TO F51-SCCF-ID             37040000
415700                                          F01-SCCF-ID             37020000
415800     END-IF.                                                      37050000
416000                                                                  36130000
416100     IF  F51-SCCF-ID       NOT NUMERIC                            36170000
416200     OR  F51-SCCF-ID         = ZEROES                             36180000
416300        PERFORM 1241-SET-XR54-ERROR          THRU 1241-EXIT       36140000
416400        GO TO 1240-EXIT                                           36160000
416500     END-IF.                                                      36170000
416600*R13.0 E_60193 END                                                36100000
429600                                                                  43340000
429601*R15.5 E_90497 DSN BEGIN - MOVED FROM PARA 1250-                  45030000
429602     MOVE F51-SCCF-ID                  TO PART-SCCF-ID            45040000
429603     PERFORM 1205-CALL-PART-KEY-NUM                               45050000
429604                                                                  45060000
429605     MOVE PART-KEY-NUM                     TO F51-PART-KEY-NUM    45070000
429606                                              F01-PART-KEY-NUM.   45080000
429608     MOVE CIART-BUS-OWNER-ID-FMT           TO F51-BUS-OWNER-ID.   45100000
429609*R15.5 E_90497 DSN END                                            45030000
429610                                                                  43340000
429611*R15.5 E_90497 DSL BEGIN                                          43330000
429612     IF WS-U200-1-IND = 'Y'                                       43690000
429614        PERFORM 1280-CHK-DF-MSG-CD    THRU 1280-EXIT
429615     END-IF
429616
429617     MOVE 3                            TO CSAPI-SUB-COMMAND.      45130000
429618     MOVE '00'                         TO F01-SCCF-ID (16:2).     43260000
429620*R15.5 E_90497 DSL END                                            43330000
416700                                                                  36130000
803995     PERFORM 1250-FDB-DF-READ                THRU 1250-EXIT.      00327300
180700                                                                  ITCP9102
361600     IF CIABP-EOJ                                                 36160000
361610        GO TO 1240-EXIT                                           36160000
361620     END-IF                                                       36160000
361630                                                                  36160000
417500     IF  CSAPI-RETURN-CODE   = 100                                36190000
362000     OR (CSAPI-RETURN-CODE      = 0    AND                        36200000
362100         F51-STS-CD        NOT  = 'V')                            36210000
417800         PERFORM 1241-SET-XR54-ERROR         THRU 1241-EXIT       36140000
363110         GO TO 1240-EXIT                                          36310000
363200     END-IF                                                       36320000
363210                                                                  36320000
363300     IF WS-INSTITUTIONAL                                           30000
363400        IF F51-ECRP-IND   = FM105-ECR-IND
363500           CONTINUE
363600        ELSE                                                      360000
363700           IF  F51-ECRP-IND   = 'H'                               370000
363800           AND FM105-ECR-IND  = SPACES                            380000
363900           AND WS-BCP-PGM-CD                                      390000
364000           AND WS-CLASS-PROV-NONPAR-IND = 'Y'                     400000
364100               CONTINUE                                           410000
181900           ELSE                                                   ITCP9102
364300               IF F51-ECRP-IND   = 'N'                            430000
364400                  CONTINUE                                        440000
364500               ELSE                                               450000
364600                  PERFORM 9960-FIND-ERROR-SLOT                    460000
364700                  MOVE 'IXR55'   TO FM105-ERR-CD (WS-SUB1)        470000
364800                  MOVE 'D'       TO FM105-STAT-CD                 480000
364900                  MOVE 'I'       TO CIABP-STAT-CD                 490000
365000               END-IF                                             500000
182100           END-IF                                                 ITCP9102
365200        END-IF                                                    520000
365300     ELSE                                                         530000
365400        IF F51-ECRP-IND  = FM1A5-ECR-IND                          540000
365500           CONTINUE                                               550000
182200        ELSE                                                      ITCP9102
365700           IF  F51-ECRP-IND   = 'H'                               570000
365800           AND FM1A5-ECR-IND  = SPACES                            580000
365900           AND WS-BCP-PGM-CD                                      590000
366000           AND WS-CLASS-PROV-NONPAR-IND = 'Y'                     600000
366100               CONTINUE                                           610000
714533           ELSE                                                   05404
366300              IF F51-ECRP-IND   = 'N'                             630000
804037                 CONTINUE                                         00331300
517500              ELSE                                                05150
366600                 PERFORM 9960-FIND-ERROR-SLOT                     660000
434500                 MOVE 'PXR55'    TO FM105-ERR-CD (WS-SUB1)        43820000
434600                 MOVE 'D'        TO FM105-STAT-CD                 43830000
366900                 MOVE 'I'        TO CIABP-STAT-CD                 690000
714537              END-IF                                              05408
714538           END-IF                                                 05409
367200        END-IF                                                    720000
435100     END-IF.                                                      43880000
435200*                                                                 43890000
435201*** R14.5 E_34072 EXL         START                               43900000
435202*R14.5.1 E_96652 DSL BEGIN REMOVE IRL8L                           43900000
435203*    IF WS-INSTITUTIONAL                                          43910000
435204*       IF  F51-INP-SUB-APFX-ID = FM130-SUB-ID-PFX-1-3            43920000
435205*       AND F51-INP-SUB-SUFX-ID = FM130-SUB-ID-SUFX-4-17          43930000
435206*           NEXT SENTENCE                                         43940000
435207*       ELSE                                                      43950000
435208*           PERFORM 9960-FIND-ERROR-SLOT                          43960000
435209*           MOVE 'IRL8L'   TO FM105-ERR-CD (WS-SUB1)              43970000
435210*           MOVE 'I'       TO FM105-STAT-CD                       43980000
435211*                             CIABP-STAT-CD                       43990000
435212*       END-IF                                                    44000000
435213*    ELSE                                                         44010000
435214*       IF  F51-INP-SUB-APFX-ID = FM1D0-SUB-ID-PFX-1-3            44020000
435215*       AND F51-INP-SUB-SUFX-ID = FM1D0-SUB-ID-SUFX-4-17          44030000
435216*           NEXT SENTENCE                                         44040000
435217*       ELSE                                                      44050000
435218*           PERFORM 9960-FIND-ERROR-SLOT                          44060000
435219*           MOVE 'PRL8L'    TO FM1A5-ERR-CD (WS-SUB1)             44070000
435220*           MOVE 'I'        TO FM1A5-STAT-CD                      44080000
435221*                              CIABP-STAT-CD                      44090000
435222*       END-IF                                                    44100000
435223*    END-IF.                                                      44110000
435224*** R14.5 E_34072 EXL         END                                 44120000
435225*R14.5.1 E_96652 DSL END                                          43900000
714540                                                                  05411
367500     IF F51-LOC-PLAN-RCPT-DT-NI = -1                              750000
367600        MOVE SPACE  TO CIABP-DF-LOC-PLAN-RCPT-DT                  760000
367700     ELSE                                                         770000
367800        MOVE F51-LOC-PLAN-RCPT-DT(1:2)                            780000
367900                    TO CIABP-DF-LOC-PLAN-RCPT-DT-CC               790000
368000        MOVE F51-LOC-PLAN-RCPT-DT(3:2)                            800000
368100                    TO CIABP-DF-LOC-PLAN-RCPT-DT-YY               810000
368200        MOVE F51-LOC-PLAN-RCPT-DT(6:2)                            820000
368300                    TO CIABP-DF-LOC-PLAN-RCPT-DT-MM               830000
368400        MOVE F51-LOC-PLAN-RCPT-DT(9:2)                             40000
368500                    TO CIABP-DF-LOC-PLAN-RCPT-DT-DD               850000
368510     END-IF                                                       770000
368520
368600     IF CIABP-HOST-PAR-SITE                                       860000
368700        IF WS-INSTITUTIONAL                                       870000
368800           MOVE F51-PRCSG-SITE-CNTL-ID                            880000
368900             TO FM105-PROC-SITE-CNTL-NUM                          890000
369000        ELSE                                                       00000
369100           MOVE F51-PRCSG-SITE-CNTL-ID                            910000
369200             TO FM1A5-PROC-SITE-CNTL-NUM                          920000
369300        END-IF                                                    930000
369400     END-IF.                                                      940000
369410                                                                   30000
369420     IF WS-INSTITUTIONAL                                           30000
369430        IF F51-CLM-TP-CD = FM105-CLM-TYPE                         910000
369440           CONTINUE                                               910000
369450        ELSE                                                       30000
369470***  IC047 - CLAIM TYPE CHANGE PROHIBITED                         670000
369471           MOVE '3'              TO CIAEI-CLM-TYPE-IND            670000
369480        END-IF                                                    680000
369490     ELSE                                                         690000
369491        IF F51-CLM-TP-CD = FM1A5-CLM-TYPE                         910000
369492           CONTINUE                                               910000
369493        ELSE                                                       30000
369495***  PC047 - CLAIM TYPE CHANGE PROHIBITED                         670000
369496           MOVE '3'              TO CIAEP-CLM-TYPE-IND            670000
369497        END-IF                                                    680000
369498     END-IF.                                                      690000
183700                                                                  ITCP9102
426400*R13.0 E_60193 DSL BEGIN                                          15130000
426500     IF FM1A5-REL-NUM > '120'                                     910000
426600       PERFORM 1270-GET-ORIG-SF              THRU 1270-EXIT       36140000
426700       IF WS-INSTITUTIONAL                                         30000
426800          IF F01-PLAN-PYR-CD NOT = FM105-PLAN-PAYER-CD            910000
426900*IRL5N - PLAN PAYER CODE MUST MATCH THE VALUE ON THE ORIGINAL SF  670000
427000             MOVE '7' TO CIAEI-PLAN-PAYER-CD-IND
427100          END-IF                                                  910000
427200       ELSE                                                        30000
427300          IF F01-PLAN-PYR-CD NOT = FM1A5-PLAN-PAYER-CD            910000
427400*PRLA8 - PLAN PAYER CODE MUST MATCH THE VALUE ON THE ORIGINAL SF  670000
427500             MOVE '7' TO CIAEP-PLAN-PAYER-CD-IND
427600          END-IF                                                  910000
427700       END-IF                                                      30000
427800     END-IF.                                                       30000
427900*R13.0 E_60193 END                                                15130000
428000                                                                  36950000
183800 1240-EXIT.  EXIT.                                                ITCP9102
804079***********************************************************       00335400
428300 1241-SET-XR54-ERROR.                                             36140000
428400     SET CIABP-DUP-XR54-FOUND        TO TRUE                      36220000
428500     PERFORM 9960-FIND-ERROR-SLOT             THRU 9960-EXIT      36230000
428600     IF WS-INSTITUTIONAL                                          36240000
428700        MOVE 'IXR54'                 TO FM105-ERR-CD (WS-SUB1)    36250000
428800        MOVE 'D'                     TO FM105-STAT-CD             36260000
428900     ELSE                                                         36270000
429000        MOVE 'PXR54'                 TO FM1A5-ERR-CD (WS-SUB1)    36280000
429100        MOVE 'D'                     TO FM1A5-STAT-CD             36290000
429200     END-IF                                                       36300000
429300     MOVE 'I'                        TO CIABP-STAT-CD.            36310000
429400                                                                  36310000
429500 1241-EXIT.    EXIT.                                              36140000
184100 1250-FDB-DF-READ.                                                ITCP9102
444200                                                                  45020000
444300*R15.5 E_90497 DSL BEGIN - TO PARA 1240-                          45020000
444400*    MOVE F51-SCCF-ID                  TO PART-SCCF-ID            45040000
444500*    PERFORM 1205-CALL-PART-KEY-NUM                               45050000
429700*                                                                 36990000
444700*    MOVE PART-KEY-NUM                     TO F51-PART-KEY-NUM    45070000
444800*                                             F01-PART-KEY-NUM.   45080000
445000*    MOVE CIART-BUS-OWNER-ID-FMT           TO F51-BUS-OWNER-ID.   45100000
445010*R15.5 E_90497 DSL END                                            45020000
184900                                                                  ITCP9102
804087     MOVE 'SELECT'                         TO CSAPI-COMMAND.      00336300
445300*    MOVE 3                                TO CSAPI-SUB-COMMAND.  45130000
804089     SET WS-ITDF51S1                       TO TRUE.               00336200
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    46830000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   46840000
804090     CALL WS-CALL-MODULE                USING WS-ITCCSAPI         00336500
804091                                              F51-DATA-AREA.      00336600
185200                                                                  ITCP9102
804093     IF CSAPI-RETURN-CODE             = 0 OR 100                  00336800
804094        NEXT SENTENCE                                             00336900
804095     ELSE                                                         00337000
804096        MOVE CWSCC-READ-ERROR          TO SIAPC-RETURN-STATUS     00337100
804097        MOVE '1250- BAD READ CLM_DISP' TO SIAPC-ERROR-MESSAGE     00337200
372900        MOVE 'SF015'            TO SIAPC-RETURN-MESSAGE-CD        37290000
373000                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)37300000
804099        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00337400
804100     END-IF.                                                      00337500
185800                                                                  ITCP9102
185900 1250-EXIT.  EXIT.                                                ITCP9102
804103***********************************************************       00337800
804104 1260-GET-SF-FIELDS.                                              00337900
186000                                                                  ITCP9102
804106     MOVE SPACE                            TO F01-DATA-AREA.      00338100
804107     MOVE CIART-BUS-OWNER-ID-FMT           TO F01-BUS-OWNER-ID.   00338200
804108     MOVE CIADBK-FORMATS-DB-NUMBER         TO F01-PART-KEY-NUM.   00338300
804109     MOVE CIADBK-SCCF-SERIAL-NUMBER        TO F01-SCCF-ID.        00338400
804110                                                                  00338500
804111     MOVE 'SELECT'                         TO CSAPI-COMMAND.      00338700
804112     MOVE 2                                TO CSAPI-SUB-COMMAND.  00338800
804113     SET WS-ITDF01S1                       TO TRUE.               00338600
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    47100000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   47110000
804114     CALL WS-CALL-MODULE                USING WS-ITCCSAPI         00338900
804115                                              F01-DATA-AREA.      00339000
804116                                                                  00339100
804117     IF CSAPI-SUCCESSFUL                                          00339200
804118        NEXT SENTENCE                                             00339300
804119     ELSE                                                         00339400
804120        MOVE CWSCC-READ-ERROR          TO SIAPC-RETURN-STATUS     00339500
804130        MOVE '1260- BAD READ CLM_SUBM' TO SIAPC-ERROR-MESSAGE     00339600
375400        MOVE 'SF016'            TO SIAPC-RETURN-MESSAGE-CD        37540000
375500                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)37550000
804132        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00339800
804133     END-IF.                                                      00339900
804134                                                                  00340000
804135 1260-EXIT.  EXIT.                                                00340100
804136**************************************************************    00340200
436600                                                                  37600000
436700*R13.0 E_60193 DSL BEGIN                                          36970000
436800 1270-GET-ORIG-SF.                                                37610000
436900     MOVE CIART-BUS-OWNER-ID-FMT           TO F01-BUS-OWNER-ID.   37160000
437000                                                                  37170000
437100     MOVE 'SELECT'                         TO CSAPI-COMMAND.      37180000
437200     MOVE 3                                TO CSAPI-SUB-COMMAND.  37190000
437300     SET WS-ITDF01S1                       TO TRUE.               37200000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    47350000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   47360000
437400     CALL WS-CALL-MODULE                USING WS-ITCCSAPI         37210000
437500                                              F01-DATA-AREA.      37220000
437600                                                                  37230000
437700     IF CSAPI-RETURN-CODE             = 0 OR 100                  37240000
437710*R13.5 E_79779 DSL BEGIN                                          36970000
437800        IF CSAPI-RETURN-CODE = 0                                  37250000
437801           IF (F01-LOC-PLAN-RCPT-DT-NI = -1)
437802              MOVE 0 TO WS-LOC-RCPT-DT
437803           ELSE
437810             MOVE F01-LOC-PLAN-RCPT-DT (1:4) TO WS-DATE-YY        37250000
437820             MOVE F01-LOC-PLAN-RCPT-DT (6:2) TO WS-DATE-MM        37250000
437830             MOVE F01-LOC-PLAN-RCPT-DT (9:2) TO WS-DATE-DD        37250000
437831           END-IF
437840        END-IF                                                    37250000
437850*R13.5 E_79779 DSL END                                            36970000
437900     ELSE                                                         37260000
438000        MOVE CWSCC-READ-ERROR          TO SIAPC-RETURN-STATUS     37270000
438100        MOVE '1270- BAD READ CLM_SUBM' TO SIAPC-ERROR-MESSAGE     37280000
438200        MOVE 'SF016'            TO SIAPC-RETURN-MESSAGE-CD        37290000
438300                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)37300000
438400        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       37310000
438500     END-IF.                                                      37320000
438600 1270-EXIT.   EXIT.                                               37610000
438700*R13.0 E_60193 END                                                36970000
452700                                                                  45870000
452710*R15.5 E_90497 DSL BEGIN                                          45560000
452720 1280-CHK-DF-MSG-CD.                                              45570000
452721     MOVE 2                                TO CSAPI-SUB-COMMAND.  45130000
452722     MOVE '1' TO F51-DISP-CD                                      45130000
452730     PERFORM 1250-FDB-DF-READ    THRU 1250-EXIT.                  45580000
452732                                                                  43360000
452733     IF CIABP-EOJ OR                                              43370000
452734        CSAPI-RETURN-CODE   = 100                                 43410000
      *R18.0 E_114613 TGW - CORRECTED GO TO STATEMENT                   47700000
              GO TO 1280-EXIT                                           47710000
452735*       GO TO 1240-EXIT                                           47720000
452736     END-IF.                                                      43390000
452737                                                                  43400000
452741     IF (F51-MSG-CD-1 = '1323' OR                                 43720000
452742         F51-MSG-CD-2 = '1323' OR                                 43720000
452743         F51-MSG-CD-3 = '1323' OR                                 43720000
452744         F51-MSG-CD-4 = '1323' OR                                 43720000
452745         F51-MSG-CD-5 = '1323')                                   43720000
452746         MOVE 'I'                TO CIABP-STAT-CD                 43840000
452747         PERFORM 9960-FIND-ERROR-SLOT                             43810000
452748        IF WS-INSTITUTIONAL                                       43480000
452749           MOVE 'IRL4V'          TO FM105-ERR-CD (WS-SUB1)        43820000
452750           MOVE 'D'              TO FM105-STAT-CD                 43830000
452751        ELSE                                                      43690000
452752           MOVE 'PRL4V'          TO FM1A5-ERR-CD (WS-SUB1)        43820000
452753           MOVE 'D'              TO FM1A5-STAT-CD                 43830000
452755        END-IF                                                    43850000
452756     END-IF.                                                      43850000
452757                                                                  45590000
452769 1280-EXIT.   EXIT.                                               45570000
452770*R15.5 E_90497 DSL END                                            45560000
438800                                                                  37610000
186200 1290-EDIT-MODULE-CHK.                                            ITCP9102
186300                                                                  ITCP9102
804139     IF  CDBP16-BATCH-CALL-PLN-MOD                                00340500
187200     AND (CIABP-PROV-DATA-YES OR CIABP-PRC-DATA-YES)              ITCP9102
187300        PERFORM 1300-PLAN-PRE-EDIT              THRU 1300-EXIT    ITCP9102
804142     END-IF.                                                      00340800
187500                                                                  ITCP9102
804144     PERFORM 9970-ERROR-CODE-CHK                THRU 9970-EXIT.   00341000
187700                                                                  ITCP9102
804146     IF CIABP-EOJ                                                 00341200
804147        GO TO 1290-EXIT.                                          00341300
440000                                                                  37720000
440001*R13.5 E_79779 DSL BEGIN                                          37930000
440010     IF WS-INSTITUTIONAL
440020        PERFORM 3000-CONVT-RCPT-DT              THRU 3000-EXIT.
440021*R13.5 E_79779 DSL END                                            37930000
804148                                                                  00341400
804149     MOVE 006.00 TO CIABP-CAPPED-ACCESS-FEE-PCT.                  00341500
804150                                                                  00341600
377500*R11.3 E_49671 DSL BEGIN                                          37930000
379400     IF CIAPP2-ACCESS-FEE-CD = ('1' OR '2' OR '4' OR '5')         37940000
379500       IF FM1A5-BCP-PROD-TYPE = 'N'                               37950000
379600       OR (FM1A5-BCP-PROD-TYPE = (SPACES OR 'P')                  37960000
379700         AND CIABP-DELIVERY-BCP-POS                               37970000
379800         AND FM1A5-PGM-CD = 'A'                                   37980000
379900         AND CIABP-MSG-1008-NOT-FOUND)                            37990000
380000             CONTINUE                                             38000000
380100       ELSE                                                       38010000
441200*R13.5 E_76693 DSL READ REC 21 FOR C/D/E/F/G/H                    38020000
441300        IF  CIAPP2-ACCT-TYPE-CD = ('C' OR 'D' OR 'E' OR 'F' OR    38020000
441310                                   'G' OR 'H')                    38020000
380210          PERFORM 0280-ACCESS-PDB-21-FOR-RATE THRU 0280-EXIT      160000
804500        ELSE                                                      00342400
380230           IF CIAPP2-ACCESS-FEE-CD = '5'                          38020000
380300             CONTINUE                                             38030000
380301           ELSE                                                   38020000
380310            IF CIAPP2-REDUCED-ACCT-TIERS                          38020000
380320            OR (FM1A5-REL-NUM >= '087'    AND                     38030000
380400                FM1A5-PGM-CD = 'A'        AND                     38040000
380500                CIABP-DELIVERY-PPO        AND                     38050000
380600                CIAPP2-NATIONAL-OOA-CD = '3')                     38060000
380700            OR (FM1A5-REL-NUM >= '093'    AND                     38070000
380800                FM1A5-PGM-CD = 'A'        AND                     38080000
380900                CIABP-DELIVERY-CN         AND                     38090000
381000                CIAPP2-NATIONAL-OOA-CD = '3')                     38100000
381100            OR (FM1A5-PGM-CD        = '9' AND                     38110000
381200                CIAPP2-ACCESS-FEE-CD = '1')                       38120000
381300            OR (CIAPP2-ACCESS-FEE-CD = ('1' OR '2' OR '4') AND
381400                CIAPP2-PROGRAM-CD = 'A'                     AND   38140000
381500                CIAPP2-NATIONAL-OOA-CD NOT = '3')                 38150000
381510            OR ((CIAPP2-DELIVERY-METHOD = 'J'               AND
381520                CIAPP2-ACCESS-FEE-CD     = '1'              AND
381530                CIAPP2-PROGRAM-CD        = 'A'              AND
381540                FM1A5-REL-NUM           >= '110')           AND
381550               (CIAPP2-ACCT-TYPE-CD      = 'B'   OR
381560                CIAPP2-NATIONAL-OOA-CD   = '3'   OR
381570                (CIAPP2-ACCT-TYPE-CD     = 'A'   AND
381580                 CIAPP2-NATIONAL-OOA-CD = '1')) )
381590*R11.3 E_49671 END                                                37930000
381600                PERFORM 0280-ACCESS-PDB-21-FOR-RATE               38160000
381610                                                 THRU 0280-EXIT
381700            END-IF                                                38170000
806100           END-IF                                                 00344000
806101        END-IF                                                    00344100
381801       END-IF                                                     38180000
806102     END-IF.                                                      00344200
338700                                                                  03842
338693     IF WS-INSTITUTIONAL                                          04864
338694        IF FM105-INTL-CD = 'I'                                    04865
338695           MOVE ZERO    TO FM150-ACCESS-FEE-PCT                   04866
338696        END-IF                                                    04867
338697     ELSE                                                         04868
338698        IF FM1A5-INTL-CD = 'I'                                    04869
338699           MOVE ZERO    TO FM1E0-ACCESS-FEE-PCT                   04870
338700        END-IF                                                    04871
806112     END-IF.                                                      00345200
338710                                                                  04874
187800     IF  CIABP-NOT-EOJ                                            ITCP9102
187900     AND WS-DONT-BYPASS-EDITS                                     ITCP9102
188000     AND CDBP16-CALL-PLN-EDIT-EXIT                                ITCP9102
188100        PERFORM 1400-PLAN-SPECIFIC-EDIT         THRU 1400-EXIT    ITCP9102
188200        PERFORM 9970-ERROR-CODE-CHK             THRU 9970-EXIT    ITCP9102
806119     END-IF.                                                      00345900
418601*R11.4.1 TD_73292 DSL BEGIN                                       35240000
418610     IF WS-PROFESSIONAL                                           35250000
418611*R11.4.1 TD_75487 DSL UPDATE COUNT OF F0 RECORDS                  35240000
418612        MOVE CIABP-F0-CTR     TO CIART-SERV-LN-CNT                35250000
418620        IF CIABP-E6-CTR NOT = CIART-NOTE-LN-CNT                   35250000
418630           MOVE CIABP-E6-CTR TO CIART-NOTE-LN-CNT                 35250000
418640           IF CIART-RT-IND = 'Y'                                  35250000
418642              PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL            35250000
418643                                       WS-SUB > CIABP-E6-CTR      35250000
418671*R11.4.P PD_77362 DSL BEGIN
418680              IF CIASC-E6-1-999-SLOT (WS-SUB) (26:1)     = 'N'    35250000
418690                 MOVE 'Y' TO CIART-SPEC-NOTE-IND                  35250000
447800*R11.4.P PD_77362 END
418648                 MOVE CIABP-E6-CTR TO WS-SUB                      35250000
418649              END-IF                                              35250000
418650              END-PERFORM                                         35250000
418652           END-IF                                                 35250000
418653        END-IF                                                    35250000
418660     ELSE                                                         35250000
418661*R11.4.1 TD_75487 DSL UPDATE COUNT OF 60 RECORDS                  35240000
418662        MOVE CIABP-60-CTR     TO CIART-SERV-LN-CNT                35250000
418670        IF CIABP-46-CTR NOT = CIART-NOTE-LN-CNT                   35250000
418680           MOVE CIABP-46-CTR TO CIART-NOTE-LN-CNT                 35250000
418690           IF CIART-RT-IND = 'Y'                                  35250000
418691              PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL            35250000
418692                                       WS-SUB > CIABP-46-CTR      35250000
418705*R11.4.P PD_77362 DSL SPECIAL NOTATION PROCESSING
418706              IF CIASC-46-1-999-SLOT (WS-SUB) (26:1)     = 'N'    35250000
418707                 MOVE 'Y' TO CIART-SPEC-NOTE-IND                  35250000
418708*R11.4.P PD_77362 END
418696                 MOVE CIABP-46-CTR TO WS-SUB                      35250000
418699              END-IF                                              35250000
418700              END-PERFORM                                         35250000
418701           END-IF                                                 35250000
418702        END-IF                                                    35250000
418703     END-IF.                                                      35250000
418704*R11.4.1 TD_73292 DSL END                                         35240000
418705                                                                  38370000
464900*** R14.5 E_34072 EXL ALLOW TMC 4 FOR ADJUSTMENTS                 47090000
418720     IF  (WS-TYPE-SUBM  = '3' OR '4')                             35250000
418730     AND CIART-RT-IND = ('Y' OR 'C')                              35260000
465200***  AND FM105-TRANSM-MODE-CD > '1'                               47120000
465300     AND FM105-TRANSM-MODE-CD > '4'                               47130000
418740         PERFORM 9960-FIND-ERROR-SLOT       THRU 9960-EXIT        35270000
418750         IF WS-PROFESSIONAL                                       35280000
418760            MOVE 'PT045'        TO FM1A5-ERR-CD (WS-SUB1)         35290000
418770            MOVE 'I'            TO FM1A5-STAT-CD                  35300000
418780         ELSE                                                     35310000
418790            MOVE 'IT045'        TO FM105-ERR-CD (WS-SUB1)         35320000
418791            MOVE 'I'            TO FM105-STAT-CD                  35330000
418792         END-IF                                                   35340000
418793         MOVE 'I'               TO CIABP-STAT-CD                  35350000
418794         GO TO 1290-EXIT                                          35360000
418795     END-IF.                                                      35370000
188400                                                                  ITCP9102
188500     IF  CIABP-NOT-EOJ                                            ITCP9102
188600     AND WS-DONT-BYPASS-EDITS                                     ITCP9102
188700        PERFORM 1500-LEVEL-1-EDIT               THRU 1500-EXIT    ITCP9102
188800        PERFORM 9970-ERROR-CODE-CHK             THRU 9970-EXIT    ITCP9102
806125     END-IF.                                                      00346500
287200                                                                  03087
419400*R11.4 E_38714 DSL BEGIN                                          44520000
189100     IF  CIABP-NOT-EOJ                                            ITCP9102
189200     AND WS-DONT-BYPASS-EDITS                                     ITCP9102
419700       IF (CIAPP2-PROGRAM-CD = '6' AND CIAPP2-LEVEL-II-EDIT-YES)  38460000
419800       OR (CIAPP2-PROGRAM-CD NOT = '6')                           38460000
419900          PERFORM 1600-LEVEL-2-EDIT             THRU 1600-EXIT    38470000
420000          PERFORM 9970-ERROR-CODE-CHK           THRU 9970-EXIT    38480000
420100       END-IF                                                     38490000
806132     END-IF.                                                      00347200
287830                                                                  03094
189700     IF  CIABP-NOT-EOJ                                            ITCP9102
189800     AND WS-DONT-BYPASS-EDITS                                     ITCP9102
420600       IF (CIAPP2-PROGRAM-CD = '6' AND CIAPP2-LEVEL-II-EDIT-YES)  38530000
420700       OR (CIAPP2-PROGRAM-CD NOT = '6')                           38530000
420800          PERFORM 1700-LEVEL-3-EDIT          THRU 1700-EXIT       38540000
420900       END-IF                                                     38530000
421000     END-IF.                                                      38550000
421100*R11.4 E_38714 END                                                44520000
421200                                                                  38560000
421300*R11.4 E_38714 DSL BEGIN                                          44520000
421400     IF (CIART-RT-IND = 'C'  OR                                   55120000
421500              SIAPC-MODULE-NAME(1) = 'ITCA9080')                  55120000
421600        AND SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                   50610000
421700        MOVE CIACAE-RECORD TO SIAPC-ERROR-MESSAGE                 50610000
806138     END-IF.                                                      00347800
421900*R11.4 E_38714 DSL END                                            44520000
190200                                                                  ITCP9102
806811*TM-2010-02-R10.0.1 - DO NOT RESET IF STAT-CD = 'R'               00348000
190300     IF CIABP-NOT-EOJ                                             ITCP9102
190400        PERFORM 9970-ERROR-CODE-CHK             THRU 9970-EXIT    ITCP9102
806142        IF WS-DUP-FILE-IND        = 'Y'                           00348200
190600           IF WS-INSTITUTIONAL                                    ITCP9102
806144              MOVE 'D'               TO FM105-STAT-CD             00348400
190900           ELSE                                                   ITCP9102
806146              MOVE 'D'               TO FM1A5-STAT-CD             00348600
191200           END-IF                                                 ITCP9102
806148           MOVE 'I'                  TO CIABP-STAT-CD             00348400
191300        ELSE                                                      ITCP9102
191400           IF WS-INSTITUTIONAL                                    ITCP9102
806151              IF FM105-ERR-CD (1) = (SPACES OR 'IRL3S') AND       00349000
471200*R16.5 E_104562 PMP NEXT LINE                                     47720000
471200                 FM105-ERR-CD (2) = SPACES              AND       47720000
471300*                FM105-ERR-CD (3) = (SPACES OR 'IRL3S') AND       47730000
471400*                FM105-ERR-CD (4) = (SPACES OR 'IRL3S') AND       47740000
471500*                FM105-ERR-CD (5) = (SPACES OR 'IRL3S') AND       47750000
806828                 FM105-STAT-CD NOT = 'R'                          00349000
191600                 MOVE WS-V           TO FM105-STAT-CD             ITCP9102
191700                                        CIABP-STAT-CD             ITCP9102
191800              ELSE                                                ITCP9102
806833                 IF FM105-STAT-CD NOT = 'R'                       00349300
806834                    MOVE WS-I        TO FM105-STAT-CD             00349300
192000                                        CIABP-STAT-CD             ITCP9102
806836                 ELSE                                             00349300
806837                    IF FM105-STAT-CD = 'R'                        00349300
806838                       MOVE WS-R     TO CIABP-STAT-CD             00349300
806840                    END-IF                                        00349400
806841                 END-IF                                           00349400
192100              END-IF                                              ITCP9102
192200           ELSE                                                   ITCP9102
473000*R15.5.1 E_103267 PMP BEGIN                                       47900000
471100*             IF FM1A5-ERR-CD (1) = (SPACES OR 'PRL5T') AND       47710000
471200*                FM1A5-ERR-CD (2) = (SPACES OR 'PRL5T') AND       47720000
471300*                FM1A5-ERR-CD (3) = (SPACES OR 'PRL5T') AND       47730000
471400*                FM1A5-ERR-CD (4) = (SPACES OR 'PRL5T') AND       47740000
471500*                FM1A5-ERR-CD (5) = (SPACES OR 'PRL5T') AND       47750000
473000*R15.5.1 E_103267 PMP END                                         47900000
473000*R16.5 E_104562 PMP BEGIN                                         47900000
471100              IF  FM1A5-ERR-CD (1) =(SPACES OR 'PRL5T' OR 'PRL5Z')47710000
471200              AND FM1A5-ERR-CD (2) =(SPACES OR 'PRL5T' OR 'PRL5Z')47720000
471300              AND FM1A5-ERR-CD (3) = SPACES                       47730000
473100              AND FM1A5-STAT-CD NOT = 'R'                         47910000
473000*R16.5 E_104562 PMP END                                           47900000
192400                 MOVE WS-V           TO FM1A5-STAT-CD             ITCP9102
192500                                        CIABP-STAT-CD             ITCP9102
192600              ELSE                                                ITCP9102
806849                 IF FM1A5-STAT-CD NOT = 'R'                       00349900
806850                    MOVE WS-I        TO FM1A5-STAT-CD             00349900
192800                                        CIABP-STAT-CD             ITCP9102
806852                 ELSE                                             00349900
806853                    IF FM1A5-STAT-CD = 'R'                        00349900
806854                       MOVE WS-R     TO CIABP-STAT-CD             00349900
806855                    END-IF                                        00350000
806856                 END-IF                                           00350000
192900              END-IF                                              ITCP9102
193000           END-IF                                                 ITCP9102
193100        END-IF                                                    ITCP9102
806173     END-IF.                                                      00350300
193500                                                                  ITCP9102
193600 1290-EXIT.  EXIT.                                                ITCP9102
193700                                                                  ITCP9102
345100/                                                                 04938
193900 1300-PLAN-PRE-EDIT.                                              ITCP9102
194000                                                                  ITCP9102
806180     MOVE CWSCC-NORMAL-PROCESSING    TO SIAPC-FUNCTION-CD-MAJOR.  00351000
806181     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB.    00351100
806182     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT.   00351200
806183     PERFORM 9010-CALL-PRE-EDIT-MODULE          THRU 9010-EXIT.   00351300
194600                                                                  ITCP9102
194700 1300-EXIT.  EXIT.                                                ITCP9102
194800                                                                  ITCP9102
476000*** R14.5 E_34072  EXL START                                      48200000
476100 1310-FIND-ADJ-REQUEST.                                           48210000
476200                                                                  48220000
476300     MOVE 'ITCP9102'                TO CSAPI-CALLING-PGM-ID.      48230000
476400     MOVE  6                        TO WS-B2-SUB-COMM.            48240000
476500     MOVE 'Q'                       TO WS-B2-REQUEST.             48250000
476600     MOVE 'ADJUST'                  TO B22-MSG-TP-CD.             48260000
476700     MOVE SPACE                     TO B22-MSG-STS-CD.            48270000
476800     IF WS-PROFESSIONAL                                           48280000
476900        MOVE FM1A5-CROSS-REF-SCCF-NUM TO CSAPI-SCCF-SER-NUM       48290000
477000     ELSE                                                         48300000
477100        MOVE FM105-CROSS-REF-SCCF-NUM TO CSAPI-SCCF-SER-NUM       48310000
477200     END-IF.                                                      48320000
477300                                                                  48332000
477900     PERFORM 2900-READ-B2-MSG       THRU 2900-EXIT.               48340000
478000                                                                  48353000
478500     EVALUATE CSAPI-RETURN-CODE                                   48360000
478600        WHEN +0                                                   48370000
478700           CONTINUE                                               48380000
478800        WHEN +100                                                 48390000
478900            MOVE 'M'          TO CIABP-STAT-CD                    48400000
479000            MOVE 'M'          TO CIASC-CLAIM-STATUS-CD(1)         48410000
479100            IF WS-PROFESSIONAL                                    48420000
479200               MOVE 'PXR13'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)  48430000
479300            ELSE                                                  48440000
479400               MOVE 'IXR13'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)  48450000
479500            END-IF                                                48460000
479600            GO TO 1310-EXIT                                       48470000
479700         WHEN OTHER                                               48480000
479800            DISPLAY CSAPI-RETURN-MESSAGE                          48490000
479900            PERFORM 2902-INSERT-IO-ERROR-MSG                      48500000
480000            GO TO 1310-EXIT                                       48510000
480100       END-EVALUATE.                                              48520000
480200                                                                  48530000
480300     IF B22-ADJ-SF-IND  = 'N'                                     48540000
482300        IF WS-PROFESSIONAL                                        48550000
482400           MOVE 'PXR14'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)     48560000
482700        ELSE                                                      48570000
482800           MOVE 'IXR14'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)     48580000
482801        END-IF                                                    48590000
482802        MOVE 'I'           TO CIASC-CLAIM-STATUS-CD(1)            48600000
482803                              CIABP-STAT-CD                       48610000
482804        GO TO 1310-EXIT                                           48620000
482805     END-IF.                                                      48630000
482806                                                                  48640000
482807     MOVE B22-MSG-ID       TO B27-MSG-ID.                         48650000
482808                                                                  48660000
482809     MOVE 'SELECT'                 TO CSAPI-COMMAND.              48670000
482810     MOVE 1                        TO CSAPI-SUB-COMMAND.          48680000
482811     SET WS-ITDB27S1               TO TRUE.                       48690000
482812     CALL WS-CALL-MODULE       USING WS-ITCCSAPI                  48700000
482813                                     B27-DATA-AREA.               48710000
482814     EVALUATE CSAPI-RETURN-CODE                                   48720000
482815        WHEN +0                                                   48730000
482816           CONTINUE                                               48740000
482817        WHEN OTHER                                                48750000
482818           MOVE 'SF024'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)      48760000
482819                             SIAPC-RETURN-MESSAGE-CD              48770000
482820           DISPLAY 'ITCP9102/1310 - BAD SELECT IPP_MSG_ADJ'       48780000
482821           DISPLAY CSAPI-RETURN-MESSAGE                           48790000
482822           PERFORM 9990-ERROR-DISPLAY         THRU 9990-EXIT      48800000
482823           GO TO 1310-EXIT                                        48810000
482824       END-EVALUATE.                                              48820000
482825                                                                  48820100
482826       IF B27-XREF-SCCF-ID-NI      = -1                           48822000
482827       OR B27-XREF-SCCF-ID         = SPACE                        48823000
482828       OR B27-XREF-SCCF-ID         = CIASC-SCCF-SERIAL-NUM(1)     48824000
482829          NEXT SENTENCE                                           48825000
483000       ELSE                                                       48826000
483001          IF WS-PROFESSIONAL                                      48827000
483002             MOVE 'PXR14'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   48828000
483003          ELSE                                                    48829000
483004             MOVE 'IXR14'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   48829100
483005          END-IF                                                  48829200
483006          MOVE 'I'           TO CIASC-CLAIM-STATUS-CD(1)          48829300
483007                                CIABP-STAT-CD                     48829400
483008          GO TO 1310-EXIT                                         48829700
483009       END-IF.                                                    48829800
483010                                                                  48830000
014430*R14.5.1, TD_98602, EXL  SET CIABP-STAT-CD FOR IA046 (2 LINES)    01490000
483011       IF B27-ADJ-RCPT-DT-NI = -1                                 48840000
483012          IF WS-PROFESSIONAL                                      48850000
483013             MOVE 'PA046'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   48860000
483014             MOVE 'I'        TO CIASC-CLAIM-STATUS-CD(1)          48870000
483015                                CIABP-STAT-CD                     48880000
483016          ELSE                                                    48890000
483017             MOVE 'IA046'    TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   48900000
483018             MOVE 'I'        TO CIASC-CLAIM-STATUS-CD(1)          48910000
483015                                CIABP-STAT-CD                     48880000
483019       ELSE                                                       48920000
483100          IF  B27-ADJ-RCPT-DT(1:4) = FM1A5-ADJ-RCPT-DT(1:4)       48930000
483200          AND B27-ADJ-RCPT-DT(6:2) = FM1A5-ADJ-RCPT-DT-MM         48940000
483300          AND B27-ADJ-RCPT-DT(9:2) = FM1A5-ADJ-RCPT-DT-DD         48950000
483400              CONTINUE                                            48960000
483500          ELSE                                                    48970000
483600             IF WS-PROFESSIONAL                                   48980000
483700                MOVE 'PA046'  TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)  48990000
483800                MOVE 'I'      TO CIASC-CLAIM-STATUS-CD(1)         49000000
483900                                 CIABP-STAT-CD                    49010000
484000             ELSE                                                 49020000
484100                MOVE 'IA046'  TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)  49030000
484200                MOVE 'I'      TO CIASC-CLAIM-STATUS-CD(1)         49040000
483015                                 CIABP-STAT-CD                    48880000
484300             END-IF                                               49050000
484400          END-IF                                                  49060000
484500       END-IF.                                                    49070000
484600                                                                  49240000
484700 1310-EXIT.  EXIT.                                                49250000
484800******************************************************************49260000
484900 1320-MAP-CA1-PROF.                                               49270000
485000                                                                  49280000
485100     MOVE +0                    TO CA1-SUSPENSE-UID.              49290000
485200     MOVE FM1A5-SCCF-SER-NUM    TO CA1-SCCF-ID.                   49300000
485300     MOVE FM1A5-BOID            TO CA1-BUS-OWNER-ID.              49310000
485400     MOVE 'SF'                  TO CA1-FMT-TXN-ID.                49320000
485500     MOVE SPACE                 TO CA1-DISP-CD.                   49330000
485600                                                                  49340000
485700     MOVE FM1A5-LOC-PLAN-RCPT-DT-CC  TO                           49350000
485800                           CA1-HOST-PLAN-RCPT-DT(1:2).            49360000
485900     MOVE FM1A5-LOC-PLAN-RCPT-DT-YY  TO                           49370000
486000                           CA1-HOST-PLAN-RCPT-DT(3:2).            49380000
486100     MOVE FM1A5-LOC-PLAN-RCPT-DT-MM  TO                           49390000
486200                           CA1-HOST-PLAN-RCPT-DT(6:2).            49400000
486300     MOVE FM1A5-LOC-PLAN-RCPT-DT-DD  TO                           49410000
486400                           CA1-HOST-PLAN-RCPT-DT(9:2).            49420000
486500     MOVE '-'           TO CA1-HOST-PLAN-RCPT-DT(5:1)             49430000
486600                           CA1-HOST-PLAN-RCPT-DT(8:1).            49440000
486700                                                                  49450000
486800     MOVE FM1A5-LOC-PLAN-CD          TO CA1-HOST-PLAN-CD.         49460000
486900     MOVE FM1A5-LOC-PLAN-STA-CD      TO CA1-HOST-PLAN-STN-CD.     49470000
487000     MOVE FM1A5-LOC-PLAN-CNTL-NUM    TO CA1-HOST-PLAN-CNTL-ID.    49480000
487100     MOVE FM1A5-LOC-PLAN-CLM-REF-NUM TO CA1-HOST-PLAN-CLM-ID.     49490000
487200     MOVE CIABP-HOST-HOME-SITE-IND   TO CA1-HOST-HOME-CD.         49500000
487300     MOVE FM1A5-CNTL-PLAN-CD         TO CA1-HOME-PLAN-CD.         49510000
487400     MOVE FM1A5-PROC-SITE-PLAN-CD    TO CA1-PRCSG-SITE-PLAN-CD    49520000
487500     MOVE FM1A5-PROC-SITE-STA-CD     TO CA1-PRCSG-SITE-STN-CD.    49530000
487600     MOVE FM1A5-PROC-SITE-CNTL-NUM   TO CA1-PRCSG-SITE-CNTL-ID    49540000
487700     MOVE FM1A5-STAT-CD              TO CA1-STS-CD.               49550000
487800     MOVE FM1A5-CROSS-REF-SCCF-NUM   TO CA1-XREF-SCCF-ID.         49560000
487900     MOVE FM1B0-BCBS-PROV-NUM        TO CA1-BCBS-PROV-ID.         49570000
488000     MOVE FM1B0-PROV-NPI             TO CA1-PROV-NPI-CD.          49580000
488100     MOVE FM1B0-FED-TAX-ID-NUM       TO CA1-FED-TAX-ID.           49590000
488200     MOVE FM1B0-TYPE-FED-TAX-ID-NUM  TO CA1-FED-TAX-ID-TP-CD.     49600000
488300                                                                  49610000
488400     MOVE FM1D0-SUB-ID-PFX-1-3       TO CA1-SUB-APFX-ID.          49620000
488500     MOVE FM1D0-SUB-ID-SUFX-4-17     TO CA1-SUB-SUFX-ID.          49630000
488600                                                                  49640000
488700     MOVE FM1A5-CLERK-NUM            TO CA1-CLERK-ID.             49650000
488800     MOVE FM1A5-CLM-TYPE             TO CA1-CLM-TP-CD.            49660000
488900                                                                  49670000
489000     MOVE CIABP-INCUR-DT-CC          TO CA1-SERV-DT(1:2).         49680000
489100     MOVE CIABP-INCUR-DT-YY          TO CA1-SERV-DT(3:2).         49690000
489200     MOVE CIABP-INCUR-DT-MM          TO CA1-SERV-DT(6:2).         49700000
489300     MOVE CIABP-INCUR-DT-DD          TO CA1-SERV-DT(9:2).         49710000
489400     MOVE '-'                        TO CA1-SERV-DT(5:1)          49720000
489500                                        CA1-SERV-DT(8:1).         49730000
489600                                                                  49740000
489700     MOVE FM1E0-TOT-CHRG             TO CA1-TOT-CHG-AMT.          49750000
489800     MOVE FM1A5-TYPE-SUBM            TO CA1-SUBM-TP-CD.           49760000
489900     MOVE FM1A5-PGM-CD               TO CA1-PGM-CD.               49770000
490000     MOVE FM1A5-BCP-PROD-TYPE        TO CA1-BCP-PROD-TP-CD.       49780000
490100     MOVE FM1A5-DEL-MTD              TO CA1-DLVY-METH-CD.         49790000
490200     MOVE FM1A5-ECR-IND              TO CA1-ECRP-IND.             49800000
490300     MOVE 'H'                        TO CA1-CLM-ADMIN-ACTION-CD.  49810000
490400     MOVE SPACES                     TO CA1-PRVCY-CD.             49820000
490500     MOVE FM1A5-RMRK-CD              TO CA1-RMK-CD.               49830000
490600     MOVE CIART-PART-KEY-NUM         TO CA1-PART-KEY-NUM.         49840000
490700     MOVE FM1A5-SYS-CRT-CD           TO CA1-SYS-CRT-CD.           49850000
490800     MOVE FM1A5-SYS-UPDT-CD          TO CA1-SYS-UPDT-CD.          49860000
490900     MOVE 'RT ADJ MISM'              TO CA1-CRTD-BY-USER-ID       49870000
491000                                        CA1-LAST-UPDT-USER-ID.    49880000
491100                                                                  49890000
491200     MOVE CIART-CURR-TS              TO CA1-CRT-TS                49900000
491300                                        CA1-LAST-UPDT-TS.         49910000
491400                                                                  49920000
491500     MOVE SPACES                     TO CA1-ACTL-SUB-APFX-ID      49930000
491600                                        CA1-ACTL-SUB-SUFX-ID      49940000
491700                                        CA1-DISP-TP-CD.           49950000
491800                                                                  49960000
491900     MOVE FM1A5-ADJ-RCPT-DT-CC       TO CA1-ADJ-RCPT-DT(1:2).     49970000
492000     MOVE FM1A5-ADJ-RCPT-DT-YY       TO CA1-ADJ-RCPT-DT(3:2).     49980000
492100     MOVE FM1A5-ADJ-RCPT-DT-MM       TO CA1-ADJ-RCPT-DT(6:2).     49990000
492200     MOVE FM1A5-ADJ-RCPT-DT-DD       TO CA1-ADJ-RCPT-DT(9:2).     50000000
492300     MOVE '-'                        TO CA1-ADJ-RCPT-DT(5:1)      50010000
492400                                        CA1-ADJ-RCPT-DT(8:1).     50020000
492500                                                                  50030000
492600     MOVE SPACES                     TO CA1-MISC-INFO.            50040000
492700     MOVE SPACES                     TO CA1-SUB-COMMAND-INFO.     50050000
492800                                                                  50060000
492900 1320-EXIT.  EXIT.                                                50070000
493000******************************************************************50080000
493100 1330-MAP-CA1-INST.                                               50090000
493200                                                                  50100000
493300     MOVE +0                    TO CA1-SUSPENSE-UID.              50110000
493400     MOVE FM105-SCCF-SER-NUM    TO CA1-SCCF-ID.                   50120000
493500     MOVE 'SF'                  TO CA1-FMT-TXN-ID.                50130000
493600     MOVE FM105-BOID            TO CA1-BUS-OWNER-ID.              50140000
493700     MOVE SPACE                 TO CA1-DISP-CD.                   50150000
493800                                                                  50160000
493900     MOVE FM105-LOC-PLAN-RCPT-DT-CC  TO                           50170000
494000                              CA1-HOST-PLAN-RCPT-DT(1:2).         50180000
494100     MOVE FM105-LOC-PLAN-RCPT-DT-YY  TO                           50190000
494200                              CA1-HOST-PLAN-RCPT-DT(3:2).         50200000
494300     MOVE FM105-LOC-PLAN-RCPT-DT-MM  TO                           50210000
494400                              CA1-HOST-PLAN-RCPT-DT(6:2).         50220000
494500     MOVE FM105-LOC-PLAN-RCPT-DT-DD  TO                           50230000
494600                              CA1-HOST-PLAN-RCPT-DT(9:2).         50240000
494700     MOVE '-'              TO CA1-HOST-PLAN-RCPT-DT(5:1)          50250000
494800                              CA1-HOST-PLAN-RCPT-DT(8:1).         50260000
494900                                                                  50270000
495000     MOVE FM105-LOC-PLAN-CD          TO CA1-HOST-PLAN-CD.         50280000
495100     MOVE FM105-LOC-PLAN-STA-CD      TO CA1-HOST-PLAN-STN-CD.     50290000
495200     MOVE FM105-LOC-PLAN-CNTL-NUM    TO CA1-HOST-PLAN-CNTL-ID.    50300000
495300     MOVE FM105-LOC-PLAN-CLM-REF-NUM TO CA1-HOST-PLAN-CLM-ID.     50310000
495400     MOVE CIABP-HOST-HOME-SITE-IND   TO CA1-HOST-HOME-CD.         50320000
495500     MOVE FM105-CNTL-PLAN-CD         TO CA1-HOME-PLAN-CD.         50330000
495600     MOVE FM105-PROC-SITE-PLAN-CD    TO CA1-PRCSG-SITE-PLAN-CD    50340000
495700     MOVE FM105-PROC-SITE-STA-CD     TO CA1-PRCSG-SITE-STN-CD.    50350000
495800     MOVE FM105-PROC-SITE-CNTL-NUM   TO CA1-PRCSG-SITE-CNTL-ID    50360000
495900     MOVE FM105-STAT-CD              TO CA1-STS-CD.               50370000
496000     MOVE FM105-CROSS-REF-SCCF-NUM   TO CA1-XREF-SCCF-ID.         50380000
496100     MOVE FM110-BCBS-PROV-NUM        TO CA1-BCBS-PROV-ID.         50390000
496200     MOVE FM110-PROV-NPI             TO CA1-PROV-NPI-CD.          50400000
496300     MOVE FM110-FED-TAX-ID-NUM       TO CA1-FED-TAX-ID.           50410000
496400     MOVE SPACES                     TO CA1-FED-TAX-ID-TP-CD.     50420000
496500                                                                  50430000
496600     MOVE CIASC-30-01-SLOT           TO FM130-RECORD.             50440000
496700     MOVE FM130-SUB-ID-PFX-1-3       TO CA1-SUB-APFX-ID.          50450000
496800     MOVE FM130-SUB-ID-SUFX-4-17     TO CA1-SUB-SUFX-ID.          50460000
496900                                                                  50470000
497000     MOVE FM105-CLERK-NUM            TO CA1-CLERK-ID.             50480000
497100     MOVE FM105-CLM-TYPE             TO CA1-CLM-TP-CD.            50490000
497200                                                                  50500000
497300     MOVE CIABP-INCUR-DT-CC          TO CA1-SERV-DT(1:2).         50510000
497400     MOVE CIABP-INCUR-DT-YY          TO CA1-SERV-DT(3:2).         50520000
497500     MOVE CIABP-INCUR-DT-MM          TO CA1-SERV-DT(6:2).         50530000
497600     MOVE CIABP-INCUR-DT-DD          TO CA1-SERV-DT(9:2).         50540000
497700     MOVE '-'                        TO CA1-SERV-DT(5:1)          50550000
497800                                        CA1-SERV-DT(8:1).         50560000
497900                                                                  50570000
498000     MOVE WS-TOTL-CHRG               TO CA1-TOT-CHG-AMT.          50580000
498100     MOVE FM105-TYPE-SUBM            TO CA1-SUBM-TP-CD.           50590000
498200     MOVE FM105-PGM-CD               TO CA1-PGM-CD.               50600000
498300     MOVE FM105-BCP-PROD-TYPE        TO CA1-BCP-PROD-TP-CD.       50610000
498400     MOVE FM105-DEL-MTD              TO CA1-DLVY-METH-CD.         50620000
498500     MOVE FM105-ECR-IND              TO CA1-ECRP-IND.             50630000
498600     MOVE 'H'                        TO CA1-CLM-ADMIN-ACTION-CD.  50640000
498700     MOVE SPACES                     TO CA1-PRVCY-CD.             50650000
498800     MOVE FM105-RMRK-CD              TO CA1-RMK-CD.               50660000
498900     MOVE CIART-PART-KEY-NUM         TO CA1-PART-KEY-NUM.         50670000
499000     MOVE FM105-SYS-CRT-CD           TO CA1-SYS-CRT-CD.           50680000
499100     MOVE FM105-SYS-UPDT-CD          TO CA1-SYS-UPDT-CD.          50690000
499200     MOVE 'RT ADJ MISM'              TO CA1-CRTD-BY-USER-ID       50700000
499300                                        CA1-LAST-UPDT-USER-ID.    50710000
499400                                                                  50720000
499500     MOVE CIART-CURR-TS              TO CA1-CRT-TS                50730000
499600                                        CA1-LAST-UPDT-TS.         50740000
499700                                                                  50750000
499800     MOVE SPACES                     TO CA1-ACTL-SUB-APFX-ID      50760000
499900                                        CA1-ACTL-SUB-SUFX-ID      50770000
500000                                        CA1-DISP-TP-CD.           50780000
500100                                                                  50790000
500200     MOVE FM105-ADJ-RCPT-DT-CC       TO CA1-ADJ-RCPT-DT(1:2).     50800000
500300     MOVE FM105-ADJ-RCPT-DT-YY       TO CA1-ADJ-RCPT-DT(3:2).     50810000
500400     MOVE FM105-ADJ-RCPT-DT-MM       TO CA1-ADJ-RCPT-DT(6:2).     50820000
500500     MOVE FM105-ADJ-RCPT-DT-DD       TO CA1-ADJ-RCPT-DT(9:2).     50830000
500600     MOVE '-'                        TO CA1-ADJ-RCPT-DT(5:1)      50840000
500700                                        CA1-ADJ-RCPT-DT(8:1).     50850000
500800                                                                  50860000
500900     MOVE SPACES                     TO CA1-MISC-INFO.            50870000
501000     MOVE SPACES                     TO CA1-SUB-COMMAND-INFO.     50880000
501100                                                                  50890000
501200 1330-EXIT.  EXIT.                                                50900000
501300******************************************************************50910000
501301 1340-CHECK-DUP-SUSPENSE.                                         50920000
501302                                                                  50930000
501303     MOVE 'ITCP9102'                TO CSAPI-CALLING-PGM-ID.      50940000
501304     SET CSAPI-SELECT               TO TRUE.                      50950000
501305     MOVE  5                        TO CSAPI-SUB-COMMAND.         50960000
501306     IF WS-PROFESSIONAL                                           50970000
501307        MOVE FM1A5-SCCF-SER-NUM       TO CA1-SCCF-ID              50980000
501308        MOVE FM1A5-CROSS-REF-SCCF-NUM TO CA1-XREF-SCCF-ID         50990000
501309        MOVE FM1A5-BOID               TO CA1-BUS-OWNER-ID         51000000
501310     ELSE                                                         51010000
501311        MOVE FM105-SCCF-SER-NUM       TO CA1-SCCF-ID              51020000
501312        MOVE FM105-CROSS-REF-SCCF-NUM TO CA1-XREF-SCCF-ID         51030000
501313        MOVE FM105-BOID               TO CA1-BUS-OWNER-ID         51040000
501314     END-IF.                                                      51050000
501315                                                                  51060000
501316     MOVE '1'                       TO CA1-HOST-HOME-CD.          51070000
501317                                                                  51080000
501318     SET WS-ITDCA1S1                TO TRUE.                      51090000
501319     CALL WS-CALL-MODULE    USING WS-ITCCSAPI                     51100000
501320                                  WS-ITDCA1X1.                    51110000
501321                                                                  51120000
501322     EVALUATE CSAPI-RETURN-CODE                                   51130000
501323        WHEN +0                                                   51140000
501324           DISPLAY 'DUPLICATE XREF - SCCF#: ' CA1-SCCF-ID         51150000
501325        WHEN +100                                                 51160000
501326           GO TO 1340-EXIT                                        51170000
501327        WHEN OTHER                                                51180000
501328           MOVE 'SF025'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)      51190000
501329                             SIAPC-RETURN-MESSAGE-CD              51200000
501330           DISPLAY 'ITCP9102/1340 - BAD SELECT FROM CA SEARCH TBL'51210000
501331           DISPLAY CSAPI-RETURN-MESSAGE                           51220000
501332           PERFORM 9990-ERROR-DISPLAY         THRU 9990-EXIT      51230000
501333           GO TO 1340-EXIT                                        51240000
501334     END-EVALUATE.                                                51250000
501335                                                                  51260000
501336     IF WS-PROFESSIONAL                                           51270000
501337        MOVE 'PXR15'         TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   51280000
501338     ELSE                                                         51290000
501339        MOVE 'IXR15'         TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   51300000
501340     END-IF.                                                      51310000
501341                                                                  51320000
501342     MOVE 'D'                TO CIASC-CLAIM-STATUS-CD(1).         51330000
501343     MOVE 'I'                TO CIABP-STAT-CD.                    51340000
501344                                                                  51350000
501345 1340-EXIT. EXIT.                                                 51360000
501346******************************************************************51370000
501347 1350-CHECK-DUP-FDB.                                              51380000
501348                                                                  51390000
501349     MOVE 'ITCP9102'                TO CSAPI-CALLING-PGM-ID.      51400000
501350     SET CSAPI-SELECT               TO TRUE.                      51410000
501351     MOVE  2                        TO CSAPI-SUB-COMMAND.         51420000
501352     IF WS-PROFESSIONAL                                           51430000
501353        MOVE FM1A5-SCCF-SER-NUM       TO CA1-SCCF-ID              51440000
501360        MOVE FM1A5-CROSS-REF-SCCF-NUM TO IPS-XREF-SCCF-ID         51450000
501361        MOVE FM1A5-BOID               TO IPS-BUS-OWNER-ID         51460000
501362     ELSE                                                         51470000
501363        MOVE FM105-SCCF-SER-NUM       TO CA1-SCCF-ID              51480000
501364        MOVE FM105-CROSS-REF-SCCF-NUM TO IPS-XREF-SCCF-ID         51490000
501365        MOVE FM105-BOID               TO IPS-BUS-OWNER-ID         51500000
501366     END-IF.                                                      51510000
501367                                                                  51520000
501368     MOVE '1'                       TO IPS-HOST-HOME-CD.          51530000
501369     MOVE 'SF'                      TO IPS-CLM-TXN-TP-CD.         51540000
501370                                                                  51550000
501371     SET WS-ITDFCXS1                TO TRUE.                      51560000
501372     CALL WS-CALL-MODULE    USING WS-ITCCSAPI                     51570000
501373                                  IPS-DATA-AREA.                  51580000
501374                                                                  51590000
501375     EVALUATE CSAPI-RETURN-CODE                                   51600000
501376        WHEN +0                                                   51610000
501377           DISPLAY 'DUPLICATE XREF - SCCF#: ' IPS-SCCF-ID         51620000
501378        WHEN +100                                                 51630000
501379           GO TO 1350-EXIT                                        51640000
501380        WHEN OTHER                                                51650000
501381           MOVE 'SF026'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)      51660000
501382                             SIAPC-RETURN-MESSAGE-CD              51670000
501383           DISPLAY 'ITCP9102/1350 - BAD SELECT FROM CLM SEARCH'   51680000
501384           DISPLAY CSAPI-RETURN-MESSAGE                           51690000
501385           PERFORM 9990-ERROR-DISPLAY         THRU 9990-EXIT      51700000
501386           GO TO 1350-EXIT                                        51710000
501387     END-EVALUATE.                                                51720000
501388                                                                  51730000
501389     IF WS-PROFESSIONAL                                           51740000
501390        MOVE 'PXR16'         TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   51750000
501391     ELSE                                                         51760000
501400        MOVE 'IXR16'         TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   51770000
501401     END-IF.                                                      51780000
501402                                                                  51790000
501403     MOVE 'D'                TO CIASC-CLAIM-STATUS-CD(1).         51800000
501404     MOVE 'I'                TO CIABP-STAT-CD.                    51810000
501405                                                                  51820000
501406 1350-EXIT. EXIT.                                                 51830000
501407*** R14.5 E_34072  EXL END                                        51840000
501408******************************************************************51850000
501500                                                                  51860000
195000 1400-PLAN-SPECIFIC-EDIT.                                         ITCP9102
195100                                                                  ITCP9102
195200     MOVE CWSCC-EDIT-DATA            TO SIAPC-FUNCTION-CD-MAJOR   ITCP9102
195300     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB     ITCP9102
195400     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT    ITCP9102
195500                                                                  ITCP9102
195600     IF WS-INSTITUTIONAL                                          ITCP9102
195700****  ITCPE004 - INSTITUTIONAL PLAN SPECIFIC EDIT MODULE      ****ITCP9102
195900        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806197        SET WS-ITCPE004              TO TRUE                      00352800
196100        MOVE WS-CALL-MODULE          TO                           ITCP9102
196200             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
806200        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00353100
806201                                  WS-ITCIABP                      00353200
196500                                  WS-INST-ERROR-INDICATORS        ITCP9102
196600                                  WS-WORK-AREA                    ITCP9102
196700                                  WS-STACKED-CLAIM                ITCP9102
332811                                  WS-ITCDBP20-RECORD              03484
196800     ELSE                                                         ITCP9102
196900****  ITCPE005 - PROFESSIONAL PLAN SPECIFIC EDIT MODULE       ****ITCP9102
197100        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806209        SET WS-ITCPE005              TO TRUE                      00354100
197300        MOVE WS-CALL-MODULE          TO                           ITCP9102
197400             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
806212        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00354400
806213                                  WS-ITCIABP                      00354500
197700                                  WS-PROF-ERROR-INDICATORS        ITCP9102
197800                                  WS-WORK-AREA                    ITCP9102
197900                                  WS-STACKED-CLAIM                ITCP9102
334210                                  WS-ITCDBP20-RECORD              03501
198000     END-IF                                                       ITCP9102
198100                                                                  ITCP9102
198200     PERFORM 9910-SIAPC-RETURN-IND-CHK          THRU 9910-EXIT    ITCP9102
198300     .                                                            ITCP9102
198400                                                                  ITCP9102
198500 1400-EXIT.  EXIT.                                                ITCP9102
198600                                                                  ITCP9102
351000/                                                                 04997
198800 1500-LEVEL-1-EDIT.                                               ITCP9102
198900                                                                  ITCP9102
199000     MOVE CWSCC-NORMAL-PROCESSING    TO SIAPC-FUNCTION-CD-MAJOR   ITCP9102
199100     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB     ITCP9102
199200     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT    ITCP9102
199300                                                                  ITCP9102
199400****  ITCP9110 - LEVEL 1 EDIT MODULE                          ****ITCP9102
199500     ADD WS-PLUS-ONE                 TO SIAPC-MODULE-LEVEL        ITCP9102
806234     SET WS-ITCP9110                 TO TRUE                      00356600
199700     MOVE WS-CALL-MODULE             TO                           ITCP9102
199800          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)                  ITCP9102
199900                                                                  ITCP9102
806238     CALL WS-CALL-MODULE USING WS-ITSIAPC                         00357000
806239                               WS-ITCIABP                         00357100
200200                               WS-STACKED-CLAIM                   ITCP9102
336510                               WS-ITCDBP20-RECORD                 03527
005695                               WS-PARM-SEQ-NUM-MAX                00569500
200300                                                                  ITCP9102
200400     PERFORM 9910-SIAPC-RETURN-IND-CHK          THRU 9910-EXIT    ITCP9102
200500     .                                                            ITCP9102
200600                                                                  ITCP9102
200700 1500-EXIT.  EXIT.                                                ITCP9102
200800                                                                  ITCP9102
200900/                                                                 ITCP9102
201000 1600-LEVEL-2-EDIT.                                               ITCP9102
201100                                                                  ITCP9102
806252     MOVE CWSCC-EDIT-DATA            TO SIAPC-FUNCTION-CD-MAJOR.  00358400
806253     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB.    00358500
806254     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT.   00358600
201500                                                                  ITCP9102
201600     IF WS-INSTITUTIONAL                                          ITCP9102
354300****  ITCP9528 - INSTITUTIONAL LEVEL 2 EDIT MODULE            *** 05030
201900        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806259        SET WS-ITCP9528              TO TRUE                      00359300
202100        MOVE WS-CALL-MODULE          TO                           ITCP9102
202200             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
399210*R11.3 E_10097 DSL BEGIN - ADD REF-REL, REF-SRC-PMT TO CALL LIST  40010000
435800*R11.4 E_38714 DSL BEGIN - ADD WS-UPF-PRC-EDIT-CD   TO CALL LIST  40010000
806262        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00359600
806263                                  WS-ITCIABP                      00359700
202600                                  WS-INST-ERROR-INDICATORS        ITCP9102
202700                                  WS-WORK-AREA                    ITCP9102
202800                                  WS-STACKED-CLAIM                ITCP9102
806267                                  WS-ITCWSPSC                     00360100
340110                                  WS-ITCDBP20-RECORD              03569
399910                                  WS-REF-REL-CD-TBL               40130000
399920                                  WS-REF-SRC-PMT-TBL              40130000
436800                                  WS-UPF-PRC-EDIT-CD              02730000
399930*R11.3 E_10097 END                                                40010000
202900     ELSE                                                         ITCP9102
357200****  ITCP9526 - PROFESSIONAL LEVEL 2 EDIT MODULE             *** 05154
203200        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806272        SET WS-ITCP9526              TO TRUE                      00360800
203400        MOVE WS-CALL-MODULE          TO                           ITCP9102
203500             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
203600                                                                  ITCP9102
400610*R11.3 E_10097 DSL BEGIN - ADD REF-REL, REF-SRC-PMT TO CALL LIST  40010000
437800*R11.4 E_38714 DSL BEGIN - ADD WS-UPF-PRC-EDIT-CD   TO CALL LIST  40010000
806276        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00361200
806277                                  WS-ITCIABP                      00361300
203900                                  WS-PROF-ERROR-INDICATORS        ITCP9102
204000                                  WS-WORK-AREA                    ITCP9102
204100                                  WS-STACKED-CLAIM                ITCP9102
806281                                  WS-ITCWSPSC                     00361700
342410                                  WS-ITCDBP20-RECORD              03598
401310                                  WS-REF-REL-CD-TBL               40130000
401320                                  WS-REF-SRC-PMT-TBL              40130000
438800                                  WS-UPF-PRC-EDIT-CD              02730000
401330*R11.3 E_10097 END                                                40010000
806283     END-IF.                                                      00361900
204300                                                                  ITCP9102
806285     PERFORM 9910-SIAPC-RETURN-IND-CHK          THRU 9910-EXIT.   00362100
204600                                                                  ITCP9102
204700 1600-EXIT.  EXIT.                                                ITCP9102
204800                                                                  ITCP9102
806289*************************************************************     00362500
205000 1700-LEVEL-3-EDIT.                                               ITCP9102
205100                                                                  ITCP9102
205200     MOVE CWSCC-EDIT-DATA            TO SIAPC-FUNCTION-CD-MAJOR   ITCP9102
205300     MOVE CWSCC-EDIT-CLAIM           TO SIAPC-FUNCTION-CD-SUB     ITCP9102
205400     PERFORM 9900-RESET-SIAPC-RETURN-DATA       THRU 9900-EXIT    ITCP9102
205500                                                                  ITCP9102
205600     IF WS-INSTITUTIONAL                                          ITCP9102
205700****  ITCP9556 - INSTITUTIONAL LEVEL 3 EDIT MODULE            ****ITCP9102
205800        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806299        SET WS-ITCP9556              TO TRUE                      00363500
206000        MOVE WS-CALL-MODULE          TO                           ITCP9102
206100             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
206200                                                                  ITCP9102
806303        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00363900
806304                                  WS-ITCIABP                      00364000
206500                                  WS-INST-ERROR-INDICATORS        ITCP9102
206600                                  WS-WORK-AREA                    ITCP9102
206700                                  WS-STACKED-CLAIM                ITCP9102
806308                                  WS-ITCWSPSC                     00364400
345310                                  WS-ITCDBP20-RECORD              03630
206800     ELSE                                                         ITCP9102
206900****  ITCP9558 - PROFESSIONAL LEVEL 3 EDIT MODULE             ****ITCP9102
207000        ADD WS-PLUS-ONE              TO SIAPC-MODULE-LEVEL        ITCP9102
806313        SET WS-ITCP9558              TO TRUE                      00364900
207200        MOVE WS-CALL-MODULE          TO                           ITCP9102
207300             SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)               ITCP9102
207400                                                                  ITCP9102
806317        CALL WS-CALL-MODULE USING WS-ITSIAPC                      00365300
806318                                  WS-ITCIABP                      00365400
207700                                  WS-PROF-ERROR-INDICATORS        ITCP9102
207800                                  WS-WORK-AREA                    ITCP9102
207900                                  WS-STACKED-CLAIM                ITCP9102
806322                                  WS-ITCWSPSC                     00365800
346820                                  WS-ITCDBP20-RECORD              03648
208000     END-IF                                                       ITCP9102
208100                                                                  ITCP9102
208200     PERFORM 9910-SIAPC-RETURN-IND-CHK          THRU 9910-EXIT    ITCP9102
208300     .                                                            ITCP9102
208400                                                                  ITCP9102
208500 1700-EXIT.  EXIT.                                                ITCP9102
443601
443602*** R12.0 E_59024 EXL NEW PARAGRAPH   START                       15850000
443700***************************************************************** 40610000
443701* WS-STN-TBL WAS POPULATED IN 9090- WITH BATCH ONLY STATION CODES 40610000
443702* SO IF THE STN-CD IS FOUND ON THIS TABLE, SET TMC TO 1           40610000
443703* OTHERWISE, STN IS CAPABLE OF RECEIVING SF IN REAL TIME MODE:    40610000
443704* SET TMC TO 4                                                    40610000
443705***************************************************************** 40610000
443706 1800-VERIFY-TMC.
443707                                                                  40590000
443708     IF WS-PROFESSIONAL                                           40590000
443709        MOVE FM1A5-PROC-SITE-STA-CD  TO S02-STN-CD                40590000
443710     ELSE                                                         40590000
443711        MOVE FM105-PROC-SITE-STA-CD  TO S02-STN-CD                40590000
443712     END-IF.                                                      40590000
443713                                                                  40590000
443714     SET WS-STN-I                 TO 1.
443715                                                                  40590000
443716     SEARCH WS-STN-CD VARYING WS-STN-I                            17990000
443717        AT END                                                    40590000
443718           MOVE '4'               TO CIASC-TRANSM-MODE-CD(1)      40590000
443719        WHEN  WS-STN-CD(WS-STN-I)  = SPACE                        40590000
443720           MOVE '4'               TO CIASC-TRANSM-MODE-CD(1)      40590000
443721        WHEN  WS-STN-CD(WS-STN-I)  = S02-STN-CD                   40590000
443722           MOVE '1'               TO CIASC-TRANSM-MODE-CD(1)      40590000
443723     END-SEARCH.                                                  40590000
443724                                                                  40590000
443725 1800-EXIT.  EXIT.                                                40600000
210000** R17.5 E_112726 EXL  NEW PARAGRAPH 19##- SERIES                 54830000
519900***************************************************************** 54840000
210000 1900-COST-GROUP-PROCESS.                                         54850000
519600                                                                  54860000
           MOVE SPACE                   TO WS-PDR-CD                    54870000
                                           WS-RL6F-SW.                  54880000
                                                                        54881000
      ** E_118591 CHECK FOR FLXBL-NETW-IND RATHER THAN DELV-MTD         54882000
      **   IF  CIABP-DELV-MTD-IND = '2'                                 54883000
519600     IF  CIAPP2-FLXBL-NETW-IND   = 'Y'                            54890000
519600        NEXT SENTENCE                                             54900000
519600     ELSE                                                         54910000
      ** DELIVERY METHOD IS NOT 2, BUT IF COST GROUP FIELDS ARE SET,    54920000
      ** SPACE OUT COST GROUP FIELDS AND ISSUE PRL6F/IRL6F              54930000
519600        IF WS-PROFESSIONAL                                        54940000
519600           IF FM1E0-FLEX-NETWK-GRP-CLM > SPACE                    54950000
519600           OR WS-CST-ENTERED-SW    = 'Y'                          54960000
                    MOVE 'Y'            TO WS-RL6F-SW                   54970000
                    MOVE SPACE          TO FM1E0-FLEX-NETWK-GRP-CLM     54980000
519600              IF WS-CST-ENTERED-SW = 'Y'                          54990000
519600                 PERFORM 1910-MAP-CST-GRP-PROF THROUGH 1910-EXIT  55000000
519600              END-IF                                              55010000
519600           END-IF                                                 55020000
519600        ELSE                                                      55030000
519600           IF FM115-FLEX-NETWK-GRP-CLM > SPACE                    55040000
                    MOVE SPACE           TO FM115-FLEX-NETWK-GRP-CLM    55050000
                    MOVE 'Y'             TO WS-RL6F-SW                  55060000
519600           END-IF                                                 55070000
519600        END-IF                                                    55080000
519600        GO TO 1900-PRL6F-IRL6F                                    55090000
519600     END-IF.                                                      55100000
519600                                                                  55110000
519600     IF WS-PROFESSIONAL                                           55120000
519600        IF WS-CST-GRP-PROV(1)          > SPACE                    55130000
519600           PERFORM 1920-PROF-COST-GRP-ACCESS   THROUGH 1920-EXIT  55140000
              ELSE                                                      55150000
519600           IF FM1E0-FLEX-NETWK-GRP-CLM > SPACE                    55160000
519600           OR WS-CST-ENTERED-SW        = 'Y'                      55170000
                    MOVE 'Y'                TO WS-RL6F-SW               55180000
                    MOVE SPACE        TO FM1E0-FLEX-NETWK-GRP-CLM       55190000
519600              IF WS-CST-ENTERED-SW     = 'Y'                      55200000
                       MOVE SPACE     TO WS-PDR-CD                      55210000
519600                 PERFORM 1910-MAP-CST-GRP-PROF THROUGH 1910-EXIT  55220000
519600              END-IF                                              55230000
519600           END-IF                                                 55240000
              END-IF                                                    55250000
              IF  FM1E0-CLASS-PROV-CLM     = SPACE                      55260000
              AND FM1E0-FLEX-NETWK-GRP-CLM > SPACE                      55270000
                  MOVE SPACE        TO FM1E0-FLEX-NETWK-GRP-CLM         55280000
                  MOVE 'Y'          TO WS-RL6F-SW                       55290000
519600     END-IF.                                                      55300000
519600                                                                  55310000
519600     IF WS-INSTITUTIONAL                                          55320000
519600        IF FM115-CLASS-PROV-CLM   = '5' OR 'S' OR 'T'             55330000
519600           PERFORM 1940-INST-COST-GRP-ACCESS   THROUGH 1940-EXIT  55340000
              ELSE                                                      55350000
519600           IF FM115-FLEX-NETWK-GRP-CLM > SPACE                    55360000
                    MOVE 'Y'          TO WS-RL6F-SW                     55370000
                    MOVE SPACE        TO FM115-FLEX-NETWK-GRP-CLM       55380000
519600           END-IF                                                 55390000
519600        END-IF                                                    55400000
519600     END-IF.                                                      55410000
519600                                                                  55420000
519600 1900-PRL6F-IRL6F.                                                55430000
519600                                                                  55440000
519600     IF WS-RL6F-SW              = 'Y'                             55450000
              PERFORM 9960-FIND-ERROR-SLOT THROUGH 9960-EXIT            55460000
519600        IF WS-INSTITUTIONAL                                       55470000
                 MOVE 'IRL6F'      TO FM105-ERR-CD(WS-SUB1)             55480000
                                      CIABP-BDC-WARNING-MSG             55490000
519600        ELSE                                                      55500000
                 MOVE 'PRL6F'      TO FM1A5-ERR-CD(WS-SUB1)             55510000
                                      CIABP-BDC-WARNING-MSG             55520000
              END-IF                                                    55530000
           END-IF.                                                      55540000
                                                                        55550000
519700 1900-EXIT. EXIT.                                                 55560000
519900***************************************************************** 55570000
519600 1910-MAP-CST-GRP-PROF.                                           55580000
519600                                                                  55590000
519600        PERFORM VARYING WS-SUB FROM 1 BY 1                        55600000
519600           UNTIL WS-SUB       > CIABP-F0-CTR                      55610000
519600           MOVE CIASC-F5-1-999-ITEM (WS-SUB)  TO FM1F5-RECORD     55620000
519600           MOVE WS-PDR-CD    TO FM1F5-FLEX-NETWK-GRP-LN           55630000
519600           MOVE FM1F5-RECORD TO CIASC-F5-1-999-ITEM (WS-SUB)      55640000
519600        END-PERFORM.                                              55650000
519600                                                                  55660000
519600        MOVE 'N'             TO WS-CST-ENTERED-SW.                55670000
519600                                                                  55680000
519700 1910-EXIT. EXIT.                                                 55690000
519900***************************************************************** 55700000
519600 1920-PROF-COST-GRP-ACCESS.                                       55710000
519600                                                                  55720000
519600** THE WS-CST-GRP TABLE IS POPULATED IN 1960- PARAGRAPH.          55730000
519600** IT CONTAINS PROVIDER ID AND THE EARLIEST SERVICE               55740000
519600** START DATE FOR THIS PROVIDER IF CLASS PROVIDER ON THE CLAIM    55750000
519600** OR LINE LEVEL IS EQUAL TO 5, S, OR T                           55760000
519600                                                                  55770000
519600     PERFORM VARYING WS-PDR-SUB FROM 1 BY 1                       55780000
519600        UNTIL WS-PDR-SUB > 10                                     55790000
519600        IF WS-CST-GRP-PROV(WS-PDR-SUB) = SPACE                    55800000
519600           MOVE 10                    TO WS-PDR-SUB               55810000
519600        ELSE                                                      55820000
519600           MOVE FM1A5-BOID                  TO D04-BUS-OWNER-ID   55830000
519600           MOVE WS-CST-GRP-PROV(WS-PDR-SUB) TO D04-BCBS-PROV-ID   55840000
519600           MOVE WS-CST-GRP-DT(WS-PDR-SUB)(1:4)                    55850000
519600                                            TO D04-DATE(1:4)      55860000
519600           MOVE WS-CST-GRP-DT(WS-PDR-SUB)(5:2)                    55870000
519600                                            TO D04-DATE(6:2)      55880000
519600           MOVE WS-CST-GRP-DT(WS-PDR-SUB)(7:2)                    55890000
519600                                            TO D04-DATE(9:2)      55900000
519600           MOVE '-'                         TO D04-DATE(5:1)      55910000
519600                                               D04-DATE(8:1)      55920000
519600           PERFORM 1950-ACCESS-CST-GRP-TBL THRU 1950-EXIT         55930000
519600           IF CIABP-EOJ                                           55940000
519600              GO TO 1920-EXIT                                     55950000
519600           END-IF                                                 55960000
519600           PERFORM 1930-MOVE-CST-GRP-PROF THRU 1930-EXIT          55970000
519600        END-IF                                                    55980000
519600     END-PERFORM.                                                 55990000
519600                                                                  56000000
519700 1920-EXIT. EXIT.                                                 56010000
519900***************************************************************** 56020000
519600 1930-MOVE-CST-GRP-PROF.                                          56030000
519600** THIS PARAGRAPH IS PERFORMED WHEN ACCESS TO COST GROUPING       56040000
519600** TABLE RETURNED A VALUE OR "NOT FOUND"                          56050000
519600** IF FOUND - POPULATE THE VALUE ON SF AND CHECK FOR PRLF6 ERROR  56060000
519600** IF NOT FOUND - CHECK FOR PRLF6 ERROR                           56070000
519600                                                                  56080000
519600     IF  FM1E0-CLASS-PROV-CLM        > SPACE                      56090000
           AND FM1E0-PERF-PROV-NUM-CLM     > SPACE                      56100000
      ** COST GROUP CODE S/B POPULATED ON THE CLAIM LEVEL               56110000
519600        IF FM1E0-FLEX-NETWK-GRP-CLM  = SPACE                      56120000
519600           MOVE WS-PDR-CD TO FM1E0-FLEX-NETWK-GRP-CLM             56130000
519600        ELSE                                                      56140000
519600           IF FM1E0-FLEX-NETWK-GRP-CLM  = WS-PDR-CD               56150000
                    CONTINUE                                            56160000
519600           ELSE                                                   56170000
                    MOVE 'Y'          TO WS-RL6F-SW                     56180000
519600              MOVE WS-PDR-CD    TO FM1E0-FLEX-NETWK-GRP-CLM       56190000
519600           END-IF                                                 56200000
519600        END-IF                                                    56210000
519600        IF WS-CST-ENTERED-SW     = 'Y'                            56220000
519600           MOVE SPACE           TO WS-PDR-CD                      56230000
519600           PERFORM 1910-MAP-CST-GRP-PROF THRU 1910-EXIT           56240000
                 MOVE 'Y'             TO WS-RL6F-SW                     56250000
519600        END-IF                                                    56260000
              GO TO 1930-EXIT                                           56270000
519600     END-IF.                                                      56280000
519600                                                                  56290000
519600** IF WE ARE HERE, EITHER CLASS OR ID OR BOTH ARE ON THE          56300000
519600** LINE LEVEL. POPULATE COST GROUPING ON THE LINE                 56310000
519600     PERFORM VARYING WS-SUB FROM 1 BY 1                           56320000
519600        UNTIL WS-SUB  > CIABP-F0-CTR                              56330000
519600        MOVE CIASC-F0-1-999-ITEM (WS-SUB)  TO FM1F0-RECORD        56340000
519600        MOVE CIASC-F5-1-999-ITEM (WS-SUB)  TO FM1F5-RECORD        56350000
              IF FM1F0-CLASS-PROV-LN       = ' ' OR '5' OR 'S' OR 'T'   56360000
                 IF FM1E0-PERF-PROV-NUM-CLM = D04-BCBS-PROV-ID          56370000
                 OR FM1F0-PERF-PROV-NUM-LN  = D04-BCBS-PROV-ID          56380000
                    IF FM1F5-FLEX-NETWK-GRP-LN = SPACE                  56390000
519600                 MOVE WS-PDR-CD    TO FM1F5-FLEX-NETWK-GRP-LN     56400000
519600                 MOVE FM1F5-RECORD TO                             56410000
519600                           CIASC-F5-1-999-ITEM (WS-SUB)           56420000
                    ELSE                                                56430000
                       IF FM1F5-FLEX-NETWK-GRP-LN = WS-PDR-CD           56440000
                          CONTINUE                                      56450000
                       ELSE                                             56460000
                          MOVE WS-PDR-CD TO FM1F5-FLEX-NETWK-GRP-LN     56470000
                          MOVE 'Y'       TO WS-RL6F-SW                  56480000
519600                    MOVE FM1F5-RECORD TO                          56490000
519600                           CIASC-F5-1-999-ITEM (WS-SUB)           56500000
                       END-IF                                           56510000
                    END-IF                                              56520000
                 ELSE                                                   56530000
                    CONTINUE                                            56540000
              ELSE                                                      56550000
                 IF FM1F5-FLEX-NETWK-GRP-LN > SPACE                     56560000
                    MOVE 'Y'               TO WS-RL6F-SW                56570000
                    MOVE SPACE             TO FM1F5-FLEX-NETWK-GRP-LN   56580000
519600              MOVE FM1F5-RECORD      TO                           56590000
519600                           CIASC-F5-1-999-ITEM (WS-SUB)           56600000
                 END-IF                                                 56610000
              END-IF                                                    56620000
519600     END-PERFORM.                                                 56630000
519600                                                                  56640000
519700 1930-EXIT. EXIT.                                                 56650000
519900***************************************************************** 56660000
519600 1940-INST-COST-GRP-ACCESS.                                       56670000
519600                                                                  56680000
519600     MOVE FM105-BOID             TO D04-BUS-OWNER-ID.             56690000
519600     MOVE FM110-BCBS-PROV-NUM    TO D04-BCBS-PROV-ID.             56700000
519600     MOVE CIABP-INCUR-DT(1:4)    TO D04-DATE(1:4).                56710000
519600     MOVE CIABP-INCUR-DT(5:2)    TO D04-DATE(6:2).                56720000
519600     MOVE CIABP-INCUR-DT(7:2)    TO D04-DATE(9:2).                56730000
519600     MOVE '-'                    TO D04-DATE(5:1)                 56740000
519600                                    D04-DATE(8:1).                56750000
519600                                                                  56760000
519600     PERFORM 1950-ACCESS-CST-GRP-TBL    THROUGH 1950-EXIT.        56770000
519600                                                                  56780000
519600     IF CIABP-EOJ                                                 56790000
519600        GO TO 1940-EXIT                                           56800000
519600     END-IF.                                                      56810000
519600                                                                  56820000
           IF FM115-FLEX-NETWK-GRP-CLM = SPACE                          56830000
              MOVE WS-PDR-CD       TO FM115-FLEX-NETWK-GRP-CLM          56840000
           ELSE                                                         56850000
              IF FM115-FLEX-NETWK-GRP-CLM = WS-PDR-CD                   56860000
                 NEXT SENTENCE                                          56870000
              ELSE                                                      56880000
                 MOVE WS-PDR-CD    TO FM115-FLEX-NETWK-GRP-CLM          56890000
                 MOVE 'Y'          TO WS-RL6F-SW                        56900000
519600        END-IF                                                    56910000
519600     END-IF.                                                      56920000
519600                                                                  56930000
519700 1940-EXIT. EXIT.                                                 56940000
519900***************************************************************** 56950000
519600 1950-ACCESS-CST-GRP-TBL.                                         56960000
519600                                                                  56970000
519600     MOVE 'SELECT'               TO CSAPI-COMMAND.                56980000
519600     MOVE 1                      TO CSAPI-SUB-COMMAND.            56990000
519600     SET  WS-ITDD04S1            TO TRUE.                         57000000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    58100000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   58110000
519600     CALL WS-CALL-MODULE      USING WS-ITCCSAPI                   57010000
                                          D04-DATA-AREA.                57020000
           EVALUATE CSAPI-RETURN-CODE                                   57030000
              WHEN +0                                                   57040000
                 MOVE D04-FXBL-NETW-PROV-COST-GRP-CD                    57050000
                                TO WS-PDR-CD                            57060000
              WHEN +100                                                 57070000
                 MOVE SPACE     TO WS-PDR-CD                            57080000
              WHEN OTHER                                                57090000
                 MOVE 'SF027'   TO CIASC-FIRST-SCCF-EDIT-ERR-CD(1)      57100000
                                   SIAPC-RETURN-MESSAGE-CD              57110000
                 DISPLAY 'ITCP9102-BAD SELECT IPP_PROVIDER_COST_GROUP'  57120000
                 DISPLAY CSAPI-RETURN-MESSAGE                           57130000
                 PERFORM 9990-ERROR-DISPLAY         THRU 9990-EXIT      57140000
                 GO TO 1950-EXIT                                        57150000
           END-EVALUATE.                                                57160000
                                                                        57170000
519700 1950-EXIT. EXIT.                                                 57180000
519900***************************************************************** 57190000
       1960-SET-WS-CST-GRP-TABLE.                                       57200000
                                                                        57210000
      ** E_118591 CHECK FOR FLXBL-NETW-IND RATHER THAN DELV-MTD         57211000
519600**   IF  CIABP-DELV-MTD-IND = '2'                                 57220000
519600     IF  CIAPP2-FLXBL-NETW-IND   = 'Y'                            57221000
           AND ((FM1E0-CLASS-PROV-CLM  = ' ' OR '5' OR 'S' OR 'T') OR   57230000
                 WS-CST-GRP-CLASS-LN   = 'Y')                           57240000
               NEXT SENTENCE                                            57250000
           ELSE                                                         57260000
               GO TO 1960-EXIT                                          57270000
           END-IF.                                                      57280000
                                                                        57290000
      ** 2 FIELDS: PRV CLASS AND PERF PRV ID EACH COULD BE EITHER ON    57300000
      ** CLAIM OR LINE, BUT NOT BOTH. THEY COULD BE ON THE SAME LEVEL OR57310000
      ** DIFFERENT. F.E. ID ON CLAIM AND CLASS ON LINE OR V.C.          57320000
      ** IF BOTH ARE ON CLAIM AND CLASS = 5, S, OR T                    57330000
      **         MAP 1-ST OCCUR OF CST-GRP, AND                         57340000
      **         USE EARLIEST SERVICE DATE                              57350000
      ** IF BOTH ON LINE - MAP EACH DISTINCT ID, WHICH HAS CLASS OF     57360000
      **         5, S, T. DETERMINE THE EARLIEST SERVICE DATE FOR       57370000
      **         THIS PROVIDER, NOT THE WHOLE CLAIM                     57380000
      ** IF ID ON CLM AND CLASS ON LINE - MAP 1-ST OCCUR OF CST-GRP.    57390000
      **         DATE IS THE EARLIEST SERVICE DATE                      57400000
      ** IF ID ON LINE AND CLASS ON CLM - MAP EACH DISTINCT ID.         57410000
      **         DATE IS THE EARLIEST SERVICE DATE FOR THIS PROVIDER    57420000
                                                                        57430000
      ** BOTH PROVIDER CLASS AND PROVIDER ID ARE ON THE CLAIM LEVEL     57440000
           IF FM1E0-CLASS-PROV-CLM          > SPACE                     57450000
              IF FM1E0-PERF-PROV-NUM-CLM    > SPACE                     57460000
                 IF WS-CST-GRP-PROV(1)      > SPACE                     57470000
                    GO TO 1960-EXIT                                     57480000
                 ELSE                                                   57490000
                    MOVE FM1E0-PERF-PROV-NUM-CLM                        57500000
                                           TO WS-CST-GRP-PROV(1)        57510000
                    MOVE CIABP-INCUR-DT TO WS-CST-GRP-DT(1)             57520000
                    GO TO 1960-EXIT                                     57530000
                 END-IF                                                 57540000
              ELSE                                                      57550000
                 NEXT SENTENCE                                          57560000
              END-IF                                                    57570000
           END-IF.                                                      57580000
                                                                        57590000
      ** PRV ID ON THE CLAIM, CLASS ON THE LINE - MAP 1-ST OCCUR        57600000
           IF FM1E0-PERF-PROV-NUM-CLM         > SPACE                   57610000
              IF WS-CST-GRP-PROV(1)           = SPACE                   57620000
                 MOVE FM1E0-PERF-PROV-NUM-CLM TO WS-CST-GRP-PROV(1)     57630000
                 MOVE CIABP-INCUR-DT          TO WS-CST-GRP-DT(1)       57640000
              END-IF                                                    57650000
              GO TO 1960-EXIT                                           57660000
           END-IF.                                                      57670000
                                                                        57680000
           PERFORM VARYING WS-SUB FROM 1 BY 1                           57690000
              UNTIL WS-SUB > CIABP-F0-CTR                               57700000
              MOVE CIASC-F0-1-999-ITEM (WS-SUB)  TO FM1F0-RECORD        57710000
              IF FM1F0-CLASS-PROV-LN  = ' ' OR '5' OR 'S' OR 'T'        57720000
                 PERFORM 1964-SET-WS-CST-GRP-LINE   THROUGH 1964-EXIT   57730000
                    VARYING WS-PDR-SUB FROM 1 BY 1                      57740000
                    UNTIL WS-PDR-SUB  > 10                              57750000
              END-IF                                                    57760000
           END-PERFORM.                                                 57770000
                                                                        57780000
519700 1960-EXIT. EXIT.                                                 57790000
519900***************************************************************** 57800000
       1964-SET-WS-CST-GRP-LINE.                                        57810000
                                                                        57820000
      ** CLASS PROVIDER IS ON THE CLAIM OR LINE AND = 5, S, OR 'T'.     57830000
      ** PROVIDER ID IS ON THE LINE - POPULATE CST-GRP TABLE WITH       57840000
      ** DISTINCT PROVIDER ID AND EARLIEST F0- DATES FOR THIS PROVIDER  57850000
                                                                        57860000
           IF WS-CST-GRP-PROV(WS-PDR-SUB)   = SPACE                     57870000
              MOVE FM1F0-PERF-PROV-NUM-LN  TO                           57880000
                           WS-CST-GRP-PROV(WS-PDR-SUB)                  57890000
              MOVE FM1F0-DT-SERV-START     TO                           57900000
                           WS-CST-GRP-DT(WS-PDR-SUB)                    57910000
              MOVE 10                      TO WS-PDR-SUB                57920000
           ELSE                                                         57930000
           IF WS-CST-GRP-PROV(WS-PDR-SUB)  = FM1F0-PERF-PROV-NUM-LN     57940000
              IF FM1F0-DT-SERV-START       < WS-CST-GRP-DT(WS-PDR-SUB)  57950000
                 MOVE FM1F0-DT-SERV-START TO                            57960000
                        WS-CST-GRP-DT(WS-PDR-SUB)                       57970000
              END-IF                                                    57980000
              MOVE 10                        TO WS-PDR-SUB              57990000
           ELSE                                                         58000000
              CONTINUE                                                  58010000
           END-IF                                                       58020000
           END-IF.                                                      58030000
                                                                        58040000
519700 1964-EXIT. EXIT.                                                 58050000
806330***************************************************************** 00366600
806331 2000-GET-AEA-AMT.                                                00366700
208600                                                                  ITCP9102
806333     INITIALIZE                         C01-DATA-AREA             00366900
806334                                        CSAPI-RETURN-MESSAGE      00367000
806335                                        CSAPI-RETURN-CODE.        00367100
208900                                                                  ITCP9102
806337     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     00367300
806338     MOVE 2                          TO CSAPI-SUB-COMMAND.        00367400
806339     MOVE 'SELECT'                   TO CSAPI-COMMAND.            00367500
209400                                                                  ITCP9102
806341     MOVE 'CLAIM TYPE'               TO C01-REF-SUBJ-AREA-NM.     00367700
806342     IF WS-PROFESSIONAL                                           00367800
806343        MOVE '1'                     TO C01-REF-KEY-CD(1:1)       00367900
806344     ELSE                                                         00368000
806345        MOVE '2'                     TO C01-REF-KEY-CD(1:1).      00368100
806350                                                                  00368200
806360     MOVE WS-D09-PRODUCT-CODE        TO C01-REF-KEY-CD(2:1).      00368300
806370     MOVE WS-RCPT-DT                 TO C01-REF-END-DT.           00368400
806371                                                                  00368500
806372     SET  WS-ITDC01S1                TO TRUE.                     00368600
806373     CALL WS-CALL-MODULE       USING WS-ITCCSAPI,                 00368700
806374                                     C01-DATA-AREA.               00368800
806375                                                                  00368900
806376     IF CSAPI-RETURN-CODE = ZERO                                  00369000
806377        MOVE C01-REF-VAL-TXT (1:5)  TO WS-AEA-AMT-X               00369100
806378     ELSE                                                         00369200
806379     IF CSAPI-RETURN-CODE = +100                                  00369300
806380        PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT             00369400
806390*                                                                 00369500
806400*.... PRL1O/IRL2A (RECORD NOT FOUND ON AEA AMOUNT TABLE)          00369600
806500*                                                                 00369700
806600        IF WS-PROFESSIONAL                                        00369800
806700           MOVE 'PRL1O'        TO FM1A5-ERR-CD (WS-SUB1)          00369900
806800           MOVE '4'            TO CIAEP-ADMIN-EXP-ALLW-IND
806900        ELSE                                                      00370000
807000           MOVE 'IRL2A'        TO FM105-ERR-CD (WS-SUB1)          00370100
807100           MOVE '4'            TO CIAEI-ADMIN-EXP-ALLW-IND
807101        END-IF                                                    00370200
807102        MOVE 'AEA AMT NOT IN REF TBL'  TO SIAPC-ERROR-MESSAGE     00370300
807103     ELSE                                                         00370500
807104        MOVE CSAPI-RETURN-MESSAGE   TO SIAPC-ERROR-MESSAGE        00370600
807105        MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND           00370700
807106        MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS        00370800
410500        MOVE 'SF018'            TO SIAPC-RETURN-MESSAGE-CD        41050000
410600                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)41060000
807108        PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT.            00371000
807109                                                                  00371100
807110 2000-EXIT.  EXIT.                                                00371200
807111******************************************************            00371300
448700*R11.4 E_38714 DSL BEGIN - DELETE PARAGRAPH 2100-LOAD-UNIQUE..    41110000
448710*   2110-LOAD-UNIQUE..., 2200-LOAD-UNIQUE-REV                     41110000
807409******************************************************            00386700
485180                                                                  40670000
467000 2300-ACCESS-REF-F0.                                              42880000
807411                                                                  00386900
467200*                                                                 42930000
807448     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     00389700
807449     MOVE 2                          TO CSAPI-SUB-COMMAND.        00389800
807450     MOVE 'SELECT'                   TO CSAPI-COMMAND.            00389900
210400                                                                  ITCP9102
807453     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      00390200
807454     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      00390300
807455     MOVE '-'                        TO C01-REF-END-DT(5:1).      00390400
807456     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      00390500
807457     MOVE '-'                        TO C01-REF-END-DT(8:1).      00390600
807458     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      00390700
468300*                                                                 43050000
807460     SET  WS-ITDC01S1                TO TRUE.                     00390900
211200                                                                  ITCP9102
526800*R16.0 E_101251 PMP BEGIN                                         54390000
577600     IF  FM1A5-DEL-MTD NOT     = '8'                              59470000
577600     OR (FM1E0-CLASS-PROV-CLM  = '3' OR '9' OR 'E' OR 'G')        59470000
               MOVE 'X'             TO WS-PROC-CD-FLAG(1)
           END-IF.
526800*R16.0 E_101251 PMP END                                           54390000
541500                                                                  55860000
807480     PERFORM VARYING WS-SUBR FROM 1 BY 1                          00391100
468700         UNTIL WS-SUBR          > CIABP-F0-CTR                    43100000
807600         OR    CIABP-EOJ                                          00391400
211800                                                                  ITCP9102
469000         MOVE CIASC-F0-1-999-ITEM (WS-SUBR) TO FM1F0-RECORD       43140000
469100                                                                  43130000
469200         IF FM1F0-PLACE-SERV >=  SPACE                            41190000
469300         PERFORM VARYING WS-SUB3 FROM 1 BY 1                      43090000
487600            UNTIL WS-SUB3       > 100                             43100000
469500            IF WS-REF-POS (WS-SUB3) = FM1F0-PLACE-SERV            43150000
469600               IF WS-REF-POS-FLAG (WS-SUB3) = 'V'                 43150000
469700                  CONTINUE                                        43160000
469800               ELSE                                               43150000
469900*                                                                 26750000
470000*.... PP049 PLACE OF SERVICE REQUIRED AND MUST BE A VALID VALUE   26760000
470100*                                                                 26770000
470200                  MOVE '2' TO CIAEP-PLACE-SERV-IND (WS-SUBR)      43160000
470300               END-IF                                             43150000
488600               MOVE 100 TO WS-SUB3                                43160000
470500            ELSE                                                  43150000
470600               IF WS-REF-POS (WS-SUB3) = SPACES                   43150000
470700                  MOVE 'SERVICE PLACE' TO C01-REF-SUBJ-AREA-NM    42980000
470800                  MOVE FM1F0-PLACE-SERV TO C01-REF-KEY-CD
470900                  PERFORM 2300A-ACCESS-REF                        43150000
471000                  PERFORM 2300B-REF-POS-RESULT                    43150000
489300                  MOVE 100 TO WS-SUB3                             43160000
471200               END-IF                                             43150000
471300            END-IF                                                43150000
471400         END-PERFORM                                              43360000
471500         END-IF                                                   43150000
471600                                                                  41190000
489810*R13.0.1 E_82574 DSL LOOP THROUGH 150 TIMES FROM 50
471700         IF FM1F0-TYPE-SERV  >=  SPACE                            41190000
471800         PERFORM VARYING WS-SUB3 FROM 1 BY 1                      43090000
490100            UNTIL WS-SUB3       > 150                             43100000
472000            IF WS-REF-TOS (WS-SUB3) = FM1F0-TYPE-SERV             43150000
472100               IF WS-REF-TOS-FLAG (WS-SUB3) = 'V'                 43150000
472200                  CONTINUE                                        43160000
472300               ELSE                                               43150000
472400*                                                                 26750000
472500*.... PT058 TYPE OF SERVICE REQUIRED AND MUST BE VALID VALUE      26760000
472600*                                                                 26770000
472700                  MOVE '2' TO CIAEP-TYPE-SERV-IND (WS-SUBR)       43160000
472800               END-IF                                             43150000
491100               MOVE 150 TO WS-SUB3                                43160000
473000            ELSE                                                  43150000
473100               IF WS-REF-TOS (WS-SUB3) = SPACES                   43150000
473200                  MOVE 'SERVICE TYPE' TO C01-REF-SUBJ-AREA-NM     42980000
473300                  MOVE FM1F0-TYPE-SERV TO C01-REF-KEY-CD
473400                  PERFORM 2300A-ACCESS-REF                        43150000
473500                  PERFORM 2300E-REF-TOS-RESULT                    43150000
491800                  MOVE 150 TO WS-SUB3                             43160000
473700               END-IF                                             43150000
473800            END-IF                                                43150000
473900         END-PERFORM                                              43360000
474000         END-IF                                                   43150000
474100                                                                  26380000
492310*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
474200         IF  FM1A5-REL-NUM  > '102'                               26380000
474300         AND FM1A5-PGM-CD   = '9'                                 26390000
474400         AND (FM1A5-DEL-MTD = '7' OR 'A' OR 'B' OR 'C' OR 'D' OR  26400000
474500                              'E' OR 'F')                         26410000
474600            IF FM1F0-PERF-PROV-OFC-ZIP-CD5 NOT NUMERIC            41520000
474700            OR FM1F0-PERF-PROV-OFC-ZIP-CD4 NUMERIC                41530000
474800                CONTINUE                                          26410000
474900            ELSE                                                  41520000
475000              PERFORM VARYING WS-SUB3 FROM 1 BY 1                 43090000
493300                 UNTIL WS-SUB3  > 800                             43100000
475200                IF FM1F0-PERF-PROV-OFC-ZIP-CD5 =                  41520000
475300                                            WS-REF-ZIP (WS-SUB3)        00
475400                    IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'            43150000
475500*                                                                 26750000
475600*.... PRL2J (PERFORMING PROVIDER ZIP - LN MUST BE 9 NUMERIC FOR          0
475700*....        MED ADVANTAGE )                                             0
475800*                                                                 26770000
475900                       MOVE '4' TO                                43160000
476000                         CIAEP-PERF-PRV-OFC-ZIP-LN-IND (WS-SUBR)        00
476100                    ELSE                                          43150000
476200                       CONTINUE                                   43160000
476300                    END-IF                                        43150000
494600                    MOVE 800 TO WS-SUB3                           43160000
476500                ELSE                                              43150000
476600                    IF WS-REF-ZIP (WS-SUB3) = SPACES              43150000
476700                       MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM 42980000
476800                       MOVE FM1F0-PERF-PROV-OFC-ZIP-CD5 TO        41520000
476900                                                  C01-REF-KEY-CD  41520000
477000                                                  WS-ZIP          41520000
477100                       PERFORM 2300A-ACCESS-REF                   43150000
477200                       PERFORM 2300H-REF-ZIP-RESULT               43150000
477300*                                                                 26750000
477400*.... PRL2J (PERFORMING PROVIDER ZIP - LN MUST BE 9 NUMERIC FOR          0
477500*....        MED ADVANTAGE )                                             0
477600*                                                                 26770000
477700                       MOVE WS-ERR-CD (1:1) TO                    43240000
477800                          CIAEP-PERF-PRV-OFC-ZIP-LN-IND (WS-SUBR) 43240000
496100                       MOVE 800 TO WS-SUB3                        43160000
478000                    END-IF                                        43150000
478100                END-IF                                            43150000
478200              END-PERFORM                                         43360000
478300            END-IF                                                43360000
478400                                                                  41520000
496610*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
478500            IF FM1F0-SERV-FAC-LOC-ZIP5-LN NOT NUMERIC             41750000
478600            OR FM1F0-SERV-FAC-LOC-ZIP4-LN NUMERIC                 41760000
478700            OR FM1F0-TYPE-SERV = '9F0'                            41770000
478800                CONTINUE                                          26410000
478900            ELSE                                                  41520000
479000              PERFORM VARYING WS-SUB3 FROM 1 BY 1                 43090000
497300                 UNTIL WS-SUB3  > 800                             43100000
479200                IF FM1F0-SERV-FAC-LOC-ZIP5-LN  =                  41750000
479300                                            WS-REF-ZIP (WS-SUB3)        00
479400                    IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'            43150000
479500*                                                                 26750000
479600*.... PRL2L (SERVICE FACILITY LOC ZIP - LN MUST BE 9 NUMERIC FOR         0
479700*....        MED ADVANTAGE )                                             0
479800*                                                                 26770000
479900                       MOVE '4' TO                                43160000
480000                         CIAEP-SERV-FAC-LOC-ZIP-LN-IND (WS-SUBR)        00
480100                    ELSE                                          43150000
480200                       CONTINUE                                   43160000
480300                    END-IF                                        43150000
498600                    MOVE 800 TO WS-SUB3                           43160000
480500                ELSE                                              43150000
480600                    IF WS-REF-ZIP (WS-SUB3) = SPACES              43150000
480700                       MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM 42980000
480800                       MOVE FM1F0-SERV-FAC-LOC-ZIP5-LN  TO        41520000
480900                                                  C01-REF-KEY-CD  41520000
481000                                                  WS-ZIP          41520000
481100                       PERFORM 2300A-ACCESS-REF                   43150000
481200                       PERFORM 2300H-REF-ZIP-RESULT               43150000
481300*                                                                 26750000
481400*.... PRL2L (SERVICE FACILITY LOC ZIP - LN MUST BE 9 NUMERIC FOR         0
481500*....        MED ADVANTAGE )                                             0
481600*                                                                 26770000
481700                       MOVE WS-ERR-CD (1:1) TO                    43240000
481800                          CIAEP-SERV-FAC-LOC-ZIP-LN-IND (WS-SUBR)       00
500100                       MOVE 800 TO WS-SUB3                        43160000
482000                    END-IF                                        43150000
482100                END-IF                                            43150000
482200              END-PERFORM                                         43360000
482300            END-IF                                                43360000
541300         END-IF                                                   55840000
541300**R16.0 E_101251 PMP BEGIN                                        55840000
               IF WS-PROC-CD-FLAG(1) = 'X'
                  CONTINUE
               ELSE
541300            PERFORM 2310-ACCESS-NDC-PRC-CD-PROF THRU 2310-EXIT    55840000
576900            IF FM1F0-PERF-PROV-SPEC-LN > SPACE                    59400000
586100               MOVE FM1F0-PERF-PROV-SPEC-LN  TO C01-REF-KEY-CD    60320000
576900               PERFORM 2320-ACCESS-ATYPICAL-PROV  THRU 2320-EXIT  59400000
576900            END-IF                                                59400000
482600         END-IF                                                   43360000
482700     END-PERFORM.                                                 43360000
541500                                                                  55860000
           IF WS-PROC-CD-FLAG(1) = 'X'
              NEXT SENTENCE
           ELSE
541500        PERFORM 2311-PRL5X-PRL5W-EDIT THRU 2311-EXIT              55860000
           END-IF.
526800*R16.0 E_101251 PMP END                                           54390000
482800                                                                  43370000
541600*R16.5 E_97628 EXL  REMOVE PRLA3 EDIT                             55870000
541700*    IF FM1B5-AMB-PICK-UP-ZIP (1:5) > SPACES  AND                 55880000
541800*       FM1B5-AMB-PICK-UP-ZIP (6:4) NOT NUMERIC AND               55890000
541900*       FM1A5-REL-NUM >= '120'           AND                      55900000
542000*       FM1A5-PGM-CD = '9'               AND                      55910000
542100*       FM1A5-DEL-MTD = ('A' OR 'B' OR 'C' OR 'D' OR 'E' OR 'F'   55920000
542200*                            OR '7')     AND                      55930000
542300*       FM1A5-TYPE-SUBM = ('1' OR '2')   AND                      55940000
542400*       FM1A5-EST-IND NOT = 'E'          AND                      55950000
542500*       FM1A5-INTL-CD NOT = 'I'                                   55960000
502010*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
542700*       PERFORM VARYING WS-SUB3 FROM 1 BY 1                       55980000
542800*               UNTIL WS-SUB3 > 800                               55990000
542900*         IF FM1B5-AMB-PICK-UP-ZIP (1:5) =                        56000000
543000*                                     WS-REF-ZIP (WS-SUB3)        56010000
543100*            IF  WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'                  56020000
482857*                                                                 26750000
482858*.... PRLA3 (AMBULANCE PICKUP ZIP MUST BE 9 NUMERIC FOR                  0
482859*....        MED ADVANTAGE )                                             0
482860*                                                                 26770000
543600*                MOVE '4' TO CIAEP-AMB-PICK-UP-ZIP-IND            56070000
543700*            END-IF                                               56080000
543800*            MOVE 800 TO WS-SUB3                                  56090000
543900*         ELSE                                                    56100000
544000*           IF WS-REF-ZIP (WS-SUB3) = SPACES                      56110000
544100*              MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM         56120000
544200*              MOVE FM1B5-AMB-PICK-UP-ZIP (1:5) TO C01-REF-KEY-CD 56130000
544300*              PERFORM 2300A-ACCESS-REF                           56140000
544400*              PERFORM 2300H-REF-ZIP-RESULT                       56150000
544500*              MOVE WS-ERR-CD (1:1) TO                            56160000
544600*                    CIAEP-AMB-PICK-UP-ZIP-IND                    56170000
544700*              MOVE 800 TO WS-SUB3                                56180000
482892*                                                                 26750000
482893*.... PRLA3 (AMBULANCE PICKUP ZIP MUST BE 9 NUMERIC FOR                  0
482894*....        MED ADVANTAGE )                                             0
482895*                                                                 26770000
545200*           END-IF                                                56230000
545300*         END-IF                                                  56240000
545400*       END-PERFORM                                               56250000
545500*    END-IF.                                                      56260000
482904                                                                  41520000
482900 2300-EXIT. EXIT.                                                 43380000
545800***********************************************************       56290000
483000 2300A-ACCESS-REF.                                                42880000
483100     CALL WS-CALL-MODULE         USING WS-ITCCSAPI,               43670000
807901                                       C01-DATA-AREA              00391800
483300     .                                                            43680000
483400 2300A-EXIT.    EXIT.                                             43380000
545800***********************************************************       56290000
483500 2300B-REF-POS-RESULT.                                            42880000
483600      IF CSAPI-RETURN-CODE = ZERO                                 43170000
483700         MOVE FM1F0-PLACE-SERV TO WS-REF-POS (WS-SUB3)            43240000
483800         MOVE 'V' TO WS-REF-POS-FLAG         (WS-SUB3)            43240000
483900         CONTINUE                                                 43180000
484000      ELSE                                                        43190000
807905         IF CSAPI-RETURN-CODE = + 100                             00392200
484200            MOVE FM1F0-PLACE-SERV TO WS-REF-POS (WS-SUB3)         43240000
484300            MOVE 'I' TO WS-REF-POS-FLAG         (WS-SUB3)         43240000
484400*                                                                 26750000
484500*.... PP049 PLACE OF SERVICE REQUIRED AND MUST BE A VALID VALUE   26760000
484600*                                                                 26770000
484700            MOVE '2'  TO CIAEP-PLACE-SERV-IND (WS-SUBR)           43240000
807912         ELSE                                                     00393100
807913            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     00393200
807914            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        00393300
807915            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     00393400
433100            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
433200                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
807917            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         00393600
368901         END-IF                                                   04186
485600      END-IF.                                                     43350000
485700 2300B-EXIT.    EXIT.                                             43380000
485800                                                                  43350000
485900 2300E-REF-TOS-RESULT.                                            42880000
486000      IF CSAPI-RETURN-CODE = ZERO                                 43170000
486100         MOVE FM1F0-TYPE-SERV  TO WS-REF-TOS (WS-SUB3)            43240000
486200         MOVE 'V' TO WS-REF-TOS-FLAG         (WS-SUB3)            43240000
486300         CONTINUE                                                 43180000
486400      ELSE                                                        43190000
486500         IF CSAPI-RETURN-CODE = + 100                             43200000
486600            MOVE FM1F0-TYPE-SERV  TO WS-REF-TOS (WS-SUB3)         43240000
486700            MOVE 'I' TO WS-REF-TOS-FLAG         (WS-SUB3)         43240000
486800*                                                                 26750000
486900*.... PT058 TYPE OF SERVICE REQUIRED AND MUST BE VALID VALUE      26760000
487000*                                                                 26770000
487100            MOVE '2'  TO CIAEP-TYPE-SERV-IND  (WS-SUBR)           43240000
487200         ELSE                                                     43240000
487300            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     43280000
487400            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        43290000
487500            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     43300000
487600            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
487700                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
487800            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         43330000
807919         END-IF                                                   00393800
488000      END-IF.                                                     43350000
488100 2300E-EXIT.    EXIT.                                             43380000
370305                                                                  05415
488300 2300H-REF-ZIP-RESULT.                                            42880000
488400      MOVE SPACE             TO WS-ERR-CD                         43240000
488500      IF CSAPI-RETURN-CODE = ZERO                                 43170000
488600         MOVE WS-ZIP         TO WS-REF-ZIP (WS-SUB3)              41520000
488700         MOVE 'Y'            TO WS-REF-ZIP-FLAG   (WS-SUB3)       43240000
488800         MOVE '4'            TO WS-ERR-CD (1:1)                   43240000
488900*        MOVE '4' TO CIAEP-PERF-PRV-OFC-ZIP-LN-IND (WS-SUBR)      43240000
489000         CONTINUE                                                 43180000
489100      ELSE                                                        43190000
489200         IF CSAPI-RETURN-CODE = + 100                             43200000
489300            MOVE WS-ZIP     TO WS-REF-ZIP (WS-SUB3)               41520000
489400            MOVE 'N'        TO WS-REF-ZIP-FLAG  (WS-SUB3)         43240000
489500         ELSE                                                     43240000
489600            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     43280000
489700            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        43290000
489800            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     43300000
489900            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
490000                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
490100            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         43330000
490200         END-IF                                                   43340000
490300      END-IF.                                                     43350000
490400 2300H-EXIT.    EXIT.                                             43380000
490500******************************************************            43390000
490700 2305-ACCESS-REF-F1.                                              42880000
221900                                                                  ITCP9102
807930     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     00394900
807940     MOVE 2                          TO CSAPI-SUB-COMMAND.        00395000
807950     MOVE 'SELECT'                   TO CSAPI-COMMAND.            00395100
222100                                                                  ITCP9102
807953     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      00395400
807954     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      00395500
807955     MOVE '-'                        TO C01-REF-END-DT(5:1).      00395600
807956     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      00395700
807957     MOVE '-'                        TO C01-REF-END-DT(8:1).      00395800
807958     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      00395900
492000*                                                                 43050000
807960     SET  WS-ITDC01S1                TO TRUE.                     00396100
807970                                                                  00396200
807980     PERFORM VARYING WS-SUBR FROM 1 BY 1                          00396300
492400         UNTIL WS-SUBR          > CIABP-F1-CTR                    43100000
808100         OR    CIABP-EOJ                                          00396600
808200                                                                  00396700
492700         MOVE CIASC-F1-1-999-ITEM (WS-SUBR) TO FM1F1-RECORD       43140000
492800                                                                  43130000
515210*R13.0.1 E_82574 DSL LOOP THROUGH 100 TIMES FROM 50
492900         IF FM1F1-PLACE-SERV >=  SPACE                            41190000
493000         PERFORM VARYING WS-SUB3 FROM 1 BY 1                      43090000
515500            UNTIL WS-SUB3       > 100                             43100000
493200            IF WS-REF-POS (WS-SUB3) = FM1F1-PLACE-SERV            43150000
493300               IF WS-REF-POS-FLAG (WS-SUB3) = 'V'                 43150000
493400                  CONTINUE                                        43160000
493500               ELSE                                               43150000
493600                  MOVE '2' TO CIAEP-PLACE-SERV-DN-IND (WS-SUBR)   43160000
493700               END-IF                                             43150000
516200               MOVE 100 TO WS-SUB3                                43160000
493900            ELSE                                                  43150000
494000               IF WS-REF-POS (WS-SUB3) = SPACES                   43150000
494100                  MOVE 'SERVICE PLACE' TO C01-REF-SUBJ-AREA-NM    42980000
494200                  MOVE FM1F1-PLACE-SERV TO C01-REF-KEY-CD
494300                  PERFORM 2300A-ACCESS-REF                        43150000
494400                  PERFORM 2300B-REF-POS-RESULT                    43150000
516900                  MOVE 100 TO WS-SUB3                             43160000
494600               END-IF                                             43150000
808421            END-IF
494800         END-PERFORM                                              43360000
808428         END-IF                                                   00398900
495000                                                                  41190000
517410*R13.0.1 E_82574 DSL LOOP THROUGH 150 TIMES FROM 50
495100         IF FM1F1-TYPE-SERV  >=  SPACE                            41190000
495200         PERFORM VARYING WS-SUB3 FROM 1 BY 1                      43090000
517700            UNTIL WS-SUB3       > 150                             43100000
495400            IF WS-REF-TOS (WS-SUB3) = FM1F1-TYPE-SERV             43150000
495500               IF WS-REF-TOS-FLAG (WS-SUB3) = 'V'                 43150000
495600                  CONTINUE                                        43160000
495700               ELSE                                               43150000
495800                  MOVE '2' TO CIAEP-TYPE-SERV-DN-IND (WS-SUBR)    43160000
495900               END-IF                                             43150000
518400               MOVE 150 TO WS-SUB3                                43160000
496100            ELSE                                                  43150000
496200               IF WS-REF-TOS (WS-SUB3) = SPACES                   43150000
496300                  MOVE 'SERVICE TYPE' TO C01-REF-SUBJ-AREA-NM     42980000
496400                  MOVE FM1F1-TYPE-SERV TO C01-REF-KEY-CD
496500                  PERFORM 2300A-ACCESS-REF                        43150000
496600                  PERFORM 2300E-REF-TOS-RESULT                    43150000
519100                  MOVE 150 TO WS-SUB3                             43160000
496800               END-IF                                             43150000
496900            END-IF                                                43150000
497000         END-PERFORM                                              43360000
808429         END-IF                                                   00399000
808431                                                                  00399200
497300         IF  FM1A5-REL-NUM  > '102'                               26380000
497400         AND FM1A5-PGM-CD   = '9'                                 26390000
497500         AND (FM1A5-DEL-MTD = '7' OR 'A' OR 'B' OR 'C' OR 'D' OR  26400000
497600                              'E' OR 'F')                         26410000
497700            IF FM1F1-PERF-PROV-OFC-ZIP-CD5 NOT NUMERIC            41520000
497800            OR FM1F1-PERF-PROV-OFC-ZIP-CD4 NUMERIC                41530000
497900                CONTINUE                                          26410000
498000            ELSE                                                  41520000
520410*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
498100              PERFORM VARYING WS-SUB3 FROM 1 BY 1                 43090000
520600                 UNTIL WS-SUB3  > 800                             43100000
498300                IF FM1F1-PERF-PROV-OFC-ZIP-CD5 =                  41520000
498400                                            WS-REF-ZIP (WS-SUB3)        00
498500                    IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'            43150000
498600                       MOVE '4' TO                                43160000
498700                         CIAEP-PERF-PRV-OFC-ZIP-LN-IND (WS-SUBR)        00
498800                    ELSE                                          43150000
498900                       CONTINUE                                   43160000
499000                    END-IF                                        43150000
521500                    MOVE 800 TO WS-SUB3                           43160000
499200                ELSE                                              43150000
499300                    IF WS-REF-ZIP (WS-SUB3) = SPACES              43150000
499400                       MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM 42980000
499500                       MOVE FM1F1-PERF-PROV-OFC-ZIP-CD5 TO        41520000
499600                                                  C01-REF-KEY-CD  41520000
499700                       PERFORM 2300A-ACCESS-REF                   43150000
499800                       PERFORM 2300H-REF-ZIP-RESULT               43150000
522300                       MOVE 800 TO WS-SUB3                        43160000
500000                    END-IF                                        43150000
500100                END-IF                                            43150000
500200              END-PERFORM                                         43360000
500300            END-IF                                                43360000
809004         ELSE                                                     00402500
500500            GO TO 2305-EXIT                                       43360000
809053         END-IF                                                   00404200
809054     END-PERFORM.                                                 00404300
809055                                                                  00404400
500900 2305-EXIT. EXIT.                                                 43380000
564200*R16.0 E_101251 PMP BEGIN                                         58130000
564300**************************************************************    58140000
576800 2310-ACCESS-NDC-PRC-CD-PROF.                                     59390000
444210                                                                  42870000
576900     IF FM1F0-NDC-CD          > SPACES                            59400000
576900     OR FM1F0-HCPCS-PROC-CD   = SPACES                            59400000
576900     OR (FM1F0-CLASS-PROV-LN  = '3' OR '9' OR 'E' OR 'G')         59400000
576900         GO TO 2310-EXIT                                          59400000
576900     END-IF.                                                      59400000

577800     PERFORM VARYING WS-SUB3 FROM 1 BY 1                          59490000
577900       UNTIL WS-SUB3 > 999                                        59500000
578000       IF WS-PROC-CD(WS-SUB3)          =  FM1F0-HCPCS-PROC-CD     59510000
578100          IF WS-PROC-CD-FLAG (WS-SUB3) = 'N' OR 'X'               59520000
578400             MOVE 999           TO WS-SUB3                        59550000
578500          ELSE                                                    59560000
580200             IF WS-PROC-CD-FLAG (WS-SUB3) = SPACE                 59730000
586100                MOVE FM1F0-HCPCS-PROC-CD  TO WS-PROC-CD(WS-SUB3)  60320000
580200                PERFORM 2315-ACCESS-REF-PROC-CD-PROF              59730000
580200                   THRU 2315-EXIT                                 59730000
580200             END-IF                                               59730000
580200             IF WS-PROC-CD-FLAG (WS-SUB3) = 'Y'                   59730000
579200*                                                                 59630000
579000*.... PRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300                MOVE '1'        TO CIAEP-NDC-CD-IND(WS-SUBR)      59640000
580200             END-IF                                               59730000
580200             MOVE 999           TO WS-SUB3                        59730000
                END-IF
580300       ELSE                                                       59740000
578000          IF WS-PROC-CD(WS-SUB3)    =  SPACE                      59510000
586100             MOVE FM1F0-HCPCS-PROC-CD  TO WS-PROC-CD(WS-SUB3)     60320000
580500             PERFORM 2315-ACCESS-REF-PROC-CD-PROF THRU 2315-EXIT  59760000
580700             MOVE 999         TO WS-SUB3                          59780000
580900          END-IF                                                  59800000
580900       END-IF                                                     59800000
581000     END-PERFORM.                                                 59810000
583600                                                                  60070000
583700 2310-EXIT. EXIT.                                                 60080000
564300**************************************************************    58140000
541500 2311-PRL5X-PRL5W-EDIT.                                           55860000

           IF  WS-ATYPICAL-PROV-SW       = 'Y'
               GO TO 2311-EXIT
           END-IF.

           IF  FM1B0-PROV-NPI           = SPACE
           OR (FM1E0-PERF-PROV-NPI-CLM  = SPACE   AND
               WS-PROV-NPI-ALL-LN-BLANK = 'Y')
               IF FM1E0-PERF-PROV-SPEC-CLM > SPACE
                  MOVE FM1E0-PERF-PROV-SPEC-CLM  TO C01-REF-KEY-CD
541500            PERFORM 2320-ACCESS-ATYPICAL-PROV THRU 2320-EXIT      55860000
587200            IF CSAPI-RETURN-CODE = + 0                            60430000
                     MOVE 'Y'     TO WS-ATYPICAL-PROV-SW
                     GO TO 2311-EXIT
                  END-IF
               END-IF
           END-IF.
576900                                                                  59400000
           IF FM1B0-PROV-NPI            = SPACE
586100*....                                                             60320000
586100*.... PRL5X (PROVIDER NPI REQUIRED FOR THIS MEDICAID CLAIM)       60320000
586100*....                                                             60320000
579300         MOVE '1'       TO CIAEP-PROV-NPI-IND                     59640000
           END-IF.
576900                                                                  59400000
           IF  FM1E0-PERF-PROV-NPI-CLM  = SPACE
           AND WS-PROV-NPI-ALL-LN-BLANK = 'Y'
586100*....                                                             60320000
586100*.... PRL5W (PROVIDER NPI REQUIRED FOR THIS MEDICAID CLAIM)       60320000
586100*....                                                             60320000
579300         MOVE '1' TO CIAEP-PERF-PROV-NPI-CLM-IND                  59640000
586100         GO TO 2311-EXIT                                          60320000
586100     END-IF.                                                      60320000
541500                                                                  55860000
541500     IF  FM1E0-PERF-PROV-SPEC-CLM = '70'                          55860000
541300         IF WS-PROV-NPI-ANY-LN-BLANK = 'Y'                        55840000
579000*.... PRL5W (PERFORMING PROVIDER NPI REQUIRED FOR THIS            59610000
579000*....        MEDICAID CLAIM)                                      59610000
579300            MOVE '1'        TO CIAEP-PERF-PROV-NPI-CLM-IND        59640000
               END-IF
               GO TO 2311-EXIT
541300     END-IF.                                                      55840000
541500                                                                  55860000
583700 2311-EXIT. EXIT.                                                 60080000
583700***********************************************************       60080000
585800 2315-ACCESS-REF-PROC-CD-PROF.                                    60290000
586300                                                                  60340000
586100      MOVE WS-PROC-CD(WS-SUB3)  TO C01-REF-KEY-CD.                60320000
586100                                                                  60320000
586100      MOVE 'NDC PROC CODE'      TO C01-REF-SUBJ-AREA-NM.          60320000
586100      CALL WS-CALL-MODULE          USING WS-ITCCSAPI,             60320000
586100                                         C01-DATA-AREA.           60320000
586100                                                                  60320000
586500      IF CSAPI-RETURN-CODE = ZERO                                 60360000
586600         MOVE 'Y'       TO  WS-PROC-CD-FLAG(WS-SUB3)              60370000
579200*                                                                 59630000
579000*.... PRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300         MOVE '1'               TO CIAEP-NDC-CD-IND(WS-SUBR)      59640000
587100      ELSE                                                        60420000
587200         IF CSAPI-RETURN-CODE = + 100                             60430000
587300            MOVE 'N'    TO  WS-PROC-CD-FLAG(WS-SUB3)              60440000
587600         ELSE                                                     60470000
587700            DISPLAY '2315- BAD ACCESS REF PROC CD'                60480000
587700            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     60480000
587800            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        60490000
587900            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     60500000
588000            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        60510000
588100                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)60520000
588200            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         60530000
588300         END-IF                                                   60540000
588400      END-IF.                                                     60550000
586100                                                                  60320000
586200 2315-EXIT.      EXIT.                                            60330000
564300**************************************************************    58140000
576800 2320-ACCESS-ATYPICAL-PROV.                                       59390000
576900                                                                  59400000
576900     IF WS-ATYPICAL-PROV-SW  = 'Y'                                59400000
576900        GO TO 2320-EXIT                                           59400000
576900     END-IF.                                                      59400000
576900                                                                  59400000
586100     MOVE 'ATYPICAL PROV'          TO C01-REF-SUBJ-AREA-NM.       60320000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    65050000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   65060000
586100     CALL WS-CALL-MODULE           USING WS-ITCCSAPI,             60320000
586100                                         C01-DATA-AREA.           60320000
586500     IF CSAPI-RETURN-CODE = ZERO                                  60360000
587100        MOVE 'Y'         TO WS-ATYPICAL-PROV-SW                   60420000
587100     ELSE                                                         60420000
587200        IF CSAPI-RETURN-CODE = + 100                              60430000
                 NEXT SENTENCE
587600        ELSE                                                      60470000
587700           DISPLAY '2320- BAD ACCESS REF PROV CD'                 60480000
587700           MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE      60480000
587800           MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND         60490000
587900           MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS      60500000
588000           MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD         60510000
588100                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 60520000
588200           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT          60530000
588300        END-IF                                                    60540000
588400     END-IF.                                                      60550000
588400                                                                  60550000
583700 2320-EXIT. EXIT.                                                 60080000
586200*R16.0 E_101251 PMP END                                           60330000
564400**************************************************************    58150000
444220 2510-GET-REF-DATA-REL.                                           42880000
444230                                                                  42890000
444240     INITIALIZE                         C01-DATA-AREA             42900000
444250                                        CSAPI-RETURN-MESSAGE      42910000
444260                                        CSAPI-RETURN-CODE.        42920000
444270                                                                  42930000
444280     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     42940000
444290     MOVE 2                          TO CSAPI-SUB-COMMAND.        42950000
444291     MOVE 'SELECT'                   TO CSAPI-COMMAND.            42960000
444292                                                                  42970000
444293     MOVE 'PAT RELATION'             TO C01-REF-SUBJ-AREA-NM.     42980000
444294     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      42990000
444295     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      43000000
444296     MOVE '-'                        TO C01-REF-END-DT(5:1).      43010000
444297     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      43020000
444298     MOVE '-'                        TO C01-REF-END-DT(8:1).      43030000
444299     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      43040000
444300                                                                  43050000
444301     SET  WS-ITDC01S1                TO TRUE.                     43060000
444302                                                                  43070000
444304     PERFORM VARYING WS-SUBR FROM 1 BY 1                          43090000
444305         UNTIL WS-SUBR          > 3                               43100000
444306         OR    CIABP-EOJ                                          43110000
444307         OR    REF-REL-CD (WS-SUBR) = SPACE                       43120000
444308                                                                  43130000
444309         MOVE REF-REL-CD (WS-SUBR)  TO C01-REF-KEY-CD             43140000
444310         CALL WS-CALL-MODULE     USING WS-ITCCSAPI,               43150000
444311                                       C01-DATA-AREA              43160000
444312         IF CSAPI-RETURN-CODE = ZERO                              43170000
444313            MOVE C01-REF-END-DT TO REF-REL-END-DT   (WS-SUBR)     43180000
444314            MOVE C01-REF-EFF-DT TO REF-REL-START-DT (WS-SUBR)     43180000
444316         ELSE                                                     43190000
444317         IF CSAPI-RETURN-CODE = + 100                             43200000
444318            CONTINUE                                              43210000
444323         ELSE                                                     43270000
444324            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     43280000
444325            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        43290000
444326            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     43300000
444327            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
444328                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
444329            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         43330000
444330         END-IF                                                   43340000
444331         END-IF                                                   43350000
444332     END-PERFORM.                                                 43360000
444333                                                                  43370000
444334 2510-EXIT. EXIT.                                                 43380000
444335                                                                  42880000
444336 2520-GET-REF-SRC-PMT.                                            42880000
444337                                                                  42890000
444338     INITIALIZE                         C01-DATA-AREA             42900000
444339                                        CSAPI-RETURN-MESSAGE      42910000
444340                                        CSAPI-RETURN-CODE.        42920000
444341                                                                  42930000
444342     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     42940000
444343     MOVE 2                          TO CSAPI-SUB-COMMAND.        42950000
444344     MOVE 'SELECT'                   TO CSAPI-COMMAND.            42960000
444345                                                                  42970000
444346     MOVE 'PAYMENT SOURCE'           TO C01-REF-SUBJ-AREA-NM.     42980000
444347     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      42990000
444348     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      43000000
444349     MOVE '-'                        TO C01-REF-END-DT(5:1).      43010000
444350     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      43020000
444351     MOVE '-'                        TO C01-REF-END-DT(8:1).      43030000
444352     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      43040000
444353                                                                  43050000
444354     SET  WS-ITDC01S1                TO TRUE.                     43060000
444355                                                                  43070000
444356     PERFORM VARYING WS-SUBR FROM 1 BY 1                          43090000
444357         UNTIL WS-SUBR          > 3                               43100000
444358         OR    CIABP-EOJ                                          43110000
444359         OR    REF-SRC-PMT (WS-SUBR) = SPACE                      43120000
444360                                                                  43130000
444361         MOVE REF-SRC-PMT (WS-SUBR)  TO C01-REF-KEY-CD            43140000
444362         CALL WS-CALL-MODULE     USING WS-ITCCSAPI,               43150000
444363                                       C01-DATA-AREA              43160000
444364         IF CSAPI-RETURN-CODE = ZERO                              43170000
444365            MOVE C01-REF-END-DT TO REF-SRC-END-DT   (WS-SUBR)     43180000
444366            MOVE C01-REF-EFF-DT TO REF-SRC-START-DT (WS-SUBR)     43180000
444368         ELSE                                                     43190000
444369         IF CSAPI-RETURN-CODE = + 100                             43200000
444370            CONTINUE                                              43210000
444374         ELSE                                                     43270000
444375            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     43280000
444376            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        43290000
444377            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     43300000
444378            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
444379                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
444380            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         43330000
444381         END-IF                                                   43340000
444382         END-IF                                                   43350000
444383     END-PERFORM.                                                 43360000
444384                                                                  43370000
444385 2520-EXIT. EXIT.                                                 43380000
444386*11.3 E_10097 END                                                 42870000
444390                                                                  44430000
809057*******************************************************           00404600
809058 2600-PROCESS-REC-60.                                             00404700
809059                                                                  00404800
809060     MOVE  'N'           TO WS-SEMI-PRIV-RM-RT-IND.               00404900
809061     MOVE ZERO           TO CIABP-60-ACCOM-CTR.                   00405000
521700     PERFORM 2601-INIT-REF-REV     THRU 2601-EXIT.                44480000
577600*R16.0 E_101251 PMP BEGIN                                         59470000
577600     IF  FM105-DEL-MTD NOT     = '8'                              59470000
577600     OR (FM115-CLASS-PROV-CLM  = '3' OR '9' OR 'E' OR 'G')        59470000
               MOVE 'X'             TO WS-PROC-CD-FLAG(1)
           END-IF.

577600*R16.0 E_101251 PMP END                                           59470000
809062     PERFORM 2610-SET-REV-TYPE     THRU 2610-EXIT                 00405100
809063             VARYING WS-SUB FROM 1 BY 1                           00405200
809064             UNTIL WS-SUB > CIABP-60-CTR.                         00405300
522100*11.4 E_38714 DSL END                                             42870000
809065                                                                  00405400
809066*.... IRL2U (NUMBER OF ACCOMMODATION LINES IS > 100               00405500
809067     IF (WS-PARM-INST-SEQ-NUM-MAX < 999 AND                       00405600
809068         CIABP-60-ACCOM-CTR       > 22)                           00405700
809069     OR  CIABP-60-ACCOM-CTR > 100                                 00405800
522700         MOVE '5'  TO CIAEI-REV-TYPE-CD-IND (1)                   44560000
809071     END-IF.                                                      00406100
809072                                                                  00406200
809073     IF WS-SEMI-PRIV-RM-RT-IND = 'Y'                              00406300
809074        MOVE '2'   TO CIABP-INST-CLM-REC-IND                      00406400
809075     END-IF.                                                      00406500
809076                                                                  00406600
575200*R16.0 E_101251 PMP START                                         59230000
576100     IF  FM110-PROV-NPI = SPACES                                  59320000
576100     AND WS-PROC-CD-FLAG(1) NOT = 'X'                             59320000
575400*.... IRL5X (PROVIDER NPI REQUIRED FOR THIS MEDICAID CLAIM)       59250000
576200        MOVE '1'   TO CIAEI-PROV-NPI-IND                          59330000
576200     END-IF.                                                      59330000
575200*R16.0 E_101251 PMP END                                           59230000
576400                                                                  59350000
809077 2600-EXIT. EXIT.                                                 00406700
583700***************************************************************   60080000
583800 2601-INIT-REF-REV.                                               60090000
583900     INITIALIZE                         C01-DATA-AREA             60100000
584000                                        CSAPI-RETURN-MESSAGE      60110000
584100                                        CSAPI-RETURN-CODE.        60120000
584200                                                                  60130000
584300     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     60140000
584400     MOVE 2                          TO CSAPI-SUB-COMMAND.        60150000
584500     MOVE 'SELECT'                   TO CSAPI-COMMAND.            60160000
584600                                                                  60170000
584800     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      60190000
584900     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      60200000
585000     MOVE '-'                        TO C01-REF-END-DT(5:1).      60210000
585100     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      60220000
585200     MOVE '-'                        TO C01-REF-END-DT(8:1).      60230000
585300     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      60240000
585400                                                                  60250000
585500     SET  WS-ITDC01S1                TO TRUE.                     60260000
585400                                                                  60250000
585600 2601-EXIT.      EXIT.                                            60270000
583700***************************************************************   60080000
585800 2602-ACCESS-REF-REV.                                             60290000

      ** R16.0 E_101251  PMP SET SUBJ-AREA
584700     MOVE 'REVENUE CODE'             TO C01-REF-SUBJ-AREA-NM.     60180000
585900     MOVE FM160-REV-CD               TO C01-REF-KEY-CD            60300000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    66870000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   66880000
586000     CALL WS-CALL-MODULE             USING WS-ITCCSAPI,           60310000
586100                                         C01-DATA-AREA.           60320000
586200 2602-EXIT.      EXIT.                                            60330000
583700***************************************************************   60080000
580200*R16.0 E_101251 PMP BEGIN                                         59730000
585800 2603-ACCESS-REF-NDC-REV.                                         60290000
586100                                                                  60320000
043900*R16.5.P1 E_117120 EXL    ADDED BYPASS FOR INPATIENT SF           04510000
586100      IF WS-PROC-CD-FLAG(1)  = 'X'                                60320000
043900      OR FM105-CLM-TYPE      = '11'                               04510000
586100         GO TO 2603-EXIT                                          60320000
586100      END-IF.                                                     60320000
586100                                                                  60320000
586100      IF  FM160-NDC-CD        > SPACE                             60320000
586100      OR  FM160-HCPCS-PROC-CD > SPACE                             60320000
586100          GO TO 2603-EXIT                                         60320000
586100      END-IF.                                                     60320000
586100                                                                  60320000
586100      MOVE FM160-REV-CD        TO C01-REF-KEY-CD                  60320000
586100                                  WS-REF-REV (WS-SUB3).           60320000
586100                                                                  60320000
586100      MOVE 'NDC REV CODES'     TO C01-REF-SUBJ-AREA-NM.           60320000
586100      CALL WS-CALL-MODULE         USING WS-ITCCSAPI,              60320000
586100                                        C01-DATA-AREA.            60320000
586100                                                                  60320000
586500      IF CSAPI-RETURN-CODE = ZERO                                 60360000
586600         MOVE 'Y'               TO WS-REF-REV-FLAG2(WS-SUB3)      60370000
579200*                                                                 59630000
579000*.... IRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300         MOVE '3'               TO CIAEI-NDC-CD-IND(WS-SUB)       59640000
587100      ELSE                                                        60420000
587200         IF CSAPI-RETURN-CODE = + 100                             60430000
587300            MOVE 'N'             TO WS-REF-REV-FLAG2(WS-SUB3)     60440000
587600         ELSE                                                     60470000
587700            DISPLAY '2603- BAD ACCESS REF DATA'                   60480000
587700            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     60480000
587800            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        60490000
587900            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     60500000
588000            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        60510000
588100                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)60520000
588200            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         60530000
588300         END-IF                                                   60540000
588400      END-IF.                                                     60550000
586100                                                                  60320000
586200 2603-EXIT.      EXIT.                                            60330000
564300**************************************************************    58140000
580200*R16.0 E_101251 PMP END                                           59730000
809078*****************************************************             00406800
809079 2610-SET-REV-TYPE.                                               00406900
809080                                                                  00407000
809090     MOVE CIASC-60-1-999-SLOT (WS-SUB) TO FM160-RECORD.           00407100
577100                                                                  59420000
043900*R16.5.P1 E_117120 EXL    ADDED BYPASS FOR INPATIENT SF           04510000
043900     IF WS-PROC-CD-FLAG(1) = 'X'                                  04510000
043900     OR FM105-CLM-TYPE     = '11'                                 04510000
043900        NEXT SENTENCE                                             04510000
043900     ELSE                                                         04510000
577100        PERFORM 2611-ACCESS-NDC-PRC-CD-INST THRU 2611-EXIT        59420000
577100     END-IF.                                                      59420000
809100                                                                  00407200
809200*** IN PARA 2500- WE ACCESSED REF_DATA WITH REV CODES AND         00407300
809300*** RETRIEVED REV TYPES. THE DISTINCT REVENUE CODES AND REVENUE   00407400
809400*** TYPES ARE STORED IN THE WS-REF- TABLE.                        00407500
809401*** MATCH THE REVENUE CODES FROM THIS TABLE WITH RECORD 60.       00407600
809402     IF  FM160-REV-CD  NUMERIC                                    00407700
809403     AND FM160-REV-CD > ZERO                                      00407800
524700       PERFORM VARYING WS-SUB3 FROM 1 BY 1                        44750000
524800          UNTIL WS-SUB3 > 999                                     44760000
524900          IF WS-REF-REV(WS-SUB3)      = FM160-REV-CD              44780000
525000             IF WS-REF-REV-FLAG (WS-SUB3) = 'N'                   44780000
525100*.... IR038 (REV CD REQ,MUST BE VAL REV TYPE CD FOR CLM TYPE &
525200*     W/N VAL DT RNGE)                                            44840000
525300                MOVE '2' TO CIAEI-REV-CD-IND (WS-SUB)             44780000
525400             ELSE                                                 44780000
809408               IF CIABP-HOME-CNTL-SITE                            00408300
809409                  IF FM160-REV-TYPE-CD = WS-REF-REV-TP(WS-SUB3)   00408400
809410                     CONTINUE                                     00408500
809411                  ELSE                                            00408600
809412*.... IRL2B (REVENUE TYPE CODE MUST = REVENUE TYPE CODE FIELD     00408700
809413*     FROM REVENUE CODE DATABASE (ITCDBD01) WHEN SF EDITS RUN     00408800
809414*     AS HOME)                                                    00408900
526200                     MOVE '4'  TO CIAEI-REV-TYPE-CD-IND (WS-SUB)  44870000
809417                  END-IF                                          00409200
809418               ELSE                                               00409300
809419                  MOVE WS-REF-REV-TP(WS-SUB3) TO                  00409400
809420                       FM160-REV-TYPE-CD                          00409500
809421                  MOVE FM160-RECORD           TO                  00409600
809422                       CIASC-60-1-999-SLOT (WS-SUB)               00409700
809423               END-IF                                             00409800
527000             END-IF                                               44940000
580200*R16.0 E_101251 PMP BEGIN                                         59730000
580200             IF WS-REF-REV-FLAG2(WS-SUB3) = SPACE                 59730000
580200                PERFORM 2603-ACCESS-REF-NDC-REV THRU 2603-EXIT    59730000
580200             END-IF                                               59730000
580200             IF WS-REF-REV-FLAG2(WS-SUB3) = 'Y'                   59730000
579200*                                                                 59630000
579000*.... IRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300                MOVE '3'  TO CIAEI-NDC-CD-IND(WS-SUB)             59640000
580200             END-IF                                               59730000
580200*R16.0 E_101251 PMP END                                           59730000
527100             MOVE 999           TO WS-SUB3                        44950000
527200          ELSE                                                    44940000
527300            IF WS-REF-REV(WS-SUB3)    = SPACES                    44780000
527400               PERFORM 2602-ACCESS-REF-REV                        44780000
527500               PERFORM 2610B-REV-CD-RESULT                        44780000
580600*R16.0 E_101251 PMP NEXT LINE                                     59770000
580600               PERFORM 2603-ACCESS-REF-NDC-REV THRU 2603-EXIT     59770000
809424               MOVE 999         TO WS-SUB3                        00409900
809427            END-IF                                                00410200
527800          END-IF                                                  44780000
527900       END-PERFORM                                                44780000
809429     ELSE                                                         00410400
809430        GO TO 2610-EXIT.                                          00410500
809431                                                                  00410600
809432     IF FM160-REV-TYPE-CD = '1' OR '2' OR '3' OR '4' OR           00410700
809433                            '6' OR '7' OR '8' OR '9'              00410800
809434        CONTINUE                                                  00410900
809435     ELSE                                                         00411000
809436****  'IR079'                                                     00411100
528800**      MOVE '2'          TO CIAEI-REV-TYPE-CD-IND (1)            45080000
528900        MOVE '2'          TO CIAEI-REV-TYPE-CD-IND (WS-SUB)       45080000
809438     END-IF.                                                      00411300
809439                                                                  00411400
582200                                                                  59930000
809440     IF FM105-REL-NUM < '089'                                     00411500
809441        MOVE FM160-REV-CD      TO WS-REV-CD                       00411600
809442        IF WS-SEMI-PRIV-REV-CD                                    00411700
809443           MOVE 'Y' TO WS-SEMI-PRIV-RM-RT-IND                     00411800
809444        END-IF                                                    00411900
809445     ELSE                                                         00412000
809446        IF FM160-PRIVATE-REV-TYPE                                 00412100
809447           MOVE 'Y' TO WS-SEMI-PRIV-RM-RT-IND                     00412200
809448        END-IF                                                    00412300
809449        IF FM160-REV-TYPE-CD = ('1' OR '2' OR '3' OR '4')         00412400
809450            ADD 1 TO CIABP-60-ACCOM-CTR                           00412500
809451        END-IF                                                    00412600
809452     END-IF.                                                      00412700
809453                                                                  00412800
809454 2610-EXIT. EXIT.                                                 00412900
583700**************************************************************    60080000
533300 2610B-REV-CD-RESULT.                                             42880000
533400      IF CSAPI-RETURN-CODE = ZERO                                 43170000
533500         MOVE FM160-REV-CD           TO WS-REF-REV (WS-SUB3)      44190000
533600         MOVE C01-REF-VAL-TXT (1:1)  TO WS-REF-REV-TP (WS-SUB3)   43240000
533610                                        FM160-REV-TYPE-CD         44910000
533700         MOVE 'Y'                  TO WS-REF-REV-FLAG (WS-SUB3)   43240000
533730         MOVE FM160-RECORD     TO  CIASC-60-1-999-SLOT (WS-SUB)   44920000
810890      ELSE                                                        00424200
810892         IF CSAPI-RETURN-CODE = + 100                             00424400
534000            MOVE FM160-REV-CD        TO WS-REF-REV (WS-SUB3)      44190000
534100            MOVE 'N'                 TO WS-REF-REV-FLAG (WS-SUB3) 43240000
534200            MOVE '2'                 TO CIAEI-REV-CD-IND (WS-SUB) 43240000
810894         ELSE                                                     00424600
534400            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     43280000
534500            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        43290000
534600            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     43300000
534700            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        43310000
534800                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)43320000
810898            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         00425000
810899         END-IF                                                   00425100
810900      END-IF.                                                     00425200
583700**************************************************************    60080000
580600*R16.0 E_101251 PMP BEGIN                                         59770000
576800 2611-ACCESS-NDC-PRC-CD-INST.                                     59390000
576900                                                                  59400000
576900     IF  FM160-NDC-CD             > SPACE                         59400000
576900         GO TO 2611-EXIT                                          59400000
576900     END-IF.                                                      59400000

577800     PERFORM VARYING WS-SUB3 FROM 1 BY 1                          59490000
577900       UNTIL WS-SUB3 > 999                                        59500000
578000       IF WS-PROC-CD(WS-SUB3)          =  FM160-HCPCS-PROC-CD     59510000
578100          IF  WS-PROC-CD-FLAG (WS-SUB3) = 'N'                     59520000
578100          AND FM160-HCPCS-PROC-CD  > SPACE                        59520000
578400             GO TO 2611-EXIT                                      59550000
578500          ELSE                                                    59560000
580200             IF WS-PROC-CD-FLAG (WS-SUB3) = SPACE                 59730000
586100                MOVE FM160-HCPCS-PROC-CD  TO WS-PROC-CD(WS-SUB3)  60320000
580200                PERFORM 2612-ACCESS-REF-PROC-CD-INST              59730000
580200                   THRU 2612-EXIT                                 59730000
580200             END-IF                                               59730000
580200             IF WS-PROC-CD-FLAG (WS-SUB3) = 'Y'                   59730000
579200*                                                                 59630000
579000*.... IRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300                MOVE '3'        TO CIAEI-NDC-CD-IND(WS-SUB)       59640000
580200             END-IF                                               59730000
580200             MOVE 999           TO WS-SUB3                        59730000
578400             GO TO 2611-EXIT                                      59550000
                END-IF
580300       ELSE                                                       59740000
578000          IF WS-PROC-CD(WS-SUB3)    =  SPACE                      59510000
586100             MOVE FM160-HCPCS-PROC-CD  TO WS-PROC-CD(WS-SUB3)     60320000
580500             PERFORM 2612-ACCESS-REF-PROC-CD-INST THRU 2612-EXIT  59760000
580700             MOVE 999         TO WS-SUB3                          59780000
580900          END-IF                                                  59800000
580900       END-IF                                                     59800000
581000     END-PERFORM.                                                 59810000
583600                                                                  60070000
583700 2611-EXIT. EXIT.                                                 60080000
583700***********************************************************       60080000
585800 2612-ACCESS-REF-PROC-CD-INST.                                    60290000
586300                                                                  60340000
586100      MOVE WS-PROC-CD(WS-SUB3)  TO C01-REF-KEY-CD.                60320000
586100                                                                  60320000
586100      MOVE 'NDC PROC CODE'      TO C01-REF-SUBJ-AREA-NM.          60320000
586100      CALL WS-CALL-MODULE          USING WS-ITCCSAPI,             60320000
586100                                         C01-DATA-AREA.           60320000
586100                                                                  60320000
586500      IF CSAPI-RETURN-CODE = ZERO                                 60360000
586600         MOVE 'Y'       TO  WS-PROC-CD-FLAG(WS-SUB3)              60370000
579200*                                                                 59630000
579000*.... IRL5V (NDC REQUIRED FOR THIS MEDICAID CLAIM)                59610000
579200*                                                                 59630000
579300         MOVE '3'               TO CIAEI-NDC-CD-IND(WS-SUB)       59640000
587100      ELSE                                                        60420000
587200         IF CSAPI-RETURN-CODE = + 100                             60430000
587300            MOVE 'N'    TO  WS-PROC-CD-FLAG(WS-SUB3)              60440000
587600         ELSE                                                     60470000
587700            DISPLAY '2612- BAD ACCESS REF PROC CD'                60480000
587700            MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE     60480000
587800            MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND        60490000
587900            MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS     60500000
588000            MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD        60510000
588100                                   CIASC-FIRST-SCCF-EDIT-ERR-CD(1)60520000
588200            PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT         60530000
588300         END-IF                                                   60540000
588400      END-IF.                                                     60550000
586100                                                                  60320000
586200 2612-EXIT.      EXIT.                                            60330000
811012***************************************************************** 00432500
583700*R16.0 E_101251 PMP END                                           60080000
811412***************************************************************** 00442800
811413 2800-REF-DATA-ZIP-INST.                                          00442900
221900                                                                  ITCP9102
811415     IF  FM110-PROV-ZIP-CD-5  NOT NUMERIC                         00443100
811416     OR  FM110-PROV-ZIP-CD-4      NUMERIC                         00443200
811417         GO TO 2800-EXIT.                                         00443300
222100                                                                  ITCP9102
811419     INITIALIZE                         C01-DATA-AREA             00443500
811420                                        CSAPI-RETURN-MESSAGE      00443600
811421                                        CSAPI-RETURN-CODE.        00443700
811422                                                                  00443800
811423     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     00443900
811424     MOVE 2                          TO CSAPI-SUB-COMMAND.        00444000
811425     MOVE 'SELECT'                   TO CSAPI-COMMAND.            00444100
811426                                                                  00444200
811427     MOVE 'MA ZIP CODE'              TO C01-REF-SUBJ-AREA-NM.     00444300
811428     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      00444400
811429     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      00444500
811430     MOVE '-'                        TO C01-REF-END-DT(5:1).      00444600
811440     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      00444700
811450     MOVE '-'                        TO C01-REF-END-DT(8:1).      00444800
811460     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      00444900
811470                                                                  00445000
811480     MOVE FM110-PROV-ZIP-CD-5        TO C01-REF-KEY-CD            00445100
811481                                                                  00445200
811482     SET  WS-ITDC01S1                TO TRUE.                     00445300
811483                                                                  00445400
811484     CALL WS-CALL-MODULE  USING WS-ITCCSAPI,                      00445500
811485                                C01-DATA-AREA.                    00445600
811486                                                                  00445700
811487     IF CSAPI-RETURN-CODE = ZERO                                  00445800
811488        PERFORM 9960-FIND-ERROR-SLOT   THRU 9960-EXIT             00445900
811489        MOVE 'IRL2Y'        TO FM1A5-ERR-CD (WS-SUB1)             00446000
811490        MOVE '4'            TO CIAEI-PROV-ZIP-CD-IND
811491     ELSE                                                         00446100
811492        IF CSAPI-RETURN-CODE = + 100                              00446200
811493           CONTINUE                                               00446300
811494        ELSE                                                      00446400
811495           MOVE CSAPI-RETURN-MESSAGE TO SIAPC-ERROR-MESSAGE       00446500
811496           MOVE CWSCC-ABORT          TO SIAPC-RETURN-IND          00446600
811497           MOVE 'I'                  TO CIABP-STAT-CD             00446700
486500           MOVE 'SF018'        TO SIAPC-RETURN-MESSAGE-CD         48650000
486600                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 48660000
811499           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT          00446900
811500        END-IF                                                    00447000
811501     END-IF.                                                      00447100
811502                                                                  00447200
811503 2800-EXIT. EXIT.                                                 00447300
569900                                                                  48720000
570000*R11.4 E_38714 DSL BEGIN                                          44520000
811504***************************************************************** 00447400
570200 2900-READ-B2-MSG.                                                48250000
570300     MOVE SPACE                    TO B22-DATA-AREA.              44220000
570400                                                                  44280000
570500     MOVE 'SELECT'                 TO CSAPI-COMMAND.              44220000
570600     MOVE WS-B2-SUB-COMM           TO CSAPI-SUB-COMMAND.          44230000
570700                                                                  44280000
595100*** R14.5 E_34072 EXL START                                       61220000
595200     MOVE CSAPI-SCCF-SER-NUM       TO B22-SCCF-ID                 61230000
595300***  MOVE CIASC-SCCF-SERIAL-NUM (1) TO B22-SCCF-ID                61240000
595400*** R14.5 E_34072 EXL END                                         61250000
570900     MOVE CIART-BUS-OWNER-ID-FMT   TO B22-BUS-OWNER-ID.           49390000
571000     MOVE WS-B2-REQUEST            TO B22-REQ-RESP-IND.           49390000
595700*** R14.5 E_34072 EXL START                                       61280000
595800     MOVE CIABP-HOST-HOME-SITE-IND TO B22-HOST-HOME-CD.           61290000
595900     MOVE WS-B2-MSG-TP-CD          TO B22-MSG-TP-CD.              61300000
596000     MOVE WS-B2-MSG-STS-CD         TO B22-MSG-STS-CD.             61310000
596100**   MOVE 'GENINQ'                 TO B22-MSG-TP-CD.              61320000
596200**   MOVE 'PRSD'                   TO B22-MSG-STS-CD.             61330000
596300*** R14.5 E_34072 EXL END                                         61340000
571300     MOVE WS-B2-ACTION             TO B22-ACT-CD.                 49390000
571400     MOVE WS-B2-RESUB              TO B22-RESUB-IND.              49390000
571600                                                                  44280000
571700     SET WS-ITDB22S1                       TO TRUE.               37200000
571800     CALL WS-CALL-MODULE       USING WS-ITCCSAPI                  37210000
571900                                     B22-DATA-AREA.               44300000
572000                                                                  44280000
572100 2900-EXIT.    EXIT.                                              48250000
572200                                                                  44300000
572300 2902-INSERT-IO-ERROR-MSG.                                        48250000
572400     MOVE CWSCC-READ-ERROR            TO SIAPC-RETURN-STATUS      09100000
572500     MOVE 'ITCP9102 - 1230-BAD READ IPP-MSG-SEARCH '              09110000
572600                                   TO SIAPC-ERROR-MESSAGE         09120000
572700     MOVE 'SF023'           TO SIAPC-RETURN-MESSAGE-CD            09130000
572800                            CIASC-FIRST-SCCF-EDIT-ERR-CD(1)       09140000
572900     PERFORM 9990-ERROR-DISPLAY              THRU 9990-EXIT.      09150000
573000 2902-EXIT.    EXIT.                                              09160000
573100***************************************************************** 48720000
556501                                                                  48720000
556510*R13.5 E_79779 DSL BEGIN                                          41110000
556520 3000-CONVT-RCPT-DT.                                              40620000
556530                                                                  40630000
556540     IF WS-LOC-RCPT-DT  NUMERIC AND                               40640000
556541        WS-LOC-RCPT-DT  >   0                                     40640000
556550        COMPUTE WS-JULIAN-DT = FUNCTION DAY-OF-INTEGER            07960000
556560           (FUNCTION INTEGER-OF-DATE(WS-LOC-RCPT-DT))             07970000
556570     ELSE                                                         40640000
556580        IF FM105-LOC-PLAN-RCPT-DT NUMERIC                         07960000
556581           MOVE FM105-LOC-PLAN-RCPT-DT TO WS-LOC-RCPT-DT          07960000
556582           COMPUTE WS-JULIAN-DT = FUNCTION DAY-OF-INTEGER         07960000
556590              (FUNCTION INTEGER-OF-DATE(WS-LOC-RCPT-DT))          07970000
556591        ELSE                                                      07960000
556592           MOVE 0 TO WS-JULIAN-DT                                 40680000
556593        END-IF                                                    07960000
556594     END-IF.                                                      40650000
556595     MOVE WS-JULIAN-DT   TO WS-LOC-RCPT-DT-JUL.                   40680000
556596 3000-EXIT.    EXIT.                                              40620000
556597*R13.5 E_79779 END                                                41110000
556598                                                                  48720000
573200 3300-REF-DATA-ZIP-PROF.                                          45270000
573300     INITIALIZE                         C01-DATA-AREA             45290000
573400                                        CSAPI-RETURN-MESSAGE      45300000
573500                                        CSAPI-RETURN-CODE.        45310000
573600                                                                  45320000
573700     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     45330000
573800     MOVE 2                          TO CSAPI-SUB-COMMAND.        45340000
573900     MOVE 'SELECT'                   TO CSAPI-COMMAND.            45350000
574000                                                                  45360000
574100     MOVE 'MA ZIP CODE'              TO C01-REF-SUBJ-AREA-NM.     45370000
574200     MOVE CIABP-INCUR-DT-CC          TO C01-REF-END-DT(1:2).      45380000
574300     MOVE CIABP-INCUR-DT-YY          TO C01-REF-END-DT(3:2).      45390000
574400     MOVE '-'                        TO C01-REF-END-DT(5:1).      45400000
574500     MOVE CIABP-INCUR-DT-MM          TO C01-REF-END-DT(6:2).      45410000
574600     MOVE '-'                        TO C01-REF-END-DT(8:1).      45420000
574700     MOVE CIABP-INCUR-DT-DD          TO C01-REF-END-DT(9:2).      45430000
574800                                                                  45440000
574900     SET  WS-ITDC01S1                TO TRUE.                     45450000
575000                                                                  45790000
575100     IF  FM1B0-BILL-PROV-ZIP-CD5         NUMERIC                  45750000
575200     AND FM1B0-BILL-PROV-ZIP-CD4     NOT NUMERIC                  45760000
575300         PERFORM 3320-CHECK-CLM-BILL-ZIP  THRU 3320-EXIT          45770000
575400     END-IF.                                                      45780000
575500                                                                  45790000
575600     IF CIABP-EOJ                                                 45800000
575700        GO TO 3300-EXIT.                                          45810000
575800                                                                  45820000
575900     IF  FM1B5-SERV-FAC-LOC-ZIP5-CLM     NUMERIC                  45830000
576000     AND FM1B5-SERV-FAC-LOC-ZIP4-CLM NOT NUMERIC                  45840000
576100         PERFORM 3330-CHECK-CLM-SERV-FAC-ZIP  THRU 3330-EXIT      45850000
576200     END-IF.                                                      45860000
576300                                                                  45870000
576400     IF CIABP-EOJ                                                 45880000
576500        GO TO 3300-EXIT.                                          45890000
576600                                                                  45900000
576700     IF  FM1E0-PERF-PROV-OFC-ZIP-CD5     NUMERIC                  45910000
576800     AND FM1E0-PERF-PROV-OFC-ZIP-CD4 NOT NUMERIC                  45920000
576900         PERFORM 3340-CHECK-CLM-PERF-PRV-ZIP  THRU 3340-EXIT      45930000
577000     END-IF.                                                      45940000
577100 3300-EXIT.    EXIT.                                              45270000
577200                                                                  45270000
577300 3320-CHECK-CLM-BILL-ZIP.                                               00
560710*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
577400     PERFORM VARYING WS-SUB3 FROM 1 BY 1                          43090000
560900        UNTIL WS-SUB3           > 800                             43100000
577600       IF FM1B0-BILL-PROV-ZIP-CD5     = WS-REF-ZIP (WS-SUB3)      45750000
577700           IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'                     43150000
577800              PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT         47220000
577900              MOVE 'PRL2H'  TO FM1A5-ERR-CD (WS-SUB1)             47230000
578000           ELSE                                                   43150000
578100              CONTINUE                                            43160000
578200           END-IF                                                 43150000
561700           MOVE 800 TO WS-SUB3                                    43160000
578400       ELSE                                                       43150000
578500           IF WS-REF-ZIP (WS-SUB3) = SPACES                       43150000
578600              MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM
578700              MOVE FM1B0-BILL-PROV-ZIP-CD5 TO C01-REF-KEY-CD      41520000
578800              PERFORM 2300A-ACCESS-REF                            43150000
578900              PERFORM 3320A-REF-ZIP-RESULT                        43150000
562400              MOVE 800 TO WS-SUB3                                 43160000
579100           END-IF                                                 43150000
579200       END-IF                                                     43150000
579300     END-PERFORM .                                                43360000
579400 3320-EXIT.    EXIT.                                                    00
579500 3320A-REF-ZIP-RESULT.                                                  00
579600     IF CSAPI-RETURN-CODE = ZERO                                  43170000
579700        MOVE FM1B0-BILL-PROV-ZIP-CD5 TO WS-REF-ZIP (WS-SUB3)            00
579800        MOVE 'Y' TO WS-REF-ZIP-FLAG          (WS-SUB3)            43240000
579900        PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT               47220000
580000        MOVE 'PRL2H'        TO FM1A5-ERR-CD (WS-SUB1)             47230000
580100     ELSE                                                         43190000
580200        IF CSAPI-RETURN-CODE = + 100                              43200000
580300           MOVE FM1B0-BILL-PROV-ZIP-CD5 TO WS-REF-ZIP (WS-SUB3)         00
580400           MOVE 'N' TO WS-REF-ZIP-FLAG          (WS-SUB3)         43240000
580500        ELSE                                                      43240000
580600           MOVE CSAPI-RETURN-MESSAGE   TO SIAPC-ERROR-MESSAGE     43280000
580700           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND        43290000
580800           MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS     43300000
580900           MOVE 'SF018'         TO SIAPC-RETURN-MESSAGE-CD        43310000
581000                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 3320000
581100           PERFORM 9990-ERROR-DISPLAY      THRU 9990-EXIT         43330000
581200        END-IF                                                    43340000
581300     END-IF.                                                      43350000
581400                                                                  43350000
581500 3330-CHECK-CLM-SERV-FAC-ZIP.                                           00
564910*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
581600     PERFORM VARYING WS-SUB3 FROM 1 BY 1                          43090000
565100        UNTIL WS-SUB3           > 800                             43100000
581800       IF FM1B5-SERV-FAC-LOC-ZIP5-CLM = WS-REF-ZIP (WS-SUB3)      45830000
581900           IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'                     43150000
582000              PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT         47220000
582100              MOVE 'PRL2K'  TO FM1A5-ERR-CD (WS-SUB1)             47230000
582200           ELSE                                                   43150000
582300              CONTINUE                                            43160000
582400           END-IF                                                 43150000
565900           MOVE 800 TO WS-SUB3                                    43160000
582600       ELSE                                                       43150000
582700           IF WS-REF-ZIP (WS-SUB3) = SPACES                       43150000
582800              MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM
582900              MOVE FM1B5-SERV-FAC-LOC-ZIP5-CLM TO C01-REF-KEY-CD   5830000
583000              PERFORM 2300A-ACCESS-REF                            43150000
583100              PERFORM 3330A-REF-ZIP-RESULT                        43150000
566600              MOVE 800 TO WS-SUB3                                 43160000
583300           END-IF                                                 43150000
583400       END-IF                                                     43150000
583500     END-PERFORM .                                                43360000
583600 3330-EXIT.    EXIT.                                                    00
583700 3330A-REF-ZIP-RESULT.                                                  00
583800     IF CSAPI-RETURN-CODE = ZERO                                  43170000
583900        MOVE FM1B5-SERV-FAC-LOC-ZIP5-CLM TO WS-REF-ZIP (WS-SUB3)        00
584000        MOVE 'Y' TO WS-REF-ZIP-FLAG          (WS-SUB3)            43240000
584100        PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT               47220000
584200        MOVE 'PRL2K'        TO FM1A5-ERR-CD (WS-SUB1)             47230000
584300     ELSE                                                         43190000
584400        IF CSAPI-RETURN-CODE = + 100                              43200000
584500           MOVE FM1B5-SERV-FAC-LOC-ZIP5-CLM TO                          00
584600                                             WS-REF-ZIP (WS-SUB3)       00
584700           MOVE 'N' TO WS-REF-ZIP-FLAG          (WS-SUB3)         43240000
584800        ELSE                                                      43240000
584900           MOVE CSAPI-RETURN-MESSAGE   TO SIAPC-ERROR-MESSAGE     43280000
585000           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND        43290000
585100           MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS     43300000
585200           MOVE 'SF018'         TO SIAPC-RETURN-MESSAGE-CD        43310000
585300                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 3320000
585400           PERFORM 9990-ERROR-DISPLAY      THRU 9990-EXIT         43330000
585500        END-IF                                                    43340000
585600     END-IF.                                                      43350000
585700                                                                        00
585800 3340-CHECK-CLM-PERF-PRV-ZIP.                                           00
569210*R13.0.1 E_82574 DSL LOOP THROUGH 800 TIMES FROM 50               26750000
585900     PERFORM VARYING WS-SUB3 FROM 1 BY 1                          43090000
569400        UNTIL WS-SUB3           > 800                             43100000
586100       IF FM1E0-PERF-PROV-OFC-ZIP-CD5 = WS-REF-ZIP (WS-SUB3)      45910000
586200           IF WS-REF-ZIP-FLAG (WS-SUB3) = 'Y'                     43150000
586300              PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT         47220000
586400              MOVE 'PRL2I'  TO FM1A5-ERR-CD (WS-SUB1)             47230000
586500           ELSE                                                   43150000
586600              CONTINUE                                            43160000
586700           END-IF                                                 43150000
570200           MOVE 800 TO WS-SUB3                                    43160000
586900       ELSE                                                       43150000
587000           IF WS-REF-ZIP (WS-SUB3) = SPACES                       43150000
587100              MOVE 'MA ZIP CODE' TO C01-REF-SUBJ-AREA-NM
587200              MOVE FM1E0-PERF-PROV-OFC-ZIP-CD5 TO C01-REF-KEY-CD        00
587300              PERFORM 2300A-ACCESS-REF                            43150000
587400              PERFORM 3340A-REF-ZIP-RESULT                        43150000
570900              MOVE 800 TO WS-SUB3                                 43160000
587600           END-IF                                                 43150000
587700       END-IF                                                     43150000
587800     END-PERFORM .                                                43360000
587900 3340-EXIT.    EXIT.                                                    00
588000 3340A-REF-ZIP-RESULT.                                                  00
588100     IF CSAPI-RETURN-CODE = ZERO                                  43170000
588200        MOVE FM1E0-PERF-PROV-OFC-ZIP-CD5 TO WS-REF-ZIP (WS-SUB3)        00
588300        MOVE 'Y' TO WS-REF-ZIP-FLAG          (WS-SUB3)            43240000
588400        PERFORM 9960-FIND-ERROR-SLOT THRU 9960-EXIT               47220000
588500        MOVE 'PRL2I'        TO FM1A5-ERR-CD (WS-SUB1)             47230000
588600     ELSE                                                         43190000
588700        IF CSAPI-RETURN-CODE = + 100                              43200000
588800           MOVE FM1E0-PERF-PROV-OFC-ZIP-CD5 TO                          00
588900                                             WS-REF-ZIP (WS-SUB3)       00
589000           MOVE 'N' TO WS-REF-ZIP-FLAG          (WS-SUB3)         43240000
589100        ELSE                                                      43240000
589200           MOVE CSAPI-RETURN-MESSAGE   TO SIAPC-ERROR-MESSAGE     43280000
589300           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND        43290000
589400           MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS     43300000
589500           MOVE 'SF018'         TO SIAPC-RETURN-MESSAGE-CD        43310000
589600                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 3320000
589700           PERFORM 9990-ERROR-DISPLAY      THRU 9990-EXIT         43330000
589800        END-IF                                                    43340000
589900     END-IF.                                                      43350000
590000*R11.4 E_38714 END                                                44520000
590100                                                                  44520000
811505 8880-ERROR-LOG.                                                  00447401
811506                                                                  00447402
811507     MOVE 'RT/MLE SF EDIT I/O ERROR'  TO LK-ORIG-SOURCE.          00447403
811508     MOVE 'IRT1'                      TO LK-TRAN-ID.              00447405
811509     MOVE 'RT/MLE'                    TO LK-USER-ID.              00447406
811510     MOVE 'SF EDIT'                   TO LK-FUNCT-DESC.           00447407
811520                                                                  00447408
811530     IF SIAPC-RETURN-IND = CWSCC-ABORT                            00447409
811540        MOVE 'ABORT       '  TO LK-RETURN-STS-DESC                00447410
811550     ELSE                                                         00447411
811560     IF SIAPC-RETURN-IND = CWSCC-UNSUCCESSFUL                     00447412
811570        MOVE 'UNSUCCESSFUL'  TO LK-RETURN-STS-DESC                00447413
811580     ELSE                                                         00447414
811590     IF SIAPC-RETURN-IND = CWSCC-SEVERE-LOG                       00447415
811600        MOVE 'SEVERE ERROR'  TO LK-RETURN-STS-DESC.               00447416
811700                                                                  00447417
811800     SET  CSAPI-INSERT       TO TRUE.                             00447418
811900     MOVE 1                  TO CSAPI-SUB-COMMAND.                00447419
812000                                                                  00447420
812100     MOVE 'ITCP9102'         TO CSAPI-CALLING-PGM-ID.             00447421
812101                                                                  00447422
812102     SET  WS-ITDE02U1        TO TRUE.                             00447423
812103     CALL WS-CALL-MODULE           USING WS-ITCCSAPI              00447424
812104                                          WS-ITSIAPC              00447425
812105                                          WS-LINK-ITDE02U1.       00447426
812106                                                                  00447427
812107 8880-EXIT. EXIT.                                                 00447429
812108***************************************************************** 00447430
222300 9010-CALL-PRE-EDIT-MODULE.                                       ITCP9102
222400****  ITCPP002 - PLAN PRE-EDIT EXIT FOR APPENDED DATA MODULE  ****ITCP9102
222500                                                                  ITCP9102
812112     ADD WS-PLUS-ONE                 TO SIAPC-MODULE-LEVEL.       00447800
812113     SET WS-ITCPP002                 TO TRUE.                     00447900
222800     MOVE WS-CALL-MODULE             TO                           ITCP9102
812115          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).                 00448100
223000                                                                  ITCP9102
812117     CALL WS-CALL-MODULE USING WS-ITSIAPC                         00448300
812118                               WS-ITCIABP                         00448400
223300                               WS-STACKED-CLAIM                   ITCP9102
812120                               WS-ITCDBP20-RECORD.                00448600
223400                                                                  ITCP9102
812122     IF SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                       00448800
812123        CONTINUE                                                  00448900
812124     ELSE                                                         00449000
812125     IF SIAPC-RETURN-IND = CWSCC-ABORT                            00449100
812126        SET CIABP-EOJ             TO TRUE                         00449200
812127        MOVE 3600                 TO RETURN-CODE
492000        MOVE 'SF017'           TO SIAPC-RETURN-MESSAGE-CD         49200000
492100                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 49210000
812129        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00449900
812130     END-IF.                                                      00449300
226400                                                                  ITCP9102
226500     MOVE SPACES                     TO                           ITCP9102
812133          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).                 00450200
812134     SUBTRACT 1                    FROM SIAPC-MODULE-LEVEL.       00450300
226900                                                                  ITCP9102
227000 9010-EXIT.  EXIT.                                                ITCP9102
227100                                                                  ITCP9102
227200/                                                                 ITCP9102
232900 9030-CALL-PPF-IO-MODULE.                                         ITCP9102
812140****  ITCP4152 - PLAN PROFILE ACCESS MODULE                   ****00450900
233100                                                                  ITCP9102
812142     ADD WS-PLUS-ONE                 TO SIAPC-MODULE-LEVEL.       00451100
812143     SET WS-ITCP4152                 TO TRUE.                     00451200
233400     MOVE WS-CALL-MODULE             TO                           ITCP9102
812145          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).                 00451400
812146     MOVE CIART-BUS-OWNER-ID-FMT     TO CIAPP1-FORMAT-BOID.       00451900
812147     MOVE CIART-BUS-OWNER-ID-PP      TO CIAPP1-PLANPF-BOID.       00452000
233600                                                                  ITCP9102
812149     CALL WS-CALL-MODULE USING WS-ITSIAPC                         00451600
812150                               WS-ITCIAPP1                        00451700
812160                               WS-ITCIAPP2                        00451800
                                     CIART-RT-IND.
234100                                                                  ITCP9102
812162     IF SIAPC-RETURN-IND = CWSCC-SEVERE-LOG                       00452100
812163        SET WS-READ-PPF-NAME      TO TRUE                         00452200
495100        MOVE WS-DB-READ-MSG    TO SIAPC-ERROR-MESSAGE             49510000
495200        MOVE 'SF022'           TO SIAPC-RETURN-MESSAGE-CD         49520000
495300                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 49530000
812165        PERFORM 9990-ERROR-DISPLAY           THRU 9990-EXIT       00452400
812166     ELSE                                                         00452500
812167        MOVE SPACES                     TO                        00452600
239500          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)                  ITCP9102
812169        SUBTRACT WS-PLUS-ONE      FROM SIAPC-MODULE-LEVEL.        00452800
239800                                                                  ITCP9102
239900 9030-EXIT.  EXIT.                                                ITCP9102
240000                                                                  ITCP9102
812173******************************************************************
812174****  ITCP9646 - PLAN CD/STATION CD/CFA ACCOUNT CD TABLE LOAD ****
812175******************************************************************
288800 9060-CALL-TBL-LOAD-MODULE.                                       ITCP9102
289000                                                                  ITCP9102
812178     ADD +1                TO SIAPC-MODULE-LEVEL.
812179     SET WS-ITCP9646       TO TRUE.
812180     MOVE WS-CALL-MODULE   TO
812190                           SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).
289500                                                                  ITCP9102
812300     CALL WS-CALL-MODULE USING WS-ITSIAPC
812400                               WS-ITCWSPSC.
289800                                                                  ITCP9102
289900     EVALUATE SIAPC-RETURN-IND                                    ITCP9102
290000        WHEN CWSCC-SUCCESSFUL                                     ITCP9102
290100           CONTINUE                                               ITCP9102
290200                                                                  ITCP9102
290800        WHEN OTHER                                                ITCP9102
813001           MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND         00456500
813002           MOVE CWSCC-READ-ERROR      TO SIAPC-RETURN-STATUS      00096000
813003           MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE      00456400
498300           MOVE 'SF019'        TO SIAPC-RETURN-MESSAGE-CD         49830000
498400                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 49840000
291400           PERFORM 9990-ERROR-DISPLAY                             ITCP9102
813006     END-EVALUATE.
291600                                                                  ITCP9102
813008     MOVE SPACES  TO SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).
813009     SUBTRACT +1                   FROM SIAPC-MODULE-LEVEL.
292100                                                                  ITCP9102
292200 9060-EXIT.  EXIT.                                                ITCP9102
813012******************************************************            00453200
813013 9065-GET-PLANCD-STACD-RT.                                        00453300
292400                                                                  ITCP9102
      *R11.2 PD_61223 YAS INITIALIZE WS-PASSING-AREA                    15130000
           MOVE SPACES               TO WS-PASSING-AREA.
813015     MOVE ZERO                 TO CWSPSC-PLAN-STATION-CODE-MAX.   00453500
813016     IF WS-PROFESSIONAL                                           00453600
813017        IF FM1A5-LOC-PLAN-CD  <  FM1A5-CNTL-PLAN-CD               00453700
813018           MOVE FM1A5-LOC-PLAN-CD  TO WS-PLAN-CD2                 00453800
813019           MOVE FM1A5-CNTL-PLAN-CD TO WS-PLAN-CD                  00453900
813020        ELSE                                                      00454000
813021           MOVE FM1A5-LOC-PLAN-CD  TO WS-PLAN-CD                  00454100
813022           MOVE FM1A5-CNTL-PLAN-CD TO WS-PLAN-CD2                 00454200
414899     ELSE                                                         04677
813024        IF FM105-LOC-PLAN-CD  <  FM105-CNTL-PLAN-CD               00454400
813025           MOVE FM105-LOC-PLAN-CD  TO WS-PLAN-CD2                 00454500
813026           MOVE FM105-CNTL-PLAN-CD TO WS-PLAN-CD                  00454600
813027        ELSE                                                      00454700
813028           MOVE FM105-LOC-PLAN-CD  TO WS-PLAN-CD                  00454800
813029           MOVE FM105-CNTL-PLAN-CD TO WS-PLAN-CD2.                00454900
414906                                                                  04684
813040     MOVE SPACE               TO CWSPSC-PLAN-STATION-TABLE.       00455100
813041     MOVE ZERO                TO CWSPSC-PLAN-STATION-CODE-MAX.    00455200
813042     MOVE 'ITCP9102'          TO CSAPI-CALLING-PGM-ID.            00455300
813043     MOVE 'SELECT'            TO CSAPI-COMMAND.                   00455400
813044     SET WS-ITDS01S1          TO TRUE.                            00455500
813045     IF WS-PLAN-CD2 > SPACE                                       00455600
813046        CALL WS-CALL-MODULE        USING WS-ITCCSAPI              00455700
813047                                         WS-PASSING-AREA          00455800
813048                                         WS-ITCWSPSC              00455900
813049        END-CALL                                                  00456000
813050        IF CSAPI-RETURN-CODE = ZERO                               00456100
813051           CONTINUE                                               00456200
813052        ELSE                                                      00456300
813053           MOVE CWSCC-ABORT          TO SIAPC-RETURN-IND          00456500
813054           MOVE CWSCC-READ-ERROR     TO SIAPC-RETURN-STATUS       00096000
813055           MOVE CSAPI-RETURN-MESSAGE TO SIAPC-ERROR-MESSAGE       00456400
502700           MOVE 'SF019'        TO SIAPC-RETURN-MESSAGE-CD         50270000
502800                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 50280000
813057           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT          00456800
813058           GO TO 9065-EXIT.                                       00456900
414921                                                                  04699
813060     IF WS-PLAN-CD  > SPACE                                       00457100
813061        MOVE WS-PLAN-CD    TO WS-PLAN-CD2                         00457200
813062        CALL WS-CALL-MODULE        USING WS-ITCCSAPI              00457300
813063                                         WS-PASSING-AREA          00457400
813064                                         WS-ITCWSPSC              00457500
813065        END-CALL                                                  00457600
813066        IF CSAPI-RETURN-CODE = ZERO OR 100                        00457700
813067           CONTINUE                                               00457800
813068        ELSE                                                      00457900
813069           MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND        00458100
813070           MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS     00096000
813080           MOVE CSAPI-RETURN-MESSAGE   TO SIAPC-ERROR-MESSAGE     00458000
504400           MOVE 'SF019'        TO SIAPC-RETURN-MESSAGE-CD         50440000
504500                                  CIASC-FIRST-SCCF-EDIT-ERR-CD(1) 50450000
813091           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT.         00458400
414930                                                                  04708
813093 9065-EXIT.  EXIT.                                                00458600
504810*REL 11.3 E_10097 DSL BEGIN                                        9620000
504811******************************************************************49620000
504820****  LOAD RELATIONSHIP CODE FROM REF DATA                    ****49630000
504830******************************************************************49640000
504840 9070-GET-REL-CD.                                                 49650000
504851     INITIALIZE                         C01-DATA-AREA
504852                                        CSAPI-RETURN-MESSAGE
504853                                        CSAPI-RETURN-CODE.
504854
504855     MOVE 'PAT RELATION'             TO C01-REF-SUBJ-AREA-NM.
504860     PERFORM 9075-DECLARE-CURSOR.                                 49650000
504870     IF CSAPI-RETURN-CODE = 0                                     49650000
504871        MOVE SPACES                  TO WS-END-REF-ITEM
504872        MOVE 'PAT RELATION'          TO C01-REF-SUBJ-AREA-NM
504874        PERFORM UNTIL WS-END-REF-ITEM = 'Y'                       49650000
504875           PERFORM 9077-FETCH-ITEMS THRU 9077-EXIT                49650000
504877           IF CSAPI-RETURN-CODE = +100
504878              MOVE 'Y' TO WS-END-REF-ITEM                         00756
504879           ELSE                                                   00758
504880             IF CSAPI-RETURN-CODE = ZERO
504881                MOVE C01-REF-KEY-CD (1:2) TO                         00759
504882                                 REF-REL-CD (REF-REL-CD-I)              59
504883                MOVE C01-REF-END-DT TO                             00759
504884                                 REF-REL-END-DT (REF-REL-CD-I)
504885                MOVE C01-REF-EFF-DT TO                               00759
504886                                 REF-REL-START-DT (REF-REL-CD-I)        59
504887                SET REF-REL-CD-I UP BY 1                          00760
504888             ELSE                                                 00762
504889                MOVE 'Y' TO WS-END-REF-ITEM                       00763
504890                MOVE CSAPI-RETURN-MESSAGE TO SIAPC-ERROR-MESSAGE  43280000
504891                MOVE CWSCC-ABORT       TO SIAPC-RETURN-IND        43290000
504892                MOVE CWSCC-READ-ERROR  TO SIAPC-RETURN-STATUS     43300000
504893                MOVE 'SF018'  TO SIAPC-RETURN-MESSAGE-CD          43310000
504894                                 CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   3320000
504895                PERFORM 9990-ERROR-DISPLAY THRU 9990-EXIT         43330000
504896             END-IF                                               00765
504897           END-IF                                                 00761
504898        END-PERFORM                                               49650000
504900     END-IF.                                                      49650000
504901     PERFORM 9075C-CLOSE-CURSOR.                                  49650000
504902 9070-EXIT.  EXIT.                                                49910000
504904                                                                  49660000
504905 9075-DECLARE-CURSOR.                                             49650000
504907     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     48350000
504908     MOVE 1                          TO CSAPI-SUB-COMMAND.
504909     MOVE 'DECLARE'                  TO CSAPI-COMMAND.
504910
504911     SET  WS-ITDC01S1                TO TRUE.                     48490000
504912     CALL WS-CALL-MODULE  USING WS-ITCCSAPI,                      48510000
504913                                C01-DATA-AREA.                    48520000
504914
504915 9075-EXIT.  EXIT.
504916
504917 9075C-CLOSE-CURSOR.                                              49650000
504918     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     48350000
504919     MOVE 1                          TO CSAPI-SUB-COMMAND.
504920     MOVE 'CLOSE'                    TO CSAPI-COMMAND.
504921
504922     SET  WS-ITDC01S1                TO TRUE.                     48490000
504923     CALL WS-CALL-MODULE  USING WS-ITCCSAPI,                      48510000
504924                                C01-DATA-AREA.                    48520000
504925
504926 9075C-EXIT. EXIT.
504927
504928 9077-FETCH-ITEMS.
504929
504930     INITIALIZE                         CSAPI-RETURN-MESSAGE
504931                                        CSAPI-RETURN-CODE.
504932
504933     MOVE 'ITCP9102'                 TO CSAPI-CALLING-PGM-ID.     48350000
504934     MOVE 1                          TO CSAPI-SUB-COMMAND.
504935     MOVE 'FETCH'                    TO CSAPI-COMMAND.
504936
504937     SET  WS-ITDC01S1                TO TRUE.                     48490000
504938     CALL WS-CALL-MODULE  USING WS-ITCCSAPI,                      48510000
504939                                C01-DATA-AREA.                    48520000
504940
504971 9077-EXIT. EXIT.                                                 00787
504972******************************************************************49620000
504973****  LOAD SOURCE OF PAYMENT FROM REF DATA                    ****49630000
504974******************************************************************49640000
504975 9080-GET-SRC-PMT.                                                49650000
504977     INITIALIZE                         C01-DATA-AREA
504978                                        CSAPI-RETURN-MESSAGE
504979                                        CSAPI-RETURN-CODE.
504980
504981     MOVE 'PAYMENT SOURCE'           TO C01-REF-SUBJ-AREA-NM.
504982     PERFORM 9075-DECLARE-CURSOR.                                 49650000
504983     IF CSAPI-RETURN-CODE = 0                                     49650000
504984        MOVE SPACES                  TO WS-END-REF-ITEM
504985        MOVE 'PAYMENT SOURCE'        TO C01-REF-SUBJ-AREA-NM
504986        PERFORM UNTIL WS-END-REF-ITEM = 'Y'                       49650000
504987           PERFORM 9077-FETCH-ITEMS THRU 9077-EXIT                49650000
504988           IF CSAPI-RETURN-CODE = +100
504989              MOVE 'Y' TO WS-END-REF-ITEM                         00756
504990           ELSE                                                   00758
504991             IF CSAPI-RETURN-CODE = ZERO
504992                MOVE C01-REF-KEY-CD (1:2) TO                         00759
504993                                 REF-SRC-PMT (REF-SRC-PMT-I)            59
504994                MOVE C01-REF-END-DT TO                             00759
504995                                 REF-SRC-END-DT (REF-SRC-PMT-I)
504996                MOVE C01-REF-EFF-DT TO                               00759
504997                                 REF-SRC-START-DT (REF-SRC-PMT-I)       59
504998                SET REF-SRC-PMT-I UP BY 1                         00760
504999             ELSE                                                 00762
505000                MOVE 'Y' TO WS-END-REF-ITEM                       00763
505001                MOVE CSAPI-RETURN-MESSAGE TO SIAPC-ERROR-MESSAGE  43280000
505002                MOVE CWSCC-ABORT       TO SIAPC-RETURN-IND        43290000
505003                MOVE CWSCC-READ-ERROR  TO SIAPC-RETURN-STATUS     43300000
505004                MOVE 'SF018'  TO SIAPC-RETURN-MESSAGE-CD          43310000
505005                                 CIASC-FIRST-SCCF-EDIT-ERR-CD(1)   3320000
505006                PERFORM 9990-ERROR-DISPLAY THRU 9990-EXIT         43330000
505007             END-IF                                               00765
505008           END-IF                                                 00761
505009        END-PERFORM                                               49650000
505010     END-IF.                                                      49650000
505011     PERFORM 9075C-CLOSE-CURSOR.                                  49650000
505012 9080-EXIT.  EXIT.                                                49910000
505013*REL 11.3 E_10097 END                                              9620000
505014                                                                  49660000
619910* R12.0 E_59024 EXL ADD 9090 PARAGRAPH
620000***************************************************************** 50490000
620001* GET ALL THE STATION CODES FROM THE BCBS_STN TABLE WHICH ARE NOT 50490000
620002* SET FOR REAL TIME PROCESS. IT'LL BE USED TO SET TMC IN 1800-    50490000
620003***************************************************************** 50490000
620004 9090-GET-TMC1-STN-CD.                                            08100000
620005                                                                  49660000
620006     MOVE SPACE                TO WS-STN-TBL.                     49660000
620007     SET CSAPI-DECLARE         TO TRUE.                           49660000
620008     MOVE 1                    TO CSAPI-SUB-COMMAND
620009     SET WS-ITDS02S1           TO TRUE.                           49660000
      *R18.0 E_114613 TGW - ADDED MOVE OF PROGRAM ID                    75120000
           MOVE WS-MODULE-ID TO CSAPI-CALLING-PGM-ID.                   75130000
620010     CALL WS-CALL-MODULE    USING WS-ITCCSAPI                     49660000
620011                                  S02-ROW.                        49660000
620012     IF CSAPI-RETURN-CODE       = 0                               49660000
620013        NEXT SENTENCE                                             49660000
620014     ELSE                                                         49660000
620015        MOVE CSAPI-RETURN-CODE      TO SIAPC-RETURN-IND           49660000
620016        MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS        49660000
620017        MOVE '9900- BAD DECLARE CURSOR BCBS_STN TABLE'            49660000
620018                                    TO SIAPC-ERROR-MESSAGE        49660000
620019        PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT             49660000
620020        GO TO 9090-EXIT
620021     END-IF.                                                      49660000
620022                                                                  49660000
620023     SET CSAPI-FETCH              TO TRUE.                        49660000
620024     MOVE 1                       TO CSAPI-SUB-COMMAND.
620025     SET WS-ITDS02S1              TO TRUE.                        49660000
620026     PERFORM VARYING WS-SUB5 FROM 1 BY 1                          49660000
620027        UNTIL CSAPI-RETURN-CODE NOT = 0                           49660000
620028        CALL WS-CALL-MODULE    USING WS-ITCCSAPI                  49660000
620029                                     S02-ROW                      49660000
620030        IF CSAPI-RETURN-CODE       = 0                            49660000
620031           MOVE S02-STN-CD        TO WS-STN-CD(WS-SUB5)           49660000
620032        ELSE                                                      49660000
620033        IF CSAPI-RETURN-CODE       = 100                          49660000
620034           CONTINUE                                               49660000
620035        ELSE                                                      49660000
620036           MOVE CSAPI-RETURN-CODE      TO SIAPC-RETURN-IND        49660000
620037           MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS     49660000
620038           MOVE '9900- BAD FETCH CURSOR BCBS_STN TABLE'           49660000
620039                                       TO SIAPC-ERROR-MESSAGE     49660000
620040           PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT          49660000
620050        END-IF
620060        END-IF
620061     END-PERFORM.
620062                                                                  49660000
620063     SET CSAPI-CLOSE           TO TRUE.                           49660000
620064     MOVE 1                    TO CSAPI-SUB-COMMAND.
620065     SET WS-ITDS02S1           TO TRUE.                           49660000
620066     CALL WS-CALL-MODULE    USING WS-ITCCSAPI                     49660000
620067                                  S02-ROW.                        49660000
620068     IF CSAPI-RETURN-CODE       = 0                               49660000
620069        NEXT SENTENCE                                             49660000
620070     ELSE                                                         49660000
620071        MOVE CSAPI-RETURN-CODE      TO SIAPC-RETURN-IND           49660000
620072        MOVE CWSCC-READ-ERROR       TO SIAPC-RETURN-STATUS        49660000
620073        MOVE '9900- BAD CLOSE CURSOR BCBS_STN TABLE'              49660000
620074                                    TO SIAPC-ERROR-MESSAGE        49660000
620075        PERFORM 9990-ERROR-DISPLAY     THRU 9990-EXIT             49660000
620076        GO TO 9090-EXIT
620077     END-IF.                                                      49660000
620078
620079 9090-EXIT. EXIT.                                                 49910000
813094**********************************************************        00458700
253500 9900-RESET-SIAPC-RETURN-DATA.                                    ITCP9102
253600                                                                  ITCP9102
813097     MOVE SPACES                         TO SIAPC-RETURN-DATA.    00459000
813098     MOVE CWSCC-SUCCESSFUL               TO SIAPC-RETURN-IND.     00459100
813099     MOVE CWSCC-ACTION-COMPLETE          TO SIAPC-RETURN-STATUS.  00459200
254100                                                                  ITCP9102
254200 9900-EXIT.  EXIT.                                                ITCP9102
254300                                                                  ITCP9102
813103*                                                                 00459600
254500 9910-SIAPC-RETURN-IND-CHK.                                       ITCP9102
254600                                                                  ITCP9102
254700     IF SIAPC-RETURN-IND EQUAL CWSCC-ABORT                        ITCP9102
255000        SET CIABP-EOJ                TO TRUE                      ITCP9102
813108     END-IF.                                                      00460100
255200                                                                  ITCP9102
255300     MOVE SPACES                     TO                           ITCP9102
813111          SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL).                 00460400
813112     SUBTRACT WS-PLUS-ONE          FROM SIAPC-MODULE-LEVEL.       00460500
255700                                                                  ITCP9102
255800 9910-EXIT.  EXIT.                                                ITCP9102
255900                                                                  ITCP9102
258000 9930-FIND-ITS-30-REC.                                            ITCP9102
258100******************************************************************ITCP9102
258200****  CAPTURE THE FM130 DATA FOR BATCH USAGE FROM THE 130     ****ITCP9102
258300****  RECORD THAT CONTAINS A 'Y' IN THE PAYER INDICATOR       ****ITCP9102
258400****  FIELD.  IF THERE'S ONLY ONE OCCURRENCE, USE IT AS THE   ****ITCP9102
258500****  ITS 30 RECORD AND SET THE PAYER INDICATOR TO 'Y'.  IF   ****ITCP9102
258600****  MULTIPLE OCCURRENCES AND NO PAYER INDICATOR EQUALS 'Y'  ****ITCP9102
258700****  OR MORE THAN ONE PAYER INDICATOR EQUALS 'Y', THEN SET   ****ITCP9102
258800****  ERROR 'II018'.                                          ****ITCP9102
258900******************************************************************ITCP9102
259000                                                                  ITCP9102
259100     MOVE WS-0                       TO WS-30-REC-CNT             ITCP9102
813128                                        WS-SUB-PAYER-IND.         00462100
259300                                                                  ITCP9102
259400     PERFORM                                                      ITCP9102
259500     VARYING WS-SUB FROM 1 BY 1                                   ITCP9102
259600     UNTIL WS-SUB > 3                                             ITCP9102
259700        EVALUATE WS-SUB                                           ITCP9102
259800           WHEN 1                                                 ITCP9102
259900              MOVE CIASC-30-01-SLOT  TO FM130-RECORD              ITCP9102
260000              IF FM130-DATA NOT = SPACES                          ITCP9102
260100                 ADD 1               TO WS-30-REC-CNT             ITCP9102
260200                 IF FM130-ITS-PAYER-IND = WS-Y                    ITCP9102
260300                    ADD 1            TO WS-SUB-PAYER-IND          ITCP9102
624510                    MOVE 1           TO WS-SUB6                   50940000
260400                 END-IF                                           ITCP9102
260500              END-IF                                              ITCP9102
260600                                                                  ITCP9102
260700           WHEN 2                                                 ITCP9102
260800              MOVE CIASC-30-02-SLOT  TO FM130-RECORD              ITCP9102
260900              IF FM130-DATA NOT = SPACES                          ITCP9102
261000                 ADD 2               TO WS-30-REC-CNT             ITCP9102
261100                 IF FM130-ITS-PAYER-IND = WS-Y                    ITCP9102
261200                    ADD 2            TO WS-SUB-PAYER-IND          ITCP9102
625410                    MOVE 2           TO WS-SUB6                   50940000
261300                 END-IF                                           ITCP9102
261400              END-IF                                              ITCP9102
261500                                                                  ITCP9102
261600           WHEN 3                                                 ITCP9102
261700              MOVE CIASC-30-03-SLOT  TO FM130-RECORD              ITCP9102
261800              IF FM130-DATA NOT = SPACES                          ITCP9102
261900                 ADD 4               TO WS-30-REC-CNT             ITCP9102
262000                 IF FM130-ITS-PAYER-IND = WS-Y                    ITCP9102
262100                    ADD 4            TO WS-SUB-PAYER-IND          ITCP9102
626310                    MOVE 3           TO WS-SUB6                   50940000
262200                 END-IF                                           ITCP9102
262300              END-IF                                              ITCP9102
262400        END-EVALUATE                                              ITCP9102
813161     END-PERFORM.                                                 00465400
262600                                                                  ITCP9102
262700     EVALUATE TRUE                                                ITCP9102
262800     ALSO     TRUE                                                ITCP9102
262900        WHEN WS-ONE-30-REC                                        ITCP9102
263000        ALSO WS-NO-ITS-SUB                                        ITCP9102
263100           IF WS-HAVE-3001-REC                                    ITCP9102
263200              MOVE CIASC-30-01-SLOT  TO FM130-RECORD              ITCP9102
263300              MOVE WS-Y              TO FM130-ITS-PAYER-IND       ITCP9102
263400              MOVE FM130-RECORD      TO CIASC-30-01-SLOT          ITCP9102
263500              SET CIABP-3001-HAS-ITS-SUB TO TRUE                  ITCP9102
263600           END-IF                                                 ITCP9102
263700           IF WS-HAVE-3002-REC                                    ITCP9102
263800              MOVE CIASC-30-02-SLOT  TO FM130-RECORD              ITCP9102
263900              MOVE WS-Y              TO FM130-ITS-PAYER-IND       ITCP9102
264000              MOVE FM130-RECORD      TO CIASC-30-02-SLOT          ITCP9102
264100              SET CIABP-3002-HAS-ITS-SUB TO TRUE                  ITCP9102
264200           END-IF                                                 ITCP9102
264300           IF WS-HAVE-3003-REC                                    ITCP9102
264400              MOVE CIASC-30-03-SLOT  TO FM130-RECORD              ITCP9102
264500              MOVE WS-Y              TO FM130-ITS-PAYER-IND       ITCP9102
264600              MOVE FM130-RECORD      TO CIASC-30-03-SLOT          ITCP9102
264700              SET CIABP-3003-HAS-ITS-SUB TO TRUE                  ITCP9102
264800           END-IF                                                 ITCP9102
264900                                                                  ITCP9102
265000        WHEN WS-ONE-30-REC                                        ITCP9102
265100        ALSO WS-3001-ITS-SUB                                      ITCP9102
265200           MOVE CIASC-30-01-SLOT      TO FM130-RECORD             ITCP9102
265300           SET CIABP-3001-HAS-ITS-SUB TO TRUE                     ITCP9102
265400                                                                  ITCP9102
265500        WHEN WS-ONE-30-REC                                        ITCP9102
265600        ALSO WS-3002-ITS-SUB                                      ITCP9102
265700           MOVE CIASC-30-02-SLOT      TO FM130-RECORD             ITCP9102
265800           SET CIABP-3002-HAS-ITS-SUB TO TRUE                     ITCP9102
265900                                                                  ITCP9102
266000        WHEN WS-ONE-30-REC                                        ITCP9102
266100        ALSO WS-3003-ITS-SUB                                      ITCP9102
266200           MOVE CIASC-30-03-SLOT      TO FM130-RECORD             ITCP9102
266300           SET CIABP-3003-HAS-ITS-SUB TO TRUE                     ITCP9102
266400                                                                  ITCP9102
266500        WHEN WS-TWO-30-REC                                        ITCP9102
266600        ALSO WS-3001-ITS-SUB                                      ITCP9102
372020           PERFORM 9944-SET-ITS-PAY-IND                           03939
266700           MOVE CIASC-30-01-SLOT     TO FM130-RECORD              ITCP9102
266800           SET CIABP-3001-HAS-ITS-SUB TO TRUE                     ITCP9102
266900                                                                  ITCP9102
267000        WHEN WS-TWO-30-REC                                        ITCP9102
267100        ALSO WS-3002-ITS-SUB                                      ITCP9102
372520           PERFORM 9944-SET-ITS-PAY-IND                           03946
267200           MOVE CIASC-30-02-SLOT     TO FM130-RECORD              ITCP9102
267300           SET CIABP-3002-HAS-ITS-SUB TO TRUE                     ITCP9102
267400                                                                  ITCP9102
267500        WHEN WS-TWO-30-REC                                        ITCP9102
267600        ALSO WS-3003-ITS-SUB                                      ITCP9102
373020           PERFORM 9944-SET-ITS-PAY-IND                           03953
267700           MOVE CIASC-30-03-SLOT     TO FM130-RECORD              ITCP9102
267800           SET CIABP-3003-HAS-ITS-SUB TO TRUE                     ITCP9102
267900                                                                  ITCP9102
268000        WHEN WS-THREE-30-REC                                      ITCP9102
268100        ALSO WS-3001-ITS-SUB                                      ITCP9102
373520           PERFORM 9944-SET-ITS-PAY-IND                           03960
268200           MOVE CIASC-30-01-SLOT     TO FM130-RECORD              ITCP9102
268300           SET CIABP-3001-HAS-ITS-SUB TO TRUE                     ITCP9102
268400                                                                  ITCP9102
268500        WHEN WS-THREE-30-REC                                      ITCP9102
268600        ALSO WS-3002-ITS-SUB                                      ITCP9102
374020           PERFORM 9944-SET-ITS-PAY-IND                           03967
268700           MOVE CIASC-30-02-SLOT     TO FM130-RECORD              ITCP9102
268800           SET CIABP-3002-HAS-ITS-SUB TO TRUE                     ITCP9102
268900                                                                  ITCP9102
269000        WHEN WS-THREE-30-REC                                      ITCP9102
269100        ALSO WS-3003-ITS-SUB                                      ITCP9102
374520           PERFORM 9944-SET-ITS-PAY-IND                           03974
269200           MOVE CIASC-30-03-SLOT     TO FM130-RECORD              ITCP9102
269300           SET CIABP-3003-HAS-ITS-SUB TO TRUE                     ITCP9102
813236     END-EVALUATE.                                                00472900
813237                                                                  00472901
      *R17.5 E_104138 TGW RENAMED FIELD                                 75960000
           MOVE FM130-SUB-ID-PFX-1-3   TO CRT001-SUB-PFX-ID.            75970000
668400*    MOVE FM130-SUB-ID-PFX-1-3   TO CRT001-ALPHA-PREFIX.          75980000
269600                                                                  ITCP9102
269700 9930-EXIT.  EXIT.                                                ITCP9102
636200                                                                  51940000
192100*R17.0 E_93016 SKG : BEGIN                                        72770000
668700******************************************************************72780000
668700** VALID VALUES FOR ITS PAYER INDICATOR ARE:                      72790000
668700** 1 RECORD 30   :'Y' OR SPACE. IF SPACE REPLACE WITH 'Y'         72800000
668700** 2 OR 3 RCD 30 : 1-ST OCCURRENCE - 'Y'.                         72810000
668700**                 2-ND OR 3-D - 'N' OR SPACE. IF SPACE, REPLACE  72820000
668700**                                                       WITH 'N'.72830000
668700******************************************************************72840000
668700                                                                  72850000
668600 9932-II018-EDIT.                                                 72860000
668700                                                                  72870000
668700** VALUE 'X' IN WS-PAYER-INDICATORS INDICATES NO RECORD 30.       72880000
668700** WHEN EXITING THIS PARAGRAPH ITCFM130 SHOULD CONTAIN RECORD 30  72881000
668700** WITH PAYER INDICATOR = 'Y'                                     72890000
668700     MOVE 'XXX' TO WS-PAYER-INDICATORS.                           72900000
668700                                                                  72910000
668700     IF CIASC-30-01-SLOT(22:2)         = '30'                     72920000
668700        MOVE CIASC-30-01-SLOT         TO  FM130-RECORD            72930000
668700        IF FM130-ITS-PAYER-IND         = 'Y' OR SPACE             72940000
668700           MOVE FM130-ITS-PAYER-IND   TO  WS-PAYER-IND(1)         72950000
668700        ELSE                                                      72960000
668700           GO TO 9932-EXIT                                        72970000
668700        END-IF                                                    72980000
668700     END-IF.                                                      72990000
668700                                                                  73000000
668700     IF CIASC-30-02-SLOT(22:2)         = '30'                     73010000
668700        MOVE CIASC-30-02-SLOT         TO  FM130-RECORD            73020000
668700        IF FM130-ITS-PAYER-IND         = 'Y' OR 'N' OR SPACE      73030000
668700           MOVE FM130-ITS-PAYER-IND   TO  WS-PAYER-IND(2)         73040000
668700        ELSE                                                      73050000
668700           GO TO 9932-EXIT                                        73060000
668700        END-IF                                                    73070000
668700     END-IF.                                                      73080000
668700                                                                  73090000
668700     IF CIASC-30-03-SLOT(22:2)         = '30'                     73100000
668700        MOVE CIASC-30-03-SLOT         TO  FM130-RECORD            73110000
668700        IF FM130-ITS-PAYER-IND         = 'Y' OR 'N' OR SPACE      73120000
668700           MOVE FM130-ITS-PAYER-IND   TO  WS-PAYER-IND(3)         73130000
668700        ELSE                                                      73140000
668700           GO TO 9932-EXIT                                        73150000
668700        END-IF                                                    73160000
668700     END-IF.                                                      73170000
668700                                                                  73180000
668700** R17.0 TD_116228 EXL POPULATE FM130-RECORD BEFORE EXIT          73181000
668700** NO II018 AND NO NEED TO RESET SPACES TO 'Y' OR 'N'             73190000
668700     IF WS-PAYER-INDICATORS = 'YXX' OR 'YNX' OR 'YNN' OR 'YXN'    73200000
668700                           OR 'XYX' OR 'XXY' OR 'XYN'             73210000
668700        EVALUATE TRUE                                             73220000
668700           WHEN WS-PAYER-IND (1) = 'Y'                            73230000
668700             SET CIABP-3001-HAS-ITS-SUB TO TRUE                   73240000
668700             MOVE CIASC-30-01-SLOT      TO  FM130-RECORD          73241000
668700           WHEN WS-PAYER-IND (2) = 'Y'                            73250000
668700             SET CIABP-3002-HAS-ITS-SUB TO TRUE                   73260000
668700             MOVE CIASC-30-02-SLOT      TO  FM130-RECORD          73261000
668700           WHEN WS-PAYER-IND (3) = 'Y'                            73270000
668700             SET CIABP-3003-HAS-ITS-SUB TO TRUE                   73280000
668700             MOVE CIASC-30-03-SLOT      TO  FM130-RECORD          73281000
668700        END-EVALUATE                                              73290000
668700        GO TO 9932-EXIT                                           73300000
668700     END-IF.                                                      73310000
668700                                                                  73320000
668700** RESET BLANK PAYER FROM SPACE TO 'Y' ON THE 1-ST RECORD 30      73330000
668700     IF WS-PAYER-INDICATORS = ' XX'                               73340000
668700        MOVE 'Y'           TO FM130-ITS-PAYER-IND                 73350000
668700        MOVE FM130-RECORD  TO CIASC-30-01-SLOT                    73360000
668700        SET CIABP-3001-HAS-ITS-SUB TO TRUE                        73370000
668700        GO TO 9932-EXIT                                           73380000
668700     END-IF.                                                      73390000
668700                                                                  73400000
668700** RESET BLANK PAYER FROM SPACE TO 'Y' ON THE 2-ND RECORD 30      73410000
668700     IF WS-PAYER-INDICATORS = 'X X'                               73420000
668700        MOVE 'Y'           TO FM130-ITS-PAYER-IND                 73430000
668700        MOVE FM130-RECORD  TO CIASC-30-02-SLOT                    73440000
668700        SET CIABP-3002-HAS-ITS-SUB TO TRUE                        73450000
668700        GO TO 9932-EXIT                                           73460000
668700     END-IF.                                                      73470000
668700                                                                  73480000
668700** RESET BLANK PAYER FROM SPACE TO 'Y' ON THE 3-D RECORD 30       73490000
668700     IF WS-PAYER-INDICATORS = 'XX '                               73500000
668700        MOVE 'Y'           TO FM130-ITS-PAYER-IND                 73510000
668700        MOVE FM130-RECORD  TO CIASC-30-03-SLOT                    73520000
668700        SET CIABP-3003-HAS-ITS-SUB TO TRUE                        73530000
668700        GO TO 9932-EXIT                                           73540000
668700     END-IF.                                                      73550000
668700                                                                  73560000
668700** R17.0 TD_116228 EXL POPULATE FM130-RECORD BEFORE EXIT          73561000
668700** NO II018, RESET BLANK PAYER FROM SPACE TO 'N' ON 2-ND RECORD   73570000
668700     IF WS-PAYER-INDICATORS = 'Y X' OR 'Y  ' OR 'Y N'             73580000
668700        MOVE CIASC-30-02-SLOT      TO FM130-RECORD                73590000
668700        MOVE 'N'                   TO FM130-ITS-PAYER-IND         73600000
668700        MOVE FM130-RECORD          TO CIASC-30-02-SLOT            73610000
668700        SET CIABP-3001-HAS-ITS-SUB TO TRUE                        73620000
668700        IF WS-PAYER-INDICATORS    = 'Y  '                         73650000
668700           MOVE 'YN '              TO WS-PAYER-INDICATORS         73651000
668700           MOVE CIASC-30-03-SLOT   TO FM130-RECORD                73660000
668700        ELSE                                                      73670000
668700           MOVE CIASC-30-01-SLOT   TO FM130-RECORD                73671000
668700           GO TO 9932-EXIT                                        73680000
668700        END-IF                                                    73690000
668700     END-IF.                                                      73700000
668700                                                                  73710000
668700** R17.0 TD_116228 EXL POPULATE FM130-RECORD BEFORE EXIT          73711000
668700** NO II018, RESET BLANK PAYER FROM SPACE TO 'N' ON 3-D RECORD-   73720000
668700     IF WS-PAYER-INDICATORS = 'XY ' OR 'YX ' OR  'YN '            73730000
668700        MOVE 'N'                      TO FM130-ITS-PAYER-IND      73750000
668700        MOVE FM130-RECORD             TO CIASC-30-03-SLOT         73760000
              IF WS-PAYER-INDICATORS = 'XY '                            73760100
668700           SET CIABP-3002-HAS-ITS-SUB TO TRUE                     73761000
668700           MOVE CIASC-30-02-SLOT      TO FM130-RECORD             73761100
              ELSE                                                      73762000
668700           SET CIABP-3001-HAS-ITS-SUB TO TRUE                     73763000
668700           MOVE CIASC-30-01-SLOT      TO FM130-RECORD             73763100
              END-IF                                                    73764000
668700        GO TO 9932-EXIT                                           73770000
668700     END-IF.                                                      73780000
668700                                                                  73790000
668700** ISSUE II018 FOR ALL OTHERS                                     73800000
668700     SET CIABP-NO-ITS-SUB-FOUND TO TRUE.                          73810000
668700                                                                  73820000
668600 9932-EXIT.  EXIT.                                                73830000
192100*R17.0 E_93016 SKG : END                                          73850000
668700                                                                  73860000
636300*R14.0 E_84725 DSL BEGIN                                          15550000
636400 9940-INST-SF-MSG-U270.                                           50710000
636500******************************************************************50720000
636600****  REMOVE U270 IF CRITERIA ARE MET                         ****50730000
813241*******************************************************           00473200
636710     IF CIABP-HOST-PAR-SITE                                       11880000
636900        IF FM105-CLM-TYPE = '11'
637000           MOVE FM120-ADM-DT  TO WS-GREG-DT
637100        ELSE
637200           MOVE FM120-STMT-COV-FROM-DT TO WS-GREG-DT
637300        END-IF
637500        IF WS-GREG-DT   < WS-SANCTION-GREG-DT
639700          MOVE SPACES TO WS-SAVED-SF-MSGS-CLM
639800                         WS-SAVED-SPEC-PRC-CC-CLM
639801*R14.0.1 TD_93033 DSL ADD DAYS
639810          PERFORM VARYING WS-SUB1 FROM 1 BY 1                     53720000
639820             UNTIL WS-SUB1 > 5                                    53730000
639900             MOVE 0 TO WS-SAVED-SPEC-PRC-AMT (WS-SUB1)
639910                       WS-SAVED-SPEC-PRC-PCT (WS-SUB1)
639911                       WS-SAVED-SPEC-PRC-DAY (WS-SUB1)
639920          END-PERFORM                                             53720000
640000          MOVE 1 TO WS-SUB2
640200          PERFORM VARYING WS-SUB1 FROM 1 BY 1                     53720000
640300             UNTIL WS-SUB1 > 5                                    53730000
640400             IF FM150-SF-MSG-CD-CLM (WS-SUB1) NOT =               52000000
640500                                           ('U270' AND SPACES)    52000000
640600                MOVE FM150-SF-MSG-CD-CLM (WS-SUB1) TO             52000000
640700                     WS-SAVED-SF-MSG (WS-SUB2)                    52000000
640800                ADD 1 TO WS-SUB2                                  52000000
640900             END-IF                                               52000000
641000          END-PERFORM                                             53720000
641100          MOVE WS-SAVED-SF-MSGS-CLM TO FM150-SF-MSG-CDS-CLM
641200*                                                                 52000000
641300         MOVE 1 TO WS-SUB2
641400         PERFORM VARYING WS-SUB1 FROM 1 BY 1                      53720000
641500                                 UNTIL WS-SUB1 > 5                53730000
641600           IF FM150-SPEC-PRC-COND-CD-CLM (WS-SUB1) NOT =          52000000
641700              ('014' AND '015' AND '016' AND '017' AND '019'      52000000
641800                     AND SPACES)                                  52000000
641900              MOVE FM150-SPEC-PRC-COND-CD-CLM (WS-SUB1) TO        52000000
642000                   WS-SAVED-SPEC-PRC-CD (WS-SUB2)                 52000000
642010              MOVE FM150-SPEC-PRC-COND-AMT-CLM (WS-SUB1) TO       52000000
642020                   WS-SAVED-SPEC-PRC-AMT (WS-SUB2)                52000000
642030              MOVE FM150-SPEC-PRC-COND-PCT-CLM (WS-SUB1) TO       52000000
642040                   WS-SAVED-SPEC-PRC-PCT (WS-SUB2)                52000000
642041*R14.0.1 TD_93033 DSL ADD DAYS
642050              MOVE FM150-SPEC-PRC-COND-DAYS    (WS-SUB1) TO       52000000
642060                   WS-SAVED-SPEC-PRC-DAY (WS-SUB2)                52000000
642100              ADD 1 TO WS-SUB2                                    52000000
642200           END-IF                                                 53720000
642300         END-PERFORM                                              53720000
642400         MOVE WS-SAVED-SPEC-PRC-CC-CLM TO                         53720000
642500              FM150-SPEC-PRC-COND-CDS-CLM                         53720000
642600         PERFORM VARYING WS-SUB1 FROM 1 BY 1                      53720000
642601                                 UNTIL WS-SUB1 > 5                53730000
642602            MOVE WS-SAVED-SPEC-PRC-PCT (WS-SUB1) TO               53720000
642603                 FM150-SPEC-PRC-COND-PCT-CLM (WS-SUB1)            53720000
642604            MOVE WS-SAVED-SPEC-PRC-AMT (WS-SUB1) TO               53720000
642605                 FM150-SPEC-PRC-COND-AMT-CLM (WS-SUB1)            53720000
642606*R14.0.1 TD_93033 DSL ADD DAYS
642607            MOVE WS-SAVED-SPEC-PRC-DAY (WS-SUB1) TO               53720000
642608                 FM150-SPEC-PRC-COND-DAYS    (WS-SUB1)            53720000
642609         END-PERFORM                                              53720000
642610        END-IF                                                    53720000
642700     END-IF.                                                      53720000
642800
674510*R15.5 E_90497 DSL BEGIN                                          26340000
674520     IF WS-U200-1-IND = SPACE                                     25180000
674540        IF FM150-SF-MSG-CD-CLM (1) = ('U200' OR 'U201') OR        26310000
674550           FM150-SF-MSG-CD-CLM (2) = ('U200' OR 'U201') OR        26310000
674560           FM150-SF-MSG-CD-CLM (3) = ('U200' OR 'U201') OR        26310000
674570           FM150-SF-MSG-CD-CLM (4) = ('U200' OR 'U201') OR        26310000
674580           FM150-SF-MSG-CD-CLM (5) = ('U200' OR 'U201')           26310000
674590           MOVE 'Y' TO WS-U200-1-IND                              26320000
674591        END-IF                                                    26330000
674592     END-IF.                                                      26330000
674593*R15.5 E_90497 DSL END                                            26340000
674594                                                                  69240000
642900 9940-EXIT.       EXIT.                                           51970000
643000 9950-PROF-SF-MSG-U270.                                           50710000
643100******************************************************************50720000
643200****  REMOVE U270 IF CRITERIA ARE MET                         ****50730000
643300*******************************************************           51960000
643310     IF CIABP-HOST-PAR-SITE                                       11880000
643370        IF WS-EARLIEST-SERV-DT < WS-SANCTION-GREG-DT
643390           MOVE SPACES TO WS-SAVED-SF-MSGS-CLM
643391                          WS-SAVED-SPEC-PRC-CC-CLM
675410*R14.0.1 TD_93033 DSL ADD DAYS                                    69340000
643392           PERFORM VARYING WS-SUB1 FROM 1 BY 1                    53720000
643393             UNTIL WS-SUB1 > 5                                    53730000
643395             MOVE 0 TO WS-SAVED-SPEC-PRC-AMT (WS-SUB1)
643396                       WS-SAVED-SPEC-PRC-PCT (WS-SUB1)
643397                       WS-SAVED-SPEC-PRC-DAY (WS-SUB1)
643398           END-PERFORM                                            53720000
643399           MOVE 1 TO WS-SUB2
643400           PERFORM VARYING WS-SUB1 FROM 1 BY 1                    53720000
643401              UNTIL WS-SUB1 > 5                                   53730000
643402              IF FM1E1-SF-MSG-CD-CLM (WS-SUB1) NOT =              52000000
643403                                            ('U270' AND SPACES)   52000000
643404                 MOVE FM1E1-SF-MSG-CD-CLM (WS-SUB1) TO            52000000
643405                      WS-SAVED-SF-MSG (WS-SUB2)                   52000000
643406                 ADD 1 TO WS-SUB2                                 52000000
643407              END-IF                                              52000000
643408           END-PERFORM                                            53720000
643409           MOVE WS-SAVED-SF-MSGS-CLM TO FM1E1-SF-MSG-CDS-CLM
643410*                                                                 52000000
643411           MOVE 1 TO WS-SUB2
643412           PERFORM VARYING WS-SUB1 FROM 1 BY 1                    53720000
643413                                   UNTIL WS-SUB1 > 5              53730000
643414             IF FM1E1-SPEC-PRC-COND-CD-CLM (WS-SUB1) NOT =        52000000
643415                ('014' AND '015' AND '016' AND '017' AND '019'    52000000
643416                       AND SPACES)                                52000000
643417                MOVE FM1E1-SPEC-PRC-COND-CD-CLM (WS-SUB1) TO      52000000
643418                     WS-SAVED-SPEC-PRC-CD (WS-SUB2)               52000000
643419                MOVE FM1E1-SPEC-PRC-COND-AMT-CLM (WS-SUB1) TO     52000000
643420                     WS-SAVED-SPEC-PRC-AMT (WS-SUB2)              52000000
643421                MOVE FM1E1-SPEC-PRC-COND-PCT-CLM (WS-SUB1) TO     52000000
643422                     WS-SAVED-SPEC-PRC-PCT (WS-SUB2)              52000000
643423*R14.0.1 TD_93033 DSL ADD DAYS
643424                MOVE FM1E1-SPEC-PRC-COND-DAYS  (WS-SUB1) TO       52000000
643425                     WS-SAVED-SPEC-PRC-DAY (WS-SUB2)              52000000
643426                ADD 1 TO WS-SUB2                                  52000000
643427             END-IF                                               53720000
643428           END-PERFORM                                            53720000
643429           PERFORM VARYING WS-SUB1 FROM 1 BY 1                    53720000
643430                                   UNTIL WS-SUB1 > 5              53730000
643431             MOVE WS-SAVED-SPEC-PRC-CD  (WS-SUB1) TO              53720000
643432                  FM1E1-SPEC-PRC-COND-CD-CLM (WS-SUB1)
643433             MOVE WS-SAVED-SPEC-PRC-PCT (WS-SUB1) TO              53720000
643434                  FM1E1-SPEC-PRC-COND-PCT-CLM (WS-SUB1)           53720000
643435             MOVE WS-SAVED-SPEC-PRC-AMT (WS-SUB1) TO              53720000
643436                  FM1E1-SPEC-PRC-COND-AMT-CLM (WS-SUB1)           53720000
643437*R14.0.1 TD_93033 DSL ADD DAYS
643438             MOVE WS-SAVED-SPEC-PRC-DAY (WS-SUB1) TO              53720000
643439                  FM1E1-SPEC-PRC-COND-DAYS (WS-SUB1)              53720000
643440           END-PERFORM                                            53720000
643441        END-IF                                                    53720000
643450     END-IF.                                                      53720000
647900
679810*R15.5 E_90497 DSL BEGIN                                          69870000
679820     IF WS-U200-1-IND = SPACE                                     25180000
679860        IF FM1E1-SF-MSG-CD-CLM (1) = ('U200' OR 'U201') OR        25000000
679870           FM1E1-SF-MSG-CD-CLM (2) = ('U200' OR 'U201') OR        25000000
679880           FM1E1-SF-MSG-CD-CLM (3) = ('U200' OR 'U201') OR        25000000
679890           FM1E1-SF-MSG-CD-CLM (4) = ('U200' OR 'U201') OR        25000000
679891           FM1E1-SF-MSG-CD-CLM (5) = ('U200' OR 'U201')           25000000
679892           MOVE 'Y' TO WS-U200-1-IND                              25000000
679893        END-IF                                                    25000000
679894     END-IF.                                                      25000000
648000 9950-EXIT.       EXIT.                                           51970000
679910*R15.5 E_90497 DSL BEGIN                                          69870000
648200
379210 9944-SET-ITS-PAY-IND.                                            04054
271700                                                                  ITCP9102
379410     IF WS-3001-ITS-SUB                                           04056
379420        IF CIASC-30-02-SLOT NOT = SPACES                          04057
379430           MOVE CIASC-30-02-SLOT TO FM130-RECORD                  04058
379440           IF FM130-ITS-PAYER-IND = SPACES                        04059
379450              MOVE 'N' TO FM130-ITS-PAYER-IND                     04060
379460              MOVE FM130-RECORD TO CIASC-30-02-SLOT               04061
379470           END-IF                                                 04062
379480        END-IF                                                    04063
379490                                                                  04064
379491        IF CIASC-30-03-SLOT NOT = SPACES                          04065
379492           MOVE CIASC-30-03-SLOT TO FM130-RECORD                  04066
379493           IF FM130-ITS-PAYER-IND = SPACES                        04067
379494              MOVE 'N' TO FM130-ITS-PAYER-IND                     04068
379495              MOVE FM130-RECORD TO CIASC-30-03-SLOT               04069
379496           END-IF                                                 04070
379497        END-IF                                                    04071
379498     END-IF                                                       04072
379499                                                                  04073
379500     IF WS-3002-ITS-SUB                                           04074
379501        IF CIASC-30-01-SLOT NOT = SPACES                          04075
379502           MOVE CIASC-30-01-SLOT TO FM130-RECORD                  04076
379503           IF FM130-ITS-PAYER-IND = SPACES                        04077
379504              MOVE 'N' TO FM130-ITS-PAYER-IND                     04078
379505              MOVE FM130-RECORD TO CIASC-30-01-SLOT               04079
379506           END-IF                                                 04080
379507        END-IF                                                    04081
379508                                                                  04082
379509        IF CIASC-30-03-SLOT NOT = SPACES                          04083
379510           MOVE CIASC-30-03-SLOT TO FM130-RECORD                  04084
379511           IF FM130-ITS-PAYER-IND = SPACES                        04085
379512              MOVE 'N' TO FM130-ITS-PAYER-IND                     04086
379513              MOVE FM130-RECORD TO CIASC-30-03-SLOT               04087
379514           END-IF                                                 04088
379515        END-IF                                                    04089
379516     END-IF                                                       04090
379517                                                                  04091
379518     IF WS-3003-ITS-SUB                                           04092
379519        IF CIASC-30-01-SLOT NOT = SPACES                          04093
379520           MOVE CIASC-30-01-SLOT TO FM130-RECORD                  04094
379521           IF FM130-ITS-PAYER-IND = SPACES                        04095
379522              MOVE 'N' TO FM130-ITS-PAYER-IND                     04096
379523              MOVE FM130-RECORD TO CIASC-30-01-SLOT               04097
379524           END-IF                                                 04098
379525        END-IF                                                    04099
379526                                                                  04100
379527        IF CIASC-30-02-SLOT NOT = SPACES                          04101
379528           MOVE CIASC-30-02-SLOT TO FM130-RECORD                  04102
379529           IF FM130-ITS-PAYER-IND = SPACES                        04103
379530              MOVE 'N' TO FM130-ITS-PAYER-IND                     04104
379531              MOVE FM130-RECORD TO CIASC-30-02-SLOT               04105
379532           END-IF                                                 04106
379533        END-IF                                                    04107
379534     END-IF                                                       04108
379535     .                                                            04109
379536                                                                  04110
379537 9944-SET-ITS-PAY-IND-EXIT.  EXIT.                                04111
379538                                                                  04112
445700/                                                                 06282
813302 9945-FIND-EARLIEST-F0-DT.                                        00479300
286500                                                                  ITCP9102
813304     IF FM1A5-EST-IND = 'E'                                       00479500
813305        MOVE SIAPC-GREG-CURRENT-DT   TO FM1F0-DT-SERV-START       00479600
813306                                        FM1F0-DT-SERV-END         00479700
813307     END-IF.                                                      00479800
813308                                                                  00479900
813309     IF  FM1F0-DT-SERV-START NUMERIC                              00480000
813310     AND FM1F0-DT-SERV-START > ZEROES                             00480100
813311         IF FM1F0-DT-SERV-START       < WS-EARLIEST-SERV-DT       00480200
813312            MOVE FM1F0-DT-SERV-START TO WS-EARLIEST-SERV-DT.      00480300
287600                                                                  ITCP9102
      ** R17.5 E_112726 EXL SET WS-CST-GRP-CLASS-LN                     79480000
813314     IF FM1F0-CLASS-PROV-LN           > SPACE                     00480500
359700        MOVE FM1F0-CLASS-PROV-LN     TO WS-CLASS-PROV             ITCP9102
813316        IF FM1A5-REL-NUM  < '084'
813317           IF WS-CLASS-PROV-VALID-VALUES
813318              MOVE 'Y'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813319           ELSE
813320              MOVE 'N'               TO WS-CLASS-PROV-LN-OK-SW    00480700
382000           END-IF                                                 04139
382100        ELSE                                                      04140
813332           IF WS-CLASS-PROV-VALID-VALUES-84                       00480600
813333              MOVE 'Y'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813334           ELSE                                                   00480600
813335              MOVE 'N'               TO WS-CLASS-PROV-LN-OK-SW    00480700
382130           END-IF                                                 04143
688700        END-IF                                                    70740000
576900        IF FM1F0-CLASS-PROV-LN = '5' OR 'S' OR 'T'                79640000
                 MOVE 'Y'           TO WS-CST-GRP-CLASS-LN              79650000
              END-IF                                                    79660000
576900        IF FM1F0-CLASS-PROV-LN = '3' OR '9' OR 'E' OR 'G'         59400000
                 CONTINUE
              ELSE
                 MOVE SPACE                TO WS-PROC-CD-FLAG(1)
688700        END-IF                                                    70740000
      **R16.0.1 TD_111483 EXL START
541300**      IF FM1F0-PERF-PROV-NPI-LN  > SPACES                       55840000
541300**         MOVE 'N' TO WS-PROV-NPI-ALL-LN-BLANK                   55840000
541300**      END-IF                                                    55840000
541300**      IF FM1F0-PERF-PROV-NPI-LN  = SPACES                       55840000
541300**         MOVE 'Y' TO WS-PROV-NPI-ANY-LN-BLANK                   55840000
541300**      END-IF                                                    55840000
      **R16.0.1 TD_111483 EXL END
813337     ELSE
              MOVE SPACE                TO WS-PROC-CD-FLAG(1)
813338        MOVE 'Y'                  TO WS-CLASS-PROV-LN-BLANK-IND
813339     END-IF.
813340                                                                  00482300
      **R16.0.1 TD_111483 EXL START
541300     IF FM1F0-PERF-PROV-NPI-LN  > SPACES                          55840000
541300        MOVE 'N' TO WS-PROV-NPI-ALL-LN-BLANK                      55840000
541300     END-IF.                                                      55840000
541300     IF FM1F0-PERF-PROV-NPI-LN  = SPACES                          55840000
541300        MOVE 'Y' TO WS-PROV-NPI-ANY-LN-BLANK                      55840000
541300     END-IF.                                                      55840000
      **R16.0.1 TD_111483 EXL END
689100                                                                  70780000
813341     IF WS-CLASS-PROV-LN-OK-SW   = 'Y'                            00482400
813342        IF WS-NON-PAR-PROV                                        00482500
813343           MOVE 'Y'   TO WS-F0-CLASS-PROV-NONPAR-IND              00482600
813344        ELSE
813345           MOVE 'Y'   TO WS-F0-CLASS-PROV-PAR-IND                 00482600
360500        END-IF                                                    ITCP9102
813347     END-IF.                                                      00482800
813348                                                                  00482900
644600     MOVE SPACES                  TO WS-ICD-DIAG-TBL.             52950000
813350     MOVE 1                       TO WS-SUB1                      00483100
813351                                     WS-SUB2.                     00483200
644910*R12.0 E_19137 DSL BEGIN                                          07320000
813353     PERFORM 4 TIMES                                              00483400
645100        IF FM1F0-ICD-DIAG-CD-IND (WS-SUB1) > SPACES               53000000
645200           MOVE FM1F0-ICD-DIAG-CD-IND (WS-SUB1)                   53010000
645300             TO WS-ICD-DIAG-IND (WS-SUB2)                         53020000
645310           IF WS-ICD-DIAG-IND (WS-SUB2) (1:1) = (SPACES OR '0')   53000000
645320              MOVE WS-ICD-DIAG-IND (WS-SUB2) (2:1) TO                   00
645330                                WS-ICD-DIAG-IND (WS-SUB2) (1:1)         00
645340              MOVE SPACE TO WS-ICD-DIAG-IND (WS-SUB2) (2:1)             00
645350           END-IF                                                 53000000
813357           ADD 1                  TO WS-SUB2                      00483800
362800        END-IF                                                    ITCP9102
813359        ADD 1                     TO WS-SUB1                      00484000
813360     END-PERFORM.                                                 00484100
362900                                                                  ITCP9102
645900     MOVE WS-ICD-DIAG-IND (1)    TO FM1F0-ICD-DIAG-CD-IND (1).    53080000
646000     MOVE WS-ICD-DIAG-IND (2)    TO FM1F0-ICD-DIAG-CD-IND (2).    53090000
646100     MOVE WS-ICD-DIAG-IND (3)    TO FM1F0-ICD-DIAG-CD-IND (3).    53100000
646200     MOVE WS-ICD-DIAG-IND (4)    TO FM1F0-ICD-DIAG-CD-IND (4).    53110000
646210*R12.0 E_19137 DSL END                                            07320000
290400                                                                  ITCP9102
290500 9945-EXIT.  EXIT.                                                ITCP9102
290600                                                                  ITCP9102
813369************************************************************      00485000
813370 9948-FIND-EARLIEST-F1-DT.                                        00485100
813371                                                                  00485200
813372     IF FM1A5-EST-IND = 'E'                                       00485300
813373        MOVE SIAPC-GREG-CURRENT-DT   TO FM1F1-DT-SERV-START       00485400
813374                                        FM1F1-DT-SERV-END         00485500
813375     END-IF.                                                      00485600
290900                                                                  ITCP9102
813377     IF  FM1F1-DT-SERV-START NUMERIC                              00485800
813378     AND FM1F1-DT-SERV-START > ZEROES                             00485900
813379         IF FM1F1-DT-SERV-START       < WS-EARLIEST-SERV-DT       00486000
813380            MOVE FM1F1-DT-SERV-START TO WS-EARLIEST-SERV-DT.      00486100
279000                                                                  ITCP9102
813382     IF FM1F1-CLASS-PROV-LN           > SPACE                     00480500
813383        MOVE FM1F1-CLASS-PROV-LN     TO WS-CLASS-PROV             00480500
813384        IF FM1A5-REL-NUM  < '084'
813385           IF WS-CLASS-PROV-VALID-VALUES
813386              MOVE 'Y'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813387           ELSE
813388              MOVE 'N'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813389           END-IF
813390        ELSE
813391           IF WS-CLASS-PROV-VALID-VALUES-84                       00480600
813392              MOVE 'Y'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813393           ELSE                                                   00480600
813394              MOVE 'N'               TO WS-CLASS-PROV-LN-OK-SW    00480700
813395           END-IF
813396     ELSE
813397        MOVE 'Y'                  TO WS-CLASS-PROV-LN-BLANK-IND
813398     END-IF.
813399                                                                  00482300
813400     IF WS-CLASS-PROV-LN-OK-SW   = 'Y'                            00482400
813401        IF WS-NON-PAR-PROV                                        00482500
813402           MOVE 'Y'   TO WS-F0-CLASS-PROV-NONPAR-IND              00482600
813403        ELSE
813404           MOVE 'Y'   TO WS-F0-CLASS-PROV-PAR-IND                 00482600
813405        END-IF                                                    00482700
813406     END-IF.                                                      00482800
813407                                                                  00482900
650500     MOVE SPACES                  TO WS-ICD-DIAG-TBL.             53540000
813409     MOVE 1                       TO WS-SUB1                      00488900
813410                                     WS-SUB2.                     00489000
298300                                                                  ITCP9102
813412     PERFORM 4 TIMES                                              00489200
813413        IF FM1F1-ICD-9-DIAG-CD-IND (WS-SUB1) > SPACES             00489300
813414           MOVE FM1F1-ICD-9-DIAG-CD-IND (WS-SUB1)                 00489400
651200             TO WS-ICD-DIAG-IND (WS-SUB2)                         53610000
813416           ADD 1                  TO WS-SUB2                      00489600
813417        END-IF                                                    00489700
813418        ADD 1                     TO WS-SUB1                      00489800
813419     END-PERFORM                                                  00489900
298600                                                                  ITCP9102
651800     MOVE WS-ICD-DIAG-IND (1)    TO FM1F1-ICD-9-DIAG-CD-IND (1).  53670000
651900     MOVE WS-ICD-DIAG-IND (2)    TO FM1F1-ICD-9-DIAG-CD-IND (2).  53680000
652000     MOVE WS-ICD-DIAG-IND (3)    TO FM1F1-ICD-9-DIAG-CD-IND (3).  53690000
652100     MOVE WS-ICD-DIAG-IND (4)    TO FM1F1-ICD-9-DIAG-CD-IND (4).  53700000
280200                                                                  ITCP9102
813426     PERFORM  VARYING WS-SUB2 FROM 1 BY 1                         00490600
813427              UNTIL WS-SUB2 > 5                                   00490700
813428       IF FM1F1-TOOTH-SURFACE-CD (WS-SUB2) > SPACES               00490800
403911          MOVE SPACES TO WS-SHIFT-FIELD3                          04466
813430          MOVE FM1F1-TOOTH-SURFACE-CD(WS-SUB2) TO WS-SHIFT-FIELD2 00491000
403913          MOVE 2 TO WS-SHIFT                                      04468
403914          PERFORM 9980-SHIFT-FIELDS                               04469
813433          MOVE WS-SHIFT-FIELD2 TO FM1F1-TOOTH-SURFACE-CD(WS-SUB2) 00491300
813434       END-IF                                                     00491400
813435     END-PERFORM.                                                 00491500
403917                                                                  04472
813437     IF FM1F1-TOOTH-NUM-LN   > SPACES                             00491700
813438        MOVE SPACES TO WS-SHIFT-FIELD3                            00491800
813439        MOVE FM1F1-TOOTH-NUM-LN     TO WS-SHIFT-FIELD2            00491900
813440        MOVE 2 TO WS-SHIFT                                        00492000
813441        PERFORM 9980-SHIFT-FIELDS                                 00492100
813442        MOVE WS-SHIFT-FIELD2 TO FM1F1-TOOTH-NUM-LN                00492200
813443     END-IF.                                                      00492300
813444                                                                  00492400
813445     PERFORM  VARYING WS-SUB2 FROM 1 BY 1                         00492500
813446       UNTIL WS-SUB2 > 5                                          00492600
813447       IF FM1F1-ORAL-CAVITY-DES (WS-SUB2) > SPACES                00492700
813448          MOVE SPACES TO WS-SHIFT-FIELD3                          00492800
813449          MOVE FM1F1-ORAL-CAVITY-DES(WS-SUB2) TO WS-SHIFT-FIELD3  00492900
813450          MOVE 3 TO WS-SHIFT                                      00493000
813451          PERFORM 9980-SHIFT-FIELDS                               00493100
813452          MOVE WS-SHIFT-FIELD3 TO FM1F1-ORAL-CAVITY-DES(WS-SUB2)  00493200
403928       END-IF                                                     04483
813454     END-PERFORM.                                                 00493400
403934                                                                  04489
403935 9948-EXIT.  EXIT.                                                04490
483410                                                                  06662
813458******************************************************            00493800
289900 9960-FIND-ERROR-SLOT.                                            ITCP9102
290000                                                                  ITCP9102
290100     IF WS-INSTITUTIONAL                                          ITCP9102
332300        EVALUATE TRUE                                             ITCP9102
332400        WHEN FM105-ERR-CD (1) = SPACES                            ITCP9102
290300           MOVE 1                    TO WS-SUB1                   ITCP9102
332600                                                                  ITCP9102
332700        WHEN FM105-ERR-CD (2) = SPACES                            ITCP9102
332800           MOVE 2                    TO WS-SUB1                   ITCP9102
332900                                                                  ITCP9102
333000        WHEN FM105-ERR-CD (3) = SPACES                            ITCP9102
333100           MOVE 3                    TO WS-SUB1                   ITCP9102
333200                                                                  ITCP9102
333300        WHEN FM105-ERR-CD (4) = SPACES                            ITCP9102
333400           MOVE 4                    TO WS-SUB1                   ITCP9102
333500                                                                  ITCP9102
333600        WHEN OTHER                                                ITCP9102
333700           MOVE 5                    TO WS-SUB1                   ITCP9102
333800        END-EVALUATE                                              ITCP9102
813478     END-IF.                                                      00496000
291200                                                                  ITCP9102
291300     IF WS-PROFESSIONAL                                           ITCP9102
334200        EVALUATE TRUE                                             ITCP9102
334300        WHEN FM1A5-ERR-CD (1) = SPACES                            ITCP9102
291500           MOVE 1                    TO WS-SUB1                   ITCP9102
334500                                                                  ITCP9102
334600        WHEN FM1A5-ERR-CD (2) = SPACES                            ITCP9102
334700           MOVE 2                    TO WS-SUB1                   ITCP9102
334800                                                                  ITCP9102
334900        WHEN FM1A5-ERR-CD (3) = SPACES                            ITCP9102
335000           MOVE 3                    TO WS-SUB1                   ITCP9102
335100                                                                  ITCP9102
335200        WHEN FM1A5-ERR-CD (4) = SPACES                            ITCP9102
335300           MOVE 4                    TO WS-SUB1                   ITCP9102
335400                                                                  ITCP9102
335500        WHEN OTHER                                                ITCP9102
335600           MOVE 5                    TO WS-SUB1                   ITCP9102
335700        END-EVALUATE                                              ITCP9102
813497     END-IF.                                                      00497900
292500                                                                  ITCP9102
292600 9960-EXIT.  EXIT.                                                ITCP9102
292700                                                                  ITCP9102
293000******************************************************************ITCP9102
293100****  THIS PARAGRAPH CHECKS EACH OF THE FIVE ERROR CODE SLOTS ****ITCP9102
293200****  FROM THE SF HEADER RECORD TO DETERMINE WHEATHER EDITING ****ITCP9102
293300****  SHOULD CONTINUE OR A CLAIM SHOULD BE PLACED ON THE      ****ITCP9102
293400****  DUPLICATE/INVALID RELEASE FILE AND REPORT.              ****ITCP9102
293500******************************************************************ITCP9102
813507 9970-ERROR-CODE-CHK.                                             00498900
293600                                                                  ITCP9102
813509     MOVE 'N'          TO WS-DUP-FILE-IND.                        00499100
293900                                                                  ITCP9102
294000     IF WS-INSTITUTIONAL                                          ITCP9102
813512        PERFORM VARYING WS-SUB FROM 1 BY 1                        00499400
813513           UNTIL WS-SUB > 5                                       00499500
813514           IF FM105-ERR-CD(WS-SUB) = SPACE                        00499600
813515              MOVE 5       TO WS-SUB                              00499700
813516           ELSE                                                   00499800
813517              MOVE FM105-ERR-CD(WS-SUB) TO WS-ERR-CD              00499900
813518              IF WS-END-EDIT-ERR-CD                               00500000
813519                 SET WS-BYPASS-EDITS    TO TRUE                   00500100
813520              END-IF                                              00500200
813521              IF WS-DUP-FILE-ERR-CD                               00500300
813522                 MOVE 'Y'        TO WS-DUP-FILE-IND               00500400
813523              END-IF                                              00500500
295000           END-IF                                                 ITCP9102
295100        END-PERFORM                                               ITCP9102
295200     ELSE                                                         ITCP9102
813527        PERFORM VARYING WS-SUB FROM 1 BY 1                        00500900
813528           UNTIL WS-SUB > 5                                       00501000
813529           IF FM1A5-ERR-CD(WS-SUB) = SPACE                        00501100
813530              MOVE 5       TO WS-SUB                              00501200
813531           ELSE                                                   00501300
813532              MOVE FM1A5-ERR-CD(WS-SUB) TO WS-ERR-CD              00501400
813533              IF WS-END-EDIT-ERR-CD                               00501500
813534                 SET WS-BYPASS-EDITS       TO TRUE                00501600
813535              END-IF                                              00501700
813536              IF WS-DUP-FILE-ERR-CD                               00501800
813537                 MOVE 'Y'      TO WS-DUP-FILE-IND                 00501900
813538              END-IF                                              00502000
296200           END-IF                                                 ITCP9102
296300        END-PERFORM                                               ITCP9102
813541     END-IF.                                                      00502300
296600                                                                  ITCP9102
296700 9970-EXIT.  EXIT.                                                ITCP9102
813544************************************************************      00502600
813545 9980-SHIFT-FIELDS.                                               00502700
296800                                                                  ITCP9102
421920      SUBTRACT 1 FROM WS-SHIFT                                    04824
421921      PERFORM WS-SHIFT TIMES                                      04825
421930         IF WS-SHIFT-3(1:1) = SPACES OR LOW-VALUES                04826
421940            MOVE WS-SHIFT-3(2:1) TO WS-SHIFT-3(1:1)               04827
421950            MOVE WS-SHIFT-3(3:1) TO WS-SHIFT-3(2:1)               04828
421960            MOVE SPACE           TO WS-SHIFT-3(3:1)               04829
421970         END-IF                                                   04830
813554      END-PERFORM.                                                00503600
813555                                                                  00503700
813556************************************************************      00503800
297000 9990-ERROR-DISPLAY.                                              ITCP9102
297100                                                                  ITCP9102
813559     MOVE CWSCC-ABORT                 TO SIAPC-RETURN-IND.        00504100
297300     MOVE '3600'                      TO SIAPC-ABEND-CODE         ITCP9102
813562     SET WS-BYPASS-EDITS              TO TRUE.                    00504400
813563     SET CIABP-EOJ                    TO TRUE.                    00504500
298200                                                                  ITCP9102
551000     DISPLAY '9102 ERROR ' SIAPC-RETURN-MESSAGE-CD ' '            55100000
666200             'RETURN CD  ' CSAPI-RETURN-CODE.                     55110000
666300
666500     IF  CIART-RT-IND = ('Y' OR 'C')                              55120000
813566     AND CSAPI-RETURN-CODE < 0                                    00504720
813567         PERFORM 8880-ERROR-LOG                                   00504730
813568     END-IF.                                                      00504740
306400                                                                  ITCP9102
551700 9990-EXIT. EXIT.                                                 55170000
713100                                                                  83490001
516700 COPY ITCPDDT.                                                    05343
813573********************************************************          00505100
