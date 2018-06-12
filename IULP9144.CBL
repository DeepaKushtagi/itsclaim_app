       IDENTIFICATION DIVISION.
       PROGRAM-ID.                       IULP9144.
       AUTHOR.                           IBS.
       DATE-WRITTEN.                     MARCH 1996.
       DATE-COMPILED.
      ******************************************************************
      *                                                                *
000800*            INTER-PLAN TELEPROCESSING SERVICES (ITS)            *
      *                                                                *
      *   MODULE:       IULP9144                                       *
001100*   NAME:         ALIM NON-LIACS INTERFACE AREAS BUILD           *
001200*   VERSION:      18.2                                           *
001300*   DATE:         11/12/2015                                     *
      *                                                                *
      *   APPLICATION:  AID LINE ITEM MANAGER (ALIM)                   *
      *   FUNCTION:     THIS MODULE IS CALLED BY THE LINE PROCESS      *
      *                 INIT CONTROLLER AND THE LINE PROCESS LINE      *
      *                 CONTROLLER TO UPDATE THE REMAINDER OF THE      *
      *                 WORK CLAIM AREAS (THE LIACS AREA IS ALREADY    *
      *                 UPDATED).                                      *
      *                                                                *
      *   MODIFICATION                                                 *
      *   HISTORY:      03/15/96 01.0  - INITIAL RELEASE               *
      *                 05/31/96 02.0  - BUILDE NEW LIAREP FIELD       *
      *                                  FOR RULES WORK-AROUND         *
      *                 10/28/96 03.0  - IN PARA C600-MOVE-.... THE    *
      *                                  SUBSCRIPT (LSUB) SHOULD BE (1)
      *                                  IN STATEMENTS:                *
      *                                  MOVE CIAAJR-MESSAGE.CD ...... *
      *                                  MOVE CIAAJR-BMSP-ADJ-RSN-CD ..*
      *                 01/17/97 04.0    CONVERSION TO VERSION 2.0     *
      *                                  (ITS REL 8.0)                 *
      *                 07/31/97 05.0    C800-MOVE-CALCS-TO-CALREP     *
      *                                  ADD:                          *
      *                                   POPULATE                     *
      *                                   LIAREP-CLM-PRC-AMT-AMT-CALC-CD
      *                 12/05/97 06.0    C200-MOVE-IUCIAAJR-TO-SFAJR   *
      *                                  MOD HOW SJR-PRC-MTD-CLM IS    *
      *                                  POPULATED, USING AJR-PRC-MTD- *
      *                                  ALT AND AJR-PRC-MTD-CLM.      *
      *                                                                *
      *                 03/27/98 07.0    PROMPT PAY NYS MANDATE        *
      *                                  ADD:1.INVEST-RECEIPT-DT       *
      *                                      2.ASO-IND                 *
      *                                                                *
      *                 08/07/98 08.0    ENHANCED  ADJUSTMENT          *
      *                                  ADD:1.LIASJR-PROCESS-CD       *
      *                                                                *
      *                 11/13/98 09.0 -  SUPPLEMENTAL AND SURCHARGE    *
      *                                  1.PARA C450-. ADD MOVE FOR:   *
      *                                   CO-SSAB-MAX-SUPP-AMT OF SFCO *
      *                                   CO-SSAB-SURCHARGE-AMT OF SFCO*
      *                                  2.PARA C700-. ADD MOVE FOR:   *
      *                                   CO-SSAB-MAX-SUPP-AMT OF ADJCO*
      *                                   CO-SSAB-SURCHARGE-AMT OF ADJCO
      *                 07/30/99 10.0 -  CAPITATION                    *
      *                                  1.ADD IUCIADR TO LINKAGE SEC  *
      *                                    AND PROCEDURE DIV USING     *
      *                                  2.MOD THE WAY                 *
      *                                    CIACP-CAP-IND OF SFCP (LSUB)*
      *                                    IS POPULATED IN PARA C350   *
      *                 03/06/00 11.0 -  SPLIT CLAIM PROCESS           *
      *                                  1.MOD C800-                   *
      *                                    MOVE                        *
      *                                    CS-LAST-PROC-SPLT-CLM-SET   *
      *                                    TO REP-LAST-PROC-SPLT-CLM-SET
      *                                  2.MOD C500-                   *
      *                                    MOVE                        *
      *                                    LM-SPLIT-CLM-SET-NUM        *
      *                                    TO LIAALM-SPLIT-CLM-SET-NUM *
      *                                  3.MOD C100-                   *
      *                                    MOVE                        *
      *                                    ZERO                        *
      *                                    TO LIALM-SPLIT-CLM-SET-NUM  *
      *                                       OF SFLM                  *
      *                 07/31/00 12.0 -  PAY MAX                       *
      *                                  1.MOD C400-                   *
      *                                    REN LIFE-BEN-MAX-ALLWD TO   *
      *                                    PAY-MAX-REM-CLM-1           *
      *                                    ADD PAY-MAX-REM-CLM-2       *
      *                                        PAY-MAX-REM-CLM-3       *
      *                                  2.MOD C450-                   *
      *                                    ADD PMS-PAY-PAX-AMT         *
      *                                    ADD PMB-PAY-PAX-AMT         *
      *                                    REMOVE REF TO BMFAP-MAX-DOL *
      *                                  3.MOD C600-                   *
      *                                    REMOVE REF TO SUB-GRP-NUM   *
      *                                              INVEST-RECEIPT-DT *
      *                                  4.MOD C700-                   *
      *                                    ADD PMS-PAY-PAX-AMT         *
      *                                    ADD PMB-PAY-PAX-AMT         *
      *                                  5.MOD C200-                   *
      *                                    ADD LIALM-LIASJR-STREAMLINE-*
      *                                        ADJ-IN                  *
      *                 10/22/01 13.0 -  OPL                           *
      *                                  1.MOD C400-                   *
      *                                    REN LIFE-BEN-MAX-ALLWD TO   *
      *                 12/10/02 14.0 -  NEW CP/AJR FIELDS(MEDICARE)   *
      *                                  1.C200-MOVE-IUCIAAJR-TO-SFAJR *
      *                                  2.C300-MOVE-ITCIACP-TO-SFCP   *
      *                 01/12/04 15.0  - DL004(SFCO.INIT-PMT-AMT( ) NEG)
      *                                  1.MOD C450-                   *
      *                 06/30/04 16.0  - NEW CP FLDS(REL-NUM,REV-TYPE-CD
      *                                  1.MOD C300-,C350-             *
      *                                - COVERED PRIV ROOM DIFFERENTIAL*
      *                                  1.MOD C800-                   *
010610*                 11/16/07 17.0  - TM-2007-11-R10.1              *
010620*                                - IBS LINE EXPANSION            *
010640*                 06/11/09 17.1 - TECH MEMO ITS-TM-09-06-R10.4   *
010650*                                 MPP E_34835                    *
010651*                 10/16/09 17.2 - TECH MEMO                      *
010652*                                 ITS-TM-2009-10-R10.4.2         *
010653*                                 DME TD_51390                   *
      ******************************************************************00020004
      * REL   - DATE       - VERS - CONTROL  - DESCRIPTION             *00030003
      *R11.2  ¦ 11/11/2010 ¦ 17.3 ¦ E_32688  ¦ DJL: 5010 CHANGES       *00040006
      *R11.4  ¦ 11/10/2011 ¦ 17.4 ¦ E_41590  ¦ DJL: COST SHARE SEQUENCE*00040006
010800*         ADD SUBSCRIBER COST SHARING SEQUENCE CODE TO ADJCI     *
      *R12.0  ¦ 05/17/2012 ¦ 17.5 ¦ E_41233  ¦ DJL: MULTIPLE INCLUSIVE *00040014
      *         GROUPINGS - POPULATE NEW FIELDS                        *00050013
      *R12.0.1¦ 08/23/2012 ¦ 17.6 ¦ TD_77549 ¦ GWS: IN PARA A120-CS-...*00341000
      *         INITIALIZE NEW FIELD LIAREP-PM42-PRC-AMT-RED-WKARND    *00342001
      *R13.0.1¦ 02/21/2013 ¦ 17.7 ¦ PD_83690 ¦ DJL: CORRECT SUBSCRIPT  *00341000
      *         USED TO POPULATE LIAREP-PM42_PRC-AMT-RED-WKARND        *
      *R13.5  ¦ 05/09/2013 ¦ 17.8 ¦ E_39938  ¦ DJL: ADD LOGIC TO SAVE  *00341000
      *         THE SYSTEM CREATOR CODE AND TRANSMISSION MODE CODE OVR *
      *R14.0  ¦ 11/07/2013 ¦ 17.9 ¦ E_05931  ¦ RDW: MOVE BYP PRV RM DIF*
      *        FROM ITCIACP TO SFCP.                                   *
      *R14.0  ¦ 11/07/2013 ¦ 17.9 ¦ E_64148  ¦ DJL: SUPPLEMENTAL       *
      *        MOVE THRESHOLD AMOUNT FROM ITCIACP TO SFCP              *
      *        MOVE SUPP ACCUMULATOR EXC SUB REDUCTIONS FROM ITCIACO   *
      *        TO SFCO AND LIACO                                       *
      *R14.0  ¦ 11/07/2013 ¦ 17.9 ¦ E_84725  ¦ RDW: ADD PROVIDER SANC  *
      *        MOVE CIACO-BMFAP-SANC-DOL-RED,                          *
      *        CIACO-BMFAP-SANC-PCT-RED, CIACO-BMFAP-EXCESS-DOL-RED,   *
      *        CIACO-BMFAS-PROV-SANC-RED,                              *
      *        CIACO-BMFAP-EXCESS-PER-DAY-RED,CIACP-PROV-SANC-INCUR-DT,*
      *        CIACP-CLASS-PROV-CLM OF ITCIACP TO SFCP & LIACO         *
      *        REMOVE CIACO-BMFAP-DOL-RED & CIACO-BMFAP-PCT-RED        *
      *R14.0  ¦ 11/07/2013 ¦ 17.9 ¦ E_72114  ¦ KJO: PROVIDER PENALTIES *
      *        - ALSO INCLUDES E_27465.  MOVE CIACP-CLASS-PROV-LN FROM *
      *        ITCIACP TO SFCP.                                        *
      *R14.5  ¦ 05/15/2014 ¦ 18.0 ¦ E_88250  ¦ RDW: ADD LENGTH STAY    *
      *R14.5  ¦ 05/15/2014 ¦ 18.0 ¦ E_34072  ¦ DJL: B2 ADJUSTMENT MSGS *
      *        CAPTURE THE ADJUSTMENT RECEIPT DATE ON AJR              *
      *R15.0  ¦ 11/06/2014 ¦ 18.1 ¦ E_24765  ¦ KJO: ALSO INCLUDES      *
      *         E_84772 & E_91421.  MOVE CIACP-DEL-MTD.                *
      *R16.0  ¦ 11/12/2015 ¦ 18.2 ¦ E_57627  ¦ DJL: AUTOMATE MEDICARE  *
      *         AND MEDICAID PAYMENT REDUCTION REASON 3 WITH PM 01     *
      *         ADD NEW ELEMENTS AT THE CLAIM LEVEL:                   *
      *            LIACS-ALT-HOME-PRC-IND                              *
      *            LIACS-INT-ALT-HOME-PRC-AMT-CLM                      *
      *            LIACS-REM-ALT-HOME-PRC-AMT-CLM                      *
      *            LIACS-TOT-REM-CHRG                                  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
012000
      *******   W O R K I N G - S T O R A G E   S E C T I O N   *******
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      *
      *11.2    E_32688  BZXDJL1 BEGIN                                   00010004
       01  WS-PROGRAM-INFO.                                             00020000
           05  WS-MODULE-ID               PIC X(8)  VALUE 'IULP9144'.   00030002
           05  WS-PROGRAM-ID              REDEFINES WS-MODULE-ID        00040001
                                          PIC X(8).                     00050001
           05  WS-VERSION-NUMBER          PIC X(4)  VALUE '18.2'.       00060002
           05  WS-RELEASE-DATE            PIC X(8)  VALUE '11/12/15'.   00070004
           05  WS-PROGRAM-DISPLAY.                                      00080000
               10  WS-COMMON-DISPLAY      PIC X(30) VALUE               00090000
                   'IULP9144-ALIM WRK CLM IA BLD '.                     00100002
               10  WS-MODULE-DISPLAY      PIC X(8)  VALUE ' MODULE '.   00110000
               10  WS-RELEASE-DISPLAY     PIC X(34) VALUE               00120000
                   'VERSION #18.2 RELEASED ON 11/12/15'.                00130004
           05  WS-DESCRIPTION             REDEFINES WS-PROGRAM-DISPLAY  00140001
                                          PIC X(72).                    00150001

       01  WS-FIRST-TIME-IND              PIC X(01) VALUE 'Y'.
           88  WS-FIRST-TIME                        VALUE 'Y'.
           88  WS-SUBSEQUENT-CALLS                  VALUE 'N'.
      *11.2   E_32688  BZXDJL1 END                                      00160004

       01  WS-REQUESTED-MODULE              PIC X(08) VALUE SPACES.
      *
       01  WS-INDICATORS.
      *11.2   E_32688  BZXDJL1                                          00160004
013200     05  WS-FIRST-TIME-FLAG           PIC X(01) VALUE 'Y'.
               88  FIRST-TIME                         VALUE 'Y'.
      *
       01  WS-CONSTANT-AREA.
           05  INST-ACCOM-LN                PIC X(01) VALUE '1'.
           05  INST-ANC-LN                  PIC X(01) VALUE '2'.
           05  PROFESSIONAL-LN              PIC X(01) VALUE '3'.
           05  INCLUSIVE-CALC               PIC X(02) VALUE '20'.
           05  INCLUSIVE-LN-CALC            PIC X(02) VALUE '21'.
           05  WS-NO                        PIC X(01) VALUE 'N'.
           05  WS-PLUS-ONE            COMP  PIC S9(4) VALUE +1.
           05  WS-PLUS-TWO            COMP  PIC S9(4) VALUE +2.
      *
       01  WS-SUBSCRIPT-AREA.
           05  SUB                    COMP  PIC S9(4) VALUE +0.
           05  LSUB                   COMP  PIC S9(4) VALUE +0.
           05  ASUB                   COMP  PIC S9(4) VALUE +0.
           05  BSUB                   COMP  PIC S9(4) VALUE +0.
      *
       01  IUCWSCC.
           COPY IUCWSCC.
015400
      *11.2   E_32688  BZXDJL1 BEGIN                                    00160004
       01  ITCCSAPI.
           COPY ITCCSAPI.
      *11.2   E_32688  BZXDJL1 END                                      00160004

       01  FILLER-1                          PIC X(60) VALUE
           '***  IULP9144 WORKING-STORAGE ENDS HERE  ***'.
015700
      **************    L I N K A G E   S E C T I O N    **************
      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*
016200
       01  ITSIAPC.
           COPY ITSIAPC.
016500
       01  IULIALM.
           COPY IULIALM.
016800
       01  IUCIAAJR.
           COPY IUCIAAJR.
017100
       01  ITCIACP.
           COPY ITCIACP.
017400
       01  ITCIACO.
           COPY ITCIACO.
017700
       01  SFLM.
           COPY IULIALM.
018000
       01  SFAJR.
           COPY IULIASJR.
018300
       01  SFCP.
           COPY ITCIACP REPLACING CIACP-TBL-ENTRY-CNT
                                BY SFCP-TBL-ENTRY-CNT.
018700
       01  SFCO.
           COPY ITCIACO REPLACING CIACO-TBL-ENTRY-CNT
                                BY SFCO-TBL-ENTRY-CNT.
019100
       01  IUCIADR.
           COPY IUCIADR.
019400
       01  ADJLM.
           COPY IULIAALM.
019700
       01  ADJAJR.
           COPY IULIAAJR.
020000
       01  ADJCO.
           COPY IULIACO.
020300
       01  CALCS.
           COPY IULIACS.
020600
       01  CALREP.
           COPY IULIAREP.
020900
      ***OPL START
       01  ITCIACI.
           COPY ITCIACI.
021300
       01  ADJCI.
           COPY IULIACI.
      ***OPL END
021600

      ******************************************************************
      ******************************************************************
      *    P R O C E D U R E    D I V I S I O N                        *
      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION USING ITSIAPC
                                IULIALM
                                IUCIAAJR
                                ITCIACP
                                ITCIACO
                                SFLM
                                SFAJR
                                SFCP
                                SFCO
                                IUCIADR
                                ADJLM
                                ADJAJR
                                ADJCO
                                CALCS
                                CALREP
      ***OPL START
                                ITCIACI
                                ADJCI.
      ***OPL END
      ******************************************************************
       0000-MAINLINE.
      ******************************************************************

      *11.2   E_32688  BZXDJL1 BEGIN                                    00160004
           INITIALIZE ITCCSAPI.
           IF LIALM-BATCH-VSAM OF IULIALM
               SET CSAPI-BATCH-ENV TO TRUE
           END-IF.
           COPY ITVERDSP.
      *11.2   E_32688  BZXDJL1 END                                      00160004
024500
           PERFORM A000-INITIALIZATION.

           IF SIAPC-FUNCTION-CD-MAJOR = CWSCC-INIT-FUNCTION AND
              SIAPC-FUNCTION-CD-SUB   = CWSCC-INIT-WK-CLAIM-AREA-UPDT
              PERFORM B100-INIT-PROCESSING
           ELSE
           IF SIAPC-FUNCTION-CD-MAJOR = CWSCC-LINE-PROCESS  AND
              SIAPC-FUNCTION-CD-SUB   = CWSCC-INIT-WK-CLAIM-AREA-UPDT
              PERFORM B200-LN-PROCESSING
           ELSE
              PERFORM Z100-INVALID-FUNCTION.

           GOBACK.

      ******************************************************************
       A000-INITIALIZATION.
      ******************************************************************

           IF FIRST-TIME
      *11.2   E_32688  BZXDJL1                                          00160004
026500        MOVE WS-NO TO WS-FIRST-TIME-FLAG.

           PERFORM  X100-INIT-SIAPC-RETURN-DATA.

      ******************************************************************
       B100-INIT-PROCESSING.
      ******************************************************************

           PERFORM C100-MOVE-IULIALM-TO-SFLM.

           PERFORM C200-MOVE-IUCIAAJR-TO-SFAJR.

           PERFORM C300-MOVE-ITCIACP-TO-SFCP.

           PERFORM C400-MOVE-ITCIACO-TO-SFCO.

      ******************************************************************
       B200-LN-PROCESSING.
      ******************************************************************

           PERFORM C500-MOVE-IULIALM-TO-ADJLM.

           PERFORM C600-MOVE-IUCIAAJR-TO-ADJAJR.

           PERFORM C700-MOVE-ITCIACO-TO-ADJCO.

      ***OPL START
           IF LIALM-L2-CIACI-SAVE-FOR-C2 OF SFLM
              PERFORM C900-MOVE-ITCIACI-TO-ADJCI.
      ***OPL END

           IF LIACS-NO-MULTI-PROC
              PERFORM C800-MOVE-CALCS-TO-CALREP.

      ******************************************************************
       C100-MOVE-IULIALM-TO-SFLM.
      ******************************************************************

           MOVE LIALM-SCCF-SER-NUM    OF IULIALM
                              TO LIALM-SCCF-SER-NUM    OF SFLM.
           MOVE LIALM-TYPE-PROCESS-CD OF IULIALM
                              TO LIALM-TYPE-PROCESS-CD OF SFLM.
           MOVE LIALM-DB-ACTION-CD    OF IULIALM
                              TO LIALM-DB-ACTION-CD    OF SFLM.
           MOVE LIALM-WRITE-RECON-CD  OF IULIALM
                              TO LIALM-WRITE-RECON-CD  OF SFLM.
           MOVE LIALM-ENVIRONMENT     OF IULIALM
                              TO LIALM-ENVIRONMENT     OF SFLM.
           MOVE LIALM-WORK-CLM-DB     OF IULIALM
                              TO LIALM-WORK-CLM-DB     OF SFLM.
           MOVE LIALM-TOT-CHRGS       OF IULIALM
                              TO LIALM-TOT-CHRGS       OF SFLM.
      *OPL START
           MOVE  LIALM-TYPE-PROCESS-COB-CD         OF IULIALM
             TO  LIALM-TYPE-PROCESS-COB-CD         OF SFLM
           MOVE  LIALM-COB-SAVE-CIACI-AREA-CD      OF IULIALM
             TO  LIALM-COB-SAVE-CIACI-AREA-CD      OF SFLM
           MOVE  LIALM-COB-USE-CIACI-AREA-CD       OF IULIALM
             TO  LIALM-COB-USE-CIACI-AREA-CD       OF SFLM
           MOVE  LIALM-COB-ACCUM-2ND-PAY-OPT-CD    OF IULIALM
             TO  LIALM-COB-ACCUM-2ND-PAY-OPT-CD    OF SFLM.
      *OPL END


           MOVE ZEROES        TO LIALM-BEN-LEV         OF SFLM
                                 LIALM-ADJUD-LN-NUM    OF SFLM
                                 LIALM-SCCF-LN-NUM     OF SFLM
                                 LIALM-SPLIT-FRM-ADJUD-LN-NUM
                                                       OF SFLM
                                 LIALM-LN-CHRG-INIT
                                                       OF SFLM
                                 LIALM-BEN-LEV-CHRG    OF SFLM
                               LIALM-SPLIT-CLM-SET-NUM OF SFLM.

           MOVE SPACE         TO LIALM-SPLIT-LN-CD     OF SFLM.

      ******************************************************************
       C200-MOVE-IUCIAAJR-TO-SFAJR.
      ******************************************************************

           MOVE CIAAJR-PRC-MTD-ALT-IND   OF IUCIAAJR
                              TO LIASJR-PRC-MTD-ALT-IND   OF SFAJR.
           MOVE CIAAJR-FUNC-CD           OF IUCIAAJR
                              TO LIASJR-FUNC-CD           OF SFAJR.
           MOVE CIAAJR-LAST-NAME-1-3     OF IUCIAAJR
                              TO LIASJR-LAST-NAME-1-3     OF SFAJR.
           MOVE CIAAJR-ADJUD-DT          OF IUCIAAJR
                              TO LIASJR-ADJUD-DT          OF SFAJR.
           MOVE CIAAJR-SUB-ID-NUM-ACTUAL OF IUCIAAJR
                              TO LIASJR-SUB-ID-NUM-ACTUAL OF SFAJR.
           MOVE CIAAJR-CLM-REASON-CDS    OF IUCIAAJR
                              TO LIASJR-CLM-REASON-CDS    OF SFAJR.
           MOVE CIAAJR-DF-MSG-CDS-CLM    OF IUCIAAJR
                              TO LIASJR-DF-MSG-CDS-CLM    OF SFAJR.
           MOVE CIAAJR-PAT-SEX           OF IUCIAAJR
                              TO LIASJR-PAT-SEX           OF SFAJR.
           MOVE CIAAJR-PAT-BRT-DT        OF IUCIAAJR
                              TO LIASJR-PAT-BRT-DT        OF SFAJR.
036700     MOVE CIAAJR-CROSS-REF-SCCF-NUM OF IUCIAAJR
                              TO LIASJR-CROSS-REF-SCCF-NUM
                                                          OF SFAJR.
           MOVE CIAAJR-ADJ-RSN-CD        OF IUCIAAJR
                              TO LIASJR-ADJ-RSN-CD        OF SFAJR.

037300     IF CIAAJR-PRC-MTD-ALT-YES     OF IUCIAAJR
037400        MOVE CIACP-PRC-MTD-LN      OF ITCIACP (WS-PLUS-ONE)
037500                           TO LIASJR-PRC-MTD-CLM    OF SFAJR
           ELSE
037700        MOVE CIAAJR-PRC-MTD-CLM    OF IUCIAAJR
037800                           TO LIASJR-PRC-MTD-CLM    OF SFAJR.

           MOVE CIAAJR-ORIG-OPERATOR-ID  OF IUCIAAJR
                              TO LIASJR-ORIG-OPERATOR-ID  OF SFAJR.
038200     MOVE CIAAJR-ORIG-PROCESS-DT   OF IUCIAAJR
                              TO LIASJR-ORIG-PROCESS-DT
                                                          OF SFAJR.
           MOVE CIAAJR-PAT-LAST-NAME     OF IUCIAAJR
                              TO LIASJR-PAT-LAST-NAME     OF SFAJR.
           MOVE CIAAJR-PAT-FIRST-NAME    OF IUCIAAJR
                              TO LIASJR-PAT-FIRST-NAME    OF SFAJR.

039000     MOVE CIAAJR-PAT-MID-INIT          OF IUCIAAJR
039100       TO LIASJR-PAT-MID-INIT          OF SFAJR.

039300     MOVE CIAAJR-SUB-GRP-NUM           OF IUCIAAJR
039400       TO LIASJR-SUB-GRP-NUM           OF SFAJR.
039500     MOVE CIAAJR-INVEST-IND            OF IUCIAAJR
039600       TO LIASJR-INVEST-IND            OF SFAJR.
039700     IF CIAAJR-INVEST-DAY              OF IUCIAAJR NUMERIC
039800        MOVE CIAAJR-INVEST-DAY         OF IUCIAAJR
039900          TO LIASJR-INVEST-DAY         OF SFAJR
           ELSE
040100        MOVE CIAAJR-INVEST-DAY-X       OF IUCIAAJR
040200          TO LIASJR-INVEST-DAY-X       OF SFAJR.
040300     MOVE CIAAJR-CARVE-OUT-IND         OF IUCIAAJR
040400       TO LIASJR-CARVE-OUT-IND         OF SFAJR.
040500     IF CIAAJR-CARVE-OUT-DAY           OF IUCIAAJR NUMERIC
040600        MOVE CIAAJR-CARVE-OUT-DAY      OF IUCIAAJR
040700          TO LIASJR-CARVE-OUT-DAY      OF SFAJR
           ELSE
040900        MOVE CIAAJR-CARVE-OUT-DAY-X    OF IUCIAAJR
041000          TO LIASJR-CARVE-OUT-DAY-X    OF SFAJR.
041100     MOVE CIAAJR-INVEST-RECEIPT-DT     OF IUCIAAJR
041200       TO LIASJR-INVEST-RECEIPT-DT     OF SFAJR.
041300     MOVE CIAAJR-ASO-IND               OF IUCIAAJR
041400       TO LIASJR-ASO-IND               OF SFAJR.
041500     MOVE CIAAJR-PROCESS-CD            OF IUCIAAJR
041600       TO LIASJR-PROCESS-CD            OF SFAJR.
      ***OPL
041800     MOVE CIAAJR-PRIM-IND              OF IUCIAAJR
041900       TO LIASJR-PRIM-IND              OF SFAJR.
042000     MOVE CIAAJR-INDEM-CUTBK-IND       OF IUCIAAJR
042100       TO LIASJR-INDEM-CUTBK-IND       OF SFAJR.
      ***OPL PART OF 8.4 RELEASE
042300     MOVE CIAAJR-STREAMLINE-ADJ-IND    OF IUCIAAJR
             TO LIASJR-STREAMLINE-ADJ-IND    OF SFAJR.

           MOVE CIAAJR-LINE-BUS              OF IUCIAAJR
             TO LIASJR-LINE-BUS              OF SFAJR.
           MOVE CIAAJR-MED-ASGNMT-IND        OF IUCIAAJR
             TO LIASJR-MED-ASGNMT-IND        OF SFAJR.
042920*TM-2009-06-R10.4 BEG E_34835
042922     MOVE CIAAJR-BOID                  OF IUCIAAJR
042923       TO LIASJR-BOID                  OF SFAJR.
042924     SET LIASJR-BOID-ASSIGNED  OF SFAJR TO TRUE.
042929*TM-2009-06-R10.4 END E_34835
042992
      *11.2   E_32688  BZXDJL1 BEGIN                                    00160004
           MOVE CIAAJR-CORR-PRI-PAYER-NM     OF IUCIAAJR
             TO LIASJR-CORR-PRI-PAYER-NM     OF SFAJR.
           MOVE CIAAJR-OTH-SUB-LAST-NM       OF IUCIAAJR
             TO LIASJR-OTH-SUB-LAST-NM       OF SFAJR.
           MOVE CIAAJR-OTH-SUB-MDL-INIT      OF IUCIAAJR
             TO LIASJR-OTH-SUB-MDL-INIT      OF SFAJR.
           MOVE CIAAJR-OTH-SUB-FIRST-NM      OF IUCIAAJR
             TO LIASJR-OTH-SUB-FIRST-NM      OF SFAJR.
           MOVE CIAAJR-OTH-SUB-ID-NUM        OF IUCIAAJR
             TO LIASJR-OTH-SUB-ID-NUM        OF SFAJR.
           MOVE CIAAJR-COV-EXP-DATE          OF IUCIAAJR
             TO LIASJR-COV-EXP-DATE          OF SFAJR.
      *11.2   E_32688  BZXDJL1 END                                      00160004

      *13.5   E_39938  DJL BEGIN                                        00160004
           MOVE CIAAJR-SYS-CREATOR-CD        OF IUCIAAJR
             TO LIASJR-SYS-CREATOR-CD        OF SFAJR.
           MOVE CIAAJR-TRANSM-MODE-CD-OVR    OF IUCIAAJR
             TO LIASJR-TRANSM-MODE-CD-OVR    OF SFAJR.
      *13.5   E_39938  DJL END                                          00160004

      *R14.5   E_34072  DJL BEGIN
           MOVE CIAAJR-ADJ-RCPT-DT           OF IUCIAAJR
             TO LIASJR-ADJ-RCPT-DT           OF SFAJR.
      *R14.5   E_34072  DJL END
042992
      ******************************************************************
       C300-MOVE-ITCIACP-TO-SFCP.
      ******************************************************************

           MOVE CIACP-SCCF-SER-NUM       OF ITCIACP
                              TO CIACP-SCCF-SER-NUM       OF SFCP.
           MOVE CIACP-FDB-INV-CD         OF ITCIACP
                              TO CIACP-FDB-INV-CD         OF SFCP.
           MOVE CIACP-PRC-DATA-PRESENT-IND   OF ITCIACP
                              TO CIACP-PRC-DATA-PRESENT-IND
                                                          OF SFCP.

           IF CIACP-TOT-CHRG  OF ITCIACP NUMERIC
              MOVE CIACP-TOT-CHRG           OF ITCIACP
                              TO CIACP-TOT-CHRG           OF SFCP
           ELSE
              MOVE CIACP-TOT-CHRG-X         OF ITCIACP
                              TO CIACP-TOT-CHRG-X         OF SFCP.

           MOVE CIACP-DRG-CD             OF ITCIACP
                              TO CIACP-DRG-CD             OF SFCP.

           IF CIACP-CASE-ALLW-AMT OF ITCIACP NUMERIC
              MOVE CIACP-CASE-ALLW-AMT      OF ITCIACP
                              TO CIACP-CASE-ALLW-AMT      OF SFCP
           ELSE
              MOVE CIACP-CASE-ALLW-AMT-X    OF ITCIACP
                              TO CIACP-CASE-ALLW-AMT-X    OF SFCP.

           IF CIACP-AVG-SEMI-PRIV-RM-RATE OF ITCIACP NUMERIC
              MOVE CIACP-AVG-SEMI-PRIV-RM-RATE OF ITCIACP
                              TO CIACP-AVG-SEMI-PRIV-RM-RATE
                                                          OF SFCP
           ELSE
              MOVE CIACP-AVG-SEMI-PRIV-RM-RATE-X OF ITCIACP
                              TO CIACP-AVG-SEMI-PRIV-RM-RATE-X
                                                          OF SFCP.

           IF CIACP-PRIV-RM-RATE OF ITCIACP NUMERIC
              MOVE CIACP-PRIV-RM-RATE       OF ITCIACP
                              TO CIACP-PRIV-RM-RATE       OF SFCP
           ELSE
              MOVE CIACP-PRIV-RM-RATE-X     OF ITCIACP
                              TO CIACP-PRIV-RM-RATE-X     OF SFCP.

           IF CIACP-PER-DIEM-RATE         OF ITCIACP NUMERIC
              MOVE CIACP-PER-DIEM-RATE         OF ITCIACP
                              TO CIACP-PER-DIEM-RATE
                                                          OF SFCP
           ELSE
              MOVE CIACP-PER-DIEM-RATE-X         OF ITCIACP
                              TO CIACP-PER-DIEM-RATE-X
                                                          OF SFCP.

           IF CIACP-PCT-FCTR-CLM  OF ITCIACP NUMERIC
              MOVE CIACP-PCT-FCTR-CLM       OF ITCIACP
                              TO CIACP-PCT-FCTR-CLM       OF SFCP
           ELSE
              MOVE CIACP-PCT-FCTR-CLM-X     OF ITCIACP
                              TO CIACP-PCT-FCTR-CLM-X     OF SFCP.

           MOVE CIACP-PRC-MTD-CLM        OF ITCIACP
                              TO CIACP-PRC-MTD-CLM        OF SFCP.
           MOVE CIACP-RULE-NUM-PRIM-CLM  OF ITCIACP
                              TO CIACP-RULE-NUM-PRIM-CLM  OF SFCP.
           MOVE CIACP-CLM-TYPE           OF ITCIACP
                              TO CIACP-CLM-TYPE           OF SFCP.
           MOVE CIACP-TBL-ENTRY-CNT      OF ITCIACP
                              TO SFCP-TBL-ENTRY-CNT       OF SFCP.

           MOVE CIACP-RULE-NUM-SEC-CLM   OF ITCIACP
             TO CIACP-RULE-NUM-SEC-CLM   OF SFCP.
           MOVE CIACP-SF-MSG-CDS-CLM-AREA OF ITCIACP
             TO CIACP-SF-MSG-CDS-CLM-AREA OF SFCP.
           IF CIACP-AMT-WORK            OF ITCIACP NUMERIC
              MOVE CIACP-AMT-WORK            OF ITCIACP
                TO CIACP-AMT-WORK            OF SFCP.
      *OPL START
           MOVE CIACP-SEC-PAYOR-PRC-QUAL  OF ITCIACP
             TO CIACP-SEC-PAYOR-PRC-QUAL  OF SFCP.
           MOVE CIACP-HOST-OPL-PROV-ARRNG-CD    OF ITCIACP
             TO CIACP-HOST-OPL-PROV-ARRNG-CD    OF SFCP.
      *R15.0  E_24765  KJO ADDED MOVE OF DEL-MTD
051100     MOVE CIACP-DEL-MTD                   OF ITCIACP
051200       TO CIACP-DEL-MTD                   OF SFCP.
           MOVE CIACP-ITS-PAYER-IND      OF ITCIACP  (1)
             TO CIACP-ITS-PAYER-IND      OF SFCP     (1).
           MOVE CIACP-ITS-PAYER-IND      OF ITCIACP  (2)
             TO CIACP-ITS-PAYER-IND      OF SFCP     (2).
           MOVE CIACP-ITS-PAYER-IND      OF ITCIACP  (3)
             TO CIACP-ITS-PAYER-IND      OF SFCP     (3).
           MOVE CIACP-LINE-BUS                      OF ITCIACP
             TO CIACP-LINE-BUS                      OF SFCP.
           MOVE CIACP-OTH-CARR-IND                  OF ITCIACP
             TO CIACP-OTH-CARR-IND                  OF SFCP.
           MOVE CIACP-OPL-VAL-CDS-AMTS-CLM          OF ITCIACP
             TO CIACP-OPL-VAL-CDS-AMTS-CLM          OF SFCP.
      *OPL END

           MOVE CIACP-MED-ASGNMT-IND                OF ITCIACP
             TO CIACP-MED-ASGNMT-IND                OF SFCP.
           MOVE CIACP-MPPQ-CD-ASGN                  OF ITCIACP
             TO CIACP-MPPQ-CD-ASGN                  OF SFCP.
           MOVE CIACP-MPPQ-CD-UNASGN                OF ITCIACP
             TO CIACP-MPPQ-CD-UNASGN                OF SFCP.

           MOVE CIACP-REL-NUM                       OF ITCIACP
             TO CIACP-REL-NUM                       OF SFCP.

      *12.0   E_41233  DJL  BEGIN
           MOVE CIACP-SRC-PMT-CD       OF ITCIACP  (1)
             TO CIACP-SRC-PMT-CD       OF SFCP     (1).
           MOVE CIACP-SRC-PMT-CD       OF ITCIACP  (2)
             TO CIACP-SRC-PMT-CD       OF SFCP     (2).
           MOVE CIACP-SRC-PMT-CD       OF ITCIACP  (3)
             TO CIACP-SRC-PMT-CD       OF SFCP     (3).

           MOVE CIACP-NBR-INCL-GRPS                 OF ITCIACP
             TO CIACP-NBR-INCL-GRPS                 OF SFCP.
      *12.0   E_41233  DJL  END

      *R14.0  E_05931  RDW BEGIN
           MOVE CIACP-BYP-PRV-RM-DIFF-CALC-IND      OF ITCIACP
             TO CIACP-BYP-PRV-RM-DIFF-CALC-IND      OF SFCP.
      *R14.0  E_05931  RDW END
054300
      *14.0   E_84725  RDW  BEGIN
           MOVE CIACP-PROV-SANC-INCUR-DT OF ITCIACP
             TO CIACP-PROV-SANC-INCUR-DT OF SFCP.
           MOVE CIACP-CLASS-PROV-CLM OF ITCIACP
             TO CIACP-CLASS-PROV-CLM OF SFCP.
      *14.0   E_84725  RDW  END

      *R14.5  E_88250  RDW  BEGIN
           IF CIACP-LNG-STAY             OF ITCIACP  NUMERIC
051900        MOVE CIACP-LNG-STAY        OF ITCIACP
052000          TO CIACP-LNG-STAY        OF SFCP
           END-IF.
      *R14.5  E_88250  RDW  END

           PERFORM C350-MOVE-CIACP-LN-TO-SFCP
             VARYING LSUB FROM +1 BY +1
               UNTIL LSUB GREATER THAN SFCP-TBL-ENTRY-CNT OF SFCP.

      ******************************************************************
       C350-MOVE-CIACP-LN-TO-SFCP.
      ******************************************************************

           MOVE CIACP-ACCOM-ANC-SERV-CD   OF ITCIACP (LSUB)
                      TO CIACP-ACCOM-ANC-SERV-CD   OF SFCP (LSUB).

           IF CIACO-PART-OF-INCLUSIVE OF ITCIACO (LSUB)
              MOVE CIADR-CAP-IND             OF IUCIADR
                TO CIACP-CAP-IND             OF SFCP (LSUB)
           ELSE
056000     IF CIADR-TBL-ENTRY-CNT >= 0 OR <= 99999
           AND LSUB NOT GREATER CIADR-TBL-ENTRY-CNT
              MOVE CIADR-4X-CAP-IND          OF IUCIADR (LSUB)
                TO CIACP-CAP-IND             OF SFCP (LSUB)
           ELSE
              MOVE SPACE
                TO CIACP-CAP-IND             OF SFCP (LSUB).

           MOVE CIACP-SF-MSG-CDS-LN-AREA  OF ITCIACP (LSUB)
             TO CIACP-SF-MSG-CDS-LN-AREA  OF SFCP (LSUB).


      *OPL START
           MOVE SPACE TO CIACP-COB-LN-DATA        OF SFCP (LSUB).
      *OPL END

           IF CIACP-ACCOM-ANC-SERV-CD   OF SFCP (LSUB)
                             EQUAL INST-ACCOM-LN
              PERFORM C353-MOVE-INST-ACCOM-LN
           ELSE
           IF CIACP-ACCOM-ANC-SERV-CD   OF SFCP (LSUB)
                             EQUAL INST-ANC-LN
              PERFORM C355-MOVE-INST-ANC-LN
           ELSE
           IF CIACP-ACCOM-ANC-SERV-CD   OF SFCP (LSUB)
                             EQUAL PROFESSIONAL-LN
              PERFORM C357-MOVE-PROF-LN
           ELSE
              MOVE CIACP-LN                   OF ITCIACP (LSUB)
                     TO CIACP-LN                   OF SFCP (LSUB).

           MOVE CIACP-REV-TYPE-CD         OF ITCIACP (LSUB)
             TO CIACP-REV-TYPE-CD         OF SFCP (LSUB).

      *12.0   E_41233  DJL  BEGIN
           MOVE CIACP-INCL-GROUPING-NBR   OF ITCIACP (LSUB)
             TO CIACP-INCL-GROUPING-NBR   OF SFCP (LSUB).

           MOVE CIACP-INCL-GRP-PRIMARY-IND OF ITCIACP (LSUB)
             TO CIACP-INCL-GRP-PRIMARY-IND OF SFCP (LSUB).
      *12.0   E_41233  DJL  END

      *R14.0   E_64148  DJL  BEGIN
           IF CIACP-SUPP-THRSHLD-AMT   OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-SUPP-THRSHLD-AMT   OF ITCIACP (LSUB)
                        TO CIACP-SUPP-THRSHLD-AMT   OF SFCP (LSUB)
           ELSE
              MOVE CIACP-SUPP-THRSHLD-AMT-X   OF ITCIACP (LSUB)
                      TO CIACP-SUPP-THRSHLD-AMT-X   OF SFCP (LSUB).
      *R14.0   E_64148  DJL  END

      *R14.0   E_72114  KJO  BEGIN
           MOVE CIACP-CLASS-PROV-LN OF ITCIACP (LSUB)
             TO CIACP-CLASS-PROV-LN OF SFCP (LSUB).
      *R14.0   E_72114  KJO  END

      ******************************************************************
       C353-MOVE-INST-ACCOM-LN.
      ******************************************************************

           MOVE CIACP-REV-PROC-CD         OF ITCIACP (LSUB)
                      TO CIACP-REV-PROC-CD         OF SFCP (LSUB).

           IF CIACP-ACCOM-RATE OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-ACCOM-RATE OF ITCIACP (LSUB)
                              TO CIACP-ACCOM-RATE  OF SFCP (LSUB)
           ELSE
              MOVE CIACP-ACCOM-RATE-X OF ITCIACP (LSUB)
                              TO CIACP-ACCOM-RATE-X OF SFCP (LSUB).

           IF CIACP-NUM-SERV OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-NUM-SERV OF ITCIACP (LSUB)
                              TO CIACP-NUM-SERV    OF SFCP (LSUB)
           ELSE
              MOVE CIACP-NUM-SERV-X OF ITCIACP (LSUB)
                              TO CIACP-NUM-SERV-X OF SFCP (LSUB).

           IF CIACP-SERV-CHRG       OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-SERV-CHRG       OF ITCIACP (LSUB)
                          TO CIACP-SERV-CHRG        OF SFCP (LSUB)
           ELSE
              MOVE CIACP-SERV-CHRG-X       OF ITCIACP (LSUB)
                         TO CIACP-SERV-CHRG-X       OF SFCP (LSUB).

           IF CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB)
                     TO CIACP-LOC-RATE-SERV-ALLW-AMT OF SFCP (LSUB)
           ELSE
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT-X OF ITCIACP (LSUB)
                     TO CIACP-LOC-RATE-SERV-ALLW-AMT-X OF SFCP (LSUB).

           MOVE CIACP-PRC-MTD-LN           OF ITCIACP (LSUB)
                      TO CIACP-PRC-MTD-LN           OF SFCP (LSUB).

           IF CIACP-PCT-FCTR-LN        OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-PCT-FCTR-LN        OF ITCIACP (LSUB)
                        TO CIACP-PCT-FCTR-LN        OF SFCP (LSUB)
           ELSE
              MOVE CIACP-PCT-FCTR-LN-X        OF ITCIACP (LSUB)
                      TO CIACP-PCT-FCTR-LN-X        OF SFCP (LSUB).

           MOVE CIACP-RULE-NUM-LN          OF ITCIACP (LSUB)
                      TO CIACP-RULE-NUM-LN          OF SFCP (LSUB).

      ******************************************************************
       C355-MOVE-INST-ANC-LN.
      ******************************************************************

           MOVE CIACP-REV-PROC-CD         OF ITCIACP (LSUB)
                      TO CIACP-REV-PROC-CD         OF SFCP (LSUB).
           MOVE CIACP-DT-SERV             OF ITCIACP (LSUB)
                      TO CIACP-DT-SERV             OF SFCP (LSUB).

           IF CIACP-NUM-SERV OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-NUM-SERV OF ITCIACP (LSUB)
                        TO CIACP-NUM-SERV OF SFCP (LSUB)
           ELSE
              MOVE CIACP-NUM-SERV-X OF ITCIACP (LSUB)
                      TO CIACP-NUM-SERV-X OF SFCP (LSUB).

           IF CIACP-SERV-CHRG     OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-SERV-CHRG         OF ITCIACP (LSUB)
                        TO CIACP-SERV-CHRG         OF SFCP (LSUB)
           ELSE
              MOVE CIACP-SERV-CHRG-X         OF ITCIACP (LSUB)
                      TO CIACP-SERV-CHRG-X         OF SFCP (LSUB).

           IF CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB)
                        TO CIACP-LOC-RATE-SERV-ALLW-AMT OF SFCP (LSUB)
           ELSE
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT-X OF ITCIACP (LSUB)
                      TO CIACP-LOC-RATE-SERV-ALLW-AMT-X OF SFCP (LSUB).

           MOVE CIACP-PRC-MTD-LN           OF ITCIACP (LSUB)
                      TO CIACP-PRC-MTD-LN           OF SFCP (LSUB).

           IF CIACP-PCT-FCTR-LN      OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-PCT-FCTR-LN            OF ITCIACP (LSUB)
                        TO CIACP-PCT-FCTR-LN       OF SFCP (LSUB)
           ELSE
              MOVE CIACP-PCT-FCTR-LN-X          OF ITCIACP (LSUB)
                      TO CIACP-PCT-FCTR-LN-X       OF SFCP (LSUB).

           MOVE CIACP-RULE-NUM-LN          OF ITCIACP (LSUB)
                      TO CIACP-RULE-NUM-LN          OF SFCP (LSUB).

      ******************************************************************
       C357-MOVE-PROF-LN.
      ******************************************************************

           MOVE CIACP-REV-PROC-CD         OF ITCIACP (LSUB)
                      TO CIACP-REV-PROC-CD         OF SFCP (LSUB).
           MOVE CIACP-DT-SERV OF ITCIACP (LSUB)
                    TO CIACP-DT-SERV OF SFCP (LSUB).

           IF CIACP-SERV-CHRG OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-SERV-CHRG   OF ITCIACP (LSUB)
                        TO CIACP-SERV-CHRG         OF SFCP (LSUB)
           ELSE
              MOVE CIACP-SERV-CHRG-X OF ITCIACP (LSUB)
                      TO CIACP-SERV-CHRG-X         OF SFCP (LSUB).

           IF CIACP-NUM-SERV OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-NUM-SERV      OF ITCIACP (LSUB)
                        TO CIACP-NUM-SERV          OF SFCP (LSUB)
           ELSE
              MOVE CIACP-NUM-SERV-X OF ITCIACP (LSUB)
                      TO CIACP-NUM-SERV-X          OF SFCP (LSUB).

           IF CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT OF ITCIACP (LSUB)
                        TO CIACP-LOC-RATE-SERV-ALLW-AMT OF SFCP (LSUB)
           ELSE
              MOVE CIACP-LOC-RATE-SERV-ALLW-AMT-X OF ITCIACP (LSUB)
                      TO CIACP-LOC-RATE-SERV-ALLW-AMT-X OF SFCP (LSUB).

           MOVE CIACP-PRC-MTD-LN           OF ITCIACP (LSUB)
                      TO CIACP-PRC-MTD-LN           OF SFCP (LSUB).

           IF CIACP-PCT-FCTR-LN       OF ITCIACP (LSUB) NUMERIC
              MOVE CIACP-PCT-FCTR-LN            OF ITCIACP (LSUB)
                        TO CIACP-PCT-FCTR-LN       OF SFCP (LSUB)
           ELSE
              MOVE CIACP-PCT-FCTR-LN-X          OF ITCIACP (LSUB)
                      TO CIACP-PCT-FCTR-LN-X       OF SFCP (LSUB).

           MOVE CIACP-RULE-NUM-LN          OF ITCIACP (LSUB)
                      TO CIACP-RULE-NUM-LN          OF SFCP (LSUB).

072610*ITS-TM-2009-10-R10.4.2 TD_51390 BEGIN
072700     MOVE CIACP-TYPE-SERV            OF ITCIACP (LSUB)
072800                TO CIACP-TYPE-SERV            OF SFCP (LSUB).
072810*ITS-TM-2009-10-R10.4.2 TD_51390 END

      ******************************************************************
       C400-MOVE-ITCIACO-TO-SFCO.
      ******************************************************************

           MOVE CIACO-SCCF-SER-NUM       OF ITCIACO
                              TO CIACO-SCCF-SER-NUM       OF SFCO.
           MOVE CIACO-CLM-STAT-CD        OF ITCIACO
                              TO CIACO-CLM-STAT-CD        OF SFCO.
           MOVE CIACO-CLM-RSN-CD        OF ITCIACO (1)
                           TO CIACO-CLM-RSN-CD        OF SFCO (1).
           MOVE CIACO-CLM-RSN-CD        OF ITCIACO (2)
                           TO CIACO-CLM-RSN-CD        OF SFCO (2).
           MOVE CIACO-CLM-RSN-CD        OF ITCIACO (3)
                           TO CIACO-CLM-RSN-CD        OF SFCO (3).
           MOVE CIACO-CLM-RSN-CD        OF ITCIACO (4)
                           TO CIACO-CLM-RSN-CD        OF SFCO (4).
           MOVE CIACO-CLM-RSN-CD        OF ITCIACO (5)
                           TO CIACO-CLM-RSN-CD        OF SFCO (5).
           MOVE CIACO-CLM-ERR-MAX-IND        OF ITCIACO
                              TO CIACO-CLM-ERR-MAX-IND    OF SFCO.
           MOVE CIACO-CALC-TYPE-CD           OF ITCIACO
                              TO CIACO-CALC-TYPE-CD       OF SFCO.

           IF CIACO-DED-REM-1  OF ITCIACO NUMERIC
              MOVE CIACO-DED-REM-1          OF ITCIACO
                TO CIACO-DED-REM-1          OF SFCO
           ELSE
              MOVE CIACO-DED-REM-1-X        OF ITCIACO
                TO CIACO-DED-REM-1-X        OF SFCO.

           IF CIACO-DED-REM-2  OF ITCIACO NUMERIC
              MOVE CIACO-DED-REM-2          OF ITCIACO
                TO CIACO-DED-REM-2          OF SFCO
           ELSE
              MOVE CIACO-DED-REM-2-X        OF ITCIACO
                TO CIACO-DED-REM-2-X        OF SFCO.

           IF CIACO-DED-REM-3  OF ITCIACO NUMERIC
              MOVE CIACO-DED-REM-3          OF ITCIACO
                TO CIACO-DED-REM-3          OF SFCO
           ELSE
              MOVE CIACO-DED-REM-3-X        OF ITCIACO
                TO CIACO-DED-REM-3-X        OF SFCO.

           IF CIACO-REM-OOP-MAX           OF ITCIACO NUMERIC
              MOVE CIACO-REM-OOP-MAX                   OF ITCIACO
                          TO CIACO-REM-OOP-MAX            OF SFCO
           ELSE
              MOVE CIACO-REM-OOP-MAX-X                 OF ITCIACO
                         TO CIACO-REM-OOP-MAX-X           OF SFCO.
      *PAYMAX BELOW ADDED
           IF CIACO-PAY-MAX-REM-CLM-1     OF ITCIACO NUMERIC
              MOVE CIACO-PAY-MAX-REM-CLM-1             OF ITCIACO
                          TO CIACO-PAY-MAX-REM-CLM-1      OF SFCO
           ELSE
              MOVE CIACO-PAY-MAX-REM-CLM-1-X           OF ITCIACO
                         TO CIACO-PAY-MAX-REM-CLM-1-X     OF SFCO.

           IF CIACO-PAY-MAX-REM-CLM-2     OF ITCIACO NUMERIC
              MOVE CIACO-PAY-MAX-REM-CLM-2             OF ITCIACO
                          TO CIACO-PAY-MAX-REM-CLM-2      OF SFCO
           ELSE
              MOVE CIACO-PAY-MAX-REM-CLM-2-X           OF ITCIACO
                         TO CIACO-PAY-MAX-REM-CLM-2-X     OF SFCO.

           IF CIACO-PAY-MAX-REM-CLM-3     OF ITCIACO NUMERIC
              MOVE CIACO-PAY-MAX-REM-CLM-3             OF ITCIACO
                          TO CIACO-PAY-MAX-REM-CLM-3      OF SFCO
           ELSE
              MOVE CIACO-PAY-MAX-REM-CLM-3-X           OF ITCIACO
                         TO CIACO-PAY-MAX-REM-CLM-3-X     OF SFCO.
080300*PAYMAX ABOVE ADDED

           IF CIACO-BCBS-PMT-AMT          OF ITCIACO NUMERIC
              MOVE CIACO-BCBS-PMT-AMT                  OF ITCIACO
                          TO CIACO-BCBS-PMT-AMT           OF SFCO
           ELSE
              MOVE CIACO-BCBS-PMT-AMT-X                OF ITCIACO
                         TO CIACO-BCBS-PMT-AMT-X          OF SFCO.

           IF CIACO-SUB-LIAB              OF ITCIACO NUMERIC
              MOVE CIACO-SUB-LIAB                      OF ITCIACO
                          TO CIACO-SUB-LIAB               OF SFCO
           ELSE
              MOVE CIACO-SUB-LIAB-X                    OF ITCIACO
                         TO CIACO-SUB-LIAB-X              OF SFCO.

           MOVE CIACO-TBL-ENTRY-CNT                    OF ITCIACO
                         TO SFCO-TBL-ENTRY-CNT            OF SFCO.

           PERFORM C450-MOVE-CIACO-LN-TO-SFCO
             VARYING LSUB FROM +1 BY +1
               UNTIL LSUB GREATER THAN SFCO-TBL-ENTRY-CNT OF SFCO.

      ******************************************************************
       C450-MOVE-CIACO-LN-TO-SFCO.
      ******************************************************************

           MOVE CIACO-LN-STAT-CD          OF ITCIACO (LSUB)
                      TO CIACO-LN-STAT-CD          OF SFCO (LSUB).
           MOVE CIACO-LN-RSN-CD           OF ITCIACO (LSUB, 1)
                      TO CIACO-LN-RSN-CD           OF SFCO (LSUB, 1).
           MOVE CIACO-LN-RSN-CD           OF ITCIACO (LSUB, 2)
                      TO CIACO-LN-RSN-CD           OF SFCO (LSUB, 2).
           MOVE CIACO-LN-RSN-CD           OF ITCIACO (LSUB, 3)
                      TO CIACO-LN-RSN-CD           OF SFCO (LSUB, 3).
           MOVE CIACO-LN-ERR-MAX-IND      OF ITCIACO (LSUB)
                      TO CIACO-LN-ERR-MAX-IND      OF SFCO (LSUB).

           IF CIACO-INIT-CHRGS          OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-INIT-CHRGS             OF ITCIACO (LSUB)
                         TO CIACO-INIT-CHRGS       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-INIT-CHRGS-X           OF ITCIACO (LSUB)
                         TO CIACO-INIT-CHRGS-X     OF SFCO (LSUB).

           IF CIACO-INIT-PMT-AMT        OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-INIT-PMT-AMT           OF ITCIACO (LSUB)
                         TO CIACO-INIT-PMT-AMT     OF SFCO (LSUB)
              IF CIACO-INIT-PMT-AMT OF SFCO (LSUB) NEGATIVE
                 MOVE ZERO
                   TO CIACO-INIT-PMT-AMT     OF SFCO (LSUB)
              END-IF
           ELSE
              MOVE CIACO-INIT-PMT-AMT-X         OF ITCIACO (LSUB)
                         TO CIACO-INIT-PMT-AMT-X   OF SFCO (LSUB).

           IF CIACO-COV-SERV-CHRGS-EXC-CS OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-COV-SERV-CHRGS-EXC-CS  OF ITCIACO (LSUB)
                    TO CIACO-COV-SERV-CHRGS-EXC-CS OF SFCO (LSUB)
           ELSE
              MOVE CIACO-COV-SERV-CHRGS-EXC-CS-X  OF ITCIACO (LSUB)
                    TO CIACO-COV-SERV-CHRGS-EXC-CS-X OF SFCO (LSUB).

           IF CIACO-REM-COV-CHRGS       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-REM-COV-CHRGS          OF ITCIACO (LSUB)
                         TO CIACO-REM-COV-CHRGS    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-REM-COV-CHRGS-X        OF ITCIACO (LSUB)
                       TO CIACO-REM-COV-CHRGS-X    OF SFCO (LSUB).

           IF CIACO-REM-PMT-ALLWD-AMT    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-REM-PMT-ALLWD-AMT      OF ITCIACO (LSUB)
                     TO CIACO-REM-PMT-ALLWD-AMT    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-REM-PMT-ALLWD-AMT-X    OF ITCIACO (LSUB)
                   TO CIACO-REM-PMT-ALLWD-AMT-X    OF SFCO (LSUB).

           IF CIACO-ACCUM-SUB-LIAB       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-ACCUM-SUB-LIAB         OF ITCIACO (LSUB)
                     TO CIACO-ACCUM-SUB-LIAB       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-ACCUM-SUB-LIAB-X       OF ITCIACO (LSUB)
                   TO CIACO-ACCUM-SUB-LIAB-X       OF SFCO (LSUB).

      *R14.0   E_64148  DJL BEGIN GROUP ACCUM FIELDS TOGETHER
           IF CIACO-INIT-ALLWD-AMT       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-INIT-ALLWD-AMT         OF ITCIACO (LSUB)
                     TO CIACO-INIT-ALLWD-AMT       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-INIT-ALLWD-AMT-X       OF ITCIACO (LSUB)
                   TO CIACO-INIT-ALLWD-AMT-X       OF SFCO (LSUB).

           IF CIACO-REM-ALLWD-AMT        OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-REM-ALLWD-AMT          OF ITCIACO (LSUB)
                     TO CIACO-REM-ALLWD-AMT        OF SFCO (LSUB)
           ELSE
              MOVE CIACO-REM-ALLWD-AMT-X        OF ITCIACO (LSUB)
                   TO CIACO-REM-ALLWD-AMT-X        OF SFCO (LSUB).

           IF CIACO-ACCUM-EXCL-SUB-REDS  OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-ACCUM-EXCL-SUB-REDS    OF ITCIACO (LSUB)
                     TO CIACO-ACCUM-EXCL-SUB-REDS  OF SFCO (LSUB)
           ELSE
              MOVE CIACO-ACCUM-EXCL-SUB-REDS-X  OF ITCIACO (LSUB)
                   TO CIACO-ACCUM-EXCL-SUB-REDS-X  OF SFCO (LSUB).
      *R14.0   E_64148  DJL END   GROUP ACCUM FIELDS TOGETHER
088600
           IF CIACO-CFAS-NON-COV-SERV    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-NON-COV-SERV      OF ITCIACO (LSUB)
                     TO CIACO-CFAS-NON-COV-SERV    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-NON-COV-SERV-X    OF ITCIACO (LSUB)
                   TO CIACO-CFAS-NON-COV-SERV-X    OF SFCO (LSUB).

           IF CIACO-CFAS-NON-COV-D-V-U   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-NON-COV-D-V-U     OF ITCIACO (LSUB)
                     TO CIACO-CFAS-NON-COV-D-V-U   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-NON-COV-D-V-U-X   OF ITCIACO (LSUB)
                   TO CIACO-CFAS-NON-COV-D-V-U-X   OF SFCO (LSUB).

           IF CIACO-CFAS-MAX-DOL         OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-MAX-DOL           OF ITCIACO (LSUB)
                     TO CIACO-CFAS-MAX-DOL         OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-MAX-DOL-X         OF ITCIACO (LSUB)
                   TO CIACO-CFAS-MAX-DOL-X         OF SFCO (LSUB).

           IF CIACO-CFAS-PRIV-RM-DIFF    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-PRIV-RM-DIFF      OF ITCIACO (LSUB)
                     TO CIACO-CFAS-PRIV-RM-DIFF    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-PRIV-RM-DIFF-X    OF ITCIACO (LSUB)
                   TO CIACO-CFAS-PRIV-RM-DIFF-X    OF SFCO (LSUB).

           IF CIACO-CFAS-DED-AMT         OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-DED-AMT           OF ITCIACO (LSUB)
                     TO CIACO-CFAS-DED-AMT         OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-DED-AMT-X         OF ITCIACO (LSUB)
                   TO CIACO-CFAS-DED-AMT-X         OF SFCO (LSUB).

           IF CIACO-CFAS-CO-PAY-AMT1     OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-CO-PAY-AMT1       OF ITCIACO (LSUB)
                     TO CIACO-CFAS-CO-PAY-AMT1     OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-CO-PAY-AMT1-X     OF ITCIACO (LSUB)
                   TO CIACO-CFAS-CO-PAY-AMT1-X     OF SFCO (LSUB).

           IF CIACO-CFAS-CO-PAY-AMT2     OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-CO-PAY-AMT2       OF ITCIACO (LSUB)
                     TO CIACO-CFAS-CO-PAY-AMT2     OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-CO-PAY-AMT2-X     OF ITCIACO (LSUB)
                   TO CIACO-CFAS-CO-PAY-AMT2-X     OF SFCO (LSUB).

           IF CIACO-CFAS-CO-INS-AMT      OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAS-CO-INS-AMT        OF ITCIACO (LSUB)
                     TO CIACO-CFAS-CO-INS-AMT      OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAS-CO-INS-AMT-X      OF ITCIACO (LSUB)
                   TO CIACO-CFAS-CO-INS-AMT-X      OF SFCO (LSUB).

           IF CIACO-CFAB-NON-COV-SERV    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-NON-COV-SERV      OF ITCIACO (LSUB)
                     TO CIACO-CFAB-NON-COV-SERV    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-NON-COV-SERV-X    OF ITCIACO (LSUB)
                   TO CIACO-CFAB-NON-COV-SERV-X    OF SFCO (LSUB).

           IF CIACO-CFAB-NON-COV-D-V-U   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-NON-COV-D-V-U     OF ITCIACO (LSUB)
                     TO CIACO-CFAB-NON-COV-D-V-U   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-NON-COV-D-V-U-X   OF ITCIACO (LSUB)
                   TO CIACO-CFAB-NON-COV-D-V-U-X   OF SFCO (LSUB).

           IF CIACO-CFAB-MAX-DOL         OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-MAX-DOL           OF ITCIACO (LSUB)
                     TO CIACO-CFAB-MAX-DOL         OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-MAX-DOL-X         OF ITCIACO (LSUB)
                   TO CIACO-CFAB-MAX-DOL-X         OF SFCO (LSUB).

           IF CIACO-CFAB-PRIV-RM-DIFF    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-PRIV-RM-DIFF      OF ITCIACO (LSUB)
                     TO CIACO-CFAB-PRIV-RM-DIFF    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-PRIV-RM-DIFF-X    OF ITCIACO (LSUB)
                   TO CIACO-CFAB-PRIV-RM-DIFF-X    OF SFCO (LSUB).

           IF CIACO-CFAB-DED-AMT         OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-DED-AMT           OF ITCIACO (LSUB)
                     TO CIACO-CFAB-DED-AMT         OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-DED-AMT-X         OF ITCIACO (LSUB)
                   TO CIACO-CFAB-DED-AMT-X         OF SFCO (LSUB).

           IF CIACO-CFAB-CO-PAY-AMT1     OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-CO-PAY-AMT1       OF ITCIACO (LSUB)
                     TO CIACO-CFAB-CO-PAY-AMT1     OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-CO-PAY-AMT1-X     OF ITCIACO (LSUB)
                   TO CIACO-CFAB-CO-PAY-AMT1-X     OF SFCO (LSUB).

           IF CIACO-CFAB-CO-PAY-AMT2     OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-CO-PAY-AMT2       OF ITCIACO (LSUB)
                     TO CIACO-CFAB-CO-PAY-AMT2     OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-CO-PAY-AMT2-X     OF ITCIACO (LSUB)
                   TO CIACO-CFAB-CO-PAY-AMT2-X     OF SFCO (LSUB).

           IF CIACO-CFAB-CO-INS-AMT      OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-CO-INS-AMT        OF ITCIACO (LSUB)
                     TO CIACO-CFAB-CO-INS-AMT      OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-CO-INS-AMT-X      OF ITCIACO (LSUB)
                   TO CIACO-CFAB-CO-INS-AMT-X      OF SFCO (LSUB).

           IF CIACO-CFAB-PRC-PCT-AMT     OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-CFAB-PRC-PCT-AMT       OF ITCIACO (LSUB)
                     TO CIACO-CFAB-PRC-PCT-AMT     OF SFCO (LSUB)
           ELSE
              MOVE CIACO-CFAB-PRC-PCT-AMT-X     OF ITCIACO (LSUB)
                   TO CIACO-CFAB-PRC-PCT-AMT-X     OF SFCO (LSUB).

101210*TM-2009-06-R10.4 BEG E_34835
101300     IF CIACO-DMEB-AMT             OF ITCIACO (LSUB) NUMERIC
101400        MOVE CIACO-DMEB-AMT               OF ITCIACO (LSUB)
101500               TO CIACO-DMEB-AMT             OF SFCO (LSUB)
           ELSE
101700        MOVE CIACO-DMEB-AMT-X             OF ITCIACO (LSUB)
101800             TO CIACO-DMEB-AMT-X             OF SFCO (LSUB).

102000     IF CIACO-DMES-AMT             OF ITCIACO (LSUB) NUMERIC
102100        MOVE CIACO-DMES-AMT               OF ITCIACO (LSUB)
102200               TO CIACO-DMES-AMT             OF SFCO (LSUB)
           ELSE
102400        MOVE CIACO-DMES-AMT-X             OF ITCIACO (LSUB)
102500             TO CIACO-DMES-AMT-X             OF SFCO (LSUB).
103410*TM-2009-06-R10.4 END E_34835

      *14.0   E_84725  RDW  BEGIN
           IF CIACO-BMFAP-SANC-DOL-RED   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFAP-SANC-DOL-RED     OF ITCIACO (LSUB)
                     TO CIACO-BMFAP-SANC-DOL-RED   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFAP-SANC-DOL-RED-X   OF ITCIACO (LSUB)
                   TO CIACO-BMFAP-SANC-DOL-RED-X   OF SFCO (LSUB).

           IF CIACO-BMFAP-SANC-PCT-RED   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFAP-SANC-PCT-RED     OF ITCIACO (LSUB)
                     TO CIACO-BMFAP-SANC-PCT-RED   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFAP-SANC-PCT-RED-X   OF ITCIACO (LSUB)
                   TO CIACO-BMFAP-SANC-PCT-RED-X   OF SFCO (LSUB).

           IF CIACO-BMFAP-EXCESS-DOL-RED   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFAP-EXCESS-DOL-RED   OF ITCIACO (LSUB)
                     TO CIACO-BMFAP-EXCESS-DOL-RED   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFAP-EXCESS-DOL-RED-X OF ITCIACO (LSUB)
                   TO CIACO-BMFAP-EXCESS-DOL-RED-X   OF SFCO (LSUB).

           IF CIACO-BMFAP-EXCESS-PER-DAY-RED OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFAP-EXCESS-PER-DAY-RED OF ITCIACO (LSUB)
                     TO CIACO-BMFAP-EXCESS-PER-DAY-RED OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFAP-EXC-PERDAY-RED-X  OF ITCIACO (LSUB)
                   TO CIACO-BMFAP-EXC-PERDAY-RED-X OF SFCO (LSUB).

           IF CIACO-BMFAS-PROV-SANC-RED OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFAS-PROV-SANC-RED OF ITCIACO (LSUB)
                     TO CIACO-BMFAS-PROV-SANC-RED OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFAS-PROV-SANC-RED-X OF ITCIACO (LSUB)
                   TO CIACO-BMFAS-PROV-SANC-RED-X OF SFCO (LSUB).

      *14.0   E_84725  RDW  END
104800
           IF CIACO-BMFASS-NON-COV-SERV  OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASS-NON-COV-SERV    OF ITCIACO (LSUB)
                     TO CIACO-BMFASS-NON-COV-SERV  OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASS-NON-COV-SERV-X  OF ITCIACO (LSUB)
                   TO CIACO-BMFASS-NON-COV-SERV-X  OF SFCO (LSUB).

           IF CIACO-BMFASS-NON-COV-D-V-U OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASS-NON-COV-D-V-U   OF ITCIACO (LSUB)
                     TO CIACO-BMFASS-NON-COV-D-V-U OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASS-NON-COV-D-V-U-X OF ITCIACO (LSUB)
                   TO CIACO-BMFASS-NON-COV-D-V-U-X OF SFCO (LSUB).

           IF CIACO-BMFASS-MAX-DOL       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASS-MAX-DOL         OF ITCIACO (LSUB)
                     TO CIACO-BMFASS-MAX-DOL       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASS-MAX-DOL-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASS-MAX-DOL-X       OF SFCO (LSUB).

           IF CIACO-BMFASS-DOL-RED       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASS-DOL-RED         OF ITCIACO (LSUB)
                     TO CIACO-BMFASS-DOL-RED       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASS-DOL-RED-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASS-DOL-RED-X       OF SFCO (LSUB).

           IF CIACO-BMFASS-PCT-RED       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASS-PCT-RED         OF ITCIACO (LSUB)
                     TO CIACO-BMFASS-PCT-RED       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASS-PCT-RED-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASS-PCT-RED-X       OF SFCO (LSUB).

           IF CIACO-BMFASB-NON-COV-SERV  OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASB-NON-COV-SERV    OF ITCIACO (LSUB)
                     TO CIACO-BMFASB-NON-COV-SERV  OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASB-NON-COV-SERV-X  OF ITCIACO (LSUB)
                   TO CIACO-BMFASB-NON-COV-SERV-X  OF SFCO (LSUB).

           IF CIACO-BMFASB-NON-COV-D-V-U OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASB-NON-COV-D-V-U   OF ITCIACO (LSUB)
                     TO CIACO-BMFASB-NON-COV-D-V-U OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASB-NON-COV-D-V-U-X OF ITCIACO (LSUB)
                   TO CIACO-BMFASB-NON-COV-D-V-U-X OF SFCO (LSUB).

           IF CIACO-BMFASB-MAX-DOL       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASB-MAX-DOL         OF ITCIACO (LSUB)
                     TO CIACO-BMFASB-MAX-DOL       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASB-MAX-DOL-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASB-MAX-DOL-X       OF SFCO (LSUB).

           IF CIACO-BMFASB-DOL-RED       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASB-DOL-RED         OF ITCIACO (LSUB)
                     TO CIACO-BMFASB-DOL-RED       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASB-DOL-RED-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASB-DOL-RED-X       OF SFCO (LSUB).

           IF CIACO-BMFASB-PCT-RED       OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-BMFASB-PCT-RED         OF ITCIACO (LSUB)
                     TO CIACO-BMFASB-PCT-RED       OF SFCO (LSUB)
           ELSE
              MOVE CIACO-BMFASB-PCT-RED-X       OF ITCIACO (LSUB)
                   TO CIACO-BMFASB-PCT-RED-X       OF SFCO (LSUB).

      *PT BELOW ADDED SUP/SUR
           IF CIACO-SSAB-MAX-SUPP-AMT    OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-SSAB-MAX-SUPP-AMT      OF ITCIACO (LSUB)
                     TO CIACO-SSAB-MAX-SUPP-AMT    OF SFCO (LSUB)
           ELSE
              MOVE CIACO-SSAB-MAX-SUPP-AMT-X    OF ITCIACO (LSUB)
                   TO CIACO-SSAB-MAX-SUPP-AMT-X    OF SFCO (LSUB).

           IF CIACO-SSAB-SURCHARGE-AMT   OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-SSAB-SURCHARGE-AMT     OF ITCIACO (LSUB)
                     TO CIACO-SSAB-SURCHARGE-AMT   OF SFCO (LSUB)
           ELSE
              MOVE CIACO-SSAB-SURCHARGE-AMT-X   OF ITCIACO (LSUB)
                   TO CIACO-SSAB-SURCHARGE-AMT-X   OF SFCO (LSUB).
114100*PT ABOVE ADDED SUP/SUR

      *PAYMAX BELOW ADDED
           IF CIACO-PMS-PAY-MAX-AMT      OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-PMS-PAY-MAX-AMT        OF ITCIACO (LSUB)
                     TO CIACO-PMS-PAY-MAX-AMT      OF SFCO (LSUB)
           ELSE
              MOVE CIACO-PMS-PAY-MAX-AMT-X      OF ITCIACO (LSUB)
                   TO CIACO-PMS-PAY-MAX-AMT-X      OF SFCO (LSUB).

           IF CIACO-PMB-PAY-MAX-AMT      OF ITCIACO (LSUB) NUMERIC
              MOVE CIACO-PMB-PAY-MAX-AMT        OF ITCIACO (LSUB)
                     TO CIACO-PMB-PAY-MAX-AMT      OF SFCO (LSUB)
           ELSE
              MOVE CIACO-PMB-PAY-MAX-AMT-X      OF ITCIACO (LSUB)
                   TO CIACO-PMB-PAY-MAX-AMT-X      OF SFCO (LSUB).
      *PAYMAX ABOVE ADDED

      ******************************************************************
       C500-MOVE-IULIALM-TO-ADJLM.
      ******************************************************************

           MOVE ZEROES                          TO ASUB
                                                   BSUB
                                                   LSUB
                                                   SUB.

           MOVE LIALM-ADJUD-LN-NUM   OF IULIALM TO ASUB.
           MOVE LIALM-BEN-LEV OF IULIALM        TO BSUB.
           MOVE LIALM-SCCF-LN-NUM   OF IULIALM  TO LSUB.

           IF LIALM-ADJUD-LN-NUM   OF IULIALM
                    GREATER THAN LIAALM-END-OF-LN
              MOVE LIALM-ADJUD-LN-NUM   OF IULIALM
                              TO LIAALM-END-OF-LN.

           IF LIALM-BEN-LEV OF IULIALM
                    GREATER THAN LIAALM-END-OF-BENS (ASUB)
              MOVE LIALM-BEN-LEV OF IULIALM
                              TO LIAALM-END-OF-BENS (ASUB).

           MOVE LIALM-SCCF-LN-NUM     OF IULIALM
                TO LIAALM-SCCF-LN-NUM   (ASUB, BSUB).
           MOVE LIALM-SPLIT-LN-CD     OF IULIALM
                TO LIAALM-SPLIT-LN-CD   (ASUB, BSUB).
           MOVE LIALM-SPLIT-FRM-ADJUD-LN-NUM   OF IULIALM
                TO LIAALM-SPLIT-FRM-ADJUD-LN-NU   (ASUB, BSUB).
           MOVE LIALM-LN-CHRG-INIT       OF IULIALM
                TO LIAALM-LN-CHRG-INIT       (ASUB, BSUB).
           MOVE LIALM-BEN-LEV-CHRG       OF IULIALM
                TO LIAALM-BEN-LEV-CHRG  (ASUB, BSUB).
           IF LIALM-SPLIT-CLM-SET-NUM  OF IULIALM NUMERIC
              MOVE LIALM-SPLIT-CLM-SET-NUM  OF IULIALM
                TO LIAALM-SPLIT-CLM-SET-NUM (ASUB, BSUB)
           ELSE
              MOVE ZERO
                TO LIAALM-SPLIT-CLM-SET-NUM (ASUB, BSUB).

      ******************************************************************
       C600-MOVE-IUCIAAJR-TO-ADJAJR.
      ******************************************************************

           IF LIALM-ADJUD-LN-NUM   OF IULIALM
                    GREATER THAN LIAAJR-END-OF-LN
              MOVE LIALM-ADJUD-LN-NUM   OF IULIALM
                              TO LIAAJR-END-OF-LN.

           MOVE CIAAJR-ADJUD-DT       OF IUCIAAJR
                TO LIAAJR-ADJUD-DT (ASUB, BSUB).
           MOVE CIAAJR-CLM-REASON-CDS OF IUCIAAJR
                TO LIAAJR-CLM-REASON-CDS (ASUB, BSUB).
           MOVE CIAAJR-DF-MSG-CDS-CLM OF IUCIAAJR
                TO LIAAJR-DF-MSG-CDS-CLM (ASUB, BSUB).
           MOVE CIAAJR-DF-MSG-CD-LN OF IUCIAAJR (1)
                TO LIAAJR-DF-MSG-CD-LN (ASUB, BSUB).
           MOVE CIAAJR-BMSP-ADJ-RSN-CD OF IUCIAAJR (1)
                TO LIAAJR-BMSP-ADJ-RSN-CD (ASUB, BSUB).
           MOVE CIAAJR-ORIG-OPERATOR-ID OF IUCIAAJR
                TO LIAAJR-ORIG-OPERATOR-ID (ASUB, BSUB).
           MOVE CIAAJR-ORIG-PROCESS-DT     OF IUCIAAJR
                TO LIAAJR-ORIG-PROCESS-DT     (ASUB, BSUB).
           MOVE CIAAJR-INVEST-IND        OF IUCIAAJR
             TO LIAAJR-INVEST-IND       (ASUB, BSUB).
           IF CIAAJR-INVEST-DAY        OF IUCIAAJR NUMERIC
              MOVE CIAAJR-INVEST-DAY     OF IUCIAAJR
                TO LIAAJR-INVEST-DAY       (ASUB, BSUB)
           ELSE
              MOVE CIAAJR-INVEST-DAY-X   OF IUCIAAJR
                TO LIAAJR-INVEST-DAY-X     (ASUB, BSUB).
           MOVE CIAAJR-CARVE-OUT-IND     OF IUCIAAJR
             TO LIAAJR-CARVE-OUT-IND    (ASUB, BSUB).
           IF CIAAJR-CARVE-OUT-DAY     OF IUCIAAJR NUMERIC
              MOVE CIAAJR-CARVE-OUT-DAY  OF IUCIAAJR
                TO LIAAJR-CARVE-OUT-DAY    (ASUB, BSUB)
           ELSE
              MOVE CIAAJR-CARVE-OUT-DAY-X OF IUCIAAJR
                TO LIAAJR-CARVE-OUT-DAY-X  (ASUB, BSUB).
           MOVE CIAAJR-ASO-IND           OF IUCIAAJR
             TO LIAAJR-ASO-IND          (ASUB, BSUB).
      ***OPL START
           MOVE CIAAJR-PRIM-IND        OF IUCIAAJR
             TO LIAAJR-PRIM-IND       (ASUB, BSUB).
           MOVE CIAAJR-INDEM-CUTBK-IND  OF IUCIAAJR
             TO LIAAJR-INDEM-CUTBK-IND  (ASUB, BSUB).
      ***OPL END
125500
      *11.2   E_32688  BZXKJO1 BEGIN                                    00160004
           IF CIAAJR-COV-EXP-DATE > SPACES
              IF LIAAJR-COV-EXP-DATE = SPACES
                 MOVE CIAAJR-COV-EXP-DATE
                   TO LIAAJR-COV-EXP-DATE
              ELSE
                 IF CIAAJR-COV-EXP-DATE > LIAAJR-COV-EXP-DATE
                    MOVE CIAAJR-COV-EXP-DATE
                      TO LIAAJR-COV-EXP-DATE
                 END-IF
              END-IF
           END-IF.
      *11.2   E_32688  BZXKJO1 END                                      00160004

      *11.2   E_32688  BZXDJL1 BEGIN                                    00160004
      *11.2   E_32688  BZXKJO1 - ONLY MOVE ON 1ST BEN LEVEL             00160004
           IF LIALM-BEN-LEV OF IULIALM = 1
              MOVE CIAAJR-HEALTH-POLICY-ID OF IUCIAAJR (1, 1)
                TO LIAAJR-HEALTH-POLICY-ID (ASUB, 1)
              MOVE CIAAJR-HEALTH-POLICY-ID OF IUCIAAJR (1, 2)
                TO LIAAJR-HEALTH-POLICY-ID (ASUB, 2)
              MOVE CIAAJR-HEALTH-POLICY-ID OF IUCIAAJR (1, 3)
                TO LIAAJR-HEALTH-POLICY-ID (ASUB, 3)
              MOVE CIAAJR-HEALTH-POLICY-ID OF IUCIAAJR (1, 4)
                TO LIAAJR-HEALTH-POLICY-ID (ASUB, 4)
              MOVE CIAAJR-HEALTH-POLICY-ID OF IUCIAAJR (1, 5)
                TO LIAAJR-HEALTH-POLICY-ID (ASUB, 5)
           END-IF.
      *11.2   E_32688  BZXDJL1 END                                      00160004
125500
      ******************************************************************
       C700-MOVE-ITCIACO-TO-ADJCO.
      ******************************************************************

           IF LIALM-ADJUD-LN-NUM   OF IULIALM
                    GREATER THAN LIACO-END-OF-LN
              MOVE LIALM-ADJUD-LN-NUM   OF IULIALM
                              TO LIACO-END-OF-LN.

           MOVE CIACO-CLM-STAT-CD       OF ITCIACO
                TO LIACO-CLM-STAT-CD       (ASUB, BSUB).
           MOVE CIACO-CALC-TYPE-CD    OF ITCIACO
                TO LIACO-CALC-TYPE-CD   (ASUB, BSUB).

           IF CIACO-BCBS-PMT-AMT OF ITCIACO NUMERIC
              MOVE CIACO-BCBS-PMT-AMT    OF ITCIACO
                TO LIACO-BCBS-PMT-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-BCBS-PMT-AMT-X  OF ITCIACO
                TO LIACO-BCBS-PMT-AMT-X (ASUB, BSUB).

           IF CIACO-SUB-LIAB OF ITCIACO NUMERIC
              MOVE CIACO-SUB-LIAB        OF ITCIACO
                TO LIACO-SUB-LIAB (ASUB, BSUB)
           ELSE
              MOVE CIACO-SUB-LIAB-X      OF ITCIACO
                TO LIACO-SUB-LIAB-X (ASUB, BSUB).

           IF CIACO-CALC-TYPE-CD   OF ITCIACO
                    EQUAL INCLUSIVE-CALC OR INCLUSIVE-LN-CALC
              MOVE WS-PLUS-TWO  TO SUB
           ELSE
              MOVE WS-PLUS-ONE  TO SUB.

           MOVE CIACO-LN-STAT-CD       OF ITCIACO (SUB)
                TO LIACO-LN-STAT-CD       (ASUB, BSUB).

           IF CIACO-INIT-CHRGS OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-INIT-CHRGS OF ITCIACO (SUB)
                TO LIACO-INIT-CHRGS (ASUB, BSUB)
           ELSE
              MOVE CIACO-INIT-CHRGS-X OF ITCIACO (SUB)
                TO LIACO-INIT-CHRGS-X (ASUB, BSUB).

           IF CIACO-INIT-PMT-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-INIT-PMT-AMT OF ITCIACO (SUB)
                TO LIACO-INIT-PMT-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-INIT-PMT-AMT-X OF ITCIACO (SUB)
                TO LIACO-INIT-PMT-AMT-X (ASUB, BSUB).

           IF CIACO-COV-SERV-CHRGS-EXC-CS OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-COV-SERV-CHRGS-EXC-CS OF ITCIACO (SUB)
                TO LIACO-COV-SERV-CHRGS-EXC-CS (ASUB, BSUB)
           ELSE
              MOVE CIACO-COV-SERV-CHRGS-EXC-CS-X OF ITCIACO (SUB)
                TO LIACO-COV-SERV-CHRGS-EXC-CS-X (ASUB, BSUB).

           IF CIACO-REM-COV-CHRGS    OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-REM-COV-CHRGS    OF ITCIACO (SUB)
                TO LIACO-REM-COV-CHRGS    (ASUB, BSUB)
           ELSE
              MOVE CIACO-REM-COV-CHRGS-X    OF ITCIACO (SUB)
                TO LIACO-REM-COV-CHRGS-X    (ASUB, BSUB).

           IF CIACO-REM-PMT-ALLWD-AMT    OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-REM-PMT-ALLWD-AMT    OF ITCIACO (SUB)
                TO LIACO-REM-PMT-ALLWD-AMT    (ASUB, BSUB)
           ELSE
              MOVE CIACO-REM-PMT-ALLWD-AMT-X    OF ITCIACO (SUB)
                TO LIACO-REM-PMT-ALLWD-AMT-X    (ASUB, BSUB).

           IF CIACO-ACCUM-SUB-LIAB OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-ACCUM-SUB-LIAB OF ITCIACO (SUB)
                TO LIACO-ACCUM-SUB-LIAB (ASUB, BSUB)
           ELSE
              MOVE CIACO-ACCUM-SUB-LIAB-X OF ITCIACO (SUB)
                TO LIACO-ACCUM-SUB-LIAB-X (ASUB, BSUB).

      *R14.0   E_64148  DJL BEGIN GROUP ACCUMULATORS TOGETHER
           IF CIACO-INIT-ALLWD-AMT   OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-INIT-ALLWD-AMT   OF ITCIACO (SUB)
                TO LIACO-INIT-ALLWD-AMT   (ASUB, BSUB)
           ELSE
              MOVE CIACO-INIT-ALLWD-AMT-X   OF ITCIACO (SUB)
                TO LIACO-INIT-ALLWD-AMT-X   (ASUB, BSUB).

           IF CIACO-REM-ALLWD-AMT      OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-REM-ALLWD-AMT      OF ITCIACO (SUB)
                TO LIACO-REM-ALLWD-AMT      (ASUB, BSUB)
           ELSE
              MOVE CIACO-REM-ALLWD-AMT-X      OF ITCIACO (SUB)
                TO LIACO-REM-ALLWD-AMT-X      (ASUB, BSUB).

           IF CIACO-ACCUM-EXCL-SUB-REDS   OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-ACCUM-EXCL-SUB-REDS OF ITCIACO (SUB)
                TO LIACO-ACCUM-EXCL-SUB-REDS (ASUB, BSUB)
           ELSE
              MOVE CIACO-ACCUM-EXCL-SUB-REDS-X OF ITCIACO (SUB)
                TO LIACO-ACCUM-EXCL-SUB-REDS-X (ASUB, BSUB).
      *R14.0   E_64148  DJL END   GROUP ACCUMULATORS TOGETHER
133000
           IF CIACO-CFAS-NON-COV-SERV OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-NON-COV-SERV OF ITCIACO (SUB)
                TO LIACO-CFAS-NON-COV-SERV (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-NON-COV-SERV-X OF ITCIACO (SUB)
                TO LIACO-CFAS-NON-COV-SERV-X (ASUB, BSUB).

           IF CIACO-CFAS-NON-COV-D-V-U OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-NON-COV-D-V-U OF ITCIACO (SUB)
                TO LIACO-CFAS-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-NON-COV-D-V-U-X OF ITCIACO (SUB)
                TO LIACO-CFAS-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACO-CFAS-MAX-DOL OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-MAX-DOL OF ITCIACO (SUB)
                TO LIACO-CFAS-MAX-DOL (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-MAX-DOL-X OF ITCIACO (SUB)
                TO LIACO-CFAS-MAX-DOL-X (ASUB, BSUB).

           IF CIACO-CFAS-PRIV-RM-DIFF OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-PRIV-RM-DIFF OF ITCIACO (SUB)
                TO LIACO-CFAS-PRIV-RM-DIFF (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-PRIV-RM-DIFF-X OF ITCIACO (SUB)
                TO LIACO-CFAS-PRIV-RM-DIFF-X (ASUB, BSUB).

           IF CIACO-CFAS-DED-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-DED-AMT OF ITCIACO (SUB)
                TO LIACO-CFAS-DED-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-DED-AMT-X OF ITCIACO (SUB)
                TO LIACO-CFAS-DED-AMT-X (ASUB, BSUB).

           IF CIACO-CFAS-CO-PAY-AMT1 OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-CO-PAY-AMT1 OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-PAY-AMT1 (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-CO-PAY-AMT1-X OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-PAY-AMT1-X (ASUB, BSUB).

           IF CIACO-CFAS-CO-PAY-AMT2 OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-CO-PAY-AMT2 OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-PAY-AMT2 (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-CO-PAY-AMT2-X OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-PAY-AMT2-X (ASUB, BSUB).

           IF CIACO-CFAS-CO-INS-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAS-CO-INS-AMT OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-INS-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAS-CO-INS-AMT-X OF ITCIACO (SUB)
                TO LIACO-CFAS-CO-INS-AMT-X (ASUB, BSUB).

           IF CIACO-CFAB-NON-COV-SERV OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-NON-COV-SERV OF ITCIACO (SUB)
                TO LIACO-CFAB-NON-COV-SERV (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-NON-COV-SERV-X OF ITCIACO (SUB)
                TO LIACO-CFAB-NON-COV-SERV-X (ASUB, BSUB).

           IF CIACO-CFAB-NON-COV-D-V-U OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-NON-COV-D-V-U OF ITCIACO (SUB)
                TO LIACO-CFAB-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-NON-COV-D-V-U-X OF ITCIACO (SUB)
                TO LIACO-CFAB-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACO-CFAB-MAX-DOL OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-MAX-DOL OF ITCIACO (SUB)
                TO LIACO-CFAB-MAX-DOL (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-MAX-DOL-X OF ITCIACO (SUB)
                TO LIACO-CFAB-MAX-DOL-X (ASUB, BSUB).

           IF CIACO-CFAB-PRIV-RM-DIFF OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-PRIV-RM-DIFF OF ITCIACO (SUB)
                TO LIACO-CFAB-PRIV-RM-DIFF (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-PRIV-RM-DIFF-X OF ITCIACO (SUB)
                TO LIACO-CFAB-PRIV-RM-DIFF-X (ASUB, BSUB).

           IF CIACO-CFAB-DED-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-DED-AMT OF ITCIACO (SUB)
                TO LIACO-CFAB-DED-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-DED-AMT-X OF ITCIACO (SUB)
                TO LIACO-CFAB-DED-AMT-X (ASUB, BSUB).

           IF CIACO-CFAB-CO-PAY-AMT1 OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-CO-PAY-AMT1 OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-PAY-AMT1 (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-CO-PAY-AMT1-X OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-PAY-AMT1-X (ASUB, BSUB).

           IF CIACO-CFAB-CO-PAY-AMT2 OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-CO-PAY-AMT2 OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-PAY-AMT2 (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-CO-PAY-AMT2-X OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-PAY-AMT2-X (ASUB, BSUB).

           IF CIACO-CFAB-CO-INS-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-CO-INS-AMT OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-INS-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-CO-INS-AMT-X OF ITCIACO (SUB)
                TO LIACO-CFAB-CO-INS-AMT-X (ASUB, BSUB).

           IF CIACO-CFAB-PRC-PCT-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-CFAB-PRC-PCT-AMT OF ITCIACO (SUB)
                TO LIACO-CFAB-PRC-PCT-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-CFAB-PRC-PCT-AMT-X OF ITCIACO (SUB)
                TO LIACO-CFAB-PRC-PCT-AMT-X (ASUB, BSUB).

           IF CIACO-BMFASS-NON-COV-SERV OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASS-NON-COV-SERV OF ITCIACO (SUB)
                TO LIACO-BMFASS-NON-COV-SERV (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASS-NON-COV-SERV-X OF ITCIACO (SUB)
                TO LIACO-BMFASS-NON-COV-SERV-X (ASUB, BSUB).

           IF CIACO-BMFASS-NON-COV-D-V-U OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASS-NON-COV-D-V-U OF ITCIACO (SUB)
                TO LIACO-BMFASS-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASS-NON-COV-D-V-U-X OF ITCIACO (SUB)
                TO LIACO-BMFASS-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACO-BMFASS-MAX-DOL OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASS-MAX-DOL OF ITCIACO (SUB)
                TO LIACO-BMFASS-MAX-DOL (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASS-MAX-DOL-X OF ITCIACO (SUB)
                TO LIACO-BMFASS-MAX-DOL-X (ASUB, BSUB).

           IF CIACO-BMFASS-DOL-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASS-DOL-RED OF ITCIACO (SUB)
                TO LIACO-BMFASS-DOL-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASS-DOL-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFASS-DOL-RED-X (ASUB, BSUB).

           IF CIACO-BMFASS-PCT-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASS-PCT-RED OF ITCIACO (SUB)
                TO LIACO-BMFASS-PCT-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASS-PCT-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFASS-PCT-RED-X (ASUB, BSUB).

           IF CIACO-BMFASB-NON-COV-SERV OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASB-NON-COV-SERV OF ITCIACO (SUB)
                TO LIACO-BMFASB-NON-COV-SERV (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASB-NON-COV-SERV-X OF ITCIACO (SUB)
                TO LIACO-BMFASB-NON-COV-SERV-X (ASUB, BSUB).

           IF CIACO-BMFASB-NON-COV-D-V-U OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASB-NON-COV-D-V-U OF ITCIACO (SUB)
                TO LIACO-BMFASB-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASB-NON-COV-D-V-U-X OF ITCIACO (SUB)
                TO LIACO-BMFASB-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACO-BMFASB-MAX-DOL OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASB-MAX-DOL OF ITCIACO (SUB)
                TO LIACO-BMFASB-MAX-DOL (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASB-MAX-DOL-X OF ITCIACO (SUB)
                TO LIACO-BMFASB-MAX-DOL-X (ASUB, BSUB).

           IF CIACO-BMFASB-DOL-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASB-DOL-RED OF ITCIACO (SUB)
                TO LIACO-BMFASB-DOL-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASB-DOL-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFASB-DOL-RED-X (ASUB, BSUB).

           IF CIACO-BMFASB-PCT-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFASB-PCT-RED OF ITCIACO (SUB)
                TO LIACO-BMFASB-PCT-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFASB-PCT-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFASB-PCT-RED-X (ASUB, BSUB).

      *PT BELOW ADDED SUP/SUR
           IF CIACO-SSAB-MAX-SUPP-AMT  OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-SSAB-MAX-SUPP-AMT  OF ITCIACO (SUB)
                TO LIACO-SSAB-MAX-SUPP-AMT  (ASUB, BSUB)
           ELSE
              MOVE CIACO-SSAB-MAX-SUPP-AMT-X  OF ITCIACO (SUB)
                TO LIACO-SSAB-MAX-SUPP-AMT-X  (ASUB, BSUB).

           IF CIACO-SSAB-SURCHARGE-AMT OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-SSAB-SURCHARGE-AMT OF ITCIACO (SUB)
                TO LIACO-SSAB-SURCHARGE-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-SSAB-SURCHARGE-AMT-X OF ITCIACO (SUB)
                TO LIACO-SSAB-SURCHARGE-AMT-X (ASUB, BSUB).
      *PT ABOVE ADDED SUP/SUR
154100
      *PAYMAX BELOW ADDED
           IF CIACO-PMS-PAY-MAX-AMT    OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-PMS-PAY-MAX-AMT OF ITCIACO (SUB)
                TO LIACO-PMS-PAY-MAX-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-PMS-PAY-MAX-AMT-X OF ITCIACO (SUB)
                TO LIACO-PMS-PAY-MAX-AMT-X (ASUB, BSUB).

           IF CIACO-PMB-PAY-MAX-AMT    OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-PMB-PAY-MAX-AMT OF ITCIACO (SUB)
                TO LIACO-PMB-PAY-MAX-AMT (ASUB, BSUB)
           ELSE
              MOVE CIACO-PMB-PAY-MAX-AMT-X OF ITCIACO (SUB)
                TO LIACO-PMB-PAY-MAX-AMT-X (ASUB, BSUB).
      *PAYMAX ABOVE ADDED
156400
      *R14.0   E_84725  RDW  BEGIN
           IF CIACO-BMFAP-SANC-DOL-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFAP-SANC-DOL-RED OF ITCIACO (SUB)
                TO LIACO-BMFAP-SANC-DOL-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFAP-SANC-DOL-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFAP-SANC-DOL-RED-X (ASUB, BSUB).

           IF CIACO-BMFAP-SANC-PCT-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFAP-SANC-PCT-RED OF ITCIACO (SUB)
                TO LIACO-BMFAP-SANC-PCT-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFAP-SANC-PCT-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFAP-SANC-PCT-RED-X (ASUB, BSUB).

           IF CIACO-BMFAP-EXCESS-DOL-RED  OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFAP-EXCESS-DOL-RED  OF ITCIACO (SUB)
                TO LIACO-BMFAP-EXCESS-DOL-RED  (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFAP-EXCESS-DOL-RED-X  OF ITCIACO (SUB)
                TO LIACO-BMFAP-EXCESS-DOL-RED-X  (ASUB, BSUB).

           IF CIACO-BMFAP-EXCESS-PER-DAY-RED  OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFAP-EXCESS-PER-DAY-RED  OF ITCIACO (SUB)
                TO LIACO-BMFAP-EXCESS-PER-DAY-RED  (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFAP-EXC-PERDAY-RED-X    OF ITCIACO (SUB)
                TO LIACO-BMFAP-EXC-PER-DAY-RED-X (ASUB, BSUB).

           IF CIACO-BMFAS-PROV-SANC-RED OF ITCIACO (SUB) NUMERIC
              MOVE CIACO-BMFAS-PROV-SANC-RED OF ITCIACO (SUB)
                TO LIACO-BMFAS-PROV-SANC-RED (ASUB, BSUB)
           ELSE
              MOVE CIACO-BMFAS-PROV-SANC-RED-X OF ITCIACO (SUB)
                TO LIACO-BMFAS-PROV-SANC-RED-X (ASUB, BSUB).
      *R14.0   E_84725  RDW  END
156400
      ******************************************************************
       C800-MOVE-CALCS-TO-CALREP.
      ******************************************************************

           ADD +1  TO  LIAREP-END-OF-LN.
           MOVE LIAREP-END-OF-LN    TO SUB.

           MOVE LIALM-SCCF-LN-NUM   OF IULIALM
                                    TO LIAREP-SCCF-LN-NUM   (SUB).

      *CLAIM-LEVEL
           MOVE LIACS-LAST-PROC-BEN-LEV
                                    TO LIAREP-LAST-PROC-BEN-LEV (SUB).
           MOVE LIACS-LAST-PROC-ADJUD-LN-NUM
                              TO LIAREP-LAST-PROC-ADJUD-LN-NU   (SUB).
           IF LIACS-LAST-PROC-SPLT-CLM-SET NUMERIC
              MOVE LIACS-LAST-PROC-SPLT-CLM-SET
                TO LIAREP-LAST-PROC-SPLT-CLM-SET(SUB).
           MOVE LIACS-BEN-LEV-MAX
                                       TO LIAREP-BEN-LEV-MAX (SUB).
           MOVE LIACS-ADJUD-LN-MAX
                                       TO LIAREP-ADJUD-LN-MAX   (SUB).
           MOVE LIACS-WC-DATA-BASE-USE-IND
                                 TO LIAREP-WC-DATA-BASE-USE-IND (SUB).
           MOVE LIACS-SUB-PROC-MULTI-PASS-CD
                               TO LIAREP-SUB-PROC-MULTI-PASS-CD (SUB).

           MOVE LIACS-CLM-PRC-AMT-RED-WKARND
                           TO LIAREP-CLM-PRC-AMT-RED-WKARN   (SUB).

      ***OPL START
           MOVE LIACS-LAST-PROC-ADJ-COB-LN-NUM
                           TO  LIAREP-LAST-PROC-ADJ-COB-LN-NU (SUB).
           MOVE LIACS-COB-CALC-MTD
                           TO  LIAREP-COB-CALC-MTD  (SUB).
           MOVE LIACS-SEC-PAYOR-QUAL-CD
                           TO  LIAREP-SEC-PAYOR-QUAL-CD (SUB).
           MOVE LIACS-PRIM-IND
                           TO  LIAREP-PRIM-IND   (SUB).
           MOVE LIACS-USE-CI-AREA-IND
                           TO  LIAREP-USE-CI-AREA-IND (SUB).
           MOVE LIACS-TYPE-PROCESS-COB-CD
                           TO  LIAREP-TYPE-PROCESS-COB-CD (SUB).
      ***OPL END
160600*R14.0  E_84725  KJO  BEGIN
           MOVE LIACS-PROV-SANC-IND
                           TO  LIAREP-PROV-SANC-IND (SUB).
           MOVE LIACS-EXCESS-DAYS
                           TO  LIAREP-EXCESS-DAYS   (SUB).
           MOVE LIACS-PROV-SANC-DT
                           TO  LIAREP-PROV-SANC-DT  (SUB).
           MOVE LIACS-ICD-10-COMPL-DT
                           TO  LIAREP-ICD-10-COMPL-DT (SUB).
160400     MOVE LIACS-PROV-SANC-REM
160500                     TO  LIAREP-PROV-SANC-REM (SUB).
160600*R14.0  E_84725  KJO  END
      *R16.0   E_57627  DJL BEGIN
           MOVE LIACS-ALT-HOME-PRC-IND
                           TO LIAREP-ALT-HOME-PRC-IND        (SUB).
           MOVE LIACS-INT-ALT-HOME-PRC-AMT-CLM
                           TO LIAREP-INT-ALT-HOME-PRC-AMT-CL (SUB).
           MOVE LIACS-REM-ALT-HOME-PRC-AMT-CLM
                           TO LIAREP-REM-ALT-HOME-PRC-AMT-CL (SUB).
           MOVE LIACS-TOT-REM-CHRG
                           TO LIAREP-TOT-REM-CHRG            (SUB).
      *R16.0   E_57627  DJL END
           MOVE LIACS-CLM-PRC-AMT-CALC-CD
                           TO LIAREP-CLM-PRC-AMT-CALC-CD     (SUB).
166300     MOVE LIACS-WKARND-ALLWD-AMT
166400       TO LIAREP-WKARND-ALLWD-AMT    (SUB).

      *GRP2 SF LINE
           MOVE LIACS-REM-CHRG     (LSUB)
                                         TO LIAREP-REM-CHRG     (SUB).
           MOVE LIACS-REM-INIT-PRC-AMT       (LSUB)
                               TO LIAREP-REM-INIT-PRC-AMT       (SUB).
           MOVE LIACS-REM-INIT-ALLWD-AMT      (LSUB)
                              TO LIAREP-REM-INIT-ALLWD-AMT      (SUB).
           MOVE LIACS-PRORATE-PCT-FCT-PRC    (LSUB)
                               TO LIAREP-PRORATE-PCT-FCT-PRC    (SUB).
           MOVE LIACS-PRORATE-PCT-FCT-ALLWD (LSUB)
                                TO LIAREP-PRORATE-PCT-FCT-ALLWD (SUB).
           MOVE LIACS-PRORATE-PCT-FCT-SAVE (LSUB)
                                 TO LIAREP-PRORATE-PCT-FCT-SAVE (SUB).
           MOVE LIACS-PRORATE-PCT-FCT-2ND-SAVE (LSUB)
                              TO LIAREP-PRORATE-PCT-FCT-2ND-SAV (SUB).
           MOVE LIACS-REM-2ND-SAVINGS    (LSUB)
                                   TO LIAREP-REM-2ND-SAVINGS    (SUB).
           MOVE LIACS-WORKRULE-PRC-AMT-REDUC   (LSUB)
                              TO LIAREP-WORKRULE-PRC-AMT-REDU   (SUB).
           MOVE LIACS-WORKRULE-PRC-REDUC-CD   (LSUB)
                              TO LIAREP-WORKRULE-PRC-REDUC-CD   (SUB).
           MOVE LIACS-PREV-SUB-LIAB (LSUB)
                                        TO LIAREP-PREV-SUB-LIAB (SUB).
           MOVE LIACS-REM-SAVINGS    (LSUB)
                                       TO LIAREP-REM-SAVINGS    (SUB).
           MOVE LIACS-REM-PCT-FCT    (LSUB)
                                       TO LIAREP-REM-PCT-FCT    (SUB).
      *R14.0  E_84725  KJO BEGIN
           MOVE LIACS-PROV-SANC-INIT-LINE (LSUB)
                                  TO LIAREP-PROV-SANC-INIT-LINE (SUB).
           MOVE LIACS-PROV-SANC-REM-LINE (LSUB)
                                  TO LIAREP-PROV-SANC-REM-LINE  (SUB).
      *R14.0  E_84725  KJO END
      * R14.0   E_64148 RDW BEGIN
167900     MOVE LIACS-REM-SUP-AMT (LSUB)
168000                            TO LIAREP-REMAIN-SUP-AMT (SUB).
      * R14.0   E_64148 RDW END
166600     MOVE LIACS-CPD-REM-2ND-SAVINGS     (LSUB)
166700       TO LIAREP-CPD-REM-2ND-SAVINGS    (SUB).
166800     MOVE LIACS-CPD-REM-SAVINGS         (LSUB)
166900       TO LIAREP-CPD-REM-SAVINGS        (SUB).
      * R12.0.1 TD_77549 GWS START
      *13.0.1 PD_83690 DJL CHANGE ASUB TO LSUB
           MOVE LIACS-PM42-PRC-AMT-RED-WKARND (LSUB)
             TO LIAREP-PM42-PRC-AMT-RED-WKARND     (SUB).
      * R12.0.1 TD_77549 GWS END

      *GRP 3 ADJUD LINE
           MOVE LIACS-ADJUD-CHRG  (ASUB)  TO LIAREP-ADJUD-CHRG  (SUB).
           MOVE LIACS-ADJUD-CHRG-INIT     (ASUB)
                                  TO LIAREP-ADJUD-CHRG-INIT     (SUB).
           MOVE LIACS-ADJUD-DISCOUNTED-CHRG  (ASUB)
                               TO LIAREP-ADJUD-DISCOUNTED-CHRG  (SUB).
           MOVE LIACS-ADJUD-PRC-AMT    (ASUB)
                                     TO LIAREP-ADJUD-PRC-AMT    (SUB).
           MOVE LIACS-ADJUD-ALLWD-AMT   (ASUB)
                                    TO LIAREP-ADJUD-ALLWD-AMT   (SUB).
           MOVE LIACS-ADJUD-SAVINGS (ASUB)
                                        TO LIAREP-ADJUD-SAVINGS (SUB).
           MOVE LIACS-ADJUD-REM-CHRG     (ASUB)
                                   TO LIAREP-ADJUD-REM-CHRG     (SUB).
           MOVE LIACS-ADJUD-PCT-FCT (ASUB)
                                        TO LIAREP-ADJUD-PCT-FCT (SUB).
           MOVE LIACS-ADJUD-REM-SAVINGS    (ASUB)
                                 TO LIAREP-ADJUD-REM-SAVINGS    (SUB).
           MOVE LIACS-ADJUD-REM-2ND-SAVINGS    (ASUB)
                              TO LIAREP-ADJUD-REM-2ND-SAVING    (SUB).
           MOVE LIACS-ADJUD-2ND-SAVINGS (ASUB)
                              TO LIAREP-ADJUD-2ND-SAVINGS (SUB).
           MOVE LIACS-ADJUD-LN-PROCESS-CNT   (ASUB)
                               TO LIAREP-ADJUD-LN-PROCESS-CNT   (SUB).
           MOVE LIACS-BENEFIT-LEV-COUNTER (ASUB)
                                  TO LIAREP-BENEFIT-LEV-COUNTER (SUB).
      *R14.0  E_84725  KJO BEGIN
165400     MOVE LIACS-PROV-SANC-APPLIED-LINE   (ASUB)
165500                        TO LIAREP-PROV-SANC-APPLIED-LINE  (SUB).
      *R14.0  E_84725  KJO END
           MOVE LIACS-CPD-ADJUD-SAVINGS       (ASUB)
             TO LIAREP-CPD-ADJUD-SAVINGS      (SUB).
           MOVE LIACS-CPD-ADJUD-REM-SAVINGS   (ASUB)
             TO LIAREP-CPD-ADJUD-REM-SAVINGS  (SUB).
           MOVE LIACS-CPD-ADJUD-REM-2ND-SAVING(ASUB)
             TO LIAREP-CPD-ADJUD-REM-2ND-SAVIN(SUB).
           MOVE LIACS-CPD-ADJUD-2ND-SAVINGS   (ASUB)
             TO LIAREP-CPD-ADJUD-2ND-SAVINGS  (SUB).
           MOVE LIACS-PRIV-RM-DIFF-CD         (ASUB)
             TO LIAREP-PRIV-RM-DIFF-CD        (SUB).
      * R14.0   E_64148 RDW BEGIN
167900     MOVE LIACS-ADJUD-REMAIN-SUP-AMT    (ASUB)
168000       TO LIAREP-ADJUD-REMAIN-SUP-AMT   (SUB).
      * R14.0   E_64148 RDW END
168100
      ******************************************************************
       C900-MOVE-ITCIACI-TO-ADJCI.
      ******************************************************************

           IF LIALM-ADJUD-LN-NUM   OF IULIALM
                    GREATER THAN LIACI-END-OF-LN
              MOVE LIALM-ADJUD-LN-NUM   OF IULIALM
                              TO LIACI-END-OF-LN.

      *11.4   E_41590  DJL BEGIN
           MOVE CIACI-SUB-COST-SHR-SEQ-CD-CLM
             TO LIACI-SUB-COST-SHR-SEQ-CD (ASUB, BSUB).
      *11.4   E_41590  DJL END
169100
           IF CIACI-DED-REM-1                             NUMERIC
              MOVE CIACI-DED-REM-1
                TO LIACI-DED-REM-1  (ASUB, BSUB)
           ELSE
              MOVE CIACI-DED-REM-1-X
                TO LIACI-DED-REM-1-X  (ASUB, BSUB).

           MOVE CIACI-REM-OOP-MAX-CD
             TO LIACI-REM-OOP-MAX-CD (ASUB, BSUB).

           IF CIACI-REM-OOP-MAX    NUMERIC
              MOVE CIACI-REM-OOP-MAX
                TO LIACI-REM-OOP-MAX (ASUB, BSUB)
           ELSE
              MOVE CIACI-REM-OOP-MAX-X
                TO LIACI-REM-OOP-MAX-X (ASUB, BSUB).

           IF CIACI-NUM-SERV   (WS-PLUS-ONE)     NUMERIC
              MOVE CIACI-NUM-SERV   (WS-PLUS-ONE)
                TO LIACI-NUM-SERV   (ASUB, BSUB)
           ELSE
              MOVE CIACI-NUM-SERV-X   (WS-PLUS-ONE)
                TO LIACI-NUM-SERV-X   (ASUB, BSUB).

           IF CIACI-SERV-CHRG  (WS-PLUS-ONE)     NUMERIC
              MOVE CIACI-SERV-CHRG  (WS-PLUS-ONE)
                TO LIACI-SERV-CHRG  (ASUB, BSUB)
           ELSE
              MOVE CIACI-SERV-CHRG-X  (WS-PLUS-ONE)
                TO LIACI-SERV-CHRG-X  (ASUB, BSUB).

           MOVE CIACI-NON-COV-SERV-CD (WS-PLUS-ONE)
             TO LIACI-NON-COV-SERV-CD (ASUB, BSUB).

           IF CIACI-NON-COV-D-V-U (WS-PLUS-ONE)  NUMERIC
              MOVE CIACI-NON-COV-D-V-U (WS-PLUS-ONE)
                TO LIACI-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACI-NON-COV-D-V-U-X (WS-PLUS-ONE)
                TO LIACI-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACI-MAX-DOL-ALLWD (WS-PLUS-ONE)  NUMERIC
              MOVE CIACI-MAX-DOL-ALLWD (WS-PLUS-ONE)
                TO LIACI-MAX-DOL-ALLWD (ASUB, BSUB)
           ELSE
              MOVE CIACI-MAX-DOL-ALLWD-X (WS-PLUS-ONE)
                TO LIACI-MAX-DOL-ALLWD-X (ASUB, BSUB).

           MOVE CIACI-PRIV-RM-DIFF-CD (WS-PLUS-ONE)
             TO LIACI-PRIV-RM-DIFF-CD (ASUB, BSUB).

           IF CIACI-PRIV-RM-PART-COV (WS-PLUS-ONE) NUMERIC
              MOVE CIACI-PRIV-RM-PART-COV (WS-PLUS-ONE)
                TO LIACI-PRIV-RM-PART-COV  (ASUB, BSUB)
           ELSE
              MOVE CIACI-PRIV-RM-PART-COV-X   (WS-PLUS-ONE)
                TO LIACI-PRIV-RM-PART-COV-X   (ASUB, BSUB).

           IF CIACI-CO-PAY-AMT1 (WS-PLUS-ONE)    NUMERIC
              MOVE CIACI-CO-PAY-AMT1 (WS-PLUS-ONE)
                TO LIACI-CO-PAY-AMT1 (ASUB, BSUB)
           ELSE
              MOVE CIACI-CO-PAY-AMT1-X (WS-PLUS-ONE)
                TO LIACI-CO-PAY-AMT1-X (ASUB, BSUB).

           IF CIACI-CO-INS-PCT (WS-PLUS-ONE)     NUMERIC
              MOVE CIACI-CO-INS-PCT (WS-PLUS-ONE)
                TO LIACI-CO-INS-PCT (ASUB, BSUB)
           ELSE
              MOVE CIACI-CO-INS-PCT-X (WS-PLUS-ONE)
                TO LIACI-CO-INS-PCT-X (ASUB, BSUB).

           MOVE CIACI-BMSP-NON-COV-SERV-CD (WS-PLUS-ONE)
             TO LIACI-BMSP-NON-COV-SERV-CD (ASUB, BSUB).

           IF CIACI-BMSP-NON-COV-D-V-U (WS-PLUS-ONE) NUMERIC
              MOVE CIACI-BMSP-NON-COV-D-V-U (WS-PLUS-ONE)
                TO LIACI-BMSP-NON-COV-D-V-U (ASUB, BSUB)
           ELSE
              MOVE CIACI-BMSP-NON-COV-D-V-U-X (WS-PLUS-ONE)
                TO LIACI-BMSP-NON-COV-D-V-U-X (ASUB, BSUB).

           IF CIACI-BMSP-MAX-DOL-ALLWD (WS-PLUS-ONE) NUMERIC
              MOVE CIACI-BMSP-MAX-DOL-ALLWD (WS-PLUS-ONE)
                TO LIACI-BMSP-MAX-DOL-ALLWD (ASUB, BSUB)
           ELSE
              MOVE CIACI-BMSP-MAX-DOL-ALLWD-X (WS-PLUS-ONE)
                TO LIACI-BMSP-MAX-DOL-ALLWD-X (ASUB, BSUB).

           IF CIACI-BMSP-DOL-RED (WS-PLUS-ONE)   NUMERIC
              MOVE CIACI-BMSP-DOL-RED (WS-PLUS-ONE)
                TO LIACI-BMSP-DOL-RED (ASUB, BSUB)
           ELSE
              MOVE CIACI-BMSP-DOL-RED-X (WS-PLUS-ONE)
                TO LIACI-BMSP-DOL-RED-X (ASUB, BSUB).

           IF CIACI-BMSP-PCT-RED (WS-PLUS-ONE)   NUMERIC
              MOVE CIACI-BMSP-PCT-RED (WS-PLUS-ONE)
                TO LIACI-BMSP-PCT-RED (ASUB, BSUB)
           ELSE
              MOVE CIACI-BMSP-PCT-RED-X (WS-PLUS-ONE)
                TO LIACI-BMSP-PCT-RED-X (ASUB, BSUB).
179400
      ******************************************************************
       X100-INIT-SIAPC-RETURN-DATA.
      ******************************************************************

           MOVE       SPACES                          TO
                      SIAPC-RETURN-DATA.

           MOVE       CWSCC-SUCCESSFUL                TO
                      SIAPC-RETURN-IND.

           MOVE       CWSCC-ACTION-COMPLETE           TO
                      SIAPC-RETURN-STATUS.

      ******************************************************************
       Z100-INVALID-FUNCTION.
      ******************************************************************
           MOVE       CWSCC-SEVERE-LOG                TO
                      SIAPC-RETURN-IND.

           MOVE       CWSCC-INVALID-FUNCTION          TO
                      SIAPC-RETURN-STATUS.

           MOVE       SPACES                          TO
                      SIAPC-RETURN-MESSAGE-CD.
