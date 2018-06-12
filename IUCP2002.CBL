       IDENTIFICATION DIVISION.
       PROGRAM-ID.                       IUCP2002.
       AUTHOR.                           IBS.
       DATE-WRITTEN.                     AUGUST, 1993.
       DATE-COMPILED.
      *********** EHIMS ************************************************
      *  COMMENT OUT PCK10 PCK24   SEARCH FOR EHIMS
      *********** EHIMS ************************************************
      ******************************************************************
      *                                                                *
      *            INTER-PLAN TELEPROCESSING SERVICES (ITS)            *
      *                                                                *
      *   MODULE:       IUCP2002                                       *
      *   NAME:         HPA BATCH ENTRY POINT MODULE                   *
      *   VERSION:      11.9                                           *
      *   DATE:         11/10/2016                                     *
      *                                                                *
      *   APPLICATION:  HOME PLAN AID                                  *
      *   TYPE:         COMMON                                         *
      *   FUNCTION:     THIS MODULE IS THE ENTRY POINT TO ACTIVATE THE *
      *                 BATCH PROCESS FOR THE FREE-STANDING VERSION OF *
      *                 THE HPA KERNEL.                                *
      *                                                                *
      *   MODIFICATION                                                 *
      *   HISTORY:      09/24/93 01.0 - INITIAL RELEASE                *
      *                 02/10/95 02.0 - ADDED IUCIAPF AS ADDITIONAL    *
      *                                 PASSING PARAMETER              *
      *                               - CHANGED MINIMUM RECORD LENGTH  *
      *                                 FROM 127 TO 48.                *
      *                 06/30/95 03.0 - HPA VERSION 4.0 UPDATE         *
      *                                 REMOVED DUP CHECKING           *
      *                 05/31/96 03.0A- CHANGED THE MINUM RECORD SIZE  *
      *                                 FROM 048 TO 127 IN FD SECTION. *
      *                                 THIS CHANGE IS REQUIRED WHEN   *
      *                                 USING COBOL II COMPILER.       *
      *                 01/17/97 04.0 - HPA VERSION 5.0.               *
      *                 04/30/97 05.0 - HPA 5.0 UPDATE BULLETIN        *
      *                                 INCREASED SIAPC-MODULE NAME    *
      *                                 DISPLAY FROM 5 TO 10 WHEN      *
      *                                 ABORTED.                       *
      *                 11/13/98 06.0 - HPA 5.2 BULLETIN               *
      *                               - INITIALIZE TABLE ENTRY COUNTS  *
      *                                 FOR OPEN CALL.                 *
      *                               - RETURN CODE CHANGED TO 3500    *
      *                                 FOR EMPTY AIDCOMM INPUT FILE.  *
      *                 10/22/01 07.0 - HPA 5.6 RELEASE                *
      *                               - ADDED ITCIACIP, ITCIACOP       *
      *                 02/08/02 07.1 - HPA 5.6 ADDENDUM               *
      *                               - MODIFIED TO READ THE AIDCOMM   *
      *                                 INTO WORKING-STORAGE SECTION   *
      *                                 AREA TO PREVENT U4038 ABEND    *
      *                                 WHEN THE LAST FILE IS EMPTY.   *
      *                 12/10/02 08.0 - HPA 5.8 RELEASE                *
      *                               - ADDED CIAAJR-MEDICARE TYPE OF  *
      *                                 PROCESSING.                    *
      *                 06/30/04 09.0 - HPA 5.11 GEN REL               *
      *                               - MODIFIED TO SUPPORT ITS8.9     *
      *                                 AEA TABLE ENHANCEMENTS.        *
      *                 08/20/04 09.1 - HPA 5.11 A1 ADDENDUM           *
      *                               - MODIFIED TO SUPPORT ITCPCD09   *
      *                                 RESEQUENCE CHANGE.             *
      *                 06/21/05 10.0 - HPA 5.13 BETA REL              *
      *                 07/01/05 10.0 - HPA 5.13 GEN REL               *
      *                               - MODIFIED TO BY PASS CALL TO HPA*
      *                                 KERNEL WHEN INPUT CLAIM FAILS  *
      *                                 STACK EDIT (MISSING RECORD OR  *
      *                                 OUT OF SEQUENCE) ENHANCED STACK*
      *                                 EDIT. CHANGED DISPLAY TO SHOW  *
      *                                 PROBLEM SCCF SERIAL NUMBER.    *
      *                 11/16/07 11.0 - HPA 5.17 (ITS 10.1)            *
      *                               - E_23333 LINE EXPANSION         *
      *                               - CHANGED SUB TO S9(4) COMP.     *
      *                               - CHANGED AID COMM RECORD FROM   *
      *                                 7376 TO 17673.                 *
      *                 02/15/08 11.1 - TECHMEMO ITS-TM-2008-02-R10.1.1*
      *                                 SA. ADDED COMMIT AND ROLLBACK  *
      *                                 LOGIC.                         *
      *                 03/21/08 11.2 - ITS-TM-2008-03-R10.1.2         *
      *                                 TD_35292 CIAPF PASS PROBLEM    *
      *                                 TD_36059 INV SEQ/MAX ERR PROB  *
      *                                 TD_36139 INV SF/TBL-ENTRY-CNT  *
      *                                        ETC.                    *
      ******************************************************************00020004
      * REL   - DATE       - VERS - CONTROL  - DESCRIPTION             *00030003
      *R11.2  ¦ 11/11/2010 ¦ 11.3 ¦ E_32688  ¦ DJL: 5010 CHANGES       *00040006
      *         CHANGE AIDCOMM RECORD LENGTH FROM 17673 TO 15642       *00050003
      *R12.0  ¦ 05/17/2012 ¦ 11.4 ¦ E_41233  ¦ DJL: MULTIPLE INCLUSIVE *00040014
      *         GROUPINGS - CHANGE NAMES OF COMMANDS SENT TO ITCP9730  *00050013
      *R14.0  ¦ 11/07/2013 ¦ 11.5 ¦ E_72114  ¦ KJO: PROVIDER PENALTIES *00040014
      *         - ALSO INCLUDES E_27465.  ADD ITCIAAI.                 *00050013
      *R14.5  ¦ 05/15/2014 ¦ 11.6 ¦ E_34072  ¦ DJL: ADD THE WORD       *00040014
      *         'INPUT' TO THE RECORD COUNT DISPLAY.  ADD THE          *00050013
      *         PROGRAM DESCRIPTION BANNER AS THE FIRST DISPLAY.       *
      *         ENHANCE OUTPUT FOR UNSUCCESSFUL CLAIMS.                *
      *R15.0  ¦ 11/06/2014 ¦ 11.7 ¦ PD_96126 ¦ DJL: SET THE VALUE OF   *00040014
      *         THE SYSTEM CREATOR CODE TO SPACES IF IT 'NULL'.        *00050013
      *         ADD CALL TO ITPEXPS1 AS A SAFEGUARD AGAINST THE        *
      *         POSSIBILITY OF NO EXIT PROGRAMS EXECUTING PRIOR TO THE *
      *         FIRST DB2 CALL (PER STANDARDS).                        *
      *R16.5.1¦ 08/25/2016 ¦ 11.8 ¦ PD_113047¦ DJL: INITIALIZE ITCIAAI *
      *R17.0  ¦ 11/10/2016 ¦ 11.9 ¦ PD_112790¦ DJL: HANDLE UNIDENTIFIED*
      *         PSEUDOAIDCOMM INPUT RECORDS.                           *
      *         REMOVE OBSOLETE WS AND CHANGE SELECTED PARAGRAPH NAMES.*
      *         REORGANIZE THE DISPLAY AND END-OF-PROCESS PARAGRAPHS   *
      *         TO ELIMINATE REDUNDANT CODE                            *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT AID-COMM-FILE ASSIGN TO UT-S-AIDCOMM.
                                                                        00069
           SELECT PARM-CARD-FILE ASSIGN TO UT-S-PARMCD00.

       DATA DIVISION.

       FILE SECTION.

       FD  AID-COMM-FILE
           RECORDING MODE IS V
      *11.2   E_32688  BZXDJL1 BEGIN                                    00010004
           LABEL RECORDS ARE OMITTED
           BLOCK CONTAINS 0 RECORDS.
      *11.2   E_32688  BZXDJL1 END                                      00010004

       01  AID-COMM-RECORD.
      *11.2   E_32688  BZXDJL1 BEGIN                                    00010004
           05  FILLER                     PIC X(15642).
      *11.2   E_32688  BZXDJL1 END                                      00010004

       FD  PARM-CARD-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE OMITTED
      *11.2   E_32688  BZXDJL1                                          00010004
           BLOCK CONTAINS 0 RECORDS.

       01  PARM-CARD-RECORD               PIC X(80).


      ***** W O R K I N G  S T O R A G E  S E C T I O N  *****
      *------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *------------------------------------------------------*

      *11.2   E_32688  BZXDJL1 BEGIN                                    00010004
       01  WS-PROGRAM-INFO.                                             00020000
           05  WS-MODULE-ID               PIC X(8)  VALUE 'IUCP2002'.   00030002
           05  WS-PROGRAM-ID              REDEFINES WS-MODULE-ID        00040001
                                          PIC X(8).                     00050001
           05  WS-VERSION-NUMBER          PIC X(4)  VALUE '11.9'.       00060002
           05  WS-RELEASE-DATE            PIC X(8)  VALUE '11/10/16'.   00070004
           05  WS-PROGRAM-DISPLAY.                                      00080000
               10  WS-COMMON-DISPLAY      PIC X(30) VALUE               00090000
                   'IUCP2002-KERNEL ENTRY POINT  '.                     00100002
               10  WS-MODULE-DISPLAY      PIC X(8)  VALUE ' MODULE '.   00110000
               10  WS-RELEASE-DISPLAY     PIC X(34) VALUE               00120000
                   'VERSION #11.9 RELEASED ON 11/10/16'.                00130004
           05  WS-DESCRIPTION             REDEFINES WS-PROGRAM-DISPLAY  00140001
                                          PIC X(72).                    00150001
      *11.2   E_32688  BZXDJL1 END                                      00160004

       01  WS-INDICATORS.
           05  WS-AID-FILE-EOF-SW          PIC X(01) VALUE 'N'.
               88  NOT-EOF-AID-FILE                  VALUE 'N'.
               88  EOF-AID-FILE                      VALUE 'Y'.
           05  WS-AID-FILE-ERROR-SW        PIC X(01) VALUE 'N'.
               88  NO-AID-FILE-ERROR                 VALUE 'N'.
               88  AID-FILE-ERROR                    VALUE 'Y'.
           05  WS-ITCP9730-ERR-PREV-TP1-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-PREV-TP1          VALUE 'N'.
               88  ITCP9730-ERR-PREV-TP1             VALUE 'Y'.
           05  WS-ITCP9730-ERR-PREV-TP2-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-PREV-TP2          VALUE 'N'.
               88  ITCP9730-ERR-PREV-TP2             VALUE 'Y'.
           05  WS-ITCP9730-ERR-PREV-TP3-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-PREV-TP3          VALUE 'N'.
               88  ITCP9730-ERR-PREV-TP3             VALUE 'Y'.
           05  WS-ITCP9730-ERR-PREV-TP4-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-PREV-TP4          VALUE 'N'.
               88  ITCP9730-ERR-PREV-TP4             VALUE 'Y'.
           05  WS-ITCP9730-ERR-PREV-TP5-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-PREV-TP5          VALUE 'N'.
               88  ITCP9730-ERR-PREV-TP5             VALUE 'Y'.
           05  WS-ITCP9730-ERR-CURR-TP1-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-CURR-TP1          VALUE 'N'.
               88  ITCP9730-ERR-CURR-TP1             VALUE 'Y'.
           05  WS-ITCP9730-ERR-CURR-TP2-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-CURR-TP2          VALUE 'N'.
               88  ITCP9730-ERR-CURR-TP2             VALUE 'Y'.
           05  WS-ITCP9730-ERR-CURR-TP3-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-CURR-TP3          VALUE 'N'.
               88  ITCP9730-ERR-CURR-TP3             VALUE 'Y'.
           05  WS-ITCP9730-ERR-CURR-TP4-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-CURR-TP4          VALUE 'N'.
               88  ITCP9730-ERR-CURR-TP4             VALUE 'Y'.
           05  WS-ITCP9730-ERR-CURR-TP5-SW PIC X(01) VALUE 'N'.
               88  NO-ITCP9730-ERR-CURR-TP5          VALUE 'N'.
               88  ITCP9730-ERR-CURR-TP5             VALUE 'Y'.
           05  WS-FILE-ERROR-SW            PIC X(01) VALUE 'N'.
               88  NO-FILE-ERROR                     VALUE 'N'.
               88  FILE-ERROR                        VALUE 'Y'.
           05  WS-END-OF-REQUEST-IND       PIC X(01) VALUE 'N'.
               88  END-OF-REQUEST                    VALUE 'Y'.
           05  WS-PREV-REC-TYPE-X          PIC X(01).
           05  FILLER REDEFINES WS-PREV-REC-TYPE-X.
               10  WS-PREV-REC-TYPE        PIC 9(01).
           05  WS-CURR-REC-TYPE-X          PIC X(01).
           05  FILLER REDEFINES WS-CURR-REC-TYPE-X.
               10  WS-CURR-REC-TYPE        PIC 9(01).

       01  WS-LITERALS.
           05  WS-YES                      PIC X(01) VALUE 'Y'.
           05  WS-NO                       PIC X(01) VALUE 'N'.
           05  WS-ONE                      PIC 9(01) VALUE 1.
           05  WS-TWO-C               COMP PIC S9(4) VALUE +2.
           05  WS-TWENTY                   PIC 9(02) VALUE 20.
           05  WS-TWENTY-ONE               PIC 9(02) VALUE 21.
      *R17.0   PD_112790 DJL ADD ONE LINE BELOW
           05  WS-MAX-1098            COMP PIC S9(04) VALUE 1098.
           05  WS-RET-CODE-3500       COMP PIC S9(4) VALUE +3500.
           05  WS-RET-CODE-3600       COMP PIC S9(4) VALUE +3600.
           05  WS-CIACI-REC-TYPE           PIC X(01) VALUE '1'.
           05  WS-AIDCOMM-FILE-TYPE        PIC X(01) VALUE 'A'.
           05  WS-REASSEMBLE-ACTION        PIC X(01) VALUE 'R'.
           05  WS-99999                    PIC X(05) VALUE '99999'.
      *R15.0   PD_96126 DJL ADD ONE LINE BELOW
           05  WS-ITPEXPS1                 PIC X(08) VALUE 'ITPEXPS1'.
           05  WS-ITDUTLS1                 PIC X(08) VALUE 'ITDUTLS1'.

      *11.2   E_32688  BZXDJL1 BEGIN                                    00160004
       01  WS-SUBSCRIPTS.
           05  RSUB                        PIC S9(04) COMP VALUE ZERO.
           05  SUB                         PIC S9(04) COMP VALUE ZEROES.
      *11.2   E_32688  BZXDJL1 END                                      00160004

      *11.2   E_32688  BZXDJL1                                          00160004
       01  WS-COUNTERS                     COMP-3.
           05  WS-TYPE-1-REC-CNT           PIC 9(07) VALUE ZEROES.
           05  WS-TYPE-2-REC-CNT           PIC 9(07) VALUE ZEROES.
           05  WS-TYPE-3-REC-CNT           PIC 9(07) VALUE ZEROES.
           05  WS-TYPE-4-REC-CNT           PIC 9(07) VALUE ZEROES.
           05  WS-TYPE-5-REC-CNT           PIC 9(07) VALUE ZEROES.
      *R17.0   PD_112790 DJL ADD ONE LINE BELOW
           05  WS-TYPE-UNK-REC-CNT         PIC 9(07) VALUE ZEROES.
           05  WS-RECORD-CNT               PIC 9(07) VALUE ZEROES.
           05  WS-COMMIT-COUNT             PIC 9(07) VALUE ZEROES.

       01  WS-MSG-AREA.
           05  RETURN-IND-MSG              PIC X(12) VALUE
               'RETURN-IND: '.
           05  RETURN-STATUS-MSG           PIC X(15) VALUE
               'RETURN-STATUS: '.
           05  ERROR-MESSAGE-MSG           PIC X(15) VALUE
               'ERROR MESSAGE: '.
      *R14.5   E_34072  DJL BEGIN
           05  ERROR-EXCEPTION-MSG         PIC X(23) VALUE
               'ERROR/EXCEPTION CODES: '.
           05  CLAIM-STATUS-CD-MSG         PIC X(17) VALUE
               'CLAIM STATUS CD: '.
           05  CLAIM-PRIMACY-MSG           PIC X(13) VALUE
               'PRIMACY IND: '.
      *R14.5   E_34072  DJL END
           05  WS-SIAPC-MODULE-NAME-1      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  1: '.
           05  WS-SIAPC-MODULE-NAME-2      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  2: '.
           05  WS-SIAPC-MODULE-NAME-3      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  3: '.
           05  WS-SIAPC-MODULE-NAME-4      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  4: '.
           05  WS-SIAPC-MODULE-NAME-5      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  5: '.
           05  WS-SIAPC-MODULE-NAME-6      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  6: '.
           05  WS-SIAPC-MODULE-NAME-7      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  7: '.
           05  WS-SIAPC-MODULE-NAME-8      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  8: '.
           05  WS-SIAPC-MODULE-NAME-9      PIC X(22) VALUE
               'SIAPC-MODULE-NAME  9: '.
           05  WS-SIAPC-MODULE-NAME-10     PIC X(22) VALUE
               'SIAPC-MODULE-NAME 10: '.

       01  WS-ASTERISKS.
           05  FILLER                      PIC X(48) VALUE
               '************************************************'.
           05  FILLER                      PIC X(28) VALUE
               '****************************'.

      *R17.0   PD_112790 DJL BEGIN
       01  WS-AID-COMM-REC-ERROR-MSG-1.
           05  FILLER                  PIC X(48) VALUE
               'UNKNOWN RECORD TYPE IN INPUT FILE               '.
           05  FILLER                      PIC X(28) VALUE
               '                            '.
       01  WS-AID-COMM-REC-ERROR-MSG-2.
           05  FILLER                  PIC X(48) VALUE
               'INPUT RECORDS OUT OF SEQUENCE, DUPLICATE RECORDS'.
           05  FILLER                      PIC X(28) VALUE
               ', OR MISSING TYPE/SCCF      '.
       01  WS-INVLD-REC-SEQ-MAX-NUM-MSG.
           05  FILLER                  PIC X(48) VALUE
               'INVALID SEQ NUM/MAX SEQ/TBL ENTRY COUNT DURING R'.
           05  FILLER                      PIC X(28) VALUE
               'EASSEMBLY                   '.
      *R17.0   PD_112790 DJL END

       01  WS-CALL-PROGRAM                 PIC X(8) VALUE SPACES.

       01  WS-SAVED-SCCF-SER-NUM           PIC X(17) VALUE SPACES.

      *R15.0   PD_96126 DJL BEGIN
       01  WS-CIAPEXN-RECORD.
           COPY ITCIPEXN.
      *R15.0   PD_96126 DJL END

       01  IUCWSCC.
           COPY IUCWSCC.

       01  IUCWSNM.
           COPY IUCWSNM.

      *------------------------------------------------------*
      *    I N P U T   A R E A                               *
      *------------------------------------------------------*

       01  WS-AID-COMM-RECORD.
           COPY IUCDB100.

       01  WS-PARMCARD-RECORD.
           05  WS-PARM-FREQUENCY-X.
               10  WS-PARM-FREQUENCY     PIC 9(05).

      *------------------------------------------------------*
      *    P A S S   A R E A                                 *
      *------------------------------------------------------*
       01  ITSIAPC.
           COPY ITSIAPC.

       01  ITCIACI-AREA.
           03  ITCIACI-ID   PIC X.
           03  ITCIACI.
               COPY ITCIACI.

       01  ITCIACP-AREA.
           03  ITCIACP-ID   PIC X.
           03  ITCIACP.
               COPY ITCIACP.

       01  IUCIAAJR-AREA.
           03  IUCIAAJR-ID  PIC X.
           03  IUCIAAJR.
               COPY IUCIAAJR.

       01  ITCIACIP-AREA.
           03  ITCIACIP-ID  PIC X.
           03  ITCIACIP.
               COPY ITCIACIP.

       01  ITCIACO.
           COPY ITCIACO.

       01  ITCIACOP.
           COPY ITCIACOP.

       01  IUCIADR.
           COPY IUCIADR.

       01  IUCIAPF-AREA.
           03  IUCIAPF-ID   PIC X.
           03  IUCIAPF.
               COPY IUCIAPF.

      *E_23333
      *AIDCOMM REASSEMBLE AREA
           COPY ITCIARAP.

       01  CSAPI-API-AREA.
           COPY ITCCSAPI.

           COPY ITDUTLX1.

       01  IUCPCD00.
           COPY IUCPCD00.

       01  IUCPCF00.
           COPY IUCPCF00.

       01  ITCPC600.
           COPY ITCPC600.

       01  ITCPCE00.
           COPY ITCPCE00.

       01  ITCPCF00.
           COPY ITCPCF00.

       01  ITCPCP00.
           COPY ITCPCP00.

       01  ITCPCQ00.
           COPY ITCPCQ00.

       01  ITCPCD09.
           COPY ITCPCD09.

       01  ITCPCK00.
           COPY ITCPCK00.

       01  ITCPCK01.
           COPY ITCPCK01.

       01  ITCPCK02.
           COPY ITCPCK02.

       01  ITCPCK03.
           COPY ITCPCK03.

       01  ITCPCK04.
           COPY ITCPCK04.

       01  ITCPCK05.
           COPY ITCPCK05.

       01  ITCPCK06.
           COPY ITCPCK06.

       01  ITCPCK07.
           COPY ITCPCK07.

       01  ITCPCK08.
           COPY ITCPCK08.

       01  ITCPCK09.
           COPY ITCPCK09.

      *********** EHIMS ************************************************0000000
      *01  ITCPCK10.
      *    COPY ITCPCK10.
      *
      *01  ITCPCK11.
      *    COPY ITCPCK11.
      *
      *01  ITCPCK12.
      *    COPY ITCPCK12.
      *
      *01  ITCPCK13.
      *    COPY ITCPCK13.
      *
      *01  ITCPCK14.
      *    COPY ITCPCK14.
      *
      *01  ITCPCK15.
      *    COPY ITCPCK15.
      *
      *01  ITCPCK16.
      *    COPY ITCPCK16.
      *
      *01  ITCPCK17.
      *    COPY ITCPCK17.
      *
      *01  ITCPCK18.
      *    COPY ITCPCK18.
      *
      *01  ITCPCK19.
      *    COPY ITCPCK19.
      *
      *01  ITCPCK20.
      *    COPY ITCPCK20.
      *
      *01  ITCPCK21.
      *    COPY ITCPCK21.
      *
      *01  ITCPCK22.
      *    COPY ITCPCK22.
      *
      *01  ITCPCK23.
      *    COPY ITCPCK23.
      *
      *01  ITCPCK24.
      *    COPY ITCPCK24.
      *********** EHIMS ************************************************0000000

      *14.0   E_72114  KJO BEGIN
       01  ITCIAAI.
           COPY ITCIAAI.
      *14.0   E_72114  KJO END

       01  FILLER                          PIC X(44) VALUE
           '**** IUCP2002 WORKING-STORAGE ENDS HERE ****'.


      ****************************************************************
      ****************************************************************
      *    P R O C E D U R E   D I V I S I O N                       *
      ****************************************************************
      ****************************************************************
       PROCEDURE DIVISION.

      ****************************************************************
       0000-MAINLINE.
      ****************************************************************

      *R14.5   E_34072  DJL ADD PROGRAM BANNER DISPLAY
           DISPLAY WS-PROGRAM-DISPLAY.

           PERFORM A100-INITIALIZATION.

           PERFORM A200-MAIN-PROCESS
               UNTIL EOF-AID-FILE.

           PERFORM A300-FINAL-PROCESS.

           PERFORM 9999-GOBACK.

      ****************************************************************
       A100-INITIALIZATION.
      ****************************************************************

      *R15.0   PD_96126 DJL ADD ONE LINE BELOW
           CALL WS-ITPEXPS1 USING WS-CIAPEXN-RECORD.


           OPEN INPUT AID-COMM-FILE.
           OPEN INPUT PARM-CARD-FILE.

           PERFORM X100-INIT-SIAPC-RETURN-DATA.

      *R17.0   PD_112790 DJL BEGIN MAKE USE OF EXISTING PARAGRAPH
           MOVE WS-NO TO WS-AID-FILE-EOF-SW
                         WS-AID-FILE-ERROR-SW
                         WS-END-OF-REQUEST-IND.
           PERFORM S600-INIT-CURR-PREV-TYX-ERR-SW.
      *R17.0   PD_112790 DJL END   MAKE USE OF EXISTING PARAGRAPH

           PERFORM J100-SETUP-FOR-CALL.
           MOVE CWSCC-OPEN-FILES-REQUEST  TO SIAPC-FUNCTION-CD-MAJOR.
           MOVE CWSCC-UPF-AND-COB         TO SIAPC-FUNCTION-CD-SUB.
           PERFORM T100-CALL-IUCP9102.

           PERFORM U100-CHECK-RETURN-STATUS.

           PERFORM X200-INIT-WORK-AREA.

           PERFORM X300-INIT-TBL-ENT-CNTS.

           PERFORM B700-CLEAR-SCCF-NUM.

           PERFORM S050-HANDLE-PARMCARD-FILE.

           PERFORM S100-READ-AID-COMM-FILE.

      ****************************************************************
       A200-MAIN-PROCESS.
      ****************************************************************

           IF NO-AID-FILE-ERROR
               IF WS-SAVED-SCCF-SER-NUM EQUAL SPACES
                   PERFORM B100-GET-AID-COMM-RECS
               ELSE
                   PERFORM B100-GET-AID-COMM-RECS
                      VARYING RSUB FROM 1 BY 1
                        UNTIL END-OF-REQUEST
                           OR EOF-AID-FILE
           ELSE
               PERFORM B700-CLEAR-SCCF-NUM
               MOVE 'N' TO WS-END-OF-REQUEST-IND
               PERFORM S600-INIT-CURR-PREV-TYX-ERR-SW
               SET NO-AID-FILE-ERROR TO TRUE.

           IF NOT-EOF-AID-FILE
               IF END-OF-REQUEST
                   IF NO-AID-FILE-ERROR
                   AND NO-ITCP9730-ERR-PREV-TP1
                   AND NO-ITCP9730-ERR-PREV-TP2
                   AND NO-ITCP9730-ERR-PREV-TP3
                   AND NO-ITCP9730-ERR-PREV-TP4
                   AND NO-ITCP9730-ERR-PREV-TP5
                       PERFORM B200-CALL-KERNEL-PROC-CLM
                       PERFORM B400-COMMIT
                   END-IF
                   PERFORM S500-INIT-PREV-TPX-ERR-SW-OTH
                   PERFORM B700-CLEAR-SCCF-NUM
               END-IF
           END-IF.

           IF EOF-AID-FILE
               IF NO-AID-FILE-ERROR
               AND NO-ITCP9730-ERR-PREV-TP1
               AND NO-ITCP9730-ERR-PREV-TP2
               AND NO-ITCP9730-ERR-PREV-TP3
               AND NO-ITCP9730-ERR-PREV-TP4
               AND NO-ITCP9730-ERR-PREV-TP5
                   PERFORM B200-CALL-KERNEL-PROC-CLM.

      ****************************************************************
       A300-FINAL-PROCESS.
      ****************************************************************

           CLOSE AID-COMM-FILE.
           CLOSE PARM-CARD-FILE.

      *R14.5   E_34072  DJL ADD ONE LINE BELOW
           PERFORM X100-INIT-SIAPC-RETURN-DATA.

           PERFORM J100-SETUP-FOR-CALL.
           MOVE CWSCC-CLOSE-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR.
           MOVE CWSCC-UPF-AND-COB         TO SIAPC-FUNCTION-CD-SUB.
           PERFORM T100-CALL-IUCP9102.

           PERFORM U100-CHECK-RETURN-STATUS.

           PERFORM Z900-DISPLAY-REC-CNTS.

      ****************************************************************
       B100-GET-AID-COMM-RECS.
      ****************************************************************

      *R17.0   PD_112790 DJL BEGIN REWRITTEN AS AN EVALUATE
           EVALUATE TRUE
             WHEN CDB100-CI-ID
               PERFORM C100-MOVE-CIACI-AREA

             WHEN CDB100-CP-ID
               PERFORM C200-MOVE-CIACP-AREA

             WHEN CDB100-AJR-ID
               PERFORM C300-MOVE-CIAAJR-AREA

             WHEN CDB100-PF-ID
               PERFORM C400-MOVE-CIAPF-AREA

             WHEN CDB100-CIP-ID
               PERFORM C500-MOVE-CIACIP-AREA

             WHEN OTHER
               PERFORM C900-HANDLE-UNKNOWN-TYPE

           END-EVALUATE.
      *R17.0   PD_112790 DJL END   REWRITTEN AS AN EVALUATE

           PERFORM S100-READ-AID-COMM-FILE.

      ****************************************************************
       B200-CALL-KERNEL-PROC-CLM.
      ****************************************************************

           PERFORM J100-SETUP-FOR-CALL.
           PERFORM D600-SET-MAJ-SUB-FUNC-CD.
           PERFORM T100-CALL-IUCP9102.

           PERFORM U100-CHECK-RETURN-STATUS.

           PERFORM X200-INIT-WORK-AREA.

      ****************************************************************
       B400-COMMIT.
      ****************************************************************

           IF WS-PARM-FREQUENCY-X NOT = WS-99999
               ADD 1 TO WS-COMMIT-COUNT
               IF (WS-COMMIT-COUNT = WS-PARM-FREQUENCY OR
                   WS-COMMIT-COUNT > WS-PARM-FREQUENCY)
                   MOVE 0 TO WS-COMMIT-COUNT
                   PERFORM B450-COMMIT-UTILITY.

      ****************************************************************
       B450-COMMIT-UTILITY.
      ****************************************************************

           PERFORM X400-CICS-COMMIT.

           IF NOT CSAPI-SUCCESSFUL
               MOVE CWSCC-SEVERE-LOG      TO SIAPC-RETURN-IND
               MOVE CSAPI-RETURN-CODE     TO SIAPC-ABEND-CODE
               MOVE CSAPI-RETURN-MESSAGE  TO SIAPC-ERROR-MESSAGE
      *R14.5   E_34072  DJL CHANGE TO NEW PARAGRAPH
               PERFORM U300-CHECK-COMMIT-RETURN-STAT
           END-IF.

      ****************************************************************
       B700-CLEAR-SCCF-NUM.
      ****************************************************************

           MOVE SPACES TO CIACI-SCCF-SER-NUM
                          CIACP-SCCF-SER-NUM
                          CIAAJR-SCCF-SER-NUM
                          CIACIP-SCCF-SER-NUM
                          CIAPF-SCCF-SER-NUM.

      ****************************************************************
       C100-MOVE-CIACI-AREA.
      ****************************************************************

           ADD WS-ONE    TO WS-TYPE-1-REC-CNT.

           MOVE CDB100-ID-IND TO WS-PREV-REC-TYPE-X.

           MOVE CIARAP-AIDCOMM-RECORD TO ITCIACI-AREA.

           MOVE CDB100-SCCF-SER-NUM TO CIACI-SCCF-SER-NUM
                                       WS-SAVED-SCCF-SER-NUM.

      ****************************************************************
       C200-MOVE-CIACP-AREA.
      ****************************************************************

           ADD WS-ONE     TO WS-TYPE-2-REC-CNT.

           MOVE CDB100-ID-IND TO WS-PREV-REC-TYPE-X.

           MOVE CIARAP-AIDCOMM-RECORD TO ITCIACP-AREA.

           MOVE CDB100-SCCF-SER-NUM TO CIACP-SCCF-SER-NUM
                                       WS-SAVED-SCCF-SER-NUM.

      ****************************************************************
       C300-MOVE-CIAAJR-AREA.
      ****************************************************************

           ADD WS-ONE    TO WS-TYPE-3-REC-CNT.

           MOVE CDB100-ID-IND TO WS-PREV-REC-TYPE-X.

           MOVE CIARAP-AIDCOMM-RECORD TO IUCIAAJR-AREA.

           MOVE CDB100-SCCF-SER-NUM TO CIAAJR-SCCF-SER-NUM
                                       WS-SAVED-SCCF-SER-NUM.

      *R15.0   PD_96126 DJL BEGIN
           IF CIAAJR-SYS-CREATOR-CD < SPACES
               MOVE SPACES          TO CIAAJR-SYS-CREATOR-CD
           END-IF.
      *R15.0   PD_96126 DJL END

      ****************************************************************
       C400-MOVE-CIAPF-AREA.
      ****************************************************************

      *R17.0   PD_112790 DJL MOVE ONE LINE FROM BELOW
           ADD WS-ONE    TO WS-TYPE-4-REC-CNT.

           MOVE WS-AID-COMM-RECORD TO CDB100-CIAPF-AREA.

           IF CDB100-CIAPF-DATA-PRESENT
               MOVE CDB100-ID-IND TO WS-PREV-REC-TYPE-X
               MOVE CDB100-SCCF-SER-NUM TO WS-SAVED-SCCF-SER-NUM
               MOVE CIARAP-AIDCOMM-RECORD TO IUCIAPF-AREA
           END-IF.

      ****************************************************************
       C500-MOVE-CIACIP-AREA.
      ****************************************************************

           ADD WS-ONE    TO WS-TYPE-5-REC-CNT.

           MOVE CDB100-ID-IND TO WS-PREV-REC-TYPE-X.

           MOVE CDB100-SCCF-SER-NUM TO CIACIP-SCCF-SER-NUM
                                       WS-SAVED-SCCF-SER-NUM.

           MOVE CIARAP-AIDCOMM-RECORD TO ITCIACIP-AREA.

      *R17.0   PD_112790 DJL BEGIN
      ****************************************************************
       C900-HANDLE-UNKNOWN-TYPE.
      ****************************************************************

           ADD WS-ONE    TO WS-TYPE-UNK-REC-CNT.

           PERFORM Z500-INVLD-RECORD-TYPE.
      *R17.0   PD_112790 DJL END

      ****************************************************************
       D600-SET-MAJ-SUB-FUNC-CD.
      ****************************************************************

           MOVE CWSCC-NORMAL-PROCESSING   TO SIAPC-FUNCTION-CD-MAJOR.
           MOVE SPACES                    TO SIAPC-FUNCTION-CD-SUB.

           IF CIAAJR-PAY-OVR-APPLIED
               IF CIAAJR-SECONDARY
               OR CIAAJR-SECONDARY-WOULD-PAY
               OR CIAAJR-MEDICARE
                   MOVE CWSCC-UPF-AND-COB
                     TO SIAPC-FUNCTION-CD-SUB
               END-IF
           END-IF.

           IF CIAAJR-PAY-OVR-NA
               IF CIAAJR-PRIMARY
                   CONTINUE
               ELSE
                   IF CIAAJR-COB-SYSA
                       MOVE CWSCC-UPF-AND-COB
                         TO SIAPC-FUNCTION-CD-SUB
                   ELSE
                       IF CIAAJR-COB-SYSZ
                           MOVE CWSCC-COB-ONLY
                             TO SIAPC-FUNCTION-CD-SUB
                       END-IF
                   END-IF
               END-IF
           END-IF.

      ****************************************************************
       J100-SETUP-FOR-CALL.
      ****************************************************************

      *16.5.1 PD_113047 DJL ADD ONE LINE BELOW
           MOVE SPACES                  TO CIAAI-DATA.

      *R17.0   PD_112790 DJL ADD ONE LINE BELOW
           PERFORM K000-SET-MODULE-NAME-TABLE.

           MOVE CWSNM-IUCP9102-NAME     TO WS-CALL-PROGRAM,
                                           SIAPC-MODULE-NAME (2).
           MOVE WS-TWO-C                TO SIAPC-MODULE-LEVEL.

      ****************************************************************
       J200-SETUP-FOR-CALL-ITCP9730.
      ****************************************************************

      *R17.0   PD_112790 DJL ADD ONE LINE BELOW
           PERFORM K000-SET-MODULE-NAME-TABLE.

           MOVE CWSNM-ITCP9730-NAME     TO WS-CALL-PROGRAM,
                                           SIAPC-MODULE-NAME (2).
           MOVE WS-TWO-C                TO SIAPC-MODULE-LEVEL.

      *R17.0   PD_112790 DJL BEGIN
      ****************************************************************
       K000-SET-MODULE-NAME-TABLE.
      ****************************************************************

           MOVE CWSNM-IUCP2002-NAME     TO SIAPC-MODULE-NAME (1).
           MOVE SPACES                  TO SIAPC-MODULE-NAME (3)
                                           SIAPC-MODULE-NAME (4)
                                           SIAPC-MODULE-NAME (5)
                                           SIAPC-MODULE-NAME (6)
                                           SIAPC-MODULE-NAME (7)
                                           SIAPC-MODULE-NAME (8)
                                           SIAPC-MODULE-NAME (9)
                                           SIAPC-MODULE-NAME (10).
      *R17.0   PD_112790 DJL END

      ****************************************************************
       S050-HANDLE-PARMCARD-FILE.
      ****************************************************************

           READ PARM-CARD-FILE INTO WS-PARMCARD-RECORD.

           IF WS-PARM-FREQUENCY-X NOT NUMERIC
               DISPLAY '*******************************************'
               DISPLAY '*******************************************'
               DISPLAY 'NO PARM CARD OR COMMIT FREQUENCY NOT VALID '
               DISPLAY 'PROGRAM DEFAULTS TO COMMIT FREQUENCY OF 1  '
               DISPLAY '*******************************************'
               DISPLAY '*******************************************'
               MOVE 1 TO WS-PARM-FREQUENCY.

      ****************************************************************
       S100-READ-AID-COMM-FILE.
      ****************************************************************

           READ AID-COMM-FILE INTO WS-AID-COMM-RECORD
             AT END
               MOVE WS-YES TO WS-AID-FILE-EOF-SW
               PERFORM V200-VALIDATE-RECORD-STACK
           END-READ.

           IF NOT-EOF-AID-FILE
      *R17.0   PD_112790 DJL BEGIN INCORPORATE OLD PARAGRAPH HERE
               IF (WS-SAVED-SCCF-SER-NUM NOT EQUAL SPACES AND
                  (CDB100-SCCF-SER-NUM NOT EQUAL WS-SAVED-SCCF-SER-NUM))
      *            ADD 'OR' CLAUSE FOR PROPER CONTROL BREAK
               OR WS-SAVED-SCCF-SER-NUM EQUAL SPACES
                   PERFORM S400-SAVE-INIT-ERR-SW.
      *R17.0   PD_112790 DJL END   INCORPORATE OLD PARAGRAPH HERE

           IF EOF-AID-FILE
               PERFORM S400-SAVE-INIT-ERR-SW.

           IF NOT-EOF-AID-FILE
               PERFORM S200-READ-N-REASSEMBLE-RECS.

           MOVE CDB100-ID-IND TO WS-CURR-REC-TYPE-X.

           IF NOT-EOF-AID-FILE
               PERFORM U200-VALIDATE-INPUT.

           IF NOT-EOF-AID-FILE
               ADD WS-ONE TO WS-RECORD-CNT.

      *R17.0   PD_112790 DJL BEGIN
      *    THIS APPEARS TO BE MISPLACED; MOVED TO U200-
      *    IF WS-SAVED-SCCF-SER-NUM NOT EQUAL SPACES
      *        IF CDB100-SCCF-SER-NUM
      *                       NOT EQUAL WS-SAVED-SCCF-SER-NUM
      *            MOVE WS-YES TO WS-END-OF-REQUEST-IND.
      *R17.0   PD_112790 DJL END

      ****************************************************************
       S200-READ-N-REASSEMBLE-RECS.
      ****************************************************************

      *E_23333
           INITIALIZE CIARAP-AIDCOMM-AREA.
      *12.0   E_41233  DJL BEGIN
           MOVE WS-AIDCOMM-FILE-TYPE TO CIARAP-RECORD-PASSED.
           MOVE WS-REASSEMBLE-ACTION TO CIARAP-RECORD-ACTION.
      *12.0   E_41233  DJL END

           MOVE WS-ONE TO SUB.
           MOVE CDB100-AREA TO CIARAP-AIDCOMM-SPLIT-ENTRY (SUB).

           IF CDB100-REC-SEQ-NUM NUMERIC AND
              CDB100-MAX-REC-NUM NUMERIC AND
              CDB100-REC-SEQ-NUM > ZERO AND
              CDB100-REC-SEQ-NUM < WS-TWENTY-ONE AND
              CDB100-MAX-REC-NUM > ZERO AND
              CDB100-MAX-REC-NUM < WS-TWENTY-ONE
               IF CDB100-REC-SEQ-NUM NOT EQUAL CDB100-MAX-REC-NUM
                   ADD WS-ONE TO SUB
                   PERFORM S300-READ-MULTI-REC-SETS
                       UNTIL SUB > CDB100-MAX-REC-NUM
                          OR EOF-AID-FILE
               END-IF
           END-IF.

           PERFORM X100-INIT-SIAPC-RETURN-DATA.

           PERFORM J200-SETUP-FOR-CALL-ITCP9730.
           PERFORM T200-CALL-ITCP9730.

           PERFORM U400-CHECK-ITCP9730-STATUS.

      ****************************************************************
       S300-READ-MULTI-REC-SETS.
      ****************************************************************

           READ AID-COMM-FILE INTO WS-AID-COMM-RECORD
             AT END
               MOVE WS-YES TO WS-AID-FILE-EOF-SW
               PERFORM V200-VALIDATE-RECORD-STACK
           END-READ.

           IF NOT-EOF-AID-FILE
               MOVE CDB100-AREA TO CIARAP-AIDCOMM-SPLIT-ENTRY (SUB)
      *R17.0   PD_112790 DJL BEGIN REWRITTEN AS AN EVALUATE
               EVALUATE TRUE
                 WHEN CDB100-CI-ID
                   ADD WS-ONE    TO WS-TYPE-1-REC-CNT

                 WHEN CDB100-CP-ID
                   ADD WS-ONE    TO WS-TYPE-2-REC-CNT

                 WHEN CDB100-AJR-ID
                   ADD WS-ONE    TO WS-TYPE-3-REC-CNT

                 WHEN CDB100-PF-ID
                   ADD WS-ONE    TO WS-TYPE-4-REC-CNT

                 WHEN CDB100-CIP-ID
                   ADD WS-ONE    TO WS-TYPE-5-REC-CNT

                 WHEN OTHER
                   ADD WS-ONE    TO WS-TYPE-UNK-REC-CNT
                   PERFORM Z500-INVLD-RECORD-TYPE

               END-EVALUATE
      *R17.0   PD_112790 DJL END   REWRITTEN AS AN EVALUATE
           END-IF.

           ADD WS-ONE TO SUB.

      ****************************************************************
       S400-SAVE-INIT-ERR-SW.
      ****************************************************************

           MOVE WS-ITCP9730-ERR-CURR-TP1-SW
             TO WS-ITCP9730-ERR-PREV-TP1-SW.

           MOVE WS-ITCP9730-ERR-CURR-TP2-SW
             TO WS-ITCP9730-ERR-PREV-TP2-SW.

           MOVE WS-ITCP9730-ERR-CURR-TP3-SW
             TO WS-ITCP9730-ERR-PREV-TP3-SW.

           MOVE WS-ITCP9730-ERR-CURR-TP4-SW
             TO WS-ITCP9730-ERR-PREV-TP4-SW.

           MOVE WS-ITCP9730-ERR-CURR-TP5-SW
             TO WS-ITCP9730-ERR-PREV-TP5-SW.

           MOVE WS-NO TO WS-ITCP9730-ERR-CURR-TP1-SW
                         WS-ITCP9730-ERR-CURR-TP2-SW
                         WS-ITCP9730-ERR-CURR-TP3-SW
                         WS-ITCP9730-ERR-CURR-TP4-SW
                         WS-ITCP9730-ERR-CURR-TP5-SW.

      ****************************************************************
       S500-INIT-PREV-TPX-ERR-SW-OTH.
      ****************************************************************

           MOVE WS-NO TO WS-AID-FILE-ERROR-SW
                         WS-ITCP9730-ERR-PREV-TP1-SW
                         WS-ITCP9730-ERR-PREV-TP2-SW
                         WS-ITCP9730-ERR-PREV-TP3-SW
                         WS-ITCP9730-ERR-PREV-TP4-SW
                         WS-ITCP9730-ERR-PREV-TP5-SW
                         WS-END-OF-REQUEST-IND.

      ****************************************************************
       S600-INIT-CURR-PREV-TYX-ERR-SW.
      ****************************************************************

           MOVE WS-NO TO WS-ITCP9730-ERR-CURR-TP1-SW
                         WS-ITCP9730-ERR-CURR-TP2-SW
                         WS-ITCP9730-ERR-CURR-TP3-SW
                         WS-ITCP9730-ERR-CURR-TP4-SW
                         WS-ITCP9730-ERR-CURR-TP5-SW
                         WS-ITCP9730-ERR-PREV-TP1-SW
                         WS-ITCP9730-ERR-PREV-TP2-SW
                         WS-ITCP9730-ERR-PREV-TP3-SW
                         WS-ITCP9730-ERR-PREV-TP4-SW
                         WS-ITCP9730-ERR-PREV-TP5-SW.

      ****************************************************************
       T100-CALL-IUCP9102.
      ****************************************************************

           CALL WS-CALL-PROGRAM USING ITSIAPC
                                      ITCIACI
                                      ITCIACP
                                      IUCIAAJR
                                      ITCIACIP
                                      ITCIACO
                                      ITCIACOP
                                      IUCIADR
                                      IUCIAPF
                                      IUCPCD00
                                      IUCPCF00
                                      ITCPCF00
                                      ITCPCE00
                                      ITCPC600
                                      ITCPCP00
                                      ITCPCQ00
                                      ITCPCD09
                                      ITCPCK00
                                      ITCPCK01
                                      ITCPCK02
                                      ITCPCK03
                                      ITCPCK04
                                      ITCPCK05
                                      ITCPCK06
                                      ITCPCK07
                                      ITCPCK08
                                      ITCPCK09
      *********** EHIMS ************************************************0000000
      *                               ITCPCK10
      *                               ITCPCK11
      *                               ITCPCK12
      *                               ITCPCK13
      *                               ITCPCK14
      *                               ITCPCK15
      *                               ITCPCK16
      *                               ITCPCK17
      *                               ITCPCK18
      *                               ITCPCK19
      *                               ITCPCK20
      *                               ITCPCK21
      *                               ITCPCK22
      *                               ITCPCK23
      *                               ITCPCK24
      *********** EHIMS ************************************************0000000
      *14.0   E_72114  KJO PASS ITCIAAI
                                      ITCIAAI.

      ****************************************************************
       T200-CALL-ITCP9730.
      ****************************************************************

           CALL WS-CALL-PROGRAM USING ITSIAPC
                                      CIARAP-AIDCOMM-AREA.

      ****************************************************************
       U100-CHECK-RETURN-STATUS.
      ****************************************************************

           IF SIAPC-RETURN-IND EQUAL TO CWSCC-ABORT
           OR SIAPC-RETURN-IND EQUAL TO CWSCC-SEVERE-LOG
      *R17.0   PD_112790 DJL BEGIN PARAGRAPH REORG
               PERFORM X500-CICS-ROLLBACK
               PERFORM Z300-ABORT
               PERFORM Z800-DISPLAY-MODULE-NAMES
               PERFORM 9999-GOBACK
      *R17.0   PD_112790 DJL END   PARAGRAPH REORG
      *R14.5   E_34072  DJL BEGIN
           ELSE
               IF SIAPC-RETURN-IND EQUAL CWSCC-UNSUCCESSFUL
                   PERFORM Z100-KERNEL-UNSUCCESS
               END-IF
           END-IF.
      *R14.5   E_34072  DJL END

      ****************************************************************
       U200-VALIDATE-INPUT.
      ****************************************************************

           IF WS-PREV-REC-TYPE-X NOT EQUAL SPACES
      *R17.0   PD_112790 DJL BEGIN
               IF (CDB100-SCCF-SER-NUM EQUAL
                   WS-SAVED-SCCF-SER-NUM AND
                   CDB100-SCCF-SER-NUM NOT EQUAL SPACES)
               OR CDB100-SCCF-SER-NUM EQUAL SPACES
               OR WS-SAVED-SCCF-SER-NUM EQUAL SPACES
      *R17.0   PD_112790 DJL END
                   IF WS-CURR-REC-TYPE GREATER THAN
                        WS-PREV-REC-TYPE
                       CONTINUE
                   ELSE
                       IF WS-CURR-REC-TYPE-X EQUAL WS-CIACI-REC-TYPE
                           MOVE WS-YES TO WS-END-OF-REQUEST-IND
                           PERFORM V200-VALIDATE-RECORD-STACK
                       END-IF
                   END-IF
               END-IF
           END-IF.

           IF CDB100-SCCF-SER-NUM NOT EQUAL
                        WS-SAVED-SCCF-SER-NUM
           AND WS-SAVED-SCCF-SER-NUM NOT EQUAL SPACES
      *R17.0   PD_112790 DJL BEGIN
           AND CDB100-SCCF-SER-NUM NOT EQUAL SPACES
               MOVE WS-YES TO WS-END-OF-REQUEST-IND
      *R17.0   PD_112790 DJL END
               PERFORM V200-VALIDATE-RECORD-STACK
           END-IF.

      *R14.5   E_34072  DJL BEGIN
      ****************************************************************
       U300-CHECK-COMMIT-RETURN-STAT.
      ****************************************************************

           IF SIAPC-RETURN-IND EQUAL TO CWSCC-ABORT
           OR SIAPC-RETURN-IND EQUAL TO CWSCC-SEVERE-LOG
      *R17.0   PD_112790 DJL BEGIN PARAGRAPH REORG
               PERFORM Z300-ABORT
               PERFORM Z800-DISPLAY-MODULE-NAMES
               PERFORM 9999-GOBACK
           END-IF.
      *R17.0   PD_112790 DJL END   PARAGRAPH REORG
      *R14.5   E_34072  DJL END

      ****************************************************************
       U400-CHECK-ITCP9730-STATUS.
      ****************************************************************

           IF SIAPC-RETURN-IND EQUAL TO CWSCC-ABORT
           OR SIAPC-RETURN-IND EQUAL TO CWSCC-SEVERE-LOG
      *R17.0   PD_112790 DJL REPLACE WITH NEW PARAGRAPH
               PERFORM V300-REASSEMBLE-ABORT-SEVERE
           ELSE
               IF SIAPC-RETURN-IND EQUAL CWSCC-UNSUCCESSFUL
                   PERFORM V400-REASSEMBLE-UNSUCCESS
               END-IF
           END-IF.

      ****************************************************************
       V200-VALIDATE-RECORD-STACK.
      ****************************************************************

      *    END OF FILE ON FIRST READ
      *    END OF FILE ON SUBSEQUENT READ, AND NO REC TYPES 1, 2 OR 3
           IF  EOF-AID-FILE AND
               CIACI-SCCF-SER-NUM = SPACES AND
               CIACP-SCCF-SER-NUM = SPACES AND
               CIAAJR-SCCF-SER-NUM = SPACES
      *R17.0   PD_112790 DJL BEGIN
               IF WS-RECORD-CNT = ZEROES
                   CONTINUE
               ELSE
                   SET AID-FILE-ERROR TO TRUE
                   SET FILE-ERROR TO TRUE
                   MOVE CDB100-SCCF-SER-NUM TO WS-SAVED-SCCF-SER-NUM
                   PERFORM Z200-INVLD-ORDER-DUPS
               END-IF
      *R17.0   PD_112790 DJL END
           ELSE
               IF (CIACI-SCCF-SER-NUM = SPACES OR
                   CIACP-SCCF-SER-NUM = SPACES OR
                   CIAAJR-SCCF-SER-NUM = SPACES)
                   SET AID-FILE-ERROR TO TRUE
                   SET FILE-ERROR TO TRUE
                   PERFORM Z200-INVLD-ORDER-DUPS
               ELSE
                   IF (CIAAJR-COB OR
                       CIAAJR-SECONDARY-WOULD-PAY)
                   AND CIACIP-SCCF-SER-NUM = SPACES
                       SET AID-FILE-ERROR TO TRUE
                       SET FILE-ERROR TO TRUE
                       PERFORM Z200-INVLD-ORDER-DUPS
                   END-IF
               END-IF
           END-IF.

      *R17.0   PD_112790 DJL BEGIN
      ****************************************************************
       V300-REASSEMBLE-ABORT-SEVERE.
      ****************************************************************

           PERFORM X500-CICS-ROLLBACK.
           PERFORM Z300-ABORT.
           PERFORM Z800-DISPLAY-MODULE-NAMES.
           PERFORM 9999-GOBACK.
      *R17.0   PD_112790 DJL END

      ****************************************************************
       V400-REASSEMBLE-UNSUCCESS.
      ****************************************************************

      *R17.0   PD_112790 DJL BEGIN REWRITTEN AS AN EVALUATE
           EVALUATE TRUE
             WHEN CDB100-CI-ID
               MOVE WS-YES TO WS-ITCP9730-ERR-CURR-TP1-SW

             WHEN CDB100-CP-ID
               MOVE WS-YES TO WS-ITCP9730-ERR-CURR-TP2-SW

             WHEN CDB100-AJR-ID
               MOVE WS-YES TO WS-ITCP9730-ERR-CURR-TP3-SW

             WHEN CDB100-PF-ID
               MOVE WS-YES TO WS-ITCP9730-ERR-CURR-TP4-SW

             WHEN CDB100-CIP-ID
               MOVE WS-YES TO WS-ITCP9730-ERR-CURR-TP5-SW

           END-EVALUATE.
      *R17.0   PD_112790 DJL END   REWRITTEN AS AN EVALUATE

           SET FILE-ERROR TO TRUE.
           PERFORM Z400-INVLD-SEQ-MAX-TBL.

      ****************************************************************
       X100-INIT-SIAPC-RETURN-DATA.
      ****************************************************************

           MOVE SPACES                  TO SIAPC-RETURN-DATA.
           MOVE CWSCC-SUCCESSFUL        TO SIAPC-RETURN-IND.
           MOVE CWSCC-ACTION-COMPLETE   TO SIAPC-RETURN-STATUS.

      ****************************************************************
       X200-INIT-WORK-AREA.
      ****************************************************************

           MOVE WS-NO              TO WS-END-OF-REQUEST-IND.

           MOVE SPACES             TO WS-PREV-REC-TYPE-X
                                      WS-CURR-REC-TYPE-X
                                      WS-SAVED-SCCF-SER-NUM.

      ****************************************************************
       X300-INIT-TBL-ENT-CNTS.
      ****************************************************************

           MOVE ZEROES   TO CIACI-TBL-ENTRY-CNT
                            CIACP-TBL-ENTRY-CNT
                            CIACO-TBL-ENTRY-CNT
                            CIADR-TBL-ENTRY-CNT
                            CIAAJR-TBL-ENTRY-CNT
                            CIACIP-TBL-ENTRY-CNT
                            CIACOP-TBL-ENTRY-CNT.

      *R17.0   PD_112790 DJL REORGANIZE THE FOLLOWING PARAGRAPHS
      *                      TO ELIMINATE DUPLICATE LOGIC AND TO
      *                      CLARIFY PROCESSING FOR ERRORS.
      *                      PREVIOUS STRUCTURE WOULD NOT HAVE
      *                      BEEN ABLE TO SUPPORT THE LOGIC NEEDED
      *                      TO RESOLVE THIS DEFECT.

      ****************************************************************
       X400-CICS-COMMIT.
      ****************************************************************

           INITIALIZE CSAPI-SRVC-HEADER
                      CSAPI-SRVC-RETURN-AREA .
           SET CSAPI-BATCH-COMMIT       TO TRUE.
           PERFORM Y100-CICS-CKPT-ROLB.

      ****************************************************************
       X500-CICS-ROLLBACK.
      ****************************************************************

           INITIALIZE CSAPI-SRVC-HEADER
                      CSAPI-SRVC-RETURN-AREA .
           SET CSAPI-BATCH-ROLLBACK     TO TRUE.
           PERFORM Y100-CICS-CKPT-ROLB.

      ****************************************************************
       Y100-CICS-CKPT-ROLB.
      ****************************************************************

           MOVE CWSNM-IUCP2002-NAME     TO CSAPI-CALLING-PGM-ID
           MOVE +1                      TO CSAPI-SUB-COMMAND
           MOVE WS-ITDUTLS1             TO WS-CALL-PROGRAM

           CALL WS-CALL-PROGRAM USING CSAPI-API-AREA
                                      UTL-DATA-AREA
           END-CALL.

      *R14.5   E_34072  DJL BEGIN
      ****************************************************************
       Z100-KERNEL-UNSUCCESS.
      ****************************************************************

           DISPLAY ' '.
           DISPLAY 'UNSUCCESSFUL KERNEL STATUS: '.
           DISPLAY '     SCCF SERIAL NUM : '  CIACI-SCCF-SER-NUM.
           DISPLAY ' '.
           DISPLAY ERROR-EXCEPTION-MSG CIACO-CLM-RSN-CD (1)
                                   ' ' CIACO-CLM-RSN-CD (2)
                                   ' ' CIACO-CLM-RSN-CD (3)
                                   ' ' CIACO-CLM-RSN-CD (4)
                                   ' ' CIACO-CLM-RSN-CD (5).
      *R17.0   PD_112790 DJL BEGIN
           IF  CIACO-CLM-RSN-CD (1) = SPACES
           AND CIACO-CLM-RSN-CD (2) = SPACES
           AND CIACO-CLM-RSN-CD (3) = SPACES
           AND CIACO-CLM-RSN-CD (4) = SPACES
           AND CIACO-CLM-RSN-CD (5) = SPACES
               PERFORM VARYING SUB FROM 1 BY 1
                         UNTIL SUB > CIACO-TBL-ENTRY-CNT
                   DISPLAY '       LINE MSG CODES: '
                                           CIACO-LN-RSN-CD (SUB, 1)
                                       ' ' CIACO-LN-RSN-CD (SUB, 2)
                                       ' ' CIACO-LN-RSN-CD (SUB, 3)
                   IF CIACO-LN-RSN-CD (SUB, 1) NOT = SPACES
                   OR CIACO-LN-RSN-CD (SUB, 2) NOT = SPACES
                   OR CIACO-LN-RSN-CD (SUB, 3) NOT = SPACES
                       MOVE WS-MAX-1098 TO SUB
                   END-IF
               END-PERFORM
           END-IF.
           DISPLAY CLAIM-STATUS-CD-MSG CIACO-CLM-STAT-CD.
           DISPLAY CLAIM-PRIMACY-MSG   CIAAJR-PRIM-IND.
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.
      *R14.5   E_34072  DJL END

      ****************************************************************
       Z200-INVLD-ORDER-DUPS.
      ****************************************************************

           DISPLAY ' '.
           DISPLAY WS-AID-COMM-REC-ERROR-MSG-2.
           EVALUATE TRUE
             WHEN WS-SAVED-SCCF-SER-NUM > SPACES
               DISPLAY '     SCCF SERIAL NUM : '  WS-SAVED-SCCF-SER-NUM
             WHEN CIACI-SCCF-SER-NUM > SPACES
               DISPLAY '     SCCF SERIAL NUM : '  CIACI-SCCF-SER-NUM
             WHEN CIACP-SCCF-SER-NUM > SPACES
               DISPLAY '     SCCF SERIAL NUM : '  CIACP-SCCF-SER-NUM
             WHEN CIAAJR-SCCF-SER-NUM > SPACES
               DISPLAY '     SCCF SERIAL NUM : '  CIAAJR-SCCF-SER-NUM
             WHEN OTHER
               DISPLAY '     SCCF SERIAL NUM : NON-NUMERIC'
           END-EVALUATE.
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.

      ****************************************************************
       Z300-ABORT.
      ****************************************************************

           MOVE WS-RET-CODE-3600 TO RETURN-CODE.

           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.
           DISPLAY RETURN-IND-MSG     SIAPC-RETURN-IND.
           DISPLAY RETURN-STATUS-MSG  SIAPC-RETURN-STATUS.
           DISPLAY ERROR-MESSAGE-MSG  SIAPC-ERROR-PCB.
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.

      ****************************************************************
       Z400-INVLD-SEQ-MAX-TBL.
      ****************************************************************

           DISPLAY ' '.
           DISPLAY WS-INVLD-REC-SEQ-MAX-NUM-MSG.
           DISPLAY '     SCCF SERIAL NUM : '  CDB100-SCCF-SER-NUM.
           DISPLAY '         RECORD TYPE : '  CDB100-ID-IND.
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.

      ****************************************************************
       Z500-INVLD-RECORD-TYPE.
      ****************************************************************

           DISPLAY ' '.
           DISPLAY WS-AID-COMM-REC-ERROR-MSG-1.
           EVALUATE TRUE
             WHEN CDB100-SCCF-SER-NUM > SPACES
               DISPLAY '     SCCF SERIAL NUM : '  CDB100-SCCF-SER-NUM
             WHEN OTHER
               DISPLAY '     SCCF SERIAL NUM : NON-NUMERIC'
           END-EVALUATE.
           DISPLAY '     RECORD TYPE     : '  CDB100-ID-IND.
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.
           DISPLAY ' '.

      ****************************************************************
       Z800-DISPLAY-MODULE-NAMES.
      ****************************************************************

           DISPLAY WS-SIAPC-MODULE-NAME-1  SIAPC-MODULE-NAME (1).
           DISPLAY WS-SIAPC-MODULE-NAME-2  SIAPC-MODULE-NAME (2).
           DISPLAY WS-SIAPC-MODULE-NAME-3  SIAPC-MODULE-NAME (3).
           DISPLAY WS-SIAPC-MODULE-NAME-4  SIAPC-MODULE-NAME (4).
           DISPLAY WS-SIAPC-MODULE-NAME-5  SIAPC-MODULE-NAME (5).
           DISPLAY WS-SIAPC-MODULE-NAME-6  SIAPC-MODULE-NAME (6).
           DISPLAY WS-SIAPC-MODULE-NAME-7  SIAPC-MODULE-NAME (7).
           DISPLAY WS-SIAPC-MODULE-NAME-8  SIAPC-MODULE-NAME (8).
           DISPLAY WS-SIAPC-MODULE-NAME-9  SIAPC-MODULE-NAME (9).
           DISPLAY WS-SIAPC-MODULE-NAME-10 SIAPC-MODULE-NAME (10).
           DISPLAY ' '.
           DISPLAY WS-ASTERISKS.

      ****************************************************************
       Z900-DISPLAY-REC-CNTS.
      ****************************************************************

      *R17.0   PD_112790 DJL BEGIN COMMENTS AND DISABLE LOGIC
      *    SINCE FILE-ERROR IS ALWAYS SET EVEN WHEN AID-FILE-ERROR
      *    IS SET, A 3600 WILL ALWAYS BE OVERWRITTEN BELOW
      *    IF AID-FILE-ERROR
      *        MOVE WS-RET-CODE-3600 TO RETURN-CODE.
      *R17.0   PD_112790 DJL END   COMMENTS AND DISABLE LOGIC

           IF FILE-ERROR
      *R17.0   PD_112790 DJL ADD ONE LINE BELOW
           OR WS-TYPE-UNK-REC-CNT > ZEROES
               MOVE WS-RET-CODE-3500 TO RETURN-CODE.

      *R17.0   PD_112790 DJL ADD COMMENTS
      *    AN EMPTY FILE SHOULD PRODUCE A 3400, BUT THAT WOULD
      *    REQUIRE CHANGES TO PLAN JCL WHICH IS OUT OF SCOPE FOR
      *    THIS TICKET.
           IF WS-RECORD-CNT = ZEROES
               MOVE WS-RET-CODE-3500 TO RETURN-CODE.

           DISPLAY ' '.
      *R14.5   E_34072  DJL ADD 'INPUT' TO THE DISPLAY
           DISPLAY '***** IUCP2002 INPUT RECORD COUNTS *****'.
           DISPLAY 'TYPE 1 RECORDS:  ' WS-TYPE-1-REC-CNT.
           DISPLAY 'TYPE 2 RECORDS:  ' WS-TYPE-2-REC-CNT.
           DISPLAY 'TYPE 3 RECORDS:  ' WS-TYPE-3-REC-CNT.
           DISPLAY 'TYPE 4 RECORDS:  ' WS-TYPE-4-REC-CNT.
           DISPLAY 'TYPE 5 RECORDS:  ' WS-TYPE-5-REC-CNT.
      *R17.0   PD_112790 DJL BEGIN
           IF WS-TYPE-UNK-REC-CNT > ZEROES
               DISPLAY ' '
               DISPLAY 'UNKNOWN TYPES :  ' WS-TYPE-UNK-REC-CNT
           END-IF.
      *R17.0   PD_112790 DJL END
           DISPLAY ' '.
           DISPLAY ' TOTAL RECORDS:  ' WS-RECORD-CNT.
           DISPLAY WS-ASTERISKS.

      ****************************************************************
       9999-GOBACK.
      ****************************************************************

           DISPLAY ' '.
           DISPLAY 'RETURN-CODE = ' RETURN-CODE.

           GOBACK.

