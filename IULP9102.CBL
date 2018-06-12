       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     IULP9102.
       AUTHOR.                         IBS.
       DATE-WRITTEN.                   MARCH 1996.
       DATE-COMPILED.
      ************ EHIMS ***********************************************
      *  COMMENT OUT PCK10 PCK24   SEARCH FOR EHIMS                    *
      ************ EHIMS ***********************************************
      ******************************************************************
      *                                                                *
000800*                INTER-PLAN TELEPROCESSING SERVICES (ITS)        *
      *                                                                *
      *   MODULE:       IULP9102                                       *
      *   NAME:         ALIM PROCESSING DRIVER MODULE                  *
001200*   VERSION:      10.7                                           *
001300*   DATE:         11/09/2017                                     *
      *                                                                *
      *   APPLICATION:  AID LINE ITEM MANAGER (ALIM)                   *
      *   TYPE:         COMMON                                         *
      *   FUNCTION:     THIS MODULE ACTS AS A DRIVER FOR THE ALIM      *
      *                 PROCESSING.                                    *
      *                                                                *
      *   MODIFICATION                                                 *
      *   HISTORY:      03/15/96 01.0  - INITIAL RELEASE               *
      *                 05/31/96 02.0  - PROGRAM CHANGED TO CALL       *
      *                                  ITCP1174 IN CICS TO SET DATE  *
      *                                  AND TIME IN ITSIAPC.          *
      *                                - SFCP/SFCO TBL-ENTRY-CNT       *
      *                                  MUST BE NUMERIC               *
      *                                - ACCEPT VERB REMOVED           *
      *                                  FOR THE CALL TO IULP9346      *
      *                                  PASS ALL PCB'S                *
      *                                - C100-SET-SCCF-NUMBERS         *
      *                                  SFLM,SFCP,SFCO SHOULD NOT     *
      *                                  BE MODIFIED                   *
      *                  10/28/96 03.0 - SUBSEQUENT BEN LEV CHRCS CALC.*
      *                                  LOG 225                       *
      *                  01/17/97 04.0 - CONVERSION TO VERSION 2.0     *
      *                                  (ITS REL 8.0)                 *
      *                                - NEW DB/FILE ITCPCQ00          *
      *                                                                *
003900*                  03/06/00 05.0 - POSITION(LAST LINE PROC) CALL *00390001
004000*                                  B200-                         *00400001
004100*                                - IF PC.FUNC/SUBFUNC=L1/LB      *00410001
004200*                                  AND WCDB.DUP THEN ISSUE L8    *00420001
004300*                                  TO POPULATE WC AREAS          *00430001
004400*                                  B400-                         *00440001
004400*                  10/22/01 06.0 - ITS REL. 8.4, HPA REL. 5.6    *00440002
004500*                                - COB FUNCTIONALITY             *00450002
004600*                                - ADDED ITCIACIP, ITCIACOP      *00460002
004700*                                - A400-                         *00470002
004800*                                  DO PRE AS WELL AS POST PROCESS*00480002
004900*                                  CALL TO IULP9346(WILL RESULT  *00490002
005000*                                  IN CORRECT PC.MAJ/SUB-FUNC    *00500002
005100*                                  VALUES ON ALIM RECON)         *00510002
      *                  08/26/03 07.0  - MEDICARE OVERRIDE            *
      *                                 - ADD P2AJR, P2LM              *
      *                                 - MOD B200-                    *
005600*                  06/30/04 08.0  - NEW ITS AEA DB/FILE(ITCPCD09)*00560000
005700*                  08/20/04 09.0  - ITS AEA DB/FILE(ITCPCD09)    *00570000
005800*                                   PLACE BEFORE K00             *00580000
005900*                  11/16/07 10.0  - ITS-TM-2007-11-R10.1         *
006000*                                   IBS LINE EXPANSION           *
      *                                                                *00020003
      ******************************************************************00010003
      * REL   - DATE       - VERS - CONTROL  - DESCRIPTION             *00030003
      *R11.1  ¦ 05/13/2010 ¦ 10.1 ¦ E_47079  ¦ DJL: REMOVE COPYBOOK    *00040003
      *R11.2  ¦ 11/11/2010 ¦ 10.2 ¦ E_32688  ¦ DJL: 5010 CHANGES       *00040003
      *         REMOVE OBSOLETE PROGRAM NAMES; ADD VERSION DISPLAY     *00050003
      *R12.0  ¦ 05/17/2012 ¦ 10.3 ¦ E_41233  ¦ DJL: MULTIPLE INCLUSIVE *00040014
      *         GROUPINGS - CORRECT MODULE NAME INITIALIZATION.        *00050013
      *R14.0  ¦ 11/07/2013 ¦ 10.4 ¦ E_72114  ¦ KJO: PROVIDER PENALTIES *
      *         - ALSO INCLUDES E_27465.  ADD ITCIAAI.                 *
      *R15.0  ¦ 11/06/2014 ¦ 10.5 ¦ PD_96126 ¦ DJL: SET THE VALUE OF   *00050003
      *         THE SYSTEM CREATOR CODE TO ALIM IF IT IS NOT ALREADY   *00050003
      *         SET TO A NON-BLANK VALUE.                              *00050003
      *R15.5  ¦ 05/14/2015 ¦ 10.6 ¦ E_95770  ¦ RDW: INIT CIAAI TO      *00050003
      *         SUPPORT ZERO PRICING.                                  *00050003
      *R18.0  ¦ 11/09/2017 ¦ 10.7 ¦ E_115045 ¦ DJL: REMOVE UNUSED WS   *00050003
      *         AND ENSURE COPY STATEMENTS ARE ON THEIR OWN LINES.     *
      ******************************************************************
006500
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.
007700
      *******   W O R K I N G - S T O R A G E   S E C T I O N   *******
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      *
      *05/13/2010 E_47079  BZXDJL1                                      00010002
       01  WS-PROGRAM-INFO.                                             00020000
           05  WS-MODULE-ID               PIC X(8)  VALUE 'IULP9102'.   00030002
           05  WS-PROGRAM-ID              REDEFINES WS-MODULE-ID        00040001
                                          PIC X(8).                     00050001
           05  WS-VERSION-NUMBER          PIC X(4)  VALUE '10.7'.       00060002
           05  WS-RELEASE-DATE            PIC X(8)  VALUE '11/09/17'.   00070002
           05  WS-PROGRAM-DISPLAY.                                      00080000
               10  WS-COMMON-DISPLAY      PIC X(30) VALUE               00090000
                   'IULP9102-ALIM PROCESSING DRVR'.                     00100002
               10  WS-MODULE-DISPLAY      PIC X(8)  VALUE ' MODULE '.   00110000
               10  WS-RELEASE-DISPLAY     PIC X(34) VALUE               00120000
                   'VERSION #10.7 RELEASED ON 11/09/17'.                00130002
           05  WS-DESCRIPTION             REDEFINES WS-PROGRAM-DISPLAY  00140001
                                          PIC X(72).                    00150001
      *05/13/2010 E_47079  BZXDJL1                                      00160000
      *11.2   E_32688  BZXDJL1 BEGIN                                    00160000
       01  WS-FIRST-TIME-IND              PIC X(01) VALUE 'Y'.
           88  WS-FIRST-TIME                        VALUE 'Y'.
           88  WS-SUBSEQUENT-CALLS                  VALUE 'N'.
      *11.2   E_32688  BZXDJL1 END                                      00160000

       01  WS-REQUESTED-MODULE              PIC X(08) VALUE SPACES.

       01  WS-SIAPC-ERR-MSG-AREA.
           05  WS-SIAPC-ERR-MSG-CD          PIC X(05) OCCURS 15 TIMES.
           05  FILLER                       PIC X(01).

       01  WS-SAVED-AREA.
      *18.0   E_115045  DJL REMOVE UNUSED FIELDS HERE
           05  SAVED-SIAPC-MOD-LVL   COMP   PIC S9(4) VALUE +0.

       01  WS-SIAPC-FUNCTION-CODES          PIC X(4) VALUE SPACES.

      *
      *18.0   E_115045  DJL REMOVE UNUSED FIELDS HERE

       01  WS-CURRENT-DT-YYYYMMDD   VALUE ZERO.
           02 WS-CURRENT-CC                PIC X(02).
           02 WS-CURRENT-DT.
              05  WS-CURRENT-YY            PIC X(02).
              05  WS-CURRENT-MM            PIC X(02).
              05  WS-CURRENT-DD            PIC X(02).
       01  WS-CURRENT-TIME VALUE ZERO.
           05  WS-CURRENT-HOUR              PIC X(02).
           05  WS-CURRENT-MINUTE            PIC X(02).
           05  WS-CURRENT-SECOND            PIC X(02).
      *
      *18.0   E_115045  DJL REMOVE UNUSED CONSTANTS BELOW
       01  WS-CONSTANT-AREA.
           05  WS-20X                       PIC X(02) VALUE '20'.
           05  WS-19X                       PIC X(02) VALUE '19'.
           05  WS-1-X                       PIC X(01) VALUE '1'.
           05  WS-1                         PIC S9(4) COMP VALUE +1.
           05  WS-10                        PIC S9(4) COMP VALUE +10.
           05  CLM-ERR-MAX-FIVE       COMP  PIC S9(4) VALUE +5.
015100     05  MAX-TBL-ENTRY-CNT      COMP  PIC S9(4) VALUE +999.
           05  WS-PLUS-ONE            COMP  PIC S9(4) VALUE +1.
           05  WS-END-OF-TBL                PIC S9(4) VALUE +11.

      *
      *18.0   E_115045  DJL REMOVE UNUSED FIELDS HERE
      *
016200 01  WS-T-ITSIAPC               PIC X(1000)   VALUE SPACE.        01620001
016300 01  WS-T-IULIALM               PIC X(1000)   VALUE SPACE.        01630001
      *11.2   E_32688  BZXDJL1                                          00160000
      *18.0   E_115045  DJL CHANGE FROM 277160 TO THE CORRECT MAXIMUM   00160000
018000 01  WS-T-ITCIACO               PIC X(383362) VALUE SPACE.
017200 01  WS-LIALM-WRITE-RECON-CD    PIC X VALUE SPACE.                01720002
017300 01  WS-LIALM-WORK-CLM-DB       PIC X VALUE SPACE.                01730002
018300
      *18.0   E_115045  DJL REMOVE UNUSED COPYBOOKS HERE
018600
       01  IUCWSCC.
           COPY IUCWSCC.
018900
       01  IULWSNM.
           COPY IULWSNM.
019700
       01  WS-ITSIAPC.
           COPY ITSIAPC.
020000
       01  IULWSEC.
           COPY IULWSEC.
020300
020400 01  SFLM.
           COPY IULIALM.

020500 01  SFAJR.
           COPY IULIASJR.
022300
020600 01  SFCP.
           COPY ITCIACP REPLACING CIACP-TBL-ENTRY-CNT BY
020700                            SFCP-TBL-ENTRY-CNT.
022300
020800 01  SFCO.
           COPY ITCIACO REPLACING CIACO-TBL-ENTRY-CNT BY
020900                            SFCO-TBL-ENTRY-CNT.

021000 01  ADJLM.
           COPY IULIAALM.

021100 01  ADJAJR.
           COPY IULIAAJR.

021200 01  ADJCO.
           COPY IULIACO.
021300
021400 01  CALCS.
           COPY IULIACS.

021500 01  CALREP.
           COPY IULIAREP.

021600 01  ADJCI.
           COPY IULIACI.

021700 01  C2CIP.
           COPY IULIACIP.

021800 01  C2COZ.
           COPY IULIACOZ.

021900 01  C2AJR.
           COPY IULIA2JR.

022000 01  C2LM.
           COPY IULIA2LM.

022100 01  P2AJR.
           COPY IULIAPJR.

022200 01  P2LM.
           COPY IULIAPLM.
022600
      *11.2   E_32688  BZXDJL1 BEGIN                                    00160000
       01  ITCCSAPI.
           COPY ITCCSAPI.
      *11.2   E_32688  BZXDJL1 END                                      00160000
022300
      *R14.0  E_72114  RDW     BEGIN                                    00160000
       01  ITCIAAI.
           COPY ITCIAAI.
      *R14.0  E_72114  RDW     END                                      00160000
022300
022400 01  WS-DUMMY-EIB      PIC X(01).
022300
022500 01  WS-DUMMY-COMMAREA PIC X(01).
022300
       01  FILLER-1                          PIC X(60) VALUE
           '***  IULP9102 WORKING-STORAGE ENDS HERE  ***'.
022900
      **************    L I N K A G E   S E C T I O N    **************
      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*
023400
       01  ITSIAPC.
           COPY ITSIAPC.
023700
       01  IULIALM.
           COPY IULIALM.
024000
       01  ITCIACI.
           COPY ITCIACI.
024300
       01  IUCIAAJR.
           COPY IUCIAAJR.
024600
       01  ITCIACP.
           COPY ITCIACP.
024900
024400 01  ITCIACIP.                                                    02440002
024500     COPY ITCIACIP.                                               02450002
025200
       01  ITCIACO.
           COPY ITCIACO.
025500
025200 01  ITCIACOP.                                                    02520002
025300     COPY ITCIACOP.                                               02530002
025800
       01  IUCIADR.
           COPY IUCIADR.
026100
       01  IUCIAPF.
           COPY IUCIAPF.

       01  IULIACSP.
           COPY IULIACSP.
027000
       01  IULIACOP.
           COPY IULIACOP.

       01  IULPCI00.
           COPY IULPCI00.
027300
       01  IULPCF00.
           COPY IULPCF00.
027600
       01  IUCPCD00.
           COPY IUCPCD00.
027900
       01  IUCPCF00.
           COPY IUCPCF00.
028200
       01  ITCPC600.
           COPY ITCPC600.
028500
       01  ITCPCF00.
           COPY ITCPCF00.
028800
       01  ITCPCE00.
           COPY ITCPCE00.
028800
       01  ITCPCP00.
            COPY ITCPCP00.
028800
       01  ITCPCQ00.
            COPY ITCPCQ00.
028800
029300 01  ITCPCD09.                                                    02930000
029400      COPY ITCPCD09.                                              02940000
028800
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
      ********* EHIMS **************************************************
      *01  ITCPCK10.
      *     COPY ITCPCK10.
      *01  ITCPCK11.
      *     COPY ITCPCK11.
      *01  ITCPCK12.
      *     COPY ITCPCK12.
      *01  ITCPCK13.
      *     COPY ITCPCK13.
      *01  ITCPCK14.
      *     COPY ITCPCK14.
      *01  ITCPCK15.
      *     COPY ITCPCK15.
      *01  ITCPCK16.
      *     COPY ITCPCK16.
      *01  ITCPCK17.
      *     COPY ITCPCK17.
      *01  ITCPCK18.
      *     COPY ITCPCK18.
      *01  ITCPCK19.
      *     COPY ITCPCK19.
      *01  ITCPCK20.
      *     COPY ITCPCK20.
      *01  ITCPCK21.
      *     COPY ITCPCK21.
      *01  ITCPCK22.
      *     COPY ITCPCK22.
      *01  ITCPCK23.
      *     COPY ITCPCK23.
      *01  ITCPCK24.
      *     COPY ITCPCK24.
      ********* EHIMS **************************************************
034700
      ******************************************************************
      ******************************************************************
      *    P R O C E D U R E    D I V I S I O N                        *
      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION USING ITSIAPC
                                IULIALM
                                ITCIACI
                                IUCIAAJR
                                ITCIACP
035400                          ITCIACIP                                03540002
                                ITCIACO
035600                          ITCIACOP                                03560002
                                IUCIADR
                                IUCIAPF
                                IULIACSP
                                IULIACOP
                                IULPCI00
                                IULPCF00
                                IUCPCD00
                                IUCPCF00
                                ITCPC600
                                ITCPCF00
                                ITCPCE00
                               ITCPCP00
                               ITCPCQ00
037200                         ITCPCD09                                 03720000
                               ITCPCK00
                               ITCPCK01
                               ITCPCK02
                               ITCPCK03
                               ITCPCK04
                               ITCPCK05
                               ITCPCK06
                               ITCPCK07
                               ITCPCK08
      ********* EHIMS **************************************************
                               ITCPCK09.
      *                        ITCPCK10
      *                        ITCPCK11
      *                        ITCPCK12
      *                        ITCPCK13
      *                        ITCPCK14
      *                        ITCPCK15
      *                        ITCPCK16
      *                        ITCPCK17
      *                        ITCPCK18
      *                        ITCPCK19
      *                        ITCPCK20
      *                        ITCPCK21
      *                        ITCPCK22
      *                        ITCPCK23
      *                        ITCPCK24.
      ********* EHIMS **************************************************
040000
040100
      ******************************************************************
       0000-MAINLINE.
      ******************************************************************
040100
      *11.2   E_32688  BZXDJL1 BEGIN                                    00160000
           IF LIALM-BATCH-VSAM OF IULIALM
               MOVE SPACES       TO ITCCSAPI
               SET CSAPI-BATCH-ENV TO TRUE
               MOVE ZEROS        TO CSAPI-SUB-COMMAND
               SET CSAPI-SUCCESSFUL TO TRUE
           END-IF.
           COPY ITVERDSP.
      *11.2   E_32688  BZXDJL1 END                                      00160000
040100
           PERFORM A100-INITIALIZATION THRU A100-EXIT.

           IF LIALM-BATCH-VSAM OF IULIALM
               IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                  CWSCC-OPEN-FILES-REQUEST
                   PERFORM A200-OPEN-FILES THRU A200-EXIT
               ELSE
               IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                  CWSCC-CLOSE-FILES-REQUEST
                   PERFORM A300-CLOSE-FILES THRU A300-EXIT.

           PERFORM A400-CALL-FIRST-LEVEL-EDIT THRU A400-EXIT.

           PERFORM A500-PROCESS-VALID THRU A500-EXIT.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       0000-EXIT.
           EXIT.

042500
      ******************************************************************
       A100-INITIALIZATION.
      ******************************************************************
042900
      *R15.5   E_95770 RDW SPACE OUT CIAAI TO SUPPORT ZERO PRICING
               MOVE SPACES TO CIAAI-DATA.
042900
      *R15.0   PD_96126 DJL BEGIN
           IF CIAAJR-SYS-CREATOR-CD <= SPACES
               SET CIAAJR-SYS-CREATOR-ALIM TO TRUE
           END-IF.
      *R15.0   PD_96126 DJL END

043000* LINE EXPANSION - CHANGE NOT NUMERIC TO < 0 OR > 99999
043100     IF SFCP-TBL-ENTRY-CNT     <  0 OR  >  99999
              MOVE ZERO TO SFCP-TBL-ENTRY-CNT.
043300     IF SFCO-TBL-ENTRY-CNT     <  0 OR  >  99999
              MOVE ZERO TO SFCO-TBL-ENTRY-CNT.

           IF SIAPC-GREG-CURRENT-DT    OF ITSIAPC NOT NUMERIC
           OR SIAPC-GREG-CURRENT-DT    OF ITSIAPC = ZERO
               IF LIALM-IMS OF IULIALM
               OR LIALM-BATCH-VSAM OF IULIALM
                   ACCEPT WS-CURRENT-TIME FROM TIME
                   ACCEPT WS-CURRENT-DT   FROM DATE
                   IF WS-CURRENT-YY < 96
                      MOVE WS-20X TO WS-CURRENT-CC
                   ELSE
                      MOVE WS-19X TO WS-CURRENT-CC
               ELSE
                   PERFORM J010-SETUP-FOR-CALL-ITCP1174 THRU J010-EXIT
                   IF SIAPC-RETURN-IND OF ITSIAPC NOT = CWSCC-SUCCESSFUL
                       PERFORM 9999-GOBACK THRU 9999-EXIT
                   ELSE
                       PERFORM X900-MOVE-DT-TIME   THRU X900-EXIT.

           PERFORM  X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

           IF SIAPC-GREG-CURRENT-DT    OF ITSIAPC NOT NUMERIC
           OR SIAPC-GREG-CURRENT-DT    OF ITSIAPC = ZERO
               IF LIALM-IMS OF IULIALM
               OR LIALM-BATCH-VSAM OF IULIALM
                   PERFORM  X200-INIT-SIAPC-DT-TIME   THRU X200-EXIT.

           PERFORM  X300-INIT-SIAPC-CALL-MOD-NAME THRU X300-EXIT.

      *18.0   E_115045  DJL REMOVE ALL FIELDS IN X600, REMOVED PARAGRAPH

       A100-EXIT.
           EXIT.

046800
      ******************************************************************
       A200-OPEN-FILES.
      ******************************************************************

           PERFORM J100-CALL-IULP9346-OPEN-FILES THRU J100-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-ABORT
               PERFORM 9999-GOBACK THRU 9999-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             MOVE SIAPC-RETURN-DATA OF WS-ITSIAPC TO
                                    SIAPC-RETURN-DATA OF ITSIAPC
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

           PERFORM B100-PRE-ALIM-PROCESS THRU B100-EXIT.

           PERFORM C500-HPA-CLM-LEVEL-PROCESS   THRU C500-EXIT.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       A200-EXIT.
           EXIT.

049700
      ******************************************************************
       A300-CLOSE-FILES.
      ******************************************************************

           PERFORM B100-PRE-ALIM-PROCESS THRU B100-EXIT.

           PERFORM J200-CALL-IULP9346-CLOSE-FILES THRU J200-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-ABORT
               PERFORM 9999-GOBACK THRU 9999-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             MOVE SIAPC-RETURN-DATA OF WS-ITSIAPC TO
                                    SIAPC-RETURN-DATA OF ITSIAPC
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

           PERFORM C500-HPA-CLM-LEVEL-PROCESS   THRU C500-EXIT.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       A300-EXIT.
           EXIT.

052600
      ******************************************************************
       A400-CALL-FIRST-LEVEL-EDIT.
      ******************************************************************
052700     MOVE    LIALM-WRITE-RECON-CD OF IULIALM                      05270002
052800       TO WS-LIALM-WRITE-RECON-CD.                                05280002
052900     MOVE    LIALM-WORK-CLM-DB    OF IULIALM                      05290002
053000       TO WS-LIALM-WORK-CLM-DB.                                   05300002
053100     SET LIALM-NO-RECON OF IULIALM TO TRUE.                       05310002
053200     SET LIALM-WC-NO    OF IULIALM TO TRUE.                       05320002

053400     PERFORM B100-PRE-ALIM-PROCESS THRU B100-EXIT.                05340002
053500                                                                  05350002
053600     MOVE WS-LIALM-WRITE-RECON-CD                                 05360002
053700       TO    LIALM-WRITE-RECON-CD OF IULIALM.                     05370002
053800     MOVE WS-LIALM-WORK-CLM-DB                                    05380002
053900       TO    LIALM-WORK-CLM-DB OF IULIALM.                        05390002
054100                                                                  05410002
           PERFORM J300-SETUP-IULP9112 THRU J300-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       A400-EXIT.
           EXIT.

055800
      ******************************************************************
       A500-PROCESS-VALID.
      ******************************************************************

           PERFORM B100-PRE-ALIM-PROCESS THRU B100-EXIT.

           PERFORM B200-PROCESS-CASE THRU B200-EXIT.

           PERFORM B300-POST-ALIM-PROCESS-SUCC THRU B300-EXIT.

052000     PERFORM B400-POSSIBLE-L8-FUNC       THRU B400-EXIT.          05200001
052100                                                                  05210001
052200                                                                  05220001
       A500-EXIT.
           EXIT.

057500
      ******************************************************************
       B100-PRE-ALIM-PROCESS.
      ******************************************************************

           PERFORM C100-SET-SCCF-NUMBERS THRU C100-EXIT.

           PERFORM J400-SETUP-IULP9346-PRE-ALIM THRU J400-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-ABORT
               PERFORM 9999-GOBACK THRU 9999-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             MOVE SIAPC-RETURN-DATA OF WS-ITSIAPC TO
                                    SIAPC-RETURN-DATA OF ITSIAPC
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       B100-EXIT.
           EXIT.

060100
      ******************************************************************
       B200-PROCESS-CASE.
      ******************************************************************

           IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
              CWSCC-INIT-FUNCTION
               PERFORM C200-INIT-FUNCTION THRU C200-EXIT
           ELSE
             IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                (CWSCC-LINE-PROCESS OR CWSCC-COB-LINE-FUNCTION
                 OR CWSCC-OVR-LINE-FUNCTION)
               PERFORM C300-LN-PROCESS   THRU C300-EXIT
             ELSE
               IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                  (CWSCC-FINAL-FUNCTION OR CWSCC-COB-FINAL-FUNCTION
                  OR CWSCC-OVR-FINAL-FUNCTION)
                 PERFORM C400-FINAL-FUNCTION THRU C400-EXIT
               ELSE
                 IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                    CWSCC-NORMAL-PROCESSING      OR
                    CWSCC-DELETE-FUNCTION        OR
                    CWSCC-OPEN-FILES-REQUEST     OR
                    CWSCC-CLOSE-FILES-REQUEST
                   PERFORM C500-HPA-CLM-LEVEL-PROCESS   THRU C500-EXIT
               ELSE
                   IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC =
                      CWSCC-WORK-CLAIM-DB-FUNCTION
057800             OR CWSCC-LAST-ADJUD-LINE-PROC                        05780001
                     NEXT SENTENCE
                   ELSE
                     PERFORM Z100-INVALID-FUNCTION THRU Z100-EXIT.

       B200-EXIT.
           EXIT.

063700
      ******************************************************************
       B300-POST-ALIM-PROCESS-SUCC.
      ******************************************************************

           PERFORM J500-SETUP-IULP9346-POST-ALIM THRU J500-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-ABORT
               PERFORM 9999-GOBACK THRU 9999-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             MOVE SIAPC-RETURN-DATA OF WS-ITSIAPC TO
                                    SIAPC-RETURN-DATA OF ITSIAPC
             IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       B300-EXIT.
           EXIT.

066000
      ******************************************************************
061100 B400-POSSIBLE-L8-FUNC.                                           06110001
061200******************************************************************06120001
061300                                                                  06130001
061400     IF SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC = CWSCC-INIT-FUNCTION  06140001
061500     AND SIAPC-FUNCTION-CD-SUB  OF ITSIAPC =                      06150001
061600                                       CWSCC-INIT-FUNCTION-NO-DUP 06160001
061700     AND SIAPC-RETURN-IND OF ITSIAPC NOT = CWSCC-SUCCESSFUL       06170001
061800     AND CIACO-CLM-RSN-CD OF ITCIACO (WS-1) =                     06180001
      *LD001
061900                                       LWSEC-WCDB-DUP-NOT-ALLOWED 06190001
062000        MOVE ITSIAPC TO WS-T-ITSIAPC                              06200001
062100        MOVE IULIALM TO WS-T-IULIALM                              06210001
062200        MOVE ITCIACO TO WS-T-ITCIACO                              06220001
062300        MOVE CWSCC-LAST-ADJUD-LINE-PROC                           06230001
062400          TO SIAPC-FUNCTION-CD-MAJOR OF ITSIAPC                   06240001
062500        MOVE SPACE TO SIAPC-FUNCTION-CD-SUB OF ITSIAPC            06250001
062600        MOVE WS-1-X TO LIALM-DB-ACTION-CD OF IULIALM              06260001
062700        MOVE WS-1-X TO LIALM-WORK-CLM-DB  OF IULIALM              06270001
062800        MOVE SPACE TO CIACO-CALC-RESULTS-CNTL-DATA OF ITCIACO     06280001
062900        MOVE ZERO  TO CIACO-CLM-STAT-CD            OF ITCIACO     06290001
063000                                                                  06300001
063100        PERFORM A400-CALL-FIRST-LEVEL-EDIT THRU A400-EXIT         06310001
063200        PERFORM B100-PRE-ALIM-PROCESS THRU B100-EXIT              06320001
063300        PERFORM B300-POST-ALIM-PROCESS-SUCC THRU B300-EXIT        06330001
063400        MOVE WS-T-ITSIAPC TO ITSIAPC                              06340001
063500        MOVE WS-T-IULIALM TO IULIALM                              06350001
063600        MOVE WS-T-ITCIACO TO ITCIACO.                             06360001
063700                                                                  06370001
063800 B400-EXIT.                                                       06380001
063900     EXIT.                                                        06390001
064000                                                                  06400001
069200
064200******************************************************************06420001
       C100-SET-SCCF-NUMBERS.
      ******************************************************************

           MOVE LIALM-SCCF-SER-NUM    OF IULIALM TO
      *18.0   E_115045  DJL REMOVE UNUSED COPYBOOK FIELDS HERE
                    CIACP-SCCF-SER-NUM    OF ITCIACP
                    CIACO-SCCF-SER-NUM    OF ITCIACO
                    CIACI-SCCF-SER-NUM
070200              CIACIP-SCCF-SER-NUM                                 07020002
070300              CIACOP-SCCF-SER-NUM                                 07030002
                    CIAAJR-SCCF-SER-NUM
                    CIADR-1A-SCCF-SER-NUM
                    CIAPF-SCCF-SER-NUM.

       C100-EXIT.
           EXIT.

071200
      ******************************************************************
       C200-INIT-FUNCTION.
      ******************************************************************

           PERFORM J600-SETUP-IULP9140-INIT THRU J600-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       C200-EXIT.
           EXIT.

073100
      ******************************************************************
       C300-LN-PROCESS.
      ******************************************************************

073600     PERFORM J700-SETUP-IULP9160-LN   THRU J700-EXIT.             07360002

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       C300-EXIT.
           EXIT.

075000
      ******************************************************************
       C400-FINAL-FUNCTION.
      ******************************************************************

           MOVE SIAPC-FUNCTION-CODES OF ITSIAPC TO
                WS-SIAPC-FUNCTION-CODES.

           PERFORM J800-SETUP-IULP9300-FINAL THRU J800-EXIT.

           MOVE WS-SIAPC-FUNCTION-CODES TO
                SIAPC-FUNCTION-CODES OF ITSIAPC.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       C400-EXIT.
           EXIT.

077500
      ******************************************************************
       C500-HPA-CLM-LEVEL-PROCESS.
      ******************************************************************

           PERFORM J900-SETUP-IUCP9102 THRU J900-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             PERFORM D100-POST-ALIM-PROCESS-ERR THRU D100-EXIT
             IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       C500-EXIT.
           EXIT.

079400
      ******************************************************************
       D100-POST-ALIM-PROCESS-ERR.
      ******************************************************************

           PERFORM J500-SETUP-IULP9346-POST-ALIM THRU J500-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-ABORT
               PERFORM 9999-GOBACK THRU 9999-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
             PERFORM F100-CLEAR-SIAPC-MOD-NAME-AREA THRU F100-EXIT
           ELSE
             IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-UNSUCCESSFUL
               PERFORM Z200-UNSUCCESSFUL THRU Z200-EXIT
             ELSE
               PERFORM Z300-SEVERE-LOG THRU Z300-EXIT.

       D100-EXIT.
           EXIT.

081500
      ******************************************************************
       F100-CLEAR-SIAPC-MOD-NAME-AREA.
      ******************************************************************

           MOVE       SIAPC-MODULE-LEVEL OF ITSIAPC   TO
                                SAVED-SIAPC-MOD-LVL.

           ADD        WS-PLUS-ONE                     TO
                          SIAPC-MODULE-LEVEL OF ITSIAPC.

           PERFORM X400-INIT-MOD-NAME-TBL THRU X400-EXIT
                     VARYING SIAPC-MODULE-LEVEL OF ITSIAPC
                       FROM    SIAPC-MODULE-LEVEL OF ITSIAPC BY 1
                         UNTIL   SIAPC-MODULE-LEVEL OF ITSIAPC NOT <
                                 WS-END-OF-TBL.

           MOVE       SAVED-SIAPC-MOD-LVL             TO
                                SIAPC-MODULE-LEVEL OF ITSIAPC.

           MOVE       SIAPC-MODULE-LEVEL OF WS-ITSIAPC TO
                                SAVED-SIAPC-MOD-LVL.

           ADD        WS-PLUS-ONE                     TO
                                SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

           PERFORM X500-INIT-MOD-NAME-TBL THRU X500-EXIT
                     VARYING SIAPC-MODULE-LEVEL OF WS-ITSIAPC
                       FROM    SIAPC-MODULE-LEVEL OF WS-ITSIAPC BY 1
                       UNTIL   SIAPC-MODULE-LEVEL OF WS-ITSIAPC NOT <
                               WS-END-OF-TBL.

           MOVE       SAVED-SIAPC-MOD-LVL             TO
                                SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

       F100-EXIT.
           EXIT.

085300
      ******************************************************************
       J010-SETUP-FOR-CALL-ITCP1174.
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA.

      *05/13/2010 E_47079  BZXDJL1                                      00010002
086000     MOVE       LWSNM-ITCP1174-NAME             TO
                      WS-REQUESTED-MODULE.

           ADD        WS-PLUS-ONE                     TO
                      SIAPC-MODULE-LEVEL OF ITSIAPC.

      *05/13/2010 E_47079  BZXDJL1                                      00010002
086600     MOVE       LWSNM-ITCP1174-NAME             TO
                      SIAPC-MODULE-NAME OF ITSIAPC
                     (SIAPC-MODULE-LEVEL OF ITSIAPC).

           PERFORM    T010-CALL-ITCP1174.

           SUBTRACT   WS-PLUS-ONE                     FROM
                      SIAPC-MODULE-LEVEL OF ITSIAPC.

       J010-EXIT.
           EXIT.

087800
      ******************************************************************
       J100-CALL-IULP9346-OPEN-FILES.
      ******************************************************************

           PERFORM    X150-INIT-WS-ITSIAPC THRU X150-EXIT.

           MOVE       LWSNM-IULP9346-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM X750-INIT-SIAPC-INV-MOD-NAME THRU X750-EXIT.

           PERFORM T400-CALL-IULP9346 THRU T400-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
               SUBTRACT   WS-PLUS-ONE                 FROM
                              SIAPC-MODULE-LEVEL OF ITSIAPC
                              SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

       J100-EXIT.
           EXIT.

090200
      ******************************************************************
       J200-CALL-IULP9346-CLOSE-FILES.
      ******************************************************************

           PERFORM    X150-INIT-WS-ITSIAPC THRU X150-EXIT.

           MOVE       LWSNM-IULP9346-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM X750-INIT-SIAPC-INV-MOD-NAME THRU X750-EXIT.

           PERFORM T400-CALL-IULP9346 THRU T400-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
               SUBTRACT   WS-PLUS-ONE                 FROM
                              SIAPC-MODULE-LEVEL OF ITSIAPC
                              SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

       J200-EXIT.
           EXIT.

092600
      ******************************************************************
       J300-SETUP-IULP9112.
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

           MOVE       LWSNM-IULP9112-NAME             TO
                                     WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM    T300-CALL-IULP9112 THRU T300-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               SUBTRACT   WS-PLUS-ONE                 FROM
                              SIAPC-MODULE-LEVEL OF ITSIAPC.

       J300-EXIT.
           EXIT.

094700
      ******************************************************************
       J400-SETUP-IULP9346-PRE-ALIM.
      ******************************************************************

           PERFORM    X150-INIT-WS-ITSIAPC THRU X150-EXIT.

           MOVE       LWSNM-IULP9346-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM X750-INIT-SIAPC-INV-MOD-NAME THRU X750-EXIT.

           MOVE CWSCC-PRE-ALIM-PROCESS TO
                          SIAPC-FUNCTION-CD-MAJOR OF WS-ITSIAPC.

           PERFORM T400-CALL-IULP9346 THRU T400-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
               SUBTRACT   WS-PLUS-ONE                 FROM
                              SIAPC-MODULE-LEVEL OF ITSIAPC
                              SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

       J400-EXIT.
           EXIT.

097400
      ******************************************************************
       J500-SETUP-IULP9346-POST-ALIM.
      ******************************************************************

           PERFORM    X150-INIT-WS-ITSIAPC THRU X150-EXIT.

           MOVE       LWSNM-IULP9346-NAME             TO
                          WS-REQUESTED-MODULE.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

      *12.0   E_41233  DJL THIS SHOULD BE DONE WHEN X700 IS PERFORMED
           PERFORM X750-INIT-SIAPC-INV-MOD-NAME THRU X750-EXIT.

           MOVE CWSCC-POST-ALIM-PROCESS TO
                          SIAPC-FUNCTION-CD-MAJOR OF WS-ITSIAPC.

           PERFORM T400-CALL-IULP9346 THRU T400-EXIT.

           IF SIAPC-RETURN-IND OF WS-ITSIAPC = CWSCC-SUCCESSFUL
               SUBTRACT   WS-PLUS-ONE                 FROM
                              SIAPC-MODULE-LEVEL OF WS-ITSIAPC
               IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
                   SUBTRACT   WS-PLUS-ONE             FROM
                                  SIAPC-MODULE-LEVEL OF ITSIAPC.

       J500-EXIT.
           EXIT.

100400
      ******************************************************************
       J600-SETUP-IULP9140-INIT.
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

           MOVE       LWSNM-IULP9140-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM T600-CALL-IULP9140 THRU T600-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               IF SIAPC-MODULE-LEVEL OF ITSIAPC > 1
                   SUBTRACT   WS-PLUS-ONE             FROM
                                  SIAPC-MODULE-LEVEL OF ITSIAPC.

       J600-EXIT.
           EXIT.

102600
      ******************************************************************
102800 J700-SETUP-IULP9160-LN.                                          10280002
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

103300     MOVE       LWSNM-IULP9160-NAME             TO                10330002
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

103800     PERFORM T700-CALL-IULP9160 THRU T700-EXIT.                   10380002

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               IF SIAPC-MODULE-LEVEL OF ITSIAPC > 1
                   SUBTRACT   WS-PLUS-ONE             FROM
                                  SIAPC-MODULE-LEVEL OF ITSIAPC.

       J700-EXIT.
           EXIT.

104800
      ******************************************************************
       J800-SETUP-IULP9300-FINAL.
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

           MOVE       LWSNM-IULP9300-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM    T800-CALL-IULP9300 THRU T800-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               IF SIAPC-MODULE-LEVEL OF ITSIAPC > 1
                   SUBTRACT   WS-PLUS-ONE             FROM
                                  SIAPC-MODULE-LEVEL OF ITSIAPC.

       J800-EXIT.
           EXIT.

107000
      ******************************************************************
       J900-SETUP-IUCP9102.
      ******************************************************************

           PERFORM    X100-INIT-SIAPC-RETURN-DATA THRU X100-EXIT.

      *05/13/2010 E_47079  BZXDJL1                                      00010002
107700     MOVE       LWSNM-IUCP9102-NAME             TO
                          WS-REQUESTED-MODULE.

           PERFORM X700-INIT-SIAPC-INV-MOD-NAME THRU X700-EXIT.

           PERFORM    T900-CALL-IUCP9102 THRU T900-EXIT.

           IF SIAPC-RETURN-IND OF ITSIAPC = CWSCC-SUCCESSFUL
               IF SIAPC-MODULE-LEVEL OF ITSIAPC > 1
                   SUBTRACT   WS-PLUS-ONE             FROM
                                  SIAPC-MODULE-LEVEL OF ITSIAPC.

       J900-EXIT.
           EXIT.

109200
      ******************************************************************
       T010-CALL-ITCP1174.
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING WS-DUMMY-EIB
                                          WS-DUMMY-COMMAREA
                                          ITSIAPC.

       T010-EXIT.
           EXIT.

110400
      ******************************************************************
       T300-CALL-IULP9112.
      ******************************************************************
           CALL WS-REQUESTED-MODULE USING ITSIAPC
                                          IULIALM
                                          ITCIACI
                                          IUCIAAJR
                                          ITCIACP
                                          ITCIACO
                                          IUCIADR
                                          SFLM
                                          SFAJR
                                          SFCP
                                          SFCO
                                          ADJLM
                                          ADJAJR
                                          ADJCO
                                          CALCS
112300                                    CALREP                        11230002
112500                                    ITCIACIP                      11250002
112600                                    ITCIACOP                      11260002
112700                                    ADJCI                         11270002
112800                                    C2CIP                         11280002
112900                                    C2COZ                         11290002
113000                                    C2AJR                         11300002
                                          C2LM
                                          P2AJR
                                          P2LM.

       T300-EXIT.
           EXIT.

113700
      ******************************************************************
       T400-CALL-IULP9346.
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING WS-ITSIAPC
                                          ITSIAPC
                                          IULIALM
                                          ITCIACI
                                          IUCIAAJR
                                          ITCIACP
                                          ITCIACO
                                          IUCIADR
                                          IUCIAPF
                                        IULIACSP
                                        IULIACOP
                                          SFLM
                                          SFAJR
                                          SFCP
                                          SFCO
                                          ADJLM
                                          ADJAJR
                                          ADJCO
                                          CALCS
                                          CALREP
116300                                    ITCIACIP                      11630002
116400                                    ITCIACOP                      11640002
116500                                    ADJCI                         11650002
116600                                    C2CIP                         11660002
116700                                    C2COZ                         11670002
116800                                    C2AJR                         11680002
116900                                    C2LM                          11690002
                                          P2AJR
                                          P2LM
                                          IULPCI00
                                          IULPCF00
                                          IUCPCD00
                                          IUCPCF00
                                          ITCPC600
                                          ITCPCF00
                                          ITCPCE00
                                        ITCPCP00
                                        ITCPCQ00
117700                                  ITCPCD09                        11770000
                                        ITCPCK00
                                        ITCPCK01
                                        ITCPCK02
                                        ITCPCK03
                                        ITCPCK04
                                        ITCPCK05
                                        ITCPCK06
                                        ITCPCK07
                                        ITCPCK08
      ********* EHIMS **************************************************
                                        ITCPCK09.
      *                                 ITCPCK10
      *                                 ITCPCK11
      *                                 ITCPCK12
      *                                 ITCPCK13
      *                                 ITCPCK14
      *                                 ITCPCK15
      *                                 ITCPCK16
      *                                 ITCPCK17
      *                                 ITCPCK18
      *                                 ITCPCK19
      *                                 ITCPCK20
      *                                 ITCPCK21
      *                                 ITCPCK22
      *                                 ITCPCK23
      *                                 ITCPCK24.
      ********* EHIMS **************************************************
120600
       T400-EXIT.
           EXIT.

121000
      ******************************************************************
       T600-CALL-IULP9140.
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING ITSIAPC
                                          IULIALM
                                          ITCIACI
                                          IUCIAAJR
                                          ITCIACP
                                          ITCIACO
                                          IUCIADR
                                          IUCIAPF
                                          SFLM
                                          SFAJR
                                          SFCP
                                          SFCO
                                          ADJLM
                                          ADJAJR
                                          ADJCO
                                          CALCS
                                          CALREP
123200                                    ITCIACIP                      12320002
123300                                    ITCIACOP                      12330002
123400                                    ADJCI                         12340002
123500                                    C2CIP                         12350002
123600                                    C2COZ                         12360002
123700                                    C2AJR                         12370002
123800                                    C2LM                          12380002
                                          P2AJR
                                          P2LM
                                          IUCPCD00
                                          IUCPCF00
                                          ITCPC600
                                          ITCPCF00
                                          ITCPCE00
                                        ITCPCP00
                                        ITCPCQ00
124500                                  ITCPCD09                        12450000
                                        ITCPCK00
                                        ITCPCK01
                                        ITCPCK02
                                        ITCPCK03
                                        ITCPCK04
                                        ITCPCK05
                                        ITCPCK06
                                        ITCPCK07
                                        ITCPCK08
125800                                  ITCPCK09
      ********* EHIMS **************************************************
      *                                 ITCPCK10
      *                                 ITCPCK11
      *                                 ITCPCK12
      *                                 ITCPCK13
      *                                 ITCPCK14
      *                                 ITCPCK15
      *                                 ITCPCK16
      *                                 ITCPCK17
      *                                 ITCPCK18
      *                                 ITCPCK19
      *                                 ITCPCK20
      *                                 ITCPCK21
      *                                 ITCPCK22
      *                                 ITCPCK23
127300*                                 ITCPCK24
      ********* EHIMS **************************************************
      *R14.0 E_72114.KJO ADD ITCIAAI
                                        ITCIAAI.
127400
       T600-EXIT.
           EXIT.

127800
      ******************************************************************
127800 T700-CALL-IULP9160.                                              12780002
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING ITSIAPC
                                          IULIALM
                                          ITCIACI
                                          IUCIAAJR
                                          ITCIACP
                                          ITCIACO
                                          IUCIADR
                                          IUCIAPF
                                          SFLM
                                          SFAJR
                                          SFCP
                                          SFCO
                                          ADJLM
                                          ADJAJR
                                          ADJCO
                                          CALCS
                                          CALREP
129900                                    ITCIACIP                      12990002
130000                                    ITCIACOP                      13000002
130100                                    ADJCI                         13010002
130200                                    C2CIP                         13020002
130300                                    C2COZ                         13030002
130400                                    C2AJR                         13040002
130500                                    C2LM                          13050002
                                          P2AJR
                                          P2LM
                                          IUCPCD00
                                          IUCPCF00
                                          ITCPC600
                                          ITCPCF00
                                          ITCPCE00
                                        ITCPCP00
                                        ITCPCQ00
131300                                  ITCPCD09                        13130000
                                        ITCPCK00
                                        ITCPCK01
                                        ITCPCK02
                                        ITCPCK03
                                        ITCPCK04
                                        ITCPCK05
                                        ITCPCK06
                                        ITCPCK07
                                        ITCPCK08
132600                                  ITCPCK09
      ********* EHIMS **************************************************
      *                                 ITCPCK10
      *                                 ITCPCK11
      *                                 ITCPCK12
      *                                 ITCPCK13
      *                                 ITCPCK14
      *                                 ITCPCK15
      *                                 ITCPCK16
      *                                 ITCPCK17
      *                                 ITCPCK18
      *                                 ITCPCK19
      *                                 ITCPCK20
      *                                 ITCPCK21
      *                                 ITCPCK22
      *                                 ITCPCK23
134100*                                 ITCPCK24
      ********* EHIMS **************************************************
      *R14.0 E_72114.KJO ADD ITCIAAI
                                        ITCIAAI.
134200
       T700-EXIT.
           EXIT.

134600
      ******************************************************************
       T800-CALL-IULP9300.
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING ITSIAPC
                                          IULIALM
                                          ITCIACI
                                          IUCIAAJR
                                          ITCIACP
                                          ITCIACO
                                          IUCIADR
                                          IUCIAPF
                                          SFLM
                                          SFAJR
                                          SFCP
                                          SFCO
                                          ADJLM
                                          ADJAJR
                                          ADJCO
                                          CALCS
136500                                    ITCIACIP                      13650002
136600                                    ITCIACOP                      13660002
136700                                    ADJCI                         13670002
136800                                    C2CIP                         13680002
136900                                    C2COZ                         13690002
137000                                    C2AJR                         13700002
137100                                    C2LM                          13710002
                                          P2AJR
                                          P2LM
                                          IUCPCD00
                                          IUCPCF00
                                          ITCPC600
                                          ITCPCF00
                                          ITCPCE00
                                        ITCPCP00
                                        ITCPCQ00
138000                                  ITCPCD09                        13800000
                                        ITCPCK00
                                        ITCPCK01
                                        ITCPCK02
                                        ITCPCK03
                                        ITCPCK04
                                        ITCPCK05
                                        ITCPCK06
                                        ITCPCK07
                                        ITCPCK08
139300                                  ITCPCK09
      ********* EHIMS **************************************************
      *                                 ITCPCK10
      *                                 ITCPCK11
      *                                 ITCPCK12
      *                                 ITCPCK13
      *                                 ITCPCK14
      *                                 ITCPCK15
      *                                 ITCPCK16
      *                                 ITCPCK17
      *                                 ITCPCK18
      *                                 ITCPCK19
      *                                 ITCPCK20
      *                                 ITCPCK21
      *                                 ITCPCK22
      *                                 ITCPCK23
140800*                                 ITCPCK24
      ********* EHIMS **************************************************
      *R14.0 E_72114.KJO ADD ITCIAAI
                                        ITCIAAI.
140900
       T800-EXIT.
           EXIT.

141300
      ******************************************************************
       T900-CALL-IUCP9102.
      ******************************************************************

           CALL WS-REQUESTED-MODULE USING ITSIAPC,
                                          ITCIACI,
                                          ITCIACP,
                                          IUCIAAJR,
141900                                    ITCIACIP,                     14190002
                                          ITCIACO,
142200                                    ITCIACOP,                     14220002
                                          IUCIADR,
                                          IUCIAPF,
                                          IUCPCD00,
                                          IUCPCF00,
                                          ITCPCF00,
                                          ITCPCE00,
                                        ITCPC600,
                                        ITCPCP00,
                                        ITCPCQ00
143100                                  ITCPCD09,                       14310000
                                        ITCPCK00,
                                        ITCPCK01,
                                        ITCPCK02,
                                        ITCPCK03,
                                        ITCPCK04,
                                        ITCPCK05,
                                        ITCPCK06,
                                        ITCPCK07,
                                        ITCPCK08,
144400                                  ITCPCK09,
      ********* EHIMS **************************************************
144500*                                 ITCPCK10,
144600*                                 ITCPCK11,
144700*                                 ITCPCK12,
144800*                                 ITCPCK13,
144900*                                 ITCPCK14,
145000*                                 ITCPCK15,
145100*                                 ITCPCK16,
145200*                                 ITCPCK17,
145300*                                 ITCPCK18,
145400*                                 ITCPCK19,
145500*                                 ITCPCK20,
145600*                                 ITCPCK21,
145700*                                 ITCPCK22,
145800*                                 ITCPCK23,
145900*                                 ITCPCK24,
      ********* EHIMS **************************************************
      *R14.0 E_72114.RDW ADD ITCIAAI
                                        ITCIAAI.
146000
       T900-EXIT.
           EXIT.

146400
      ******************************************************************
       X100-INIT-SIAPC-RETURN-DATA.
      ******************************************************************

           MOVE       SPACES                          TO
                          SIAPC-RETURN-DATA OF ITSIAPC.

           MOVE       CWSCC-SUCCESSFUL                TO
                          SIAPC-RETURN-IND OF ITSIAPC.

           MOVE       CWSCC-ACTION-COMPLETE           TO
                          SIAPC-RETURN-STATUS OF ITSIAPC.

       X100-EXIT.
           EXIT.

148100
      ******************************************************************
       X150-INIT-WS-ITSIAPC.
      ******************************************************************

           MOVE       ITSIAPC                         TO
                      WS-ITSIAPC.

           MOVE       SPACES                          TO
                          SIAPC-RETURN-DATA OF WS-ITSIAPC.

           MOVE       CWSCC-SUCCESSFUL                TO
                          SIAPC-RETURN-IND OF WS-ITSIAPC.

           MOVE       CWSCC-ACTION-COMPLETE           TO
                          SIAPC-RETURN-STATUS OF WS-ITSIAPC.

       X150-EXIT.
           EXIT.

150100
      ******************************************************************
       X200-INIT-SIAPC-DT-TIME.
      ******************************************************************

           IF
             SIAPC-GREG-CURRENT-DT OF ITSIAPC IS NOT NUMERIC OR
            (SIAPC-GREG-CURRENT-DT OF ITSIAPC IS NUMERIC AND
             SIAPC-GREG-CURRENT-DT OF ITSIAPC IS EQUAL TO ZEROES)
               MOVE       WS-CURRENT-CC                   TO
                          SIAPC-DISPLAY-CURRENT-CENTURY OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-CENTURY OF WS-ITSIAPC
               MOVE       WS-CURRENT-MM                   TO
                          SIAPC-DISPLAY-CURRENT-MONTH OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-MONTH OF WS-ITSIAPC
               MOVE       WS-CURRENT-DD                   TO
                          SIAPC-DISPLAY-CURRENT-DAY OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-DAY OF WS-ITSIAPC
               MOVE       WS-CURRENT-YY                   TO
                          SIAPC-DISPLAY-CURRENT-YEAR OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-YEAR OF WS-ITSIAPC
               MOVE       WS-CURRENT-CC                   TO
                          SIAPC-GREG-CURRENT-CENTURY OF ITSIAPC
                          SIAPC-GREG-CURRENT-CENTURY OF WS-ITSIAPC
               MOVE       WS-CURRENT-MM                   TO
                          SIAPC-GREG-CURRENT-MONTH OF ITSIAPC
                          SIAPC-GREG-CURRENT-MONTH OF WS-ITSIAPC
               MOVE       WS-CURRENT-DD                   TO
                          SIAPC-GREG-CURRENT-DAY OF ITSIAPC
                          SIAPC-GREG-CURRENT-DAY OF WS-ITSIAPC
               MOVE       WS-CURRENT-YY                   TO
                          SIAPC-GREG-CURRENT-YEAR OF ITSIAPC
                          SIAPC-GREG-CURRENT-YEAR OF WS-ITSIAPC.

           IF
             SIAPC-CURRENT-TIME OF ITSIAPC IS NOT NUMERIC OR
            (SIAPC-CURRENT-TIME OF ITSIAPC IS NUMERIC AND
             SIAPC-CURRENT-TIME OF ITSIAPC IS EQUAL TO ZEROES)
               MOVE       WS-CURRENT-HOUR                 TO
                          SIAPC-CURRENT-HOUR OF ITSIAPC
                          SIAPC-CURRENT-HOUR OF WS-ITSIAPC
               MOVE       WS-CURRENT-MINUTE               TO
                          SIAPC-CURRENT-MINUTE OF ITSIAPC
                          SIAPC-CURRENT-MINUTE OF WS-ITSIAPC
               MOVE       WS-CURRENT-SECOND               TO
                          SIAPC-CURRENT-SECOND OF ITSIAPC
                          SIAPC-CURRENT-SECOND OF WS-ITSIAPC
               MOVE       WS-CURRENT-HOUR                 TO
                          SIAPC-DISPLAY-CURRENT-HOUR OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-HOUR OF WS-ITSIAPC
               MOVE       WS-CURRENT-MINUTE               TO
                          SIAPC-DISPLAY-CURRENT-MINUTE OF ITSIAPC
                          SIAPC-DISPLAY-CURRENT-MINUTE OF WS-ITSIAPC.

       X200-EXIT.
           EXIT.
155700
      ******************************************************************
       X300-INIT-SIAPC-CALL-MOD-NAME.
      ******************************************************************

           MOVE WS-1 TO SIAPC-MODULE-LEVEL OF ITSIAPC.
           MOVE LWSNM-IULP9102-NAME TO
                    SIAPC-MODULE-NAME OF ITSIAPC
                   (SIAPC-MODULE-LEVEL OF ITSIAPC).

           MOVE WS-1 TO SIAPC-MODULE-LEVEL OF WS-ITSIAPC.
           MOVE LWSNM-IULP9102-NAME TO
                    SIAPC-MODULE-NAME OF WS-ITSIAPC
                   (SIAPC-MODULE-LEVEL OF WS-ITSIAPC).

       X300-EXIT.
           EXIT.

157500
      ******************************************************************
       X400-INIT-MOD-NAME-TBL.
      ******************************************************************

           MOVE       SPACES                          TO
                          SIAPC-MODULE-NAME OF ITSIAPC
                         (SIAPC-MODULE-LEVEL OF ITSIAPC).

       X400-EXIT.
           EXIT.

158700
      ******************************************************************
       X500-INIT-MOD-NAME-TBL.
      ******************************************************************

           MOVE       SPACES                          TO
                          SIAPC-MODULE-NAME OF WS-ITSIAPC
                         (SIAPC-MODULE-LEVEL OF WS-ITSIAPC).

       X500-EXIT.
           EXIT.

159900
      *18.0   E_115045  DJL REMOVE ALL FIELDS IN X600, REMOVED PARAGRAPH

161300
      ******************************************************************
       X700-INIT-SIAPC-INV-MOD-NAME.
      ******************************************************************
           IF SIAPC-MODULE-LEVEL OF ITSIAPC < ZERO
               MOVE ZERO TO SIAPC-MODULE-LEVEL OF ITSIAPC.

           IF SIAPC-MODULE-LEVEL OF ITSIAPC < WS-10
               ADD WS-1 TO SIAPC-MODULE-LEVEL OF ITSIAPC
               MOVE WS-REQUESTED-MODULE TO
                        SIAPC-MODULE-NAME OF ITSIAPC
                       (SIAPC-MODULE-LEVEL OF ITSIAPC)
           ELSE
               MOVE WS-10 TO SIAPC-MODULE-LEVEL OF ITSIAPC
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (2) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (1)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (3) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (2)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (4) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (3)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (5) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (4)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (6) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (5)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (7) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (6)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (8) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (7)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (9) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (8)
               MOVE SIAPC-MODULE-NAME OF ITSIAPC (10) TO
                        SIAPC-MODULE-NAME OF ITSIAPC (9)
               MOVE WS-REQUESTED-MODULE   TO
                        SIAPC-MODULE-NAME OF ITSIAPC
                       (SIAPC-MODULE-LEVEL OF ITSIAPC).

       X700-EXIT.
           EXIT.

165200
      ******************************************************************
       X750-INIT-SIAPC-INV-MOD-NAME.
      ******************************************************************
           IF SIAPC-MODULE-LEVEL OF WS-ITSIAPC < ZERO
               MOVE ZERO TO SIAPC-MODULE-LEVEL OF WS-ITSIAPC.

           IF SIAPC-MODULE-LEVEL OF WS-ITSIAPC < WS-10
               ADD WS-1 TO SIAPC-MODULE-LEVEL OF WS-ITSIAPC
               MOVE WS-REQUESTED-MODULE TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC
                       (SIAPC-MODULE-LEVEL OF WS-ITSIAPC)
           ELSE
               MOVE WS-10 TO SIAPC-MODULE-LEVEL OF WS-ITSIAPC
               MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (2) TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC (1)
               MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (3) TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC (2)
               MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (4) TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC (3)
               MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (5) TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC (4)
      *12.0   E_41233  DJL BEGIN
167400         MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (6) TO
167500                  SIAPC-MODULE-NAME OF WS-ITSIAPC (5)
167600         MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (7) TO
167700                  SIAPC-MODULE-NAME OF WS-ITSIAPC (6)
167800         MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (8) TO
167900                  SIAPC-MODULE-NAME OF WS-ITSIAPC (7)
168000         MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (9) TO
168100                  SIAPC-MODULE-NAME OF WS-ITSIAPC (8)
168200         MOVE SIAPC-MODULE-NAME OF WS-ITSIAPC (10) TO
168300                  SIAPC-MODULE-NAME OF WS-ITSIAPC (9)
      *12.0   E_41233  DJL END
               MOVE WS-REQUESTED-MODULE   TO
                        SIAPC-MODULE-NAME OF WS-ITSIAPC
                       (SIAPC-MODULE-LEVEL OF WS-ITSIAPC).

       X750-EXIT.
           EXIT.

169100
      ******************************************************************
       X900-MOVE-DT-TIME.
      ******************************************************************

           MOVE SIAPC-GREG-CURRENT-DT OF ITSIAPC
             TO SIAPC-GREG-CURRENT-DT OF WS-ITSIAPC
           MOVE SIAPC-JULIAN-CURRENT-DT OF ITSIAPC
             TO SIAPC-JULIAN-CURRENT-DT OF WS-ITSIAPC
           MOVE SIAPC-DISPLAY-CURRENT-DT OF ITSIAPC
             TO SIAPC-DISPLAY-CURRENT-DT OF WS-ITSIAPC
           MOVE SIAPC-CURRENT-TIME OF ITSIAPC
             TO SIAPC-CURRENT-TIME OF WS-ITSIAPC
           MOVE SIAPC-DISPLAY-CURRENT-TIME OF ITSIAPC
             TO SIAPC-DISPLAY-CURRENT-TIME OF WS-ITSIAPC.

       X900-EXIT.
           EXIT.

173400
      ******************************************************************
       Z100-INVALID-FUNCTION.
      ******************************************************************

           MOVE       CWSCC-SEVERE-LOG                TO
                          SIAPC-RETURN-IND OF ITSIAPC.

           MOVE       CWSCC-INVALID-FUNCTION          TO
                          SIAPC-RETURN-STATUS OF ITSIAPC.

           MOVE       SPACES                          TO
                          SIAPC-RETURN-MESSAGE-CD OF ITSIAPC.

           PERFORM    9999-GOBACK THRU 9999-EXIT.

       Z100-EXIT.
           EXIT.

177500
      ******************************************************************
       Z200-UNSUCCESSFUL.
      ******************************************************************

           MOVE       CWSCC-UNSUCCESSFUL              TO
                          SIAPC-RETURN-IND OF ITSIAPC.

           PERFORM    9999-GOBACK THRU 9999-EXIT.

       Z200-EXIT.
           EXIT.

178800
      ******************************************************************
       Z300-SEVERE-LOG.
      ******************************************************************

           MOVE       CWSCC-SEVERE-LOG       TO
                          SIAPC-RETURN-IND OF ITSIAPC.

           PERFORM    9999-GOBACK THRU 9999-EXIT.

       Z300-EXIT.
           EXIT.

180100
      ******************************************************************
       9999-GOBACK.
      ******************************************************************

           GOBACK.

       9999-EXIT.
           EXIT.

