       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     IUCP2112.
       AUTHOR.                         IBS.
       DATE-WRITTEN.                   JANUARY 17, 1997.
       DATE-COMPILED.
      ******************************************************************
      *                                                                *
      *            INTER-PLAN TELEPROCESSING SERVICES (ITS)            *
      *                                                                *
      *   MODULE:       IUCP2112                                       *
      *   NAME:         HPA OEC SUSPENSE BATCH I/O HANDLER             *
      *   VERSION:      02.3                                           *
      *   DATE:         05/17/2012                                     *
      *                                                                *
      *   APPLICATION:  HOME PLAN AID                                  *
      *   TYPE:         VSAM                                           *
      *   FUNCTION:     THIS MODULE RETRIEVES RECORDS FROM THE OEC     *
      *                 SUSPENSE FILE.                                 *
      *                                                                *
      *   MODIFICATION                                                 *
      *   HISTORY:      01/17/97 01.0 - INITIAL RELEASE                *
      *                 11/16/07 02.0 - E_23333  LINE EXPANSION        *
      *                               - CHANGED VSAM IO TO CALL DB2    *
      *                                 COMMON MODULE                  *
      *                                 ADDED CALL TO EXIT MODULE TO   *
      *                                 DERIVE BUSINESS OWNER ID       *
      *                 06/11/09 02.1 - TECH MEMO ITS-TM-09-06-R10.4   *
      *                                 MPP E_34835 - ADD BOID         *
      ******************************************************************00020004
      * REL   - DATE       - VERS - CONTROL  - DESCRIPTION             *00030003
      *R11.2  ¦ 11/11/2010 ¦ 02.2 ¦ I_53936  ¦ DJL: OBSOLETE MODULES   *00040006
      *R12.0  ¦ 05/17/2012 ¦ 02.3 ¦ E_19137  ¦ DJL: CHANGES RELATED TO *00040001
      *       ICD-10/HIPAA PROJECT, ALSO CHANGES INCLUDED FOR THE      *00050001
      *       FOLLOWING ENHANCEMENTS E_59631, E_49746, E_61141.        *00060001
      *       REMOVE REFERENCES TO THE SF STACK                        *
      ******************************************************************
      *
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

      *******   W O R K I N G - S T O R A G E   S E C T I O N   *******
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      *
      *11.2   I_53936  BZXDJL1 BEGIN                                    00010004
       01  WS-PROGRAM-INFO.                                             00020000
           05  WS-MODULE-ID               PIC X(8)  VALUE 'IUCP2112'.   00030002
           05  WS-PROGRAM-ID              REDEFINES WS-MODULE-ID        00040001
                                          PIC X(8).                     00050001
           05  WS-VERSION-NUMBER          PIC X(4)  VALUE '02.3'.       00060002
           05  WS-RELEASE-DATE            PIC X(8)  VALUE '05/17/12'.   00070004
           05  WS-PROGRAM-DISPLAY.                                      00080000
               10  WS-COMMON-DISPLAY      PIC X(30) VALUE               00090000
                   'IUCP2112-HPA OEC SUSP BATCH  '.                     00100002
               10  WS-MODULE-DISPLAY      PIC X(8)  VALUE ' MODULE '.   00110000
               10  WS-RELEASE-DISPLAY     PIC X(34) VALUE               00120000
                   'VERSION #02.3 RELEASED ON 05/17/12'.                00130004
           05  WS-DESCRIPTION             REDEFINES WS-PROGRAM-DISPLAY  00140001
                                          PIC X(72).                    00150001
      *11.2   I_53936  BZXDJL1 END                                      00160004
      *
      *****************************************************************

       01  WS-ITCP0002                    PIC X(08) VALUE 'ITCP0002'.
       01  WS-BUS-OWNER-CICS              PIC X(08) VALUE 'ITPEXOS1'.
       01  WS-CONSTANTS.
           05  WS-NO                      PIC X VALUE 'N'.
           05  WS-YES                     PIC X VALUE 'Y'.

           05  WS-ONE                     PIC X VALUE '1'.
           05  WS-TWO                     PIC X VALUE '2'.

           05  WS-END-OF-PROCESSING-SW    PIC X VALUE 'N'.
               88  WS-END-OF-PROCESSING         VALUE 'Y'.

           05  OEC-FILE-STATUS            PIC X(02) VALUE SPACES.
               88  OEC-SUCCESSFUL-FILE              VALUE '00' '97'.
               88  OEC-END-OF-FILE                  VALUE '10'.
               88  OEC-NO-RECORD-FOUND              VALUE '23'.

           05  WS-FIRST-PASS-SWITCH       PIC X VALUE 'Y'.
               88  FIRST-PASS             VALUE 'Y'.
               88  CURSOR-OPEN            VALUE 'O'.
               88  CURSOR-CLOSED          VALUE 'C'.

           05  WS-1                       PIC X(01) VALUE '1'.
           05  WS-2                       PIC X(01) VALUE '2'.

       01  IUCWSCC.
           COPY IUCWSCC.

      *****************************************************************
      *---------------- PASS AREA FOR OEC DB2 I/O      ---------------*
      *****************************************************************
      **
       01  LS-PASS-DATA.
           05  FILLER.
               10  PASS-SCCF-ID              PIC X(017).
               10  FILLER REDEFINES PASS-SCCF-ID.
                   15  PASS-SCCF-LOC-PLAN-CD PIC X(003).
                   15  PASS-SCCF-JUL-DT      PIC X(007).
                   15  PASS-SCCF-SEQ-NUM     PIC X(005).
                   15  PASS-SCCF-SUFX-ID     PIC X(002).
               10  PASS-BUS-OWNER-ID         PIC X(004).
      *********** TRANSLATE TO 2 POS
               10  PASS-FMT-TXN-ID           PIC X(001).
               10  PASS-DISP-CD              PIC X(001).
               10  PASS-SEQ-NUM              PIC S9(04) COMP.
               10  PASS-ROW-CNT              PIC S9(04) COMP.
               10  PASS-CLERK-ID             PIC X(004).
               10  PASS-SUB-ID               PIC X(017).
               10  FILLER REDEFINES PASS-SUB-ID.
                   15  PASS-SUB-APFX-ID      PIC X(003).
                   15  PASS-SUB-SUFX-ID      PIC X(014).
               10  PASS-STS-CD               PIC X(01).
               10  PASS-OEC-ACTION-CD        PIC X(01).
      *********** TRANSLATE TO 2 POS
               10  PASS-CLM-TP-CD            PIC X(01).
      **************
           05  PASS-ACTION               PIC X(01).
             88  WRITE-THE-STACK                 VALUE 'W'.
             88  BUILD-THE-STACK                 VALUE 'B'.
             88  EXTRACT                         VALUE 'E'.
             88  DECLARE                         VALUE 'D'.
      * PASS-DB2-SIZE WILL BE IN THE PARAMETERS D/B DEFINED BY PLAN
           05  PASS-DB2-SIZE             PIC 9(02).
           05  PASS-INVALID-REQUEST-SW   PIC X(01).
             88  INVALID-REQUEST                 VALUE 'I'.
             88  VALID-REQUEST                   VALUE 'V'.
           05  PASS-CLAIM-TYPE           PIC X(02) VALUE SPACES.
           05  PASS-FILE-STATUS          PIC X(02) VALUE ZEROES.
             88  SUCCESSFUL-COMPLETION              VALUE '00' '04'.
             88  OPEN-SUCCESSFUL-W-IMP-VERIFY       VALUE '97'.
             88  RECORD-FOUND                       VALUE '  '.
             88  RECORD-NOT-FOUND                   VALUE '23'.
             88  END-OF-FILE                        VALUE '10'.
             88  CRITICAL-ERROR                     VALUE '99'.


           05  PASS-STACK                PIC X(1023315).

           05  LS-FDB-IO-AREA-SLOTS      REDEFINES
               PASS-STACK.
             10  LS-FDB-IO-SLOT          OCCURS 4013
                                     INDEXED BY LS-SLOT-INDEX.
               15  LS-FDB-KEY.
                 20  LS-FDB-SERNUM       PIC  X(17).
                 20  LS-FDB-TRANID       PIC  X(02).
                     88  LS-FDB-TRANID-SF        VALUE '11'
                                                       ' 1'.
                     88  LS-FDB-TRANID-DF        VALUE '22'
                                                       ' 2'.
                     88  LS-FDB-TRANID-NF        VALUE '38'
                                                       ' 8'.
                     88  LS-FDB-TRANID-RF        VALUE '43'
                                                       ' 3'.
                     88  LS-TRAILER              VALUE ' 9'.
                 20  FILLER REDEFINES
                     LS-FDB-TRANID.
                     25  LS-FDB-TRANID-1 PIC  X(01).
                     25  LS-FDB-TRANID-2 PIC  X(01).
                 20  LS-FDB-TRANQUAL     PIC  X(02).
                     88  LS-FDB-TRANQUAL-SF          VALUE '  '.
                     88  LS-FDB-TRANQUAL-DFRF-ORIG   VALUE '1 '.
                     88  LS-FDB-TRANQUAL-DFRF-VOID   VALUE '4 '.
                 20  LS-FDB-RECTYP       PIC  X(02).
                 20  LS-FDB-SEQNUM       PIC  S9(4) COMP.

               15  LS-FDB-DATA.
                 20  LS-FDB-SF-CLMTYP    PIC  X(02).
                     88  LS-FDB-SF-CLMTYP-INST       VALUE '11'
                                                           '12'.
                     88  LS-FDB-SF-CLMTYP-PROF       VALUE '20'.
                     88  LS-FDB-SF-CLMTYP-DENT       VALUE '40'.
                 20  FILLER              PIC  X(21).
                 20  LS-FDB-DFRF-DSPCOD  PIC  X(01).
                     88  LS-FDB-DFRF-DSPCOD-ORIG     VALUE '1'.
                     88  LS-FDB-DFRF-DSPCOD-VOID     VALUE '4'.
                 20  FILLER              PIC  X(206).


       01  OEC-COMM-AREA               PIC X(1029875).

       01  OEC-SUSPENSE-RECORD-DF   REDEFINES OEC-COMM-AREA.
           COPY ITCFMQ00.
           COPY ITCFMXDF.

       01  LS-ITCCSAPI-RECORD.
           COPY ITCCSAPI.

       01  LS-ITCOECOM-RECORD.
           COPY ITCOECOM.

       01  LS-ITCIADBL.
           COPY ITCIADBL.

       01  ITCIABP-RECORD.
           COPY ITCIABP.

       01  ITCPCQ00-RECORD.
           COPY ITCPCQ00.

       01  ITCIADF-RECORD.
           COPY ITCIADF.

       01  ITPEXN-PASS-AREA.
           COPY ITCIPEXN.


      *****************************************************************
      **************    L I N K A G E   S E C T I O N    **************
      *---------------------------------------------------------------*
       LINKAGE SECTION.
      *---------------------------------------------------------------*

       01  ITSIAPC.
           COPY ITSIAPC.

       01  IUCIAQD.
           COPY IUCIAQD.

      *ITS-TM-2009-06-R10.4 E_34835 BEGIN
       01  ITCCABOI.
           COPY ITCCABOI.
      *ITS-TM-2009-06-R10.4 E_34835 END

      *****************************************************************
      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************
      *****************************************************************
       PROCEDURE DIVISION  USING ITSIAPC
                                 IUCIAQD
      *ITS-TM-2009-06-R10.4 E_34835 BEGIN
                                 ITCCABOI.
      *ITS-TM-2009-06-R10.4 E_34835 END

      *****************************************************************
       0000-MAINLINE-IUCP2112.
      *****************************************************************

           PERFORM A100-INITIALIZATION THRU A100-EXIT.

           IF FIRST-PASS
      *11.2   I_53936  BZXDJL1                                          00010004
              DISPLAY WS-PROGRAM-DISPLAY
              MOVE 'N' TO WS-FIRST-PASS-SWITCH.

           IF  SIAPC-FUNCTION-CD-MAJOR = CWSCC-OPEN-FILES-REQUEST
           AND SIAPC-FUNCTION-CD-SUB   = CWSCC-OPEN-AS-I-O-ACCESS
               PERFORM A200-OPEN-OEC-FILE-I-O THRU A200-EXIT
           ELSE
           IF  SIAPC-FUNCTION-CD-MAJOR = CWSCC-READ-FILES-REQUEST
           AND SIAPC-FUNCTION-CD-SUB   = CWSCC-GENERIC-READ-ACCESS
               PERFORM A300-GENERIC-READ THRU A300-EXIT
           ELSE
           IF  SIAPC-FUNCTION-CD-MAJOR = CWSCC-READ-FILES-REQUEST
           AND SIAPC-FUNCTION-CD-SUB   = CWSCC-READ-NEXT
               PERFORM A400-READ-NEXT THRU A400-EXIT
           ELSE
           IF  SIAPC-FUNCTION-CD-MAJOR = CWSCC-UPDATE-FILES-REQUEST
               PERFORM A500-UPDATE THRU A500-EXIT
           ELSE
           IF  SIAPC-FUNCTION-CD-MAJOR = CWSCC-CLOSE-FILES-REQUEST
               PERFORM A600-CLOSE-OEC-FILE THRU A600-EXIT
           ELSE
               PERFORM Z100-INVALID-FUNCTION THRU Z100-EXIT.

           PERFORM 9990-GOBACK THRU 9990-EXIT.

       0000-EXIT.
           EXIT.

      *****************************************************************
       A100-INITIALIZATION.
      *****************************************************************

           PERFORM X100-INIT-SIAPC THRU X100-EXIT.
           IF FIRST-PASS
      *ITS-TM-2009-06-R10.4 E_34835 BEGIN
              IF  CABOI-FORMAT-BOID NOT = SPACES AND LOW-VALUES
                  MOVE CABOI-FORMAT-BOID TO CIAPEXN-BUS-OWNER-ID
              ELSE
      *ITS-TM-2009-06-R10.4 E_34835 END
                  PERFORM X110-BUS-OWNER-ID-DB THRU X110-EXIT.

       A100-EXIT.
           EXIT.


      *****************************************************************
       A200-OPEN-OEC-FILE-I-O.
      *****************************************************************

           MOVE ZEROES      TO OEC-FILE-STATUS.

       A200-EXIT.
           EXIT.


      *****************************************************************
       A300-GENERIC-READ.
      *****************************************************************

           PERFORM B100-FORMAT-FMQ00-AREA THRU B100-EXIT.

      **
      *CLOSE OPEN CURSER SO THAT IT CAN BE RESET APROPRIATELY
      **
           IF CURSOR-OPEN
              PERFORM A600-CLOSE-OEC-FILE THRU A600-EXIT
              SET CURSOR-CLOSED TO TRUE.

           PERFORM B200-OEC-GENERIC-READ  THRU B200-EXIT.

       A300-EXIT.
           EXIT.


      *****************************************************************
       A400-READ-NEXT.
      *****************************************************************

           PERFORM B300-FORMAT-FMQ00-AREA THRU B300-EXIT.

           PERFORM B400-OEC-READ-NEXT     THRU B400-EXIT.

       A400-EXIT.
           EXIT.


      *****************************************************************
       A500-UPDATE.
      *****************************************************************

           PERFORM B500-FORMAT-FMQ00-AREA THRU B500-EXIT.

           PERFORM B600-OEC-READ-UPDATE   THRU B600-EXIT.

      **
      *SET CURSOR CLOSED BECAUSE COMMIT CLOSES
      **
           IF CURSOR-OPEN
              SET CURSOR-CLOSED TO TRUE.

       A500-EXIT.
           EXIT.


      *****************************************************************
       A600-CLOSE-OEC-FILE.
      *****************************************************************

           PERFORM T700-CLOSE THRU T700-EXIT.

           IF NOT OEC-SUCCESSFUL-FILE
               PERFORM Z600-CLOSE-ERROR THRU Z600-EXIT.

       A600-EXIT.
           EXIT.


      *****************************************************************
       B100-FORMAT-FMQ00-AREA.
      *****************************************************************

           MOVE LOW-VALUES      TO OEC-SUSPENSE-RECORD-DF

           MOVE '2'             TO FMQ00-REC-TYPE

           MOVE CIAQD-SCCF-SER-NUM (1:15) TO
                FMQ00-SCCF-NUM (1:15).

       B100-EXIT.
           EXIT.


      *****************************************************************
       B200-OEC-GENERIC-READ.
      *****************************************************************

           MOVE ZEROES      TO OEC-FILE-STATUS.

           MOVE WS-NO       TO WS-END-OF-PROCESSING-SW.

           PERFORM C100-START-OEC-SUSPENSE-KEY THRU C100-EXIT.

           PERFORM C300-READ-NEXT              THRU C300-EXIT
               UNTIL WS-END-OF-PROCESSING.

       B200-EXIT.
           EXIT.


      *****************************************************************
       B300-FORMAT-FMQ00-AREA.
      *****************************************************************


      **   DISPLAY ' B300 SCCF  ' CIAQD-SCCF-SER-NUM (1:15)
           MOVE LOW-VALUES      TO OEC-SUSPENSE-RECORD-DF

           MOVE '2'             TO FMQ00-REC-TYPE

           MOVE CIAQD-SCCF-SER-NUM (1:15) TO
                FMQ00-SCCF-NUM (1:15)


           MOVE CIAQD-TRANS-QUALIFIER TO
                FMQ00-TRANS-QUALIFIER.


       B300-EXIT.
           EXIT.


      *****************************************************************
       B400-OEC-READ-NEXT.
      *****************************************************************

      **   DISPLAY ' B400 SCCF  ' CIAQD-SCCF-SER-NUM (1:15)
           MOVE ZEROES      TO OEC-FILE-STATUS.

           MOVE WS-NO       TO WS-END-OF-PROCESSING-SW.

           PERFORM C200-START-OEC-SUSPENSE-KEY THRU C200-EXIT.

           PERFORM C400-READ-NEXT              THRU C400-EXIT
               UNTIL WS-END-OF-PROCESSING.

       B400-EXIT.
           EXIT.


      *****************************************************************
       B500-FORMAT-FMQ00-AREA.
      *****************************************************************

           MOVE IUCIAQD (1:19) TO FMQ00-RECORD (1:19).

       B500-EXIT.
           EXIT.


      *****************************************************************
       B600-OEC-READ-UPDATE.
      *****************************************************************

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T500-READ THRU T500-EXIT.

           IF OEC-SUCCESSFUL-FILE
               PERFORM C500-REWRITE THRU C500-EXIT
           ELSE
      **
      **ADDED TO END PROCESSING
      **
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               IF  OEC-NO-RECORD-FOUND
                   PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
               ELSE
                   PERFORM Z400-READ-ERROR       THRU Z400-EXIT.

       B600-EXIT.
           EXIT.


      *****************************************************************
       C100-START-OEC-SUSPENSE-KEY.
      *****************************************************************

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T200-START THRU T200-EXIT.

           IF NOT OEC-SUCCESSFUL-FILE
               IF  OEC-NO-RECORD-FOUND
               OR  OEC-END-OF-FILE
                   PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
               ELSE
                   PERFORM Z400-READ-ERROR       THRU Z400-EXIT.

       C100-EXIT.
           EXIT.


      *****************************************************************
       C200-START-OEC-SUSPENSE-KEY.
      *****************************************************************

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T300-START THRU T300-EXIT.

           IF NOT OEC-SUCCESSFUL-FILE
               IF  OEC-NO-RECORD-FOUND
               OR  OEC-END-OF-FILE
                   PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
               ELSE
                   PERFORM Z400-READ-ERROR       THRU Z400-EXIT.

       C200-EXIT.
           EXIT.


      *****************************************************************
       C300-READ-NEXT.
      *****************************************************************

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T400-READ-NEXT THRU T400-EXIT.

      **   DISPLAY ' C300 FILE-STATUS '  OEC-FILE-STATUS
      **   DISPLAY ' C300 SCCF  FMQ   ' FMQ00-SCCF-NUM (1:15)
      **   DISPLAY ' C300 CIAQD ' CIAQD-SCCF-SER-NUM OF IUCIAQD (1:15)
           IF NOT OEC-SUCCESSFUL-FILE
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               IF OEC-NO-RECORD-FOUND
               OR OEC-END-OF-FILE
                   PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
               ELSE
                   PERFORM Z400-READ-ERROR       THRU Z400-EXIT
           ELSE
           IF FMQ00-FILE-TRAILER-REC
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
           ELSE
           IF FMQ00-SCCF-NUM (1:15) >
              CIAQD-SCCF-SER-NUM OF IUCIAQD (1:15)
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
           ELSE
           IF FMQ00-SCCF-NUM (1:15) =
              CIAQD-SCCF-SER-NUM OF IUCIAQD (1:15)
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               PERFORM D200-MOVE-FIELDS THRU D200-EXIT.

       C300-EXIT.
           EXIT.


      *****************************************************************
       C400-READ-NEXT.
      *****************************************************************

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T400-READ-NEXT THRU T400-EXIT.

           IF NOT OEC-SUCCESSFUL-FILE
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               IF OEC-NO-RECORD-FOUND
               OR OEC-END-OF-FILE
                   PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
               ELSE
                   PERFORM Z400-READ-ERROR       THRU Z400-EXIT
           ELSE
           IF FMQ00-FILE-TRAILER-REC
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               PERFORM Z300-RECORD-NOT-FOUND THRU Z300-EXIT
           ELSE
               MOVE WS-YES TO WS-END-OF-PROCESSING-SW
               PERFORM D200-MOVE-FIELDS THRU D200-EXIT.

       C400-EXIT.
           EXIT.


      *****************************************************************
       C500-REWRITE.
      *****************************************************************

           MOVE CIAQD-CLAIM-EDIT-STATUS TO
                    FMQ00-CLAIM-EDIT-STATUS
                    FMX21A-STAT-CD.

           MOVE CIAQD-CLAIM-RELEASE-STATUS TO
                    FMQ00-CLAIM-RELEASE-STATUS.

           MOVE ZEROES TO OEC-FILE-STATUS.

           PERFORM T600-REWRITE THRU T600-EXIT.

           IF NOT OEC-SUCCESSFUL-FILE
               PERFORM Z500-REWRITE-ERROR THRU Z500-EXIT.

       C500-EXIT.
           EXIT.


      *****************************************************************
       D200-MOVE-FIELDS.
      *****************************************************************

           MOVE FMQ00-RECORD (1:22) TO IUCIAQD (1:22).

           MOVE FMX21A-ERR-CDS
             TO CIAQD-ERR-CDS.

           IF CIAQD-SOURCE-ID = HIGH-VALUES
               MOVE FMX21A-CROSS-REF-SCCF-NUM
                 TO CIAQD-CROSS-REF-SCCF-NUM.

           MOVE FMX21A-PROC-SITE-CNTL-NUM
             TO CIAQD-PROC-SITE-CNTL-NUM.

           MOVE WS-YES TO WS-END-OF-PROCESSING-SW.

       D200-EXIT.
           EXIT.


      *****************************************************************
       T200-START.
      *****************************************************************

           MOVE SPACES TO PASS-INVALID-REQUEST-SW
           MOVE ZEROES TO PASS-FILE-STATUS

           MOVE CWSCC-READ-FILES-REQUEST TO
                   SIAPC-FUNCTION-CD-MAJOR
           MOVE '62'             TO
              SIAPC-FUNCTION-CD-SUB.

           PERFORM T999-SET-UP-PASS THRU T999-EXIT
           MOVE 'D'                TO PASS-ACTION.
           MOVE '2'                TO PASS-FMT-TXN-ID
           MOVE +5                 TO CSAPI-SUB-COMMAND

           IF  CURSOR-OPEN
               GO TO T200-EXIT.



           CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS
           SET CURSOR-OPEN TO TRUE.

       T200-EXIT.
           EXIT.


      *****************************************************************
       T300-START.
      *****************************************************************

           MOVE SPACES TO PASS-INVALID-REQUEST-SW
           MOVE ZEROES TO PASS-FILE-STATUS

           MOVE CWSCC-READ-FILES-REQUEST TO
                   SIAPC-FUNCTION-CD-MAJOR
           MOVE '62'             TO
              SIAPC-FUNCTION-CD-SUB.

           PERFORM T999-SET-UP-PASS THRU T999-EXIT
           IF  CURSOR-OPEN
               GO TO T300-EXIT.



           MOVE 'D'                TO PASS-ACTION.
           MOVE '2'                TO PASS-FMT-TXN-ID
           MOVE +5                 TO CSAPI-SUB-COMMAND

           CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS
           SET CURSOR-OPEN TO TRUE.

       T300-EXIT.
           EXIT.


      *****************************************************************
       T400-READ-NEXT.
      *****************************************************************

      *REPLACED BY FETCH
      ************************
      *SECOND TIME THRU      *
           MOVE SPACES TO PASS-INVALID-REQUEST-SW
           MOVE ZEROES TO PASS-FILE-STATUS
      **
      **PASS-ACTION  OF 'E' INDICATES TO DO A FETCH
      **AFTER THE INITIAL DECLARE IS DONE.
      **
           MOVE CWSCC-READ-FILES-REQUEST TO
                   SIAPC-FUNCTION-CD-MAJOR
           MOVE '62'             TO
              SIAPC-FUNCTION-CD-SUB.

           PERFORM T999-SET-UP-PASS THRU T999-EXIT
           MOVE SPACES             TO PASS-STACK.
           MOVE 'E'                TO PASS-ACTION.
           MOVE '2'                TO PASS-FMT-TXN-ID
           MOVE +5                 TO CSAPI-SUB-COMMAND

           CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS
           MOVE PASS-STACK       TO FMXDF-CLAIM

           MOVE PASS-FMT-TXN-ID     TO FMQ00-REC-TYPE
           MOVE PASS-DISP-CD        TO FMQ00-TRANS-QUALIFIER
           MOVE PASS-CLM-TP-CD      TO FMQ00-CLAIM-TYPE-IND
           MOVE PASS-STS-CD         TO FMQ00-CLAIM-EDIT-STATUS
           MOVE PASS-OEC-ACTION-CD  TO FMQ00-CLAIM-RELEASE-STATUS
           MOVE PASS-SCCF-ID        TO FMQ00-SCCF-NUM.

       T400-EXIT.
           EXIT.


      *****************************************************************
       T500-READ.
      *****************************************************************

      ****
      * TO TRIGGER SELECT ON SCCF / BUS OWNER /  REC-TYPE / DISP ONLY
      **DO SELECT - TO SEE IF SCCF KEY IS ON FILE BEFORE UPDATE
      ****
           MOVE '2'                        TO FMQ00-REC-TYPE.
           MOVE CIAQD-TRANS-QUALIFIER      TO FMQ00-TRANS-QUALIFIER.
           MOVE CIAQD-SCCF-SER-NUM         TO FMQ00-SCCF-NUM.
           MOVE CIAQD-CLAIM-EDIT-STATUS    TO FMQ00-CLAIM-EDIT-STATUS
           MOVE CIAQD-CLAIM-RELEASE-STATUS TO FMQ00-CLAIM-RELEASE-STATUS

      ******************
           MOVE SPACES TO PASS-INVALID-REQUEST-SW
           MOVE ZEROES TO PASS-FILE-STATUS.
      **
           MOVE CWSCC-READ-FILES-REQUEST TO
                   SIAPC-FUNCTION-CD-MAJOR
           MOVE CWSCC-RANDOM-READ-ACCESS TO
                   SIAPC-FUNCTION-CD-SUB

           PERFORM T999-SET-UP-PASS THRU T999-EXIT

           MOVE +2  TO CSAPI-SUB-COMMAND

           CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS.
           MOVE PASS-STACK       TO FMXDF-CLAIM

           MOVE PASS-FMT-TXN-ID     TO FMQ00-REC-TYPE
           MOVE PASS-DISP-CD        TO FMQ00-TRANS-QUALIFIER
           MOVE PASS-CLM-TP-CD      TO FMQ00-CLAIM-TYPE-IND
           MOVE PASS-STS-CD         TO FMQ00-CLAIM-EDIT-STATUS
           MOVE PASS-OEC-ACTION-CD  TO FMQ00-CLAIM-RELEASE-STATUS
           MOVE PASS-SCCF-ID        TO FMQ00-SCCF-NUM.

       T500-EXIT.
           EXIT.


      *****************************************************************
       T600-REWRITE.
      *****************************************************************

           MOVE ZEROES TO PASS-FILE-STATUS
           MOVE CWSCC-UPDATE-FILES-REQUEST TO
                      SIAPC-FUNCTION-CD-MAJOR
           MOVE CWSCC-WRITE-FILES-REQUEST TO
                      SIAPC-FUNCTION-CD-SUB

           PERFORM T999-SET-UP-PASS THRU T999-EXIT
      **
      ** NEW PROCESSING WILL DO A DELETE / THEN INSERT
      **
           MOVE +1                 TO CSAPI-SUB-COMMAND

           MOVE FMXDF-CLAIM TO PASS-STACK

           CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS.

       T600-EXIT.
           EXIT.


      *****************************************************************
       T700-CLOSE.
      *****************************************************************

           MOVE SPACES TO PASS-INVALID-REQUEST-SW
           MOVE ZEROES TO PASS-FILE-STATUS
      **
           MOVE CWSCC-CLOSE-FILES-REQUEST TO
                   SIAPC-FUNCTION-CD-MAJOR
           MOVE SPACES TO
              SIAPC-FUNCTION-CD-SUB.

           PERFORM T999-SET-UP-PASS THRU T999-EXIT
           MOVE 'C'                TO PASS-ACTION.
           MOVE '2'                TO PASS-FMT-TXN-ID
           MOVE +5                 TO CSAPI-SUB-COMMAND

           IF  CURSOR-OPEN
               CALL WS-ITCP0002 USING
                             LS-PASS-DATA
                             OEC-COMM-AREA
                             LS-ITCCSAPI-RECORD
                             LS-ITCIADBL
                             ITPEXN-PASS-AREA
                             ITCIABP-RECORD
                             ITSIAPC.

           MOVE PASS-FILE-STATUS TO OEC-FILE-STATUS.

       T700-EXIT.
           EXIT.


      /*****************************************************************
      *          S E T   U P   T H E  P A S S  A R E A                 *
      ******************************************************************
       T999-SET-UP-PASS.

      **   DISPLAY ' T999 '
           MOVE WS-MODULE-ID             TO CSAPI-CALLING-PGM-ID
           SET CSAPI-VSAM-INST TO TRUE.
           SET CSAPI-BATCH-ENV TO TRUE.
           MOVE +1                       TO CSAPI-SUB-COMMAND

           MOVE CIAPEXN-BUS-OWNER-ID     TO PASS-BUS-OWNER-ID.
           MOVE FMX22C-SUB-ID-NUM-ACTUAL TO PASS-SUB-ID
           MOVE FMX29A-CLERK-NUM         TO PASS-CLERK-ID

           INITIALIZE PASS-SCCF-ID
                      PASS-FMT-TXN-ID
                      PASS-DISP-CD
                      PASS-SEQ-NUM
                      PASS-ROW-CNT
                      PASS-STS-CD
                      PASS-OEC-ACTION-CD
                      PASS-CLM-TP-CD
                      PASS-ACTION
                      PASS-INVALID-REQUEST-SW
            MOVE ZEROS TO PASS-FILE-STATUS
      **************
            MOVE 16 TO PASS-DB2-SIZE.
      **************

      *** SET PROPER CLAIM TYPE.
      *12.0   E_19137  DJL REMOVE SF LOGIC
           MOVE FMX21A-CLM-TYPE TO PASS-CLAIM-TYPE.
           IF FMX21A-CLM-TYPE  = '11' OR = '12'
              MOVE 'I' TO PASS-CLM-TP-CD
           ELSE
              IF FMX21A-CLM-TYPE = '20' OR '40'
                 MOVE 'P' TO PASS-CLM-TP-CD.

           MOVE FMQ00-REC-TYPE             TO PASS-FMT-TXN-ID
           MOVE FMQ00-TRANS-QUALIFIER      TO PASS-DISP-CD
           MOVE FMQ00-CLAIM-EDIT-STATUS    TO PASS-STS-CD
           MOVE FMQ00-CLAIM-RELEASE-STATUS TO PASS-OEC-ACTION-CD
           MOVE FMQ00-SCCF-NUM             TO PASS-SCCF-ID.

       T999-EXIT.
           EXIT.


      *****************************************************************
       X100-INIT-SIAPC.
      *****************************************************************

           INITIALIZE SIAPC-RETURN-DATA.
           MOVE CWSCC-SUCCESSFUL      TO SIAPC-RETURN-IND.
           MOVE CWSCC-ACTION-COMPLETE TO SIAPC-RETURN-STATUS.

       X100-EXIT.
           EXIT.


      /***************************************************************
      *    SET BUSINESS OWNER ID BY CALLING OEC BATCH EXIT ROUTINE
      ****************************************************************
       X110-BUS-OWNER-ID-DB.

           MOVE WS-BUS-OWNER-CICS TO
                   SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL),

           SET  CSAPI-BATCH-ENV TO TRUE.

           CALL WS-BUS-OWNER-CICS USING
                                         ITPEXN-PASS-AREA,
                                         ITSIAPC,
                                         LS-ITCCSAPI-RECORD,
                                         LS-ITCOECOM-RECORD,
                                         LS-ITCIADBL

      *11.2    I_53936  BZXDJL1
           DISPLAY 'IUCP2112 - BUS OWNER: ' CIAPEXN-BUS-OWNER-ID.

           PERFORM Z000-CHECK-RETURN-IND THRU Z000-EXIT.

       X110-EXIT.
           EXIT.


      /**************************************************************
      *         C H E C K     R E T U R N     I N D                 *
      ***************************************************************
       Z000-CHECK-RETURN-IND.

           IF SIAPC-RETURN-IND = CWSCC-ABORT
              PERFORM Z020-ERROR-MESSAGE THRU Z020-EXIT.

           COMPUTE SIAPC-MODULE-LEVEL = SIAPC-MODULE-LEVEL - 1.

       Z000-EXIT.
           EXIT.


      /**************************************************************
      *            E R R O R     M E S S A G E                      *
      ***************************************************************
       Z020-ERROR-MESSAGE.

      **   DISPLAY ' Z020 '
           MOVE CWSCC-ABORT       TO SIAPC-RETURN-IND.
           MOVE '3600'            TO SIAPC-ABEND-CODE.

      **JMK ERROR MESSAGE ?
           MOVE CWSCC-SEVERE-ERROR         TO SIAPC-RETURN-STATUS.
           DISPLAY 'IUCP2112 - BUS OWNER MODULE ERROR '
           SET CIABP-EOJ                   TO TRUE.

       Z020-EXIT.
           EXIT.


      *****************************************************************
       Z100-INVALID-FUNCTION.
      *****************************************************************

      **   DISPLAY ' Z100 '
           MOVE CWSCC-SEVERE-LOG       TO SIAPC-RETURN-IND.

           MOVE CWSCC-INVALID-FUNCTION TO SIAPC-RETURN-STATUS.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z100-EXIT.
           EXIT.


      *****************************************************************
       Z200-OPEN-ERROR.
      *****************************************************************

      **   DISPLAY ' Z200 '
           MOVE CWSCC-SEVERE-LOG           TO SIAPC-RETURN-IND.

           MOVE CWSCC-OPEN-ERROR           TO SIAPC-RETURN-STATUS.

           MOVE OEC-FILE-STATUS            TO SIAPC-RETURN-MESSAGE-CD.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z200-EXIT.
           EXIT.


      *****************************************************************
       Z300-RECORD-NOT-FOUND.
      *****************************************************************

      **   DISPLAY ' Z300 '
           MOVE CWSCC-UNSUCCESSFUL         TO SIAPC-RETURN-IND.

           MOVE CWSCC-RECORD-NOT-FOUND     TO SIAPC-RETURN-STATUS.

           MOVE OEC-FILE-STATUS            TO SIAPC-RETURN-MESSAGE-CD.

           IF OEC-END-OF-FILE
              PERFORM 9990-GOBACK THRU 9990-EXIT
           ELSE
           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z300-EXIT.
           EXIT.


      *****************************************************************
       Z400-READ-ERROR.
      *****************************************************************

      **   DISPLAY ' Z400 ' OEC-SUSPENSE-RECORD-DF.
           MOVE CWSCC-SEVERE-LOG           TO SIAPC-RETURN-IND.

           MOVE CWSCC-READ-ERROR           TO SIAPC-RETURN-STATUS.

           MOVE OEC-FILE-STATUS            TO SIAPC-RETURN-MESSAGE-CD.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z400-EXIT.
           EXIT.


      *****************************************************************
       Z500-REWRITE-ERROR.
      *****************************************************************

      **   DISPLAY ' Z500 '
           MOVE CWSCC-SEVERE-LOG           TO SIAPC-RETURN-IND.

           MOVE CWSCC-WRITE-ERROR          TO SIAPC-RETURN-STATUS.

           MOVE OEC-FILE-STATUS            TO SIAPC-RETURN-MESSAGE-CD.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z500-EXIT.
           EXIT.


      *****************************************************************
       Z600-CLOSE-ERROR.
      *****************************************************************

      **   DISPLAY ' Z600 '
           MOVE CWSCC-SEVERE-LOG           TO SIAPC-RETURN-IND.

           MOVE CWSCC-CLOSE-ERROR          TO SIAPC-RETURN-STATUS.

           MOVE OEC-FILE-STATUS            TO SIAPC-RETURN-MESSAGE-CD.

           PERFORM 9999-GOBACK THRU 9999-EXIT.

       Z600-EXIT.
           EXIT.


      *****************************************************************
       9990-GOBACK.
      *****************************************************************

           GOBACK.

       9990-EXIT.
           EXIT.


      *****************************************************************
      **  THIS GOBACK IS FOR A NOT GOOD CONDITION AND CLOSES CURSOR****
      *****************************************************************
       9999-GOBACK.
      *****************************************************************
           PERFORM A600-CLOSE-OEC-FILE THRU A600-EXIT
      **   DISPLAY ' CURS CLOSED'
           SET CURSOR-CLOSED TO TRUE.

           GOBACK.

       9999-EXIT.
           EXIT.

