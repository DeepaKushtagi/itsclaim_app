000099CBL DATA(31),TRUNC(BIN),DYNAM,AWO,XREF(FULL)
000100*****************************************************************
000200*                                                               *
000300*                  LINE ITEM LINK (LIL)                         *
000400*                                                               *
000500* MODULE:   IUIP9103                                            *
000600* NAME:     INTERPRETER ALIM CALLER                             *
000700* VERSION:  11.0                                                * C-021215
000800* DATE:     DEC. 15, 2002                                       * C-021215
000900*                                                               *
001000* APPLICATION: INTERPRETER                                      *
001100* TYPE:     COMMON                                              *
001200* FUNCTION: IUIP9103 CALLS ALIM AND ANALYZES RETURN CODES       *
001300*                                                               *
001400* MODIFICATION                                                  *
001500* HISTORY:  03/15/96 01.0 - INITIAL RELEASE                     *
001510*           07/10/96 02.0 - CALL IUIP6109 TO DO AUDITS          *
001520*                           INSTEAD OF DOING IT HERE            *
001530*           08/30/96 03.0 - SHOW ALIM ERROR EVEN THOUGH         *
001540*                           INTERP ERROR 042 OCCURRED.          *
001550*                         - CHECK FOR FATAL STATUS AFTER        *
001560*                           DOING AUDIT, EXITING IF ONE IS      *
001570*                           FOUND, AND DO NOT DO AUDIT IF       *
001580*                           ALIM SENDS BACK SEVERE ERROR STATUS.*
001590*                         - SET ERROR STATUS TO FATAL FOR MOST  *
001591*                           SEVERE RETURN STATUS PASSED RECEIVED*
001592*                           FROM ALIM.                          *
001593*           10/28/96 04.0 - PASS TWO NEW AREAS TO ALIM IN ORDER * A-961014
001595*                           TO HAVE ACCESS TO WORK CLAIM DATA   * A-961014
001596*                           AND TO SF PRICING DATA.  THESE AREAS* A-961014
001597*                           MUST THEN BE PASSED TO IUIP9116 SO  * A-961014
001598*                           THEY ARE AVAILABLE FOR THE USER EXIT* A-961014
001599*                         - INTERP-SCRATCH-PAD-AREA ADDED TO    * A-961014
001600*                           LINKAGE AREA.                       * A-961014
001601*           01/31/97 05.0 - MODIFIED FOR ITS 8.0.               * A-970104
001610*           04/02/97      - FOR DL CK FOR SPACES DO NOT SET 5042* AHB00008
001611*           10/17/97      - FOR DL CK FOR SPACES DO NOT SET 5042* AHB00015
001612*                         - FOR FOR SIAPC-RETURN-IND = 0        * AHB00015
001801*           07/20/98      -RECOMPILE FOR Y2K  COPY VSCPCLXC     * Y2K
      *           CR29369  NEED INFO FROM TRIGGER FOR PIGGY PROCESSING* CX3988.J
      ***06/20/99  3.9.5  CR 35070 - RE-ASSEMBLE FOR MASS SURCHG/SUPP * AHB00022
      ***                            CHANGES TO COPY BOOKS FOR NOTE97 * AHB00022
      ***09/01/00         CR 48283 - RE-ASSEMBLE FOR ITS 8.2          * AHB00022
      **** 02/01/00 RE-COMPILE TO PROCESS DATAFLEX FORMAT OF WF AND WF2 X405
      **** COMMON AREA.                                                 X405
      ***  03/01/01       CR 56224 - RECOMPILE FOR COBOL LE.          * X410
      ***  12/10/01       CR 63986 - ADDED NEW ITS 8.4 COPYBOOKS      * C-011001
      ***  12/01/01 4.1.5 CR 63269 - CR 68414- HIPAA.                 * HIPA
      ***  12/19/02 4.1.5 CR 69227 - SAME DAY ADO                     *
001611*    12/15/02 11.0 - ADDED SUPPORT FOR ITS 8.6 MEDICARE         * A-021215
C96360*  08/18/2004         CR 96360 - ITS 8.9
001700*                    CR 75782                                   * A-021215
C97522*  11/04/2004       CR 97693 - ITS CLAIMS WITH C7873 DUE TO UE022
C99635*  01/13/2005         CR 99635 - DUMMY FINAL.                   * CR99635
130499*  02/13/2007         CR 130499 - BYPASS UBL01/UBL02/UBL03      * CR99635
130499*  02/13/2007                   - XREF C130499                  * CR99635
130499*  04/15/2007         CR 130182 - BYPASS UD001/UE069            * CR99635
130499*  04/15/2007                   - FOR ITS 9.4 RELEASE           * CR99635
IR3199*  04/15/2007         IR319996  - INTIALIZE DUMMY FINAL FLAG    * CR99635
IR3199*  04/15/2007                   - CORREXCT B1438 XREF IR3199    * CR99635
144564*  11/09/2008         CR144564  - ITS 10.2 SET DF MEESAGE CODE  * CR99635
144564*                               - 1257 FOR NON PAR XREF C14456  * CR99635
ITS102*  11/09/2008         CR141946  - ITS 10.2 BYPAAS UE062         * CR99635
ITS102*                               - MESSAGE                       * CR99635
147766*  01/08/2009         CR147766  - DF CLASS 3 NEEDS 1257 MESSAGE * CR99635
147766*                                 CODE XREF 147766              * CR99635
143336*  03/05/2009         CR143336  - DO NOT SET B1438 ON ITS       * CR99635
143336*                                 ADJUSTMENTS WITH U602         * CR99635
159805*  07/01/2010         CR159805  - PROHIBIT VOID FOR SAME DAY    * CR99635
159805*                                 ADO AND SAME DAY ADJ - IU831  * CR99635
159805*  10/17/2010         CR161884  - ENSURE FORMATS HAS DEFAULT    * CR99635
159805*                                 CLAIM RESULT CODE FOR DFDF    * CR99635
164645*  01/14/2011         CR164645  - DFDF SAME DAY CLADJ CREATING  *
164645*                                 DUPLICATE DF                  *
481439*  02/14/2014         CQ481439  - INTERPRETER BUG CAUSING THE   *
481439*                                 UE091                         *
720462*  05/16/2014         CQ720462  - ITS HOME LIL ERROR B1486      *
720462*                                 CANNOT FINALIZE               *
133764*  04/17/2015        CQ133764   - BYPASS UE098 EDIT             *
291016*  02/17/17          RTC291016  DN002 - NATIONAL/OOA CODE ON DF *
291016*                               MUST = VALUE ON SF  DF MSG 0291 *
291016*                               TO 1083                         *
001700* NOTES:    NONE                                                *
001800*                                                               *
001900*                                                               *
002000*****************************************************************
002100 IDENTIFICATION DIVISION.
002200*-----------------------*
002300 PROGRAM-ID.  IUIP9103.
002400 INSTALLATION. BSCA.
002500 DATE-WRITTEN.
002600     MARCH 1996.
002700 DATE-COMPILED.
002800     TODAY.
002900
003000 ENVIRONMENT DIVISION.
003100*--------------------*
003200 CONFIGURATION SECTION.
003300 SOURCE-COMPUTER. IBM-370.
003400 OBJECT-COMPUTER. IBM-370.
003500 EJECT
003600
003700 INPUT-OUTPUT SECTION.
003800
003900 FILE-CONTROL.
004000
004100
004200 DATA DIVISION.
004300
004400 FILE SECTION.
004500
004600
004700*-------------*
004800 WORKING-STORAGE SECTION.
004900*-----------------------*
005000
005100 01  WS-MODULE-DISPLAY.                                                 89
005200     05  WS-MODULE-ID            PIC X(09) VALUE 'IUIP9103-'.
005300     05  WS-MODULE-DESCRIPTION   PIC X(50) VALUE
005400         'ALIM CALLER MODULE'.                                          89
005500     05  FILLER                  PIC X(09) VALUE 'VERSION #'.
005600     05  WS-VERSION-NUMBER       PIC X(04) VALUE '11.0'.          C-021215
005700     05  FILLER                  PIC X(13) VALUE
005800         'RELEASED ON '.
C99635     05  WS-RELEASE-DATE         PIC X(08) VALUE '01/13/05'.      CR99635
006000
006100*--------------------------------------------------------------*
006200*            WORK AREAS                                        *
006300*--------------------------------------------------------------*
006400
006500 01  FILLER.
006600     05  MOD-NAME                PIC X(8) VALUE 'IUIP9103'.
006700     05  MOD-LEVEL               PIC 99 COMP VALUE 1.
006800     05  ALIM                    PIC X(08) VALUE 'IULP9102'.
006900     05  IUIP6109                PIC X(08) VALUE 'IUIP6109'.
006910     05  IUIP9115                PIC X(08) VALUE 'IUIP9115'.
007000     05  IUIP9116                PIC X(08) VALUE 'IUIP9116'.
161884     05  WS-PARTKEY              PIC X(08) VALUE 'PARTKEY'.
161884     05  WS-ITDF51S1             PIC X(08) VALUE 'ITDF51S1'.
007100     05  CALL-WHO                PIC X(08).
007200     05  FUNC-INSERT             PIC X(04) VALUE 'ISRT'.
007200     05  GU-CALL                 PIC X(04) VALUE 'GU  '.
C13018 01  WS-MISC.
C13018     05  SUB                     PIC 999   COMP.                  01000064
C13018     05  SUB1                    PIC 999   COMP.                  01000064
C13018     05  SUB2                    PIC 999   COMP.                  01000064
C14456     05  SUB3                    PIC 999   COMP.                  01000064
143336     05  SUB4                    PIC 999   COMP.                  01000064
161884     05  ERR-SUB                 PIC 999   COMP.                  01000064
164645     05  WS-RSN-CD-OCC           PIC 9(01) VALUE ZERO.
164645     05  WS-RSN-CD-OCC1          PIC 9(01) VALUE ZERO.
C13018     05 WS-UE069-MSG-FLAG        PIC X(1).                        00940064
C13018         88  W88-UE069-PRESENT          VALUE 'Y'.                00950064
ITS102     05 WS-UE062-MSG-FLAG        PIC X(1).                        00940064
ITS102         88  W88-UE062-PRESENT          VALUE 'Y'.                00950064
133764     05 WS-UE098-MSG-FLAG        PIC X(1).                        00940064
133764         88  W88-UE098-PRESENT          VALUE 'Y'.                00950064
C13018     05 WS-UD001-MSG-FLAG        PIC X(1).                        00940064
C13018         88  W88-UD001-PRESENT          VALUE 'Y'.                00950064
161884     05 WS-DFDF-FLAG-FOUND       PIC X(1).                        00940064
161884         88  WS-DFDF-FLAG-YES           VALUE 'Y'.                00950064
161884     05 WS-DFDF-ERROR-FLAG       PIC X(1).                        00940064
161884         88  WS-DFDF-ERROR-FOUND        VALUE 'Y'.                00950064
161884     05 WS-DFDF-LOOP-FLAG        PIC X(1).                        00940064
161884         88  DFDF-LOOP-DONE        VALUE 'Y'.                     00950064
164645     05 WS-DFDF-UD001-FLAG        PIC X(1).                       00940064
164645         88  W88-DFDF-UD001-FOUND       VALUE 'Y'.                00950064
161884     05 WS-DFDF-SCCF-ID          PIC 9(17).                       00940064
007210
ITS102 01  WS-CAP-CHRG-INVT-FLAG      PIC X(4).
ITS102     88 W88-CAP-CHRG-INVT      VALUE  '1134','1135','1137',
ITS102                                      '1138','1139','1140' .
ITS102 01  WS-CAP-CHRG-INVES2-FLAG     PIC X(4).
ITS102     88 W88-CAP-CHRG-INVES2    VALUE  '1134','1135','1137',
ITS102                                      '1138','1139','1140' .
ITS102 01  WS-CAP-CHRG-INVES3-FLAG     PIC X(4).
ITS102     88 W88-CAP-CHRG-INVES3    VALUE  '1134', '1135','1137',
ITS102                                      '1138', '1139','1140' .
ITS102 01  WS-CAP-CHRG-INVES4-FLAG     PIC X(4).
ITS102     88 W88-CAP-CHRG-INVES4    VALUE  '1134', '1135','1137',
ITS102                                      '1138', '1139','1140' .
ITS102 01  WS-CAP-CHRG-INVES5-FLAG     PIC X(4).
ITS102     88 W88-CAP-CHRG-INVES5    VALUE  '1134', '1135','1137',
ITS102                                      '1138', '1139','1140' .
        01  WS-HPA-DF-SEG.
            05  WS-HPA-DF-SEQ-NO            PIC 9(005) VALUE ZEROS.
            05  WS-HPA-DF-DATA.
               10  WS-HPA-DF-KEY.
                  20  WS-HPA-DF-SFSER       PIC X(017) VALUE ZEROS.
                  20  WS-HPA-DF-TYPE        PIC X(006) VALUE ZEROS.
                  20  WS-HPA-DF-SEG-SEQ-NO  PIC X(002) VALUE ZEROS.
               10  WS-HPA-DF-DATA2          PIC X(230) VALUE ZEROS.

       01  DLI-CALL.
           05  PCB-AREA.
               10  PCB-DBD-NM              PIC  X(08).
               10  PCB-SEGM-LEV            PIC  X(02).
               10  PCB-DLI-RET-CD          PIC  X(02).
               10  PCB-DLI-PROC-OPT        PIC  X(04).
               10  PCB-RESERVE-DLI         PIC  X(04).
               10  PCB-SEGM-NM-FB          PIC  X(08).
               10  PCB-LEN-FB-KEY          PIC  X(04).
               10  PCB-NO-SENS-SEG         PIC  X(04).
               10  PCB-KEY-FB-AREA.
                   15  PCB-GRP-ID-KEY      PIC  X(46).
           05  SSA-QUAL-DF-ROOT.
               10  SSA-DF-SEGM-NM         PIC  X(08)  VALUE 'IUCSGDSK'.
               10  SSA-DF-L-ASTK          PIC  X(01)  VALUE '*'.
               10  SSA-DF-L-CC1           PIC  X(01)  VALUE '-'.
               10  SSA-DF-L-CC2           PIC  X(01)  VALUE '-'.
               10  SSA-DF-L-PAREN         PIC  X(01)  VALUE '('.
               10  SSA-DF-KEY-NM          PIC  X(08)  VALUE 'IUCSKDSK'.
               10  SSA-DF-RO              PIC  X(02)  VALUE '= '.
               10  SSA-DF-ITS-SER-NO      PIC  X(17).
               10  SSA-DF-R-PAREN         PIC  X(01)  VALUE ')'.
           05  SSA-UNQUAL-DF-SEG.
               10  SSA-DF-SEGM-NM         PIC  X(08)  VALUE 'IUCSGDDK'.
               10  SSA-DF-CMD             PIC  X(01)  VALUE ' '.

007210
007300 01  WS-WHICH-AUDIT          PIC X.
007400
008100 01  ERROR-HANDLER-INTERFACE.               COPY IUIERRHC.
008200
008210 01  IULIACSP-INTERFACE.                    COPY IULIACSP.        A-961014
008211
008220 01  IULIACOP-INTERFACE.                    COPY IULIACOP.        A-961014
161884
161884 01  WS-ITCCSAPI.                           COPY ITCCSAPI.        A-961014
161884
161884 COPY ITDF51X1.                                                   A-961014
161884
161884 01  PART-ALGO-PASS-AREA.
161884     05 PART-KEY-NUM                        PIC  X(03).
161884     05 PART-SCCF-ID                        PIC  X(17).
161884     05 FILLER                              PIC  X(04).
161884
161884 01  WS-FDB-PASSING-AREA.                   COPY ITCIADBK.        A-961014
008300*--------------------------------------------------------------*
008400*            LINKAGE SECTION                                   *
008500*--------------------------------------------------------------*
008600 LINKAGE SECTION.
008700 01  ADJUDICATION-CONTROL-AREA.             COPY IUIIAADJ.
008710 01  INTERP-SCRATCH-PAD.                    COPY IUISCRPA.        A-961014
008800 01  CLAIM-AREA-01.                         COPY VSCPCLXC.
008900****01  CLAIM-AREA-02.                         COPY VCLMNTEC.
      *---- CLAIM-AREA-02 HOLDS ITS NOTES (624 BYTES) FOLLOWED BY
      *---- ALL CLAIM NOTES
008900 01  CLAIM-AREA-02.
           02  NT-FILLER.
               COPY S9CPNTEC.
           02  NT-97-FILLER        REDEFINES NT-FILLER.
               COPY VCLMNTEC.
       01  CLAIM-AREA-03.                         COPY VSCPCL2C.        CX3988.J
009100 01  CLAIM-AREA-04                          PIC X.
009200 01  PLAN-OPTIONS.                          COPY IUITLOPC.
009300 01  PROCESS-CONTROL-INTERFACE.             COPY ITSIAPC.
009400 01  LIL-ALIM-INTERFACE.                    COPY IULIALM.
009500 01  ALIM-TO-HPA-INTERFACE.                 COPY IUCIAAJR.
009600 01  UPF-INPUT-INTERFACE.                   COPY ITCIACI.
009700 01  UPF-OUTPUT-INTERFACE.                  COPY ITCIACO.
009800 01  UPF-PRICING-INTERFACE.                 COPY ITCIACP.
009900 01  HPA-TO-ALIM-INTERFACE.                 COPY IUCIADR.
010000 01  PLAN-DEFINED-INTERFACE.                COPY IUCIAPF.
010010 01  UPF-COB-CAL-INPUT.                     COPY ITCIACIP.        A-011001
010010 01  UPF-COB-CAL-OUTPUT.                    COPY ITCIACOP.        A-011001
010100 01  PCB-ALIM-WORK-CLAIM.                   COPY IULPCI00.
010200 01  PCB-ALIM-RECON.                        COPY IULPCF00.
010400 01  PCB-HPA-DF.                            COPY IUCPCD00.
010500 01  PCB-HPA-RECON.                         COPY IUCPCF00.
010600 01  PCB-PLAN-PROFILE.                      COPY ITCPC600.
010700 01  PCB-UPF-RECON.                         COPY ITCPCF00.
010800 01  PCB-UPF-RULES-GS.                      COPY ITCPCE00.
010810 01  PCB-ITS-PARAMETER.   COPY ITCPCP00.                          A-970104
010811 01  PCB-ITS-OEC-SUSP.    COPY ITCPCQ00.                          A-970109
010820 01  PCB-ITS-FORMAT-00.   COPY ITCPCK00.                          A-970104
010830 01  PCB-ITS-FORMAT-01.   COPY ITCPCK01.                          A-970104
010840 01  PCB-ITS-FORMAT-02.   COPY ITCPCK02.                          A-970104
010850 01  PCB-ITS-FORMAT-03.   COPY ITCPCK03.                          A-970104
010860 01  PCB-ITS-FORMAT-04.   COPY ITCPCK04.                          A-970104
010870 01  PCB-ITS-FORMAT-05.   COPY ITCPCK05.                          A-970104
010880 01  PCB-ITS-FORMAT-06.   COPY ITCPCK06.                          A-970104
010890 01  PCB-ITS-FORMAT-07.   COPY ITCPCK07.                          A-970104
010891 01  PCB-ITS-FORMAT-08.   COPY ITCPCK08.                          A-970104
010892 01  PCB-ITS-FORMAT-09.   COPY ITCPCK09.                          A-970104
010900 01  PCB-IUIDBA00-L.                        COPY IUIPCA00.
C96360 01  PCB-ITCDBD09-AEADB.                    COPY ITCPCD09.
011000 PROCEDURE DIVISION    USING
011100               ADJUDICATION-CONTROL-AREA
011110               INTERP-SCRATCH-PAD                                 A-961014
011200               CLAIM-AREA-01
011300               CLAIM-AREA-02
011400               CLAIM-AREA-03
011500               CLAIM-AREA-04
011600               PLAN-OPTIONS
011700               PROCESS-CONTROL-INTERFACE
011800               LIL-ALIM-INTERFACE
011900               ALIM-TO-HPA-INTERFACE
012000               UPF-INPUT-INTERFACE
012100               UPF-OUTPUT-INTERFACE
012200               UPF-PRICING-INTERFACE
012300               HPA-TO-ALIM-INTERFACE
012400               PLAN-DEFINED-INTERFACE
012400               UPF-COB-CAL-INPUT                                  A-011001
012400               UPF-COB-CAL-OUTPUT                                 A-011001
012500               PCB-ALIM-WORK-CLAIM
012600               PCB-ALIM-RECON
012800               PCB-HPA-DF
012900               PCB-HPA-RECON
013000               PCB-PLAN-PROFILE
013100               PCB-UPF-RECON
013200               PCB-UPF-RULES-GS
013210               PCB-ITS-PARAMETER                                  A-970104
013211               PCB-ITS-OEC-SUSP                                   A-970109
013220               PCB-ITS-FORMAT-00                                  A-970104
013230               PCB-ITS-FORMAT-01                                  A-970104
013240               PCB-ITS-FORMAT-02                                  A-970104
013250               PCB-ITS-FORMAT-03                                  A-970104
013260               PCB-ITS-FORMAT-04                                  A-970104
013270               PCB-ITS-FORMAT-05                                  A-970104
013280               PCB-ITS-FORMAT-06                                  A-970104
013290               PCB-ITS-FORMAT-07                                  A-970104
013291               PCB-ITS-FORMAT-08                                  A-970104
013292               PCB-ITS-FORMAT-09                                  A-970104
013300               PCB-IUIDBA00-L
C96360               PCB-ITCDBD09-AEADB.
013400
013500
013600 00000-MAINLINE.
013700
013800     PERFORM 10000-INITIALIZATION THRU 10000-EXIT.
013900     PERFORM 20000-PROCESSING   THRU 20000-EXIT.
014000
014100 00000-EXIT.
014200     GOBACK.
014300 EJECT
014400 10000-INITIALIZATION.
014500     MOVE MOD-NAME   TO SIAPC-MODULE-NAME (MOD-LEVEL).
014600     MOVE MOD-LEVEL  TO SIAPC-MODULE-LEVEL.
C13018     INITIALIZE SUB SUB1 SUB2 SUB3 .                              C414043L
C13018     MOVE 'N' TO WS-UE069-MSG-FLAG.                               C414043L
ITS102     MOVE 'N' TO WS-UE062-MSG-FLAG.                               C414043L
C13018     MOVE 'N' TO WS-UD001-MSG-FLAG.                               C414043L
133764     MOVE 'N' TO WS-UE098-MSG-FLAG.                               C414043L
ITS102     MOVE      SPACES         TO WS-CAP-CHRG-INVT-FLAG.           CR80712
ITS102     MOVE      SPACES         TO WS-CAP-CHRG-INVES2-FLAG.         CR80712
ITS102     MOVE      SPACES         TO WS-CAP-CHRG-INVES3-FLAG.         CR80712
ITS102     MOVE      SPACES         TO WS-CAP-CHRG-INVES4-FLAG.         CR80712
ITS102     MOVE      SPACES         TO WS-CAP-CHRG-INVES5-FLAG.         CR80712
014700
014900 10000-EXIT.  EXIT.
015000 EJECT
015100 20000-PROCESSING.
015200
015300     MOVE IAADJ-ORIG-SCCF-SERIAL-NO TO
015400                                   CIACI-SCCF-SER-NUM             C-970104
015500                                   CIACO-SCCF-SER-NUM             C-970104
015600                                   CIACP-SCCF-SER-NUM             C-970104
015700                                   CIAAJR-SCCF-SER-NUM            C-970104
015800                                   CIADR-1A-SCCF-SER-NUM          C-970104
015900                                   CIAPF-SCCF-SER-NUM             C-970104
016000                                   LIALM-SCCF-SER-NUM.            C-970104
016100
016200     IF LIL-INTERPRETER-AUD-FILE-1-Y
016300         MOVE 'P' TO WS-WHICH-AUDIT
016400         PERFORM 20025-ADD-AUDIT-SEGMENT THRU 20025-EXIT
016410         IF IAADJ-ERROR-STATUS-FATAL
016420            GO TO 20000-EXIT.
016500
016510     MOVE SCRPA-SKIP-ERROR-TABLE TO CIAAJR-CLM-REASON-CDS.
016510*#*# DISPLAY 'CIAAJR-CLM-REA  ' CIAAJR-CLM-REASON-CDS.
JOEP  *    DISPLAY 'SIAPC: ' PROCESS-CONTROL-INTERFACE.
JOEP  *    DISPLAY '9103 LIALM: ' LIL-ALIM-INTERFACE.
JOEP  *    DISPLAY '9103 CIAJR: ' ALIM-TO-HPA-INTERFACE.
69227
69227      IF IAADJ-FUNCTION = 'IN'
              PERFORM 80000-CHECK-DF-EXTRACT THRU 80000-EXIT
              IF IERRH-ERROR-NUMBER = 830
                 GO TO 20000-EXIT.
C99635                                                                  CR99635
161884     IF IAADJ-FUNCTION = 'FN' AND WF-TRAN-FLG = 'J'
161884        PERFORM 80000-CHECK-DF-EXTRACT THRU 80000-EXIT
161884        IF IERRH-ERROR-NUMBER = 830 OR 575 OR 576
161884           GO TO 20000-EXIT.
C99635                                                                  CR99635
159805     IF IAADJ-FUNCTION = 'DL' AND
159805        (IAADJ-ADJUSTMENT-REASON = '009' OR '013' OR '014')
159805     THEN
159805        PERFORM 80000-CHECK-DF-EXTRACT THRU 80000-EXIT
159805        IF IERRH-ERROR-NUMBER  NOT = 830
159805        THEN
159805           MOVE 831    TO IERRH-ERROR-NUMBER
159805           MOVE 'F'    TO IAADJ-ERROR-STATUS
159805           PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
159805           GO TO 20000-EXIT
159805        ELSE
159805           MOVE ZERO   TO IERRH-ERROR-NUMBER
159805           MOVE SPACES TO IAADJ-ERROR-STATUS
159805        END-IF
159805     END-IF.
C99635                                                                  CR99635
C99635     IF IAADJ-FUNCTION      = 'FN'   AND                          CR99635
C99635        SCRPA-DUMMY-FINAL   = 'DFIN'                              CR99635
C99635       MOVE 'UD001'                TO CIAAJR-CLM-REASON-CD  (1)   CR99635
C99635       MOVE 'UD001'                TO SCRPA-SKIP-ERROR      (1)   CR99635
C99635       MOVE 'UD001'                TO CIACO-CLM-RSN-CD      (1)   CR99635
C99635     END-IF.                                                      CR99635
C99635                                                                  CR99635
C97522***  PASS THE UE002 TO ALIM TO TELL IT TO IGNORE THIS ERROR CODE
C97522     IF CIAAJR-CLM-REASON-CD (1) = SPACES
C97522        MOVE 'UE002' TO CIAAJR-CLM-REASON-CD (1)
C97522     ELSE
C97522        IF CIAAJR-CLM-REASON-CD (2) = SPACES
C97522           MOVE 'UE002' TO CIAAJR-CLM-REASON-CD (2)
C97522        ELSE
C97522           IF CIAAJR-CLM-REASON-CD (3) = SPACES
C97522              MOVE 'UE002' TO CIAAJR-CLM-REASON-CD (3)
C97522           ELSE
C97522              IF CIAAJR-CLM-REASON-CD (4) = SPACES
C97522                 MOVE 'UE002' TO CIAAJR-CLM-REASON-CD (4)
C97522              ELSE
C97522                 IF CIAAJR-CLM-REASON-CD (5) = SPACES
C97522                    MOVE 'UE002' TO CIAAJR-CLM-REASON-CD (5).

C13018***  ITS9.4 PASS UE069   TO IGNORE THIS ERROR CODE
C13018     IF CIAAJR-CLM-REASON-CD (1) = SPACES
C13018        MOVE 'UE069' TO CIAAJR-CLM-REASON-CD (1)
C13018     ELSE
C13018        IF CIAAJR-CLM-REASON-CD (2) = SPACES
C13018           MOVE 'UE069' TO CIAAJR-CLM-REASON-CD (2)
C13018        ELSE
C13018           IF CIAAJR-CLM-REASON-CD (3) = SPACES
C13018              MOVE 'UE069' TO CIAAJR-CLM-REASON-CD (3)
C13018           ELSE
C13018              IF CIAAJR-CLM-REASON-CD (4) = SPACES
C13018                 MOVE 'UE069' TO CIAAJR-CLM-REASON-CD (4)
C13018              ELSE
C13018                 IF CIAAJR-CLM-REASON-CD (5) = SPACES
C13018                    MOVE 'UE069' TO CIAAJR-CLM-REASON-CD (5).


ITS102***  ITS10.2 PASS UE062   TO IGNORE THIS ERROR CODE
ITS102     IF IAADJ-FUNCTION-FINALIZE
ITS102      IF CIAAJR-CLM-REASON-CD (1) = SPACES
ITS102        MOVE 'UE062' TO CIAAJR-CLM-REASON-CD (1)
ITS102      ELSE
ITS102        IF CIAAJR-CLM-REASON-CD (2) = SPACES
ITS102           MOVE 'UE062' TO CIAAJR-CLM-REASON-CD (2)
ITS102        ELSE
ITS102           IF CIAAJR-CLM-REASON-CD (3) = SPACES
ITS102              MOVE 'UE062' TO CIAAJR-CLM-REASON-CD (3)
ITS102           ELSE
ITS102              IF CIAAJR-CLM-REASON-CD (4) = SPACES
ITS102                 MOVE 'UE062' TO CIAAJR-CLM-REASON-CD (4)
ITS102              ELSE
ITS102                 IF CIAAJR-CLM-REASON-CD (5) = SPACES
ITS102                    MOVE 'UE062' TO CIAAJR-CLM-REASON-CD (5).

133764***  ITS15.0  PASS UE098   TO IGNORE THIS ERROR CODE
133764     IF CIAAJR-CLM-REASON-CD (1) = SPACES
133764        MOVE 'UE098' TO CIAAJR-CLM-REASON-CD (1)
133764     ELSE
133764        IF CIAAJR-CLM-REASON-CD (2) = SPACES
133764           MOVE 'UE098' TO CIAAJR-CLM-REASON-CD (2)
133764        ELSE
133764           IF CIAAJR-CLM-REASON-CD (3) = SPACES
133764              MOVE 'UE098' TO CIAAJR-CLM-REASON-CD (3)
133764           ELSE
133764              IF CIAAJR-CLM-REASON-CD (4) = SPACES
133764                 MOVE 'UE098' TO CIAAJR-CLM-REASON-CD (4)
133764              ELSE
133764                 IF CIAAJR-CLM-REASON-CD (5) = SPACES
133764                    MOVE 'UE098' TO CIAAJR-CLM-REASON-CD (5).


C14456     IF IAADJ-FUNCTION-LINE AND WF-CLM-TOT-AMT  = 0
291016       IF (WF-NT-INVT-CD  = '1083'
291016          OR  CHN-INVES2  = '1083'
291016          OR  CHN-INVES3  = '1083'
291016          OR  CHN-INVES4  = '1083'
291016          OR  CHN-INVES5  = '1083')
291016         IF CIAAJR-DF-MSG-CD-LN (1)  = SPACES
291016            MOVE '1083' TO CIAAJR-DF-MSG-CD-LN (1)
291016         END-IF
291016       ELSE
C14456         IF CIAAJR-DF-MSG-CD-LN (1)  = SPACES
C14456           MOVE '0291' TO CIAAJR-DF-MSG-CD-LN (1)
C14456         END-IF
291016       END-IF
C14456     END-IF.
C14456     MOVE       WF-NT-INVT-CD  TO WS-CAP-CHRG-INVT-FLAG.          CR80712
C14456     MOVE       CHN-INVES2     TO WS-CAP-CHRG-INVES2-FLAG.        CR80712
C14456     MOVE       CHN-INVES3     TO WS-CAP-CHRG-INVES3-FLAG.        CR80712
C14456     MOVE       CHN-INVES4     TO WS-CAP-CHRG-INVES4-FLAG.        CR80712
C14456     MOVE       CHN-INVES5     TO WS-CAP-CHRG-INVES5-FLAG.        CR80712
C14456     IF WF-OOAPRCLS = '3' OR
C14456             ( W88-CAP-CHRG-INVT  OR                              CR80712
C14456             W88-CAP-CHRG-INVES2      OR                          CR80712
C14456             W88-CAP-CHRG-INVES3      OR                          CR80712
C14456             W88-CAP-CHRG-INVES4      OR                          CR80712
C14456             W88-CAP-CHRG-INVES5      )                           CR36988
147766      OR (  (VLCSF-LCLCC-1 = '1022' OR '1023' OR '1024' )
147766         OR (VLCSF-LCLCC-2 = '1022' OR '1023' OR '1024' )
147766         OR (VLCSF-LCLCC-3 = '1022' OR '1023' OR '1024' )
147766         OR (VLCSF-LCLCC-4 = '1022' OR '1023' OR '1024' )
147766         OR (VLCSF-LCLCC-5 = '1022' OR '1023' OR '1024' )
147766              )
C14456      PERFORM VARYING SUB3     FROM 1 BY 1
C14456        UNTIL SUB3 > 5
C14456       IF CIAAJR-DF-MSG-CD-CLM (SUB3) = '0291'
C14456         MOVE '1257'  TO CIAAJR-DF-MSG-CD-CLM (SUB3)
C14456       END-IF
C14456      END-PERFORM
C14456      IF IAADJ-FUNCTION-LINE
C14456       IF CIAAJR-DF-MSG-CD-LN (1)  = '0291'
C14456          MOVE '1257' TO CIAAJR-DF-MSG-CD-LN (1)
C14456       END-IF
C14456      END-IF
C14456      IF IAADJ-FUNCTION-FINALIZE  AND CIAAJR-PAY-OVR-APPLIED
C14456       PERFORM VARYING SUB3 FROM 1 BY 1                           02440064
C14456        UNTIL SUB3 > CIAAJR-TBL-ENTRY-CNT                         02440064
C14456        IF CIAAJR-DF-MSG-CD-LN (SUB3)  = '0291'
C14456          MOVE '1257' TO CIAAJR-DF-MSG-CD-LN (SUB3)
C14456        END-IF
C14456       END-PERFORM                                                02440064
C14456      END-IF
C14456     END-IF.
016550     MOVE ZERO TO CIACOP-CLM-STAT-CD.                             ITS8.4

164645     MOVE SPACES               TO WS-DFDF-UD001-FLAG
164645     IF (WF-NT-INVT-CD = 'DFDF' OR
164645        CHN-INVES2    = 'DFDF' OR
164645        CHN-INVES3    = 'DFDF' OR
164645        CHN-INVES4    = 'DFDF' OR
164645        CHN-INVES5    = 'DFDF' ) AND
164645        IAADJ-FUNCTION-FINALIZE
164645         IF CIAAJR-CLM-REASON-CD (1) = 'UD001'  OR
164645            CIAAJR-CLM-REASON-CD (2) = 'UD001'  OR
164645            CIAAJR-CLM-REASON-CD (3) = 'UD001'  OR
164645            CIAAJR-CLM-REASON-CD (4) = 'UD001'  OR
164645            CIAAJR-CLM-REASON-CD (5) = 'UD001'
164645             SET W88-DFDF-UD001-FOUND   TO TRUE
164645         END-IF
164645         IF NOT W88-DFDF-UD001-FOUND
164645            EVALUATE TRUE
164645                WHEN CIAAJR-CLM-REASON-CD (1) = SPACES
164645                     MOVE 'UD001' TO CIAAJR-CLM-REASON-CD (1)
164645                WHEN CIAAJR-CLM-REASON-CD (2) = SPACES
164645                     MOVE 'UD001' TO CIAAJR-CLM-REASON-CD (2)
164645                WHEN CIAAJR-CLM-REASON-CD (3) = SPACES
164645                     MOVE 'UD001' TO CIAAJR-CLM-REASON-CD (3)
164645                WHEN CIAAJR-CLM-REASON-CD (4) = SPACES
164645                     MOVE 'UD001' TO CIAAJR-CLM-REASON-CD (4)
164645                WHEN CIAAJR-CLM-REASON-CD (5) = SPACES
164645                     MOVE 'UD001' TO CIAAJR-CLM-REASON-CD (5)
164645             END-EVALUATE
164645         END-IF
164645     END-IF

481439     IF CIAAJR-VOID-REISSUE-CLOSE
720462        AND CIAAJR-REJECTED
481439       MOVE 0   TO CIACI-TBL-ENTRY-CNT
481439       MOVE 0   TO CIACP-TBL-ENTRY-CNT
481439       MOVE 0   TO CIAAJR-TBL-ENTRY-CNT
481439       MOVE 0   TO CIACI-TOT-CHRG
481439       MOVE 'A' TO CIAAJR-PROCESS-CD                              A-000815
481439     END-IF
481439*
01651 *    DISPLAY 'CIAAJR-CLM-REA  ' CIAAJR-CLM-REASON-CDS.
016600     CALL ALIM           USING
016700                         PROCESS-CONTROL-INTERFACE
016800                         LIL-ALIM-INTERFACE
016900                         UPF-INPUT-INTERFACE
017000                         ALIM-TO-HPA-INTERFACE
017100                         UPF-PRICING-INTERFACE
012400                         UPF-COB-CAL-INPUT                        A-011001
017200                         UPF-OUTPUT-INTERFACE
012400                         UPF-COB-CAL-OUTPUT                       A-011001
017300                         HPA-TO-ALIM-INTERFACE
017400                         PLAN-DEFINED-INTERFACE
017420                         IULIACSP-INTERFACE                       A-961014
017430                         IULIACOP-INTERFACE                       A-961014
017500                         PCB-ALIM-WORK-CLAIM
017600                         PCB-ALIM-RECON
017800                         PCB-HPA-DF
017900                         PCB-HPA-RECON
018000                         PCB-PLAN-PROFILE
018100                         PCB-UPF-RECON
018200                         PCB-UPF-RULES-GS                         C-970104
018210                         PCB-ITS-PARAMETER                        A-970104
018211                         PCB-ITS-OEC-SUSP                         A-970109
C96360                         PCB-ITCDBD09-AEADB
018220                         PCB-ITS-FORMAT-00                        A-970104
018230                         PCB-ITS-FORMAT-01                        A-970104
018240                         PCB-ITS-FORMAT-02                        A-970104
018250                         PCB-ITS-FORMAT-03                        A-970104
018260                         PCB-ITS-FORMAT-04                        A-970104
018270                         PCB-ITS-FORMAT-05                        A-970104
018280                         PCB-ITS-FORMAT-06                        A-970104
018290                         PCB-ITS-FORMAT-07                        A-970104
018291                         PCB-ITS-FORMAT-08                        A-970104
018292                         PCB-ITS-FORMAT-09.                       A-970104
018300
JOEP  *    DISPLAY 'AFTER-SIAPC : ' SIAPC-RETURN-DATA.
JOEP  *    DISPLAY 'AFTER-CIACO : ' UPF-OUTPUT-INTERFACE.
JOEP  *    DISPLAY 'AFTER-CIACOP: ' UPF-COB-CAL-OUTPUT.
161884* WITH I/R=DFDF, WE IGNORE THE UE011 ERROR AND PROCEED WITH LINE
161884* ITEM PRICING.
161884*    IF WF-NT-INVT-CD = 'DFDF' OR
161884*       CHN-INVES2 = 'DFDF' OR
161884*       CHN-INVES3 = 'DFDF' OR
161884*       CHN-INVES4 = 'DFDF' OR
161884*       CHN-INVES5 = 'DFDF'
161884*
161884*     PERFORM 80700-RESET-DFDF     THRU 80700-EXIT
161884*
161884*    END-IF.
JOEP  *    DISPLAY 'AFT12-SIAPC : ' SIAPC-RETURN-DATA.
JOEP  *    DISPLAY 'AFT12-CIACO : ' UPF-OUTPUT-INTERFACE.
JOEP  *    DISPLAY 'AFT12-CIACOP: ' UPF-COB-CAL-OUTPUT.
164645     IF ( WF-NT-INVT-CD = 'DFDF' OR
164645        CHN-INVES2    = 'DFDF' OR
164645        CHN-INVES3    = 'DFDF' OR
164645        CHN-INVES4    = 'DFDF' OR
164645        CHN-INVES5    = 'DFDF' ) AND
164645        IAADJ-FUNCTION-FINALIZE
164645         IF W88-DFDF-UD001-FOUND
164645            CONTINUE
164645         ELSE
164645            EVALUATE TRUE
164645                WHEN CIAAJR-CLM-REASON-CD (1) = 'UD001'
164645                     MOVE SPACES  TO CIAAJR-CLM-REASON-CD (1)
164645                WHEN CIAAJR-CLM-REASON-CD (2) = 'UD001'
164645                     MOVE SPACES  TO CIAAJR-CLM-REASON-CD (2)
164645                WHEN CIAAJR-CLM-REASON-CD (3) = 'UD001'
164645                     MOVE SPACES  TO CIAAJR-CLM-REASON-CD (3)
164645                WHEN CIAAJR-CLM-REASON-CD (4) = 'UD001'
164645                     MOVE SPACES  TO CIAAJR-CLM-REASON-CD (4)
164645                WHEN CIAAJR-CLM-REASON-CD (5) = 'UD001'
164645                     MOVE SPACES  TO CIAAJR-CLM-REASON-CD (5)
164645            END-EVALUATE
164645            EVALUATE TRUE
164645                WHEN CIACO-CLM-RSN-CD (1)  = 'UD001'
164645                     MOVE SPACES  TO CIACO-CLM-RSN-CD (1)
164645                WHEN CIACO-CLM-RSN-CD (2)  = 'UD001'
164645                     MOVE SPACES  TO CIACO-CLM-RSN-CD (2)
164645                WHEN CIACO-CLM-RSN-CD (3)  = 'UD001'
164645                     MOVE SPACES  TO CIACO-CLM-RSN-CD (3)
164645                WHEN CIACO-CLM-RSN-CD (4)  = 'UD001'
164645                     MOVE SPACES  TO CIACO-CLM-RSN-CD (4)
164645                WHEN CIACO-CLM-RSN-CD (5)  = 'UD001'
164645                     MOVE SPACES  TO CIACO-CLM-RSN-CD (5)
164645            END-EVALUATE
164645            PERFORM
164645            VARYING WS-RSN-CD-OCC FROM 1 BY 1
164645               UNTIL WS-RSN-CD-OCC > 5
164645                  IF CIACO-CLM-RSN-CD (WS-RSN-CD-OCC) = SPACES
164645                     ADD 1 TO WS-RSN-CD-OCC GIVING WS-RSN-CD-OCC1
164645                     IF WS-RSN-CD-OCC1 < 6
164645                        MOVE CIACO-CLM-RSN-CD (WS-RSN-CD-OCC1)
164645                             TO CIACO-CLM-RSN-CD (WS-RSN-CD-OCC)
164645                        MOVE SPACES  TO
164645                             CIACO-CLM-RSN-CD (WS-RSN-CD-OCC1)
164645                     END-IF
164645                  END-IF
164645            END-PERFORM
164645         END-IF
164645     END-IF
018310** WHEN MEDICARE CLAIM, IGNORE THE DF SUPPRESSION ERROR UD001     A-021215
018320** SINCE WE INSTRUCTED HPA TO SUPPRESS THE DF, WE WILL GENERATE   A-021215
018330** THE DF WITH AN HPA COB OVERRIDE CALL                           A-021215
018340     IF SIAPC-RETURN-IND  = '0' AND                               A-021215
018350        CIACO-CLM-STAT-CD = '2' AND                               A-021215
018360        SCRPA-SKIP-ERROR (1) = 'UD001' AND                        A-021215
018370        CIACO-CLM-RSN-CD (1) = 'UD001' AND                        A-021215
018375        (WF-ITS-MED-TYPE = 'M' OR 'X')                            A-021215
C99635        MOVE SPACES TO CIACO-CLM-RSN-CD (1)                       CR99635
C99635        MOVE SPACES TO SCRPA-SKIP-ERROR (1)                       CR99635
018380        MOVE '0' TO CIACO-CLM-STAT-CD.                            A-021215
018380                                                                  A-021215
C99635     IF SIAPC-RETURN-IND  = '0' AND                               CR99635
C99635        CIACO-CLM-STAT-CD = '2' AND                               CR99635
C99635        SCRPA-SKIP-ERROR (1) = 'UD001'                            CR99635
143336*       CIACO-CLM-RSN-CD (1) = 'UD001'                            CR99635
143336*       MOVE SPACES TO CIACO-CLM-RSN-CD (1)                       CR99635
C99635        MOVE SPACES TO SCRPA-SKIP-ERROR (1)                       CR99635
IR3199        IF IAADJ-FUNCTION      = 'FN'   AND                       CR99635
IR3199          SCRPA-DUMMY-FINAL   = 'DFIN'                            CR99635
IR3199          MOVE SPACES TO  SCRPA-DUMMY-FINAL                       CR99635
IR3199        END-IF                                                    CR99635
143336        PERFORM VARYING SUB4 FROM 1 BY 1
143336          UNTIL SUB4 > 5
143336           IF CIACO-CLM-RSN-CD (SUB4) = 'U602 ' OR 'UD001'
143336             MOVE SPACES TO CIACO-CLM-RSN-CD (SUB4)
143336           END-IF
143336        END-PERFORM
143336        IF CIACO-CLM-RSN-CD (1) = SPACES AND                      CR99635
C99635           CIACO-CLM-RSN-CD (2) = SPACES AND                      CR99635
C99635           CIACO-CLM-RSN-CD (3) = SPACES AND                      CR99635
C99635           CIACO-CLM-RSN-CD (4) = SPACES AND                      CR99635
C99635           CIACO-CLM-RSN-CD (5) = SPACES                          CR99635
C99635             MOVE '0' TO CIACO-CLM-STAT-CD                        CR99635
C99635        END-IF                                                    CR99635
C99635     END-IF.                                                      CR99635
C99635                                                                  CR99635
C13049     IF SIAPC-RETURN-IND  = '0' AND                               CR99635
C13049        CIACO-CLM-STAT-CD = '2' AND                               CR99635
C13049        IAADJ-FUNCTION    = 'FN'   AND
C13049        SCRPA-NONCOB-OVRIDE-FLAG = 'Y'                            CR99635
C13049      IF (CIACO-CLM-RSN-CD (1) = 'UBL01' OR 'UBL02' OR 'UBL03' )  CR99635
C13049          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13049          MOVE SPACES TO CIACO-CLM-RSN-CD (1)                     CR99635
C13049          MOVE SPACES TO SCRPA-SKIP-ERROR (1)                     CR99635
C13049      END-IF                                                      CR99635
C13049      IF (CIACO-CLM-RSN-CD (2) = 'UBL01' OR 'UBL02' OR 'UBL03' )  CR99635
C13049          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13049          MOVE SPACES TO CIACO-CLM-RSN-CD (2)                     CR99635
C13049          MOVE SPACES TO SCRPA-SKIP-ERROR (2)                     CR99635
C13049      END-IF                                                      CR99635
C13049      IF (CIACO-CLM-RSN-CD (3) = 'UBL01' OR 'UBL02' OR 'UBL03' )  CR99635
C13049          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13049          MOVE SPACES TO CIACO-CLM-RSN-CD (3)                     CR99635
C13049          MOVE SPACES TO SCRPA-SKIP-ERROR (3)                     CR99635
C13049      END-IF                                                      CR99635
C13049      IF (CIACO-CLM-RSN-CD (4) = 'UBL01' OR 'UBL02' OR 'UBL03' )  CR99635
C13049          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13049          MOVE SPACES TO CIACO-CLM-RSN-CD (4)                     CR99635
C13049          MOVE SPACES TO SCRPA-SKIP-ERROR (4)                     CR99635
C13049      END-IF                                                      CR99635
C13049      IF (CIACO-CLM-RSN-CD (5) = 'UBL01' OR 'UBL02' OR 'UBL03' )  CR99635
C13049          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13049          MOVE SPACES TO CIACO-CLM-RSN-CD (5)                     CR99635
C13049          MOVE SPACES TO SCRPA-SKIP-ERROR (5)                     CR99635
C13049      END-IF                                                      CR99635
C13049     END-IF.                                                      CR99635
C13049*                                                                 CR99635
C13018     IF SIAPC-RETURN-IND  = '0' AND                               CR99635
C13018        CIACO-CLM-STAT-CD = '2' AND                               CR99635
C13018        IAADJ-FUNCTION    = 'FN'
C13018        MOVE 'N' TO WS-UE069-MSG-FLAG                             C414043L
C13018        MOVE 'N' TO WS-UD001-MSG-FLAG                             C414043L
C13018        PERFORM VARYING SUB  FROM 1 BY 1 UNTIL                    C414043L
C13018          SUB > 5                                                 C414043L
C13018         IF CIACO-CLM-RSN-CD (SUB) =  'UE069'                     C414043L
C13018             SET W88-UE069-PRESENT TO TRUE                        C414043L
C13018             MOVE SUB TO SUB1
C13018         END-IF                                                   C414043L
C13018         IF CIACO-CLM-RSN-CD (SUB) =  'UD001'                     C414043L
C13018             SET W88-UD001-PRESENT TO TRUE                        C414043L
C13018             MOVE SUB TO SUB2
C13018         END-IF                                                   C414043L
C13018        END-PERFORM                                               C414043L
C13018        IF W88-UE069-PRESENT   AND                                CR99635
C13018           W88-UD001-PRESENT                                      CR99635
C13018          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13018          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB1)                  CR99635
C13018          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB2)                  CR99635
C13018          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB1)                  CR99635
C13018          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB2)                  CR99635
C13018        END-IF                                                    CR99635
C13018     END-IF.                                                      CR99635
C13018*                                                                 CR99635
C13018     IF SIAPC-RETURN-IND  = '0' AND                               CR99635
C13018        CIACO-CLM-STAT-CD = '2' AND                               CR99635
C13018        IAADJ-FUNCTION    = 'FN'
C13018        MOVE 'N' TO WS-UE062-MSG-FLAG                             C414043L
C13018        MOVE 'N' TO WS-UD001-MSG-FLAG                             C414043L
C13018        PERFORM VARYING SUB  FROM 1 BY 1 UNTIL                    C414043L
C13018          SUB > 5                                                 C414043L
C13018         IF CIACO-CLM-RSN-CD (SUB) =  'UE062'                     C414043L
C13018             SET W88-UE062-PRESENT TO TRUE                        C414043L
C13018             MOVE SUB TO SUB1
C13018         END-IF                                                   C414043L
C13018         IF CIACO-CLM-RSN-CD (SUB) =  'UD001'                     C414043L
C13018             SET W88-UD001-PRESENT TO TRUE                        C414043L
C13018             MOVE SUB TO SUB2
C13018         END-IF                                                   C414043L
C13018        END-PERFORM                                               C414043L
C13018        IF W88-UE062-PRESENT   AND                                CR99635
C13018           W88-UD001-PRESENT                                      CR99635
C13018          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
C13018          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB1)                  CR99635
C13018          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB2)                  CR99635
C13018          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB1)                  CR99635
C13018          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB2)                  CR99635
C13018        END-IF                                                    CR99635
C13018     END-IF.                                                      CR99635
133764     IF SIAPC-RETURN-IND  = '0' AND                               CR99635
133764        CIACO-CLM-STAT-CD = '2' AND                               CR99635
133764        IAADJ-FUNCTION    = 'FN'
133764        MOVE 'N' TO WS-UE098-MSG-FLAG                             C414043L
133764        MOVE 'N' TO WS-UD001-MSG-FLAG                             C414043L
133764        PERFORM VARYING SUB  FROM 1 BY 1 UNTIL                    C414043L
133764          SUB > 5                                                 C414043L
133764         IF CIACO-CLM-RSN-CD (SUB) =  'UE098'                     C414043L
133764             SET W88-UE098-PRESENT TO TRUE                        C414043L
133764             MOVE SUB TO SUB1
133764         END-IF                                                   C414043L
133764         IF CIACO-CLM-RSN-CD (SUB) =  'UD001'                     C414043L
133764             SET W88-UD001-PRESENT TO TRUE                        C414043L
133764             MOVE SUB TO SUB2
133764         END-IF                                                   C414043L
133764        END-PERFORM                                               C414043L
133764        IF W88-UE098-PRESENT   AND                                CR99635
133764           W88-UD001-PRESENT                                      CR99635
133764          MOVE '0' TO CIACO-CLM-STAT-CD                           CR99635
133764          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB1)                  CR99635
133764          MOVE SPACES TO CIACO-CLM-RSN-CD (SUB2)                  CR99635
133764          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB1)                  CR99635
133764          MOVE SPACES TO SCRPA-SKIP-ERROR (SUB2)                  CR99635
133764        END-IF                                                    CR99635
133764     END-IF.                                                      CR99635
C13018*                                                                 CR99635
018800     IF SIAPC-RETURN-IND NOT = SPACES
018900                        AND NOT = '0'
019000                        AND NOT = '1'
019100                        AND NOT = '2'
019200         IF SIAPC-ABEND-CODE > SPACES
019300             MOVE 043 TO IERRH-ERROR-NUMBER
019400             PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
019410             MOVE 'F' TO IAADJ-ERROR-STATUS
019500         ELSE
019600             MOVE 041 TO IERRH-ERROR-NUMBER
019700             PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
019710             MOVE 'F' TO IAADJ-ERROR-STATUS
019800     ELSE
019900     IF CIACO-CLM-STAT-CD NOT = '0'                               C-970104
020000                       AND NOT = '1'
020100                       AND NOT = '2'
020200                       AND NOT = '4'
020300                       AND NOT = SPACES
020400         MOVE 035 TO IERRH-ERROR-NUMBER
020500         PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
020510         MOVE 'F' TO IAADJ-ERROR-STATUS
020600     ELSE
020700     IF SIAPC-RETURN-IND = 2
020800         MOVE 050 TO IERRH-ERROR-NUMBER
020900         MOVE SIAPC-RETURN-STATUS TO IERRH-DYNAM-FULL (1)
021000         PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
021010         MOVE 'F' TO IAADJ-ERROR-STATUS
021100     ELSE
021110     IF LIL-INTERPRETER-AUD-FILE-1-Y
021120         MOVE 'R' TO WS-WHICH-AUDIT
021130         PERFORM 20025-ADD-AUDIT-SEGMENT THRU 20025-EXIT
021131         IF IAADJ-ERROR-STATUS-FATAL
021132            GO TO 20000-EXIT
021133         END-IF
021134     END-IF
021200     IF (SIAPC-RETURN-IND = 0 AND
021300      (CIACO-CLM-STAT-CD NOT = SPACES                             C-970104
021400                           AND NOT = '0'
021500                           AND NOT = '2'
021600                           AND NOT = '4'  AND                     AHB00015
021903                            (IAADJ-FUNCTION NOT = 'DL')))         AHB00015
021700       OR
021800      (SIAPC-RETURN-IND = 1 AND
021900*******CIACO-CLM-STAT-CD NOT = '1')                               C-970104
021901      (CIACO-CLM-STAT-CD NOT = '1' AND                            AHB00008
021902     (CIACO-CLM-STAT-CD NOT = ' ' AND                             AHB00008
021903                             IAADJ-FUNCTION NOT = 'DL')))         AHB00008
022000        MOVE 042 TO IERRH-ERROR-NUMBER
022100        PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
022110        IF SIAPC-RETURN-IND = 1
022120            PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
022130        END-IF
022200     ELSE
022300     IF (SIAPC-RETURN-IND = 0 AND
022400      CIACO-CLM-STAT-CD = '2' OR '4')                             C-970104
022500      OR
022600      (SIAPC-RETURN-IND = 1 AND
022700      CIACO-CLM-STAT-CD = '1')                                    C-970104
022800        PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT.
022900
023000 20000-EXIT.  EXIT.
023100 20025-ADD-AUDIT-SEGMENT.
023200
023502     CALL IUIP6109 USING
023503               WS-WHICH-AUDIT
023504               ADJUDICATION-CONTROL-AREA
023505               INTERP-SCRATCH-PAD                                 A-961014
023506               CLAIM-AREA-01
023507               CLAIM-AREA-02
023508               CLAIM-AREA-03
023509               CLAIM-AREA-04
023510               PLAN-OPTIONS
023511               PROCESS-CONTROL-INTERFACE
023520               LIL-ALIM-INTERFACE
023530               ALIM-TO-HPA-INTERFACE
023540               UPF-INPUT-INTERFACE
023550               UPF-OUTPUT-INTERFACE
023560               UPF-PRICING-INTERFACE
023570               HPA-TO-ALIM-INTERFACE
023580               PLAN-DEFINED-INTERFACE
023590               PCB-IUIDBA00-L.
024900 20025-EXIT. EXIT.
69227
       80000-CHECK-DF-EXTRACT.
      *
161884*    IF WF-NT-INVT-CD = 'DFDF' OR
161884*       CHN-INVES2 = 'DFDF' OR
161884*       CHN-INVES3 = 'DFDF' OR
161884*       CHN-INVES4 = 'DFDF' OR
161884*       CHN-INVES5 = 'DFDF'
161884*
161884     PERFORM 80500-CHK-DEFLT-FLAG THRU 80500-EXIT.
161884     PERFORM 80600-SET-ERROR-CODE THRU 80600-EXIT.
161884
161884*    END-IF.


      *
           MOVE VLCSF-SFSER              TO   SSA-DF-ITS-SER-NO.
      *
           INITIALIZE                         WS-HPA-DF-SEG.
      *
           CALL  'CBLTDLI'           USING   GU-CALL,
                                             PCB-HPA-DF,
                                             WS-HPA-DF-SEG,
                                             SSA-QUAL-DF-ROOT,
                                             SSA-UNQUAL-DF-SEG.
      *
           MOVE PCB-HPA-DF TO PCB-AREA.
           IF PCB-DLI-RET-CD IS EQUAL TO '  '
              IF WS-HPA-DF-TYPE = 'X2  1A'
                 MOVE 'F' TO IAADJ-ERROR-STATUS
                 MOVE 830 TO IERRH-ERROR-NUMBER
                 IF IAADJ-FUNCTION NOT = 'DL'
                 THEN
                     PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
                 END-IF
              END-IF
           ELSE
              IF PCB-DLI-RET-CD NOT = 'GE'
                 DISPLAY 'VFIUCD00 ERROR: ' PCB-AREA.

       80000-EXIT. EXIT.

161884 80500-CHK-DEFLT-FLAG.
161884
161884     MOVE SPACE                        TO F51-DATA-AREA.
161884     MOVE 'Y'                          TO WS-DFDF-ERROR-FLAG.
161884     MOVE 'N'                          TO WS-DFDF-LOOP-FLAG.
161884     SET CSAPI-ON-LINE-ENV             TO TRUE.
161884     MOVE 100                          TO
161884                                CIADBK-NUMBER-OF-FORMATS-DBS.
161884     MOVE VLCSF-SFSER                  TO WS-DFDF-SCCF-ID.
161884     PERFORM UNTIL DFDF-LOOP-DONE
161884       MOVE WS-DFDF-SCCF-ID              TO F51-SCCF-ID
161884       IF  F51-SCCF-ID         NUMERIC
161884       AND F51-SCCF-ID       > ZEROES
161884          MOVE F51-SCCF-ID               TO PART-SCCF-ID
161884          PERFORM 80510-CALL-PART-KEY-NUM THRU 80510-EXIT
161884       ELSE
161884          GO TO 80500-EXIT
161884       END-IF
161884       MOVE PART-KEY-NUM                     TO F51-PART-KEY-NUM
161884       MOVE 1                                TO F51-DISP-CD
161884       MOVE 'CS02'                           TO F51-BUS-OWNER-ID
161884       MOVE 'SELECT'                         TO CSAPI-COMMAND
161884       MOVE 1                                TO CSAPI-SUB-COMMAND
161884       CALL WS-ITDF51S1                   USING WS-ITCCSAPI
161884                                              F51-DATA-AREA
161884
161884       IF CSAPI-RETURN-CODE             = 0
161884         IF F51-DEFLT-CLM-RSLTN-IND = 'Y'
161884           MOVE 'N'               TO         WS-DFDF-ERROR-FLAG
161884         ELSE
161884           MOVE 'Y'               TO         WS-DFDF-ERROR-FLAG
161884         END-IF
161884       ELSE
161884         MOVE  'Y'                TO         WS-DFDF-LOOP-FLAG
161884       END-IF
161884       ADD   1                    TO         WS-DFDF-SCCF-ID
161884
161884     END-PERFORM.
161884
161884 80500-EXIT. EXIT.
161884*
161884 80510-CALL-PART-KEY-NUM.
161884
161884     CALL WS-PARTKEY    USING PART-ALGO-PASS-AREA
161884                              WS-FDB-PASSING-AREA.
161884
161884 80510-EXIT. EXIT.
161884*
161884 80600-SET-ERROR-CODE.
161884
161884     MOVE 'N'         TO         WS-DFDF-FLAG-FOUND.
161884
161884     IF WF-NT-INVT-CD = 'DFDF' OR
161884        CHN-INVES2 = 'DFDF' OR
161884        CHN-INVES3 = 'DFDF' OR
161884        CHN-INVES4 = 'DFDF' OR
161884        CHN-INVES5 = 'DFDF'
161884
161884      MOVE 'Y'        TO         WS-DFDF-FLAG-FOUND
161884
161884     END-IF.
161884
161884     IF WS-DFDF-ERROR-FOUND
161884      IF WS-DFDF-FLAG-YES
161884       MOVE 575        TO        IERRH-ERROR-NUMBER
161884       MOVE 'F'        TO        IAADJ-ERROR-STATUS
161884       PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
161884      END-IF
161884     ELSE
161884      IF NOT WS-DFDF-FLAG-YES
161884       MOVE 576        TO        IERRH-ERROR-NUMBER
161884       MOVE 'F'        TO        IAADJ-ERROR-STATUS
161884       PERFORM 90086-ERROR-HANDLER THRU 90086-EXIT
161884      END-IF
161884     END-IF.
161884
161884
161884 80600-EXIT. EXIT.
161884
161884 80700-RESET-DFDF.
161884
161884     PERFORM VARYING ERR-SUB FROM 1 BY 1
161884             UNTIL   ERR-SUB > 5
161884       IF CIACO-CLM-RSN-CD (ERR-SUB) = 'UE011'
161884       THEN
161884           MOVE SPACES TO CIACO-CLM-RSN-CD (ERR-SUB)
161884           SET CIACO-SUCCESSFUL TO TRUE
161884           MOVE '0'    TO SIAPC-RETURN-IND
161884           MOVE '0000' TO SIAPC-RETURN-STATUS
161884           MOVE SPACES TO SIAPC-EIB-AREA
161884       END-IF
161884     END-PERFORM.
161884
161884 80700-EXIT. EXIT.
161884
025000 90086-ERROR-HANDLER.
025100*****IF IAADJ-DOING-USEREXIT = 'Y'                                D-961014
025200*****    MOVE IUIP9116 TO CALL-WHO                                D-961014
025300*****ELSE                                                         D-961014
025400*****    MOVE IUIP9115 TO CALL-WHO.                               D-961014
025500*****CALL CALL-WHO USING                                          D-961014
025501     IF IAADJ-DOING-USEREXIT = 'Y'                                A-961014
025510         CALL IUIP9116 USING                                      A-961014
025600               ERROR-HANDLER-INTERFACE
025700               ADJUDICATION-CONTROL-AREA
025710               INTERP-SCRATCH-PAD                                 A-961014
025800               CLAIM-AREA-01
025900               CLAIM-AREA-02
026000               CLAIM-AREA-03
026100               CLAIM-AREA-04
026200               PLAN-OPTIONS
026300               PROCESS-CONTROL-INTERFACE
026400               LIL-ALIM-INTERFACE
026500               ALIM-TO-HPA-INTERFACE
026600               UPF-INPUT-INTERFACE
026700               UPF-OUTPUT-INTERFACE
026800               UPF-PRICING-INTERFACE
026900               HPA-TO-ALIM-INTERFACE
027000               PLAN-DEFINED-INTERFACE
027300      ELSE                                                        A-961014
027400         CALL IUIP9115 USING                                      A-961014
027500               ERROR-HANDLER-INTERFACE                            A-961014
027600               ADJUDICATION-CONTROL-AREA                          A-961014
027610               INTERP-SCRATCH-PAD                                 A-961014
027700               CLAIM-AREA-01                                      A-961014
027800               CLAIM-AREA-02                                      A-961014
027900               CLAIM-AREA-03                                      A-961014
027910               CLAIM-AREA-04                                      A-961014
027920               PLAN-OPTIONS                                       A-961014
027930               PROCESS-CONTROL-INTERFACE                          A-961014
027940               LIL-ALIM-INTERFACE                                 A-961014
027950               ALIM-TO-HPA-INTERFACE                              A-961014
027960               UPF-INPUT-INTERFACE                                A-961014
027970               UPF-OUTPUT-INTERFACE                               A-961014
027980               UPF-PRICING-INTERFACE                              A-961014
027990               HPA-TO-ALIM-INTERFACE                              A-961014
027991               PLAN-DEFINED-INTERFACE                             A-961014
027992               IULIACSP-INTERFACE                                 A-961014
027993               IULIACOP-INTERFACE                                 C-011001
027994               UPF-COB-CAL-OUTPUT.                                A-011001
028000 90086-EXIT. EXIT.
