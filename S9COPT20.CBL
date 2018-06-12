       CBL MAP,XREF(SHORT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    S9COPT20.
       AUTHOR.        MANEESHA.
       INSTALLATION.  ANTHEM.
       DATE-WRITTEN.  DEC 2015.
       DATE-COMPILED.
      ******************************************************************
      *                                                                *
      *                  THE OPT FILE CREATION PROGRAM                 *
      *                *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*           *
      *                                                                *
      *   S9COPT20 READS THE OUTPUT OF THE OPT RSI SELECTION PROGRAM.  *
      *   FOR EACH RECORD, CURRENT CLAIMS HISTORY IS SCANNED.          *
      *   IF THE ADJUDICATION NUMBER ON THE SELECTED CLAIM IS GREATER  *
      *   THAN ZERO, THEN THE PRIOR CLAIMS HISTORY IS SCANNED.         *
      *   THE DEDUCTIBLE, COINSURANCE, LTM DOLLARS ARE CALCULATED AND  *
      *   POPULATED TO THE OUTPUT INTERMEDIATE FILE ALONG WITH THE     *
      *   OTHER FIELD VALUES RETRIEVED FROM THE HISTORY DATABASE.      *
      *   (CURRENT/PRIOR)                                              *
      *----------------------------------------------------------------*
      *                                                                *
      *  REVISION LOG:                                                 *
      *                                                                *
      *  REQUEST #   PROGRAMMER     DATE     DESCRIPTION               *
      *  ----------  ----------   --------   ------------------------- *
      *  CQ 1764548  MANEESHA     12/11/2015 INITIAL RELEASE           *
      *                                                                *
      *  CQ 2269118  MANEESHA     01/22/2016 BYPASS COUNTER LOGIC-CLHST*
      *                                      TAG BY - 226911           *
      *  RTC 468578  REKHA        08/18/2017 LESSER LOGIC UPDATE.      *
      *                                                                *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-3090.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RSI-TRANSACTIONS ASSIGN TO S9CR100.
           SELECT NEXIMPACT-RETURN ASSIGN TO S9CR200.
      *
       DATA DIVISION.
      *
       FILE SECTION.
       FD  RSI-TRANSACTIONS
           BLOCK CONTAINS 0
           LABEL RECORDS STANDARD
           RECORDING MODE IS F.
           COPY S9CRSI03.
      *
       FD  NEXIMPACT-RETURN
           BLOCK CONTAINS 0
           LABEL RECORDS STANDARD
           RECORDING MODE IS F.
       01  RETURN-RECORD PIC X(601).
      *
       WORKING-STORAGE SECTION.
      *
       77  START-WORKING-STORAGE         PIC X(40) VALUE                00660000
           '****** WORKING STORAGE BEGINS HERE *****'.                  00670000
       77  WS-PROGRAM-ID                 PIC X(8) VALUE 'S9COPT20'.     00760001
       77  ABEND-CODE                    PIC 9(4) BINARY.
       77  INPUT-RECORD-COUNT            PIC 9(7) VALUE ZERO.
       77  OUTPUT-RECORD-COUNT           PIC 9(7) VALUE ZERO.
       77  OUTPUT-RECORD-BYPASS          PIC 9(7) VALUE ZERO.
       77  FOURTH-QTR-RECORD-CNT         PIC 9(7) VALUE ZERO.
      *
           COPY S9COPT20.
      *
           COPY PICALLS.
      *
       01  WS-FIELDS.
           05  WS-RUN-DATE               PIC 9(08) VALUE ZEROES.
           05  WF-LINE                   PIC S9(04) COMP VALUE ZEROES.
           05  WS-CLM-ADJ-NO             PIC 9(05) VALUE ZEROES.
           05  WS-DOB                    PIC 9(07) VALUE ZEROES.        00980000
           05  WS-STRT-SVDT              PIC 9(07) VALUE ZEROES.
           05  WS-END-SVDT               PIC 9(07) VALUE ZEROES.
           05  WS-GO-NT35-EFF-DATE       PIC 9(07) VALUE ZEROES.
           05  WS-GO-NT35-TRM-DATE       PIC 9(07) VALUE ZEROES.
           05  WS-INTER-ALT-ID           PIC X(10) VALUE SPACES.
           05  WS-CHK-SUB-ID-LEN         PIC 9(4)  COMP SYNC
                                                   VALUE ZEROES.
           05  WS-CHK-DIGIT              PIC X(03) VALUE SPACES.
           05  WS-CHK-SUB-ID-NO          PIC X(12) VALUE SPACES.
           05  WS-SUB-ID-CKDIG           PIC X(03) VALUE SPACES.
           05  WS-EARLIEST-SSDT          PIC 9(07) VALUE ZEROES.
           05  WS-EARLIEST-DT            PIC 9(08) VALUE ZEROES.
           05  WS-LATEST-ESDT            PIC 9(07) VALUE ZEROES.
           05  WS-CURR-DATE              PIC 9(08) VALUE ZEROES.
           05  WS-DEP-NO                 PIC 9(03) VALUE ZEROES.
           05  WS-PRIOR-DATE             PIC 9(08) VALUE ZEROES.
           05  WS-CURR-ADJ-DTE           PIC 9(08) VALUE ZEROES.
           05  WS-PRIOR-ADJ-DTE          PIC 9(08) VALUE ZEROES.
           05  WS-BRTH-DT                PIC 9(07) VALUE ZEROES.
           05  WS-ADDT                   PIC 9(07) VALUE ZEROES.
           05  WS-EDDT                   PIC 9(07) VALUE ZEROES.
           05  WS-ADDT-PRIOR             PIC 9(07) VALUE ZEROES.
           05  SUB                       PIC S9(04) COMP VALUE ZEROES.  00980000
           05  SUB1                      PIC 9(04) VALUE ZEROES.        00980000
           05  WS-SUB                    PIC 9(05) VALUE ZEROES.
           05  WS-LINE-CNT               PIC 9(04) VALUE ZEROES.
           05  WS-ADJ-NO                 PIC 9(05) VALUE ZEROES.
           05  WS-PKG-NO1                PIC 9(03) VALUE ZEROES.
           05  WS-SEX-REL-CD             PIC X(04) VALUE SPACES.
           05  WS-SUBR-LST-NM            PIC X(15) VALUE SPACES.
           05  WS-SUBR-DPN-LAST-NAME     PIC X(15) VALUE SPACES.
           05  WS-1ST-NM                 PIC X(10) VALUE SPACES.
           05  WS-MBR-MI-NM              PIC X(01) VALUE SPACES.
           05  WS-MEM-DOB                PIC 9(08) VALUE ZEROES.
           05  WS-GENDER-CODE            PIC X(01) VALUE SPACES.
           05  PRIOR-SUB                 PIC S9(5) COMP-3 VALUE ZEROS.
           05  RULE3-VFC                 PIC X(03) VALUE SPACES.
           05  DED1-VFC                  PIC X(03) VALUE SPACES.
           05  DED2-VFC                  PIC X(03) VALUE SPACES.
           05  COPAY-VFC                 PIC X(03) VALUE SPACES.
           05  WS-COINSURANCE-AMOUNT     PIC S9(7)V99 VALUE ZERO.
           05  WS-DEDUCTIBLE-AMOUNT      PIC S9(7)V99 VALUE ZERO.
           05  WS-COPAY-AMOUNT           PIC S9(7)V99 VALUE ZERO.
           05  WS-REIMBURSMENT           PIC S9(7)V99 VALUE ZERO.
           05  WS-OOP-AMT                PIC S9(7)V99 VALUE ZERO.
           05  WS-DED1-STEP-RED          PIC S9(7)V99 VALUE ZERO.
           05  WS-DED2-STEP-RED          PIC S9(7)V99 VALUE ZERO.
           05  WS-COPAY-STEP-RED         PIC S9(7)V99 VALUE ZERO.
           05  S9-PLACE-OF-SERVICE       PIC X(01) VALUE SPACES.
           05  WS-LINE                   PIC S9(4) COMP VALUE ZEROS.
           05 WS-DOS-YEAR                PIC 9(04) VALUE ZEROES.
           05 WS-PREV-YEAR               PIC 9(04) VALUE ZEROES.
           05 WS-RUN-DATE1               PIC 9(04) VALUE ZEROES.
           05 WS-NEXT-YEAR               PIC 9(04) VALUE ZEROES.
           05 WS-LEAP-YEAR               PIC 9(3)V99 VALUE ZEROES.
           05 WS-NT35-BEN-EFF-PRV        PIC 9(02) VALUE ZEROES.
           05 WS-CALENDER-YEAR            PIC X    VALUE 'N'.
              88 OFF-CAL-YEAR-FOUND                VALUE 'Y'.
           05 WS-NOTE41-FOUND             PIC X    VALUE 'N'.
              88  NOTE41-FOUND                     VALUE 'Y'.
           05 WS-CLM-GNT41-DATCHK         PIC X    VALUE 'N'.
              88 CLM-GNT41-DATCHK                  VALUE 'Y'.
           05 WS-NT35-BEN-EFF-MO         PIC X(02) VALUE SPACES.
           05 WS-NT41-DED-FOUND           PIC X    VALUE 'N'.
           05 WS-NT41-DEDI-FOUND          PIC X    VALUE 'N'.
           05 WS-NT41-DEDO-FOUND          PIC X    VALUE 'N'.
           05 WS-NT41-COINS-FOUND         PIC X    VALUE 'N'.
           05 WS-NT41-COINSI-FOUND        PIC X    VALUE 'N'.
           05 WS-NT41-COINSO-FOUND        PIC X    VALUE 'N'.
           05 WS-NT41-COPAY-FOUND         PIC X    VALUE 'N'.
           05 WS-NT41-COPAYI-FOUND        PIC X    VALUE 'N'.
           05 WS-NT41-COPAYO-FOUND        PIC X    VALUE 'N'.
           05 WS-NT41-DEDIX-FOUND         PIC X    VALUE 'N'.
           05 WS-NT41-DEDOX-FOUND         PIC X    VALUE 'N'.
           05 WS-INT-NET-IND             PIC X(01) VALUE SPACES.
           05 WS-FIRST-TIME1              PIC X    VALUE 'N'.
              88  FIRST-TIME1                      VALUE 'Y'.
           05  WS-NON-DENIED-LINE-SW      PIC  X   VALUE 'N'.
               88 NON-DENIED-LINE-FND              VALUE 'Y'.
           05  WS-NON-DENIED-LINE-SW1     PIC  X   VALUE 'N'.
               88 NON-DENIED-LINE-FND1             VALUE 'Y'.
      *
       01  WS-RECORD-TIME.
           05  WS-YEAR                   PIC 9(04) VALUE ZEROES.
           05  WS-DASHE1                 PIC X(01) VALUE '-'.
           05  WS-MONTH                  PIC 9(02) VALUE ZEROES.
           05  WS-DASHE2                 PIC X(01) VALUE '-'.
           05  WS-DATE                   PIC 9(02) VALUE ZEROES.
           05  WS-DASHE3                 PIC X(01) VALUE '-'.
           05  WS-HOUR                   PIC 9(02) VALUE ZEROES.
           05  WS-DASHE4                 PIC X(01) VALUE '.'.
           05  WS-MINUTE                 PIC 9(02) VALUE ZEROES.
           05  WS-DASHE4                 PIC X(01) VALUE '.'.
           05  WS-SECONDS                PIC 9(02) VALUE ZEROES.
           05  WS-DASHE5                 PIC X(01) VALUE '.'.
           05  WS-EXTRA                  PIC 9(06) VALUE ZEROES.
       01  WS-WHEN-COMPILED.                                            00790000
           05  WS-COMP-DATE              PIC X(08).                     00800000
           05  WS-COMP-TIME              PIC X(08).                     00810000
           05  FILLER                    PIC X(04).                     00820000
      *
       01  WS-PKG-NUM                    PIC 9(03).
       01  WS-PKG-NO                     REDEFINES WS-PKG-NUM.
           05  WS-PKGNO                  PIC X(03).
       01  WS-GO-NT41-EFF-DATE           PIC 9(7).
       01  WS-GO-NT41-TRM-DATE           PIC 9(7).
       01  WS-GO-NT41-PKG-NO             PIC X(03).
       01  WS-GO-NT41-VENNAME            PIC X(02) VALUE 'UH'.
       01  WS-GO-NT41-VENTYPE            PIC X(01).
       01  WS-GO-NT41-PLANMD             PIC 9(04) VALUE ZEROES.
       01  WS-DSCH-DT                    PIC 9(7).
       01  WS-IVF-GRP                    PIC X(1)  VALUE 'N'.
       01  PROD-COMB-CD.
           05 PROD-COMB-CD-1-2      PIC X(02).
           05 PROD-COMB-CD-3-4      PIC X(02).
           05 PROD-COMB-CD-5-6      PIC X(02).
       01  WS-SUBSCRIBER-ID.
           05  IP-CS90-SUB-ID            PIC X(12).
           05  WS-IP-SUB-ID  REDEFINES
               IP-CS90-SUB-ID.
               10  WS-SPACES             PIC X(03).
               10  WS-SI-SUBID           PIC X(09).

      *
       01  WS-SAVE-SUBSCRIBER.
           05  WS-SUB-1.
              10  WS-SAVE-SUBSCRIBER-1   PIC 9.
              10  WS-SAVE-SUBSCRIBER-2-9 PIC 9(8).
           05  WS-SUB-2.
              10  WS-SAVE-SUBSCRIBER-1-8 PIC 9(8).
              10  WS-SAVE-SUBSCRIBER-9   PIC X.
           05  WS-SUBID                  PIC X(9).
      *
      *
       01  PCB-TABLE.
           05  CLAIMS-PCB OCCURS 24 TIMES USAGE IS POINTER.
       01  LNCKDIG-LENGTH                PIC 9(4) BINARY VALUE 12.
       01  LNCKDIG-CHECKDIGIT            PIC 9(1).
       01  LNCKDIG-SUBR-ID-KEY.
           03 LNCKDIG-SUBR-NO.
             05  LNCKDIG-SUBR-NO-FIRST   PIC X(3).
             05  LNCKDIG-SUBR REDEFINES LNCKDIG-SUBR-NO-FIRST
                                         PIC 9(3).
             05  LNCKDIG-SUBR-NO-LAST    PIC X(9).
           03 LNCKDIG-SUBR-MDF.
             05  LNCKDIG-SUBR-CK         PIC X(1).
             05  LNCKDIG-SUBR-END        PIC X(2).
       01  CNHISTRY-KEY.
           10 CNHISTRY-KEY-SUBR-ID       PIC X(15).
           10 CNHISTRY-KEY-DEP-NO        PIC 9(3) PACKED-DECIMAL.
           10 CNHISTRY-KEY-CLM-ID        PIC 9(11) PACKED-DECIMAL.
           10 CNHISTRY-KEY-ADJ-NO        PIC 9(5) COMP-3.
       01  CNHISTRY-FUNCTION             PIC X(4) VALUE SPACES.
       01  CNHISTRY-RTN-CD.
           10 CNHISTRY-RTN-LEVEL         PIC X(2).
           10 CNHISTRY-RTN-STATUS        PIC X(2).
       01  CNHISTRY-HISTCLC              PIC 9(8) BINARY.
      *
      ********** 0 - GET FIRST CLAIM
      ********** 1 - GET NEXT  CLAIM
      ********** 2 - GET NEXT DEPENDENT
      *
       01  CNHISTRY-AREA.
           COPY VSCPCLXC.
           02  CJ-VSCPNTEC-AREA.
               05 FILLER                 PIC X(56) OCCURS 100.
           02  CJ-VSCPNTEC   REDEFINES
               CJ-VSCPNTEC-AREA  OCCURS 100 TIMES.
                              COPY VSCPNTEC.
       01   NOTE-DATA-AREA.
            02    NOTE-01-02-DATA.
               COPY  S9CPN02C.
            02    NOTE-01-06-DATA.
               COPY  S9CPN06C.
            02    NOTE-01-10-DATA.
               COPY  S9CPN10C.
            02    NOTE-01-14-DATA.
               COPY  S9CPN14C.
            02    NOTE-01-30-DATA.
               COPY  S9CPN30C.
      ***  02    VLCSF-NOTE-97-01 - 06 ****
               COPY  S9CPN97C.
      *
       01  NOTE-W.
           05 NOTE-LENGTH-W.
              10 NOTE-LENGTH-NUM-W       PIC S9(4) COMP.
           05 NOTE-DATA-W.
              10 NOTE-PAGE-NUM-W         PIC 999   COMP-3.
              10 NOTE-NOTE-NUM-W         PIC 999   COMP-3.
              10 NOTE-TEXT-W             PIC X(50).
           05  NOTE-97-FLAG              PIC X VALUE ' '.
      *
       01  CNHISTRY-FIN-INFO.
           COPY VSCPCLFC.
      *
       01  WS-SUBSCRIBER.
           05  WS-PREFIX                 PIC X(3).
           05  WS-ORI-SUBSCRIBER         PIC 9(9).
           05  WS-SUB-CHK-ID             PIC X(1).

       01  WS-BEN-CAT-GRP.
           05 WS-BEN-CAT                 PIC 9(3).
           05 WS-BEN-CAT-CODE            PIC X(3).
           05  WS-BEN-CAT-CODE-SW        PIC  X VALUE 'N'.
               88 BEN-CATEGORY           VALUE 'Y'.
       01  FILLER.
           05  DATA-99                   PIC 9(4) COMP VALUE ZEROES.
           05  FILLER  REDEFINES DATA-99.
               10  FILLER                PIC X.
               10  DATA-X                PIC X.
           05  DATA-99-2                 PIC 9(4) COMP VALUE ZEROES.
           05  FILLER  REDEFINES DATA-99-2.
               10  FILLER                PIC X.
               10  DATA-X-2              PIC X.
      *
       01  GO-VSGPNTEC.
           COPY VSGPNTEC.
      *
       01  GO-S9MGNT41.
           COPY S9MGNT41.

      *
       01  IMS-FUNCTION                  PIC X(4) VALUE SPACES.
      *
       01 WS-SWITCHES.
          05 WS-END-OF-FILE              PIC X    VALUE 'N'.
             88  END-OF-FILE                      VALUE 'Y'.
          05 WS-CURR-HIST                PIC X    VALUE 'N'.
             88  CURR-HIST                        VALUE 'Y'.
          05 WS-PRIOR-HIST               PIC X    VALUE 'N'.
             88  PRIOR-HIST                       VALUE 'Y'.
          05 WS-PRIOR-DATE-FOUND         PIC X    VALUE 'N'.
             88  PRIOR-DATE-FOUND                 VALUE 'Y'.
          05 WS-END-OF-PRIOR-SEG         PIC X    VALUE 'N'.
             88  END-OF-PRIOR-SEG                 VALUE 'Y'.
          05 WS-CURR-CLAIM-NOT-GOOD      PIC X    VALUE 'N'.
             88  CURR-CLAIM-NOT-GOOD              VALUE 'Y'.
          05 WS-END-OF-NOTES             PIC X    VALUE 'N'.
             88  END-OF-NOTES                     VALUE 'Y'.
          05 WS-DATE-FOUND               PIC X    VALUE 'N'.
             88  DATE-FOUND                       VALUE 'Y'.
          05 WS-GRP-FOUND                PIC X    VALUE 'N'.
             88  GRP-FOUND                        VALUE 'Y'.
          05 WS-PKG-FOUND                PIC X    VALUE 'N'.
             88  PKG-FOUND                        VALUE 'Y'.
          05 WS-SELECT-RECORD            PIC X    VALUE 'N'.
             88  SELECT-RECORD                    VALUE 'Y'.
          05 WS-FIRST-TIME               PIC X    VALUE 'N'.
             88  FIRST-TIME                       VALUE 'Y'.
          05 WS-BYPASS-RECORD            PIC X    VALUE 'N'.
             88  BYPASS-RECORD                    VALUE 'Y'.
          05 WS-BYPASS-PRIOR-RECORD      PIC X    VALUE 'N'.
             88  BYPASS-PRIOR-RECORD              VALUE 'Y'.
          05 WS-ES-FIRST-TIME            PIC X    VALUE 'Y'.
             88  ES-FIRST-TIME                    VALUE 'Y'.
          05 WS-NR-FIRST-TIME            PIC X    VALUE 'Y'.
             88  NR-FIRST-TIME                    VALUE 'Y'.
          05 WS-ES-DATE-MATCH            PIC X    VALUE 'N'.
             88  ES-DATE-MATCH                    VALUE 'Y'.
          05 VSSPDPN-END-OF-SEG-SW       PIC X    VALUE 'N'.
             88  VSSPDPN-END-OF-SEG               VALUE 'Y'.
      *
      ******************************************************************
      *   GROUP DATABASE - QUALIFIED SSA.
      ******************************************************************
      *
       01  VSGPGRU-SSA-Q.
           05  FILLER                    PIC X(8)  VALUE 'VSGPGRU '.
           05  VSGPGRU-CMD-BOOLEAN       PIC X(3)  VALUE '*--'.
           05  FILLER                    PIC X     VALUE '('.
           05  VSGPGRU-KEYFIELD1         PIC X(8)  VALUE 'GRUPKEYI'.
           05  VSGPGRU-OPER1             PIC X(2)  VALUE '>='.
           05  VSGPGRU-KEY1              PIC X(09).
           05  VSGPGRU-BOOLEAN-AND       PIC X     VALUE '*'.
           05  VSGPGRU-KEYFIELD2         PIC X(8)  VALUE 'GRUPKEYI'.
           05  VSGPGRU-OPER2             PIC X(2)  VALUE '<='.
           05  VSGPGRU-KEY2              PIC X(09).
           05  FILLER                    PIC X     VALUE ')'.
      *
       01  VSGPNTE-SSA-Q.
           05  FILLER                    PIC  X(08) VALUE 'VSGPNTE '.
           05  VSGPNTE-CMD-CD            PIC  X(03) VALUE '*--'.
           05  VSGPNTE-SEG1              PIC  X(09) VALUE '(GPDFNTEI'.
           05  VSGPNTE-OPER1             PIC  X(02) VALUE '>='.
           05  VSGPNTE-KEY1.
               10  VSGPNTE-PAGE1         PIC  9(02) COMP-3 VALUE 35.
               10  VSGPNTE-NOTE1         PIC  9(03) COMP-3 VALUE 0.
           05  VSGPNTE-BOOLEAN-AND       PIC  X(01) VALUE '*'.
           05  VSGPNTE-SEG2              PIC  X(08) VALUE 'GPDFNTEI'.
           05  VSGPNTE-OPER2             PIC  X(02) VALUE '<='.
           05  VSGPNTE-KEY2.
               10  VSGPNTE-PAGE2         PIC  9(02) COMP-3 VALUE 35.
               10  VSGPNTE-NOTE2         PIC  9(03) COMP-3 VALUE 999.
           05  VSGPNTE-CLOSE             PIC  X(01) VALUE ')'.
      *                                                                 01410000
      *                                                                 02220000
      ******************************************************************09270000
      *    SUBSCRIBER DATABASE SSA'S AND IO-AREAS.                      09280000
      ******************************************************************09290000
       01  SUB-ROOT-SSA.
           05  SUB-SSA-SEGNAME           PIC X(08) VALUE 'VSSPSUB '.
           05  SUB-SSA-CMD-CODES         PIC X(03) VALUE '*--'.
           05  SUB-SSA-L-PAREN           PIC X(01) VALUE '('.
           05  SUB-SSA-FIELD-NAME        PIC X(08) VALUE 'SUBRIDD1'.
           05  SUB-SSA-OPER              PIC X(02) VALUE ' ='.
           05  SUB-SSA-SUBR-NO           PIC X(12).
           05  SUB-SSA-MDF               PIC X(03).
           05  SUB-SSA-R-PAREN           PIC X(01) VALUE ')'.
           SKIP1
       01  SUB-ROOT-SEG.
           COPY VSSPSUBC.
      *
      *01  SA-VSSPSUBC.
      *    COPY VSSPSUBC.
      *
           COPY WISSSUB.
      *
       01  SUB-MEMB-SSA.
           05  SMEMB-SSA-SEGNAME         PIC X(08) VALUE 'VSSPMEM '.
           05  SMEMB-SSA-CMD-CODES       PIC X(03) VALUE '*--'.
           05  SMEMB-SSA-L-PAREN         PIC X(01) VALUE '('.
           05  SMEMB-SSA-FIELD-NAME      PIC X(08) VALUE 'NULLDEPN'.
           05  SMEMB-SSA-OPER            PIC X(02) VALUE ' ='.
           05  SMEMB-SSA-DEP-NO          PIC 9(03) COMP-3.
           05  SMEMB-SSA-R-PAREN         PIC X(01) VALUE ')'.
           SKIP1
       01  SUB-UNQ-MEMB-SSA.
           05  SMB-SSA-SEGNAME           PIC X(08) VALUE 'VSSPMEM '.
           05  SMB-SSA-CMD-CODES         PIC X(01) VALUE ' '.
           SKIP1
       01  SUB-MEMB-SEG.
           COPY VSSPMEMC.
           EJECT
           SKIP1
       01  SUB-UNQ-ADDR-SSA.
           05  SUB-ADDR-SEGNM            PIC X(08) VALUE 'VSSPADR '.
           05  SUB-ADDR-CMD-CD           PIC X(01) VALUE ' '.
           SKIP1
       01  SUB-ADDR-SEG.
           COPY VSSPADRC.
       01  SSA-VSSPADR.
           05  FILLER                    PIC X(17)  VALUE
               'VSSPADR (GPDFTYPL'.
           05  BOOLE                     PIC X(02)  VALUE 'EQ'.
           05  TYPESS                    PIC X(02).
           05  ADDR-LN-NO                PIC 9(04)  COMP.
           05  FILLER                    PIC X(01)  VALUE ')'.
      *                                                                 01410000
      *
       01  SUB-UNQ-DPN-SSA.
           05  SMB-SSA-SEGNAME           PIC X(08) VALUE 'VSSPDPN '.
           05  SMB-SSA-CMD-CODES         PIC X(01) VALUE ' '.
           SKIP1
       01  SUB-DPN-SEG.
           COPY VSSPDPNC.
           EJECT
      *
      *                                                                 01410000
      *************************************************                 01410000
      *  ALT-ID COMMUNICATION AREA
      *************************************************
      *                                                                 01410000
       01  ALT-ID-COMM-AREA.
           COPY S9SSSEXC.
       01  WS-ALTID-WORK-AREA.
           05  S9MALTCB                  PIC X(8)  VALUE 'S9MALTCB'.
           05  WS-HOLD-SUBR-NO.
               10  WS-HOLD-SUBR-3        PIC X(03).
               10  WS-HOLD-SUBR-9        PIC X(09).
               10  WS-HOLD-SUBR-3A       PIC X(03).
           EJECT
       01  WS-SUBSCRIBER.
           05  WS-PREFIX                 PIC X(3).
           05  WS-ORI-SUBSCRIBER         PIC 9(9).
           05  WS-SUB-CHK-ID             PIC X(1).
      *
      *                                                                 01410000
      ******************************************************************09270000
      *    ENCODE-DECODE-TABLE-CODES.                                   09280000
      ******************************************************************09290000
      *                                                                 01410000
       01  TABLE-WORK.
           05  ED-TABLE                  PIC X(8).
           05  ED-VALUE-IN2              PIC X(2).
           05  WS-ED-OUTPUT              PIC X(3).
           05  WS-ENCD-VALUE             PIC X.
           05  WS-TABLE-STATUS           PIC 9(4) COMP.
           05  WS-TABLE-INDEX            COMP    PIC 9(4).
           05  WS-ENCD-STATUS            PIC 9(4) COMP.
           05  ENCODE-DECODE-FUNCTIONS.
               10  B4                    PIC 9(4) COMP SYNC VALUE 4.
               10  B5                    PIC 9(4) COMP SYNC VALUE 5.
               10  B6                    PIC 9(4) COMP SYNC VALUE 6.
               10  B8                    PIC 9(4) COMP SYNC VALUE 8.
               10  B9                    PIC 9(4) COMP SYNC VALUE 9.
               10  B10                   PIC 9(4) COMP SYNC VALUE 10.
               10  B12                   PIC 9(4) COMP SYNC VALUE 12.
               10  B14                   PIC 9(4) COMP SYNC VALUE 14.
               10  STATUS-CD             PIC 9(4) COMP SYNC.
           05  ED-IX.                                                   09310000
               10  ED-IX-N               PIC 9(4)   COMP.               09320000
               10  ED-IX-X REDEFINES ED-IX-N.                           09330000
                   15  ED-LV             PIC X.                         09340000
                   15  ED-CD             PIC X.                         09350000
      *
      ******************************************************************09370000
      *    ENCODE-DECODE-OUTPUT-AREA.                                   09380000
      ******************************************************************09390000
      *
       01  FILLER.                                                      09400000
           05  ED-ARGUE.                                                09410000
               10  ED-1-INPUT            PIC X(1).                      09420000
               10  ED-1-DESC             PIC X(77).                     09430000
           05  FILLER     REDEFINES ED-ARGUE.                           09440000
               10  ED-2-INPUT            PIC X(2).                      09450000
               10  ED-2-DESC             PIC X(76).                     09460000
           05  FILLER     REDEFINES ED-ARGUE.                           09470000
               10  ED-3-INPUT            PIC X(3).                      09480000
               10  ED-3-DESC             PIC X(75).                     09490000
           05  FILLER     REDEFINES ED-ARGUE.                           09500000
               10  ED-4-INPUT            PIC X(4).                      09510000
               10  ED-4-DESC             PIC X(74).                     09520000
           05  FILLER     REDEFINES ED-ARGUE.
               10  ED-6-INPUT            PIC X(6).
               10  ED-6-DESC             PIC X(72).
           05  FILLER                    PIC X(300).                    09530000
           05  ED-OUTPUT                 PIC X(200).                    09540000
           05  FILLER      REDEFINES ED-OUTPUT.                         09590000
               10  ED-1-O1.                                             09550000
                   15  ED-1-O1-N         PIC 9(1).                      09560000
               10  FILLER                PIC X(4).                      09570000
               10  ED-1-O2               PIC X(74).                     09580000
               10  FILLER                PIC X(121).                    09570000
           05  FILLER      REDEFINES ED-OUTPUT.                         09590000
               10  ED-2-O1.                                             09600000
                   15  ED-2-O1-N         PIC 9(2).                      09610000
               10  FILLER                PIC X(3).                      09620000
               10  ED-2-O2               PIC X(74).                     09630000
               10  FILLER                PIC X(121).                    09570000
           05  FILLER      REDEFINES ED-OUTPUT.                         09640000
               10  ED-3-O1.                                             09650000
                   15  ED-3-O1-N         PIC 9(3).                      09660000
               10  FILLER                PIC X(2).                      09670000
               10  ED-3-O2               PIC X(74).                     09680000
               10  FILLER                PIC X(121).                    09570000
           05  FILLER      REDEFINES ED-OUTPUT.                         09690000
               10  ED-4-O1.                                             09700000
                   15  ED-4-O1-N         PIC 9(4).                      09710000
               10  FILLER                PIC X(1).                      09720000
               10  ED-4-O2               PIC X(74).                     09730000
               10  FILLER                PIC X(121).                    09570000
           05  FILLER      REDEFINES ED-OUTPUT.                          CR446
               10  ED-6-O1.                                              CR446
                   15  ED-6-O1-N         PIC 9(6).                       CR446
               10  FILLER                PIC X(1).                       CR446
               10  ED-6-O2               PIC X(72).                      CR446
               10  FILLER                PIC X(121).                    09570000
           05  FILLER      REDEFINES ED-OUTPUT.                          CR446
               10  ED-4X.                                                CR446
                 15  ED-4X-OUT           PIC X(04).                      CR446
                 15  ED-4X-IN            PIC X(04).                      CR446
                 15  ED-4X-DESC-36       PIC X(36).                      CR446
               10  FILLER                PIC X(156).                    09570000
           05  WS-ENCD-ARGUMENT-3.
               10  WS-ENCD-INPUT-3                 PIC X(3).
               10  WS-ENCD-DESC-3                  PIC X(176).
           05  WS-ENCD-ARGUMENT-9.
               10  WS-ENCD-INPUT-9       PIC X(9).
               10  FILLER                PIC X(69).
      *        10  FILLER                PIC X(170).
           05  ED-CD-9                   PIC X(9).
      *
       01  NOTE-FLAG                     PIC X(1) VALUE ' '.
       01  WS-SYSIN-DATE                 PIC 9(07).
       01  CHN-SUB                       PIC S9(4) COMP.
      *
      ******************************************************************09370000
      *    SELECTED GROUPS AND SUB-GROUPS                               09380000
      ******************************************************************09390000
      *
       01  WS-GROUP                      PIC X(06).
           88  DIOCERSE-GRP              VALUE '720100'.
      *
       01  WS-SUBGRP                     PIC X(03).
           88  DIOCERSE-SGRP             VALUE '  9' ' 9A' ' 9B' ' 9C'
                                               ' 9E' '9AE' '9BE' '9CE'
                                               '9RE' '9SE'.
      *
      ******************************************************************09370000
      *    SELECTED SERVICE CLASS                                       09380000
      ******************************************************************09390000
      *
       01  WS-SERVICE-CLASS       PIC 9(3).
             88 SERV-CLASS        VALUES  153
                                          075.
             88 SERV-CLASS-T      VALUE   153.
      *
      ******************************************************************09370000
      *    SELECTED HSC                                                 09380000
      ******************************************************************09390000
      *
       01  WS-HSC                 PIC X(5).
             88 HSC               VALUES  'S4026' 'S4027'
                                          'S4028' 'S4030' 'S4031'.
      *
      ******************************************************************09370000
      *    SELECTED SURGERY CODE                                        09380000
      ******************************************************************09390000
       01  WS-SURG-PRCD           PIC X(5).
             88 SURG-PRCD         VALUES  'K6992'.
             88 SURGERY-PRCD      VALUES  'K6511'
                                          'K6591'.
       01  WS-SURG-PRCD10         PIC X(7).
             88 SURG-PRCD10         VALUES  '3E0P3LZ' '3E0P7LZ'.
             88 SURGERY-PRCD10      VALUES  '0U903ZX' '0UB03ZX'
                                            '0U913ZX' '0UB13ZX'
                                            '0U923ZX' '0UB23ZX'
                                            '0U9230Z'
                                            '0U9030Z' '0U913ZZ'
                                            '0U923ZZ' '0U903ZZ'
                                            '0U9130Z' '0UDN3ZZ'
                                            '0UDN4ZZ' '0UDN0ZZ'.
      ******************************************************************09370000
      *    SELECTED HEADER INVEST CODE                                  09380000
      ******************************************************************09390000
       01  WS-NT-INVT-CD          PIC X(4).
             88  HEAD-CODE        VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED HEADER INVEST CODE 2                                09380000
      ******************************************************************09390000
       01  WS-CLAIM-INVES2        PIC X(4).
             88  INVES-CODE2      VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED HEADER INVEST CODE 3                                09380000
      ******************************************************************09390000
       01  WS-CLAIM-INVES3        PIC X(4).
             88  INVES-CODE3      VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED HEADER INVEST CODE 4                                09380000
      ******************************************************************09390000
       01  WS-CLAIM-INVES4        PIC X(4).
             88  INVES-CODE4      VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED HEADER INVEST CODE 5                                09380000
      ******************************************************************09390000
       01  WS-CLAIM-INVES5        PIC X(4).
             88  INVES-CODE5      VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED LINE INVEST CODE                                    09380000
      ******************************************************************09390000
       01  WS-LN-NT-INVT-CD       PIC X(4).
             88  LINE-CODE        VALUES 'INVT' 'ARIN'
                                         'INCY'.
      *
      ******************************************************************09370000
      *    SELECTED DIAG CODES                                          09380000
      ******************************************************************09390000
       01  WS-PRIM-DIAG-CD        PIC X(5).
           88 PRIM-DIAG-CD        VALUE '628  ' THRU '6289 '.
           88 PRIM-DIAG-CD1       VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 PDIAG               VALUE 'V261'.
      *
       01  WS-10PRIM-DIAG         PIC X(7).
           88 10PRIM-DIAG         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10PDIAG             VALUE 'Z3189  '.
      *
       01  WS-ADM-DIAG-CD         PIC X(5).
           88 ADM-DIAG-CD         VALUE '628  ' THRU '6289 '.
           88 ADM-DIAG-CD1        VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
      *
       01  WS-10ADMT-DIAG         PIC X(7).
           88 10ADMT-DIAG         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
      *
       01  WS-DRG-DIAG-1          PIC X(5).
           88 DRG-DIAG-1          VALUE '628  ' THRU '6289 '.
           88 DRG-DIAG-11         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-DIAG-12         VALUE 'V261'.
      *
       01  WS-10DIAG01            PIC X(7).
           88 10DIAG01            VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10DIAG01-12         VALUE 'Z3189  '.
      *
       01  WS-DRG-DIAG-2          PIC X(5).
           88 DRG-DIAG-2          VALUE '628  ' THRU '6289 '.
           88 DRG-DIAG-21         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-DIAG-22         VALUE 'V261'.
      *
       01  WS-10DIAG02            PIC X(7).
           88 10DIAG02            VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10DIAG02-22         VALUE 'Z3189  '.
      *
       01  WS-DRG-DIAG-3          PIC X(5).
           88 DRG-DIAG-3          VALUE '628  ' THRU '6289 '.
           88 DRG-DIAG-31         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-DIAG-32         VALUE 'V261'.
      *
       01  WS-10DIAG03            PIC X(7).
           88 10DIAG03            VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10DIAG03-32         VALUE 'Z3189  '.
      *
       01  WS-DRG-DIAG-4          PIC X(5).
           88 DRG-DIAG-4          VALUE '628  ' THRU '6289 '.
           88 DRG-DIAG-41         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-DIAG-42         VALUE 'V261'.
      *
       01  WS-10DIAG04            PIC X(7).
           88 10DIAG04            VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10DIAG04-42         VALUE 'Z3189  '.
      *
       01  WS-DRG-PROC-1          PIC X(5).
           88 DRG-PROC-1          VALUE '628  ' THRU '6289 '.
           88 DRG-PROC-11         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-PROC-12         VALUE 'V261'.
      *
       01  WS-10PROC-CD02         PIC X(7).
           88 10PROC-CD02         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10PROC-CD02-12      VALUE 'Z3189  '.
      *
       01  WS-DRG-PROC-2          PIC X(5).
           88 DRG-PROC-2          VALUE '628  ' THRU '6289 '.
           88 DRG-PROC-21         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-PROC-22         VALUE 'V261'.
      *
       01  WS-10PROC-CD03         PIC X(7).
           88 10PROC-CD03         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10PROC-CD03-22      VALUE 'Z3189  '.
      *
       01  WS-DRG-PROC-3          PIC X(5).
           88 DRG-PROC-3          VALUE '628  ' THRU '6289 '.
           88 DRG-PROC-31         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-PROC-32         VALUE 'V261'.
      *
       01  WS-10PROC-CD04         PIC X(7).
           88 10PROC-CD04         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10PROC-CD04-32      VALUE 'Z3189  '.
      *
       01  WS-DRG-PROC-4          PIC X(5).
           88 DRG-PROC-4          VALUE '628  ' THRU '6289 '.
           88 DRG-PROC-41         VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
           88 DRG-PROC-42         VALUE 'V261'.
      *
       01  WS-10PROC-CD05         PIC X(7).
           88 10PROC-CD05         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
           88 10PROC-CD05-42      VALUE 'Z3189  '.
      *
       01  WS-LINE-DIAG           PIC X(5).
           88 LINE-DIAG           VALUE '628  ' THRU '6289 '.
           88 LINE-DIAG1          VALUE '628  ' '6280 ' '6281 '
                                        '6282 ' '6283 ' '6284 '
                                        '6288 ' '6289 '.
      *
       01  WS-LINE-DIAG10         PIC X(7).
           88 LINE-DIAG10         VALUE 'N970   ' 'E230   ' 'N971   '
                                        'N972   ' 'N978   ' 'N979   '.
      *
       01  WS-CLAIM-CSD-CSX.
           05  CSD-CSX-CODE-AREA.
                 10  CSD-CSX-DESC10       PIC X(10).
                 10  CSD-CSX-INDEX            VALUE LOW-VALUES.
                     15  WS-CSD-CSX-INDEX PIC  X.
                     15  FILLER           PIC  X.
                 10  CSD-CSX-CLAIM-VFC-X3.
                     15  CSD-CSX-CLAIM-VFC-IX     PIC 9(3).
           05  EDT-RC-N                   PIC 9(4) COMP SYNC VALUE 10.
           05  EDT-CODE-N                 PIC 9(4) COMP SYNC VALUE 00.
           05  CLAIM-VFC-GRP.
               10  CLAIM-VFC-D1-COPAY     PIC X VALUE ' '.
               10  CLAIM-VFC-D2-COPAY     PIC X VALUE ' '.
               10  CLAIM-VFC-COINS        PIC X VALUE ' '.
               10  CLAIM-VFC-D2-MANAGED   PIC X VALUE ' '.
               10  CLAIM-VFC-D2-COINS     PIC X VALUE ' '.
               10  CLAIM-VFC-CP-COPAY     PIC X VALUE ' '.
               10  CLAIM-VFC-CP-MANAGED   PIC X VALUE ' '.
               10  CLAIM-VFC-CP-OOP       PIC X VALUE ' '.
               10  CLAIM-VFC-R3-RULEI     PIC X VALUE ' '.
               10  CLAIM-VFC-R3-RULEO     PIC X VALUE ' '.
      *
      *
       LINKAGE SECTION.
      *
       01  IO-PCB                        PIC X(28).
       01  VFPCHS01-PCB                  PIC X(100).
       01  VFPCHS02-PCB                  PIC X(100).
       01  VFPCHS03-PCB                  PIC X(100).
       01  VFPCHS04-PCB                  PIC X(100).
       01  VFPCHS05-PCB                  PIC X(100).
       01  VFPCHS06-PCB                  PIC X(100).
       01  VFPCHS07-PCB                  PIC X(100).
       01  VFPCHS08-PCB                  PIC X(100).
       01  VFPCHS09-PCB                  PIC X(100).
       01  VFPCHS10-PCB                  PIC X(100).
       01  VFCCHS01-PCB                  PIC X(100).
       01  VFCCHS02-PCB                  PIC X(100).
       01  VFCCHS03-PCB                  PIC X(100).
       01  VFCCHS04-PCB                  PIC X(100).
       01  VFCCHS05-PCB                  PIC X(100).
       01  VFCCHS06-PCB                  PIC X(100).
       01  VFCCHS07-PCB                  PIC X(100).
       01  VFCCHS08-PCB                  PIC X(100).
       01  VFCCHS09-PCB                  PIC X(100).
       01  VFCCHS10-PCB                  PIC X(100).
       01  VFCRHS01-PCB                  PIC X(100).
       01  VFCQHS01-PCB                  PIC X(100).
      *
       01  VFHPRO10-PCB.
           05  PCB-DBD-NAME              PIC X(08).
           05  PCB-SEGMENT-LEVEL         PIC X(02).
           05  PCB-RETURN-CODE-P         PIC X(02).
           05  PCB-PROC-OPTION           PIC X(04).
           05  PCB-RESERVE-DLI           PIC X(04).
           05  PCB-SEGMENT-NAME          PIC X(08).
           05  PCB-KEY-FEEDBACK-LENGTH   PIC S9(8)  COMP.
           05  PCB-NUM-SENS-SEGMENTS     PIC S9(8)  COMP.
           05  PCB-KEY-FEEDBACK-AREA     PIC X(100).
      *
       01  VFGGRP10-PCB.
           05  PCB-DBD-NAME              PIC X(08).
           05  PCB-SEGMENT-LEVEL         PIC X(02).
           05  PCB-RETURN-CODE           PIC X(02).
           05  PCB-PROC-OPTION           PIC X(04).
           05  PCB-RESERVE-DLI           PIC X(04).
           05  PCB-SEGMENT-NAME          PIC X(08).
           05  PCB-KEY-FEEDBACK-LENGTH   PIC S9(8)  COMP.
           05  PCB-NUM-SENS-SEGMENTS     PIC S9(8)  COMP.
           05  PCB-KEY-FEEDBACK-AREA     PIC X(100).
      *
       01  SUB-DB-PCB.
           05  SUB-DBD-NAME               PIC X(8).
           05  SUB-DB-SEG-LEVEL           PIC XX.
           05  SUB-DB-STATUS-CODE         PIC XX.
               88  SUB-DB-SEG-FOUND                 VALUE '  '.
               88  SUB-DB-SEG-NOT-FOUND             VALUE 'GE'.
               88  SUB-END-OF-DB                    VALUE 'GB'.
               88  SUB-NEW-SEG-SAME-LEVEL           VALUE 'GK'.
           05  SUB-PROC-OPT               PIC X(4).
           05  SUB-RESERVED-AREA          PIC S9(5) COMP.
           05  SUB-DB-SEG-NAME            PIC X(8).
           05  SUB-KEY-LENGTH             PIC S9(5) COMP.
           05  SUB-NUMBER-SEGS            PIC S9(5) COMP.
           05  SUB-KEY-FEEDBACK           PIC X(24).
           EJECT
       01  ZFMAGP10-PCB                  PIC X(100).
       01  ZFMAXF10-PCB                  PIC X(100).
       01  ZFMAXX11-PCB                  PIC X(100).
      *
       01  DB01-PCB.
           05  FILLER           PIC X(10).
           05  LS-STC           PIC XX.
           05  FILLER           PIC X(88).
           EJECT
      *
       PROCEDURE DIVISION USING IO-PCB VFPCHS01-PCB VFPCHS02-PCB
             VFPCHS03-PCB VFPCHS04-PCB VFPCHS05-PCB VFPCHS06-PCB
             VFPCHS07-PCB VFPCHS08-PCB VFPCHS09-PCB VFPCHS10-PCB
             VFCCHS01-PCB VFCCHS02-PCB VFCCHS03-PCB VFCCHS04-PCB
             VFCCHS05-PCB VFCCHS06-PCB VFCCHS07-PCB VFCCHS08-PCB
             VFCCHS09-PCB VFCCHS10-PCB VFCRHS01-PCB VFCQHS01-PCB
             VFHPRO10-PCB VFGGRP10-PCB SUB-DB-PCB ZFMAGP10-PCB
             ZFMAXF10-PCB ZFMAXX11-PCB DB01-PCB.
      *
      *****************************************************************
      * THIS PARAGRAPH IS THE MAIN PROCESS FOR THIS MODULE.           *
      *****************************************************************
      *
       A000-MAINLINE.
      *
           ACCEPT WS-SYSIN-DATE FROM SYSIN
           OPEN INPUT  RSI-TRANSACTIONS
                OUTPUT NEXIMPACT-RETURN

           CALL 'ZNCLMPCB' USING PCB-TABLE VFPCHS01-PCB VFPCHS02-PCB
             VFPCHS03-PCB VFPCHS04-PCB VFPCHS05-PCB VFPCHS06-PCB
             VFPCHS07-PCB VFPCHS08-PCB VFPCHS09-PCB VFPCHS10-PCB
             VFCCHS01-PCB VFCCHS02-PCB VFCCHS03-PCB VFCCHS04-PCB
             VFCCHS05-PCB VFCCHS06-PCB VFCCHS07-PCB VFCCHS08-PCB
             VFCCHS09-PCB VFCCHS10-PCB VFCRHS01-PCB VFCQHS01-PCB.

           INITIALIZE INPUT-RECORD-COUNT
                      OUTPUT-RECORD-COUNT
                      OUTPUT-RECORD-BYPASS
           MOVE  WHEN-COMPILED  TO   WS-WHEN-COMPILED                   07610000
           ACCEPT WS-RUN-DATE   FROM DATE YYYYMMDD                      07620000
           DISPLAY 'START PROGRAM "' WS-PROGRAM-ID                      07660000
              '" COMPILED ON '  WS-COMP-DATE ' AT ' WS-COMP-TIME        07670000
              ', RUNNING ON '   WS-RUN-DATE                             07680000
           MOVE WS-RUN-DATE(1:4) TO WS-YEAR
           MOVE WS-RUN-DATE(5:2) TO WS-MONTH
           MOVE WS-RUN-DATE(7:2) TO WS-DATE
           MOVE WS-COMP-TIME(1:2) TO WS-HOUR
           MOVE WS-COMP-TIME(4:2) TO WS-MINUTE
           MOVE WS-COMP-TIME(4:2) TO WS-SECONDS

           PERFORM A100-READ-INPUT     THRU A100-EXIT
                   UNTIL END-OF-FILE
           PERFORM F100-DISPLAY-COUNTS THRU F100-EXIT

           CLOSE  RSI-TRANSACTIONS
                  NEXIMPACT-RETURN
           GOBACK.
      *
       A000-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH READS THE INPUT FILE.
      ******************************************************************
      *
       A100-READ-INPUT.
      *
           READ RSI-TRANSACTIONS
           AT END SET END-OF-FILE TO TRUE
           NOT AT END
             IF NOT END-OF-FILE
                ADD 1 TO INPUT-RECORD-COUNT
                MOVE 'N' TO WS-CURR-CLAIM-NOT-GOOD
                PERFORM A200-SEARCH-CLAIMS THRU A200-EXIT
             END-IF
           END-READ.
      *
       A100-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH SCANS THE CLAIMS CURRENT/PRIOR HISTORY DATABASE
      * BASED ON CERTAIN CONDITIONS.
      ******************************************************************
      *
       A200-SEARCH-CLAIMS.
      *
           INITIALIZE WS-FIELDS
           INITIALIZE CNHISTRY-KEY CNHISTRY-HISTCLC CI-GRP-LN
           MOVE S9-SUBSCRIBER-ID    TO LNCKDIG-SUBR-ID-KEY
           MOVE SPACES              TO LNCKDIG-SUBR-NO-FIRST

           CALL 'LNCKDIG'        USING LNCKDIG-LENGTH
                                       LNCKDIG-SUBR-NO
                                       LNCKDIG-CHECKDIGIT

           MOVE SPACES              TO LNCKDIG-SUBR-END
           MOVE LNCKDIG-CHECKDIGIT  TO LNCKDIG-SUBR-CK
           MOVE LNCKDIG-SUBR-ID-KEY TO CNHISTRY-KEY-SUBR-ID
           MOVE S9-DEPENDENT-NUMBER TO CNHISTRY-KEY-DEP-NO
           MOVE S9-CLAIM-NUMBER     TO CNHISTRY-KEY-CLM-ID
           MOVE ZEROES              TO CNHISTRY-HISTCLC
           MOVE 99999               TO CNHISTRY-KEY-ADJ-NO
           MOVE '**'   TO CLM-VERS-NO OF WF-CLM-VERS-NO
      *
           MOVE 'GU#9'              TO  CNHISTRY-FUNCTION
           CALL 'ZNCLMHST'        USING PCB-TABLE
                                        CNHISTRY-KEY
                                        CNHISTRY-AREA
                                        CNHISTRY-FUNCTION
                                        CNHISTRY-RTN-CD
                                        CNHISTRY-FIN-INFO
                                        CNHISTRY-HISTCLC

              IF (CNHISTRY-RTN-CD = '03GE' OR SPACES)
              AND CLM-VERS-NO OF WF-CLM-VERS-NO = '**'
                 DISPLAY
                  '*** CLAIMS DB ERROR * '
                   S9-DEPENDENT-NUMBER '/' S9-CLAIM-NUMBER
                   '* CONTACT SYSTEMS! ***'
              ELSE
                IF CNHISTRY-RTN-CD = '00GE'
                   DISPLAY
                     '* NO CLAIMS FOR CUSTOMER ON CURRENT CLAIM HIST*'
                ELSE
                   IF CNHISTRY-RTN-CD = '03GE'
                     IF (WF-CLM-STCD = X'03' OR X'04' OR X'05' OR X'06'
                                       OR X'0A')
                       PERFORM B200-SHIFT-SUBSCRIBER-ID THRU B200-EXIT
                       IF WF-CLM-ADJ-NO > 0
                         PERFORM A205-LOAD-NOTES        THRU A205-EXIT
                         SET CURR-HIST TO TRUE
                         PERFORM A230-HISTORY-DOLLAR-AMT THRU A230-EXIT
                         MOVE 'N' TO WS-CURR-HIST
                         MOVE 'N' TO WS-PRIOR-DATE-FOUND
                                     WS-END-OF-PRIOR-SEG
                         MOVE 'GU#2' TO CNHISTRY-FUNCTION
                         SUBTRACT 1 FROM WF-CLM-ADJ-NO
                         MOVE WF-CLM-ADJ-NO TO PRIOR-SUB
                                               WS-SUB
                         PERFORM A220-ACCESS-PRIOR THRU A220-EXIT
                                 VARYING WS-SUB FROM WS-SUB
                                 BY -1 UNTIL PRIOR-DATE-FOUND
                                 OR END-OF-PRIOR-SEG
                       ELSE
                         PERFORM A205-LOAD-NOTES      THRU A205-EXIT
                         SET CURR-HIST TO TRUE
                         PERFORM A230-HISTORY-DOLLAR-AMT THRU A230-EXIT
                         MOVE 'N' TO WS-CURR-HIST
                       END-IF
                     ELSE
                       IF WF-CLM-ADJ-NO > 0
                         PERFORM A205-LOAD-NOTES      THRU A205-EXIT
                         MOVE 'N' TO WS-PRIOR-DATE-FOUND
                                     WS-END-OF-PRIOR-SEG
                         MOVE 'GU#2' TO CNHISTRY-FUNCTION
                         SUBTRACT 1 FROM WF-CLM-ADJ-NO
                         MOVE WF-CLM-ADJ-NO TO PRIOR-SUB
                                               WS-SUB
                         SET CURR-CLAIM-NOT-GOOD TO TRUE
                         PERFORM A220-ACCESS-PRIOR THRU A220-EXIT
                                 VARYING WS-SUB FROM WS-SUB
                                 BY -1 UNTIL PRIOR-DATE-FOUND
                                 OR END-OF-PRIOR-SEG
                       ELSE
                         ADD 1 TO OUTPUT-RECORD-BYPASS
                       END-IF
                     END-IF
                   END-IF
                END-IF
              END-IF.
      *
       A200-EXIT.
            EXIT.
      *
      *
      ******************************************************************
      * THIS PARAGRAPH ACCESS THE CLAIMS PRIOR HISTORY DATABASE
      ******************************************************************
      *
       A220-ACCESS-PRIOR.
      *
           INITIALIZE CNHISTRY-AREA CNHISTRY-FIN-INFO
           MOVE WS-SUB                 TO CNHISTRY-KEY-ADJ-NO
           MOVE SPACES                 TO CNHISTRY-RTN-CD
           MOVE ZEROES                 TO CNHISTRY-HISTCLC
           CALL 'ZNCLMHST'  USING         PCB-TABLE
                                          CNHISTRY-KEY
                                          CNHISTRY-AREA
                                          CNHISTRY-FUNCTION
                                          CNHISTRY-RTN-CD
                                          CNHISTRY-FIN-INFO
                                          CNHISTRY-HISTCLC
           IF (CNHISTRY-RTN-CD = '03GE' OR SPACES)
           AND CLM-VERS-NO OF WF-CLM-VERS-NO = '**'
              DISPLAY
               '*** CLAIMS DB ERROR * '
                S9-DEPENDENT-NUMBER '/' S9-CLAIM-NUMBER
                '* CONTACT SYSTEMS! ***'
           ELSE
             IF CNHISTRY-RTN-CD = '00GE'
                DISPLAY
                  '* NO CLAIMS FOR CUSTOMER ON PRIOR CLAIM HIST *'
             ELSE
                IF CNHISTRY-RTN-CD = '03GE'
                  IF WF-CLM-STCD = X'03' OR X'04' OR X'05' OR X'06' OR
                                   X'0A'
                     PERFORM A205-LOAD-NOTES  THRU  A205-EXIT
                     PERFORM A250-CHECK-PRIOR-ADJ-DATE THRU A250-EXIT
                     IF WS-CURR-DATE = WS-PRIOR-DATE
                        IF CURR-CLAIM-NOT-GOOD
                           SET PRIOR-HIST TO TRUE
                           PERFORM A230-HISTORY-DOLLAR-AMT
                              THRU A230-EXIT
                           MOVE 'N' TO WS-CURR-CLAIM-NOT-GOOD
                                       WS-PRIOR-HIST
                           SET PRIOR-DATE-FOUND TO TRUE
                        END-IF
                     ELSE
                        IF WS-PRIOR-DATE < WS-CURR-DATE
                          IF NOT CURR-CLAIM-NOT-GOOD
                             SET PRIOR-DATE-FOUND TO TRUE
                             SET PRIOR-HIST TO TRUE
                             PERFORM A230-HISTORY-DOLLAR-AMT
                                THRU A230-EXIT
                             MOVE 'N'       TO WS-PRIOR-HIST
                          END-IF
                        END-IF
                     END-IF
                  ELSE
                     ADD 1  TO OUTPUT-RECORD-BYPASS
                  END-IF
                END-IF
             END-IF
           END-IF
           IF WS-SUB = ZERO
              SET END-OF-PRIOR-SEG TO TRUE
           END-IF.
      *
       A220-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH IS TO CALCULATE THE DOLLAR AMOUNTS FROM THE
      * CURRENT CLAIM HISTORY.
      ******************************************************************
      *
       A230-HISTORY-DOLLAR-AMT.
      *
            MOVE SPACES                 TO WS-GROUP
                                           WS-SUBGRP
                                           VSGPGRU-KEY1
                                           VSGPGRU-KEY2
      *
            MOVE ZEROES                 TO SUB
            MOVE ZEROS                  TO WS-PKG-NO1

            MOVE 'N'                    TO WS-GRP-FOUND
            MOVE 'N'                    TO WS-PKG-FOUND
            PERFORM VARYING SUB FROM 1 BY 1
              UNTIL SUB > WF-LN-ITEM-CNT OR
                    GRP-FOUND
              IF WF-GRP-KEY-ID (SUB, 1) NOT = SPACES AND LOW-VALUES
                 SET GRP-FOUND          TO TRUE
                 MOVE WF-GRP-KEY-ID (SUB, 1) (1:6)
                                       TO WS-GROUP
                 MOVE WF-GRP-KEY-ID (SUB, 1) (7:3)
                                       TO WS-SUBGRP
              END-IF
            END-PERFORM
            MOVE ZEROES                 TO SUB
            PERFORM VARYING SUB FROM 1 BY 1
              UNTIL SUB > WF-LN-ITEM-CNT OR
                    PKG-FOUND
              IF WF-PKG-NO (SUB, 1) NOT = ZEROS
                 SET PKG-FOUND         TO TRUE
                 MOVE WF-PKG-NO (SUB, 1)
                                       TO WS-PKG-NO1
              END-IF
             END-PERFORM

            PERFORM B300-ALT-ID       THRU B300-EXIT
            MOVE 'Y' TO WS-FIRST-TIME1
            MOVE 'N' TO WS-NON-DENIED-LINE-SW
            PERFORM VARYING WFX-GRPF-LN-ITEMS FROM 1 BY 1
              UNTIL WFX-GRPF-LN-ITEMS > WF-LN-ITEM-CNT
      *
              PERFORM B050-INITIALIZE THRU B050-EXIT
              IF WF-COVER-STEP-VAL (WFX-GRPF-LN-ITEMS 1) > 0
                 MOVE 'Y'   TO WS-NON-DENIED-LINE-SW
              END-IF
              MOVE WF-CLM-ADJ-NO        TO WS-ADJ-NO
              MOVE WF-STRT-SVDT(WFX-GRPF-LN-ITEMS)
                                        TO WS-STRT-SVDT
              MOVE WF-END-SVDT (WFX-GRPF-LN-ITEMS)
                                        TO WS-END-SVDT
              PERFORM B100-COMPUTE-DOLLARS
                                      THRU B100-EXIT
              IF SELECT-RECORD
      *
                 INITIALIZE RULE3-VFC
                            CSD-CSX-CLAIM-VFC-IX
                 MOVE WF-VAL-FUNC-CD (WFX-GRPF-LN-ITEMS, 1, 4)
                                        TO ED-IX-N
                 MOVE 'ETUCDFCN'        TO ED-TABLE
                 MOVE SPACES           TO CLAIM-VFC-GRP
                 PERFORM E100-EDT-14  THRU E100-X
                 MOVE ED-3-O1           TO RULE3-VFC
      *
                 MOVE 'CTECSDSX'        TO ED-TABLE
                 PERFORM E200-EDT-4-6 THRU E200-EXIT
                 IF CLAIM-VFC-R3-RULEI = 'Y'
                    MOVE 'I'            TO WS-INT-NET-IND
                 ELSE
                    IF CLAIM-VFC-R3-RULEO = 'Y'
                       MOVE 'O'         TO WS-INT-NET-IND
                    ELSE
                       MOVE ' '         TO WS-INT-NET-IND
                    END-IF
                 END-IF

                 MOVE 'N'               TO WS-END-OF-NOTES
                                           WS-DATE-FOUND
                                           WS-ES-DATE-MATCH
                 SET FIRST-TIME         TO TRUE
                 SET NR-FIRST-TIME      TO TRUE
                 SET ES-FIRST-TIME      TO TRUE
                 PERFORM A240-GNT41-LOOKUP
                                      THRU A240-EXIT
                   UNTIL DATE-FOUND OR END-OF-NOTES
                 PERFORM A270-POPULATE-VALUES THRU A270-EXIT
                 MOVE 'N'               TO WS-BYPASS-RECORD
      *
                 INITIALIZE WS-PKG-NO
      *
                 IF DATE-FOUND
                    IF NOT BYPASS-RECORD
                          MOVE 0 TO SUB1
                       IF CURR-HIST

                        IF DIOCERSE-GRP
                           CONTINUE
                        ELSE
                         IF  (WS-NT41-DEDI-FOUND = 'Y' )
      *                     WS-DEDUCTIBLE-AMOUNT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'F' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-DEDUCTIBLE-AMOUNT TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF
                        END-IF
                        IF   (WS-NT41-COPAYI-FOUND = 'Y' ) OR
                             (WS-NT41-DEDI-FOUND = 'Y') OR
                             (WS-NT41-DEDIX-FOUND= 'Y')
      *                     (WS-OOP-AMT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'G' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-OOP-AMT      TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                        END-IF
                        IF DIOCERSE-GRP
                           CONTINUE
                        ELSE
                         IF  (WS-NT41-DEDO-FOUND = 'Y' )
      *                     WS-DEDUCTIBLE-AMOUNT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'F' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-DEDUCTIBLE-AMOUNT TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF

                         IF  ((WS-NT41-COPAYO-FOUND = 'Y' ) OR
                             (WS-NT41-DEDO-FOUND = 'Y') OR
                             (WS-NT41-DEDOX-FOUND= 'Y'))
      *                     (WS-OOP-AMT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'G' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-OOP-AMT      TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF
                        END-IF
                        IF WS-NON-DENIED-LINE-SW='Y'
                           IF S9-INT-NW-IND(SUB1) = 'I'
                           WRITE RETURN-RECORD
                                    FROM S9-INT-OUT-RECORD
                           ADD  1        TO OUTPUT-RECORD-COUNT
                                            WS-LINE-CNT
                           END-IF
                        END-IF
                       ELSE
                        IF PRIOR-HIST
                        IF DIOCERSE-GRP
                           CONTINUE
                        ELSE
                         IF  (WS-NT41-DEDI-FOUND = 'Y' )
      *                     WS-DEDUCTIBLE-AMOUNT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'F' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-DEDUCTIBLE-AMOUNT TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE '-'   TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF
                        END-IF
                        IF  ((WS-NT41-COPAYI-FOUND = 'Y' ) OR
                             (WS-NT41-DEDI-FOUND = 'Y')  OR
                             (WS-NT41-DEDIX-FOUND= 'Y'))
      *                     (WS-OOP-AMT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'G' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-OOP-AMT      TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE '-'   TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                        END-IF
                        IF DIOCERSE-GRP
                           CONTINUE
                        ELSE
                         IF  (WS-NT41-DEDO-FOUND = 'Y' )
      *                     WS-DEDUCTIBLE-AMOUNT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'F' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-DEDUCTIBLE-AMOUNT TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE '-'   TO S9-INT-NEG-AMT-IND(SUB1)
      *                      MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF

                         IF  ((WS-NT41-COPAYO-FOUND = 'Y' ) OR
                             (WS-NT41-DEDO-FOUND = 'Y')   OR
                             (WS-NT41-DEDOX-FOUND= 'Y'))
      *                     (WS-OOP-AMT NOT EQUAL TO ZEROES)
                             COMPUTE SUB1 = SUB1 + 1
                             MOVE 'G' TO S9-INT-ADJ-AMT-TYP(SUB1)
                             MOVE 'I' TO S9-INT-BNFT-TYP(SUB1)
                             MOVE WS-OOP-AMT      TO
                               S9-INT-AMT-ACC(SUB1)
                             MOVE '-'   TO S9-INT-NEG-AMT-IND(SUB1)
      *                      MOVE SPACES TO S9-INT-NEG-AMT-IND(SUB1)
                             PERFORM A260-NETWORK
                         END-IF
                        END-IF
      *                 PERFORM A260-NETWORK
                        IF WS-NON-DENIED-LINE-SW='Y'
                           IF S9-INT-NW-IND(SUB1) = 'I'
                              WRITE RETURN-RECORD
                                    FROM S9-INT-OUT-RECORD
                              ADD  1        TO OUTPUT-RECORD-COUNT
                                               WS-LINE-CNT
                           END-IF
                        END-IF
                        END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
            END-PERFORM.
      *
       A230-EXIT.
            EXIT.
      *
       A260-NETWORK.
                 IF WS-INT-NET-IND = 'I'
                    MOVE 'I'            TO
                     S9-INT-NW-IND (SUB1)
                 ELSE
                    IF WS-INT-NET-IND= 'O'
                       MOVE 'O'         TO
                       S9-INT-NW-IND (SUB1)
                    ELSE
                       MOVE ' '         TO
                       S9-INT-NW-IND (SUB1)
                    END-IF
                 END-IF.
      *
       B050-INITIALIZE.
               INITIALIZE              WS-ADJ-NO
                                       WS-STRT-SVDT
                                       WS-END-SVDT
                                       WS-DEDUCTIBLE-AMOUNT
                                       WS-COINSURANCE-AMOUNT
                                       WS-COPAY-AMOUNT
                                       WS-REIMBURSMENT
                                       WS-OOP-AMT
                                       WS-DED1-STEP-RED
                                       WS-DED2-STEP-RED
                                       WS-COPAY-STEP-RED
            MOVE SPACES             TO S9-INT-VERSION
                                       S9-INT-RECORD-TYP
                                       S9-INT-RECORD-TS
                                       S9-INT-RECORD-ID
                                       S9-INT-SENDER-TYP
                                       S9-INT-SENDER-ID
                                       S9-INT-SENDER-NOTE
                                       S9-INT-SENDER-CLMNUM
                                       S9-INT-REASON-CODE
                                       S9-INT-ADJ-DATE
                                       S9-INT-AMT-TYP
                                       S9-INT-CARRIER
                                       S9-INT-ACCOUNT-NUM
                                       S9-INT-GROUP
                                       S9-INT-MEMBER-ID
                                       S9-INT-MEMBER-FIRST-NM
                                       S9-INT-MEMBER-MID-INIT
                                       S9-INT-MEMBER-LAST-NM
                                       S9-INT-MEMBER-GENDER
                                       S9-INT-MEMBER-REL-CD
                                       S9-INT-SUB-ID
                                       S9-INT-RETURN-CD
            MOVE ZEROES TO S9-INT-MEMBER-DOB
                           S9-INT-TOT-ADJ
                           S9-INT-RECORD-ORDER
            .
      *
      *
       B050-EXIT.
            EXIT.
      *
       B100-COMPUTE-DOLLARS.
      *
            MOVE ZEROS                 TO WS-DEDUCTIBLE-AMOUNT
                                          WS-COINSURANCE-AMOUNT
                                          WS-COPAY-AMOUNT
                                          WS-REIMBURSMENT
                                          WS-OOP-AMT
                                          WS-DED1-STEP-RED
                                          WS-DED2-STEP-RED
                                          WS-COPAY-STEP-RED
            MOVE SPACES                TO DED1-VFC
                                          DED2-VFC
                                          COPAY-VFC
            INITIALIZE  CSD-CSX-CLAIM-VFC-IX
            MOVE 'N'                   TO WS-SELECT-RECORD
            IF (WF-BENE-RSN-CD (1, 1) NOT = 096)
               SET SELECT-RECORD       TO TRUE
468578*****************************************************************
468578*   LESSER LOGIC                                                *
468578*****************************************************************
468578         IF ( ( WF-NT-INVT-CD = 'LESS' ) OR
468578              ( CHN-INVES2 = 'LESS' ) OR
468578              ( CHN-INVES3 = 'LESS' ) OR
468578              ( CHN-INVES4 = 'LESS' ) OR
468578              ( CHN-INVES5 = 'LESS' ) )
468578            COMPUTE WS-DED1-STEP-RED =
468578                 + WF-PREV-BEN-VAL (WFX-GRPF-LN-ITEMS 1)
468578                 - WF-DED1-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
468578         ELSE
468578            COMPUTE WS-DED1-STEP-RED =
                       + WF-COVER-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
                       - WF-DED1-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
468578         END-IF
               INITIALIZE ED-IX-N
                          ED-3-O1
               MOVE WF-VAL-FUNC-CD (WFX-GRPF-LN-ITEMS, 1, 6)
                                       TO ED-IX-N
               MOVE 'ETUCDFCN'         TO ED-TABLE
               MOVE SPACES             TO CLAIM-VFC-GRP
               PERFORM E100-EDT-14   THRU E100-X
               MOVE ED-3-O1            TO DED1-VFC
               MOVE 'CTECSDSX'         TO ED-TABLE
               PERFORM E200-EDT-4-6  THRU E200-EXIT
                 IF WS-DED1-STEP-RED NOT = ZEROS
                    IF CLAIM-VFC-D1-COPAY = 'Y'
                       ADD WS-DED1-STEP-RED
                                      TO WS-COPAY-AMOUNT
                    END-IF
                 END-IF
               COMPUTE WS-DED2-STEP-RED =
                       + WF-DED1-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
                       - WF-DED2-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
               INITIALIZE ED-IX-N
                          ED-3-O1
               MOVE WF-VAL-FUNC-CD (WFX-GRPF-LN-ITEMS, 1, 7)
                                       TO ED-IX-N
               MOVE 'ETUCDFCN'         TO ED-TABLE
               MOVE SPACES             TO CLAIM-VFC-GRP
               PERFORM E100-EDT-14   THRU E100-X
               MOVE ED-3-O1            TO DED2-VFC
               MOVE 'CTECSDSX'         TO ED-TABLE
               PERFORM E200-EDT-4-6  THRU E200-EXIT
               IF WS-DED2-STEP-RED NOT = ZEROS
                  IF CLAIM-VFC-D2-COPAY = 'Y'                             102503
                     ADD WS-DED2-STEP-RED
                                      TO WS-COPAY-AMOUNT
                  ELSE
                     IF CLAIM-VFC-D2-MANAGED = 'Y'
                        CONTINUE
                     ELSE
                        ADD WS-DED2-STEP-RED
                                      TO WS-DEDUCTIBLE-AMOUNT
                     END-IF
                  END-IF
               END-IF
      *
               COMPUTE WS-COPAY-STEP-RED =
                       + WF-DED2-STEP-VAL (WFX-GRPF-LN-ITEMS 1)
                       - WF-COPAY-STEP-VAL(WFX-GRPF-LN-ITEMS 1)
               INITIALIZE ED-IX-N
                          ED-3-O1
               MOVE WF-VAL-FUNC-CD (WFX-GRPF-LN-ITEMS, 1, 8)
                                      TO ED-IX-N
               MOVE SPACES            TO CLAIM-VFC-GRP
               MOVE 'ETUCDFCN'        TO ED-TABLE
               PERFORM E100-EDT-14  THRU E100-X
               MOVE ED-3-O1           TO COPAY-VFC
               MOVE 'CTECSDSX'        TO ED-TABLE
               PERFORM E200-EDT-4-6 THRU E200-EXIT
               IF WS-COPAY-STEP-RED NOT = ZEROS
                  IF CLAIM-VFC-CP-COPAY = 'Y'                             102503
                     ADD WS-COPAY-STEP-RED
                                      TO WS-COPAY-AMOUNT
                  ELSE
                     IF CLAIM-VFC-CP-MANAGED = 'Y'                        102503
                        CONTINUE
                     ELSE
                        ADD WS-COPAY-STEP-RED
                                      TO WS-COINSURANCE-AMOUNT
                    END-IF
                  END-IF
               END-IF
               IF WF-BENE-RMB-AMT (WFX-GRPF-LN-ITEMS 1) > ZERO
                  MOVE WF-BENE-RMB-AMT
                      (WFX-GRPF-LN-ITEMS WFX-GRPF-BEN)
                                      TO WS-REIMBURSMENT
               END-IF
      *
      *
               IF WS-COPAY-AMOUNT > 0
                  ADD WS-COPAY-AMOUNT   TO WS-COINSURANCE-AMOUNT
               END-IF
      *
                  COMPUTE WS-OOP-AMT =
                          WS-DEDUCTIBLE-AMOUNT + WS-COINSURANCE-AMOUNT
      *

226911      ELSE
226911         ADD 1 TO OUTPUT-RECORD-BYPASS
            END-IF
            .
      *
       B100-EXIT.
            EXIT.
      *
      *
       B210-INITIALIZE.
            INITIALIZE                 WS-CLAIM-INVES2
                                       WS-CLAIM-INVES3
                                       WS-CLAIM-INVES4
                                       WS-CLAIM-INVES5
                                       WS-SURG-PRCD
                                       WS-PRIM-DIAG-CD
                                       WS-ADM-DIAG-CD
                                       WS-DRG-DIAG-1
                                       WS-DRG-DIAG-2
                                       WS-DRG-DIAG-3
                                       WS-DRG-DIAG-4
                                       WS-DRG-PROC-1
                                       WS-DRG-PROC-2
                                       WS-DRG-PROC-3
                                       WS-DRG-PROC-4
                                       WS-SURG-PRCD10
                                       WS-10PRIM-DIAG
                                       WS-10ADMT-DIAG
                                       WS-10DIAG01
                                       WS-10DIAG02
                                       WS-10DIAG03
                                       WS-10DIAG04
                                       WS-10PROC-CD02
                                       WS-10PROC-CD03
                                       WS-10PROC-CD04
                                       WS-10PROC-CD05
                                       WS-LINE-DIAG
                                       WS-LINE-DIAG10
                                       WS-NT-INVT-CD
                                       WS-LN-NT-INVT-CD
                                       WS-PKG-NO
                                       WS-HSC
                                       WS-ENCD-VALUE
                                       WS-SERVICE-CLASS
            .
      *
      *
       B210-EXIT.
            EXIT.
      ******************************************************************
      * THIS PARAGRAPH CONVERTS THE PRIOR ADJ DATE TO INTEGER.
      ******************************************************************
      *
       A250-CHECK-PRIOR-ADJ-DATE.
      *
           INITIALIZE WS-PRIOR-ADJ-DTE
                      WS-PRIOR-DATE
           MOVE WF-ADDT    TO WS-ADDT-PRIOR
           IF WS-ADDT-PRIOR (1:1) = 1
              MOVE '19'               TO WS-PRIOR-ADJ-DTE (1:2)
              MOVE WS-ADDT-PRIOR(2:2) TO WS-PRIOR-ADJ-DTE (3:2)
              MOVE WS-ADDT-PRIOR(4:4) TO WS-PRIOR-ADJ-DTE (5:4)
           ELSE
              MOVE '20'               TO WS-PRIOR-ADJ-DTE (1:2)
              MOVE WS-ADDT-PRIOR(2:2) TO WS-PRIOR-ADJ-DTE (3:2)
              MOVE WS-ADDT-PRIOR(4:4) TO WS-PRIOR-ADJ-DTE (5:4)
           END-IF
           COMPUTE WS-PRIOR-DATE =
                             FUNCTION INTEGER-OF-DATE(WS-PRIOR-ADJ-DTE).
      *
       A250-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH IS TO CALCULATE THE DOLLAR AMOUNTS FROM THE
      * PRIOR CLAIM HISTORY.
      ******************************************************************
       A270-POPULATE-VALUES.
            MOVE SPACES             TO S9-INT-VERSION
                                       S9-INT-RECORD-TYP
                                       S9-INT-RECORD-TS
                                       S9-INT-RECORD-ID
                                       S9-INT-SENDER-TYP
                                       S9-INT-SENDER-ID
                                       S9-INT-SENDER-NOTE
                                       S9-INT-SENDER-CLMNUM
                                       S9-INT-REASON-CODE
                                       S9-INT-ADJ-DATE
                                       S9-INT-AMT-TYP
                                       S9-INT-CARRIER
                                       S9-INT-ACCOUNT-NUM
                                       S9-INT-GROUP
                                       S9-INT-MEMBER-ID
                                       S9-INT-MEMBER-FIRST-NM
                                       S9-INT-MEMBER-MID-INIT
                                       S9-INT-MEMBER-LAST-NM
                                       S9-INT-MEMBER-GENDER
                                       S9-INT-MEMBER-REL-CD
                                       S9-INT-SUB-ID
                                       S9-INT-RETURN-CD
            MOVE ZEROES TO S9-INT-MEMBER-DOB
                           S9-INT-TOT-ADJ
                           S9-INT-RECORD-ORDER
           MOVE WS-GROUP                TO S9-INT-ACCOUNT-NUM
           MOVE WS-SUBGRP               TO S9-INT-GROUP
           IF WS-INTER-ALT-ID NOT = LOW-VALUES AND SPACES
              MOVE WS-INTER-ALT-ID      TO S9-INT-MEMBER-ID
           ELSE
              MOVE S9-SUBSCRIBER-ID(4:9) TO S9-INT-SUB-ID
           END-IF
           MOVE S9-SUBSCRIBER-ID(4:9)   TO S9-INT-OUT-RECORD(421:9)
           MOVE '10'                    TO S9-INT-VERSION
           MOVE 'D'                     TO S9-INT-RECORD-TYP
           MOVE 'M'                     TO S9-INT-SENDER-TYP
           MOVE 'EMPIRENY'              TO S9-INT-SENDER-ID
           MOVE 'MED'                   TO S9-INT-REASON-CODE
           MOVE 'PSI3552'               TO S9-INT-CARRIER
           MOVE S9-DEPENDENT-NUMBER     TO WS-DEP-NO
           MOVE WS-DEP-NO               TO S9-INT-OUT-RECORD(442:3)
           IF ( WS-OOP-AMT NOT EQUAL TO ZEROES )
              MOVE 'Y'   TO WS-NON-DENIED-LINE-SW1
           END-IF
      *    MOVE WF-STRT-SVDT (1)        TO WS-EARLIEST-SSDT
      *                                    WS-EDDT
      *    MOVE WF-END-SVDT  (1)        TO WS-LATEST-ESDT
DEBUG *    DISPLAY 'WF-END-SVDT-' WF-END-SVDT(1)
DEBUG *    DISPLAY 'WF-STRT-SVDT'WF-STRT-SVDT (1)
      *    PERFORM VARYING WF-LINE  FROM 1 BY 1
      *      UNTIL WF-LINE > WF-LN-ITEM-CNT
      *      IF WF-STRT-SVDT (WF-LINE) < WS-EARLIEST-SSDT
      *         MOVE WF-STRT-SVDT (WF-LINE)
      *                                 TO WS-EARLIEST-SSDT
      *                                    WS-EDDT
      *      END-IF
      *      IF WF-END-SVDT  (WF-LINE) > WS-LATEST-ESDT
      *         MOVE WF-END-SVDT  (WF-LINE)
      *                                 TO WS-LATEST-ESDT
      *      END-IF
      *    END-PERFORM
           IF NON-DENIED-LINE-FND
              IF FIRST-TIME1
                  MOVE WF-STRT-SVDT (WFX-GRPF-LN-ITEMS)
                                    TO WS-EARLIEST-SSDT
                                       WS-EDDT
                  MOVE WF-END-SVDT  (WFX-GRPF-LN-ITEMS)
                                    TO WS-LATEST-ESDT
                  MOVE 'N' TO WS-FIRST-TIME1
              END-IF
             IF WF-STRT-SVDT (WFX-GRPF-LN-ITEMS)
                             < WS-EARLIEST-SSDT
                MOVE WF-STRT-SVDT (WFX-GRPF-LN-ITEMS)
                                        TO WS-EARLIEST-SSDT
                                           WS-EDDT
             END-IF
             IF WF-END-SVDT  (WFX-GRPF-LN-ITEMS)
                                  >  WS-LATEST-ESDT
                MOVE WF-END-SVDT  (WFX-GRPF-LN-ITEMS)
                                        TO WS-LATEST-ESDT
             END-IF
           END-IF
      *    END-PERFORM
           MOVE 'T'                     TO S9-INT-AMT-TYP
           MOVE WF-ADDT                 TO WS-ADDT
           IF WS-ADDT   (1:1) = 1
              MOVE '19'                 TO WS-CURR-ADJ-DTE (1:2)
              MOVE WS-ADDT(2:2)         TO WS-CURR-ADJ-DTE (3:2)
              MOVE WS-ADDT(4:4)         TO WS-CURR-ADJ-DTE (5:4)
           ELSE
              MOVE '20'                 TO WS-CURR-ADJ-DTE (1:2)
              MOVE WS-ADDT(2:2)         TO WS-CURR-ADJ-DTE (3:2)
              MOVE WS-ADDT(4:4)         TO WS-CURR-ADJ-DTE (5:4)
           END-IF
           MOVE WS-CURR-ADJ-DTE         TO S9-INT-RECORD-TS
           COMPUTE WS-CURR-DATE =
                   FUNCTION INTEGER-OF-DATE(WS-CURR-ADJ-DTE).
           IF WS-EDDT   (1:1) = 1
              MOVE '19'                 TO WS-EARLIEST-DT(1:2)
              MOVE WS-EDDT(2:2)         TO WS-EARLIEST-DT(3:2)
              MOVE WS-EDDT(4:4)         TO WS-EARLIEST-DT(5:4)
           ELSE
              MOVE '20'                 TO WS-EARLIEST-DT(1:2)
              MOVE WS-EDDT(2:2)         TO WS-EARLIEST-DT(3:2)
              MOVE WS-EDDT(4:4)         TO WS-EARLIEST-DT(5:4)
           END-IF
           MOVE  WS-EARLIEST-DT         TO S9-INT-ADJ-DATE

      *    MOVE WS-RECORD-TIME          TO S9-INT-RECORD-ORDER
      *    MOVE ZEROES                  TO S9-INT-SEND-TIME-S
           MOVE WF-MEM-LST-NM           TO S9-INT-MEMBER-LAST-NM
           MOVE WF-1ST-NM               TO S9-INT-MEMBER-FIRST-NM
           MOVE WF-BRTH-DT              TO WS-DOB
           MOVE SPACES                  TO S9-INT-MEMBER-REL-CD
           IF WS-DOB (1:1) = '1'
              MOVE '19'              TO S9-INT-MEMBER-DOB(1:2)
              MOVE WS-DOB(2:6)   TO S9-INT-MEMBER-DOB(3:6)
           ELSE
              MOVE '20'              TO S9-INT-MEMBER-DOB(1:2)
              MOVE WS-DOB(2:6)   TO S9-INT-MEMBER-DOB(3:6)
           END-IF
           IF (WS-NT41-DEDOX-FOUND = 'Y' ) OR
              (WS-NT41-DEDIX-FOUND = 'Y' )
              MOVE '1' TO S9-INT-TOT-ADJ
           ELSE
              MOVE '2' TO S9-INT-TOT-ADJ
           END-IF
           .
      *
       A270-EXIT.
            EXIT.
      ******************************************************************
      * THIS PARAGRAPH ACCESS THE GROUP  GNT41 DATABASE.
      ******************************************************************
      *
       A240-GNT41-LOOKUP.
      *
           MOVE 'N' TO WS-CALENDER-YEAR
           MOVE 'N' TO WS-CLM-GNT41-DATCHK
           MOVE 'N' TO WS-NT41-DED-FOUND
           MOVE 'N' TO WS-NT41-DEDI-FOUND
           MOVE 'N' TO WS-NT41-DEDO-FOUND
           MOVE 'N' TO WS-NT41-COINS-FOUND
           MOVE 'N' TO WS-NT41-COINSI-FOUND
           MOVE 'N' TO WS-NT41-COINSO-FOUND
           MOVE 'N' TO WS-NT41-COPAY-FOUND
           MOVE 'N' TO WS-NT41-COPAYI-FOUND
           MOVE 'N' TO WS-NT41-COPAYO-FOUND
           MOVE 'N' TO WS-NT41-DEDIX-FOUND
           MOVE 'N' TO WS-NT41-DEDOX-FOUND
           MOVE WF-GRP-KEY-ID (WFX-GRPF-LN-ITEMS, 1)
                                  TO VSGPGRU-KEY1
                                     VSGPGRU-KEY2
      *
           PERFORM A251-GET-VSGPNTE THRU A251-EXIT
           MOVE GO-VSGPNTEC       TO GO-S9MGNT41

           IF NOTE41-FOUND
              IF (GO-NT41-VENNAME = 'OR' ) AND
                 (GO-NT41-VENTYPE = 'R' )
              MOVE GO-NT41-EFF-DATE  TO WS-GO-NT41-EFF-DATE
              MOVE GO-NT41-TRM-DATE  TO WS-GO-NT41-TRM-DATE
              MOVE GO-NT41-PKG-NO    TO WS-GO-NT41-PKG-NO
              MOVE GO-NT41-VENTYPE   TO WS-GO-NT41-VENTYPE
              IF WS-GO-NT41-TRM-DATE = ZEROES
                 MOVE '2991231' TO WS-GO-NT41-TRM-DATE
              END-IF
              MOVE WF-DSCH-DT TO WS-DSCH-DT
              IF CHN-FRMTYP = '1'
                 IF (WS-DSCH-DT >= WS-GO-NT41-EFF-DATE) AND
                    (WS-DSCH-DT <= WS-GO-NT41-TRM-DATE)
                    SET CLM-GNT41-DATCHK TO TRUE
                 END-IF
              ELSE
                 IF (WS-STRT-SVDT >= WS-GO-NT41-EFF-DATE) AND
                    (WS-STRT-SVDT <= WS-GO-NT41-TRM-DATE) AND
                    (WS-END-SVDT >= WS-GO-NT41-EFF-DATE) AND
                    (WS-END-SVDT<= WS-GO-NT41-TRM-DATE)
                    SET CLM-GNT41-DATCHK TO TRUE
                 END-IF
              END-IF
      *
              IF CLM-GNT41-DATCHK
                IF  WS-GO-NT41-PKG-NO = (WS-PKGNO OR 'ALL')
                    IF GO-NT41-VENNAME = 'OR'
                      IF WS-GO-NT41-VENTYPE = 'R'
      *
                       IF WS-INT-NET-IND= 'I'
                          IF (GO-NT41-DEDO   = 'I' OR 'B' )
                           MOVE  'Y'  TO     WS-NT41-DEDI-FOUND
                          END-IF
                          IF (GO-NT41-DEDO   = 'X') AND DIOCERSE-GRP
                           MOVE  'Y'  TO     WS-NT41-DEDIX-FOUND
                          END-IF
                          IF  (GO-NT41-COINSO = 'I' OR 'B' )
                           MOVE  'Y'  TO     WS-NT41-COINSI-FOUND
                          END-IF
                          IF  (GO-NT41-COPAYO = 'I' OR 'B' )
                           MOVE  'Y'  TO     WS-NT41-COPAYI-FOUND
                          END-IF
                          IF WS-NT41-DEDI-FOUND = 'Y' OR
                             WS-NT41-COINSI-FOUND = 'Y' OR
                             WS-NT41-COPAYI-FOUND = 'Y' OR
                             WS-NT41-DEDIX-FOUND  = 'Y'
                             SET DATE-FOUND TO TRUE
                          ELSE
                             MOVE 'N' TO WS-DATE-FOUND
                          END-IF
                       ELSE
      *
      *                IF CLAIM-VFC-R3-RULEO = 'Y'
                       IF WS-INT-NET-IND= 'O'
                          IF (GO-NT41-DEDO   = 'O' OR 'B' )
                           MOVE  'Y'  TO WS-NT41-DEDO-FOUND
                          END-IF
                          IF (GO-NT41-DEDO   = 'X'  ) AND DIOCERSE-GRP
                           MOVE  'Y'  TO WS-NT41-DEDOX-FOUND
                          END-IF
                          IF  (GO-NT41-COINSO = 'O' OR 'B' )
                           MOVE  'Y'  TO   WS-NT41-COINSO-FOUND
                          END-IF
                          IF  (GO-NT41-COPAYO = 'O' OR 'B')
                           MOVE  'Y'  TO   WS-NT41-COPAYO-FOUND
                          END-IF
                          IF WS-NT41-DEDO-FOUND   = 'Y' OR
                             WS-NT41-COINSO-FOUND = 'Y' OR
                             WS-NT41-COPAYO-FOUND = 'Y' OR
                             WS-NT41-DEDOX-FOUND = 'Y'
                             SET DATE-FOUND TO TRUE
                          ELSE
                             MOVE 'N' TO WS-DATE-FOUND
                          END-IF
      *                ELSE
      *
      *                IF RULE3-VFC = '000'
      *                   IF GO-NT41-DEDO   = 'N'
      *                    MOVE  'Y' TO  WS-NT41-DED-FOUND
      *                   END-IF
      *                   IF GO-NT41-COINSO = 'N'
      *                    MOVE  'Y' TO WS-NT41-COINS-FOUND
      *                   END-IF
      *                   IF GO-NT41-COPAYO = 'N'
      *                    MOVE  'Y' TO  WS-NT41-COPAY-FOUND
      *                   END-IF

      *                   IF WS-NT41-DED-FOUND   ='Y'OR
      *                      WS-NT41-COINS-FOUND ='Y'OR
      *                      WS-NT41-COPAY-FOUND ='Y'
      *                      SET DATE-FOUND TO TRUE
      *                   ELSE
      *                      MOVE 'N' TO WS-DATE-FOUND
      *                   END-IF
      *                END-IF
                       END-IF
                       END-IF
                    END-IF
                  END-IF
              END-IF
              IF DATE-FOUND
                 MOVE 'N' TO WS-CALENDER-YEAR
                 MOVE ZEROES TO WS-GO-NT41-PLANMD
                 IF GO-NT41-PLANMD NOT = ZEROES
                    MOVE GO-NT41-PLANMD TO WS-GO-NT41-PLANMD
                    IF WS-GO-NT41-PLANMD = '0101'
                      CONTINUE
                    ELSE
                      SET OFF-CAL-YEAR-FOUND  TO TRUE
                    END-IF
                 END-IF
              END-IF
              END-IF
           END-IF.
      *
       A240-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH CALLS THE NOTE41 SEGMENT OF THE GROUP DATABASE.
      ******************************************************************
      *
       A251-GET-VSGPNTE.
      *
           MOVE 'VSGPGRU '   TO VSGPGRU-SSA-Q(1:8)
           MOVE '*--'        TO VSGPGRU-CMD-BOOLEAN
           MOVE '('          TO VSGPGRU-SSA-Q(12:1)
           MOVE ')'          TO VSGPGRU-SSA-Q(52:1)
           MOVE 'GRUPKEYI'   TO VSGPGRU-KEYFIELD1
                                VSGPGRU-KEYFIELD2
           MOVE '*'          TO VSGPGRU-BOOLEAN-AND
           MOVE '>='         TO VSGPGRU-OPER1
           MOVE '<='         TO VSGPGRU-OPER2
           IF FIRST-TIME
              MOVE GU        TO IMS-FUNCTION
              MOVE 'N'       TO WS-FIRST-TIME
           ELSE
              MOVE GN        TO IMS-FUNCTION
           END-IF
           MOVE 'VSGPNTE '   TO VSGPNTE-SSA-Q
           MOVE '*--'        TO VSGPNTE-CMD-CD
           MOVE '(GPDFNTEI'  TO VSGPNTE-SEG1
           MOVE 'GPDFNTEI'   TO VSGPNTE-SEG2
           MOVE '>='         TO VSGPNTE-OPER1
           MOVE '<='         TO VSGPNTE-OPER2
           MOVE '*'          TO VSGPNTE-BOOLEAN-AND
           MOVE ')'          TO VSGPNTE-CLOSE
           MOVE '41'         TO VSGPNTE-PAGE1
                                VSGPNTE-PAGE2
           MOVE ZERO         TO VSGPNTE-NOTE1
           MOVE '999'        TO VSGPNTE-NOTE2
      *
           CALL  'CBLTDLI'  USING  IMS-FUNCTION
                                   VFGGRP10-PCB
                                   GO-VSGPNTEC
                                   VSGPGRU-SSA-Q
                                   VSGPNTE-SSA-Q.
           IF PCB-RETURN-CODE = SPACES
              MOVE 'Y'   TO WS-NOTE41-FOUND
           ELSE
              IF PCB-RETURN-CODE = 'GE'
                 OR PCB-RETURN-CODE = 'GB'
                 SET END-OF-NOTES TO TRUE
              END-IF
           END-IF.
      *
       A251-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH CALLS THE ENCODE/DECODE TABLE.
      ******************************************************************
      ***************************************************************
      * THIS PARAGRAPH LOADS NOTES                                  *
      ***************************************************************
      *
       A205-LOAD-NOTES.
      *
           MOVE LOW-VALUES TO NOTE-DATA-AREA.
           MOVE SPACES TO NOTE-FLAG.

           PERFORM VARYING CHN-SUB FROM 1 BY 1
                  UNTIL CHN-SUB > 100
                  OR NOTE-FLAG = '0' OR 'E'
              MOVE CJ-VSCPNTEC (CHN-SUB) TO NOTE-W
              IF NOTE-LENGTH-W = SPACES OR LOW-VALUES
                 OR NOTE-LENGTH-NUM-W NOT = +56
                 IF NOTE-FLAG = ' '
                    MOVE '0' TO NOTE-FLAG
                 ELSE
                    MOVE 'E' TO NOTE-FLAG
                 END-IF
                 ELSE
                   IF NOTE-PAGE-NUM-W NOT NUMERIC
                      OR NOTE-NOTE-NUM-W NOT NUMERIC
                      NEXT SENTENCE
                   ELSE
                    EVALUATE NOTE-PAGE-NUM-W
                     WHEN 01
                        EVALUATE NOTE-NOTE-NUM-W
                          WHEN 02
                            MOVE NOTE-DATA-W TO NOTE-01-02-DATA
                          WHEN 06
                            MOVE NOTE-DATA-W TO NOTE-01-06-DATA
                          WHEN 10
                            MOVE NOTE-DATA-W TO NOTE-01-10-DATA
                          WHEN 14
                            MOVE NOTE-DATA-W TO NOTE-01-14-DATA
                          WHEN 30
                            MOVE NOTE-DATA-W TO NOTE-01-30-DATA
                            WHEN OTHER
                              CONTINUE
                          END-EVALUATE
                       WHEN 97
                          MOVE 'X' TO NOTE-97-FLAG
                          EVALUATE NOTE-NOTE-NUM-W
                            WHEN 01
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-01-DATA
                            WHEN 02
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-02-DATA
                            WHEN 03
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-03-DATA
                            WHEN 04
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-04-DATA
                            WHEN 05
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-05-DATA
                            WHEN 06
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-06-DATA
                            WHEN 07
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-07-DATA
                            WHEN 08
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-08-DATA
                            WHEN 09
                               MOVE NOTE-DATA-W TO VLCSF-NOTE-97-09-DATA
                            WHEN OTHER
                               CONTINUE
                          END-EVALUATE
                          WHEN OTHER
                             CONTINUE
                         END-EVALUATE
                        END-IF
                       END-IF
                  END-PERFORM.
       A205-EXIT.
            EXIT.
      *
       B200-SHIFT-SUBSCRIBER-ID.
      *
            INITIALIZE WS-SAVE-SUBSCRIBER
            MOVE S9-SUBSCRIBER-ID(4:9)   TO  WS-SUB-1.
            IF  WS-SAVE-SUBSCRIBER-1 NOT NUMERIC
                MOVE WS-SAVE-SUBSCRIBER-2-9
                  TO WS-SAVE-SUBSCRIBER-1-8
                MOVE SPACE    TO WS-SAVE-SUBSCRIBER-9
                MOVE WS-SUB-2 TO WS-SUBID
            ELSE
                MOVE S9-SUBSCRIBER-ID(4:9) TO WS-SUBID
            END-IF.
      *
       B200-EXIT.
            EXIT.
      *
       B300-ALT-ID.
      ************************************************************
      * THIS PARAGRAPH CALL THE ALT-ID MODULE S9MALTCB TO GET THE*
      * ALT-ID TO THE CORROSPONDING SUBSCRIBER ID.               *
      *                                                          *
      ************************************************************
           INITIALIZE ALT-ID-COMM-AREA
           MOVE SPACES                   TO WS-HOLD-SUBR-NO
                                            WS-INTER-ALT-ID
                                            MIPI-GROUP-ID
                                            MIPI-SUBSCRIBER-ID
                                            MIPI-STATE-CODE
           MOVE WS-SUBID                 TO WS-HOLD-SUBR-NO(4:9)
           MOVE WS-HOLD-SUBR-9           TO MIPI-SUBSCRIBER-ID

           MOVE WS-GROUP                 TO MIPI-GROUP-ID


           MOVE 'X'                      TO MIPI-REQUEST-TYPE
           SET IMS-CALL-DLI              TO TRUE
           CALL S9MALTCB USING  ALT-ID-COMM-AREA
                                ZFMAXF10-PCB
                                ZFMAXX11-PCB
                                ZFMAGP10-PCB
           IF MIPO-RC-SUCCESS
              MOVE MIPO-MEMBER-ID        TO WS-INTER-ALT-ID
           END-IF
           .
       B300-EXIT.
            EXIT.
      *
      ******************************************************************
      * THIS PARAGRAPH CALLS THE ENCODE/DECODE TABLE.
      ******************************************************************
      *
      *
       E100-EDT-14.
      *
           MOVE '14'                   TO B14
           MOVE SPACES                 TO ED-ARGUE ED-OUTPUT
           MOVE ZEROS                  TO STATUS-CD
           CALL ED-TABLE            USING ED-ARGUE
                                          ED-IX-N
                                          B14
                                          STATUS-CD.
           IF STATUS-CD NOT = ZEROES
              MOVE ED-IX-N             TO ED-3-O1-N
              MOVE ZEROES              TO RETURN-CODE
           ELSE
              MOVE ED-3-INPUT          TO ED-3-O1
              MOVE ED-3-DESC           TO ED-3-O2.
      *
       E100-X.
           EXIT.
      *
      *
       E200-EDT-4-6.
      *
           MOVE LOW-VALUES             TO CSD-CSX-INDEX
           MOVE 4                      TO B4
           MOVE ZEROS                  TO STATUS-CD
           IF ED-IX-N NOT > +999
              MOVE ED-IX-N             TO CSD-CSX-CLAIM-VFC-IX             CL**6
              CALL ED-TABLE         USING CSD-CSX-CLAIM-VFC-IX
                                          CSD-CSX-INDEX
                                          B4
                                          STATUS-CD
           ELSE
              MOVE ED-3-O1             TO CSD-CSX-CLAIM-VFC-X3             CL**6
              CALL ED-TABLE         USING CSD-CSX-CLAIM-VFC-X3
                                          CSD-CSX-INDEX
                                          B4
                                          STATUS-CD
           END-IF
           IF STATUS-CD = ZEROS
              MOVE 6                   TO B6
              MOVE ZEROS               TO STATUS-CD
              MOVE SPACES              TO CSD-CSX-DESC10
              CALL ED-TABLE         USING CSD-CSX-DESC10
                                          CSD-CSX-INDEX
                                          B6
                                          STATUS-CD
              IF STATUS-CD = ZEROS
                 MOVE CSD-CSX-DESC10   TO CLAIM-VFC-GRP
              ELSE
                 MOVE ZEROES         TO RETURN-CODE
              END-IF
           ELSE
              MOVE ZEROES         TO RETURN-CODE
           END-IF.
      *
       E200-EXIT.
            EXIT.
      *
      *
       E300-EDT-8.
      *
           MOVE '8'                TO B8
           MOVE SPACES             TO WS-ENCD-ARGUMENT-3
           MOVE ZERO               TO WS-TABLE-STATUS
           CALL ED-TABLE        USING WS-ENCD-ARGUMENT-3
                                      WS-ENCD-VALUE
                                      B8
                                      WS-TABLE-STATUS.
           IF WS-TABLE-STATUS = ZEROES
              MOVE WS-ENCD-INPUT-3 TO WS-SERVICE-CLASS
           ELSE
              MOVE ZEROES         TO RETURN-CODE.
      *
       E300-X. EXIT.
      *
      *
       E500-EDT-10.

           MOVE SPACES             TO ED-ARGUE ED-OUTPUT.
           MOVE ZEROS              TO STATUS-CD
           CALL ED-TABLE        USING ED-ARGUE
                                      ED-CD
                                      B10
                                      STATUS-CD.
           IF STATUS-CD NOT = ZEROES
               MOVE LOW-VALUES     TO ED-LV
               MOVE ED-IX-N        TO ED-1-O1-N
               MOVE ZEROES         TO RETURN-CODE
           ELSE
               MOVE ED-1-INPUT     TO ED-1-O1
               MOVE ED-1-DESC      TO ED-1-O2.
       E500-X. EXIT.
      *
      ***************************************************************
      * THIS PARAGRAPH DISPLAY THE CONTROL COUNTS AT THE END OF     *
      * THE JOB                                                     *
      ***************************************************************
      *
       F100-DISPLAY-COUNTS.
      *
           DISPLAY '*******     END OF JOB CONTROL COUNTS   *******'.
           DISPLAY ' '.
           DISPLAY 'TOTAL INPUT RSI RECORD   = ' INPUT-RECORD-COUNT.
           DISPLAY 'READ'
           DISPLAY ' '.
           DISPLAY 'TOTAL OUTPUT RECORD      = ' OUTPUT-RECORD-COUNT.
           DISPLAY 'WRITTEN TO INTERMEDIATE'
           DISPLAY 'FILE'
           DISPLAY ' '.
           DISPLAY 'TOTAL INPUT CLAIMS       = ' OUTPUT-RECORD-BYPASS.
           DISPLAY 'BYPASSED'
           DISPLAY ' '.
           DISPLAY ' '.
           DISPLAY '*******     END OF MODULE S9COPT20      *******'.
           DISPLAY ' '.
      *
       F100-EXIT.
            EXIT.
      *
      *****************************************************************
      *                     END OF MODULE S9COPT20                    *
      *****************************************************************
