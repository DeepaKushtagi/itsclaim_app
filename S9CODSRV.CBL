       IDENTIFICATION DIVISION.
       PROGRAM-ID.    S9CODSRV.
       AUTHOR.        TANNISTHA
       DATE-WRITTEN.  DEC 2014.
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *            INCOMING DAILY FILE PROCESSING PROGRAM              *
      *            --------------------------------------              *
      *                                                                *
      *   THIS PROGRAM READS THE INCOMING FILE FROM ODS SCRIPTING      *
      *   CREATES A POSITIVE AND NEGATIVE INTERMEDIATE FILE.           *
      *   THIS AS WELL VALIDATES THE MEMBRERSHIP INFORMATION .         *
      *   HEADER DATE IS VALIDATED TO ENSURE THE CORRECT FILE IS       *
      *   RECEIVED FROM ODS.                                           *
      *----------------------------------------------------------------*
      *                                                                *00350000
      *  REVISION LOG                                                  *00340000
      *  =============                                                 *
      *                                                                *
      *  REQUEST #   PROGRAMMER         DATE        DESCRIPTION        *
      *  ---------   ------------  ---------   ----------------------- *00370000
      *  CQ803447    TANNISTHA     12/12/2014  INITIAL RELEASE         *
      *  CQ1363870   TANNISTHA     03/13/2015  HANDLING SUB ID OF      *
      *                                        LESS THAN 9 BYTES       *
      *                                        ERROR REPORT FIX        *
      *                                        KEYWORD RULE3 VFC FIX   *
      *  CQ1439681   TANVI         04/17/2015  RICHLINE INFERTILITY    *
      *                                        ACCUMULATIONS BETWEEN   *
      *                                        EMPIRE AND ESI FOR      *
      *                                        NEW INSTALLATION.       *
      *                                        TAGGED WITH - RICHLN    *
      *  CQ1524066   MANEESHA      04/17/2015  HCR EHB -  MODIFY       *
      *                                        CLHST FOR REVERSALS     *
      *                                        FROM ODS                *
      *  CQ1531606   TANNISTHA     04/17/2015  REJECT CLAIMS WITH DED  *
      *                                        ACCUM NAMES BACK TO ODS *
      *                                        TAGGED WITH -  153160   *
      *  CQ1557250   TANNISTHA     06/12/2015  MODIFICATION FOR DAILY  *
      *                                        ZERO DOLLAR ERR REPORTS *
      *                                        TAGGED WITH -  155725   *
      *  CQ1550035   TANNISTHA     06/12/2015  IGNORE ACCUM REASON     *
      *                                        CODES ON IB ODS FILE    *
      *                                        TAGGED WITH -  155003   *
      *  CQ1589169   TANNISTHA     07/17/2015  IGNORE INBOUND DEDUCTIB-*
      *                                        -LE ACCUN NAMES.        *
      *                                        TAGGED WITH -  158916   *
145266*  CQ1452665   TUSHAR        09/11/2015  IGNORE PRODUCT TYPE CODE*
      *                                        RCO WHEN RECEIVED IN THE*
      *                                        IB ODS FILE -  1452665  *
      *  CQ1714977   ADYA          10/09/2015  FIX FILE                *
      *                                        S9CODSRVODSERRRPT REPORT*
      *                                        TO SHOW DOLLAR VALUES ON*
      *                                        REVIEW GNT 41 ERROR     *
      *                                        TAGGED WITH - 171497    *
      *  CQ1946457   RAVI RAJANA   07/15/2016  DO NOT ABEND IF PCC IN  *
      *                                        BYTES 1-4 IS 'XXXX'.    *
      *                                        GENERATE ERROR REPORT IF*
      *                                        NOT FOUND.              *
      *                                        TAGGED WITH - 194645    *
      *  RTC333803   SWAPNA        03/17/2017  PACKAGES NEED TO BE *
      *                                        CHECKED FOR AN ACTIVE   *
      *                                        DATE RANGE              *
      *  RTC440881   SWAPNA        04/21/2017  MEMBERS FALLING OUT     *
      *                                        WITH VALID GROUP NOT    *
      *                                             FOUND              *
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-FROM ODS*-*-*-*-*-*-*-

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-3090.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ODS-INFILE   ASSIGN TO S9CE001.
           SELECT ODS-DATFILE  ASSIGN TO S9CE002.
           SELECT ODS-POSFILE  ASSIGN TO S9CE003.
           SELECT ODS-NEGFILE  ASSIGN TO S9CE004.
           SELECT ODS-ERRFILE  ASSIGN TO S9CE005.
1073       SELECT ODS-RESPONSE ASSIGN TO S9CE006.
           SELECT ODS-RESPONS1 ASSIGN TO S9CE007.
155725     SELECT ODS-ZEROERR  ASSIGN TO S9CE008.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ODS-INFILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 4500 CHARACTERS.
       01  INPUT-REC                    PICTURE X(4500).
      *
       FD  ODS-DATFILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  DATE-REC.
           05 TODAY-DATE                   PICTURE 9(8).
           05 TIMESTMP                     PICTURE 9(8).
           05 FILLER                       PICTURE X(64).
      *
       FD  ODS-POSFILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 250 CHARACTERS.
       01  POS-OUT-REC            PIC X(250).
      *
       FD  ODS-NEGFILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 250 CHARACTERS.
       01  NEG-OUT-REC            PIC X(250).
      *
       FD  ODS-ERRFILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 256 CHARACTERS.
       01  ODS-ERR-REC            PIC X(256).
      *
1073   FD  ODS-RESPONSE
1073       LABEL RECORDS ARE STANDARD
1073       BLOCK CONTAINS 0 RECORDS
1073       RECORDING MODE IS F
1073       RECORD CONTAINS 4500 CHARACTERS.
1073   01  RESPONSE-REC                 PICTURE X(4500).
      *
       FD  ODS-RESPONS1
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 4500 CHARACTERS.
       01  RESPONS1-REC                 PICTURE X(4500).
      *
155725 FD  ODS-ZEROERR
155725     LABEL RECORDS ARE STANDARD
155725     BLOCK CONTAINS 0 RECORDS
155725     RECORDING MODE IS F
155725     RECORD CONTAINS 256 CHARACTERS.
155725 01  ODS-ZERO-ERR           PIC X(256).
      *
      *
       WORKING-STORAGE SECTION.
      *
       77  WS-PROGRAM-ID             PIC X(8) VALUE IS 'S9CODSRV'.
      *
       01  WS-INPUT-D-MEM-DOB        PIC 9(07) VALUE ZEROES.
       01  WS-INPUT-D-MEM-DEP        PIC 9(03) VALUE ZEROES.
       01  WS-HIGH-DATE              PIC 9(7) COMP-3 VALUE 2991231.
       01  WS-INPUT-REL              PIC 9(01).
       01  WS-RELATION               PIC 9(01).
       01  WS-GENDER                 PIC X(01).
       01  WS-SUBGROUP               PIC X(03).
       01  WS-SUBGROUP1              PIC X(03).
136387 01  WS-SUBID                  PIC X(9) VALUE SPACES.
       01  WS-SPACE-COUNT            PIC 9(1) VALUE ZEROES.

       77  WS-SUBDB-PKG-NOTFND       PIC 9(7)  VALUE ZERO.

       01  WS-SUB                    PIC 9(4) VALUE ZEROES.
       01  WS-ACCUM                  PIC 9(4) VALUE ZEROES.
       01  WS-IN-BNFT-DTL-AMT        PIC S9(012)V9(02).
       01  WS-IN-BNFT-DTL-AMT1       PIC +9(012).9(02).
       01  WS-DED-AMOUNT-P           PIC S9(012)V9(02).
       01  WS-DED-AMOUNT-N           PIC S9(012)V9(02).
       01  WS-OOP-AMOUNT-P           PIC S9(012)V9(02).
       01  WS-OOP-AMOUNT-N           PIC S9(012)V9(02).
       01  WS-IVF-AMOUNT-P           PIC S9(012)V9(02).
       01  WS-IVF-AMOUNT-N           PIC S9(012)V9(02).
       01  WS-DEDUCT-AMOUNT          PIC S9(012)V9(02).
       01  WS-OOP-AMOUNT             PIC S9(012)V9(02).
       01  WS-IVF-AMOUNT             PIC S9(012)V9(02).
155725 01  PINDED-POS                PIC S9(012)V9(02).
155725 01  PINDED-NEG                PIC S9(012)V9(02).
155725 01  PINDED-NEG1               PIC S9(012)V9(02).
155725 01  NINDED-POS                PIC S9(012)V9(02).
155725 01  NINDED-NEG                PIC S9(012)V9(02).
155725 01  NINDED-NEG1               PIC S9(012)V9(02).
155725 01  IINOOP-POS                PIC S9(012)V9(02).
155725 01  IINOOP-NEG                PIC S9(012)V9(02).
155725 01  IINOOP-NEG1               PIC S9(012)V9(02).
155725 01  OINOOP-POS                PIC S9(012)V9(02).
155725 01  OINOOP-NEG                PIC S9(012)V9(02).
155725 01  OINOOP-NEG1               PIC S9(012)V9(02).
155725 01  FERTLMAX-POS              PIC S9(012)V9(02).
155725 01  FERTLMAX-NEG              PIC S9(012)V9(02).
155725 01  FERTLMAX-NEG1             PIC S9(012)V9(02).
       01  WS-COINS-AMT              PIC S9(12)V99.
       01  WS-MEM-DED-AMT            PIC S9(12)V99.
       01  WS-MEM-OOP-AMT            PIC S9(12)V99.
       01  WS-MEM-IVF-AMT            PIC S9(12)V99.
       01  WS-LAST-NAME              PIC X(15).
     **01  WS-AMT-PAID               PIC 9(08)V99.
       01  WS-AMT-PAID               PIC S9(12)V99.
PROD **01  WS-AMT                    PIC 9(08)V99.
PROD   01  WS-AMT                    PIC S9(07)V99.
       01  WS-INTER-DED-AMT          PIC 9(08)V99.
       01  WS-INTER-AMT-APP-OOP      PIC 9(08)V99.
       01  WS-INTER-AMT-APP-PLAN-MAX PIC 9(08)V99.
       01  WS-AMT-APP-PLAN-MAX       PIC 9(08)V99.
       01  WS-AMT-APP-INFT-MAX       PIC 9(08)V99.
PROD   01  WS-AMT-TOT                PIC 9(08)V99.
       01  WS-EFF-DATE               PIC 9(08).
       01  WS-TERM-DATE              PIC 9(08).
       01  WS-GNT-EFF-DATE           PIC 9(08).
       01  WS-GNT-TRM-DATE           PIC 9(08).
       01  WS-MBR-EFF-DT             PIC 9(07).
       01  WS-MBR-TERM-DT            PIC 9(07).
       01  WS-PKG-NOX                PIC X(3).
010915 01  WS-CON-TY-CD              PIC 9(4)  COMP.
       01  WS-NT41-BEN-EFF-MO-PRV        PIC 9(2) VALUE ZEROES.
       01  WS-NT41-BEN-EFF-MO            PIC 9(2) VALUE ZEROES.
       01  WS-LEAP-YEAR                  PIC 9(3)V99 VALUE ZEROES.
       01  LEAST-VAR                     PIC 9(2) VALUE 01.
       01  WS-YEAR                       PIC 9(1).
       01  WS-DOS-YEAR                   PIC 9(4) VALUE ZEROES.
       01  WS-PREV-YEAR                  PIC 9(4) VALUE ZEROES.
       01  WS-RUN-DATE1                  PIC 9(4) VALUE ZEROES.
       01  WS-NEXT-YEAR                  PIC 9(4) VALUE ZEROES.
       01  WS-INTER-BEN-START-DTE        PIC 9(8) VALUE ZEROES.
       01  WS-INTER-BEN-END-DTE          PIC 9(8) VALUE ZEROES.
       01  WS-BENFT-START-DTE            PIC 9(4) VALUE ZEROES.
       01  WS-IN-NTWK-CD                 PIC X(1) VALUE SPACES.
      *
       01  WS-GNT41-LOOKUP.
           05 GNT41-TABLE OCCURS    999 TIMES.
              10 WS-SI-GRP-NO-ID     PIC  X(6).
              10 WS-SI-SUBGR-NO      PIC  X(3).
              10 WS-GS-PKG-NO        PIC  9(3) COMP-3.

       01  WS-WHEN-COMPILED.
           05  WS-COMP-DATE         PIC X(08).
           05  WS-COMP-TIME         PIC X(08).
           05  FILLER               PIC X(04).
      *
       01  WS-FORMAT-DATE.
           05  WS-FORMAT-MM         PIC 99.
           05  FILLER               PIC X VALUE '/'.
           05  WS-FORMAT-DD         PIC 99.
           05  FILLER               PIC X VALUE '/'.
           05  WS-FORMAT-YY         PIC 99.
      *
       01 WS-DATE-VARIABLES.
           05 WS-TODAY-HEADER-DATE  PIC 9(08) VALUE ZEROES.
           05 WS-CC-ADDT            PIC 9(07) VALUE ZEROES.
           05 YYYYMMDD              PIC 9(08) VALUE ZEROES.
           05 WS-RUN-DATE           PIC 9(08) VALUE ZEROES.
           05 WS-RUN-TIME           PIC 9(08) VALUE ZEROES.
           05 WS-TEMP-DATE          PIC 9(08) VALUE ZEROES.
           05 WS-TEMP-DATE1         PIC 9(07) VALUE ZEROES.
           05 WS-TODAY-INPUT-DATE   PIC 9(08) VALUE ZEROES.
           05 TODAY-DATE-TEMP       PIC 9(8).
           05 FILE-DATE-HEADER-TEMP PIC 9(8).
           05 WS-FN-DATE.
               10 WS-D-YYYYMMDD     PIC 9(8).
               10 WS-D-FILLER       PIC X(13).
      *
       01 WS-RUN-DATE-T         PIC X(8).
       01 WS-RUN-TIME-T         PIC X(8).
       01  WS-END-OF-DATE-FILE      PIC X VALUE 'N'.
           88  END-OF-DATE-FILE           VALUE 'Y'.
       01  WS-END-OF-INFILE         PIC X VALUE 'N'.
           88  END-OF-INFILE              VALUE 'Y'.
333803 01  WS-END-OF-PAC                 PIC X(01) VALUE 'N'.
333803     88  END-OF-PAC                          VALUE 'Y'.
       01  WS-ERR-MSG-SW                 PIC X(01) VALUE 'N'.
           88  ERR-FN-LN-DOB                       VALUE 'Y'.
       01  WS-ERROR-SW                   PIC X     VALUE 'N'.
           88  ERROR-RPT-TRUE                      VALUE 'Y'.
PROD   01  WS-ZERO-ERROR-SW              PIC X     VALUE 'N'.
PROD       88  ERROR-ZERO-TRUE                      VALUE 'Y'.
       01  WS-END-OF-FILE                PIC X VALUE 'N'.
           88  END-OF-FILE                     VALUE 'Y'.
       01  WK-SEX-CODE            PIC X(1).
       01  WS-SEX-CODE            PIC X(1).
      *
       01  WS-RECORD-COUNTS.
           05  WS-REC-COUNT         PIC 9(10) VALUE 0.
           05  WS-POS-REC           PIC 9(10) VALUE 0.
           05  WS-NEG-REC           PIC 9(10) VALUE 0.
           05  WS-BYPASS-COUNT      PIC 9(10) VALUE 0.
           05  WS-RESPONSE-SUS      PIC 9(10) VALUE 0.
      *    05  WS-RESPONS1-SUS      PIC 9(10) VALUE 0.
           05  WS-RESPONSE-ERR      PIC 9(10) VALUE 0.
       01  WS-SUBSCRIBER-ID.
           05  IP-CS90-SUB-ID            PIC X(12).
           05  WS-IP-SUB-ID              REDEFINES
               IP-CS90-SUB-ID.
               10  WS-SPACES             PIC XXX.
               10  WS-SI-SUBID           PIC X(09).
      *
       01  GO-S9MGNT41.
           COPY S9MGNT41.
      *
       01  GO-VSGPNTEC.
           COPY VSGPNTEC.
      *
       01  WS-GO-NT41-EFF-DATE           PIC 9(7).
       01  WS-GO-NT41-TRM-DATE           PIC 9(7).
       01  WK-PKG-EFF-DT                 PIC 9(7) .
       01  WS-PKG-EFF-DT                 PIC 9(8) VALUE ZEROS.
       01  WK-PKG-TRM-DT                 PIC 9(7) .
       01  WS-PKG-TRM-DT                 PIC 9(8) VALUE ZEROS.
333803 01  WK-PCC-EFF-DT                 PIC 9(7) .
333803 01  WS-PCC-EFF-DT                 PIC 9(8) VALUE ZEROS.
333803 01  WK-PCC-TRM-DT                 PIC 9(7) .
333803 01  WS-PCC-TRM-DT                 PIC 9(8) VALUE ZEROS.
       01  WS-GO-NT41-PKG-NO             PIC X(3).
       01  WS-VENNAME                    PIC X(2) VALUE 'DS'.
       01  WS-GO-NT41-VENTYPE            PIC X(1).
       01  WS-GO-NT41-PLANMD             PIC 9(4) VALUE ZEROES.
       01  WS-STRT-SVDT                  PIC 9(7) VALUE ZEROES.
       01  WS-END-SVDT                   PIC 9(7) VALUE ZEROES.
       01  WS-ERR-LOCAL                  PIC X(97).
       01  WS-NTEC-PKGNO                 PIC X(03).
       01  WS-G41-MESSAGE          PIC X(94) VALUE 'DOLLARS NOT LOADED'.

      *
       01 WS-SWITCHES.
          05 WS-CALENDER-YEAR        PIC X    VALUE 'N'.
             88 OFF-CAL-YEAR-FOUND            VALUE 'Y'.
          05 WS-CHECK-A-NOTE         PIC X    VALUE 'N'.
             88 CHECK-A-NOTE                  VALUE 'Y'.
          05 WS-FIRST-TIME           PIC X    VALUE 'N'.
             88 FIRST-TIME                    VALUE 'Y'.
          05 WS-FIRST-TIME-HEADER    PIC X    VALUE 'N'.
             88 FIRST-TIME-HEADER             VALUE 'Y'.
          05 WS-FIRST-TIME-HEADER-GNT PIC X   VALUE 'N'.
             88 FIRST-TIME-HEADER-GNT         VALUE 'Y'.
          05 WS-FIRST-TIME-HEADER-VER PIC X   VALUE 'N'.
             88 FIRST-TIME-HEADER-VER         VALUE 'Y'.
          05 WS-FIRST-TIME-HEADER-G41 PIC X   VALUE 'N'.
             88 FIRST-TIME-HEADER-G41         VALUE 'Y'.
          05 WS-BYPASS-RECORD        PIC X    VALUE 'N'.
             88 BYPASS-RECORD                 VALUE 'Y'.
          05 WS-BYPASS-ACCUM         PIC X    VALUE 'N'.
             88 BYPASS-ACCUM                  VALUE 'Y'.
158916    05 WS-BYPASS-DED           PIC X    VALUE 'N'.
158916       88 BYPASS-DED                    VALUE 'Y'.
          05 WS-DATE-FOUND           PIC X    VALUE 'N'.
             88 DATE-FOUND                    VALUE 'Y'.
          05 WS-CHECK-NEXT-GP        PIC X    VALUE 'N'.
             88 CHECK-NEXT-GP                 VALUE 'Y'.
          05 WS-END-OF-NOTES         PIC X    VALUE 'N'.
             88 END-OF-NOTES                  VALUE 'Y'.
          05 WS-NOTE41-FOUND         PIC X    VALUE 'N'.
             88  NOTE41-FOUND                 VALUE 'Y'.
          05 WS-GNT41-LINE-VISITED   PIC X    VALUE 'N'.
             88  GNT41-LINE-VISITED           VALUE 'Y'.
          05 WS-GNT41-FIRST-ERROR    PIC X    VALUE 'N'.
             88  GNT41-FIRST-ERROR            VALUE 'Y'.
          05 WS-DED-IND              PIC X    VALUE 'N'.
             88  DED-IND                      VALUE 'Y'.
          05 WS-DED-ERR-REP-IND      PIC X    VALUE 'N'.
             88  DED-ERR-REP-IND              VALUE 'Y'.
          05 WS-COINS-IND            PIC X    VALUE 'N'.
             88  COINS-IND                    VALUE 'Y'.
          05 WS-COIN-ERR-REP-IND     PIC X    VALUE 'N'.
             88  COIN-ERR-REP-IND             VALUE 'Y'.
          05 WS-COPAY-IND            PIC X    VALUE 'N'.
             88  COPAY-IND                    VALUE 'Y'.
          05 WS-COPAY-ERR-REP-IND    PIC X    VALUE 'N'.
             88  COPAY-ERR-REP-IND            VALUE 'Y'.
          05 WS-BEN-CAL-FOUND        PIC X    VALUE 'N'.
             88  BEN-CAL-FOUND                VALUE 'Y'.
155725    05 WS-ALL-ZERO-RECEIVED    PIC X    VALUE 'N'.
155725       88  ALL-ZERO-RECEIVED            VALUE 'Y'.
155725    05 WS-ALL-ZERO             PIC X    VALUE 'N'.
155725       88  ALL-ZERO                     VALUE 'Y'.
155725    05 WS-ERROR-NEW-REP        PIC X    VALUE 'N'.
155725       88  ERROR-NEW-REP                VALUE 'Y'.
155725    05 WS-REVIEW-GNT41         PIC X    VALUE 'N'.
155725       88  REVIEW-GNT41                 VALUE 'Y'.
155725    05 WS-WRITE-ERROR-REC-NEW  PIC X    VALUE 'N'.
155725       88  WRITE-ERROR-REC-NEW          VALUE 'Y'.
194645    05 NEW-BYPASS-SWITCH       PIC X    VALUE 'N'.
194645       88  NEW-BYPASS                   VALUE 'Y'.
      *
       01  WS-END-SUB-PKG-SEG            PIC X     VALUE 'N'.
           88  END-SUB-PKG-SEG                     VALUE 'Y'.
       01  WS-SUB-PKG                    PIC X     VALUE 'N'.
           88  PKG-NOT-FOUND                       VALUE 'N'.
           88  PKG-FOUND                           VALUE 'Y'.
       01  VSSLGRU-END-OF-SEG-SW         PIC X(01) VALUE 'N'.
           88  VSSLGRU-END-OF-SEG                  VALUE 'Y'.
       01  VSSPMEM-END-OF-SEG-SW         PIC X     VALUE 'N'.
           88  VSSPMEM-END-OF-SEG                  VALUE 'Y'.
       01  VSSPMEM-SEARCH-IND            PIC X     VALUE 'N'.
           88  VSSPMEM-SEARCH-FOUND                VALUE 'Y'.
       01  VSSPDPN-END-OF-SEG-SW         PIC X     VALUE 'N'.
           88  VSSPDPN-END-OF-SEG                  VALUE 'Y'.
       01  VSSPDPN-SEARCH-IND            PIC X     VALUE 'N'.
           88  VSSPDPN-SEARCH-FOUND                VALUE 'Y'.
       01  VSSPCON-SEARCH-IND            PIC X     VALUE 'N'.
           88  VSSPCON-SEARCH-FOUND                VALUE 'Y'.
       01  VSSPCON-END-OF-SEG-SW         PIC X     VALUE 'N'.
           88  VSSPCON-END-OF-SEG                  VALUE 'Y'.
       01  WS-DOS-IN-RANGE               PIC X     VALUE 'N'.
           88  WS-DOS-IN-RANGE-FOUND               VALUE 'Y'.
      *
       01  WS-CHK-SUB-ID-LEN             PIC 9(4) COMP SYNC.
       01  WS-CHK-DIGIT                  PIC X(3).
       01  WS-CHK-SUB-ID-NO              PIC X(12).
       01  WS-IVF-ELIGIBLE-GROUPS        PIC X(09) VALUE SPACES.
           88 DIRECT-BRAND-GRP             VALUE '295918100'
                                                 '295918110'
                                                 '295918120'
                                                 '295918130'
                                                 '295918200'
                                                 '295918210'
                                                 '295918220'
                                                 '295918230'
                                                 '295918300'
                                                 '295918310'
                                                 '295918320'
                                                 '295918330'
                                                 '295918400'
                                                 '295918410'
                                                 '295918420'
                                                 '295918430'
                                                 '295918500'
                                                 '295918510'
                                                 '295918520'
                                                 '295918530'
                                                 '295918600'
                                                 '295918610'
                                                 '295918620'
                                                 '295918630'
                                                 '295918700'
                                                 '295918710'
                                                 '295918720'
                                                 '295918730'
                                                 '295918800'
                                                 '295918810'
                                                 '295918820'
                                                 '295918830'
                                                 '295918900'
                                                 '295918910'
                                                 '295918920'
                                                 '295918930'
                                                 '295918140'
                                                 '295918160'.
      *
           88 TOURO-COLLEGE                VALUE '295617 CE'
                                                 '295617 CS'
                                                 '295617 ES'
                                                 '295617 PS'
                                                 '295617 CP'
                                                 '295617 EP'
                                                 '295617 PP'.

           88 RICHLINE-GRP                 VALUE '720990DIA'
                                                 '720990EME'
                                                 '720990SPR'
                                                 '720990SPP'
                                                 '720990RUB'
                                                 '720990HSL'
                                                 '720990SLL'
                                                 '720990HSH'
                                                 '720990SLH'
RICHLN                                           '720990HSM'
RICHLN                                           '720990SLM'.
      *
      *ENCODE DECODE FUNCTIONS START
       01  WS-ED-STATUS             PICTURE 9(4) COMP.
       01  WS-ED-FUNC-INDEX         PICTURE 9(4) COMP VALUE 6.
       01  WS-ED-CODE2              PICTURE 9(4) COMP.
       01  S-GROUP-KEY.
           05 S-GRP-NO-ID           PICTURE X(6).
           05 S-SUBGR-NO            PICTURE X(3).
       01  S-DEST                   PICTURE X(6).
       01  S-ERROR-CODE             PICTURE X(2).
       01  S-ERROR-DESC             PICTURE X(64).
       01  WS-DEST-ID               PICTURE X(6) VALUE SPACES.
       01  CTEGPDST                 PICTURE X(8) VALUE 'CTEGPDST'.
       01  CTEGSDST                 PICTURE X(8) VALUE 'CTEGSDST'.
       01  CTECALLS                 PICTURE X(8) VALUE 'CTECALLS'.
       01  CTPCVFC1                 PICTURE X(8) VALUE 'CTPCVFC1'.
       01  CTPCVFC2                 PICTURE X(8) VALUE 'CTPCVFC2'.
       01  NTUPRDCD                 PICTURE X(8) VALUE 'NTUPRDCD'.
010915 01  TAG1                     PICTURE 9(04) COMP.
010915 01  TAG2                     PICTURE 9(06).
010915 01  RULE3-VFC                PICTURE X(06).
010915 01  RULE3-VFC-FINAL          PICTURE X(03).
010915 01  B5                       PIC 9(04) COMP SYNC VALUE 5.
010915 01  RET-STATUS               PIC 9(04) COMP.
010915 01  PROD-COMB-CD.
           05 PROD-COMB-CD-1-2      PIC X(02).
           05 PROD-COMB-CD-3-4      PIC X(02).
           05 PROD-COMB-CD-5-6      PIC X(02).
      *ENCODE DECODE FUNCTIONS END
       01  WS-ERROR-REPORT-AREA.
           05  WS-ERROR-HEADER-1.
               10  FILLER           PIC X VALUE SPACE.
               10  FILLER           PIC X(30) VALUE
                   'PGMNAME: S9CODSRV AND S9CODSHT'.
               10  FILLER           PIC X(05) VALUE SPACES.
               10  FILLER           PIC X(18) VALUE
                   'ERROR REPORT - ODS'.
               10  FILLER           PIC X(56) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE 'RUN DATE: '.
               10  WS-REPORT-DATE   PIC X(8).
      *
           05  WS-ERROR-BLANK-LINE  PIC X(132) VALUE SPACES.
      *
           05  WS-ERROR-HEADER-2.
               10  FILLER           PIC X(01) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE 'SUB-ID    '.
               10  FILLER           PIC X(03) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE 'DEP-NO    '.
               10  FILLER           PIC X(03) VALUE SPACES.
               10  FILLER           PIC X(05) VALUE 'GROUP'.
               10  FILLER           PIC X(08) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE 'FIRSTNAME '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE ' LASTNAME '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(08) VALUE 'SERV IND'.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(07) VALUE 'N/W IND'.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(08) VALUE '  DOS   '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE 'TOT CHARGES'.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE ' DED AMT   '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE ' COIN AMT  '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE 'INFERT AMT '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE ' LTM AMT   '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE ' COPAY     '.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(06) VALUE 'REASON'.
               10  FILLER           PIC X(84) VALUE SPACES.
      *        10  FILLER           PIC X(154) VALUE SPACES.
      *
           05  WS-ERROR-HEADER-3.
               10  FILLER           PIC X(01) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE '======'.
               10  FILLER           PIC X(03) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE '======'.
               10  FILLER           PIC X(03) VALUE SPACES.
               10  FILLER           PIC X(05) VALUE '====='.
               10  FILLER           PIC X(08) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE '=========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(10) VALUE '=========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(08) VALUE '========'.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(07) VALUE '======='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(08) VALUE '========'.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(11) VALUE '==========='.
               10  FILLER           PIC X(02) VALUE SPACES.
               10  FILLER           PIC X(06) VALUE '======'.
               10  FILLER           PIC X(79) VALUE SPACES.
      *        10  FILLER           PIC X(154) VALUE SPACES.
      *
155725     05  WS-ERROR-HEADER-4.
155725         10  FILLER           PIC X VALUE SPACE.
155725         10  FILLER           PIC X(30) VALUE
155725             'PGMNAME: S9CODSRV AND S9CODSHT'.
155725         10  FILLER           PIC X(05) VALUE SPACES.
155725         10  FILLER           PIC X(23) VALUE
155725             'ODS ZERO DOLLAR RECORDS'.
155725         10  FILLER           PIC X(51) VALUE SPACES.
155725         10  FILLER           PIC X(10) VALUE 'RUN DATE: '.
155725         10  WS-REPORT-DATE1  PIC X(8).
      *
           05  WS-ERROR-DETAIL-LINE.
               10  FILLER            PIC X(01) VALUE SPACES.                0113
               10  ERPT-ID            PIC X(10) VALUE SPACES.               0113
               10  FILLER            PIC X(03) VALUE SPACES.                0113
               10  ERPT-DEP           PIC X(03) VALUE SPACES.               0113
               10  FILLER            PIC X(10) VALUE SPACES.                0113
               10  ERPT-GROUP       PIC X(09) VALUE SPACES.                 0129
               10  FILLER            PIC X(04) VALUE SPACES.                0113
               10  ERPT-FNAME         PIC X(10) VALUE SPACES.               0115
               10  FILLER            PIC X(02) VALUE SPACES.                0113
               10  ERPT-LNAME         PIC X(10) VALUE SPACES.               0117
               10  FILLER            PIC X(02) VALUE SPACES.                0113
               10  ERPT-SERV-IND      PIC X(08) VALUE SPACES.               0123
               10  FILLER            PIC X(02) VALUE SPACES.                0113
               10  ERPT-NW-IND        PIC X(07) VALUE SPACES.               0123
               10  FILLER            PIC X(02) VALUE SPACES.                0113
               10  ERPT-DOS          PIC 9(08) VALUE ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
      *        10  ERPT-TOT-AMT      PIC ZZZZZZ9.99, BLANK WHEN ZEROES.     0127
               10  ERPT-TOT-AMT      PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.    0127
               10  FILLER            PIC X(03) VALUE SPACES.                0113
               10  ERPT-DED-AMT      PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
               10  ERPT-COIN-AMT     PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
               10  ERPT-INFERT-AMT   PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
               10  ERPT-LTM-AMT      PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
               10  ERPT-COPAY-AMT    PIC -ZZZZZZ9.99, BLANK WHEN ZEROES.
               10  FILLER            PIC X(02) VALUE SPACES.
               10  WS-ERR-MESSAGE     PIC X(50) VALUE SPACES.
               10  FILLER            PIC X(35) VALUE SPACES.                0128
      *        10  FILLER            PIC X(110) VALUE SPACES.               0128
      *
      * WORKING VARIBALE FOR THE ERROR FILES.  ( END  HERE)
      * IMS FUNCTIONS FOR THE MEMBERSHIP DATABASE ( START HERE)
       01  IMS-FUNCTION.
           05  GU                      PIC X(04) VALUE 'GU  '.
           05  GNP                     PIC X(04) VALUE 'GNP '.
           05  GN                      PIC X(04) VALUE 'GN  '.
      * IMS FUNCTIONS FOR THE MEMBERSHIP DATABASE ( END HERE)
      *
      ******************************************************************
      *  SSA'S
      ******************************************************************
      * SSA DECLARATIONS FOR THE MEMBERSHIP DATABASE ( START HERE)
      *
       01  SUB-ROOT-SSA.
           05  SUB-SSA-SEGNAME         PIC X(08) VALUE 'VSSPSUB '.
           05  SUB-SSA-CMD-CODES       PIC X(03) VALUE '*--'.
           05  SUB-SSA-L-PAREN         PIC X(01) VALUE '('.
           05  SUB-SSA-FIELD-NAME      PIC X(08) VALUE 'SUBRIDD1'.
           05  SUB-SSA-OPER            PIC X(02) VALUE ' ='.
           05  SUB-SSA-SUBR-NO         PIC X(12).
           05  SUB-SSA-MDF             PIC X(03).
           05  SUB-SSA-R-PAREN         PIC X(01) VALUE ')'.
           SKIP1
       01  SUB-ROOT-SEG.
           COPY VSSPSUBC.
           COPY WISSSUB.
      *
       01  SUB-UNQ-MEMB-SSA.
           05  SMB-SSA-SEGNAME         PIC X(08) VALUE 'VSSPMEM '.
           05  SMB-SSA-CMD-CODES       PIC X(01) VALUE ' '.
           SKIP1
       01  SUB-MEMB-SEG.
           COPY VSSPMEMC.
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
               10  VSGPNTE-PAGE1         PIC  9(02) COMP-3 VALUE 41.
               10  VSGPNTE-NOTE1         PIC  9(03) COMP-3 VALUE 0.
           05  VSGPNTE-BOOLEAN-AND       PIC  X(01) VALUE '*'.
           05  VSGPNTE-SEG2              PIC  X(08) VALUE 'GPDFNTEI'.
           05  VSGPNTE-OPER2             PIC  X(02) VALUE '<='.
           05  VSGPNTE-KEY2.
               10  VSGPNTE-PAGE2         PIC  9(02) COMP-3 VALUE 41.
               10  VSGPNTE-NOTE2         PIC  9(03) COMP-3 VALUE 999.
           05  VSGPNTE-CLOSE             PIC  X(01) VALUE ')'.
      *                                                                 01410000
       01  VSGPGRU-SSA-QUAL.
           04  FILLER                    PIC X(8)    VALUE 'VSGPGRU '.
           04  FILLER                    PIC X       VALUE '('.
           04  FILLER                    PIC X(8)    VALUE 'GRUPKEYI'.
           04  VSGPGRU-OPR               PIC X(2)    VALUE '= '.
           04  VSGPGRU-KEY               PIC X(9).
           04  FILLER                    PIC X       VALUE ')'.
      *
010915 01  VSGPPAC-SSA-QUAL.
           04  FILLER                    PIC X(8)    VALUE 'VSGPPAC '.
           04  FILLER                    PIC X       VALUE '('.
           04  FILLER                    PIC X(8)    VALUE 'NULLPKGN'.
           04  VSGPPAC-OPR               PIC X(2)    VALUE '= '.
           04  VSGPPAC-KEY               PIC 9(3)    COMP-3.
           04  FILLER                    PIC X       VALUE ')'.
      *
333803 01  VSGPPAC-SSA-UNQUAL.
333803     04  FILLER                    PIC X(8)    VALUE 'VSGPPAC '.
333803     04  FILLER                    PIC X       VALUE ' '.
      *
440881 01  SSA-VSGPGRU-QUAL.
440881     05  SGN-VSGPGRU                  PIC X(08) VALUE 'VSSLGRU '.
440881     05  CCS-VSGPGRU                  PIC X(03) VALUE '*--'.
440881     05  BQ-VSGPGRU                   PIC X(01) VALUE '('.
440881     05  FLN-VSGPGRU                  PIC X(08) VALUE 'GRUPKEYI'.
440881     05  RO-VSGPGRU                   PIC X(02) VALUE '= '.
440881     05  VALUE-VSGPGRU                PIC X(09).
440881     05  EQ-VSGPGRU                   PIC X(01) VALUE ')'.
      *
       01  SUB-GRPPTR-SSA.
           05  SUB-GPTR-UNQ-SEGNAME    PIC X(8) VALUE 'VSSLGRU '.
           05  SUB-GPTR-UNQ-L-PAREN    PIC X(1) VALUE ' '.
           SKIP1
       01  SUB-GROUP-PTR-SEG.
           COPY VSSVGRPC.
      *
       01  SUB-PKG-SSA.
           05  SUB-PKG-UNQ-SEGNAME     PIC X(8) VALUE 'VSGPPKG '.
           05  SUB-PKG-UNQ-L-PAREN     PIC X(1) VALUE ' '.
           SKIP1

       01  SUB-PKG-SEG.
           COPY VSGPPKGC.
      *
       01  VSGPGRU-SEGMENT.
           COPY VSGPGRUC.
      *
       01  GRP-PKG-SEGMENT.
           COPY VSGPPACC.
      *
       01  SUB-QAL-MEMB-SSA.
          05  SSA-VSSPMEM-SEG-NAME       PIC X(008)  VALUE 'VSSPMEM '.
          05  SSA-VSSPMEM-AST            PIC X(001)  VALUE '*'.
          05  SSA-VSSPMEM-COM-CODES      PIC X(002)  VALUE '--'.
          05  SSA-VSSPMEM-LEFT-PAREN     PIC X(001)  VALUE '('.
          05  SSA-VSSPMEM-KEY-NAME       PIC X(008)  VALUE 'NULLDEPN'.
          05  SSA-VSSPMEM-REL-OP         PIC X(002)  VALUE ' ='.
          05  SSA-VSSPMEM-KEY-VALUE.
              10  SSA-VSCPMEM-MEM-KEY    PIC 9(003)  COMP-3 VALUE 0.
          05  SSA-VSSPMEM-RIGHT-PAREN    PIC X(001)  VALUE ')'.
      *
        01  SUB-UNQ-DPN-SSA.
            05  SMB-SSA-SEGNAME         PIC X(08) VALUE 'VSSPDPN '.
            05  SMB-SSA-CMD-CODES       PIC X(01) VALUE ' '.
            SKIP1
        01  SUB-DPN-SEG.
            COPY VSSPDPNC.
            EJECT
        01  SUB-UNQ-CON-SSA.
            05  SMB-SSA-SEGNAME         PIC X(08) VALUE 'VSSPCON '.
            05  SMB-SSA-CMD-CODES       PIC X(01) VALUE ' '.
            SKIP1
        01  SUB-CON-SEG.
            COPY VSSPCONC.
      *
      *
      * SSA DECLARATIONS FOR THE MEMBERSHIP DATABASE ( END  HERE)
      ****************************************************************
      *          COPY BOOK                                           *
      ****************************************************************
       COPY S9CODSIO.
       COPY S9CODSC1.
      ******************************************************************
      *                   L I N K A G E  S E C T I O N                 *
      ******************************************************************
      *
       LINKAGE SECTION.
      *
       01  IO-PCB.
           05  PCB-DBD-NAME            PIC X(08).
           05  PCB-SEGMENT-LEVEL       PIC X(02).
           05  PCB-RETURN-CODE         PIC X(02).
           05  PCB-PROC-OPTION         PIC X(04).
           05  PCB-RESERVE-DLI         PIC X(04).
           05  PCB-SEGMENT-NAME        PIC X(08).
           05  PCB-KEY-FEEDBACK-LENGTH PIC S9(8)  COMP.
           05  PCB-NUM-SENS-SEGMENTS   PIC S9(8)  COMP.
           05  PCB-KEY-FEEDBACK-AREA   PIC X(100).
      *
      ******************************************************************
      * GROUP DATABASE PCB'S
      ******************************************************************
      *
       01  VFGGRP10-PCB.
           05  PCB-DBD-NAME            PIC X(08).
           05  PCB-SEGMENT-LEVEL       PIC X(02).
           05  GRP-STATUS-CODE         PIC X(02).
               88  GRP-DB-SEG-FOUND                 VALUE '  '.
               88  GRP-DB-SEG-NOT-FOUND             VALUE 'GE'.
               88  GRP-END-OF-DB                    VALUE 'GB'.
               88  GRP-NEW-SEG-SAME-LEVEL           VALUE 'GK'.
           05  PCB-PROC-OPTION         PIC X(04).
           05  PCB-RESERVE-DLI         PIC X(04).
           05  PCB-SEGMENT-NAME        PIC X(08).
           05  PCB-KEY-FEEDBACK-LENGTH PIC S9(8)  COMP.
           05  PCB-NUM-SENS-SEGMENTS   PIC S9(8)  COMP.
           05  PCB-KEY-FEEDBACK-AREA   PIC X(100).
      *
      ******************************************************************
      * DUMMY SUBSCRIBER DATABASE PCB'S
      ******************************************************************
      *
       01  DUMMY-PCB.
           05  DMY-DBD-NAME            PIC X(08).
           05  DMY-SEGMENT-LEVEL       PIC X(02).
           05  DMY-STATUS-CODE         PIC X(02).
               88  DMY-DB-SEG-FOUND                 VALUE '  '.
               88  DMY-DB-SEG-NOT-FOUND             VALUE 'GE'.
               88  DMY-END-OF-DB                    VALUE 'GB'.
               88  DMY-NEW-SEG-SAME-LEVEL           VALUE 'GK'.
           05  DMY-PROC-OPTION         PIC X(04).
           05  DMY-RESERVE-DLI         PIC X(04).
           05  DMY-SEGMENT-NAME        PIC X(08).
           05  DMY-KEY-FEEDBACK-LENGTH PIC S9(8)  COMP.
           05  DMY-NUM-SENS-SEGMENTS   PIC S9(8)  COMP.
           05  DMY-KEY-FEEDBACK-AREA   PIC X(100).
      *
      ******************************************************************
      * SUBSCRIBER DATABASE PCB'S
      ******************************************************************
      *
       01  VDSSUB10-PCB.
           05  PCB-DBD-NAME            PIC X(08).
           05  PCB-SEGMENT-LEVEL       PIC X(02).
           05  SUB-STATUS-CODE         PIC X(02).
               88  SUB-DB-SEG-FOUND                 VALUE '  '.
               88  SUB-DB-SEG-NOT-FOUND             VALUE 'GE'.
               88  SUB-END-OF-DB                    VALUE 'GB'.
               88  SUB-NEW-SEG-SAME-LEVEL           VALUE 'GK'.
           05  PCB-PROC-OPTION         PIC X(04).
           05  PCB-RESERVE-DLI         PIC X(04).
           05  PCB-SEGMENT-NAME        PIC X(08).
           05  PCB-KEY-FEEDBACK-LENGTH PIC S9(8)  COMP.
           05  PCB-NUM-SENS-SEGMENTS   PIC S9(8)  COMP.
           05  PCB-KEY-FEEDBACK-AREA   PIC X(100).
      *
      ******************************************************************
      *           P R O C E D U R E   D I V I S I O N                  *
      ******************************************************************
      *
       PROCEDURE DIVISION  USING IO-PCB
                                 VFGGRP10-PCB
                                 DUMMY-PCB
                                 VDSSUB10-PCB.

      *
       1000-MAIN-PARA.
           OPEN INPUT  ODS-INFILE.
           OPEN I-O    ODS-DATFILE.

           OPEN OUTPUT ODS-POSFILE
                       ODS-NEGFILE
                       ODS-ERRFILE
155725                 ODS-ZEROERR
                       ODS-RESPONSE
                       ODS-RESPONS1.

           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-RUN-TIME FROM TIME.
           MOVE WS-RUN-DATE TO WS-RUN-DATE-T.
           MOVE WS-RUN-TIME TO WS-RUN-TIME-T.
1073       MOVE WS-RUN-DATE-T   TO S9-FILE-DT.
1073       MOVE WS-RUN-TIME-T   TO S9-FILE-TME.

           MOVE WS-RUN-DATE(5:2) TO WS-FORMAT-MM.
           MOVE WS-RUN-DATE(7:2) TO WS-FORMAT-DD.
           MOVE WS-RUN-DATE(3:2) TO WS-FORMAT-YY.
      *
           MOVE WS-RUN-DATE      TO WS-REPORT-DATE
155725     MOVE WS-RUN-DATE      TO WS-REPORT-DATE1
      *
           DISPLAY 'START PROGRAM:'  WS-PROGRAM-ID
                   ' COMPILED ON:'  WS-COMP-DATE ' AT ' WS-COMP-TIME
                   ', RUNNING ON:'   WS-FORMAT-DATE.
      *
           SET FIRST-TIME TO TRUE
           INITIALIZE WS-DATE-VARIABLES
                      WS-RECORD-COUNTS
                      S9-HEADER
                      S9-PARMLIST
                      S9-TRAILER.

            READ ODS-DATFILE
             AT END SET END-OF-DATE-FILE TO TRUE.

            READ ODS-INFILE INTO S9-HEADER
             AT END SET END-OF-INFILE TO TRUE.

           IF S9-H-RECORD-ID = 'HDR'
1073        MOVE 'RESPONSE' TO S9-H-TRANSACTION-ID
1073        WRITE RESPONSE-REC FROM S9-HEADER
              PERFORM 1000-VALIDATE-PARA THRU 1000-EXIT
           ELSE
              DISPLAY 'HEADER RECORD IS MISSING IN THE ODS'
              DISPLAY 'FILE RECEIVED'
                 PERFORM  5020-ABEND THRU 5020-EXIT
           END-IF.

              DISPLAY 'FINAL REPORT OF THE PROGRAM S9CODSRV'.
              DISPLAY 'TOTAL NO OF DETAIL RECORDS READ:', WS-REC-COUNT.
              DISPLAY 'TOTAL POSITIVE RECORDS     :', WS-POS-REC.
              DISPLAY 'TOTAL NEGATIVE RECORDS     :', WS-NEG-REC.

              CLOSE ODS-INFILE
                    ODS-DATFILE
                    ODS-POSFILE
                    ODS-NEGFILE
                    ODS-ERRFILE
155725              ODS-ZEROERR
                    ODS-RESPONSE
                    ODS-RESPONS1.
              GOBACK.
      *
       1000-VALIDATE-PARA.
      *
            IF (S9-TRANSACTION-DT = TODAY-DATE AND
                S9-TRANSACTION-TME = TIMESTMP )
                DISPLAY 'DUPLICATE FILE RECEIVED'
                DISPLAY 'SAME HEADER DATE AND TIMESTAMP AS'
                DISPLAY 'PREVIOUS FILE'
                DISPLAY 'PLEASE CONTACT THE ODS VENDOR'
                DISPLAY 'DATE RECEIVED ON THE ODS FILE:',
                                           S9-TRANSACTION-DT
                DISPLAY 'TIMESTAMP RECEIVED ON THE ODS FILE:',
                                           S9-TRANSACTION-TME
                PERFORM  5020-ABEND THRU 5020-EXIT
            ELSE
                MOVE S9-TRANSACTION-DT  TO TODAY-DATE
                MOVE S9-TRANSACTION-TME TO TIMESTMP
                REWRITE DATE-REC
                PERFORM 1010-READ-INPUT-FILE THRU 1010-EXIT
                        UNTIL END-OF-FILE
            END-IF.
      *
       1000-EXIT.
           EXIT.
       1010-READ-INPUT-FILE.
      *
           READ ODS-INFILE
           AT END SET END-OF-FILE TO TRUE
           NOT AT END
            IF INPUT-REC(1:3) EQUAL TO 'TRL'
1073              MOVE INPUT-REC TO S9-TRAILER
1073              MOVE 'RESPONSE'      TO S9-T-TRANSACTION-ID
1073              MOVE WS-REC-COUNT    TO S9-OUT-REC-CNT
1073              MOVE WS-REC-COUNT    TO S9-OUT-REC-REQ-CNT
1073              MOVE WS-RESPONSE-SUS TO S9-OUT-REC-SUCCESS-CNT
1073              MOVE WS-RESPONSE-ERR TO S9-OUT-REC-FAIL-CNT
1073              MOVE 'Y'             TO S9-FILE-AUDIT-STS
1073              WRITE RESPONSE-REC FROM S9-TRAILER
                  CONTINUE
            ELSE
              MOVE INPUT-REC TO S9-PARMLIST
              MOVE SPACES TO  WS-IVF-ELIGIBLE-GROUPS
                              WK-SEX-CODE
                              WS-SEX-CODE
1073            MOVE '00' TO S9-CALL-STATUS
              INSPECT S9-IN-CVRG-PLAN-ID(1:9) REPLACING ALL '#' BY ' '
               MOVE S9-IN-CVRG-PLAN-ID(1:9)
                                  TO WS-IVF-ELIGIBLE-GROUPS(1:9)
                                     S9-INPUT-D-GRP-ID(1:9)
               MOVE S9-IN-SRVC-STRT-DT TO S9-INPUT-D-DTE-CLM-SSDT
               MOVE S9-IN-SRVC-END-DT TO S9-INPUT-D-DTE-CLM-ESDT
041715         MOVE S9-IN-CLM-NBR     TO S9-INPUT-D-IN-CLM-NBR
               ADD 1 TO WS-REC-COUNT
               MOVE 'N' TO WS-ERROR-SW
PROD           MOVE 'N' TO WS-ZERO-ERROR-SW
194645         MOVE 'N'        TO NEW-BYPASS-SWITCH
194645         MOVE 'N'        TO WS-CHECK-NEXT-GP
               IF FIRST-TIME
                  WRITE ODS-ERR-REC   FROM WS-ERROR-HEADER-1
                  WRITE ODS-ERR-REC   FROM WS-ERROR-BLANK-LINE
                  WRITE ODS-ERR-REC   FROM WS-ERROR-HEADER-2
                  WRITE ODS-ERR-REC   FROM WS-ERROR-HEADER-3
155725            WRITE ODS-ZERO-ERR  FROM WS-ERROR-HEADER-4
155725            WRITE ODS-ZERO-ERR  FROM WS-ERROR-BLANK-LINE
155725            WRITE ODS-ZERO-ERR  FROM WS-ERROR-HEADER-2
155725            WRITE ODS-ZERO-ERR  FROM WS-ERROR-HEADER-3
                  MOVE  'N' TO   WS-FIRST-TIME
               END-IF
155003         IF S9-IN-ACM-RSN-CD = ('RV' OR 'SA')
145266                               AND (NOT S9-IN-PROD-TYPE-RX-CO)
155725            MOVE 'N' TO WS-WRITE-ERROR-REC-NEW
158916            MOVE 'N' TO WS-BYPASS-DED
                  PERFORM 1020-VALIDATE-INPUT-FILE THRU 1020-EXIT
136387            PERFORM 4020-ACM-DETAIL-REC THRU 4020-EXIT
                  IF ERROR-RPT-TRUE
                     IF NOT END-OF-FILE
                        PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
                     END-IF
                  ELSE
158916               IF BYPASS-DED
158916                  MOVE  'DED ACCUM RECEIVED' TO
158916                         WS-ERR-MESSAGE
158916                  MOVE '94' TO S9-CALL-STATUS
158916                  PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
158916               ELSE
                        PERFORM 3010-CHECK-AMOUNTS THRU 3010-EXIT
158916               END-IF
                  END-IF
               ELSE
155003           MOVE '00' TO S9-CALL-STATUS
155003           ADD 1     TO  WS-RESPONSE-SUS
155003           PERFORM 6010-ERROR-DES THRU 6010-EXIT
155003           WRITE RESPONSE-REC FROM S9-PARMLIST
               END-IF
            END-IF.

       1010-EXIT.
           EXIT.
      *
       1020-VALIDATE-INPUT-FILE.
      *
           IF S9-D-RECORD-ID = 'DTL'
                    PERFORM 2000-READ-SUBSCRIBER THRU 2000-EXIT
           ELSE
              DISPLAY 'INVALID DETAIL RECORD TYPE FROM ODS'
              DISPLAY 'RECORD TYPE RECEIVED FROM ODS IS:',
                                                S9-D-RECORD-ID
              PERFORM  5020-ABEND THRU 5020-EXIT
           END-IF.
      *
       1020-EXIT.
            EXIT.
      *
       2000-READ-SUBSCRIBER.
      *
      ***************************************************************
      *  CALL SUBSCRIBER DATABASE                                   *
      *  READS THE SUBSCRIBER ROOT SEGMENT WITH SUBID KEY           *
      ***************************************************************
      *
           INITIALIZE WS-SUBSCRIBER-ID
136387                WS-SUBID
136387                WS-SPACE-COUNT
136387                WS-SI-SUBID
136387     MOVE S9-IN-MBR-HCID           TO WS-SUBID
136387     INSPECT WS-SUBID TALLYING  WS-SPACE-COUNT FOR ALL ' '
136387     IF WS-SPACE-COUNT = 0
136387        MOVE WS-SUBID TO WS-SI-SUBID
136387     ELSE
136387        IF WS-SPACE-COUNT = 1
136387           MOVE SPACE          TO WS-SI-SUBID (1:1)
136387           MOVE WS-SUBID (1:8) TO WS-SI-SUBID (2:8)
136387        ELSE
136387           IF WS-SPACE-COUNT = 2
136387              MOVE SPACE          TO WS-SI-SUBID (1:2)
136387              MOVE WS-SUBID (1:7) TO WS-SI-SUBID (3:7)
136387           ELSE
136387              IF WS-SPACE-COUNT = 3
136387                 MOVE SPACE          TO WS-SI-SUBID (1:3)
136387                 MOVE WS-SUBID (1:6) TO WS-SI-SUBID (4:6)
136387              END-IF
136387           END-IF
136387        END-IF
136387     END-IF
           MOVE SPACES             TO  WS-SPACES
           MOVE IP-CS90-SUB-ID     TO  SUB-SSA-SUBR-NO
           MOVE 12                 TO  WS-CHK-SUB-ID-LEN
           MOVE IP-CS90-SUB-ID     TO  WS-CHK-SUB-ID-NO
           MOVE SPACES TO WS-CHK-DIGIT
           CALL 'LNCKDIG' USING WS-CHK-SUB-ID-LEN
                                WS-CHK-SUB-ID-NO
                                WS-CHK-DIGIT
           MOVE WS-CHK-DIGIT TO SUB-SSA-MDF

           MOVE SPACES             TO  SUB-STATUS-CODE
           MOVE 'GU  '             TO  IMS-FUNCTION
      *
           CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                   VDSSUB10-PCB
                                   SUB-ROOT-SEG
                                   SUB-ROOT-SSA.

           IF SUB-DB-SEG-FOUND
                 MOVE 'N'         TO VSSPMEM-SEARCH-IND
                                     VSSPMEM-END-OF-SEG-SW
                 PERFORM 2010-READ-MEMBER THRU 2010-EXIT
                   UNTIL VSSPMEM-END-OF-SEG OR VSSPMEM-SEARCH-FOUND
           ELSE
           IF SUB-DB-SEG-NOT-FOUND
              MOVE  'SUB ID NOT FOUND' TO
                    WS-ERR-MESSAGE
              MOVE 'Y' TO WS-ERROR-SW
1073          MOVE '98' TO S9-CALL-STATUS
           ELSE
              DISPLAY '********************************************'
              DISPLAY 'INVALID CALL                                '
              DISPLAY 'ACCESSING SUBSCRIBER DB - SUB ROOT SEGMENT  '
              DISPLAY 'SEGMENT NAME - VSSPSUB                      '
              DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
              DISPLAY '********************************************'
              CALL 'ZNCANCEL'
           END-IF.
      *
       2000-EXIT.
           EXIT.
      *
       2010-READ-MEMBER.
      *
      ***************************************************************
      *  CALL MEMBER SEGMENT IN SUBSCRIBER DATABASE                 *
      *  READS THE VSSPMEM SEGMENT AND VALIDATE THE MEMBER          *
      ***************************************************************
      *
      *     DISPLAY '2010-READ-MEMBER'
            MOVE SPACES             TO  SUB-STATUS-CODE
                                        WS-LAST-NAME
            MOVE S9-IN-MBR-SEQ-NBR(1:2)  TO  SSA-VSCPMEM-MEM-KEY
            MOVE 'GU  '                  TO  IMS-FUNCTION

            CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                    VDSSUB10-PCB
                                    SUB-MEMB-SEG
                                    SUB-ROOT-SSA
                                    SUB-QAL-MEMB-SSA.

             IF SUB-DB-SEG-FOUND
               MOVE S9-IN-MBR-SEQ-NBR (1:2) TO S9-INPUT-D-MEM-DEP
               INITIALIZE WS-INPUT-REL
                          WS-RELATION
                          WS-GENDER
               PERFORM 2011-POPULATE-SEXREL THRU 2011-EXIT
               MOVE 'N'                TO VSSPDPN-END-OF-SEG-SW
                                          VSSPDPN-SEARCH-IND
               PERFORM 2015-READ-DEPENDENT THRU 2015-EXIT
                 UNTIL VSSPDPN-END-OF-SEG  OR VSSPDPN-SEARCH-FOUND
               IF WS-LAST-NAME = LOW-VALUES OR SPACES
                  MOVE SA-SUBR-LST-NM        TO WS-LAST-NAME
               END-IF
                     MOVE 'Y'       TO VSSPMEM-SEARCH-IND
                     MOVE SB-1ST-NM    TO S9-INPUT-D-MEM-FNAME
                     MOVE SB-BRTH-DT   TO S9-INPUT-D-MEM-DOB
                     MOVE WS-LAST-NAME TO S9-INPUT-D-MEM-LNAME
                        PERFORM 2020-SUB-DOS-CHECK
      *
             ELSE
             IF SUB-DB-SEG-NOT-FOUND
                   MOVE  'MISMATCH WITH MEMBERSHIP' TO
                                WS-ERR-MESSAGE
                   MOVE 'Y' TO WS-ERROR-SW
                   MOVE 'Y' TO VSSPMEM-END-OF-SEG-SW
1073               MOVE '98' TO  S9-CALL-STATUS
             ELSE
                MOVE 'Y' TO VSSPMEM-END-OF-SEG-SW
                DISPLAY '********************************************'
                DISPLAY 'INVALID CALL '
                DISPLAY 'ACCESSING SUBSCRIBER DB - SUB MEMBER SEGMENT'
                DISPLAY 'SEGMENT NAME - VSSPMEM '
                DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
                DISPLAY '********************************************'
                CALL 'ZNCANCEL'
             END-IF
             END-IF.
       2010-EXIT.
            EXIT.
      *
       2011-POPULATE-SEXREL.
      *
            IF SB-MBR-REL-CD = 01 OR 03 OR 05 OR 07 OR 09 OR
                               11 OR 13 OR 15 OR 17
               MOVE 'M'             TO WS-GENDER
            ELSE
               IF SB-MBR-REL-CD = 02 OR 04 OR 06 OR 08 OR 10 OR
                                  12 OR 14 OR 16 OR 18
                  MOVE 'F'          TO WS-GENDER
               END-IF
            END-IF
VIK           MOVE WS-GENDER             TO S9-INPUT-D-MEM-GENDER

            IF SB-SB-MALE-SUB           OR
               SB-SB-FEMALE-SUB         OR
               SB-SB-MALE-SPNSRD-SUB    OR
               SB-SB-FEMALE-SPNSRD-SUB  OR
               SB-SB-MALE-RESIDUAL-SUB  OR
               SB-SB-FEMALE-RESIDUAL-SUB
               MOVE  1              TO WS-RELATION
            ELSE
            IF SB-SB-MALE-SPOUSE   OR
               SB-SB-FEMALE-SPOUSE
               MOVE  2              TO WS-RELATION
            ELSE
            IF SB-SB-MALE-CHILD    OR
               SB-SB-FEMALE-CHILD
               MOVE  3              TO WS-RELATION
            ELSE
            IF SB-SB-MALE-STUDENT  OR
               SB-SB-FEMALE-STUDENT
               MOVE  4              TO WS-RELATION
            ELSE
            IF SB-SB-MALE-HNDCPD-DEP OR
               SB-SB-FEMALE-HNDCPD-DEP
               MOVE  5              TO WS-RELATION
            ELSE
            IF SB-SB-MALE-SPNSRD-DEP OR
               SB-SB-FEMALE-SPNSRD-DEP
               MOVE  7              TO WS-RELATION
            ELSE
               MOVE  0              TO WS-RELATION
            .
            MOVE WS-RELATION TO S9-INPUT-D-MEM-REL-CDE.
      *
       2011-EXIT.
            EXIT.
      *
      *
       2015-READ-DEPENDENT.
      *
      **************************************************************
      *  CALL DEPENDENT LAST NAME SEGMENT IN SUBSRCIBER DATABASE
      *  READS THE VSSPDPN SEGMENT AND VALIDATE THE LAST NAME
      **************************************************************
      *
      *     DISPLAY '2015-READ-DEPENDENT'
            MOVE SPACES             TO  SUB-STATUS-CODE
            MOVE 'GNP '             TO  IMS-FUNCTION

            CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                    VDSSUB10-PCB
                                    SUB-DPN-SEG
                                    SUB-UNQ-DPN-SSA.

            IF SUB-DB-SEG-FOUND
               IF SG-DEP-LST-NM NOT = LOW-VALUES AND SPACES
                  MOVE SG-DEP-LST-NM  TO WS-LAST-NAME
                  MOVE 'Y'            TO VSSPDPN-SEARCH-IND
               END-IF
            ELSE
            IF SUB-DB-SEG-NOT-FOUND
               MOVE 'Y' TO VSSPDPN-END-OF-SEG-SW
            ELSE
               MOVE 'Y' TO VSSPDPN-END-OF-SEG-SW
               DISPLAY '********************************************'
               DISPLAY 'INVALID CALL'
               DISPLAY 'ACCESSING SUBSCRIBER DB - SUB MEMBER SEGMENT'
               DISPLAY 'SEGMENT NAME  - VSSPDPN'
               DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
               DISPLAY '********************************************'
               CALL 'ZNCANCEL'
            END-IF.
      *
       2015-EXIT.
            EXIT.
      *
       2020-SUB-DOS-CHECK.
      *
                 INITIALIZE WS-EFF-DATE
                            WS-TERM-DATE
                            WS-MBR-EFF-DT
                            WS-MBR-TERM-DT
                 MOVE SB-MBR-EFF-DT  TO WS-MBR-EFF-DT
                 MOVE SB-MBR-TERM-DT TO WS-MBR-TERM-DT
                 MOVE 'N'            TO WS-DOS-IN-RANGE
                 MOVE 'N'            TO VSSPCON-END-OF-SEG-SW
                 IF WS-MBR-TERM-DT = SPACES
                   MOVE '2991231'    TO WS-MBR-TERM-DT
                 END-IF
                 IF WS-MBR-EFF-DT (1:1) = 1
                    MOVE 19                  TO  WS-EFF-DATE(1:2)
                    MOVE WS-MBR-EFF-DT (2:6) TO  WS-EFF-DATE(3:6)
                 ELSE
                  IF WS-MBR-EFF-DT (1:1) = 2
                     MOVE 20                  TO  WS-EFF-DATE(1:2)
                     MOVE WS-MBR-EFF-DT (2:6) TO  WS-EFF-DATE(3:6)
                  END-IF
                 END-IF
                 IF  WS-MBR-TERM-DT (1:1) = 1
                     MOVE 19                   TO  WS-TERM-DATE(1:2)
                     MOVE WS-MBR-TERM-DT (2:6) TO  WS-TERM-DATE(3:6)
                 ELSE
                  IF WS-MBR-TERM-DT (1:1) = 2
                     MOVE 20                   TO  WS-TERM-DATE(1:2)
                     MOVE WS-MBR-TERM-DT (2:6) TO  WS-TERM-DATE(3:6)
                  END-IF
                 END-IF
                IF S9-IN-SRVC-STRT-DT >= WS-EFF-DATE   AND
                      S9-IN-SRVC-END-DT <= WS-TERM-DATE
                   CONTINUE
                ELSE
                   MOVE SPACES     TO  SUB-STATUS-CODE
                   MOVE SB-DEP-NO  TO  SSA-VSCPMEM-MEM-KEY
                   MOVE 'GU'       TO  IMS-FUNCTION
                   CALL 'CBLTDLI'  USING IMS-FUNCTION
                                         VDSSUB10-PCB
                                         SUB-MEMB-SEG
                                         SUB-ROOT-SSA
                                         SUB-QAL-MEMB-SSA
                 IF SUB-DB-SEG-FOUND
                  PERFORM 2018-MEMB-CONTIN-SEGMENT THRU 2018-EXIT
                   UNTIL VSSPCON-END-OF-SEG OR WS-DOS-IN-RANGE-FOUND
                 END-IF
                 IF  NOT WS-DOS-IN-RANGE-FOUND
                    MOVE 'NO ACTIVE MEMBERSHIP'   TO
                         WS-ERR-MESSAGE
                    MOVE 'Y' TO WS-ERROR-SW
1073                MOVE '92' TO S9-CALL-STATUS
                 END-IF
                END-IF.
      *
       2018-MEMB-CONTIN-SEGMENT.
      *
      ****************************************************************
      * READ THE MEMBER CONTINUITY SEGMENT WILL CONTAIN ANY
      * PREVIOUS HISTORY.
      * **************************************************************
      *
                 MOVE SPACES             TO  SUB-STATUS-CODE
                 MOVE 'GNP '             TO  IMS-FUNCTION
                 CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                         VDSSUB10-PCB
                                         SUB-CON-SEG
                                         SUB-UNQ-CON-SSA.
                 IF SUB-DB-SEG-FOUND
                    PERFORM 2025-SUB-DOS-CHECK
                 ELSE
                 IF SUB-DB-SEG-NOT-FOUND
                    MOVE 'Y' TO VSSPCON-END-OF-SEG-SW
                 ELSE
                    MOVE 'Y' TO VSSPCON-END-OF-SEG-SW
                    DISPLAY '*************************************'
                    DISPLAY 'INVALID CALL'
                    DISPLAY 'ACCESSING SUBSCRIBER DB- SUB MEMBER SEG'
                    DISPLAY 'SEGMENT NAME  - VSSPCON'
                    DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
                    DISPLAY '*************************************'
                    CALL 'ZNCANCEL'
                 END-IF
                 END-IF.
       2018-EXIT.
            EXIT.
      *
      ***************************************************************
       2025-SUB-DOS-CHECK.
      ***************************************************************
                INITIALIZE WS-EFF-DATE
                           WS-TERM-DATE
                           WS-MBR-EFF-DT
                           WS-MBR-TERM-DT
                MOVE SC-MBR-EFF-DT  TO WS-MBR-EFF-DT
                MOVE SC-MBR-TERM-DT TO WS-MBR-TERM-DT
                IF WS-MBR-TERM-DT = SPACES
                   MOVE '2991231'    TO WS-MBR-TERM-DT
                END-IF
                IF WS-MBR-EFF-DT (1:1) = 1
                   MOVE 19                  TO  WS-EFF-DATE(1:2)
                   MOVE WS-MBR-EFF-DT (2:6) TO  WS-EFF-DATE(3:6)
                ELSE
                 IF WS-MBR-EFF-DT (1:1) = 2
                   MOVE 20                  TO  WS-EFF-DATE(1:2)
                   MOVE WS-MBR-EFF-DT (2:6) TO  WS-EFF-DATE(3:6)
                 END-IF
                END-IF
                IF WS-MBR-TERM-DT (1:1) = 1
                   MOVE 19                   TO  WS-TERM-DATE(1:2)
                   MOVE WS-MBR-TERM-DT (2:6) TO  WS-TERM-DATE(3:6)
                ELSE
                  IF WS-MBR-TERM-DT (1:1) = 2
                     MOVE 20                   TO  WS-TERM-DATE(1:2)
                     MOVE WS-MBR-TERM-DT (2:6) TO  WS-TERM-DATE(3:6)
                  END-IF
                END-IF
                IF S9-IN-SRVC-STRT-DT >= WS-EFF-DATE   AND
                        S9-IN-SRVC-END-DT <= WS-TERM-DATE
                   SET WS-DOS-IN-RANGE-FOUND TO TRUE
                END-IF.
      *
      ***************************************************************
       3010-CHECK-AMOUNTS.
      ***************************************************************
      *
           PERFORM 4010-DEST-ID THRU 4010-EXIT
           MOVE WS-DEST-ID TO S9-INPUT-D-DEST-ID
      *
136387*    PERFORM 4020-ACM-DETAIL-REC THRU 4020-EXIT.
      *
           PERFORM 2030-READ-SUBSCRIBER-GROUP THRU 2030-EXIT
      *
           IF CHECK-NEXT-GP
              PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
           END-IF
333803*    PERFORM 1110-GET-GROUP-PKG-SEG THRU 1110-EXIT
      *
194645     IF ( NOT NEW-BYPASS AND NOT CHECK-NEXT-GP)
            MOVE 'N' TO WS-DATE-FOUND
            PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL
              DATE-FOUND OR (WS-SI-GRP-NO-ID (WS-SUB) = SPACES)
              SET FIRST-TIME TO TRUE
            MOVE 'N' TO WS-DATE-FOUND
                       WS-END-OF-NOTES
                       WS-BYPASS-RECORD
                       WS-GNT41-LINE-VISITED
                       WS-GNT41-FIRST-ERROR
                       WS-DED-IND
                       WS-DED-ERR-REP-IND
                       WS-COINS-IND
                       WS-COIN-ERR-REP-IND
                       WS-COPAY-IND
                       WS-COPAY-ERR-REP-IND
155725                 WS-ALL-ZERO-RECEIVED
155725                 WS-ALL-ZERO
155725                 WS-ERROR-NEW-REP
155725                 WS-REVIEW-GNT41
155725*                WS-WRITE-ERROR-REC-NEW
155725     INITIALIZE  PINDED-NEG1
155725                 NINDED-NEG1
155725                 IINOOP-NEG1
155725                 OINOOP-NEG1
155725                 FERTLMAX-NEG1
           PERFORM 4030-GNT41-LOOKUP THRU 4030-EXIT
                 UNTIL DATE-FOUND OR END-OF-NOTES
           END-PERFORM

           IF (DIRECT-BRAND-GRP OR RICHLINE-GRP OR TOURO-COLLEGE)
              CONTINUE
           ELSE
              MOVE ZEROES TO S9-INPUT-D-CAP-MEM
           END-IF
           IF S9-INPUT-D-DED-MEM IS NOT NUMERIC
              MOVE ZEROS  TO S9-INPUT-D-DED-MEM
           END-IF
           IF S9-INPUT-D-OOP-MEM IS NOT NUMERIC
              MOVE ZEROS  TO S9-INPUT-D-OOP-MEM
           END-IF
           IF S9-INPUT-D-CAP-MEM IS NOT NUMERIC
              MOVE ZEROS  TO S9-INPUT-D-CAP-MEM
           END-IF

           IF END-OF-NOTES
              IF GNT41-LINE-VISITED
                 IF GNT41-FIRST-ERROR
                    CONTINUE
                 ELSE
                    MOVE SPACES TO WS-G41-MESSAGE
                    MOVE 'NO MATCH TO A LINE ON GNT41 '
                                         TO WS-ERR-MESSAGE
                 END-IF
1073             MOVE '93' TO S9-CALL-STATUS
                 PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
              ELSE
               MOVE SPACES TO WS-G41-MESSAGE
               MOVE  'NO GNT41 ON CS90' TO WS-ERR-MESSAGE
1073           MOVE '93' TO S9-CALL-STATUS
               PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
              END-IF
           END-IF

153160     IF (WS-BYPASS-RECORD = 'N') AND NOT BYPASS-ACCUM
             IF  (S9-INPUT-D-DED-MEM >  ZERO OR
                  S9-INPUT-D-OOP-MEM >  ZERO OR
                  S9-INPUT-D-CAP-MEM >  ZERO) AND
                  S9-IN-ACM-RSN-CD = 'RV'
                  MOVE  '+ AMT RECEIVED WHEN  RV '    TO
                         WS-ERR-MESSAGE
                  MOVE 'Y' TO WS-BYPASS-RECORD
1073              MOVE '91' TO S9-CALL-STATUS
                  PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
             END-IF
             IF  (S9-INPUT-D-DED-MEM <  ZERO OR
                  S9-INPUT-D-OOP-MEM <  ZERO OR
                  S9-INPUT-D-CAP-MEM <  ZERO) AND
                  S9-IN-ACM-RSN-CD = 'SA'
                  MOVE  '- AMT RECEIVED WHEN  SA '    TO
                         WS-ERR-MESSAGE
                  MOVE 'Y' TO WS-BYPASS-RECORD
1073              MOVE '91' TO S9-CALL-STATUS
                  PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
             END-IF

153160       IF WS-BYPASS-RECORD = 'N'
      *
155725        COMPUTE PINDED-NEG1   = PINDED-NEG   * -1
155725        COMPUTE NINDED-NEG1   = NINDED-NEG   * -1
155725        COMPUTE IINOOP-NEG1   = IINOOP-NEG   * -1
155725        COMPUTE OINOOP-NEG1   = OINOOP-NEG   * -1
155725        COMPUTE FERTLMAX-NEG1 = FERTLMAX-NEG * -1
      *
155725        IF PINDED-POS    = ZERO  AND
155725           NINDED-POS    = ZERO  AND
155725           IINOOP-POS    = ZERO  AND
155725           OINOOP-POS    = ZERO  AND
155725           FERTLMAX-POS  = ZERO  AND
155725           PINDED-NEG1   = ZERO  AND
155725           NINDED-NEG1   = ZERO  AND
155725           IINOOP-NEG1   = ZERO  AND
155725           OINOOP-NEG1   = ZERO  AND
155725           FERTLMAX-NEG1 = ZERO
155725            SET ALL-ZERO-RECEIVED TO TRUE
155725        END-IF
      *
155725        IF S9-INPUT-D-DED-MEM = ZERO AND
155725           S9-INPUT-D-OOP-MEM = ZERO AND
155725           S9-INPUT-D-CAP-MEM = ZERO
155725           SET ALL-ZERO TO TRUE
                 IF ALL-ZERO-RECEIVED
                   CONTINUE
                 ELSE
155725             IF PINDED-POS  = PINDED-NEG1 AND
155725                NINDED-POS  = NINDED-NEG1 AND
155725                IINOOP-POS  = IINOOP-NEG1 AND
155725                OINOOP-POS  = OINOOP-NEG1 AND
155725                FERTLMAX-POS= FERTLMAX-NEG1
155725                  SET ERROR-NEW-REP TO TRUE
155725             END-IF
155725           END-IF
155725        END-IF
      *
      *
155725        IF (S9-INPUT-D-DED-MEM = ZERO OR
155725         (S9-INPUT-D-DED-MEM NOT= ZERO AND DED-ERR-REP-IND)) AND
155725          (S9-INPUT-D-OOP-MEM = ZERO OR
155725           (S9-INPUT-D-OOP-MEM NOT= ZERO AND COPAY-ERR-REP-IND
155725           AND COIN-ERR-REP-IND)) AND
155725           (S9-INPUT-D-CAP-MEM = ZERO) AND NOT ALL-ZERO
155725           SET REVIEW-GNT41 TO TRUE
155725        END-IF
171497        IF REVIEW-GNT41
171497           CONTINUE
171497        ELSE
                 IF DED-ERR-REP-IND
                    MOVE ZEROES TO S9-INPUT-D-DED-MEM
                 END-IF
                 IF COPAY-ERR-REP-IND AND COIN-ERR-REP-IND
                    MOVE ZEROES TO S9-INPUT-D-OOP-MEM
                 END-IF
171497        END-IF

             MOVE S9-INPUT-D-DED-MEM TO WS-MEM-DED-AMT
             MOVE S9-INPUT-D-OOP-MEM TO WS-MEM-OOP-AMT
             MOVE S9-INPUT-D-CAP-MEM TO WS-MEM-IVF-AMT

             IF ((S9-INPUT-D-DED-MEM < ZEROES AND DED-IND)   OR
                 (S9-INPUT-D-OOP-MEM < ZEROES AND COPAY-IND) OR
                 (S9-INPUT-D-OOP-MEM < ZEROES AND COINS-IND) OR
                 (S9-INPUT-D-CAP-MEM < ZEROES            ))
                  MOVE SPACES        TO S9-INPUT-D-MEM-NO
                  MOVE WS-SI-SUBID   TO S9-INPUT-D-MEM-NO
      *
                  IF S9-INPUT-D-DED-MEM < 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-DED-MEM
                  END-IF
      *
                  IF S9-INPUT-D-OOP-MEM  < 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-OOP-MEM
                  END-IF
      *
                  IF S9-INPUT-D-CAP-MEM   < 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-CAP-MEM
                  END-IF
      *
                  IF  S9-INPUT-D-DED-MEM = ZEROES  AND
                      S9-INPUT-D-OOP-MEM = ZEROES  AND
                      S9-INPUT-D-CAP-MEM = ZEROES
                      CONTINUE
                  ELSE
                     ADD 1              TO WS-NEG-REC
                     WRITE NEG-OUT-REC  FROM S9-INPUT-DETAIL
                     WRITE RESPONS1-REC FROM S9-PARMLIST
                  END-IF
             END-IF

             MOVE WS-MEM-DED-AMT   TO S9-INPUT-D-DED-MEM
             MOVE WS-MEM-OOP-AMT   TO S9-INPUT-D-OOP-MEM
             MOVE WS-MEM-IVF-AMT   TO S9-INPUT-D-CAP-MEM

             IF ((S9-INPUT-D-DED-MEM > ZEROES AND DED-IND)   OR
                 (S9-INPUT-D-OOP-MEM > ZEROES AND COPAY-IND) OR
                 (S9-INPUT-D-OOP-MEM > ZEROES AND COINS-IND) OR
                 (S9-INPUT-D-CAP-MEM > ZEROES            ))
                  MOVE SPACES        TO S9-INPUT-D-MEM-NO
                  MOVE WS-SI-SUBID   TO S9-INPUT-D-MEM-NO
                  IF S9-INPUT-D-DED-MEM > 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-DED-MEM
                  END-IF
      *
                  IF S9-INPUT-D-OOP-MEM > 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-OOP-MEM
                  END-IF
      *
                  IF S9-INPUT-D-CAP-MEM > 0
                     CONTINUE
                  ELSE
                     MOVE ZEROES          TO S9-INPUT-D-CAP-MEM
                  END-IF

                  IF S9-INPUT-D-DED-MEM = ZEROES  AND
                     S9-INPUT-D-OOP-MEM = ZEROES  AND
                     S9-INPUT-D-CAP-MEM = ZEROES
                     CONTINUE
                  ELSE
                     ADD 1              TO WS-POS-REC
                     WRITE POS-OUT-REC  FROM S9-INPUT-DETAIL
1073                 MOVE '00' TO S9-OUT-TOT-REC-STS(1)
1073                 ADD 1 TO WS-RESPONSE-SUS
                     PERFORM 6010-ERROR-DES THRU 6010-EXIT
1073                 WRITE RESPONSE-REC FROM S9-PARMLIST
                  END-IF
             END-IF
155725       IF ERROR-NEW-REP
155725          MOVE  'AS RECEIVED FROM ODS' TO
                       WS-ERR-MESSAGE
                MOVE 'Y' TO WS-ERROR-SW
PROD            MOVE 'Y' TO WS-ZERO-ERROR-SW
155725          MOVE '00' TO S9-CALL-STATUS
155725          SET WRITE-ERROR-REC-NEW TO TRUE
155725          PERFORM 5010-WRITE-ERROR-RECORD
155725       ELSE
155725          IF REVIEW-GNT41
155725             MOVE  'REVIEW GNT41' TO
155725                     WS-ERR-MESSAGE
155725             MOVE 'Y' TO WS-ERROR-SW
155725             MOVE '94' TO S9-CALL-STATUS
155725              PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
155725          ELSE
155725            IF S9-INPUT-D-DED-MEM =  ZERO AND
                     S9-INPUT-D-OOP-MEM =  ZERO AND
                     S9-INPUT-D-CAP-MEM =  ZERO
                     MOVE  'ZERO DOLLAR RECORD RECEIVED' TO
                       WS-ERR-MESSAGE
                     MOVE 'Y' TO WS-ERROR-SW
PROD                 MOVE 'Y' TO WS-ZERO-ERROR-SW
155725               MOVE '00' TO S9-CALL-STATUS
155725               SET WRITE-ERROR-REC-NEW TO TRUE
155725               PERFORM 5010-WRITE-ERROR-RECORD
                  END-IF
                END-IF
             END-IF
153160       END-IF
153160     ELSE
153160       IF WS-BYPASS-ACCUM = 'Y'
153160          MOVE  'OOP AMOUNT OUT OF SYNC WITH DED AMOUNT' TO
153160                   WS-ERR-MESSAGE
153160          MOVE '94' TO S9-CALL-STATUS
153160          PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
             END-IF
194645     END-IF
194645     ELSE
194645        IF NEW-BYPASS
194645          MOVE '94' TO S9-CALL-STATUS
194645          PERFORM 5010-WRITE-ERROR-RECORD THRU 5010-EXIT
194645        END-IF
           END-IF.

       3010-EXIT.
            EXIT.
      *
       4010-DEST-ID.
      *
             INITIALIZE S-DEST
                        WS-DEST-ID.
             MOVE ZEROS TO WS-ED-STATUS
             MOVE 4 TO WS-ED-FUNC-INDEX.
             MOVE S9-IN-CVRG-PLAN-ID(1:9) TO S-GROUP-KEY
             MOVE ZEROS TO WS-ED-CODE2
             CALL CTEGSDST USING S-GROUP-KEY
                                 WS-ED-CODE2
                                 WS-ED-FUNC-INDEX
                                 WS-ED-STATUS.
      *
             IF WS-ED-STATUS = ZEROS
                MOVE 5 TO WS-ED-FUNC-INDEX
      *
                CALL CTEGSDST USING S-DEST
                                    WS-ED-CODE2
                                    WS-ED-FUNC-INDEX
                                    WS-ED-STATUS
      *
                IF WS-ED-STATUS = ZERO
                   MOVE S-DEST               TO  WS-DEST-ID
                ELSE
                   MOVE ZEROS TO WS-ED-STATUS
                   MOVE 4 TO WS-ED-FUNC-INDEX
                   MOVE ZEROS TO WS-ED-CODE2
                   CALL CTEGPDST USING S-GRP-NO-ID
                                       WS-ED-CODE2
                                       WS-ED-FUNC-INDEX
                                       WS-ED-STATUS
      *
                   IF WS-ED-STATUS = ZEROS
                     MOVE 5 TO WS-ED-FUNC-INDEX
      *
                     CALL CTEGPDST USING S-DEST
                                       WS-ED-CODE2
                                       WS-ED-FUNC-INDEX
                                       WS-ED-STATUS
                     IF WS-ED-STATUS = ZERO
                      MOVE S-DEST               TO  WS-DEST-ID
                     ELSE
                      MOVE 'DEFT'               TO  WS-DEST-ID
                     END-IF
                   ELSE
                     MOVE 'DEFT'            TO WS-DEST-ID
                   END-IF
                END-IF
             ELSE
                   MOVE ZEROS TO WS-ED-STATUS
                   MOVE 4 TO WS-ED-FUNC-INDEX
                   MOVE ZEROS TO WS-ED-CODE2
                   CALL CTEGPDST USING S-GRP-NO-ID
                                       WS-ED-CODE2
                                       WS-ED-FUNC-INDEX
                                       WS-ED-STATUS
      *
                   IF WS-ED-STATUS = ZEROS
                     MOVE 5 TO WS-ED-FUNC-INDEX
      *
                     CALL CTEGPDST USING S-DEST
                                       WS-ED-CODE2
                                       WS-ED-FUNC-INDEX
                                       WS-ED-STATUS
                     IF WS-ED-STATUS = ZERO
                      MOVE S-DEST               TO  WS-DEST-ID
                     ELSE
                      MOVE 'DEFT'               TO  WS-DEST-ID
                     END-IF
                   ELSE
                      MOVE 'DEFT'               TO WS-DEST-ID
                   END-IF
             END-IF
             .
      *
       4010-EXIT.
            EXIT.
      *
       4020-ACM-DETAIL-REC.
      *
            INITIALIZE WS-IN-NTWK-CD
                       WS-DED-AMOUNT-P
                       WS-DED-AMOUNT-N
                       WS-OOP-AMOUNT-P
                       WS-OOP-AMOUNT-N
                       WS-IVF-AMOUNT-P
                       WS-IVF-AMOUNT-N
                       WS-DEDUCT-AMOUNT
                       WS-OOP-AMOUNT
                       WS-IVF-AMOUNT
                       WS-IN-BNFT-DTL-AMT1
                       WS-IN-BNFT-DTL-AMT
155725                 PINDED-POS
155725                 PINDED-NEG
155725                 NINDED-POS
155725                 NINDED-NEG
155725                 IINOOP-POS
155725                 IINOOP-NEG
155725                 OINOOP-POS
155725                 OINOOP-NEG
155725                 FERTLMAX-POS
155725                 FERTLMAX-NEG
153160      MOVE 'N' TO WS-BYPASS-ACCUM
            PERFORM VARYING WS-ACCUM FROM 1 BY 1
                                     UNTIL WS-ACCUM > S9-IN-TOT-RECS
             MOVE S9-IN-BNFT-DTL-AMT(WS-ACCUM) TO WS-IN-BNFT-DTL-AMT1
             MOVE WS-IN-BNFT-DTL-AMT1(2:12) TO WS-IN-BNFT-DTL-AMT(1:12)
             MOVE WS-IN-BNFT-DTL-AMT1(15:2) TO WS-IN-BNFT-DTL-AMT(13:2)
             IF (WS-IN-BNFT-DTL-AMT1(1:1) = '+')
               CONTINUE
             ELSE
                 MULTIPLY WS-IN-BNFT-DTL-AMT BY -1 GIVING
                     WS-IN-BNFT-DTL-AMT
             END-IF
158916       IF S9-IN-ACCUM-NM(WS-ACCUM)  = 'PINDED' OR 'PFMDED' OR
158916                                      'NINDED' OR 'NFMDED'
158916          SET BYPASS-DED TO TRUE
158916       END-IF
             IF S9-IN-ACCUM-NM(WS-ACCUM)  = 'PINDED'
155725        IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725            ADD WS-IN-BNFT-DTL-AMT TO PINDED-POS
155725        ELSE
155725            ADD WS-IN-BNFT-DTL-AMT TO PINDED-NEG
155725        END-IF
              ADD WS-IN-BNFT-DTL-AMT TO WS-DED-AMOUNT-P
             ELSE
              IF S9-IN-ACCUM-NM(WS-ACCUM) = 'NINDED'
155725         IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725            ADD WS-IN-BNFT-DTL-AMT TO NINDED-POS
155725         ELSE
155725            ADD WS-IN-BNFT-DTL-AMT TO NINDED-NEG
155725         END-IF
               ADD WS-IN-BNFT-DTL-AMT TO WS-DED-AMOUNT-N
               ELSE
                 IF S9-IN-ACCUM-NM(WS-ACCUM) = 'IINOOP'
155725           IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725               ADD WS-IN-BNFT-DTL-AMT TO IINOOP-POS
155725           ELSE
155725               ADD WS-IN-BNFT-DTL-AMT TO IINOOP-NEG
155725           END-IF
                  ADD WS-IN-BNFT-DTL-AMT TO WS-OOP-AMOUNT-P
                 ELSE
                  IF S9-IN-ACCUM-NM(WS-ACCUM) = 'OINOOP'
155725             IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725               ADD WS-IN-BNFT-DTL-AMT TO OINOOP-POS
155725             ELSE
155725               ADD WS-IN-BNFT-DTL-AMT TO OINOOP-NEG
155725             END-IF
                   ADD WS-IN-BNFT-DTL-AMT TO WS-OOP-AMOUNT-N
                  ELSE
                    IF S9-IN-ACCUM-NM(WS-ACCUM) = 'FERTLMAX' AND
                       S9-IN-NTWK-CD(WS-ACCUM) = 'P'
155725                IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725                   ADD WS-IN-BNFT-DTL-AMT TO FERTLMAX-POS
155725                ELSE
155725                   ADD WS-IN-BNFT-DTL-AMT TO FERTLMAX-NEG
155725                END-IF
                      ADD WS-IN-BNFT-DTL-AMT TO
                                               WS-IVF-AMOUNT-P
                    ELSE
                     IF S9-IN-ACCUM-NM(WS-ACCUM) = 'FERTLMAX' AND
                         S9-IN-NTWK-CD(WS-ACCUM) = 'N'
155725                IF  (WS-IN-BNFT-DTL-AMT > ZERO)
155725                   ADD WS-IN-BNFT-DTL-AMT TO FERTLMAX-POS
155725                ELSE
155725                   ADD WS-IN-BNFT-DTL-AMT TO FERTLMAX-NEG
155725                END-IF
                     ADD WS-IN-BNFT-DTL-AMT TO
                                               WS-IVF-AMOUNT-N
                     END-IF
                    END-IF
                  END-IF
                 END-IF
                END-IF
              END-IF
              IF WS-IN-NTWK-CD = ' '
                MOVE S9-IN-NTWK-CD(WS-ACCUM) TO WS-IN-NTWK-CD
                MOVE S9-IN-NTWK-CD(WS-ACCUM) TO S9-INPUT-D-NTWK-IND
              END-IF
            END-PERFORM.
      *
            IF WS-DED-AMOUNT-P  NOT = ZERO
               MOVE WS-DED-AMOUNT-P TO WS-DEDUCT-AMOUNT
            ELSE
               IF WS-DED-AMOUNT-N  NOT = ZERO
                  MOVE WS-DED-AMOUNT-N TO WS-DEDUCT-AMOUNT
               END-IF
            END-IF
            MOVE WS-DEDUCT-AMOUNT TO S9-INPUT-D-DED-MEM
            IF WS-IVF-AMOUNT-P  NOT = ZERO
               MOVE WS-IVF-AMOUNT-P TO WS-IVF-AMOUNT
            ELSE
               IF WS-IVF-AMOUNT-N  NOT = ZERO
                  MOVE WS-IVF-AMOUNT-N TO WS-IVF-AMOUNT
               END-IF
            END-IF
            MOVE WS-IVF-AMOUNT TO S9-INPUT-D-CAP-MEM
            IF WS-OOP-AMOUNT-P NOT = ZERO
               MOVE WS-OOP-AMOUNT-P TO WS-OOP-AMOUNT
            ELSE
               IF WS-OOP-AMOUNT-N NOT = ZERO
                  MOVE WS-OOP-AMOUNT-N TO WS-OOP-AMOUNT
               END-IF
            END-IF
      *
153160      IF ((WS-DEDUCT-AMOUNT >  ZERO) AND
153160          (WS-OOP-AMOUNT < WS-DEDUCT-AMOUNT)) OR
153160         ((WS-DEDUCT-AMOUNT <  ZERO) AND
153160          (WS-OOP-AMOUNT > WS-DEDUCT-AMOUNT))
153160            MOVE 'Y' TO WS-BYPASS-ACCUM
153160      END-IF
      *
            INITIALIZE WS-COINS-AMT
               COMPUTE WS-COINS-AMT     = WS-OOP-AMOUNT
                                        - WS-DEDUCT-AMOUNT
               INITIALIZE WS-OOP-AMOUNT
               MOVE WS-COINS-AMT             TO WS-OOP-AMOUNT
            MOVE WS-OOP-AMOUNT TO S9-INPUT-D-OOP-MEM.
      *
       4020-EXIT.
           EXIT.
      *
       2030-READ-SUBSCRIBER-GROUP.
      ***************************************************************
      *  CALL GROUP SEGMENT IN SUBSCRIBER DATABASE                  *
      *  READS THE VSSLGRU SEGMENT AND VALIDATE THE GROUP INFO      *
      *  GN CALL IS REPLACED BY GU CALL TO VALIDATE INPUT GROUP     *
      ***************************************************************


           MOVE SPACES             TO  SUB-STATUS-CODE
440881     MOVE 'GU  '             TO  IMS-FUNCTION
440881     MOVE SPACES TO VALUE-VSGPGRU
440881     MOVE S9-IN-CVRG-PLAN-ID(1:9) TO VALUE-VSGPGRU

           CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                   VDSSUB10-PCB
                                   SUB-GROUP-PTR-SEG
                                   SUB-ROOT-SSA
440881                             SSA-VSGPGRU-QUAL.

           IF SUB-DB-SEG-FOUND
              SET PKG-NOT-FOUND  TO TRUE
              MOVE 'N'    TO   WS-END-SUB-PKG-SEG
              PERFORM 2031-GET-SUB-PKG THRU 2031-EXIT
              UNTIL END-SUB-PKG-SEG OR PKG-FOUND
           ELSE
              IF SUB-DB-SEG-NOT-FOUND OR SUB-NEW-SEG-SAME-LEVEL
                 MOVE 'Y' TO VSSLGRU-END-OF-SEG-SW
                 MOVE  'VALID GROUP NOT FOUND' TO
                                               WS-ERR-MESSAGE
                        MOVE 'Y' TO WS-ERROR-SW
                        MOVE 'Y' TO WS-CHECK-NEXT-GP
1073                    MOVE '98' TO S9-CALL-STATUS
                        MOVE 'N' TO WS-ERR-MSG-SW
              ELSE
                 MOVE 'Y' TO VSSLGRU-END-OF-SEG-SW
                 DISPLAY '********************************************'
                 DISPLAY 'INVALID CALL                                '
                 DISPLAY 'ACCESSING SUBSCRIBER DB - SUB GROUP SEGMENT '
                 DISPLAY 'SEGMENT NAME - VSSLGRU                      '
                 DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
                 DISPLAY '********************************************'
                 CALL 'ZNCANCEL'
              END-IF
           END-IF.

      *
       2030-EXIT.
           EXIT.
      *
       2031-GET-SUB-PKG.
      ***************************************************************
      *  SUBSCRIBER DATABASE                                        *
      *  READS THE PACKAGE SEGMENT FOR A PARTICULAR GROUP           *
      *  CHECKS FOR ACTIVE PACKAGE AND THEN POPULATES GRP AND SUBGRP*
      ***************************************************************

010915     INITIALIZE              WS-CON-TY-CD
           MOVE SPACES             TO SUB-STATUS-CODE
           MOVE 'GNP '             TO IMS-FUNCTION
           CALL 'CBLTDLI'  USING   IMS-FUNCTION
                                   VDSSUB10-PCB
                                   SUB-PKG-SEG
                                   SUB-PKG-SSA.

           IF SUB-STATUS-CODE = SPACES

      *          MOVE GS-PKG-NO       TO WS-PKG-NO
010915           MOVE GS-CON-TY-CD    TO WS-CON-TY-CD
                 IF GS-PKG-TERM-DT = ZEROS
                    MOVE  WS-HIGH-DATE TO GS-PKG-TERM-DT
                 END-IF

                 MOVE  GS-COV-EFF-DT        TO  WK-PKG-EFF-DT
                 MOVE  GS-PKG-TERM-DT       TO  WK-PKG-TRM-DT
                 IF WK-PKG-EFF-DT (1:1) = 1
                    MOVE '19'               TO WS-PKG-EFF-DT(1:2)
                 ELSE
                    MOVE '20'               TO WS-PKG-EFF-DT(1:2)
                 END-IF
                 MOVE WK-PKG-EFF-DT(2:2)    TO WS-PKG-EFF-DT(3:2)
                 MOVE WK-PKG-EFF-DT(4:4)    TO WS-PKG-EFF-DT(5:4)
                 IF WK-PKG-TRM-DT (1:1) = 1
                    MOVE '19'               TO WS-PKG-TRM-DT(1:2)
                 ELSE
                    MOVE '20'               TO WS-PKG-TRM-DT(1:2)
                 END-IF
                 MOVE WK-PKG-TRM-DT(2:2)    TO WS-PKG-TRM-DT(3:2)
                 MOVE WK-PKG-TRM-DT(4:4)    TO WS-PKG-TRM-DT(5:4)

                 IF (S9-IN-SRVC-STRT-DT >= WS-PKG-EFF-DT) AND
                    (S9-IN-SRVC-END-DT <= WS-PKG-TRM-DT)
194645              MOVE 'N'        TO NEW-BYPASS-SWITCH
333803*             SET PKG-FOUND     TO TRUE
                    ADD  1            TO WS-SUB
                    MOVE GS-PKG-NO    TO WS-GS-PKG-NO    (WS-SUB)
                    MOVE 'N'          TO WS-CHECK-NEXT-GP
                    MOVE 'N'          TO WS-ERROR-SW
                    MOVE 'N'          TO WS-ERR-MSG-SW
333803** CALLS GROUP SEGEMENT TO CHECK FOR PACAKGES FOR ACTIVE GRP*
333803** VALIDATES THE PACAKGES AND WHEN ERROR FLAG IS SET,      **
333803** APPROPRIATE ERROR MESSAGES ARE POPULATED TO ERROR REPORT**
333803              MOVE 'N' TO WS-END-OF-PAC
333803              SET PKG-NOT-FOUND  TO TRUE
333803              MOVE S9-IN-CVRG-PLAN-ID(1:9) TO VSGPGRU-KEY(1:9)
333803              MOVE 'GU  '             TO  IMS-FUNCTION
333803              CALL 'CBLTDLI' USING IMS-FUNCTION
333803                          VFGGRP10-PCB
333803                          GRP-PKG-SEGMENT
333803                          VSGPGRU-SSA-QUAL
333803              PERFORM 1110-GET-GROUP-PKG-SEG THRU 1110-EXIT
333803                UNTIL END-OF-PAC OR PKG-FOUND
333803              IF PKG-FOUND
333803                IF WS-ED-STATUS = ZEROES
333803                  IF S9-INPUT-D-NTWK-IND = 'P'
333803                   MOVE RULE3-VFC(1:3) TO RULE3-VFC-FINAL
333803                  ELSE
333803                   IF S9-INPUT-D-NTWK-IND = 'N'
333803                    MOVE RULE3-VFC(4:3) TO RULE3-VFC-FINAL
333803                   END-IF
333803                  END-IF
333803                  IF RULE3-VFC-FINAL = SPACES
333803                     MOVE '000' TO RULE3-VFC-FINAL
333803                  END-IF
333803                  MOVE RULE3-VFC-FINAL TO S9-INPUT-D-RUL3-VFC
333803                ELSE
333803                  MOVE 'VALID PCC NOT FOUND ON EDT'
333803                                 TO WS-ERR-MESSAGE
333803                  SET NEW-BYPASS                    TO TRUE
333803                END-IF
333803              ELSE
333803                 SET NEW-BYPASS               TO TRUE
333803                 MOVE 'VALID GROUP NOT FOUND' TO WS-ERR-MESSAGE
333803              END-IF
                 ELSE
                    MOVE 'Y'          TO WS-CHECK-NEXT-GP
333803              MOVE 'NO MEMBER COVERAGE FOR DOS' TO WS-ERR-MESSAGE
                 END-IF
           ELSE
            IF SUB-STATUS-CODE = 'GE' OR 'GB'
               MOVE 'Y' TO WS-END-SUB-PKG-SEG
               ADD 1 TO WS-SUBDB-PKG-NOTFND
            ELSE
               DISPLAY '********************************************'
               DISPLAY 'INVALID CALL'
               DISPLAY 'ACCESSING SUBSCRIBER DB - PACKAGE SEGMENT'
               DISPLAY 'IMS ERROR, STATUS CODE = ' SUB-STATUS-CODE
               DISPLAY '********************************************'
               CALL 'ZNCANCEL'
            END-IF
           END-IF.

       2031-EXIT.
            EXIT.
      *
       4030-GNT41-LOOKUP.
           MOVE 'N' TO WS-BEN-CAL-FOUND
                       WS-NOTE41-FOUND
                       WS-CHECK-A-NOTE

            MOVE S9-IN-CVRG-PLAN-ID(1:9) TO VSGPGRU-KEY1 (1:9)
                                            VSGPGRU-KEY2 (1:9)

           PERFORM 4040-GET-VSGPNTE THRU 4040-EXIT
           MOVE GO-VSGPNTEC       TO GO-S9MGNT41


           IF NOTE41-FOUND
              MOVE 'N' TO WS-CALENDER-YEAR
              MOVE ZEROES TO WS-GO-NT41-EFF-DATE
              MOVE ZEROES TO WS-GO-NT41-TRM-DATE
              MOVE SPACES TO WS-GO-NT41-PKG-NO
      *
              MOVE GO-NT41-EFF-DATE  TO WS-GO-NT41-EFF-DATE
              MOVE GO-NT41-TRM-DATE  TO WS-GO-NT41-TRM-DATE
              MOVE GO-NT41-PG-NO     TO WS-GO-NT41-PKG-NO
              MOVE GO-NT41-VENTYPE   TO WS-GO-NT41-VENTYPE
              IF WS-GO-NT41-TRM-DATE = ZEROES
                 MOVE '2991231' TO WS-GO-NT41-TRM-DATE
              END-IF

              IF WS-GO-NT41-EFF-DATE (1:1) = 1
                MOVE 19                        TO WS-GNT-EFF-DATE(1:2)
                MOVE WS-GO-NT41-EFF-DATE (2:6) TO WS-GNT-EFF-DATE(3:6)
              ELSE
                IF WS-GO-NT41-EFF-DATE (1:1) = 2
                  MOVE 20                        TO WS-GNT-EFF-DATE(1:2)
                  MOVE WS-GO-NT41-EFF-DATE (2:6) TO WS-GNT-EFF-DATE(3:6)
                END-IF
              END-IF
              IF WS-GO-NT41-TRM-DATE (1:1) = 1
                MOVE 19                        TO WS-GNT-TRM-DATE(1:2)
                MOVE WS-GO-NT41-TRM-DATE (2:6) TO WS-GNT-TRM-DATE(3:6)
              ELSE
                IF WS-GO-NT41-TRM-DATE (1:1) = 2
                  MOVE 20                        TO WS-GNT-TRM-DATE(1:2)
                  MOVE WS-GO-NT41-TRM-DATE (2:6) TO WS-GNT-TRM-DATE(3:6)
                END-IF
              END-IF

              IF (S9-IN-SRVC-STRT-DT >= WS-GNT-EFF-DATE
136387            AND S9-IN-SRVC-END-DT <= WS-GNT-TRM-DATE )
                  PERFORM A260-CAL-BENEFIT-PERIOD
                  IF (BEN-CAL-FOUND AND
                      GO-NT41-VENNAME = WS-VENNAME)
                    IF WS-GO-NT41-VENTYPE = 'O'
                        MOVE 'Y' TO WS-CHECK-A-NOTE
                    ELSE
                         MOVE 'N' TO WS-CHECK-A-NOTE
                         MOVE  SPACES TO WS-G41-MESSAGE
                         MOVE  'INVALID VENDOR TYPE'
                                                  TO WS-G41-MESSAGE
                         MOVE 'Y' TO WS-GNT41-FIRST-ERROR
                         MOVE 'Y' TO WS-GNT41-LINE-VISITED
                         ADD 1 TO WS-BYPASS-COUNT
                         MOVE 'Y' TO WS-BYPASS-RECORD
                    END-IF
                    IF CHECK-A-NOTE
                       MOVE 'N' TO WS-BYPASS-RECORD
                       MOVE 'Y' TO WS-DATE-FOUND
                       IF WS-DEDUCT-AMOUNT NOT = ZEROES
                        EVALUATE GO-NT41-DEDI
                          WHEN 'I'
                              IF WS-IN-NTWK-CD = 'P'
                                 MOVE 'Y' TO WS-DED-IND
                              ELSE
                                 MOVE 'Y' TO WS-DED-ERR-REP-IND
                              END-IF
                          WHEN 'O'
                              IF WS-IN-NTWK-CD = 'N'
                                 MOVE 'Y' TO WS-DED-IND
                              ELSE
                                 MOVE 'Y' TO WS-DED-ERR-REP-IND
                              END-IF
                          WHEN 'B'
                              IF WS-IN-NTWK-CD = 'N' OR 'P'
                                 MOVE 'Y' TO WS-DED-IND
                              ELSE
                                 MOVE 'Y' TO WS-DED-ERR-REP-IND
                              END-IF
                          WHEN 'N'
                              IF WS-IN-NTWK-CD = ' '
                                 MOVE 'Y' TO WS-DED-IND
                              ELSE
                                 MOVE 'Y' TO WS-DED-ERR-REP-IND
                              END-IF
                          WHEN 'X'
                              IF WS-IN-NTWK-CD = 'N' OR 'P' OR ' '
                                 MOVE 'Y' TO WS-DED-ERR-REP-IND
                              END-IF
                          WHEN 'L'
                              MOVE 'Y' TO WS-DED-IND
                        END-EVALUATE
                       END-IF

                       IF WS-OOP-AMOUNT NOT = ZEROES
                        EVALUATE GO-NT41-COINSI
                          WHEN 'I'
                              IF WS-IN-NTWK-CD = 'P'
                                 MOVE 'Y' TO WS-COINS-IND
                              ELSE
                                 MOVE 'Y' TO WS-COIN-ERR-REP-IND
                              END-IF
                          WHEN 'O'
                              IF WS-IN-NTWK-CD = 'N'
                                 MOVE 'Y' TO WS-COINS-IND
                              ELSE
                                 MOVE 'Y' TO WS-COIN-ERR-REP-IND
                              END-IF
                          WHEN 'B'
                              IF WS-IN-NTWK-CD = 'N' OR 'P'
                                 MOVE 'Y' TO WS-COINS-IND
                              ELSE
                                 MOVE 'Y' TO WS-COIN-ERR-REP-IND
                              END-IF
                          WHEN 'N'
                              IF WS-IN-NTWK-CD = ' '
                                 MOVE 'Y' TO WS-COINS-IND
                              ELSE
                                 MOVE 'Y' TO WS-COIN-ERR-REP-IND
                              END-IF
                          WHEN 'X'
                              IF WS-IN-NTWK-CD = 'N' OR 'P' OR ' '
                                 MOVE 'Y' TO WS-COIN-ERR-REP-IND
                              END-IF
                          WHEN 'L'
                              MOVE 'Y' TO WS-COINS-IND
                        END-EVALUATE
                       END-IF

                       IF WS-OOP-AMOUNT NOT = ZEROES
                        EVALUATE GO-NT41-COPAYI
                          WHEN 'I'
                              IF WS-IN-NTWK-CD = 'P'
                                 MOVE 'Y' TO WS-COPAY-IND
                              ELSE
                                 MOVE 'Y' TO WS-COPAY-ERR-REP-IND
                              END-IF
                          WHEN 'O'
                              IF WS-IN-NTWK-CD = 'N'
                                 MOVE 'Y' TO WS-COPAY-IND
                              ELSE
                                 MOVE 'Y' TO WS-COPAY-ERR-REP-IND
                              END-IF
                          WHEN 'B'
                              IF WS-IN-NTWK-CD = 'N' OR 'P'
                                 MOVE 'Y' TO WS-COPAY-IND
                              ELSE
                                 MOVE 'Y' TO WS-COPAY-ERR-REP-IND
                              END-IF
                          WHEN 'N'
                              IF WS-IN-NTWK-CD = ' '
                                 MOVE 'Y' TO WS-COPAY-IND
                              ELSE
                                 MOVE 'Y' TO WS-COPAY-ERR-REP-IND
                              END-IF
                          WHEN 'X'
                              IF WS-IN-NTWK-CD = 'N' OR 'P' OR ' '
                                 MOVE 'Y' TO WS-COPAY-ERR-REP-IND
                              END-IF
                          WHEN 'L'
                              MOVE 'Y' TO WS-COPAY-IND
                        END-EVALUATE
                       END-IF
                    ELSE
                       MOVE 'N' TO WS-DATE-FOUND
                       MOVE 'Y' TO WS-GNT41-LINE-VISITED
                       ADD 1 TO WS-BYPASS-COUNT
                       MOVE 'Y' TO WS-BYPASS-RECORD
                    END-IF
                  ELSE
                     MOVE 'N' TO WS-BEN-CAL-FOUND
                     MOVE 'Y' TO WS-GNT41-LINE-VISITED
                     ADD 1 TO WS-BYPASS-COUNT
                     MOVE 'Y' TO WS-BYPASS-RECORD
                  END-IF
              ELSE
                 MOVE 'Y' TO WS-GNT41-LINE-VISITED
                 ADD 1 TO WS-BYPASS-COUNT
                 MOVE 'Y' TO WS-BYPASS-RECORD
              END-IF
             ELSE
      *         MOVE 'Y' TO WS-GNT41-LINE-VISITED
                ADD 1 TO WS-BYPASS-COUNT
                MOVE 'Y' TO WS-BYPASS-RECORD
           END-IF.

      *
       4030-EXIT.
            EXIT.
      *
       4040-GET-VSGPNTE.
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
              MOVE 'GU  '    TO IMS-FUNCTION
              MOVE 'N'       TO WS-FIRST-TIME
           ELSE
              MOVE 'GN  '    TO IMS-FUNCTION
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
           IF GRP-DB-SEG-FOUND
              INITIALIZE WS-NTEC-PKGNO
                         WS-PKG-NOX
              MOVE GO-GRP-MISC-TX(1:3) TO WS-NTEC-PKGNO
              MOVE WS-GS-PKG-NO (WS-SUB) TO WS-PKG-NOX
              IF WS-NTEC-PKGNO = (WS-PKG-NOX OR 'ALL')
                 MOVE 'Y'   TO WS-NOTE41-FOUND
              END-IF
           ELSE
              IF GRP-DB-SEG-NOT-FOUND
                 OR GRP-END-OF-DB
                 MOVE 'Y' TO WS-END-OF-NOTES
                 MOVE 'N' TO WS-NOTE41-FOUND
              ELSE
                 MOVE 'N' TO WS-NOTE41-FOUND
                 MOVE 'Y' TO WS-END-OF-NOTES
              DISPLAY '********************************************'
              DISPLAY 'INVALID CALL                                '
              DISPLAY 'ACCESSING GNT41 SEGMENT ON GROUP DB         '
              DISPLAY 'GRP: ' SI-GRP-NO-ID ',SUBGRP: ' SI-SUBGR-NO
              DISPLAY 'IMS ERROR, STATUS CODE = ' GRP-STATUS-CODE
              DISPLAY '********************************************'
              CALL 'ZNCANCEL'
              END-IF
           END-IF.
      *
       4040-EXIT.
            EXIT.
      *
       A260-CAL-BENEFIT-PERIOD.
           MOVE ZEROES TO WS-GO-NT41-PLANMD
           MOVE 'N' TO WS-CALENDER-YEAR
           IF GO-NT41-PLANMD NOT = ZEROES AND LOW-VALUES
              MOVE GO-NT41-PLANMD  TO WS-GO-NT41-PLANMD
              IF WS-GO-NT41-PLANMD = SPACES
                 CONTINUE
              ELSE
                SET OFF-CAL-YEAR-FOUND  TO TRUE
              END-IF
           END-IF
           IF OFF-CAL-YEAR-FOUND
              MOVE WS-GO-NT41-PLANMD (1:2) TO WS-NT41-BEN-EFF-MO
              IF WS-NT41-BEN-EFF-MO NOT = ZEROES
                 MOVE ZEROS TO WS-DOS-YEAR
                 MOVE S9-IN-SRVC-STRT-DT (1:4) TO WS-DOS-YEAR
                 MOVE WS-NT41-BEN-EFF-MO TO
                                  WS-INTER-BEN-START-DTE (5:2)
                                  WS-NT41-BEN-EFF-MO-PRV
                 COMPUTE WS-NT41-BEN-EFF-MO-PRV =
                                  WS-NT41-BEN-EFF-MO-PRV - 1
                 IF WS-NT41-BEN-EFF-MO-PRV = 00
                    MOVE  12  TO WS-NT41-BEN-EFF-MO-PRV
                 END-IF
                 MOVE WS-NT41-BEN-EFF-MO-PRV
                                  TO WS-INTER-BEN-END-DTE (5:2)

                 IF S9-IN-SRVC-STRT-DT (5:2) >= WS-NT41-BEN-EFF-MO
                    MOVE  WS-DOS-YEAR TO
                                  WS-INTER-BEN-START-DTE (1:4)
                                  WS-NEXT-YEAR
                                  WS-RUN-DATE1
                    COMPUTE WS-NEXT-YEAR = WS-NEXT-YEAR + 1
                    MOVE WS-NEXT-YEAR TO WS-INTER-BEN-END-DTE (1:4)
                 ELSE
                    COMPUTE WS-PREV-YEAR = WS-DOS-YEAR - 1
                    MOVE WS-PREV-YEAR TO
                                  WS-INTER-BEN-START-DTE (1:4)
                                  WS-NEXT-YEAR
                                  WS-RUN-DATE1
                    COMPUTE WS-NEXT-YEAR = WS-NEXT-YEAR + 1
                    MOVE WS-NEXT-YEAR TO WS-INTER-BEN-END-DTE (1:4)
                 END-IF
                 MOVE WS-GO-NT41-PLANMD (3:2) TO
                                    WS-INTER-BEN-START-DTE (7:2)
                 IF WS-NT41-BEN-EFF-MO-PRV = (01 OR  03  OR  05  OR  07
                                        OR  08  OR  10  OR  12)
                    MOVE  31  TO WS-INTER-BEN-END-DTE (7:2)
                 ELSE
                  IF WS-NT41-BEN-EFF-MO-PRV = (04 OR 06 OR 09 OR 11)
                    MOVE  30  TO WS-INTER-BEN-END-DTE (7:2)
                  ELSE
                    COMPUTE WS-LEAP-YEAR =
                            WS-NEXT-YEAR / 4
                    IF WS-LEAP-YEAR(4:2) > 0
                       MOVE  28  TO WS-INTER-BEN-END-DTE (7:2)
                    ELSE
                       MOVE  29  TO WS-INTER-BEN-END-DTE (7:2)
                    END-IF
                  END-IF
                 END-IF
                 IF (S9-IN-SRVC-STRT-DT >= WS-INTER-BEN-START-DTE
                     AND S9-IN-SRVC-END-DT <= WS-INTER-BEN-END-DTE)
                       MOVE 'Y' TO WS-BEN-CAL-FOUND
                 ELSE
                       MOVE 'Y' TO WS-BEN-CAL-FOUND
                 END-IF
              END-IF
           ELSE
              MOVE 'Y' TO WS-BEN-CAL-FOUND
           END-IF.

      *
      **************************************************************
      *    CALL GROUP PACKAGE SEGMENTS                             *
      * READS ALL THE PACKAGE SEGMENTS IN GROUP AND WHEN PACKAGE   *
      * IS FOUND APPROPRIATE RULE3 VFC WILL BE DECODED AND IT WILL *
      * BE USED IN CLHST CREATION                                  *
      **************************************************************
      *
010915 1110-GET-GROUP-PKG-SEG.
      *
010915     INITIALIZE             VSGPPAC-KEY
                                  VSGPGRU-KEY
PCCFIX     MOVE S9-IN-CVRG-PLAN-ID(1:9) TO VSGPGRU-KEY(1:9)
333803     MOVE GS-PKG-NO                TO VSGPPAC-KEY
333803     MOVE 'GN  '             TO  IMS-FUNCTION
           CALL 'CBLTDLI' USING IMS-FUNCTION
                                VFGGRP10-PCB
                                GRP-PKG-SEGMENT
                                VSGPGRU-SSA-QUAL
333803                          VSGPPAC-SSA-UNQUAL.
      *
           IF GRP-STATUS-CODE = '  '
333803      IF (GB-PKG-NO = GS-PKG-NO)
333803        MOVE  GB-PKG-STRT-DT       TO   WK-PCC-EFF-DT
333803        MOVE  GB-PKG-END-DT        TO  WK-PCC-TRM-DT
333803        IF WK-PCC-EFF-DT (1:1) = 1
333803           MOVE '19'               TO WS-PCC-EFF-DT(1:2)
333803        ELSE
333803           MOVE '20'               TO WS-PCC-EFF-DT(1:2)
333803        END-IF
333803        MOVE WK-PCC-EFF-DT(2:2)    TO WS-PCC-EFF-DT(3:2)
333803        MOVE WK-PCC-EFF-DT(4:4)    TO WS-PCC-EFF-DT(5:4)
333803        IF WK-PCC-TRM-DT (1:1) = 1
333803           MOVE '19'               TO WS-PCC-TRM-DT(1:2)
333803        ELSE
333803           MOVE '20'               TO WS-PCC-TRM-DT(1:2)
333803        END-IF
333803        MOVE WK-PCC-TRM-DT(2:2)    TO WS-PCC-TRM-DT(3:2)
333803        MOVE WK-PCC-TRM-DT(4:4)    TO WS-PCC-TRM-DT(5:4)
333803        IF (S9-IN-SRVC-STRT-DT >= WS-PCC-EFF-DT) AND
333803           (S9-IN-SRVC-END-DT <= WS-PCC-TRM-DT)
                 PERFORM 1121-TRANS-PROD-COMB
333803        END-IF
333803      END-IF
           ELSE
             IF GRP-STATUS-CODE = 'GE' OR 'GB'
333803        MOVE 'Y' TO WS-END-OF-PAC
             ELSE
              DISPLAY '****                                    ****'
              DISPLAY '****        1110-GET-GROUP-PKG-SEG      ****'
              DISPLAY '****      INVALID IMS STATUS CODE       ****'
              DISPLAY '****       ABNORMAL  TERMINATION        ****'
              DISPLAY '**** STATUS CODE      = ' GRP-STATUS-CODE
              DISPLAY '**** SSA1             = ' VSGPGRU-SSA-QUAL
              DISPLAY '**** SSA2             = ' VSGPPAC-SSA-QUAL
              DISPLAY '*                                          *'
              DISPLAY '********************************************'
              CALL 'ZNCANCEL'
             END-IF
           END-IF.
      *
       1110-EXIT.
           EXIT.
      *
010915 1121-TRANS-PROD-COMB.
      *
           MOVE GB-PROD-COMB-CD TO TAG1.
           MOVE SPACES TO PROD-COMB-CD.

           CALL NTUPRDCD USING PROD-COMB-CD
                               TAG1
                               B5
                               RET-STATUS.

           IF RET-STATUS = ZEROES
              PERFORM 1122-GET-VFC
           END-IF.
      *
010915 1122-GET-VFC.
      *
           INITIALIZE RULE3-VFC
                      WS-ED-STATUS
           IF WS-CON-TY-CD = 18 OR 19
333803       IF PROD-COMB-CD-3-4 NOT = 'XX'
333803*      IF PROD-COMB-CD-3-4 = 'XX'
333803*         SET NEW-BYPASS               TO TRUE
333803*         MOVE 'VALID GROUP NOT FOUND' TO WS-ERR-MESSAGE
333803*      ELSE
333803         SET PKG-FOUND     TO TRUE
               MOVE PROD-COMB-CD-3-4 TO TAG2(1:2)
               MOVE SPACES TO TAG2(3:4)
               MOVE SPACES TO RULE3-VFC
               MOVE ZEROS TO WS-ED-STATUS
               MOVE 4 TO WS-ED-FUNC-INDEX
               MOVE ZEROS TO WS-ED-CODE2
               CALL CTPCVFC2 USING TAG2
                                   WS-ED-CODE2
                                   WS-ED-FUNC-INDEX
                                   WS-ED-STATUS
      *
               IF WS-ED-STATUS = ZEROS
                MOVE 5 TO WS-ED-FUNC-INDEX
                CALL CTPCVFC2 USING RULE3-VFC
                                    WS-ED-CODE2
                                    WS-ED-FUNC-INDEX
                                    WS-ED-STATUS
               END-IF
             END-IF
           ELSE
333803       IF PROD-COMB-CD-1-2 NOT = 'XX'
333803*      IF PROD-COMB-CD-1-2 = 'XX'
333803*         SET NEW-BYPASS               TO TRUE
333803*         MOVE 'VALID GROUP NOT FOUND' TO WS-ERR-MESSAGE
333803*      ELSE
333803         SET PKG-FOUND     TO TRUE
               MOVE PROD-COMB-CD-1-2 TO TAG2(1:2)
               MOVE SPACES TO TAG2(3:4)
               MOVE SPACES TO RULE3-VFC
               MOVE ZEROS TO WS-ED-STATUS
               MOVE 4 TO WS-ED-FUNC-INDEX
               MOVE ZEROS TO WS-ED-CODE2
               CALL CTPCVFC1 USING TAG2
                                   WS-ED-CODE2
                                   WS-ED-FUNC-INDEX
                                   WS-ED-STATUS
      *
               IF WS-ED-STATUS = ZEROS
                MOVE 5 TO WS-ED-FUNC-INDEX
                CALL CTPCVFC1 USING RULE3-VFC
                                    WS-ED-CODE2
                                    WS-ED-FUNC-INDEX
                                    WS-ED-STATUS
               END-IF
             END-IF
           END-IF.
      *
333803*    IF NOT NEW-BYPASS
      *
       5010-WRITE-ERROR-RECORD.
      *
           INITIALIZE ERPT-ID
                      WS-AMT-APP-INFT-MAX
PROD                  WS-AMT-TOT
                      WS-AMT-APP-PLAN-MAX
                      WS-INTER-DED-AMT
                      WS-INTER-AMT-APP-OOP
                      WS-AMT-PAID
                      WS-AMT
                      ERPT-DED-AMT
PROD                  ERPT-INFERT-AMT
PROD                  ERPT-LTM-AMT
                      ERPT-COIN-AMT
PROD                  ERPT-COPAY-AMT
                      ERPT-TOT-AMT
           IF S9-IN-MBR-HCID (1:3) = SPACES
              MOVE S9-IN-MBR-HCID (4:9) TO
                                       ERPT-ID(1:9)
           ELSE
              MOVE S9-IN-MBR-HCID (1:9) TO
                                       ERPT-ID(1:9)
           END-IF
           MOVE  S9-IN-MBR-SEQ-NBR(1:2)  TO  ERPT-DEP
           MOVE  S9-IN-CVRG-PLAN-ID(1:9) TO ERPT-GROUP
           MOVE  S9-IN-PAT-FRST-NM TO ERPT-FNAME
           MOVE  S9-IN-PAT-LST-NM TO ERPT-LNAME
           MOVE  SPACES               TO ERPT-SERV-IND
           MOVE  WS-IN-NTWK-CD    TO ERPT-NW-IND
           MOVE  S9-IN-SRVC-STRT-DT TO ERPT-DOS
PROD       IF WS-ZERO-ERROR-SW = 'Y'
PROD          MOVE ZEROES                  TO ERPT-DED-AMT
PROD          MOVE ZEROES                  TO ERPT-INFERT-AMT
PROD          MOVE ZEROES                  TO ERPT-COIN-AMT
PROD          MOVE ZEROES                  TO ERPT-LTM-AMT
PROD          MOVE ZEROES                  TO ERPT-COPAY-AMT
PROD          MOVE ZEROES                  TO ERPT-TOT-AMT
PROD       ELSE
PROD          IF S9-INPUT-D-DED-MEM = ZEROES
PROD             MOVE ZEROES                  TO ERPT-DED-AMT
PROD          ELSE
PROD             MOVE S9-INPUT-D-DED-MEM(5:8) TO  WS-INTER-DED-AMT(1:8)
PROD             MOVE S9-INPUT-D-DED-MEM(13:2) TO WS-INTER-DED-AMT(9:2)
PROD             MOVE WS-INTER-DED-AMT  TO  ERPT-DED-AMT
                 IF S9-INPUT-D-DED-MEM > ZERO
                  MOVE '+' TO ERPT-DED-AMT(1:1)
                 ELSE
                  MOVE '-' TO ERPT-DED-AMT(1:1)
                 END-IF
PROD          END-IF
PROD          IF S9-INPUT-D-OOP-MEM = ZEROES
PROD             MOVE ZEROES                  TO ERPT-COIN-AMT
PROD          ELSE
PROD             MOVE S9-INPUT-D-OOP-MEM(5:8)
PROD                                      TO  WS-INTER-AMT-APP-OOP(1:8)
PROD             MOVE S9-INPUT-D-OOP-MEM(13:2)
PROD                                       TO WS-INTER-AMT-APP-OOP(9:2)
PROD             MOVE WS-INTER-AMT-APP-OOP TO ERPT-COIN-AMT
                 IF S9-INPUT-D-OOP-MEM > ZERO
                  MOVE '+' TO ERPT-COIN-AMT(1:1)
                 ELSE
                  MOVE '-' TO ERPT-COIN-AMT(1:1)
                 END-IF
PROD          END-IF
PROD          IF S9-INPUT-D-CAP-MEM = ZEROES
PROD             MOVE ZEROES                  TO ERPT-INFERT-AMT
PROD          ELSE
PROD             MOVE S9-INPUT-D-CAP-MEM(5:8)
PROD                                       TO  WS-AMT-APP-INFT-MAX(1:8)
PROD             MOVE S9-INPUT-D-CAP-MEM(13:2)
PROD                                       TO WS-AMT-APP-INFT-MAX(9:2)
PRDO             MOVE WS-AMT-APP-INFT-MAX TO ERPT-INFERT-AMT
                 IF S9-INPUT-D-CAP-MEM > ZERO
                  MOVE '+' TO ERPT-INFERT-AMT(1:1)
                 ELSE
                  MOVE '-' TO ERPT-INFERT-AMT(1:1)
                 END-IF
PROD          END-IF
      *       COMPUTE WS-AMT-PAID = WS-INTER-DED-AMT +
      *            WS-INTER-AMT-APP-OOP + WS-AMT-APP-INFT-MAX
              COMPUTE WS-AMT-PAID = S9-INPUT-D-DED-MEM+
                   S9-INPUT-D-OOP-MEM + S9-INPUT-D-CAP-MEM
              MOVE WS-AMT-PAID(7:6)     TO  WS-AMT(2:6)
              MOVE WS-AMT-PAID(12:2)     TO  WS-AMT(8:2)
              IF WS-AMT-PAID > ZERO
               MOVE '+' TO WS-AMT(1:1)
              ELSE
               MOVE '-' TO WS-AMT(1:1)
PROD          END-IF
PROD          MOVE WS-AMT               TO  WS-AMT-TOT
PROD          MOVE WS-AMT-PAID          TO ERPT-TOT-AMT
PROD          MOVE ZEROES               TO ERPT-LTM-AMT
PROD          MOVE ZEROES               TO ERPT-COPAY-AMT
           END-IF

      *
           PERFORM 4010-DEST-ID THRU 4010-EXIT
      *
           IF WRITE-ERROR-REC-NEW
155725        WRITE ODS-ZERO-ERR FROM WS-ERROR-DETAIL-LINE
155725        MOVE '00' TO S9-OUT-TOT-REC-STS(1)
155725        ADD 1     TO  WS-RESPONSE-ERR
155725        PERFORM 6010-ERROR-DES THRU 6010-EXIT
155725        WRITE RESPONSE-REC FROM S9-PARMLIST
           ELSE
              WRITE ODS-ERR-REC FROM WS-ERROR-DETAIL-LINE
1073          MOVE '94' TO S9-OUT-TOT-REC-STS(1)
1073          ADD 1     TO  WS-RESPONSE-ERR
              PERFORM 6010-ERROR-DES THRU 6010-EXIT
1073          WRITE RESPONSE-REC FROM S9-PARMLIST
           END-IF.

      *
       5010-EXIT.
           EXIT.
      *
       6010-ERROR-DES.
      *
             INITIALIZE S-ERROR-CODE
                        S-ERROR-DESC.
             MOVE ZEROS TO WS-ED-STATUS
             MOVE 4 TO WS-ED-FUNC-INDEX.
             MOVE S9-CALL-STATUS TO S-ERROR-CODE
             MOVE ZEROS TO WS-ED-CODE2
             CALL CTECALLS USING S-ERROR-CODE
                                 WS-ED-CODE2
                                 WS-ED-FUNC-INDEX
                                 WS-ED-STATUS.
      *
             IF WS-ED-STATUS = ZEROS
                MOVE 6 TO WS-ED-FUNC-INDEX
      *
                CALL CTECALLS USING S-ERROR-DESC
                                    WS-ED-CODE2
                                    WS-ED-FUNC-INDEX
                                    WS-ED-STATUS
                MOVE S-ERROR-DESC TO S9-ERROR-TXT
             END-IF.
      *
       6010-EXIT.
           EXIT.
      *
       5020-ABEND.
            CALL 'ZNCNCLDP'.
      *
       5020-EXIT.
           EXIT.
      *
      *
      *****************************************************************
      *                   END OF PROGRAM S9CODSRV                     *
      *****************************************************************
