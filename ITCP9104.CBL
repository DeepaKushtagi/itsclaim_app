000100 IDENTIFICATION DIVISION.                                         ITCP9104
000200                                                                  ITCP9104
000200 PROGRAM-ID. ITCP9104.                                            ITCP9104
000300 AUTHOR.  INTERPLAN TELEPROCESSING SERVICES TEAM.                 ITCP9104
000400 DATE-COMPILED.                                                   ITCP9104
000500 DATE-WRITTEN.  FEBRUARY 1988.                                    ITCP9104
000700 TITLE 'SF BATCH PROCESS I/O MODULE'.                             ITCP9104
000800                                                                  ITCP9104
001400******************************************************************ITCP9104
001500*                                                                *ITCP9104
001600*             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *ITCP9104
001700*                                                                *ITCP9104
001800*   MODULE:       ITCP9104                                       *ITCP9104
001900*   NAME:         BATCH PROCESS I/O MODULE                       *ITCP9104
001500*   VERSION:      11.4                                           *00001500
001600*   DATE:         MAY 15, 2014                                   *00001600
002200*                                                                *ITCP9104
002300*   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *ITCP9104
002400*   TYPE:         COMMON                                         *ITCP9104
002500*   FUNCTION:     THIS MODULE IS THE BATCH PROCESS I/O MODULE    *ITCP9104
00021 *                 AND IS CALLED BY MODULE 'ITCP9102'.            *
002600*                                                                *ITCP9104
002700*   MODIFICATION                                                 *ITCP9104
002700*        HISTORY: 11/18/96 10.0 RELEASE 8.0 INITIAL VS COBOL II  *ITCP9104
003500*                                                                *ITCP9104
002900*                 10/16/98 10.1 TECH MEMO ITS-TM-98-10 CHANGES:  *ITCP9104
003000*                          - STANDARDIZE 3600 PROJECT            *ITCP9104
003100*                          - ALLOW PROCESS TO CONTINUE TO EOF    *ITCP9104
003200*                            WHEN THE FOLLOWING OCCURS:          *ITCP9104
003300*                              *INVALID TRANS ID ON 05/A5 RECORD *ITCP9104
003400*                              *INVALID SF RECORD TYPE           *ITCP9104
003500*                              *DUPLICATE RECORD SEQUENCE NUMBER *ITCP9104
003600*                              *FILE SEQUENCE ERROR              *ITCP9104
003700*                              *MISSING RECORD X0 OR 90          *ITCP9104
003800*                          - MOVED ALL PARAGRAPH NAME FIELDS     *ITCP9104
003900*                            UNDER WS-PARAGRAPH-NM AS 88 LEVELS. *ITCP9104
004000*                          - RESTRUCTURED WORKING STORAGE AND    *ITCP9104
004100*                            REMOVED UNUSED DATA FIELDS.         *ITCP9104
004200*                        WORKING-STORAGE AFFECTED:               *ITCP9104
004300*                            WS-DUMMY-FLD                  (DEL) *ITCP9104
004400*                            WS-VERSION-NUMBER                   *ITCP9104
004500*                            WS-RELEASE-DATE                     *ITCP9104
004600*                            WS-RELEASE-DISPLAY                  *ITCP9104
004700*                            WS-PARAGRAPH-NAME-AREA        (DEL) *ITCP9104
004800*                            WORK-DATE-1                   (DEL) *ITCP9104
004900*                            WS-HOLD-SEQ-NUM               (DEL) *ITCP9104
005000*                            WS-CURRENT-SEQ-NUM            (DEL) *ITCP9104
005100*                            WS-PARAGRAPH-NM                     *ITCP9104
005200*                            WS-3                          (DEL) *ITCP9104
005300*                            WS-4                          (DEL) *ITCP9104
005400*                            WS-SEQUENCE-IND               (DEL) *ITCP9104
005500*                            WS-SAVE-RESUBMISSION-IND   (RENAME) *ITCP9104
005600*                            WS-TYPE-SUBM             (NEW NAME) *ITCP9104
005700*                            INVALID-RECORD-ERROR-MSG      (DEL) *ITCP9104
005800*                            DUPLICATE-RECORD-ERROR-MSG    (DEL) *ITCP9104
005900*                            SEQUENCE-RANGE-ERROR-MSG      (DEL) *ITCP9104
006000*                            ERROR-CLAIM-COUNT-ERROR-MSG   (DEL) *ITCP9104
006100*                            ERROR-RECORD-COUNT-ERROR-MSG  (DEL) *ITCP9104
006200*                            MISSING-CLAIM-TRAILER-MSG     (DEL) *ITCP9104
006300*                            WS-MISSING-HEADER-ERR-MSG           *ITCP9104
006400*                            BAD-DATA-ERROR-MSG            (DEL) *ITCP9104
006500*                            WS-OUT-OF-BALANCE-MSG               *ITCP9104
006600*                            WS-TRLR-ONLY-MSG                    *ITCP9104
006700*                            WS-EMPTY-FILE-ERR-MSG               *ITCP9104
006800*                            WS-TBL-OVERFLOW-ERR-MSG       (NEW) *ITCP9104
006900*                            WS-ERR-CD                           *ITCP9104
007000*                            ITCWSRR COPYLIB                     *ITCP9104
007100*                            ITCWSLR COPYLIB                     *ITCP9104
007200*                            ITCWSCC-AREA              (RENAMED) *ITCP9104
007300*                            WS-CONDITION-CODES       (NEW NAME) *ITCP9104
007400*                            REC-SUB                       (NEW) *ITCP9104
007500*                            WS-HOLD-CLAIM                 (NEW) *ITCP9104
007600*                            WS-HOLD-SCCF-INPUT            (NEW) *ITCP9104
007700*                        LINKAGE SECTION AFFECTED:               *ITCP9104
007800*                            ITCIABP COPYLIB                     *ITCP9104
007900*                        PARAGRAPHS AFFECTED:                    *ITCP9104
008000*                            0000-MAINLINE THRU                  *ITCP9104
008100*                            9300-CHECK-SPEC-NOTATION            *ITCP9104
008200*                            X100-ITCPDSB-SEQUENCE-CHECK         *ITCP9104
008300*                            X210-SEQ-RANGE-CHECK                *ITCP9104
004600*                                                                *ITCP9104
008500*                 02/11/00 - 10.2  TECH MEMO ITS-TM-00-02-R8.1:  *ITCP9104
008600*                          COMPLETED BY:  MICHAEL HISE           *ITCP9104
008700*                          - ADDED NEW VALID ELECTRONIC CLAIMS   *ITCP9104
008800*                            ROUTING FILE (DD CARD ITCDDO10).    *ITCP9104
008900*                          - ACCUMULATED ECR COUNTS FOUND IN     *ITCP9104
009000*                            LS-BATCH-PROCESS (COPYLIB ITCIABP). *ITCP9104
009100*                        ENVIRONMENT DIVISION AFFECTED:          *ITCP9104
009200*                          ADDED:                                *ITCP9104
009300*                            ASSIGNED VALID-ECR-PRT TO ITCDDO06  *ITCP9104
009400*                        DATA DIVISION AFFECTED:                 *ITCP9104
009500*                          ADDED:                                *ITCP9104
009600*                            DEFINED VALID-ECR-FILE WITH DATA    *ITCP9104
009700*                            RECORD VALID-ECR-FILE-REC           *ITCP9104
009800*                        WORKING-STORAGE AFFECTED:               *ITCP9104
009900*                          MODIFIED:                             *ITCP9104
010000*                            WS-VERSION-NUMBER                   *ITCP9104
010100*                            WS-RELEASE-DATE                     *ITCP9104
010200*                            WS-RELEASE-DISPLAY                  *ITCP9104
010300*                          ADDED:                                *ITCP9104
010400*                            WS-ECR-IND                          *ITCP9104
010500*                        LINKAGE SECTION AFFECTED:               *ITCP9104
010600*                          MODIFIED:                             *ITCP9104
010700*                            LS-BATCH-PROCESS                    *ITCP9104
010800*                            (COPY MEMBER ITCIABP)               *ITCP9104
010900*                        PROCEDURE DIVISION AFFECTED:            *ITCP9104
011000*                          MODIFIED:                             *ITCP9104
011100*                            1000-INITIAL-RTN                    *ITCP9104
011200*                            3100-END-OF-FILE-PROCESSING         *ITCP9104
011300*                            4100-WRITE-CLAIMS                   *ITCP9104
011400*                            5000-CLOSE-RTN                      *ITCP9104
011500*                          ADDED:                                *ITCP9104
011600*                            4160-WRITE-VALID-ECR-FILE           *ITCP9104
011700*                                                                *ITCP9104
00118 *                 08/03/01 - 10.3  TECH MEMO ITS-TM-01-08-R8.4:  *   CL*13
00119 *                          COMPLETED BY:  B. PEPPERS             *   CL**2
00120 *                          - HIPAA                               *   CL**2
00121 *                          COMPLETED BY: D. LEE                  *   CL*15
00122 *                          - OPL                                 *   CL*15
00122 *                                                                *00123
00122 *                  06/17/05 - 10.4 DEAD CODE REVIEW  REL 9.2     *00124
00122 *                                                                *00125
      *                  11/16/07 - 10.5 REL 10.1 - 999 LINE EXPANSION *00125
      *                                                                *00012700
      *                  02/15/08 - 10.6 REL 10.1.1 PERF ENHANCEMENTS  *00012800
      *                  03/21/08 - 10.7 REL 10.1.1 PATCH FOR TD_35981 *00012810
      *                             FIXED LOGIC TO SET NUMBER OF E6/46 *00012900
      *                             RECORDS - E.LITOVSKY               *00012910
      *                  06/27/08 - 10.8 REL 10.2.1SA                  *00012911
      *                             C.CRITSER    ITSIASC COPYBOOK      *00012912
00118 *               09/19/08 - 10.9 TECH MEMO ITS-TM-09-08-R10.2.1SA *00012913
      *                          C.CRITSER   COMBINE 03/21/08 WITH     *00012914
      *                             06/27/08 RELEASE                   *00012915
00118 * 11/12/09 11.0 RELEASE 11.0                         EXL         *
      *    MERGE RT/MLE WITH CORE SOFTWARE                             *
013600* REL   - DATE       - VERS - CONTROL  - DESCRIPTION             *00012920
013700*R11.1  ¦ 05/13/2010 ¦ 11.1 ¦ TD_57006 ¦ EXL: ADD SF020 AND SF021*00012920
013800*       TO WS-INVLD-RELSE-ERR-CD, CHANGED INITIALIZATION OF SF   *00012920
013900*       AREA                                                     *00012920
013910*R11.4  ¦ 11/10/2011 ¦ 11.2 ¦ E_38714  ¦ DXL: ADD IXR56/PXR56 TO *00012920
013920*       WS-INVLD-RELSE-ERR-CD                                    *00012920
013930*R12.0  ¦ 05/17/2012 ¦ 11.3 ¦ E_19137  ¦ DEM: CHANGES RELATED TO *
013940*       ICD-10/HIPAA PROJECT, ALSO CHANGES INCLUDED FOR THE      *
013950*       FOLLOWING ENHANCEMENTS E_59631, E_49746, E_61141.        *
013960*       CHANGE SIZE OF FILLER FROM 1023315 TO 1026120.           *
013970*       CHANGE WS-HOLD-CLAIM FROM 4018 TO 4024.                  *
013980*       SUBSTITUTE HARD-CODED STACK VALUES WITH LITERALS.        *
013990*       POPULATE NEW RECORD TYPES IN STACK (33, 42-45, 47, 71-75,*
013991*                                           D3, E2).             *00012920
015000*R14.5  ¦ 05/15/2014 ¦ 11.4 ¦ E_34072  ¦ EXL: WRITE MISMATCH ADJ *00012920
015100*       TO A NEW FILE                                            *00012920
015200*R14.5  ¦ 05/15/2014 ¦ 11.4 ¦ PD_57538 ¦ SKG: STOP PROCESS FOR   *00760000
015300*       ABNORMAL FILE STATUS.                                    *00760000
      *                                                                *00125
008500*   NOTES:         NONE                                          *ITCP9104
008600*                                                                *ITCP9104
008700******************************************************************ITCP9104
004700/                                                                 ITCP9104
004800 ENVIRONMENT DIVISION.                                            ITCP9104
004900 CONFIGURATION SECTION.                                           ITCP9104
005000 INPUT-OUTPUT SECTION.                                            ITCP9104
005100                                                                  ITCP9104
005200 FILE-CONTROL.                                                    ITCP9104
016400                                                                  00013900
016500*R14.5 PD_57538 SKG START - ADDED FILE STATUS TO ALL FILES        44520000
016600     SELECT SCCF-INPUT-FILE        ASSIGN TO ITCDDI01             00014000
016700                        FILE STATUS  IS ITCDDI01-STAT.
005300                                                                  ITCP9104
016900     SELECT VALID-FILE             ASSIGN TO ITCDDO01             00014200
017000                        FILE STATUS  IS ITCDDO01-STAT.
009600                                                                  ITCP9104
017200     SELECT INVALID-FILE           ASSIGN TO ITCDDO02             00014400
017300                        FILE STATUS  IS ITCDDO02-STAT.
009800                                                                  ITCP9104
017500     SELECT REJECT-FILE            ASSIGN TO ITCDDO03             00014600
017600                        FILE STATUS  IS ITCDDO03-STAT.
010000                                                                  ITCP9104
017800     SELECT RELEASE-FILE           ASSIGN TO ITCDDO07             00014800
017900                        FILE STATUS  IS ITCDDO07-STAT.
010200                                                                  ITCP9104
018100     SELECT VALID-ADJ-FILE         ASSIGN TO ITCDDO08             00015000
018200                        FILE STATUS  IS ITCDDO08-STAT.
010400                                                                  ITCP9104
018400     SELECT VALID-ECR-FILE         ASSIGN TO ITCDDO10             00015200
018500                        FILE STATUS  IS ITCDDO10-STAT.
018600*R14.5 PD_57538 SKG END                                           44520000
014000                                                                  ITCP9104
018611*R14.5 E_34072 DSL ADD NEW FILE                                   44520000
018620     SELECT MISMATCH-FILE          ASSIGN TO ITCDDO11             00015200
018630                        FILE STATUS  IS ITCDDO11-STAT.
010600/                                                                 ITCP9104
006100 DATA DIVISION.                                                   ITCP9104
006200 FILE SECTION.                                                    ITCP9104
006300                                                                  ITCP9104
006400 FD  SCCF-INPUT-FILE                                              ITCP9104
006500     RECORDING MODE IS F                                          ITCP9104
006600     LABEL RECORDS ARE STANDARD                                   ITCP9104
006700     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
011400     DATA RECORD IS FS-SCCF-INPUT-RECORD.                         ITCP9104
011500 01  FS-SCCF-INPUT-RECORD            PIC X(255).                  ITCP9104
007000                                                                  ITCP9104
007100 FD  VALID-FILE                                                   ITCP9104
007200     RECORDING MODE IS F                                          ITCP9104
007300     LABEL RECORDS ARE STANDARD                                   ITCP9104
007400     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
007500     DATA RECORD IS VALID-RECORD.                                 ITCP9104
007600                                                                  ITCP9104
012300 01  VALID-FILE-RECORD               PIC X(255).                  ITCP9104
007800                                                                  ITCP9104
007900 FD  INVALID-FILE                                                 ITCP9104
008000     RECORDING MODE IS F                                          ITCP9104
008100     LABEL RECORDS ARE STANDARD                                   ITCP9104
008200     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
008300     DATA RECORD IS INVALID-FILE-RECORD.                          ITCP9104
008400                                                                  ITCP9104
013100 01  INVALID-FILE-RECORD             PIC X(255).                  ITCP9104
008600                                                                  ITCP9104
008700 FD  REJECT-FILE                                                  ITCP9104
008800     RECORDING MODE IS F                                          ITCP9104
008900     LABEL RECORDS ARE STANDARD                                   ITCP9104
009000     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
009100     DATA RECORD IS REJECT-RECORD.                                ITCP9104
009200                                                                  ITCP9104
013900 01  REJECT-FILE-RECORD              PIC X(255).                  ITCP9104
009400                                                                  ITCP9104
009500 FD  RELEASE-FILE                                                 ITCP9104
009600     RECORDING MODE IS F                                          ITCP9104
009700     LABEL RECORDS ARE STANDARD                                   ITCP9104
009800     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
009900     DATA RECORD IS RELEASE-RECORD.                               ITCP9104
010000                                                                  ITCP9104
014700 01  RELEASE-FILE-RECORD             PIC X(255).                  ITCP9104
010200                                                                  ITCP9104
010400 FD  VALID-ADJ-FILE                                               ITCP9104
010500     RECORDING MODE IS F                                          ITCP9104
010600     LABEL RECORDS ARE STANDARD                                   ITCP9104
010700     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
010800     DATA RECORD IS VALID-ADJ-RECORD.                             ITCP9104
010900                                                                  ITCP9104
015500 01  VALID-ADJ-FILE-RECORD           PIC X(255).                  ITCP9104
019200                                                                  ITCP9104
019300 FD  VALID-ECR-FILE                                               ITCP9104
019400     RECORDING MODE IS F                                          ITCP9104
019500     LABEL RECORDS ARE STANDARD                                   ITCP9104
019600     BLOCK CONTAINS 0 RECORDS                                     ITCP9104
019700     DATA RECORD IS VALID-ECR-RECORD.                             ITCP9104
019800                                                                  ITCP9104
019900 01  VALID-ECR-FILE-RECORD           PIC X(255).                  ITCP9104
024401                                                                  00020300
024402*R14.5 E_34072 DSL BEGIN                                          00020300
024410 FD  MISMATCH-FILE                                                00020400
024420     RECORDING MODE IS F                                          00020500
024430     LABEL RECORDS ARE STANDARD                                   00020600
024440     BLOCK CONTAINS 0 RECORDS                                     00020700
024450     DATA RECORD IS MISMATCH-RECORD.                              00020800
024460                                                                  00020900
024470 01  MISMATCH-FILE-RECORD            PIC X(255).                  00021000
024480*R14.5 E_34072 DSL END                                            00020300
011100/                                                                 ITCP9104
011200 WORKING-STORAGE SECTION.                                         ITCP9104
011300                                                                  ITCP9104
015900 01  WS-START-LIT                    PIC X(60)  VALUE             ITCP9104
011600     'ITCP9104-W O R K I N G  S T O R A G E STARTS HERE *******'. ITCP9104
011700                                                                  ITCP9104
016200 01  WS-MODULE-ID                    PIC X(8)   VALUE 'ITCP9104'. ITCP9104
025200 01  WS-VERSION-NUMBER               PIC X(4)   VALUE '11.4'.     00021900
025300 01  WS-RELEASE-DATE                 PIC X(8)   VALUE '05/15/14'. 00022000
012100                                                                  ITCP9104
012500 01  WS-AREA-PERTAINING-TO-RUN.                                   ITCP9104
012700     05  WS-UPDATE-DISPLAY.                                       ITCP9104
016800        10  WS-PROG-NAME             PIC X(9)   VALUE 'ITCP9104'. ITCP9104
016900        10  WS-COMMON-DISPLAY        PIC X(19)  VALUE             ITCP9104
017000            '-BATCH PROCESS I-O'.                                 ITCP9104
017100        10  WS-MODULE-DISPLAY        PIC X(8)   VALUE ' MODULE '. ITCP9104
017200        10  WS-RELEASE-DISPLAY       PIC X(34)  VALUE             ITCP9104
026200            'VERSION #11.4 RELEASED ON 05/15/14'.                 00022900
012800                                                                  ITCP9104
017500 01  WS-BINARY-FIELDS    COMP.                                    ITCP9104
00228      05  SUB1                        PIC S9(04) VALUE +0.         00023400
00228      05  SUBR                        PIC S9(04) VALUE +0.         00023410
017800     05  WS-ERR-SUB                  PIC S9(02) VALUE +0.         ITCP9104
00232      05  REC-SUB                     PIC S9(04) VALUE +0.         00023800
           05  WS-SUBR                     PIC S9(04) VALUE +0.         00024100
           05  PREV-CIABP-F0-CTR           PIC S9(04) VALUE +0.         00024200
           05  PREV-CIABP-F1-CTR           PIC S9(04) VALUE +0.         00024300
           05  PREV-CIABP-E6-CTR           PIC S9(04) VALUE +0.         00024400
           05  PREV-CIABP-60-CTR           PIC S9(04) VALUE +0.         00024500
           05  PREV-CIABP-46-CTR           PIC S9(04) VALUE +0.         00024600

018100 01  WS-WORK-FIELDS.                                              ITCP9104
           05  PREV-CIABP-CLM-TYPE         PIC X(2).                    00025400
018200     05  WS-TYPE-SUBM                PIC X(1) VALUE SPACES.       ITCP9104
018300         88 WS-RESUBMISSION-CLM                 VALUE '3' '4'.    ITCP9104
022800     05  WS-ECR-IND                  PIC X(1) VALUE SPACES.       ITCP9104
022900         88 WS-ECR-CLM                          VALUE 'Y'.        ITCP9104
018400     05  WS-TRAILER-TRANS-ID         PIC X(02)  VALUE ' 9'.       ITCP9104
026110*R11.4 E_38714 DSL ADD IXR56/PXR56
018500     05  WS-ERR-CD                   PIC X(05).                   ITCP9104
018600            88  WS-INVLD-RELSE-ERR-CD           VALUE             ITCP9104
018700                'II018' 'IR007' 'IR021' 'IS001' 'IS042' 'IT040'   ITCP9104
018800                'IT041' 'IT059' 'IXR01' 'IXR02' 'IXR21' 'IXR22'   ITCP9104
026600                'IXR50' 'IXR51' 'IXR52' 'IXR53' 'IXR54' 'IXR56'   00026900
026700                'PR021' 'PS001' 'PS042' 'PT040' 'PT041' 'PR007'   00027000
00247                 'PT059' 'PXR01' 'PXR02' 'PXR03' 'PXR21' 'PXR22'      CL**7
026900                'PXR50' 'PXR51' 'PXR52' 'PXR53' 'PXR54' 'PXR56'   00027200
027000                'SF020' 'SF021'.
019300     05  WS-ERR-REC-AND-SEQ-NUM.                                  ITCP9104
019400         10  WS-ERR-REC-TYPE         PIC X(02)  VALUE SPACES.     ITCP9104
00251          10  WS-ERR-SEQ-NUM          PIC S9(04) COMP              00253
                                                      VALUE 0.
019600     05  WS-ERR-LOC-PLAN-CNTL-NUM    PIC X(17)  VALUE SPACES.     ITCP9104
019700     05  WS-ERR-SCCF-SER-NUM         PIC X(17)  VALUE SPACES.     ITCP9104
019800     05  WS-PARAGRAPH-NM             PIC X(30)  VALUE SPACES.     ITCP9104
019900        88  WS-0000-PARAGRAPH                   VALUE             ITCP9104
020000            '0000-MAINLINE                 '.                     ITCP9104
020100        88  WS-3100-PARAGRAPH                   VALUE             ITCP9104
020200            '3100-END-OF-FILE-PROCESSING   '.                     ITCP9104
020300        88  WS-3200-PARAGRAPH                   VALUE             ITCP9104
020400            '3200-MISSING-05-OR-A5         '.                     ITCP9104
020500        88  WS-3800-PARAGRAPH                   VALUE             ITCP9104
020600            '3800-STACK-CLAIMS             '.                     ITCP9104
020700        88  WS-3810-PARAGRAPH                   VALUE             ITCP9104
020800            '3810-TABLE-OVERFLOW           '.                     ITCP9104
020900        88  WS-4000-PARAGRAPH                   VALUE             ITCP9104
021000            '4000-DETERMINE-SUB-FUNCTION-CD'.                     ITCP9104
021100        88  WS-X100-PARAGRAPH                   VALUE             ITCP9104
021200            'X100-ITCPDSB-SEQUENCE-CHECK   '.                     ITCP9104
021300        88  WS-X200-PARAGRAPH                   VALUE             ITCP9104
021400            'X200-SINGLE-MULTI-CHECK       '.                     ITCP9104
013700                                                                  ITCP9104
021600 01  WS-INDICATORS.                                               ITCP9104
021700     05  WS-INV-REL-IND              PIC X(01)  VALUE SPACE.      ITCP9104
021800         88  WS-RELSE-ERROR-FOUND               VALUE 'Y'.        ITCP9104
021900     05  WS-NOTATION-REC-IND         PIC X(01)  VALUE SPACE.      ITCP9104
022000         88  WS-BYPASS-NOTATION-RECORD          VALUE 'Y'.        ITCP9104
022100     05  WS-TRAILER-IND              PIC X(01).                   ITCP9104
022200         88  TRAILER-ONLY                       VALUE 'T'.        ITCP9104
022300         88  TRAILER-OUT-OF-BALANCE             VALUE 'O'.        ITCP9104
022400         88  TRAILER-BAD-DATA                   VALUE 'B'.        ITCP9104
022500     05  WS-DISPLAY-MSG-IND          PIC X(01)  VALUE SPACE.      ITCP9104
00282          88 WS-INVALID-FUNCTION                 VALUE 'I'.
00283          88 WS-MISSING-TRAILER                  VALUE 'M'.
00284          88 WS-MISSING-CLM-HDR                  VALUE 'H'.
022900         88 WS-TABLE-OVERFLOW                   VALUE '1'.        ITCP9104
023000         88 WS-FILE-SEQUENCE                    VALUE '2'.        ITCP9104
00287          88 WS-EMPTY-FILE                       VALUE 'E'.
033300/                                                                 00031300
033400*R14.5 PD_57538 SKG START                                         44520000
033500 01  WS-FILES-STATUS.
033600     05 ITCDDI01-STAT                PIC X(02) VALUE SPACES.
033700     05 ITCDDO01-STAT                PIC X(02) VALUE SPACES.
033800     05 ITCDDO02-STAT                PIC X(02) VALUE SPACES.
033900     05 ITCDDO03-STAT                PIC X(02) VALUE SPACES.
034000     05 ITCDDO07-STAT                PIC X(02) VALUE SPACES.
034100     05 ITCDDO08-STAT                PIC X(02) VALUE SPACES.
034200     05 ITCDDO10-STAT                PIC X(02) VALUE SPACES.
034210     05 ITCDDO11-STAT                PIC X(02) VALUE SPACES.
034300     05 WS-FILE-IND                  PIC X(01)  VALUE SPACE.      00030600
034400         88 WS-VALID-FILE-I                     VALUE '1'.        00030700
034500         88 WS-INVALID-FILE-I                   VALUE '2'.        00030800
034600         88 WS-REJECT-FILE-I                    VALUE '3'.        00030900
034700         88 WS-RELEASE-FILE-I                   VALUE '4'.        00031000
034800         88 WS-VALID-ADJ-FILE-I                 VALUE '5'.        00031100
034900         88 WS-VALID-ECR-FILE-I                 VALUE '6'.        00031200
034910         88 WS-MIS-MATCH-FILE-I                 VALUE '7'.        00031200
035000*R14.5 PD_57538 SKG END                                           44520000
015200/                                                                 ITCP9104
023300 01  WS-ERROR-MESSAGES.                                           ITCP9104
023400     05  WS-ASTERICKS                PIC X(76)  VALUE ALL '*'.    ITCP9104
023500     05  WS-FILE-SEQUENCE-ERR-MSG.                                ITCP9104
023600         10  FILLER                  PIC X(42)  VALUE             ITCP9104
023700             'FILE SEQUENCE ERROR DETECTED - JOB ABORTED'.        ITCP9104
023800     05  WS-INVALID-FUNCTION-ERR-MSG.                             ITCP9104
023900         10  FILLER                  PIC X(30)  VALUE             ITCP9104
024000             'INVALID FUNCTION - JOB ABORTED'.                    ITCP9104
024100     05  WS-MISSING-FILE-TRLR-MSG.                                ITCP9104
024200         10  FILLER                  PIC X(44)  VALUE             ITCP9104
024300             'MISSING 9D FILE TRAILER RECORD - JOB ABORTED'.      ITCP9104
024400     05  WS-MISSING-HEADER-ERR-MSG.                               ITCP9104
024500         10  FILLER                  PIC X(35)  VALUE             ITCP9104
024600             'HEADER RECORD MISSING - JOB ABORTED'.               ITCP9104
024700     05  WS-OUT-OF-BALANCE-MSG.                                   ITCP9104
024800         10  FILLER                  PIC X(29)  VALUE             ITCP9104
024900             'TRAILER RECORD OUT OF BALANCE'.                     ITCP9104
025000     05  WS-TRLR-ONLY-MSG.                                        ITCP9104
025100         10  FILLER                  PIC X(19)  VALUE             ITCP9104
025200             'TRAILER RECORD ONLY'.                               ITCP9104
025300     05  WS-EMPTY-FILE-ERR-MSG.                                   ITCP9104
025400         10  FILLER                  PIC X(32)  VALUE             ITCP9104
025500             'INPUT FILE MISSING - JOB ABORTED'.                  ITCP9104
025600     05  WS-TBL-OVERFLOW-ERR-MSG.                                 ITCP9104
025700         10  FILLER                  PIC X(34)  VALUE             ITCP9104
025800             'TABLE OVERFLOW ON WS-HOLD-RECORD, '.                ITCP9104
               10  FILLER                  PIC X(39)  VALUE             00317
                   'CLAIM RECORDS EXCEED 4018 - JOB ABORTED'.           00319
015400                                                                  ITCP9104
026200 01  WS-HOLD-CLAIM.                                               ITCP9104
034110* R12.0 E_19137 DEM START
034200     05  WS-HOLD-RECORD  OCCURS 4024 TIMES.                       00034700
034210* R12.0 E_19137 END
026400         10 WS-HOLD-COMMON-KEY.                                   ITCP9104
026500             15 FILLER               PIC X(17).                   ITCP9104
026600             15 WS-HOLD-TRANS-ID     PIC X(02).                   ITCP9104
026700                 88 WS-HOLD-TRANS-ID-VALID      VALUE ' 1'.       ITCP9104
026800             15 WS-HOLD-TRANS-QUAL   PIC X(02).                   ITCP9104
026900             15 WS-HOLD-RECORD-SEQ.                               ITCP9104
027000                20 WS-HOLD-REC-TYPE  PIC X(02).                   ITCP9104
                      20 WS-HOLD-SEQ-NUM   PIC S9(04) COMP.             00333
027200         10 WS-HOLD-CLAIM-DATA       PIC X(230).                  ITCP9104
016000                                                                  ITCP9104
027400 01  WS-HOLD-SCCF-INPUT.                                          ITCP9104
027500     05  WS-HOLD-SCCF-INPUT-RECORD.                               ITCP9104
027600         10 WS-HOLD-SCCF-INPUT-KEY.                               ITCP9104
027700             15 WS-HOLD-SCCF-SERNUM  PIC X(17).                   ITCP9104
027800             15 WS-HOLD-SCCF-TRANSID PIC X(02).                   ITCP9104
027900                 88 WS-HOLD-SCCF-TRANSID-VALID  VALUE ' 1'.       ITCP9104
028000             15 WS-HOLD-SCCF-TRANQUAL                             ITCP9104
028100                                     PIC X(02).                   ITCP9104
028200             15 WS-HOLD-SCCF-REC-SEQ.                             ITCP9104
028300                20 WS-HOLD-SCCF-RECTYPE                           ITCP9104
028400                                     PIC X(02).                   ITCP9104
028500                   88 WS-HOLD-SCCF-CLAIM-HDR    VALUE '05' 'A5'.  ITCP9104
028600                20 WS-HOLD-SCCF-SEQNUM                            ITCP9104
                                           PIC S9(04) COMP.             00349
028800         10 WS-HOLD-SCCF-CLAIM-DATA  PIC X(230).                  ITCP9104
026200/                                                                 ITCP9104
026300 01  ITCFM146-RECORD.                                             ITCP9104
026400     COPY ITCFM146.                                               ITCP9104
026500/                                                                 ITCP9104
026600 01  ITCFM1E6-RECORD.                                             ITCP9104
026700     COPY ITCFM1E6.                                               ITCP9104
029500/                                                                 ITCP9104
029600 01  SCCF-INPUT-RECORD               PIC X(255).                  ITCP9104
029700                                                                  ITCP9104
029800     COPY ITCWSRR.                                                ITCP9104
029900/                                                                 ITCP9104
030000     COPY ITCWSLR.                                                ITCP9104
026900/                                                                 ITCP9104
027000 01  FILE-TRAILER-RECORD.                                         ITCP9104
027100     COPY ITCDB99D.                                               ITCP9104
027200/                                                                 ITCP9104
030500 01  WS-CONDITION-CODES.                                          ITCP9104
027400     COPY ITCWSCC.                                                ITCP9104
027800                                                                  ITCP9104
038700* R12.0 E_19137 DEM START
038800     COPY ITCSFCNT.                                               00039100
038801* R12.0 E_19137 END
030800 01  WS-END-LIT                      PIC X(60)  VALUE             ITCP9104
028000     'ITCP9104-W O R K I N G  S T O R A G E ENDS **************'. ITCP9104
028100/                                                                 ITCP9104
028200 LINKAGE SECTION.                                                 ITCP9104
028300                                                                  ITCP9104
00374  01  WS-ITCIAPC.
028500     COPY ITSIAPC.                                                ITCP9104
031500/                                                                 ITCP9104
00377  01  WS-ITCIABP.
028700     COPY ITCIABP.                                                ITCP9104
031800/                                                                 ITCP9104
00380  01  WS-ITCIASC.
029000     COPY ITCIASC.                                                ITCP9104
032100/                                                                 ITCP9104
00383  01  FILLER                REDEFINES WS-ITCIASC.
016600     05  FILLER                      PIC X(10).                   00040720
029300     COPY ITCFM105.                                               ITCP9104
040310* R12.0 E_19137 DEM START
040400     05  FILLER                      PIC X(1026120).              00041200
040410* R12.0 E_19137 END
00390
00391  01  FILLER                REDEFINES WS-ITCIASC.
016600     05  FILLER                      PIC X(10).                   00041520
029700     COPY ITCFM1A5.                                               ITCP9104
040810* R12.0 E_19137 DEM START
040900     05  FILLER                      PIC X(1026120).              00042000
040910* R12.0 E_19137 END
00398 /***********************************************************
00399  PROCEDURE DIVISION  USING WS-ITCIAPC
00400                            WS-ITCIABP
00401                            WS-ITCIASC.
033600                                                                  ITCP9104
030400 0000-MAINLINE.                                                   ITCP9104
030500                                                                  ITCP9104
034000     EVALUATE SIAPC-FUNCTION-CD-MAJOR                             ITCP9104
034100     WHEN  CWSCC-OPEN-FILES-REQUEST                               ITCP9104
034200         PERFORM 1000-INITIAL-RTN               THRU 1000-EXIT    ITCP9104
034300                                                                  ITCP9104
034400     WHEN  CWSCC-READ-FILES-REQUEST                               ITCP9104
034500         INITIALIZE SCCF-INPUT-RECORD                             ITCP9104
034700         MOVE 'NO '     TO CWSLR-CLAIM-READ-SW                    ITCP9104
034800         MOVE SPACES    TO CIABP-TRANS-ID-IND                     ITCP9104
034900                           CIABP-REC-TYPE-IND                     ITCP9104
035000                           CIABP-SEQ-NUM-IND                      ITCP9104
035100                           CIABP-DUP-REC-IND                      ITCP9104
00423                            CIABP-FOUND-F0-F1-IND                     CL**6
00426          PERFORM 2000-INIT-CLAIM-TABLE          THRU 2000-EXIT    00044800
00425          MOVE 1               TO REC-SUB

035400         IF  CIABP-MISS-90-OR-X0-ERR                              ITCP9104
035500             PERFORM 2010-EDIT-RECORD           THRU 2010-EXIT    ITCP9104
035600         END-IF                                                   ITCP9104
035700         PERFORM 3005-READ-RECORDS              THRU 3005-EXIT    ITCP9104
035800         PERFORM 3000-READ-N-STACK-RECS         THRU 3000-EXIT    ITCP9104
035900             UNTIL CWSLR-CLAIM-READ OR CIABP-EOJ                  ITCP9104
00434      WHEN CWSCC-WRITE-FILES-REQUEST
00433                                                                   00045700
               MOVE  CIABP-F1-CTR   TO PREV-CIABP-F1-CTR                00045900
               MOVE  CIABP-F0-CTR   TO PREV-CIABP-F0-CTR                00046000
               MOVE  CIABP-E6-CTR   TO PREV-CIABP-E6-CTR                00046100
               MOVE  CIABP-60-CTR   TO PREV-CIABP-60-CTR                00046200
               MOVE  CIABP-46-CTR   TO PREV-CIABP-46-CTR                00046300
               MOVE  CIABP-CLM-TYPE TO PREV-CIABP-CLM-TYPE              00046400
036000                                                                  ITCP9104
031900         PERFORM 4000-DETERMINE-SUB-FUNCTION-CD THRU 4000-EXIT    ITCP9104
032500                                                                  ITCP9104
036400     WHEN CWSCC-CLOSE-FILES-REQUEST                               ITCP9104
036500         PERFORM 5000-CLOSE-RTN                 THRU 5000-EXIT    ITCP9104
032600                                                                  ITCP9104
036700     WHEN OTHER                                                   ITCP9104
036800         SET WS-0000-PARAGRAPH TO TRUE                            ITCP9104
036900         PERFORM 4300-INVALID-FUNCTION          THRU 4300-EXIT    ITCP9104
00443      END-EVALUATE.
037100                                                                  ITCP9104
00445      GOBACK.
032700                                                                  ITCP9104
00448 /************************************************************
033000 1000-INITIAL-RTN.                                                ITCP9104
033100                                                                  ITCP9104
033200     DISPLAY     WS-UPDATE-DISPLAY                                ITCP9104
037900     DISPLAY SPACES                                               ITCP9104
038000                                                                  ITCP9104
00454      PERFORM 1010-INITIALIZE-SIAPC       THRU 1010-EXIT.
038200                                                                  ITCP9104
00456      OPEN INPUT  SCCF-INPUT-FILE.
051700                                                                  00048700
051800*R14.5 PD_57538 SKG START                                         44520000
051900     IF ITCDDI01-STAT NOT = '00'
052000        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
052100        MOVE 'UNSUCCESSFUL OPEN - ITCDDI01, FILE STATUS:'
052200                                   TO SIAPC-ERROR-MESSAGE         21033701
052300        MOVE ITCDDI01-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
052400        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
052500        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
052600        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
052700     END-IF.
052800*R14.5 PD_57538 SKG END                                           44520000
052900
00457      OPEN OUTPUT VALID-FILE.
053100                                                                  00048700
053200*R14.5 PD_57538 SKG START                                         44520000
053300     IF ITCDDO01-STAT NOT = '00'
053400        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
053500        MOVE 'UNSUCCESSFUL OPEN - ITCDDO01, FILE STATUS:'
053600                                   TO SIAPC-ERROR-MESSAGE         21033701
053700        MOVE ITCDDO01-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
053800        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
053900        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
054000        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
054100     END-IF.
054200*R14.5 PD_57538 SKG END                                           44520000
054300
00458      OPEN OUTPUT INVALID-FILE.
054500                                                                  00048700
054600*R14.5 PD_57538 SKG START                                         44520000
054700     IF ITCDDO02-STAT NOT = '00'
054800        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
054900        MOVE 'UNSUCCESSFUL OPEN - ITCDDO02, FILE STATUS:'
055000                                   TO SIAPC-ERROR-MESSAGE         21033701
055100        MOVE ITCDDO02-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
055200        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
055300        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
055400        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
055500     END-IF.
055600*R14.5 PD_57538 SKG END                                           44520000
055700
00459      OPEN OUTPUT REJECT-FILE.
055900                                                                  00048700
056000*R14.5 PD_57538 SKG START                                         44520000
056100     IF ITCDDO03-STAT NOT = '00'
056200        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
056300        MOVE 'UNSUCCESSFUL OPEN - ITCDDO03, FILE STATUS:'
056400                                   TO SIAPC-ERROR-MESSAGE         21033701
056500        MOVE ITCDDO03-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
056600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
056700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
056800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
056900     END-IF.
057000*R14.5 PD_57538 SKG END                                           44520000
057100
00460      OPEN OUTPUT RELEASE-FILE.
057300                                                                  00048700
057400*R14.5 PD_57538 SKG START                                         44520000
057500     IF ITCDDO07-STAT NOT = '00'
057600        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
057700        MOVE 'UNSUCCESSFUL OPEN - ITCDDO07, FILE STATUS:'
057800                                   TO SIAPC-ERROR-MESSAGE         21033701
057900        MOVE ITCDDO07-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
058000        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
058100        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
058200        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
058300     END-IF.
058400*R14.5 PD_57538 SKG END                                           44520000
058500
00461      OPEN OUTPUT VALID-ADJ-FILE.
058700                                                                  00048700
058800*R14.5 PD_57538 SKG START                                         44520000
058900     IF ITCDDO08-STAT NOT = '00'
059000        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
059100        MOVE 'UNSUCCESSFUL OPEN - ITCDDO08, FILE STATUS:'
059200                                   TO SIAPC-ERROR-MESSAGE         21033701
059300        MOVE ITCDDO08-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
059400        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
059500        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
059600        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
059700     END-IF.
059800*R14.5 PD_57538 SKG END                                           44520000
059900
00463      OPEN OUTPUT VALID-ECR-FILE.
060100                                                                  00048700
060200*R14.5 PD_57538 SKG START                                         44520000
060300     IF ITCDDO10-STAT NOT = '00'
060400        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
060500        MOVE 'UNSUCCESSFUL OPEN - ITCDDO10, FILE STATUS:'
060600                                   TO SIAPC-ERROR-MESSAGE         21033701
060700        MOVE ITCDDO10-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
060800        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
060900        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
061000        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
061100     END-IF.
061200*R14.5 PD_57538 SKG END                                           44520000
061300
061301*R14.5 E_34072 DSL BEGIN                                          44520000
061310     OPEN OUTPUT MISMATCH-FILE .                                  00049400
061320                                                                  00048700
061340     IF ITCDDO11-STAT NOT = '00'
061350        MOVE '1000-INITIAL-RTN   ' TO WS-PARAGRAPH-NM
061360        MOVE 'UNSUCCESSFUL OPEN - ITCDDO11, FILE STATUS:'
061370                                   TO SIAPC-ERROR-MESSAGE         21033701
061380        MOVE ITCDDO11-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
061390        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
061391        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
061392        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
061393     END-IF.
061394*R14.5 E_34072 DSL END                                            44520000
033900                                                                  ITCP9104
           MOVE SPACES TO CIASC-NEXT-RECORD                             00461
                          CIASC-STACKED-CLAIM-TABLE.
039300                                                                  ITCP9104
034300 1000-EXIT.  EXIT.                                                ITCP9104
034400                                                                  ITCP9104
039600/                                                                 ITCP9104
039700 1010-INITIALIZE-SIAPC.                                           ITCP9104
039800                                                                  ITCP9104
039900     MOVE SPACES                       TO SIAPC-RETURN-DATA       ITCP9104
040000     MOVE CWSCC-SUCCESSFUL             TO SIAPC-RETURN-IND        ITCP9104
040100     MOVE CWSCC-ACTION-COMPLETE        TO SIAPC-RETURN-STATUS     ITCP9104
040200     .                                                            ITCP9104
040300                                                                  ITCP9104
040400 1010-EXIT.  EXIT.                                                ITCP9104
034600                                                                  ITCP9104
040600/                                                                 ITCP9104
040700 2000-INIT-CLAIM-TABLE.                                           ITCP9104
040800                                                                  ITCP9104
           MOVE ZEROS  TO CIASC-APPENDED.
034900                                                                  ITCP9104
049410* R12.0 E_19137 DEM START
049500     IF REC-SUB > 0 AND < SFCNT-MAX-INST-RECS                     00052200
049510* R12.0 E_19137 END
              CONTINUE                                                  00052300
           ELSE                                                         00052400
              MOVE 1  TO REC-SUB.                                       00052500
                                                                        00052600
           MOVE SPACES TO WS-HOLD-CLAIM (1: (255 * REC-SUB)).           00052700
00490                                                                   00052800
           MOVE ZEROES TO      CIABP-F0-CTR                             00052900
                               CIABP-F1-CTR                             00053000
                               CIABP-E6-CTR                             00053100
                               CIABP-60-CTR                             00053200
                               CIABP-46-CTR.                            00053300
00491                                                                   00053400
           IF PREV-CIABP-CLM-TYPE = '20' OR '40'
              PERFORM 2001-INIT-PROF-CLAIM-STACK                        00053800
           ELSE                                                         00053900
              PERFORM 2002-INIT-INST-CLAIM-STACK                        00054000
           END-IF.                                                      00054100
041500                                                                  ITCP9104
           MOVE SPACE           TO PREV-CIABP-CLM-TYPE.
           MOVE 0               TO PREV-CIABP-F1-CTR
                                   PREV-CIABP-F0-CTR
                                   PREV-CIABP-E6-CTR
                                   PREV-CIABP-60-CTR
                                   PREV-CIABP-46-CTR.

035200 2000-EXIT.  EXIT.                                                ITCP9104
                                                                        00054400
00231*********************************************************
       2001-INIT-PROF-CLAIM-STACK.                                      00054600
                                                                        00054700
           PERFORM VARYING WS-SUBR FROM 1 BY 1                          00054800
052610* R12.0 E_19137 DEM START
052700           UNTIL WS-SUBR > SFCNT-CLM-CNT1-PROF                    00054900
052710* R12.0 E_19137 END
               MOVE SPACES TO CIASC-STACKED-CLAIM-ITEM (WS-SUBR)        00055000
           END-PERFORM.
                                                                        00055200
           PERFORM VARYING WS-SUBR FROM 1 BY 1                          00055300
                 UNTIL WS-SUBR > PREV-CIABP-E6-CTR                      00055400
                 MOVE SPACES TO CIASC-E6-1-999-SLOT (WS-SUBR)           00055500
           END-PERFORM.                                                 00055600
                                                                        00055700
           IF PREV-CIABP-F0-CTR  > 0                                    00055800
               PERFORM VARYING WS-SUBR FROM 1 BY 1                      00055900
                 UNTIL WS-SUBR > PREV-CIABP-F0-CTR                      00056000
                 MOVE SPACES TO CIASC-F0-1-999-SLOT (WS-SUBR)           00056100
                 MOVE SPACES TO CIASC-F5-1-999-SLOT (WS-SUBR)           00056200
                 MOVE SPACES TO CIASC-F6-1-999-SLOT (WS-SUBR)           00056300
               END-PERFORM                                              00056400
           ELSE                                                         00056500
               PERFORM VARYING WS-SUBR FROM 1 BY 1                      00056600
                 UNTIL WS-SUBR > PREV-CIABP-F1-CTR                      00056700
                   MOVE SPACES TO CIASC-F1-1-999-SLOT (WS-SUBR)         00056800
                   MOVE SPACES TO CIASC-F5-1-999-SLOT (WS-SUBR)         00056900
                   MOVE SPACES TO CIASC-F6-1-999-SLOT (WS-SUBR)         00057000
               END-PERFORM                                              00057100
           END-IF.                                                      00057200
                                                                        00057300
           MOVE SPACES TO CIASC-G0-SLOT.                                00057400
           MOVE SPACES TO CIASC-X0-SLOT.                                00057500
                                                                        00057600
       2001-INIT-EXIT.                                                  00057700
            EXIT.                                                       00057800
                                                                        00057900
       2002-INIT-INST-CLAIM-STACK.                                      00058000
                                                                        00058100
            PERFORM VARYING WS-SUBR FROM 1 BY 1                         00058200
056010* R12.0 E_19137 DEM START
056100           UNTIL WS-SUBR > SFCNT-CLM-CNT1-INST                    00058300
056110* R12.0 E_19137 END
               MOVE SPACES TO CIASC-STACKED-CLAIM-ITEM (WS-SUBR)        00058400
            END-PERFORM.                                                00058500
                                                                        00058600
            PERFORM VARYING WS-SUBR FROM 1 BY 1                         00058700
                 UNTIL WS-SUBR > PREV-CIABP-46-CTR                      00058800
                 MOVE SPACES TO CIASC-46-1-999-SLOT (WS-SUBR)           00058900
            END-PERFORM.                                                00059000
                                                                        00059100
057000* R12.0 E_19137 DEM START
057100      MOVE SPACES TO CIASC-47-SLOT.                               00059200
057101* R12.0 E_19137 END
            MOVE SPACES TO CIASC-50-SLOT.                               00059200
                                                                        00059300
            PERFORM VARYING WS-SUBR FROM 1 BY 1                         00059400
                 UNTIL WS-SUBR > PREV-CIABP-60-CTR                      00059500
               MOVE SPACES TO CIASC-60-1-999-SLOT (WS-SUBR)             00059600
               MOVE SPACES TO CIASC-65-1-999-SLOT (WS-SUBR)             00059700
               MOVE SPACES TO CIASC-66-1-999-SLOT (WS-SUBR)             00059800
            END-PERFORM.                                                00059900
                                                                        00060000
            MOVE SPACES TO CIASC-70-SLOT.                               00060100
058000* R12.0 E_19137 DEM START
058100      MOVE SPACES TO CIASC-71-SLOT.                               00059200
058200      MOVE SPACES TO CIASC-72-SLOT.                               00059200
058300      MOVE SPACES TO CIASC-73-SLOT.                               00059200
058400      MOVE SPACES TO CIASC-74-SLOT.                               00059200
058500      MOVE SPACES TO CIASC-75-SLOT.                               00059200
058501* R12.0 E_19137 END
            MOVE SPACES TO CIASC-80-SLOT.                               00060200
            MOVE SPACES TO CIASC-90-SLOT.                               00060300
                                                                        00060400
       2002-INIT-EXIT.                                                  00060500
           EXIT.                                                        00060600
041700                                                                  ITCP9104
035300/                                                                 ITCP9104
041900 2010-EDIT-RECORD.                                                ITCP9104
035500                                                                  ITCP9104
042100     MOVE 'NO '                     TO CWSLR-MISSING-90-OR-X0-SW  ITCP9104
042200     MOVE SPACES                    TO CIABP-MISS-90-OR-X0-IND    ITCP9104
042300     MOVE WS-HOLD-SCCF-INPUT-RECORD TO SCCF-INPUT-RECORD          ITCP9104
042400     MOVE SPACES                    TO WS-HOLD-SCCF-INPUT-RECORD  ITCP9104
036000                                                                  ITCP9104
042600     IF  CWSRR-TYPE-05                                            ITCP9104
042700     OR  CWSRR-TYPE-A5                                            ITCP9104
042800         MOVE CWSRR-CLM-TYPE     TO CIABP-CLM-TYPE                ITCP9104
042900         MOVE CWSRR-LOC-PLAN-CNTL-NUM                             ITCP9104
043000                                 TO WS-ERR-LOC-PLAN-CNTL-NUM      ITCP9104
043100         IF  CWSRR-TRANS-ID-VALID                                 ITCP9104
043200             PERFORM X200-SINGLE-MULTI-CHECK                      ITCP9104
043300                THRU X200-SINGLE-MULTI-CHECK-EXIT                 ITCP9104
043400             IF CWSLR-SEQ-NUM-ERROR                               ITCP9104
043500                 PERFORM 3400-FILE-SEQ-ERROR    THRU 3400-EXIT    ITCP9104
043600             END-IF                                               ITCP9104
037100         ELSE                                                     ITCP9104
043800             SET CWSLR-TRANS-ID-ERROR TO TRUE                     ITCP9104
043900             PERFORM 3020-TRANS-ID-INV          THRU 3020-EXIT    ITCP9104
037800         END-IF                                                   ITCP9104
044100         PERFORM 3600-DETERMINE-RECORD-CLAIM    THRU 3600-EXIT    ITCP9104
044200         PERFORM 3700-ACCUMULATE-RECORD-CLAIM   THRU 3700-EXIT    ITCP9104
044300         PERFORM 3800-STACK-CLAIMS              THRU 3800-EXIT    ITCP9104
037900                                                                  ITCP9104
044500         IF CWSRR-VALID-RECORD-READ                               ITCP9104
044600             MOVE SCCF-INPUT-RECORD                               ITCP9104
044700                                 TO CWSLR-TEST-AREA               ITCP9104
038200         END-IF                                                   ITCP9104
040300     ELSE                                                         ITCP9104
045000         IF CWSRR-TRAILER                                         ITCP9104
045100             MOVE SCCF-INPUT-RECORD                               ITCP9104
045200                                 TO FILE-TRAILER-RECORD           ITCP9104
045300                                    CWSLR-TEST-AREA               ITCP9104
045400         ELSE                                                     ITCP9104
045500             PERFORM 3200-MISSING-05-OR-A5      THRU 3200-EXIT    ITCP9104
045600         END-IF                                                   ITCP9104
045700     END-IF                                                       ITCP9104
045800     .                                                            ITCP9104
045900                                                                  ITCP9104
046000 2010-EXIT.  EXIT.                                                ITCP9104
046100                                                                  ITCP9104
046200/                                                                 ITCP9104
046300 3000-READ-N-STACK-RECS.                                          ITCP9104
046400                                                                  ITCP9104
046500     IF CWSRR-VALID-RECORD-READ                                   ITCP9104
046600         PERFORM X100-ITCPDSB-SEQUENCE-CHECK                      ITCP9104
046700            THRU X199-ITCPDSB-EXIT                                ITCP9104
046800     ELSE                                                         ITCP9104
046900         MOVE 'YES'              TO CWSLR-INVALID-RECORD-SW       ITCP9104
00546      END-IF.
040600                                                                  ITCP9104
047200     PERFORM 3010-ERROR-CHECK                   THRU 3010-EXIT    ITCP9104
047300     PERFORM 3600-DETERMINE-RECORD-CLAIM        THRU 3600-EXIT    ITCP9104
047400     PERFORM 3700-ACCUMULATE-RECORD-CLAIM       THRU 3700-EXIT    ITCP9104
047500     PERFORM 3800-STACK-CLAIMS                  THRU 3800-EXIT    ITCP9104
047600                                                                  ITCP9104
047700     IF  CWSRR-VALID-RECORD-READ                                  ITCP9104
047800         MOVE SCCF-INPUT-RECORD  TO CWSLR-TEST-AREA               ITCP9104
047900     END-IF                                                       ITCP9104
048000                                                                  ITCP9104
048100     IF NOT CWSLR-CLAIM-READ                                      ITCP9104
048200         PERFORM 3005-READ-RECORDS              THRU 3005-EXIT    ITCP9104
040900     END-IF                                                       ITCP9104
048400     .                                                            ITCP9104
041000                                                                  ITCP9104
048600 3000-EXIT.  EXIT.                                                ITCP9104
041400                                                                  ITCP9104
048800/                                                                 ITCP9104
048900 3005-READ-RECORDS.                                               ITCP9104
049000                                                                  ITCP9104
049100     MOVE 'NO '                  TO CWSLR-TRANS-ID-ERROR-SW       ITCP9104
049200                                    CWSLR-INVALID-RECORD-SW       ITCP9104
049300                                    CWSLR-SEQ-NUM-ERROR-SW        ITCP9104
049400                                    CWSLR-DUPLICATE-RECORD-SW     ITCP9104
00572                                     CWSLR-FOUND-F0-F1-SW             CL*16
049500                                                                  ITCP9104
049600     READ SCCF-INPUT-FILE INTO SCCF-INPUT-RECORD                  ITCP9104
049700     AT END                                                       ITCP9104
049800         PERFORM 3100-END-OF-FILE-PROCESSING    THRU 3100-EXIT    ITCP9104
049900     END-READ                                                     ITCP9104
041600     .                                                            ITCP9104
050100                                                                  ITCP9104
082700*R14.5 PD_57538 SKG START                                         44520000
082800     IF ITCDDI01-STAT NOT = '00' AND '10'
082900        MOVE '3005-READ-RECORDS  ' TO WS-PARAGRAPH-NM
083000        MOVE 'UNSUCCESSFUL READ - ITCDDI01, FILE STATUS:'
083100                                   TO SIAPC-ERROR-MESSAGE         21033701
083200        MOVE ITCDDI01-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
083300        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
083400        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
083500        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
083600     END-IF.
083700*R14.5 PD_57538 SKG END                                           44520000
083800                                                                  00069400
050200 3005-EXIT.  EXIT.                                                ITCP9104
050300                                                                  ITCP9104
050400/                                                                 ITCP9104
050500 3010-ERROR-CHECK.                                                ITCP9104
050600                                                                  ITCP9104
050700     IF CWSLR-MISSING-90-OR-X0                                    ITCP9104
050800         CONTINUE                                                 ITCP9104
050900     ELSE                                                         ITCP9104
051000         IF CWSRR-TYPE-05                                         ITCP9104
051100         OR CWSRR-TYPE-A5                                         ITCP9104
051200             MOVE CWSRR-CLM-TYPE TO CIABP-CLM-TYPE                ITCP9104
051300             MOVE CWSRR-LOC-PLAN-CNTL-NUM                         ITCP9104
051400                                 TO WS-ERR-LOC-PLAN-CNTL-NUM      ITCP9104
051500         END-IF                                                   ITCP9104
051600     END-IF                                                       ITCP9104
051700                                                                  ITCP9104
051800     EVALUATE TRUE                                                ITCP9104
051900     WHEN CWSLR-MISSING-05-OR-A5                                  ITCP9104
052000         PERFORM 3200-MISSING-05-OR-A5          THRU 3200-EXIT    ITCP9104
052100                                                                  ITCP9104
052200     WHEN CWSLR-FILE-SEQ-ERROR                                    ITCP9104
052300         PERFORM 3400-FILE-SEQ-ERROR            THRU 3400-EXIT    ITCP9104
052400                                                                  ITCP9104
052500     WHEN CWSLR-INVALID-RECORD-TYPE                               ITCP9104
052600         PERFORM 3030-INVALID-RECORD            THRU 3030-EXIT    ITCP9104
052700                                                                  ITCP9104
052800     WHEN CWSLR-DUPLICATE-RECORD                                  ITCP9104
052900         PERFORM 3500-DUPLICATE-RECORD-ERROR    THRU 3500-EXIT    ITCP9104
053000                                                                  ITCP9104
053100     WHEN CWSLR-MISSING-90-OR-X0                                  ITCP9104
053200         PERFORM 3300-MISSING-90-OR-X0          THRU 3300-EXIT    ITCP9104
053300                                                                  ITCP9104
00614      WHEN CWSLR-FOUND-F0-F1                                          CL**3
00615          PERFORM 3520-FOUND-F0-F1               THRU 3520-EXIT       CL**3
00617                                                                      CL**3
053400     WHEN OTHER                                                   ITCP9104
053500         IF NOT CWSRR-TRAILER                                     ITCP9104
053600             IF CWSRR-TRANS-ID-VALID                              ITCP9104
053700                 PERFORM X200-SINGLE-MULTI-CHECK                  ITCP9104
053800                    THRU X200-SINGLE-MULTI-CHECK-EXIT             ITCP9104
053900                 IF CWSLR-SEQ-NUM-ERROR                           ITCP9104
054000                     PERFORM 3400-FILE-SEQ-ERROR                  ITCP9104
054100                        THRU 3400-EXIT                            ITCP9104
054200                 END-IF                                           ITCP9104
054300             ELSE                                                 ITCP9104
054400                 SET CWSLR-TRANS-ID-ERROR TO TRUE                 ITCP9104
054500                 PERFORM 3020-TRANS-ID-INV      THRU 3020-EXIT    ITCP9104
054600             END-IF                                               ITCP9104
054700         END-IF                                                   ITCP9104
054800     END-EVALUATE                                                 ITCP9104
054900     .                                                            ITCP9104
055000                                                                  ITCP9104
055100 3010-EXIT.  EXIT.                                                ITCP9104
055200                                                                  ITCP9104
055300/                                                                 ITCP9104
055400 3020-TRANS-ID-INV.                                               ITCP9104
055500                                                                  ITCP9104
055600     IF  CIABP-TRANS-ID-ERR                                       ITCP9104
055700         CONTINUE                                                 ITCP9104
055800     ELSE                                                         ITCP9104
055900         PERFORM 1010-INITIALIZE-SIAPC THRU 1010-EXIT             ITCP9104
056000         SET CIABP-TRANS-ID-ERR        TO TRUE                    ITCP9104
056100     END-IF                                                       ITCP9104
056200     .                                                            ITCP9104
056300                                                                  ITCP9104
056400 3020-EXIT.  EXIT.                                                ITCP9104
056500                                                                  ITCP9104
056600/                                                                 ITCP9104
056700 3030-INVALID-RECORD.                                             ITCP9104
056800                                                                  ITCP9104
056900     IF CIABP-REC-TYPE-ERR                                        ITCP9104
057000        CONTINUE                                                  ITCP9104
057100     ELSE                                                         ITCP9104
057200         PERFORM 1010-INITIALIZE-SIAPC THRU 1010-EXIT             ITCP9104
057300         SET CIABP-REC-TYPE-ERR          TO TRUE                  ITCP9104
057400     END-IF                                                       ITCP9104
057500     .                                                            ITCP9104
057600                                                                  ITCP9104
057700 3030-EXIT.  EXIT.                                                ITCP9104
057800                                                                  ITCP9104
041900/                                                                 ITCP9104
042000 3100-END-OF-FILE-PROCESSING.                                     ITCP9104
092100                                                                  00077900
092200*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
092300     MOVE '3100-END-OF-FILE-PROCESSING'  TO WS-PARAGRAPH-NM
042100                                                                  ITCP9104
058200     MOVE 'Y'                    TO CIABP-EOJ-IND                 ITCP9104
042300                                                                  ITCP9104
058400     IF  SCCF-INPUT-RECORD = (SPACES OR LOW-VALUES)               ITCP9104
058500     AND (CIABP-CLAIMS-READ    NUMERIC                            ITCP9104
058600         AND CIABP-CLAIMS-READ = ZEROS)                           ITCP9104
058700     AND (CIABP-RECS-READ      NUMERIC                            ITCP9104
058800         AND CIABP-RECS-READ   = ZEROS)                           ITCP9104
058900         SET WS-3100-PARAGRAPH TO TRUE                            ITCP9104
059000         MOVE CWSCC-ABORT        TO SIAPC-RETURN-IND              ITCP9104
059100         MOVE CWSCC-EMPTY-FILE   TO SIAPC-RETURN-STATUS           ITCP9104
059200         MOVE WS-EMPTY-FILE-ERR-MSG                               ITCP9104
059300                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
059400         MOVE '3600'             TO SIAPC-ABEND-CODE              ITCP9104
059500         SET WS-EMPTY-FILE TO TRUE                                ITCP9104
059600         PERFORM 9000-ERROR-MESSAGE             THRU 9000-EXIT    ITCP9104
043200     END-IF                                                       ITCP9104
043300                                                                  ITCP9104
043400     IF NOT CWSLR-TRAILER                                         ITCP9104
060000         SET WS-3100-PARAGRAPH   TO TRUE                          ITCP9104
060100         SET CWSLR-TRAILER-MISSING TO TRUE                        ITCP9104
060200         MOVE CWSCC-ABORT        TO SIAPC-RETURN-IND              ITCP9104
060300         MOVE CWSCC-MISSING-FILE-TRLR-REC                         ITCP9104
060400                                 TO SIAPC-RETURN-STATUS           ITCP9104
060500         MOVE WS-MISSING-FILE-TRLR-MSG                            ITCP9104
060600                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
060700         MOVE '3600'             TO SIAPC-ABEND-CODE              ITCP9104
060800         SET WS-MISSING-TRAILER  TO TRUE                          ITCP9104
060900         PERFORM 9000-ERROR-MESSAGE             THRU 9000-EXIT    ITCP9104
043600     END-IF                                                       ITCP9104
043700                                                                  ITCP9104
043800     IF  CWSRR-TRAILER                                            ITCP9104
061300     AND (CIABP-CLAIMS-READ    NUMERIC                            ITCP9104
061400         AND CIABP-CLAIMS-READ = ZEROS)                           ITCP9104
061500     AND (CIABP-RECS-READ      NUMERIC                            ITCP9104
061600         AND CIABP-RECS-READ   = ZEROS)                           ITCP9104
061700         SET TRAILER-ONLY TO TRUE                                 ITCP9104
061800         PERFORM 3110-DISPLAY-TRAILER-MESSAGE   THRU 3110-EXIT    ITCP9104
044500     ELSE                                                         ITCP9104
062000         IF CWSLR-TRAILER                                         ITCP9104
062100             IF (CIABP-CLAIMS-READ NUMERIC                        ITCP9104
062200             AND CDB99D-CLM-TRANS-CNT NUMERIC                     ITCP9104
062300             AND (CIABP-CLAIMS-READ NOT = CDB99D-CLM-TRANS-CNT))  ITCP9104
062400                 SET TRAILER-OUT-OF-BALANCE TO TRUE               ITCP9104
062500                 MOVE CWSCC-FILE-TRLR-REC-CNT-ERR                 ITCP9104
062600                                 TO SIAPC-RETURN-STATUS           ITCP9104
062700                 PERFORM 3110-DISPLAY-TRAILER-MESSAGE             ITCP9104
062800                    THRU 3110-EXIT                                ITCP9104
062900                 MOVE CIABP-CLAIMS-READ                           ITCP9104
063000                                 TO CDB99D-CLM-TRANS-CNT          ITCP9104
063100             ELSE                                                 ITCP9104
063200                 IF CDB99D-CLM-TRANS-CNT NOT NUMERIC              ITCP9104
063300                     SET TRAILER-BAD-DATA TO TRUE                 ITCP9104
063400                     MOVE CWSCC-FILE-TRLR-REC-CNT-ERR             ITCP9104
063500                                 TO SIAPC-RETURN-STATUS           ITCP9104
063600                     PERFORM 3110-DISPLAY-TRAILER-MESSAGE         ITCP9104
063700                        THRU 3110-EXIT                            ITCP9104
063800                     MOVE CIABP-CLAIMS-READ                       ITCP9104
063900                                  TO CDB99D-CLM-TRANS-CNT         ITCP9104
064000                 END-IF                                           ITCP9104
064100             END-IF                                               ITCP9104
064200         END-IF                                                   ITCP9104
046800                                                                  ITCP9104
064400         IF CWSLR-TRAILER                                         ITCP9104
064500             IF (CIABP-RECS-READ  NUMERIC                         ITCP9104
064600             AND CDB99D-CLM-TRANS-CNT NUMERIC                     ITCP9104
064700             AND (CIABP-RECS-READ NOT = CDB99D-REC-CNT))          ITCP9104
064800                 SET TRAILER-OUT-OF-BALANCE TO TRUE               ITCP9104
048200                 MOVE CWSCC-FILE-TRLR-REC-CNT-ERR                 ITCP9104
065000                                 TO SIAPC-RETURN-STATUS           ITCP9104
048400                 PERFORM 3110-DISPLAY-TRAILER-MESSAGE             ITCP9104
048500                    THRU 3110-EXIT                                ITCP9104
048600                 MOVE CIABP-RECS-READ TO CDB99D-REC-CNT           ITCP9104
065400             ELSE                                                 ITCP9104
065500                 IF CDB99D-REC-CNT NOT NUMERIC                    ITCP9104
065600                     SET TRAILER-BAD-DATA TO TRUE                 ITCP9104
065700                     MOVE CWSCC-FILE-TRLR-REC-CNT-ERR             ITCP9104
065800                                 TO SIAPC-RETURN-STATUS           ITCP9104
065900                     PERFORM 3110-DISPLAY-TRAILER-MESSAGE         ITCP9104
066000                        THRU 3110-EXIT                            ITCP9104
066100                     MOVE CIABP-RECS-READ TO CDB99D-REC-CNT       ITCP9104
066200                 END-IF                                           ITCP9104
066300             END-IF                                               ITCP9104
066400         END-IF                                                   ITCP9104
049000     END-IF                                                       ITCP9104
049100                                                                  ITCP9104
049200     IF CDB99D-CLM-TRANS-CNT NUMERIC                              ITCP9104
066800         MOVE CDB99D-CLM-TRANS-CNT                                ITCP9104
066900                                 TO CIABP-FILE-TRAILER-CLAIMS     ITCP9104
049500     END-IF                                                       ITCP9104
049600                                                                  ITCP9104
049700     IF CDB99D-REC-CNT NUMERIC                                    ITCP9104
067300         MOVE CDB99D-REC-CNT     TO CIABP-FILE-TRAILER-RECS       ITCP9104
049900     END-IF                                                       ITCP9104
050000                                                                  ITCP9104
067600     MOVE SPACES                 TO CDB99D-RECORD                 ITCP9104
050200                                                                  ITCP9104
050300     IF CWSLR-TRAILER                                             ITCP9104
067900         MOVE CWSLR-REC-TYPE     TO CDB99D-REC-TYPE               ITCP9104
068000         MOVE CWSLR-SEQ-NUM      TO CDB99D-SEQ-NUM                ITCP9104
068100         MOVE WS-TRAILER-TRANS-ID                                 ITCP9104
068200                                 TO CDB99D-TRANS-ID               ITCP9104
050800     END-IF                                                       ITCP9104
050900                                                                  ITCP9104
068500     MOVE CIABP-VALID-CLAIMS     TO CDB99D-CLM-TRANS-CNT          ITCP9104
051100     MOVE CIABP-VALID-TOTAL-FILE-RECS                             ITCP9104
068700                                 TO CDB99D-REC-CNT                ITCP9104
068800     MOVE SIAPC-GREG-CURRENT-DT  TO CDB99D-CREATE-DT              ITCP9104
068900     MOVE SIAPC-CURRENT-TIME     TO CDB99D-CREATE-TIME            ITCP9104
069000     MOVE 'SCSF-VALID-FILE'      TO CDB99D-INP-SRC-ID             ITCP9104
051800                                                                  ITCP9104
051900     WRITE VALID-FILE-RECORD FROM FILE-TRAILER-RECORD             ITCP9104
103600*R14.5 PD_57538 SKG START                                         44520000
103700     SET  WS-VALID-FILE-I        TO TRUE                          00190100
103800     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
103900*R14.5 PD_57538 SKG END                                           44520000
052000                                                                  ITCP9104
069400     MOVE CIABP-INVALID-CLAIMS   TO CDB99D-CLM-TRANS-CNT          ITCP9104
052200     MOVE CIABP-INVALID-TOTAL-FILE-RECS                           ITCP9104
069600                                 TO CDB99D-REC-CNT                ITCP9104
069700     MOVE 'SCSF-INVALID-FILE'    TO CDB99D-INP-SRC-ID             ITCP9104
052500                                                                  ITCP9104
052600     WRITE INVALID-FILE-RECORD FROM FILE-TRAILER-RECORD           ITCP9104
104700*R14.5 PD_57538 SKG START                                         44520000
104800     SET  WS-INVALID-FILE-I      TO TRUE                          00190100
104900     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
105000*R14.5 PD_57538 SKG END                                           44520000
052700                                                                  ITCP9104
070100     MOVE CIABP-REJECT-CLAIMS    TO CDB99D-CLM-TRANS-CNT          ITCP9104
052900     MOVE CIABP-REJECT-TOTAL-FILE-RECS                            ITCP9104
070300                                 TO CDB99D-REC-CNT                ITCP9104
070400     MOVE 'SCSF-REJECT-FILE'     TO CDB99D-INP-SRC-ID             ITCP9104
053200                                                                  ITCP9104
053300     WRITE REJECT-FILE-RECORD FROM FILE-TRAILER-RECORD            ITCP9104
105800*R14.5 PD_57538 SKG START                                         44520000
105900     SET  WS-REJECT-FILE-I       TO TRUE                          00190100
106000     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
106100*R14.5 PD_57538 SKG END                                           44520000
106200                                                                  00090500
106210*R14.5 E_34072 DSL BEGIN                                          00089800
106220     MOVE CIABP-MMATCH-CLAIMS    TO CDB99D-CLM-TRANS-CNT          00089900
106230     MOVE CIABP-MMATCH-TOTAL-FILE-RECS                            00090000
106240                                 TO CDB99D-REC-CNT                00090100
106250     MOVE 'SCSF-MISMATCH-FILE'   TO CDB99D-INP-SRC-ID             00090200
106260                                                                  00090300
106270     WRITE MISMATCH-FILE-RECORD FROM FILE-TRAILER-RECORD          00090400
106290     SET  WS-MIS-MATCH-FILE-I    TO TRUE                          00190100
106291     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
106292*R14.5 E_34072 DSL END                                            00089800
053400                                                                  ITCP9104
070800     MOVE CIABP-RELEASE-CLAIMS   TO CDB99D-CLM-TRANS-CNT          ITCP9104
053600     MOVE CIABP-RELEASE-TOTAL-FILE-RECS                           ITCP9104
071000                                 TO CDB99D-REC-CNT                ITCP9104
071100     MOVE 'SCSF-RELEASE-FILE'    TO CDB99D-INP-SRC-ID             ITCP9104
053900                                                                  ITCP9104
054000     WRITE RELEASE-FILE-RECORD FROM FILE-TRAILER-RECORD           ITCP9104
106900*R14.5 PD_57538 SKG START                                         44520000
107000     SET  WS-RELEASE-FILE-I      TO TRUE                          00190100
107100     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
107200*R14.5 PD_57538 SKG END                                           44520000
054100                                                                  ITCP9104
071500     MOVE CIABP-ADJ-CLAIMS       TO CDB99D-CLM-TRANS-CNT          ITCP9104
054300     MOVE CIABP-ADJ-TOTAL-FILE-RECS                               ITCP9104
071700                                 TO CDB99D-REC-CNT                ITCP9104
054500     MOVE 'SCSF-VALID-ADJ-FILE'  TO CDB99D-INP-SRC-ID             ITCP9104
054600                                                                  ITCP9104
054700     WRITE VALID-ADJ-FILE-RECORD FROM FILE-TRAILER-RECORD         ITCP9104
108000*R14.5 PD_57538 SKG START                                         44520000
108100     SET  WS-VALID-ADJ-FILE-I    TO TRUE                          00190100
108200     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
108300*R14.5 PD_57538 SKG END                                           44520000
077000                                                                  ITCP9104
077200     MOVE CIABP-ECR-CLAIMS       TO CDB99D-CLM-TRANS-CNT          ITCP9104
077300     MOVE CIABP-ECR-TOTAL-FILE-RECS                               ITCP9104
077400                                 TO CDB99D-REC-CNT                ITCP9104
077500     MOVE 'SCSF-VALID-ECR-FILE'  TO CDB99D-INP-SRC-ID             ITCP9104
077600                                                                  ITCP9104
077700     WRITE VALID-ECR-FILE-RECORD FROM FILE-TRAILER-RECORD         ITCP9104
109100*R14.5 PD_57538 SKG START                                         44520000
109200     SET  WS-VALID-ECR-FILE-I    TO TRUE                          00190100
109300     PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT                00190100
109400*R14.5 PD_57538 SKG END                                           44520000
054800     .                                                            ITCP9104
072200                                                                  ITCP9104
054900 3100-EXIT.  EXIT.                                                ITCP9104
055000                                                                  ITCP9104
072500/                                                                 ITCP9104
055100 3110-DISPLAY-TRAILER-MESSAGE.                                    ITCP9104
055200                                                                  ITCP9104
055300     EVALUATE TRUE                                                ITCP9104
072900     WHEN TRAILER-OUT-OF-BALANCE                                  ITCP9104
073000     WHEN TRAILER-BAD-DATA                                        ITCP9104
073100         MOVE '3500'             TO SIAPC-ABEND-CODE              ITCP9104
073200         MOVE CWSCC-UNSUCCESSFUL TO SIAPC-RETURN-IND              ITCP9104
073300         MOVE WS-OUT-OF-BALANCE-MSG                               ITCP9104
073400                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
073500                                                                  ITCP9104
073600     WHEN TRAILER-ONLY                                            ITCP9104
073700         MOVE '3400'             TO SIAPC-ABEND-CODE              ITCP9104
073800         MOVE CWSCC-UNSUCCESSFUL TO SIAPC-RETURN-IND              ITCP9104
073900         MOVE CWSCC-END-OF-FILE  TO SIAPC-RETURN-STATUS           ITCP9104
074000         MOVE WS-TRLR-ONLY-MSG                                    ITCP9104
074100                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
057300     END-EVALUATE                                                 ITCP9104
057400                                                                  ITCP9104
057500     DISPLAY WS-ASTERICKS                                         ITCP9104
057600     DISPLAY '*** ITCP9104 ERROR DETECTED ***'                    ITCP9104
074600     SET WS-3100-PARAGRAPH TO TRUE                                ITCP9104
057800     DISPLAY 'PARAGRAPH NAME: ' WS-PARAGRAPH-NM                   ITCP9104
057900                                                                  ITCP9104
074900     PERFORM 9100-DISPLAY-SIAPC                 THRU 9100-EXIT    ITCP9104
058100                                                                  ITCP9104
058200     IF TRAILER-OUT-OF-BALANCE                                    ITCP9104
058300     OR TRAILER-BAD-DATA                                          ITCP9104
075300         DISPLAY 'TRAILER CLAIM COUNT: ' CDB99D-CLM-TRANS-CNT     ITCP9104
075400         DISPLAY 'TRAILER RECORD COUNT: ' CDB99D-REC-CNT          ITCP9104
075500         DISPLAY 'PROGRAM CLAIM COUNT: '  CIABP-CLAIMS-READ       ITCP9104
075600         DISPLAY 'PROGRAM RECORD COUNT: ' CIABP-RECS-READ         ITCP9104
075700         DISPLAY WS-ASTERICKS                                     ITCP9104
058900     END-IF                                                       ITCP9104
059000     .                                                            ITCP9104
076000                                                                  ITCP9104
059100 3110-EXIT.  EXIT.                                                ITCP9104
059200                                                                  ITCP9104
076300/                                                                 ITCP9104
076400 3200-MISSING-05-OR-A5.                                           ITCP9104
059400                                                                  ITCP9104
076600     SET WS-3200-PARAGRAPH       TO TRUE                          ITCP9104
076700     MOVE SPACES                 TO WS-ERR-LOC-PLAN-CNTL-NUM      ITCP9104
076800     MOVE CWSRR-SCCF-SER-NUM     TO WS-ERR-SCCF-SER-NUM           ITCP9104
076900     MOVE CWSRR-REC-TYPE         TO WS-ERR-REC-TYPE               ITCP9104
077000     MOVE CWSRR-SEQ-NUM-READ     TO WS-ERR-SEQ-NUM                ITCP9104
077100     MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND              ITCP9104
077200     MOVE CWSCC-MISSING-CLAIM-HDR-REC                             ITCP9104
077300                                 TO SIAPC-RETURN-STATUS           ITCP9104
077400     MOVE WS-MISSING-HEADER-ERR-MSG                               ITCP9104
077500                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
077600     MOVE '3600'                 TO SIAPC-ABEND-CODE              ITCP9104
077700     SET WS-MISSING-CLM-HDR      TO TRUE                          ITCP9104
077800     PERFORM 9000-ERROR-MESSAGE                 THRU 9000-EXIT    ITCP9104
059900     .                                                            ITCP9104
060100                                                                  ITCP9104
062700 3200-EXIT.  EXIT.                                                ITCP9104
062800                                                                  ITCP9104
078300/                                                                 ITCP9104
062900 3300-MISSING-90-OR-X0.                                           ITCP9104
063000                                                                  ITCP9104
078600     IF  CIABP-MISS-90-OR-X0-ERR                                  ITCP9104
078700         CONTINUE                                                 ITCP9104
078800     ELSE                                                         ITCP9104
078900         PERFORM 1010-INITIALIZE-SIAPC          THRU 1010-EXIT    ITCP9104
079000         SET CIABP-MISS-90-OR-X0-ERR     TO TRUE                  ITCP9104
079100     END-IF                                                       ITCP9104
063800     .                                                            ITCP9104
063900                                                                  ITCP9104
064000 3300-EXIT.  EXIT.                                                ITCP9104
064100                                                                  ITCP9104
079600/                                                                 ITCP9104
064200 3400-FILE-SEQ-ERROR.                                             ITCP9104
064300                                                                  ITCP9104
079900     IF  CWSLR-FILE-SEQ-ERROR                                     ITCP9104
080000         SET WS-X100-PARAGRAPH   TO TRUE                          ITCP9104
080100         MOVE SPACES             TO WS-ERR-REC-AND-SEQ-NUM        ITCP9104
080200         MOVE CWSRR-SCCF-SER-NUM TO WS-ERR-SCCF-SER-NUM           ITCP9104
080300         MOVE CWSRR-REC-TYPE     TO WS-ERR-REC-TYPE               ITCP9104
080400         MOVE CWSRR-SEQ-NUM-READ TO WS-ERR-SEQ-NUM                ITCP9104
080500         MOVE CWSCC-ABORT        TO SIAPC-RETURN-IND              ITCP9104
080600         MOVE CWSCC-FILE-SEQUENCE-ERROR                           ITCP9104
080700                                 TO SIAPC-RETURN-STATUS           ITCP9104
080800         MOVE WS-FILE-SEQUENCE-ERR-MSG                            ITCP9104
080900                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
081000         MOVE '3600'             TO SIAPC-ABEND-CODE              ITCP9104
081100         SET WS-FILE-SEQUENCE TO TRUE                             ITCP9104
081200         PERFORM 9000-ERROR-MESSAGE             THRU 9000-EXIT    ITCP9104
065400     ELSE                                                         ITCP9104
081400         IF  CIABP-SEQ-NUM-ERR                                    ITCP9104
081500             CONTINUE                                             ITCP9104
081600         ELSE                                                     ITCP9104
081700             PERFORM 1010-INITIALIZE-SIAPC      THRU 1010-EXIT    ITCP9104
081800             SET CIABP-SEQ-NUM-ERR TO TRUE                        ITCP9104
081900         END-IF                                                   ITCP9104
065700     END-IF                                                       ITCP9104
066000     .                                                            ITCP9104
066100                                                                  ITCP9104
066200 3400-EXIT.  EXIT.                                                ITCP9104
066300                                                                  ITCP9104
082500/                                                                 ITCP9104
066400 3500-DUPLICATE-RECORD-ERROR.                                     ITCP9104
066500                                                                  ITCP9104
082800     IF  CIABP-DUP-REC-ERR                                        ITCP9104
082900         CONTINUE                                                 ITCP9104
083000     ELSE                                                         ITCP9104
083100         PERFORM 1010-INITIALIZE-SIAPC          THRU 1010-EXIT    ITCP9104
083200         SET CIABP-DUP-REC-ERR       TO TRUE                      ITCP9104
083300     END-IF                                                       ITCP9104
067300     .                                                            ITCP9104
083500                                                                  ITCP9104
067400 3500-EXIT.  EXIT.                                                ITCP9104
083700                                                                  ITCP9104
00932 *** DONE IF A CLAIM HAS BOTH F0 AND F1 RECORDS ON IT ****            CL**3
00933  3520-FOUND-F0-F1.                                                   CL**3
00934                                                                      CL**3
00935      IF  CIABP-FOUND-F0-F1                                           CL**9
00936          CONTINUE                                                    CL**3
00937      ELSE                                                            CL**3
00938          PERFORM 1010-INITIALIZE-SIAPC          THRU 1010-EXIT       CL**3
00939          SET CIABP-FOUND-F0-F1       TO TRUE                         CL**9
00940      END-IF                                                          CL**3
00941      .                                                               CL**3
00942  3520-EXIT. EXIT.                                                    CL**3
00943 *TM-01-08-R8.4 END                                                   CL**3
067500/                                                                 ITCP9104
067600 3600-DETERMINE-RECORD-CLAIM.                                     ITCP9104
067700                                                                  ITCP9104
084100     MOVE 'NO '                  TO CWSLR-RECORD-READ-SW          ITCP9104
084200     MOVE SPACE                  TO WS-NOTATION-REC-IND           ITCP9104
068000                                                                  ITCP9104
084400     IF CWSRR-TYPE-90                                             ITCP9104
084500     OR CWSRR-TYPE-X0                                             ITCP9104
084600     OR CWSLR-MISSING-90-OR-X0                                    ITCP9104
084700         MOVE 'YES'              TO CWSLR-CLAIM-READ-SW           ITCP9104
084800         IF NOT CWSLR-MISSING-90-OR-X0                            ITCP9104
084900             MOVE 'YES'          TO CWSLR-RECORD-READ-SW          ITCP9104
085000         END-IF                                                   ITCP9104
068500     ELSE                                                         ITCP9104
068600         IF CWSRR-TRAILER                                         ITCP9104
085300             CONTINUE                                             ITCP9104
068800         ELSE                                                     ITCP9104
085500             MOVE 'YES'          TO CWSLR-RECORD-READ-SW          ITCP9104
069000         END-IF                                                   ITCP9104
069100     END-IF                                                       ITCP9104
069200                                                                  ITCP9104
069400     IF CWSRR-TYPE-46                                             ITCP9104
086000         MOVE SCCF-INPUT-RECORD  TO ITCFM146-RECORD               ITCP9104
086100         IF FM146-SPEC-NOTE-TRNSM-IND = 'N'                       ITCP9104
086200             SET WS-BYPASS-NOTATION-RECORD TO TRUE                ITCP9104
069800         END-IF                                                   ITCP9104
069900     END-IF                                                       ITCP9104
070000                                                                  ITCP9104
070100     IF CWSRR-TYPE-E6                                             ITCP9104
086700         MOVE SCCF-INPUT-RECORD  TO ITCFM1E6-RECORD               ITCP9104
086800         IF FM1E6-SPEC-NOTE-TRNSM-IND = 'N'                       ITCP9104
086900             SET WS-BYPASS-NOTATION-RECORD TO TRUE                ITCP9104
070500         END-IF                                                   ITCP9104
070600     END-IF                                                       ITCP9104
070800     .                                                            ITCP9104
087300                                                                  ITCP9104
070900 3600-EXIT.  EXIT.                                                ITCP9104
071000                                                                  ITCP9104
087600/                                                                 ITCP9104
071100 3700-ACCUMULATE-RECORD-CLAIM.                                    ITCP9104
071200                                                                  ITCP9104
071400     IF WS-BYPASS-NOTATION-RECORD                                 ITCP9104
088000         CONTINUE                                                 ITCP9104
071600     ELSE                                                         ITCP9104
088200         IF CWSLR-RECORD-READ                                     ITCP9104
088300             ADD 1               TO CIABP-RECS-READ               ITCP9104
088400         END-IF                                                   ITCP9104
072000     END-IF                                                       ITCP9104
072200                                                                  ITCP9104
072300     IF CWSLR-CLAIM-READ                                          ITCP9104
088800         ADD 1                   TO CIABP-CLAIMS-READ             ITCP9104
072500     END-IF                                                       ITCP9104
072600     .                                                            ITCP9104
089100                                                                  ITCP9104
072700 3700-EXIT.  EXIT.                                                ITCP9104
072800                                                                  ITCP9104
089400/                                                                 ITCP9104
072900 3800-STACK-CLAIMS.                                               ITCP9104
073000                                                                  ITCP9104
089700     IF CWSLR-MISSING-90-OR-X0                                    ITCP9104
089800         MOVE SCCF-INPUT-RECORD  TO WS-HOLD-SCCF-INPUT-RECORD     ITCP9104
073600     ELSE                                                         ITCP9104
109010* R12.0 E_19137 DEM START
109100         IF REC-SUB > SFCNT-MAX-INST-RECS                         00111900
109110* R12.0 E_19137 END
090100             PERFORM 3810-TABLE-OVERFLOW        THRU 3810-EXIT    ITCP9104
090200         END-IF                                                   ITCP9104
090300         MOVE SCCF-INPUT-RECORD  TO WS-HOLD-RECORD (REC-SUB)      ITCP9104
01012                                     CIASC-STACKED-CLAIM-ITEM
01014                                                    (REC-SUB)
090400         ADD +1                  TO REC-SUB                       ITCP9104
01022          IF CWSRR-TRAILER
01023             MOVE SCCF-INPUT-RECORD  TO FILE-TRAILER-RECORD
01024          END-IF
01026      END-IF.
172800                                                                  ITCP9104
092000 3800-EXIT.  EXIT.                                                ITCP9104
173000                                                                  ITCP9104
01031 ******************************************************
092300 3810-TABLE-OVERFLOW.                                             ITCP9104
173500                                                                  ITCP9104
092500     SET WS-3810-PARAGRAPH       TO TRUE                          ITCP9104
092600     MOVE CWSRR-SCCF-SER-NUM     TO WS-ERR-SCCF-SER-NUM           ITCP9104
092700     MOVE CWSRR-REC-TYPE         TO WS-ERR-REC-TYPE               ITCP9104
092800     MOVE CWSRR-SEQ-NUM-READ     TO WS-ERR-SEQ-NUM                ITCP9104
092900     MOVE SPACES                 TO SIAPC-RETURN-DATA             ITCP9104
093000     MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND              ITCP9104
093100     MOVE CWSCC-TABLE-OVERFLOW   TO SIAPC-RETURN-STATUS           ITCP9104
093200     MOVE WS-TBL-OVERFLOW-ERR-MSG TO SIAPC-ERROR-MESSAGE          ITCP9104
093300     MOVE '3600'                 TO SIAPC-ABEND-CODE              ITCP9104
093400     SET WS-TABLE-OVERFLOW       TO TRUE                          ITCP9104
093500     PERFORM 9000-ERROR-MESSAGE                 THRU 9000-EXIT    ITCP9104
174300     .                                                            ITCP9104
093700                                                                  ITCP9104
174400 3810-EXIT.  EXIT.                                                ITCP9104
02206 ************************************************************
174600 4000-DETERMINE-SUB-FUNCTION-CD.                                  ITCP9104
174700                                                                  ITCP9104
177300     EVALUATE SIAPC-FUNCTION-CD-SUB                               ITCP9104
177400     WHEN  SPACES                                                 ITCP9104
177500         PERFORM 4100-WRITE-CLAIMS              THRU 4100-EXIT    ITCP9104
177600                                                                  ITCP9104
177700     WHEN CWSCC-WRITE-VALID-ONLY                                  ITCP9104
177800         PERFORM 4200-WRITE-VALIDS-ONLY         THRU 4200-EXIT    ITCP9104
177900                                                                  ITCP9104
178000     WHEN OTHER                                                   ITCP9104
178100         SET WS-4000-PARAGRAPH TO TRUE                            ITCP9104
178200         PERFORM 4300-INVALID-FUNCTION          THRU 4300-EXIT    ITCP9104
178300     END-EVALUATE                                                 ITCP9104
175700     .                                                            ITCP9104
178500                                                                  ITCP9104
175800 4000-EXIT.  EXIT.                                                ITCP9104
175900                                                                  ITCP9104
178800/                                                                 ITCP9104
176000 4100-WRITE-CLAIMS.                                               ITCP9104
176100                                                                  ITCP9104
179100     MOVE SPACES                 TO WS-TYPE-SUBM                  ITCP9104
185100                                    WS-ECR-IND                    ITCP9104
176300                                                                  ITCP9104
179300     EVALUATE TRUE                                                ITCP9104
179400     WHEN CIABP-VALID-STAT-CD                                     ITCP9104
179500         IF  CIABP-HDR-INSTITUTIONAL                              ITCP9104
179600             MOVE FM105-TYPE-SUBM TO WS-TYPE-SUBM                 ITCP9104
185700             MOVE FM105-ECR-IND   TO WS-ECR-IND                   ITCP9104
176700         END-IF                                                   ITCP9104
176900         IF  CIABP-HDR-PROFESSIONAL                               ITCP9104
179900             MOVE FM1A5-TYPE-SUBM TO WS-TYPE-SUBM                 ITCP9104
186100             MOVE FM1A5-ECR-IND   TO WS-ECR-IND                   ITCP9104
180000         END-IF                                                   ITCP9104
186300         EVALUATE TRUE                                            ITCP9104
186400         WHEN WS-ECR-CLM                                          ITCP9104
186500             PERFORM 4160-WRITE-VALID-ECR-FILE  THRU 4160-EXIT    ITCP9104
186800             ADD 1               TO CIABP-ECR-CLAIMS              ITCP9104
186900             ADD CIASC-INPUT     TO CIABP-ECR-RECS                ITCP9104
187000             ADD CIASC-APPENDED  TO CIABP-ECR-APPENDED-RECS       ITCP9104
187100                                                                  ITCP9104
02468              IF CIABP-HDR-INSTITUTIONAL                           00156300
02469                 ADD 1     TO CIABP-ECR-INS-CLAIMS                 00156400
02470              ELSE                                                 00156500
02471               IF CIABP-HDR-PROFESSIONAL                           00156600
02472                  ADD 1     TO CIABP-ECR-PROF-CLAIMS               00156700
02473               END-IF                                              00156800
02474              END-IF                                               00156900
02475                                                                   00157000
136800*** REINSTATE RESUBMISSION                                        00157100
136810*** R14.5 E_34072  EXL START                                      00157100
187200         WHEN WS-RESUBMISSION-CLM                                 ITCP9104
180200             PERFORM 4150-WRITE-VALID-ADJ-FILE  THRU 4150-EXIT    ITCP9104
180500             ADD 1               TO CIABP-ADJ-CLAIMS              ITCP9104
180600             ADD CIASC-INPUT     TO CIABP-ADJ-RECS                ITCP9104
180700             ADD CIASC-APPENDED  TO CIABP-ADJ-APPENDED-RECS       ITCP9104
187900                                                                  ITCP9104
02468              IF CIABP-HDR-INSTITUTIONAL                           00158100
02469                 ADD 1     TO CIABP-ADJ-INS-CLAIMS                 00158200
02470              ELSE                                                 00158300
02471               IF CIABP-HDR-PROFESSIONAL                           00158400
02472                  ADD 1     TO CIABP-ADJ-PROF-CLAIMS               00158500
02473               END-IF                                              00158600
02474              END-IF                                               00158700
138200*** R14.5 E_34072  EXL END                                        00158800
188000         WHEN OTHER                                               ITCP9104
180900             PERFORM 4110-WRITE-VALIDS-FILE     THRU 4110-EXIT    ITCP9104
181200             ADD 1               TO CIABP-VALID-CLAIMS            ITCP9104
181300             ADD CIASC-INPUT     TO CIABP-VALID-RECS              ITCP9104
181400             ADD CIASC-APPENDED  TO CIABP-VALID-APPENDED-RECS     ITCP9104
02468              IF CIABP-HDR-INSTITUTIONAL                           00159800
02469                 ADD 1     TO CIABP-VALID-INS-CLAIMS               00159900
02470              ELSE                                                 00160000
02471               IF CIABP-HDR-PROFESSIONAL                           00160100
02472                  ADD 1     TO CIABP-VALID-PROF-CLAIMS             00160200
02473               END-IF                                              00160300
02474              END-IF                                               00160400
02475                                                                   00160500
188700         END-EVALUATE                                             ITCP9104
181600                                                                  ITCP9104
181700     WHEN CIABP-INVALID-STAT-CD                                   ITCP9104
181800         PERFORM 9200-FLAG-RELEASE-ERROR        THRU 9200-EXIT    ITCP9104
181900         IF  WS-RELSE-ERROR-FOUND                                 ITCP9104
182500             PERFORM 4140-WRITE-RELEASE-FILE    THRU 4140-EXIT    ITCP9104
182800             ADD 1               TO CIABP-RELEASE-CLAIMS          ITCP9104
182900             ADD CIASC-INPUT     TO CIABP-RELEASE-RECS            ITCP9104
183000             ADD CIASC-APPENDED  TO CIABP-RELEASE-APPENDED-RECS   ITCP9104
02468              IF CIABP-HDR-INSTITUTIONAL                           00162900
02469                 ADD 1     TO CIABP-RELEASE-INS-CLAIMS             00163000
02470              ELSE                                                 00163100
02471               IF CIABP-HDR-PROFESSIONAL                           00163200
02472                  ADD 1     TO CIABP-RELEASE-PROF-CLAIMS           00163300
02473               END-IF                                              00163400
02474              END-IF                                               00163500
02475                                                                   00163600
183100         ELSE                                                     ITCP9104
183200             PERFORM 4120-WRITE-INVALIDS-FILE   THRU 4120-EXIT    ITCP9104
183500             ADD 1               TO CIABP-INVALID-CLAIMS          ITCP9104
183600             ADD CIASC-INPUT     TO CIABP-INVALID-RECS            ITCP9104
183700             ADD CIASC-APPENDED  TO CIABP-INVALID-APPENDED-RECS   ITCP9104
                                                                        00164400
02468              IF CIABP-HDR-INSTITUTIONAL                           00164600
02469                 ADD 1     TO CIABP-INVALID-INS-CLAIMS             00164700
02470              ELSE                                                 00164800
02471                 IF CIABP-HDR-PROFESSIONAL                         00164900
02472                    ADD 1     TO CIABP-INVALID-PROF-CLAIMS         00165000
02473                 END-IF                                            00165100
02474              END-IF                                               00165200
02475                                                                   00165300
177300         END-IF                                                   ITCP9104
183900                                                                  ITCP9104
184000     WHEN CIABP-REJECT-STAT-CD                                    ITCP9104
184100         PERFORM 4130-WRITE-REJECT-FILE         THRU 4130-EXIT    ITCP9104
184400         ADD 1                   TO CIABP-REJECT-CLAIMS           ITCP9104
184500         ADD CIASC-INPUT         TO CIABP-REJECT-RECS             ITCP9104
184600         ADD CIASC-APPENDED      TO CIABP-REJECT-APPENDED-RECS    ITCP9104
                                                                        00166300
02468          IF CIABP-HDR-INSTITUTIONAL                               00166500
02469             ADD 1     TO CIABP-REJECT-INS-CLAIMS                  00166600
02470          ELSE                                                     00166700
02471            IF CIABP-HDR-PROFESSIONAL                              00166800
02472               ADD 1     TO CIABP-REJECT-PROF-CLAIMS               00166900
02473            END-IF                                                 00167000
02474          END-IF                                                   00167100
02475                                                                   00167200
144300*** R14.5 E_34072  EXL START                                      00157100
144400     WHEN CIABP-MISMATCH-STAT-CD                                  00165600
144500          PERFORM 4170-WRITE-MISMATCH-FILE  THRU 4170-EXIT        00157300
144600          ADD 1               TO CIABP-MMATCH-CLAIMS              00157600
144700          ADD CIASC-INPUT     TO CIABP-MMATCH-RECS                00157700
144800          ADD CIASC-APPENDED  TO CIABP-MMATCH-APPENDED-RECS       00157800
144900                                                                  00157900
145000          IF CIABP-HDR-INSTITUTIONAL                              00158100
145100             ADD 1     TO CIABP-MMATCH-INS-CLAIMS                 00158200
145200          ELSE                                                    00158300
145300             IF CIABP-HDR-PROFESSIONAL                            00158400
145400                ADD 1     TO CIABP-MMATCH-PROF-CLAIMS             00158500
145500             END-IF                                               00158600
145600          END-IF                                                  00158700
145700*** R14.5 E_34072  EXL END                                        00158800
145800     END-EVALUATE.                                                00167300
145900                                                                  00167400
184900                                                                  ITCP9104
179200 4100-EXIT.  EXIT.                                                ITCP9104
185100                                                                  ITCP9104
02310 /*****************************************************************
185400**  WRITE VALID FILE (ITCDDO01)                                 **ITCP9104
185500******************************************************************ITCP9104
185600 4110-WRITE-VALIDS-FILE.                                          ITCP9104
146700                                                                  00168300
146800*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
146900     MOVE '4110-WRITE-VALIDS-FILE     '  TO WS-PARAGRAPH-NM
183601                                                                  ITCP9104
02333      IF CIABP-HDR-PROFESSIONAL                                    00170380
02333         IF CIABP-PROFESSIONAL                                     00170381
02333            MOVE CIABP-F0-CTR   TO SUBR                            00170382
02333         ELSE                                                      00170383
02333            MOVE CIABP-F1-CTR   TO SUBR                            00170385
              END-IF                                                    00170386
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170390
126210* R12.0 E_19137 DEM START
126300           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00170391
126310* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00170392
02333               WRITE VALID-FILE-RECORD                             00170393
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00170394
                       ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS   00170395
148500*R14.5 PD_57538 SKG START                                         44520000
148600                 SET  WS-VALID-FILE-I        TO TRUE              00190100
148700                 PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT    00190100
148800*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170396
              END-PERFORM                                               00170397
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170398
02333            UNTIL SUB1 > CIABP-E6-CTR                              00170399
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00170400
02333               WRITE VALID-FILE-RECORD                             00170402
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00170403
149600*R14.5 PD_57538 SKG START                                         44520000
149700              SET  WS-VALID-FILE-I        TO TRUE                 00190100
149800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
149900*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00170406
                       CONTINUE                                         00170407
                    ELSE                                                00170408
                       ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS   00170406
02333               END-IF                                              00170407
02333            END-IF                                                 00170408
02333         END-PERFORM                                               00170409
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170410
02333            UNTIL SUB1 > SUBR                                      00170411
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00170412
02333               WRITE VALID-FILE-RECORD                             00170413
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00170414
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170415
151300*R14.5 PD_57538 SKG START                                         44520000
151400              SET  WS-VALID-FILE-I        TO TRUE                 00190100
151500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
151600*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170416
              END-PERFORM                                               00170417
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170418
02333            UNTIL SUB1 > SUBR                                      00170419
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00170420
02333               WRITE VALID-FILE-RECORD                             00170421
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00170422
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170423
152500*R14.5 PD_57538 SKG START                                         44520000
152600              SET  WS-VALID-FILE-I        TO TRUE                 00190100
152700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
152800*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170424
              END-PERFORM                                               00170425
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170426
02333            UNTIL SUB1 > SUBR                                      00170427
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00170428
02333               WRITE VALID-FILE-RECORD                             00170429
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00170430
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170431
153700*R14.5 PD_57538 SKG START                                         44520000
153800              SET  WS-VALID-FILE-I        TO TRUE                 00190100
153900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
154000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170432
              END-PERFORM                                               00170433
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00170434
02333            WRITE VALID-FILE-RECORD                                00170435
02333                  FROM CIASC-G0-SLOT                               00170436
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170437
154700*R14.5 PD_57538 SKG START                                         44520000
154800           SET  WS-VALID-FILE-I        TO TRUE                    00190100
154900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
155000*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00170438
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00170439
02333            WRITE VALID-FILE-RECORD                                00170440
02333                  FROM CIASC-X0-SLOT                               00170441
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170442
155600*R14.5 PD_57538 SKG START                                         44520000
155700           SET  WS-VALID-FILE-I        TO TRUE                    00190100
155800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
155900*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00170443
183700                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00170445
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170449
131810* R12.0 E_19137 DEM START
131900           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00170451
131910* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00170451
02333               WRITE VALID-FILE-RECORD                             00170452
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00170453
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170454
157100*R14.5 PD_57538 SKG START                                         44520000
157200              SET  WS-VALID-FILE-I        TO TRUE                 00190100
157300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
157400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170455
              END-PERFORM                                               00170456
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170457
02333            UNTIL SUB1 > CIABP-46-CTR                              00170458
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00170459
02333               WRITE VALID-FILE-RECORD                             00170461
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00170462
158200*R14.5 PD_57538 SKG START                                         44520000
158300              SET  WS-VALID-FILE-I        TO TRUE                 00190100
158400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
158500*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00170466
                       CONTINUE                                         00170467
                    ELSE                                                00170468
                       ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS   00170464
02333               END-IF                                              00170465
02333            END-IF                                                 00170466
02333         END-PERFORM                                               00170467
133800*R12.0 E_19137 DSL BEGIN                                          00170481
133810        IF CIASC-47-SLOT(1:1) > SPACE                             00170481
133900           WRITE VALID-FILE-RECORD                                00170482
134000                 FROM CIASC-47-SLOT                               00170483
134100           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170484
159800*R14.5 PD_57538 SKG START                                         44520000
159900           SET  WS-VALID-FILE-I        TO TRUE                    00190100
160000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
160100*R14.5 PD_57538 SKG END                                           44520000
134200        END-IF                                                    00170485
134201*R12.0 E_19137 DSL END                                            00170481
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00170468
02333            WRITE VALID-FILE-RECORD                                00170469
02333                  FROM CIASC-50-SLOT                               00170470
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170471
160800*R14.5 PD_57538 SKG START                                         44520000
160900           SET  WS-VALID-FILE-I        TO TRUE                    00190100
161000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
161100*R14.5 PD_57538 SKG END                                           44520000
184110        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170473
02333            UNTIL SUB1 > CIABP-60-CTR                              00170474
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00170475
02333               WRITE VALID-FILE-RECORD                             00170476
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00170477
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170478
161900*R14.5 PD_57538 SKG START                                         44520000
162000              SET  WS-VALID-FILE-I        TO TRUE                 00190100
162100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
162200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170479
              END-PERFORM                                               00170480
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170481
02333            UNTIL SUB1 > CIABP-60-CTR                              00170482
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00170483
02333               WRITE VALID-FILE-RECORD                             00170484
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00170485
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170486
163100*R14.5 PD_57538 SKG START                                         44520000
163200              SET  WS-VALID-FILE-I        TO TRUE                 00190100
163300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
163400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170487
              END-PERFORM                                               00170488
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00170489
02333            UNTIL SUB1 > CIABP-60-CTR                              00170490
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00170491
02333               WRITE VALID-FILE-RECORD                             00170492
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00170493
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170494
164300*R14.5 PD_57538 SKG START                                         44520000
164400              SET  WS-VALID-FILE-I        TO TRUE                 00190100
164500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
164600*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00170495
              END-PERFORM                                               00170496
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00170497
02333            WRITE VALID-FILE-RECORD                                00170498
02333                  FROM CIASC-70-SLOT                               00170499
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00170500
165300*R14.5 PD_57538 SKG START                                         44520000
165400              SET  WS-VALID-FILE-I        TO TRUE                 00190100
165500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
165600*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00170501
137200* R12.0 E_19137 DEM START
137300        IF CIASC-71-SLOT(1:1) > SPACE                             00170510
137400           WRITE VALID-FILE-RECORD                                00170511
137500                 FROM CIASC-71-SLOT                               00170512
137600           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170513
166300*R14.5 PD_57538 SKG START                                         44520000
166400           SET  WS-VALID-FILE-I        TO TRUE                    00190100
166500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
166600*R14.5 PD_57538 SKG END                                           44520000
137700        END-IF                                                    00170514
137800        IF CIASC-72-SLOT(1:1) > SPACE                             00170510
137900           WRITE VALID-FILE-RECORD                                00170511
138000                 FROM CIASC-72-SLOT                               00170512
138100           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170513
167200*R14.5 PD_57538 SKG START                                         44520000
167300           SET  WS-VALID-FILE-I        TO TRUE                    00190100
167400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
167500*R14.5 PD_57538 SKG END                                           44520000
138200        END-IF                                                    00170514
138300        IF CIASC-73-SLOT(1:1) > SPACE                             00170510
138400           WRITE VALID-FILE-RECORD                                00170511
138500                 FROM CIASC-73-SLOT                               00170512
138600           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170513
168100*R14.5 PD_57538 SKG START                                         44520000
168200           SET  WS-VALID-FILE-I        TO TRUE                    00190100
168300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
168400*R14.5 PD_57538 SKG END                                           44520000
138700        END-IF                                                    00170514
138800        IF CIASC-74-SLOT(1:1) > SPACE                             00170510
138900           WRITE VALID-FILE-RECORD                                00170511
139000                 FROM CIASC-74-SLOT                               00170512
139100           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170513
169000*R14.5 PD_57538 SKG START                                         44520000
169100           SET  WS-VALID-FILE-I        TO TRUE                    00190100
169200           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
169300*R14.5 PD_57538 SKG END                                           44520000
139200        END-IF                                                    00170514
139300        IF CIASC-75-SLOT(1:1) > SPACE                             00170510
139400           WRITE VALID-FILE-RECORD                                00170511
139500                 FROM CIASC-75-SLOT                               00170512
139600           ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170513
169900*R14.5 PD_57538 SKG START                                         44520000
170000           SET  WS-VALID-FILE-I        TO TRUE                    00190100
170100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
170200*R14.5 PD_57538 SKG END                                           44520000
139700        END-IF                                                    00170514
139701* R12.0 E_19137 END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00170502
02333            WRITE VALID-FILE-RECORD                                00170503
02333                  FROM CIASC-80-SLOT                               00170504
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170505
170900*R14.5 PD_57538 SKG START                                         44520000
171000           SET  WS-VALID-FILE-I        TO TRUE                    00190100
171100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
171200*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00170506
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00170507
02333            WRITE VALID-FILE-RECORD                                00170508
02333                  FROM CIASC-90-SLOT                               00170509
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00170510
171800*R14.5 PD_57538 SKG START                                         44520000
171900           SET  WS-VALID-FILE-I        TO TRUE                    00190100
172000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
172100*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00170511
184500                                                                  ITCP9104
187700 4110-EXIT.  EXIT.                                                ITCP9104
185000                                                                  ITCP9104
02337 /*****************************************************************
188100**  WRITE INVALID FILE (ITCDDO02)                               **ITCP9104
188200******************************************************************ITCP9104
188300 4120-WRITE-INVALIDS-FILE.                                        ITCP9104
191900                                                                  ITCP9104
173100*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
173200     MOVE '4120-WRITE-INVALIDS-FILE   '  TO WS-PARAGRAPH-NM
173300                                                                  00171100
02333      IF CIABP-HDR-PROFESSIONAL                                    00172520
02333         IF CIABP-PROFESSIONAL                                     00172530
02333            MOVE CIABP-F0-CTR   TO SUBR                            00172540
02333         ELSE                                                      00172550
02333            MOVE CIABP-F1-CTR   TO SUBR                            00172560
              END-IF                                                    00172570
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172580
139727* R12.0 E_19137 DEM START
139728           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00172590
139729* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00172591
02333               WRITE INVALID-FILE-RECORD                           00172592
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00172593
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172594
174800*R14.5 PD_57538 SKG START                                         44520000
174900              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
175000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
175100*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172595
              END-PERFORM                                               00172596
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172597
02333            UNTIL SUB1 > CIABP-E6-CTR                              00172598
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00172599
02333               WRITE INVALID-FILE-RECORD                           00172601
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00172602
175900*R14.5 PD_57538 SKG START                                         44520000
176000              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
176100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
176200*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00172607
                       CONTINUE                                         00172608
                    ELSE                                                00172609
                       ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS 00172604
02333               END-IF                                              00172605
02333            END-IF                                                 00172606
02333         END-PERFORM                                               00172607
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172608
02333            UNTIL SUB1 > SUBR                                      00172609
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00172610
02333               WRITE INVALID-FILE-RECORD                           00172611
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00172612
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172613
177600*R14.5 PD_57538 SKG START                                         44520000
177700              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
177800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
177900*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172614
              END-PERFORM                                               00172615
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172616
02333            UNTIL SUB1 > SUBR                                      00172617
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00172618
02333               WRITE INVALID-FILE-RECORD                           00172619
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00172620
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172621
178800*R14.5 PD_57538 SKG START                                         44520000
178900              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
179000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
179100*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172622
              END-PERFORM                                               00172623
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172624
02333            UNTIL SUB1 > SUBR                                      00172625
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00172626
02333               WRITE INVALID-FILE-RECORD                           00172627
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00172628
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172629
180000*R14.5 PD_57538 SKG START                                         44520000
180100              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
180200              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
180300*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172630
              END-PERFORM                                               00172631
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00172632
02333            WRITE INVALID-FILE-RECORD                              00172633
02333                  FROM CIASC-G0-SLOT                               00172634
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172635
181000*R14.5 PD_57538 SKG START                                         44520000
181100           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
181200           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
181300*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00172636
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00172637
02333            WRITE INVALID-FILE-RECORD                              00172638
02333                  FROM CIASC-X0-SLOT                               00172639
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172640
181900*R14.5 PD_57538 SKG START                                         44520000
182000           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
182100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
182200*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00172641
192700                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00172643
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172644
145210* R12.0 E_19137 DEM START
145300           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00172654
145310* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00172646
02333               WRITE INVALID-FILE-RECORD                           00172647
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00172648
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172649
183400*R14.5 PD_57538 SKG START                                         44520000
183500              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
183600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
183700*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172650
              END-PERFORM                                               00172651
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172652
02333            UNTIL SUB1 > CIABP-46-CTR                              00172653
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00172654
02333               WRITE INVALID-FILE-RECORD                           00172656
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00172657
184500*R14.5 PD_57538 SKG START                                         44520000
184600              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
184700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
184800*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00172669
                       CONTINUE                                         00172670
                    ELSE                                                00172671
                       ADD 1        TO CIABP-INVALID-TOTAL-FILE-RECS    00172659
02333               END-IF                                              00172660
02333            END-IF                                                 00172661
02333         END-PERFORM                                               00172662
147200* R12.0 E_19137 DEM START
147300        IF CIASC-47-SLOT(1:1) > SPACE                             00172681
147400           WRITE INVALID-FILE-RECORD                              00172682
147500                 FROM CIASC-47-SLOT                               00172683
147600           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
186100*R14.5 PD_57538 SKG START                                         44520000
186200           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
186300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
186400*R14.5 PD_57538 SKG END                                           44520000
147700        END-IF                                                    00172685
147701* R12.0 E_19137 END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00172663
02333            WRITE INVALID-FILE-RECORD                              00172664
02333                  FROM CIASC-50-SLOT                               00172665
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172666
187100*R14.5 PD_57538 SKG START                                         44520000
187200           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
187300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
187400*R14.5 PD_57538 SKG END                                           44520000
193110        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172668
02333            UNTIL SUB1 > CIABP-60-CTR                              00172669
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00172670
02333               WRITE INVALID-FILE-RECORD                           00172671
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00172672
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172673
188200*R14.5 PD_57538 SKG START                                         44520000
188300              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
188400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
188500*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172674
              END-PERFORM                                               00172675
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172676
02333            UNTIL SUB1 > CIABP-60-CTR                              00172677
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00172678
02333               WRITE INVALID-FILE-RECORD                           00172679
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00172680
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172681
189400*R14.5 PD_57538 SKG START                                         44520000
189500              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
189600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
189700*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172682
              END-PERFORM                                               00172683
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00172684
02333            UNTIL SUB1 > CIABP-60-CTR                              00172685
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00172686
02333               WRITE INVALID-FILE-RECORD                           00172687
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00172688
                    ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS    00172689
190600*R14.5 PD_57538 SKG START                                         44520000
190700              SET  WS-INVALID-FILE-I      TO TRUE                 00190100
190800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
190900*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00172690
              END-PERFORM                                               00172691
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00172692
02333            WRITE INVALID-FILE-RECORD                              00172693
02333                  FROM CIASC-70-SLOT                               00172694
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172695
191600*R14.5 PD_57538 SKG START                                         44520000
191700           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
191800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
191900*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00172696
150600* R12.0 E_19137 DEM START
150700        IF CIASC-71-SLOT(1:1) > SPACE                             00172681
150800           WRITE INVALID-FILE-RECORD                              00172682
150900                 FROM CIASC-71-SLOT                               00172683
151000           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
192600*R14.5 PD_57538 SKG START                                         44520000
192700           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
192800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
192900*R14.5 PD_57538 SKG END                                           44520000
151100        END-IF                                                    00172685
151200        IF CIASC-72-SLOT(1:1) > SPACE                             00172681
151300           WRITE INVALID-FILE-RECORD                              00172682
151400                 FROM CIASC-72-SLOT                               00172683
151500           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
193500*R14.5 PD_57538 SKG START                                         44520000
193600           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
193700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
193800*R14.5 PD_57538 SKG END                                           44520000
151600        END-IF                                                    00172685
151700        IF CIASC-73-SLOT(1:1) > SPACE                             00172681
151800           WRITE INVALID-FILE-RECORD                              00172682
151900                 FROM CIASC-73-SLOT                               00172683
152000           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
194400*R14.5 PD_57538 SKG START                                         44520000
194500           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
194600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
194700*R14.5 PD_57538 SKG END                                           44520000
152100        END-IF                                                    00172685
152200        IF CIASC-74-SLOT(1:1) > SPACE                             00172681
152300           WRITE INVALID-FILE-RECORD                              00172682
152400                 FROM CIASC-74-SLOT                               00172683
152500           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
195300*R14.5 PD_57538 SKG START                                         44520000
195400           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
195500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
195600*R14.5 PD_57538 SKG END                                           44520000
152600        END-IF                                                    00172685
152700        IF CIASC-75-SLOT(1:1) > SPACE                             00172681
152800           WRITE INVALID-FILE-RECORD                              00172682
152900                 FROM CIASC-75-SLOT                               00172683
153000           ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172684
196200*R14.5 PD_57538 SKG START                                         44520000
196300           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
196400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
196500*R14.5 PD_57538 SKG END                                           44520000
153100        END-IF                                                    00172685
153101* R12.0 E_19137 END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00172697
02333            WRITE INVALID-FILE-RECORD                              00172698
02333                  FROM CIASC-80-SLOT                               00172699
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172700
197200*R14.5 PD_57538 SKG START                                         44520000
197300           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
197400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
197500*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00172701
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00172702
02333            WRITE INVALID-FILE-RECORD                              00172703
02333                  FROM CIASC-90-SLOT                               00172704
                 ADD 1           TO CIABP-INVALID-TOTAL-FILE-RECS       00172705
198100*R14.5 PD_57538 SKG START                                         44520000
198200           SET  WS-INVALID-FILE-I      TO TRUE                    00190100
198300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
198400*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00172706
193500                                                                  ITCP9104
190400 4120-EXIT.  EXIT.                                                ITCP9104
190500                                                                  ITCP9104
02364 /*****************************************************************
190800**  WRITE REJECT FILE (ITCDDO03)                                **ITCP9104
190900******************************************************************ITCP9104
191000 4130-WRITE-REJECT-FILE.                                          ITCP9104
193700                                                                  ITCP9104
199400*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
199500     MOVE '4130-WRITE-REJECT-FILE     '  TO WS-PARAGRAPH-NM
199600                                                                  00173400
02333      IF CIABP-HDR-PROFESSIONAL                                    00174901
02333         IF CIABP-PROFESSIONAL                                     00174902
02333            MOVE CIABP-F0-CTR   TO SUBR                            00174903
02333         ELSE                                                      00174904
02333            MOVE CIABP-F1-CTR   TO SUBR                            00174905
              END-IF                                                    00174906
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174907
153127* R12.0 E_19137 DEM START
153128           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00174908
153129* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00174909
02333               WRITE REJECT-FILE-RECORD                            00174910
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00174911
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174912
201100*R14.5 PD_57538 SKG START                                         44520000
201200              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
201300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
201400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174913
              END-PERFORM                                               00174914
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174915
02333            UNTIL SUB1 > CIABP-E6-CTR                              00174916
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00174917
02333               WRITE REJECT-FILE-RECORD                            00174919
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00174920
202200*R14.5 PD_57538 SKG START                                         44520000
202300              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
202400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
202500*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00174925
                       CONTINUE                                         00174926
                    ELSE                                                00174927
                       ADD 1           TO CIABP-REJECT-TOTAL-FILE-RECS  00174922
02333               END-IF                                              00174923
02333            END-IF                                                 00174924
02333         END-PERFORM                                               00174925
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174926
02333            UNTIL SUB1 > SUBR                                      00174927
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00174928
02333               WRITE REJECT-FILE-RECORD                            00174929
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00174930
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174931
203900*R14.5 PD_57538 SKG START                                         44520000
204000              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
204100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
204200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174932
              END-PERFORM                                               00174933
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174934
02333            UNTIL SUB1 > SUBR                                      00174935
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00174936
02333               WRITE REJECT-FILE-RECORD                            00174937
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00174938
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174939
205100*R14.5 PD_57538 SKG START                                         44520000
205200              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
205300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
205400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174940
              END-PERFORM                                               00174941
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174942
02333            UNTIL SUB1 > SUBR                                      00174943
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00174944
02333               WRITE REJECT-FILE-RECORD                            00174945
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00174946
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174947
206300*R14.5 PD_57538 SKG START                                         44520000
206400              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
206500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
206600*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174948
              END-PERFORM                                               00174949
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00174950
02333            WRITE REJECT-FILE-RECORD                               00174951
02333                  FROM CIASC-G0-SLOT                               00174952
                 ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00174953
207300*R14.5 PD_57538 SKG START                                         44520000
207400           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
207500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
207600*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00174954
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00174955
02333            WRITE REJECT-FILE-RECORD                               00174956
02333                  FROM CIASC-X0-SLOT                               00174957
                 ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00174958
208200*R14.5 PD_57538 SKG START                                         44520000
208300           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
208400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
208500*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00174959
194500                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00174961
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174962
158610* R12.0 E_19137 DEM START
158700           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00174972
158710* R12.0 E_19137 DEM START
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00174964
02333               WRITE REJECT-FILE-RECORD                            00174965
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00174966
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174967
209700*R14.5 PD_57538 SKG START                                         44520000
209800              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
209900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
210000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174968
              END-PERFORM                                               00174969
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174970
02333            UNTIL SUB1 > CIABP-46-CTR                              00174971
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00174972
02333               WRITE REJECT-FILE-RECORD                            00174974
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00174975
210800*R14.5 PD_57538 SKG START                                         44520000
210900              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
211000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
211100*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00174987
                       CONTINUE                                         00174988
                    ELSE                                                00174989
                       ADD 1        TO CIABP-REJECT-TOTAL-FILE-RECS     00174977
02333               END-IF                                              00174978
02333            END-IF                                                 00174979
02333         END-PERFORM                                               00174980
160600* R12.0 E_19137 DEM START
160700        IF CIASC-47-SLOT(1:1) > SPACE                             00174999
160800           WRITE REJECT-FILE-RECORD                               00175000
160900                 FROM CIASC-47-SLOT                               00175001
161000           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
212400*R14.5 PD_57538 SKG START                                         44520000
212500           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
212600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
212700*R14.5 PD_57538 SKG END                                           44520000
161100        END-IF                                                    00175003
161101* R12.0 E_19137 END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00174981
02333            WRITE REJECT-FILE-RECORD                               00174982
02333                  FROM CIASC-50-SLOT                               00174983
194900           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    ITCP9104
213400*R14.5 PD_57538 SKG START                                         44520000
213500           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
213600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
213700*R14.5 PD_57538 SKG END                                           44520000
194910        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174986
02333            UNTIL SUB1 > CIABP-60-CTR                              00174987
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00174988
02333               WRITE REJECT-FILE-RECORD                            00174989
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00174990
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174991
214500*R14.5 PD_57538 SKG START                                         44520000
214600              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
214700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
214800*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00174992
              END-PERFORM                                               00174993
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00174994
02333            UNTIL SUB1 > CIABP-60-CTR                              00174995
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00174996
02333               WRITE REJECT-FILE-RECORD                            00174997
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00174998
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00174999
215700*R14.5 PD_57538 SKG START                                         44520000
215800              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
215900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
216000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175000
              END-PERFORM                                               00175001
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175002
02333            UNTIL SUB1 > CIABP-60-CTR                              00175003
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00175004
02333               WRITE REJECT-FILE-RECORD                            00175005
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00175006
                    ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS 00175007
216900*R14.5 PD_57538 SKG START                                         44520000
217000              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
217100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
217200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175008
              END-PERFORM                                               00175009
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00175010
02333            WRITE REJECT-FILE-RECORD                               00175011
02333                  FROM CIASC-70-SLOT                               00175012
                 ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175013
217900*R14.5 PD_57538 SKG START                                         44520000
218000              SET  WS-REJECT-FILE-I       TO TRUE                 00190100
218100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
218200*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175014
164000* R12.0 E_19137 DEM START
164100        IF CIASC-71-SLOT(1:1) > SPACE                             00174999
164200           WRITE REJECT-FILE-RECORD                               00175000
164300                 FROM CIASC-71-SLOT                               00175001
164400           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
218900*R14.5 PD_57538 SKG START                                         44520000
219000           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
219100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
219200*R14.5 PD_57538 SKG END                                           44520000
164500        END-IF                                                    00175003
164600        IF CIASC-72-SLOT(1:1) > SPACE                             00174999
164700           WRITE REJECT-FILE-RECORD                               00175000
164800                 FROM CIASC-72-SLOT                               00175001
164900           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
219800*R14.5 PD_57538 SKG START                                         44520000
219900           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
220000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
220100*R14.5 PD_57538 SKG END                                           44520000
165000        END-IF                                                    00175003
165100        IF CIASC-73-SLOT(1:1) > SPACE                             00174999
165200           WRITE REJECT-FILE-RECORD                               00175000
165300                 FROM CIASC-73-SLOT                               00175001
165400           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
220700*R14.5 PD_57538 SKG START                                         44520000
220800           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
220900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
221000*R14.5 PD_57538 SKG END                                           44520000
165500        END-IF                                                    00175003
165600        IF CIASC-74-SLOT(1:1) > SPACE                             00174999
165700           WRITE REJECT-FILE-RECORD                               00175000
165800                 FROM CIASC-74-SLOT                               00175001
165900           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
221600*R14.5 PD_57538 SKG START                                         44520000
221700           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
221800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
221900*R14.5 PD_57538 SKG END                                           44520000
166000        END-IF                                                    00175003
166100        IF CIASC-75-SLOT(1:1) > SPACE                             00174999
166200           WRITE REJECT-FILE-RECORD                               00175000
166300                 FROM CIASC-75-SLOT                               00175001
166400           ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175002
222500*R14.5 PD_57538 SKG START                                         44520000
222600           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
222700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
222800*R14.5 PD_57538 SKG END                                           44520000
166500        END-IF                                                    00175003
166501* R12.0 E_19137 END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00175015
02333            WRITE REJECT-FILE-RECORD                               00175016
02333                  FROM CIASC-80-SLOT                               00175017
                 ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175018
223500*R14.5 PD_57538 SKG START                                         44520000
223600           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
223700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
223800*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175019
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00175020
02333            WRITE REJECT-FILE-RECORD                               00175021
02333                  FROM CIASC-90-SLOT                               00175022
                 ADD 1               TO CIABP-REJECT-TOTAL-FILE-RECS    00175023
224400*R14.5 PD_57538 SKG START                                         44520000
224500           SET  WS-REJECT-FILE-I       TO TRUE                    00190100
224600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
224700*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00175024
195300                                                                  ITCP9104
193100 4130-EXIT.  EXIT.                                                ITCP9104
193200                                                                  ITCP9104
02391 /*****************************************************************
193500**  WRITE DUPLICATE/INVALID RELEASE FILE (ITCDDO07)             **ITCP9104
193600******************************************************************ITCP9104
193700 4140-WRITE-RELEASE-FILE.                                         ITCP9104
193800                                                                  ITCP9104
225700*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
225800     MOVE '4140-WRITE-RELEASE-FILE    '  TO WS-PARAGRAPH-NM
225900                                                                  00175700
02395      IF  CIABP-SF-IO-NO-3510-ISSUED                               00175702
02395      AND NOT CIABP-FOUND-F0-F1                                    00175711
               CONTINUE                                                 00175712
194100     ELSE                                                         ITCP9104
               GO TO 4140-WRITE-FROM-WS-HOLD.                           00175714
194600                                                                  ITCP9104
02333      IF CIABP-HDR-PROFESSIONAL                                    00175716
02333         IF CIABP-PROFESSIONAL                                     00175717
02333            MOVE CIABP-F0-CTR   TO SUBR                            00175718
02333         ELSE                                                      00175719
02333            MOVE CIABP-F1-CTR   TO SUBR                            00175720
              END-IF                                                    00175721
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175722
167010* R12.0 E_19137 DEM START
167100           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00175723
167110* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00175724
02333               WRITE RELEASE-FILE-RECORD                           00175725
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00175726
                    ADD 1             TO CIABP-RELEASE-TOTAL-FILE-RECS  00175727
228000*R14.5 PD_57538 SKG START                                         44520000
228100              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
228200              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
228300*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175728
              END-PERFORM                                               00175729
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175730
02333            UNTIL SUB1 > CIABP-E6-CTR                              00175731
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00175732
02333               WRITE RELEASE-FILE-RECORD                           00175734
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00175735
229100*R14.5 PD_57538 SKG START                                         44520000
229200              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
229300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
229400*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00175740
                       CONTINUE                                         00175741
                    ELSE                                                00175742
                       ADD 1           TO CIABP-RELEASE-TOTAL-FILE-RECS 00175737
02333               END-IF                                              00175738
02333            END-IF                                                 00175739
02333         END-PERFORM                                               00175740
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175741
02333            UNTIL SUB1 > SUBR                                      00175742
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00175743
02333               WRITE RELEASE-FILE-RECORD                           00175744
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00175745
                    ADD 1             TO CIABP-RELEASE-TOTAL-FILE-RECS  00175746
230800*R14.5 PD_57538 SKG START                                         44520000
230900              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
231000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
231100*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175747
              END-PERFORM                                               00175748
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175749
02333            UNTIL SUB1 > SUBR                                      00175750
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00175751
02333               WRITE RELEASE-FILE-RECORD                           00175752
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00175753
                    ADD 1             TO CIABP-RELEASE-TOTAL-FILE-RECS  00175754
232000*R14.5 PD_57538 SKG START                                         44520000
232100              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
232200              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
232300*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175755
              END-PERFORM                                               00175756
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175757
02333            UNTIL SUB1 > SUBR                                      00175758
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00175759
02333               WRITE RELEASE-FILE-RECORD                           00175760
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00175761
                    ADD 1            TO CIABP-RELEASE-TOTAL-FILE-RECS   00175762
233200*R14.5 PD_57538 SKG START                                         44520000
233300              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
233400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
233500*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175763
              END-PERFORM                                               00175764
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00175765
02333            WRITE RELEASE-FILE-RECORD                              00175766
02333                  FROM CIASC-G0-SLOT                               00175767
                 ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175768
234200*R14.5 PD_57538 SKG START                                         44520000
234300           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
234400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
234500*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175769
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00175770
02333            WRITE RELEASE-FILE-RECORD                              00175771
02333                  FROM CIASC-X0-SLOT                               00175772
                 ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175773
235100*R14.5 PD_57538 SKG START                                         44520000
235200           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
235300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
235400*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00175774
194800                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00175776
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175777
172610* R12.0 E_19137 DEM START
172700           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00175787
172710* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00175779
02333               WRITE RELEASE-FILE-RECORD                           00175780
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00175781
                    ADD 1           TO CIABP-RELEASE-TOTAL-FILE-RECS    00175782
236600*R14.5 PD_57538 SKG START                                         44520000
236700              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
236800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
236900*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175783
              END-PERFORM                                               00175784
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175785
02333            UNTIL SUB1 > CIABP-46-CTR                              00175786
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00175787
02333               WRITE RELEASE-FILE-RECORD                           00175789
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00175790
237700*R14.5 PD_57538 SKG START                                         44520000
237800              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
237900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
238000*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00175802
                       CONTINUE                                         00175803
                    ELSE                                                00175804
                       ADD 1        TO CIABP-RELEASE-TOTAL-FILE-RECS    00175792
02333               END-IF                                              00175793
02333            END-IF                                                 00175794
02333         END-PERFORM                                               00175795
174600* R12.0 E_19137 DEM START
174700        IF CIASC-47-SLOT(1:1) > SPACE                             00175814
174800           WRITE RELEASE-FILE-RECORD                              00175815
174900                 FROM CIASC-47-SLOT                               00175816
175000           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
239300*R14.5 PD_57538 SKG START                                         44520000
239400           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
239500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
239600*R14.5 PD_57538 SKG END                                           44520000
175100        END-IF                                                    00175818
175101* R12.0 E_19137 END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00175796
02333            WRITE RELEASE-FILE-RECORD                              00175797
02333                  FROM CIASC-50-SLOT                               00175798
                 ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175799
240300*R14.5 PD_57538 SKG START                                         44520000
240400           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
240500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
240600*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175800
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175801
02333            UNTIL SUB1 > CIABP-60-CTR                              00175802
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00175803
02333               WRITE RELEASE-FILE-RECORD                           00175804
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00175805
                    ADD 1             TO CIABP-RELEASE-TOTAL-FILE-RECS  00175806
241400*R14.5 PD_57538 SKG START                                         44520000
241500              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
241600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
241700*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175807
              END-PERFORM                                               00175808
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175809
02333            UNTIL SUB1 > CIABP-60-CTR                              00175810
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00175811
02333               WRITE RELEASE-FILE-RECORD                           00175812
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00175813
242500*R14.5 PD_57538 SKG START                                         44520000
242600              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
242700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
242800*R14.5 PD_57538 SKG END                                           44520000
                    ADD 1             TO CIABP-RELEASE-TOTAL-FILE-RECS  00175814
02333            END-IF                                                 00175815
              END-PERFORM                                               00175816
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00175817
02333            UNTIL SUB1 > CIABP-60-CTR                              00175818
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00175819
02333               WRITE RELEASE-FILE-RECORD                           00175820
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00175821
                    ADD 1           TO CIABP-RELEASE-TOTAL-FILE-RECS    00175822
243800*R14.5 PD_57538 SKG START                                         44520000
243900              SET  WS-RELEASE-FILE-I      TO TRUE                 00190100
244000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
244100*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00175823
              END-PERFORM                                               00175824
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00175825
02333            WRITE RELEASE-FILE-RECORD                              00175826
02333                  FROM CIASC-70-SLOT                               00175827
                 ADD 1              TO CIABP-RELEASE-TOTAL-FILE-RECS    00175828
244800*R14.5 PD_57538 SKG START                                         44520000
244900           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
245000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
245100*R14.5 PD_57538 SKG END                                           44520000
177900        END-IF                                                    00175847
178000* R12.0 E_19137 DEM START
178100        IF CIASC-71-SLOT(1:1) > SPACE                             00175814
178200           WRITE RELEASE-FILE-RECORD                              00175815
178300                 FROM CIASC-71-SLOT                               00175816
178400           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
245800*R14.5 PD_57538 SKG START                                         44520000
245900           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
246000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
246100*R14.5 PD_57538 SKG END                                           44520000
178500        END-IF                                                    00175818
178600        IF CIASC-72-SLOT(1:1) > SPACE                             00175814
178700           WRITE RELEASE-FILE-RECORD                              00175815
178800                 FROM CIASC-72-SLOT                               00175816
178900           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
246700*R14.5 PD_57538 SKG START                                         44520000
246800           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
246900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
247000*R14.5 PD_57538 SKG END                                           44520000
179000        END-IF                                                    00175818
179100        IF CIASC-73-SLOT(1:1) > SPACE                             00175814
179200           WRITE RELEASE-FILE-RECORD                              00175815
179300                 FROM CIASC-73-SLOT                               00175816
179400           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
247600*R14.5 PD_57538 SKG START                                         44520000
247700           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
247800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
247900*R14.5 PD_57538 SKG END                                           44520000
179500        END-IF                                                    00175818
179600        IF CIASC-74-SLOT(1:1) > SPACE                             00175814
179700           WRITE RELEASE-FILE-RECORD                              00175815
179800                 FROM CIASC-74-SLOT                               00175816
179900           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
248500*R14.5 PD_57538 SKG START                                         44520000
248600           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
248700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
248800*R14.5 PD_57538 SKG END                                           44520000
180000        END-IF                                                    00175818
180100        IF CIASC-75-SLOT(1:1) > SPACE                             00175814
180200           WRITE RELEASE-FILE-RECORD                              00175815
180300                 FROM CIASC-75-SLOT                               00175816
180400           ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175817
249400*R14.5 PD_57538 SKG START                                         44520000
249500           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
249600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
249700*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175829
180501* R12.0 E_19137 END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00175830
02333            WRITE RELEASE-FILE-RECORD                              00175831
02333                  FROM CIASC-80-SLOT                               00175832
                 ADD 1              TO CIABP-RELEASE-TOTAL-FILE-RECS    00175833
250400*R14.5 PD_57538 SKG START                                         44520000
250500           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
250600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
250700*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00175834
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00175835
02333            WRITE RELEASE-FILE-RECORD                              00175836
02333                  FROM CIASC-90-SLOT                               00175837
                 ADD 1               TO CIABP-RELEASE-TOTAL-FILE-RECS   00175838
251300*R14.5 PD_57538 SKG START                                         44520000
251400           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
251500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
251600*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00175839
02395                                                                   00175840
02395      GO TO 4140-EXIT.                                             00175854
02395                                                                   00175855
       4140-WRITE-FROM-WS-HOLD.                                         00175856
02395                                                                   00175857
252300*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
252400     MOVE '4140-WRITE-FROM-WS-HOLD    '  TO WS-PARAGRAPH-NM
252500
02418      IF WS-HOLD-REC-TYPE (1) = ('05' OR 'A5')                     00175858
02419         MOVE CIASC-STACKED-CLAIM-ITEM (1)                         00175859
02420                                  TO WS-HOLD-RECORD (1)            00175860
02421      END-IF.                                                      00175861
                                                                        00175862
           PERFORM VARYING SUB1 FROM 1 BY 1                             00175863
180523* R12.0 E_19137 DEM START
180524        UNTIL SUB1 > SFCNT-MAX-INST-RECS                          00175869
180525* R12.0 E_19137 END
02395         IF WS-HOLD-REC-TYPE (SUB1)    > SPACE                     00175865
02423            WRITE RELEASE-FILE-RECORD FROM                         00175869
02424                       WS-HOLD-RECORD (SUB1)                       00175870
253800*R14.5 PD_57538 SKG START                                         44520000
253900           SET  WS-RELEASE-FILE-I      TO TRUE                    00190100
254000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
254100*R14.5 PD_57538 SKG END                                           44520000
02395            IF WS-HOLD-REC-TYPE (SUB1) = 'E6' OR '46'              00175874
02333               IF WS-HOLD-RECORD (SUB1) (26:1) = 'N'               00175880
                       CONTINUE                                         00175881
                    ELSE                                                00175882
02426                  ADD 1        TO CIABP-RELEASE-TOTAL-FILE-RECS    00175883
                    END-IF                                              00175884
                 ELSE                                                   00175885
02426               ADD 1           TO CIABP-RELEASE-TOTAL-FILE-RECS    00175887
02427            END-IF                                                 00175873
02395         ELSE                                                      00175874
181500** R11.1 TD_57006 START
181600            IF PREV-CIABP-CLM-TYPE = '20' OR '40'
181700               MOVE SUB1          TO PREV-CIABP-F0-CTR
181800                                     PREV-CIABP-E6-CTR
181900            ELSE
182000               MOVE SUB1          TO PREV-CIABP-60-CTR
182100                                     PREV-CIABP-46-CTR
182200            END-IF
182300** R11.1 TD_57006 END
182310* R12.0 E_19137 DEM START
182400            MOVE SFCNT-MAX-INST-RECS TO SUB1                      00175890
182410* R12.0 E_19137 END
02395         END-IF                                                    00175900
02395      END-PERFORM.                                                 00175910
197100                                                                  ITCP9104
197200 4140-EXIT.  EXIT.                                                ITCP9104
197300                                                                  ITCP9104
02435 /*****************************************************************
197600**  WRITE VALID ADJUSTMENT FILE (ITCDDO08)                      **ITCP9104
197700******************************************************************ITCP9104
197800 4150-WRITE-VALID-ADJ-FILE.                                       ITCP9104
257300                                                                  00179700
257400*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
257500     MOVE '4150-WRITE-VALID-ADJ-FILE  '  TO WS-PARAGRAPH-NM
197900                                                                  ITCP9104
02333      IF CIABP-HDR-PROFESSIONAL                                    00181220
02333         IF CIABP-PROFESSIONAL                                     00181230
02333            MOVE CIABP-F0-CTR   TO SUBR                            00181240
02333         ELSE                                                      00181250
02333            MOVE CIABP-F1-CTR   TO SUBR                            00181260
              END-IF                                                    00181270
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181280
184110* R12.0 E_19137 DEM START
184200           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00181290
184210* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00181291
02333               WRITE VALID-ADJ-FILE-RECORD                         00181292
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00181293
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181294
259100*R14.5 PD_57538 SKG START                                         44520000
259200              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
259300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
259400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181295
              END-PERFORM                                               00181296
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181297
02333            UNTIL SUB1 > CIABP-E6-CTR                              00181298
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00181299
02333               WRITE VALID-ADJ-FILE-RECORD                         00181301
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00181302
260200*R14.5 PD_57538 SKG START                                         44520000
260300              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
260400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
260500*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00181307
                       CONTINUE                                         00181308
                    ELSE                                                00181309
                       ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS     00181304
02333               END-IF                                              00181305
02333            END-IF                                                 00181306
02333         END-PERFORM                                               00181307
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181308
02333            UNTIL SUB1 > SUBR                                      00181309
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00181310
02333               WRITE VALID-ADJ-FILE-RECORD                         00181311
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00181312
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181313
261900*R14.5 PD_57538 SKG START                                         44520000
262000              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
262100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
262200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181314
              END-PERFORM                                               00181315
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181316
02333            UNTIL SUB1 > SUBR                                      00181317
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00181318
02333               WRITE VALID-ADJ-FILE-RECORD                         00181319
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00181320
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181321
263100*R14.5 PD_57538 SKG START                                         44520000
263200              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
263300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
263400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181322
              END-PERFORM                                               00181323
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181324
02333            UNTIL SUB1 > SUBR                                      00181325
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00181326
02333               WRITE VALID-ADJ-FILE-RECORD                         00181327
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00181328
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181329
264300*R14.5 PD_57538 SKG START                                         44520000
264400              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
264500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
264600*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181330
              END-PERFORM                                               00181331
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00181332
02333            WRITE VALID-ADJ-FILE-RECORD                            00181333
02333                  FROM CIASC-G0-SLOT                               00181334
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181335
265300*R14.5 PD_57538 SKG START                                         44520000
265400           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
265500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
265600*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00181336
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00181337
02333            WRITE VALID-ADJ-FILE-RECORD                            00181338
02333                  FROM CIASC-X0-SLOT                               00181339
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181340
266200*R14.5 PD_57538 SKG START                                         44520000
266300           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
266400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
266500*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00181341
198900                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00181343
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181344
189710* R12.0 E_19137 DEM START
189800           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00181354
189810* R12.0 E_19137 DEM END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00181346
02333               WRITE VALID-ADJ-FILE-RECORD                         00181347
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00181348
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181349
267700*R14.5 PD_57538 SKG START                                         44520000
267800              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
267900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
268000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181350
              END-PERFORM                                               00181351
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181352
02333            UNTIL SUB1 > CIABP-46-CTR                              00181353
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00181354
02333               WRITE VALID-ADJ-FILE-RECORD                         00181356
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00181357
268800*R14.5 PD_57538 SKG START                                         44520000
268900              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
269000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
269100*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00181369
                       CONTINUE                                         00181370
                    ELSE                                                00181371
                       ADD 1        TO CIABP-ADJ-TOTAL-FILE-RECS        00181359
02333               END-IF                                              00181360
02333            END-IF                                                 00181361
02333         END-PERFORM                                               00181362
191700* R12.0 E_19137 DEM START
191800        IF CIASC-47-SLOT(1:1) > SPACE                             00181381
191900           WRITE VALID-ADJ-FILE-RECORD                            00181382
192000                 FROM CIASC-47-SLOT                               00181383
192100           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
270400*R14.5 PD_57538 SKG START                                         44520000
270500           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
270600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
270700*R14.5 PD_57538 SKG END                                           44520000
192200        END-IF                                                    00181385
192201* R12.0 E_19137 END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00181363
02333            WRITE VALID-ADJ-FILE-RECORD                            00181364
02333                  FROM CIASC-50-SLOT                               00181365
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181366
271400*R14.5 PD_57538 SKG START                                         44520000
271500           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
271600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
271700*R14.5 PD_57538 SKG END                                           44520000
199500        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181368
02333            UNTIL SUB1 > CIABP-60-CTR                              00181369
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00181370
02333               WRITE VALID-ADJ-FILE-RECORD                         00181371
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00181372
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181373
272500*R14.5 PD_57538 SKG START                                         44520000
272600              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
272700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
272800*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181374
              END-PERFORM                                               00181375
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181376
02333            UNTIL SUB1 > CIABP-60-CTR                              00181377
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00181378
02333               WRITE VALID-ADJ-FILE-RECORD                         00181379
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00181380
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181381
273700*R14.5 PD_57538 SKG START                                         44520000
273800              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
273900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
274000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181382
              END-PERFORM                                               00181383
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00181384
02333            UNTIL SUB1 > CIABP-60-CTR                              00181385
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00181386
02333               WRITE VALID-ADJ-FILE-RECORD                         00181387
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00181388
                    ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS        00181389
274900*R14.5 PD_57538 SKG START                                         44520000
275000              SET  WS-VALID-ADJ-FILE-I    TO TRUE                 00190100
275100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
275200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00181390
              END-PERFORM                                               00181391
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00181392
02333            WRITE VALID-ADJ-FILE-RECORD                            00181393
02333                  FROM CIASC-70-SLOT                               00181394
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181395
275900*R14.5 PD_57538 SKG START                                         44520000
276000           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
276100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
276200*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00181396
195100* R12.0 E_19137 DEM START
195200        IF CIASC-71-SLOT(1:1) > SPACE                             00181381
195300           WRITE VALID-ADJ-FILE-RECORD                            00181382
195400                 FROM CIASC-71-SLOT                               00181383
195500           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
276900*R14.5 PD_57538 SKG START                                         44520000
277000           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
277100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
277200*R14.5 PD_57538 SKG END                                           44520000
195600        END-IF                                                    00181385
195700        IF CIASC-72-SLOT(1:1) > SPACE                             00181381
195800           WRITE VALID-ADJ-FILE-RECORD                            00181382
195900                 FROM CIASC-72-SLOT                               00181383
196000           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
277800*R14.5 PD_57538 SKG START                                         44520000
277900           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
278000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
278100*R14.5 PD_57538 SKG END                                           44520000
196100        END-IF                                                    00181385
196200        IF CIASC-73-SLOT(1:1) > SPACE                             00181381
196300           WRITE VALID-ADJ-FILE-RECORD                            00181382
196400                 FROM CIASC-73-SLOT                               00181383
196500           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
278700*R14.5 PD_57538 SKG START                                         44520000
278800           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
278900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
279000*R14.5 PD_57538 SKG END                                           44520000
196600        END-IF                                                    00181385
196700        IF CIASC-74-SLOT(1:1) > SPACE                             00181381
196800           WRITE VALID-ADJ-FILE-RECORD                            00181382
196900                 FROM CIASC-74-SLOT                               00181383
197000           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
279600*R14.5 PD_57538 SKG START                                         44520000
279700           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
279800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
279900*R14.5 PD_57538 SKG END                                           44520000
197100        END-IF                                                    00181385
197200        IF CIASC-75-SLOT(1:1) > SPACE                             00181381
197300           WRITE VALID-ADJ-FILE-RECORD                            00181382
197400                 FROM CIASC-75-SLOT                               00181383
197500           ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181384
280500*R14.5 PD_57538 SKG START                                         44520000
280600           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
280700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
280800*R14.5 PD_57538 SKG END                                           44520000
197600        END-IF                                                    00181385
197601* R12.0 E_19137 END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00181397
02333            WRITE VALID-ADJ-FILE-RECORD                            00181398
02333                  FROM CIASC-80-SLOT                               00181399
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181400
281500*R14.5 PD_57538 SKG START                                         44520000
281600           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
281700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
281800*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00181401
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00181402
02333            WRITE VALID-ADJ-FILE-RECORD                            00181403
02333                  FROM CIASC-90-SLOT                               00181404
                 ADD 1           TO CIABP-ADJ-TOTAL-FILE-RECS           00181405
282400*R14.5 PD_57538 SKG START                                         44520000
282500           SET  WS-VALID-ADJ-FILE-I    TO TRUE                    00190100
282600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
282700*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00181406
199800                                                                  ITCP9104
199900 4150-EXIT.  EXIT.                                                ITCP9104
200000                                                                  ITCP9104
02463 /*****************************************************************
207700**  WRITE VALID ELECTRONIC CLAIM FILE (ITCDDO10)                **ITCP9104
207800******************************************************************ITCP9104
207900 4160-WRITE-VALID-ECR-FILE.                                       ITCP9104
208000                                                                  ITCP9104
283700*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
283800     MOVE '4160-WRITE-VALID-ECR-FILE  '  TO WS-PARAGRAPH-NM
283900                                                                  00182000
02333      IF CIABP-HDR-PROFESSIONAL                                    00183520
02333         IF CIABP-PROFESSIONAL                                     00183530
02333            MOVE CIABP-F0-CTR   TO SUBR                            00183540
02333         ELSE                                                      00183550
02333            MOVE CIABP-F1-CTR   TO SUBR                            00183560
              END-IF                                                    00183570
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183580
197627* R12.0 E_19137 DEM START
197628           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00183590
197629* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00183591
02333               WRITE VALID-ECR-FILE-RECORD                         00183592
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00183593
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183594
285400*R14.5 PD_57538 SKG START                                         44520000
285500              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
285600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
285700*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183595
              END-PERFORM                                               00183596
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183597
02333            UNTIL SUB1 > CIABP-E6-CTR                              00183598
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00183599
02333               WRITE VALID-ECR-FILE-RECORD                         00183601
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00183602
286500*R14.5 PD_57538 SKG START                                         44520000
286600              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
286700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
286800*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00183607
                       CONTINUE                                         00183608
                    ELSE                                                00183609
                       ADD 1           TO CIABP-ECR-TOTAL-FILE-RECS     00183604
02333               END-IF                                              00183605
02333            END-IF                                                 00183606
02333         END-PERFORM                                               00183607
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183608
02333            UNTIL SUB1 > SUBR                                      00183609
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00183610
02333               WRITE VALID-ECR-FILE-RECORD                         00183611
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00183612
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183613
288200*R14.5 PD_57538 SKG START                                         44520000
288300              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
288400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
288500*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183614
              END-PERFORM                                               00183615
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183616
02333            UNTIL SUB1 > SUBR                                      00183617
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00183618
02333               WRITE VALID-ECR-FILE-RECORD                         00183619
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00183620
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183621
289400*R14.5 PD_57538 SKG START                                         44520000
289500              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
289600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
289700*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183622
              END-PERFORM                                               00183623
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183624
02333            UNTIL SUB1 > SUBR                                      00183625
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00183626
02333               WRITE VALID-ECR-FILE-RECORD                         00183627
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00183628
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183629
290600*R14.5 PD_57538 SKG START                                         44520000
290700              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
290800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
290900*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183630
              END-PERFORM                                               00183631
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00183632
02333            WRITE VALID-ECR-FILE-RECORD                            00183633
02333                  FROM CIASC-G0-SLOT                               00183634
                 ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183635
291600*R14.5 PD_57538 SKG START                                         44520000
291700           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
291800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
291900*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00183636
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00183637
02333            WRITE VALID-ECR-FILE-RECORD                            00183638
02333                  FROM CIASC-X0-SLOT                               00183639
                 ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183640
292500*R14.5 PD_57538 SKG START                                         44520000
292600           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
292700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
292800*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00183641
209000                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00183643
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183644
203110* R12.0 E_19137 DEM START
203200           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00183654
203210* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00183646
02333               WRITE VALID-ECR-FILE-RECORD                         00183647
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00183648
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183649
294000*R14.5 PD_57538 SKG START                                         44520000
294100              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
294200              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
294300*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183650
              END-PERFORM                                               00183651
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183652
02333            UNTIL SUB1 > CIABP-46-CTR                              00183653
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00183654
02333               WRITE VALID-ECR-FILE-RECORD                         00183656
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00183657
295100*R14.5 PD_57538 SKG START                                         44520000
295200              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
295300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
295400*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00183669
                       CONTINUE                                         00183670
                    ELSE                                                00183671
                       ADD 1        TO CIABP-ECR-TOTAL-FILE-RECS        00183659
02333               END-IF                                              00183660
02333            END-IF                                                 00183661
02333         END-PERFORM                                               00183662
205100* R12.0 E_19137 DEM START
205200        IF CIASC-47-SLOT(1:1) > SPACE                             00183681
205300           WRITE VALID-ECR-FILE-RECORD                            00183682
205400                 FROM CIASC-47-SLOT                               00183683
205500           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183684
296700*R14.5 PD_57538 SKG START                                         44520000
296800           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
296900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
297000*R14.5 PD_57538 SKG END                                           44520000
205600        END-IF                                                    00183685
205601* R12.0 E_19137 DEM END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00183663
02333            WRITE VALID-ECR-FILE-RECORD                            00183664
02333                  FROM CIASC-50-SLOT                               00183665
209500           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       ITCP9104
297700*R14.5 PD_57538 SKG START                                         44520000
297800           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
297900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
298000*R14.5 PD_57538 SKG END                                           44520000
209600        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183668
02333            UNTIL SUB1 > CIABP-60-CTR                              00183669
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00183670
02333               WRITE VALID-ECR-FILE-RECORD                         00183671
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00183672
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183673
298800*R14.5 PD_57538 SKG START                                         44520000
298900              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
299000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
299100*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183674
              END-PERFORM                                               00183675
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183676
02333            UNTIL SUB1 > CIABP-60-CTR                              00183677
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00183678
02333               WRITE VALID-ECR-FILE-RECORD                         00183679
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00183680
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183681
300000*R14.5 PD_57538 SKG START                                         44520000
300100              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
300200              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
300300*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183682
              END-PERFORM                                               00183683
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00183684
02333            UNTIL SUB1 > CIABP-60-CTR                              00183685
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00183686
02333               WRITE VALID-ECR-FILE-RECORD                         00183687
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00183688
                    ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS    00183689
301200*R14.5 PD_57538 SKG START                                         44520000
301300              SET  WS-VALID-ECR-FILE-I    TO TRUE                 00190100
301400              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
301500*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00183690
              END-PERFORM                                               00183691
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00183692
02333            WRITE VALID-ECR-FILE-RECORD                            00183693
02333                  FROM CIASC-70-SLOT                               00183694
208300           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183713
302200*R14.5 PD_57538 SKG START                                         44520000
302300           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
302400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
302500*R14.5 PD_57538 SKG END                                           44520000
208400        END-IF                                                    00183714
208500* R12.0 E_19137 DEM START
208600        IF CIASC-71-SLOT(1:1) > SPACE                             00183681
208700           WRITE VALID-ECR-FILE-RECORD                            00183682
208800                 FROM CIASC-71-SLOT                               00183683
208900           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183684
303200*R14.5 PD_57538 SKG START                                         44520000
303300           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
303400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
303500*R14.5 PD_57538 SKG END                                           44520000
209000        END-IF                                                    00183685
209100        IF CIASC-72-SLOT(1:1) > SPACE                             00183681
209200           WRITE VALID-ECR-FILE-RECORD                            00183682
209300                 FROM CIASC-72-SLOT                               00183683
209400           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183684
304100*R14.5 PD_57538 SKG START                                         44520000
304200           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
304300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
304400*R14.5 PD_57538 SKG END                                           44520000
209500        END-IF                                                    00183685
209600        IF CIASC-73-SLOT(1:1) > SPACE                             00183681
209700           WRITE VALID-ECR-FILE-RECORD                            00183682
209800                 FROM CIASC-73-SLOT                               00183683
209900           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183684
305000*R14.5 PD_57538 SKG START                                         44520000
305100           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
305200           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
305300*R14.5 PD_57538 SKG END                                           44520000
210000        END-IF                                                    00183685
210100        IF CIASC-74-SLOT(1:1) > SPACE                             00183681
210200           WRITE VALID-ECR-FILE-RECORD                            00183682
210300                 FROM CIASC-74-SLOT                               00183683
210400           ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183684
305900*R14.5 PD_57538 SKG START                                         44520000
306000           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
306100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
306200*R14.5 PD_57538 SKG END                                           44520000
210500        END-IF                                                    00183685
210600        IF CIASC-75-SLOT(1:1) > SPACE                             00183681
210700           WRITE VALID-ECR-FILE-RECORD                            00183682
210800                 FROM CIASC-75-SLOT                               00183683
                 ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183695
306800*R14.5 PD_57538 SKG START                                         44520000
306900           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
307000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
307100*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00183696
211001* R12.0 E_19137 DEM END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00183697
02333            WRITE VALID-ECR-FILE-RECORD                            00183698
02333                  FROM CIASC-80-SLOT                               00183699
                 ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183700
307800*R14.5 PD_57538 SKG START                                         44520000
307900           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
308000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
308100*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00183701
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00183702
02333            WRITE VALID-ECR-FILE-RECORD                            00183703
02333                  FROM CIASC-90-SLOT                               00183704
                 ADD 1               TO CIABP-ECR-TOTAL-FILE-RECS       00183705
308700*R14.5 PD_57538 SKG START                                         44520000
308800           SET  WS-VALID-ECR-FILE-I    TO TRUE                    00190100
308900           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309000*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00183706
209900                                                                  ITCP9104
210000 4160-EXIT.  EXIT.                                                ITCP9104
210200                                                                  ITCP9104
309401*R14.5 E_34072 DSL BEGIN                                          44520000
309410 4170-WRITE-MISMATCH-FILE.                                        00179600
309420                                                                  00179700
309440     MOVE '4170-WRITE-MISMATCH-FILE   '  TO WS-PARAGRAPH-NM
309450                                                                  00179700
309460     IF CIABP-HDR-PROFESSIONAL                                    00181220
309470        IF CIABP-PROFESSIONAL                                     00181230
309480           MOVE CIABP-F0-CTR   TO SUBR                            00181240
309490        ELSE                                                      00181250
309491           MOVE CIABP-F1-CTR   TO SUBR                            00181260
309492        END-IF                                                    00181270
309493        PERFORM VARYING SUB1 FROM 1 BY 1                          00181280
309495           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00181290
309497           IF CIASC-REC-TYPE(SUB1) > SPACE                        00181291
309498              WRITE MISMATCH-FILE-RECORD                          00181292
309499                    FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00181293
309500              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181294
309501*R14.5 PD_57538 SKG START                                         44520000
309502              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309503              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309504*R14.5 PD_57538 SKG END                                           44520000
309505           END-IF                                                 00181295
309506        END-PERFORM                                               00181296
309507        PERFORM VARYING SUB1 FROM 1 BY 1                          00181297
309508           UNTIL SUB1 > CIABP-E6-CTR                              00181298
309509           IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00181299
309510              WRITE MISMATCH-FILE-RECORD                          00181302
309511                    FROM CIASC-E6-1-999-SLOT (SUB1)               00181303
309512*R14.5 PD_57538 SKG START                                         44520000
309513              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309514              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309515*R14.5 PD_57538 SKG END                                           44520000
309516              IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00181307
309517                 CONTINUE                                         00181308
309518              ELSE                                                00181309
309519                 ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS  00181313
309520              END-IF                                              00181314
309521           END-IF                                                 00181315
309522        END-PERFORM                                               00181316
309523        PERFORM VARYING SUB1 FROM 1 BY 1                          00181317
309524           UNTIL SUB1 > SUBR                                      00181318
309525           IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00181319
309526              WRITE MISMATCH-FILE-RECORD                          00181320
309527                    FROM CIASC-F0-1-999-SLOT(SUB1)                00181321
309528              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181322
309529*R14.5 PD_57538 SKG START                                         44520000
309530              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309531              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309532*R14.5 PD_57538 SKG END                                           44520000
309533           END-IF                                                 00181323
309534        END-PERFORM                                               00181324
309535        PERFORM VARYING SUB1 FROM 1 BY 1                          00181325
309536           UNTIL SUB1 > SUBR                                      00181326
309537           IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00181327
309538              WRITE MISMATCH-FILE-RECORD                          00181328
309539                    FROM CIASC-F5-1-999-SLOT(SUB1)                00181329
309540              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181330
309541*R14.5 PD_57538 SKG START                                         44520000
309542              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309543              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309544*R14.5 PD_57538 SKG END                                           44520000
309545           END-IF                                                 00181331
309546        END-PERFORM                                               00181332
309547        PERFORM VARYING SUB1 FROM 1 BY 1                          00181333
309548           UNTIL SUB1 > SUBR                                      00181334
309549           IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00181335
309550              WRITE MISMATCH-FILE-RECORD                          00181336
309551                    FROM CIASC-F6-1-999-SLOT(SUB1)                00181337
309552              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181338
309553*R14.5 PD_57538 SKG START                                         44520000
309554              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309555              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309556*R14.5 PD_57538 SKG END                                           44520000
309557           END-IF                                                 00181339
309558        END-PERFORM                                               00181340
309559        IF CIASC-G0-SLOT(1:1) > SPACE                             00181341
309560           WRITE MISMATCH-FILE-RECORD                             00181342
309561                 FROM CIASC-G0-SLOT                               00181343
309562           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181344
309563*R14.5 PD_57538 SKG START                                         44520000
309564           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309565           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309566*R14.5 PD_57538 SKG END                                           44520000
309567        END-IF                                                    00181345
309568        IF CIASC-X0-SLOT(1:1) > SPACE                             00181346
309569           WRITE MISMATCH-FILE-RECORD                             00181347
309570                 FROM CIASC-X0-SLOT                               00181348
309571           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181349
309572*R14.5 PD_57538 SKG START                                         44520000
309573           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309574           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309575*R14.5 PD_57538 SKG END                                           44520000
309576        END-IF.                                                   00181350
309577                                                                  00181351
309578     IF CIABP-HDR-INSTITUTIONAL                                   00181352
309579        PERFORM VARYING SUB1 FROM 1 BY 1                          00181353
309580* R12.0 E_19137 DEM START
309581           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00181354
309582* R12.0 E_19137 DEM END
309583           IF CIASC-REC-TYPE(SUB1) > SPACE                        00181355
309584              WRITE MISMATCH-FILE-RECORD                          00181356
309585                    FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00181357
309586              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181358
309587*R14.5 PD_57538 SKG START                                         44520000
309588              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309589              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309590*R14.5 PD_57538 SKG END                                           44520000
309591           END-IF                                                 00181359
309592        END-PERFORM                                               00181360
309593        PERFORM VARYING SUB1 FROM 1 BY 1                          00181361
309594           UNTIL SUB1 > CIABP-46-CTR                              00181362
309595           IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00181363
309596              WRITE MISMATCH-FILE-RECORD                          00181366
309597                    FROM CIASC-46-1-999-SLOT (SUB1)               00181367
309598*R14.5 PD_57538 SKG START                                         44520000
309599              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309601*R14.5 PD_57538 SKG END                                           44520000
309602              IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00181369
309603                 CONTINUE                                         00181370
309604              ELSE                                                00181371
309605                 ADD 1        TO CIABP-MMATCH-TOTAL-FILE-RECS     00181377
309606              END-IF                                              00181378
309607           END-IF                                                 00181379
309608        END-PERFORM                                               00181380
309609* R12.0 E_19137 DEM START
309610        IF CIASC-47-SLOT(1:1) > SPACE                             00181381
309611           WRITE MISMATCH-FILE-RECORD                             00181382
309612                 FROM CIASC-47-SLOT                               00181383
309613           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309614*R14.5 PD_57538 SKG START                                         44520000
309615           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309616           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309617*R14.5 PD_57538 SKG END                                           44520000
309618        END-IF                                                    00181385
309619* R12.0 E_19137 END
309620        IF CIASC-50-SLOT(1:1) > SPACE                             00181381
309621           WRITE MISMATCH-FILE-RECORD                             00181382
309622                 FROM CIASC-50-SLOT                               00181383
309623           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309624*R14.5 PD_57538 SKG START                                         44520000
309625           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309626           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309627*R14.5 PD_57538 SKG END                                           44520000
309628        END-IF                                                    00181385
309629        PERFORM VARYING SUB1 FROM 1 BY 1                          00181386
309630           UNTIL SUB1 > CIABP-60-CTR                              00181387
309631           IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00181388
309632              WRITE MISMATCH-FILE-RECORD                          00181389
309633                    FROM CIASC-60-1-999-SLOT(SUB1)                00181390
309634              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181391
309635*R14.5 PD_57538 SKG START                                         44520000
309636              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309637              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309638*R14.5 PD_57538 SKG END                                           44520000
309639           END-IF                                                 00181392
309640        END-PERFORM                                               00181393
309641        PERFORM VARYING SUB1 FROM 1 BY 1                          00181394
309642           UNTIL SUB1 > CIABP-60-CTR                              00181395
309643           IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00181396
309644              WRITE MISMATCH-FILE-RECORD                          00181397
309645                    FROM CIASC-65-1-999-SLOT(SUB1)                00181398
309646              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181399
309647*R14.5 PD_57538 SKG START                                         44520000
309648              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309649              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309650*R14.5 PD_57538 SKG END                                           44520000
309651           END-IF                                                 00181400
309652        END-PERFORM                                               00181401
309653        PERFORM VARYING SUB1 FROM 1 BY 1                          00181402
309654           UNTIL SUB1 > CIABP-60-CTR                              00181403
309655           IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00181404
309656              WRITE MISMATCH-FILE-RECORD                          00181405
309657                    FROM CIASC-66-1-999-SLOT(SUB1)                00181406
309658              ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS     00181407
309659*R14.5 PD_57538 SKG START                                         44520000
309660              SET  WS-MIS-MATCH-FILE-I    TO TRUE                 00190100
309661              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
309662*R14.5 PD_57538 SKG END                                           44520000
309663           END-IF                                                 00181408
309664        END-PERFORM                                               00181409
309665        IF CIASC-70-SLOT(1:1) > SPACE                             00181410
309666           WRITE MISMATCH-FILE-RECORD                             00181411
309667                 FROM CIASC-70-SLOT                               00181412
309668           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181413
309669*R14.5 PD_57538 SKG START                                         44520000
309670           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309671           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309672*R14.5 PD_57538 SKG END                                           44520000
309673        END-IF                                                    00181414
309674* R12.0 E_19137 DEM START
309675        IF CIASC-71-SLOT(1:1) > SPACE                             00181381
309676           WRITE MISMATCH-FILE-RECORD                             00181382
309677                 FROM CIASC-71-SLOT                               00181383
309678           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309679*R14.5 PD_57538 SKG START                                         44520000
309680           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309681           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309682*R14.5 PD_57538 SKG END                                           44520000
309683        END-IF                                                    00181385
309684        IF CIASC-72-SLOT(1:1) > SPACE                             00181381
309685           WRITE MISMATCH-FILE-RECORD                             00181382
309686                 FROM CIASC-72-SLOT                               00181383
309687           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309688*R14.5 PD_57538 SKG START                                         44520000
309689           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309690           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309691*R14.5 PD_57538 SKG END                                           44520000
309692        END-IF                                                    00181385
309693        IF CIASC-73-SLOT(1:1) > SPACE                             00181381
309694           WRITE MISMATCH-FILE-RECORD                             00181382
309695                 FROM CIASC-73-SLOT                               00181383
309696           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309697*R14.5 PD_57538 SKG START                                         44520000
309698           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309699           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309700*R14.5 PD_57538 SKG END                                           44520000
309701        END-IF                                                    00181385
309702        IF CIASC-74-SLOT(1:1) > SPACE                             00181381
309703           WRITE MISMATCH-FILE-RECORD                             00181382
309704                 FROM CIASC-74-SLOT                               00181383
309705           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309706*R14.5 PD_57538 SKG START                                         44520000
309707           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309708           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309709*R14.5 PD_57538 SKG END                                           44520000
309710        END-IF                                                    00181385
309711        IF CIASC-75-SLOT(1:1) > SPACE                             00181381
309712           WRITE MISMATCH-FILE-RECORD                             00181382
309713                 FROM CIASC-75-SLOT                               00181383
309714           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181384
309715*R14.5 PD_57538 SKG START                                         44520000
309716           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309717           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309718*R14.5 PD_57538 SKG END                                           44520000
309719        END-IF                                                    00181385
309720* R12.0 E_19137 END
309721        IF CIASC-80-SLOT(1:1) > SPACE                             00181415
309722           WRITE MISMATCH-FILE-RECORD                             00181416
309723                 FROM CIASC-80-SLOT                               00181417
309724           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181418
309725*R14.5 PD_57538 SKG START                                         44520000
309726           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309727           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309728*R14.5 PD_57538 SKG END                                           44520000
309729        END-IF                                                    00181419
309730        IF CIASC-90-SLOT(1:1) > SPACE                             00181420
309731           WRITE MISMATCH-FILE-RECORD                             00181421
309732                 FROM CIASC-90-SLOT                               00181422
309733           ADD 1           TO CIABP-MMATCH-TOTAL-FILE-RECS        00181423
309734*R14.5 PD_57538 SKG START                                         44520000
309735           SET  WS-MIS-MATCH-FILE-I    TO TRUE                    00190100
309736           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
309737*R14.5 PD_57538 SKG END                                           44520000
309738        END-IF.                                                   00181424
309739                                                                  00181426
309740 4170-EXIT.  EXIT.                                                00181427
210300/                                                                 ITCP9104
195400 4200-WRITE-VALIDS-ONLY.                                          ITCP9104
195500                                                                  ITCP9104
02493      PERFORM  4210-WRITE-VALIDS-FILE THRU 4210-EXIT.
195900                                                                  ITCP9104
196000     ADD 1                     TO CIABP-VALID-TOTAL-FILE-CLAIMS   ITCP9104
196100                                                                  ITCP9104
201000     EVALUATE TRUE                                                ITCP9104
201100     WHEN CIABP-VALID-STAT-CD                                     ITCP9104
201200         ADD 1                   TO CIABP-VALID-CLAIMS            ITCP9104
201300         ADD CIASC-INPUT         TO CIABP-VALID-RECS              ITCP9104
201400                                                                  ITCP9104
201500     WHEN CIABP-REJECT-STAT-CD                                    ITCP9104
201600         ADD 1                   TO CIABP-REJECT-CLAIMS           ITCP9104
201700         ADD CIASC-INPUT         TO CIABP-REJECT-RECS             ITCP9104
201800                                                                  ITCP9104
201900     WHEN CIABP-INVALID-STAT-CD                                   ITCP9104
202000         PERFORM 9200-FLAG-RELEASE-ERROR        THRU 9200-EXIT    ITCP9104
202100         IF WS-RELSE-ERROR-FOUND                                  ITCP9104
202200             ADD 1               TO CIABP-RELEASE-CLAIMS          ITCP9104
202300             ADD CIASC-INPUT     TO CIABP-RELEASE-RECS            ITCP9104
196900         ELSE                                                     ITCP9104
202500             ADD 1               TO CIABP-INVALID-CLAIMS          ITCP9104
202600             ADD CIASC-INPUT     TO CIABP-INVALID-RECS            ITCP9104
197700         END-IF                                                   ITCP9104
202800     END-EVALUATE                                                 ITCP9104
197900     .                                                            ITCP9104
203000                                                                  ITCP9104
198000 4200-EXIT.  EXIT.                                                ITCP9104
198100                                                                  ITCP9104
02523 /*****************************************************************
203500**  WRITE VALID FILE (ITCDDO01)                                 **ITCP9104
203600******************************************************************ITCP9104
198200 4210-WRITE-VALIDS-FILE.                                          ITCP9104
312900                                                                  00188910
313000*R14.5 PD_57538 SKG - ADDED BELOW MOVE STATEMENT                  44520000
313100     MOVE '4210-WRITE-VALIDS-FILE     '  TO WS-PARAGRAPH-NM
198300                                                                  ITCP9104
02333      IF CIABP-HDR-PROFESSIONAL                                    00188920
02333         IF CIABP-PROFESSIONAL                                     00188930
02333            MOVE CIABP-F0-CTR   TO SUBR                            00188940
02333         ELSE                                                      00188950
02333            MOVE CIABP-F1-CTR   TO SUBR                            00188960
              END-IF                                                    00188970
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00188980
213910* R12.0 E_19137 DEM START
214000           UNTIL SUB1 > SFCNT-CLM-CNT1-PROF                       00188990
214010* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00188991
02333               WRITE VALID-FILE-RECORD                             00188992
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00188993
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00188994
314700*R14.5 PD_57538 SKG START                                         44520000
314800              SET  WS-VALID-FILE-I        TO TRUE                 00190100
314900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
315000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00188995
              END-PERFORM                                               00188996
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00188997
02333            UNTIL SUB1 > CIABP-E6-CTR                              00188998
02333            IF CIASC-E6-1-999-SLOT(SUB1) (1:1) > SPACE             00188999
02333               WRITE VALID-FILE-RECORD                             00189001
02333                     FROM CIASC-E6-1-999-SLOT (SUB1)               00189002
315800*R14.5 PD_57538 SKG START                                         44520000
315900              SET  WS-VALID-FILE-I        TO TRUE                 00190100
316000              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
316100*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-E6-1-999-SLOT(SUB1) (26:1) = 'N'           00189007
                       CONTINUE                                         00189008
                    ELSE                                                00189009
                       ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS   00189004
02333               END-IF                                              00189005
02333            END-IF                                                 00189006
02333         END-PERFORM                                               00189007
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189008
02333            UNTIL SUB1 > SUBR                                      00189009
02333            IF CIASC-F0-1-999-SLOT(SUB1) (1:1) > SPACE             00189010
02333               WRITE VALID-FILE-RECORD                             00189011
02333                     FROM CIASC-F0-1-999-SLOT(SUB1)                00189012
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189013
317500*R14.5 PD_57538 SKG START                                         44520000
317600              SET  WS-VALID-FILE-I        TO TRUE                 00190100
317700              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
317800*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189014
              END-PERFORM                                               00189015
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189016
02333            UNTIL SUB1 > SUBR                                      00189017
02333            IF CIASC-F5-1-999-SLOT(SUB1) (1:1) > SPACE             00189018
02333               WRITE VALID-FILE-RECORD                             00189019
02333                     FROM CIASC-F5-1-999-SLOT(SUB1)                00189020
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189021
318700*R14.5 PD_57538 SKG START                                         44520000
318800              SET  WS-VALID-FILE-I        TO TRUE                 00190100
318900              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
319000*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189022
              END-PERFORM                                               00189023
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189024
02333            UNTIL SUB1 > SUBR                                      00189025
02333            IF CIASC-F6-1-999-SLOT(SUB1) (1:1) > SPACE             00189026
02333               WRITE VALID-FILE-RECORD                             00189027
02333                     FROM CIASC-F6-1-999-SLOT(SUB1)                00189028
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189029
319900*R14.5 PD_57538 SKG START                                         44520000
320000              SET  WS-VALID-FILE-I        TO TRUE                 00190100
320100              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
320200*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189030
              END-PERFORM                                               00189031
02333         IF CIASC-G0-SLOT(1:1) > SPACE                             00189032
02333            WRITE VALID-FILE-RECORD                                00189033
02333                  FROM CIASC-G0-SLOT                               00189034
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00189035
320900*R14.5 PD_57538 SKG START                                         44520000
321000           SET  WS-VALID-FILE-I        TO TRUE                    00190100
321100           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
321200*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00189036
02333         IF CIASC-X0-SLOT(1:1) > SPACE                             00189037
02333            WRITE VALID-FILE-RECORD                                00189038
02333                  FROM CIASC-X0-SLOT                               00189039
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00189040
321800*R14.5 PD_57538 SKG START                                         44520000
321900           SET  WS-VALID-FILE-I        TO TRUE                    00190100
322000           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
322100*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00189041
198330                                                                  ITCP9104
02333      IF CIABP-HDR-INSTITUTIONAL                                   00189043
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189044
219510* R12.0 E_19137 DEM START
219600           UNTIL SUB1 > SFCNT-CLM-CNT1-INST                       00189054
219610* R12.0 E_19137 END
02333            IF CIASC-REC-TYPE(SUB1) > SPACE                        00189046
02333               WRITE VALID-FILE-RECORD                             00189047
02333                     FROM CIASC-STACKED-CLAIM-ITEM (SUB1)          00189048
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189049
323300*R14.5 PD_57538 SKG START                                         44520000
323400              SET  WS-VALID-FILE-I        TO TRUE                 00190100
323500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
323600*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189050
              END-PERFORM                                               00189051
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189052
02333            UNTIL SUB1 > CIABP-46-CTR                              00189053
02333            IF CIASC-46-1-999-SLOT(SUB1) (1:1) > SPACE             00189054
02333               WRITE VALID-FILE-RECORD                             00189056
02333                     FROM CIASC-46-1-999-SLOT (SUB1)               00189057
324400*R14.5 PD_57538 SKG START                                         44520000
324500              SET  WS-VALID-FILE-I        TO TRUE                 00190100
324600              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
324700*R14.5 PD_57538 SKG END                                           44520000
02333               IF CIASC-46-1-999-SLOT(SUB1) (26:1) = 'N'           00189069
                       CONTINUE                                         00189070
                    ELSE                                                00189071
                       ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS   00189059
02333               END-IF                                              00189060
02333            END-IF                                                 00189061
02333         END-PERFORM                                               00189062
221500* R12.0 E_19137 DEM START
221600        IF CIASC-47-SLOT(1:1) > SPACE                             00189081
221700           WRITE VALID-FILE-RECORD                                00189082
221800                 FROM CIASC-47-SLOT                               00189083
221900            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
326000*R14.5 PD_57538 SKG START                                         44520000
326100            SET  WS-VALID-FILE-I        TO TRUE                   00190100
326200            PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT         00190100
326300*R14.5 PD_57538 SKG END                                           44520000
222000        END-IF                                                    00189085
222001* R12.0 E_19137 DEM END
02333         IF CIASC-50-SLOT(1:1) > SPACE                             00189063
02333            WRITE VALID-FILE-RECORD                                00189064
02333                  FROM CIASC-50-SLOT                               00189065
                  ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189066
327000*R14.5 PD_57538 SKG START                                         44520000
327100            SET  WS-VALID-FILE-I        TO TRUE                   00190100
327200            PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT         00190100
327300*R14.5 PD_57538 SKG END                                           44520000
198710        END-IF                                                    ITCP9104
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189068
02333            UNTIL SUB1 > CIABP-60-CTR                              00189069
02333            IF CIASC-60-1-999-SLOT(SUB1) (1:1) > SPACE             00189070
02333               WRITE VALID-FILE-RECORD                             00189071
02333                     FROM CIASC-60-1-999-SLOT(SUB1)                00189072
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189073
328100*R14.5 PD_57538 SKG START                                         44520000
328200              SET  WS-VALID-FILE-I        TO TRUE                 00190100
328300              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
328400*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189074
              END-PERFORM                                               00189075
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189076
02333            UNTIL SUB1 > CIABP-60-CTR                              00189077
02333            IF CIASC-65-1-999-SLOT(SUB1) (1:1) > SPACE             00189078
02333               WRITE VALID-FILE-RECORD                             00189079
02333                     FROM CIASC-65-1-999-SLOT(SUB1)                00189080
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189081
329300*R14.5 PD_57538 SKG START                                         44520000
329400              SET  WS-VALID-FILE-I        TO TRUE                 00190100
329500              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
329600*R14.5 PD_57538 SKG END                                           44520000
198330                                                                  ITCP9104
02333            END-IF                                                 00189083
              END-PERFORM                                               00189084
02333         PERFORM VARYING SUB1 FROM 1 BY 1                          00189085
02333            UNTIL SUB1 > CIABP-60-CTR                              00189086
02333            IF CIASC-66-1-999-SLOT(SUB1) (1:1) > SPACE             00189087
02333               WRITE VALID-FILE-RECORD                             00189088
02333                     FROM CIASC-66-1-999-SLOT(SUB1)                00189089
                    ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS      00189090
330600*R14.5 PD_57538 SKG START                                         44520000
330700              SET  WS-VALID-FILE-I        TO TRUE                 00190100
330800              PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT       00190100
330900*R14.5 PD_57538 SKG END                                           44520000
02333            END-IF                                                 00189091
              END-PERFORM                                               00189092
02333         IF CIASC-70-SLOT(1:1) > SPACE                             00189093
02333            WRITE VALID-FILE-RECORD                                00189094
02333                  FROM CIASC-70-SLOT                               00189095
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00189096
331600*R14.5 PD_57538 SKG START                                         44520000
331700           SET  WS-VALID-FILE-I        TO TRUE                    00190100
331800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
331900*R14.5 PD_57538 SKG END                                           44520000
224900        END-IF                                                    00189115
225000* R12.0 E_19137 DEM START
225100        IF CIASC-71-SLOT(1:1) > SPACE                             00189081
225200           WRITE VALID-FILE-RECORD                                00189082
225300                 FROM CIASC-71-SLOT                               00189083
225400            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
332600*R14.5 PD_57538 SKG START                                         44520000
332700           SET  WS-VALID-FILE-I        TO TRUE                    00190100
332800           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
332900*R14.5 PD_57538 SKG END                                           44520000
225500        END-IF                                                    00189085
225600        IF CIASC-72-SLOT(1:1) > SPACE                             00189081
225700           WRITE VALID-FILE-RECORD                                00189082
225800                 FROM CIASC-72-SLOT                               00189083
225900            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
333500*R14.5 PD_57538 SKG START                                         44520000
333600           SET  WS-VALID-FILE-I        TO TRUE                    00190100
333700           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
333800*R14.5 PD_57538 SKG END                                           44520000
226000        END-IF                                                    00189085
226100        IF CIASC-73-SLOT(1:1) > SPACE                             00189081
226200           WRITE VALID-FILE-RECORD                                00189082
226300                 FROM CIASC-73-SLOT                               00189083
226400            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
334400*R14.5 PD_57538 SKG START                                         44520000
334500           SET  WS-VALID-FILE-I        TO TRUE                    00190100
334600           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
334700*R14.5 PD_57538 SKG END                                           44520000
226500        END-IF                                                    00189085
226600        IF CIASC-74-SLOT(1:1) > SPACE                             00189081
226700           WRITE VALID-FILE-RECORD                                00189082
226800                 FROM CIASC-74-SLOT                               00189083
226900            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
335300*R14.5 PD_57538 SKG START                                         44520000
335400           SET  WS-VALID-FILE-I        TO TRUE                    00190100
335500           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
335600*R14.5 PD_57538 SKG END                                           44520000
227000        END-IF                                                    00189085
227100        IF CIASC-75-SLOT(1:1) > SPACE                             00189081
227200           WRITE VALID-FILE-RECORD                                00189082
227300                 FROM CIASC-75-SLOT                               00189083
227400            ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS        00189084
336200*R14.5 PD_57538 SKG START                                         44520000
336300           SET  WS-VALID-FILE-I        TO TRUE                    00190100
336400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
336500*R14.5 PD_57538 SKG END                                           44520000
198710        END-IF                                                    ITCP9104
227501* R12.0 E_19137 DEM END
02333         IF CIASC-80-SLOT(1:1) > SPACE                             00189098
02333            WRITE VALID-FILE-RECORD                                00189099
02333                  FROM CIASC-80-SLOT                               00189100
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00189101
337200*R14.5 PD_57538 SKG START                                         44520000
337300           SET  WS-VALID-FILE-I        TO TRUE                    00190100
337400           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
337500*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF                                                    00189102
02333         IF CIASC-90-SLOT(1:1) > SPACE                             00189103
02333            WRITE VALID-FILE-RECORD                                00189104
02333                  FROM CIASC-90-SLOT                               00189105
                 ADD 1           TO CIABP-VALID-TOTAL-FILE-RECS         00189106
338100*R14.5 PD_57538 SKG START                                         44520000
338200           SET  WS-VALID-FILE-I        TO TRUE                    00190100
338300           PERFORM 4400-CHECK-FILE-STATUS THRU 4400-EXIT          00190100
338400*R14.5 PD_57538 SKG END                                           44520000
02333         END-IF.                                                   00189107
199000                                                                  ITCP9104
199100 4210-EXIT.  EXIT.                                                ITCP9104
199200                                                                  ITCP9104
205200/                                                                 ITCP9104
199300 4300-INVALID-FUNCTION.                                           ITCP9104
199400                                                                  ITCP9104
205500     MOVE CWSCC-ABORT            TO SIAPC-RETURN-IND              ITCP9104
205600     MOVE CWSCC-INVALID-FUNCTION TO SIAPC-RETURN-STATUS           ITCP9104
205700     MOVE WS-INVALID-FUNCTION-ERR-MSG                             ITCP9104
205800                                 TO SIAPC-ERROR-MESSAGE           ITCP9104
205900     MOVE '3600'                 TO SIAPC-ABEND-CODE              ITCP9104
206000     SET  WS-INVALID-FUNCTION    TO TRUE                          ITCP9104
206100     PERFORM 9000-ERROR-MESSAGE                 THRU 9000-EXIT    ITCP9104
200000     .                                                            ITCP9104
206300                                                                  ITCP9104
200100 4300-EXIT.  EXIT.                                                ITCP9104
200200                                                                  ITCP9104
340300*R14.5 PD_57538 SKG START                                         44520000
206600/                                                                 ITCP9104
340500 4400-CHECK-FILE-STATUS.                                          00195700
340600                                                                  00195800
340700     EVALUATE TRUE                                                00195800
340800        WHEN WS-VALID-FILE-I                                      00195800
340900          IF ITCDDO01-STAT NOT = '00'
341000             MOVE 'UNSUCCESSFUL WRITE- ITCDDO01, FILE STATUS:'
341100                                  TO SIAPC-ERROR-MESSAGE          21033701
341200             MOVE ITCDDO01-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
341300             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
341400             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
341500             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
341600          END-IF
341700        WHEN WS-INVALID-FILE-I                                    00195800
341800          IF ITCDDO02-STAT NOT = '00'
341900             MOVE 'UNSUCCESSFUL WRITE- ITCDDO02, FILE STATUS:'
342000                                  TO SIAPC-ERROR-MESSAGE          21033701
342100             MOVE ITCDDO02-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
342200             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
342300             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
342400             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
342500          END-IF
342600        WHEN WS-REJECT-FILE-I                                     00195800
342700          IF ITCDDO03-STAT NOT = '00'
342800             MOVE 'UNSUCCESSFUL WRITE- ITCDDO03, FILE STATUS:'
342900                                  TO SIAPC-ERROR-MESSAGE          21033701
343000             MOVE ITCDDO03-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
343100             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
343200             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
343300             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
343400          END-IF
343500        WHEN WS-RELEASE-FILE-I                                    00195800
343600          IF ITCDDO07-STAT NOT = '00'
343700             MOVE 'UNSUCCESSFUL WRITE- ITCDDO07, FILE STATUS:'
343800                                  TO SIAPC-ERROR-MESSAGE          21033701
343900             MOVE ITCDDO07-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
344000             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
344100             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
344200             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
344300          END-IF
344400        WHEN WS-VALID-ADJ-FILE-I                                  00195800
344500          IF ITCDDO08-STAT NOT = '00'
344600             MOVE 'UNSUCCESSFUL WRITE- ITCDDO08, FILE STATUS:'
344700                                  TO SIAPC-ERROR-MESSAGE          21033701
344800             MOVE ITCDDO08-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
344900             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
345000             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
345100             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
345200          END-IF
345300        WHEN WS-VALID-ECR-FILE-I                                  00195800
345400          IF ITCDDO10-STAT NOT = '00'
345500             MOVE 'UNSUCCESSFUL WRITE- ITCDDO10, FILE STATUS:'
345600                                  TO SIAPC-ERROR-MESSAGE          21033701
345700             MOVE ITCDDO10-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
345800             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
345900             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
346000             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
346100          END-IF
346110        WHEN WS-MIS-MATCH-FILE-I                                  00195800
346120          IF ITCDDO11-STAT NOT = '00'
346130             MOVE 'UNSUCCESSFUL WRITE- ITCDDO11, FILE STATUS:'
346140                                  TO SIAPC-ERROR-MESSAGE          21033701
346150             MOVE ITCDDO11-STAT   TO SIAPC-ERROR-MESSAGE(44:2)    21033701
346160             MOVE CWSCC-ABORT     TO SIAPC-RETURN-IND             00078800
346170             MOVE '3600'          TO SIAPC-ABEND-CODE             00079200
346180             PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT            00079400
346190          END-IF
346200     END-EVALUATE.                                                00195800
346300
346400     MOVE SPACE  TO WS-FILE-IND.                                  00078800
346500                                                                  00196400
346600 4400-EXIT.  EXIT.                                                00196500
346700                                                                  00196600
346800*R14.5 PD_57538 SKG END                                           44520000
346900/                                                                 00190700
200300 5000-CLOSE-RTN.                                                  ITCP9104
200400                                                                  ITCP9104
347110*R14.5 E_34072 ADD MISMATCH FILE                                  44520000
200500     CLOSE SCCF-INPUT-FILE                                        ITCP9104
200600     CLOSE VALID-FILE                                             ITCP9104
200700     CLOSE INVALID-FILE                                           ITCP9104
200800     CLOSE REJECT-FILE                                            ITCP9104
200900     CLOSE RELEASE-FILE                                           ITCP9104
201000     CLOSE VALID-ADJ-FILE                                         ITCP9104
217800     CLOSE VALID-ECR-FILE                                         ITCP9104
347810     CLOSE MISMATCH-FILE                                          00191600
201100     .                                                            ITCP9104
348000*R14.5 PD_57538 SKG START                                         44520000
348100     IF ITCDDI01-STAT NOT = '00'
348200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
348300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDI01, FILE STATUS:'
348400                                   TO SIAPC-ERROR-MESSAGE         21033701
348500        MOVE ITCDDI01-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
348600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
348700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
348800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
348900     END-IF.
349000                                                                  00048700
349100     IF ITCDDO01-STAT NOT = '00'
349200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
349300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO01, FILE STATUS:'
349400                                   TO SIAPC-ERROR-MESSAGE         21033701
349500        MOVE ITCDDO01-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
349600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
349700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
349800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
349900     END-IF.
350000
350100     IF ITCDDO02-STAT NOT = '00'
350200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
350300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO02, FILE STATUS:'
350400                                   TO SIAPC-ERROR-MESSAGE         21033701
350500        MOVE ITCDDO02-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
350600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
350700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
350800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
350900     END-IF.
351000                                                                  00048700
351100     IF ITCDDO03-STAT NOT = '00'
351200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
351300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO03, FILE STATUS:'
351400                                   TO SIAPC-ERROR-MESSAGE         21033701
351500        MOVE ITCDDO03-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
351600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
351700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
351800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
351900     END-IF.
352000
352100     IF ITCDDO07-STAT NOT = '00'
352200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
352300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO07, FILE STATUS:'
352400                                   TO SIAPC-ERROR-MESSAGE         21033701
352500        MOVE ITCDDO07-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
352600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
352700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
352800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
352900     END-IF.
353000
353100     IF ITCDDO08-STAT NOT = '00'
353200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
353300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO08, FILE STATUS:'
353400                                   TO SIAPC-ERROR-MESSAGE         21033701
353500        MOVE ITCDDO08-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
353600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
353700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
353800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
353900     END-IF.
354000
354100     IF ITCDDO10-STAT NOT = '00'
354200        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
354300        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO10, FILE STATUS:'
354400                                   TO SIAPC-ERROR-MESSAGE         21033701
354500        MOVE ITCDDO10-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
354600        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
354700        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
354800        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
354900     END-IF.
354901
354910     IF ITCDDO11-STAT NOT = '00'
354920        MOVE '5000-CLOSE-RTN     ' TO WS-PARAGRAPH-NM
354930        MOVE 'UNSUCCESSFUL CLOSE- ITCDDO11, FILE STATUS:'
354940                                   TO SIAPC-ERROR-MESSAGE         21033701
354950        MOVE ITCDDO11-STAT         TO SIAPC-ERROR-MESSAGE(44:2)   21033701
354960        MOVE CWSCC-ABORT           TO SIAPC-RETURN-IND            00078800
354970        MOVE '3600'                TO SIAPC-ABEND-CODE            00079200
354980        PERFORM 9000-ERROR-MESSAGE THRU 9000-EXIT                 00079400
354990     END-IF.
355000*R14.5 PD_57538 SKG END                                           44520000
207600                                                                  ITCP9104
201200 5000-EXIT.  EXIT.                                                ITCP9104
201300                                                                  ITCP9104
207900/                                                                 ITCP9104
201400 9000-ERROR-MESSAGE.                                              ITCP9104
201500                                                                  ITCP9104
203600     DISPLAY WS-ASTERICKS                                         ITCP9104
208300     DISPLAY '*** ITCP9104 PROCESSING TERMINATED ***'             ITCP9104
203800     DISPLAY 'PARAGRAPH NAME: ' WS-PARAGRAPH-NM                   ITCP9104
203900                                                                  ITCP9104
208600     PERFORM 9100-DISPLAY-SIAPC                 THRU 9100-EXIT    ITCP9104
204100                                                                  ITCP9104
208800     IF WS-INVALID-FUNCTION                                       ITCP9104
208900         DISPLAY 'FUNCTION CD MAJOR: ' SIAPC-FUNCTION-CD-MAJOR    ITCP9104
209000         DISPLAY 'FUNCTION CD SUB:   ' SIAPC-FUNCTION-CD-SUB      ITCP9104
204700     END-IF                                                       ITCP9104
204800                                                                  ITCP9104
209300     IF WS-MISSING-TRAILER                                        ITCP9104
209400     OR WS-TABLE-OVERFLOW                                         ITCP9104
209500     OR WS-FILE-SEQUENCE                                          ITCP9104
209600         DISPLAY 'LOCAL PLAN CONTROL NUMBER: '                    ITCP9104
209700                 WS-ERR-LOC-PLAN-CNTL-NUM                         ITCP9104
205900     END-IF                                                       ITCP9104
209900                                                                  ITCP9104
210000     IF WS-MISSING-TRAILER                                        ITCP9104
210100     OR WS-MISSING-CLM-HDR                                        ITCP9104
210200     OR WS-TABLE-OVERFLOW                                         ITCP9104
210300     OR WS-FILE-SEQUENCE                                          ITCP9104
210400         DISPLAY 'SCCF SERIAL NUMBER: ' WS-ERR-SCCF-SER-NUM       ITCP9104
210500         DISPLAY 'RECORD: '             WS-ERR-REC-AND-SEQ-NUM    ITCP9104
210600     END-IF                                                       ITCP9104
210700                                                                  ITCP9104
210800     DISPLAY WS-ASTERICKS                                         ITCP9104
206000     GOBACK                                                       ITCP9104
206100     .                                                            ITCP9104
211100                                                                  ITCP9104
211200 9000-EXIT.  EXIT.                                                ITCP9104
211300                                                                  ITCP9104
206300/                                                                 ITCP9104
211500 9100-DISPLAY-SIAPC.                                              ITCP9104
206500                                                                  ITCP9104
211700     DISPLAY 'ERROR-MESSAGE:           '  SIAPC-ERROR-MESSAGE     ITCP9104
211800     DISPLAY 'SIAPC-RETURN-STATUS:     '  SIAPC-RETURN-STATUS     ITCP9104
211900     DISPLAY 'SIAPC-RETURN-MESSAGE-CD: '  SIAPC-RETURN-MESSAGE-CD ITCP9104
212000     DISPLAY 'SIAPC-ABEND-CODE:        '  SIAPC-ABEND-CODE        ITCP9104
207900     .                                                            ITCP9104
208000                                                                  ITCP9104
208100 9100-EXIT.  EXIT.                                                ITCP9104
208200                                                                  ITCP9104
212500/                                                                 ITCP9104
208300 9200-FLAG-RELEASE-ERROR.                                         ITCP9104
208400                                                                  ITCP9104
208500     IF  CIABP-HDR-INSTITUTIONAL                                  ITCP9104
212900         MOVE SPACE TO WS-INV-REL-IND                             ITCP9104
208700         PERFORM                                                  ITCP9104
208800         VARYING WS-ERR-SUB FROM 1 BY 1                           ITCP9104
208900         UNTIL WS-ERR-SUB > 5                                     ITCP9104
209000               MOVE FM105-ERR-CD(WS-ERR-SUB) TO WS-ERR-CD         ITCP9104
209100               IF WS-INVLD-RELSE-ERR-CD                           ITCP9104
213500                  SET WS-RELSE-ERROR-FOUND TO TRUE                ITCP9104
209300               END-IF                                             ITCP9104
209400         END-PERFORM                                              ITCP9104
209500     ELSE                                                         ITCP9104
209600         IF  CIABP-HDR-PROFESSIONAL                               ITCP9104
214000             MOVE SPACE TO WS-INV-REL-IND                         ITCP9104
209800             PERFORM                                              ITCP9104
209900             VARYING WS-ERR-SUB FROM 1 BY 1                       ITCP9104
210000             UNTIL WS-ERR-SUB > 5                                 ITCP9104
210100                   MOVE FM1A5-ERR-CD(WS-ERR-SUB) TO WS-ERR-CD     ITCP9104
210200                   IF WS-INVLD-RELSE-ERR-CD                       ITCP9104
214600                      SET WS-RELSE-ERROR-FOUND TO TRUE            ITCP9104
210400                   END-IF                                         ITCP9104
210500             END-PERFORM                                          ITCP9104
210600         END-IF                                                   ITCP9104
210700     END-IF                                                       ITCP9104
210800     .                                                            ITCP9104
215200                                                                  ITCP9104
210900 9200-EXIT.  EXIT.                                                ITCP9104
210910                                                                  ITCP9104
211020/                                                                 ITCP9104
211100     COPY ITCPDSB.                                                ITCP9104
