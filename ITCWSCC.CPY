000100******************************************************************ITCWSCC
000200*                                                                *ITCWSCC
000300*             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *ITCWSCC
000400*                                                                *ITCWSCC
000500*   COPY MEMBER:  ITCWSCC                                        *ITCWSCC
000600*   VERSION:      13.4                                           *00006
000700*   DATE:         NOVEMBER 16, 2007                              *00007
000800*   LENGTH:       520 CHARACTERS                                 *00008
000900*                                                                *ITCWSCC
001000*   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *ITCWSCC
001100*   TYPE:         COMMON                                         *ITCWSCC
001200*   FUNCTION:     COMMON AREA STORAGE FOR SYSTEM INDICATORS      *ITCWSCC
001300*                 CONDITION NAMES                                *ITCWSCC
001400*                                                                *ITCWSCC
001500*   MODIFICATION                                                 *ITCWSCC
007510*         HISTORY 11/18/96 - 13.0 NEW RETURN STATUS ADDED:       *ITCWSCC
007511*                                 CWSCC-EMPTY-FILE  VALUE '5026' *ITCWSCC
007512*                                 NEW IO SUB FUNC CODE ADDED:    *ITCWSCC
007513*                                 CWSCC-WRITE-MISMATCH VALUE '70'*ITCWSCC
007600*                                 NEW RETURN STATUS ADDED:       *ITCWSCC
007601*                                 CWSCC-FILE-ALREADY-OPEN        *ITCWSCC
007602*                                                    VALUE '0041'*ITCWSCC
007603*                                 CWSCC-FILE-ALREADY-CLOSED      *ITCWSCC
007604*                                                    VALUE '0042'*ITCWSCC
007606*                 03/14/97 - 13.1 ITS-TM-97-03-R80               *ITCWSCC
007607*                                 NEW RETURN STATUS ADDED:       *ITCWSCC
007608*                                 CWSCC-WRITE-ERROR  VALUE '5044'*ITCWSCC
007606*                                                                *ITCWSCC
007630*                 08/03/01 - 13.2 ITS-TM-01-08-R84               *00029
007631*                                 FOR UPF COB CALCULATOR ADDED:  *00030
007640*                                 CWSCC-BUILD-COB-RPT-FILE       *00031
007651*                                 CWSCC-RECALCULATE-SETUP        *00032
007652*                                 CWSCC-UPF-AND-COB              *00033
007654*                                 CWSCC-COB-ONLY                 *00034
007660*                                                                *00035
      *                 10/28/05 - 13.3 ITS_RDBMS CODE REMEDIATION     *00111
      *                                 PROJECT MODIFICATIONS          *
007660*                                                                *00035
007630*                 11/16/07 - 13.4 ITS-TM-07-11-R10.1             *00029
007654*                                 LINE EXPANSION FROM 23 TO 999  *
021500*                                 ADD CWSCC-DB2-ERR              *00182
007660*                                                                *00035
007700*   NOTES:        NONE                                           *ITCWSCC
007800*                                                                *ITCWSCC
007900*                                                                *ITCWSCC
008000******************************************************************ITCWSCC
008100     05  CWSCC-CONDITION-NAME-AREA.                               ITCWSCC
008200         10  CWSCC-RETURN-CONTROL-CODES.                          ITCWSCC
008300             15  CWSCC-RETURN-IND.                                ITCWSCC
008400                 20  CWSCC-SUCCESSFUL                PIC X        ITCWSCC
008500                                                 VALUE '0'.       ITCWSCC
008600                 20  CWSCC-UNSUCCESSFUL              PIC X        ITCWSCC
008700                                                 VALUE '1'.       ITCWSCC
008800                 20  CWSCC-SEVERE-LOG                PIC X        ITCWSCC
008900                                                 VALUE '2'.       ITCWSCC
009000                 20  CWSCC-SEVERE-NO-LOG             PIC X        ITCWSCC
009100                                                 VALUE '3'.       ITCWSCC
009200                 20  CWSCC-ABORT                     PIC X        ITCWSCC
009300                                                 VALUE '4'.       ITCWSCC
009400                 20  CWSCC-LOG-CONTINUE              PIC X        ITCWSCC
009500                                                 VALUE '5'.       ITCWSCC
009600             15  CWSCC-RETURN-STATUS.                             ITCWSCC
009700                 20  CWSCC-ACTION-COMPLETE           PIC X(4)     ITCWSCC
009800                                                 VALUE '0000'.    ITCWSCC
009900                 20  CWSCC-EDIT-ERROR                PIC X(4)     ITCWSCC
010000                                                 VALUE '0001'.    ITCWSCC
010100                 20  CWSCC-RECORD-NOT-FOUND          PIC X(4)     ITCWSCC
010200                                                 VALUE '0002'.    ITCWSCC
010300                 20  CWSCC-PRIOR-RECORD-FOUND        PIC X(4)     ITCWSCC
010400                                                 VALUE '0003'.    ITCWSCC
010500                 20  CWSCC-SECURITY-DENIED           PIC X(4)     ITCWSCC
010600                                                 VALUE '0004'.    ITCWSCC
010700                 20  CWSCC-DUPLICATE-RECORD          PIC X(4)     ITCWSCC
010800                                                 VALUE '0005'.    ITCWSCC
010900                 20  CWSCC-LENGTH-ERROR              PIC X(4)     ITCWSCC
011000                                                 VALUE '0006'.    ITCWSCC
011100                 20  CWSCC-PROGRAM-ID-ERROR          PIC X(4)     ITCWSCC
011200                                                 VALUE '0007'.    ITCWSCC
011300                 20  CWSCC-INVALID-CICS-REQUEST      PIC X(4)     ITCWSCC
011400                                                 VALUE '0008'.    ITCWSCC
011500                 20  CWSCC-GENERAL-CICS-ERROR        PIC X(4)     ITCWSCC
011600                                                 VALUE '0009'.    ITCWSCC
011700                 20  CWSCC-INVALID-FUNCTION          PIC X(4)     ITCWSCC
011800                                                 VALUE '0010'.    ITCWSCC
011900                 20  CWSCC-UNEXPECTED-SYS-PASS-DATA  PIC X(4)     ITCWSCC
012000                                                 VALUE '0011'.    ITCWSCC
012100                 20  CWSCC-SHORT-SIF                 PIC X(4)     ITCWSCC
012200                                                 VALUE '0012'.    ITCWSCC
012300                 20  CWSCC-LONG-SIF                  PIC X(4)     ITCWSCC
012400                                                 VALUE '0013'.    ITCWSCC
012500                 20  CWSCC-END-OF-INPUT              PIC X(4)     ITCWSCC
012600                                                 VALUE '0014'.    ITCWSCC
012700                 20  CWSCC-UNEXPECTED-IMS-STATUS     PIC X(4)     ITCWSCC
012800                                                 VALUE '0015'.    ITCWSCC
012900                 20  CWSCC-GENERAL-IMS-ERROR         PIC X(4)     ITCWSCC
013000                                                 VALUE '0016'.    ITCWSCC
013100                 20  CWSCC-BAD-IMS-DB-RETURN-STATUS  PIC X(4)     ITCWSCC
013200                                                 VALUE '0017'.    ITCWSCC
013300                 20  CWSCC-BAD-VSAM-FILE-STATUS      PIC X(4)     ITCWSCC
013400                                                 VALUE '0018'.    ITCWSCC
013500                 20  CWSCC-END-OF-FILE               PIC X(4)     ITCWSCC
013600                                                 VALUE '0019'.    ITCWSCC
013700                 20  CWSCC-MULTIPLE-USERS-TRY-AGAIN  PIC X(4)     ITCWSCC
013800                                                 VALUE '0020'.    ITCWSCC
013900                 20  CWSCC-DATE-SEQ-ERROR            PIC X(4)     ITCWSCC
014000                                                 VALUE '0021'.    ITCWSCC
014001                 20  CWSCC-FILE-ALREADY-OPEN         PIC X(4)     ITCWSCC
014002                                                 VALUE '0041'.    ITCWSCC
014003                 20  CWSCC-FILE-ALREADY-CLOSED       PIC X(4)     ITCWSCC
014004                                                 VALUE '0042'.    ITCWSCC
014100                 20  CWSCC-SEVERE-ERROR              PIC X(4)     ITCWSCC
014200                                                 VALUE '0099'.    ITCWSCC
014300                 20  CWSCC-INVALID-CONTROL-FILE      PIC X(4)     ITCWSCC
014400                                                 VALUE '4000'.    ITCWSCC
014500                 20  CWSCC-FILE-SEQUENCE-ERROR       PIC X(4)     ITCWSCC
014600                                                 VALUE '4004'.    ITCWSCC
014700                 20  CWSCC-MISSING-CLAIM-HDR-REC     PIC X(4)     ITCWSCC
014800                                                 VALUE '4008'.    ITCWSCC
014900                 20  CWSCC-MISSING-CLAIM-TRLR-REC    PIC X(4)     ITCWSCC
015000                                                 VALUE '4012'.    ITCWSCC
015100                 20  CWSCC-INVALID-REC-TYPE-SEQ-NUM  PIC X(4)     ITCWSCC
015200                                                 VALUE '4016'.    ITCWSCC
015300                 20  CWSCC-INVALID-CLAIM-TYPE        PIC X(4)     ITCWSCC
015400                                                 VALUE '4020'.    ITCWSCC
015500                 20  CWSCC-FILE-TRLR-REC-CNT-ERR  PIC X(4)        ITCWSCC
015600                                                 VALUE '4024'.    ITCWSCC
015700                 20  CWSCC-FILE-TRLR-CLAIM-CNT-ERR  PIC X(4)      ITCWSCC
015800                                                 VALUE '4028'.    ITCWSCC
015900                 20  CWSCC-EOF-FILE-TRAILER         PIC X(4)      ITCWSCC
016000                                                 VALUE '4029'.    ITCWSCC
016100                 20  CWSCC-END-OF-SCROLL            PIC X(4)      ITCWSCC
016200                                                 VALUE '4030'.    ITCWSCC
016300                 20  CWSCC-MISSING-SERIAL-NUMBER    PIC X(4)      ITCWSCC
016400                                                 VALUE '4400'.    ITCWSCC
016500                 20  CWSCC-INPUT-CNTRL-PLAN-NE-PP   PIC X(4)      ITCWSCC
016600                                                 VALUE '5004'.    ITCWSCC
016700                 20  CWSCC-PP-INVALID-CLAIM-TYPE     PIC X(4)     ITCWSCC
016800                                                 VALUE '5006'.    ITCWSCC
016900                 20  CWSCC-NOT-IN-EFF-DATE-RANGE     PIC X(4)     ITCWSCC
017000                                                 VALUE '5008'.    ITCWSCC
017100                 20  CWSCC-INCUR-DATE-NOT-IN-RANGE   PIC X(4)     ITCWSCC
017200                                                 VALUE '5010'.    ITCWSCC
017300                 20  CWSCC-RECPT-DATE-NOT-IN-RANGE   PIC X(4)     ITCWSCC
017400                                                 VALUE '5012'.    ITCWSCC
017500                 20  CWSCC-TABLE-OVERFLOW            PIC X(4)     ITCWSCC
017600                                                 VALUE '5014'.    ITCWSCC
017700                 20  CWSCC-INVALID-INPUT             PIC X(4)     ITCWSCC
017800                                                 VALUE '5016'.    ITCWSCC
017900                 20  CWSCC-MISSING-FILE-TRLR-REC    PIC X(4)      ITCWSCC
018000                                                 VALUE '5018'.    ITCWSCC
018100                 20  CWSCC-MISPLACED-FILE-TRLR-REC  PIC X(4)      ITCWSCC
018200                                                 VALUE '5020'.    ITCWSCC
018300                 20  CWSCC-CLAIM-STATUS-CD-NOT-V    PIC X(4)      ITCWSCC
018400                                                 VALUE '5024'.    ITCWSCC
018410                 20  CWSCC-EMPTY-FILE               PIC X(4)      ITCWSCC
018420                                                 VALUE '5026'.    ITCWSCC
018500                 20  CWSCC-MRF-DATA-REC-PROBLEM     PIC X(4)      ITCWSCC
018600                                                 VALUE '5028'.    ITCWSCC
018700                 20  CWSCC-MRF-MISSING-HDR-RECORD   PIC X(4)      ITCWSCC
018800                                                 VALUE '5029'.    ITCWSCC
018900                 20  CWSCC-INVLD-TIMEPFM-RPT-REQ    PIC X(4)      ITCWSCC
019000                                                 VALUE '5030'.    ITCWSCC
019100                 20  CWSCC-INVLD-TRNSATN-RPT-REQ    PIC X(4)      ITCWSCC
019200                                                 VALUE '5031'.    ITCWSCC
019300                 20  CWSCC-INVLD-AGESUMM-RPT-REQ    PIC X(4)      ITCWSCC
019400                                                 VALUE '5032'.    ITCWSCC
019500                 20  CWSCC-INVLD-AGEDETL-RPT-REQ    PIC X(4)      ITCWSCC
019600                                                 VALUE '5033'.    ITCWSCC
019700                 20  CWSCC-INVLD-LOAD-SEL           PIC X(4)      ITCWSCC
019800                                                 VALUE '5034'.    ITCWSCC
019900                 20  CWSCC-OPEN-ERROR               PIC X(4)      ITCWSCC
020000                                                 VALUE '5037'.    ITCWSCC
020100                 20  CWSCC-READ-ERROR               PIC X(4)      ITCWSCC
020200                                                 VALUE '5038'.    ITCWSCC
020300                 20  CWSCC-CLOSE-ERROR              PIC X(4)      ITCWSCC
020400                                                 VALUE '5039'.    ITCWSCC
020500                 20  CWSCC-INVALID-RULE             PIC X(4)      ITCWSCC
020600                                                 VALUE '5040'.    ITCWSCC
020700                 20  CWSCC-EXCEED-9999              PIC X(4)      ITCWSCC
020800                                                 VALUE '5041'.    ITCWSCC
020900                 20  CWSCC-NON-CFA-CLAIM            PIC X(4)      ITCWSCC
021000                                                 VALUE '5042'.    ITCWSCC
021100                 20  CWSCC-INVALID-VERSION          PIC X(4)      ITCWSCC
021200                                                 VALUE '5043'.    ITCWSCC
021210                 20  CWSCC-WRITE-ERROR              PIC X(4)      ITCWSCC
021220                                                 VALUE '5044'.    ITCWSCC
021300                 20  CWSCC-VALID-PREFIX-NON-PP      PIC X(4)      ITCWSCC
021400                                                 VALUE '5099'.    ITCWSCC
021500                 20  CWSCC-PP-REC-VERSION-NUM-ERR   PIC X(4)      ITCWSCC
021600                                                 VALUE '5100'.    ITCWSCC
021500                 20  CWSCC-DB2-ERR                  PIC X(4)      00182
021600                                                 VALUE '6000'.    00183
021700                                                                  ITCWSCC
021710         10  CWSCC-FUNCTION-CODES.                                ITCWSCC
021800             15  CWSCC-FUNCTION-CD-VALUES.                        ITCWSCC
021900                 20  CWSCC-RECEIVE-MAP               PIC XX       ITCWSCC
022000                                                 VALUE '01'.      ITCWSCC
022100                 20  CWSCC-SEND-MAP-AND-DATA         PIC XX       ITCWSCC
022200                                                 VALUE '02'.      ITCWSCC
022300                 20  CWSCC-SEND-DATA-ONLY            PIC XX       ITCWSCC
022400                                                 VALUE '03'.      ITCWSCC
022500                 20  CWSCC-SEND-MAP-ONLY             PIC XX       ITCWSCC
022600                                                 VALUE '04'.      ITCWSCC
022700                 20  CWSCC-SEND-MESSAGE              PIC XX       ITCWSCC
022800                                                 VALUE '05'.      ITCWSCC
022900                 20  CWSCC-SEND-MESSAGE-AND-DATE     PIC XX       ITCWSCC
023000                                                 VALUE '06'.      ITCWSCC
023100                 20  CWSCC-SEND-TEXT                 PIC XX       ITCWSCC
023200                                                 VALUE '07'.      ITCWSCC
023300                 20  CWSCC-JULIAN-TO-GREGORIAN       PIC XX       ITCWSCC
023400                                                 VALUE '08'.      ITCWSCC
023500                 20  CWSCC-PLAN-PREPROCESS           PIC XX       ITCWSCC
023600                                                 VALUE '09'.      ITCWSCC
023700                 20  CWSCC-PLAN-POSTPROCESS          PIC XX       ITCWSCC
023800                                                 VALUE '10'.      ITCWSCC
023900                 20  CWSCC-EDIT-MAIN-MENU            PIC XX       ITCWSCC
024000                                                 VALUE '11'.      ITCWSCC
024100                 20  CWSCC-SECURITY-STORE            PIC XX       ITCWSCC
024200                                                 VALUE '12'.      ITCWSCC
024300                 20  CWSCC-SECURITY-RETRIEVE         PIC XX       ITCWSCC
024400                                                 VALUE '13'.      ITCWSCC
024500                 20  CWSCC-SECURITY-DELETE           PIC XX       ITCWSCC
024600                                                 VALUE '14'.      ITCWSCC
024700                 20  CWSCC-LOG-DATABASE              PIC XX       ITCWSCC
024800                                                 VALUE '15'.      ITCWSCC
024900                 20  CWSCC-LOG-TERMINAL              PIC XX       ITCWSCC
025000                                                 VALUE '16'.      ITCWSCC
025100                 20  CWSCC-LOG-BOTH                  PIC XX       ITCWSCC
025200                                                 VALUE '17'.      ITCWSCC
025300                 20  CWSCC-SIF-CONDENSE              PIC XX       ITCWSCC
025400                                                 VALUE '18'.      ITCWSCC
025500                 20  CWSCC-SIF-EXPAND                PIC XX       ITCWSCC
025600                                                 VALUE '19'.      ITCWSCC
025700                 20  CWSCC-SNH-INITIALIZE            PIC XX       ITCWSCC
025800                                                 VALUE '20'.      ITCWSCC
025900                 20  CWSCC-SNH-PROCESS               PIC XX       ITCWSCC
026000                                                 VALUE '21'.      ITCWSCC
026100                 20  CWSCC-SNH-COMPLETE              PIC XX       ITCWSCC
026200                                                 VALUE '22'.      ITCWSCC
026300                 20  CWSCC-ASSEMBLE-PARAMETERS       PIC XX       ITCWSCC
026400                                                 VALUE '23'.      ITCWSCC
026500                 20  CWSCC-DISASSEM-PARAMETERS       PIC XX       ITCWSCC
026600                                                 VALUE '24'.      ITCWSCC
026700                 20  CWSCC-TRANSLATE-MESSAGE-CD      PIC XX       ITCWSCC
026800                                                 VALUE '25'.      ITCWSCC
026900                 20  CWSCC-BUILD-INITIAL-SCREEN      PIC XX       ITCWSCC
027000                                                 VALUE '26'.      ITCWSCC
027100                 20  CWSCC-BUILD-SUBSEQ-SCREEN       PIC XX       ITCWSCC
027200                                                 VALUE '27'.      ITCWSCC
027300                 20  CWSCC-UNBUILD-SCREEN            PIC XX       ITCWSCC
027400                                                 VALUE '28'.      ITCWSCC
027500                 20  CWSCC-FIRST-TIME-THRU           PIC XX       ITCWSCC
027600                                                 VALUE '30'.      ITCWSCC
027700                 20  CWSCC-SUBSEQUENT-ENTRY          PIC XX       ITCWSCC
027800                                                 VALUE '31'.      ITCWSCC
027900                 20  CWSCC-INVOKE-USER-ABEND         PIC XX       ITCWSCC
028000                                                 VALUE '32'.      ITCWSCC
028100                 20  CWSCC-CLEAR-KEY                 PIC XX       ITCWSCC
028200                                                 VALUE '43'.      ITCWSCC
028300                 20  CWSCC-PLAN-SECURITY             PIC XX       ITCWSCC
028400                                                 VALUE '44'.      ITCWSCC
028500                 20  CWSCC-IMS-DEFER-SWITCH          PIC XX       ITCWSCC
028600                                                 VALUE '45'.      ITCWSCC
028700                 20  CWSCC-IMS-IMMED-SWITCH          PIC XX       ITCWSCC
028800                                                 VALUE '46'.      ITCWSCC
028900                 20  CWSCC-IMS-CONTROL-TERM-OUTPUT   PIC XX       ITCWSCC
029000                                                 VALUE '47'.      ITCWSCC
029100                 20  CWSCC-IMS-INPUT                 PIC XX       ITCWSCC
029200                                                 VALUE '48'.      ITCWSCC
029300                 20  CWSCC-IMS-DELETE-ITSDB2LT       PIC XX       ITCWSCC
029400                                                 VALUE '49'.      ITCWSCC
029500                 20  CWSCC-IMS-DELETE-ITSDB2WK       PIC XX       ITCWSCC
029600                                                 VALUE '50'.      ITCWSCC
029700                 20  CWSCC-EDIT-DATA                 PIC XX       ITCWSCC
029800                                                 VALUE '51'.      ITCWSCC
029900                 20  CWSCC-ACCESS-SUMMARY            PIC XX       ITCWSCC
030000                                                 VALUE '52'.      ITCWSCC
030100                 20  CWSCC-GENERATE-CONTROL-REPORT   PIC XX       ITCWSCC
030200                                                 VALUE '53'.      ITCWSCC
030300                 20  CWSCC-ABNORMAL-TERMINATION      PIC XX       ITCWSCC
030400                                                 VALUE '54'.      ITCWSCC
030500                 20  CWSCC-FIND-FIRST-ERROR-SCREEN   PIC XX       ITCWSCC
030600                                                 VALUE '55'.      ITCWSCC
030700                 20  CWSCC-EDIT-SCREEN               PIC XX       ITCWSCC
030800                                                 VALUE '56'.      ITCWSCC
030900                 20  CWSCC-EDIT-CLAIM                PIC XX       ITCWSCC
031000                                                 VALUE '57'.      ITCWSCC
031100                 20  CWSCC-TRANSLATE-MESSAGE         PIC XX       ITCWSCC
031200                                                 VALUE '58'.      ITCWSCC
031300                 20  CWSCC-PROCESS-SEL-CRIT          PIC XX       ITCWSCC
031400                                                 VALUE 'A0'.      ITCWSCC
031500                                                                  ITCWSCC
031600             15  CWSCC-IO-MAJOR-FUNCTION-CDS.                     ITCWSCC
031700               20  CWSCC-OPEN-FILES-REQUEST          PIC X(2)     ITCWSCC
031800                                                 VALUE '81'.      ITCWSCC
031900               20  CWSCC-CLOSE-FILES-REQUEST         PIC X(2)     ITCWSCC
032000                                                 VALUE '82'.      ITCWSCC
032100               20  CWSCC-READ-FILES-REQUEST          PIC X(2)     ITCWSCC
032200                                                 VALUE '83'.      ITCWSCC
032300               20  CWSCC-WRITE-FILES-REQUEST         PIC X(2)     ITCWSCC
032400                                                 VALUE '84'.      ITCWSCC
032500               20  CWSCC-DELETE-FILES-REQUEST        PIC X(2)     ITCWSCC
032600                                                 VALUE '85'.      ITCWSCC
032700               20  CWSCC-UPDATE-FILES-REQUEST        PIC X(2)     ITCWSCC
032800                                                 VALUE '86'.      ITCWSCC
032900               20  CWSCC-NORMAL-PROCESSING           PIC X(2)     ITCWSCC
033000                                                 VALUE '90'.      ITCWSCC
033100                                                                  ITCWSCC
033200             15  CWSCC-IO-SUB-FUNCTION-CODES.                     ITCWSCC
033300               20  CWSCC-GENERIC-READ-ACCESS         PIC X(2)     ITCWSCC
033400                                                 VALUE '91'.      ITCWSCC
033500               20  CWSCC-SEQUENTIAL-READ-ACCESS      PIC X(2)     ITCWSCC
033600                                                 VALUE '92'.      ITCWSCC
033700               20  CWSCC-RANDOM-READ-ACCESS          PIC X(2)     ITCWSCC
033800                                                 VALUE '93'.      ITCWSCC
033900               20  CWSCC-EQUAL-TO-READ-ACCESS        PIC X(2)     ITCWSCC
034000                                                 VALUE '94'.      ITCWSCC
034100               20  CWSCC-OPEN-AS-INPUT-ACCESS        PIC X(2)     ITCWSCC
034200                                                 VALUE '95'.      ITCWSCC
034300               20  CWSCC-OPEN-AS-OUTPUT-ACCESS       PIC X(2)     ITCWSCC
034400                                                 VALUE '96'.      ITCWSCC
034500               20  CWSCC-OPEN-AS-I-O-ACCESS          PIC X(2)     ITCWSCC
034600                                                 VALUE '97'.      ITCWSCC
034700               20  CWSCC-READ-RANDOM-FILE-TRAILER    PIC X(2)     ITCWSCC
034800                                                 VALUE '98'.      ITCWSCC
034900               20  CWSCC-WRITE-LOAD-TRAILER          PIC X(2)     ITCWSCC
035000                                                 VALUE '60'.      ITCWSCC
035100               20  CWSCC-WRITE-EXTRACT-TRAILER       PIC X(2)     ITCWSCC
035200                                                 VALUE '61'.      ITCWSCC
035300               20  CWSCC-ES-EXTRACT                  PIC X(2)     ITCWSCC
035400                                                 VALUE '62'.      ITCWSCC
035500               20  CWSCC-WRITE-VALID-ONLY            PIC X(2)     ITCWSCC
035600                                                 VALUE '63'.      ITCWSCC
035700               20  CWSCC-WRITE-TRAILER               PIC X(2)     ITCWSCC
035800                                                 VALUE '64'.      ITCWSCC
035900               20  CWSCC-WRITE-MORE-RULES-RECORDS    PIC X(2)     ITCWSCC
036000                                                 VALUE '65'.      ITCWSCC
036100               20  CWSCC-WRITE-LOCAL-REPORT          PIC X(2)     ITCWSCC
036200                                                 VALUE '66'.      ITCWSCC
036300               20  CWSCC-WRITE-CONTROL-REPORT        PIC X(2)     ITCWSCC
036400                                                 VALUE '67'.      ITCWSCC
036500               20  CWSCC-INITIAL-READ                PIC X(02)    ITCWSCC
036600                                                 VALUE '3A'.      ITCWSCC
036700               20  CWSCC-SUBSEQ-READ                 PIC X(02)    ITCWSCC
036800                                                 VALUE '3B'.      ITCWSCC
036900               20  CWSCC-REPL-LEVEL-TWO              PIC X(02)    ITCWSCC
037000                                                 VALUE '3C'.      ITCWSCC
037100               20  CWSCC-REPL-ALL-LEVELS             PIC X(02)    ITCWSCC
037200                                                 VALUE '3D'.      ITCWSCC
037300               20  CWSCC-ADD-ALL-LEVELS              PIC X(02)    ITCWSCC
037400                                                 VALUE '3E'.      ITCWSCC
037500               20  CWSCC-REPL-N-ADD-FOR-DT-CHG       PIC X(02)    ITCWSCC
037600                                                 VALUE '3F'.      ITCWSCC
037700               20  CWSCC-REPL-N-ADD-FOR-REJ          PIC X(02)    ITCWSCC
037800                                                 VALUE '3G'.      ITCWSCC
037900               20  CWSCC-READ-ALL-LEVELS             PIC X(02)    ITCWSCC
038000                                                 VALUE '3H'.      ITCWSCC
038100               20  CWSCC-RECEPTION-READ              PIC X(02)    ITCWSCC
038200                                                 VALUE '3I'.      ITCWSCC
038300               20  CWSCC-REPL-N-ADD-INT-DT-CHG       PIC X(02)    ITCWSCC
038400                                                 VALUE '3J'.      ITCWSCC
038500               20  CWSCC-REPL-N-ADD-INT-REJ          PIC X(02)    ITCWSCC
038600                                                 VALUE '3K'.      ITCWSCC
038700               20  CWSCC-PP-ONLINE-READ-NEXT         PIC X(02)    ITCWSCC
038800                                                 VALUE '3L'.      ITCWSCC
038900               20  CWSCC-UPDATE-FT-WRITE-SC          PIC X(02)    ITCWSCC
039000                                                 VALUE '59'.      ITCWSCC
039100               20  CWSCC-DISTRIBUTION-DELETE         PIC X(02)    ITCWSCC
039200                                                 VALUE '1A'.      ITCWSCC
039300               20  CWSCC-STD-RULE-DELETE             PIC X(02)    ITCWSCC
039400                                                 VALUE '1B'.      ITCWSCC
039500               20  CWSCC-PRE-CALC-SETUP              PIC X(2)     ITCWSCC
039600                                                 VALUE 'E1'.      ITCWSCC
039700               20  CWSCC-INC-CLAIM-SETUP             PIC X(2)     ITCWSCC
039800                                                 VALUE 'E2'.      ITCWSCC
039900               20  CWSCC-CLAIM-CALC                  PIC X(2)     ITCWSCC
040000                                                 VALUE 'E3'.      ITCWSCC
040100               20  CWSCC-BUILD-RPT-FILE              PIC X(2)     ITCWSCC
040200                                                 VALUE 'E4'.      ITCWSCC
040300               20  CWSCC-REPL-N-ADD-DT-CHG-OL        PIC X(02)    ITCWSCC
040400                                                 VALUE 'E5'.      ITCWSCC
040500               20  CWSCC-REPL-N-ADD-REJ-OL           PIC X(02)    ITCWSCC
040600                                                 VALUE 'E6'.      ITCWSCC
040700                                                                  ITCWSCC
040800****************************************************************  ITCWSCC
040900*  UPF MODIFICATIONS FOR 08/03/01 RELEASE 8.4 START HERE       *  00377
041000****************************************************************  00378
041001               20  CWSCC-BUILD-COB-RPT-FILE          PIC X(2)     00379
041002                                                 VALUE 'E7'.      00380
041003               20  CWSCC-RECALCULATE-SETUP           PIC X(2)     00381
041004                                                 VALUE 'E8'.      00382
041005                                                                  00383
041006****************************************************************  00384
041010****************************************************************  00385
040900*  CFA MODIFICATIONS FOR 04/01/92 RELEASE START HERE           *  ITCWSCC
041000****************************************************************  ITCWSCC
041100                                                                  ITCWSCC
041200               20  CWSCC-RECEIVE-FILES-REQUEST       PIC X(2)     ITCWSCC
041300                                                 VALUE 'C1'.      ITCWSCC
041400               20  CWSCC-TRANSMIT-FILES-REQUEST      PIC X(2)     ITCWSCC
041500                                                 VALUE 'C2'.      ITCWSCC
041600               20  CWSCC-WRITE-HEADER                PIC X(2)     ITCWSCC
041700                                                 VALUE 'C3'.      ITCWSCC
041800               20  CWSCC-WRITE-HEADER-TRAILER        PIC X(2)     ITCWSCC
041900                                                 VALUE 'C4'.      ITCWSCC
042000               20  CWSCC-WRITE-DETAIL-LINE           PIC X(2)     ITCWSCC
042100                                                 VALUE 'C5'.      ITCWSCC
042200               20  CWSCC-WRITE-CONTROL-LINE          PIC X(2)     ITCWSCC
042300                                                 VALUE 'C6'.      ITCWSCC
042400               20  CWSCC-WRITE-BATCH-HEADER          PIC X(2)     ITCWSCC
042500                                                 VALUE 'C7'.      ITCWSCC
042600               20  CWSCC-WRITE-BATCH-TRAILER         PIC X(2)     ITCWSCC
042700                                                 VALUE 'C8'.      ITCWSCC
042800               20  CWSCC-CALCULATE                   PIC X(2)     ITCWSCC
042900                                                 VALUE 'C9'.      ITCWSCC
043000               20  CWSCC-VALIDATE                    PIC X(2)     ITCWSCC
043100                                                 VALUE 'CA'.      ITCWSCC
043200               20  CWSCC-OUT-OF-BALANCE              PIC X(2)     ITCWSCC
043300                                                 VALUE 'CB'.      ITCWSCC
043400****************************************************************  ITCWSCC
043500*  CFA MODIFICATIONS FOR 04/01/92 RELEASE END HERE             *  ITCWSCC
043600****************************************************************  ITCWSCC
043610****************************************************************  ITCWSCC
043620*  ITS MODIFICATIONS FOR 08/23/96 STARTS HERE                  *  ITCWSCC
043630****************************************************************  ITCWSCC
043700                                                                  ITCWSCC
043710               20  CWSCC-WRITE-DUP-FILE              PIC X(2)     ITCWSCC
043720                                                 VALUE '68'.      ITCWSCC
043721               20  CWSCC-WRITE-INVALID-FILE          PIC X(2)     ITCWSCC
043722                                                 VALUE '69'.      ITCWSCC
043723               20  CWSCC-WRITE-MISMATCH              PIC X(2)     ITCWSCC
043724                                                 VALUE '70'.      ITCWSCC
043730****************************************************************  ITCWSCC
043726*  UPF MODIFICATIONS FOR 08/03/2001 REL 8.4 STARTS HERE        *  00425
043727****************************************************************  00426
043728                                                                  00427
043729               20  CWSCC-UPF-AND-COB                 PIC X(2)     00428
043730                                                 VALUE 'UA'.      00429
043731               20  CWSCC-COB-ONLY                    PIC X(2)     00430
043732                                                 VALUE 'UZ'.      00431
043733****************************************************************  00432
043734*  UPF MODIFICATIONS FOR 08/03/2001 REL 8.4 END HERE           *  00433
043735****************************************************************  00434
043736****************************************************************  00435
043740*  ITS MODIFICATIONS FOR 08/23/96 RELEASE END HERE             *  ITCWSCC
043750****************************************************************  ITCWSCC
043800             15  CWSCC-TYPE-FILEKEY-CODES.                        ITCWSCC
043900               20  CWSCC-PRIMARY-TYPE-KEY            PIC X(1)     ITCWSCC
044000                                                 VALUE 'P'.       ITCWSCC
044100               20  CWSCC-ALTERNATE-TYPE-KEY          PIC X(1)     ITCWSCC
044200                                                 VALUE 'A'.       ITCWSCC
044300***************************************************************** ITCWSCC
044400*  THE FOLLOWING FUNCTION VALUES ARE USED AS PROCESS SWITCHES   * ITCWSCC
044500*  FOR EITHER/OR SITUATIONS WHICH CAN BE DETERMINED BY THE      * ITCWSCC
044600*  INSTALLING PLAN. THE VALUE ASSIGNED MUST CORRESPOND TO THE   * ITCWSCC
044700*  VALUE FOR THE DESIRED PROCESS OPTION.                        * ITCWSCC
044800*  SOME OF THE FUNCTION SWITCHES ARE RELEVENT TO SPECIFIC TP    * ITCWSCC
044900*  MONITORS AND DO NOT REQUIRE INITIALIZING IF NOT APPLICABLE.  * ITCWSCC
045000***************************************************************** ITCWSCC
045100             15  CWSCC-FUNCTION-SWITCH-VALUES.                    ITCWSCC
045200                 20  CWSCC-IMS-WORK-DATABASE         PIC XX       ITCWSCC
045300                                                 VALUE 'WK'.      ITCWSCC
045400                 20  CWSCC-IMS-MESSAGE-QUEUE         PIC XX       ITCWSCC
045500                                                 VALUE 'MQ'.      ITCWSCC
045600             15  CWSCC-IMS-FUNCTION-SWITCHES.                     ITCWSCC
045700                 20  CWSCC-ITMQ-TO-ITSX              PIC XX       ITCWSCC
045800                                                 VALUE 'WK'.      ITCWSCC
045900***** POSSIBLE VALUES =  -WORK-DATABASE  OR  -MESSAGE-QUEUE       ITCWSCC
046000                 20  CWSCC-ITSX-TO-ITMA              PIC XX       ITCWSCC
046100                                                 VALUE 'WK'.      ITCWSCC
046200***** POSSIBLE VALUES =  -WORK-DATABASE  OR  -MESSAGE-QUEUE       ITCWSCC
046300                 20  CWSCC-ITMA-TO-ITSX              PIC XX       ITCWSCC
046400                                                 VALUE 'WK'.      ITCWSCC
046500***** POSSIBLE VALUES =  -WORK-DATABASE  OR  -MESSAGE-QUEUE       ITCWSCC
046600                 20  CWSCC-ITSX-TO-ITMI              PIC XX       ITCWSCC
046700                                                 VALUE 'WK'.      ITCWSCC
046800***** POSSIBLE VALUES =  -WORK-DATABASE  OR  -MESSAGE-QUEUE       ITCWSCC
046900                                                                  ITCWSCC
047000***                                                               ITCWSCC
047100***   FOLLOWING SCREEN VALUES ARE BEING USED BY DISPLAY CONTROL   ITCWSCC
047200***   MODULE TO CORRESSPOND TO 1 , 2 ,3 ,ETC. SCREENS  BUILD      ITCWSCC
047300***   IN DISPLAY BUILD MODULES.                                   ITCWSCC
047400***                                                               ITCWSCC
047500             15  CWSCC-FUNC-SUB-CD-VALUES.                        ITCWSCC
047600                 20  CWSCC-FIRST-SCREEN              PIC XX       ITCWSCC
047700                                                 VALUE '01'.      ITCWSCC
047800                 20  CWSCC-SECOND-SCREEN             PIC XX       ITCWSCC
047900                                                 VALUE '02'.      ITCWSCC
048000                 20  CWSCC-THIRD-SCREEN              PIC XX       ITCWSCC
048100                                                 VALUE '03'.      ITCWSCC
048200                 20  CWSCC-FOURTH-SCREEN             PIC XX       ITCWSCC
048300                                                 VALUE '04'.      ITCWSCC
048400                 20  CWSCC-FIFTH-SCREEN              PIC XX       ITCWSCC
048500                                                 VALUE '05'.      ITCWSCC
048600                 20  CWSCC-SIXTH-SCREEN              PIC XX       ITCWSCC
048700                                                 VALUE '06'.      ITCWSCC
048800                 20  CWSCC-SEVENTH-SCREEN            PIC XX       ITCWSCC
048900                                                 VALUE '07'.      ITCWSCC
049000                 20  CWSCC-EIGHTH-SCREEN             PIC XX       ITCWSCC
049100                                                 VALUE '08'.      ITCWSCC
049200                 20  CWSCC-NINETH-SCREEN             PIC XX       ITCWSCC
049300                                                 VALUE '09'.      ITCWSCC
049400                 20  CWSCC-TENTH-SCREEN              PIC XX       ITCWSCC
049500                                                 VALUE '10'.      ITCWSCC
