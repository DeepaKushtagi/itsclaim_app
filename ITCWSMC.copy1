000100******************************************************************ITCWSMC
000200*                                                                *ITCWSMC
000300*             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *ITCWSMC
000400*                                                                *ITCWSMC
000500*   COPY MEMBER:  ITCWSMC                                        *ITCWSMC
000600*   VERSION:      13.1                                           *00006
000700*   DATE:         10/28/05                                       *00007
000800*   LENGTH:       1001  CHARACTERS                               *ITCWSMC
000900*                                                                *ITCWSMC
001000*   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *ITCWSMC
001100*   TYPE:         COMMON                                         *ITCWSMC
001200*   FUNCTION:     WORKING STORAGE AREA FOR THE DESCRIPTIVE       *ITCWSMC
001300*                 DATA NAMES AND THE ASSOCIATED VALUES OF        *ITCWSMC
001400*                 THE MESSAGE CODES CONTAINED ON THE MESSAGE     *ITCWSMC
001500*                 DATA BASE FOR THE SCCF APPLICATION             *ITCWSMC
001600*                                                                *ITCWSMC
001700*                                                                *ITCWSMC
001800*   MODIFICATION                                                 *ITCWSMC
001900*   HISTORY:      6/01/88 - 01.0 INITIAL RELEASE                 *ITCWSMC
003901*                11/18/96 - 13.0 ADD MSGS 'ITC903/904/905/906 MB *ITCWSMC
003902*                                ADD MSGS 'ITC907/908/909/910' MB*ITCWSMC
003903*                                ADD MSGS 'ITC604/605'           *ITCWSMC
003904*                                                                *ITCWSMC
      *                10/28/05 - 13.1 ITS_RDBMS CODE REMEDIATION      *00111
      *                                PROJECT MODIFICATIONS           *
004000*                                                                *ITCWSMC
004100*   NOTES:        NONE                                           *ITCWSMC
004200*                                                                *ITCWSMC
004300******************************************************************ITCWSMC
004400     05  CWSMC-MESSAGE-CODE-AREA.                                 ITCWSMC
004500         10  CWSMC-MESSAGE-CODES.                                 ITCWSMC
004600             15  CWSMC-SC-APPLICATION-MESSAGES       PIC X(6)     ITCWSMC
004700                                                 VALUE 'ITC000'.  ITCWSMC
004800             15  CWSMC-INVALID-SCREEN-SELECT-CD      PIC X(6)     ITCWSMC
004900                                                 VALUE 'ITC001'.  ITCWSMC
005000             15  CWSMC-EDIT-ERROR                    PIC X(6)     ITCWSMC
005100                                                 VALUE 'ITC002'.  ITCWSMC
005200             15  CWSMC-NUMERIC-OPTION-TOO-HIGH       PIC X(6)     ITCWSMC
005300                                                 VALUE 'ITC003'.  ITCWSMC
005400             15  CWSMC-SUMMARY-RECORD-NOT-FOUND      PIC X(6)     ITCWSMC
005500                                                 VALUE 'ITC004'.  ITCWSMC
005600             15  CWSMC-INVALID-FUNCTION-CP9502       PIC X(6)     ITCWSMC
005700                                                 VALUE 'ITC005'.  ITCWSMC
005800             15  CWSMC-INVALID-FUNCTION-CP9504       PIC X(6)     ITCWSMC
005900                                                 VALUE 'ITC006'.  ITCWSMC
006000             15  CWSMC-INVALID-FUNCTION-CP9506       PIC X(6)     ITCWSMC
006100                                                 VALUE 'ITC007'.  ITCWSMC
006200             15  CWSMC-INVALID-FUNCTION-CP9508       PIC X(6)     ITCWSMC
006300                                                 VALUE 'ITC008'.  ITCWSMC
006400             15  CWSMC-INVALID-FUNCTION-CP1102       PIC X(6)     ITCWSMC
006500                                                 VALUE 'ITC009'.  ITCWSMC
006600             15  CWSMC-ITCDB400-FILE-ERROR           PIC X(6)     ITCWSMC
006700                                                 VALUE 'ITC010'.  ITCWSMC
006800             15  CWSMC-WRONG-FUNCTION-KEY            PIC X(6)     ITCWSMC
006900                                                 VALUE 'ITC011'.  ITCWSMC
007000             15  CWSMC-PROGRAM-ID-ERROR-CP1002       PIC X(6)     ITCWSMC
007100                                                 VALUE 'ITC012'.  ITCWSMC
007200             15  CWSMC-PROGRAM-ID-ERROR-CP1004       PIC X(6)     ITCWSMC
007300                                                 VALUE 'ITC013'.  ITCWSMC
007400             15  CWSMC-PROGRAM-ID-ERROR-CP1006       PIC X(6)     ITCWSMC
007500                                                 VALUE 'ITC014'.  ITCWSMC
007600             15  CWSMC-INVALID-REQUEST-CP1002        PIC X(6)     ITCWSMC
007700                                                 VALUE 'ITC015'.  ITCWSMC
007800             15  CWSMC-INVALID-REQUEST-CP1004        PIC X(6)     ITCWSMC
007900                                                 VALUE 'ITC016'.  ITCWSMC
008000             15  CWSMC-INVALID-REQUEST-CP1006        PIC X(6)     ITCWSMC
008100                                                 VALUE 'ITC017'.  ITCWSMC
008200             15  CWSMC-CICS-ERROR-CP1002             PIC X(6)     ITCWSMC
008300                                                 VALUE 'ITC018'.  ITCWSMC
008400             15  CWSMC-CICS-ERROR-CP1004             PIC X(6)     ITCWSMC
008500                                                 VALUE 'ITC019'.  ITCWSMC
008600             15  CWSMC-CICS-ERROR-CP1006             PIC X(6)     ITCWSMC
008700                                                 VALUE 'ITC020'.  ITCWSMC
008800             15  CWSMC-CICS-ERROR-CP1102             PIC X(6)     ITCWSMC
008900                                                 VALUE 'ITC021'.  ITCWSMC
009000             15  CWSMC-INVOKED-MODULE-NOT-SET        PIC X(6)     ITCWSMC
009100                                                 VALUE 'ITC022'.  ITCWSMC
009200             15  CWSMC-RETURN-IND-NOT-SET            PIC X(6)     ITCWSMC
009300                                                 VALUE 'ITC023'.  ITCWSMC
009400             15  CWSMC-END-OF-SCROLL-BACK            PIC X(6)     ITCWSMC
009500                                                 VALUE 'ITC024'.  ITCWSMC
009600             15  CWSMC-END-OF-SCROLL-BACK-LIMIT      PIC X(6)     ITCWSMC
009700                                                 VALUE 'ITC025'.  ITCWSMC
009800             15  CWSMC-NO-MORE-DATA                  PIC X(6)     ITCWSMC
009900                                                 VALUE 'ITC026'.  ITCWSMC
010000             15  CWSMC-DATA-ERROR-CP1102             PIC X(6)     ITCWSMC
010100                                                 VALUE 'ITC027'.  ITCWSMC
010200             15  CWSMC-DATA-ERROR-CP9504             PIC X(6)     ITCWSMC
010300                                                 VALUE 'ITC028'.  ITCWSMC
010400             15  CWSMC-INVALID-FUNCTION-CP5102       PIC X(6)     ITCWSMC
010500                                                 VALUE 'ITC029'.  ITCWSMC
010600             15  CWSMC-IMS-ERROR-CP5102              PIC X(6)     ITCWSMC
010700                                                 VALUE 'ITC030'.  ITCWSMC
010800             15  CWSMC-BAD-IMS-DB-STATUS-CP5102      PIC X(6)     ITCWSMC
010900                                                 VALUE 'ITC031'.  ITCWSMC
011000             15  CWSMC-DATA-ERROR-CP5102             PIC X(6)     ITCWSMC
011100                                                 VALUE 'ITC032'.  ITCWSMC
011200             15  CWSMC-OPTION-VALID-FROM-DETAIL      PIC X(6)     ITCWSMC
011300                                                 VALUE 'ITC033'.  ITCWSMC
011400             15  CWSMC-INVALD-STATUS-FOR-OPTION      PIC X(6)     ITCWSMC
011500                                                 VALUE 'ITC034'.  ITCWSMC
011600             15  CWSMC-PLAN-PROF-REC-NOT-FOUND       PIC X(6)     ITCWSMC
011700                                                 VALUE 'ITC035'.  ITCWSMC
011800             15  CWSMC-DUPLICATE-RECORD-EXISTS       PIC X(6)     ITCWSMC
011900                                                 VALUE 'ITC036'.  ITCWSMC
012000             15  CWSMC-INACTIVE-RECORD-EXISTS        PIC X(6)     ITCWSMC
012100                                                 VALUE 'ITC037'.  ITCWSMC
012200             15  CWSMC-MUST-BE-CNTRL-PL              PIC X(6)     ITCWSMC
012300                                                 VALUE 'ITC038'.  ITCWSMC
012400             15  CWSMC-RECPT-INCUR-DATE-OVERLAP      PIC X(6)     ITCWSMC
012500                                                 VALUE 'ITC039'.  ITCWSMC
012600             15  CWSMC-INVALID-FUNCTION-CP9534       PIC X(6)     ITCWSMC
012700                                                 VALUE 'ITC040'.  ITCWSMC
012800             15  CWSMC-INVALID-FUNCTION-CP9536       PIC X(6)     ITCWSMC
012900                                                 VALUE 'ITC041'.  ITCWSMC
013000             15  CWSMC-INVALID-FUNCTION-CP9538       PIC X(6)     ITCWSMC
013100                                                 VALUE 'ITC042'.  ITCWSMC
013200             15  CWSMC-INVALID-FUNCTION-CP9540       PIC X(6)     ITCWSMC
013300                                                 VALUE 'ITC043'.  ITCWSMC
013400             15  CWSMC-INVALID-FUNCTION-CP9542       PIC X(6)     ITCWSMC
013500                                                 VALUE 'ITC044'.  ITCWSMC
013600             15  CWSMC-INVALID-FUNCTION-CP1108       PIC X(6)     ITCWSMC
013700                                                 VALUE 'ITC045'.  ITCWSMC
013800             15  CWSMC-ITCDB600-FILE-ERROR           PIC X(6)     ITCWSMC
013900                                                 VALUE 'ITC046'.  ITCWSMC
014000             15  CWSMC-PROGRAM-ID-ERROR-CP1010       PIC X(6)     ITCWSMC
014100                                                 VALUE 'ITC047'.  ITCWSMC
014200             15  CWSMC-PROGRAM-ID-ERROR-CP1012       PIC X(6)     ITCWSMC
014300                                                 VALUE 'ITC048'.  ITCWSMC
014400             15  CWSMC-PROGRAM-ID-ERROR-CP1014       PIC X(6)     ITCWSMC
014500                                                 VALUE 'ITC049'.  ITCWSMC
014600             15  CWSMC-PROGRAM-ID-ERROR-CP1016       PIC X(6)     ITCWSMC
014700                                                 VALUE 'ITC050'.  ITCWSMC
014800             15  CWSMC-INVALID-REQUEST-CP1010        PIC X(6)     ITCWSMC
014900                                                 VALUE 'ITC051'.  ITCWSMC
015000             15  CWSMC-INVALID-REQUEST-CP1012        PIC X(6)     ITCWSMC
015100                                                 VALUE 'ITC052'.  ITCWSMC
015200             15  CWSMC-INVALID-REQUEST-CP1014        PIC X(6)     ITCWSMC
015300                                                 VALUE 'ITC053'.  ITCWSMC
015400             15  CWSMC-INVALID-REQUEST-CP1016        PIC X(6)     ITCWSMC
015500                                                 VALUE 'ITC054'.  ITCWSMC
015600             15  CWSMC-CICS-ERROR-CP1010             PIC X(6)     ITCWSMC
015700                                                 VALUE 'ITC055'.  ITCWSMC
015800             15  CWSMC-CICS-ERROR-CP1012             PIC X(6)     ITCWSMC
015900                                                 VALUE 'ITC056'.  ITCWSMC
016000             15  CWSMC-CICS-ERROR-CP1014             PIC X(6)     ITCWSMC
016100                                                 VALUE 'ITC057'.  ITCWSMC
016200             15  CWSMC-CICS-ERROR-CP1016             PIC X(6)     ITCWSMC
016300                                                 VALUE 'ITC058'.  ITCWSMC
016400             15  CWSMC-CICS-ERROR-CP1108             PIC X(6)     ITCWSMC
016500                                                 VALUE 'ITC059'.  ITCWSMC
016600             15  CWSMC-DATA-ERROR-CP9534             PIC X(6)     ITCWSMC
016700                                                 VALUE 'ITC060'.  ITCWSMC
016800             15  CWSMC-DATA-ERROR-CP9536             PIC X(6)     ITCWSMC
016900                                                 VALUE 'ITC061'.  ITCWSMC
017000             15  CWSMC-DATA-ERROR-CP9538             PIC X(6)     ITCWSMC
017100                                                 VALUE 'ITC062'.  ITCWSMC
017200             15  CWSMC-DATA-ERROR-CP9540             PIC X(6)     ITCWSMC
017300                                                 VALUE 'ITC063'.  ITCWSMC
017400             15  CWSMC-DATA-ERROR-CP1108             PIC X(6)     ITCWSMC
017500                                                 VALUE 'ITC064'.  ITCWSMC
017600             15  CWSMC-INVALID-FUNCTION-CP5108       PIC X(6)     ITCWSMC
017700                                                 VALUE 'ITC065'.  ITCWSMC
017800             15  CWSMC-IMS-ERROR-CP5108              PIC X(6)     ITCWSMC
017900                                                 VALUE 'ITC066'.  ITCWSMC
018000             15  CWSMC-BAD-IMS-DB-STATUS-CP5108      PIC X(6)     ITCWSMC
018100                                                 VALUE 'ITC067'.  ITCWSMC
018200             15  CWSMC-DATA-ERROR-CP5108             PIC X(6)     ITCWSMC
018300                                                 VALUE 'ITC068'.  ITCWSMC
018400             15  CWSMC-MULTIPLE-USERS-TRY-AGAIN      PIC X(6)     ITCWSMC
018500                                                 VALUE 'ITC069'.  ITCWSMC
018600             15  CWSMC-DATE-SEQ-ERROR                PIC X(6)     ITCWSMC
018700                                                 VALUE 'ITC070'.  ITCWSMC
018800             15  CWSMC-DATA-ERROR-CP1012             PIC X(6)     ITCWSMC
018900                                                 VALUE 'ITC071'.  ITCWSMC
019000             15  CWSMC-COPY-RECORD-NOT-FOUND         PIC X(6)     ITCWSMC
019100                                                 VALUE 'ITC072'.  ITCWSMC
019200             15  CWSMC-NOT-IN-EFF-DT-RANGE           PIC X(6)     ITCWSMC
019300                                                 VALUE 'ITC073'.  ITCWSMC
019400             15  CWSMC-INCUR-DT-NOT-IN-RANGE         PIC X(6)     ITCWSMC
019500                                                 VALUE 'ITC074'.  ITCWSMC
019600             15  CWSMC-RECPT-DT-NOT-IN-RANGE         PIC X(6)     ITCWSMC
019700                                                 VALUE 'ITC075'.  ITCWSMC
019800             15  CWSMC-INVALID-FUNCTION-CP2108       PIC X(6)     ITCWSMC
019900                                                 VALUE 'ITC076'.  ITCWSMC
020000             15  CWSMC-UPDATE-SUCESSFUL              PIC X(6)     ITCWSMC
020100                                                 VALUE 'ITC077'.  ITCWSMC
020200             15  CWSMC-DATA-ERROR-CP1016             PIC X(6)     ITCWSMC
020300                                                 VALUE 'ITC078'.  ITCWSMC
020400             15  CWSMC-NO-UPDATE-TO-REJECT-REC       PIC X(6)     ITCWSMC
020500                                                 VALUE 'ITC079'.  ITCWSMC
020600             15  CWSMC-INPUT-CNTRL-PLAN-NE-PP        PIC X(6)     ITCWSMC
020700                                                 VALUE 'ITC080'.  ITCWSMC
020800             15  CWSMC-PP-INVALID-CLAIM-TYPE         PIC X(6)     ITCWSMC
020900                                                 VALUE 'ITC081'.  ITCWSMC
021000             15  CWSMC-CICS-ERROR-CP1110             PIC X(6)     ITCWSMC
021100                                                 VALUE 'ITC082'.  ITCWSMC
021200             15  CWSMC-PROGRAM-ID-ERROR-CP1110       PIC X(6)     ITCWSMC
021300                                                 VALUE 'ITC083'.  ITCWSMC
021400             15  CWSMC-INVALID-REQUEST-CP1110        PIC X(6)     ITCWSMC
021500                                                 VALUE 'ITC084'.  ITCWSMC
021600             15  CWSMC-EFF-END-DATE-INVALID          PIC X(6)     ITCWSMC
021700                                                 VALUE 'ITC085'.  ITCWSMC
021800             15  CWSMC-NOT-VALID-FOR-OPTION          PIC X(6)     ITCWSMC
021900                                                 VALUE 'ITC086'.  ITCWSMC
022000             15  CWSMC-DATA-ERROR-CP1110             PIC X(6)     ITCWSMC
022100                                                 VALUE 'ITC087'.  ITCWSMC
022200             15  CWSMC-INVALID-FUNCTION-CP1110       PIC X(6)     ITCWSMC
022300                                                 VALUE 'ITC088'.  ITCWSMC
022400             15  CWSMC-EFFECTIVE-DATE-OVERLAP        PIC X(6)     ITCWSMC
022500                                                 VALUE 'ITC089'.  ITCWSMC
022600             15  CWSMC-EFF-DATE-NOT-GTR-EXIST        PIC X(6)     ITCWSMC
022700                                                 VALUE 'ITC090'.  ITCWSMC
022800             15  CWSMC-INVLD-FUNC-REC-ACTIVE         PIC X(6)     ITCWSMC
022900                                                 VALUE 'ITC091'.  ITCWSMC
023000             15  CWSMC-DATA-ERROR-CP9152             PIC X(6)     ITCWSMC
023100                                                 VALUE 'ITC092'.  ITCWSMC
023200             15  CWSMC-INVALID-FUNCTION-CP9152       PIC X(6)     ITCWSMC
023300                                                 VALUE 'ITC093'.  ITCWSMC
023400             15  CWSMC-END-DATE-PRIOR-TO-START       PIC X(6)     ITCWSMC
023500                                                 VALUE 'ITC094'.  ITCWSMC
023600             15  CWSMC-R-I-END-DT-GT-EFF-END         PIC X(6)     ITCWSMC
023700                                                 VALUE 'ITC095'.  ITCWSMC
023800             15  CWSMC-R-I-END-CROSS-BOUNDARY        PIC X(6)     ITCWSMC
023900                                                 VALUE 'ITC096'.  ITCWSMC
024000             15  CWSMC-REJECT-RECORD-AWAIT-ACK       PIC X(6)     ITCWSMC
024100                                                 VALUE 'ITC097'.  ITCWSMC
024200             15  CWSMC-AWAIT-ACKNOWLEDGEMENT         PIC X(6)     ITCWSMC
024300                                                 VALUE 'ITC098'.  ITCWSMC
024400*                                                                 ITCWSMC
024500* * * * * * * * * * * * 09/30/93  BEGIN * * * * * * * * * * * * * ITCWSMC
024600*                                                                 ITCWSMC
024700             15  CWSMC-ITCDBJ00-FILE-ERROR           PIC X(6)     ITCWSMC
024800                                                 VALUE 'ITC099'.  ITCWSMC
024900*                                                                 ITCWSMC
025000* * * * * * * * * * * * 09/30/93  END * * * * * * * * * * * * * * ITCWSMC
025100*                                                                 ITCWSMC
025200             15  CWSMC-INQUIRY-SESSION-ENDED         PIC X(6)     ITCWSMC
025300                                                 VALUE 'ITC100'.  ITCWSMC
025400             15  CWSMC-DATA-ERROR-CP5012             PIC X(6)     ITCWSMC
025500                                                 VALUE 'ITC101'.  ITCWSMC
025600             15  CWSMC-CONTROL-PLAN-ONLY             PIC X(6)     ITCWSMC
025700                                                 VALUE 'ITC102'.  ITCWSMC
025800             15  CWSMC-NOT-VALID-FOR-STD-RULE        PIC X(6)     ITCWSMC
025900                                                 VALUE 'ITC103'.  ITCWSMC
026000             15  CWSMC-AEA-AMT-MUST-BE-NUMERIC       PIC X(6)     ITCWSMC
026100                                                 VALUE 'ITC104'.  ITCWSMC
026200             15  CWSMC-AEA-AMT-MUST-BE-SPACE         PIC X(6)     ITCWSMC
026300                                                 VALUE 'ITC105'.  ITCWSMC
026400             15  CWSMC-R-I-END-GTR-EFF-END-DT        PIC X(6)     ITCWSMC
026500                                                 VALUE 'ITC106'.  ITCWSMC
026600             15  CWSMC-CORRECT-HIGHLIGHTED-FLDS      PIC X(6)     ITCWSMC
026700                                                 VALUE 'ITC200'.  ITCWSMC
026800             15  CWSMC-GENERAL-CICS-ERROR            PIC X(6)     ITCWSMC
026900                                                 VALUE 'ITC201'.  ITCWSMC
027000             15  CWSMC-RECORD-NOT-FOUND              PIC X(6)     ITCWSMC
027100                                                 VALUE 'ITC202'.  ITCWSMC
027200             15  CWSMC-FUNCTION-CANCELLED            PIC X(6)     ITCWSMC
027300                                                 VALUE 'ITC203'.  ITCWSMC
027400             15  CWSMC-INVALID-OPTION                PIC X(6)     ITCWSMC
027500                                                 VALUE 'ITC204'.  ITCWSMC
027600             15  CWSMC-INVALID-SCROLL-OPTION         PIC X(6)     ITCWSMC
027700                                                 VALUE 'ITC205'.  ITCWSMC
027800             15  CWSMC-END-OF-SCROLL                 PIC X(6)     ITCWSMC
027900                                                 VALUE 'ITC206'.  ITCWSMC
028000             15  CWSMC-FAILED-REL                    PIC X(6)     ITCWSMC
028100                                                 VALUE 'ITC207'.  ITCWSMC
028200             15  CWSMC-DATA-ACCEPTED                 PIC X(6)     ITCWSMC
028300                                                 VALUE 'ITC208'.  ITCWSMC
028400             15  CWSMC-CLAIM-REL-FOR-EXTRACT         PIC X(6)     ITCWSMC
028500                                                 VALUE 'ITC209'.  ITCWSMC
028600             15  CWSMC-OPT-CD-REJ-CD-CONFLICT        PIC X(6)     ITCWSMC
028700                                                 VALUE 'ITC210'.  ITCWSMC
028800             15  CWSMC-INVALID-FUNCTION              PIC X(6)     ITCWSMC
028900                                                 VALUE 'ITC211'.  ITCWSMC
029000             15  CWSMC-INVALID-REC-SEQNUM            PIC X(6)     ITCWSMC
029100                                                 VALUE 'ITC212'.  ITCWSMC
029200             15  CWSMC-PROGRAM-ID-ERROR              PIC X(6)     ITCWSMC
029300                                                 VALUE 'ITC213'.  ITCWSMC
029400             15  CWSMC-DATASET-ID-ERROR              PIC X(6)     ITCWSMC
029500                                                 VALUE 'ITC214'.  ITCWSMC
029600             15  CWSMC-LENGTH-ERROR                  PIC X(6)     ITCWSMC
029700                                                 VALUE 'ITC215'.  ITCWSMC
029800             15  CWSMC-INVALID-PSWD                  PIC X(6)     ITCWSMC
029900                                                 VALUE 'ITC216'.  ITCWSMC
030000             15  CWSMC-INVALID-REQUEST-SUB           PIC X(6)     ITCWSMC
030100                                                 VALUE 'ITC217'.  ITCWSMC
030200             15  CWSMC-INVALID-REQ-LINE-OF-SERV      PIC X(6)     ITCWSMC
030300                                                 VALUE 'ITC218'.  ITCWSMC
030400             15  CWSMC-ESCAPE-MSG                    PIC X(6)     ITCWSMC
030500                                                 VALUE 'ITC219'.  ITCWSMC
030600             15  CWSMC-ESCAPE-INVALID                PIC X(6)     ITCWSMC
030700                                                 VALUE 'ITC220'.  ITCWSMC
030800             15  CWSMC-PP-ERROR                      PIC X(6)     ITCWSMC
030900                                                 VALUE 'ITC221'.  ITCWSMC
031000             15  CWSMC-EOF-FILE-TRAILER              PIC X(6)     ITCWSMC
031100                                                 VALUE 'ITC222'.  ITCWSMC
031200             15  CWSMC-INV-OPT-INV-REQ-SUB           PIC X(6)     ITCWSMC
031300                                                 VALUE 'ITC223'.  ITCWSMC
031400             15  CWSMC-INV-OPT-INV-LOS               PIC X(6)     ITCWSMC
031500                                                 VALUE 'ITC224'.  ITCWSMC
031600             15  CWSMC-PP-IND-NOT-A-SCCF-CLAIM       PIC X(6)     ITCWSMC
031700                                                 VALUE 'ITC225'.  ITCWSMC
031800             15  CWSMC-GENERAL-IMS-ERROR             PIC X(6)     ITCWSMC
031900                                                 VALUE 'ITC226'.  ITCWSMC
032000             15  CWSMC-INVALID-REQ-SPEC-NOTAT        PIC X(6)     ITCWSMC
032100                                                 VALUE 'ITC227'.  ITCWSMC
032200             15  CWSMC-CC-SESSION-ENDED              PIC X(6)     ITCWSMC
032300                                                 VALUE 'ITC300'.  ITCWSMC
032400             15  CWSMC-DE-FAILED-REL                 PIC X(6)     ITCWSMC
032500                                                 VALUE 'ITC400'.  ITCWSMC
032600             15  CWSMC-DE-SESSION-ENDED              PIC X(6)     ITCWSMC
032700                                                 VALUE 'ITC401'.  ITCWSMC
032800             15  CWSMC-DE-PF-PA-KEY-ERROR            PIC X(6)     ITCWSMC
032900                                                 VALUE 'ITC402'.  ITCWSMC
033000             15  CWSMC-DE-CLAIM-DELETED              PIC X(6)     ITCWSMC
033100                                                 VALUE 'ITC403'.  ITCWSMC
033200             15  CWSMC-DE-PP-REC-NOT-FOUND           PIC X(6)     ITCWSMC
033300                                                 VALUE 'ITC404'.  ITCWSMC
033400             15  CWSMC-DE-PP-EDIT-ERROR              PIC X(6)     ITCWSMC
033500                                                 VALUE 'ITC405'.  ITCWSMC
033600             15  CWSMC-DE-PP-CNTRL-PLAN-NE           PIC X(6)     ITCWSMC
033700                                                 VALUE 'ITC406'.  ITCWSMC
033800             15  CWSMC-DE-PP-INVALID-CLAIM-TYPE      PIC X(6)     ITCWSMC
033900                                                 VALUE 'ITC407'.  ITCWSMC
034000             15  CWSMC-DE-PP-NOT-A-SCCF-CLAIM        PIC X(6)     ITCWSMC
034100                                                 VALUE 'ITC408'.  ITCWSMC
034200             15  CWSMC-DE-RM-NOT-ALLOWED             PIC X(6)     ITCWSMC
034300                                                 VALUE 'ITC409'.  ITCWSMC
034400             15  CWSMC-DE-RM-DUP-FLD-EQUAL           PIC X(6)     ITCWSMC
034500                                                 VALUE 'ITC410'.  ITCWSMC
034600             15  CWSMC-DE-INVALID-OPTION-CHG         PIC X(6)     ITCWSMC
034700                                                 VALUE 'ITC411'.  ITCWSMC
034800******************  ADDE FOR RELAESE 7.0 **********************   ITCWSMC
034900             15  CWSMC-INVALID-REL-DATE              PIC X(6)     ITCWSMC
035000                                                 VALUE 'ITC499'.  ITCWSMC
035100******************  ADDE FOR RELAESE 7.0 **********************   ITCWSMC
035200             15  CWSMC-RI-END-DT-OVERLAPS            PIC X(6)     ITCWSMC
035300                                                 VALUE 'ITC600'.  ITCWSMC
035400             15  CWSMC-DUPLICATE-REC-RECEPT          PIC X(6)     ITCWSMC
035500                                                 VALUE 'ITC601'.  ITCWSMC
035600             15  CWSMC-NOT-FOUND-ACKNW-RECEPT        PIC X(6)     ITCWSMC
035700                                                 VALUE 'ITC602'.  ITCWSMC
035800             15  CWSMC-NOT-FOUND-UPDATE-RECEPT       PIC X(6)     ITCWSMC
035801                                                 VALUE 'ITC603'.  ITCWSMC
035802             15  CWSMC-NOT-FOUND-ACKNW-TRN-IND       PIC X(6)     ITCWSMC
035803                                                 VALUE 'ITC604'.  ITCWSMC
035804             15  CWSMC-NOT-FOUND-UPDATE-TRN-IND      PIC X(6)     ITCWSMC
035805                                                 VALUE 'ITC605'.  ITCWSMC
036000             15  CWSMC-SESSION-ENDED                  PIC X(6)    ITCWSMC
036100                                                 VALUE 'ITC700'.  ITCWSMC
036200             15  CWSMC-INVALID-CLERK-NO               PIC X(6)    ITCWSMC
036300                                                 VALUE 'ITC701'.  ITCWSMC
036400             15  CWSMC-ENTER-OPTION-CLERK             PIC X(6)    ITCWSMC
036500                                                 VALUE 'ITC702'.  ITCWSMC
036600             15  CWSMC-DUPL-NOTE-EXISTS               PIC X(6)    ITCWSMC
036700                                                 VALUE 'ITC703'.  ITCWSMC
036800             15  CWSMC-CONFIRM-DATA                   PIC X(6)    ITCWSMC
036900                                                 VALUE 'ITC704'.  ITCWSMC
037000             15  CWSMC-ENTER-REQ-DATA-COMMENTS        PIC X(6)    ITCWSMC
037100                                                 VALUE 'ITC705'.  ITCWSMC
037200             15  CWSMC-DATA-ERROR-CP9264              PIC X(6)    ITCWSMC
037300                                                 VALUE 'ITC800'.  ITCWSMC
037400             15  CWSMC-INVALID-FUNCTION-CP9264        PIC X(6)    ITCWSMC
037500                                                 VALUE 'ITC801'.  ITCWSMC
037600             15  CWSMC-VALID-PREFIX-NON-PP            PIC X(6)    ITCWSMC
037700                                                 VALUE 'ITC899'.  ITCWSMC
037800             15  CWSMC-PP-REC-VERSION-NUM-ERR         PIC X(6)    ITCWSMC
037900                                                 VALUE 'ITC900'.  ITCWSMC
038000             15  CWSMC-PLAN-PAYER-MUST-BE-TWO         PIC X(6)    ITCWSMC
038100                                                 VALUE 'ITC901'.  ITCWSMC
038200             15  CWSMC-ADJUST-EDIT-INVALID            PIC X(6)    ITCWSMC
038300                                                 VALUE 'ITC902'.  ITCWSMC
038301             15  CWSMC-SUBMISS-IND-INVALID            PIC X(6)    ITCWSMC
038302                                                 VALUE 'ITC903'.  ITCWSMC
038303             15  CWSMC-TYPE-OF-TRAN-REQUIRED          PIC X(6)    ITCWSMC
038304                                                 VALUE 'ITC904'.  ITCWSMC
038305             15  CWSMC-INVALID-VALUE-ENTERED          PIC X(6)    ITCWSMC
038306                                                 VALUE 'ITC905'.  ITCWSMC
038307             15  CWSMC-CFA-CD-INVALID                 PIC X(6)    ITCWSMC
038308                                                 VALUE 'ITC906'.  ITCWSMC
038309*MB NEXT 4:                                                       ITCWSMC
038310             15  CWSMC-BAD-CLAIM-TYP                 PIC X(6)     ITCP9534
038311                                                 VALUE 'ITC907'.  ITCWSMC
038312             15  CWSMC-BAD-LOC-PLAN-CD               PIC X(6)     ITCP9534
038313                                                 VALUE 'ITC908'.  ITCWSMC
038314             15  CWSMC-BAD-CLAIM-LOC-PLAN            PIC X(6)     ITCP9534
038315                                                 VALUE 'ITC909'.  ITCWSMC
038316             15  CWSMC-INCMPTBL-CLM-TYP              PIC X(6)     ITCP9534
038317                                                 VALUE 'ITC910'.  ITCWSMC
038316             15  CWSMC-DB2-ERR                       PIC X(6)     00387
038317                                                 VALUE 'ITC911'.  00388
