000100 IDENTIFICATION DIVISION.                                              ***
000200 PROGRAM-ID.  ITCP9100.                                                ...
000300 AUTHOR.      ITS DESIGN CENTER.                                  02/28/03
000400 INSTALLATION.                                                    ITCP9100
000500 DATE-WRITTEN.                                                       LV002
000600 DATE-COMPILED.                                                   00006
000700******************************************************************00007
000800*                                                                *00008
000900*             INTER-PLAN TELEPROCESSING SERVICES (ITS)           *00009
001000*                                                                *00010
001100*                                                                *00011
001200*   MODULE:       ITCP9100                                       *00012
001300*   NAME:         PARAMETRS DB 21 LOAD MODULE                    *00013
001400*   VERSION:      01.1                                           *00014
001500*   DATE:         JUNE  17, 2005                                 *00015
001600*                                                                *00016
001700*   APPLICATION:  STANDARD CLAIMS COLLECTION FACILITY (SCCF)     *00017
001800*   TYPE:         COMMON                                         *00018
001900*   FUNCTION:     THIS MODULE IS THE PARAMETERS DB 21 LOAD       *00019
002000*                                                                *00020
002100*                                                                *00021
002200*   MODIFICATION                                                 *00022
002300*        HISTORY: 04/25/2003 01.0 TECH MEMO ITS-TM-2003-04-R8.7  *00023
002400*                 (VY JACKSON)  CR02-0188                        *00024
002500*                                                                *00025
002510*                 06/17/05   01.1 DEAD CODE REVIEW REL 9.2       *00026
002600*   NOTES:                                                       *00026
002700*                                                                *00027
002800******************************************************************00028
002900 ENVIRONMENT DIVISION.                                            00029
003000 CONFIGURATION SECTION.                                           00030
003100 SOURCE-COMPUTER.  IBM-370.                                       00031
003200 OBJECT-COMPUTER.  IBM-370.                                       00032
003300*                                                                 00033
003400 INPUT-OUTPUT SECTION.                                            00034
003500 FILE-CONTROL.                                                    00035
003600                                                                  00036
003700     SELECT IN-RECORD-TYPE-21-FILE   ASSIGN TO IRECRD21.          00037
003800     SELECT OUT-RECORD-TYPE-21-FILE  ASSIGN TO ORECRD21.          00038
003900 DATA DIVISION.                                                   00039
004000                                                                  00040
004100 FILE SECTION.                                                    00041
004200                                                                  00042
004300 FD  IN-RECORD-TYPE-21-FILE                                       00043
004400     RECORDING MODE IS F                                          00044
004500     LABEL RECORDS ARE STANDARD                                   00045
004600     BLOCK CONTAINS 0 RECORDS                                     00046
004700     DATA RECORD IS RT-21-IN-REC.                                 00047
004800                                                                  00048
004900 01  RT-21-IN-REC                     PIC X(125).                 00049
005000                                                                  00050
005100 FD  OUT-RECORD-TYPE-21-FILE                                      00051
005200     RECORDING MODE IS F                                          00052
005300     LABEL RECORDS ARE STANDARD                                   00053
005400     BLOCK CONTAINS 0 RECORDS                                     00054
005500     DATA RECORD IS RT-21-OUT-REC.                                00055
005600                                                                  00056
005700 01  RT-21-OUT-REC                    PIC X(125).                 00057
005800                                                                  00058
005900 WORKING-STORAGE SECTION.                                         00059
006000                                                                  00060
006100 01  FILLER                           PIC X(4032) VALUE           00061
006200      '**** BEGIN ITCP9100 WORKING-STORAGE ****'.                 00062
006300                                                                  00063
006400 01  WS-DISPLAY.                                                  00064
006500     05  WS-MESSAGE.                                              00065
006600         10  WS-PROG-NAME          PIC X(8).                      00066
006700         10  WS-COMMON-DISPLAY     PIC X(34) VALUE                00067
006800            '-PARAMETERS DB 21 LOAD   '.                          00068
006900         10  WS-MODULE-DISPLAY    PIC X(8)   VALUE                00069
007000            ' MODULE '.                                           00070
007100         10  WS-RELEASE-DISPLAY   PIC X(56)  VALUE                00071
007200            'VERSION 01.1 RELEASED ON 06/17/2005'.                00073
007300                                                                  00073
007400 01  WS-CALL-PROGRAM     PIC X(8)  VALUE SPACES.                  00074
007500 01  WS-GR               PIC X(2)  VALUE 'GR'.                    00075
007600 01  WS-TJ               PIC X(2)  VALUE 'TJ'.                    00076
007700 01  WS-RETURN-CODE                PIC 9999 VALUE ZERO.           00077
007800                                                                  00078
007900 01  WS-SIAPC-MODULE-NAME-1.                                      00079
008000     10  FILLER                      PIC X(21)                    00080
008100             VALUE 'SIAPC-MODULE-NAME 1:='.                       00081
008200 01  WS-SIAPC-MODULE-NAME-2.                                      00082
008300     10  FILLER                      PIC X(21)                    00083
008400             VALUE 'SIAPC-MODULE-NAME 2:='.                       00084
008500 01  WS-SIAPC-MODULE-NAME-3.                                      00085
008600     10  FILLER                      PIC X(21)                    00086
008700             VALUE 'SIAPC-MODULE-NAME 3:='.                       00087
008800 01  WS-SIAPC-MODULE-NAME-4.                                      00088
008900     10  FILLER                      PIC X(21)                    00089
009000             VALUE 'SIAPC-MODULE-NAME 4:='.                       00090
009100 01  WS-SIAPC-MODULE-NAME-5.                                      00091
009200     10  FILLER                      PIC X(21)                    00092
009300             VALUE 'SIAPC-MODULE-NAME 5:='.                       00093
009400                                                                  00094
009500 01  WS-CURRENT-DATE.                                             00095
009600     05  WS-CURRENT-YY                   PIC X(2).                00096
009700     05  WS-CURRENT-MM                   PIC X(2).                00097
009800     05  WS-CURRENT-DD                   PIC X(2).                00098
009900                                                                  00099
010500 01  WS-CURRENT-TIME.                                             00105
010600     05  WS-CURRENT-HOUR                 PIC X(2).                00106
010700     05  WS-CURRENT-MINUTE               PIC X(2).                00107
010800     05  WS-CURRENT-SECOND               PIC X(2).                00108
010900                                                                  00109
011000 01  WS-CONSTANTS.                                                00110
011100     05  WS-YES                  PIC X(01)  VALUE 'Y'.            00111
011200     05  WS-NO                   PIC X(01)  VALUE 'N'.            00112
011300     05  WS-21                   PIC X(02)  VALUE '21'.           00113
011400                                                                  00114
011500/                                                                 00115
011600 01  ITCWSCC.                                                     00116
011700     COPY ITCWSCC.                                                00117
011800/                                                                 00118
011900 01  ITCWSFC.                                                     00119
012000     COPY ITCWSFC.                                                00120
012100/                                                                 00121
012200 01  ITCWSDT.                                                     00122
012300     COPY ITCWSDT.                                                00123
012400/                                                                 00124
012500 01  ITCIADBP-RECORD.                                             00125
012600     COPY ITCIADBP.                                               00126
012700/                                                                 00127
012800 01  ITCDBP21-RECORD.                                             00128
012900     COPY ITCDBP21.                                               00129
013000/                                                                 00130
013100 01  WS-HOLD-TYPE-21-TABLES.                                      00131
013200     05  WS-HOLD-NEW-TYPE-21-TABLE.                               00132
013300         10  WSH-NEW-TYPE-21-TABLE OCCURS 1000 TIMES PIC X(125).  00133
013400     05  WS-HOLD-OLD-TYPE-21-TABLE.                               00134
013500         10  WSH-OLD-TYPE-21-TABLE OCCURS 1000 TIMES PIC X(125).  00135
013600                                                                  00136
013700                                                                  00137
013800                                                                  00138
013900 01  WSO-SUB                             PIC S9(04) VALUE 0.      00139
014000 01  WSN-SUB                             PIC S9(04) VALUE 0.      00140
014100 01  WSO-21-MAX-CNT                      PIC  9(04) VALUE 0.      00141
014200 01  WSN-21-MAX-CNT                      PIC  9(04) VALUE 0.      00142
014300 01  WS-NEW-21-RECS-RD-CNTR              PIC  9(04) VALUE 0.      00143
014400 01  WS-NEW-21-RECS-WR-CNTR              PIC  9(04) VALUE 0.      00144
014500 01  WS-OLD-21-RECS-WR-CNTR              PIC  9(04) VALUE 0.      00145
014600 01  WS-OLD-21-RECS-DL-CNTR              PIC  9(04) VALUE 0.      00146
014700                                                                  00147
014800                                                                  00148
014900 01  WS-NEW-21-REC.                                               00149
015000     05  WS-NEW-21-KEY-DATA.                                      00150
015100         10  WS-NEW-21-REC-TYPE             PIC X(02).            00151
015200         10  WS-NEW-21-POS-2                PIC X(01).            00152
015300         10  WS-NEW-21-ACCT-TYPE-CD         PIC X(01).            00153
015400         10  WS-NEW-21-RATE-EFF-YEAR        PIC X(02).            00154
015500         10  WS-NEW-21-RATE-EFF-YEAR-9   REDEFINES                00155
015600             WS-NEW-21-RATE-EFF-YEAR        PIC 9(02).            00156
015700     05  WS-NEW-21-ACCT-TYPE-CD-RATE        PIC 9(04).            00157
015800     05  WS-NEW-21-FILLER                   PIC X(115).           00158
015900                                                                  00159
016000 01  WS-OLD-21-REC.                                               00160
016100     05  WS-OLD-21-KEY-DATA.                                      00161
016200         10  WS-OLD-21-REC-TYPE             PIC X(02).            00162
016300         10  WS-OLD-21-POS-2                PIC X(01).            00163
016400         10  WS-OLD-21-ACCT-TYPE-CD         PIC X(01).            00164
016500         10  WS-OLD-21-RATE-EFF-YEAR        PIC X(02).            00165
016600         10  WS-OLD-21-RATE-EFF-YEAR-9   REDEFINES                00166
016700             WS-OLD-21-RATE-EFF-YEAR        PIC 9(02).            00167
016800     05  WS-OLD-21-ACCT-TYPE-CD-RATE        PIC 9(04).            00168
016900     05  WS-OLD-21-FILLER                   PIC X(115).           00169
017000                                                                  00170
017100 01  WS-INDICATORS.                                               00171
017200     05  WS-EOF-OLD-IND               PIC X(01) VALUE 'N'.        00172
017300         88  WS-EOF-OLD               VALUE 'Y'.                  00173
017400                                                                  00174
017500     05  WS-EOF-NEW-IND               PIC X(01) VALUE 'N'.        00175
017600         88  WS-EOF-NEW               VALUE 'Y'.                  00176
017700                                                                  00177
017800     05  WS-EOR-OLD-IND               PIC X(01) VALUE 'N'.        00178
017900         88  WS-EOR-OLD               VALUE 'Y'.                  00179
018000                                                                  00180
018100     05  WS-EOR-NEW-IND               PIC X(01) VALUE 'N'.        00181
018200         88  WS-EOR-NEW               VALUE 'Y'.                  00182
018300                                                                  00183
018400     05  WS-21-ERR-IND                PIC X(01) VALUE 'N'.        00184
018500         88  WS-21-ERR                VALUE 'Y'.                  00185
018600                                                                  00186
018700                                                                  00187
018800 01  WS-ERROR-MESSAGES.                                           00188
018900     05  WS-ASTERISKS                 PIC X(80) VALUE ALL '*'.    00189
019000     05  WS-ERROR-MSG-01              PIC X(80) VALUE             00190
019100         ' RECORD TYPE (POSITIONS 1 - 2) MUST BE NUMERIC '.       00191
019200     05  WS-ERROR-MSG-02              PIC X(80) VALUE             00192
019300         ' POSITION 3 MUST = SPACE '.                             00193
019400     05  WS-ERROR-MSG-03              PIC X(80) VALUE             00194
019500         ' ACCOUNT TYPE (POSITION 4) CANNOT BE SPACE '.           00195
019600     05  WS-ERROR-MSG-04              PIC X(80) VALUE             00196
019700         ' RATE EFFECTIVE YEAR (POSITIONS 5-6) MUST BE NUMERIC '. 00197
019800     05  WS-ERROR-MSG-05              PIC X(80) VALUE             00198
019900         ' RATE AMOUNT (POSITIONS 7-13) MUST BE NUMERIC '.        00199
020000     05  WS-ERROR-MSG-06              PIC X(80) VALUE             00200
020100         ' INPUT FILE (IRECRD21) DUMMIED OR EMPTY '.              00201
020200     05  WS-PDB-MISSING-REC-MSG.                                  00202
020300         10  FILLER                   PIC X(34) VALUE             00203
020400         'PARAMETER DATABASE 21 RECORD ERROR'.                    00204
020500 01  FILLER                           PIC X(41) VALUE             00205
020600      '**** END OF ITCP9100 WORKING-STORAGE ****'.                00206
020700                                                                  00207
020800 LINKAGE SECTION.                                                 00208
020900                                                                  00209
021000 01  ITSIAPC-RECORD.                                              00210
021100     COPY ITSIAPC.                                                00211
021200                                                                  00212
021300 01  ITCPCP00.                                                    00213
021400     COPY ITCPCP00.                                               00214
021500                                                                  00215
021600                                                                  00216
021700 PROCEDURE DIVISION    USING  ITSIAPC-RECORD,                     00217
021800                              ITCPCP00.                           00218
021900                                                                  00219
022000     PERFORM 1000-INITIALIZATION                                  00220
022100        THRU 1000-EXIT.                                           00221
022200                                                                  00222
022300     IF  WS-RETURN-CODE = ZERO                                    00223
022400         PERFORM 4000-READ-NEW-21-RECORDS                         00224
022500            THRU 4000-EXIT                                        00225
022600            UNTIL WS-21-ERR                                       00226
022700             OR   WS-EOF-NEW                                      00227
022800     END-IF.                                                      00228
022900                                                                  00229
023000     IF  WS-RETURN-CODE = ZERO                                    00230
023100         PERFORM 4500-READ-OLD-21-RECORDS                         00231
023200           THRU 4500-EXIT                                         00232
023300     END-IF.                                                      00233
023400                                                                  00234
023500     IF  WS-RETURN-CODE = ZERO                                    00235
023600         PERFORM 5000-DELETE-OLD-21-RECORDS                       00236
023700            THRU 5000-EXIT                                        00237
023800     END-IF.                                                      00238
023900                                                                  00239
024000     IF  WS-RETURN-CODE = ZERO                                    00240
024100         PERFORM 5500-LOAD-NEW-21-RECORDS                         00241
024200            THRU 5500-EXIT                                        00242
024300     END-IF.                                                      00243
024400                                                                  00244
024500     PERFORM 9000-END-OF-JOB                                      00245
024600        THRU 9000-EXIT.                                           00246
024700                                                                  00247
024800     IF  WS-RETURN-CODE NOT = ZERO                                00248
024900         MOVE  CWSCC-ABORT TO SIAPC-RETURN-IND                    00249
025000     END-IF.                                                      00250
025100                                                                  00251
025200     GOBACK.                                                      00252
025300/                                                                 00253
025400 1000-INITIALIZATION.                                             00254
025500                                                                  00255
025600     MOVE 'ITCP9100'  TO WS-PROG-NAME.                            00256
025700     DISPLAY          WS-DISPLAY.                                 00257
025800     OPEN INPUT       IN-RECORD-TYPE-21-FILE                      00258
025900     OPEN OUTPUT     OUT-RECORD-TYPE-21-FILE                      00259
026000     MOVE ZEROS            TO   WS-NEW-21-RECS-RD-CNTR            00260
026100                                WS-NEW-21-RECS-WR-CNTR            00261
026200                                WS-OLD-21-RECS-WR-CNTR            00262
026300                                WS-OLD-21-RECS-DL-CNTR.           00263
026400     MOVE SPACES           TO   WS-NEW-21-REC                     00264
026500                                WS-OLD-21-REC.                    00265
026600     MOVE WS-NO            TO   WS-INDICATORS.                    00266
026700     INITIALIZE                 WS-HOLD-NEW-TYPE-21-TABLE.        00267
026800     INITIALIZE                 WS-HOLD-OLD-TYPE-21-TABLE.        00268
026900     MOVE 'ITCP9100'       TO   SIAPC-MODULE-NAME (2).            00269
027000     MOVE 2                TO   SIAPC-MODULE-LEVEL                00270
027100                                                                  00271
027200     MOVE SPACES           TO   SIAPC-MODULE-NAME (3)             00272
027300                                SIAPC-MODULE-NAME (4)             00273
027400                                SIAPC-MODULE-NAME (5).            00274
027500                                                                  00275
027600     ACCEPT WS-CURRENT-DATE FROM DATE.                            00276
027700     MOVE WS-CURRENT-MM     TO  SIAPC-GREG-CURRENT-MONTH.         00277
027800     MOVE WS-CURRENT-DD     TO  SIAPC-GREG-CURRENT-DAY.           00278
027900     MOVE WS-CURRENT-YY     TO  SIAPC-GREG-CURRENT-YEAR.          00279
028000     MOVE WS-CURRENT-YY     TO  CWSDT-G-YEAR.                     00280
028100     MOVE WS-CURRENT-MM     TO  CWSDT-G-MONTH.                    00281
028200     MOVE WS-CURRENT-DD     TO  CWSDT-G-DAY.                      00282
028300     MOVE WS-GR             TO  CWSDT-DC-FROM.                    00283
028400     MOVE WS-TJ             TO  CWSDT-DC-TO.                      00284
028500                                                                  00285
028600     MOVE '2'               TO  CWSDT-DATE-IND.                   00286
028700                                                                  00287
028800     PERFORM X100-ITCPDDT-DATECONV THRU X100-ITCPDDT-DC-EXIT.     00288
028900                                                                  00289
029000     MOVE CWSDT-G-CENTURY   TO  SIAPC-GREG-CURRENT-CENTURY.       00290
029100     MOVE CWSDT-G-CENTURY   TO  SIAPC-JULIAN-CURRENT-CENTURY.     00291
029200     MOVE CWSDT-DC-JY       TO  SIAPC-JULIAN-CURRENT-YEAR.        00292
029300     MOVE CWSDT-DC-JD       TO  SIAPC-JULIAN-CURRENT-DAY.         00293
029400     MOVE WS-CURRENT-MM     TO  SIAPC-DISPLAY-CURRENT-MONTH.      00294
029500     MOVE WS-CURRENT-DD     TO  SIAPC-DISPLAY-CURRENT-DAY.        00295
029600     MOVE WS-CURRENT-YY     TO  SIAPC-DISPLAY-CURRENT-YEAR.       00296
029700                                                                  00297
029800     ACCEPT WS-CURRENT-TIME FROM TIME.                            00298
029900     MOVE WS-CURRENT-HOUR   TO  SIAPC-CURRENT-HOUR.               00299
030000     MOVE WS-CURRENT-MINUTE TO  SIAPC-CURRENT-MINUTE.             00300
030100     MOVE WS-CURRENT-SECOND TO  SIAPC-CURRENT-SECOND.             00301
030200     MOVE WS-CURRENT-HOUR   TO  SIAPC-DISPLAY-CURRENT-HOUR.       00302
030300     MOVE WS-CURRENT-MINUTE TO  SIAPC-DISPLAY-CURRENT-MINUTE.     00303
030400                                                                  00304
030500     IF CWSFC-CICS-INSTALLATION                                   00305
030600        MOVE CWSCC-OPEN-FILES-REQUEST TO  SIAPC-FUNCTION-CD-MAJOR 00306
030700        MOVE CWSCC-OPEN-AS-I-O-ACCESS TO  SIAPC-FUNCTION-CD-SUB   00307
030800        PERFORM 2000-CALL-ITCPX162-MODULE                         00308
030900           THRU 2000-EXIT                                         00309
031000     END-IF.                                                      00310
031100                                                                  00311
031200 1000-EXIT.                                                       00312
031300      EXIT.                                                       00313
031400                                                                  00314
031500/                                                                 00315
031600 2000-CALL-ITCPX162-MODULE.                                       00316
031700                                                                  00317
031800     PERFORM 9100-RESET-SIAPC-RETURN-DATA                         00318
031900        THRU 9100-EXIT                                            00319
032000                                                                  00320
032100     MOVE 3  TO         SIAPC-MODULE-LEVEL                        00321
032200     IF   CWSFC-CICS-INSTALLATION                                 00322
032300          MOVE 'ITCP2162'   TO       WS-CALL-PROGRAM              00323
032400                                     SIAPC-MODULE-NAME (3)        00324
032500          CALL WS-CALL-PROGRAM USING ITSIAPC-RECORD,              00325
032600                                     ITCIADBP-RECORD,             00326
032700                                     ITCPCP00                     00327
032800     ELSE                                                         00328
032900          MOVE 'ITCP6162'   TO       WS-CALL-PROGRAM              00329
033000                                     SIAPC-MODULE-NAME (3)        00330
033100          CALL WS-CALL-PROGRAM USING ITSIAPC-RECORD,              00331
033200                                     ITCIADBP-RECORD,             00332
033300                                     ITCPCP00.                    00333
033400                                                                  00334
033500     IF   SIAPC-RETURN-IND  =  CWSCC-SUCCESSFUL                   00335
033600          SUBTRACT 1 FROM      SIAPC-MODULE-LEVEL                 00336
033700          MOVE SPACES       TO SIAPC-MODULE-NAME (3)              00337
033800     ELSE                                                         00338
033900          PERFORM 2500-CHECK-RETURN-STATUS-CD                     00339
034000             THRU 2500-EXIT                                       00340
034100     END-IF                                                       00341
034200                                                                  00342
034300     IF WS-RETURN-CODE > ZERO                                     00343
034400        IF CWSFC-CICS-INSTALLATION                                00344
034500           DISPLAY '************************************'         00345
034600           DISPLAY '*** ERROR ITCP2162  MODULE '                  00346
034700           DISPLAY '************************************'         00347
034800        ELSE                                                      00348
034900           DISPLAY '************************************'         00349
035000           DISPLAY '*** ERROR ITCP6162  MODULE '                  00350
035100           DISPLAY '************************************'         00351
035200        END-IF                                                    00352
035300     END-IF.                                                      00353
035400                                                                  00354
035500 2000-EXIT.                                                       00355
035600       EXIT.                                                      00356
035700/                                                                 00357
035800 2500-CHECK-RETURN-STATUS-CD.                                     00358
035900                                                                  00359
036000     IF SIAPC-RETURN-IND  =  CWSCC-ABORT  OR                      00360
036100                             CWSCC-SEVERE-LOG                     00361
036200        MOVE     3600   TO   WS-RETURN-CODE                       00362
036400        DISPLAY WS-ASTERISKS                                      00363
036500        DISPLAY '*** ITCP9100 PROCESSING TERMINATED ***'          00364
036600        DISPLAY 'SIAPC-ERROR-MESSAGE   = ' SIAPC-ERROR-MESSAGE    00365
036700        DISPLAY 'SIAPC-RETURN-STATUS   = ' SIAPC-RETURN-STATUS    00366
036800        DISPLAY 'SIAPC-RETURN-MESSAGE-CD = '                      00367
036900                 SIAPC-RETURN-MESSAGE-CD                          00368
037000        DISPLAY 'SIAPC-ABEND-CODE      = ' SIAPC-ABEND-CODE       00369
037100        DISPLAY 'SIAPC-RETURN-IND      = ' SIAPC-RETURN-IND       00370
037200        DISPLAY  WS-SIAPC-MODULE-NAME-1    SIAPC-MODULE-NAME (1)  00371
037300        DISPLAY  WS-SIAPC-MODULE-NAME-2    SIAPC-MODULE-NAME (2)  00372
037400        DISPLAY  WS-SIAPC-MODULE-NAME-3    SIAPC-MODULE-NAME (3)  00373
037500        DISPLAY  WS-SIAPC-MODULE-NAME-4    SIAPC-MODULE-NAME (4)  00374
037600        DISPLAY  WS-SIAPC-MODULE-NAME-5    SIAPC-MODULE-NAME (5)  00375
037700        DISPLAY  WS-ASTERISKS                                     00376
037800        DISPLAY  SPACES                                           00377
037900                                                                  00378
038000     END-IF.                                                      00379
038100                                                                  00380
038200 2500-EXIT.                                                       00381
038300      EXIT.                                                       00382
038400/                                                                 00383
038500 4000-READ-NEW-21-RECORDS.                                        00384
038600                                                                  00385
038700     READ  IN-RECORD-TYPE-21-FILE INTO                            00386
038800           WS-NEW-21-REC                                          00387
038900        AT END                                                    00388
039000           MOVE WS-YES TO WS-EOF-NEW-IND                          00389
039100           MOVE WS-NEW-21-RECS-RD-CNTR TO WSN-21-MAX-CNT          00390
039200           IF   WS-NEW-21-RECS-RD-CNTR = 0                        00391
039300                MOVE WS-YES TO WS-21-ERR-IND                      00392
039400                DISPLAY WS-ERROR-MSG-06                           00393
039500                MOVE  3600  TO WS-RETURN-CODE                     00394
039700                MOVE  CWSCC-ABORT TO SIAPC-RETURN-IND             00395
039800           END-IF                                                 00396
039900           GO TO 4000-EXIT.                                       00397
040000                                                                  00398
040100      ADD 1 TO WS-NEW-21-RECS-RD-CNTR.                            00399
040200                                                                  00400
040300      IF WS-NEW-21-REC-TYPE NOT EQUAL '21'                        00401
040400         MOVE WS-YES TO WS-21-ERR-IND                             00402
040500         DISPLAY WS-ERROR-MSG-01                                  00403
040600      END-IF.                                                     00404
040700                                                                  00405
040800      IF WS-NEW-21-POS-2 NOT EQUAL SPACES                         00406
040900         MOVE WS-YES TO WS-21-ERR-IND                             00407
041000         DISPLAY WS-ERROR-MSG-02                                  00408
041100      END-IF.                                                     00409
041200                                                                  00410
041300      IF WS-NEW-21-ACCT-TYPE-CD EQUAL SPACES                      00411
041400         MOVE WS-YES TO WS-21-ERR-IND                             00412
041500         DISPLAY WS-ERROR-MSG-03                                  00413
041600      END-IF.                                                     00414
041700                                                                  00415
041800      IF WS-NEW-21-RATE-EFF-YEAR-9 NOT NUMERIC                    00416
041900         MOVE WS-YES TO WS-21-ERR-IND                             00417
042000         DISPLAY WS-ERROR-MSG-04                                  00418
042100      END-IF.                                                     00419
042200                                                                  00420
042300      IF WS-NEW-21-ACCT-TYPE-CD-RATE NOT NUMERIC                  00421
042400         MOVE WS-YES TO WS-21-ERR-IND                             00422
042500         DISPLAY WS-ERROR-MSG-05                                  00423
042600      END-IF.                                                     00424
042700                                                                  00425
042800      IF WS-21-ERR                                                00426
042900         DISPLAY 'NEW 21 RECS READ = ' WS-NEW-21-RECS-RD-CNTR     00427
043000         DISPLAY 'NEW 21 INPUT REC = ' WS-NEW-21-REC              00428
043100         MOVE  3600  TO WS-RETURN-CODE                            00429
043300         MOVE  CWSCC-ABORT TO SIAPC-RETURN-IND                    00430
043400      ELSE                                                        00431
043500         ADD +1 TO WSN-SUB                                        00432
043600         MOVE WS-NEW-21-REC TO WSH-NEW-TYPE-21-TABLE (WSN-SUB)    00433
043700      END-IF.                                                     00434
043800                                                                  00435
043900 4000-EXIT.                                                       00436
044000     EXIT.                                                        00437
044100/                                                                 00438
044200 4500-READ-OLD-21-RECORDS.                                        00439
044400     MOVE CWSCC-READ-FILES-REQUEST  TO SIAPC-FUNCTION-CD-MAJOR    00440
044500     MOVE CWSCC-GENERIC-READ-ACCESS TO SIAPC-FUNCTION-CD-SUB      00441
044600     MOVE SPACES                    TO CIADBP-RECORD              00442
044700     MOVE '21    '                  TO CIADBP-KEY                 00443
044800                                                                  00444
044900     PERFORM 4510-RD-OLD-21-LOOP                                  00445
045000        THRU 4510-EXIT                                            00446
045100        UNTIL WS-EOR-OLD.                                         00447
045200     MOVE WS-OLD-21-RECS-WR-CNTR TO WSO-21-MAX-CNT.               00448
045300 4500-EXIT.                                                       00449
045400     EXIT.                                                        00450
045500                                                                  00451
045600 4510-RD-OLD-21-LOOP.                                             00452
045800     PERFORM 2000-CALL-ITCPX162-MODULE                            00453
045900        THRU 2000-EXIT                                            00454
046000                                                                  00455
046100     EVALUATE TRUE                                                00456
046200        WHEN SIAPC-RETURN-STATUS   = CWSCC-RECORD-NOT-FOUND       00457
046300           MOVE CWSCC-READ-ERROR         TO SIAPC-RETURN-STATUS   00458
046400           MOVE WS-PDB-MISSING-REC-MSG   TO SIAPC-ERROR-MESSAGE   00459
046500           MOVE WS-YES                   TO WS-EOR-OLD-IND        00460
046600                                                                  00461
046700        WHEN SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                  00462
046800           MOVE CIADBP-RECORD            TO ITCDBP21-RECORD       00463
046900                                                                  00464
047000           IF CDBP21-REC-TYPE NOT = WS-21                         00465
047100              MOVE WS-YES                TO WS-EOR-OLD-IND        00466
047200           ELSE                                                   00467
047300              ADD +1 TO WSO-SUB                                   00468
047400              MOVE CDBP21-DATA TO WSH-OLD-TYPE-21-TABLE (WSO-SUB) 00469
047500              MOVE CDBP21-DATA TO RT-21-OUT-REC                   00470
047600              WRITE RT-21-OUT-REC                                 00471
047700              ADD +1 TO WS-OLD-21-RECS-WR-CNTR                    00472
047800           END-IF                                                 00473
047900                                                                  00474
048000        WHEN SIAPC-RETURN-STATUS   = CWSCC-END-OF-FILE            00475
048100           MOVE WS-YES               TO WS-EOR-OLD-IND            00476
048200                                                                  00477
048300        WHEN SIAPC-RETURN-IND = CWSCC-UNSUCCESSFUL                00478
048400             MOVE WS-YES                TO WS-EOR-OLD-IND         00479
048500                                                                  00480
048600        WHEN OTHER                                                00481
048700           MOVE CWSCC-READ-ERROR        TO SIAPC-RETURN-STATUS    00482
048800     END-EVALUATE                                                 00483
048900                                                                  00484
049000     .                                                            00485
049100                                                                  00486
049200 4510-EXIT.                                                       00487
049300     EXIT.                                                        00488
049400/                                                                 00489
049500 5000-DELETE-OLD-21-RECORDS.                                      00490
049700     IF  WS-OLD-21-RECS-WR-CNTR = ZERO                            00491
049800         GO TO 5000-EXIT.                                         00492
049900     MOVE CWSCC-DELETE-FILES-REQUEST  TO SIAPC-FUNCTION-CD-MAJOR  00493
050000     MOVE SPACES                      TO SIAPC-FUNCTION-CD-SUB    00494
050100     MOVE SPACES                      TO CIADBP-RECORD            00495
050200     MOVE SPACES                      TO ITCDBP21-RECORD          00496
050300                                                                  00497
050400     MOVE 1 TO WSO-SUB.                                           00498
050500     MOVE WSH-OLD-TYPE-21-TABLE (WSO-SUB) TO                      00499
050600          CDBP21-DATA.                                            00500
050700     MOVE CDBP21-KEY-DATA TO CIADBP-KEY                           00501
050800                                                                  00502
050900     MOVE WS-NO  TO WS-EOR-OLD-IND.                               00503
051000     PERFORM 5010-DEL-OLD-21-LOOP                                 00504
051100        THRU 5010-EXIT                                            00505
051200        UNTIL WS-EOR-OLD.                                         00506
051300 5000-EXIT.                                                       00507
051400     EXIT.                                                        00508
051500                                                                  00509
051600 5010-DEL-OLD-21-LOOP.                                            00510
051800     PERFORM 2000-CALL-ITCPX162-MODULE                            00511
051900        THRU 2000-EXIT                                            00512
052000                                                                  00513
052100     EVALUATE TRUE                                                00514
052200        WHEN SIAPC-RETURN-IND = CWSCC-UNSUCCESSFUL                00515
052300             MOVE WS-YES                TO WS-EOR-OLD-IND         00516
052400             MOVE  3600  TO WS-RETURN-CODE                        00517
052600                                                                  00518
052700        WHEN SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                  00519
052800             ADD  1 TO WS-OLD-21-RECS-DL-CNTR                     00520
052900             ADD  1 TO WSO-SUB                                    00521
053000             MOVE WSH-OLD-TYPE-21-TABLE (WSO-SUB) TO              00522
053100                  CDBP21-DATA                                     00523
053200             MOVE CDBP21-KEY-DATA TO CIADBP-KEY                   00524
053300                                                                  00525
053400     END-EVALUATE                                                 00526
053500                                                                  00527
053600     IF WSO-SUB GREATER THAN WSO-21-MAX-CNT                       00528
053700        MOVE WS-YES  TO WS-EOR-OLD-IND.                           00529
053800                                                                  00530
053900     IF SIAPC-RETURN-IND  = CWSCC-UNSUCCESSFUL                    00531
054000        DISPLAY WS-ASTERISKS                                      00532
054100        DISPLAY '** PROGRAM: '                                    00533
054200            SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)                00534
054300                '   ABORTING ... '                                00535
054400            DISPLAY '---- SIAPC CONTENTS ---'                     00536
054500            DISPLAY 'FUNCTION-CD-MAJOR = '                        00537
054600                     SIAPC-FUNCTION-CD-MAJOR                      00538
054700            DISPLAY 'FUNCTION-CD-SUB   = '                        00539
054800                     SIAPC-FUNCTION-CD-SUB                        00540
054900            DISPLAY 'RETURN-IND        = '                        00541
055000                     SIAPC-RETURN-IND                             00542
055100            DISPLAY 'RETURN-STATUS     = '                        00543
055200                     SIAPC-RETURN-STATUS                          00544
055300            DISPLAY 'ERROR-MESSAGE     = '                        00545
055400                     SIAPC-ERROR-MESSAGE                          00546
055500     END-IF                                                       00547
055600     .                                                            00548
055700                                                                  00549
055800 5010-EXIT.                                                       00550
055900     EXIT.                                                        00551
056000/                                                                 00552
056100/                                                                 00553
056200 5500-LOAD-NEW-21-RECORDS.                                        00554
056400                                                                  00555
056500     MOVE CWSCC-WRITE-FILES-REQUEST  TO SIAPC-FUNCTION-CD-MAJOR   00556
056600     MOVE SPACES                     TO SIAPC-FUNCTION-CD-SUB     00557
056700     MOVE SPACES                     TO CIADBP-RECORD             00558
056800     MOVE 1                          TO WSN-SUB.                  00559
056900     MOVE WSH-NEW-TYPE-21-TABLE (WSN-SUB) TO                      00560
057000                  CIADBP-RECORD.                                  00561
057100                                                                  00562
057200     PERFORM 5510-LD-NEW-21-LOOP                                  00563
057300        THRU 5510-EXIT                                            00564
057400        UNTIL WS-EOR-NEW.                                         00565
057500 5500-EXIT.                                                       00566
057600     EXIT.                                                        00567
057700                                                                  00568
057800 5510-LD-NEW-21-LOOP.                                             00569
058000                                                                  00570
058100     PERFORM 2000-CALL-ITCPX162-MODULE                            00571
058200        THRU 2000-EXIT.                                           00572
058300                                                                  00573
058400     EVALUATE TRUE                                                00574
058500        WHEN SIAPC-RETURN-IND = CWSCC-UNSUCCESSFUL                00575
058600             MOVE WS-YES          TO WS-EOR-NEW-IND               00576
058700             MOVE  3600  TO WS-RETURN-CODE                        00577
058900             MOVE  CWSCC-ABORT TO SIAPC-RETURN-IND                00578
059000                                                                  00579
059100        WHEN SIAPC-RETURN-IND = CWSCC-SUCCESSFUL                  00580
059200             ADD  1 TO WS-NEW-21-RECS-WR-CNTR                     00581
059300             ADD  1 TO WSN-SUB                                    00582
059400             MOVE WSH-NEW-TYPE-21-TABLE (WSN-SUB) TO              00583
059500                  CIADBP-RECORD                                   00584
059600                                                                  00585
059700                                                                  00586
059800     END-EVALUATE                                                 00587
059900                                                                  00588
060000     IF WSN-SUB GREATER THAN WSN-21-MAX-CNT                       00589
060100        MOVE WS-YES TO WS-EOR-NEW-IND.                            00590
060200                                                                  00591
060300     IF SIAPC-RETURN-IND  = CWSCC-UNSUCCESSFUL                    00592
060400        DISPLAY WS-ASTERISKS                                      00593
060500        DISPLAY '** PROGRAM: '                                    00594
060600            SIAPC-MODULE-NAME (SIAPC-MODULE-LEVEL)                00595
060700                '   ABORTING ... '                                00596
060800            DISPLAY '---- SIAPC CONTENTS ---'                     00597
060900            DISPLAY 'FUNCTION-CD-MAJOR = '                        00598
061000                     SIAPC-FUNCTION-CD-MAJOR                      00599
061100            DISPLAY 'FUNCTION-CD-SUB   = '                        00600
061200                     SIAPC-FUNCTION-CD-SUB                        00601
061300            DISPLAY 'RETURN-IND        = '                        00602
061400                     SIAPC-RETURN-IND                             00603
061500            DISPLAY 'RETURN-STATUS     = '                        00604
061600                     SIAPC-RETURN-STATUS                          00605
061700            DISPLAY 'ERROR-MESSAGE     = '                        00606
061800                     SIAPC-ERROR-MESSAGE                          00607
061900     END-IF                                                       00608
062000     .                                                            00609
062100                                                                  00610
062200 5510-EXIT.                                                       00611
062300     EXIT.                                                        00612
062400/                                                                 00613
062500 9000-END-OF-JOB.                                                 00614
062600                                                                  00615
062700     CLOSE IN-RECORD-TYPE-21-FILE                                 00616
062800           OUT-RECORD-TYPE-21-FILE                                00617
062900     DISPLAY WS-ASTERISKS                                         00618
063000     DISPLAY 'OLD 21 RECORDS WRITTEN = ' WS-OLD-21-RECS-WR-CNTR   00619
063100     DISPLAY 'OLD 21 RECORDS DELETED = ' WS-OLD-21-RECS-DL-CNTR   00620
063200     DISPLAY ' '                                                  00621
063300     DISPLAY 'NEW 21 RECORDS READ    = ' WS-NEW-21-RECS-RD-CNTR   00622
063400     DISPLAY 'NEW 21 RECORDS WRITTEN = ' WS-NEW-21-RECS-WR-CNTR   00623
063500     DISPLAY WS-ASTERISKS                                         00624
063600      IF CWSFC-CICS-INSTALLATION                                  00625
063700         MOVE CWSCC-CLOSE-FILES-REQUEST TO SIAPC-FUNCTION-CD-MAJOR00626
063800         MOVE SPACE                    TO SIAPC-FUNCTION-CD-SUB   00627
063900         PERFORM 2000-CALL-ITCPX162-MODULE                        00628
064000            THRU 2000-EXIT                                        00629
064100      END-IF.                                                     00630
064200                                                                  00631
064300 9000-EXIT.                                                       00632
064400      EXIT.                                                       00633
064500/                                                                 00634
064600 9100-RESET-SIAPC-RETURN-DATA.                                    00635
064700                                                                  00636
064800       MOVE SPACES                 TO   SIAPC-RETURN-DATA         00637
064900                                        SIAPC-RETURN-MESSAGE-CD.  00638
065000                                                                  00639
065100       MOVE CWSCC-SUCCESSFUL       TO   SIAPC-RETURN-IND.         00640
065200                                                                  00641
065300       MOVE CWSCC-ACTION-COMPLETE  TO   SIAPC-RETURN-STATUS       00642
065400                                        SIAPC-ABEND-CODE.         00643
065500 9100-EXIT.                                                       00644
065600      EXIT.                                                       00645
065700/                                                                 00646
065800     COPY ITCPDDT.                                                00647
