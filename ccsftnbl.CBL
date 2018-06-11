000100 IDENTIFICATION DIVISION.                                                 
000200*-----------------------*                                                 
000300 PROGRAM-ID.                                                              
000400     'CCSFTNBL'.                                                          
000500 AUTHOR.                                                                  
000600     BSCA.                                                                
000700 INSTALLATION.                                                            
000800     BSCA.                                                                
000900 DATE-WRITTEN.  MAY    1991.                                              
001000 DATE-COMPILED.                                                           
001100     TODAY.                                                               
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400*--------------------*                                                    
001500 CONFIGURATION SECTION.                                                   
001600 SOURCE-COMPUTER. IBM-370.                                                
001700 OBJECT-COMPUTER. IBM-370.                                                
001800     SKIP3                                                                
001900******************************************************************        
002000****                                                          ****        
002100****                    MODULE OVERVIEW OF                    ****        
002200****                                                          ****        
002300****                         CCSFTNBL                         ****        
002400****                                                          ****        
002500****                 (CSF TRANSACTION BUILD)                  ****        
002600****                                                          ****        
002700******************************************************************        
002800****                                                                      
002900****  PRODUCT PACKAGE:  CLAIMS                                            
003000****  SUBSYSTEM:        ITS                                               
003100****  VERSION:    3.9.7                                           CX397322
003200****                                                                      
003300****   PURPOSE:  MODULE CCSFTNBL RECEIVES SCSF RECORDS,                   
003400****             DISTINGUISHES BETWEEN INSTITUTIONAL AND                  
003500****             PROFESSIONAL CLAIMS, EDITS THE RECORD SET FOR            
003600****             COMPLETENESS, OBTAINS INFORMATION ON RESUBMITTED         
003700****             CLAIMS FROM THE CLAIMS HISTORY OR TIP DATA BASE,         
003800****             AND CALLS THE APPROPRIATE PROCESSING MODULE.             
003900****                                                                      
004000******************************************************************        
004100     EJECT                                                                
004200******************************************************************        
004300***                                                            ***        
004400***               INTERNAL MODULE SPECIFICATIONS               ***        
004500***                                                            ***        
004600******************************************************************        
004700***                                                                       
004800***  SUMMARY OF CHANGES:                                                  
004900***    DATE    V.L.M    DESCRIPTION                                       
005000***  ---------/-------/------------------------------------------         
005100***   11/05/92  3.9.3   CR 624    - INITIAL RELEASE.              CL393611
005101***                                 CROSS REFERENCE CL393611.     CL393611
005102***   02/03/93  3.9.4   DN37751   - COBOLII/VS COBOL COMPATIBILE. CL393611
005103***                                 (REPLACE MODULE)              CL393611
005104***                                 CROSS REFERENCE CL394079.     CL393611
005105***   05/01/93  3.9.4   CR 635    - ITS 7.0 UPGRADE               CL393611
005106***                                 CROSS REFERENCE CL394618.     CL393611
005107***   10/04/93  3.9.5   DN38020   - TYPE OF SUBMISSION.                   
005108***                                 CROSS REFERENCE CL395129.             
005109***   11/11/93  3.9.5   CR 652    - ITS 7.1 UPGRADE               CL395543
005110***                                 CROSS REFERENCE CL395543.     CL395543
005111***   09/10/93  3.9.5   CR 647    - ADD LNE ITM SEG CALL. IF LNE WAS      
005112***                                 SPLIT MOVE Q TO BRQ STAT CD FIELD.    
005113***                                 CROSS REFERENCE CL395545.             
005114***   11/05/93  3.9.5   CR 648    - RECOMPILE ONLY/CICOMWKA.              
005115***                                 CROSS REFERENCE CL395695.             
005116***   12/03/93  3.9.5   CR 651    - RECOMPILE ONLY/VSCPCLLC.      CL395724
005117***                                 CROSS REFERENCE CL395724.     CL395724
005118***   01/28/94  3.9.5   DN37983     RECOMPILE FOR COPY CICOMWKA    DN37983
005119***                                 CROSS REFERENCE CL395187.      DN37983
005220***   02/25/94  3.9.5   DN38102   - COMPILED WITH NEW CICOMWKA.   CL395743
005221***                                 CROSS REFERENCE CL395743.     CL395743
005200***                                                               CL393611
005230***   06/06/94  3.9.5   DN38154     CHANGE RESUBMIT CHECK CODE    CL395782
005240***                                 CROSS REFERENCE DN38154 .     CL395782
005250***   06/30/94  3.9.5   DN38121 - NEW BRQ CODES L & M ADDED .     CL395794
005260***                                                               CL395794
005270***   07/05/94  3.9.5   DN38117 - ITS AND CLAIMS HISTORY LINE     CL395801
005280***                               ITEM COUNT MISMATCH.            CL395801
005281***   09/23/94   3.9.5   CORRECT TRUNCATION OF VFCITS10 DB WHEN   CL395833
005282***                      MORE THAN 21 LINE ITEMS INSERTED         CL395833
005283***                                                  - DN38173    CL395833
005289***   03/15/96   3.9.7   CR670 - LOGIC FOR LINE ITEM LINK         CX397034
005290***                      X-REF CX397034                           CX397034
005291***   04/30/96   3.9.7   CR670.02 - CORRECT HISTORY MISMATCH      CX397302
005292***                      LINE COUNT TO ALLOW ADDITION OF LINES    CX397302
005293***                      ON AN ADJUSTMENT                         CX397302
005294***   07/10/06   3.9.7   CR670 - FIX PRE-LIL BUG FOR              CX397322
005295***                      RESUBMIT PROCESSING                      CX397322
005300***                                                                       
005300***                                                                       
005122***   04/10/94  3.9.5   EBCBS     - COMPRESS CL??? INTO A          EBCBS  
005123***                                 STANDARD 256 BYTE RECORD.      EBCBS  
005303***   05/24/95  3.9.5   EBCBS     - CCS 95011002 APPLY CODE FOR    EBCBS  
005304***                                 RETRIEVE SUB ADDRESS FROM      EBCBS  
005305***                                 SUB DATABASE IF MISSING ON     EBCBS  
005306***                                 THE SF.                        EBCBS  
005307***   08/24/95  3.9.5   EBCBS     - CR # 658  CCS # 95186013       EBCBS  
005308***                                 NEW CLAIM NUMBERING LOGIC      EBCBS  
005309***   11/07/95  3.9.5   EBCBS     - CR # 2629 ABEND WHEN SFSER     EBCBS  
005310***                                 IS DUPLICATE AND RESUB IND=1   EBCBS  
005309***   02/28/95  3.9.5   EBCBS     - CR # 121 APPEND SUB ADDR WHEN  EBCBS  
005310***                                 MISSING FROM SF.               EBCBS  
005311***   09/27/96  3.9.5   EBCBS     - CR # 6651 ITS TRLR BALANCING  EB6651  
005312***   10/25/96  3.9.5   EBCBS     - CR # 12459 NET LIAB BALANCING EB12459 
005313***   11/18/98  3.9.9   EBCBS     - CR # 32202 RE-COMP Y2K COMPL. EB12459 
007408***   01/03/2000 3.9.9   -IR # 187301 APPLY LOGIC FOR EMC CYCLE DT        
005314***   12/29/99   3.9.9   -CR # 28352 RE-COMP ADJ RECPDT.          EB12459 
005314***   10/31/00   3.9.9   -CR # 46528 DATAFLEX EXPANSION.          EB12459 
071410***   03/01/2001 4.1.0    CR # 56224 RECOMPILE FOR COBOL LE.      X410    
005318***   07/31/2001 4.1.0    CR # 61022 CORRECT SID LOGIC.           X410    
005319***   08/07/2001 4.1.0    CR # 58645 LOOK AT PURGED HISTORY       X410    
005320***                       FOR CLAIM IN ADJUSTMENT PROCESSING.     X410    
071412***   10/23/2001 4.1.0   CR # 61665 SET PCB FOR 2 NEW D/B                 
071413***               HEALTH CARE SERVICES AND DIAGNOSIS CODES.               
071414***   10/01/01   4.1.5   CR726 - ITS 8.4 - ADDED NEW RECORD TYPES C414004L
071415***                      FOR COB FUNCTIONALITY ---                C414004L
071416***                      ITCFM -- 32,D2: 66,F6                    C414004L
071417***   03/17/2002 4.1.5   CR66137 ITS 8.4 - FIX SOP TO OCL XWALK   01181016
071418***   05/10/2002 4.1.5   CR63269 HIPAA 99 LINE.                   01181016
C82615***   01/09/2004         CR82615 ALTERNATE ID                     01181016
C83330***   04/15/2004         CR83330 GROUP ID                         01181016
C97522***   11/04/2004         70 REC NOT REQUIRED IF TOB 4XX, 5XX, 14X         
113822***   09/25/2004         CR113822 - NPI                                   
113822***                                                                       
124519***   12/05/2006         CR124519 - ITS SET THE HMO TYPE                  
124519***                                                                       
141946***   11/09/2008      CR141946   --ITS10.2 ROLL UP CLAIMS                 
141946***                                XREF ITS102                            
177017***   01/03/2012      CR177017 OCALLOW MAPPING FOR ITS                    
177017***                            PROFESSIONAL MEDICARE                      
172756***   10/12/2012      CQ172756   --ITS HOME RELEASE 12.0 TO MAP           
172756***                                THE THE INCLUSIVE GROUPING             
172756***                                INDICATOR                              
257760***   11/09/2012      CQ172756   --ITS RELEASE 12.0 HOME DAY 2            
257760***                                ICD10 MAPPING                          
430147***   10/11/2013      CQ430147   --MEDICARE CROSSOVER ITS HOME            
430147***                                MODIFIER MAPPING CHANGES               
551903***   12/13/2013      CQ551903   --ITS HOME CHANGE FOR THE ICD            
551903***                                PROVIDER SUBMITTED VERSION             
551903***                                 FIELD                                 
114219***   01/09/2015      CQ1142191  --COPYBOOK RECOMPILE                     
189277***   12/11/2015      CQ1892773  --UNITED NATIONS ITS ID CROSSOVER        
005400******************************************************************        
005500     EJECT                                                                
005600******************************************************************        
005700***                                                                       
005800***  MODULE INTERFACES:                                                   
005900***                                                                       
006000***  ACTIVATION: CCSFTNBL IS INVOKED BY JCL.                              
006100***                                                                       
006200***  PARAMETERS RECEIVED:                                                 
006300***                                                                       
006400***      NONE                                                             
006500***                                                                       
006600******************************************************************        
006700***                                                                       
006800***  COMPONENTS REFERENCED:                                               
006900***                                                                       
007000***  APPLICATION MODULES CALLED:                                          
007100***                                                                       
007200***      CNSFIFMT - STANDARD FORMAT INSTITUTIONAL CLAIMS                  
007300***      CNSFILIN - STANDARD FORMAT INSTITUTIONAL LINE ITEMS              
007400***      CNSFPFMT - STANDARD FORMAT PROFESSIONAL CLAIMS                   
007500***      CNSFPLIN - STANDARD FORMAT PROFESSIONAL LINE ITEMS               
007501***      CNITSADR - SEN DIAG/ADDRESS TO SF MODULE                  EBCBS  
007600***                                                                       
007700***  COMMON MODULES CALLED:                                               
007800***                                                                       
007900***      LNCKDIG  - CHECK DIGIT CALCULATION                               
008000***      LNDLICBL - COBOL/DLI INTERFACE                                   
008100***      LNDLIINT - COBOL/DLI INTERFACE (INITIALIZATION)                  
008200***      LXKILL   - DISPLAY MODULE HIERARCHY                              
008300***      ZNCNCLDP - PROCESSING TERMINATION (WITH DUMP)                    
008400***                                                                       
008500***  COPY LIBRARY MEMBERS USED:                                           
008600***                                                                       
008700***      CICOMWKA - CCSF COMMON WORK AREA                                 
008800***      CIPRMTBL - CCSF PROMPT ID TABLE                                  
008900***      ITCFM1A5 - ITS PROFESSIONAL CLAIM HEADER RECORD                  
009000***      ITCFM1B0 - ITS PROFESSIONAL PROVIDER RECORD                      
009100***      ITCFM1B5 - ITS PROFESSIONAL APPENDED PROVIDER RECORD             
009200***      ITCFM1C0 - ITS PROFESSIONAL PATIENT RECORD                       
009300***      ITCFM1D0 - ITS PROFESSIONAL SUBSCRIBER/PRIMARY PAYER REC         
009400***      ITCFM1D1 - ITS PROFESSIONAL OTHER INSURED/OTHER CARRIER          
009401***      ITCFM1D2 - ITS PROFESSIONAL OPL CLAIM                    C414004L
009600***      ITCFM1E0 - ITS PROFESSIONAL CLAIM RECORD                         
009700***      ITCFM1E1 - ITS PROFESSIONAL CLAIM PAY                            
009800***      ITCFM1E6 - ITS PROFESSIONAL SPECIAL NOTATIONS RECORD             
009900***      ITCFM1F0 - ITS PROFESSIONAL LINE OF SERVICE RECORD               
010000***      ITCFM1F5 - ITS PROFESSIONAL APPENDED PRICING RECORD              
010001***      ITCFM1F6 - ITS PROFESSIONAL OPL LINE                     C414004L
010100***      ITCFM1G0 - ITS PROFESSIONAL REFERRING PHYSICIAN RECORD           
010200***      ITCFM1X0 - ITS PROFESSIONAL CLAIM TRAILER RECORD                 
010300***      ITCFM105 - ITS INSTITUTIONAL CLAIM HEADER RECORD                 
010400***      ITCFM110 - ITS INSTITUTIONAL PROVIDER RECORD                     
010500***      ITCFM115 - ITS INSTITUTIONAL APPENDED PROVIDER RECORD            
010600***      ITCFM120 - ITS INSTITUTIONAL PATIENT RECORD                      
010700***      ITCFM130 - ITS INSTITUTIONAL SUBSCRIBER RECORD                   
010800***      ITCFM131 - ITS INSTITUTIONAL ADDITIONAL SUBSCRIBER REC           
010800***      ITCFM132 - ITS INSTITUTIONAL OPL CLAIM                   C414004L
011000***      ITCFM140 - ITS INSTITUTIONAL CLAIM RECORD                        
011100***      ITCFM141 - ITS INSTITUTIONAL CLAIM PAY                           
011200***      ITCFM146 - ITS INSTITUTIONAL SPECIAL NOTATIONS RECORD            
011300***      ITCFM150 - ITS INSTITUTIONAL ACCOMODATIONS RECORD                
011500***      ITCFM160 - ITS INSTITUTIONAL ANCILLARY RECORD                    
011600***      ITCFM165 - ITS INSTITUTIONAL APPENDED ANCILLARY PRICING          
011601***      ITCFM166 - ITS INSTITUTIONAL OPL LINE                    C414004L
011700***      ITCFM170 - ITS INSTITUTIONAL MEDICAL RECORD                      
011800***      ITCFM180 - ITS INSTITUTIONAL PHYSICIAN DATA RECORD               
011900***      ITCFM190 - ITS INSTITUTIONAL CLAIM TRAILER RECORD                
011910***      ITCFM71A - ITS MCF HEADER RECORD                         CL393611
011920***      ITCFM72A - ITS MCF SUBSCRIBER RECORD                     CL393611
011930***      ITCFM73A - ITS MCF PATIENT RECORD                        CL393611
011940***      ITCFM74A - ITS MCF CASE SET-UP RECORD                    CL393611
011950***      ITCFM75A - ITS MCF CASE DESCRIPTION RECORD               CL393611
011960***      ITCFM76A - ITS MCF REFERRAL RECORD                       CL393611
011970***      ITCFM77A - ITS MCF AUTHORIZATION RECORD                  CL393611
011980***      ITCFM78A - ITS MCF LOCAL/VENDOR USE RECORD               CL393611
011990***      ITCFM79A - ITS MCF TRAILER RECORD                        CL393611
012000***      VSCPCLMC - CLAIMS HISTORY CLAIMS HEADER                          
012100***      VSCPHSTC - CLAIMS HISTORY ROOT                                   
012200***      VSCPMEMC - CLAIMS HISTORY MEMBER                                 
012300***      VSLPTAPC - TRANSACTION HISTORY                                   
012310***      VSCPITSC - ITS ROOT SEGMENT                              CL393611
012320***      VSCPISFC - ITS SF DETAIL                                 CL393611
014600***      VSBPCLSC - HEALTH CARE SERVIE D/B                                
014600***      VSBPDNSC - DIAGNOSIS D/B                                         
012400***                                                                       
012500***  TABLES REFERENCED:                                                   
012600***                                                                       
012700***      NONE                                                             
012800***                                                                       
012900***  DATA BASE SEGMENTS USED:                                             
013000***                                                                       
013100***      VSCPCLM - CLAIMS HISTORY CLAIMS HEADER                           
013200***      VSCPHST - CLAIMS HISTORY ROOT                                    
013300***      VSCPMEM - CLAIMS HISTORY MEMBER                                  
013400***      VSLPTAP - TRANSACTION HISTORY                                    
013410***      VSCPITS  - ITS ROOT SEGMENT                              CL393611
013420***      VSCPISF  - ITS SF DETAIL SEGMENT                         CL393611
013500***                                                                       
013600***  TRANSACTION INPUT:                                                   
013700***                                                                       
013800***      NONE                                                             
013900***                                                                       
014000***  DISPLAY OUTPUT:                                                      
014100***                                                                       
014200***      MODULE HIERARCHY ON SYSTEM ERROR VIA LXKILL                      
014300***                                                                       
014400***  REPORT OUTPUT:                                                       
014500***                                                                       
014600***      ITS CLAIM CONTROL REPORT                                         
014700***                                                                       
014800******************************************************************        
014900***                                                                       
015000***  ERROR HANDLING:                                                      
015100***                                                                       
015200***      INVALID TRANSACTION SETS ARE WRITTEN TO AN ERROR FILE.           
015300***                                                                       
015400***  SPECIAL ERRORS:                                                      
015500***                                                                       
015600***      THE FOLLOWING RETURN CODES ARE SET:                              
015700***           4 - INVALID TRANSACTION SETS ENCOUNTERED                    
015800***           8 - NO SCSF INPUT                                           
015900***                                                                       
016000******************************************************************        
016100***                                                                       
016200***  USER LIMITATIONS:                                                    
016300***                                                                       
016400***      NONE                                                             
016500******************************************************************        
016600     EJECT                                                                
016700 SPECIAL-NAMES.                                                           
016800      C01  IS TOP-OF-PAGE.                                                
016900                                                                          
017000 INPUT-OUTPUT SECTION.                                                    
017100*--------------------*                                                    
017200 FILE-CONTROL.                                                            
017300*------------*                                                            
017400                                                                          
017500     SELECT ERROR-FILE          ASSIGN TO UT-S-VWCERROR.                  
017600     SELECT SCSE-INPUT-FILE     ASSIGN TO UT-S-VWCSCSE.                   
017700     SELECT BATCH-KEYWORD-FILE  ASSIGN TO UT-S-VWCBATCH.                  
017800     SELECT CONTROL-REPORT      ASSIGN TO UT-S-PRINTER.                   
017801     SELECT CS90DT-CNTL-FILE-IN  ASSIGN TO UT-S-S9Z0DTIN.          EBCBS  
017802     SELECT CS90DT-CNTL-FILE-OUT ASSIGN TO UT-S-S9Z0DTOT.          EBCBS  
089277     SELECT CROSS-OVER-FILE      ASSIGN TO UT-S-VWCCROSS.          EBCBS  
017900     SKIP3                                                                
018000 DATA DIVISION.                                                           
018100*-------------*                                                           
018200 FILE SECTION.                                                            
018300*------------*                                                            
018400                                                                          
018500 FD  SCSE-INPUT-FILE                                                      
018600     RECORDING MODE IS F                                                  
018700     BLOCK CONTAINS 0 RECORDS                                             
018800     LABEL RECORDS ARE STANDARD.                                          
018900                                                                          
019000 01  SCSE-INPUT-REC.                                                      
019100     05  FILLER-SER-NUM                  PIC  X(017).                     
019300     05  FILLER-TRANS-ID                 PIC  X(002).             CL395543
019300     05  FILLER-TRANS-QUAL               PIC  X(002).             CL395543
019100     05  SCSE-RCD-TYPE                   PIC  X(002).                     
ITS101*    05  SCSE-SEQ-NUM                    PIC  X(002).                     
ITS101     05  SCSE-SEQ-NUM                    PIC S9(4) COMP.                  
019300     05  FILLER                          PIC  X(230).             CL395543
019400     SKIP3                                                                
019500 FD  BATCH-KEYWORD-FILE                                                   
019600     RECORDING MODE IS F                                                  
019700     BLOCK CONTAINS 0 RECORDS                                             
019800     LABEL RECORDS ARE STANDARD.                                          
019900                                                                          
020000 01  BATCH-KEYWORD-REC              PIC  X(256).                   EBCBS  
020100     SKIP3                                                                
020200 FD  ERROR-FILE                                                           
020300     RECORDING MODE IS F                                                  
020400     BLOCK CONTAINS 0 RECORDS                                             
020500     LABEL RECORDS ARE STANDARD.                                          
020600                                                                          
020700 01  ERROR-REC                          PIC  X(255).                      
020800                                                                          
020900 FD  CONTROL-REPORT                                                       
021000                    RECORDING MODE   IS F                                 
021100                    RECORD CONTAINS 133 CHARACTERS                        
021200                    LABEL RECORDS   ARE OMITTED                           
021300                    DATA RECORD      IS PRINT-LINE.                       
021400 01  PRINT-LINE.                                                          
021500     05  FILLER                     PIC X(01).                            
021600     05  PRINT-LINE-DATA            PIC X(132).                           
021601*                                                                  EBCBS  
021602*  DATE CONTROL FILE TO PREVENT DUPLICATE CLAIM NUMBERS            EBCBS  
021603*                                                                  EBCBS  
021604 FD  CS90DT-CNTL-FILE-IN                                           EBCBS  
021605     RECORDING MODE IS F                                           EBCBS  
021606     BLOCK CONTAINS 0 RECORDS                                      EBCBS  
021607     LABEL RECORDS ARE STANDARD.                                   EBCBS  
021608                                                                   EBCBS  
021609 01  CS90DT-CNTL-IN-REC.                                           EBCBS  
021610     05  CS90DT-CNTL-IN-JUL-DATE         PIC  9(005).              EBCBS  
021611     05  CS90DT-CNTL-IN-SEQ-NUM          PIC  9(006).              EBCBS  
021612     05  FILLER                          PIC  X(068).              EBCBS  
021613     05  CS90DT-CNTL-OVERRIDE            PIC  X(001).              EBCBS  
021613                                                                   EBCBS  
021614 FD  CS90DT-CNTL-FILE-OUT                                          EBCBS  
021615                    RECORDING MODE   IS F                          EBCBS  
021616                    RECORD CONTAINS 80 CHARACTERS                  EBCBS  
021617                    LABEL RECORDS   ARE OMITTED                    EBCBS  
021618                    DATA RECORD      IS CS90DT-CNTL-OUT-REC.       EBCBS  
021619                                                                   EBCBS  
021620 01  CS90DT-CNTL-OUT-REC.                                          EBCBS  
021621     05  CS90DT-CNTL-OUT-JUL-DATE        PIC  9(005).              EBCBS  
021622     05  CS90DT-CNTL-OUT-SEQ-NUM         PIC  9(006).              EBCBS  
021623     05  FILLER                          PIC  X(069).              EBCBS  
021700     SKIP3                                                                
189277 FD  CROSS-OVER-FILE                                               EBCBS  
189277     RECORDING MODE IS F                                           EBCBS  
189277     BLOCK CONTAINS 0 RECORDS                                      EBCBS  
189277     LABEL RECORDS ARE STANDARD.                                   EBCBS  
189277                                                                   EBCBS  
189277 01 UN-XWALK-RECORD.                                                      
189277    05 UN-NEW-SUBID                         PIC  X(09).                   
189277    05 UN-OLD-SUBID                         PIC  X(09).                   
189277    05 FILLER                               PIC  X(62).                   
                                                                                
021800     EJECT                                                                
021900 WORKING-STORAGE SECTION.                                                 
022000*-----------------------*                                                 
022100 77  BASEVLM        PIC X(20) VALUE 'VER.03 LEV.09 MOD.05'.       CL395743
022200 77  PRINT-LINE-CC                  PIC 99      VALUE 0.                  
022212 01  WP-SEGM-LGT-CD        PIC S9(9) COMP.                        CL395833
022213 01  FILLER  REDEFINES WP-SEGM-LGT-CD.                            CL395833
022214     04  FILLER            PIC  X(2).                             CL395833
022215     04  WX-SEGM-LGT-CD    PIC  X(2).                             CL395833
022300                                                                          
       01  WS-ITS-DT.                                                           
           10  WS-ITS-CC           PIC XX.                                      
           10  WS-ITS-YY           PIC XX.                                      
           10  WS-ITS-MM           PIC XX.                                      
           10  WS-ITS-DD           PIC XX.                                      
       01  WS-LRSP-DT.                                                          
           10  WS-LRSP-MM          PIC XX.                                      
           10  WS-LRSP-DD          PIC XX.                                      
           10  WS-LRSP-YY          PIC XX.                                      
022301 01   WS-JUL-DATE.                                                 EBCBS  
022302      05  WS-JUL-DATE-DECADE        PIC 9.                         EBCBS  
022303      05  WS-JUL-DATE-YEAR          PIC 9.                         EBCBS  
022304      05  WS-JUL-DATE-DA            PIC 9(003).                    EBCBS  
022310 01   WS-DATE.                                                    CL393611
022320      05  WS-DATE-MO                PIC 99.                       CL393611
022330      05  WS-DATE-DA                PIC 99.                       CL393611
022340      05  WS-DATE-YR                PIC 99.                       CL393611
022345 01   WS-TIME.                                                     EBCBS  
022346      05  WS-TIME-DAY               PIC 99.                        EBCBS  
022347      05  WS-TIME-HOUR              PIC 99.                        EBCBS  
022348      05  WS-TIME-MIN               PIC 99.                        EBCBS  
022349      05  WS-TIME-SEC               PIC 99.                        EBCBS  
022350 01   WS-PURG-DATE.                                               CL393611
022360      05  WS-PURG-MM                PIC 99.                       CL393611
022370      05  WS-PURG-DD                PIC 99.                       CL393611
022380      05  WS-PURG-YY                PIC 99.                       CL393611
022390 01   WS-PLAN-CD-X                  PIC X(005).                   CL393611
022391 01   WS-PLAN-CD-N REDEFINES WS-PLAN-CD-X  PIC 9(005).            CL393611
022400 01   INPUT-DATE.                                                         
022500      05  INPUT-YEAR                PIC 99.                               
022600      05  INPUT-MONTH               PIC 99.                               
022700      05  INPUT-DAY                 PIC 99.                               
022800 01   INPUT-TIME.                                                         
022900      05  INPUT-HOUR                PIC 99.                               
023000      05  INPUT-MINUTE              PIC 99.                               
023100      05  INPUT-SECOND              PIC 99.                               
023200      05  FILLER                    PIC XX.                               
023300 01   REFORMATTED-DATE.                                                   
023400      05  REF-MONTH                 PIC 99.                               
023500      05  FILLER                    PIC X   VALUE '/'.                    
023600      05  REF-DAY                   PIC 99.                               
023700      05  FILLER                    PIC X   VALUE '/'.                    
023800      05  REF-YEAR                  PIC 99.                               
023900                                                                          
023910 01  SUBSCRIPTS.                                                  CL393611
023920     05  BD-SUB            COMP   PIC S9(4) VALUE +0.             CL393611
023930     05  SF-SUB            COMP   PIC S9(4) VALUE +0.             CL393611
       01  X-PTR                               PIC 9(002) VALUE ZERO.           
       01  WS-START-POS                        PIC 9(003) VALUE ZERO.           
       01  WS-END-POS                          PIC 9(003) VALUE ZERO.           
       01  WS-STR-LEN                          PIC 9(003) VALUE ZERO.           
ITS113 01  IX                                  PIC 9(004) VALUE ZERO.           
177017 01  WS-OCL-CODE                         PIC X(02)  VALUE SPACES.         
ITS113                                                                  CL393611
ITS113 COPY  CTUFCTBL.                                                          
ITS113                                                                          
ITS113 01  TABLE-WORK.                                                          
ITS113     05  ED-TABLE                  PIC X(8).                              
ITS113     05  ENCODE-DECODE-FUNCTIONS.                                         
ITS113         10  B6                    PIC 9(4) COMP SYNC VALUE 6.            
ITS113     05  ENC-RET-CD                PIC S9(04) COMP.                       
ITS113     05  ED-ARGUE.                                                        
ITS113*******  10  ED-1-INPUT            PIC X(1).                              
ITS113         10  ED-1-DESC             .                                      
ITS113           15 ED-FL                PIC X(04).                             
ITS113           15 FILLER               PIC X(01).                             
ITS113           15 ED-FH                PIC X(04).                             
ITS113           15 FILLER               PIC X(01).                             
ITS113           15 ED-R1                PIC X(17).                             
ITS113           15 FILLER               PIC X(01).                             
ITS113           15 ED-R2                PIC X(17).                             
ITS113           15 FILLER               PIC X(01).                             
ITS113           15 ED-PROV              PIC X(06).                             
ITS113           15 FILLER               PIC X(01).                             
ITS113           15 ED-CK                PIC X(01).                             
ITS113           15 FILLER               PIC X(18).                             
ITS113     05  ED-IX-N                   PIC 9(4)   COMP.                       
ITS113                                                                          
023940                                                                  CL393611
       01  PROGRAM-SECTION-PARMLIST.                                    EB6651  
           05 W-4110-PARMLIST.                                          EB6651  
ITS102*       10  W-4110-SUB               PIC 9(02)     VALUE ZEROS.   EB6651  
ITS102*       10  W-4110-SUB-2             PIC 9(02)     VALUE ZEROS.   EB6651  
ITS102        10  W-4110-SUB               PIC 9(04)     VALUE ZEROS.   EB6651  
ITS102        10  W-4110-SUB-2             PIC 9(04)     VALUE ZEROS.   EB6651  
                                                                        EB6651  
           05 W-4120-PARMLIST.                                          EB6651  
ITS102*       10  W-4120-SUB               PIC 9(02)     VALUE ZEROS.   EB6651  
              10  W-4120-SUB               PIC 9(04)     VALUE ZEROS.   EB6651  
023940                                                                  CL393611
024000 01  ACCUMULATORS.                                                        
024100                                                                          
024200     05  SUB                  COMP-3 PIC S9(04)    VALUE ZERO.            
024300     05  SUB-COMP             COMP-3 PIC S9(04)    VALUE ZERO.            
024400     05  SUB-SUB-ID           COMP-3 PIC S9(04)    VALUE ZERO.            
024500     05  AC-INPUT-CNT         COMP-3 PIC S9(09)    VALUE ZERO.            
024501     05  AC-RECD-CNT          COMP-3 PIC S9(09)    VALUE ZERO.     EBCBS  
024600     05  AC-ADO-INS-CNT       COMP-3 PIC S9(09)    VALUE ZERO.            
024700     05  AC-ADO-PRO-CNT       COMP-3 PIC S9(09)    VALUE ZERO.            
024800     05  AC-CFS-INS-CNT       COMP-3 PIC S9(09)    VALUE ZERO.            
024900     05  AC-CFS-PRO-CNT       COMP-3 PIC S9(09)    VALUE ZERO.            
025000     05  AC-ERROR-CNT         COMP-3 PIC S9(09)    VALUE ZERO.            
025010     05  AC-ADJUST-CNT        COMP-3 PIC S9(09)    VALUE ZERO.    CL395543
025100     05  AC-OUTPUT-CNT        COMP-3 PIC S9(09)    VALUE ZERO.            
025110     05  AC-MCF-CNT           COMP-3 PIC S9(09)    VALUE ZERO.    CL393611
025200     05  AC-CFS-AMT           COMP-3 PIC S9(09)V99 VALUE ZERO.            
025300     05  AC-ADO-AMT           COMP-3 PIC S9(09)V99 VALUE ZERO.            
025400                                                                          
025500     05  VTG-VALID-GROUP-CNT  COMP-3 PIC 99        VALUE ZERO.            
025600     05  VTG-30               COMP-3 PIC 99        VALUE ZERO.            
025700     05  VTG-31               COMP-3 PIC 99        VALUE ZERO.            
025800     05  VTG-46               COMP-3 PIC 99        VALUE ZERO.            
025900     05  VTG-E6               COMP-3 PIC 99        VALUE ZERO.            
026000     05  VTG-D1               COMP-3 PIC 99        VALUE ZERO.            
026100     05  VTG-50               COMP-3 PIC 99        VALUE ZERO.            
026110     05  VTG-52               COMP-3 PIC 99        VALUE ZERO.    CL393611
026200     05  VTG-55               COMP-3 PIC 99        VALUE ZERO.            
ITS102*    05  VTG-60               COMP-3 PIC 99        VALUE ZERO.            
ITS102     05  VTG-60               COMP-3 PIC 9999      VALUE ZERO.            
ITS102*    05  VTG-61               COMP-3 PIC 99        VALUE ZERO.    CL393611
ITS102*    05  VTG-62               COMP-3 PIC 99        VALUE ZERO.    CL393611
ITS102     05  VTG-61               COMP-3 PIC 9999      VALUE ZERO.    CL393611
ITS102     05  VTG-62               COMP-3 PIC 9999      VALUE ZERO.    CL393611
ITS102*    05  VTG-65               COMP-3 PIC 99        VALUE ZERO.            
ITS102     05  VTG-65               COMP-3 PIC 9999      VALUE ZERO.            
ITS102*    05  VTG-66               COMP-3 PIC 99        VALUE ZERO.    C414004L
ITS102     05  VTG-66               COMP-3 PIC 9999      VALUE ZERO.    C414004L
026500     05  VTG-F0               COMP-3 PIC 99        VALUE ZERO.            
026600     05  VTG-F5               COMP-3 PIC 99        VALUE ZERO.            
026601     05  VTG-F6               COMP-3 PIC 99        VALUE ZERO.    C414004L
026700                                                                          
026800     05  VALIDATE-THE-GROUP.                                              
026900         10  VTG-50-GROUPS            OCCURS 5 TIMES.                     
027000             15  VTG-50-55-GROUP.                                         
027100                 20 VTG-50-INDICATOR PIC X.                               
027200                 20 VTG-55-INDICATOR PIC X.                               
ITS102*        10  VTG-60-GROUPS            OCCURS 23 TIMES.                    
ITS102         10  VTG-60-GROUPS            OCCURS 999 TIMES.                   
027400             15  VTG-60-65-GROUP.                                         
027500                 20 VTG-60-INDICATOR PIC X.                               
027600                 20 VTG-65-INDICATOR PIC X.                               
ITS102*        10  VTG-F0-GROUPS            OCCURS 23 TIMES.                    
ITS102         10  VTG-F0-GROUPS            OCCURS  50 TIMES.                   
027800             15  VTG-F0-F5-GROUP.                                         
027900                 20 VTG-F0-INDICATOR PIC X.                               
028000                 20 VTG-F5-INDICATOR PIC X.                               
028100                                                                          
028110 01  BUILD-SF-AREA                PIC X(16065).                   CL393611
028120 01  BUILD-SF-AREA-R  REDEFINES  BUILD-SF-AREA.                   CL393611
028130     05  SF-RECORDS   OCCURS  63  TIMES.                          CL393611
028140         10  ONE-SF-RECORD        PIC X(255).                     CL393611
028150                                                                  CL393611
189277 01  WS-CROSS-RECORD.                                                     
189277     05  WS-CROSS-ID OCCURS 12000 TIMES                                   
189277                     INDEXED BY SUB-UN-ID.                                
189277         10  WS-CROSS-NEW         PIC X(09).                              
189277         10  WS-CROSS-OLD         PIC X(09).                              
189277         10  FILLER               PIC X(62).                              
028200 01  WORK-AREA.                                                           
028300     05  WS-CHECK-LENGHT   COMP   PIC S9(4) VALUE +12.                    
028400     05  WS-TIP-USER-KEY.                                                 
028410         10  FILLER               PIC X(4).                       CX397322
028500         10  WS-TIP-SUB-ID-1-12   PIC X(12).                              
028600         10  WS-TIP-SUB-MODF      PIC X(3).                               
028700         10  FILLER               PIC X(2).                               
028800     05  WS-SUB-MODIFIER          PIC X     VALUE ZERO.                   
028900     05  WS-SUBSRIBER-ID.                                                 
029000         10  WS-SUB-ID-1-3        PIC X(3).                               
029100         10  WS-SUB-ID-4-15.                                              
029200             15  WS-SUB-ID-4-15-OCCUR   PIC X(1)                          
029300                                 OCCURS 12 TIMES.                         
029400         10  WS-SUB-ID-16-17      PIC X(2).                               
029500     05  WS-RESUBMISSION-IND      PIC X     VALUE SPACES.                 
029600     05  WS-RECORD-LEVEL-NO       PIC 99    VALUE ZEROES.                 
029700     05  WS-DEPENDENT-NO          PIC 99    VALUE ZEROES.                 
029710     05  WS-SFSER                 PIC X(17) VALUE SPACES.         CL393611
029800     05  WS-SUB-ID-4-15-COMP.                                             
029900         10  WS-SUB-ID-COMP   PIC X(1) OCCURS 12 TIMES.           CL394079
030000                                                                          
030100     05  WS-CONVERT-SEQ-NBR.                                              
030200         10  WS-CONV-PLAN-NBR     PIC 9(3).                        EBCBS  
030200         10  WS-CONV-CENT-NBR     PIC 9(2).                        EBCBS  
030300         10  WS-CONV-YEAR-X.                                              
030400             15  WS-CONV-YEAR     PIC 9(2).                               
030500             15  FILLER           REDEFINES   WS-CONV-YEAR.               
030600                 20 WS-CONV-YEAR-1 PIC 9.                                 
030700                 20 WS-CONV-YEAR-2 PIC 9.                                 
030800         10  WS-CONV-DAYS-X.                                              
030900             15  WS-CONV-DAYS     PIC 9(3).                               
031000         10  WS-CONV-NBR          PIC 9(5).                        EBCBS  
031300         10  FILLER.                                                      
031400             15  FILLER           PIC X(1).                               
031500             15  WS-CONV-NBR-17   PIC X(1).                               
031600                                                                          
031700     05  WS-ALPHA-CLAIM-NBR.                                              
031800         10  WS-CLAIM-YEAR        PIC X(1).                        EBCBS  
031900         10  WS-CLAIM-DATE        PIC 9(3).                        EBCBS  
032000         10  WS-CLAIM-SEQ-NBR     PIC 9(6)       VALUE 830000.     EBCBS  
032100         10  WS-CLAIM-END-NBR     PIC X(1).                               
032200     05  WS-NUMERIC-CLAIM-NBR REDEFINES WS-ALPHA-CLAIM-NBR                
032300                                  PIC 9(11).                              
032400                                                                          
032500     05  WS-CLAIM-ADJUST-COMP     PIC 9(4) COMP.                          
032600     05  WS-CLAIM-ADJUST-X REDEFINES  WS-CLAIM-ADJUST-COMP.               
032700         10  WS-CLAIM-ADJUST      PIC XX.                                 
032800                                                                          
032900     05  WS-NUMERIC-ADJUST-CD     PIC 9(4).                               
033000                                                                          
033100     05  WS-CLAIM-STATUS-COMP     PIC 9(4) COMP.                          
033200     05  WS-CLAIM-STATUS-X REDEFINES  WS-CLAIM-STATUS-COMP.               
033300         10  FILLER               PIC X.                                  
033400         10  WS-CLAIM-STATUS      PIC X.                                  
033500                                                                          
033600     05  WS-NUMERIC-STATUS-CD     PIC 9(4).                               
033610     05  WS-LINE-ITEM-COUNT       PIC 9(4) COMP.                  CL395801
033620     05  WS-LINE-ITEMS-X   REDEFINES  WS-LINE-ITEM-COUNT.         CL395801
033630         10  FILLER               PIC X.                          CL395801
033640         10  WS-NBR-OF-LINE-ITEMS PIC X.                          CL395801
033700                                                                          
033800     03  WS-HOLD-TIME-GRP.                                                
033900         05  WS-H-TIME-HH           PIC  X(02).                           
034000         05  WS-H-TIME-MM           PIC  X(02).                           
034100         05  WS-H-TIME-SS           PIC  X(02).                           
034200                                                                          
034300     05  WS-PLAN-CONV-LITERAL     PIC X(08) VALUE 'CTUPLNNO'.             
034400     05  WS-CLCSF                 PIC X(10) VALUE '^CLCSF    '.           
034500     05  WS-CLRLI                 PIC X(10) VALUE '^CLRLI    '.           
034600     05  WS-CLADO                 PIC X(10) VALUE '^CLADO    '.           
034610     05  WS-LICT                  PIC X(10) VALUE '*LICT    ='.   CL395801
034620     05  WS-BRQ                   PIC X(10) VALUE '*BRQ     ='.   CL395801
034700                                                                          
034800     05  WS-OPT                   PIC X(10) VALUE '*OPT     ='.           
034900     05  WS-DN                    PIC X(10) VALUE '*DN      ='.           
035000     05  WS-RKEY                  PIC X(10) VALUE '*RKEY    ='.           
           05  WS-DISDT                 PIC X(10) VALUE '*DISDT   ='.           
035100                                                                          
035200     05  WS-CFS                   PIC X(10) VALUE 'CSF       '.           
035300     05  WS-RLI                   PIC X(10) VALUE 'RLI       '.           
035400     05  WS-ADO                   PIC X(10) VALUE 'ADO       '.           
035410     05  TXN-LINE-CNT             PIC X(02) VALUE ZEROES.         CL395801
035500                                                                          
035600     SKIP3                                                                
035700 01  COMMON-MODULE-PARMS.                                                 
035800                                                                          
035900     05  LKDLIERR-PARMS.                                                  
036000         10  DLIERR-RET-CD    COMP   PIC S9(04).                          
036100                                                                          
036200     05  LXKILL-PARMS.                                                    
036300         10  KILL-MODULE-NM          PIC  X(08)  VALUE 'CCSFTNBL'.        
036400         10  KILL-MODULE-POSITION                                         
036500                              COMP   PIC S9(04).                          
036600                                                                          
036700*                                                                         
036800*****************************************************************         
036900*     R E P O R T   H E A D I N G   A R E A                     *         
037000*****************************************************************         
037100                                                             SKIP1        
037200 01  HEADING-LINES.                                                       
037300     05  HDR1.                                                            
037400         10  HDR1-CC              PIC X(01) VALUE ' '.                    
037500         10  FILLER               PIC X(14) VALUE 'REPORT DATE: '.        
037600         10  HDR1-REPORT-DATE     PIC X(08) VALUE SPACES.                 
037700         10  FILLER               PIC X(25) VALUE SPACES.                 
037800         10  FILLER               PIC X(39) VALUE                         
037900             'BUSINESS SYSTEMS CORPORATION OF AMERICA'.                   
038000         10  FILLER               PIC X(24) VALUE SPACES.                 
038100         10  FILLER               PIC X(09) VALUE 'PAGE NO: '.            
038200         10  HDR1-PAGE-NO         PIC 9.                                  
038300     05  HDR2.                                                            
038400         10  HDR2-CC              PIC X(01) VALUE ' '.                    
038500         10  FILLER               PIC X(14) VALUE 'REPORT TIME: '.        
038600         10  HDR2-REPORT-TIME-HH  PIC X(02) VALUE SPACES.                 
038700         10  FILLER               PIC X(01) VALUE ':'.                    
038800         10  HDR2-REPORT-TIME-MM  PIC X(02) VALUE SPACES.                 
038900         10  FILLER               PIC X(01) VALUE ':'.                    
039000         10  HDR2-REPORT-TIME-SS  PIC X(02) VALUE SPACES.                 
039100         10  FILLER               PIC X(20) VALUE SPACES.                 
039200         10  FILLER               PIC X(39) VALUE                         
039300             '      ITS CLAIM CONTROL REPORT         '.                   
039400         10  FILLER               PIC X(29) VALUE SPACES.                 
039500         10  FILLER               PIC X(09) VALUE 'PROGRAM: '.            
039600         10  HDR2-PROGRAM-NAME    PIC X(08) VALUE 'CCSFTNBL'.             
039700        EJECT                                                             
039800     05 ERROR-LINE.                                                       
039900        10 FILLER                   PIC X(01)   VALUE ' '.                
040000        10 FILLER                   PIC X(02)   VALUE ' '.                
040100        10 FILLER                   PIC X(50)   VALUE                     
040200        'SCSE FILE EMPTY OR 1ST RECORD NOT AN (05) OR (A5)'.              
040300     05 DETAIL-LINE-1.                                                    
040400        10 FILLER                   PIC X(01)   VALUE ' '.                
040500        10 FILLER                   PIC X(02)   VALUE ' '.                
040600        10 FILLER                   PIC X(50)   VALUE                     
040700        '1. TOTAL OF ALL TRANSACTION SETS READ FROM THE ITS'.             
040800        10 FILLER                   PIC X(17)   VALUE             CL393611
040900        ' SYSTEM'.                                                        
041000        10  DL1-CNT                 PIC ZZZ,ZZZ,ZZZ.                      
041010     05 DETAIL-LINE-2.                                            CL393611
041020        10 FILLER                   PIC X(01)   VALUE ' '.        CL393611
041030        10 FILLER                   PIC X(02)   VALUE ' '.        CL393611
041040        10 FILLER                   PIC X(50)   VALUE             CL393611
041050        '2. TOTAL OF ALL MCF RECORD SETS READ FROM THE ITS '.     CL393611
041060        10 FILLER                   PIC X(17)   VALUE             CL393611
041070        'SYSTEM     '.                                            CL393611
041080        10  DL2-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
041100     05 DETAIL-LINE-3.                                            CL393611
041200        10 FILLER                   PIC X(01)   VALUE ' '.                
041300        10 FILLER                   PIC X(02)   VALUE ' '.                
041400        10 FILLER                   PIC X(50)   VALUE                     
041500        '3. TOTAL OF ALL TRANSACTION SETS SENT TO INSTITUTI'.     CL393611
041600        10 FILLER                   PIC X(17)   VALUE             CL393611
041700        'ONAL CLCSF '.                                                    
041800        10  DL3-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
041900     05 DETAIL-LINE-4.                                            CL393611
042000        10 FILLER                   PIC X(01)   VALUE ' '.                
042100        10 FILLER                   PIC X(02)   VALUE ' '.                
042200        10 FILLER                   PIC X(50)   VALUE                     
042300        '4. TOTAL OF ALL TRANSACTION SETS SENT TO PROFESSIO'.     CL393611
042400        10 FILLER                   PIC X(17)   VALUE             CL393611
042500        'NAL CLCSF  '.                                                    
042600        10  DL4-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
042700     05 DETAIL-LINE-5.                                            CL393611
042800        10 FILLER                   PIC X(01)   VALUE ' '.                
042900        10 FILLER                   PIC X(02)   VALUE ' '.                
043000        10 FILLER                   PIC X(50)   VALUE                     
043100        '5. TOTAL OF ALL TRANSACTION SETS SENT TO INSTITUTI'.     CL393611
043200        10 FILLER                   PIC X(17)   VALUE             CL393611
043300        'ONAL CLADO / RLI '.                                      CL393611
043400        10  DL5-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
043500     05 DETAIL-LINE-6.                                            CL393611
043600        10 FILLER                   PIC X(01)   VALUE ' '.                
043700        10 FILLER                   PIC X(02)   VALUE ' '.                
043800        10 FILLER                   PIC X(50)   VALUE                     
043900        '6. TOTAL OF ALL TRANSACTION SETS SENT TO PROFESSIO'.     CL393611
044000        10 FILLER                   PIC X(17)   VALUE             CL393611
044100        'NAL CLADO / RLI '.                                       CL393611
044200        10  DL6-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
044300     05 DETAIL-LINE-7.                                            CL393611
044400        10 FILLER                   PIC X(01)   VALUE ' '.                
044500        10 FILLER                   PIC X(02)   VALUE ' '.                
044600        10 FILLER                   PIC X(50)   VALUE                     
044700        '7. TOTAL OF ALL TRANSACTION SETS SENT TO THE ERROR'.     CL393611
044800        10 FILLER                   PIC X(17)   VALUE             CL393611
044900        ' FILE  '.                                                        
045000        10  DL7-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
045010     05 DETAIL-LINE-7A.                                           CL395543
045020        10 FILLER                   PIC X(01)   VALUE ' '.        CL395543
045030        10 FILLER                   PIC X(02)   VALUE ' '.        CL395543
045040        10 FILLER                   PIC X(50)   VALUE             CL395543
045050        '8. TOTAL OF ALL TRANSACTION SETS SENT TO THE RECYC'.     CL395543
045060        10 FILLER                   PIC X(17)   VALUE             CL393611
045070        'LE DATABASE'.                                            CL395543
045080        10  DL7A-CNT                PIC ZZZ,ZZZ,ZZZ.              CL395543
045100     05 DETAIL-LINE-8.                                            CL393611
045200        10 FILLER                   PIC X(01)   VALUE ' '.                
045300        10 FILLER                   PIC X(02)   VALUE ' '.                
045400        10 FILLER                   PIC X(50)   VALUE                     
045500        '9. TOTAL OF ALL BATCH KEYWORD GROUPS WRITTEN TO TH'.     CL395543
045600        10 FILLER                   PIC X(17)   VALUE             CL393611
045700        'E OUTPUT '.                                                      
045800        10  DL8-CNT                 PIC ZZZ,ZZZ,ZZZ.              CL393611
045900     05 DETAIL-LINE-9.                                            CL393611
046000        10 FILLER                   PIC X(01)   VALUE ' '.                
046100        10 FILLER                   PIC X(02)   VALUE ' '.                
046200        10 FILLER                   PIC X(50)   VALUE                     
046300        '10. TOTAL OF ALL CFS TRANSACTION CHARGES          '.     CL395543
046400        10 FILLER                   PIC X(17)   VALUE             CL393611
046500        '       '.                                                        
046600        10  DL9-AMT                 PIC ZZZ,ZZZ,ZZZ.99.           CL393611
046700     05 DETAIL-LINE-10.                                           CL393611
046800        10 FILLER                   PIC X(01)   VALUE ' '.                
046900        10 FILLER                   PIC X(02)   VALUE ' '.                
047000        10 FILLER                   PIC X(50)   VALUE                     
047100        '11. TOTAL OF ALL ADO / RLI TRANSACTION CHARGES    '.     CL395543
047200        10 FILLER                   PIC X(17)   VALUE             CL393611
047300        '       '.                                                        
047400        10  DL10-AMT                PIC ZZZ,ZZZ,ZZZ.99.           CL393611
047500     EJECT                                                                
047600 01  DLI-CALL-FIELDS.                                                     
047700                                                                          
047800     05  DLI-FUNCTION-CODES.                                              
047900         10  GU-CALL                 PIC  X(04)  VALUE 'GU  '.            
048000         10  GNP-CALL                PIC  X(04)  VALUE 'GNP '.            
048010         10  ISRT-CALL               PIC  X(04)  VALUE 'ISRT'.    CL393611
048020         10  REPL-CALL               PIC  X(04)  VALUE 'REPL'.    CL393611
048030         10  DLET-CALL               PIC  X(04)  VALUE 'DLET'.    CL393611
048040         10  GHNP-CALL               PIC  X(04)  VALUE 'GHNP'.    CL393611
048050         10  GHU-CALL                PIC  X(04)  VALUE 'GHU '.    CL393611
048060         10  GHN-CALL                PIC  X(04)  VALUE 'GHN '.    CL393611
048070         10  GN-CALL                 PIC  X(04)  VALUE 'GN  '.    CL393611
048071         10  CHKP-FUNCTION           PIC  X(04)  VALUE 'CHKP'.     EBCBS  
048100     SKIP3                                                                
048101*                                                                         
048102     05  IO-AREALEN                  PIC  X(1)   VALUE SPACE.             
048103*                                                                         
048200     05  PCB-NAMES.                                                       
048300         10  PCB-NM-ES01             PIC  X(04)  VALUE 'ES01'.            
048400         10  PCB-NM-ES02             PIC  X(04)  VALUE 'ES02'.            
048500         10  PCB-NM-ES03             PIC  X(04)  VALUE 'ES03'.            
048600         10  PCB-NM-ES04             PIC  X(04)  VALUE 'ES04'.            
048700         10  PCB-NM-ES05             PIC  X(04)  VALUE 'ES05'.            
048800         10  PCB-NM-ES06             PIC  X(04)  VALUE 'ES06'.            
048900         10  PCB-NM-ES07             PIC  X(04)  VALUE 'ES07'.            
049000         10  PCB-NM-ES08             PIC  X(04)  VALUE 'ES08'.            
049100         10  PCB-NM-ES09             PIC  X(04)  VALUE 'ES09'.            
049200         10  PCB-NM-ES10             PIC  X(04)  VALUE 'ES10'.            
049201         10  PCB-NM-ZS01             PIC  X(04)  VALUE 'ZS01'.            
049202         10  PCB-NM-ZS02             PIC  X(04)  VALUE 'ZS02'.            
049203         10  PCB-NM-ZS03             PIC  X(04)  VALUE 'ZS03'.            
049204         10  PCB-NM-ZS04             PIC  X(04)  VALUE 'ZS04'.            
049205         10  PCB-NM-ZS05             PIC  X(04)  VALUE 'ZS05'.            
049206         10  PCB-NM-ZS06             PIC  X(04)  VALUE 'ZS06'.            
049207         10  PCB-NM-ZS07             PIC  X(04)  VALUE 'ZS07'.            
049208         10  PCB-NM-ZS08             PIC  X(04)  VALUE 'ZS08'.            
049209         10  PCB-NM-ZS09             PIC  X(04)  VALUE 'ZS09'.            
049210         10  PCB-NM-ZS10             PIC  X(04)  VALUE 'ZS10'.            
049300         10  PCB-NM-ES11             PIC  X(04)  VALUE 'ES11'.            
049400         10  PCB-NM-COMMON           PIC  X(04)  VALUE '    '.            
049401         10  PCB-NM-ZCOMMON          PIC  X(04)  VALUE '    '.            
049410         10  PCB-NM-ITS              PIC  X(04)  VALUE 'ITS1'.    CL393611
049420         10  PCB-NM-ITR              PIC  X(04)  VALUE 'ITR1'.    CL395543
049421         10  PCB-NM-SUB              PIC  X(04)  VALUE 'SUB1'.     EBCBS  
089301         10  PCB-NM-HCSD             PIC  X(04)  VALUE 'HCSD'.     EBCBS  
089301         10  PCB-NM-DIAG             PIC  X(04)  VALUE 'DIAG'.     EBCBS  
C82615         10  PCB-NM-BK01             PIC  X(04)  VALUE 'BK01'.     EBCBS  
C82615         10  PCB-NM-BK02             PIC  X(04)  VALUE 'BK02'.     EBCBS  
C82615         10  PCB-NM-BK03             PIC  X(04)  VALUE 'BK03'.     EBCBS  
C82615         10  PCB-NM-BK04             PIC  X(04)  VALUE 'BK04'.     EBCBS  
C82615         10  PCB-NM-BK05             PIC  X(04)  VALUE 'BK05'.     EBCBS  
C82615         10  PCB-NM-BK06             PIC  X(04)  VALUE 'BK06'.     EBCBS  
C82615         10  PCB-NM-BK07             PIC  X(04)  VALUE 'BK07'.     EBCBS  
C82615         10  PCB-NM-BK08             PIC  X(04)  VALUE 'BK08'.     EBCBS  
C82615         10  PCB-NM-BK09             PIC  X(04)  VALUE 'BK09'.     EBCBS  
C82615         10  PCB-NM-BK10             PIC  X(04)  VALUE 'BK10'.     EBCBS  
C82615         10  PCB-NM-AGP              PIC  X(04)  VALUE 'AIGS'.     EBCBS  
C82615         10  PCB-NM-AXF              PIC  X(04)  VALUE 'AIXF'.     EBCBS  
C82615         10  PCB-NM-AXI              PIC  X(04)  VALUE 'AIDK'.     EBCBS  
C83330         10  PCB-NM-GRP              PIC  X(04)  VALUE 'GRPR'.     EBCBS  
124519         10  PCB-NM-PRV              PIC  X(04)  VALUE 'PRVO'.     EBCBS  
049500     SKIP3                                                                
049600     05  PCB-AREA.                                                        
049700         10  PCB-DBD-NM              PIC  X(08).                          
049800         10  PCB-SEGM-LEV            PIC  X(02).                          
049900         10  PCB-DLI-RET-CD          PIC  X(02).                          
050000         10  PCB-DLI-PROC-OPT        PIC  X(04).                          
050100         10  PCB-RESERVE-DLI         PIC  X(04).                          
050200         10  PCB-SEGM-NM-FB          PIC  X(08).                          
050300         10  PCB-LEN-FB-KEY          PIC  X(04).                          
050400         10  PCB-NO-SENS-SEG         PIC  X(04).                          
050500         10  PCB-KEY-FB-AREA.                                             
050600             15  PCB-GRP-ID-KEY      PIC  X(26).                          
050700                                                                          
050710     05  PCB-AREA-2.                                              CL395545
050712         10  PCB-DBD-NM2            PIC X(08).                    CL395545
050714         10  PCB-SEGM-LEV2          PIC X(02).                    CL395545
050716         10  PCB-DLI-CLM-LN-RET-CD  PIC X(02).                    CL395545
050718         10  PCB-DLI-PROC-OPT2      PIC X(04).                    CL395545
050720         10  PCB-RESERVE-DLI2       PIC X(04).                    CL395545
050722         10  PCB-SEGM-NM-FB2        PIC X(08).                    CL395545
050724         10  PCB-LEN-FB-KEY2        PIC X(04).                    CL395545
050726         10  PCB-NO-SENS-SEG2       PIC X(04).                    CL395545
050728         10  PCB-KEY-FB-AREA.                                     CL395545
050730             15 PCB-GRP-ID-KEY2     PIC X(36).                    CL395545
050800     EJECT                                                                
050900 01  SSAS.                                                                
051000                                                                          
051100     05  SSA-HIST-ROOT.                                                   
051200         10  SSA-HIST-SEGM-NM        PIC  X(08)  VALUE 'VSCPHST '.        
051300         10  SSA-HIST-CMD            PIC  X(01)  VALUE '*'.               
051400         10  SSA-HIST-CMD-CD         PIC  X(01)  VALUE 'D'.               
051500         10  SSA-HIST-L-PAREN        PIC  X(01)  VALUE '('.               
051600         10  SSA-HIST-KEY-NM         PIC  X(08)  VALUE 'SUBRIDD1'.        
051700         10  SSA-HIST-RO             PIC  X(02)  VALUE ' ='.              
051800         10  SSA-HIST-KEY-ID.                                             
051900             15  SSA-HIST-SUBR-NO    PIC  X(12).                          
052000             15  SSA-HIST-SUBR-MDF   PIC  X(03).                          
052100         10  SSA-HIST-R-PAREN        PIC  X(01)  VALUE ')'.               
052200                                                                          
052300     05  SSA-HIST-MEMBER.                                                 
052400         10  SSA-MBR-SEGM-NM         PIC  X(08)  VALUE 'VSCPMEM '.        
052500         10  SSA-MBR-CMD             PIC  X(01)  VALUE '*'.               
052600         10  SSA-MBR-CMD             PIC  X(01)  VALUE 'D'.               
052700         10  SSA-MBR-L-PAREN         PIC  X(01)  VALUE ' '.               
052800                                                                          
052900     05  SSA-HIST-CLAIM.                                                  
053000         10  SSA-CLM-SEGM-NM         PIC  X(08)  VALUE 'VSCPCLM '.        
053100         10  SSA-CLM-L-PAREN         PIC  X(01)  VALUE '('.               
053200         10  SSA-CLM-KEY-NM          PIC  X(08)  VALUE 'NULLCLMI'.        
053300         10  SSA-CLM-RO              PIC  X(02)  VALUE ' ='.              
053400         10  SSA-CLM-CLAIM-NO COMP-3 PIC  9(11).                          
053500         10  SSA-CLM-R-PAREN         PIC  X(01)  VALUE ')'.               
053600                                                                          
053610     05  SSA-HIST-CLAIM-LINE-UNQ.                                 CL395545
053620         10  SSA-CLM-LINE-SEQM-NM   PIC X(08) VALUE 'VSCPCLL '.   CL395545
053630         10  SSA-CLM-LINE-UNQ       PIC X(01) VALUE ' '.          CL395545
053640                                                                  CL395545
053700     05  SSA-TIP-ROOT.                                                    
053800         10  SSA-TIP-SEGM-NM         PIC  X(08)  VALUE 'VSLPTAP '.        
053900         10  SSA-TIP-L-PAREN         PIC  X(01)  VALUE '('.               
054000         10  SSA-TIP-KEY-NM1         PIC  X(08)  VALUE 'TRNSAPLK'.        
054100         10  SSA-TIP-RO1             PIC  X(02)  VALUE ' ='.              
054200         10  SSA-TIP-KEY-ID1.                                             
054300             15  SSA-TIP-FILLER      PIC  X(05).                          
054400             15  SSA-TIP-CLAIM-NBR   PIC  X(11).                          
054500         10  SSA-TIP-R-PAREN         PIC  X(01)  VALUE ')'.               
054600                                                                          
054611     05  SSA-ITS-ROOT.                                            CL393611
054612         10  SSA-ITS-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL393611
054613         10  SSA-ITS-L-PAREN        PIC  X(01)  VALUE '('.        CL393611
054614         10  SSA-ITS-KEY-NM         PIC  X(08)  VALUE 'SCCFSRLN'. CL393611
054615         10  SSA-ITS-RO             PIC  X(02)  VALUE ' ='.       CL393611
054616         10  SSA-ITS-SER-NO         PIC  X(17).                   CL393611
054617         10  SSA-ITS-R-PAREN        PIC  X(01)  VALUE ')'.        CL393611
054618                                                                  CL393611
054619     05  SSA-ISRT-ITS-ROOT.                                       CL393611
054620         10  SSA-ITS-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL393611
054621         10  SSA-ITS-CMD            PIC  X(01)  VALUE ' '.        CL393611
054622                                                                  CL393611
054623     05  SSA-SET-ITS-ROOT.                                        CL393611
054624         10  SSA-ITS-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL393611
054625         10  SSA-ITS-L-PAREN        PIC  X(01)  VALUE '('.        CL393611
054626         10  SSA-ITS-KEY-NM         PIC  X(08)  VALUE 'SCCFSRLN'. CL393611
054627         10  SSA-ITS-RO             PIC  X(02)  VALUE ' ='.       CL393611
054628         10  SSA-ISRT-ITS-SER-NO    PIC  X(17).                   CL393611
054629         10  SSA-ITS-R-PAREN        PIC  X(01)  VALUE ')'.        CL393611
054630                                                                  CL393611
054631     05  SSA-ISRT-SF-DATA.                                        CL393611
054632         10  SSA-SF-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.   CL393611
054633         10  SSA-SF-CMD           PIC  X(01)  VALUE ' '.          CL393611
054634                                                                  CL393611
054635     05  SSA-REPL-SF-DATA.                                        CL393611
054636         10  SSA-SF-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.   CL393611
054637         10  SSA-SF-CMD           PIC  X(01)  VALUE ' '.          CL393611
054638                                                                  CL393611
054639     05  SSA-SF-DATA.                                             CL393611
054640         10  SSA-SF-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.   CL393611
054641         10  SSA-SF-CMD           PIC  X(01)  VALUE ' '.          CL393611
054649                                                                  CL395543
054650     05  SSA-ITR-ROOT.                                            CL395543
054651         10  SSA-ITR-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL395543
054652         10  SSA-ITR-L-PAREN        PIC  X(01)  VALUE '('.        CL395543
054653         10  SSA-ITR-KEY-NM         PIC  X(08)  VALUE 'SCCFSRLN'. CL395543
054654         10  SSA-ITR-RO             PIC  X(02)  VALUE ' ='.       CL395543
054655         10  SSA-ITR-SER-NO         PIC  X(17).                   CL395543
054656         10  SSA-ITR-R-PAREN        PIC  X(01)  VALUE ')'.        CL395543
054657                                                                  CL395543
054658     05  SSA-ISRT-ITR-ROOT.                                       CL395543
054659         10  SSA-ITR-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL395543
054660         10  SSA-ITR-CMD            PIC  X(01)  VALUE ' '.        CL395543
054661                                                                  CL395543
054662     05  SSA-SET-ITR-ROOT.                                        CL395543
054663         10  SSA-ITR-SEGM-NM        PIC  X(08)  VALUE 'VSCPITS '. CL395543
054664         10  SSA-ITR-L-PAREN        PIC  X(01)  VALUE '('.        CL395543
054665         10  SSA-ITR-KEY-NM         PIC  X(08)  VALUE 'SCCFSRLN'. CL395543
054666         10  SSA-ITR-RO             PIC  X(02)  VALUE ' ='.       CL395543
054667         10  SSA-ISRT-ITR-SER-NO    PIC  X(17).                   CL395543
054668         10  SSA-ITR-R-PAREN        PIC  X(01)  VALUE ')'.        CL395543
054669                                                                  CL395543
054670     05  SSA-ISRT-ITR-SF-DATA.                                    CL395543
054671         10  SSA-SFR-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.  CL395543
054672         10  SSA-SFR-CMD           PIC  X(01)  VALUE ' '.         CL395543
054673                                                                  CL395543
054674     05  SSA-REPL-ITR-SF-DATA.                                    CL395543
054675         10  SSA-SFR-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.  CL395543
054676         10  SSA-SFR-CMD           PIC  X(01)  VALUE ' '.         CL395543
054677                                                                  CL395543
054678     05  SSA-ITR-SF-DATA.                                         CL395543
054679         10  SSA-SFR-SEGM-NM       PIC  X(08)  VALUE 'VSCPISF '.  CL395543
054680         10  SSA-SFR-CMD           PIC  X(01)  VALUE ' '.         CL395543
054700     EJECT                                                                
054800*    SEGMENT I/O AREAS                                                    
054900                                                                          
055000 01  VS-CLAIM-HIST-IO.                                                    
055100     02  VS-HIST-ROOT-IO.         COPY  VSCPHSTC.                         
055300     02  VS-HIST-MBR-IO.          COPY  VSCPMEMC.                         
055400     02  HIST-VSCPCLM.            COPY  VSCPCLMC.                         
055500*                                                                 CL395545
055501 01  HIST-CNEXPAND-IND.                                           11883003
055502     02  HIST-CLM-IND                    PIC X(001) VALUE SPACE.  11884003
055503     02  HIST-CLL-IND                    PIC X(001) VALUE SPACE.  11885003
055504*                                                                 11887003
055505 01  HIST-C399CLM.                                                11888003
055506     COPY C399CLMC.                                               11889003
055507 01  HIST-C398CLM.                                                11888003
055508     COPY C398CLMC.                                               11889003
055509 01  HIST-C395CLM.                                                11889103
055510     COPY C395CLMC.                                               11889203
055511 01  HIST-C391CLM.                                                11889303
055512     COPY C391CLMC.                                               11889403
164353 01  HIST-C405CLM.                                                11889303
164353     COPY C405CLMC.                                               11889403
055513*                                                                 11889503
055514 01  HIST-VSCPCLL.                                                CL395815
055515     COPY VSCPCLLC.                                               CL395815
055516 01  HIST-C399CLL.                                                11914003
055517     COPY C399CLLC.                                               11915003
055518 01  HIST-C398CLL.                                                11914003
055519     COPY C398CLLC.                                               11915003
055520 01  HIST-C395CLL.                                                11916003
055521     COPY C395CLLC.                                               11917003
055522 01  HIST-C391CLL.                                                11918003
055523     COPY C391CLLC.                                               11919003
102935 01  WS-HEALTH-CARE.                                              11918003
102936     COPY VSBPCLSC.                                               11919003
102935 01  WS-DIAG.                                                     11918003
102936     COPY VSBPDNSC.                                               11919003
055524*                                                                 11919103
055600     EJECT                                                                
055700 01  VS-TIP-IO.                   COPY  VSLPTAPC.                         
055800     EJECT                                                                
055810 01  VS-ITS-ROOT-SEGMENT-AREA.    COPY  VSCPITSC.                 CL393611
055820     EJECT                                                        CL393611
055830 01  VS-ITS-SF-DATA-SEGMENT.      COPY  VSCPISFC.                 CL393611
055840     EJECT                                                        CL393611
055850 01  VS-ITR-ROOT-SEGMENT-AREA.    COPY  VSCPITSW.                 CL395543
055855     EJECT                                                        CL395543
055860 01  VS-ITR-SF-DATA-SEGMENT.      COPY  VSCPISFW.                 CL395543
055861 01  ITR-DATA-R REDEFINES  VS-ITR-SF-DATA-SEGMENT.                CL395543
055862     05  ITR-RECORDS   OCCURS  63  TIMES INDEXED BY IX2.          CL395543
055863         10  ITR-DATA-TYPE         PIC X(002).                    CL395543
055864         10  ITR-DATA-SEQ          PIC X(002).                    CL395543
055865         10  ONE-ITR-DATA          PIC X(188).                    CL395543
055900                                                                          
056000     EJECT                                                        CL395543
056100 01  SWITCHES.                                                            
056200                                                                          
056300     05  WS-LINE-SPLT-NEEDED         PIC  X(01)        VALUE 'N'.         
056400       88  W88-LINE-SPLT-NEEDED                        VALUE 'Y'.         
056300     05  END-OF-FILE-SWITCH          PIC  X(01)        VALUE 'N'.         
056400       88  END-OF-FILE                                 VALUE 'Y'.         
189277     05  END-OF-CROSS-SWITCH         PIC  X(01)        VALUE 'N'.         
189277       88  END-OF-CROSS                                VALUE 'Y'.         
056500     05  GROUP-SWITCH                PIC  X(01)        VALUE 'N'.         
056600       88  GROUP-COMPLETED                             VALUE 'Y'.         
056700     05  NO-MORE-RECORD-SWITCH       PIC  X(01)        VALUE 'N'.         
056800       88  NO-MORE-TABLE-RECORDS                       VALUE 'Y'.         
056810     05  ADJUST-SWITCH               PIC  X(01)        VALUE 'N'. CL395543
056820       88  ADJUST-SWITCH-ON                            VALUE 'Y'. CL395543
056900     05  ERROR-SWITCH                PIC  X(01)        VALUE 'N'.         
057000       88  ERROR-SWITCH-ON                             VALUE 'Y'.         
057010     05  ITS-ROOT-SWITCH             PIC  X(01)        VALUE 'N'. CL393611
057020       88  ITS-ROOT-FOUND                              VALUE 'N'. CL393611
057030       88  ITS-ROOT-NOT-FOUND                          VALUE 'Y'. CL393611
057040     05  ITS-SF-DATA-SWITCH          PIC  X(01)        VALUE 'N'. CL393611
057050       88  ITS-SF-DATA-FOUND                           VALUE 'N'. CL393611
057060       88  ITS-SF-DATA-NOT-FOUND                       VALUE 'Y'. CL393611
057061     05  ITR-ROOT-SWITCH             PIC  X(01)        VALUE 'N'. CL395543
057062       88  ITR-ROOT-FOUND                              VALUE 'N'. CL395543
057063       88  ITR-ROOT-NOT-FOUND                          VALUE 'Y'. CL395543
057064     05  ITR-SF-DATA-SWITCH          PIC  X(01)        VALUE 'N'. CL395543
057065       88  ITR-SF-DATA-FOUND                           VALUE 'N'. CL395543
057066       88  ITR-SF-DATA-NOT-FOUND                       VALUE 'Y'. CL395543
057070     05  ITS-INST-PROF-SWITCH        PIC  X(01)        VALUE ' '. CL393611
057080       88  INST-CLAIM                                  VALUE 'I'. CL393611
057090       88  PROF-CLAIM                                  VALUE 'P'. CL393611
C97522     05  FM170-SWITCH                PIC  X(01)        VALUE ' '.         
551903     05  REC-70-SWITCH               PIC  X(01)        VALUE 'N'. CL393611
551903       88  REC-70-PRESENT                              VALUE 'Y'. CL393611
551903       88  REC-70-NOT-PRESENT                          VALUE 'N'. CL393611
057100                                                                          
057200     EJECT                                                                
065412 01  CCFMDATA.  COPY CCFMDATA.                                    C414004L
065400     EJECT                                                        CL393611
065412 01  MANAGED-CARE-RECORD-SET.                                     CL393611
065413         COPY ITCFM71A.                                           CL393611
065414     EJECT                                                        CL393611
065415         COPY ITCFM72A.                                           CL393611
065416     EJECT                                                        CL393611
065417         COPY ITCFM73A.                                           CL393611
065418     EJECT                                                        CL393611
065419         COPY ITCFM74A.                                           CL393611
065420     EJECT                                                        CL393611
065421         COPY ITCFM75A.                                           CL393611
065422     EJECT                                                        CL393611
065423         COPY ITCFM76A.                                           CL393611
065424     EJECT                                                        CL393611
065425         COPY ITCFM77A.                                           CL393611
065426     EJECT                                                        CL393611
065427         COPY ITCFM78A.                                           CL393611
065428     EJECT                                                        CL393611
065429         COPY ITCFM79A.                                           CL393611
065500     EJECT                                                                
065600     COPY CICOMWKA.                                                       
065700     EJECT                                                                
065800     COPY CIPRMTBL.                                                       
065801*--------------------------------------------------------------*   EBCBS  
065802*     EMPIRE BC/BC ADDED WORKING STORAGE SECTION               *   EBCBS  
065803*--------------------------------------------------------------*   EBCBS  
065804 01  ITS-CL-TRAN-PTR                     PIC 9(004)  VALUE ZEROS.  EBCBS  
065805 01  ITS-CL-TRAN-COUNT                   PIC 9(004)  VALUE ZEROS.  EBCBS  
065806 01  ITS-CL-TRAN-TABLE.                                            EBCBS  
065807     05  ITS-CL-TABLE-BRKDWN              OCCURS 1000 TIMES.       EBCBS  
065808        10  ITS-CL-TABLE-REC             PIC X(256).               EBCBS  
065809*                                                                  EBCBS  
065810 01  ITS-CL-WORK-PTR                     PIC 9(003)  VALUE ZEROS.  EBCBS  
065811 01  ITS-CL-WORK-AREA.                                             EBCBS  
065812     05  ITS-CL-WORK-BRKDWN               OCCURS 256 TIMES.        EBCBS  
065813        10  ITS-CL-WORK-BYTE             PIC X(001).               EBCBS  
065814*                                                                  EBCBS  
ITS102 01  WS-LINEITEM-KW-TABLE.                                         EBCBS  
065819         10  WS-LINEITEM-KW-1.                                     EBCBS  
ITS102             20  WS-KW-CHG               PIC X(007).               EBCBS  
ITS102             20  WS-KW-CHG-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-HSC               PIC X(007).               EBCBS  
ITS102             20  WS-KW-HSC-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-POS               PIC X(007).               EBCBS  
ITS102             20  WS-KW-POS-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-SSDT              PIC X(008).               EBCBS  
ITS102             20  WS-KW-SSDT-LEN          PIC 999.                  EBCBS  
IST102             20  WS-KW-ESDT              PIC X(008).               EBCBS  
IST102             20  WS-KW-ESDT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-SMC               PIC X(007).               EBCBS  
ITS102             20  WS-KW-SMC-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-UNIT              PIC X(007).               EBCBS  
ITS102             20  WS-KW-UNIT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-PLO               PIC X(007).               EBCBS  
ITS102             20  WS-KW-PLO-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-DIAG              PIC X(008).               EBCBS  
ITS102             20  WS-KW-DIAG-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-PI                PIC X(006).               EBCBS  
ITS102             20  WS-KW-PI-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-KW-OCAL              PIC X(008).               EBCBS  
ITS102             20  WS-KW-OCAL-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-NTE               PIC X(007).               EBCBS  
ITS102             20  WS-KW-NTE-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-KW-RSLT              PIC X(008).               EBCBS  
ITS102             20  WS-KW-RSLT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-PRVID             PIC X(009).               EBCBS  
ITS102             20  WS-KW-PRVID-LEN         PIC 999.                  EBCBS  
ITS102             20  WS-KW-PART              PIC X(008).               EBCBS  
ITS102             20  WS-KW-PART-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-PRLOC             PIC X(009).               EBCBS  
ITS102             20  WS-KW-PRLOC-LEN         PIC 999.                  EBCBS  
065852         10  WS-LINEITEM-KW-2.                                     EBCBS  
065853*            20  WS-KW-RMTH              PIC X(008).               EBCBS  
065554*            20  CLCSF-RMTH              PIC X(003).               EBCBS  
065855*            20  WS-KW-RMTV              PIC X(008).               EBCBS  
065856*            20  CLCSF-RMTV              PIC X(007).               EBCBS  
065857*            20  WS-KW-RI                PIC X(006).               EBCBS  
065858*            20  CLCSF-RI                PIC X(003).               EBCBS  
ITS102             20  WS-KW-INIT              PIC X(008).               EBCBS  
ITS102             20  WS-KW-INIT-LEN          PIC 999.                  EBCBS  
065861*            20  WS-KW-NTC               PIC X(007).               EBCBS  
065862*            20  CLCSF-NTC               PIC X(003).               EBCBS  
065863*            20  WS-KW-IS                PIC X(006).               EBCBS  
065864*            20  CLCSF-IS                PIC X(001).               EBCBS  
ITS102             20  WS-KW-PM                PIC X(006).               EBCBS  
ITS102             20  WS-KW-PM-LEN            PIC 999   .               EBCBS  
ITS102             20  WS-KW-RL                PIC X(006).               EBCBS  
ITS102             20  WS-KW-RL-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-KW-SE                PIC X(006).               EBCBS  
ITS102             20  WS-KW-SE-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-KW-SPLT              PIC X(008).               EBCBS  
ITS102             20  WS-KW-SPLT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-ORIG              PIC X(008).               EBCBS  
ITS102             20  WS-KW-ORIG-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-KW-OCPLN             PIC X(009).              10250012
ITS102             20  WS-KW-OCPLN-LEN         PIC 999.                 10250012
ITS102             20  WS-KW-OCCV              PIC X(008).              10270012
ITS102             20  WS-KW-OCCV-LEN          PIC 999.                 10270012
ITS102             20  WS-KW-NDC               PIC X(008).              10270012
ITS102             20  WS-KW-NDC-LEN           PIC 999.                 10270012
ITS102             20  WS-KW-837CHG            PIC X(010).              10270012
ITS102             20  WS-KW-837CHG-LEN        PIC 999.                 10270012
ITS102             20  WS-KW-LNCEX             PIC X(009).              10270012
ITS102             20  WS-KW-LNCEX-LEN         PIC 999.                 10270012
ITS102             20  WS-KW-LNNT              PIC X(008).              10270012
ITS102             20  WS-KW-LNNT-LEN          PIC 999.                 10270012
ITS102             20  WS-KW-LLNB              PIC X(008).              10270012
ITS102             20  WS-KW-LLNB-LEN          PIC 999.                 10270012
ITS102             20  WS-KW-CPT4              PIC X(008).              10270012
ITS102             20  WS-KW-CPT4-LEN          PIC 999.                 10270012
ITS102             20  WS-KW-LNPI              PIC X(008).              10270012
ITS102             20  WS-KW-LNPI-LEN          PIC 999.                 10270012
175203             20  WS-KW-M2                PIC X(006).                      
175203             20  WS-KW-M2-LEN            PIC 999.                         
172756             20  WS-KW-IG                PIC X(006).                      
172756             20  WS-KW-IG-LEN            PIC 999.                         
257760             20  WS-KW-LDX               PIC X(009).                      
257760             20  WS-KW-LDX-LEN           PIC 999.                         
430147             20  WS-KW-M3                PIC X(006).                      
430147             20  WS-KW-M3-LEN            PIC 999.                         
430147             20  WS-KW-M4                PIC X(006).                      
430147             20  WS-KW-M4-LEN            PIC 999.                         
065873*                                                                  EBCBS  
ITS102 01  WS-LINEITEM-DT-TABLE.                                         EBCBS  
065819         10  WS-LINEITEM-DT-1.                                     EBCBS  
ITS102             20  WS-DT-CHG               PIC X(008).               EBCBS  
ITS102             20  WS-DT-CHG-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-HSC               PIC X(008).               EBCBS  
ITS102             20  WS-DT-HSC-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-POS               PIC X(001).               EBCBS  
ITS102             20  WS-DT-POS-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-SSDT              PIC X(006).               EBCBS  
ITS102             20  WS-DT-SSDT-LEN          PIC 999.                  EBCBS  
IST102             20  WS-DT-ESDT              PIC X(006).               EBCBS  
IST102             20  WS-DT-ESDT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-SMC               PIC X(003).               EBCBS  
ITS102             20  WS-DT-SMC-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-UNIT              PIC X(003).               EBCBS  
ITS102             20  WS-DT-UNIT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-PLO               PIC X(002).               EBCBS  
ITS102             20  WS-DT-PLO-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-DIAG              PIC X(005).               EBCBS  
ITS102             20  WS-DT-DIAG-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-PI                PIC X(002).               EBCBS  
ITS102             20  WS-DT-PI-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-DT-OCAL              PIC X(008).               EBCBS  
ITS102             20  WS-DT-OCAL-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-NTE               PIC X(002).               EBCBS  
ITS102             20  WS-DT-NTE-LEN           PIC 999.                  EBCBS  
ITS102             20  WS-DT-RSLT              PIC X(004).               EBCBS  
ITS102             20  WS-DT-RSLT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-PRVID             PIC X(010).               EBCBS  
ITS102             20  WS-DT-PRVID-LEN         PIC 999.                  EBCBS  
ITS102             20  WS-DT-PART              PIC X(001).               EBCBS  
ITS102             20  WS-DT-PART-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-PRLOC             PIC X(003).               EBCBS  
ITS102             20  WS-DT-PRLOC-LEN         PIC 999.                  EBCBS  
065852         10  WS-LINEITEM-DT-2.                                     EBCBS  
065853*            20  WS-DT-RMTH              PIC X(008).               EBCBS  
065554*            20  CLCSF-RMTH              PIC X(003).               EBCBS  
065855*            20  WS-DT-RMTV              PIC X(008).               EBCBS  
065856*            20  CLCSF-RMTV              PIC X(007).               EBCBS  
065857*            20  WS-DT-RI                PIC X(006).               EBCBS  
065858*            20  CLCSF-RI                PIC X(003).               EBCBS  
ITS102             20  WS-DT-INIT              PIC X(008).               EBCBS  
ITS102             20  WS-DT-INIT-LEN          PIC 999.                  EBCBS  
065861*            20  WS-DT-NTC               PIC X(007).               EBCBS  
065862*            20  CLCSF-NTC               PIC X(003).               EBCBS  
065863*            20  WS-DT-IS                PIC X(006).               EBCBS  
065864*            20  CLCSF-IS                PIC X(001).               EBCBS  
ITS102             20  WS-DT-PM                PIC X(002).               EBCBS  
ITS102             20  WS-DT-PM-LEN            PIC 999   .               EBCBS  
ITS102             20  WS-DT-RL                PIC X(005).               EBCBS  
ITS102             20  WS-DT-RL-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-DT-SE                PIC X(001).               EBCBS  
ITS102             20  WS-DT-SE-LEN            PIC 999.                  EBCBS  
ITS102             20  WS-DT-SPLT              PIC X(002).               EBCBS  
ITS102             20  WS-DT-SPLT-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-ORIG              PIC X(002).               EBCBS  
ITS102             20  WS-DT-ORIG-LEN          PIC 999.                  EBCBS  
ITS102             20  WS-DT-OCPLN             PIC X(008).              10250012
ITS102             20  WS-DT-OCPLN-LEN         PIC 999.                 10250012
ITS102             20  WS-DT-OCCV              PIC X(001).              10270012
ITS102             20  WS-DT-OCCV-LEN          PIC 999.                 10270012
ITS102             20  WS-DT-NDC               PIC X(011).              10270012
ITS102             20  WS-DT-NDC-LEN           PIC 999.                 10270012
ITS102             20  WS-DT-837CHG            PIC X(008).              10270012
ITS102             20  WS-DT-837CHG-LEN        PIC 999.                 10270012
ITS102             20  WS-DT-LNCEX             PIC X(001).              10270012
ITS102             20  WS-DT-LNCEX-LEN         PIC 999.                 10270012
ITS102             20  WS-DT-LNNT              PIC X(001).              10270012
ITS102             20  WS-DT-LNNT-LEN          PIC 999.                 10270012
ITS102             20  WS-DT-LLNB              PIC X(003).              10270012
ITS102             20  WS-DT-LLNB-LEN          PIC 999.                 10270012
ITS102             20  WS-DT-CPT4              PIC X(008).              10270012
ITS102             20  WS-DT-CPT4-LEN          PIC 999.                 10270012
ITS102             20  WS-DT-LNPI              PIC X(010).              10270012
ITS102             20  WS-DT-LNPI-LEN          PIC 999.                 10270012
065873*                                                                  EBCBS  
ITS102*01  CLCSF-LINEITEM-PTR                  PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  CLCSF-LINEITEM-PTR                  PIC 9(004)  VALUE ZEROS.  EBCBS  
ITS102*01  CLCSF-LINEITEM-COUNT                PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  CLCSF-LINEITEM-COUNT                PIC 9(004)  VALUE ZEROS.  EBCBS  
065817 01  CLCSF-LINEITEM-TABLE.                                         EBCBS  
ITS102*    05  CLCSF-LINEITEM-TABLE-BRKDWN      OCCURS 30 TIMES.         EBCBS  
ITS102     05  CLCSF-LINEITEM-TABLE-BRKDWN      OCCURS 999 TIMES.        EBCBS  
065819         10  CLCSF-LINEITEM-BRKDWN-1.                              EBCBS  
065820             20  CLCSF-KW-CHG            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-CHG            PIC X(008).               EBCBS  
ITS102*            20  CLCSF-CHG               PIC X(008).               EBCBS  
065821             20  CLCSF-CHG               PIC X(10).                EBCBS  
065822             20  CLCSF-KW-HSC            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-HSC            PIC X(008).               EBCBS  
065823             20  CLCSF-HSC               PIC X(008).               EBCBS  
065824             20  CLCSF-KW-POS            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-POS            PIC X(008).               EBCBS  
065825             20  CLCSF-POS               PIC X(001).               EBCBS  
065826             20  CLCSF-KW-SSDT           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-SSDT           PIC X(009).               EBCBS  
065827             20  CLCSF-SSDT              PIC X(006).               EBCBS  
065828             20  CLCSF-KW-ESDT           PIC X(008).               EBCBS  
IST102*            20  CLCSF-KW-ESDT           PIC X(009).               EBCBS  
065829             20  CLCSF-ESDT              PIC X(006).               EBCBS  
065830             20  CLCSF-KW-SMC            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-SMC            PIC X(008).               EBCBS  
065831             20  CLCSF-SMC               PIC X(003).               EBCBS  
065832             20  CLCSF-KW-UNIT           PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-UNIT           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-UNIT              PIC X(003).               EBCBS  
ITS102             20  CLCSF-UNIT              PIC X(004).               EBCBS  
065834             20  CLCSF-KW-PLO            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-PLO            PIC X(008).               EBCBS  
065835             20  CLCSF-PLO               PIC X(002).               EBCBS  
065836             20  CLCSF-KW-DIAG           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-DIAG           PIC X(009).               EBCBS  
065837             20  CLCSF-DIAG              PIC X(005).               EBCBS  
065838             20  CLCSF-KW-PI             PIC X(006).               EBCBS  
ITS102*            20  CLCSF-KW-PI             PIC X(007).               EBCBS  
065839             20  CLCSF-PI                PIC X(002).               EBCBS  
065840             20  CLCSF-KW-OCAL           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-OCAL           PIC X(009).               EBCBS  
065841             20  CLCSF-OCAL              PIC X(10).                EBCBS  
065842             20  CLCSF-KW-NTE            PIC X(007).               EBCBS  
ITS102*            20  CLCSF-KW-NTE            PIC X(008).               EBCBS  
065843             20  CLCSF-NTE               PIC X(002).               EBCBS  
065844             20  CLCSF-KW-RSLT           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-RSLT           PIC X(009).               EBCBS  
065845             20  CLCSF-RSLT              PIC X(004).               EBCBS  
065846             20  CLCSF-KW-PRVID          PIC X(009).               EBCBS  
ITS102*            20  CLCSF-KW-PRVID          PIC X(010).               EBCBS  
065847             20  CLCSF-PRVID             PIC X(010).               EBCBS  
065848             20  CLCSF-KW-PART           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-PART           PIC X(009).               EBCBS  
065849             20  CLCSF-PART              PIC X(001).               EBCBS  
065850             20  CLCSF-KW-PRLOC          PIC X(009).               EBCBS  
ITS102*            20  CLCSF-KW-PRLOC          PIC X(010).               EBCBS  
065851             20  CLCSF-PRLOC             PIC X(003).               EBCBS  
065819         10  CSF-LINE1-ARRAY REDEFINES CLCSF-LINEITEM-BRKDWN-1.    EBCBS  
065813            20  CSF-LINE1-BYTE             PIC X(198).             EBCBS  
065852         10  CLCSF-LINEITEM-BRKDWN-2.                              EBCBS  
065853*            20  CLCSF-KW-RMTH           PIC X(008).               EBCBS  
065554*            20  CLCSF-RMTH              PIC X(003).               EBCBS  
065855*            20  CLCSF-KW-RMTV           PIC X(008).               EBCBS  
065856*            20  CLCSF-RMTV              PIC X(007).               EBCBS  
065857*            20  CLCSF-KW-RI             PIC X(006).               EBCBS  
065858*            20  CLCSF-RI                PIC X(003).               EBCBS  
065859             20  CLCSF-KW-INIT           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-INIT           PIC X(009).               EBCBS  
ITS102*            20  CLCSF-INIT              PIC X(008).               EBCBS  
ITS102             20  CLCSF-INIT              PIC X(10).                EBCBS  
065861*            20  CLCSF-KW-NTC            PIC X(007).               EBCBS  
065862*            20  CLCSF-NTC               PIC X(003).               EBCBS  
065863*            20  CLCSF-KW-IS             PIC X(006).               EBCBS  
065864*            20  CLCSF-IS                PIC X(001).               EBCBS  
065865             20  CLCSF-KW-PM             PIC X(006).               EBCBS  
ITS102*            20  CLCSF-KW-PM             PIC X(007).               EBCBS  
065866             20  CLCSF-PM                PIC X(002).               EBCBS  
065867             20  CLCSF-KW-RL             PIC X(006).               EBCBS  
ITS102*            20  CLCSF-KW-RL             PIC X(007).               EBCBS  
065868             20  CLCSF-RL                PIC X(005).               EBCBS  
065869             20  CLCSF-KW-SE             PIC X(006).               EBCBS  
ITS102*            20  CLCSF-KW-SE             PIC X(007).               EBCBS  
065870             20  CLCSF-SE                PIC X(001).               EBCBS  
065871             20  CLCSF-KW-SPLT           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-SPLT           PIC X(009).               EBCBS  
065872             20  CLCSF-SPLT              PIC X(002).               EBCBS  
065873             20  CLCSF-KW-ORIG           PIC X(008).               EBCBS  
ITS102*            20  CLCSF-KW-ORIG           PIC X(009).               EBCBS  
ITS102*            20  CLCSF-ORIG              PIC X(002).               EBCBS  
ITS102             20  CLCSF-ORIG              PIC X(3).                 EBCBS  
                   20  CLCSF-KW-OCPLN          PIC X(009).              10250012
ITS102*            20  CLCSF-KW-OCPLN          PIC X(010).              10250012
ITS102*            20  CLCSF-OCPLN             PIC X(008).              10260012
ITS102             20  CLCSF-OCPLN             PIC ZZZZZZZZZ.           10260012
                   20  CLCSF-KW-OCCV           PIC X(008).              10270012
ITS102*            20  CLCSF-KW-OCCV           PIC X(009).              10270012
                   20  CLCSF-OCCV              PIC X(001).              10280012
                   20  CLCSF-KW-NDC            PIC X(008).              10270012
ITS102*            20  CLCSF-KW-NDC            PIC X(009).              10270012
                   20  CLCSF-NDC               PIC X(011).              10280012
                   20  CLCSF-KW-837CHG         PIC X(010).              10270012
ITS102*            20  CLCSF-KW-837CHG         PIC X(011).              10270012
ITS102*            20  CLCSF-837CHG            PIC X(008).              10280012
ITS102             20  CLCSF-837CHG            PIC ZZZZZZ999.           10280012
                   20  CLCSF-KW-LNCEX          PIC X(009).              10270012
ITS102*            20  CLCSF-KW-LNCEX          PIC X(010).              10270012
                   20  CLCSF-LNCEX             PIC X(001).              10280012
                   20  CLCSF-KW-LNNT           PIC X(008).              10270012
ITS102*            20  CLCSF-KW-LNNT           PIC X(009).              10270012
                   20  CLCSF-LNNT              PIC X(001).              10280012
                   20  CLCSF-KW-LLNB           PIC X(008).              10270012
ITS102*            20  CLCSF-KW-LLNB           PIC X(009).              10270012
                   20  CLCSF-LLNB              PIC X(003).              10280012
                   20  CLCSF-KW-CPT4           PIC X(008).              10270012
ITS102*            20  CLCSF-KW-CPT4           PIC X(009).              10270012
                   20  CLCSF-CPT4              PIC X(008).              10280012
113822             20  CLCSF-KW-LNPI           PIC X(008).              10270012
ITS102*            20  CLCSF-KW-LNPI           PIC X(009).              10270012
113822             20  CLCSF-LNPI              PIC X(010).              10280012
065819         10  CSF-LINE2-ARRAY REDEFINES CLCSF-LINEITEM-BRKDWN-2.    EBCBS  
065813            20  CSF-LINE2-BYTE             PIC X(194).             EBCBS  
065873*                                                                  EBCBS  
ITS102*01  LRSP-CL-WORK-CNT                    PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  LRSP-CL-WORK-CNT                    PIC 9(004)  VALUE ZEROS.  EBCBS  
ITS102*01  LRSP-CL-WORK-PTR                    PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  LRSP-CL-WORK-PTR                    PIC 9(004)  VALUE ZEROS.  EBCBS  
ITS102*01  LRSP-CL-WORK-STR-LOC                PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  LRSP-CL-WORK-STR-LOC                PIC 9(004)  VALUE ZEROS.  EBCBS  
ITS102*01  LRSP-CL-WORK-KWD-LGTH               PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  LRSP-CL-WORK-KWD-LGTH               PIC 9(004)  VALUE ZEROS.  EBCBS  
ITS102*01  LRSP-CL-WORK-SPC-CNT                PIC 9(002)  VALUE ZEROS.  EBCBS  
ITS102 01  LRSP-CL-WORK-SPC-CNT                PIC 9(004)  VALUE ZEROS.  EBCBS  
065879 01  LRSP-CL-WORK-AREA.                                            EBCBS  
ITS102*    05  LRSP-CL-WORK-BRKDWN              OCCURS  80 TIMES.        EBCBS  
065880     05  LRSP-CL-WORK-BRKDWN              OCCURS  86 TIMES.        EBCBS  
065881        10  LRSP-CL-WORK-BYTE            PIC X(001).               EBCBS  
065882 01  LRSP-CL-WORK-LN-BRKDWN REDEFINES LRSP-CL-WORK-AREA.           EBCBS  
065883        10  LRSP-CL-WORK-LN-FIL1         PIC X(003).               EBCBS  
ITS102*       10  LRSP-CL-WORK-LN-NUM          PIC 9(002).               EBCBS  
ITS102        10  LRSP-CL-WORK-LN-NUM          PIC 9(003).               EBCBS  
065885        10  LRSP-CL-WORK-LN-SW           PIC X(001).               EBCBS  
065886        10  LRSP-CL-WORK-LN-FIL2         PIC X(002).               EBCBS  
065887        10  LRSP-CL-WORK-LN-DATA         PIC X(077).               EBCBS  
065888*                                                                  EBCBS  
065889 01  DELIM                               PIC X(001) VALUE ','.     EBCBS  
065890 01  LN-UNSTRING-RECORDS                 PIC 9(004) VALUE ZEROS.   EBCBS  
065891 01  LN-KEYWORD-TABLE.                                             EBCBS  
065892     05  UNSTRING-LN-RECORD               OCCURS 25 TIMES          EBCBS  
065893           INDEXED BY INDX1.                                       EBCBS  
065894         10  UNSTRING-RECORD             PIC X(040).               EBCBS  
065895*                                                                  EBCBS  
ITS102*01  WS-LN-DATA-FORMATTED.                                         EBCBS  
ITS102*    05  WS-LN-DATA-AMOUNT               PIC ZZZZZZ9V99.           EBCBS  
ITS102*    05  WS-LN-DATA-UNIT                 PIC ZZZ9.                 EBCBS  
065896 01  LN-KEYWORD-BRKDWN.                                            EBCBS  
065897     05  LN-KEYWORD-LN-ASTKL             PIC X(001) VALUE '*'.     EBCBS  
ITS102     05  LN-KEYWORD-LN-NUM               PIC 9(002).               EBCBS  
065899     05  LN-KEYWORD-LN-KEYWORD           PIC X(008).               EBCBS  
065900*                                                                  EBCBS  
ITS102 01  LN-KEYWORD-BRKDWN-BIG.                                        EBCBS  
ITS102     05  LN-KEYWORD-LN-ASTKL-B           PIC X(001) VALUE '*'.     EBCBS  
ITS102     05  LN-KEYWORD-LN-NUM-B             PIC 9(003).               EBCBS  
065899     05  LN-KEYWORD-LN-KEYWORD-B         PIC X(008).               EBCBS  
065900*                                                                  EBCBS  
065901 01  IGZEDT4                        PIC X(007) VALUE 'IGZEDT4'.   05230101
065902 01   WS-EMC-DATE-CCYYMMDD.                                       05230201
065903      05   WS-EMC-DATE-CC           PIC 99.                       05230301
065904      05   WS-EMC-DATE.                                           05231001
065905           10  WS-EMC-DATE-YR       PIC 99.                       05240001
065906           10  WS-EMC-DATE-MO       PIC 99.                       05250001
065907           10  WS-EMC-DATE-DA       PIC 99.                       05260001
065908*                                                                  EBCBS  
065901 01  WS-HEADER-TRAILER-RECS.                                       EBCBS  
065902     05  WS-EMC-HEADER-LITERAL.                                    EBCBS  
065903         10  FILLER                  PIC X(08) VALUE '^EMCHDR*'.   EBCBS  
065904         10  FILLER                  PIC X(08) VALUE 'HEADRID='.   EBCBS  
065905         10  FILLER                  PIC X(08) VALUE 'SYSTEM90'.   EBCBS  
065906         10  FILLER                  PIC X(10) VALUE '*EMCCRDTE='. EBCBS  
065907         10  EMC-CREATION-DATE.                                    EBCBS  
065908             20  EMC-CREATION-CENT   PIC 9(02) VALUE 19.           EBCBS  
065909             20  EMC-CREATION-YY     PIC 9(02).                    EBCBS  
065910             20  EMC-CREATION-MM     PIC 9(02).                    EBCBS  
065911             20  EMC-CREATION-DD     PIC 9(02).                    EBCBS  
065912         10  FILLER                  PIC X(10) VALUE '*EMCCRTME='. EBCBS  
065913         10  EMC-CREATION-TIME.                                    EBCBS  
065914             20  EMC-CREATION-HH     PIC 9(02).                    EBCBS  
065915             20  EMC-CREATION-MN     PIC 9(02).                    EBCBS  
065916             20  EMC-CREATION-SS     PIC 9(02).                    EBCBS  
065917         10  FILLER                  PIC X(10) VALUE '*EMCCYDTE='. EBCBS  
065918         10  EMC-CYCLE-DATE.                                       EBCBS  
065919             20  EMC-CYCLE-CENT      PIC 9(02) VALUE 19.           EBCBS  
065920             20  EMC-CYCLE-YY        PIC 9(02).                    EBCBS  
065921             20  EMC-CYCLE-MM        PIC 9(02).                    EBCBS  
065922             20  EMC-CYCLE-DD        PIC 9(02).                    EBCBS  
065923         10  FILLER                  PIC X(180).                   EBCBS  
065924*                                                                  EBCBS  
065925     05  WS-EMC-TRAILER-LITERAL.                                   EBCBS  
065926         10  FILLER                  PIC X(08) VALUE '^EMCTRL*'.   EBCBS  
065927         10  FILLER                  PIC X(08) VALUE 'TRAILID='.   EBCBS  
065928         10  FILLER                  PIC X(08) VALUE 'SYSTEM90'.   EBCBS  
065929         10  FILLER                  PIC X(06) VALUE '*ZINP='.     EBCBS  
065930         10  EMC-ZINP-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065931         10  FILLER                  PIC X(06) VALUE '*ZOUT='.     EBCBS  
065932         10  EMC-ZOUT-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065933         10  FILLER                  PIC X(06) VALUE '*ZMED='.     EBCBS  
065934         10  EMC-ZMED-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065935         10  FILLER                  PIC X(06) VALUE '*ZXIP='.     EBCBS  
065936         10  EMC-ZXIP-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065937         10  FILLER                  PIC X(06) VALUE '*ZXOP='.     EBCBS  
065938         10  EMC-ZXOP-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065939         10  FILLER                  PIC X(06) VALUE '*ZXMD='.     EBCBS  
065940         10  EMC-ZXMD-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065941         10  FILLER                  PIC X(06) VALUE '*ZU82='.     EBCBS  
065942         10  EMC-ZU82-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065943         10  FILLER                  PIC X(06) VALUE '*ZHAC='.     EBCBS  
065944         10  EMC-ZHAC-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065945         10  FILLER                  PIC X(06) VALUE '*ZUBF='.     EBCBS  
065946         10  EMC-ZUBF-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065947         10  FILLER                  PIC X(06) VALUE '*ZSVN='.     EBCBS  
065948         10  EMC-ZSVN-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065949         10  FILLER                  PIC X(06) VALUE '*ZU92='.     EBCBS  
065950         10  EMC-ZU92-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065951         10  FILLER                  PIC X(06) VALUE '*ZITS='.     EBCBS  
065952         10  EMC-ZITS-CNT            PIC 9(08) VALUE ZEROS.        EBCBS  
065953         10  FILLER                  PIC X(07) VALUE '*FILCT='.    EBCBS  
065954         10  EMC-FILE-RECORD-CNT     PIC 9(08) VALUE ZEROS.        EBCBS  
065955         10  FILLER                  PIC X(07) VALUE '*TOTCH='.    EBCBS  
065956         10  EMC-HASH-TOTAL-CHARGES  PIC S9(13)V99 VALUE ZEROS.    EBCBS  
065957         10  FILLER                  PIC X(02).                    EBCBS  
065958*                                                                  EBCBS  
065959 01  WS-DATE-WORK-FIELD              PIC  9(04)  VALUE ZEROS.      EBCBS  
065960 01  WS-DATE-WORK-FILED-BRKDWN REDEFINES WS-DATE-WORK-FIELD.       EBCBS  
065961     05  WS-DATE-WORK-FIELD-1        PIC  9(01).                   EBCBS  
065962     05  WS-DATE-WORK-FIELD-2-4      PIC  9(03).                   EBCBS  
065963*                                                                         
C97522 01  TEST-TYPE-BILL.                                                      
C97522     05  TEST-TYPE-BILL-1-2.                                              
C97522         10  TEST-TYPE-BILL-1    PIC X(01).                               
C97522         10  FILLER              PIC X(01).                               
C97522     05  FILLER                  PIC X(01).                               
           SKIP3                                                        EB6651  
065965 01  ITCDB99D-ITS-TRAILER.                                        EB6651  
065966     COPY ITCDB99D.                                               EB6651  
065963*                                                                         
065964 01  EMC-TRAILER-REC-SW              PIC  X(03)      VALUE 'NO '. CEBCBS11
065965     88  EMC-TRAILER-REC                             VALUE 'YES'. CL393611
065963*--------------------------------------------------------------*   EBCBS  
065970*    EJECT                                                         EBCBS  
066000 LINKAGE SECTION.                                                         
066100*---------------*                                                         
066200     SKIP3                                                                
066300 01  PCB-DUMMY               PIC  X(01).                                  
066400 01  PCB-ES01                PIC  X(01).                                  
066500 01  PCB-ES02                PIC  X(01).                                  
066600 01  PCB-ES03                PIC  X(01).                                  
066700 01  PCB-ES04                PIC  X(01).                                  
066800 01  PCB-ES05                PIC  X(01).                                  
066900 01  PCB-ES06                PIC  X(01).                                  
067000 01  PCB-ES07                PIC  X(01).                                  
067100 01  PCB-ES08                PIC  X(01).                                  
067200 01  PCB-ES09                PIC  X(01).                                  
067300 01  PCB-ES10                PIC  X(01).                                  
067301 01  PCB-ZS01                PIC  X(01).                                  
067302 01  PCB-ZS02                PIC  X(01).                                  
067303 01  PCB-ZS03                PIC  X(01).                                  
067304 01  PCB-ZS04                PIC  X(01).                                  
067305 01  PCB-ZS05                PIC  X(01).                                  
067306 01  PCB-ZS06                PIC  X(01).                                  
067307 01  PCB-ZS07                PIC  X(01).                                  
067308 01  PCB-ZS08                PIC  X(01).                                  
067309 01  PCB-ZS09                PIC  X(01).                                  
067310 01  PCB-ZS10                PIC  X(01).                                  
067400 01  PCB-ES11                PIC  X(01).                                  
067410 01  PCB-ITS                 PIC  X(01).                          CL393611
067420 01  PCB-ITR                 PIC  X(01).                          CL395543
067420 01  PCB-SUB                 PIC  X(01).                           EBCBS  
C82615 01  PCB-BK01                PIC  X(01).                                  
C82615 01  PCB-BK02                PIC  X(01).                                  
C82615 01  PCB-BK03                PIC  X(01).                                  
C82615 01  PCB-BK04                PIC  X(01).                                  
C82615 01  PCB-BK05                PIC  X(01).                                  
C82615 01  PCB-BK06                PIC  X(01).                                  
C82615 01  PCB-BK07                PIC  X(01).                                  
C82615 01  PCB-BK08                PIC  X(01).                                  
C82615 01  PCB-BK09                PIC  X(01).                                  
C82615 01  PCB-BK10                PIC  X(01).                                  
128601 01  PCB-HCSD                PIC  X(01).                           EBCBS  
128601 01  PCB-DIAG                PIC  X(01).                           EBCBS  
C82615 01  PCB-AGP                 PIC  X(01).                                  
C82615 01  PCB-AXF                 PIC  X(01).                                  
C82615 01  PCB-AXI                 PIC  X(01).                                  
C83330 01  PCB-GRP                 PIC  X(01).                                  
124519 01  PCB-PRV                 PIC  X(01).                                  
067500     EJECT                                                                
067600 PROCEDURE DIVISION               USING PCB-DUMMY                         
067700                                        PCB-ES01                          
067800                                        PCB-ES02                          
067900                                        PCB-ES03                          
068000                                        PCB-ES04                          
068100                                        PCB-ES05                          
068200                                        PCB-ES06                          
068300                                        PCB-ES07                          
068400                                        PCB-ES08                          
068500                                        PCB-ES09                          
068600                                        PCB-ES10                          
068601                                        PCB-ZS01                          
068602                                        PCB-ZS02                          
068603                                        PCB-ZS03                          
068604                                        PCB-ZS04                          
068605                                        PCB-ZS05                          
068606                                        PCB-ZS06                          
068607                                        PCB-ZS07                          
068608                                        PCB-ZS08                          
068609                                        PCB-ZS09                          
068610                                        PCB-ZS10                          
068700                                        PCB-ES11                  CL393611
068800                                        PCB-ITS                   CL395543
068810                                        PCB-ITR                    EBCBS  
068811                                        PCB-SUB                    EBCBS  
C82615                                        PCB-BK01                          
C82615                                        PCB-BK02                          
C82615                                        PCB-BK03                          
C82615                                        PCB-BK04                          
C82615                                        PCB-BK05                          
C82615                                        PCB-BK06                          
C82615                                        PCB-BK07                          
C82615                                        PCB-BK08                          
C82615                                        PCB-BK09                          
C82615                                        PCB-BK10                          
                                              PCB-HCSD,                         
                                              PCB-DIAG,                         
C82615                                        PCB-AGP                           
C82615                                        PCB-AXF                           
C82615                                        PCB-AXI                           
C83330                                        PCB-GRP                           
124519                                        PCB-PRV.                          
068900                                                                          
069000 0000-MAIN-LINE   SECTION.                                                
069100                                                                          
069200     PERFORM  0100-INITIALIZE                                             
069300        THRU  0100-EXIT                                                   
069302*                                                                  EBCBS  
069303     PERFORM 99999-TAKE-A-CHECK-POINT.                             EBCBS  
069400                                                                          
069402     CALL IGZEDT4                 USING WS-EMC-DATE-CCYYMMDD.     05262001
069403*                                                                 05263001
069404     MOVE WS-EMC-DATE-CCYYMMDD TO INPUT-DATE.                     05264001
069405*                                                                 05265001
069406     MOVE WS-EMC-DATE-CC       TO EMC-CREATION-CENT,              05266001
069407                                  EMC-CYCLE-CENT.                 05267001
069408*                                                                 05268001
069402     MOVE WS-EMC-DATE-YR       TO EMC-CREATION-YY,                 EBCBS  
069412                                  EMC-CYCLE-YY.                    EBCBS  
069411*                                                                 05280001
069404     MOVE WS-EMC-DATE-MO       TO EMC-CREATION-MM,                 EBCBS  
069414                                  EMC-CYCLE-MM.                    EBCBS  
069414*                                                                 05310001
069406     MOVE WS-EMC-DATE-DA       TO EMC-CREATION-DD,                 EBCBS  
069416                                  EMC-CYCLE-DD.                    EBCBS  
069417*                                                                  EBCBS  
069409     ACCEPT WS-TIME            FROM TIME.                          EBCBS  
069410     MOVE WS-TIME              TO EMC-CREATION-TIME.               EBCBS  
069400*                                                                  EBCBS  
069420     WRITE BATCH-KEYWORD-REC   FROM WS-EMC-HEADER-LITERAL.         EBCBS  
069421     ADD 1                     TO EMC-FILE-RECORD-CNT.             EBCBS  
069422*                                                                  EBCBS  
ITS113     PERFORM  0500-SETUP-PROV-MAPPING-TABLE THRU 0500-EXIT.               
ITS113*                                                                  EBCBS  
069500     PERFORM  1000-PROCESS-INPUT                                          
069600       THRU   1000-EXIT                                                   
069700      UNTIL   END-OF-FILE OR EMC-TRAILER-REC.                      EBCBS  
069800                                                                          
069900     MOVE  AC-INPUT-CNT        TO       DL1-CNT                           
069910     MOVE  AC-MCF-CNT          TO       DL2-CNT                   CL393611
070000     MOVE  AC-CFS-INS-CNT      TO       DL3-CNT                   CL393611
070100     MOVE  AC-CFS-PRO-CNT      TO       DL4-CNT                   CL393611
070200     MOVE  AC-ADO-INS-CNT      TO       DL5-CNT                   CL393611
070300     MOVE  AC-ADO-PRO-CNT      TO       DL6-CNT                   CL393611
070400     MOVE  AC-ERROR-CNT        TO       DL7-CNT                   CL393611
070410     MOVE  AC-ADJUST-CNT       TO       DL7A-CNT                  CL395543
070500     MOVE  AC-OUTPUT-CNT       TO       DL8-CNT                   CL393611
070600     MOVE  AC-CFS-AMT          TO       DL9-AMT                   CL393611
070700     MOVE  AC-ADO-AMT          TO       DL10-AMT.                 CL393611
070800                                                                          
070900     MOVE  HDR1                TO       PRINT-LINE.                       
071000     WRITE PRINT-LINE  AFTER ADVANCING  TOP-OF-PAGE.                      
071100                                                                          
071200     MOVE  HDR2                TO       PRINT-LINE.                       
071300     MOVE  1                   TO       PRINT-LINE-CC.                    
071400     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
071500                                                                          
071600     MOVE  3                   TO       PRINT-LINE-CC.                    
071700     MOVE  DETAIL-LINE-1       TO       PRINT-LINE                        
071800     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
071900     MOVE  2                   TO       PRINT-LINE-CC                     
072000     MOVE  DETAIL-LINE-2       TO       PRINT-LINE                        
072100     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
072200     MOVE  DETAIL-LINE-3       TO       PRINT-LINE                        
072300     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
072400     MOVE  DETAIL-LINE-4       TO       PRINT-LINE                        
072500     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
072600     MOVE  DETAIL-LINE-5       TO       PRINT-LINE                        
072700     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
072800     MOVE  DETAIL-LINE-6       TO       PRINT-LINE                        
072900     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
073000     MOVE  DETAIL-LINE-7       TO       PRINT-LINE                        
073100     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
073110     MOVE  DETAIL-LINE-7A      TO       PRINT-LINE                CL395543
073120     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.      CL395543
073200     MOVE  DETAIL-LINE-8       TO       PRINT-LINE                        
073300     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
073400     MOVE  DETAIL-LINE-9       TO       PRINT-LINE                        
073500     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.              
073510     MOVE  DETAIL-LINE-10      TO       PRINT-LINE                CL393611
073520     WRITE PRINT-LINE  AFTER ADVANCING  PRINT-LINE-CC LINES.      CL393611
073600                                                                          
073610     COMPUTE EMC-HASH-TOTAL-CHARGES = (AC-CFS-AMT + AC-ADO-AMT)    EBCBS  
073611     ADD 1                     TO EMC-FILE-RECORD-CNT.             EBCBS  
073612     WRITE BATCH-KEYWORD-REC   FROM WS-EMC-TRAILER-LITERAL.        EBCBS  
073613*                                                                  EBCBS  
073700     CLOSE    SCSE-INPUT-FILE                                             
073800              BATCH-KEYWORD-FILE                                          
073900              ERROR-FILE                                                  
074000              CONTROL-REPORT.                                             
074100                                                                          
074200     IF   AC-INPUT-CNT  =  0                                              
074300          MOVE  8   TO  RETURN-CODE                                       
074400     ELSE                                                                 
074500     IF   AC-ERROR-CNT  >  0                                              
074600          MOVE  4   TO  RETURN-CODE                                       
074700     ELSE                                                                 
074800          MOVE  0   TO  RETURN-CODE.                                      
074900                                                                          
074901     IF EMC-TRAILER-REC                                            EBCBS  
             DISPLAY 'CDB99D-CLM-TRANS-CNT  => ' CDB99D-CLM-TRANS-CNT           
             DISPLAY 'AC-CFS-INS-CNT        => ' AC-CFS-INS-CNT                 
             DISPLAY 'AC-CFS-PRO-CNT        => ' AC-CFS-PRO-CNT                 
             DISPLAY 'AC-ADO-INS-CNT        => ' AC-ADO-INS-CNT                 
             DISPLAY 'AC-ADO-PRO-CNT        => ' AC-ADO-PRO-CNT                 
             DISPLAY 'CDB99D-REC-CNT        => ' CDB99D-REC-CNT                 
             DISPLAY 'AC-RECD-CNT           => ' AC-RECD-CNT                    
074902       IF CDB99D-CLM-TRANS-CNT IS NOT EQUAL TO                     EBCBS  
074903                       (AC-CFS-INS-CNT + AC-CFS-PRO-CNT +                 
074904                        AC-ADO-INS-CNT + AC-ADO-PRO-CNT)                  
074905       OR CDB99D-REC-CNT IS NOT EQUAL TO AC-RECD-CNT               EBCBS  
074906*      OR CDB99D-NET-LIAB-AMT IS NOT EQUAL TO                      EBCBS  
074907*                      (AC-CFS-AMT +  AC-ADO-AMT)                  EBCBS  
074909          MOVE 0999                   TO RETURN-CODE               EBCBS  
074910       END-IF                                                      EBCBS  
074911     END-IF.                                                       EBCBS  
074911*                                                                  EBCBS  
074912*                                                                  EBCBS  
074913*  SAVE JULIAN DATE AND SEQ NUMBER FOR MULTIPLE PROCESSING         EBCBS  
074914*  WITHIN THE SAME DAY.                                            EBCBS  
074915*                                                                  EBCBS  
074916     OPEN OUTPUT  CS90DT-CNTL-FILE-OUT.                            EBCBS  
074917     INITIALIZE   CS90DT-CNTL-OUT-REC.                             EBCBS  
074918     MOVE WS-JUL-DATE       TO CS90DT-CNTL-OUT-JUL-DATE.           EBCBS  
074919     MOVE WS-CLAIM-SEQ-NBR  TO CS90DT-CNTL-OUT-SEQ-NUM.            EBCBS  
074920     WRITE CS90DT-CNTL-OUT-REC.                                    EBCBS  
074921     CLOSE        CS90DT-CNTL-FILE-OUT.                            EBCBS  
074922*                                                                  EBCBS  
075000     GOBACK.                                                              
075100     EJECT                                                                
075200 0100-INITIALIZE  SECTION.                                                
075300                                                                          
075400****************************************************************          
ITS102* COMPUTE THE LENGTH OF KEYWORDS                                          
075500*                                                                         
075900****************************************************************          
ITS102       INITIALIZE    WS-LINEITEM-KW-TABLE.                                
ITS102       COMPUTE WS-KW-CHG-LEN  = FUNCTION LENGTH (WS-KW-CHG )              
ITS102       COMPUTE WS-KW-HSC-LEN  = FUNCTION LENGTH (WS-KW-HSC )              
ITS102       COMPUTE WS-KW-POS-LEN  = FUNCTION LENGTH (WS-KW-POS )              
ITS102       COMPUTE WS-KW-SSDT-LEN = FUNCTION LENGTH (WS-KW-SSDT )             
ITS102       COMPUTE WS-KW-ESDT-LEN = FUNCTION LENGTH (WS-KW-ESDT )             
ITS102       COMPUTE WS-KW-SMC-LEN  = FUNCTION LENGTH (WS-KW-SMC )              
ITS102       COMPUTE WS-KW-UNIT-LEN = FUNCTION LENGTH (WS-KW-UNIT)              
ITS102       COMPUTE WS-KW-PLO-LEN  = FUNCTION LENGTH (WS-KW-PLO )              
ITS102       COMPUTE WS-KW-DIAG-LEN = FUNCTION LENGTH (WS-KW-DIAG)              
ITS102       COMPUTE WS-KW-PI-LEN   = FUNCTION LENGTH (WS-KW-PI  )              
ITS102       COMPUTE WS-KW-OCAL-LEN = FUNCTION LENGTH (WS-KW-OCAL)              
ITS102       COMPUTE WS-KW-NTE-LEN  = FUNCTION LENGTH (WS-KW-NTE )              
ITS102       COMPUTE WS-KW-RSLT-LEN = FUNCTION LENGTH (WS-KW-RSLT)              
ITS102       COMPUTE WS-KW-PRVID-LEN = FUNCTION LENGTH (WS-KW-PRVID )           
ITS102       COMPUTE WS-KW-PART-LEN = FUNCTION LENGTH (WS-KW-PART )             
ITS102       COMPUTE WS-KW-PRLOC-LEN = FUNCTION LENGTH (WS-KW-PRLOC )           
ITS102       COMPUTE WS-KW-INIT-LEN = FUNCTION LENGTH (WS-KW-INIT )             
ITS102       COMPUTE WS-KW-PM-LEN   = FUNCTION LENGTH (WS-KW-PM )               
ITS102       COMPUTE WS-KW-RL-LEN   = FUNCTION LENGTH (WS-KW-RL )               
ITS102       COMPUTE WS-KW-SE-LEN   = FUNCTION LENGTH (WS-KW-SE )               
ITS102       COMPUTE WS-KW-SPLT-LEN = FUNCTION LENGTH (WS-KW-SPLT)              
ITS102       COMPUTE WS-KW-ORIG-LEN = FUNCTION LENGTH (WS-KW-ORIG)              
ITS102       COMPUTE WS-KW-OCPLN-LEN = FUNCTION LENGTH (WS-KW-OCPLN )           
ITS102       COMPUTE WS-KW-OCCV-LEN = FUNCTION LENGTH (WS-KW-OCCV )             
ITS102       COMPUTE WS-KW-NDC-LEN  = FUNCTION LENGTH (WS-KW-NDC )              
ITS102       COMPUTE WS-KW-837CHG-LEN = FUNCTION LENGTH (WS-KW-837CHG )         
ITS102       COMPUTE WS-KW-LNCEX-LEN = FUNCTION LENGTH (WS-KW-LNCEX)            
ITS102       COMPUTE WS-KW-LLNB-LEN = FUNCTION LENGTH (WS-KW-LLNB )             
ITS102       COMPUTE WS-KW-LNNT-LEN = FUNCTION LENGTH (WS-KW-LNNT )             
ITS102       COMPUTE WS-KW-CPT4-LEN = FUNCTION LENGTH (WS-KW-CPT4 )             
ITS102       COMPUTE WS-KW-LNPI-LEN = FUNCTION LENGTH (WS-KW-LNPI )             
175203       COMPUTE WS-KW-M2-LEN   = FUNCTION LENGTH (WS-KW-M2 )               
172756       COMPUTE WS-KW-IG-LEN   = FUNCTION LENGTH (WS-KW-IG )               
257760       COMPUTE WS-KW-LDX-LEN  = FUNCTION LENGTH (WS-KW-LDX)               
430147       COMPUTE WS-KW-M3-LEN   = FUNCTION LENGTH (WS-KW-M3 )               
430147       COMPUTE WS-KW-M4-LEN   = FUNCTION LENGTH (WS-KW-M4 )               
076000                                                                          
075400****************************************************************          
075500*                                                                         
075600*     OPEN THE INPUT FILE, READ THE FIRST RECORD AND                      
075700*     INITIALIZE PRINT REPORT                                             
075800*                                                                         
075900****************************************************************          
076100     OPEN INPUT   SCSE-INPUT-FILE                                         
076101                  CS90DT-CNTL-FILE-IN                              EBCBS  
189277                  CROSS-OVER-FILE                                         
076200         OUTPUT   BATCH-KEYWORD-FILE                                      
076300                  ERROR-FILE                                              
076400                  CONTROL-REPORT.                                         
076500                                                                          
076600     ACCEPT INPUT-DATE         FROM   DATE.                               
076610                                                                  CL393611
076612     MOVE   INPUT-YEAR           TO   WS-DATE-YR.                 CL393611
076614     MOVE   INPUT-DAY            TO   WS-DATE-DA.                 CL393611
076616     MOVE   INPUT-MONTH          TO   WS-DATE-MO.                 CL393611
076618     MOVE   WS-DATE              TO   WS-PURG-DATE.               CL393611
076620                                                                  CL393611
076622*        DETERMINE PURGE DATE FOR ITS DATA BASE SEGMENTS          CL393611
076624*        TODYS DATE PLUS 2 MONTHS                                 CL393611
076626                                                                  CL393611
076628     ADD    2                    TO   WS-PURG-MM.                 CL393611
076630     IF  WS-PURG-MM   >  12                                       CL393611
076632         SUBTRACT  12          FROM   WS-PURG-MM                  CL393611
076634         ADD       1             TO   WS-PURG-YY                  CL393611
076636     ELSE                                                         CL393611
076638         NEXT SENTENCE.                                           CL393611
076640                                                                  CL393611
076642     IF  WS-PURG-DD   >  28                                       CL393611
076644         MOVE      28            TO   WS-PURG-DD                  CL393611
076646     ELSE                                                         CL393611
076648         NEXT SENTENCE.                                           CL393611
076650                                                                  CL393611
076700     MOVE   INPUT-YEAR           TO   REF-YEAR.                           
076800     MOVE   INPUT-DAY            TO   REF-DAY.                            
076900     MOVE   INPUT-MONTH          TO   REF-MONTH.                          
077000     MOVE   REFORMATTED-DATE     TO   HDR1-REPORT-DATE.                   
077100     ACCEPT INPUT-TIME         FROM   TIME.                               
077200     MOVE   INPUT-HOUR           TO   WS-H-TIME-HH.                       
077300     MOVE   INPUT-MINUTE         TO   WS-H-TIME-MM.                       
077400     MOVE   INPUT-SECOND         TO   WS-H-TIME-SS.                       
077500     MOVE   WS-H-TIME-HH         TO   HDR2-REPORT-TIME-HH.                
077600     MOVE   WS-H-TIME-MM         TO   HDR2-REPORT-TIME-MM.                
077700     MOVE   WS-H-TIME-SS         TO   HDR2-REPORT-TIME-SS.                
077800     MOVE   1                    TO   HDR1-PAGE-NO                        
077900                                                                          
077901*                                                                  EBCBS  
077902*  MAKE SURE THAT SET DUPLICATE CLAIM NUMBER ACCORDING TO THE      EBCBS  
077903*  DATE CONTROL FILE.                                              EBCBS  
077904*                                                                  EBCBS  
077904     ACCEPT WS-JUL-DATE        FROM DAY.                           EBCBS  
077905     READ  CS90DT-CNTL-FILE-IN                                     EBCBS  
077905       AT END                                                      EBCBS  
077906         MOVE 0911             TO RETURN-CODE                      EBCBS  
077907     END-READ.                                                     EBCBS  
077908*                                                                  EBCBS  
077909     IF CS90DT-CNTL-OVERRIDE IS EQUAL TO 'T'                       EBCBS  
077910       MOVE CS90DT-CNTL-IN-JUL-DATE (2: 1)                         EBCBS  
077911                                      TO WS-JUL-DATE-YEAR          EBCBS  
077912       MOVE CS90DT-CNTL-IN-JUL-DATE (3: 3)                         EBCBS  
077913                                      TO WS-JUL-DATE-DA            EBCBS  
077914       MOVE CS90DT-CNTL-IN-SEQ-NUM    TO WS-CLAIM-SEQ-NBR          EBCBS  
077915     ELSE                                                          EBCBS  
077916       IF CS90DT-CNTL-IN-JUL-DATE IS EQUAL TO WS-JUL-DATE          EBCBS  
077917         MOVE CS90DT-CNTL-IN-SEQ-NUM  TO WS-CLAIM-SEQ-NBR          EBCBS  
077918       END-IF                                                      EBCBS  
077911     END-IF.                                                       EBCBS  
077920*                                                                  EBCBS  
077912     CLOSE        CS90DT-CNTL-FILE-IN.                             EBCBS  
077913*                                                                  EBCBS  
078000     READ  SCSE-INPUT-FILE                                                
078100       AT END                                                             
078200         MOVE  'Y'               TO   END-OF-FILE-SWITCH                  
078300         MOVE  1000              TO   KILL-MODULE-POSITION                
078400         CALL  'LXKILL'       USING   KILL-MODULE-NM                      
078500                                      KILL-MODULE-POSITION                
078600                                                                          
078700         MOVE  HDR1              TO   PRINT-LINE                          
078800         WRITE PRINT-LINE                                                 
078900                     AFTER ADVANCING  TOP-OF-PAGE                         
079000         MOVE  HDR2              TO   PRINT-LINE                          
079100         MOVE  1                 TO   PRINT-LINE-CC                       
079200         WRITE PRINT-LINE                                                 
079300                     AFTER ADVANCING  PRINT-LINE-CC LINES                 
079400         MOVE  ERROR-LINE        TO   PRINT-LINE                          
079500         MOVE  3                 TO   PRINT-LINE-CC                       
079600         WRITE PRINT-LINE                                                 
079700                     AFTER ADVANCING  PRINT-LINE-CC LINES                 
079800                                                                          
079900         CLOSE    SCSE-INPUT-FILE                                         
080000                  BATCH-KEYWORD-FILE                                      
080100                  ERROR-FILE                                              
080200                  CONTROL-REPORT                                          
080300         CALL 'ZNCNCLDP'.                                                 
080400                                                                          
080500     IF  SCSE-RCD-TYPE    NOT =   '05' AND 'A5' AND '1A' AND '9D' CEBCBS11
080600         MOVE  1001              TO   KILL-MODULE-POSITION                
080700                                                                          
080800         CALL  'LXKILL'       USING   KILL-MODULE-NM                      
080900                                      KILL-MODULE-POSITION                
081000                                                                          
081100         MOVE  HDR1              TO   PRINT-LINE                          
081200         WRITE PRINT-LINE                                                 
081300                     AFTER ADVANCING  TOP-OF-PAGE                         
081400         MOVE  HDR2              TO   PRINT-LINE                          
081500         MOVE  1                 TO   PRINT-LINE-CC                       
081600         WRITE PRINT-LINE                                                 
081700                     AFTER ADVANCING  PRINT-LINE-CC LINES                 
081800         MOVE  ERROR-LINE        TO   PRINT-LINE                          
081900         MOVE  3                 TO   PRINT-LINE-CC                       
082000         WRITE PRINT-LINE                                                 
082100                     AFTER ADVANCING  PRINT-LINE-CC LINES                 
082200                                                                          
082300         CLOSE    SCSE-INPUT-FILE                                         
082400                  BATCH-KEYWORD-FILE                                      
082500                  ERROR-FILE                                              
082600                  CONTROL-REPORT                                          
082700         CALL 'ZNCNCLDP'.                                                 
082800                                                                          
189277        PERFORM 9450-READ-CROSS-OVER                                      
189277                THROUGH 9450-EXIT                                         
189277                VARYING SUB-UN-ID FROM 1 BY 1                             
189277                UNTIL SUB-UN-ID > 12000 OR END-OF-CROSS.                  
189277        CLOSE    CROSS-OVER-FILE.                                         
                                                                                
082900     CALL  'LNDLIINT'         USING  PCB-NM-ES01  PCB-ES01                
083000                                     PCB-NM-ES02  PCB-ES02                
083100                                     PCB-NM-ES03  PCB-ES03                
083200                                     PCB-NM-ES04  PCB-ES04                
083300                                     PCB-NM-ES05  PCB-ES05                
083400                                     PCB-NM-ES06  PCB-ES06                
083500                                     PCB-NM-ES07  PCB-ES07                
083600                                     PCB-NM-ES08  PCB-ES08                
083700                                     PCB-NM-ES09  PCB-ES09                
083800                                     PCB-NM-ES10  PCB-ES10                
083801                                     PCB-NM-ZS01  PCB-ZS01                
083802                                     PCB-NM-ZS02  PCB-ZS02                
083803                                     PCB-NM-ZS03  PCB-ZS03                
083804                                     PCB-NM-ZS04  PCB-ZS04                
083805                                     PCB-NM-ZS05  PCB-ZS05                
083806                                     PCB-NM-ZS06  PCB-ZS06                
083807                                     PCB-NM-ZS07  PCB-ZS07                
083808                                     PCB-NM-ZS08  PCB-ZS08                
083809                                     PCB-NM-ZS09  PCB-ZS09                
083810                                     PCB-NM-ZS10  PCB-ZS10                
083900                                     PCB-NM-ES11  PCB-ES11        CL393611
083910                                     PCB-NM-ITS   PCB-ITS         CL395543
083920                                     PCB-NM-ITR   PCB-ITR          EBCBS  
083921                                     PCB-NM-SUB   PCB-SUB          EBCBS  
C82615                                     PCB-NM-BK01  PCB-BK01                
C82615                                     PCB-NM-BK02  PCB-BK02                
C82615                                     PCB-NM-BK03  PCB-BK03                
C82615                                     PCB-NM-BK04  PCB-BK04                
C82615                                     PCB-NM-BK05  PCB-BK05                
C82615                                     PCB-NM-BK06  PCB-BK06                
C82615                                     PCB-NM-BK07  PCB-BK07                
C82615                                     PCB-NM-BK08  PCB-BK08                
C82615                                     PCB-NM-BK09  PCB-BK09                
C82615                                     PCB-NM-BK10  PCB-BK10                
247201                                     PCB-NM-HCSD  PCB-HCSD         EBCBS  
247201                                     PCB-NM-DIAG  PCB-DIAG         EBCBS  
C82615                                     PCB-NM-AGP   PCB-AGP                 
C82615                                     PCB-NM-AXF   PCB-AXF                 
C82615                                     PCB-NM-AXI   PCB-AXI                 
C83330                                     PCB-NM-GRP   PCB-GRP                 
124519                                     PCB-NM-PRV   PCB-PRV.                
084000                                                                          
084100                                                                          
084200 0100-EXIT.                                                               
084300     EXIT.                                                                
084400     EJECT                                                                
084500 1000-PROCESS-INPUT    SECTION.                                           
084600                                                                          
084700****************************************************************          
084800*                                                                         
084900*     INITIALIZE THE WORK AREAS AND DETERMINE WHICH GROUP                 
085000*     IS BEING PROCESSED                                                  
085100*                                                                         
085200****************************************************************          
085300                                                                          
085400     MOVE SPACES                 TO   VALIDATE-THE-GROUP                  
085500                                      WS-PROMPT-ID-TABLE                  
085600                                      COMMON-INTERNAL-DATA                
085700                                      MANAGED-CARE-RECORD-SET     CL393611
085710                                      CCFMDATA,                   C414004L
085810     MOVE SPACE                  TO   ITS-INST-PROF-SWITCH.       CL393611
C97522     MOVE 'N'                    TO   FM170-SWITCH                        
085900     MOVE 'N'                    TO   ERROR-SWITCH                        
085910                                      ADJUST-SWITCH               CL395543
086000                                      GROUP-SWITCH                        
086100                                      NO-MORE-RECORD-SWITCH.              
086200     MOVE 1                      TO   KW-INDEX                            
551903     MOVE 'N'                    TO   REC-70-SWITCH                       
086300     MOVE ZERO                   TO   VTG-VALID-GROUP-CNT                 
086400                                      VTG-30                              
086500                                      VTG-31                              
086600                                      VTG-46                              
086700                                      VTG-E6                              
086800                                      VTG-D1                              
086900                                      VTG-50                              
087000                                      VTG-55                              
087100                                      VTG-60                              
087200                                      VTG-65                              
087300                                      VTG-F0                              
087400                                      VTG-F5                              
087500****                                  CID-PRIVATE-ROOM-DIFF       CX397034
087501                                      CID-ADJ-RECDT               CX397034
087600                                      CID-CFS-CHARGES                     
087700                                      CID-ADO-CHARGES.                    
087800                                                                          
087900     PERFORM 9000-READ-SCSE-FILE                                          
088000       THRU  9000-EXIT                                                    
088100      UNTIL  END-OF-FILE                                                  
088200       OR    GROUP-COMPLETED.                                             
088300                                                                          
088400     ADD   1                     TO   AC-INPUT-CNT                        
088500                                                                          
088600     IF    ERROR-SWITCH-ON                                                
088700         PERFORM  9300-WRITE-GROUP-IN-ERROR                               
088800           THRU   9300-EXIT                                               
088810           GO TO 1000-EXIT                                        CL395543
088900     ELSE                                                                 
088910       IF ADJUST-SWITCH-ON                                        CL395543
088911           IF    FM105-REC-TYPE       = '05'                              
088912             ADD 1               TO   AC-ADO-INS-CNT                      
088913           ELSE                                                           
088914             ADD 1               TO   AC-ADO-PRO-CNT                      
088915           END-IF                                                         
088920           PERFORM 4100-WRITE-TO-SF-RECYCLE-DB                    CL395543
088930              THRU 4100-EXIT                                      CL395543
088940               GO TO 1000-EXIT                                    CL395543
088950       ELSE                                                       CL395543
C97522         MOVE SPACES          TO TEST-TYPE-BILL                           
089000         IF    FM105-REC-TYPE       = '05'                                
C97522            MOVE FM110-TYPE-BILL TO TEST-TYPE-BILL                        
C97522         END-IF                                                           
089000         IF    FM105-REC-TYPE       = '05'                                
C97522         AND  (VTG-VALID-GROUP-CNT  = 7                                   
C97522             OR ((TEST-TYPE-BILL-1 = '4' OR '5' OR                        
C97522                   TEST-TYPE-BILL-1-2 = '14')   AND                       
C97522                   FM170-SWITCH = 'N'           AND                       
C97522                   VTG-VALID-GROUP-CNT  = 6 ))                            
089110             MOVE 'I'             TO   ITS-INST-PROF-SWITCH       CL393611
089120             MOVE  FM105-SCCF-SER-NUM      TO WS-SFSER,            EBCBS  
089121                                              CID-SFSER            EBCBS  
089122             INITIALIZE                       CID-XRFSERNO         EBCBS  
089200*                                                                 CL393611
089300*                                                                 CL393611
089400*                                                                 CL393611
089500*                                                                 CL393611
089600*                                                                 CL393611
089700*                                                                 CL393611
089800*                                                                 CL393611
089900*                                                                 CL393611
090000*                                                                 CL393611
090100*                                                                 CL393611
090200*                                                                 CL393611
C98668* DO NOT CALL THE ALTID BLACK BOX ANY MORE SINCE EMC                      
C98668* WILL POPULATE THE TRAILER RECORD WITH THE REAL ID FOR CS90              
C98668* PROCESSING UNDER THE CORREXCT ID.                                       
C98668         PERFORM                                                          
C98668           TEST AFTER                                                     
C98668           VARYING X-PTR FROM 1 BY 1                                      
C98668           UNTIL X-PTR IS EQUAL TO 3  OR                                  
C98668              FM130-ITS-PAYER-IND                                         
C98668              OF CIASF-30-RECORD      (X-PTR) IS EQUAL TO 'Y'             
C98668*            MOVE FM190-NARRATIVE (73:9)                                  
C98668             MOVE FM190-NARRATIVE (01:9)                                  
C98668              TO  FM130-SUB-ID-SUFX-4-17                                  
C98668                  OF CIASF-30-RECORD (X-PTR)                              
C98668         END-PERFORM                                                      
C82615*            CALL 'CNALTITS'     USING ITS-INST-PROF-SWITCH,       EBCBS  
C82615*                                      CCFMDATA,                  C414004L
C82615*                                      PCB-NM-AGP,                 EBCBS  
C82615*                                      PCB-NM-AXF,                 EBCBS  
C82615*                                      PCB-NM-AXI                  EBCBS  
090200*                                                                 CL393611
090201             CALL 'CNITSADR'     USING ITS-INST-PROF-SWITCH,       EBCBS  
090202                                       CCFMDATA,                  C414004L
C83330                                       COMMON-INTERNAL-DATA,              
090203                                       PCB-NM-SUB                  EBCBS  
C83330                                       PCB-NM-GRP                  EBCBS  
189277                                       WS-CROSS-RECORD                    
090200*                                                                 CL393611
090300             PERFORM  1500-PROCESS-INSTITUTION                            
090400               THRU   1500-EXIT                                           
090500             ADD  1               TO  AC-OUTPUT-CNT                       
090600             ADD  CID-CFS-CHARGES TO  AC-CFS-AMT                          
090700             ADD  CID-ADO-CHARGES TO  AC-ADO-AMT                          
090701             INITIALIZE               ITS-CL-TRAN-COUNT,           EBCBS  
090702                                      ITS-CL-TRAN-TABLE,           EBCBS  
090703                                      ITS-CL-WORK-PTR,             EBCBS  
090704                                      ITS-CL-WORK-AREA,            EBCBS  
ITS102*                                     WS-LN-DATA-FORMATTED,        EBCBS  
090705                                      CLCSF-LINEITEM-COUNT,        EBCBS  
090706                                      CLCSF-LINEITEM-TABLE         EBCBS  
090707             PERFORM  99999-COMPRES-ITS-CL-TRAN                    EBCBS  
091000             VARYING  KW-INDEX  FROM  1 BY 1                              
ITS102*              UNTIL   KW-INDEX >  200                             EBCBS  
ITS102               UNTIL   KW-INDEX >  9000                            EBCBS  
091200               OR     NO-MORE-TABLE-RECORDS                               
090711*                                                                  EBCBS  
090712             PERFORM  99999-WRITE-BATCH-KEYWORD-RCDS               EBCBS  
090713             ADD 1                        TO EMC-ZITS-CNT          EBCBS  
090800*            PERFORM  9400-WRITE-BATCH-KEYWORD-RCDS                EBCBS  
090900*             THRU    9400-EXIT                                    EBCBS  
091000*            VARYING  KW-INDEX  FROM  1 BY 1                       EBCBS  
091100*             UNTIL   KW-INDEX     >  200                          EBCBS  
091200*              OR     NO-MORE-TABLE-RECORDS                        EBCBS  
091300         ELSE                                                             
091400             IF   FM1A5-REC-TYPE        = 'A5'                            
091500             AND  VTG-VALID-GROUP-CNT   = 6                               
091510                 MOVE 'P'             TO   ITS-INST-PROF-SWITCH   CL393611
091520                 MOVE  FM1A5-SCCF-SER-NUM      TO WS-SFSER,        EBCBS  
091521                                                  CID-SFSER        EBCBS  
091522                 INITIALIZE                       CID-XRFSERNO     EBCBS  
091600*                                                                 CL393611
091700*                                                                 CL393611
091800*                                                                 CL393611
091900*                                                                 CL393611
092000*                                                                 CL393611
092100*                                                                 CL393611
092200*                                                                 CL393611
092300*                                                                 CL393611
092400*                                                                 CL393611
092500*                                                                 CL393611
092600*                                                                 CL393611
092700*                                                                 CL393611
092800*                                                                 CL393611
092900*                                                                 CL393611
093000*                                                                 CL393611
093100*                                                                 CL393611
093200*                                                                 CL393611
093300*                                                                 CL393611
093400*                                                                 CL393611
093500*                                                                 CL393611
093600*                                                                 CL393611
093700*                                                                 CL393611
093800*                                                                 CL393611
C98668* DO NOT CALL THE ALTID BLACK BOX ANY MORE SINCE EMC                      
C98668* WILL POPULATE THE TRAILER RECORD WITH THE REAL ID FOR CS90              
C98668* PROCESSING UNDER THE CORREXCT ID.                                       
C98668*                MOVE  FM1X0-FILLER-1  (41:9)  TO                         
C98668*                MOVE  FM1X0-FILLER-1  (01:9)  TO                         
C98668                 MOVE  FM1X0-BLUE2-USER-ID (01:9) TO                      
C98668                   FM1D0-SUB-ID-SUFX-4-17                                 
C82615*                CALL 'CNALTITS'     USING IT7-INST-PROF-SWITCH,   EBCBS  
C82615*                                      CCFMDATA,                  C414004L
C82615*                                      PCB-NM-AGP,                 EBCBS  
C82615*                                      PCB-NM-AXF,                 EBCBS  
C82615*                                      PCB-NM-AXI                  EBCBS  
093900*                                                                 CL393611
093901                 CALL 'CNITSADR'     USING                         EBCBS  
093902                                       ITS-INST-PROF-SWITCH,       EBCBS  
093903                                       CCFMDATA,                  C414004L
C83330                                       COMMON-INTERNAL-DATA,              
093904                                       PCB-NM-SUB,                 EBCBS  
C83330                                       PCB-NM-GRP,                 EBCBS  
189277                                       WS-CROSS-RECORD                    
093900*                                                                 CL393611
094000                 PERFORM  2000-PROCESS-PROFESSIONAL                       
094100                   THRU   2000-EXIT                                       
094200                 ADD CID-CFS-CHARGES    TO AC-CFS-AMT                     
094300                 ADD CID-ADO-CHARGES    TO AC-ADO-AMT                     
094400                 ADD 1                  TO AC-OUTPUT-CNT                  
094401                 INITIALIZE                ITS-CL-TRAN-COUNT,      EBCBS  
094402                                           ITS-CL-TRAN-TABLE,      EBCBS  
094403                                           ITS-CL-WORK-PTR,        EBCBS  
094404                                           ITS-CL-WORK-AREA,       EBCBS  
ITS102*                                          WS-LN-DATA-FORMATTED,   EBCBS  
094405                                           CLCSF-LINEITEM-COUNT,   EBCBS  
094406                                           CLCSF-LINEITEM-TABLE    EBCBS  
094407                 PERFORM  99999-COMPRES-ITS-CL-TRAN                EBCBS  
094408                 VARYING  KW-INDEX  FROM  1 BY 1                   EBCBS  
ITS102*                UNTIL   KW-INDEX     >  200                       EBCBS  
ITS102                 UNTIL   KW-INDEX     >  9000                      EBCBS  
094410                 OR     NO-MORE-TABLE-RECORDS                      EBCBS  
094411                 PERFORM  99999-WRITE-BATCH-KEYWORD-RCDS           EBCBS  
094412                 ADD 1                      TO EMC-ZITS-CNT        EBCBS  
094413*                                                                  EBCBS  
094500*                PERFORM  9400-WRITE-BATCH-KEYWORD-RCDS            EBCBS  
094600*                 THRU    9400-EXIT                                EBCBS  
094700*                VARYING  KW-INDEX    FROM 1 BY 1                  EBCBS  
094800*                 UNTIL   KW-INDEX       > 200                     EBCBS  
094900*                  OR     NO-MORE-TABLE-RECORDS                    EBCBS  
094910             ELSE                                                 CL393611
094920                 IF  FM71A-REC-TYPE       =  '1A'                 CL393611
094930                     GO TO 1000-EXIT                              CL393611
095000                 ELSE                                             CL393611
095001                   IF EMC-TRAILER-REC                              EBCBS  
095002                     GO TO 1000-EXIT                               EBCBS  
095003                   ELSE                                            EBCBS  
095100                     PERFORM  9300-WRITE-GROUP-IN-ERROR           CL393611
095200                        THRU  9300-EXIT                           CL393611
095210                     GO TO 1000-EXIT.                             CL393611
095300                                                                          
095312*        WRITE SF RECORDS TO THE ITS DATA BASE                    CL393611
095314*        FIRST READ THE ROOT SEGMENT                              CL393611
095316                                                                  CL393611
095318     PERFORM  9500-READ-ITS-ROOT                                  CL393611
095320        THRU  9500-EXIT.                                          CL393611
095322                                                                  CL393611
095324*        IF ROOT SEGMENT NOT FOUND - INSERT ROOT                  CX397034
095326*        IF ROOT SEGMENT IS FOUND - UPDATE ROOT                   CX397034
095328*                                                                 CX397034
095330                                                                  CL393611
095332     IF  PCB-DLI-RET-CD   =   '  '                                CL393611
095334         PERFORM  9600-UPDATE-ITS-ROOT                            CL393611
095336            THRU  9600-EXIT                                       CL393611
095338         PERFORM  9510-READ-SF-DATA                               CL393611
095340            THRU  9510-EXIT                                       CL393611
095342         IF  PCB-DLI-RET-CD   =   '  '                            CL393611
095344             PERFORM  9610-UPDATE-SF-DATA                         CL393611
095346                THRU  9610-EXIT                                   CL393611
095348         ELSE                                                     CL393611
095350             PERFORM  9710-ADD-SF-DATA                            CL393611
095352                THRU  9710-EXIT                                   CL393611
095354     ELSE                                                         CL393611
095356         PERFORM  9700-ADD-ITS-ROOT                               CL393611
095358            THRU  9700-EXIT.                                      CX397034
095360         PERFORM  9710-ADD-SF-DATA                                CL393611
095362            THRU  9710-EXIT.                                      CL393611
095364                                                                  CL393611
095400 1000-EXIT.                                                               
095500     EXIT.                                                                
095600     EJECT                                                                
095700 1500-PROCESS-INSTITUTION    SECTION.                                     
095800                                                                          
095900****************************************************************          
096000*                                                                         
096100*     PROCESS INSTITUTIONAL GROUP                                         
096200*                                                                         
096300****************************************************************          
096400                                                                          
096500     MOVE  FM105-SCCF-SER-NUM      TO   WS-CONVERT-SEQ-NBR                
096600*    MOVE  WS-CONV-PLAN-NBR        TO   WS-CLAIM-PLAN-NBR          EBCBS  
096700*    MOVE  WS-CONV-NBR-2-5         TO   WS-CLAIM-SEQ-NBR           EBCBS  
096800*    MOVE  ZERO                    TO   WS-CLAIM-END-NBR           EBCBS  
096900*                                                                  EBCBS  
097000*    IF   WS-CONV-NBR-1          NOT =  ZERO                       EBCBS  
097100*        ADD 1                     TO   WS-CONV-YEAR.              EBCBS  
097200*                                                                  EBCBS  
097300*    IF   WS-CONV-YEAR-2           =    0 OR 2 OR 4 OR 6 OR 8      EBCBS  
097400*        ADD 500                   TO   WS-CONV-DAYS.              EBCBS  
097500*                                                                  EBCBS  
097600*    MOVE WS-CONV-DAYS             TO   WS-CLAIM-DATE              EBCBS  
097601     PERFORM 99999-COMPUTE-CS90-CLMID.                             EBCBS  
097700     MOVE  WS-ALPHA-CLAIM-NBR      TO   CID-CLAIM-ID                      
097800                                                                          
097900     MOVE SPACES                   TO   WS-SUBSRIBER-ID.                  
097901     MOVE FM130-SUB-ID-NUM(1) (4: 9)                                      
097902                                   TO   WS-SUBSRIBER-ID (4: 9).           
098000                                                                          
098100     PERFORM 5000-SUB-ID-COMP                                             
098200      THRU   5000-EXIT.                                                   
098300                                                                          
098400     CALL  'LNCKDIG'            USING   WS-CHECK-LENGHT                   
098500                                        WS-SUB-ID-4-15-COMP               
098600                                        WS-SUB-MODIFIER.                  
098700                                                                          
098800     MOVE SPACES                   TO   SSA-HIST-SUBR-MDF                 
098900     MOVE WS-SUB-MODIFIER          TO   SSA-HIST-SUBR-MDF                 
099000                                        CID-SUB-MODIFIER                  
099100     MOVE WS-SUB-ID-4-15-COMP      TO   SSA-HIST-SUBR-NO                  
099200     MOVE WS-NUMERIC-CLAIM-NBR     TO   SSA-CLM-CLAIM-NO                  
099300                                                                          
099400     IF  FM105-TYPE-SUBM           EQUAL '2'                      CL395782
099410                                   OR    '3'                      CL395782
099420        NEXT SENTENCE                                             CL395782
099430     ELSE                                                         CL395782
099500         ADD 1                     TO   AC-CFS-INS-CNT                    
099600         PERFORM 4200-PROCESS-ORIGINALS                                   
099700          THRU   4200-EXIT                                                
099800         PERFORM 3000-BUILD-INST-CLAIM-GRP                                
099900          THRU   3000-EXIT                                                
100000         PERFORM 3100-BUILD-INST-CLAIM-LINE                               
100100          THRU   3100-EXIT                                                
100200         GO TO 1500-EXIT.                                                 
100300                                                                          
100400     PERFORM 9100-FIND-CLAIM-HIST-RCD                                     
100500      THRU   9100-EXIT                                                    
100600*                        SPACES MEANS HISTORY FOUND               CL393611
100700     IF   PCB-DLI-RET-CD           =    SPACES                            
100800         MOVE CC-CLM-STCD OF HIST-VSCPCLM                                 
100801                                   TO   WS-CLAIM-STATUS                   
100900         MOVE WS-CLAIM-STATUS-COMP TO   WS-NUMERIC-STATUS-CD              
101000         MOVE CC-CLM-ADJ-NO OF HIST-VSCPCLM                               
101001                                   TO   WS-CLAIM-ADJUST                   
101100         COMPUTE WS-NUMERIC-ADJUST-CD                                     
101200                                   =    999                               
101300                                   -    WS-CLAIM-ADJUST-COMP              
101400         MOVE WS-NUMERIC-ADJUST-CD TO   WS-RECORD-LEVEL-NO                
101500         MOVE CB-DEP-NO            TO   WS-DEPENDENT-NO                   
101600         ADD 1                     TO   AC-ADO-INS-CNT                    
101700         IF FM105-TYPE-SUBM         =   '2'                               
101800             PERFORM 4000-PROCESS-RESUBMITS                               
101900               THRU  4000-EXIT                                            
102000             PERFORM 3000-BUILD-INST-CLAIM-GRP                            
102100              THRU   3000-EXIT                                            
102200             PERFORM 3100-BUILD-INST-CLAIM-LINE                           
102300              THRU 3100-EXIT                                      CL395801
102301             PERFORM 4310-VERIFY-LINE-ITEM-COUNT                  CL395801
102302              THRU 4310-EXIT                                      CL395801
102303             PERFORM 4400-CHANGE-BRQ-CODE                         CL395801
102304              THRU 4400-EXIT.                                     CL395801
102400*                                                                 CL393611
102500*                                                                 CL393611
102600*                                                                 CL393611
102700*                                                                 CL393611
102800*                                                                 CL393611
102900*                                                                 CL393611
103000*                                                                 CL393611
103100*                                                                 CL393611
103200*                        NOT SPACES MEANS HISTORY NOT FOUND       CL393611
103210*                                              TRY TIP            CL393611
103300     IF   PCB-DLI-RET-CD      NOT  =    SPACES                            
103400         MOVE  FM105-TYPE-SUBM         TO   WS-RESUBMISSION-IND           
103500         ADD  1                        TO   AC-CFS-INS-CNT                
103600         PERFORM 4300-PROCESS-TIP-DATA-BASE                               
103700           THRU  4300-EXIT                                                
103800         PERFORM 3000-BUILD-INST-CLAIM-GRP                                
103900          THRU   3000-EXIT                                                
104000         PERFORM 3100-BUILD-INST-CLAIM-LINE                               
104100          THRU   3100-EXIT.                                               
104200                                                                          
104300 1500-EXIT.                                                               
104400     EXIT.                                                                
104500     EJECT                                                                
104600 2000-PROCESS-PROFESSIONAL   SECTION.                                     
104700                                                                          
104800****************************************************************          
104900*                                                                         
105000*     PROCESS PROFESSIONAL  GROUP                                         
105100*                                                                         
105200****************************************************************          
105300                                                                          
105400     MOVE  FM1A5-SCCF-SER-NUM      TO   WS-CONVERT-SEQ-NBR                
105500*    MOVE  WS-CONV-PLAN-NBR        TO   WS-CLAIM-PLAN-NBR          EBCBS  
105600*    MOVE  WS-CONV-NBR-2-5         TO   WS-CLAIM-SEQ-NBR           EBCBS  
105700*    MOVE  ZERO                    TO   WS-CLAIM-END-NBR           EBCBS  
105800*                                                                  EBCBS  
105900*    IF   WS-CONV-NBR-1          NOT =  ZERO                       EBCBS  
106000*        ADD 1                     TO   WS-CONV-YEAR.              EBCBS  
106100*                                                                  EBCBS  
106200*    IF    WS-CONV-YEAR-2          =    0 OR 2 OR 4 OR 6 OR 8      EBCBS  
106300*        ADD 500                   TO   WS-CONV-DAYS.              EBCBS  
106400*                                                                  EBCBS  
106500*    MOVE  WS-CONV-DAYS            TO   WS-CLAIM-DATE              EBCBS  
160501     PERFORM 99999-COMPUTE-CS90-CLMID.                             EBCBS  
106600     MOVE  WS-ALPHA-CLAIM-NBR      TO   CID-CLAIM-ID                      
106700                                                                          
106800     MOVE SPACES                   TO   WS-SUBSRIBER-ID.                  
106801     MOVE FM1D0-SUB-ID-NUM (4: 9)  TO   WS-SUBSRIBER-ID (4: 9).           
106900     PERFORM 5000-SUB-ID-COMP                                             
107000      THRU   5000-EXIT.                                                   
107100     CALL  'LNCKDIG'            USING   WS-CHECK-LENGHT                   
107200                                        WS-SUB-ID-4-15-COMP               
107300                                        WS-SUB-MODIFIER.                  
107400                                                                          
107500     MOVE SPACES                   TO   SSA-HIST-SUBR-MDF                 
107600     MOVE WS-SUB-MODIFIER          TO   SSA-HIST-SUBR-MDF                 
107700                                        CID-SUB-MODIFIER                  
107800     MOVE WS-SUB-ID-4-15-COMP      TO   SSA-HIST-SUBR-NO                  
107900     MOVE WS-NUMERIC-CLAIM-NBR     TO   SSA-CLM-CLAIM-NO                  
108000                                                                          
108100     IF    FM1A5-TYPE-SUBM         =    '2'                       CL395782
108110                                   OR   '3'                       CL395782
108120         NEXT SENTENCE                                            CL395782
108130     ELSE                                                         CL395782
108200         ADD 1                     TO   AC-CFS-PRO-CNT                    
108300         PERFORM 4200-PROCESS-ORIGINALS                                   
108400          THRU   4200-EXIT                                                
108500         PERFORM 3200-BUILD-PROF-CLAIM-GRP                                
108600          THRU   3200-EXIT                                                
108700         PERFORM 3300-BUILD-PROF-CLAIM-LINE                               
108800          THRU   3300-EXIT                                                
108900         GO TO 2000-EXIT.                                                 
109000                                                                          
109100     PERFORM 9100-FIND-CLAIM-HIST-RCD                                     
109200      THRU   9100-EXIT                                                    
109300*                        SPACES MEANS HISTORY FOUND               CL393611
109400     IF   PCB-DLI-RET-CD           =    SPACES                            
109500         MOVE CC-CLM-STCD OF HIST-VSCPCLM                                 
109501                                   TO   WS-CLAIM-STATUS                   
109600         MOVE WS-CLAIM-STATUS-COMP TO   WS-NUMERIC-STATUS-CD              
109700         MOVE CC-CLM-ADJ-NO OF HIST-VSCPCLM                               
109701                                   TO   WS-CLAIM-ADJUST                   
109800         COMPUTE WS-NUMERIC-ADJUST-CD                                     
109900                                   =    999                               
110000                                   -    WS-CLAIM-ADJUST-COMP              
110100         MOVE WS-NUMERIC-ADJUST-CD TO   WS-RECORD-LEVEL-NO                
110200         MOVE CB-DEP-NO            TO   WS-DEPENDENT-NO                   
110300         ADD 1                     TO   AC-ADO-PRO-CNT                    
110400         IF    FM1A5-TYPE-SUBM          =  '2'                            
110500             PERFORM 4000-PROCESS-RESUBMITS                               
110600               THRU  4000-EXIT                                            
110700             PERFORM 3200-BUILD-PROF-CLAIM-GRP                            
110800              THRU   3200-EXIT                                            
110900             PERFORM 3300-BUILD-PROF-CLAIM-LINE                           
111000              THRU 3300-EXIT                                      CL395801
111010             PERFORM 4310-VERIFY-LINE-ITEM-COUNT                  CL395801
111020              THRU 4310-EXIT                                      CL395801
111030             PERFORM 4400-CHANGE-BRQ-CODE                         CL395801
111040              THRU 4400-EXIT.                                     CL395801
111100*                                                                 CL393611
111200*                                                                 CL393611
111300*                                                                 CL393611
111400*                                                                 CL393611
111500*                                                                 CL393611
111600*                                                                 CL393611
111700*                                                                 CL393611
111800*                                                                 CL393611
111900*                        NOT SPACES MEANS HISTORY NOT FOUND       CL393611
111910*                                              TRY TIP            CL393611
112000     IF   PCB-DLI-RET-CD              NOT = SPACES                        
112100         MOVE  FM1A5-TYPE-SUBM         TO   WS-RESUBMISSION-IND           
112200         ADD  1                        TO   AC-CFS-PRO-CNT                
112300         PERFORM 4300-PROCESS-TIP-DATA-BASE                               
112400           THRU  4300-EXIT                                                
112500         PERFORM 3200-BUILD-PROF-CLAIM-GRP                                
112600          THRU   3200-EXIT                                                
112700         PERFORM 3300-BUILD-PROF-CLAIM-LINE                               
112800          THRU   3300-EXIT.                                               
112900                                                                          
113000                                                                          
113100 2000-EXIT.                                                               
113200     EXIT.                                                                
113300     EJECT                                                                
113400 3000-BUILD-INST-CLAIM-GRP   SECTION.                                     
113500                                                                          
113600****************************************************************          
113700*                                                                         
113800*     CALL THE CNSFIFMT PROGRAM AND BUILD THE INSTITUTIONAL               
113900*     CLAIM INFORMATION                                                   
114000*                                                                         
114100****************************************************************          
114200                                                                          
           MOVE SPACES TO CID-PATS.                                             
           MOVE ZERO   TO CID-LAST-ESDT.                                        
ITS113     IF WS-TC  > 0                                                        
114300                                                                          
ITS113        PERFORM 050C-UPDATE-CTUFCTBL THRU 050C-EXIT                       
ITS113                                                                          
ITS113     END-IF.                                                              
114300                                                                          
114400     CALL   'CNSFIFMT'         USING   CCFMDATA,                  C414004L
114500                                       COMMON-INTERNAL-DATA,      C414004L
114600                                       WS-PROMPT-ID-TABLE,                
ITS113                                  WS-PROVIDER-ASSIGNMENT-TABLE.           
114700 3000-EXIT.                                                               
114800     EXIT.                                                                
114900                                                                          
115000                                                                          
115100 3100-BUILD-INST-CLAIM-LINE  SECTION.                                     
115200                                                                          
115300****************************************************************          
115400*                                                                         
115500*     CALL THE CNSFILIN PROGRAM AND BUILD THE INSTITUTIONAL               
115600*     LINE  INFORMATION                                                   
115700*                                                                         
115800****************************************************************          
115900                                                                          
116000                                                                          
116100     CALL   'CNSFILIN'         USING   CCFMDATA,                  C414004L
116200                                       COMMON-INTERNAL-DATA,      C414004L
116300                                       WS-PROMPT-ID-TABLE.                
116400                                                                          
116500 3100-EXIT.                                                               
116600     EXIT.                                                                
116700                                                                          
116800                                                                          
116900 3200-BUILD-PROF-CLAIM-GRP   SECTION.                                     
117000                                                                          
117100****************************************************************          
117200*                                                                         
117300*     CALL THE CNSFPFMT PROGRAM AND BUILD THE PROFESSIONAL                
117400*     CLAIM INFORMATION                                                   
117500*                                                                         
117600****************************************************************          
117700                                                                          
117800                                                                          
117900     CALL   'CNSFPFMT'         USING   CCFMDATA,                  C414004L
118000                                       COMMON-INTERNAL-DATA,      C414004L
118100                                       WS-PROMPT-ID-TABLE,                
124519                                       PCB-NM-PRV,                        
177017                                       WS-OCL-CODE.                       
118200 3200-EXIT.                                                               
118300     EXIT.                                                                
118400                                                                          
118500                                                                          
118600 3300-BUILD-PROF-CLAIM-LINE  SECTION.                                     
118700                                                                          
118800****************************************************************          
118900*                                                                         
119000*     CALL THE CNSFPLIN PROGRAM AND BUILD THE PROFESSIONAL                
119100*     LINE  INFORMATION                                                   
119200*                                                                         
119300****************************************************************          
119400                                                                          
119500                                                                          
119600     CALL   'CNSFPLIN'         USING   CCFMDATA,                  C414004L
119700                                       COMMON-INTERNAL-DATA,      C414004L
119800                                       WS-PROMPT-ID-TABLE,                
177017                                       WS-OCL-CODE.                       
119900                                                                          
120000 3300-EXIT.                                                               
120100     EXIT.                                                                
120200     EJECT                                                                
120300 4000-PROCESS-RESUBMITS         SECTION.                                  
120400                                                                          
120500****************************************************************          
120600*                                                                         
120700*     MOVE THE RESUBMIT RECORD TO THE TABLE                               
120800*     COME HERE FOR A RESUBMIT (2) AND CLAIM HISTORY FOUND        CL393611
120810*     IF CLM STATUS IS SUSPEND (2) = RLI, ALL OTHERS = ADO        CL393611
120820*                                                                 CL393611
120900****************************************************************          
121000                                                                          
121100     MOVE 1                        TO   KW-INDEX                          
121200                                                                          
121300     MOVE WS-CLAIM-STATUS-COMP     TO   WS-NUMERIC-STATUS-CD              
121400*                                                                 CL393611
121500*                                                                 CL393611
121600                                                                          
121700     IF  WS-NUMERIC-STATUS-CD      =    2                                 
121800         MOVE WS-CLRLI             TO   WS-FILLER (KW-INDEX)              
121900         MOVE WS-OPT               TO   WS-PROMPT-ID (KW-INDEX)           
122000         MOVE WS-RLI               TO   WS-PROMPT-DATA (KW-INDEX)         
122100     ELSE                                                                 
122200         MOVE WS-CLADO             TO   WS-FILLER (KW-INDEX)              
122300         MOVE WS-OPT               TO   WS-PROMPT-ID (KW-INDEX)           
122400         MOVE WS-ADO               TO   WS-PROMPT-DATA (KW-INDEX).        
122500                                                                          
122600     ADD  1                        TO   KW-INDEX                          
122700     MOVE SPACES                   TO   WS-FILLER (KW-INDEX)              
122800     MOVE WS-DN                    TO   WS-PROMPT-ID (KW-INDEX)           
122900     MOVE WS-DEPENDENT-NO          TO   WS-PROMPT-DATA (KW-INDEX)         
123000     ADD  1                        TO   KW-INDEX.                         
123100                                                                          
123200 4000-EXIT.                                                               
123300     EXIT.                                                                
123400     EJECT                                                        CL395543
123500 4100-WRITE-TO-SF-RECYCLE-DB.                                     CL395543
123510                                                                  CL395543
123511     IF FM105-REC-TYPE     = '05' AND                             CL395543
123512        FM105-TYPE-SUBM         = '3'                             CL395543
123513          MOVE 'I' TO ITS-INST-PROF-SWITCH                        CL395543
123514          MOVE  FM105-SCCF-SER-NUM      TO WS-SFSER               CL395543
                PERFORM 4110-HASH-INST-NET-LIAB-AMT                     EB6651  
                   THRU 4110-EXIT                                       EB6651  
123515     ELSE                                                         CL395543
123520     IF FM1A5-REC-TYPE     = 'A5' AND                             CL395543
123521        FM1A5-TYPE-SUBM         = '3'                             CL395543
123522          MOVE 'P' TO ITS-INST-PROF-SWITCH                        CL395543
123523          MOVE  FM1A5-SCCF-SER-NUM      TO WS-SFSER               CL395543
                PERFORM 4120-HASH-PROF-NET-LIAB-AMT                     EB6651  
                   THRU 4120-EXIT.                                      EB6651  
123525                                                                  CL395543
123530* WRITE SF RECORDS TO THE ITR DATA BASE                           CL395543
123531* FIRST READ THE ROOT SEGMENT                                     CL395543
123532                                                                  CL395543
123540     ADD 1                    TO AC-ADJUST-CNT.                   CL395543
123550     PERFORM  9520-READ-ITR-ROOT                                  CL395543
123560        THRU  9520-EXIT.                                          CL395543
123570                                                                  CL395543
123580*        IF ROOT SEGMENT NOT FOUND - INSERT ROOT & SF DATA        CL395543
123590*        IF ROOT SEGMENT IS FOUND - UPDATE ROOT & UPDATE          CL395543
123600*        SF DATA IF IT EXISTS, ELSE INSERT SF DATA                CL395543
123610                                                                  CL395543
123620     IF  PCB-DLI-RET-CD   =   '  '                                CL395543
123630         PERFORM  9620-UPDATE-ITR-ROOT                            CL395543
123640            THRU  9620-EXIT                                       CL395543
123650         PERFORM  9530-READ-ITR-SF-DATA                           CL395543
123660            THRU  9530-EXIT                                       CL395543
123670         IF  PCB-DLI-RET-CD   =   '  '                            CL395543
123680             PERFORM  9630-UPDATE-ITR-SF-DATA                     CL395543
123690                THRU  9630-EXIT                                   CL395543
123700         ELSE                                                     CL395543
123710             PERFORM  9730-ADD-ITR-SF-DATA                        CL395543
123720                THRU  9730-EXIT                                   CL395543
123730     ELSE                                                         CL395543
123740         PERFORM  9720-ADD-ITR-ROOT                               CL395543
123750            THRU  9720-EXIT                                       CL395543
123760         PERFORM  9730-ADD-ITR-SF-DATA                            CL395543
123780            THRU  9730-EXIT.                                      CL395543
123790                                                                  CL395543
123800*                                                                         
123900*                                                                 CL393611
124000*                                                                         
124100*                                                                 CL393611
124200*                                                                 CL393611
124300*                                                                 CL393611
124400*                                                                 CL393611
124500*                                                                 CL393611
124600*                                                                 CL393611
124700*                                                                 CL393611
124800*                                                                 CL393611
124900*                                                                 CL393611
125000*                                                                 CL393611
125100*                                                                 CL393611
125200*                                                                 CL393611
125300*                                                                 CL393611
125400*                                                                 CL393611
125500*                                                                 CL393611
125600*                                                                 CL393611
125700*                                                                 CL393611
125800*                                                                 CL393611
125900*                                                                 CL393611
126000 4100-EXIT.                                                       CL395543
126100     EXIT.                                                        CL395543
126200     EJECT                                                                
       4110-HASH-INST-NET-LIAB-AMT.                                     EB6651  
                                                                        EB6651  
           IF VTG-50 IS GREATER THAN ZEROS                              EB6651  
              PERFORM WITH TEST AFTER                                   EB6651  
                      VARYING W-4110-SUB FROM 1 BY 1                    EB6651  
                      UNTIL   W-4110-SUB IS GREATER THAN VTG-50         EB6651  
                 PERFORM WITH TEST AFTER                                EB6651  
                         VARYING W-4110-SUB-2 FROM 1 BY 1               EB6651  
ITS102*                  UNTIL   W-4110-SUB-2 IS GREATER THAN 23        EB6651  
ITS102                   UNTIL   W-4110-SUB-2 IS GREATER THAN 999       EB6651  
                          IF  FM160-SERV-CHRG                           EB6651  
                              (            W-4110-SUB-2) IS NUMERIC     EB6651  
                              ADD FM160-SERV-CHRG                       EB6651  
                                  (            W-4110-SUB-2)            EB6651  
                               TO AC-ADO-AMT                            EB6651  
                          END-IF                                        EB6651  
                 END-PERFORM                                            EB6651  
              END-PERFORM                                               EB6651  
           END-IF.                                                      EB6651  
                                                                        EB6651  
           IF VTG-60 IS GREATER THAN ZEROS                              EB6651  
              PERFORM WITH TEST AFTER                                   EB6651  
                      VARYING W-4110-SUB FROM 1 BY 1                    EB6651  
                      UNTIL   W-4110-SUB IS GREATER THAN VTG-60         EB6651  
                 PERFORM WITH TEST AFTER                                EB6651  
                         VARYING W-4110-SUB-2 FROM 1 BY 1               EB6651  
                         UNTIL   W-4110-SUB-2 IS GREATER THAN 5         EB6651  
                          IF  FM160-SERV-CHRG                           EB6651  
                              (W-4110-SUB              ) IS NUMERIC     EB6651  
                              ADD FM160-SERV-CHRG                       EB6651  
                                  (W-4110-SUB              )            EB6651  
                               TO AC-ADO-AMT                            EB6651  
                          END-IF                                        EB6651  
                 END-PERFORM                                            EB6651  
              END-PERFORM                                               EB6651  
           END-IF.                                                      EB6651  
                                                                        EB6651  
       4110-EXIT.                                                       EB6651  
           EXIT.                                                        EB6651  
           EJECT                                                        EB6651  
       4120-HASH-PROF-NET-LIAB-AMT.                                     EB6651  
                                                                        EB6651  
           IF VTG-F0 IS GREATER THAN ZEROS                              EB6651  
              PERFORM WITH TEST AFTER                                   EB6651  
                      VARYING W-4120-SUB FROM 1 BY 1                    EB6651  
                      UNTIL   W-4120-SUB IS GREATER THAN VTG-F0         EB6651  
                       IF  FM1F0-SERV-CHRG    (W-4120-SUB) IS NUMERIC   EB6651  
                           ADD FM1F0-SERV-CHRG    (W-4120-SUB)          EB6651  
                            TO AC-ADO-AMT                               EB6651  
                       END-IF                                           EB6651  
              END-PERFORM                                               EB6651  
           END-IF.                                                      EB6651  
                                                                        EB6651  
       4120-EXIT.                                                       EB6651  
126100     EXIT.                                                        CL395543
126200     EJECT                                                                
126300 4200-PROCESS-ORIGINALS    SECTION.                                       
126400                                                                          
126500****************************************************************          
126600*                                                                         
126700*     MOVE THE NEW CLAIM INFORMATION TO THE TABLE                         
126800*                                                                         
126900****************************************************************          
127000                                                                          
127100     MOVE 1                        TO   KW-INDEX                          
127200     MOVE WS-CLCSF                 TO   WS-FILLER (KW-INDEX)              
127300     MOVE WS-OPT                   TO   WS-PROMPT-ID (KW-INDEX)           
127400     MOVE WS-CFS                   TO   WS-PROMPT-DATA (KW-INDEX).        
127500     ADD  1                        TO   KW-INDEX.                         
127600                                                                          
127700 4200-EXIT.                                                               
127800     EXIT.                                                                
127900     EJECT                                                                
128000 4300-PROCESS-TIP-DATA-BASE  SECTION.                                     
128100                                                                          
128200****************************************************************          
128300*                                                                         
128400*     READ THE TIP DATA BASE AND MOVE THE PROPER INFORMATION              
128500*     TO THE TABLE                                                        
128600*     CLAIM WASNT ON CLM HIST - TRY TIP.  IF ON TIP, SET UP AS    CL393611
128610*     A CLCSF REENTRY.  IF NOT ON TIP - ERROR (BRQ = J)           CL393611
128620*                                                                 CL393611
128700****************************************************************          
128800                                                                          
128900     PERFORM 9200-FIND-TIP-RCD                                            
129000       THRU  9200-EXIT.                                                   
129100                                                                          
129200     IF    WS-RESUBMISSION-IND     =    '2'                               
129300         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'                      
129400             MOVE 'J'              TO   CID-BRQ-STATUS-CODE               
129500         ELSE                                                             
129600             MOVE 'Y'              TO   CID-RE-ENTRY-CODE.                
129700                                                                          
129800     IF    WS-RESUBMISSION-IND      =    '3'                      CL395794
129900                                                                  CL395794
130000         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'              CL395794
130100             MOVE 'M'              TO   CID-BRQ-STATUS-CODE       CL395794
130200         ELSE                                                     CL395794
130300             MOVE 'L'              TO   CID-BRQ-STATUS-CODE.      CL395794
130400     MOVE 1                        TO   KW-INDEX                          
130500     MOVE WS-CLCSF                 TO   WS-FILLER (KW-INDEX)              
130600     MOVE WS-OPT                   TO   WS-PROMPT-ID (KW-INDEX)           
130700     MOVE WS-CFS                   TO   WS-PROMPT-DATA (KW-INDEX)         
130800                                                                          
130900     ADD  1                        TO   KW-INDEX                          
131000     MOVE SPACES                   TO   WS-FILLER (KW-INDEX)              
131100     MOVE WS-RKEY                  TO   WS-PROMPT-ID (KW-INDEX)           
131200     MOVE CID-CLAIM-ID             TO   WS-PROMPT-DATA (KW-INDEX).        
131300     ADD  1                        TO   KW-INDEX.                         
131400                                                                          
131500 4300-EXIT.                                                               
131600     EXIT.                                                                
131700      EJECT                                                               
131710 4310-VERIFY-LINE-ITEM-COUNT.                                     CL395801
131720*---------------------------------------------------------------  CL395801
131730*     WHEN BUILDING AN ADO OR RLI AND THE LINE COUNT IN THE       CL395801
131740*     TRANSACTION IS LESS THAN THE LINE COUNT ON CLAIMS           CL395801
131750*     HISTORY,  THEN SET THE BRQ CODE R                           CL395801
131760*---------------------------------------------------------------  CL395801
131770     IF      WS-FILLER(1)  EQUAL WS-CLADO OR                      CL395801
131780             WS-FILLER(1)  EQUAL WS-CLRLI                         CL395801
131790       PERFORM 4320-VERIFY-LINE-COUNT THRU 4320-EXIT              CL395801
131791               WITH TEST AFTER                                    CL395801
131792               VARYING KW-INDEX  FROM  1 BY   1                   CL395801
131793               UNTIL WS-PROMPT-ID(KW-INDEX) EQUAL WS-LICT         CL395801
131794     END-IF.                                                      CL395801
131795 4310-EXIT.                                                       CL395801
131796     EXIT.                                                        CL395801
131797     EJECT                                                        CL395801
131798 4320-VERIFY-LINE-COUNT.                                          CL395801
131799       IF  WS-PROMPT-ID(KW-INDEX) EQUAL WS-LICT                   CL395801
131800           MOVE WS-OPT-TYPE(KW-INDEX) TO TXN-LINE-CNT             CL395801
131801           MOVE TXN-LINE-CNT TO  WS-LINE-ITEM-COUNT               CL395801
131802***********IF WS-NBR-OF-LINE-ITEMS  NOT EQUAL CC-LIN-ITEM-CNT     CX397302
131803           IF WS-NBR-OF-LINE-ITEMS  <                             CX397302
131804                    CC-LIN-ITEM-CNT OF HIST-VSCPCLM               CX397302
131804                 MOVE  'R'        TO   CID-BRQ-STATUS-CODE        CL395801
131805           END-IF                                                 CL395801
131806       END-IF.                                                    CL395801
131807 4320-EXIT.                                                       CL395801
131808     EXIT.                                                        CL395801
131809 4400-CHANGE-BRQ-CODE.                                            CL395801
131810*     WHEN BUILDING AN ADO OR RLI AND THE LINE COUNT IN THE       CL395801
131811*     TRANSACTION IS LESS THAN THE LINE COUNT ON CLAIMS           CL395801
131812*     HISTORY,  THEN SET THE BRQ CODE R                           CL395801
131813*---------------------------------------------------------------  CL395801
131814     IF  CID-BRQ-STATUS-CODE EQUAL 'R'                            CL395801
131815       PERFORM 4410-FIND-BATCH-BRQ-CODE THRU 4410-EXIT            CL395801
131816               WITH TEST AFTER                                    CL395801
131817               VARYING KW-INDEX  FROM  1 BY   1                   CL395801
131818               UNTIL WS-PROMPT-ID(KW-INDEX) EQUAL WS-BRQ          CL395801
131819     END-IF.                                                      CL395801
131820 4400-EXIT.                                                       CL395801
131821     EXIT.                                                        CL395801
131822 4410-FIND-BATCH-BRQ-CODE.                                        CL395801
131823       IF  WS-PROMPT-ID(KW-INDEX) EQUAL WS-BRQ AND                CL395801
131824           WS-PROMPT-DATA (KW-INDEX) NOT EQUAL 'Q'                CL395801
131825         MOVE CID-BRQ-STATUS-CODE TO  WS-PROMPT-DATA (KW-INDEX)   CL395801
131826       END-IF.                                                    CL395801
131827 4410-EXIT.                                                       CL395801
131828     EXIT.                                                        CL395801
131829     EJECT                                                        CL395801
131800 5000-SUB-ID-COMP.                                                        
131900     MOVE 12                    TO SUB-COMP.                              
132000     MOVE 12                    TO SUB-SUB-ID.                            
132100     MOVE ZERO                  TO WS-SUB-ID-4-15-COMP.                   
132200     PERFORM 5100-COMPRESS                                                
132300        THRU 5100-EXIT                                                    
132400        UNTIL SUB-SUB-ID    EQUAL ZERO.                                   
132500 5000-EXIT.                                                               
132600     EXIT.                                                                
132700                                                                          
132800 5100-COMPRESS.                                                           
132900     IF WS-SUB-ID-4-15-OCCUR(SUB-SUB-ID) EQUAL SPACES                     
133000        SUBTRACT 1          FROM SUB-SUB-ID                               
133100     ELSE                                                                 
133200        MOVE WS-SUB-ID-4-15-OCCUR(SUB-SUB-ID)                             
133300                              TO WS-SUB-ID-COMP(SUB-COMP)                 
133400        SUBTRACT 1          FROM SUB-SUB-ID                               
133500        SUBTRACT 1          FROM SUB-COMP.                                
133600 5100-EXIT.                                                               
133700     EXIT.                                                                
133800      EJECT                                                               
133900 9000-READ-SCSE-FILE    SECTION.                                          
134000                                                                          
134100****************************************************************          
134200*                                                                         
134300*     READ THE INPUT FILE AND MOVE THE RECORD TO THEIR                    
134400*     PROPER LOCATION IN THE INPUT TABLE                                  
134500*                                                                         
134600****************************************************************          
134700                                                                          
134800     IF  SCSE-RCD-TYPE            =    '05'                               
134900         MOVE  SCSE-INPUT-REC     TO   FM105-RECORD                       
134910         MOVE  1                  TO   CQ-TRN-CNT                 CL393611
135000         ADD   1                  TO   VTG-VALID-GROUP-CNT                
135002       IF FM105-TYPE-SUBM       = '3'                             CL395543
135004          MOVE 'Y'                TO   ADJUST-SWITCH              CL395543
135006       ELSE                                                       CL395543
135007          NEXT SENTENCE                                           CL395543
135008       END-IF                                                             
135010     ELSE                                                         CL393611
135020         IF  SCSE-RCD-TYPE        =    'A5'                       CL393611
135030             MOVE  SCSE-INPUT-REC TO   FM1A5-RECORD               CL393611
135040             MOVE  1              TO   CQ-TRN-CNT                 CL393611
135050             ADD   1              TO   VTG-VALID-GROUP-CNT        CL393611
135060           IF FM1A5-TYPE-SUBM         = '3'                       CL395543
135070              MOVE 'Y'           TO   ADJUST-SWITCH               CL395543
135080           ELSE                                                   CL395543
135081              NEXT SENTENCE                                       CL395543
135082           END-IF                                                         
135100         ELSE                                                     CL393611
ITS101*            IF  SCSE-RCD-TYPE        =    ' 7'                   CL393611
ITS101*            AND SCSE-SEQ-NUM         =    '1A'                   CL393611
ITS101             IF  SCSE-RCD-TYPE        =    '1A'                   CL393611
ITS101             AND SCSE-SEQ-NUM         >    0                      CL393611
135300                 MOVE  SCSE-INPUT-REC TO   FM71A-RECORD           CL393611
135400                 ADD   1              TO   AC-MCF-CNT              EBCBS  
135401             ELSE                                                  EBCBS  
135402               IF  SCSE-RCD-TYPE        =    '9D'                  EBCBS  
135404                   MOVE  SCSE-INPUT-REC                            EBCBS  
135405                                      TO   ITCDB99D-ITS-TRAILER    EBCBS  
135406                   MOVE 'YES'         TO   EMC-TRAILER-REC-SW      EBCBS  
135407               END-IF                                              EBCBS  
135408             END-IF                                                EBCBS  
135409         END-IF                                                    EBCBS  
135410     END-IF.                                                       EBCBS  
135404*                                                                         
135500                                                                          
135600     READ  SCSE-INPUT-FILE                                                
135700       AT END                                                             
135800         MOVE  'Y'                TO   END-OF-FILE-SWITCH                 
135900         GO TO 9000-EXIT.                                                 
135901*                                                                  EBCBS  
135902     ADD 1                        TO   AC-RECD-CNT.                EBCBS  
136000                                                                          
136100     IF  SCSE-RCD-TYPE            =    '05' OR 'A5' OR            CL393611
ITS101*     (SCSE-RCD-TYPE = ' 7'  AND  SCSE-SEQ-NUM = '1A')            CL393611
ITS101      (SCSE-RCD-TYPE = '1A'  AND  SCSE-SEQ-NUM > 0   )            CL393611
136200         MOVE 'Y'                 TO   GROUP-SWITCH                       
136300         GO TO 9000-EXIT.                                                 
136400                                                                          
136500     IF  SCSE-RCD-TYPE            =    '10'                               
136600         MOVE  SCSE-INPUT-REC     TO   FM110-RECORD                       
136610         ADD   1                  TO   CQ-TRN-CNT                 CL393611
136700         ADD   1                  TO   VTG-VALID-GROUP-CNT                
136800         GO TO 9000-EXIT.                                                 
136900                                                                          
137000     IF  SCSE-RCD-TYPE            =    '15'                               
137100         MOVE  SCSE-INPUT-REC     TO   FM115-RECORD                       
137110         ADD   1                  TO   CQ-TRN-CNT                 CL393611
137200         GO TO 9000-EXIT.                                                 
137300                                                                          
137400     IF  SCSE-RCD-TYPE            =    '20'                               
137500         MOVE  SCSE-INPUT-REC     TO   FM120-RECORD                       
137600         ADD   1                  TO   VTG-VALID-GROUP-CNT                
137610         ADD   1                  TO   CQ-TRN-CNT                 CL393611
137700         GO TO 9000-EXIT.                                                 
137800                                                                          
137900     IF  SCSE-RCD-TYPE            =    '30'                               
138000*    AND SCSE-SEQ-NUM             =    '01'                               
ITS101     AND SCSE-SEQ-NUM             =     01                                
138100         ADD   1                  TO   VTG-30                             
138200         MOVE  SCSE-INPUT-REC     TO   FM130-RECORD(VTG-30)               
138300         ADD   1                  TO   VTG-VALID-GROUP-CNT                
138310         ADD   1                  TO   CQ-TRN-CNT                 CL393611
138400         GO TO 9000-EXIT                                                  
138500     ELSE                                                                 
138600        IF  SCSE-RCD-TYPE            =    '30'                            
138700         ADD   1                  TO   VTG-30                             
138800         MOVE  SCSE-INPUT-REC     TO   FM130-RECORD(VTG-30)               
138900         GO TO 9000-EXIT.                                                 
139000                                                                          
139100     IF  SCSE-RCD-TYPE            =    '31'                               
139200         ADD   1                  TO   VTG-31                             
139300         MOVE  SCSE-INPUT-REC     TO   FM131-RECORD (VTG-31)              
139310         ADD   1                  TO   CQ-TRN-CNT                 CL393611
139400         GO TO 9000-EXIT.                                                 
139500                                                                          
           IF  SCSE-RCD-TYPE            =    '32'                       C414004L
               MOVE  SCSE-INPUT-REC     TO   FM132-RECORD               C414004L
139310         ADD   1                  TO   CQ-TRN-CNT                 CL393611
139400         GO TO 9000-EXIT.                                                 
139500                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '33'                       C414004L
ITS120         MOVE  SCSE-INPUT-REC     TO   FM133-RECORD               C414004L
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
140000     IF  SCSE-RCD-TYPE            =    '40'                               
140100         MOVE  SCSE-INPUT-REC     TO   FM140-RECORD                       
140200*        ADD   1                  TO   VTG-VALID-GROUP-CNT                
140210         ADD   1                  TO   CQ-TRN-CNT                 CL393611
140300         GO TO 9000-EXIT.                                                 
140400                                                                          
140500     IF  SCSE-RCD-TYPE            =    '41'                               
140600         MOVE  SCSE-INPUT-REC     TO   FM141-RECORD                       
140610         ADD   1                  TO   CQ-TRN-CNT                 CL393611
140700         GO TO 9000-EXIT.                                                 
140800                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '42'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM142-RECORD                       
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '43'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM143-RECORD                       
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '44'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM144-RECORD                       
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '45'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM145-RECORD                       
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
140900     IF  SCSE-RCD-TYPE            =    '46'                               
141000         ADD   1                  TO   VTG-46                             
141100         MOVE  SCSE-INPUT-REC     TO   FM146-RECORD (VTG-46)              
141110         ADD   1                  TO   CQ-TRN-CNT                 CL393611
141200         GO TO 9000-EXIT.                                                 
141300                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '47'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM147-RECORD                       
141110         ADD   1                  TO   CQ-TRN-CNT                 CL393611
141200         GO TO 9000-EXIT.                                                 
141300                                                                          
141400     IF  SCSE-RCD-TYPE            =    '50'                               
142100             MOVE SCSE-INPUT-REC  TO   FM150-RECORD                       
142110             ADD   1              TO   VTG-50                     CL393611
142110             ADD   1              TO   CQ-TRN-CNT                 CL393611
143400             GO TO 9000-EXIT.                                             
143500                                                                          
143600     IF  SCSE-RCD-TYPE            =    '60'                               
143700         MOVE SCSE-SEQ-NUM        TO   VTG-60                             
ITS102*        IF  VTG-60               >    23                                 
ITS102         IF  VTG-60               >    999                                
143900             MOVE 'Y'             TO   ERROR-SWITCH                       
144000             GO TO 9000-EXIT                                              
144100         ELSE                                                             
144200*                                                                 CL393611
144300             MOVE SCSE-INPUT-REC  TO   FM160-RECORD (VTG-60)              
144310             ADD   1              TO   CQ-TRN-CNT                 CL393611
ITS101*            IF SCSE-SEQ-NUM        =    '01'                             
ITS101             IF SCSE-SEQ-NUM        =     01                              
144321               ADD   1            TO   VTG-VALID-GROUP-CNT                
144322             END-IF                                                       
144400*                                                                 CL393611
144500             GO TO 9000-EXIT.                                             
144510                                                                  CL393611
144700     IF  SCSE-RCD-TYPE            =    '65'                               
144800         MOVE SCSE-SEQ-NUM        TO   VTG-65                             
ITS102*        IF  VTG-65               >    23                                 
ITS102         IF  VTG-65               >    999                                
145000             MOVE 'Y'             TO   ERROR-SWITCH                       
145100             GO TO 9000-EXIT                                              
145200         ELSE                                                             
145300*                                                                 CL393611
145400             MOVE SCSE-INPUT-REC  TO   FM165-RECORD (VTG-65)              
145410             ADD   1              TO   CQ-TRN-CNT                 CL393611
145500*                                                                 CL393611
145600             GO TO 9000-EXIT.                                             
145700                                                                          
           IF  SCSE-RCD-TYPE            =    '66'                       C414004L
               MOVE SCSE-SEQ-NUM        TO   VTG-66                     C414004L
C14194*        IF  VTG-66               >    23                         C414004L
C14194         IF  VTG-66               >    999                        C414004L
                   MOVE 'Y'             TO   ERROR-SWITCH               C414004L
                   GO TO 9000-EXIT                                      C414004L
               ELSE                                                     C414004L
                   MOVE SCSE-INPUT-REC  TO   FM166-RECORD (VTG-66)      C414004L
                   ADD   1              TO   CQ-TRN-CNT                 C414004L
                   GO TO 9000-EXIT.                                     C414004L
145700                                                                          
145800     IF  SCSE-RCD-TYPE            =    '70'                               
145900         MOVE  SCSE-INPUT-REC     TO   FM170-RECORD                       
146000         ADD   1                  TO   VTG-VALID-GROUP-CNT                
146010         ADD   1                  TO   CQ-TRN-CNT                 CL393611
C97522         MOVE 'Y'                 TO   FM170-SWITCH                       
551903         SET REC-70-PRESENT       TO TRUE                                 
146100         GO TO 9000-EXIT.                                                 
146200                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '71'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM171-RECORD                       
ITS120*        ADD   1                  TO   VTG-VALID-GROUP-CNT                
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
551903         IF REC-70-NOT-PRESENT                                            
551903            ADD   1                  TO   VTG-VALID-GROUP-CNT             
551903         END-IF                                                           
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '72'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM172-RECORD                       
ITS120*        ADD   1                  TO   VTG-VALID-GROUP-CNT                
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '73'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM173-RECORD                       
ITS120*        ADD   1                  TO   VTG-VALID-GROUP-CNT                
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    '74'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM174-RECORD                       
ITS120*        ADD   1                  TO   VTG-VALID-GROUP-CNT                
ITS120         ADD   1                  TO   CQ-TRN-CNT                 CL393611
ITS120         GO TO 9000-EXIT.                                                 
ITS120                                                                          
146300     IF  SCSE-RCD-TYPE            =    '80'                               
146400         MOVE  SCSE-INPUT-REC     TO   FM180-RECORD                       
146510         ADD   1                  TO   CQ-TRN-CNT                 CL393611
146600         GO TO 9000-EXIT.                                                 
146700                                                                          
146800     IF  SCSE-RCD-TYPE            =    '90'                               
146900         MOVE  SCSE-INPUT-REC     TO   FM190-RECORD                       
147000         ADD   1                  TO   VTG-VALID-GROUP-CNT                
147010         ADD   1                  TO   CQ-TRN-CNT                 CL393611
147100         GO TO 9000-EXIT.                                                 
147200                                                                          
147300     IF  SCSE-RCD-TYPE            =    'B0'                               
147400         MOVE  SCSE-INPUT-REC     TO   FM1B0-RECORD                       
147500         ADD   1                  TO   VTG-VALID-GROUP-CNT                
147510         ADD   1                  TO   CQ-TRN-CNT                 CL393611
147600         GO TO 9000-EXIT.                                                 
147700                                                                          
147800     IF  SCSE-RCD-TYPE            =    'B5'                               
147900         MOVE  SCSE-INPUT-REC     TO   FM1B5-RECORD                       
147910         ADD   1                  TO   CQ-TRN-CNT                 CL393611
148000         GO TO 9000-EXIT.                                                 
148100                                                                          
148200     IF  SCSE-RCD-TYPE            =    'C0'                               
148300         MOVE  SCSE-INPUT-REC     TO   FM1C0-RECORD                       
148400         ADD   1                  TO   VTG-VALID-GROUP-CNT                
148410         ADD   1                  TO   CQ-TRN-CNT                 CL393611
148500         GO TO 9000-EXIT.                                                 
148600                                                                          
148700     IF  SCSE-RCD-TYPE            =    'D0'                               
148800         MOVE  SCSE-INPUT-REC     TO   FM1D0-RECORD                       
148900         ADD   1                  TO   VTG-VALID-GROUP-CNT                
148910         ADD   1                  TO   CQ-TRN-CNT                 CL393611
149000         GO TO 9000-EXIT.                                                 
149100                                                                          
149200     IF  SCSE-RCD-TYPE            =    'D1'                               
149300         ADD   1                  TO   VTG-D1                             
149400         MOVE  SCSE-INPUT-REC     TO   FM1D1-RECORD (VTG-D1)              
149810         ADD   1                  TO   CQ-TRN-CNT                 CL393611
149900         GO TO 9000-EXIT.                                                 
150000                                                                          
           IF  SCSE-RCD-TYPE            =    'D2'                       C414004L
               MOVE  SCSE-INPUT-REC     TO   FM1D2-RECORD               C414004L
               ADD   1                  TO   CQ-TRN-CNT                 C414004L
               GO TO 9000-EXIT.                                         C414004L
150000                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    'D3'                       C414004L
ITS120         MOVE  SCSE-INPUT-REC     TO   FM1D3-RECORD               C414004L
ITS120         ADD   1                  TO   CQ-TRN-CNT                 C414004L
ITS120         GO TO 9000-EXIT.                                         C414004L
ITS120                                                                          
150100     IF  SCSE-RCD-TYPE            =    'E0'                               
150200         MOVE  SCSE-INPUT-REC     TO   FM1E0-RECORD                       
150300         ADD   1                  TO   VTG-VALID-GROUP-CNT                
150310         ADD   1                  TO   CQ-TRN-CNT                 CL393611
150400         GO TO 9000-EXIT.                                                 
150500                                                                          
150600     IF  SCSE-RCD-TYPE            =    'E1'                               
150700         MOVE  SCSE-INPUT-REC     TO   FM1E1-RECORD                       
150710         ADD   1                  TO   CQ-TRN-CNT                 CL393611
150800         GO TO 9000-EXIT.                                                 
150900                                                                          
ITS120     IF  SCSE-RCD-TYPE            =    'E2'                               
ITS120         MOVE  SCSE-INPUT-REC     TO   FM1E2-RECORD                       
150710         ADD   1                  TO   CQ-TRN-CNT                 CL393611
150800         GO TO 9000-EXIT.                                                 
150900                                                                          
151000     IF  SCSE-RCD-TYPE            =    'E6'                               
151100         ADD   1                  TO   VTG-E6                             
151200         MOVE  SCSE-INPUT-REC     TO   FM1E6-RECORD (VTG-E6)              
151210         ADD   1                  TO   CQ-TRN-CNT                 CL393611
151300         GO TO 9000-EXIT.                                                 
151400                                                                          
151500     IF  SCSE-RCD-TYPE            =    'F0'                               
151600         MOVE SCSE-SEQ-NUM        TO   VTG-F0                             
ITS102*        IF  VTG-F0               >    23                                 
ITS102         IF  VTG-F0               >    50                                 
151800             MOVE 'Y'             TO   ERROR-SWITCH                       
151900             GO TO 9000-EXIT                                              
152000         ELSE                                                             
152100*                                                                 CL393611
152200             MOVE SCSE-INPUT-REC  TO   FM1F0-RECORD (VTG-F0)              
152210             ADD   1              TO   CQ-TRN-CNT                 CL393611
152300*                                                                 CL393611
152400             GO TO 9000-EXIT.                                             
152500                                                                          
152600     IF  SCSE-RCD-TYPE            =    'F5'                               
152700         MOVE SCSE-SEQ-NUM        TO   VTG-F5                             
ITS102*        IF  VTG-F5               >    23                                 
ITS102         IF  VTG-F5               >    50                                 
152900             MOVE 'Y'             TO   ERROR-SWITCH                       
153000             GO TO 9000-EXIT                                              
153100         ELSE                                                             
153200*                                                                 CL393611
153210             ADD   1              TO   CQ-TRN-CNT                 CL393611
153300             MOVE SCSE-INPUT-REC  TO   FM1F5-RECORD (VTG-F5)              
153400*                                                                 CL393611
153500             GO TO 9000-EXIT.                                             
153600                                                                          
           IF  SCSE-RCD-TYPE            =    'F6'                       C414004L
               MOVE SCSE-SEQ-NUM        TO   VTG-F6                     C414004L
ITS102*        IF  VTG-F6               >    23                         C414004L
ITS102         IF  VTG-F6               >    50                         C414004L
                   MOVE 'Y'             TO   ERROR-SWITCH               C414004L
                   GO TO 9000-EXIT                                      C414004L
               ELSE                                                     C414004L
                                                                        C414004L
                   ADD   1              TO   CQ-TRN-CNT                 C414004L
                   MOVE SCSE-INPUT-REC  TO   FM1F6-RECORD (VTG-F6)      C414004L
                                                                        C414004L
                   GO TO 9000-EXIT.                                     C414004L
                                                                        C414004L
153700     IF  SCSE-RCD-TYPE            =    'G0'                               
153800         MOVE  SCSE-INPUT-REC     TO   FM1G0-RECORD                       
153810         ADD   1                  TO   CQ-TRN-CNT                 CL393611
153900         GO TO 9000-EXIT.                                                 
154000                                                                          
154100     IF  SCSE-RCD-TYPE            =    'X0'                               
154200         MOVE  SCSE-INPUT-REC     TO   FM1X0-RECORD                       
154210         ADD   1                  TO   VTG-VALID-GROUP-CNT        CL393611
154220         ADD   1                  TO   CQ-TRN-CNT                 CL393611
154300         GO TO 9000-EXIT.                                         CL393611
154310*                                            MCF RECORDS          CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '1A'                       CL393611
ITS101     IF  SCSE-RCD-TYPE            =    '1A'                       CL393611
154314         MOVE  SCSE-INPUT-REC     TO   FM71A-RECORD               CL393611
154316         GO TO 9000-EXIT.                                         CL393611
154318                                                                  CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '2A'                       CL393611
ITS101*    IF  SCSE-RCD-TYPE            =    '2A'                       CL393611
154322         MOVE  SCSE-INPUT-REC     TO   FM72A-RECORD               CL393611
154324         GO TO 9000-EXIT.                                         CL393611
154326                                                                  CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '3A'                       CL393611
IT101      IF  SCSE-RCD-TYPE            =    '3A'                       CL393611
154330         MOVE  SCSE-INPUT-REC     TO   FM73A-RECORD               CL393611
154332         GO TO 9000-EXIT.                                         CL393611
154334                                                                  CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '4A'                       CL393611
ITS101     IF  SCSE-RCD-TYPE            =    '4A'                       CL393611
154338         MOVE  SCSE-INPUT-REC     TO   FM74A-RECORD               CL393611
154340         GO TO 9000-EXIT.                                         CL393611
154342                                                                  CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '5A'                       CL393611
ITS101     IF  SCSE-RCD-TYPE            =    '5A'                       CL393611
154346         MOVE  SCSE-INPUT-REC     TO   FM75A-RECORD               CL393611
154348         GO TO 9000-EXIT.                                         CL393611
154350                                                                  CL393611
154352*    IF  SCSE-SEQ-NUM             =    '6A'                       CL393611
ITS101     IF  SCSE-RCD-TYPE            =    '6A'                       CL393611
154354         MOVE  SCSE-INPUT-REC     TO   FM76A-RECORD               CL393611
154356         GO TO 9000-EXIT.                                         CL393611
154358                                                                  CL393611
154360*    IF  SCSE-SEQ-NUM             =    '7A'                       CL393611
ITS101     IF  SCSE-RCD-TYPE            =    '7A'                       CL393611
154362         MOVE  SCSE-INPUT-REC     TO   FM77A-RECORD               CL393611
154364         GO TO 9000-EXIT.                                         CL393611
154366                                                                  CL393611
ITS101*    IF  SCSE-SEQ-NUM             =    '8A'                       CL393611
154368     IF  SCSE-RCD-TYPE            =    '8A'                       CL393611
154370         MOVE  SCSE-INPUT-REC     TO   FM78A-RECORD               CL393611
154372         GO TO 9000-EXIT.                                         CL393611
154374                                                                  CL393611
154376*    IF  SCSE-SEQ-NUM             =    '9A'                       CL393611
154376     IF  SCSE-RCD-TYPE            =    '9A'                       CL393611
154378         MOVE  SCSE-INPUT-REC     TO   FM79A-RECORD               CL393611
154380         GO TO 9000-EXIT.                                         CL393611
154400                                                                          
154500 9000-EXIT.                                                               
154600     EXIT.                                                                
154700     EJECT                                                                
154800 9100-FIND-CLAIM-HIST-RCD    SECTION.                                     
154900                                                                          
155000****************************************************************          
155100*                                                                         
155200*     READ THE CLAIM DATA BASE                                            
155300*                                                                         
155400****************************************************************          
155500                                                                          
155600     IF WS-SUB-ID-COMP (12)        =    '0'                       CL393611
155700         MOVE  PCB-NM-ES01         TO   PCB-NM-COMMON,                    
155701         MOVE  PCB-NM-ZS01         TO   PCB-NM-ZCOMMON                    
155800     ELSE                                                                 
155900     IF WS-SUB-ID-COMP (12)        =    '1'                       CL393611
156000         MOVE  PCB-NM-ES02         TO   PCB-NM-COMMON,                    
156001         MOVE  PCB-NM-ZS02         TO   PCB-NM-ZCOMMON                    
156100     ELSE                                                                 
156200     IF WS-SUB-ID-COMP (12)        =    '2'                       CL393611
156300         MOVE  PCB-NM-ES03         TO   PCB-NM-COMMON,                    
156301         MOVE  PCB-NM-ZS03         TO   PCB-NM-ZCOMMON                    
156400     ELSE                                                                 
156500     IF WS-SUB-ID-COMP (12)        =    '3'                       CL393611
156600         MOVE  PCB-NM-ES04         TO   PCB-NM-COMMON,                    
156601         MOVE  PCB-NM-ZS04         TO   PCB-NM-ZCOMMON                    
156700     ELSE                                                                 
156800     IF WS-SUB-ID-COMP (12)        =    '4'                       CL393611
156900         MOVE  PCB-NM-ES05         TO   PCB-NM-COMMON,                    
156901         MOVE  PCB-NM-ZS05         TO   PCB-NM-ZCOMMON                    
157000     ELSE                                                                 
157100     IF WS-SUB-ID-COMP (12)        =    '5'                       CL393611
157200         MOVE  PCB-NM-ES06         TO   PCB-NM-COMMON,                    
157201         MOVE  PCB-NM-ZS06         TO   PCB-NM-ZCOMMON                    
157300     ELSE                                                                 
157400     IF WS-SUB-ID-COMP (12)        =    '6'                       CL393611
157500         MOVE  PCB-NM-ES07         TO   PCB-NM-COMMON,                    
157501         MOVE  PCB-NM-ZS07         TO   PCB-NM-ZCOMMON                    
157600     ELSE                                                                 
157700     IF WS-SUB-ID-COMP (12)        =    '7'                       CL393611
157800         MOVE  PCB-NM-ES08         TO   PCB-NM-COMMON,                    
157801         MOVE  PCB-NM-ZS08         TO   PCB-NM-ZCOMMON                    
157900     ELSE                                                                 
158000     IF WS-SUB-ID-COMP (12)        =    '8'                       CL393611
158100         MOVE  PCB-NM-ES09         TO   PCB-NM-COMMON,                    
158101         MOVE  PCB-NM-ZS09         TO   PCB-NM-ZCOMMON                    
158200     ELSE                                                                 
158300         MOVE  PCB-NM-ES10         TO   PCB-NM-COMMON,                    
158301         MOVE  PCB-NM-ZS10         TO   PCB-NM-ZCOMMON.                   
158400                                                                          
158500     CALL  'LNDLICBL'           USING   PCB-NM-COMMON                     
158600                                        GU-CALL                           
158700                                        PCB-AREA                          
158800                                        VS-CLAIM-HIST-IO                  
158900                                        SSA-HIST-ROOT                     
159000                                        SSA-HIST-MEMBER                   
159100                                        SSA-HIST-CLAIM.                   
159200                                                                          
159200                                                                          
159300     IF PCB-DLI-RET-CD         = '  '                             CL395545
159301        PERFORM S101-HST-CLM-VRS                                          
159302*                                                                         
159305        MOVE SPACES    TO   PCB-DLI-CLM-LN-RET-CD                         
159310        PERFORM 9150-GET-LINE-SEGMENTS THRU 9150-EXIT             CL395545
159315        UNTIL PCB-DLI-CLM-LN-RET-CD  NOT = SPACES                 CL395545
159320     ELSE                                                         CL395545
159330     IF PCB-DLI-RET-CD         = 'GE' OR 'GB'                     CL395545
159400         NEXT SENTENCE                                                    
159500     ELSE                                                                 
159600         MOVE  9100                TO   KILL-MODULE-POSITION              
159700         CALL  'LXKILL'         USING   KILL-MODULE-NM                    
159800                                        KILL-MODULE-POSITION              
159900         CALL 'ZNCNCLDP'.                                                 
160000                                                                          
160001     IF PCB-DLI-RET-CD         = 'GE' OR 'GB'                     CL395545
160002       CALL  'LNDLICBL'         USING   PCB-NM-ZCOMMON,                   
160003                                        GU-CALL,                          
160004                                        PCB-AREA,                         
160005                                        VS-CLAIM-HIST-IO,                 
160006                                        SSA-HIST-ROOT,                    
160007                                        SSA-HIST-MEMBER,                  
160008                                        SSA-HIST-CLAIM                    
160009       IF PCB-DLI-RET-CD         = '  '                           CL395545
160010         PERFORM S101-HST-CLM-VRS                                         
160011*                                                                         
160012         MOVE SPACES                 TO PCB-DLI-CLM-LN-RET-CD             
160013         PERFORM 9151-GET-LINE-SEGMENTS                           CL395545
160014            THRU 9151-EXIT                                        CL395545
160015           UNTIL PCB-DLI-CLM-LN-RET-CD  NOT = SPACES              CL395545
160016       ELSE                                                       CL395545
160017         IF PCB-DLI-RET-CD         = 'GE' OR 'GB'                 CL395545
160018           NEXT SENTENCE                                                  
160019         ELSE                                                             
160020           MOVE  9100                TO KILL-MODULE-POSITION              
160021           CALL  'LXKILL'         USING KILL-MODULE-NM                    
160022                                        KILL-MODULE-POSITION              
160023           CALL 'ZNCNCLDP'                                                
160024         END-IF                                                           
160025       END-IF                                                             
160026     END-IF.                                                      CL395545
160027*                                                                 CL395545
160100 9100-EXIT.                                                               
160200     EXIT.                                                                
160300     EJECT                                                                
160305 9150-GET-LINE-SEGMENTS.                                          CL395545
160306                                                                  CL395545
160310**************************************************************    CL395545
160320*   GET CLAIM LINE SEGMENTS                                       CL395545
160330**************************************************************    CL395545
160340                                                                  CL395545
160350     CALL 'LNDLICBL'   USING  PCB-NM-COMMON                       CL395545
160352                              GNP-CALL                            CL395545
160353                              PCB-AREA-2                          CL395545
160354                              HIST-VSCPCLL                        CL395545
160356                              SSA-HIST-CLAIM-LINE-UNQ.            CL395545
160358                                                                  CL395545
160360     IF PCB-DLI-CLM-LN-RET-CD  = SPACES                           CL395545
160361        PERFORM S101-HST-CLL-VRS                                          
160362*                                                                         
160370        IF CE-SPLT-LN-ID OF HIST-VSCPCLL                          CL395545
160371                               = LOW-VALUES OR SPACES             CL395545
160380           NEXT SENTENCE                                          CL395545
160390        ELSE                                                      CL395545
160392           MOVE 'Q'        TO CID-BRQ-STATUS-CODE.                CL395545
160393                                                                          
160395 9150-EXIT.                                                       CL395545
160396     EXIT.                                                        CL395545
160397     EJECT                                                        CL395545
160398*                                                                 CL395545
160399 9151-GET-LINE-SEGMENTS.                                          CL395545
160400                                                                  CL395545
160401**************************************************************    CL395545
160402*   GET CLAIM LINE SEGMENTS FROM PURGED CLAIMS HISTORY            CL395545
160403**************************************************************    CL395545
160404                                                                  CL395545
160405     CALL 'LNDLICBL'     USING PCB-NM-ZCOMMON                     CL395545
160406                               GNP-CALL                           CL395545
160407                               PCB-AREA-2                         CL395545
160408                               HIST-VSCPCLL                       CL395545
160409                               SSA-HIST-CLAIM-LINE-UNQ.           CL395545
160410                                                                  CL395545
160411     IF PCB-DLI-CLM-LN-RET-CD  = SPACES                           CL395545
160412       PERFORM S101-HST-CLL-VRS                                           
160413*                                                                         
160414       IF CE-SPLT-LN-ID OF HIST-VSCPCLL                           CL395545
160415                              = LOW-VALUES OR SPACES              CL395545
160415         NEXT SENTENCE                                            CL395545
160416       ELSE                                                       CL395545
160417         MOVE 'Q'           TO CID-BRQ-STATUS-CODE                CL395545
160418       END-IF                                                     CL395545
160419     END-IF.                                                      CL395545
160420*                                                                 CL395545
160421 9151-EXIT.                                                       CL395545
160422     EXIT.                                                        CL395545
160423     EJECT                                                        CL395545
160422*                                                                 CL395545
160400 9200-FIND-TIP-RCD           SECTION.                                     
160500                                                                          
160600****************************************************************          
160700*                                                                         
160800*     READ THE TIP DATA BASE                                              
160900*                                                                         
161000****************************************************************          
161100                                                                          
161200     MOVE SPACES                TO  SSA-TIP-FILLER                        
161300     MOVE WS-NUMERIC-CLAIM-NBR  TO  SSA-TIP-CLAIM-NBR                     
161400                                                                          
161500                                                                          
161600*                                                                 CL393611
161700                                                                          
161800     CALL  'LNDLICBL'        USING  PCB-NM-ES11                           
161900                                    GU-CALL                               
162000                                    PCB-AREA                              
162100                                    VS-TIP-IO                             
162200                                    SSA-TIP-ROOT.                         
162300                                                                          
162400     IF   PCB-DLI-RET-CD        =    SPACES                               
162500         MOVE LE-USR-KEY-FLD    TO   WS-TIP-USER-KEY                      
162600     ELSE                                                                 
162700     IF   PCB-DLI-RET-CD        =    'GE' OR 'GB'                         
162800         MOVE SPACES            TO   WS-TIP-USER-KEY                      
162900     ELSE                                                                 
163000         MOVE  9200             TO   KILL-MODULE-POSITION                 
163100         CALL  'LXKILL'      USING   KILL-MODULE-NM                       
163200                                     KILL-MODULE-POSITION                 
163300         CALL 'ZNCNCLDP'.                                                 
163400                                                                          
163500     IF   SSA-HIST-SUBR-NO     NOT = WS-TIP-SUB-ID-1-12                   
163600******OR  SSA-HIST-SUBR-MDF    NOT = WS-TIP-SUB-MODF                      
163700         MOVE 'GE'              TO   PCB-DLI-RET-CD.                      
163800                                                                          
163900 9200-EXIT.                                                               
164000     EXIT.                                                                
164100     EJECT                                                                
164200 9300-WRITE-GROUP-IN-ERROR            SECTION.                            
164300                                                                          
164400****************************************************************          
164500*                                                                         
164600*     WRITE THE GROUP TO THE ERROR OUTPUT FILE                            
164700*                                                                         
164800****************************************************************          
164900                                                                          
165000     ADD   1                      TO   AC-ERROR-CNT                       
165100                                                                          
165200     IF  FM105-REC-TYPE           =    '05'                               
165300         WRITE ERROR-REC        FROM   FM105-RECORD.                      
165400                                                                          
165500     IF  FM110-REC-TYPE           =    '10'                               
165600         WRITE ERROR-REC        FROM   FM110-RECORD.                      
165700                                                                          
165800     IF  FM115-REC-TYPE           =    '15'                               
165900         WRITE ERROR-REC        FROM   FM115-RECORD.                      
166000                                                                          
166100     IF  FM120-REC-TYPE           =    '20'                               
166200         WRITE ERROR-REC        FROM   FM120-RECORD.                      
166300                                                                          
166400     IF  FM130-REC-TYPE(1)        =    '30'                               
166500         WRITE ERROR-REC        FROM   FM130-RECORD(1).                   
166600                                                                          
166700     IF  FM130-REC-TYPE(2)        =    '30'                               
166800         WRITE ERROR-REC        FROM   FM130-RECORD(2).                   
166900                                                                          
167000     IF  FM130-REC-TYPE(3)        =    '30'                               
167100         WRITE ERROR-REC        FROM   FM130-RECORD(3).                   
167200                                                                          
167300     IF  FM131-REC-TYPE(1)        =    '31'                               
167400         WRITE ERROR-REC        FROM   FM131-RECORD(1).                   
167500                                                                          
167600     IF  FM131-REC-TYPE(2)        =    '31'                               
167700         WRITE ERROR-REC        FROM   FM131-RECORD(2).                   
167800                                                                          
167900     IF  FM131-REC-TYPE(3)        =    '31'                               
168000         WRITE ERROR-REC        FROM   FM131-RECORD(3).                   
168100                                                                          
168500     IF  FM140-REC-TYPE           =    '40'                               
168600         WRITE ERROR-REC        FROM   FM140-RECORD.                      
168400                                                                          
168800     IF  FM141-REC-TYPE           =    '41'                               
168900         WRITE ERROR-REC        FROM   FM141-RECORD.                      
168700                                                                          
169100     IF  FM146-REC-TYPE     (1)   =    '46'                               
169200         WRITE ERROR-REC        FROM   FM146-RECORD (1).                  
169000                                                                          
169400     IF  FM146-REC-TYPE     (2)   =    '46'                               
169500         WRITE ERROR-REC        FROM   FM146-RECORD (2).                  
169300                                                                          
169700     IF  FM146-REC-TYPE     (3)   =    '46'                               
169800         WRITE ERROR-REC        FROM   FM146-RECORD (3).                  
169600                                                                          
170000     IF  FM146-REC-TYPE     (4)   =    '46'                               
170100         WRITE ERROR-REC        FROM   FM146-RECORD (4).                  
169900                                                                          
170300     IF  FM146-REC-TYPE     (5)   =    '46'                               
170400         WRITE ERROR-REC        FROM   FM146-RECORD (5).                  
170200                                                                          
170600     IF  FM150-REC-TYPE           =    '50'                               
170700         WRITE ERROR-REC        FROM   FM150-RECORD.                      
170500                                                                          
173600     IF  FM160-REC-TYPE     (1)   =    '60'                               
173700         WRITE ERROR-REC        FROM   FM160-RECORD (1).                  
170800                                                                          
173900     IF  FM165-REC-TYPE     (1)   =    '65'                               
174000         WRITE ERROR-REC        FROM   FM165-RECORD (1).                  
171100                                                                          
174200     IF  FM160-REC-TYPE     (2)   =    '60'                               
174300         WRITE ERROR-REC        FROM   FM160-RECORD (2).                  
171400                                                                          
174500     IF  FM165-REC-TYPE     (2)   =    '65'                               
174600         WRITE ERROR-REC        FROM   FM165-RECORD (2).                  
171700                                                                          
174800     IF  FM160-REC-TYPE     (3)   =    '60'                               
174900         WRITE ERROR-REC        FROM   FM160-RECORD (3).                  
172000                                                                          
175100     IF  FM165-REC-TYPE     (3)   =    '65'                               
175200         WRITE ERROR-REC        FROM   FM165-RECORD (3).                  
172300                                                                          
175400     IF  FM160-REC-TYPE     (4)   =    '60'                               
175500         WRITE ERROR-REC        FROM   FM160-RECORD (4).                  
172600                                                                          
175700     IF  FM165-REC-TYPE     (4)   =    '65'                               
175800         WRITE ERROR-REC        FROM   FM165-RECORD (4).                  
172900                                                                          
176000     IF  FM160-REC-TYPE     (05)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (05).                 
173200                                                                          
176300     IF  FM165-REC-TYPE     (05)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (05).                 
173500                                                                          
176000     IF  FM160-REC-TYPE     (06)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (06).                 
173800                                                                          
176300     IF  FM165-REC-TYPE     (06)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (06).                 
174100                                                                          
176000     IF  FM160-REC-TYPE     (07)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (07).                 
174400                                                                          
176300     IF  FM165-REC-TYPE     (07)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (07).                 
174700                                                                          
176000     IF  FM160-REC-TYPE     (08)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (08).                 
175000                                                                          
176300     IF  FM165-REC-TYPE     (08)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (08).                 
175300                                                                          
176000     IF  FM160-REC-TYPE     (09)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (09).                 
175600                                                                          
176300     IF  FM165-REC-TYPE     (09)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (09).                 
175900                                                                          
176000     IF  FM160-REC-TYPE     (10)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (10).                 
176200                                                                          
176300     IF  FM165-REC-TYPE     (10)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (11).                 
176500                                                                          
176000     IF  FM160-REC-TYPE     (11)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (11).                 
176800                                                                          
176300     IF  FM165-REC-TYPE     (11)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (11).                 
177100                                                                          
176000     IF  FM160-REC-TYPE     (12)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (12).                 
177400                                                                          
176300     IF  FM165-REC-TYPE     (12)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (12).                 
177700                                                                          
176000     IF  FM160-REC-TYPE     (13)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (13).                 
178000                                                                          
176300     IF  FM165-REC-TYPE     (13)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (13).                 
178300                                                                          
176000     IF  FM160-REC-TYPE     (14)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (14).                 
178600                                                                          
176300     IF  FM165-REC-TYPE     (14)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (14).                 
178900                                                                          
176000     IF  FM160-REC-TYPE     (15)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (15).                 
179200                                                                          
176300     IF  FM165-REC-TYPE     (15)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (15).                 
179500                                                                          
176000     IF  FM160-REC-TYPE     (16)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (16).                 
179800                                                                          
176300     IF  FM165-REC-TYPE     (16)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (16).                 
180100                                                                          
176000     IF  FM160-REC-TYPE     (17)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (17).                 
180400                                                                          
176300     IF  FM165-REC-TYPE     (17)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (17).                 
180700                                                                          
176000     IF  FM160-REC-TYPE     (18)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (18).                 
181000                                                                          
176300     IF  FM165-REC-TYPE     (18)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (18).                 
181300                                                                          
176000     IF  FM160-REC-TYPE     (19)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (19).                 
181600                                                                          
176300     IF  FM165-REC-TYPE     (19)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (19).                 
181900                                                                          
176000     IF  FM160-REC-TYPE     (20)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (20).                 
176200                                                                          
176300     IF  FM165-REC-TYPE     (20)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (20).                 
176500                                                                          
176000     IF  FM160-REC-TYPE     (21)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (21).                 
176200                                                                          
176300     IF  FM165-REC-TYPE     (21)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (21).                 
176500                                                                          
176000     IF  FM160-REC-TYPE     (22)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (22).                 
176200                                                                          
176300     IF  FM165-REC-TYPE     (22)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (22).                 
176500                                                                          
176000     IF  FM160-REC-TYPE     (23)   =    '60'                              
176100         WRITE ERROR-REC        FROM   FM160-RECORD (23).                 
176200                                                                          
176300     IF  FM165-REC-TYPE     (23)   =    '65'                              
176400         WRITE ERROR-REC        FROM   FM165-RECORD (23).                 
176500                                                                          
176600     IF  FM170-REC-TYPE           =    '70'                               
176700         WRITE ERROR-REC        FROM   FM170-RECORD.                      
176800                                                                          
176900     IF  FM180-REC-TYPE           =    '80'                               
177000         WRITE ERROR-REC        FROM   FM180-RECORD.                      
177100                                                                          
177200     IF  FM190-REC-TYPE           =    '90'                               
177300         WRITE ERROR-REC        FROM   FM190-RECORD.                      
177400                                                                          
177500     IF  FM1A5-REC-TYPE           =    'A5'                               
177600         WRITE ERROR-REC        FROM   FM1A5-RECORD.                      
177700                                                                          
177800     IF  FM1B0-REC-TYPE           =    'B0'                               
177900         WRITE ERROR-REC        FROM   FM1B0-RECORD.                      
178000                                                                          
178100     IF  FM1B5-REC-TYPE           =    'B5'                               
178200         WRITE ERROR-REC        FROM   FM1B5-RECORD.                      
178300                                                                          
178400     IF  FM1C0-REC-TYPE           =    'C0'                               
178500         WRITE ERROR-REC        FROM   FM1C0-RECORD.                      
178600                                                                          
178700     IF  FM1D0-REC-TYPE           =    'D0'                               
178800         WRITE ERROR-REC        FROM   FM1D0-RECORD.                      
178900                                                                          
179000     IF  FM1D1-REC-TYPE     (1)   =    'D1'                               
179100         WRITE ERROR-REC        FROM   FM1D1-RECORD (1).                  
179200                                                                          
179300     IF  FM1D1-REC-TYPE     (2)   =    'D1'                               
179400         WRITE ERROR-REC        FROM   FM1D1-RECORD (2).                  
179500                                                                          
179900     IF  FM1E0-REC-TYPE           =    'E0'                               
180000         WRITE ERROR-REC        FROM   FM1E0-RECORD.                      
180100                                                                          
180200     IF  FM1E1-REC-TYPE           =    'E1'                               
180300         WRITE ERROR-REC        FROM   FM1E1-RECORD.                      
180400                                                                          
180500     IF  FM1E6-REC-TYPE     (1)   =    'E6'                               
180600         WRITE ERROR-REC        FROM   FM1E6-RECORD (1).                  
180700                                                                          
180800     IF  FM1E6-REC-TYPE     (2)   =    'E6'                               
180900         WRITE ERROR-REC        FROM   FM1E6-RECORD (2).                  
181000                                                                          
181100     IF  FM1E6-REC-TYPE     (3)   =    'E6'                               
181200         WRITE ERROR-REC        FROM   FM1E6-RECORD (3).                  
181300                                                                          
181400     IF  FM1E6-REC-TYPE     (4)   =    'E6'                               
181500         WRITE ERROR-REC        FROM   FM1E6-RECORD (4).                  
181600                                                                          
181700     IF  FM1E6-REC-TYPE     (5)   =    'E6'                               
181800         WRITE ERROR-REC        FROM   FM1E6-RECORD (5).                  
181900                                                                          
182000     PERFORM 9310-WRITE-F0-RECORDS                                        
182100       THRU  9310-EXIT                                                    
182200     VARYING VTG-F0             FROM   1 BY 1                             
ITS102*     UNTIL  VTG-F0                >   23.                                
ITS102      UNTIL  VTG-F0                >   50.                                
182400                                                                          
182500     IF  FM1G0-REC-TYPE           =    'G0'                               
182600         WRITE ERROR-REC        FROM   FM1G0-RECORD.                      
182700                                                                          
182800     IF  FM1X0-REC-TYPE           =    'X0'                               
182900         WRITE ERROR-REC        FROM   FM1X0-RECORD.                      
183000                                                                          
183100 9300-EXIT.                                                               
183200     EXIT.                                                                
183300     EJECT                                                                
183400 9310-WRITE-F0-RECORDS  SECTION.                                          
183500                                                                          
183600****************************************************************          
183700*                                                                         
183800*     WRITE THE F0/F5 RECORDS TO THE ERROR OUTPUT FILE                    
183900*                                                                         
184000****************************************************************          
184100                                                                          
184200     IF  FM1F0-REC-TYPE     (VTG-F0)   =    'F0'                          
184300         WRITE ERROR-REC             FROM   FM1F0-RECORD (VTG-F0).        
184400                                                                          
184500     IF  FM1F5-REC-TYPE     (VTG-F0)   =    'F5'                          
184600         WRITE ERROR-REC             FROM   FM1F5-RECORD (VTG-F0).        
184700                                                                          
184800 9310-EXIT.                                                               
184900     EXIT.                                                                
185000*    EJECT                                                         EBCBS  
185100*9400-WRITE-BATCH-KEYWORD-RCDS  SECTION.                           EBCBS  
185200*                                                                  EBCBS  
185300****************************************************************          
185400*                                                                         
185500*     WRITE THE GROUP TO THE KEYWORD OUTPUT FILE                          
185600*                                                                         
185700****************************************************************          
185800*                                                                  EBCBS  
185900*    IF WS-PROMPT-ENTRY (KW-INDEX) =    SPACES                     EBCBS  
186000*        MOVE 'Y'                  TO   NO-MORE-RECORD-SWITCH      EBCBS  
186100*    ELSE                                                          EBCBS  
186200*        WRITE BATCH-KEYWORD-REC FROM WS-PROMPT-ENTRY (KW-INDEX).  EBCBS  
186300*                                                                  EBCBS  
186400*9400-EXIT.                                                        EBCBS  
186500*    EXIT.                                                         EBCBS  
186600        EJECT                                                     CL393611
186700                                                                  CL393611
189277 9450-READ-CROSS-OVER              SECTION.                               
189277           READ CROSS-OVER-FILE                                           
189277             AT END                                                       
189277             MOVE  'Y'               TO   END-OF-CROSS-SWITCH             
189277           NOT AT END                                                     
189277           MOVE UN-OLD-SUBID TO WS-CROSS-OLD                              
189277                             OF WS-CROSS-RECORD(SUB-UN-ID)                
189277           MOVE UN-NEW-SUBID TO WS-CROSS-NEW                              
189277                             OF WS-CROSS-RECORD(SUB-UN-ID)                
189277                                                                          
189277           END-READ.                                                      
189277                                                                          
189277 9450-EXIT.                                                               
189277     EXIT.                                                                
                                                                                
186800 9500-READ-ITS-ROOT                SECTION.                       CL393611
186900                                                                  CL393611
186910****************************************************************  CL393611
186920*                                                                 CL393611
186930*     READ AND HOLD THE ITS ROOT SEGMENT                          CL393611
186940*                                                                 CL393611
186950****************************************************************  CL393611
187000                                                                  CL393611
187100     MOVE  WS-SFSER                TO   SSA-ITS-SER-NO            CL393611
187200     MOVE 'N'                      TO   ITS-ROOT-SWITCH.          CL393611
187300                                                                  CL393611
187400     CALL  'LNDLICBL'       USING   PCB-NM-ITS                    CL393611
187500                                    GHU-CALL                      CL393611
187600                                    PCB-AREA                      CL393611
187700                                    VS-ITS-ROOT-SEGMENT-AREA      CL393611
187800                                    SSA-ITS-ROOT.                 CL393611
187900                                                                  CL393611
188000     IF  PCB-DLI-RET-CD            =    '  '                      CL393611
188001       IF (FM105-REC-TYPE     = '05' AND                          EBCBS   
188002                                 FM105-TYPE-SUBM         = '1')   EBCBS   
188003       OR (FM1A5-REC-TYPE     = 'A5' AND                          EBCBS   
188004                                 FM1A5-TYPE-SUBM         = '1')   EBCBS   
188005         MOVE  9511             TO   KILL-MODULE-POSITION         EBCBS   
CRAIG *        CALL  'LXKILL'      USING   KILL-MODULE-NM               EBCBS   
CRAIG *                                    KILL-MODULE-POSITION         EBCBS   
CRAIG *        CALL 'ZNCNCLDP'                                          EBCBS   
188100       END-IF                                                     EBCBS   
188200     ELSE                                                         CL393611
188300         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'              CL393611
188400             MOVE 'Y'              TO   ITS-ROOT-SWITCH           CL393611
188500             GO TO 9500-EXIT                                      CL393611
188600         ELSE                                                     CL393611
188700             MOVE  9500         TO   KILL-MODULE-POSITION         CL393611
188800             CALL  'LXKILL'  USING   KILL-MODULE-NM               CL393611
188900                                     KILL-MODULE-POSITION         CL393611
189000             CALL 'ZNCNCLDP'.                                     CL393611
189100                                                                  CL393611
189200 9500-EXIT.                                                       CL393611
189300     EXIT.                                                        CL393611
189400*-----------------------------------------------------------      CL393611
189500                                                                  CL393611
189600 9510-READ-SF-DATA                 SECTION.                       CL393611
189610**                                                                CX397034
189620****************************************************************  CL393611
189630*                                                                 CL393611
189640*     READ AND HOLD THE ITS SF SEGMENT (GET/HOLD NEXT/PARENT)     CL393611
189700*                                                                 CL393611
189800****************************************************************  CL393611
189900**                                                                CX397034
190000     MOVE 'N'                      TO   ITS-SF-DATA-SWITCH.       CL393611
190100**                                                                CX397034
190200     CALL  'LNDLICBL'       USING   PCB-NM-ITS                    CL393611
190300                                    GHNP-CALL                     CL393611
190400                                    PCB-AREA                      CL393611
190500                                    VS-ITS-SF-DATA-SEGMENT        CL393611
190600                                    SSA-SF-DATA.                  CL393611
190700**                                                                CX397034
190800     IF  PCB-DLI-RET-CD            =    '  '                      CL393611
190900         NEXT SENTENCE                                            CL393611
191000     ELSE                                                         CL393611
191100         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'              CL393611
191200             MOVE 'Y'              TO   ITS-SF-DATA-SWITCH        CL393611
191300         ELSE                                                     CL393611
191400             MOVE  9510         TO   KILL-MODULE-POSITION         CL393611
191500             CALL  'LXKILL'  USING   KILL-MODULE-NM               CL393611
191600                                     KILL-MODULE-POSITION         CL393611
191700             CALL 'ZNCNCLDP'.                                     CL393611
191800**                                                                CX397034
191900 9510-EXIT.                                                       CL393611
192000     EXIT.                                                        CL393611
192010     EJECT                                                        CL395543
192020 9520-READ-ITR-ROOT SECTION.                                      CL395543
192021                                                                  CL395543
192022****************************************************************  CL395543
192023*                                                                 CL395543
192024*     READ AND HOLD THE ITR ROOT SEGMENT                          CL395543
192025*                                                                 CL395543
192026****************************************************************  CL395543
192027                                                                  CL395543
192028     MOVE  WS-SFSER                TO   SSA-ITR-SER-NO            CL395543
192029     MOVE 'N'                      TO   ITR-ROOT-SWITCH.          CL395543
192030                                                                  CL395543
192040     CALL  'LNDLICBL'       USING   PCB-NM-ITR                    CL395543
192041                                    GHU-CALL                      CL395543
192042                                    PCB-AREA                      CL395543
192043                                    VS-ITR-ROOT-SEGMENT-AREA      CL395543
192044                                    SSA-ITR-ROOT.                 CL395543
192050                                                                  CL395543
192051     IF  PCB-DLI-RET-CD            =    '  '                      CL395543
192052         NEXT SENTENCE                                            CL395543
192053     ELSE                                                         CL395543
192054         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'              CL395543
192055             MOVE 'Y'              TO   ITR-ROOT-SWITCH           CL395543
192057         ELSE                                                     CL395543
192060             MOVE  9520         TO   KILL-MODULE-POSITION         CL395543
192061             CALL  'LXKILL'  USING   KILL-MODULE-NM               CL395543
192062                                     KILL-MODULE-POSITION         CL395543
192063             CALL 'ZNCNCLDP'.                                     CL395543
192070                                                                  CL395543
192071 9520-EXIT.                                                       CL395543
192072     EXIT.                                                        CL395543
192080*-----------------------------------------------------------      CL395543
192081                                                                  CL395543
192090 9530-READ-ITR-SF-DATA             SECTION.                       CL395543
192091                                                                  CL395543
192092****************************************************************  CL395543
192093*                                                                 CL395543
192094*     READ AND HOLD THE ITR SF SEGMENT (GET/HOLD NEXT/PARENT)     CL395543
192095*                                                                 CL395543
192096****************************************************************  CL395543
192097                                                                  CL395543
192100     MOVE 'N'                      TO   ITR-SF-DATA-SWITCH.       CL395543
192101                                                                  CL395543
192110     CALL  'LNDLICBL'       USING   PCB-NM-ITR                    CL395543
192111                                    GHNP-CALL                     CL395543
192112                                    PCB-AREA                      CL395543
192113                                    VS-ITR-SF-DATA-SEGMENT        CL395543
192114                                    SSA-ITR-SF-DATA.              CL395543
192115                                                                  CL395543
192120     IF  PCB-DLI-RET-CD            =    '  '                      CL395543
192121         NEXT SENTENCE                                            CL395543
192122     ELSE                                                         CL395543
192123         IF  PCB-DLI-RET-CD        =    'GE' OR 'GB'              CL395543
192124             MOVE 'Y'              TO   ITR-SF-DATA-SWITCH        CL395543
192130         ELSE                                                     CL395543
192131             MOVE  9530         TO   KILL-MODULE-POSITION         CL395543
192132             CALL  'LXKILL'  USING   KILL-MODULE-NM               CL395543
192133                                     KILL-MODULE-POSITION         CL395543
192134             CALL 'ZNCNCLDP'.                                     CL395543
192135                                                                  CL395543
192140 9530-EXIT.                                                       CL395543
192141     EXIT.                                                        CL395543
192142                                                                  CL395543
192200        EJECT                                                     CL393611
192300 9600-UPDATE-ITS-ROOT                SECTION.                     CL393611
192400                                                                  CL393611
192500****************************************************************  CL393611
192600*                                                                 CL393611
192700*     UPDATE THE ITS ROOT SEGMENT                                 CL393611
192800*                                                                 CL393611
192900****************************************************************  CL393611
193000                                                                  CL393611
193100     CALL  'LNDLICBL'           USING   PCB-NM-ITS                CL393611
193200                                        REPL-CALL                 CL393611
193300                                        PCB-AREA                  CL393611
193400                                        VS-ITS-ROOT-SEGMENT-AREA. CL393611
193500                                                                  CL393611
193600     IF  PCB-DLI-RET-CD            =    '  '                      CL393611
193700         NEXT SENTENCE                                            CL393611
193800     ELSE                                                         CL393611
193900         MOVE  9600         TO   KILL-MODULE-POSITION             CL393611
194000         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL393611
194100                                 KILL-MODULE-POSITION             CL393611
194200         CALL 'ZNCNCLDP'.                                         CL393611
194300                                                                  CL393611
194400 9600-EXIT.                                                       CL393611
194500     EXIT.                                                        CL393611
194600*-----------------------------------------------------------      CL393611
194700                                                                  CL393611
194800 9610-UPDATE-SF-DATA          SECTION.                            CL393611
194900**                                                                CX397034
195000****************************************************************  CL393611
195100*                                                                 CL393611
195200*     UPDATE THE ITS SF DATA SEGMENT                              CL393611
195300*                                                                 CL393611
195400****************************************************************  CL393611
195500**                                                                CX397034
195600     COMPUTE WP-SEGM-LGT-CD = ((255 * CQ-TRN-CNT) + 13).          CX397034
195601     MOVE    WX-SEGM-LGT-CD        TO CQ-SEGM-LGT-CD-X            CL395833
195700     MOVE   SPACES              TO   CQ-TXT.                      CL393611
195800     IF  INST-CLAIM                                               CL393611
195900         MOVE   'I'             TO   CQ-INST-PROF-IND             CL393611
196000         PERFORM  9800-BUILD-INST-SF                              CL393611
196100            THRU  9800-EXIT                                       CL393611
196200         MOVE   BUILD-SF-AREA                                     CL393611
196300                                TO   CQ-ISF-DA                    CL393611
196400     ELSE                                                         CL393611
196500         MOVE   'P'             TO   CQ-INST-PROF-IND             CL393611
196600         PERFORM  9810-BUILD-PROF-SF                              CL393611
196700            THRU  9810-EXIT                                       CL393611
196800         MOVE   BUILD-SF-AREA                                     CL393611
196900                                TO   CQ-ISF-DA.                   CL393611
197000****                                                              CX397034
197100     CALL  'LNDLICBL'           USING   PCB-NM-ITS                CL393611
197200                                        REPL-CALL                 CL393611
197300                                        PCB-AREA                  CL393611
197400                                        VS-ITS-SF-DATA-SEGMENT.   CL393611
197500**                                                                CX397034
197600     IF  PCB-DLI-RET-CD            =    '  '                      CL393611
197700         GO TO 9610-EXIT                                          CL393611
197800     ELSE                                                         CL393611
197900         MOVE  9610         TO   KILL-MODULE-POSITION             CL393611
198000         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL393611
198100                                 KILL-MODULE-POSITION             CL393611
198200         CALL 'ZNCNCLDP'.                                         CL393611
198300**                                                                CX397034
198400 9610-EXIT.                                                       CL393611
198500     EXIT.                                                        CL393611
198510        EJECT                                                     CL395543
198520 9620-UPDATE-ITR-ROOT                SECTION.                     CL395543
198521                                                                  CL395543
198522****************************************************************  CL395543
198523*                                                                 CL395543
198524*     UPDATE THE ITR ROOT SEGMENT                                 CL395543
198525*                                                                 CL395543
198526****************************************************************  CL395543
198527                                                                  CL395543
198530     CALL  'LNDLICBL'           USING   PCB-NM-ITR                CL395543
198531                                        REPL-CALL                 CL395543
198532                                        PCB-AREA                  CL395543
198533                                        VS-ITR-ROOT-SEGMENT-AREA. CL395543
198534                                                                  CL395543
198540     IF  PCB-DLI-RET-CD            =    '  '                      CL395543
198541         NEXT SENTENCE                                            CL395543
198542     ELSE                                                         CL395543
198543         MOVE  9620         TO   KILL-MODULE-POSITION             CL395543
198544         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL395543
198545                                 KILL-MODULE-POSITION             CL395543
198546         CALL 'ZNCNCLDP'.                                         CL395543
198550                                                                  CL395543
198551 9620-EXIT.                                                       CL395543
198552     EXIT.                                                        CL395543
198553*-----------------------------------------------------------      CL395543
198554                                                                  CL395543
198560 9630-UPDATE-ITR-SF-DATA      SECTION.                            CL395543
198561                                                                  CL395543
198562****************************************************************  CL395543
198563*                                                                 CL395543
198564*     UPDATE THE ITR SF DATA SEGMENT                              CL395543
198565*                                                                 CL395543
198566****************************************************************  CL395543
198567     COMPUTE WP-SEGM-LGT-CD = ((255 * CQ-TRN-CNT) + 13).          CL395833
198568     MOVE    WX-SEGM-LGT-CD        TO WQ-SEGM-LGT-CD-X            CL395833
198569     MOVE   SPACES              TO   WQ-TXT.                      CL395543
198570     IF  INST-CLAIM                                               CL395543
198571         MOVE   'I'             TO   WQ-INST-PROF-IND             CL395543
198572         PERFORM  9800-BUILD-INST-SF                              CL395543
198573            THRU  9800-EXIT                                       CL395543
198574         MOVE   BUILD-SF-AREA   TO   WQ-ISF-DA                    CL395543
198575                                     WQ-ISF-DA                    CL395543
198576     ELSE                                                         CL395543
198577         MOVE   'P'             TO   WQ-INST-PROF-IND             CL395543
198578         PERFORM  9810-BUILD-PROF-SF                              CL395543
198579            THRU  9810-EXIT                                       CL395543
198580         MOVE   BUILD-SF-AREA                                     CL395543
198581                                TO   WQ-ISF-DA.                   CL395543
198582                                                                  CL395543
198583     CALL  'LNDLICBL'           USING   PCB-NM-ITR                CL395543
198584                                        REPL-CALL                 CL395543
198585                                        PCB-AREA                  CL395543
198586                                        VS-ITR-SF-DATA-SEGMENT    CL395543
198590                                                                  CL395543
198600     IF  PCB-DLI-RET-CD            =    '  '                      CL395543
198601         GO TO 9630-EXIT                                          CL395543
198602     ELSE                                                         CL395543
198603         MOVE  9630         TO   KILL-MODULE-POSITION             CL395543
198604         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL395543
198605                                 KILL-MODULE-POSITION             CL395543
198606         CALL 'ZNCNCLDP'.                                         CL395543
198610                                                                  CL395543
198620 9630-EXIT.                                                       CL395543
198621     EXIT.                                                        CL395543
198690        EJECT                                                     CL395543
198700 9700-ADD-ITS-ROOT              SECTION.                          CL393611
198800**                                                                CL393611
198900****************************************************************  CL393611
199000*                                                                 CL393611
199100*     ADD THE ITS ROOT SEGMENT                                    CL393611
199200*                                                                 CL393611
199300****************************************************************  CL393611
199400**                                                                CL393611
199500     MOVE WS-SFSER              TO  SSA-ISRT-ITS-SER-NO.          CL393611
199600**                                                                CL393611
199700*        SET DATA IN ROOT HEADER                                  CL393611
199800**                                                                CL393611
199900     MOVE WS-SFSER              TO  CO-SCCF-SER-NO.               CL393611
200000     MOVE WS-DATE               TO  CO-RKD-CRTD-DT.               CL393611
200100     MOVE WS-PURG-DATE          TO  CO-TO-PURG-DT.                CL393611
200200                                                                  CL393611
200300     IF  INST-CLAIM                                               CL393611
200400          MOVE FM105-LOC-PLAN-CD           TO  WS-PLAN-CD-X       CL393611
200500          MOVE WS-PLAN-CD-N                TO  CO-LCL-PLN-CD      CL393611
200600          MOVE FM105-PROC-SITE-PLAN-CD     TO  WS-PLAN-CD-X       CL393611
200700          MOVE WS-PLAN-CD-N                TO  CO-PRCS-PLN-CD     CL393611
200800     ELSE                                                         CL393611
200900          MOVE FM1A5-LOC-PLAN-CD           TO  WS-PLAN-CD-X       CL393611
201000          MOVE WS-PLAN-CD-N                TO  CO-LCL-PLN-CD      CL393611
201100          MOVE FM1A5-PROC-SITE-PLAN-CD     TO  WS-PLAN-CD-X       CL393611
201200          MOVE WS-PLAN-CD-N                TO  CO-PRCS-PLN-CD.    CL393611
201300**                                                                CL393611
201400     CALL  'LNDLICBL'      USING   PCB-NM-ITS                     CL393611
201500                                   ISRT-CALL                      CL393611
201600                                   PCB-AREA                       CL393611
201700                                   VS-ITS-ROOT-SEGMENT-AREA       CL393611
201800                                   SSA-ISRT-ITS-ROOT.             CL393611
201900                                                                  CL393611
202000     IF  PCB-DLI-RET-CD            =    '  '                      CL393611
202100         NEXT SENTENCE                                            CL393611
202200     ELSE                                                         CL393611
202300         MOVE  9700         TO   KILL-MODULE-POSITION             CL393611
202400         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL393611
202500                                 KILL-MODULE-POSITION             CL393611
202600         CALL 'ZNCNCLDP'.                                         CL393611
202700**                                                                CL393611
202800 9700-EXIT.                                                       CL393611
202900     EXIT.                                                        CL393611
203000        EJECT                                                     CL393611
203100 9710-ADD-SF-DATA             SECTION.                            CL393611
203200**                                                                CX397034
203300****************************************************************  CL393611
203400*                                                                 CL393611
203500*     ADD THE ITS SF DATA SEGMENT                                 CL393611
203600*                                                                 CL393611
203700****************************************************************  CL393611
203800**                                                                CX397034
203900     COMPUTE WP-SEGM-LGT-CD = ((255 * CQ-TRN-CNT) + 13).          CX397034
203901     MOVE    WX-SEGM-LGT-CD        TO CQ-SEGM-LGT-CD-X            CL395833
204000     MOVE   SPACES              TO   CQ-TXT.                      CL393611
204100     IF  INST-CLAIM                                               CL393611
204200         MOVE   'I'             TO   CQ-INST-PROF-IND             CL393611
204300         PERFORM  9800-BUILD-INST-SF                              CL393611
204400            THRU  9800-EXIT                                       CL393611
204500         MOVE   BUILD-SF-AREA                                     CL393611
204600                                TO   CQ-ISF-DA                    CL393611
204700     ELSE                                                         CL393611
204800         MOVE   'P'             TO   CQ-INST-PROF-IND             CL393611
204900         PERFORM  9810-BUILD-PROF-SF                              CL393611
205000            THRU  9810-EXIT                                       CL393611
205100         MOVE   BUILD-SF-AREA                                     CL393611
205200                                TO   CQ-ISF-DA.                   CL393611
205300**                                                                CX397034
205301*  USING FORMATS DATABASE NO LONGER ON ITR DATABASE               19152070
205302*                                                                 19153070
205400*    CALL  'LNDLICBL'           USING   PCB-NM-ITS                CX397034
205500*                                       ISRT-CALL                 CX397034
205600*                                       PCB-AREA                  CX397034
205700*                                       VS-ITS-SF-DATA-SEGMENT    CX397034
205800*                                       SSA-ISRT-SF-DATA.         CX397034
205900**                                                                CX397034
206000*    IF  PCB-DLI-RET-CD            =    '  '                      CX397034
206100*        GO TO 9710-EXIT                                          CX397034
206200*    ELSE                                                         CX397034
206300*        MOVE  9710         TO   KILL-MODULE-POSITION             CX397034
206400*        CALL  'LXKILL'  USING   KILL-MODULE-NM                   CX397034
206500*                                KILL-MODULE-POSITION             CX397034
206600*        CALL 'ZNCNCLDP'.                                         CX397034
206700**                                                                CX397034
206800 9710-EXIT.                                                       CL393611
206900     EXIT.                                                        CL393611
206910       EJECT                                                      CL395543
206920 9720-ADD-ITR-ROOT              SECTION.                          CL395543
206921                                                                  CL395543
206922****************************************************************  CL395543
206923*                                                                 CL395543
206924*     ADD THE ITR ROOT SEGMENT                                    CL395543
206925*                                                                 CL395543
206926****************************************************************  CL395543
206927                                                                  CL395543
206930     MOVE WS-SFSER              TO  SSA-ISRT-ITR-SER-NO.          CL395543
206931                                                                  CL395543
206932*        SET DATA IN ROOT HEADER                                  CL395543
206933                                                                  CL395543
206940     MOVE WS-SFSER              TO  WO-SCCF-SER-NO.               CL395543
206941     MOVE WS-DATE               TO  WO-RKD-CRTD-DT.               CL395543
206942     MOVE WS-PURG-DATE          TO  WO-TO-PURG-DT.                CL395543
206950                                                                  CL395543
206960     IF  INST-CLAIM                                               CL395543
206961          MOVE FM105-LOC-PLAN-CD           TO  WS-PLAN-CD-X       CL395543
206962          MOVE WS-PLAN-CD-N                TO  WO-LCL-PLN-CD      CL395543
206963          MOVE FM105-PROC-SITE-PLAN-CD     TO  WS-PLAN-CD-X       CL395543
206964          MOVE WS-PLAN-CD-N                TO  WO-PRCS-PLN-CD     CL395543
206965     ELSE                                                         CL395543
206966          MOVE FM1A5-LOC-PLAN-CD           TO  WS-PLAN-CD-X       CL395543
206970          MOVE WS-PLAN-CD-N                TO  WO-LCL-PLN-CD      CL395543
206971          MOVE FM1A5-PROC-SITE-PLAN-CD     TO  WS-PLAN-CD-X       CL395543
206972          MOVE WS-PLAN-CD-N                TO  WO-PRCS-PLN-CD.    CL395543
206973                                                                  CL395543
206980     CALL  'LNDLICBL'      USING   PCB-NM-ITR                     CL395543
206981                                   ISRT-CALL                      CL395543
206982                                   PCB-AREA                       CL395543
206983                                   VS-ITR-ROOT-SEGMENT-AREA       CL395543
206984                                   SSA-ISRT-ITR-ROOT.             CL395543
206985                                                                  CL395543
206986     IF  PCB-DLI-RET-CD            =    '  '                      CL395543
206987         NEXT SENTENCE                                            CL395543
206988     ELSE                                                         CL395543
206989         MOVE  9720         TO   KILL-MODULE-POSITION             CL395543
206990         CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL395543
206991                                 KILL-MODULE-POSITION             CL395543
206992         CALL 'ZNCNCLDP'.                                         CL395543
207000                                                                  CL395543
207010 9720-EXIT.                                                       CL395543
207011     EXIT.                                                        CL395543
207020        EJECT                                                     CL395543
207030 9730-ADD-ITR-SF-DATA         SECTION.                            CL395543
207031                                                                  CL395543
207032****************************************************************  CL395543
207033*                                                                 CL395543
207034*     ADD THE ITR SF DATA SEGMENT                                 CL395543
207035*                                                                 CL395543
207036****************************************************************  CL395543
207037                                                                  CL395543
207038     MOVE   CQ-TRN-CNT          TO   WQ-TRN-CNT.                  CL395543
207039     COMPUTE WP-SEGM-LGT-CD = ((255 * WQ-TRN-CNT) + 13).          CL395833
207040     MOVE    WX-SEGM-LGT-CD        TO WQ-SEGM-LGT-CD-X            CL395833
207041     MOVE   SPACES              TO   WQ-TXT.                      CL395543
207042     IF  INST-CLAIM                                               CL395543
207043         MOVE   'I'             TO   WQ-INST-PROF-IND             CL395543
207044         PERFORM  9800-BUILD-INST-SF                              CL395543
207045            THRU  9800-EXIT                                       CL395543
207046         MOVE   BUILD-SF-AREA                                     CL395543
207047                                TO   WQ-ISF-DA                    CL395543
207050     ELSE                                                         CL395543
207051         MOVE   'P'             TO   WQ-INST-PROF-IND             CL395543
207052         PERFORM  9810-BUILD-PROF-SF                              CL395543
207053            THRU  9810-EXIT                                       CL395543
207054         MOVE   BUILD-SF-AREA                                     CL395543
207055                                TO   WQ-ISF-DA.                   CL395543
207060                                                                  CL395543
207061*                                                                 CX397034
207062*  USING FORMATS DATABASE NO LONGER ON ITR DATABASE               19152070
207063*                                                                 19153070
207070*    CALL  'LNDLICBL'           USING   PCB-NM-ITR                CL395543
207071*                                       ISRT-CALL                 CL395543
207072*                                       PCB-AREA                  CL395543
207073*                                       VS-ITR-SF-DATA-SEGMENT    CL395543
207074*                                       SSA-ISRT-ITR-SF-DATA.     CL395543
207075*                                                                 CL395543
207076*    IF  PCB-DLI-RET-CD            =    '  '                      CL395543
207077*        GO TO 9730-EXIT                                          CL395543
207078*    ELSE                                                         CL395543
207080*        MOVE  9730         TO   KILL-MODULE-POSITION             CL395543
207081*        CALL  'LXKILL'  USING   KILL-MODULE-NM                   CL395543
207082*                                KILL-MODULE-POSITION             CL395543
207083*        CALL 'ZNCNCLDP'.                                         CL395543
207089*                                                                 CL395543
207090 9730-EXIT.                                                       CL395543
207091     EXIT.                                                        CL395543
207099        EJECT                                                     CL395543
207100 9800-BUILD-INST-SF            SECTION.                           CL393611
207200                                                                  CL393611
207300****************************************************************  CL393611
207400*                                                                 CL393611
207500*     BUILD INST SF SEGMENT - ONLY USE NON-BLANK RECORDS          CL393611
207600*     INSTITUTIONAL RECORDS OCCUR 56 TIMES - MAXIMUM              CL393611
207700*     BUILD-SF-AREA OCCURS 63 TIMES - MAXIMUM FOR PROFESSIONAL    CL393611
207800*                                                                 CL393611
207900****************************************************************  CL393611
208000                                                                  CL393611
208100     MOVE   +1               TO  SF-SUB.                          CL393611
208200     MOVE   +1               TO  BD-SUB.                          CL393611
208300     MOVE   SPACES           TO  BUILD-SF-AREA.                   CL393611
208400                                                                  CL393611
208500*    OUTPUT IS BUILD-SF-AREA WITH ONLY NON-BLANK SF RCDS          CL393611
208600                                                                  CL393611
208700 9800-BUILD-LOOP.                                                 CL393611
208800                                                                  CL393611
208900     IF  SF-SUB  GREATER THAN  +67                                CL393611
209000         GO  TO  9800-EXIT.                                       CL393611
209100                                                                  CL393611
209200     IF  INS-REC-TYPE (SF-SUB)  NOT  =  SPACES                    CL393611
209300         MOVE  INST-SET-OCCUR (SF-SUB)  TO                        CL393611
209400                                 ONE-SF-RECORD (BD-SUB)           CL393611
209500         ADD   +1            TO  SF-SUB                           CL393611
209600         ADD   +1            TO  BD-SUB                           CL393611
209700         GO  TO  9800-BUILD-LOOP                                  CL393611
209800     ELSE                                                         CL393611
209900         ADD   +1            TO  SF-SUB                           CL393611
210000         GO  TO  9800-BUILD-LOOP.                                 CL393611
210100                                                                  CL393611
210200 9800-EXIT.                                                       CL393611
210300     EXIT.                                                        CL393611
210400        EJECT                                                     CL393611
210500 9810-BUILD-PROF-SF            SECTION.                           CL393611
210600                                                                  CL393611
210700****************************************************************  CL393611
210800*                                                                 CL393611
210900*     BUILD PROF SF SEGMENT - ONLY USE NON-BLANK RECORDS          CL393611
211000*     PROFESSIONAL RECORDS OCCUR 63 TIMES - MAXIMUM               CL393611
211100*     BUILD-SF-AREA OCCURS 63 TIMES - MAXIMUM FOR PROFESSIONAL    CL393611
211200*                                                                 CL393611
211300****************************************************************  CL393611
211400                                                                  CL393611
211500     MOVE   +1               TO  SF-SUB.                          CL393611
211600     MOVE   +1               TO  BD-SUB.                          CL393611
211700     MOVE   SPACES           TO  BUILD-SF-AREA.                   CL393611
211800                                                                  CL393611
211900*           OUTPUT IS BUILD-SF-AREA WITH ONLY NON-BLANK SFS       CL393611
212000                                                                  CL393611
212100 9810-BUILD-LOOP.                                                 CL393611
212200                                                                  CL393611
212300     IF  SF-SUB  GREATER THAN  +63                                CL393611
212400         GO  TO  9810-EXIT.                                       CL393611
212500                                                                  CL393611
212600     IF  PRO-REC-TYPE (SF-SUB)  NOT  =  SPACES                    CL393611
212700         MOVE  PROF-SET-OCCUR (SF-SUB)  TO                        CL393611
212800                                 ONE-SF-RECORD (BD-SUB)           CL393611
212900         ADD   +1            TO  SF-SUB                           CL393611
213000         ADD   +1            TO  BD-SUB                           CL393611
213100         GO  TO  9810-BUILD-LOOP                                  CL393611
213200     ELSE                                                         CL393611
213300         ADD   +1            TO  SF-SUB                           CL393611
213400         GO  TO  9810-BUILD-LOOP.                                 CL393611
213500                                                                  CL393611
213600 9810-EXIT.                                                       CL393611
213700     EXIT.                                                        CL393611
213711*                                                                  EBCBS  
213712*********************************                                  EBCBS  
213713 99999-COMPRES-ITS-CL-TRAN.                                        EBCBS  
213714*                               *******************************    EBCBS  
213715*  COMPRESS THE CLCSF INTO A 256 BYTE RECORD.                 *    EBCBS  
213716***************************************************************    EBCBS  
213717*                                                                  EBCBS  
213718     IF WS-PROMPT-ENTRY (KW-INDEX) = SPACES                        EBCBS  
213719       MOVE 'Y'              TO  NO-MORE-RECORD-SWITCH             EBCBS  
213720       IF ITS-CL-WORK-PTR IS GREATER THAN ZERO                     EBCBS  
213721         ADD 1               TO ITS-CL-TRAN-COUNT                  EBCBS  
XXXXXX*        DISPLAY 'ITS-CL-CNT:'  ITS-CL-TRAN-COUNT                  EBCBS  
213722         MOVE ITS-CL-WORK-AREA                                     EBCBS  
213723                             TO                                    EBCBS  
213724                           ITS-CL-TABLE-REC (ITS-CL-TRAN-COUNT)    EBCBS  
213725       END-IF                                                      EBCBS  
213726     ELSE                                                          EBCBS  
213727       MOVE WS-PROMPT-ENTRY (KW-INDEX)                             EBCBS  
213728                             TO  LRSP-CL-WORK-AREA                 EBCBS  
XXXXXX***    DISPLAY 'FTN: CL-WA'      LRSP-CL-WORK-AREA                 EBCBS  
XXXXXX***    DISPLAY 'FTN: IDX  '      KW-INDEX                          EBCBS  
213729       INITIALIZE                LRSP-CL-WORK-CNT,                 EBCBS  
213730                                 LRSP-CL-WORK-PTR,                 EBCBS  
213731                                 LRSP-CL-WORK-SPC-CNT              EBCBS  
213732*                                                                  EBCBS  
213733       IF LRSP-CL-WORK-LN-NUM IS NOT NUMERIC                       EBCBS  
XXXXXX***    DISPLAY 'FTN: CL-WA'      LRSP-CL-WORK-AREA                 EBCBS  
XXXXXX***    DISPLAY 'FTN: IDX  '      KW-INDEX                          EBCBS  
XXXXXX***    DISPLAY 'FTN: LN   '      LRSP-CL-WORK-LN-NUM               EBCBS  
213734         PERFORM 99999-DETR-LRSP-KEYWORD-LGTH                      EBCBS  
213735           TEST AFTER                                              EBCBS  
213736           VARYING LRSP-CL-WORK-PTR FROM 1 BY 1                    EBCBS  
ITS102*          UNTIL LRSP-CL-WORK-PTR IS EQUAL TO 80                   EBCBS  
ITS102           UNTIL LRSP-CL-WORK-PTR IS EQUAL TO 81                   EBCBS  
213738         PERFORM 99999-POP-CS90-KEYWORD                            EBCBS  
213739       ELSE                                                        EBCBS  
213740         PERFORM 99999-EXPAND-LINE-ITEMS                           EBCBS  
213741       END-IF                                                      EBCBS  
213742     END-IF.                                                       EBCBS  
213800*                                                                  EBCBS  
213801*********************************                                  EBCBS  
213802 99999-DETR-LRSP-KEYWORD-LGTH.                                     EBCBS  
213803*                               *******************************    EBCBS  
213804*  GET THE LEGTH OF THE LRSP SUPPLIED KEYWORD.                *    EBCBS  
213805***************************************************************    EBCBS  
213806*                                                                  EBCBS  
213807     IF LRSP-CL-WORK-BYTE (LRSP-CL-WORK-PTR) IS EQUAL TO ' '       EBCBS  
213808       IF LRSP-CL-WORK-CNT IS GREATER THAN ZERO                    EBCBS  
213809         ADD 1                TO LRSP-CL-WORK-SPC-CNT,             EBCBS  
213810                                 LRSP-CL-WORK-CNT                  EBCBS  
213811       END-IF                                                      EBCBS  
213812     ELSE                                                          EBCBS  
213813       INITIALIZE                LRSP-CL-WORK-SPC-CNT              EBCBS  
213814       ADD 1                  TO LRSP-CL-WORK-CNT                  EBCBS  
213815     END-IF.                                                       EBCBS  
213816*                                                                  EBCBS  
213900*********************************                                  EBCBS  
213901 99999-POP-CS90-KEYWORD.                                           EBCBS  
213902*                               *******************************    EBCBS  
213903*  PLACE THE LRSP SUPPLIED KEYWORD IN THE CS90 256 BYTE ARRAY.*    EBCBS  
213904***************************************************************    EBCBS  
213905*                                                                  EBCBS  
ITS102*    COMPUTE LRSP-CL-WORK-STR-LOC = (80 - LRSP-CL-WORK-CNT + 1).   EBCBS  
ITS102     COMPUTE LRSP-CL-WORK-STR-LOC = (81 - LRSP-CL-WORK-CNT + 1).   EBCBS  
XXXXXX*    DISPLAY  'STR LOC :'   LRSP-CL-WORK-STR-LOC.                  EBCBS  
ITS102*    COMPUTE LRSP-CL-WORK-KWD-LGTH = (80 - LRSP-CL-WORK-SPC-CNT).  EBCBS  
ITS102     COMPUTE LRSP-CL-WORK-KWD-LGTH = (81 - LRSP-CL-WORK-SPC-CNT).  EBCBS  
XXXXXX*    DISPLAY  'WRK KWD :'   LRSP-CL-WORK-KWD-LGTH.                 EBCBS  
ITS102*    COMPUTE LRSP-CL-WORK-KWD-LGTH = (80 - LRSP-CL-WORK-SPC-CNT).  EBCBS  
ITS102     COMPUTE LRSP-CL-WORK-KWD-LGTH = (81 - LRSP-CL-WORK-SPC-CNT).  EBCBS  
XXXXXX*    DISPLAY 'WRK KWD : ' LRSP-CL-WORK-KWD-LGTH.                   EBCBS  
213908*                                                                  EBCBS  
213909     IF (ITS-CL-WORK-PTR + (LRSP-CL-WORK-KWD-LGTH -                EBCBS  
213910                                  LRSP-CL-WORK-STR-LOC) + 1)       EBCBS  
213911                                IS GREATER THAN 256                EBCBS  
213912       ADD 1                  TO ITS-CL-TRAN-COUNT                 EBCBS  
213913       MOVE ITS-CL-WORK-AREA  TO                                   EBCBS  
213914                           ITS-CL-TABLE-REC (ITS-CL-TRAN-COUNT)    EBCBS  
213915       INITIALIZE                ITS-CL-WORK-AREA,                 EBCBS  
213916                                 ITS-CL-WORK-PTR                   EBCBS  
213917     END-IF.                                                       EBCBS  
213918*                                                                  EBCBS  
213919     PERFORM                                                       EBCBS  
213920     TEST AFTER                                                    EBCBS  
213921     VARYING LRSP-CL-WORK-PTR FROM LRSP-CL-WORK-STR-LOC BY 1       EBCBS  
213922     UNTIL LRSP-CL-WORK-PTR IS EQUAL TO LRSP-CL-WORK-KWD-LGTH      EBCBS  
213923       ADD 1                  TO ITS-CL-WORK-PTR                   EBCBS  
213924       MOVE LRSP-CL-WORK-BYTE (LRSP-CL-WORK-PTR)                   EBCBS  
213925                              TO                                   EBCBS  
213926                            ITS-CL-WORK-BYTE (ITS-CL-WORK-PTR)     EBCBS  
213927     END-PERFORM.                                                  EBCBS  
214000*                                                                  EBCBS  
214001*********************************                                  EBCBS  
214002 99999-EXPAND-LINE-ITEMS.                                          EBCBS  
214003*                               *******************************    EBCBS  
214004*  EXPAND LINE ITEMS FOR RESEQ.                               *    EBCBS  
214005***************************************************************    EBCBS  
214006*                                                                  EBCBS  
214007     PERFORM 99999-UNSTRING-LINE-ITEMS.                            EBCBS  
214008*                                                                  EBCBS  
DIS:*****  DISPLAY 'CCSFTNBL'                                                   
DIS:*****  DISPLAY '99999-EXPAND-LINE-ITEMS'                                    
DIS:*****  DISPLAY 'LRSP-CL-WORK-LN-SW   =>  ' LRSP-CL-WORK-LN-SW               
214009     IF  LRSP-CL-WORK-LN-NUM < 100                                 EBCBS  
ITS102        MOVE LRSP-CL-WORK-LN-NUM TO LN-KEYWORD-LN-NUM              EBCBS  
ITS102     ELSE                                                                 
ITS102        MOVE LRSP-CL-WORK-LN-NUM TO LN-KEYWORD-LN-NUM-B            EBCBS  
ITS102     END-IF.                                                              
214010     IF LRSP-CL-WORK-LN-SW IS EQUAL TO ' '                         EBCBS  
214011       ADD 1                  TO  CLCSF-LINEITEM-COUNT             EBCBS  
214012       PERFORM 99999-POP-FIRST-LN-ITEM                             EBCBS  
214073     ELSE                                                          EBCBS  
             IF LRSP-CL-WORK-LN-SW IS EQUAL TO 'A'                              
      *        ADD 1                TO CLCSF-LINEITEM-COUNT                     
               PERFORM 99999-POP-SECOND-LN-ITEM                                 
             ELSE                                                               
                 PERFORM 99999-POP-THIRD-LN-ITEM                                
             END-IF                                                             
214015     END-IF.                                                       EBCBS  
      *    DISPLAY 'CLCSF-LINEITEM-COUNT   => ' CLCSF-LINEITEM-COUNT            
      *    DISPLAY 'AT END'.                                                    
214100*                                                                  EBCBS  
214101*********************************                                  EBCBS  
214102 99999-UNSTRING-LINE-ITEMS.                                        EBCBS  
214103*                               *******************************    EBCBS  
214104*  GET LINE INFORMATION IN SUB ARRAYS.                        *    EBCBS  
214105***************************************************************    EBCBS  
214106*                                                                  EBCBS  
214107     INITIALIZE LN-KEYWORD-TABLE.                                  EBCBS  
214107     INITIALIZE LN-UNSTRING-RECORDS.                               EBCBS  
214108*                                                                  EBCBS  
      *    DISPLAY 'CCSFTNBL'                                                   
      *    DISPLAY 'LRSP-CL-WORK-LN-DATA  => '  LRSP-CL-WORK-LN-DATA            
214109     UNSTRING LRSP-CL-WORK-LN-DATA  DELIMITED BY DELIM             EBCBS  
214110       INTO UNSTRING-RECORD (01)                                   EBCBS  
214111            UNSTRING-RECORD (02)                                   EBCBS  
214112            UNSTRING-RECORD (03)                                   EBCBS  
214113            UNSTRING-RECORD (04)                                   EBCBS  
214114            UNSTRING-RECORD (05)                                   EBCBS  
214115            UNSTRING-RECORD (06)                                   EBCBS  
214116            UNSTRING-RECORD (07)                                   EBCBS  
214117            UNSTRING-RECORD (08)                                   EBCBS  
214118            UNSTRING-RECORD (09)                                   EBCBS  
214119            UNSTRING-RECORD (10)                                   EBCBS  
214120            UNSTRING-RECORD (11)                                   EBCBS  
214121            UNSTRING-RECORD (12)                                   EBCBS  
214122            UNSTRING-RECORD (13)                                   EBCBS  
214123            UNSTRING-RECORD (14)                                   EBCBS  
214124            UNSTRING-RECORD (15)                                   EBCBS  
214125            UNSTRING-RECORD (16)                                   EBCBS  
214126            UNSTRING-RECORD (17)                                   EBCBS  
214127            UNSTRING-RECORD (18)                                   EBCBS  
214128            UNSTRING-RECORD (19)                                   EBCBS  
214129            UNSTRING-RECORD (20)                                   EBCBS  
214130            UNSTRING-RECORD (21)                                   EBCBS  
214131            UNSTRING-RECORD (22)                                   EBCBS  
214132            UNSTRING-RECORD (23)                                   EBCBS  
214133            UNSTRING-RECORD (24)                                   EBCBS  
214134            UNSTRING-RECORD (25)                                   EBCBS  
214135       TALLYING IN LN-UNSTRING-RECORDS                             EBCBS  
214136       ON OVERFLOW DISPLAY 'UNSTRING OVERFLOW ERROR'.              EBCBS  
XXXXXX*     DISPLAY 'UNS COUNT'  LN-UNSTRING-RECORDS.                    EBCBS  
214200*                                                                  EBCBS  
214201*********************************                                  EBCBS  
214202 99999-POP-FIRST-LN-ITEM.                                          EBCBS  
214203*                               *******************************    EBCBS  
214204*  EXPAND FIRST LINE ITEM INTO CS90 FORMAT.                   *    EBCBS  
214205***************************************************************    EBCBS  
ITS102       MOVE 1   TO WS-START-POS                                    EBCBS  
ITS102                   WS-END-POS,                                     EBCBS  
ITS102                   WS-STR-LEN.                                     EBCBS  
ITS102       MOVE SPACES TO WS-LINE-SPLT-NEEDED.                                
ITS102       MOVE SPACES TO CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT).              
214207       IF UNSTRING-RECORD (01) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214208         MOVE 'CHG=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102*        MOVEUTE WS-END-POS = FUNCTION                                    
ITS102         MOVE WS-KW-CHG-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
ITS102         MOVE 'CHG=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-CHG-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        IF  UNSTRING-RECORD (01) (1:10) NUMERIC                           
ITS102         IF  UNSTRING-RECORD (01) (1:2) = '00'                            
ITS102          MOVE 8 TO WS-STR-LEN                                            
214212          MOVE UNSTRING-RECORD (01) (3:08) TO                      EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                          (WS-START-POS:WS-STR-LEN)                       
ITS102         ELSE                                                             
ITS102          SET W88-LINE-SPLT-NEEDED TO TRUE                                
ITS102          MOVE 10 TO WS-STR-LEN                                           
214212          MOVE UNSTRING-RECORD (01) (1:10) TO                      EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102         END-IF                                                           
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102        END-IF                                                            
214215       END-IF.                                                     EBCBS  
214216*                                                                  EBCBS  
214217       IF UNSTRING-RECORD (02) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214218         MOVE 'HSC=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-HSC-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'HSC=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-HSC-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 8 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (02) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214225       END-IF.                                                     EBCBS  
214226*                                                                  EBCBS  
214227       IF UNSTRING-RECORD (03) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214228         MOVE 'POS=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-POS-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'POS=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-POS-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (03) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214235       END-IF.                                                     EBCBS  
214236*                                                                  EBCBS  
214237       IF UNSTRING-RECORD (04) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214238         MOVE 'SSDT= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-SSDT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214238         MOVE 'SSDT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-SSDT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 6 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (04) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214245       END-IF.                                                     EBCBS  
214246*                                                                  EBCBS  
214247       IF UNSTRING-RECORD (05) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214248         MOVE 'ESDT= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-ESDT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214248         MOVE 'ESDT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-ESDT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 6 TO WS-STR-LEN                                              
214252        MOVE UNSTRING-RECORD (05) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214255       END-IF.                                                     EBCBS  
214256*                                                                  EBCBS  
214257       IF UNSTRING-RECORD (06) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214258         MOVE 'SMC=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-SMC-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'SMC=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-SMC-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 3 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (06) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214265       END-IF.                                                     EBCBS  
214266*                                                                  EBCBS  
214267       IF UNSTRING-RECORD (07) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214268         MOVE 'UNT=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-UNIT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'UNT=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-UNIT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
              IF UNSTRING-RECORD (07) (1:1) = '0'                               
ITS102         MOVE 3 TO WS-STR-LEN                                             
214222         MOVE UNSTRING-RECORD (07)(2:3)  TO                        EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
ITS102         SET W88-LINE-SPLT-NEEDED TO TRUE                                 
ITS102         MOVE 4 TO WS-STR-LEN                                             
214222         MOVE UNSTRING-RECORD (07) TO                              EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
214275       END-IF.                                                     EBCBS  
214276*                                                                  EBCBS  
214277       IF UNSTRING-RECORD (08) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214278         MOVE 'PLO=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-PLO-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'PLO=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-PLO-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 2 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (08) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214285       END-IF.                                                     EBCBS  
214286*                                                                  EBCBS  
214287       IF UNSTRING-RECORD (09) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214288         MOVE 'DIAG= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-DIAG-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'DIAG= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-DIAG-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 5 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (09) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214295       END-IF.                                                     EBCBS  
214296*                                                                  EBCBS  
214297       IF UNSTRING-RECORD (10) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214298         MOVE 'PI=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-PI-LEN TO  WS-STR-LEN                                 
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'PI=   '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-PI-LEN + 1                           
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 2 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (10) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214305       END-IF.                                                     EBCBS  
214306*                                                                  EBCBS  
214307       IF UNSTRING-RECORD (11) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214308         MOVE 'OCAL= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-OCAL-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'OCAL= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-OCAL-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        IF  UNSTRING-RECORD (11) (1:2) = '00'                             
ITS102          MOVE 8 TO WS-STR-LEN                                            
214212          MOVE UNSTRING-RECORD (11) (3:08) TO                      EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                          (WS-START-POS:WS-STR-LEN)                       
ITS102        ELSE                                                              
ITS102          MOVE 10 TO WS-STR-LEN                                           
214212          MOVE UNSTRING-RECORD (11) (1:10) TO                      EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
214315       END-IF.                                                     EBCBS  
214316*                                                                  EBCBS  
214317       IF UNSTRING-RECORD (12) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214318         MOVE 'NTE=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-NTE-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'NTE=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-NTE-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 2 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (12) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214325       END-IF.                                                     EBCBS  
214326*                                                                  EBCBS  
214327       IF UNSTRING-RECORD (13) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214328         MOVE 'RSLT= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-RSLT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'RSLT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-RSLT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 4 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (13) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214335       END-IF.                                                     EBCBS  
214336*                                                                  EBCBS  
214337       IF UNSTRING-RECORD (14) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214338         MOVE 'PRVID='        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-PRVID-LEN TO  WS-STR-LEN                              
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'PRVID='        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-PRVID-LEN + 1                        
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 10 TO WS-STR-LEN                                             
214222        MOVE UNSTRING-RECORD (14) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214345       END-IF.                                                     EBCBS  
214346*                                                                  EBCBS  
214347       IF UNSTRING-RECORD (15) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214348         MOVE 'PART= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-PART-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'PART= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-PART-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (15) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214355       END-IF.                                                     EBCBS  
214356*                                                                  EBCBS  
214357       IF UNSTRING-RECORD (16) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214358         MOVE 'PRLOC='        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-PRLOC-LEN TO  WS-STR-LEN                              
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
214218         MOVE 'PRLOC='        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-PRLOC-LEN + 1                        
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 3 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (16) TO                               EBCBS  
ITS102           CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214365       END-IF.                                                     EBCBS  
XXXXXX*        DISPLAY 'LINE1- ARRAY'                                           
XXXXXX*          CSF-LINE1-BYTE (CLCSF-LINEITEM-COUNT).                         
214400*                                                                  EBCBS  
214401*********************************                                  EBCBS  
214402 99999-POP-SECOND-LN-ITEM.                                         EBCBS  
214403*                               *******************************    EBCBS  
214404*  EXPAND FIRST LINE ITEM INTO CS90 FORMAT.                   *    EBCBS  
214405***************************************************************    EBCBS  
214406*                                                                  EBCBS  
214407*      IF UNSTRING-RECORD (01) IS NOT EQUAL TO SPACES              EBCBS  
214408*        MOVE 'RMTH= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214409*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214410*                             TO                                   EBCBS  
214411*                           CLCSF-KW-RMTH (CLCSF-LINEITEM-COUNT)   EBCBS  
214412*        MOVE UNSTRING-RECORD (01)                                 EBCBS  
214413*                             TO                                   EBCBS  
214414*                           CLCSF-RMTH (CLCSF-LINEITEM-COUNT)      EBCBS  
214415*      END-IF.                                                     EBCBS  
214416*                                                                  EBCBS  
214417*      IF UNSTRING-RECORD (02) IS NOT EQUAL TO SPACES              EBCBS  
214418*        MOVE 'RMTV= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214419*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214420*                             TO                                   EBCBS  
214421*                           CLCSF-KW-RMTV (CLCSF-LINEITEM-COUNT)   EBCBS  
214422*        MOVE UNSTRING-RECORD (02)                                 EBCBS  
214423*                             TO                                   EBCBS  
214424*                           CLCSF-RMTV (CLCSF-LINEITEM-COUNT)      EBCBS  
214425*      END-IF.                                                     EBCBS  
214426*                                                                  EBCBS  
214427*      IF UNSTRING-RECORD (03) IS NOT EQUAL TO SPACES              EBCBS  
214428*        MOVE 'RI=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214429*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214430*                             TO                                   EBCBS  
214431*                           CLCSF-KW-RI (CLCSF-LINEITEM-COUNT)     EBCBS  
214432*        MOVE UNSTRING-RECORD (03)                                 EBCBS  
214433*                             TO                                   EBCBS  
214434*                           CLCSF-RI (CLCSF-LINEITEM-COUNT)        EBCBS  
214435*      END-IF.                                                     EBCBS  
214436*                                                                  EBCBS  
ITS102       MOVE 1   TO WS-START-POS                                    EBCBS  
ITS102                   WS-END-POS,                                     EBCBS  
ITS102                   WS-STR-LEN.                                     EBCBS  
ITS102       MOVE SPACES TO CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT).              
214437       IF UNSTRING-RECORD (01) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214438         MOVE 'INIT= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102*        MOVEUTE WS-END-POS = FUNCTION                                    
ITS102         MOVE WS-KW-INIT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
ITS102         MOVE 'INIT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-INIT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        IF  UNSTRING-RECORD (01) (1:10) NUMERIC                           
ITS102         IF  UNSTRING-RECORD (01) (1:2) = '00'                            
ITS102          MOVE 8 TO WS-STR-LEN                                            
214212          MOVE UNSTRING-RECORD (01) (3:08) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                          (WS-START-POS:WS-STR-LEN)                       
ITS102         ELSE                                                             
ITS102          MOVE 10 TO WS-STR-LEN                                           
214212          MOVE UNSTRING-RECORD (01) (1:10) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102         END-IF                                                           
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102        END-IF                                                            
214445       END-IF.                                                     EBCBS  
214446*                                                                  EBCBS  
214447*      IF UNSTRING-RECORD (05) IS NOT EQUAL TO SPACES              EBCBS  
214448*        MOVE 'NTC=  '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214449*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214450*                             TO                                   EBCBS  
214451*                           CLCSF-KW-NTC (CLCSF-LINEITEM-COUNT)    EBCBS  
214452*        MOVE UNSTRING-RECORD (05)                                 EBCBS  
214453*                             TO                                   EBCBS  
214454*                           CLCSF-NTC (CLCSF-LINEITEM-COUNT)       EBCBS  
214455*      END-IF.                                                     EBCBS  
214456*                                                                  EBCBS  
214457*      IF UNSTRING-RECORD (06) IS NOT EQUAL TO SPACES              EBCBS  
214458*        MOVE 'IS=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214459*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214460*                             TO                                   EBCBS  
214461*                           CLCSF-KW-IS (CLCSF-LINEITEM-COUNT)     EBCBS  
214462*        MOVE UNSTRING-RECORD (06)                                 EBCBS  
214463*                             TO                                   EBCBS  
214464*                           CLCSF-IS (CLCSF-LINEITEM-COUNT)        EBCBS  
214465*      END-IF.                                                     EBCBS  
214466*                                                                  EBCBS  
214467*      IF UNSTRING-RECORD (02) IS NOT EQUAL TO SPACES              EBCBS  
214468*        MOVE 'PM=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214469*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214470*                             TO                                   EBCBS  
214471*                           CLCSF-KW-PM (CLCSF-LINEITEM-COUNT)     EBCBS  
214472*        MOVE UNSTRING-RECORD (02)                                 EBCBS  
214473*                             TO                                   EBCBS  
214474*                           CLCSF-PM (CLCSF-LINEITEM-COUNT)        EBCBS  
214475*      END-IF.                                                     EBCBS  
214476*                                                                  EBCBS  
214477*      IF UNSTRING-RECORD (03) IS NOT EQUAL TO SPACES              EBCBS  
214478*        MOVE 'RL=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
214479*        MOVE LN-KEYWORD-BRKDWN                                    EBCBS  
214480*                             TO                                   EBCBS  
214481*                           CLCSF-KW-RL (CLCSF-LINEITEM-COUNT)     EBCBS  
214482*        MOVE UNSTRING-RECORD (03)                                 EBCBS  
214783*                             TO                                   EBCBS  
214484*                           CLCSF-RL (CLCSF-LINEITEM-COUNT)        EBCBS  
214485*      END-IF.                                                     EBCBS  
214486*                                                                  EBCBS  
214486*                                                                  EBCBS  
214487       IF UNSTRING-RECORD (04) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214488         MOVE 'SE=   '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-SE-LEN TO  WS-STR-LEN                                 
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'SE=   '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-SE-LEN + 1                           
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (04) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214495       END-IF.                                                     EBCBS  
214496*                                                                  EBCBS  
214497       IF UNSTRING-RECORD (05) IS NOT EQUAL TO SPACES OR           EBCBS  
                W88-LINE-SPLT-NEEDED                                            
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214498         MOVE 'SPLT= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-SPLT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102         MOVE 2 TO WS-STR-LEN                                             
214222         MOVE '00'                 TO                              EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'SPLT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-SPLT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102         MOVE 3 TO WS-STR-LEN                                             
214222         MOVE '000'                 TO                             EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
214505       END-IF.                                                     EBCBS  
214600*                                                                  EBCBS  
214507       IF UNSTRING-RECORD (06) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214508         MOVE 'ORIG= '        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
ITS102         MOVE WS-KW-ORIG-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102         MOVE 2 TO WS-STR-LEN                                             
214222         MOVE LRSP-CL-WORK-LN-NUM (2:2) TO                         EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'ORIG= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-ORIG-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102         MOVE 3 TO WS-STR-LEN                                             
214222         MOVE LRSP-CL-WORK-LN-NUM (1:3)   TO                       EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
214515       END-IF.                                                     EBCBS  
214600*                                                                  EBCBS  
214517       IF UNSTRING-RECORD (07) IS NOT EQUAL TO SPACES             39610012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'OCPLN= '       TO LN-KEYWORD-LN-KEYWORD            39620012
ITS102*        MOVEUTE WS-END-POS = FUNCTION                                    
ITS102         MOVE WS-KW-OCPLN-LEN TO  WS-STR-LEN                              
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
ITS102         MOVE 'OCPLN= '       TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-OCPLN-LEN + 1                        
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        IF  UNSTRING-RECORD (07) (1:10) NUMERIC                           
ITS102         IF  UNSTRING-RECORD (07) (1:2) = '00'                            
ITS102          MOVE 8 TO WS-STR-LEN                                            
214212          MOVE UNSTRING-RECORD (07) (3:08) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                          (WS-START-POS:WS-STR-LEN)                       
ITS102         ELSE                                                             
ITS102          MOVE 10 TO WS-STR-LEN                                           
214212          MOVE UNSTRING-RECORD (07) (1:10) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102         END-IF                                                           
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102        END-IF                                                            
214525       END-IF.                                                    39690012
214516*                                                                 39700012
214517       IF UNSTRING-RECORD (08) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'OCCV= '        TO LN-KEYWORD-LN-KEYWORD            39720012
ITS102         MOVE WS-KW-OCCV-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'OCCV= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-OCCV-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (08) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
214600*                                                                 39800012
214517       IF UNSTRING-RECORD (09) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'NDC=  '        TO LN-KEYWORD-LN-KEYWORD            39720012
ITS102         MOVE WS-KW-NDC-LEN TO  WS-STR-LEN                                
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'NDC=  '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-NDC-LEN + 1                          
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 11 TO WS-STR-LEN                                             
214222        MOVE UNSTRING-RECORD (09) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
214516*                                                                 39700012
214517       IF UNSTRING-RECORD (10) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE '837CHG='       TO LN-KEYWORD-LN-KEYWORD            39720012
ITS102*        MOVEUTE WS-END-POS = FUNCTION                                    
ITS102         MOVE WS-KW-837CHG-LEN TO  WS-STR-LEN                             
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        ELSE                                                              
ITS102         MOVE '837CHG='       TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-837CHG-LEN + 1                       
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN) TO              EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102        END-IF                                                            
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        IF  UNSTRING-RECORD (10) (1:10) NUMERIC                           
ITS102         IF  UNSTRING-RECORD (10) (1:2) = '00'                            
ITS102          MOVE 8 TO WS-STR-LEN                                            
214212          MOVE UNSTRING-RECORD (10) (3:08) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                          (WS-START-POS:WS-STR-LEN)                       
ITS102         ELSE                                                             
ITS102          MOVE 10 TO WS-STR-LEN                                           
214212          MOVE UNSTRING-RECORD (10) (1:10) TO                      EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
ITS102         END-IF                                                           
               ADD WS-STR-LEN TO WS-START-POS                                   
ITS102        END-IF                                                            
214525       END-IF.                                                    39790012
214516*                                                                 39700012
214517       IF UNSTRING-RECORD (11) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'LNCEX='       TO LN-KEYWORD-LN-KEYWORD             39720012
ITS102         MOVE WS-KW-LNCEX-LEN TO  WS-STR-LEN                              
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'LNCEX='        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-LNCEX-LEN + 1                        
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (11) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
214600*                                                                 39800012
214517       IF UNSTRING-RECORD (12) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'LNNT= '       TO LN-KEYWORD-LN-KEYWORD             39720012
ITS102         MOVE WS-KW-LNNT-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'LNNT= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-LNNT-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 1 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (12) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
214600*                                                                 39800012
214517       IF UNSTRING-RECORD (13) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'LLNB= '       TO LN-KEYWORD-LN-KEYWORD             39720012
ITS102         MOVE WS-KW-LLNB-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'LLNB= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-LLNB-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 3 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (13) (1:3) TO                         EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
XXXXXX*        DISPLAY 'LINE2- ARRAY'                                           
XXXXXX*          CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT).                         
214600*                                                                 39800012
214400*                                                                  EBCBS  
214401*********************************                                  EBCBS  
214402 99999-POP-THIRD-LN-ITEM.                                          EBCBS  
214403*                               *******************************    EBCBS  
214404*  EXPAND FIRST LINE ITEM INTO CS90 FORMAT.                   *    EBCBS  
214405***************************************************************    EBCBS  
214406*                                                                  EBCBS  
214517       IF UNSTRING-RECORD (01) IS NOT EQUAL TO SPACES             39710012
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
214518         MOVE 'CPT4= '       TO LN-KEYWORD-LN-KEYWORD             39720012
ITS102         MOVE WS-KW-CPT4-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'CPT4= '        TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-CPT4-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 8 TO WS-STR-LEN                                              
214222        MOVE UNSTRING-RECORD (01) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
214525       END-IF.                                                    39790012
214600*                                                                 39800012
113822       IF UNSTRING-RECORD (02) IS NOT EQUAL TO SPACES              EBCBS  
214009        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
113822         MOVE 'LNPI=  '        TO LN-KEYWORD-LN-KEYWORD            EBCBS  
ITS102         MOVE WS-KW-LNPI-LEN TO  WS-STR-LEN                               
214209         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ELSE                                                              
214228         MOVE 'LNPI=  '       TO LN-KEYWORD-LN-KEYWORD-B           EBCBS  
ITS102         COMPUTE WS-STR-LEN =  WS-KW-LNPI-LEN + 1                         
214209         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
ITS102         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
ITS102                         (WS-START-POS:WS-STR-LEN)                        
214235        END-IF                                                     EBCBS  
              ADD WS-STR-LEN TO WS-START-POS                                    
ITS102        MOVE 10 TO WS-STR-LEN                                             
214222        MOVE UNSTRING-RECORD (02) TO                               EBCBS  
ITS102           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
ITS102                         (WS-START-POS:WS-STR-LEN)                        
              ADD WS-STR-LEN TO WS-START-POS                                    
113822       END-IF.                                                     EBCBS  
175203       IF UNSTRING-RECORD (03) IS NOT EQUAL TO SPACES              EBCBS  
175203        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
175203         MOVE 'M2='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
175203         MOVE WS-KW-M2-LEN TO  WS-STR-LEN                                 
175203         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
175203         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
175203                         (WS-START-POS:WS-STR-LEN)                        
175203        ELSE                                                              
175203         MOVE 'M2='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
175203         COMPUTE WS-STR-LEN =  WS-KW-M2-LEN + 1                           
175203         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
175203         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
175203                         (WS-START-POS:WS-STR-LEN)                        
175203        END-IF                                                     EBCBS  
175203        ADD WS-STR-LEN TO WS-START-POS                                    
175203        MOVE 02 TO WS-STR-LEN                                             
175203        MOVE UNSTRING-RECORD (03) TO                               EBCBS  
175203           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
175203                         (WS-START-POS:WS-STR-LEN)                        
175203        ADD WS-STR-LEN TO WS-START-POS                                    
175203       END-IF.                                                     EBCBS  
172756                                                                          
172756       IF UNSTRING-RECORD (04) IS NOT EQUAL TO SPACES              EBCBS  
172756        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
172756         MOVE 'IG='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
172756         MOVE WS-KW-IG-LEN TO  WS-STR-LEN                                 
172756         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ELSE                                                              
172756         MOVE 'IG='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
172756         COMPUTE WS-STR-LEN =  WS-KW-IG-LEN + 1                           
172756         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        END-IF                                                     EBCBS  
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756        MOVE 02 TO WS-STR-LEN                                             
172756        MOVE UNSTRING-RECORD (04) TO                               EBCBS  
172756           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756       END-IF                                                      EBCBS  
172756                                                                          
172756       IF UNSTRING-RECORD (05) IS NOT EQUAL TO SPACES              EBCBS  
172756        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
172756         MOVE 'PM='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
172756         MOVE WS-KW-PM-LEN TO  WS-STR-LEN                                 
172756         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ELSE                                                              
172756         MOVE 'PM='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
172756         COMPUTE WS-STR-LEN =  WS-KW-PM-LEN + 1                           
172756         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        END-IF                                                     EBCBS  
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756        MOVE 02 TO WS-STR-LEN                                             
172756        MOVE UNSTRING-RECORD (05) TO                               EBCBS  
172756           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756       END-IF                                                      EBCBS  
172756                                                                          
172756       IF UNSTRING-RECORD (06) IS NOT EQUAL TO SPACES              EBCBS  
172756        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
172756         MOVE 'RL='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
172756         MOVE WS-KW-RL-LEN TO  WS-STR-LEN                                 
172756         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ELSE                                                              
172756         MOVE 'RL='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
172756         COMPUTE WS-STR-LEN =  WS-KW-RL-LEN + 1                           
172756         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
172756         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        END-IF                                                     EBCBS  
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756        MOVE 03 TO WS-STR-LEN                                             
172756        MOVE UNSTRING-RECORD (06) TO                               EBCBS  
172756           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
172756                         (WS-START-POS:WS-STR-LEN)                        
172756        ADD WS-STR-LEN TO WS-START-POS                                    
172756       END-IF.                                                     EBCBS  
257760       IF UNSTRING-RECORD (07) IS NOT EQUAL TO SPACES              EBCBS  
257760        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
257760         MOVE 'LDX10='        TO LN-KEYWORD-LN-KEYWORD             EBCBS  
257760         MOVE WS-KW-LDX-LEN TO  WS-STR-LEN                                
257760         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
257760         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
257760                         (WS-START-POS:WS-STR-LEN)                        
257760        ELSE                                                              
257760         MOVE 'LDX10='    TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
257760         COMPUTE WS-STR-LEN =  WS-KW-LDX-LEN + 1                          
257760         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
257760         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
257760                         (WS-START-POS:WS-STR-LEN)                        
257760        END-IF                                                     EBCBS  
257760        ADD WS-STR-LEN TO WS-START-POS                                    
257760        MOVE 07 TO WS-STR-LEN                                             
257760        MOVE UNSTRING-RECORD (07) TO                               EBCBS  
257760           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
257760                         (WS-START-POS:WS-STR-LEN)                        
257760        ADD WS-STR-LEN TO WS-START-POS                                    
257760       END-IF.                                                     EBCBS  
430147       IF UNSTRING-RECORD (08) IS NOT EQUAL TO SPACES              EBCBS  
430147        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
430147         MOVE 'M3='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
430147         MOVE WS-KW-M3-LEN TO  WS-STR-LEN                                 
430147         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
430147         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        ELSE                                                              
430147         MOVE 'M3='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
430147         COMPUTE WS-STR-LEN =  WS-KW-M3-LEN + 1                           
430147         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
430147         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        END-IF                                                     EBCBS  
430147        ADD WS-STR-LEN TO WS-START-POS                                    
430147        MOVE 02 TO WS-STR-LEN                                             
430147        MOVE UNSTRING-RECORD (08) TO                               EBCBS  
430147           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        ADD WS-STR-LEN TO WS-START-POS                                    
430147       END-IF.                                                     EBCBS  
430147       IF UNSTRING-RECORD (09) IS NOT EQUAL TO SPACES              EBCBS  
430147        IF  LRSP-CL-WORK-LN-NUM < 100                              EBCBS  
430147         MOVE 'M4='        TO LN-KEYWORD-LN-KEYWORD                EBCBS  
430147         MOVE WS-KW-M4-LEN TO  WS-STR-LEN                                 
430147         MOVE LN-KEYWORD-BRKDWN (1:WS-STR-LEN) TO                  EBCBS  
430147         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        ELSE                                                              
430147         MOVE 'M4='       TO LN-KEYWORD-LN-KEYWORD-B               EBCBS  
430147         COMPUTE WS-STR-LEN =  WS-KW-M4-LEN + 1                           
430147         MOVE LN-KEYWORD-BRKDWN-BIG (1:WS-STR-LEN)   TO            EBCBS  
430147         CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                            
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        END-IF                                                     EBCBS  
430147        ADD WS-STR-LEN TO WS-START-POS                                    
430147        MOVE 02 TO WS-STR-LEN                                             
430147        MOVE UNSTRING-RECORD (09) TO                               EBCBS  
430147           CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT)                          
430147                         (WS-START-POS:WS-STR-LEN)                        
430147        ADD WS-STR-LEN TO WS-START-POS                                    
430147       END-IF.                                                     EBCBS  
XXXXXX*        DISPLAY 'LINE2- ARRAY'                                           
XXXXXX*          CSF-LINE2-BYTE (CLCSF-LINEITEM-COUNT).                         
214601*********************************                                  EBCBS  
214602 99999-WRITE-BATCH-KEYWORD-RCDS.                                   EBCBS  
214603*                               *******************************    EBCBS  
214604*  WRITE OUT THE CS90 256 BYTE KEYWORD.                       *    EBCBS  
214605***************************************************************    EBCBS  
214606*                                                                  EBCBS  
214607       PERFORM                                                     EBCBS  
214608         TEST AFTER                                                EBCBS  
214609         VARYING ITS-CL-TRAN-PTR FROM 1 BY 1                       EBCBS  
214610         UNTIL ITS-CL-TRAN-PTR IS EQUAL TO ITS-CL-TRAN-COUNT       EBCBS  
214611         WRITE BATCH-KEYWORD-REC FROM                              EBCBS  
214612                         ITS-CL-TABLE-REC (ITS-CL-TRAN-PTR)        EBCBS  
214613         ADD 1                          TO EMC-FILE-RECORD-CNT     EBCBS  
214614       END-PERFORM.                                                EBCBS  
214615*                                                                  EBCBS  
DIS: *****   DISPLAY ' GOING TO WRITE CNT' CLCSF-LINEITEM-COUNT                 
214616       PERFORM                                                     EBCBS  
214617         TEST AFTER                                                EBCBS  
214618         VARYING CLCSF-LINEITEM-PTR FROM 1 BY 1                    EBCBS  
214619         UNTIL CLCSF-LINEITEM-PTR IS EQUAL TO                      EBCBS  
214620                              CLCSF-LINEITEM-COUNT                 EBCBS  
214621         IF CLCSF-LINEITEM-BRKDWN-1 (CLCSF-LINEITEM-PTR)           EBCBS  
214622                                          IS NOT EQUAL TO SPACES   EBCBS  
214623           WRITE BATCH-KEYWORD-REC FROM                            EBCBS  
214624                    CSF-LINE1-BYTE (CLCSF-LINEITEM-PTR)            EBCBS  
214625           ADD 1                        TO EMC-FILE-RECORD-CNT     EBCBS  
214626         END-IF                                                    EBCBS  
214627*                                                                  EBCBS  
DIS: *****     DISPLAY ' COUNT IS;' CLCSF-LINEITEM-PTR                          
214628         IF CLCSF-LINEITEM-BRKDWN-2 (CLCSF-LINEITEM-PTR)           EBCBS  
214629                                          IS NOT EQUAL TO SPACES   EBCBS  
214630           WRITE BATCH-KEYWORD-REC FROM                            EBCBS  
214624                    CSF-LINE2-BYTE (CLCSF-LINEITEM-PTR)            EBCBS  
214631*                   CLCSF-LINEITEM-BRKDWN-2 (CLCSF-LINEITEM-PTR)   EBCBS  
214632           ADD 1                        TO EMC-FILE-RECORD-CNT     EBCBS  
214633         END-IF                                                    EBCBS  
214634       END-PERFORM.                                                EBCBS  
300000*                                                                  EBCBS  
300001*********************************                                  EBCBS  
300002 99999-COMPUTE-CS90-CLMID.                                         EBCBS  
300003*                               *******************************    EBCBS  
300004*  FORMULATE THE CS90 CLAIM NUMBER FROM SF SERIAL NUMBER.     *    EBCBS  
300005***************************************************************    EBCBS  
300006*                                                                  EBCBS  
300007     INITIALIZE                      WS-DATE-WORK-FIELD.           EBCBS  
300008*                                                                  EBCBS  
300009     MOVE WS-JUL-DATE-YEAR        TO WS-CLAIM-YEAR.                EBCBS  
300010*                                                                  EBCBS  
300011     COMPUTE WS-DATE-WORK-FIELD = (WS-JUL-DATE-DA + 366).          EBCBS  
300014*                                                                  EBCBS  
300020     MOVE WS-DATE-WORK-FIELD-2-4  TO WS-CLAIM-DATE.                EBCBS  
300030*                                                                  EBCBS  
300015     ADD 1                        TO WS-CLAIM-SEQ-NBR.             EBCBS  
300032*                                                                  EBCBS  
300033     MOVE 0                       TO WS-CLAIM-END-NBR.             EBCBS  
900000*                                                                  EBCBS  
400002****************************                                      34442003
400003*S101-HST-CLM-VRS.                                                34443003
400004*                          ************************************   34444003
400005*                                                             *   34445003
400006***************************************************************   34446003
400007*                                                                 34447003
400008     COPY S9CLMVRS.                                                       
400009*                                                                 34456603
400010     EJECT                                                                
400100*                                                                 34456903
400101****************************                                      34457003
400102*S101-HST-CLL-VRS.                                                34457103
400103*                          ************************************   34458003
400105*                                                             *   34459003
400106***************************************************************   34460003
400107*                                                                 34461003
400108     COPY S9CLLVRS.                                                       
400109*                                                                 34465003
900000*                                                                  EBCBS  
900001*********************************                                  EBCBS  
900002 99999-TAKE-A-CHECK-POINT.                                         EBCBS  
900003*                               *******************************    EBCBS  
900004*  TAKE A CHECK POINT TO ALLOW ONLINE IMS TO BACK-OUT AUTO,   *    EBCBS  
900005***************************************************************    EBCBS  
900006*                                                                  EBCBS  
900007     CALL 'CBLTDLI'            USING CHKP-FUNCTION,                EBCBS  
900008                                     PCB-DUMMY,                           
900009                                     IO-AREALEN.                          
999996*                                                                  EBCBS  
ITS113                                                                          
ITS113 0500-SETUP-PROV-MAPPING-TABLE.                                           
ITS113                                                                          
ITS113     MOVE  ZEROS          TO        ENC-RET-CD                            
ITS113                                    ED-IX-N                               
ITS113                                    WS-TC.                                
ITS113     MOVE  'CTUFCMAP'     TO        ED-TABLE.                             
ITS113     PERFORM  050A-READ-CTUFCMAP THRU 050A-EXIT                           
ITS113       UNTIL  ENC-RET-CD         NOT  = ZEROS.                            
ITS113                                                                          
ITS113                                                                          
ITS113 0500-EXIT.                                                               
ITS113     EXIT.                                                                
ITS113 050A-READ-CTUFCMAP.                                                      
ITS113                                                                          
ITS113     ADD 1  TO WS-TC GIVING ED-IX-N.                                      
ITS113                                                                          
ITS113     CALL ED-TABLE              USING  ED-ARGUE                           
ITS113                                       ED-IX-N                            
ITS113                                       B6                                 
ITS113                                       ENC-RET-CD.                        
ITS113                                                                          
ITS113     IF ENC-RET-CD         = ZEROS                                        
ITS113                                                                          
ITS113         IF ED-FL          = SPACES                                       
ITS113          OR WS-TC         > 998                                          
ITS113                                                                          
ITS113            MOVE  1       TO ENC-RET-CD                                   
ITS113         ELSE                                                             
ITS113                                                                          
ITS113         PERFORM 050B-FILL-CTUFCTBL THRU 050B-EXIT                        
ITS113                                                                          
ITS113         END-IF                                                           
ITS113                                                                          
ITS113     END-IF.                                                              
ITS113                                                                          
ITS113 050A-EXIT.                                                               
ITS113     EXIT.                                                                
ITS113                                                                          
ITS113 050B-FILL-CTUFCTBL.                                                      
ITS113                                                                          
ITS113     ADD    1             TO         WS-TC.                               
ITS113     MOVE   ED-FL         TO         WS-FAC-LOW   (WS-TC).                
ITS113     MOVE   ED-FH         TO         WS-FAC-HI    (WS-TC).                
ITS113     MOVE   ED-R1         TO         WS-RULE-1    (WS-TC).                
ITS113     MOVE   ED-R2         TO         WS-RULE-2    (WS-TC).                
ITS113     MOVE   ED-PROV       TO         WS-FC-PROV   (WS-TC).                
ITS113     MOVE   ED-CK         TO         WS-FC-CK     (WS-TC).                
ITS113                                                                          
ITS113 050B-EXIT.                                                               
ITS113     EXIT.                                                                
ITS113                                                                          
ITS113 050C-UPDATE-CTUFCTBL.                                                    
ITS113                                                                          
ITS113     MOVE   1             TO         WS-FIRST-MATCH.                      
ITS113                                                                          
ITS113     PERFORM VARYING IX FROM 1 BY 1                                       
ITS113         UNTIL IX > WS-TC                                                 
ITS113                                                                          
ITS113            ADD 1 TO IX GIVING WS-NEXT-MATCH (IX)                         
ITS113*****  DISPLAY WS-FAC-LOW (IX)                                            
ITS113*****          WS-FAC-HI  (IX)                                            
ITS113*****          WS-RULE-1  (IX)                                            
ITS113*****          WS-RULE-2   (IX)                                           
ITS113*****          WS-FC-PROV    (IX)                                         
ITS113*****          WS-FC-CK      (IX)                                         
ITS113*****          WS-NEXT-MATCH (IX)                                         
ITS113*****          ':'  IX                                                    
ITS113*****                                                                     
ITS113     END-PERFORM.                                                         
ITS113                                                                          
ITS113 050C-EXIT.                                                               
ITS113     EXIT.                                                                
ITS113                                                                          
999996*                                                                  EBCBS  
999997***************************************************************    EBCBS  
999998*   THATS ALL FOLKS ......                                    *    EBCBS  
999999***************************************************************    EBCBS  
