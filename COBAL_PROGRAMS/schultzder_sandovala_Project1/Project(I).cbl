       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROJECT1.
       Author. Derek R Schultz Andre J Sandoval.
       Date-Written. 3/21/2018.
       Date-Compiled. 4/2/2018.
      ******************************************************************
      * This program will read from an input file and then produce a 
      * document in a cetain format. The input file contains information 
      * pertaining to Bon Voyage Travel Agency's sales trips. The output
      * will produce a formated document while calculating the commision 
      * for the sales person                                            
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
      * Select statment for record output file.
        
       SELECT OUTPUT-FILE-REPORT
                ASSIGN TO UT-SYS-OUTVFILERPT
                ORGANIZATION IS LINE SEQUENTIAL.
                
      * Select statment for the data output file
        
       SELECT OUTPUT-FILE-DATA 
                ASSIGN TO UT-SYS-OUTVFILEDAT
                ORGANIZATION IS SEQUENTIAL.
                
      * Beginning of data division. 
        
       DATA DIVISION.
       FILE SECTION.
       
      * File decription for data output file
        
       FD  OUTPUT-FILE-DATA
           RECORD CONTAINS 67 CHARACTERS.
       01 CUST-NO                            PIC X(5).
       01 CUST-NAME                          PIC X(25).
       01 UNIT-PRICE                         PIC 9999V99.
       01 QNTY-SOLD                          PIC 9999. 
       01 TOTAL-SALE                         PIC 9999V99.
       01 SALES-TAX                          PIC 99V99.
       01 FINAL-SALE                         PIC 9999V99.
       
       
       FD  OUTPUT-FILE-REPORT
           RECORD CONTAINS 69 CHARACTERS.
       01  OUTPUT-RECORD-REPORT                     PIC X(69).
       
      * Working storage variables.
        
       WORKING-STORAGE SECTION.
       
       01 WS-VARS.
          05 WS-PAGE-NO                      PIC 999   VALUE 1.
          05 WS-DETAILS-START                PIC 999   VALUE 8.
          05 WS-LINE-COUNT                   PIC 999.
          05 WS-CUR-DATE.
             10 CUR-YEAR                     PIC 9999.
             10 CUR-DAY                      PIC 99.
             10 CUR-MONTH                    PIC 99. 
          05 WS-FULL-PAGE                    PIC 99    VALUE 55.
          05 WS-CLIENT-COUNT                 PIC 999.
          05 WS-TOTAL-SALE                   PIC 9999V99.
          05 WS-SALES-TAX-CONST              PIC 9V999 VALUE 0.065.
          05 WS-SALES-TAX                    PIC 9999V99.
          05 WS-SALES-TAX-SUM                PIC 9999V99.
          05 WS-TOTAL-SALES-SUM              PIC 9999V99.
          05 WS-CLIENT-NO                    PIC XXXXX.
          05 WS-CLIENT-NAME                  PIC X(25).
          05 WS-UNIT-PRICE                   PIC 9999V99.
          05 WS-QUANTITY-SOLD                PIC 9999.
          05 WS-ADD-REC-FILE                 PIC XXX.
          05 WS-EOF-FLAG                     PIC XXX  VALUE "YES".
       
      * Heading 1
        
       01 HEADING1.
          05                                 PIC X(13)
                                             VALUE "DEREK SCHULTZ".
          05                                 PIC X(6)  VALUE SPACES.
          05                                 PIC X(18) 
                                             VALUE "REPORT MASTER FILE".
          05                                 PIC X(24) VALUE SPACES.
          05                                 PIC X(4)  VALUE "PAGE".
          05                                 PIC X     VALUE SPACES.
          05 H-PAGE-NO                       PIC Z99.
          
      * Heading 2   
          
       01 HEADING2. 
          05                                 PIC X(14) 
                                             VALUE "ANDRE SANDOVAL".
          05                                 PIC X(55) VALUE SPACES.
       
      * Heading 3
       
       01 HEADING3.
          05                                 PIC X(59) VALUE SPACES.
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          
      * Heading 4   
          
       01 HEADING4. 
          05                                 PIC X(69) VALUE SPACES.
          
      * Heading 5   
          
       01 HEADING5.
          05                                 PIC X(6)  VALUE "CLIENT".
          05                                 PIC X(3)  VALUE SPACES.
          05                                 PIC X(6)  VALUE "CLIENT".
          05                                 PIC X(18) VALUE SPACES.
          05                                 PIC X(10) 
                                             VALUE "UNIT PRICE".
          05                                 PIC X(3)  VALUE SPACES.
          05                                 PIC X(8)  VALUE "QUANTITY".
          05                                 PIC X(3)  VALUE SPACES.
          05                                 PIC X(11) 
                                             VALUE "TOTAL SALE".
      * Heading 6                                      
                                             
       01 HEADING6.
          05                                 PIC XX    VALUE SPACES.
          05                                 PIC XX    VALUE "NO".
          05                                 PIC X(6)  VALUE SPACES.
          05                                 PIC X(4)  VALUE "NAME".
          05                                 PIC X(35) VALUE SPACES.
          05                                 PIC X(4)  VALUE "SOLD".
          05                                 PIC X(16) VALUE SPACES.
          
      * Heading 7   
          
       01 HEADING7.
          05                                 PIC X(69) VALUE SPACES.
         
      * Details 1  
         
       01 DETAILS1.
          05 D-CLIENT-NO                     PIC X(5).
          05                                 PIC X(4)  VALUE SPACES.
          05 D-CLIENT-NAME                   PIC X(25).
          05                                 PIC X(3)  VALUE SPACES.
          05 D-UNIT-PRICE                    PIC $,$$$.99.
          05                                 PIC X(6)  VALUE SPACES.
          05 D-QUANTITY-SOLD                 PIC ZZZ9.
          05                                 PIC X(5)  VALUE SPACES.
          05 D-TOTAL-SALE                    PIC $,$$$.99.
          05                                 PIC X     VALUE SPACES.
          
      * Footer 1   
          
       01 FOOTER1.
          05                                 PIC X(21)
                                             VALUE "****CLIENTS ".
          05 NO-OF-CLIENTS                   PIC ZZ9.
          05                                 PIC X(4)  VALUE "****".
          05                                 PIC X(10) VALUE SPACES.
          05                                 PIC X(10) 
                                             VALUE "SALES TAX ".
          05 F-SALES-TAX                     PIC $,$$$.99.
          05                                 PIC X(2)  VALUE SPACES.
          05                                 PIC X(11)
                                             VALUE "FINAL SALE ".
          05 F-FINAL-SALE                    PIC $,$$$.99.
       
      * Files
        
       01 WS-FILES.
          05 UT-SYS-OUTVFILERPT              PIC X(60)
          VALUE "J:\tirpoutRPT.doc".
          05 UT-SYS-OUTVFILEDAT              PIC X(60)
          VALUE "J:\tipoutDAT.txt".
          
      * Start of procedure division
        
       PROCEDURE DIVISION.
       
      ******************************************************************
      * 000-MAIN-MODULE RUNS ALL THE KEY MODULES TO PERFORM THE PROGRAMS 
      * PURPOSE.
      ******************************************************************
       
       000-MAIN-MODULE.
           PERFORM 100-INITIALIZATION-OPEN THRU 100-EXIT
           PERFORM 200-WRITE-HEADER THRU 200-EXIT
           PERFORM 400-PROMPT-USER THRU 400-EXIT
               UNTIL WS-EOF-FLAG = "NO"
       STOP RUN.
          
                                            
          
      ******************************************************************
      * 100-INITIALIZATION-OPEN SIMPLY OPENS BOTH OUTPUTFILES FOR 
      * READING. ALSO CONTAINS CODE TO STEP UP THE CURRENT DATE.
      ******************************************************************
       100-INITIALIZATION-OPEN. 
           OPEN OUTPUT OUTPUT-FILE-DATA 
           OPEN OUTPUT OUTPUT-FILE-REPORT
           MOVE FUNCTION CURRENT-DATE TO WS-CUR-DATE
           MOVE CUR-YEAR TO H-YEAR
           MOVE CUR-DAY TO H-DAY
           MOVE CUR-MONTH TO H-MONTH.
       100-EXIT.
 
      ******************************************************************
      * 200-WRITE-HEADER WRITES THE OUTPUT RECORD FROM THE HEADERS SET
      * UP IN THE WORKING STORAGE SECTION. AT THE BEGGINING IT MOVES 
      * THE PAGE NUMBER TO THE OUTPUT PAGE NUMBER VARIABLE, AND AT THE 
      * END IT MOVES THE 'DETAILS START' TO THE LINE COUNT SO THAT THE
      * DETAILS ARE BEGGINING ON THE CORRECT LINE NUMBER.
      ******************************************************************
       200-WRITE-HEADER.
           MOVE WS-PAGE-NO TO H-PAGE-NO
           WRITE OUTPUT-RECORD-REPORT FROM HEADING1
           WRITE OUTPUT-RECORD-REPORT FROM HEADING2
           WRITE OUTPUT-RECORD-REPORT FROM HEADING3
           WRITE OUTPUT-RECORD-REPORT FROM HEADING4
           WRITE OUTPUT-RECORD-REPORT FROM HEADING5
           WRITE OUTPUT-RECORD-REPORT FROM HEADING6
           WRITE OUTPUT-RECORD-REPORT FROM HEADING7
           MOVE SPACES TO OUTPUT-RECORD-REPORT
           WRITE OUTPUT-RECORD-REPORT
           ADD 1 TO WS-PAGE-NO
           MOVE WS-DETAILS-START TO WS-LINE-COUNT.
       200-EXIT.
       
       
      ******************************************************************
      * 400-PROMPT-USER ASKS USER TO ENTER IN THE FIRST FOUR DATA
      * MEMBERS FOR THE OUTPUTDATA. STORES IT IN WORKING STORAGE 
      * VARIABLES 
      ******************************************************************
       400-PROMPT-USER.
           DISPLAY "ENTER CLIENT NUMBER"
           ACCEPT WS-CLIENT-NO
           DISPLAY "ENTER CLIENT NAME"
           ACCEPT WS-CLIENT-NAME
           DISPLAY "ENTER UNIT PRICE"
           ACCEPT WS-UNIT-PRICE
           DISPLAY "ENTER QUANTITY SOLD"
           ACCEPT WS-QUANTITY-SOLD
           DISPLAY "ADD RECORD TO FILE?  (YES/N0)"
           ACCEPT WS-ADD-REC-FILE
           DISPLAY "ENTER MORE RECORDS?  (YES/NO)"
           ACCEPT WS-EOF-FLAG.
       400-EXIT.
       
      ******************************************************************
      * 
       
  