       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROJECT1.
       Author. Derek R Schultz Andre J Sandoval.
       Date-Written. 3/21/2018.
       Date-Compiled. 4/2/2018.
      ******************************************************************
      * This program is interactive and will take in data from the user.
      * The data entered will be processed and then formatted into both
      * a report file and a sequential file. The data entered will be 
      * a client number, client name, unit price, and quantity sold. 
      * The data calculated will be the total sale, sales tax, and the
      * final sale. Running totals will be kept of the sales tax, number
      * of clients, and the final sale.  
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
       01 OUTPUT-RECORD-DATA.
          05 O-CLIENT-NO                         PIC X(4).
          05                                     PIC X VALUE SPACES.
          05 O-CLIENT-NAME                       PIC X(25).
          05 O-UNIT-PRICE                        PIC 9(4)V99.
          05                                     PIC X(3) VALUE SPACES.
          05 O-QUANTITY-SOLD                     PIC S9999.
          05                                     PIC X(7) VALUE SPACES.
          05 O-TOTAL-SALE                        PIC S9(4)V99.
          05 O-SALES-TAX                         PIC S9(3)V99.
          05 O-FINAL-SALE                        PIC S9(4)V99.
          
       
       
       FD  OUTPUT-FILE-REPORT
           RECORD CONTAINS 99 CHARACTERS.
       01  OUTPUT-RECORD-REPORT                     PIC X(99).
       
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
          05 WS-TOTAL-SALE                   PIC S9999V99.
          05 WS-SALES-TAX-CONST              PIC 9V999 VALUE 0.065.
          05 WS-SALES-TAX                    PIC S9999V99.
          05 WS-SALES-TAX-SUM                PIC S9999V99.
          05 WS-TOTAL-SALES-SUM              PIC S9999V99.
          05 WS-CLIENT-NO                    PIC X(5).
          05 WS-CLIENT-NAME                  PIC X(25).
          05 WS-UNIT-PRICE                   PIC 9999V99.
          05 WS-QUANTITY-SOLD                PIC S9999.
          05 WS-ADD-REC-FILE                 PIC XXX.
          05 WS-EOF-FLAG                     PIC XXX VALUE "YES".
          05 WS-FINAL-SALE                   PIC S9999V99.
          
       
      * Heading 1
        
       01 HEADING1.
          05                                 PIC X(13)
                                             VALUE "DEREK SCHULTZ".
          05                                 PIC X(30)  VALUE SPACES.
          05                                 PIC X(18) 
                                             VALUE "REPORT MASTER FILE".
          05                                 PIC X(29) VALUE SPACES.
          05                                 PIC X(4)  VALUE "PAGE".
          05                                 PIC X     VALUE SPACES.
          05 H-PAGE-NO                       PIC Z99.
          
      * Heading 2   
          
       01 HEADING2. 
          05                                 PIC X(14) 
                                             VALUE "ANDRE SANDOVAL".
          05                                 PIC X(85) VALUE SPACES.
       
      * Heading 3
       
       01 HEADING3.
          05                                 PIC X(89) VALUE SPACES.
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          
      * Heading 4   
          
       01 HEADING4. 
          05                                 PIC X(99) VALUE SPACES.
          
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
          05                                 PIC X(2) VALUE SPACES.
          05                                 PIC X(9) VALUE "SALES TAX".
          05                                 PIC X(2) VALUE SPACES.
          05                                 PIC X(10) 
                                             VALUE "FINAL SALE".
          05                                 PIC X(2) VALUE SPACES.
          05                                 PIC X(6) VALUE "ADDED?".
      * Heading 6                                      
                                             
       01 HEADING6.
          05                                 PIC XX    VALUE SPACES.
          05                                 PIC XX    VALUE "NO".
          05                                 PIC X(6)  VALUE SPACES.
          05                                 PIC X(4)  VALUE "NAME".
          05                                 PIC X(35) VALUE SPACES.
          05                                 PIC X(4)  VALUE "SOLD".
          05                                 PIC X(23) VALUE SPACES.
          
      * Heading 7   
          
       01 HEADING7.
          05                                 PIC X(99) VALUE SPACES.
         
      * Details 1  
         
       01 DETAILS1.
          05 D-CLIENT-NO                     PIC X(5).
          05                                 PIC X(4)  VALUE SPACES.
          05 D-CLIENT-NAME                   PIC X(23).
          05                                 PIC X(2)  VALUE SPACES.
          05 D-UNIT-PRICE                    PIC -$$,$$$.99.
          05                                 PIC X(5)  VALUE SPACES.
          05 D-QUANTITY-SOLD                 PIC -ZZZ9.
          05                                 PIC X(3)  VALUE SPACES.
          05 D-TOTAL-SALE                    PIC -$$,$$9.99.
          05                                 PIC X(3)  VALUE SPACES.
          05 D-SALES-TAX                     PIC -$,$$9.99.
          05                                 PIC X(2)  VALUE SPACES.
          05 D-FINAL-SALE                    PIC -$$$,$$9.99.
          05                                 PIC X(4)  VALUE SPACES.
          05 D-ADDED                         PIC XXX.
       
      * Footer
        
       01 FOOTER.
          05                                 PIC X(99) VALUE SPACES.
          
      * Footer 1   
          
       01 FOOTER1.
          05                                 PIC X(12)
                                             VALUE "****CLIENTS ".
          05 NO-OF-CLIENTS                   PIC ZZ9.
          05                                 PIC X(4)  VALUE "****".
          05                                 PIC X(10) VALUE SPACES.
          05                                 PIC X(15) 
                                             VALUE "TOAL SALES TAX ".
          05 F-SALES-TAX                     PIC -$,$$9.99.
          05                                 PIC X(2)  VALUE SPACES.
          05                                 PIC X(16)
                                             VALUE "TOAL FINAL SALE ".
          05 F-FINAL-SALE                    PIC -$$$,$$9.99.
       
      * Files
        
       01 WS-FILES.
          05 UT-SYS-OUTVFILERPT              PIC X(76)
          VALUE "C:\COBOL\tirpoutRPT.txt".
          05 UT-SYS-OUTVFILEDAT              PIC X(76)
          VALUE "C:\COBOL\outDAT.txt".
          
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
           PERFORM 900-WRITE-FOOTER THRU 900-EXIT
           PERFORM 950-TERMINATION-MODULE THRU 950-EXIT
              
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
           MOVE CUR-MONTH TO H-MONTH
           MOVE CUR-DAY TO H-DAY.
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
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD-REPORT FROM HEADING2
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD-REPORT FROM HEADING3
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD-REPORT FROM HEADING4
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD-REPORT FROM HEADING5
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD-REPORT FROM HEADING6
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD-REPORT FROM HEADING7
            AFTER ADVANCING 1 LINE
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
           DISPLAY "ADD RECORD TO FILE?  (YES/NO)"
           ACCEPT WS-ADD-REC-FILE
           DISPLAY "ENTER MORE RECORDS?  (YES/NO)"
           ACCEPT WS-EOF-FLAG
           PERFORM 600-DETAIL THRU 600-EXIT.
       400-EXIT.
       
       
      ******************************************************************
      * 600-DETAIL WRITES THE DETAILS OF THE TRANSACTIONS INCLUDING 
      * THE CUSTOMER NUMBER, NAME, UNIT PRICE, QUANTITY SOLD / RETURNED,
      * THE TOTAL SALE, THE SALES TAX, AND THE FINAL SALE. 
      ******************************************************************
       
       600-DETAIL.
           IF WS-LINE-COUNT > WS-FULL-PAGE
               PERFORM 200-WRITE-HEADER
           END-IF
           MOVE WS-CLIENT-NO TO D-CLIENT-NO
           MOVE WS-CLIENT-NAME TO D-CLIENT-NAME
           MOVE WS-UNIT-PRICE TO D-UNIT-PRICE
           MOVE WS-QUANTITY-SOLD TO D-QUANTITY-SOLD
           MOVE WS-ADD-REC-FILE TO D-ADDED
           PERFORM 800-CALCULATIONS THRU 800-EXIT
           MOVE WS-TOTAL-SALE TO D-TOTAL-SALE
           MOVE WS-SALES-TAX TO D-SALES-TAX
           MOVE WS-FINAL-SALE TO D-FINAL-SALE
           PERFORM 850-CLACLUATE-TOTALS THRU 850-EXIT
              WRITE OUTPUT-RECORD-REPORT FROM DETAILS1
               AFTER ADVANCING 1 LINE 
           ADD 1 TO WS-LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT   
           
           IF WS-ADD-REC-FILE = "YES"
              PERFORM 700-ADD-DATA THRU 700-EXIT
           END-IF.  
           
       600-EXIT.
       
      ******************************************************************
      * 700-ADD-DATA ADDS THE INPUT DATA TO THE SEQUNTIAL FILE ONLY 
      * IF THE USER TELLS THE PROGRAM TO DO SO. OTHERWISE IT WILL NOT 
      * ADD THE DATA TO THE OUTPUT FILE
      ******************************************************************
       
       700-ADD-DATA.
           MOVE WS-CLIENT-NO TO O-CLIENT-NO
           MOVE WS-CLIENT-NAME TO O-CLIENT-NAME
           MOVE WS-UNIT-PRICE TO O-UNIT-PRICE
           MOVE WS-QUANTITY-SOLD TO O-QUANTITY-SOLD
           MOVE WS-TOTAL-SALE TO O-TOTAL-SALE
           MOVE WS-SALES-TAX TO O-SALES-TAX
           MOVE WS-FINAL-SALE TO O-FINAL-SALE
           WRITE  OUTPUT-RECORD-DATA.
       700-EXIT.
       
      ******************************************************************
      * 800-CALCULATIONS CALCULATES THE TOTAL SALE, SALES TAX, AND
      * FINAL SALE. ALL CALCULATIONS HAVE AN ON SIZE ERROR CHECK 
      * THAT WILL MOVE ZEROS INTO THE VAIRABLE IF IT EXCEEDES ITS PIC 
      * CLAUSE
      ******************************************************************
       800-CALCULATIONS.
           MULTIPLY WS-QUANTITY-SOLD BY WS-UNIT-PRICE GIVING 
           WS-TOTAL-SALE 
               ON SIZE ERROR MOVE ZEROS TO WS-TOTAL-SALE
           END-MULTIPLY
           MULTIPLY WS-TOTAL-SALE BY WS-SALES-TAX-CONST GIVING 
           WS-SALES-TAX 
               ON SIZE ERROR MOVE ZEROS TO WS-SALES-TAX
           END-MULTIPLY
           ADD WS-TOTAL-SALE TO WS-SALES-TAX GIVING WS-FINAL-SALE
               ON SIZE ERROR MOVE ZEROS TO WS-FINAL-SALE
           END-ADD.
       800-EXIT.
       
      ******************************************************************
      * 850-CALCULATE-TOTALS CALCULATES THE TOTALS TO BE PRINTED IN THE 
      * FOOTERS FOR ALL TRANSACTIONS.
      ******************************************************************
       
       850-CLACLUATE-TOTALS.
           COMPUTE WS-SALES-TAX-SUM = WS-SALES-TAX-SUM + WS-SALES-TAX
           COMPUTE WS-TOTAL-SALES-SUM = WS-TOTAL-SALES-SUM + 
                   WS-FINAL-SALE.
       850-EXIT.     
       
      ******************************************************************
      * 900-WRITE-FOOTER WRITES THE FOOTER FOR THE REPORT OUTPUT FILE.
      * THE FOOTER CONTAINS THE TOTAL NUMBER OF CLIENTS, TOTAL SALES TAX
      * AND THE TOTAL FINAL SALES AMOUNT.
      ******************************************************************
       900-WRITE-FOOTER.
           MOVE WS-CLIENT-COUNT TO NO-OF-CLIENTS
           MOVE WS-SALES-TAX-SUM TO F-SALES-TAX
           MOVE WS-TOTAL-SALES-SUM TO F-FINAL-SALE
           WRITE OUTPUT-RECORD-REPORT FROM FOOTER
           WRITE OUTPUT-RECORD-REPORT FROM FOOTER
           WRITE OUTPUT-RECORD-REPORT FROM FOOTER1.
       900-EXIT.
       
      ******************************************************************
      * 950-TERMINATION MODULE CLOSES BOTH OUTPUTFILES
      ******************************************************************
       
       950-TERMINATION-MODULE.
           CLOSE OUTPUT-FILE-DATA
           CLOSE OUTPUT-FILE-REPORT.
       950-EXIT.