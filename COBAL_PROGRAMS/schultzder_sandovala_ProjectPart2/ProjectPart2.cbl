       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROJECT1.
       Author. Derek R Schultz Andre J Sandoval.
       Date-Written. 4/23/2018.
       Date-Compiled. 4/30/2018.
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
       
      * Select statement for input file
        
       SELECT INPUT-FILE 
              ASSIGN TO UT-SYS-INVFILE
              ORGANIZATION IS SEQUENTIAL.       
       
      * Select statement for sorted file.
       
       SELECT SORT-X
              ASSIGN TO DISK.
       
      *Select statment for report output file.
        
       SELECT SORTED-MASTER-FILE
              ASSIGN TO UT-SYS-SORTED-OUTVFILE
              ORGANIZATION IS LINE SEQUENTIAL.
              
      * Select statment for report output file.     
        
       SELECT SORTED-MASTER-REPORT
              ASSIGN TO UT-SYS-OUTVFILE
              ORGANIZATION IS LINE SEQUENTIAL.
              
              
      * Beginning of data division. 
        
       DATA DIVISION.
       FILE SECTION.
       
      * File decription for data output file
        
       FD  INPUT-FILE
           RECORD CONTAINS 56 CHARACTERS.
       01 OUTPUT-RECORD-DATA.
          05 O-CLIENT-NO                          PIC X(4).
          05 O-CLIENT-NAME                        PIC X(25).
          05 O-UNIT-PRICE                         PIC 9(4)V99.
          05 O-QUANTITY-SOLD                      PIC 9999.
          05 O-TOTAL-SALE                         PIC 9(4)V99.
          05 O-SALES-TAX                          PIC 9(3)V99.
          05 O-FINAL-SALE                         PIC 9(4)V99.
          
          
       FD  SORTED-MASTER-FILE
           RECORD CONTAINS 56 CHARACTERS.
       01 SORTED-RECORD-DATA.
          05 A-CLIENT-NO                         PIC X(4).
          05 A-CLIENT-NAME                       PIC X(25).
          05 A-UNIT-PRICE                        PIC 9(4)V99.
          05 A-QUANTITY-SOLD                     PIC S9999.
          05 A-TOTAL-SALE                        PIC S9(4)V99.
          05 A-SALES-TAX                         PIC S9(3)V99.
          05 A-FINAL-SALE                        PIC S9(4)V99.
          
       FD SORTED-MASTER-REPORT
          RECORD CONTAINS 99 CHARACTERS.
       01 OUTPUT-REC                                 PIC X(99). 

       SD SORT-X.
       01 SORT-RECORD.
          05 S-CLIENT-NO                         PIC X(4).
          05 S-CLIENT-NAME                       PIC X(25).
          05 S-UNIT-PRICE                        PIC 9(4)V99.
          05 S-QUANTITY-SOLD                     PIC 9999.
          05 S-TOTAL-SALE                        PIC 9(4)V99.
          05 S-SALES-TAX                         PIC 9(3)V99.
          05 S-FINAL-SALE                        PIC 9(4)V99.
       
       WORKING-STORAGE SECTION.
       
       01 WS-VARS.
          05 DO-IT-AGAIN                            PIC X   VALUE "N".
          05 PAGE-COUNT                             PIC 99  VALUE 1.
          05 LINE-COUNT                             PIC 99  VALUE 1.
          05 DETAILS-START                          PIC 999 VALUE 9.
          05 FULL-PAGE-COUNT                        PIC 99  VALUE 55.
          05 WS-CLIENT-COUNT                        PIC 999. 
          05 WS-SALES-TAX-SUM                       PIC S9999V99.
          05 WS-TOTAL-SALES-SUM                     PIC S9999V99.
          05 WS-CUR-DATE.
             10 CUR-YEAR                            PIC 9999.
             10 CUR-DAY                             PIC 99.
             10 CUR-MONTH                           PIC 99.
          
          
          
          
       01 HEADING1.
          05                                 PIC X(13)
                                             VALUE "DEREK SCHULTZ".
          05                                 PIC X(30)  VALUE SPACES.
          05                                 PIC X(18) 
                                             VALUE "SORTED MASTER FILE".
          05                                 PIC X(29) VALUE SPACES.
          05                                 PIC X(4)  VALUE "PAGE".
          05                                 PIC X     VALUE SPACES.
          05 H-PAGE-NO                       PIC Z99.
          
          
       01 HEADING2. 
          05                                 PIC X(14) 
                                             VALUE "ANDRE SANDOVAL".
          05                                 PIC X(85) VALUE SPACES.
          
       
       
       01 HEADING3.
          05                                 PIC X(89) VALUE SPACES.
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          
          
       01 HEADING4. 
          05                                 PIC X(99) VALUE SPACES.
          
          
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
                                           
       01 HEADING6.
          05                                 PIC XX    VALUE SPACES.
          05                                 PIC XX    VALUE "NO".
          05                                 PIC X(6)  VALUE SPACES.
          05                                 PIC X(4)  VALUE "NAME".
          05                                 PIC X(35) VALUE SPACES.
          05                                 PIC X(4)  VALUE "SOLD".
          05                                 PIC X(23) VALUE SPACES.
          
          
       01 HEADING7.
          05                                 PIC X(99) VALUE SPACES.
          
       01 DETAILS1.
          05 D-CLIENT-NO                     PIC X(4).
          05                                 PIC X(4)  VALUE SPACES.
          05 D-CLIENT-NAME                   PIC X(25).
          05                                 PIC X(2)  VALUE SPACES.
          05 D-UNIT-PRICE                    PIC -$,$$$.99.
          05                                 PIC X(5)  VALUE SPACES.
          05 D-QUANTITY-SOLD                 PIC -ZZZ9.
          05                                 PIC X(3)  VALUE SPACES.
          05 D-TOTAL-SALE                    PIC -$$,$$9.99.
          05                                 PIC X(3)  VALUE SPACES.
          05 D-SALES-TAX                     PIC -$,$$9.99.
          05                                 PIC X(2)  VALUE SPACES.
          05 D-FINAL-SALE                    PIC -$$,$$9.99.
       
       
       01 FOOTER.
          05                                 PIC X(99) VALUE SPACES.
          
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

       
       
       01 WS-FILES.
          05 UT-SYS-OUTVFILE                 PIC X(76)
          VALUE "C:\COBOL\OUTRPT.txt".
          05 UT-SYS-INVFILE                  PIC X(76)
          VALUE "C:\COBOL\outDAT.txt".
          05 UT-SYS-SORTED-OUTVFILE          PIC X(76)
          VALUE "C:\COBOL\sortedDAT.txt".
 
          
      * Start of procedure division
        
       PROCEDURE DIVISION.
       
       000-MAIN-MODULE.
           PERFORM 100-SORT THRU 100-EXIT
           PERFORM 200-OPEN THRU 200-EXIT
           PERFORM 300-WRITE-HEADER THRU 300-EXIT
           PERFORM 400-READ-RECORDS THRU 400-EXIT 
             UNTIL DO-IT-AGAIN = "Y"
           PERFORM 550-WRITE-FOOTER THRU 550-EXIT
           PERFORM 600-TERMINATION-MODULE THRU 600-EXIT.
       STOP RUN.
       
       
       
       100-SORT.
          SORT SORT-X 
               ON ASCENDING KEY S-CLIENT-NO 
               USING INPUT-FILE
               GIVING SORTED-MASTER-FILE.
       100-EXIT.
       
       200-OPEN.
           OPEN INPUT SORTED-MASTER-FILE
           OPEN OUTPUT SORTED-MASTER-REPORT
           MOVE FUNCTION CURRENT-DATE TO WS-CUR-DATE
           MOVE CUR-YEAR TO H-YEAR
           MOVE CUR-MONTH TO H-MONTH
           MOVE CUR-DAY TO H-DAY.
       200-EXIT.
       
       400-READ-RECORDS.
           READ SORTED-MASTER-FILE
                AT END MOVE "Y" TO DO-IT-AGAIN
           NOT AT END    
              PERFORM 500-WRITE-DETAIL THRU 500-EXIT.
       400-EXIT.
       
       300-WRITE-HEADER.
           MOVE PAGE-COUNT TO H-PAGE-NO
           WRITE OUTPUT-REC FROM HEADING1
           WRITE OUTPUT-REC FROM HEADING2
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC FROM HEADING3
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC FROM HEADING4
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC FROM HEADING5
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC FROM HEADING6
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC FROM HEADING7
           ADD 1 TO PAGE-COUNT
           MOVE DETAILS-START TO LINE-COUNT.
       300-EXIT.
       
       500-WRITE-DETAIL.
           IF LINE-COUNT > FULL-PAGE-COUNT
              PERFORM 300-WRITE-HEADER THRU 400-EXIT    
              MOVE ZEROS TO LINE-COUNT
           END-IF 
           MOVE A-CLIENT-NO TO D-CLIENT-NO
           MOVE A-CLIENT-NAME TO D-CLIENT-NAME
           MOVE A-UNIT-PRICE TO D-UNIT-PRICE
           MOVE A-QUANTITY-SOLD TO D-QUANTITY-SOLD
           MOVE A-TOTAL-SALE TO D-TOTAL-SALE
           MOVE A-SALES-TAX TO D-SALES-TAX
           MOVE A-FINAL-SALE TO D-FINAL-SALE
           PERFORM 540-CALCULATE-TOTALS THRU 540-EXIT
           WRITE OUTPUT-REC FROM DETAILS1
             AFTER ADVANCING 1 LINE
           ADD 1 TO LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT.
       
       500-EXIT.
       
       540-CALCULATE-TOTALS.
           
           COMPUTE WS-SALES-TAX-SUM = WS-SALES-TAX-SUM + A-SALES-TAX
           COMPUTE WS-TOTAL-SALES-SUM = WS-TOTAL-SALES-SUM + 
                   A-FINAL-SALE.
       540-EXIT.
       
       
       
       550-WRITE-FOOTER.
           MOVE WS-CLIENT-COUNT TO NO-OF-CLIENTS
           MOVE WS-SALES-TAX-SUM TO F-SALES-TAX
           MOVE WS-TOTAL-SALES-SUM TO F-FINAL-SALE
           WRITE OUTPUT-REC FROM FOOTER
           WRITE OUTPUT-REC FROM FOOTER
           WRITE OUTPUT-REC FROM FOOTER1.
       550-EXIT.
       
       600-TERMINATION-MODULE.
           CLOSE SORTED-MASTER-FILE
           CLOSE SORTED-MASTER-REPORT.
       600-EXIT.
       
       
       
       
           
       