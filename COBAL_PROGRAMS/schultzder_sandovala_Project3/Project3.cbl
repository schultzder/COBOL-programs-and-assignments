       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROJECT1.
       Author. Derek R Schultz Andre J Sandoval.
       Date-Written. 5/3/2018.
       Date-Compiled. 5/11/2018.
      ******************************************************************
      * Purpose of the Program: Sorting a sales file by the customer 
      * number in ascending order and printing it out to a report
      *
      *  Input File:
      *   1-4   Client Number
      *   5-29  Client Name
      *   30-36 Unit Price
      *   37-40 Quantity Sold 
      *   41-46 Total Sale 
      *   47-52 Sales Tax
      *   53-59 Final Sale
      *
      *  Output File:
      *   The report will output all the records sorted in ascending 
      *   order by client number
      *
      *   Infile = C:\COBOL\outDAT.txt
      *   Outfile = C:\COBOL\OUTRPT.txt
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
      * Select statement for input file
        
       SELECT INPUT-OLD-MASTER 
              ASSIGN TO UT-SYS-INVFILE
              ORGANIZATION IS LINE SEQUENTIAL.       
       
      * Select statement for sorted file.
       
       SELECT SORT-X
              ASSIGN TO DISK.
       
      *Select statment for report output file.
        
       SELECT NEW-MASTER-REC
              ASSIGN TO UT-SYS-NEW-OUTVFILE
              ORGANIZATION IS LINE SEQUENTIAL.
              
      * Select statment for report output file.     
        
       SELECT MASTER-REC-REPORT
              ASSIGN TO UT-SYS-OUTVFILE
              ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT SORTED-IN-TRANS-FILE
              ASSIGN TO UT-SYS-TRANSVFILE
              ORGANIZATION IS LINE SEQUENTIAL. 
       
       SELECT INPUT-TRANS-FILE
              ASSIGN TO UT-SYS-INTRANS
              ORGANIZATION IS LINE SEQUENTIAL. 
              
              
      * Beginning of data division. 
        
       DATA DIVISION.
       FILE SECTION.
       
       FD INPUT-TRANS-FILE
          RECORD CONTAINS 68 CHARACTERS.
       01 TRANSACTION-FILE.
          05 T-CLIENT-NO                         PIC X(4).
          05                                     PIC X VALUE SPACES.
          05 T-CLIENT-NAME                       PIC X(25).
          05 T-UNIT-PRICE                        PIC 9(4)V99.
          05                                     PIC X(3) VALUE SPACES.
          05 T-QUANTITY-SOLD                     PIC S9999.
          05                                     PIC X(7) VALUE SPACES.
          05 T-TOTAL-SALE                        PIC S9(4)V99.
          05 T-SALES-TAX                         PIC S9(3)V99.
          05 T-FINAL-SALE                        PIC S9(4)V99.
          05 T-TRANS-CODE                  PIC X.
             88 T-DELETE-RECORD              VALUE "D".
             88 T-ADD-RECORD                 VALUE "A".
             88 T-UPDATE-RECORD              VALUE "C".
          
          
       FD SORTED-IN-TRANS-FILE
          RECORD CONTAINS 68 CHARACTERS.
       01 TRANSACTION-FILE-S.
          05 ST-CLIENT-NO                         PIC X(4).
          05                                     PIC X VALUE SPACES.
          05 ST-CLIENT-NAME                       PIC X(25).
          05 ST-UNIT-PRICE                        PIC 9(4)V99.
          05                                     PIC X(3) VALUE SPACES.
          05 ST-QUANTITY-SOLD                     PIC S9999.
          05                                     PIC X(7) VALUE SPACES.
          05 ST-TOTAL-SALE                        PIC S9(4)V99.
          05 ST-SALES-TAX                         PIC S9(3)V99.
          05 ST-FINAL-SALE                        PIC S9(4)V99.
          05 ST-TRANS-CODE                 PIC X.
             88 ST-DELETE-RECORD              VALUE "D".
             88 ST-ADD-RECORD                 VALUE "A".
             88 ST-UPDATE-RECORD              VALUE "C".
       
      * File decription for input file
        
       FD  INPUT-OLD-MASTER
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
          
      * File decription for Sorted file

       FD  NEW-MASTER-REC
           RECORD CONTAINS 67 CHARACTERS.
       01 NEW-RECORD-DATA.
          05 A-CLIENT-NO                         PIC X(4).
          05                                     PIC X VALUE SPACES.
          05 A-CLIENT-NAME                       PIC X(25).
          05 A-UNIT-PRICE                        PIC 9(4)V99.
          05                                     PIC X(3) VALUE SPACES.
          05 A-QUANTITY-SOLD                     PIC S9999.
          05                                     PIC X(7) VALUE SPACES.
          05 A-TOTAL-SALE                        PIC S9(4)V99.
          05 A-SALES-TAX                         PIC S9(3)V99.
          05 A-FINAL-SALE                        PIC S9(4)V99.

      * File decription for input file

       FD MASTER-REC-REPORT
          RECORD CONTAINS 134 CHARACTERS.
       01 OUTPUT-REC                                 PIC X(134). 

       SD SORT-X.
       01 SORT-RECORD.
          05 S-CLIENT-NO                         PIC X(4).
          05                                     PIC X VALUE SPACES.
          05 S-CLIENT-NAME                       PIC X(25).
          05 S-UNIT-PRICE                        PIC 9(4)V99.
          05                                     PIC X(3) VALUE SPACES.
          05 S-QUANTITY-SOLD                     PIC S9999.
          05                                     PIC X(7) VALUE SPACES.
          05 S-TOTAL-SALE                        PIC S9(4)V99.
          05 S-SALES-TAX                         PIC S9(3)V99.
          05 S-FINAL-SALE                        PIC S9(4)V99.
          05 ST-TRANS-CODE                 PIC X.
             88 S-DELETE-RECORD              VALUE "D".
             88 S-ADD-RECORD                 VALUE "A".
             88 S-UPDATE-RECORD              VALUE "C".
       
       WORKING-STORAGE SECTION.
       
       01 WS-VARS.
          05 PAGE-COUNT                             PIC 99  VALUE 1.
          05 LINE-COUNT                             PIC 99  VALUE 1.
          05 DETAILS-START                          PIC 999 VALUE 9.
          05 FULL-PAGE-COUNT                        PIC 99  VALUE 55.
          05 WS-CLIENT-COUNT                        PIC 999. 
          05 WS-SALES-TAX-SUM                       PIC S9999V99.
          05 WS-TOTAL-SALES-SUM                     PIC S9999V99.
          05 WS-SALES-TAX-CONST                     PIC 9V999 
                                                    VALUE 0.065.
          05 WS-HIGH-VALUES                         PIC 9999
                                                    VALUE 9999.
          05 WS-PREVEOUS-REC                        PIC 9999.
          05 WS-CUR-DATE.
             10 CUR-YEAR                            PIC 9999.
             10 CUR-DAY                             PIC 99.
             10 CUR-MONTH                           PIC 99.
          
          
          
          
       01 HEADING1.
          05                                 PIC X(13)
                                             VALUE "DEREK SCHULTZ".
          05                                 PIC X(40)  VALUE SPACES.
          05                                 PIC X(18) 
                                             VALUE "REPORT MASTER FILE".
          05                                 PIC X(45) VALUE SPACES.
          05                                 PIC X(4)  VALUE "PAGE".
          05                                 PIC X     VALUE SPACES.
          05 H-PAGE-NO                       PIC Z99.
          
          
       01 HEADING2. 
          05                                 PIC X(14) 
                                             VALUE "ANDRE SANDOVAL".
          05                                 PIC X(110) VALUE SPACES.
          
       
       
       01 HEADING3.
          05                                 PIC X(114) VALUE SPACES.
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          
          
       01 HEADING4. 
          05                                 PIC X(124) VALUE SPACES.
          
          
       01 HEADING5.
          05                                 PIC X(6)  VALUE "CLIENT".
          05                                 PIC X(1)  VALUE SPACES.
          05                                 PIC X(6)  VALUE "CLIENT".
          05                                 PIC X(19) VALUE SPACES.
          05                                 PIC X(10) 
                                             VALUE "UNIT PRICE".
          05                                 PIC X(3)     VALUE SPACES.
          05                                 PIC X(8)  VALUE "QUANTITY".
          05                                 PIC X(8)  VALUE SPACES.
          05                                 PIC X(11) 
                                             VALUE "TOTAL SALE".
          05                                 PIC X(6) VALUE SPACES.
          05                                 PIC X(9) VALUE "SALES TAX".
          05                                 PIC X(4) VALUE SPACES.
          05                                 PIC X(10) 
                                             VALUE "FINAL SALE".
          05                                 PIC X VALUE SPACES.
          05                                 PIC X(11) 
                                             VALUE "TRANSACTION".
                                           
       01 HEADING6.
          05                                 PIC X     VALUE SPACES.
          05                                 PIC XX    VALUE "NO".
          05                                 PIC X(5)  VALUE SPACES.
          05                                 PIC X(4)  VALUE "NAME".
          05                                 PIC X(35) VALUE SPACES.
          05                                 PIC X(4)  VALUE "SOLD".
          05                                 PIC X(54) VALUE SPACES.
          05                                 PIC X(4)  VALUE "CODE".
          05                                 PIC X(15)  VALUE SPACES.
          
          
       01 HEADING7.
          05                                 PIC X(124) VALUE SPACES.
          
       01 DETAILS1.
          05 D-CLIENT-NO                     PIC X(4).
          05                                 PIC X(3)  VALUE SPACES.
          05 D-CLIENT-NAME                   PIC X(25).
          05                                 PIC X(3)  VALUE SPACES.
          05 D-UNIT-PRICE                    PIC -$,$$$.99.
          05                                 PIC X(4)  VALUE SPACES.
          05 D-QUANTITY-SOLD                 PIC -ZZZ9.
          05                                 PIC X(8)  VALUE SPACES.
          05 D-TOTAL-SALE                    PIC -$$,$$9.99.
          05                                 PIC X(6)  VALUE SPACES.
          05 D-SALES-TAX                     PIC -$,$$9.99.
          05                                 PIC X(4)  VALUE SPACES.
          05 D-FINAL-SALE                    PIC -$$,$$9.99.
          05                                 PIC X(2)  VALUE SPACES.
          05 D-TRANS-CODE                    PIC X(35).
       
       
       01 FOOTER.
          05                                 PIC X(124) VALUE SPACES.
          
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
          05 UT-SYS-NEW-OUTVFILE             PIC X(76)
          VALUE "C:\COBOL\out_new_mst.txt".
          05 UT-SYS-INVFILE                  PIC X(76)
          VALUE "C:\COBOL\sortedDAT.txt".
          05 UT-SYS-OUTVFILE                 PIC X(76)
          VALUE "C:\COBOL\new_mstRPT.txt".
          05 UT-SYS-INTRANS                  PIC X(76)
          VALUE "C:\COBOL\transactionFile.txt".
          05 UT-SYS-TRANSVFILE               PIC X(76)
          VALUE "C:\COBOL\sortedInput.txt".
 
          
      * Start of procedure division
        
       PROCEDURE DIVISION.
       
       000-MAIN-MODULE.
           PERFORM 100-SORT THRU 100-EXIT
           PERFORM 200-OPEN THRU 200-EXIT
           PERFORM 250-WRITE-HEADER THRU 250-EXIT
           PERFORM 300-UPDATE-RTN THRU 300-EXIT
                   UNTIL O-CLIENT-NO = WS-HIGH-VALUES
                   AND ST-CLIENT-NO = WS-HIGH-VALUES
           PERFORM 1000-TERMINATION-MODULE THRU 1000-EXIT.
       STOP RUN.
       
      ******************************************************************
      * Sorts the records by client number in ascendng order and writes
      * it to the output master file
      ******************************************************************
       100-SORT.
          SORT SORT-X 
               ON ASCENDING KEY S-CLIENT-NO 
               USING INPUT-TRANS-FILE
               GIVING SORTED-IN-TRANS-FILE.
       100-EXIT.
       
      ******************************************************************
      * Opens the input and output files and moves the current date to
      * the correct spot 
      ******************************************************************
       200-OPEN.
           OPEN INPUT INPUT-OLD-MASTER
           OPEN INPUT SORTED-IN-TRANS-FILE
           OPEN OUTPUT NEW-MASTER-REC
           OPEN OUTPUT MASTER-REC-REPORT
           MOVE FUNCTION CURRENT-DATE TO WS-CUR-DATE
           MOVE CUR-YEAR TO H-YEAR
           MOVE CUR-MONTH TO H-MONTH
           MOVE CUR-DAY TO H-DAY.
           PERFORM 800-READ-MASTER THRU 800-EXIT
           PERFORM 900-READ-TRANS THRU 900-EXIT.
       200-EXIT.
       
      ******************************************************************
      * Reads in the records from the sorted master file and outputs 
      * it to the sorted master file 
      ******************************************************************
       
       250-WRITE-HEADER.
           MOVE PAGE-COUNT TO H-PAGE-NO
           WRITE OUTPUT-REC FROM HEADING1
           WRITE OUTPUT-REC  FROM HEADING2
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC  FROM HEADING3
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC  FROM HEADING4
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC  FROM HEADING5
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC  FROM HEADING6
             AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-REC  FROM HEADING7
           ADD 1 TO PAGE-COUNT
           MOVE DETAILS-START TO LINE-COUNT.
       250-EXIT.

      ******************************************************************
      * Writes the heading with the current date and page number 
      ******************************************************************
  
       
       300-UPDATE-RTN.
          EVALUATE TRUE 
              WHEN O-CLIENT-NO = ST-CLIENT-NO
                   PERFORM 400-UPDATE-TEST THRU 400-EXIT
              WHEN O-CLIENT-NO > ST-CLIENT-NO
                   PERFORM 550-ADD-RTN THRU 550-EXIT
              WHEN OTHER 
                   PERFORM 650-WRITE-OLD-REC THRU 650-EXIT
          END-EVALUATE. 
       300-EXIT.
       
       
       400-UPDATE-TEST.
           EVALUATE TRUE
              WHEN ST-DELETE-RECORD
                   MOVE "DELETED" TO D-TRANS-CODE
                   PERFORM 510-WRITE-DETAILS-DELETE THRU 510-EXIT
              WHEN ST-ADD-RECORD
                   PERFORM 600-ERROR-RTN THRU 600-EXIT
              WHEN OTHER 
                   PERFORM 700-UPDATE-RECORD THRU 700-EXIT
           END-EVALUATE
           PERFORM 800-READ-MASTER THRU 800-EXIT
           PERFORM 900-READ-TRANS THRU 900-EXIT.
       400-EXIT.
       
       
        500-WRITE-DETAILS.
           IF LINE-COUNT > FULL-PAGE-COUNT
              PERFORM 250-WRITE-HEADER THRU 250-EXIT    
              MOVE ZEROS TO LINE-COUNT
           END-IF 
           MOVE ST-CLIENT-NO TO D-CLIENT-NO
           MOVE ST-CLIENT-NAME TO D-CLIENT-NAME
           MOVE ST-UNIT-PRICE TO D-UNIT-PRICE
           MOVE ST-QUANTITY-SOLD TO D-QUANTITY-SOLD
           MOVE ST-TOTAL-SALE TO D-TOTAL-SALE
           MOVE ST-SALES-TAX TO D-SALES-TAX
           MOVE ST-FINAL-SALE TO D-FINAL-SALE
           WRITE OUTPUT-REC FROM DETAILS1
             AFTER ADVANCING 1 LINE
           ADD 1 TO LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT.
       500-EXIT.
       

       
       
       510-WRITE-DETAILS-DELETE.
           IF LINE-COUNT > FULL-PAGE-COUNT
              PERFORM 250-WRITE-HEADER THRU 250-EXIT    
              MOVE ZEROS TO LINE-COUNT
           END-IF 
           MOVE O-CLIENT-NO TO D-CLIENT-NO
           MOVE O-CLIENT-NAME TO D-CLIENT-NAME
           MOVE O-UNIT-PRICE TO D-UNIT-PRICE
           MOVE O-QUANTITY-SOLD TO D-QUANTITY-SOLD
           MOVE O-TOTAL-SALE TO D-TOTAL-SALE
           MOVE O-SALES-TAX TO D-SALES-TAX
           MOVE O-FINAL-SALE TO D-FINAL-SALE
           WRITE OUTPUT-REC FROM DETAILS1
             AFTER ADVANCING 1 LINE
           ADD 1 TO LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT.
       510-EXIT.
       
       520-WRITE-DETAILS-UPDATE.
           IF LINE-COUNT > FULL-PAGE-COUNT
              PERFORM 250-WRITE-HEADER THRU 250-EXIT    
              MOVE ZEROS TO LINE-COUNT
           END-IF 
           MOVE A-CLIENT-NO TO D-CLIENT-NO
           MOVE A-CLIENT-NAME TO D-CLIENT-NAME
           MOVE A-UNIT-PRICE TO D-UNIT-PRICE
           MOVE A-QUANTITY-SOLD TO D-QUANTITY-SOLD
           MOVE A-TOTAL-SALE TO D-TOTAL-SALE
           MOVE A-SALES-TAX TO D-SALES-TAX
           MOVE A-FINAL-SALE TO D-FINAL-SALE
           WRITE OUTPUT-REC FROM DETAILS1
             AFTER ADVANCING 1 LINE
           ADD 1 TO LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT.
       520-EXIT.
       
      ****************************************************************
      * Adds a record to the file
      ******************************************************************       
       550-ADD-RTN.
          IF ST-DELETE-RECORD OR ST-UPDATE-RECORD
            EVALUATE TRUE 
               WHEN ST-DELETE-RECORD
                   PERFORM 560-ERROR-CODE THRU 560-EXIT
               WHEN ST-UPDATE-RECORD
                   PERFORM 570-ERROR-CODE THRU 570-EXIT
            END-EVALUATE
           
          ELSE  
           MOVE "ADDED" TO D-TRANS-CODE
           MOVE ST-CLIENT-NO TO A-CLIENT-NO
           MOVE ST-CLIENT-NAME TO A-CLIENT-NAME
           MOVE ST-UNIT-PRICE TO A-UNIT-PRICE
           MOVE ST-QUANTITY-SOLD TO A-QUANTITY-SOLD
           MOVE ST-SALES-TAX TO A-SALES-TAX
           MOVE ST-TOTAL-SALE TO A-TOTAL-SALE
           MOVE ST-FINAL-SALE TO A-FINAL-SALE
           WRITE NEW-RECORD-DATA
           PERFORM 500-WRITE-DETAILS THRU 500-EXIT
           PERFORM 900-READ-TRANS THRU 900-EXIT
          END-IF.
       550-EXIT.
       
       560-ERROR-CODE.
           MOVE "RECORD DOES NOT EXIST-NO DELETE" TO D-TRANS-CODE
           PERFORM 500-WRITE-DETAILS THRU 500-EXIT
           PERFORM 900-READ-TRANS THRU 900-EXIT.
       560-EXIT.
       
       570-ERROR-CODE.
           MOVE "RECORD DOES NOT EXIST-NO CHANGE" TO D-TRANS-CODE
           PERFORM 500-WRITE-DETAILS THRU 500-EXIT
           PERFORM 900-READ-TRANS THRU 900-EXIT.
       570-EXIT.
       
       600-ERROR-RTN.
           MOVE "RECORD ALREADY EXITS-NO ADD" TO D-TRANS-CODE
           PERFORM 510-WRITE-DETAILS-DELETE THRU 510-EXIT
           MOVE O-CLIENT-NO TO A-CLIENT-NO
           MOVE O-CLIENT-NAME TO A-CLIENT-NAME
           MOVE O-UNIT-PRICE TO A-UNIT-PRICE
           MOVE O-QUANTITY-SOLD TO A-QUANTITY-SOLD
           MOVE O-SALES-TAX TO A-SALES-TAX
           MOVE O-TOTAL-SALE TO A-TOTAL-SALE
           MOVE O-FINAL-SALE TO A-FINAL-SALE
           WRITE NEW-RECORD-DATA.
       600-EXIT.
       
       650-WRITE-OLD-REC.
           MOVE O-CLIENT-NO TO A-CLIENT-NO
           MOVE O-CLIENT-NAME TO A-CLIENT-NAME
           MOVE O-UNIT-PRICE TO A-UNIT-PRICE
           MOVE O-QUANTITY-SOLD TO A-QUANTITY-SOLD
           MOVE O-SALES-TAX TO A-SALES-TAX
           MOVE O-TOTAL-SALE TO A-TOTAL-SALE
           MOVE O-FINAL-SALE TO A-FINAL-SALE
           WRITE NEW-RECORD-DATA
           MOVE "UNCHANGED" TO D-TRANS-CODE
           PERFORM 500-WRITE-DETAILS THRU 500-EXIT.
           PERFORM 800-READ-MASTER THRU 800-EXIT.
       650-EXIT.
       
       700-UPDATE-RECORD.
           MOVE O-CLIENT-NO TO A-CLIENT-NO
           MOVE O-CLIENT-NAME TO A-CLIENT-NAME
           MOVE ST-UNIT-PRICE TO A-UNIT-PRICE
           MOVE O-QUANTITY-SOLD TO A-QUANTITY-SOLD
            MULTIPLY O-QUANTITY-SOLD BY ST-UNIT-PRICE GIVING 
           A-TOTAL-SALE 
               ON SIZE ERROR MOVE ZEROS TO A-TOTAL-SALE
           END-MULTIPLY
           MULTIPLY A-TOTAL-SALE BY WS-SALES-TAX-CONST GIVING 
           A-SALES-TAX 
               ON SIZE ERROR MOVE ZEROS TO A-SALES-TAX
           END-MULTIPLY
           ADD A-TOTAL-SALE TO A-SALES-TAX GIVING A-FINAL-SALE
               ON SIZE ERROR MOVE ZEROS TO A-FINAL-SALE
           END-ADD
           WRITE NEW-RECORD-DATA 
           MOVE "UPDATED" TO D-TRANS-CODE
           PERFORM 520-WRITE-DETAILS-UPDATE THRU 520-EXIT.
       700-EXIT.
       
           

      ******************************************************************
      * Writes the sorted output record and moves everything to the 
      * correct spots 
      ******************************************************************

       800-READ-MASTER.
           READ INPUT-OLD-MASTER
                AT END MOVE WS-HIGH-VALUES TO O-CLIENT-NO
           END-READ.
       800-EXIT.

      ******************************************************************
      * Computes the total sales tax and the total sales 
      ******************************************************************
       900-READ-TRANS.
           READ SORTED-IN-TRANS-FILE
               AT END MOVE WS-HIGH-VALUES TO ST-CLIENT-NO
           END-READ.
       900-EXIT.
       
       
      ******************************************************************
      * Writes the footer to the output record 
      *****************************************************************
       
      ******************************************************************
      * Closes all the files and terminates the program
      ******************************************************************
       1000-TERMINATION-MODULE.
           CLOSE INPUT-OLD-MASTER
           CLOSE SORTED-IN-TRANS-FILE
           CLOSE NEW-MASTER-REC.
       1000-EXIT.
       
       