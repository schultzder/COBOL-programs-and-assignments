       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE1.
       AUTHOR. LISA M LANDGRAF.
       DATE-WRITTEN. JAN 29,2012.
       DATE-COMPILED. JAN 24, 2016.
      ******************************************************************
      *                                                                *
      * In this program the user enters a starting balance for
      * a checkbook.  We are going to assume that the user will not
      * overdraw their account.  THe user is able to enter deposits
      * and withdrawals.  After each transaction, the new balance is
      * displayed.  The user enters no to stop the program.
      *
      * Input file:  none
      *                                                                *
      * Output: There are prompts for entering the starting balance,
      * whether it is a deposit or withdrawal, the amount of the
      * transaction and what the ending balance is.  There is also a
      * prompt to stop the program.
      *
      * Date/Time due: N/A
      * Date assigned: N/A 
      * data files: none
      ******************************************************************
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARS.
           05 WV-BAL          PIC 9(4)V99.
           05 WV-TRX-AMT      PIC 9(3)V99.
           05 WV-TRX-TYPE     PIC X.
      
       01  WS-SWITCHES.
           05  WS-RESPONSE    PIC X(3).
      *
       01  WS-OUTPUT-FIELDS.
           05  WO-BAL         PIC $$,$$9.99.
           05  WO-TRX-AMT     PIC $$$9.99.
      *
       PROCEDURE DIVISION.
      ******************************************************************
      * INITIALIZE RESPONSE AND GET THE BEGINNING BALANCE
      * LOOP UNTIL RESPONSE IS NO
      * STOP PROGRAM
      ******************************************************************
       000-MAIN-RTN.
           PERFORM 100-INIT-RTN THRU 100-INIT-RTN-EXIT
           PERFORM 200-ENTER-TRXS THRU 200-EXIT
               UNTIL WS-RESPONSE = "NO"
           DISPLAY "END OF SESSION"
           STOP RUN.
      ******************************************************************
      *100-INIT-RTN WILL INITIALIZE RESPONSE TO YES, GET THE BEGINNING
      *             BALANCE OF THE CHECKBOOK AND DISPLAY IT.
      ******************************************************************
       100-INIT-RTN.
          MOVE "YES" TO WS-RESPONSE
          DISPLAY "ENTER BEGINNING BALANCE (9999.99)"
          ACCEPT WV-BAL
          MOVE WV-BAL TO WO-BAL
          DISPLAY "BEGINNING BALANCE IS " WO-BAL.
       100-INIT-RTN-EXIT.
           EXIT.
      ******************************************************************
      * 200-ENTER-TRXS IS A LOOP THAT ASKS THE USER IF THE TRANSACTION
      *                 IS A DEPOSIT OR WITHDRAWAL AND FOR THE AMOUNT
      *                 OF THE TRANSACTION.  IT THEN COMPUTES AND
      *                 DISPLAYS THE BALANCE.
      ******************************************************************
       200-ENTER-TRXS.
           DISPLAY "ENTER D FOR DEPOSIT OR W FOR WITHDRAWAL"
           ACCEPT WV-TRX-TYPE
           DISPLAY "ENTER TRANSACTION AMOUNT (999.99)"
           ACCEPT WV-TRX-AMT
           DISPLAY "----------"
           DISPLAY WO-BAL
           IF WV-TRX-TYPE = "D"
              ADD WV-TRX-AMT TO WV-BAL
           ELSE
              SUBTRACT WV-TRX-AMT FROM WV-BAL
           END-IF
           MOVE WV-TRX-AMT TO WO-TRX-AMT
           MOVE WV-BAL TO WV-BAL
           DISPLAY "  " WO-TRX-AMT
           DISPLAY WO-BAL
           DISPLAY "=========="
           DISPLAY "ENTER NO TO STOP"
           ACCEPT WS-RESPONSE.
       200-EXIT.
           EXIT.
           
       END PROGRAM SAMPLE1.
