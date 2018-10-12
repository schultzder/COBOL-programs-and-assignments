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
       01 WS-VARS.
           05 SOME-NUM		PIC S9(3)V9.
           05 SOME-NUM2		PIC S9(2)V99.
           05 SOME-NUM3		PIC S9(6).
           05 SOME-NUM4		PIC S9(2)v9.
           05 SOME-NUM5		PIC S9(4)v99.
           05 WS-COST1      PIC 9(3) VALUE 200.
           05 WS-COST2      PIC 9(3) VALUE 100.
           05 WS-COST3      PIC 9(3) VALUE 300.
       01 WS-ANS.
          05 WS-RESULT		PIC Z,ZZ9.9-.
          05 WS-RESULT2		PIC $$Z9.99CR.
          05 WS-RESULT3		PIC Z,ZZZ,ZZZ-.
          05 WS-RESULT4		PIC ++++.9.
          05 WS-RESULT5		PIC $$,ZZ9.99.



       PROCEDURE DIVISION.
      ******************************************************************
      * INITIALIZE RESPONSE AND GET THE BEGINNING BALANCE
      * LOOP UNTIL RESPONSE IS NO
      * STOP PROGRAM
      ******************************************************************
       000-MAIN-RTN.
           
           DISPLAY "SOME-NUM IS " 
           ACCEPT SOME-NUM
           MOVE SOME-NUM TO WS-RESULT
           DISPLAY "SOME-NUM IS " SOME-NUM 
           DISPLAY "WS-RESULT IS " WS-RESULT 
           DISPLAY "SOME-NUM2 IS " 
           ACCEPT SOME-NUM2
           MOVE SOME-NUM2 TO WS-RESULT2
           DISPLAY "SOME-NUM2 IS " SOME-NUM2 
           DISPLAY "WS-RESULT2 IS " WS-RESULT2 
           DISPLAY "SOME-NUM3 IS " 
           ACCEPT SOME-NUM3
           MOVE SOME-NUM3 TO WS-RESULT3
           DISPLAY "SOME-NUM3 IS " SOME-NUM3 
           DISPLAY "WS-RESULT3 IS " WS-RESULT3 