       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDITCODE.
       AUTHOR. Lisa M Landgraf.
       DATE-WRITTEN. March 1, 2017.
      *****************************************************************
      * THis program is to be used by you to play around with edit
      * codes.  You will need to modify it several times.  Look for
      * the Word document that goes with it. 
      * Input: Is several constants.
      *
      * Output: Is both to display and to a report so you can see how
      *         the contents are shown. 
      * 
      *Date/time due: N/A
      *Date assigned: N/A
      *
      *Data set name: N/A
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIST-NUM-FILE
               ASSIGN UT-SYS-LISTNUM
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LIST-NUM-FILE
           RECORD CONTAINS 30 CHARACTERS.
       01  LIST-NUM-REC        PIC X(30).
       
       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
          05  WV-FIRST         PIC S9999V99.
          05  WV-SECOND        PIC S9(4)V99 VALUE -34.56.
       01 WS-FILE-LOC.
          05 UT-SYS-LISTNUM    PIC X(50)
                   VALUE "J:\COBOL\COBOL_sample\list.doc".
       01 RPT-LINE-1.
          05                   PIC X(30)
                   VALUE "         1         2         3".
       01 RPT-LINE-2.
          05                   PIC X(30)
                   VALUE "123456789012345678901234567890".
       01 RPT-LINE-3.
          05 WD-FIRST          PIC $$$$,$$9.99CR.
          05                   PIC X(24) VALUE SPACES.
       01 RPT-LINE-4.
          05 WD-SECOND         PIC 9(4).99-.
          05                   PIC X(22) VALUE SPACES.
                      
       PROCEDURE DIVISION.
      *****************************************************************
      * Open the report file.  Write out first two lines of the report
      * These are to show positions on the line.
      * Then write out next lines to show different edit codes.
      * Close the file and end the program.   
      ***************************************************************** 
       100-MAIN.
           OPEN OUTPUT LIST-NUM-FILE
           DISPLAY "ENTER First Number"
           ACCEPT WV-FIRST
           WRITE LIST-NUM-REC FROM RPT-LINE-1
               AFTER ADVANCING PAGE
           WRITE LIST-NUM-REC FROM RPT-LINE-2
               AFTER ADVANCING 1 LINE
           PERFORM 200-MOVE-NUMS THRU 200-EXIT
           CLOSE LIST-NUM-FILE
           STOP RUN.
       200-MOVE-NUMS.
           MOVE WV-FIRST TO WD-FIRST
           DISPLAY WD-FIRST
           MOVE WV-SECOND to WD-SECOND
           WRITE LIST-NUM-REC FROM RPT-LINE-3
               AFTER ADVANCING 1 LINE
           WRITE LIST-NUM-REC FROM RPT-LINE-4
               AFTER ADVANCING 1 LINE.
       200-EXIT.
           EXIT.
       
