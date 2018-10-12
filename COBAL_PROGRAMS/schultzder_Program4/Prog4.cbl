       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM4.
       Author. Derek R Schultz
       Date-Written. 4/18/2018.
       Date-Compiled. 4/23/2018.
      ******************************************************************
      * This program is a control break program that prints out clients
      * infromation based on their region number. The client number,
      * name, and their total trip cost is displayed in the details 
      * section. The footer displays the clients region number, how many
      * clients were in that region, and the total cost of all their 
      * trips.  
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
      * Select statment for input file.
        
       SELECT INPUT-FILE
               ASSIGN TO UT-SYS-INVFILE
               ORGANIZATION IS LINE SEQUENTIAL.
               
      * Select statment for output file.
        
       SELECT OUTPUT-FILE 
               ASSIGN TO UT-SYS-OUTVFILE
               ORGANIZATION IS LINE SEQUENTIAL.
       
      * Beginning of data division.
        
       DATA DIVISION. 
       FILE SECTION.
       
      * File description for input file. 
       
       FD INPUT-FILE 
          RECORD CONTAINS 80 CHARACTERS.
       01 INPUT-RECORD.
          05 IN-BOOKING-TYPE                       PIC X.
          05 IN-CLIENT-NO                          PIC XXX.
          05 IN-CLIENT-NAME                        PIC X(19).
          05 IN-FILLER1                            PIC X(2)
                                                   VALUE SPACES.
          05 IN-REGION-NO                          PIC X(2).
          05 IN-FILLER2                            PIC X(11)
                                                   VALUE SPACES.
          05 IN-COST-OF-TRIP                       PIC 9(5).
          05 IN-FILLER3                            PIC X(37)
                                                   VALUE SPACES.
       
      * File description for output file
        
       FD OUTPUT-FILE
          RECORD CONTAINS 60 CHARACTERS.
       01 OUTPUT-RECORD                            PIC X(69).
       
      * Working storage variables
        
       WORKING-STORAGE SECTION.
       
       01 WS-VARS.
          05 WS-PAGE-NO                            PIC 999 VALUE 1.
          05 WS-DETAILS-START                      PIC 999 VALUE 7.
          05 WS-LINE-COUNT                         PIC 999.
          05 WS-EOF-FLAG                           PIC XXX VALUE "NO".
          05 WS-FIRST-RECORD                       PIC XXX VALUE "YES".
          05 WS-HOLD-REGION                        PIC XX
                                                   VALUE SPACES.
          05 WS-REGION-TOTAL                       PIC 9(6)V99.
          05 WS-CLIENT-COUNT                       PIC 999.
          05 WS-END-OF-JOB                         PIC XXX VALUE 'NO'.
          05 WS-CUR-DATE.
             10 CUR-YEAR                           PIC 9999.
             10 CUR-DAY                            PIC 99.
             10 CUR-MONTH                          PIC 99.
       
      * Heading 1
       
       01 HEADING1.
          05                                 PIC X(20) 
                                    VALUE "DEREK SCHULTZ".
          05                                 PIC X(3)  VALUE SPACES.
          05 H-COMPANY-NAME                  PIC X(24) 
                                    VALUE "BON VOYAGE TRAVEL AGENCY".
          05                                 PIC X(8)  VALUE SPACES.
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          05                                 PIC X(6) VALUE SPACES.
          
       
      * Heading 2
        
       01 HEADING2.
          05                                 PIC X(23) VALUE SPACES.
          05                                 PIC X(20) 
                                    VALUE "REGION TOTALS REPORT".
          05                                 PIC X(17) VALUE SPACES.
          
      * Heading 3
        
       01 HEADING3.
          05                                 PIC X(60) VALUE SPACES.
       
      * Heading 4
       
       01 HEADING4.                          
          05                                 PIC X(60) VALUE SPACES.
          
      * Heading 5
        
       01 HEADING5. 
          05                                 PIC X(6) VALUE "CLIENT".
          05                                 PIC X(29) VALUE SPACES.
          05                                 PIC X(9)
                                             VALUE "TRIP COST".
          05                                 PIC X(16) VALUE SPACES.
          
      * Details 1
        
       01 DETAILS1. 
          05 D-CLIENT-NO                     PIC XXX.
          05                                 PIC X(2) VALUE SPACES.
          05 D-CLIENT-NAME                   PIC X(19).
          05                                 PIC X(13) VALUE SPACES.
          05 D-TRIP-COST                     PIC $$$$,$$9.
          
      * Footer
        
       01 FOOTER1.
          05                                 PIC XXX VALUE '***'.
          05                                 PIC X(6) VALUE "REGION".
          05 F-REGION-NO                     PIC 99. 
          05                                 PIC X VALUE SPACES.
          05                                 PIC XXX VALUE '***'.
          05                                 PIC X VALUE SPACES.
          05 F-NO-CLIENTS                    PIC ZZ9.
          05                                 PIC X VALUE SPACES.
          05                                 PIC X(7) VALUE "CLIENTS".
          05                                 PIC X(4) VALUE SPACES.
          05 F-TOTAL-TRIP-COST               PIC $$$,$$$,$$9.
          
      * Files
        
       01 WS-FILES.
          05 UT-SYS-INVFILE                  PIC X(30)
          VALUE "C:\COBOL\strip.dat".
          05 UT-SYS-OUTVFILE                 PIC X(30)
          VALUE "C:\COBOL\out4.doc".
          
      * Start of procedure division
        
       PROCEDURE DIVISION.
       
      ******************************************************************
      * 000-MAIN-MODULE RUNS ALL THE KEY MODULES TO PERFORM THE PROGRAMS
      * PURPOSE.
      ******************************************************************
       000-MAIN-MODULE. 
           PERFORM 100-INITIALIZATION-OPEN THRU 100-EXIT
           PERFORM 200-WRITE-HEADER THRU 200-EXIT
           PERFORM 400-READ-REC UNTIL WS-EOF-FLAG = "YES"
           PERFORM 900-END-OF-JOB-RTN THRU 900-EXIT.
       STOP RUN.
        
        
      ******************************************************************
      * 100-INITALIZATION-OPEN SIMPLY OPENS THE INPUT AND OUTPUT 
      * FILE FOR READING. ALSO CONTAINS CODE TO SET UP THE CURRENT DATE.
      ******************************************************************
       100-INITIALIZATION-OPEN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           MOVE FUNCTION CURRENT-DATE TO WS-CUR-DATE
           MOVE CUR-YEAR TO H-YEAR
           MOVE CUR-MONTH TO H-MONTH
           MOVE CUR-DAY TO H-DAY.
       100-EXIT.
   
      ******************************************************************
      * 200-WRITE-HEADER WRITES THE OUTPUT RECORD FROM THE HEADERS.
      * THIS IS THE PRIMINING MODULE TO PRINT THE FIRST HEADER ON THE 
      * FIRST PAGE. 
      ******************************************************************
       200-WRITE-HEADER.
           WRITE OUTPUT-RECORD FROM HEADING1
           WRITE OUTPUT-RECORD FROM HEADING2
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD FROM HEADING3
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING4
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING5
            AFTER ADVANCING 1 LINE 
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           ADD 1 TO WS-PAGE-NO
           MOVE WS-DETAILS-START TO WS-LINE-COUNT.
       200-EXIT.
           
      ******************************************************************
      * 400-READ-REC READS THE INPUT FILE UNTIL THERE ARE NO MORE 
      * RECORDS. THIS MODULE ALSO CALLS THE DETAILS MODULE.  
      ******************************************************************
       400-READ-REC.
           READ INPUT-FILE
               AT END 
                   MOVE "YES" TO WS-EOF-FLAG
               NOT AT END 
                   PERFORM 510-WRITE-DETAILS THRU 510-EXIT
           END-READ.
           
           
      ******************************************************************
      * 510-WRITE-DETAILS WRITES THE DETAILS OF THE INPUT RECORD. HAS
      * CHECKS FOR THE CONTROL BREAK AND WEATHER OR NOT TO PRINT THE 
      * HEADER IF THERE ARE TOO MANY CLIENTS. 
      ******************************************************************
       510-WRITE-DETAILS.
           EVALUATE TRUE 
               WHEN WS-FIRST-RECORD = 'YES'
                    MOVE IN-REGION-NO TO WS-HOLD-REGION
                    MOVE 'NO' TO WS-FIRST-RECORD
               WHEN IN-REGION-NO NOT = WS-HOLD-REGION
                    PERFORM 600-CONTROL-BREAK THRU 600-EXIT
           END-EVALUATE
           IF WS-CLIENT-COUNT > 15
               PERFORM 700-WRITE-HEADER2 THRU 700-EXIT
           END-IF
           MOVE IN-CLIENT-NO TO D-CLIENT-NO
           MOVE IN-CLIENT-NAME TO D-CLIENT-NAME
           MOVE IN-COST-OF-TRIP TO D-TRIP-COST
           WRITE OUTPUT-RECORD FROM DETAILS1
               AFTER ADVANCING 2 LINES
           ADD 1 TO WS-LINE-COUNT
           ADD 1 TO WS-CLIENT-COUNT
           ADD IN-COST-OF-TRIP TO WS-REGION-TOTAL.
       510-EXIT.
       
      ******************************************************************
      * 600-CONTROL-BREAK RESETS THE CLIENT INFORMATION FOR THE NEXT
      * REGION NUMBER TO BE PROCSSED AND PRINTS THE FOOTER FOR THE 
      * REGION. 
      ******************************************************************
       600-CONTROL-BREAK.
       MOVE WS-REGION-TOTAL TO F-TOTAL-TRIP-COST
       MOVE WS-HOLD-REGION TO F-REGION-NO
       MOVE WS-CLIENT-COUNT TO F-NO-CLIENTS
       WRITE OUTPUT-RECORD FROM FOOTER1
           AFTER ADVANCING 2 LINES 
       ADD 1 TO WS-LINE-COUNT
       MOVE ZEROS TO WS-CLIENT-COUNT
       MOVE ZEROS TO WS-REGION-TOTAL
       MOVE IN-REGION-NO TO WS-HOLD-REGION
       IF WS-END-OF-JOB = 'NO'
           PERFORM 700-WRITE-HEADER2 THRU 700-EXIT
       END-IF.
       600-EXIT.

      ******************************************************************
      * 700-WRITE-HEADER2 WRITES THE HEADER WITH AN ADVANCING PAGE 
      * CONDITION 
      ******************************************************************
       700-WRITE-HEADER2.
        ADD 1 TO WS-PAGE-NO
           WRITE OUTPUT-RECORD FROM HEADING1
            AFTER ADVANCING PAGE
           WRITE OUTPUT-RECORD FROM HEADING2
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD FROM HEADING3
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING4
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING5
            AFTER ADVANCING 1 LINE 
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           ADD 1 TO WS-PAGE-NO
           MOVE WS-DETAILS-START TO WS-LINE-COUNT.
       700-EXIT.

      ******************************************************************
      * 900-END-OF-JOB-RTN CLOSES BOTH THE INPUT AND OUTPUT FILES AND
      * PERFROMS THE CONTROL BREAK ONE LAST TIME. 
      ******************************************************************
       900-END-OF-JOB-RTN. 
       MOVE 'YES' TO WS-END-OF-JOB
       PERFORM 600-CONTROL-BREAK THRU 600-EXIT
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.
       900-EXIT.