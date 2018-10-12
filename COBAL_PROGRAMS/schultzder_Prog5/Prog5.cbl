       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM5.
       Author. Derek R Schultz
       Date-Written. 5/1/2018.
       Date-Compiled. 5/9/2018.
      ******************************************************************
      * This program is a control break program that utalizes a table
      * and search to correctly process an input file. First, the 
      * program sorts the input by region number. Then loads in the 
      * region number table which is set up as such:
      *
      * 01United Kingdom
      * 02New Zeland
      * 03China
      * 04India
      * 05Florida
      * 06Califonia
      * 07New York
      * 08Washington
      * 09Costa Rica
      *
      * The program then begins a control break algorithm. For each
      * region number it calculates a total, and the smallest and 
      * largest cost for each region. At the end, the program calculates
      * an average and prints and output report with the following 
      * format:
      * 
      * 1-2   Region Number
      * 3-22  Region Name
      * 30-23 Trip Avg rounded
      * 31-36 Lowest Trip
      * 37-42 Highest Trip 
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
               
      * Select statement for the sort file
        
       SELECT SORT-FILE 
              ASSIGN TO DISK.
              
      * Select statement for the sorted input
        
       SELECT SORTED-INPUT
              ASSIGN TO UT-SYS-SORT
              ORGANIZATION IS LINE SEQUENTIAL.
              
      * Select statment for the region table
        
       SELECT REGION-TABLE 
              ASSIGN TO UT-SYS-REGION
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
      * FD for the sorted input file
        
       FD SORTED-INPUT
          RECORD CONTAINS 80 CHARACTERS.
       01 INPUT-SORTED.
          05 ST-BOOKING-TYPE                       PIC X.
          05 ST-CLIENT-NO                          PIC XXX.
          05 ST-CLIENT-NAME                        PIC X(19).
          05 ST-FILLER1                            PIC X(2)
                                                   VALUE SPACES.
          05 ST-REGION-NO                          PIC X(2).
          05 ST-FILLER2                            PIC X(11)
                                                   VALUE SPACES.
          05 ST-COST-OF-TRIP                       PIC 9(5).
          05 ST-FILLER3                            PIC X(37)
                                                   VALUE SPACES.
      * FD for the region table
        
       FD REGION-TABLE.
       01 REGION-REC.
          05 R-REGION-CODE                         PIC XX.
          05 R-REGION-TITLE                        PIC X(14).
          
       
      * File description for output file
        
       FD OUTPUT-FILE
          RECORD CONTAINS 60 CHARACTERS.
       01 OUTPUT-RECORD.
          05 OUT-REGION-NUMBER                     PIC X(2).
          05 OUT-REGION-NAME                       PIC X(20).
          05 OUT-AVG-TRIP                          PIC 9(6)V99.
          05 OUT-LOWEST-TRIP                       PIC 9(6).
          05 OUT-HIGHEST-TRIP                      PIC 9(6).
          
      * SD for sort file
        
       SD SORT-FILE.
       01 SORT-REC.
          05 S-BOOKING-TYPE                       PIC X.
          05 S-CLIENT-NO                          PIC XXX.
          05 S-CLIENT-NAME                        PIC X(19).
          05 S-FILLER1                            PIC X(2)
                                                  VALUE SPACES.
          05 S-REGION-NO                          PIC X(2).
          05 S-FILLER2                            PIC X(11)
                                                  VALUE SPACES.
          05 S-COST-OF-TRIP                       PIC 9(5).
          05 S-FILLER3                            PIC X(37)
                                                  VALUE SPACES.
          
       
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
          05 WS-LARGEST                            PIC 9(5) VALUE 0.
          05 WS-SMALEST                            PIC 9(5) VALUE 0.
          05 WS-AVERAGE                            PIC 9(6)V99.
             
       01 REGION-NUMBER-TABLE.
          05 REGION-ENTRIES OCCURS 9 TIMES
               ASCENDING KEY IS REGION-NO INDEXED BY X1.
             10 REGION-NO                          PIC 99.
             10 REGION-NAME                        PIC X(15).
          
      * Files
        
       01 WS-FILES.
          05 UT-SYS-INVFILE                  PIC X(30)
          VALUE "C:\COBOL\Client5.txt".
          05 UT-SYS-OUTVFILE                 PIC X(30)
          VALUE "C:\COBOL\Tripcost.txt".
          05 UT-SYS-REGION                   PIC X(30)
          VALUE "C:\COBOL\RegionTable.txt".
          05 UT-SYS-SORT                     PIC X(30)
          VALUE "C:\COBOL\sortedClient.txt".
          
      * Start of procedure division
        
       PROCEDURE DIVISION.
       
      ******************************************************************
      * 000-MAIN-MODULE RUNS ALL THE KEY MODULES TO PERFORM THE PROGRAMS
      * PURPOSE.
      ******************************************************************
       000-MAIN-MODULE. 
           PERFORM 100-SORT THRU 100-EXIT
           PERFORM 200-INITIALIZATION-OPEN THRU 200-EXIT
           PERFORM 300-LOAD-TITLE-TABLE THRU 300-EXIT
           PERFORM 500-READ-REC UNTIL WS-EOF-FLAG = "YES"
           PERFORM 900-END-OF-JOB-RTN THRU 900-EXIT.
       STOP RUN.
        
        
      ******************************************************************
      * 100-SORT SORTS THE INPUT FILE AND CREATES A NEW SORTED INPUT 
      * THAT WILL BE USED FOR READING
      ******************************************************************
       100-SORT.
           SORT SORT-FILE
           ON ASCENDING KEY S-REGION-NO
           USING INPUT-FILE
           GIVING SORTED-INPUT.
       100-EXIT.
      ******************************************************************
      * 200-INITALIZATION-OPEN SIMPLY OPENS THE INPUT AND OUTPUT 
      * FILE FOR READING.
      ******************************************************************
       200-INITIALIZATION-OPEN.
           OPEN INPUT REGION-TABLE
           OPEN INPUT SORTED-INPUT
           OPEN OUTPUT OUTPUT-FILE.
       200-EXIT.
   
      ******************************************************************
      * 300-LOAD-TITLE-TABLE LOADS THE TABLE WITH THE APPROPRIATE VALUES
      ******************************************************************
       300-LOAD-TITLE-TABLE.
           PERFORM VARYING X1 FROM 1 BY 1
               UNTIL X1 > 9 
           READ REGION-TABLE
               AT END DISPLAY 'NOT ENOUGH RECORDS'
                   STOP RUN
           END-READ 
           MOVE REGION-REC TO REGION-ENTRIES (X1)
           END-PERFORM.
       300-EXIT.
           
      ******************************************************************
      * 500-READ-REC READS THE INPUT FILE UNTIL THERE ARE NO MORE 
      * RECORDS. THIS MODULE ALSO CALLS THE CALC-RTN MODULE
      ******************************************************************
       500-READ-REC.
           READ SORTED-INPUT
               AT END 
                   MOVE "YES" TO WS-EOF-FLAG
               NOT AT END 
                   PERFORM 510-CALC-RTN THRU 510-EXIT
           END-READ.
           
           
      ******************************************************************
      * 510-CALC-RTN CALCULATES THE TOTAL TRIP COST AND KEEPS TRACK
      * OF THE LARGEST AND SMALLEST COSTING TRIPS.  
      ******************************************************************
       510-CALC-RTN.
           EVALUATE TRUE 
               WHEN WS-FIRST-RECORD = 'YES'
                    MOVE ST-REGION-NO TO WS-HOLD-REGION
                    MOVE ST-COST-OF-TRIP TO WS-SMALEST
                    MOVE 'NO' TO WS-FIRST-RECORD
               WHEN ST-REGION-NO NOT = WS-HOLD-REGION
                    PERFORM 600-CONTROL-BREAK THRU 600-EXIT
           END-EVALUATE
           ADD 1 TO WS-CLIENT-COUNT
           ADD ST-COST-OF-TRIP TO WS-REGION-TOTAL
           IF ST-COST-OF-TRIP > WS-LARGEST
              MOVE ST-COST-OF-TRIP TO WS-LARGEST
           END-IF
           IF ST-COST-OF-TRIP < WS-SMALEST
              MOVE ST-COST-OF-TRIP TO WS-SMALEST.
       510-EXIT.
       
      ******************************************************************
      * 600-CONTROL-BREAK RESETS THE CLIENT INFORMATION FOR THE NEXT
      * REGION NUMBER TO BE PROCSSED AND PRINTS THE OUTPUT REC FOR THE 
      * REGION. 
      ******************************************************************
       600-CONTROL-BREAK.
       COMPUTE WS-AVERAGE ROUNDED = WS-REGION-TOTAL / WS-CLIENT-COUNT
       MOVE WS-AVERAGE TO OUT-AVG-TRIP
       MOVE WS-SMALEST TO OUT-LOWEST-TRIP
       MOVE WS-LARGEST TO OUT-HIGHEST-TRIP
       MOVE WS-HOLD-REGION TO OUT-REGION-NUMBER
       SET X1 TO 1 
       SEARCH REGION-ENTRIES
           AT END PERFORM 650-ERROR-RTN THRU 650-EXIT
           WHEN WS-HOLD-REGION = REGION-NO (X1)
               MOVE REGION-NAME (X1) TO OUT-REGION-NAME
       END-SEARCH
       MOVE ZEROS TO WS-CLIENT-COUNT
       MOVE ZEROS TO WS-REGION-TOTAL
       MOVE ZEROS TO WS-LARGEST
       MOVE ST-COST-OF-TRIP TO WS-SMALEST
       MOVE ST-REGION-NO TO WS-HOLD-REGION
       WRITE OUTPUT-RECORD.
       600-EXIT.


       650-ERROR-RTN.
           MOVE "BAD REGION NO" TO OUT-REGION-NAME.
       650-EXIT.    

      ******************************************************************
      * 900-END-OF-JOB-RTN CLOSES BOTH THE INPUT AND OUTPUT FILES AND
      * PERFROMS THE CONTROL BREAK ONE LAST TIME. 
      ******************************************************************
       900-END-OF-JOB-RTN. 
       MOVE 'YES' TO WS-END-OF-JOB
       PERFORM 600-CONTROL-BREAK THRU 600-EXIT
           CLOSE REGION-TABLE
           CLOSE SORTED-INPUT
           CLOSE OUTPUT-FILE.
       900-EXIT.