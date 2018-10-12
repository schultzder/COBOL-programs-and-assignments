       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM2.
       Author. Derek R Schultz
       Date-Written. 2/28/2018.
       Date-Compiled. 3/2/2018.
      *****************************************************************
      * This program will read in records from a stock input file. 
      * The input file is line sequential. The program will then copmute
      * the price per earnings. Then it will put together the data in a 
      * price per earnings record as an output file. The output file
      * is sequential. Including the date the data was added 
      * stored as (MMDDYYYY). All reconds in the stock file will be 
      * proccessed.  
      *                                                                 
      *
      * Input file
      *  1  -  3  stock code (alphanumeric)
      *  4  - 20  stock name (alphanumeric)
      * 21  - 25  price per share (numeric, 2 decimal places)
      * 26  - 30  unused
      * 31  - 35  latests earnings per share (numeric, 2 decimal places)
      * Output
      *  1  -  3  stock code (alphanumeric)
      *  4  - 20  stock name (alphanumeric)
      * 21  - 25  Price per share (numeric, 2 decimal places)
      * 26  - 40  unused
      * 41  - 48  current date (numeric, MMDDYYYY)
      * 49  - 58  latest earnings per share (numeric, 2 decimal places)
      * 59  - 65  P/E ratio (numeric, 3 decimal places) 
      *
      * infile = C:\COBOL
      * outfile = C:\COBOL 
      * 
      *****************************************************************

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
                ORGANIZATION IS SEQUENTIAL.
                
      * Beginning of data division. 
        
       DATA DIVISION.
       FILE SECTION.
       
      * File description for input file.
        
       FD  INPUT-FILE
           RECORD CONTAINS 35 CHARACTERS. 
       01  INPUT-RECORD.
           05 IN-STOCK-CODE                      PIC X(3).
           05 IN-STOCK-NAME                      PIC X(17).
           05 IN-PRICE-PER-SHARE                 PIC 999V99.
           05 IN-UN-USED                         PIC X(5).
           05 IN-LATEST-EARININGS-PER-SHARE      PIC 9(3)V99.
           
      * File description for output file. 
        
       FD  OUTPUT-FILE
           RECORD CONTAINS 60 CHARACTERS.
       01  OUTPUT-RECORD.
           05 OUT-STOCK-CODE                     PIC X(3).
           05 OUT-STOCK-NAME                     PIC X(17).
           05 OUT-PRICE-PER-SHARE                PIC 999V99.
           05 OUT-UN-USED                        PIC X(15).
           05 OUT-CUR-DATE.
               10 CUR-DAY                        PIC X(2).
               10 CUR-MONTH                      PIC X(2).
               10 CUR-YEAR                       PIC X(4).
           05 OUT-LATEST-EARINGINS-PER-SHARE     PIC 9(3)V99.
           05 OUT-PE-RATIO                       PIC 9(4)V999.
       
      * Working storage variables. 
        
       WORKING-STORAGE SECTION.
       01 EOF-FLAG                              PIC XXX VALUE "NO".
       01 PER-CALC                              PIC 9(4)V99.
       01 WS-FILES.
           05 UT-SYS-INVFILE                    PIC X(60) 
           VALUE "C:\COBOL".
           05 UT-SYS-OUTVFILE                   PIC X(60)
           VALUE "C:\COBOL".
           

       PROCEDURE DIVISION.
      
      ******************************************************************
      * 000-MAIN-MODULE RUNS ALL THE KEY MODULES TO PERFORM THE PROGRAMS
      * PURPOSE. 
      ******************************************************************
       000-MAIN-MODULE.
           PERFORM 100-INITIALIZATION-OPEN
           PERFORM 200-READ-INPUT-FILE
           PERFORM 500-WRITE-OUTPUT
                   UNTIL EOF-FLAG = "YES"
           PERFORM 900-TERMINATION-MODULE
       STOP RUN.
       

      ******************************************************************
      * 100-INITALIZAZION-OPEN SIMPLY OPENS THE INPUT AND OUTPUT 
      * FILE FOR READING.
      ******************************************************************
       
       100-INITIALIZATION-OPEN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE.
       
      ******************************************************************
      * 200-READ-INPUT-FILE STARTS THE READ FOR THE INPUT FILE AND MOVES
      * THE VALUES CONTAINED WINTHIN THE FILE TO THE VARIABLES FOR THE
      * INPUT. CHECKS FOR AN EOF FLAG. 
      ******************************************************************
       
           
       200-READ-INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE "YES" TO EOF-FLAG.
           
      ******************************************************************
      * 500-WRITE-OUTPUT DOES A CALCULATION FOR THE PRICE EARNINGS RATIO
      * AND THEN TRANSFERS THE INPUT VARIABLES TO THE OUTPUT VARIABLES.
      * THE MODULE THEN PROMPTS THE USER FOR THE CURRENT DATE. IT THEN
      * CONTINUES UNTILL THERE ARE NO MORE RECORDS IN THE FILE. 
      ******************************************************************
           
       500-WRITE-OUTPUT.
           DIVIDE IN-PRICE-PER-SHARE BY IN-LATEST-EARININGS-PER-SHARE 
               GIVING PER-CALC.
           MOVE IN-STOCK-CODE TO OUT-STOCK-CODE
           MOVE IN-STOCK-NAME TO OUT-STOCK-NAME
           MOVE IN-PRICE-PER-SHARE TO OUT-PRICE-PER-SHARE
           MOVE ZEROS TO OUT-UN-USED
           MOVE FUNCTION CURRENT-DATE TO OUT-CUR-DATE
           MOVE IN-LATEST-EARININGS-PER-SHARE TO 
                OUT-LATEST-EARINGINS-PER-SHARE
           MOVE PER-CALC TO OUT-PE-RATIO
           WRITE OUTPUT-RECORD.
           READ INPUT-FILE
                AT END MOVE "YES" TO EOF-FLAG.
       
      ******************************************************************
      * 900-TERMINATION-MODULE CLOSES THE INPUT AND OUTPUT FILES. 
      ******************************************************************
       
       900-TERMINATION-MODULE.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.
