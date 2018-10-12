       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM3.
       Author. Derek R Schultz
       Date-Written. 3/21/2018.
       Date-Compiled. 4/2/2018.
      *****************************************************************
      * This program will read from an input file and then produce a 
      * document in a cetain format. The input file contains information 
      * pertaining to Bon Voyage Travel Agency's sales trips. The output
      * will produce a formated document while calculating the commision 
      * for the sales person                                            
      *                                                                 
      * Input file
      *  1  -     booking type (numeric)
      *  2  -  4  client number (numeric)
      *  5  - 23  client name (alphanumeric)
      * 24  - 25  unused
      * 26  - 27  region number (numeric)
      * 28  - 38  unused
      * 39  - 43  cost of trip in dollars (numeric, no pennies)
      * 44  - 80  unused 
      * 
      * Output
      * 
      * HEADING 1 LINE 1:
      *  
      *  1  - 10 current date (numeric MM/DD/YYY)
      * 11  - 20 unused
      * 21  - 44 the name 'BON VOYAGE TRAVEL AGENCY' (alphabetic)
      * 45  - 61 unused
      * 62  - 65 the word 'PAGE' (alphabetic)
      * 66  -    unused
      * 67  - 69 
      * 
      * HEADING 2 LINE 2:
      * 
      *  1  - 21 user's name (alphabetic)
      * 21  - 24 unused
      * 25  - 41 'COMMISSION REPORT' (alphabetic)
      * 42  - 69 unused
      *
      * HEADING 3 LINE 3:
      * 
      *  1  - 69 unused 
      * 
      * HEADING 4 LINE 4: 
      * 
      *  1  -  14 the words 'CLIENT CLIENT' (alphabetic)
      * 15  -  30 unused
      * 31  -  37 the word 'BOOKING' (alphabetic)
      * 38  -  39 unused
      * 40  -  45 the word 'REIGON' (alphabetic)
      * 46  -  52 unused
      * 53  -  56 the word 'TRIP' (alphabetic) 
      * 57  -  69 unused
      * 
      * HEADING 5 LINE 5:
      * 
      *  1  -   2 unused
      *  3  -   4 the word 'NO' (alphabetic, for 'number')
      *  5  -   8 unused
      *  9  -  12 the word 'NAME' (alphabetic)
      * 13  -  31 unused
      * 32  -  35 the word 'TYPE' (alphabetic)
      * 36  -  41 unused
      * 42  -  43 the word 'NO' (alphabetic, for 'number')
      * 44  -  52 unused
      * 53  -  56 the word 'COST' (alphabetic)
      * 57  -  59 unused
      * 60  -  69 the word 'COMMISSION' (alphabetic)
      *
      * HEADING 6 LINE 6:
      * 
      *  1  -  69 unused 
      * 
      * DETAILS 1 LINE 7: (REPEAT FOR ALL RECORDS)
      * 
      *  1  -    unused
      *  2  -  4 client number (alphanumeric)
      *  5  -  8 unused
      *  9  - 28 client name (alphanumeric)
      * 29  - 32 unused
      * 33  -    booking type (numeric)
      * 34  - 41 unused
      * 42  - 43 region number (numberic)
      * 44  - 50 unused 
      * 51  - 56 tip cost (numeric)
      * 57  - 59 unused
      * 60  - 69 commission (numeric)
      *  
      * FOOTER 1 END LINE:
      * 
      *  1  - 16 the phrase '*****TOTALS*****' (alphanumeric)
      * 17  - 29 unused
      * 30  - 32 number of clients (numeric)
      * 33  -    unused
      * 34  - 40 the word 'CLIENTS' (alphanumeric)
      * 41  - 45 unused
      * 46  - 56 trip cost (numeric)
      * 57  - 58 unused
      * 59  - 69 commision cost 
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
                ORGANIZATION IS LINE SEQUENTIAL.
                
      * Beginning of data division. 
        
       DATA DIVISION.
       FILE SECTION.
       
      * File description for input file.
       
       FD  INPUT-FILE
           RECORD CONTAINS 80 CHARACTERS. 
       01  INPUT-RECORD.
           05 IN-BOOKING-TYPE                PIC X.
           05 IN-CLIENT-NO                   PIC XXX.
           05 IN-CLIENT-NAME                 PIC X(19).
           05 IN-FILLER1                     PIC XX    VALUE SPACES.
           05 IN-REGION-NO                   PIC XX.
           05 IN-FILLER2                     PIC X(11) VALUE SPACES.
           05 IN-COST-OF-TRIP                PIC 9(5).
           05 IN-FILLER3                     PIC X(37) VALUE SPACES.
           
      * File description for output file. 
        
       FD  OUTPUT-FILE
           RECORD CONTAINS 69 CHARACTERS.
       01  OUTPUT-RECORD                     PIC X(69).
       
      * Working storage variables. 
        
       WORKING-STORAGE SECTION.
        
       01 WS-VARS.
          05 WS-PAGE-NO                      PIC 999 VALUE 1.
          05 WS-DETAILS-START                PIC 999 VALUE 9.
          05 WS-LINE-COUNT                   PIC 999.
          05 WS-EOF-FLAG                     PIC XXX VALUE "NO".
          05 WS-TOTAL-COST                   PIC 9(9)V99.
          05 WS-COMMISSION-COST              PIC 9(9)V99.
          05 WS-COMMISSION1-3                PIC 9V99 VALUE 0.10.
          05 WS-COMMISSION1-O                PIC 999  VALUE 300. 
          05 WS-COMMISSION2-3                PIC 9V99 VALUE 0.09.
          05 WS-COMMISSION2-O                PIC 999  VALUE 250.
          05 WS-COMMISSION3-1                PIC 9V99 VALUE 0.08.
          05 WS-COMMISSION4-2                PIC 999  VALUE 600.
          05 WS-COMMISSION-O                 PIC 9V99 VALUE 0.15.
          05 WS-CLIENT-COUNT                 PIC 999.
          05 WS-FULL-PAGE                    PIC 99   VALUE 55.
          05 WS-COMMISSION-TOTAL             PIC 9(9)V99.
          05 WS-CUR-DATE.
             10 CUR-YEAR                     PIC 9999.
             10 CUR-DAY                      PIC 99.
             10 CUR-MONTH                    PIC 99.
          
      * Heading 1 
          
       01 HEADING1.
          05 H-DAY                           PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-MONTH                         PIC 99.
          05                                 PIC X     VALUE "/".
          05 H-YEAR                          PIC 9999.
          05                                 PIC X(10) VALUE SPACES.
          05 H-COMPANY-NAME                  PIC X(24) 
                                    VALUE "BON VOYAGE TRAVEL AGENCY".
          05                                 PIC X(17) VALUE SPACES.
          05                                 PIC X(4)  VALUE "PAGE".
          05 H-PAGE-NO                        PIC Z99.
          
      * Heading 2
        
       01 HEADING2.
          05                                 PIC X(20) 
                                    VALUE "DEREK SCHULTZ". 
          05                                 PIC X(4)  VALUE SPACES.
          05                                 PIC X(17) 
                                    VALUE "COMMISSION REPORT".
          05                                 PIC X(28) VALUE SPACES.
          
      * Heading 3
        
       01 HEADING3.
          05                                 PIC X(69) VALUE SPACES.
          
      * Heading 4
        
       01 HEADING4.
          05                                 PIC X(14) 
                                    VALUE "CLIENT CLIENT".
          05                                 PIC X(16)  VALUE SPACES.
          05                                 PIC X(15) 
                                    VALUE "BOOKING  REGION". 
          05                                 PIC X(7)   VALUE SPACES.
          05                                 PIC X(4)   VALUE "TRIP".
          05                                 PIC X(13)  VALUE SPACES.
          
      * Heading 5
        
       01 HEADING5. 
          05                                 PIC XX     VALUE SPACES.
          05                                 PIC XX     VALUE "NO".
          05                                 PIC XXXX   VALUE SPACES.
          05                                 PIC XXXX   VALUE "NAME".
          05                                 PIC X(19)  VALUE SPACES.
          05                                 PIC XXXX   VALUE "TYPE".
          05                                 PIC X(6)   VALUE SPACES.
          05                                 PIC XX     VALUE "NO".
          05                                 PIC X(9)   VALUE SPACES.
          05                                 PIC XXXX   VALUE "COST".
          05                                 PIC XXX    VALUE SPACES.
          05                                 PIC X(10)  
                                    VALUE "COMMISSION".
                                    
      * Heading 6
        
       01 HEADING6. 
          05                                 PIC X(69)  VALUE SPACES.
          
      * Record Details
        
       01 DEATIL1.
          05                                 PIC X      VALUE SPACES.
          05 CLIENT-NO                       PIC XXX.
          05                                 PIC XXX    VALUE SPACES.
          05 CLIENT-NAME                     PIC X(20). 
          05                                 PIC XXXX   VALUE SPACES.
          05 BOOKING-TYPE                    PIC 9.
          05                                 PIC X(8)   VALUE SPACES.
          05 REGION-NO                       PIC 99. 
          05                                 PIC X(7)   VALUE SPACES.
          05 TRIP-COST                       PIC ZZ,Z99.
          05                                 PIC XXX    VALUE SPACES.
          05 COMMISSION                      PIC ZZZ,ZZ9.99.
          
      * Footer 
        
       01 FOOTER.
          05                                 PIC X(69) VALUE SPACES.
          
      * Footer
        
       01 FOOTER1.
          05                                 PIC X(16) 
                                    VALUE "*****TOTALS*****".
          05                                 PIC X(13)  VALUE SPACES.
          05 NO-OF-CLIENTS                   PIC ZZ9.
          05                                 PIC X      VALUE SPACES.
          05                                 PIC X(7)   VALUE "CLIENTS".
          05                                 PIC X(5)   VALUE SPACES.
          05 TOTAL-TRIP-COST                 PIC $$$,$$$,$99.
          05                                 PIC XX     VALUE SPACES.
          05 TOTAL-COMMISION-COST            PIC $$$$,$$9.99.
          
      * Files
        
       01 WS-FILES.
          05 UT-SYS-INVFILE                  PIC X(60)
          VALUE "C:\Users\schultzder\trip.dat".
          05 UT-SYS-OUTVFILE                 PIC X(60)
          VALUE "C:\Users\schultzder\tripout.doc".
       
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
           PERFORM 700-PRINT-FOOTER1 THRU 700-EXIT
           PERFORM 900-TERMINATION-MODULE THRU 900-EXIT
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
           WRITE OUTPUT-RECORD FROM HEADING1
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD FROM HEADING2
            AFTER ADVANCING 1 LINE
           WRITE OUTPUT-RECORD FROM HEADING3
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING4
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING5
            AFTER ADVANCING 1 LINE 
           WRITE OUTPUT-RECORD FROM HEADING6
            AFTER ADVANCING 1 LINE 
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           ADD 1 TO WS-PAGE-NO
           MOVE WS-DETAILS-START TO WS-LINE-COUNT.
       200-EXIT.
           
      ******************************************************************
      * 500-WRITE-HEADERS TAKES THE HEADERS SET UP IN WS-VARS AND WRITES
      * THEM TO THE OUTPUT FILE WITH THE CORRECT FORMAT.
      ******************************************************************
       400-READ-REC.
           READ INPUT-FILE
               AT END 
                   MOVE "YES" TO WS-EOF-FLAG
               NOT AT END 
                   PERFORM 510-WRITE-DETAILS THRU 510-EXIT
           END-READ.
           
           
      ******************************************************************
      * 510-WRITE-DETAILS WRITES THE DETAILS OF THE INPUT RECORD TO 
      * THE OUTPUT RECORD FOR HOWEVER MANY RECORD ARE IN THE INPUT FILE.
      ******************************************************************
       510-WRITE-DETAILS.
           IF WS-LINE-COUNT > WS-FULL-PAGE
               PERFORM 200-WRITE-HEADER
           END-IF
           MOVE IN-BOOKING-TYPE TO BOOKING-TYPE
           MOVE IN-CLIENT-NO TO CLIENT-NO
           MOVE IN-CLIENT-NAME TO CLIENT-NAME
           MOVE IN-REGION-NO TO REGION-NO
           MOVE IN-COST-OF-TRIP TO TRIP-COST.
           PERFORM 600-CALCULATE-COMMISSION THRU 600-EXIT
           MOVE WS-COMMISSION-COST TO COMMISSION
           PERFORM 610-CALUCULATE-TOTALS THRU 610-EXIT
            WRITE OUTPUT-RECORD FROM DEATIL1
             AFTER ADVANCING 1 LINE
            ADD 1 TO WS-LINE-COUNT
            ADD 1 TO WS-CLIENT-COUNT.
       510-EXIT.
       
      ******************************************************************
      * 600-CALCULATE-COMMISION HAS AN EVALUATE SATEMENT TO DETERMINE 
      * WHAT COMMISSION RATE TO GIVE TO THE EMPLOYEE. THE COMMISSION 
      * RATE IS BASED ON THE BOOKING TYPE AND THE REGION NUMBER. 
      ******************************************************************
       
       600-CALCULATE-COMMISSION.
       Evaluate IN-BOOKING-TYPE
	   WHEN 1 
		IF IN-REGION-NO = 01 OR 03
			COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP * 
            WS-COMMISSION1-3
		ELSE 
			MOVE WS-COMMISSION1-O TO WS-COMMISSION-COST
		END-IF
	   WHEN 2
		IF IN-REGION-NO = 01 OR 02 
			COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP * 
            WS-COMMISSION2-3	
		ELSE 
			 MOVE WS-COMMISSION2-O TO WS-COMMISSION-COST
	    END-IF
	   WHEN 3
		IF IN-REGION-NO = 01
			COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP *  
           WS-COMMISSION3-1
		ELSE 
           COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP *
           WS-COMMISSION-O
        END-IF
	   WHEN 4
		IF IN-REGION-NO = 02 
			MOVE WS-COMMISSION4-2 TO WS-COMMISSION-COST
        ELSE 
            COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP *
            WS-COMMISSION-O
		END-IF
	   WHEN OTHER
		COMPUTE WS-COMMISSION-COST = IN-COST-OF-TRIP * WS-COMMISSION-O
       END-EVALUATE.
       600-EXIT.
       
      ******************************************************************
      * 610-CLACLUATE-TOTALS KEEPS A RUNNING TOTAL OF THE TOTAL TRIP 
      * COST FOR ALL EMPLOYEES AS WELL AS A TOTAL COMMISION COST.
      ******************************************************************
       
       610-CALUCULATE-TOTALS.
       COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + IN-COST-OF-TRIP
       COMPUTE WS-COMMISSION-TOTAL = WS-COMMISSION-TOTAL + 
               WS-COMMISSION-COST.
       610-EXIT.
       
      ******************************************************************
      * 650-PRINT-FOOTER PRINTS A BLANK LINE BEFORE PRINTNING THE FINAL
      * FOOTER OF THE PROGRAM
      ******************************************************************
       650-PRINT-FOOTER.
       WRITE OUTPUT-RECORD FROM FOOTER.
       650-EXIT.
      ******************************************************************
      * 700-PRINT-FOOTER PRINTS THE FINAL FOOTER OF THE REPORT. 
      ******************************************************************
       
       700-PRINT-FOOTER1.
       WRITE OUTPUT-RECORD FROM FOOTER
           AFTER ADVANCING 1 LINE
       MOVE WS-CLIENT-COUNT TO NO-OF-CLIENTS
       MOVE WS-TOTAL-COST TO TOTAL-TRIP-COST
       MOVE WS-COMMISSION-TOTAL TO TOTAL-COMMISION-COST
       WRITE OUTPUT-RECORD FROM FOOTER1.
       700-EXIT.
       
      ******************************************************************
      * 900-TERMINATION-MODULE CLOSES BOTH THE INPUT AND OUTPUT FILES
      ******************************************************************
       
       900-TERMINATION-MODULE. 
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.
       900-EXIT.
       