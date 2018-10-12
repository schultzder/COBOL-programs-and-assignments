       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM1.
       AUTHOR. DEREK R SCHULTZ.
       DATE-WRITTEN. FEB 14, 2018.
       DATE-COMPILED. TBD.
      ******************************************************************
      *                                                                
      * In this program customers will enter there personal information
      * as well as a bike with any accessories to pruchase. The Program
      * will then calculate a subtotal, discount total, net sale, and 
      * tax rate. After the calculations are completed, the program 
      * will produce a printable invoice. 
      *
      * Input: Customer name, adress, city, state, zip, description of
      * bicyle and price, and accessories with a description and price.
      *
      *
      * Output: The output will be a single invoice of the purchase.
      * 
      * 
      * Date/Time due: Feb 23, 2018
      * Date assigned: Feb 12, 2018
      * data files: none
      ******************************************************************
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Working vaiables section for customer information.
        
       01  WV-CUSTOMER-INFO.
           05 WV-NAME                  PIC X(40).
           05 WV-ADDRESS               PIC X(25).
           05 WV-PHONE                 PIC X(15).
           05 WV-CITY-STATE-ZIP        PIC X(22).
           05 WV-BIKE.                  
              10 WV-BIKE-DESCRIPTION   PIC X(25).
              10 WV-BIKE-PRICE         PIC 99999V99.
           05 WV-ACESSORY.          
              10 WV-ACCESS-DESCRIPTION PIC X(20).
              10 WV-ACCESS-PRICE       PIC 99999V99.
           05 WV-SUM-ACCESS            PIC 999999V99.  
           05 WV-LINE-COUNT            PIC 999.       
           05 WV-ACCESS-COUNT          PIC 9          VALUE 1.
           05 WV-SUBTOTAL-SUM          PIC 999999V99.
           05 WV-ACCESS-DISCOUNT       PIC 99999V99.
           05 WV-BIKE-DISCOUNT         PIC 99999V99.
           05 WV-DISCOUNT-SUM          PIC 99999V99.
           05 WV-NET-SALE              PIC 99999V99.
           05 WV-SALES-TAX             PIC 99999V99.
           05 WV-ENTER                 PIC X.
           
      * Constant variables used for calculations.
        
       01  CO-CONSTANTS.
           05 CO-NO-DISCOUNT           PIC 9     VALUE 0.
           05 CO-MID-DISCOUNT-BIKE     PIC 9V99  VALUE 0.05.
           05 CO-HIGH-DISCOUNT-BIKE    PIC 9V9   VALUE 0.1.
           05 CO-MID-DISCOUNT-ACCESS   PIC 9V99  VALUE 0.02.
           05 CO-HIGH-DISCOUNT-ACCESS  PIC 9V99  VALUE 0.03.
           05 CO-SALES-TAX             PIC 9V999 VALUE 0.055.
           
      * Variables used for outputting purposes.
        
       01  WO-OUTPUT-FIELDS.
           05  WO-SUBTOTAL             PIC ZZZZ9.99.
           05  WO-NET-SALE             PIC ZZZZ9.99.
           05  WO-INVOICE-TOTAL        PIC ZZZZ9.99.
           05  WO-ACCESS-PRICE         PIC ZZZZ9.99.
           05  WO-BIKE-PRICE           PIC ZZZZ9.99.
           05  WO-DISCOUNT-SUM         PIC ZZZZ9.99.
           05  WO-SALES-TAX            PIC ZZZZ9.99.
      *
       PROCEDURE DIVISION.
      ******************************************************************
      * 000-MAIN-RTN RUNS THE MAIN MODULES FOR THE ENTIRE PROGRAM
      ******************************************************************
       000-MAIN-RTN.
       
      * Main block of performs.
        
           PERFORM 100-PRINT-BLOCK1 THRU 100-EXIT.
           PERFORM 200-PRINT-BLOCK2 THRU 200-EXIT.
           PERFORM 400-PRINT-RESULTS THRU 400-EXIT.
           
      * Final display.
        
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "PRESS ENTER TO EXIT"  
               LINE 25 COLUMN 1
           STOP RUN.
           
           
      ******************************************************************
      * 100-PRINT-BLOCK1 WILL DISPLAY THE CUSTOMER INFORMATION HEADING
      * 
      ******************************************************************
       100-PRINT-BLOCK1.
       
      * Displays the first 'block' of the invoice. Contains customer
      * information. 
        
           DISPLAY "Derek Schultz"
           DISPLAY "Bicycle Invoice"            LINE 1 COLUMN 40
           DISPLAY "Customer Name"              LINE 3 COLUMN 1
           DISPLAY ":"                          LINE 3 COLUMN 30
           ACCEPT WV-NAME                       LINE 3 COLUMN 32
           DISPLAY "Customer Address"           LINE 4 COLUMN 1
           DISPLAY ":"                          LINE 4 COLUMN 30
           ACCEPT WV-ADDRESS                    LINE 4 COLUMN 32
           DISPLAY "Customer City, State, Zip"  LINE 5 COLUMN 1
           DISPLAY ":"                          LINE 5 COLUMN 30
           ACCEPT WV-CITY-STATE-ZIP             LINE 5 COLUMN 32
           DISPLAY "Customer Phone #"           LINE 6 COLUMN 1
           DISPLAY ":"                          LINE 6 COLUMN 30
           ACCEPT WV-PHONE                      LINE 6 COLUMN 32
           ADD 8 TO WV-LINE-COUNT.
       100-EXIT.
       
      ******************************************************************
      * 200-ENTER-TRXS IS A LOOP THAT ASKS THE USER IF THE TRANSACTION
      *                 IS A DEPOSIT OR WITHDRAWAL AND FOR THE AMOUNT
      *                 OF THE TRANSACTION.  IT THEN COMPUTES AND
      *                 DISPLAYS THE BALANCE.
      ******************************************************************
       200-PRINT-BLOCK2.
       
      * Displays the second 'block' of the invoice. Contains 
      * informations regardering the items ordered.
        
           DISPLAY "ITEMS ORDERED"        LINE WV-LINE-COUNT COLUMN 1
           DISPLAY "Price"                LINE WV-LINE-COUNT COLUMN 50
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Bicycle:"             LINE WV-LINE-COUNT COLUMN 1
           ACCEPT WV-BIKE-DESCRIPTION     LINE WV-LINE-COUNT COLUMN 11
           ACCEPT WV-BIKE-PRICE           LINE WV-LINE-COUNT COLUMN 47
           ADD WV-BIKE-PRICE TO WV-SUBTOTAL-SUM
           MOVE WV-BIKE-PRICE TO WO-BIKE-PRICE
           DISPLAY WO-BIKE-PRICE          LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "XXXXX to end accessories"
               LINE WV-LINE-COUNT COLUMN 1
           ADD 1 TO WV-LINE-COUNT
           
      * Start of the accessory loop. Will stop once accessory count is 
      * greater than 4 or the accessory description is 'XXXXX'
        
           PERFORM 300-PROCESS-ACCESS THRU 300-EXIT
               UNTIL WV-ACCESS-COUNT > 4 
               OR WV-ACCESS-DESCRIPTION = "XXXXX".
       200-EXIT.
               
           
           
      ******************************************************************
      * 300-PROCESS-ACCESS IS A LOOP THAT PROCESSES BIKE ACCESSORIES
      * UNTILL THE ACESSORY NAME VALUE IS 'XXXXX' OR THE ACCESSORY COUNT
      * REACHES 5. 
      ******************************************************************
       300-PROCESS-ACCESS.
       
      * Display set up for the accessories.
        
           DISPLAY "Accessory "           LINE WV-LINE-COUNT COLUMN 1
           DISPLAY WV-ACCESS-COUNT        LINE WV-LINE-COUNT COLUMN 11
           DISPLAY ":"                    LINE WV-LINE-COUNT COLUMN 12
           ACCEPT WV-ACCESS-DESCRIPTION   LINE WV-LINE-COUNT COLUMN 14
        
      * If statement to check if the accessory entered is 'XXXXX'
        
           IF WV-ACCESS-DESCRIPTION = "XXXXX"
               GO TO 300-EXIT
           END-IF
           
      * If the if condition fails, then contiues to accept the price
      * and do calculations for sub totaling.
        
           ACCEPT WV-ACCESS-PRICE         LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-ACCESS-COUNT
           ADD WV-ACCESS-PRICE TO WV-SUBTOTAL-SUM
           ADD WV-ACCESS-PRICE TO WV-SUM-ACCESS
           MOVE WV-ACCESS-PRICE TO WO-ACCESS-PRICE
           MOVE SPACES TO WV-ACCESS-DESCRIPTION
           MOVE ZEROS TO WV-ACCESS-PRICE
           DISPLAY WO-ACCESS-PRICE        LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-LINE-COUNT.
       300-EXIT.
           
      ******************************************************************
      * 400-PRINT-RESULTS DOES THE FINAL SUBTOTALING OF THE ORDER. 
      * THIS MODUAL ALSO CALLS A SUB FUNCTION TO CALCULATE 
      * THE DISCOUNTS. WITHIN THIS MODUAL, THE SALES TAX WILL ALSO BE 
      * CALCULATED.
      * FINALLY IT WILL DISPLAY THE INVOICE TOTAL FOR THE ORDER
      ******************************************************************
       400-PRINT-RESULTS.
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "----------"           LINE WV-LINE-COUNT COLUMN 45
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Subtotal"             LINE WV-LINE-COUNT COLUMN 1
           MOVE WV-SUBTOTAL-SUM TO WO-SUBTOTAL
           DISPLAY WO-SUBTOTAL            LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Discount"             LINE WV-LINE-COUNT COLUMN 1
           
      * Start of perform for calculating the discount
        
           PERFORM 500-CALC-DISCOUNT-BIKE THRU 500-EXIT
           PERFORM 600-CLAC-DISCOUNT-ACCESS THRU 600-EXIT
           ADD WV-ACCESS-DISCOUNT TO WV-BIKE-DISCOUNT GIVING 
           WV-DISCOUNT-SUM
           MOVE WV-DISCOUNT-SUM TO WO-DISCOUNT-SUM
           DISPLAY WO-DISCOUNT-SUM        LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "----------"           LINE WV-LINE-COUNT COLUMN 45
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Net Sale"             LINE WV-LINE-COUNT COLUMN 1
           SUBTRACT WV-DISCOUNT-SUM FROM WV-SUBTOTAL-SUM GIVING 
           WO-NET-SALE
           DISPLAY WO-NET-SALE            LINE WV-LINE-COUNT COLUMN 47
           MOVE WO-NET-SALE TO WV-NET-SALE
           MULTIPLY CO-SALES-TAX BY WV-NET-SALE GIVING WO-SALES-TAX
           MOVE WO-SALES-TAX TO WV-SALES-TAX
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Sales Tax"            LINE WV-LINE-COUNT COLUMN 1
           DISPLAY WO-SALES-TAX           LINE WV-LINE-COUNT COLUMN 47
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "----------"           LINE WV-LINE-COUNT COLUMN 45
           ADD 1 TO WV-LINE-COUNT
           DISPLAY "Invoice Total"        LINE WV-LINE-COUNT COLUMN 1
           ADD WV-SALES-TAX TO WV-NET-SALE GIVING 
           WO-INVOICE-TOTAL
           DISPLAY WO-INVOICE-TOTAL       LINE WV-LINE-COUNT COLUMN 47.
       400-EXIT.
       
      ******************************************************************
      * 500-CALC-DISCOUNT CALCULATES THE TOATAL DISCOUNT APPLIED FOR 
      * THE BIKE. SENDS THE VALUES CALCULATED TO VARIABLES USED FOR
      * STROING THE VALUES. 
      ******************************************************************
       500-CALC-DISCOUNT-BIKE.
       
      * If statments for the bike discount.
        
           IF WV-BIKE-PRICE < 1000
               MULTIPLY CO-NO-DISCOUNT BY WV-BIKE-PRICE GIVING 
               WV-BIKE-DISCOUNT
           ELSE IF 1000 <= WV-BIKE-PRICE AND WV-BIKE-PRICE <= 1500
               MULTIPLY CO-MID-DISCOUNT-BIKE BY WV-BIKE-PRICE GIVING 
               WV-BIKE-DISCOUNT
           ELSE 
               MULTIPLY CO-HIGH-DISCOUNT-BIKE BY WV-BIKE-PRICE GIVING 
               WV-BIKE-DISCOUNT
           END-IF.
       500-EXIT.
       
      ******************************************************************
      * 600-CALC-DISCOUNT-ACCESS CALCULATES THE TOATAL DISCOUNT APPLIED
      * FOR THE ACCESSORIES. SENDS THE VALUES CALCULATED TO VARIABLES
      * USED FOR STROING THE VALUES. 
      ******************************************************************
       600-CLAC-DISCOUNT-ACCESS.
        
      * If statments for the accessory discount.    
           
           IF WV-SUM-ACCESS < 100
               MULTIPLY CO-NO-DISCOUNT BY WV-SUM-ACCESS GIVING 
               WV-ACCESS-DISCOUNT
           ELSE IF 100 <= WV-SUM-ACCESS AND WV-SUM-ACCESS <= 200
               MULTIPLY CO-MID-DISCOUNT-ACCESS BY WV-SUM-ACCESS GIVING 
               WV-ACCESS-DISCOUNT
           ELSE 
               MULTIPLY CO-HIGH-DISCOUNT-ACCESS BY WV-SUM-ACCESS GIVING 
               WV-ACCESS-DISCOUNT
           END-IF.
       600-EXIT.
       
           
           