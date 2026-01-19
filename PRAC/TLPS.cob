       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLPS.
      *AUTHOR. LAWRENCE.
      *INSTALLATION. HOME.
      *DATE-WRITTEN. JANUARY 19.
      *SECURITY. EXCLUSIVE FOR BSIT 2-4.
      *REMARKS. PRACTICE PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "LAWR".
       
       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS OUTREC.
       01  OUTREC.
           02 D-MN PIC X(12).
           02 D-NOTS PIC X(25).
           02 D-NOV PIC 9(6).

       WORKING-STORAGE SECTION.
       01  SC PIC 9 VALUE 0.
       01  NOTS PIC X(20) VALUE SPACES.
       01  MC PIC 99 VALUE 0.
       01  MN PIC X(12) VALUE SPACES.
       01  TC PIC 9 VALUE 0.
       01  AIRT PIC X(16) VALUE SPACES.
       01  NOV PIC 9(6) VALUE 0.

       01  VALID-SW PIC X VALUE SPACES.
       01  ANS PIC X VALUE SPACES.

       01  ARRAYS.
           05 STORED-TOTALS PIC 9(7) OCCURS 4 TIMES VALUE 0.
           05 STORED-TIMES PIC X(16) OCCURS 4 TIMES VALUE SPACES.
           05 STORED-NAMES PIC X(25) OCCURS 4 TIMES VALUE SPACES.
           
       01  I PIC 9 VALUE 0.
       01  MW-INDEX PIC 9 VALUE 0.

       01  MWTVS PIC X(25) VALUE SPACES.
       01  MAXTV PIC 9(6) VALUE 0.
       01  MWAIRT PIC X(16) VALUE SPACES.
       01  DISPLAY-MAX PIC 9(6).

       SCREEN SECTION.
       01  SCRE.
           02 DISPLAY BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN OUTPUT OUTFILE.
           PERFORM PROCESS-RTN THRU PROCESS-END
               UNTIL ANS = 'N' OR ANS = 'n'
           PERFORM SUMMARY-RTN.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           DISPLAY SCRE.
           DISPLAY (1, 22) "Television's Leading Primetime Shows".
           DISPLAY (2, 24) "ABS-CBN and GMA Network".
           DISPLAY (4, 25) "January - December, 2025 Survey".

           DISPLAY (6, 1) "Show Code: ".
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
           ACCEPT (6, 45) SC
               IF SC >= 1 AND SC <= 4
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "            "
               ELSE 
                   DISPLAY (24, 1) "Invalid Show Code."
               END-IF
           END-PERFORM.

           EVALUATE SC
               WHEN 1
                   MOVE "Eat Bulaga" TO NOTS
               WHEN 2
                   MOVE "Its Showtime" TO NOTS
               WHEN 3
                   MOVE "PBB" TO NOTS
               WHEN 4
                   MOVE "Probinsiano" TO NOTS
           END-EVALUATE.

           DISPLAY (7, 1) "Name of the Show: ".
           DISPLAY (7, 45) NOTS.

           DISPLAY (8, 1) "Month Code: ".
           MOVE 'N' TO VALID-SW
           PERFORM UNTIL VALID-SW = 'Y'
           ACCEPT (8, 45) MC
               IF MC >= 1 AND MC <= 12
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "             "
               ELSE 
                   DISPLAY (24, 1) "Invalid Month Code."
               END-IF
           END-PERFORM.

           EVALUATE MC
               WHEN 1 MOVE "JANUARY" TO MN
               WHEN 2 MOVE "FEBRUARY" TO MN
               WHEN 3 MOVE "MARCH" TO MN
               WHEN 4 MOVE "APRIL" TO MN
               WHEN 5 MOVE "MAY" TO MN
               WHEN 6 MOVE "JUNE" TO MN
               WHEN 7 MOVE "JULY" TO MN
               WHEN 8 MOVE "AUGUST" TO MN
               WHEN 9 MOVE "SEPTEMBER" TO MN
               WHEN 10 MOVE "OCTOBER" TO MN
               WHEN 11 MOVE "NOVEMBER" TO MN
               WHEN 12 MOVE "DECEMBER" TO MN
           END-EVALUATE.

           DISPLAY (9, 1) "Month Name: ".
           DISPLAY (9, 45) MN.

           DISPLAY (10, 1) "Time Code: ".
           MOVE 'N' TO VALID-SW
           PERFORM UNTIL VALID-SW = 'Y'
           ACCEPT (10, 45) TC
               IF TC = 1 OR TC = 2
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "         "
               ELSE
                   DISPLAY (24, 1) "Invalid Time Code."
               END-IF
           END-PERFORM.

           EVALUATE TC
               WHEN 1
                   MOVE "12:00 - 2:30 pm" TO AIRT
               WHEN 2
                   MOVE "8:00 - 10:00 pm" TO AIRT
           END-EVALUATE.

           DISPLAY (11, 1) "Airtime: ".  
           DISPLAY (11, 45) AIRT.

           DISPLAY (12, 1) "Number of Televiewers: ".
           ACCEPT (12, 45) NOV.

           ADD NOV TO STORED-TOTALS(SC).
           MOVE AIRT TO STORED-TIMES(SC).
           MOVE NOTS TO STORED-NAMES(SC).

           DISPLAY (14, 1) "Input Another Record (Y/N)?".
           ACCEPT ANS.

           MOVE MN TO D-MN.
           MOVE NOTS TO D-NOTS.
           MOVE NOV TO D-NOV.

           WRITE OUTREC.
       PROCESS-END.

       SUMMARY-RTN.
           MOVE 0 TO MAXTV.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF STORED-TOTALS(I) >= MAXTV
                   MOVE STORED-TOTALS(I) TO MAXTV
                   MOVE STORED-TIMES(I) TO MWAIRT
                   MOVE STORED-NAMES(I) TO MWTVS
               END-IF
           END-PERFORM.

           DISPLAY (16, 1) "Most Watched Television Show: ".
           DISPLAY (16, 45) MWTVS.

           DISPLAY (17, 1) "Total No. of Televiewers: ".
           DISPLAY (17, 45) MAXTV.

           DISPLAY (18, 1) "Airtime: ".
           DISPLAY (18, 45) MWAIRT.

           DISPLAY (22, 20) 'Press Enter to Exit'.
           ACCEPT ANS.