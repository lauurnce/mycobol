       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSP.
      *AUTHOR. HAROLD.
      *INSTALLATION. HOME.
      *DATE-WRITTEN. JANUARY.
      *SECURITY. EXCLUSIVE FOR BSIT 2-4.
      *REMARKS. PRACTICE PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "ECNERWAL".
       
       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS OUTREC.
       01  OUTREC.
           02 D-NOS PIC X(20).
           02 D-TOS PIC X(20).
           02 D-CCRWS PIC X(6).
           02 D-MATA PIC X(10).
           02 D-MON PIC X(12).

       WORKING-STORAGE SECTION.
       01  NOS PIC X(20) VALUE SPACES.
       01  WS PIC 9(3) VALUE 0.
       01  TOS PIC X(20) VALUE SPACES.
       01  CCRWS PIC X(6) VALUE SPACES.
       01  AR PIC X(20) VALUE SPACES.
       01  MATA PIC X(10) VALUE SPACES.
       01  MONO PIC 9(2) VALUE 0.
       01  MON PIC X(12) VALUE SPACES.

       01  MAXS PIC 9(3) VALUE 0.
       01  SSY PIC X(20) VALUE SPACES.

       01  MRL PIC 9 VALUE 0.
       01  CRL PIC 9 VALUE 0.
       01  MFFA PIC X(10) VALUE SPACES.

       01  MONTH-TABLE.
           05 MONTH-COUNT    PIC 99 VALUE 0 OCCURS 12 TIMES.
       01  IDX               PIC 99 VALUE 0.
       01  MAX-MONTH-COUNT   PIC 99 VALUE 0.
       01  MMNS              PIC X(12) VALUE SPACES.

       01  ANS PIC X VALUE SPACES.
       01  VALID-SW PIC X VALUE SPACES.

       SCREEN SECTION.
       01  SCRE.
           02 DISPLAY BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN OUTPUT OUTFILE.
           PERFORM PROCESS-RTN THRU PROCESS-END
               UNTIL ANS = 'N' OR ANS = 'n'.
           PERFORM SUMMARY-RTN.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           DISPLAY SCRE.
           DISPLAY (1, 37) 'PAG-ASA'.
           DISPLAY (3, 22) 'Weather Situtaion in the Philippines'.
           DISPLAY (4, 35) 'Year 2025'.

           DISPLAY (6, 1) 'Name of Storm: '.
           ACCEPT (6, 45) NOS.

           DISPLAY (7, 1) 'Wind Speed (in kmph): '.
           MOVE 'N' TO VALID-SW
           ACCEPT (7, 45) WS.
           EVALUATE WS
               WHEN 0 THRU 62
                   MOVE "LOW PRESSURE AREA" TO TOS
               WHEN 63 THRU 88
                   MOVE "TROPICAL DEPRESSION" TO TOS
               WHEN 89 THRU 118
                   MOVE "TYPHOON SIGNAL NO.3" TO TOS
               WHEN 119 THRU 184
                   MOVE "TYPHOON SIGNAL NO.4" TO TOS
               WHEN 185 THRU 999
                   MOVE "TYPHOON SIGNAL NO.5" TO TOS
           END-EVALUATE.

           IF WS >= MAXS
               MOVE WS TO MAXS
               MOVE NOS TO SSY
           END-IF.

           DISPLAY (8, 1) 'Type of Storm: '.
           DISPLAY (8, 45) TOS.
           
           DISPLAY (9, 1) 'Color Coded Rainfall Warning System: '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
           ACCEPT (9, 45) CCRWS
           EVALUATE CCRWS
               WHEN "YELLOW"
                   MOVE "Response Monitor" TO AR
                   MOVE 1 TO CRL
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN "ORANGE"
                   MOVE "Response Alert" TO AR
                   MOVE 2 TO CRL
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN "RED"
                   MOVE "Response Evacuation" TO AR
                   MOVE 3 TO CRL
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN OTHER
                   DISPLAY (23, 1) "Invalid Input!"
                   MOVE 'N' TO VALID-SW
           END-EVALUATE
           END-PERFORM.

           DISPLAY (10, 1) 'Action/Response: '.
           DISPLAY (10, 45) AR.

           DISPLAY (11, 1) 'Most Affected or Target Area: '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
           ACCEPT (11, 45) MATA
           EVALUATE MATA
               WHEN "LUZON"
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN "VISAYAS"
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN "MINDANAO"
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                   "
               WHEN OTHER
                   DISPLAY (23, 1) "Invalid Input!"
                   MOVE 'N' TO VALID-SW
           END-EVALUATE
           END-PERFORM.

           IF CRL >= MRL
               MOVE CRL TO MRL
               MOVE MATA TO MFFA
           END-IF.

           DISPLAY (12, 1) 'Month Occurred (1-12): '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (12, 45) MONO
                   IF MONO > 0 AND MONO <= 12
                       MOVE 'Y' TO VALID-SW
                       DISPLAY (23, 1) "                "
                   ELSE
                       DISPLAY (23, 1) "1-12 Only!"
                       MOVE ZERO TO MONO
                   END-IF
           END-PERFORM.
           EVALUATE MONO
               WHEN 1 MOVE "January" TO MON
               WHEN 2 MOVE "February" TO MON
               WHEN 3 MOVE "March" TO MON
               WHEN 4 MOVE "April" TO MON
               WHEN 5 MOVE "May" TO MON
               WHEN 6 MOVE "June" TO MON
               WHEN 7 MOVE "July" TO MON
               WHEN 8 MOVE "August" TO MON
               WHEN 9 MOVE "September" TO MON
               WHEN 10 MOVE "October" TO MON
               WHEN 11 MOVE "November" TO MON
               WHEN 12 MOVE "December" TO MON
           END-EVALUATE.

           IF MONO >= 1 AND MONO <= 12
               ADD 1 TO MONTH-COUNT(MONO)
           END-IF.

           MOVE NOS TO D-NOS.
           MOVE TOS TO D-TOS.
           MOVE CCRWS TO D-CCRWS.
           MOVE MATA TO D-MATA.
           MOVE MON TO D-MON.

           WRITE OUTREC.

           DISPLAY (14, 30) 'Input Another Record?'.
           ACCEPT ANS. 
       PROCESS-END.

       SUMMARY-RTN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
               IF MONTH-COUNT(IDX) >= MAX-MONTH-COUNT
                   MOVE MONTH-COUNT(IDX) TO MAX-MONTH-COUNT
                   EVALUATE IDX
                       WHEN 1 MOVE "January" TO MMNS
                       WHEN 2 MOVE "February" TO MMNS
                       WHEN 3 MOVE "March" TO MMNS
                       WHEN 4 MOVE "April" TO MMNS
                       WHEN 5 MOVE "May" TO MMNS
                       WHEN 6 MOVE "June" TO MMNS
                       WHEN 7 MOVE "July" TO MMNS
                       WHEN 8 MOVE "August" TO MMNS
                       WHEN 9 MOVE "September" TO MMNS
                       WHEN 10 MOVE "October" TO MMNS
                       WHEN 11 MOVE "November" TO MMNS
                       WHEN 12 MOVE "December" TO MMNS
                   END-EVALUATE
               END-IF
           END-PERFORM. 

           DISPLAY (16, 1) 'The Strongest Storm in a Year is: '.
           DISPLAY (16, 45) SSY.

           DISPLAY (17, 1) 'The Most Flooded Flooded Area: '.
           DISPLAY (17, 45) MFFA.

           DISPLAY (18, 1) 'The Month with the Most Storms'.
           DISPLAY (18, 45) MMNS.

           DISPLAY (22, 20) 'Press Enter to Exit'.
           ACCEPT ANS.