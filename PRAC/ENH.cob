       IDENTIFICATION DIVISION.
       PROGRAM-ID. STU.
      *AUTHOR. LAWRENCE .
      *INSTALLATION. BAHAY.
      *DATE-WRITTEN. JANUARY 9, 2026.
      *DATE-COMPILED. JANUARY 9, 2026.
      *SECURITY. EXCLUSIVE FOR BSIT 2-4.
      *REMARKS. ENHANCED PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO 'STUDSOUT'.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS OUTREC.
       01  OUTREC.
           02 D-SNO PIC 9(5).
           02 D-SNA PIC X(25).
           02 D-CRS PIC 9.
           02 D-YRSEC PIC X(5).
           02 D-STYPE PIC 9.
           02 D-MIDG PIC 9V99.
           02 D-FING PIC 9V99.
           02 D-AVE PIC 9V99.
           02 D-REM PIC X(6).

       WORKING-STORAGE SECTION.
       01  SNO PIC 9(5) VALUE ZERO.
       01  SNA PIC X(25) VALUE ZERO.
       01  CRS PIC 9 VALUE ZERO. 
       01  YRSEC PIC X(5) VALUE ZERO.
       01  STYPE PIC 9 VALUE ZERO.
       01  MIDG PIC 9V99 VALUE ZERO.
       01  FING PIC 9V99 VALUE ZERO.
       01  AVE PIC 9V99 VALUE ZERO.
       01  REM PIC X(6) VALUE SPACES.
       01  VALID PIC X VALUE 'N'.
       01  ANS PIC X VALUE 'Y'.
       01  DISP-AVE PIC 9.999 VALUE ZERO.
       
       SCREEN SECTION.
       01  SCRE. 
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN OUTPUT OUTFILE.
           PERFORM PROCESS-RTN THRU PROCESS-END
               UNTIL ANS = 'N' OR ANS = 'n'.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           DISPLAY SCRE.
           DISPLAY 'PUP' LINE 1 COLUMN 38.
           DISPLAY 'CCIS' LINE 2 COLUMN 38.
       
           DISPLAY 'Student Number: ' LINE 5 COLUMN 1.
           ACCEPT SNO LINE 5 COLUMN 40.

           DISPLAY 'Student Name: ' LINE 6 COLUMN 1.
           ACCEPT SNA LINE 6 COLUMN 40.

           DISPLAY 'Course: ' LINE 7 COLUMN 1.
           MOVE 'N' TO VALID. 
           PERFORM UNTIL VALID = 'Y'
               ACCEPT CRS LINE 7 COLUMN 40
               IF CRS = 1 OR CRS = 2
                   MOVE 'Y' TO VALID
                   DISPLAY '                         ' LINE 23 COLUMN 1
               ELSE
                   DISPLAY 'Invalid Course (1-2 Only)' LINE 23 COLUMN 1
               END-IF
           END-PERFORM.

           DISPLAY 'Year & Section: ' LINE 8 COLUMN 1.
           ACCEPT YRSEC LINE 8 COLUMN 40.
           
           DISPLAY 'Student Type: ' LINE 9 COLUMN 1.
           MOVE 'N' TO VALID. 
           PERFORM UNTIL VALID = 'Y'
               ACCEPT STYPE LINE 9 COLUMN 40
               IF STYPE = 1 OR STYPE = 2
                   MOVE 'Y' TO VALID
                   DISPLAY '                       ' LINE 23 COLUMN 1
               ELSE
                   DISPLAY 'Invalid Type (1-2 Only)' LINE 23 COLUMN 1
               END-IF
           END-PERFORM.

           DISPLAY 'Midterm Grade: ' LINE 10 COLUMN 1.
           MOVE 'N' TO VALID. 
           PERFORM UNTIL VALID = 'Y'
               ACCEPT MIDG LINE 10 COLUMN 40
               IF MIDG GREATER THAN 0.99 AND LESS THAN 5.01
                   MOVE 'Y' TO VALID
                   DISPLAY '                         ' LINE 23 COLUMN 1
               ELSE
                   DISPLAY 'Invalid Grade (1.00-5.00)' LINE 23 COLUMN 1
               END-IF
           END-PERFORM.

           DISPLAY 'Final Grade: ' LINE 11 COLUMN 1.
           MOVE 'N' TO VALID. 
           PERFORM UNTIL VALID = 'Y'
               ACCEPT FING LINE 11 COLUMN 40
               IF FING GREATER THAN 0.99 AND LESS THAN 5.01
                   MOVE 'Y' TO VALID
                   DISPLAY '                       ' LINE 23 COLUMN 1
               ELSE
                   DISPLAY 'Invalid Grade (1.00-5.00)' LINE 23 COLUMN 1
               END-IF
           END-PERFORM.

           DISPLAY 'Average: ' LINE 12 COLUMN 1.
           COMPUTE AVE = (MIDG + FING) / 2
           MOVE AVE TO DISP-AVE.
           DISPLAY DISP-AVE LINE 12 COLUMN 40.

           DISPLAY 'Remarks: ' LINE 13 COLUMN 1.
           IF AVE LESS THAN 3.01
               DISPLAY "PASSED" LINE 13 COLUMN 40           
           ELSE
               DISPLAY "FAILED" LINE 13 COLUMN 40.

           MOVE SNO TO D-SNO.
           MOVE SNA TO D-SNA.
           MOVE CRS TO D-CRS.
           MOVE YRSEC TO D-YRSEC.
           MOVE STYPE TO D-STYPE.
           MOVE MIDG TO D-MIDG.
           MOVE FING TO D-FING.
           MOVE AVE TO D-AVE.
           MOVE REM TO D-REM.

           DISPLAY "Enter Another Record? (Y/N)" LINE 15 COLUMN 1
           ACCEPT ANS.
           WRITE OUTREC.
       PROCESS-END.