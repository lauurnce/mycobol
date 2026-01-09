       IDENTIFICATION DIVISION.
       PROGRAM-ID. KEZIA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
         
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SNO PIC 9(5) VALUE ZERO.
       01  SNA PIC X(25) VALUE ZERO.
       01  CRS PIC 9 VALUE ZERO.
       01  YS PIC X(5) VALUE ZERO.
       01  ST PIC 9 VALUE ZERO.
       01  MG PIC 9V99 VALUE ZERO.
       01  FG PIC 9V99 VALUE ZERO.
       01  AVG PIC 9V99 VALUE ZERO.
       01  REM PIC X(6) VALUE SPACES.
       01  VAL PIC X VALUE ZERO.

       SCREEN SECTION.
       01  SCRE.
           02 BLANK SCREEN.
       
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM PROCESS-RTN THRU PROCESS-END
               UNTIL VAL = 'N' OR VAL = 'n'.
           STOP RUN.

       PROCESS-RTN.
           DISPLAY SCRE.
           DISPLAY 'PUP' LINE 1 COLUMN 20 .
           DISPLAY 'CCIS' LINE 2 COLUMN 19.
           DISPLAY 'Student Number: ' LINE 5 COLUMN 1.
           ACCEPT SNO LINE 5 COLUMN 20.
           DISPLAY 'Student Name: ' LINE 6 COLUMN 1.
           ACCEPT SNA LINE 6 COLUMN 20.
           DISPLAY 'Course: ' LINE 7 COLUMN 1.
           ACCEPT CRS LINE 7 COLUMN 20.
           DISPLAY 'Year & Section: ' LINE 8 COLUMN 1.
           ACCEPT YS LINE 8 COLUMN 20.
           DISPLAY 'Student Type: ' LINE 9 COLUMN 1.
           ACCEPT ST LINE 9 COLUMN 20.
           DISPLAY 'Midterm Grade: ' LINE 10 COLUMN 1.
           ACCEPT MG LINE 10 COLUMN 20.
           DISPLAY 'Final Grade: ' LINE 11 COLUMN 1.
           ACCEPT FG LINE 11 COLUMN 20.
           DISPLAY 'Average Grade: ' LINE 12 COLUMN 1.
           COMPUTE AVG = (MG + FG) / 2.
           DISPLAY AVG LINE 12 COLUMN 20.
           DISPLAY 'Remarks: ' LINE 13 COLUMN 1.
           IF AVG < 3.01
               MOVE "PASSED" TO REM
           ELSE
               MOVE "FAILED" TO REM.
           DISPLAY REM LINE 13 COLUMN 20.
           DISPLAY "Enter Another Record? (Y/N)" LINE 15 COLUMN 1
           ACCEPT VAL.
       PROCESS-END.