       IDENTIFICATION DIVISION.
       PROGRAM-ID. KEZIA-REV.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENT.TXT"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE.
       01  STUDENT-REC.
           05 R-SNO       PIC 9(5).
           05 R-SNA       PIC X(25).
           05 R-CRS-DESC  PIC X(4).
           05 R-YS        PIC X(5).
           05 R-ST-DESC   PIC X(9).
           05 R-MG        PIC 9V99.
           05 R-FG        PIC 9V99.
           05 R-AVG       PIC 9V99.
           05 R-REM       PIC X(6).

       WORKING-STORAGE SECTION.
       01  SNO            PIC 9(5) VALUE ZERO.
       01  SNA            PIC X(25) VALUE SPACES.
       01  YS             PIC X(5) VALUE SPACES.
       01  MG             PIC 9V99 VALUE ZERO.
       01  FG             PIC 9V99 VALUE ZERO.

       01  RAW-CRS        PIC 9 VALUE ZERO.
       01  RAW-ST         PIC 9 VALUE ZERO.
       01  VALID-FLAG     PIC X VALUE 'N'.

       01  D-CRS          PIC X(4) VALUE SPACES.
       01  D-ST           PIC X(9) VALUE SPACES.
       01  AVG            PIC 9V99 VALUE ZERO.
       01  DISP-AVG       PIC Z9.99.
       01  REM            PIC X(6) VALUE SPACES.
       
       01  VAL            PIC X VALUE 'Y'.

       SCREEN SECTION.
       01  CLS.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND STUDENT-FILE.

           PERFORM PROCESS-RTN UNTIL VAL = 'N' OR VAL = 'n'.

           CLOSE STUDENT-FILE.
           STOP RUN.

       PROCESS-RTN.
           DISPLAY CLS.
           DISPLAY 'PUP - CCIS ENROLLMENT SYSTEM' LINE 2 COLUMN 25.
           DISPLAY '----------------------------' LINE 3 COLUMN 25.

      
           DISPLAY 'Student Number : ' LINE 5 COLUMN 5.
           ACCEPT SNO LINE 5 COLUMN 25.
           
           DISPLAY 'Student Name   : ' LINE 6 COLUMN 5.
           ACCEPT SNA LINE 6 COLUMN 25.

           MOVE 'N' TO VALID-FLAG.
           PERFORM UNTIL VALID-FLAG = 'Y'
               DISPLAY 'Course [1-BSIT, 2-BSCS]: ' LINE 7 COLUMN 5
               ACCEPT RAW-CRS LINE 7 COLUMN 30
               
               IF RAW-CRS = 1
                   MOVE "BSIT" TO D-CRS
                   MOVE 'Y' TO VALID-FLAG
               ELSE IF RAW-CRS = 2
                   MOVE "BSCS" TO D-CRS
                   MOVE 'Y' TO VALID-FLAG
               ELSE
                   DISPLAY 'INVALID! ENTER 1 OR 2   ' LINE 7 COLUMN 40
               END-IF
           END-PERFORM.
    
           DISPLAY '                        ' LINE 7 COLUMN 40.

           DISPLAY 'Year & Section : ' LINE 8 COLUMN 5.
           ACCEPT YS LINE 8 COLUMN 25.

           MOVE 'N' TO VALID-FLAG.
           PERFORM UNTIL VALID-FLAG = 'Y'
               DISPLAY 'Type [1-Reg, 2-Irreg]  : ' LINE 9 COLUMN 5
               ACCEPT RAW-ST LINE 9 COLUMN 30

               IF RAW-ST = 1
                   MOVE "REGULAR  " TO D-ST
                   MOVE 'Y' TO VALID-FLAG
               ELSE IF RAW-ST = 2
                   MOVE "IRREGULAR" TO D-ST
                   MOVE 'Y' TO VALID-FLAG
               ELSE
                   DISPLAY 'INVALID! ENTER 1 OR 2   ' LINE 9 COLUMN 40
               END-IF
           END-PERFORM.
   
           DISPLAY '                        ' LINE 9 COLUMN 40.

           DISPLAY 'Midterm Grade  : ' LINE 10 COLUMN 5.
           ACCEPT MG LINE 10 COLUMN 25.
           
           DISPLAY 'Final Grade    : ' LINE 11 COLUMN 5.
           ACCEPT FG LINE 11 COLUMN 25.

           COMPUTE AVG = (MG + FG) / 2.
           MOVE AVG TO DISP-AVG.

           IF AVG < 3.01 OR AVG = 3.00
               MOVE "PASSED" TO REM
           ELSE
               MOVE "FAILED" TO REM.

           DISPLAY '----------------------------' LINE 13 COLUMN 25.
           DISPLAY 'SUMMARY REPORT'             LINE 14 COLUMN 28.
           DISPLAY 'Course: ' LINE 15 COLUMN 5.
           DISPLAY D-CRS LINE 15 COLUMN 15.
           DISPLAY 'Type  : ' LINE 16 COLUMN 5.
           DISPLAY D-ST LINE 16 COLUMN 15.
           DISPLAY 'Ave   : ' LINE 17 COLUMN 5.
           DISPLAY DISP-AVG LINE 17 COLUMN 15.
           DISPLAY 'Remark: ' LINE 18 COLUMN 5.
           DISPLAY REM LINE 18 COLUMN 15.
           DISPLAY '----------------------------' LINE 19 COLUMN 25.
           DISPLAY 'RECORD SAVED TO DISK.' LINE 20 COLUMN 25.

           MOVE SNO TO R-SNO.
           MOVE SNA TO R-SNA.
           MOVE D-CRS TO R-CRS-DESC.
           MOVE YS TO R-YS.
           MOVE D-ST TO R-ST-DESC.
           MOVE MG TO R-MG.
           MOVE FG TO R-FG.
           MOVE AVG TO R-AVG.
           MOVE REM TO R-REM.
           WRITE STUDENT-REC.

           DISPLAY "Enter Another Record? (Y/N): " LINE 22 COLUMN 5.
           ACCEPT VAL LINE 22 COLUMN 35.
       PROCESS-END. 