       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRC.
      *AUTHOR. LAWRENCE PANES.
      *DATE-WRITTEN. JANUARY 12, 2026.
      *SECURITY. EXCLUSIVE FOR BSIT 2-4.
      

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
             SOURCE-COMPUTER. IBM-PC.
             OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO 'ECNERW'.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.
       01  OUTREC PIC X(120).

       WORKING-STORAGE SECTION.
       01  ENO PIC 9(10) VALUE ZERO.
       01  ENA PIC X(25) VALUE ZERO.
       01  DOB PIC X(20) VALUE ZERO.
       01  UC PIC 9 VALUE ZERO.
       01  UN PIC X(5) VALUE ZERO.
       01  CC PIC 9 VALUE ZERO.
       01  CN PIC X(4) VALUE ZERO.
       01  TNI PIC 9(3) VALUE ZERO.
       01  TR PIC 99 VALUE ZERO.
       01  AVE PIC 99V99 VALUE ZERO.
       01  REM PIC X(6) VALUE SPACES.
       01  TNP PIC 99 VALUE ZERO.
       01  TNF PIC 99 VALUE ZERO.
       01  VALID-SW PIC X VALUE ZERO.
       01  ANS PIC X VALUE ZERO.
       01  COUNTER-1 PIC 9 VALUE ZERO.
       01  COUNTER-2 PIC 9 VALUE ZERO.

       01  HEAD-1.
           02 FILLER PIC X(15) VALUE "Examinee".
           02 FILLER PIC X(30) VALUE "Examinee".
           02 FILLER PIC X(25) VALUE "Date of".
           02 FILLER PIC X(15) VALUE "University".
           02 FILLER PIC X(10) VALUE "Course".
           02 FILLER PIC X(10) VALUE "Remarks".

       01  HEAD-2.
           02 FILLER PIC X(15) VALUE "No.".
           02 FILLER PIC X(30) VALUE "Name".
           02 FILLER PIC X(25) VALUE "Birth".
           02 FILLER PIC X(15) VALUE "Name".
           02 FILLER PIC X(10) VALUE "Name".
           02 FILLER PIC X(10) VALUE " ".

       01  HEAD-LINE.
           02 FILLER PIC X(120) VALUE ALL "-".

       01  DET-LINE.
           02 D-ENO PIC 9(10).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 D-ENA PIC X(25).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 D-DOB PIC X(20).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 D-UN PIC X(5).
           02 FILLER PIC X(10) VALUE SPACES.
           02 D-CN PIC X(4).
           02 FILLER PIC X(6)  VALUE SPACES.
           02 D-REM PIC X(6).

       SCREEN SECTION.
       01  SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN OUTPUT OUTFILE.
           WRITE OUTREC FROM HEAD-LINE.
           WRITE OUTREC FROM HEAD-1.
           WRITE OUTREC FROM HEAD-2.
           WRITE OUTREC FROM HEAD-LINE.
           PERFORM PROCESS-RTN THRU PROCESS-END
                   UNTIL ANS = 'N' OR ANS = 'n'.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           MOVE SPACES TO REM.
           DISPLAY SCRE.
           DISPLAY (1, 23) 'Professional Regulation Commission'.
           DISPLAY (4, 23) 'IT Professional Board Exam Result'.

           DISPLAY (6, 1) 'Examinee Number: '.
           ACCEPT (6, 45) ENO.

           DISPLAY (7, 1) 'Examinee Name: '.
           ACCEPT (7, 45) ENA.

           DISPLAY (8, 1) 'Date of Birth: '.
           ACCEPT (8, 45) DOB.

           DISPLAY (9, 1) 'University Code: '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (9, 45) UC
               IF UC >= 1 AND UC <= 5
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                        "
               ELSE
                   DISPLAY (23, 1) "Invalid University Code"
               END-IF
           END-PERFORM.

           DISPLAY (10, 1) 'University Name: '.
           EVALUATE UC
               WHEN 1
                   MOVE "UP" TO UN
               WHEN 2
                   MOVE "PUP" TO UN
               WHEN 3
                   MOVE "DLSU" TO UN
               WHEN 4
                   MOVE "ADMU" TO UN
               WHEN 5
                   MOVE "MAPUA" TO UN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
           DISPLAY (10, 45) UN.

           DISPLAY (11, 1) 'Course Code: '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (11, 45) CC
               IF CC >= 1 AND CC <= 3
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                     "
               ELSE
                   DISPLAY (23, 1) "Invalid Course Code"
               END-IF
           END-PERFORM.

           DISPLAY (12, 1) 'Course Name: '.
           EVALUATE CC
               WHEN 1
                   MOVE "BSIT" TO CN
               WHEN 2
                   MOVE "BSCS" TO CN
               WHEN 3
                   MOVE "BSIS" TO CN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
           DISPLAY (12, 45) CN

           DISPLAY (13, 1) 'Total No. of Items: '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (13, 45) TNI
               IF TNI > 0
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                 "
               ELSE
                   DISPLAY (23, 1) "Invalid Input"
               END-IF
           END-PERFORM.

           DISPLAY (14, 1) 'Test Results (Score): '.
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (14, 45) TR
               IF TR > -1 AND TR <= TNI
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (23, 1) "                        "
               ELSE
                   DISPLAY (23, 1) "Invalid Score"
               END-IF
           END-PERFORM.

           DISPLAY (15, 1) 'Remarks: '.
           COMPUTE AVE = (TR * 1.0) / TNI.
           EVALUATE CC
               WHEN 1
                   IF AVE >= .60
                       MOVE "PASSED" TO REM
                   ELSE
                       MOVE "FAILED" TO REM
                   END-IF
               WHEN 2
                   IF AVE >= .70
                       MOVE "PASSED" TO REM
                   ELSE
                       MOVE "FAILED" TO REM
                   END-IF
               WHEN 3
                   IF AVE >= .50
                       MOVE "PASSED" TO REM
                   ELSE
                       MOVE "FAILED" TO REM
                   END-IF
           END-EVALUATE.
           DISPLAY (15, 45) REM.

           IF REM = "PASSED"
               ADD 1 TO COUNTER-1
           ELSE
               ADD 1 TO COUNTER-2
           END-IF.

           MOVE ENO TO D-ENO.
           MOVE ENA TO D-ENA.
           MOVE DOB TO D-DOB.
           MOVE UN TO D-UN.
           MOVE CN TO D-CN.
           MOVE REM TO D-REM.

           WRITE OUTREC FROM DET-LINE.

           DISPLAY (17, 45) "Input Another Record? (Y/N)".
           ACCEPT ANS.

           DISPLAY (19, 1) "Total No. of Passed: ".
           DISPLAY (19, 23) COUNTER-1.
           DISPLAY (20, 1) "Total No. of Failed: ".
           DISPLAY (20, 23) COUNTER-2.

       PROCESS-END.