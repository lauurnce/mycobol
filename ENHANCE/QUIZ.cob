       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADES.
       AUTHOR. Lawrence Panes.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO 'INPUT.TXT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT REPORT-FILE  ASSIGN TO 'OUTPUT.TXT'.

       DATA DIVISION.
       FILE SECTION.
       
       FD  STUDENT-FILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS INREC.
       01  INREC.
           02 SNO      PIC 9(5).
           02 SNA      PIC X(25).
           02 QUIZ     PIC 99 OCCURS 5 TIMES.

       FD  REPORT-FILE
           LABEL RECORD IS STANDARD.
       01  PRINT-LINE  PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 EOF-SW        PIC X VALUE 'N'.
           
       01  WS-CALCS.
           05 SUB           PIC 9.
           05 WS-TOTAL      PIC 9(3).
           05 WS-AVG        PIC 99V99.

       01  HEADER-1.
           05 FILLER        PIC X(40) VALUE SPACES.
           05 FILLER        PIC X(10) VALUE "PUP".
           05 FILLER        PIC X(40) VALUE SPACES.

       01  HEADER-2.
           05 FILLER        PIC X(40) VALUE SPACES.
           05 FILLER        PIC X(10) VALUE "CCIS".
           05 FILLER        PIC X(40) VALUE SPACES.

       01  HEADER-3.
           05 FILLER        PIC X(15) VALUE "Student".
           05 FILLER        PIC X(26) VALUE "Student".
           05 FILLER        PIC X(8)  VALUE "Quiz".
           05 FILLER        PIC X(8)  VALUE "Quiz".
           05 FILLER        PIC X(8)  VALUE "Quiz".
           05 FILLER        PIC X(8)  VALUE "Quiz".
           05 FILLER        PIC X(8)  VALUE "Quiz".
           05 FILLER        PIC X(10) VALUE "Average".

       01  HEADER-4.
           05 FILLER        PIC X(15) VALUE "Number".
           05 FILLER        PIC X(26) VALUE "Name".
           05 FILLER        PIC X(8)  VALUE "1".
           05 FILLER        PIC X(8)  VALUE "2".
           05 FILLER        PIC X(8)  VALUE "3".
           05 FILLER        PIC X(8)  VALUE "4".
           05 FILLER        PIC X(8)  VALUE "5".
           05 FILLER        PIC X(10) VALUE SPACES.

       01  DETALYE.
           05 P-SNO         PIC 9(5).
           05 FILLER        PIC X(10) VALUE SPACES.
           05 P-SNA         PIC X(25).
           05 FILLER        PIC X(2)  VALUE SPACES.
           05 PRT-Q-GROUP OCCURS 5 TIMES.
              10 PRT-QUIZ   PIC 99.
              10 FILLER     PIC X(6) VALUE SPACES.
           05 P-AVE         PIC 99.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT STUDENT-FILE
                OUTPUT REPORT-FILE.

           PERFORM PRINT-HEADERS.
           
           READ STUDENT-FILE
                AT END MOVE 'Y' TO EOF-SW.

           PERFORM PROCESS-RECORDS UNTIL EOF-SW = 'Y'.

           CLOSE STUDENT-FILE
                 REPORT-FILE.
           STOP RUN.

       PRINT-HEADERS.
           WRITE PRINT-LINE FROM HEADER-1 AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADER-2 AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADER-3 AFTER ADVANCING 2 LINES.
           WRITE PRINT-LINE FROM HEADER-4 AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       PROCESS-RECORDS.
           MOVE SNO TO P-SNO.
           MOVE SNA TO P-SNA.
           MOVE 0 TO WS-TOTAL.
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE QUIZ(SUB) TO PRT-QUIZ(SUB)
              ADD QUIZ(SUB) TO WS-TOTAL
           END-PERFORM.

           COMPUTE WS-AVG = WS-TOTAL / 5.
           MOVE WS-AVG TO P-AVE.

           WRITE PRINT-LINE FROM DETALYE AFTER ADVANCING 1 LINE.

           READ STUDENT-FILE
                AT END MOVE 'Y' TO EOF-SW.