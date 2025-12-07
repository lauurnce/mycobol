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
           02 SNO          PIC 9(5).
           02 SNA          PIC X(25).
      * Changed per Requirement: Input is 1 Quiz + 1 Code per line
           02 IN-QUIZ      PIC 999.
           02 IN-QCODE     PIC 9.

       FD  REPORT-FILE
           LABEL RECORD IS STANDARD.
       01  PRINT-LINE  PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 EOF-SW         PIC X VALUE 'N'.
           05 FIRST-REC-SW   PIC X VALUE 'Y'.
           
       01  WS-CALCS.
           05 SUB            PIC 9.
           05 WS-TOTAL       PIC 9(3).
           05 WS-AVG         PIC 99V99.
      * New Requirement: Counter for total students
           05 WS-STUDENT-CTR PIC 9(3) VALUE 0.

      * Buffer to hold data for the current student being processed
       01  WS-HOLD-STUDENT.
           05 WS-HOLD-SNO    PIC 9(5).
           05 WS-HOLD-SNA    PIC X(25).
           05 WS-HOLD-TBL.
              10 WS-HOLD-QUIZ PIC 9(3) OCCURS 5 TIMES.

       01  HEADER-1.
           05 FILLER         PIC X(40) VALUE SPACES.
           05 FILLER         PIC X(10) VALUE "PUP".
           05 FILLER         PIC X(40) VALUE SPACES.

       01  HEADER-2.
           05 FILLER         PIC X(30) VALUE SPACES.
           05 FILLER         PIC X(45) VALUE
              "College of Computer and Information Sciences".
           05 FILLER         PIC X(15) VALUE SPACES.

       01  HEADER-3.
           05 FILLER         PIC X(15) VALUE "Student".
           05 FILLER         PIC X(26) VALUE "Student".
           05 FILLER         PIC X(8)  VALUE "Quiz".
           05 FILLER         PIC X(8)  VALUE "Quiz".
           05 FILLER         PIC X(8)  VALUE "Quiz".
           05 FILLER         PIC X(8)  VALUE "Quiz".
           05 FILLER         PIC X(8)  VALUE "Quiz".
           05 FILLER         PIC X(10) VALUE "Average".

       01  HEADER-4.
           05 FILLER         PIC X(15) VALUE "Number".
           05 FILLER         PIC X(26) VALUE "Name".
           05 FILLER         PIC X(8)  VALUE "1".
           05 FILLER         PIC X(8)  VALUE "2".
           05 FILLER         PIC X(8)  VALUE "3".
           05 FILLER         PIC X(8)  VALUE "4".
           05 FILLER         PIC X(8)  VALUE "5".
           05 FILLER         PIC X(10) VALUE SPACES.

       01  DETALYE.
           05 P-SNO          PIC 9(5).
           05 FILLER         PIC X(10) VALUE SPACES.
           05 P-SNA          PIC X(25).
           05 FILLER         PIC X(2)  VALUE SPACES.
           05 PRT-Q-GROUP OCCURS 5 TIMES.
              10 PRT-QUIZ    PIC 999.
              10 FILLER      PIC X(5) VALUE SPACES.
           05 P-AVE          PIC 999.99.

      * New Requirement: Footer line for Total Records
       01  TRAILER-1.
           05 FILLER         PIC X(21) VALUE "Total no. of Records.".
           05 FILLER         PIC X(1)  VALUE SPACES.
           05 P-TOTAL-CTR    PIC ZZ9.
           05 FILLER         PIC X(75) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT STUDENT-FILE
                OUTPUT REPORT-FILE.

           PERFORM PRINT-HEADERS.
           
           READ STUDENT-FILE
                AT END MOVE 'Y' TO EOF-SW.

           IF EOF-SW = 'N'
      * Initialize the first student holder
               MOVE SNO TO WS-HOLD-SNO
               MOVE SNA TO WS-HOLD-SNA
               INITIALIZE WS-HOLD-TBL
           END-IF.

           PERFORM PROCESS-RECORDS UNTIL EOF-SW = 'Y'.

      * Print the last student remaining in the buffer
           IF FIRST-REC-SW = 'N'
               PERFORM PRINT-ROUTINE
           END-IF.

      * Print the Total Count at the end
           MOVE WS-STUDENT-CTR TO P-TOTAL-CTR.
           WRITE PRINT-LINE FROM SPACES AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM TRAILER-1 AFTER ADVANCING 1 LINE.

           CLOSE STUDENT-FILE
                 REPORT-FILE.
           STOP RUN.

       PRINT-HEADERS.
           WRITE PRINT-LINE FROM HEADER-1 AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADER-2 AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM SPACES AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADER-3 AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM HEADER-4 AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       PROCESS-RECORDS.
      * Check if Student Number changed (Control Break)
           IF SNO NOT = WS-HOLD-SNO
               PERFORM PRINT-ROUTINE
               
      * Reset for the new student
               MOVE SNO TO WS-HOLD-SNO
               MOVE SNA TO WS-HOLD-SNA
               INITIALIZE WS-HOLD-TBL
           END-IF.

           MOVE 'N' TO FIRST-REC-SW.

      * Store the quiz based on the Quiz Code (1-5)
           IF IN-QCODE >= 1 AND IN-QCODE <= 5
               MOVE IN-QUIZ TO WS-HOLD-QUIZ(IN-QCODE)
           END-IF.

           READ STUDENT-FILE
                AT END MOVE 'Y' TO EOF-SW.

       PRINT-ROUTINE.
           MOVE WS-HOLD-SNO TO P-SNO.
           MOVE WS-HOLD-SNA TO P-SNA.
           MOVE 0 TO WS-TOTAL.
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
      * Move quiz from holder table to output line
              MOVE WS-HOLD-QUIZ(SUB) TO PRT-QUIZ(SUB)
              ADD WS-HOLD-QUIZ(SUB) TO WS-TOTAL
           END-PERFORM.

           COMPUTE WS-AVG = WS-TOTAL / 5.
           MOVE WS-AVG TO P-AVE.

           WRITE PRINT-LINE FROM DETALYE AFTER ADVANCING 1 LINE.

      * Increment the total student counter
           ADD 1 TO WS-STUDENT-CTR.