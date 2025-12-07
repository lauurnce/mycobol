       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-CENSUS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "BILANG.TXT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "DAMI.TXT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORD IS STANDARD.
       01  DAGDAG.
           02 IN-YR     PIC 9.
           02 IN-CC     PIC X(5).
           02 IN-SNO    PIC X(10).
           02 IN-SNA    PIC X(25).

       FD  OUTFILE
           LABEL RECORD IS OMITTED.
       01  BAWAS       PIC X(80).

       WORKING-STORAGE SECTION.
       01  EOFSW       PIC 9 VALUE 0.
       01  SUB1        PIC 9 VALUE 0.
       01  SUB2        PIC 9 VALUE 0.
       01  COURSE-IDX  PIC 9 VALUE 0.

       01  MESA.
           02 TAON OCCURS 4 TIMES.
              03 KURSO OCCURS 2 TIMES.
                 04 COUNT-VAL PIC 9(4) VALUE 0.


       01  TS-YEAR. 
           02 TOT-YR OCCURS 4 TIMES PIC 9(4) VALUE 0.

       01  TS-COUR.
           02 TOT-CR OCCURS 2 TIMES PIC 9(4) VALUE 0.

       01  YEAR-NAMES-TABLE.
           02 FILLER PIC X(10) VALUE "Freshmen  ".
           02 FILLER PIC X(10) VALUE "Sophomore ".
           02 FILLER PIC X(10) VALUE "Junior    ".
           02 FILLER PIC X(10) VALUE "Senior    ".
       01  YEAR-LABELS REDEFINES YEAR-NAMES-TABLE.
           02 Y-NAME PIC X(10) OCCURS 4 TIMES.

       01  HEADER-1.
           02 FILLER PIC X(20) VALUE SPACES.
           02 FILLER PIC X(45) VALUE 
              "Polytechnic University of the Philippines".
           02 FILLER PIC X(15) VALUE SPACES.

       01  HEADER-2.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "Sta. Mesa, Manila".
           02 FILLER PIC X(30) VALUE SPACES.

       01  HEADER-3.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "Student Population".
           02 FILLER PIC X(30) VALUE SPACES.

       01  COL-HEADER-1.
           02 FILLER PIC X(12) VALUE "Year Level".
           02 FILLER PIC X(15) VALUE SPACES.
           02 FILLER PIC X(10) VALUE "Course".
           02 FILLER PIC X(43) VALUE SPACES.

       01  COL-HEADER-2.
           02 FILLER PIC X(20) VALUE SPACES.
           02 FILLER PIC X(10) VALUE "BSCS".
           02 FILLER PIC X(10) VALUE "BSIT".
           02 FILLER PIC X(10) VALUE "Total".
           02 FILLER PIC X(30) VALUE SPACES.

       01  DETALYE.
           02 P-YEAR-NAME PIC X(12).
           02 FILLER      PIC X(8) VALUE SPACES.
           02 P-TAB       PIC ZZZ9 OCCURS 2 TIMES.
           02 FILLER      PIC X(5) VALUE SPACES.
           02 P-TOT-YEAR  PIC ZZZ9.
           02 FILLER      PIC X(35) VALUE SPACES.

       01  TOTAL-LINE.
           02 FILLER      PIC X(12) VALUE "Total".
           02 FILLER      PIC X(8) VALUE SPACES.
           02 P-TOT-CO    PIC Z,ZZ9 OCCURS 2 TIMES.
           02 FILLER      PIC X(5) VALUE SPACES.
           02 P-GRAND-TOT PIC ZZ,ZZ9.

       01  GRAND-TOTAL    PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           PERFORM FINISH-RTN.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE
                OUTPUT OUTFILE.
           READ INFILE AT END MOVE 1 TO EOFSW.
           PERFORM HEADING-RTN.

       HEADING-RTN.
           WRITE BAWAS FROM HEADER-1 AFTER ADVANCING 1 LINE.
           WRITE BAWAS FROM HEADER-2 AFTER ADVANCING 1 LINE.
           WRITE BAWAS FROM HEADER-3 AFTER ADVANCING 2 LINES.
           WRITE BAWAS FROM COL-HEADER-1 AFTER ADVANCING 2 LINES.
           WRITE BAWAS FROM COL-HEADER-2 AFTER ADVANCING 1 LINE.
           MOVE SPACES TO BAWAS.
           WRITE BAWAS AFTER ADVANCING 1 LINE.

       PROCESS-RTN.
           IF IN-CC = "BSCS "
               MOVE 1 TO COURSE-IDX
           ELSE 
               IF IN-CC = "BSIT "
                   MOVE 2 TO COURSE-IDX
               ELSE
                   MOVE 2 TO COURSE-IDX
               END-IF
           END-IF.

           ADD 1 TO COUNT-VAL(IN-YR, COURSE-IDX).

           ADD 1 TO TOT-CR(COURSE-IDX).

           ADD 1 TO TOT-YR(IN-YR).

           READ INFILE AT END MOVE 1 TO EOFSW.

       FINISH-RTN.
           PERFORM MOVE1-RTN VARYING SUB1 FROM 1 BY 1 
                   UNTIL SUB1 > 4.
           
           PERFORM TOTAL-COUR-RTN.

           CLOSE INFILE, OUTFILE.

       MOVE1-RTN.
           MOVE Y-NAME(SUB1) TO P-YEAR-NAME.
           PERFORM MOVE2-RTN VARYING SUB2 FROM 1 BY 1 
                   UNTIL SUB2 > 2.

           MOVE TOT-YR(SUB1) TO P-TOT-YEAR.

           WRITE BAWAS FROM DETALYE AFTER ADVANCING 1 LINE.

       MOVE2-RTN.
           MOVE COUNT-VAL(SUB1, SUB2) TO P-TAB(SUB2).

       TOTAL-COUR-RTN.
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
               MOVE TOT-CR(SUB2) TO P-TOT-CO(SUB2)
               ADD TOT-CR(SUB2) TO GRAND-TOTAL
           END-PERFORM.
           
           MOVE GRAND-TOTAL TO P-GRAND-TOT.
           WRITE BAWAS FROM TOTAL-LINE AFTER ADVANCING 2 LINES.