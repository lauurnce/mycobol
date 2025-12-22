       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-REPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "BILANG.txt".
           SELECT OUTFILE ASSIGN TO "DAMI.txt".

       DATA DIVISION.
       FILE SECTION.
       FD INFILE
           LABEL RECORD IS STANDARD.
       01 DAGDAG.
           02 YR   PIC 9.
           02 CC   PIC X(5).
           02 SNO  PIC X(10).
           02 SNA  PIC X(25).

       FD OUTFILE
           LABEL RECORD IS OMITTED.
       01 BAWAS    PIC X(80).

       WORKING-STORAGE SECTION.
       01 EOFSW    PIC 9 VALUE 0.
       01 SUB1     PIC 9 VALUE 0.
       01 SUB2     PIC 9 VALUE 0.

       01 MESA.
           02 TAON OCCURS 4 TIMES.
               03 KURS PIC 999 OCCURS 2 TIMES VALUE ZERO.

       01 TS-YEAR-TABLE.
           02 TOT-YR    PIC 9(4) OCCURS 4 TIMES VALUE ZERO.
       01 TS-COUR-TABLE.
           02 TOT-CR    PIC 9(4) OCCURS 2 TIMES VALUE ZERO.

       01 HDR-1.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(3) VALUE "PUP".
           02 FILLER PIC X(57) VALUE SPACES.
       01 HDR-2.
           02 FILLER PIC X(36) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "Sta. Mesa, Manila".
       01 HDR-3. 
           02 FILLER PIC X(36) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "Student Population".
       01 HDR-4.
           02 FILLER PIC X(30) VALUE "Year Level".
           02 FILLER PIC X(15) VALUE "BSCS".
           02 FILLER PIC X(15) VALUE "BSIT".
           02 FILLER PIC X(10) VALUE "Total".

       01 DETALYE.
           02 P-YEAR    PIC X(15).
           02 FILLER    PIC X(15) VALUE SPACES.
           02 P-BSCS    PIC ZZ9.
           02 FILLER    PIC X(12) VALUE SPACES.
           02 P-BSIT    PIC ZZ9.
           02 FILLER    PIC X(10) VALUE SPACES.
           02 P-TOT-YR  PIC Z,ZZ9.

       01 TOTAL-LINE.
           02 FILLER    PIC X(15) VALUE "Total".
           02 FILLER    PIC X(14) VALUE SPACES.
           02 P-TOT-CS  PIC Z,ZZ9.
           02 FILLER    PIC X(10) VALUE SPACES.
           02 P-TOT-IT  PIC Z,ZZ9.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           PERFORM FINISH-RTN.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE, OUTPUT OUTFILE.
           PERFORM HEADING-RTN.
           READ INFILE AT END MOVE 1 TO EOFSW.

       HEADING-RTN.
           WRITE BAWAS FROM HDR-1 AFTER ADVANCING 1 LINE.
           WRITE BAWAS FROM HDR-2 AFTER ADVANCING 1 LINE.
           MOVE SPACES TO BAWAS.
           WRITE BAWAS AFTER ADVANCING 2 LINES.
           WRITE BAWAS FROM HDR-3 AFTER ADVANCING 1 LINE.
           WRITE BAWAS AFTER ADVANCING 1 LINE.
           WRITE BAWAS FROM HDR-4 AFTER ADVANCING 1 LINE.

       PROCESS-RTN.
           IF CC = "BSCS"
           MOVE 1 TO SUB2
           ELSE
           MOVE 2 TO SUB2.
    
           ADD 1 TO KURS(YR, SUB2).
    
           ADD 1 TO TOT-YR(YR).
           ADD 1 TO TOT-CR(SUB2).
    
           READ INFILE AT END MOVE 1 TO EOFSW.

       FINISH-RTN.
           PERFORM MOVE-AND-WRITE-RTN VARYING SUB1 FROM 1 BY 1 
           UNTIL SUB1 > 4.
   
           MOVE TOT-CR(1) TO P-TOT-CS.
           MOVE TOT-CR(2) TO P-TOT-IT.
           WRITE BAWAS FROM TOTAL-LINE AFTER ADVANCING 2 LINES.
           CLOSE INFILE, OUTFILE.

       MOVE-AND-WRITE-RTN.
           EVALUATE SUB1
           WHEN 1 MOVE "Freshmen" TO P-YEAR
           WHEN 2 MOVE "Sophomore" TO P-YEAR
           WHEN 3 MOVE "Junior"    TO P-YEAR
           WHEN 4 MOVE "Senior"    TO P-YEAR
           END-EVALUATE.
    
           MOVE KURS(SUB1, 1) TO P-BSCS.
           MOVE KURS(SUB1, 2) TO P-BSIT.
           MOVE TOT-YR(SUB1)  TO P-TOT-YR.
           WRITE BAWAS FROM DETALYE AFTER ADVANCING 1 LINE.