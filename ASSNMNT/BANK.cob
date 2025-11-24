       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-REPORT.
      *AUTHOR.     LAWRENCE PANES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "TRANS.TXT"
               ORGANIZATION IS SEQUENTIAL.

           SELECT REPORT-FILE ASSIGN TO "REPORT.OUT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
       01  TRANS-REC.
           05 TR-ACCT-NO        PIC X(10).
           05 TR-ACCT-NAME      PIC X(25).
           05 TR-CODE           PIC X.
              88 IS-DEPOSIT     VALUE 'D'.
              88 IS-WITHDRAWAL  VALUE 'W'.
           05 TR-AMOUNT         PIC 9(7)V99.
           05 FILLER            PIC X(2).

       FD  REPORT-FILE.
       01  PRINT-LINE           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 WS-EOF            PIC X VALUE 'N'.
              88 END-OF-FILE    VALUE 'Y'.

       01  WS-HOLDERS.
           05 PREV-ACCT-NO      PIC X(10) VALUE SPACES.
           05 PREV-ACCT-NAME    PIC X(25) VALUE SPACES.

       01  WS-CALCS.
           05 WS-ACCT-BALANCE   PIC S9(7)V99 VALUE 0.
           05 WS-TOTAL-BALANCE  PIC S9(10)V99 VALUE 0.
           05 WS-REC-COUNT      PIC 9(4) VALUE 0.

       01  HEADING-1.
           05 FILLER            PIC X(32) VALUE SPACES.
           05 FILLER            PIC X(16) VALUE "China Trust Bank".
           05 FILLER            PIC X(32) VALUE SPACES.

       01  HEADING-2.
           05 FILLER            PIC X(33) VALUE SPACES.
           05 FILLER            PIC X(13) VALUE "Makati Avenue".
           05 FILLER            PIC X(34) VALUE SPACES.

       01  HEADING-3.
           05 FILLER            PIC X(34) VALUE SPACES.
           05 FILLER            PIC X(11) VALUE "Makati City".
           05 FILLER            PIC X(35) VALUE SPACES.

       01  HEADING-4.
           05 FILLER            PIC X(32) VALUE SPACES.
           05 FILLER            PIC X(16) VALUE "Account's Report".
           05 FILLER            PIC X(32) VALUE SPACES.

       01  HEADING-5.
           05 FILLER            PIC X(7)  VALUE SPACES.
           05 FILLER            PIC X(7)  VALUE "Account".
           05 FILLER            PIC X(21) VALUE SPACES.
           05 FILLER            PIC X(7)  VALUE "Account".
           05 FILLER            PIC X(16) VALUE SPACES.
           05 FILLER            PIC X(7)  VALUE "Balance".
           05 FILLER            PIC X(15) VALUE SPACES.

       01  HEADING-6.
           05 FILLER            PIC X(8)  VALUE SPACES.
           05 FILLER            PIC X(6)  VALUE "Number".
           05 FILLER            PIC X(23) VALUE SPACES.
           05 FILLER            PIC X(4)  VALUE "Name".
           05 FILLER            PIC X(39) VALUE SPACES.

       01  HEADING-7.
           05 FILLER            PIC X(8)  VALUE SPACES.
           05 FILLER            PIC X(5)  VALUE "X(10)".
           05 FILLER            PIC X(23) VALUE SPACES.
           05 FILLER            PIC X(5)  VALUE "X(25)".
           05 FILLER            PIC X(13) VALUE SPACES.
           05 FILLER            PIC X(14) VALUE "ZZZ,ZZZ,ZZ9.99".
           05 FILLER            PIC X(12) VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER            PIC X(5)  VALUE SPACES.
           05 DL-ACCT-NO        PIC X(10).
           05 FILLER            PIC X(10) VALUE SPACES.
           05 DL-ACCT-NAME      PIC X(25).
           05 FILLER            PIC X(5)  VALUE SPACES.
           05 DL-BALANCE        PIC Z,ZZZ,ZZ9.99.
           05 FILLER            PIC X(10) VALUE SPACES.

       01  FOOTER-COUNT.
           05 FILLER            PIC X(5)  VALUE SPACES.
           05 FILLER            PIC X(29) VALUE
              "Total No. of Records printed:".
           05 FILLER            PIC X(1)  VALUE SPACE.
           05 FT-COUNT          PIC ZZZ9.
           05 FILLER            PIC X(40) VALUE SPACES.

       01  FOOTER-TOTAL.
           05 FILLER            PIC X(5)  VALUE SPACES.
           05 FILLER            PIC X(27) VALUE
              "Total Accumulated Balance: ".
           05 FILLER            PIC X(2)  VALUE "P ".
           05 FT-TOTAL-BAL      PIC Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER            PIC X(25) VALUE SPACES.

       PROCEDURE DIVISION.
       000-MAIN-LOGIC.
           OPEN INPUT TRANS-FILE
                OUTPUT REPORT-FILE.

           PERFORM 100-PRINT-HEADINGS.

           READ TRANS-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

           IF NOT END-OF-FILE
               MOVE TR-ACCT-NO TO PREV-ACCT-NO
               MOVE TR-ACCT-NAME TO PREV-ACCT-NAME
           END-IF.

           PERFORM 200-PROCESS-DATA UNTIL END-OF-FILE.

           IF PREV-ACCT-NO NOT = SPACES
               PERFORM 300-PRINT-DETAIL
           END-IF.

           PERFORM 400-PRINT-FOOTER.

           CLOSE TRANS-FILE
                 REPORT-FILE.
           STOP RUN.

       100-PRINT-HEADINGS.
           WRITE PRINT-LINE FROM HEADING-1.
           WRITE PRINT-LINE FROM HEADING-2.
           WRITE PRINT-LINE FROM HEADING-3.
           
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.
           WRITE PRINT-LINE.

           WRITE PRINT-LINE FROM HEADING-4.

           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

           WRITE PRINT-LINE FROM HEADING-5.
           WRITE PRINT-LINE FROM HEADING-6.
           WRITE PRINT-LINE FROM HEADING-7.
           
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

       200-PROCESS-DATA.
           IF TR-ACCT-NO NOT = PREV-ACCT-NO
               PERFORM 300-PRINT-DETAIL
               MOVE 0 TO WS-ACCT-BALANCE
               MOVE TR-ACCT-NO TO PREV-ACCT-NO
               MOVE TR-ACCT-NAME TO PREV-ACCT-NAME
           END-IF.

           IF IS-DEPOSIT
               ADD TR-AMOUNT TO WS-ACCT-BALANCE
           ELSE
               IF IS-WITHDRAWAL
                   SUBTRACT TR-AMOUNT FROM WS-ACCT-BALANCE
               END-IF
           END-IF.

           READ TRANS-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

       300-PRINT-DETAIL.
           MOVE PREV-ACCT-NO TO DL-ACCT-NO.
           MOVE PREV-ACCT-NAME TO DL-ACCT-NAME.
           MOVE WS-ACCT-BALANCE TO DL-BALANCE.
           
           WRITE PRINT-LINE FROM DETAIL-LINE.

           ADD 1 TO WS-REC-COUNT.
           ADD WS-ACCT-BALANCE TO WS-TOTAL-BALANCE.

       400-PRINT-FOOTER.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.
           WRITE PRINT-LINE.

           MOVE WS-REC-COUNT TO FT-COUNT.
           WRITE PRINT-LINE FROM FOOTER-COUNT.

           MOVE WS-TOTAL-BALANCE TO FT-TOTAL-BAL.
           WRITE PRINT-LINE FROM FOOTER-TOTAL.