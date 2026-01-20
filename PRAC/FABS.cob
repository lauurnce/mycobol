       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALQUIZ.
      *AUTHOR.       LAWRENCE PANES.
      *INSTALLATION. HOME.
      *DATE-WRITTEN. JANUARY 20.
      *SECURITY.     EXCLUSIVE FOR BSIT.
      *REMARKS.      FINAL PRACTICAL QUIZ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "PANES".

       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS OUTREC.
       01  OUTREC.
           02 FILE-ACCT-NUM       PIC 9(10).
           02 FILE-ACCT-NAME      PIC X(25).
           02 FILE-SN       PIC X(6).
           02 FILE-TN     PIC X(12).
           02 FILE-ACCT-TYPE-NAME PIC X(15).
           02 FILE-BAL        PIC 9(12)V99.
           02 FILE-BRANCH-NAME    PIC X(15).

       WORKING-STORAGE SECTION.
       01  VALID-SW        PIC X VALUE SPACES.
       01  ANS             PIC X VALUE SPACES.
       01  TOTAL-CUST      PIC 9(5) VALUE 0.

       01  IN-ACCT-NUM     PIC 9(10) VALUE 0.
       01  IN-ACCT-NAME    PIC X(25) VALUE SPACES.
       01  IN-SEX-CODE     PIC X VALUE SPACES.
       01  IN-TRANS-TYPE   PIC X VALUE SPACES.
       01  IN-AMOUNT       PIC 9(7)V99 VALUE 0.
       01  IN-ACCT-TYPE    PIC X VALUE SPACES.
       01  IN-INIT-BAL     PIC 9(12)V99 VALUE 0.
       01  IN-BRANCH-CODE  PIC X(3) VALUE SPACES.

       01  DER-SN       PIC X(6)  VALUE SPACES.
       01  DER-TN       PIC X(12) VALUE SPACES.
       01  DER-ACCT-TYPE-NAME  PIC X(15) VALUE SPACES.
       01  DER-BRANCH-NAME     PIC X(15) VALUE SPACES.
       01  DER-CURR-BAL        PIC 9(12)V 99 VALUE 0.
       01  DISPLAY-BAL         PIC Z(12).99.

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
           DISPLAY (2, 35) "ChinaTrust Bank".
           DISPLAY (3, 36) "Makati City".
           DISPLAY (5, 32) "Customer's Account".

           DISPLAY (7, 20) "Account Number:".
           ACCEPT  (7, 40) IN-ACCT-NUM.

           DISPLAY (8, 20) "Account Name:".
           ACCEPT  (8, 40) IN-ACCT-NAME.

           DISPLAY (9, 20) "Sex Code:".
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (9, 40) IN-SEX-CODE
               IF IN-SEX-CODE = 'F' OR IN-SEX-CODE = 'M'
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "                      "
               ELSE
                   DISPLAY (24, 1) "Invalid Sex Code (F/M)"
               END-IF
           END-PERFORM.

           EVALUATE IN-SEX-CODE
               WHEN 'F' MOVE "Female" TO DER-SN
               WHEN 'M' MOVE "Male"   TO DER-SN
           END-EVALUATE.

           DISPLAY (10, 20) "Sex Name:".
           DISPLAY (10, 40) DER-SN.

           DISPLAY (11, 20) "Transaction Type:".
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (11, 40) IN-TRANS-TYPE
               IF IN-TRANS-TYPE = 'D' OR IN-TRANS-TYPE = 'W'
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "                      "
               ELSE
                   DISPLAY (24, 1) "Invalid Type (D/W)    "
               END-IF
           END-PERFORM.

           EVALUATE IN-TRANS-TYPE
               WHEN 'D' MOVE "Deposit"    TO DER-TN
               WHEN 'W' MOVE "Withdrawal" TO DER-TN
           END-EVALUATE.

           DISPLAY (12, 20) "Transaction Name:".
           DISPLAY (12, 40) DER-TN.

           DISPLAY (13, 20) "Amount:".
           ACCEPT  (13, 40) IN-AMOUNT.

           DISPLAY (14, 20) "Account Type:".
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (14, 40) IN-ACCT-TYPE
               IF IN-ACCT-TYPE = 'S' OR IN-ACCT-TYPE = 'C' 
               OR IN-ACCT-TYPE = 'D'
                   MOVE 'Y' TO VALID-SW
                   DISPLAY (24, 1) "                      "
               ELSE
                   DISPLAY (24, 1) "Invalid Acct Type(S/C/D)"
               END-IF
           END-PERFORM.

           EVALUATE IN-ACCT-TYPE
               WHEN 'S' MOVE "Savings Deposit"  TO DER-ACCT-TYPE-NAME
               WHEN 'C' MOVE "Checking Account" TO DER-ACCT-TYPE-NAME
               WHEN 'D' MOVE "Dollar Account"   TO DER-ACCT-TYPE-NAME
           END-EVALUATE.

           DISPLAY (15, 20) "Account Type Name:".
           DISPLAY (15, 40) DER-ACCT-TYPE-NAME.

           DISPLAY (16, 20) "Initial Balance:".
           ACCEPT  (16, 40) IN-INIT-BAL.

           IF IN-TRANS-TYPE = 'D'
               COMPUTE DER-CURR-BAL = IN-INIT-BAL + IN-AMOUNT
           ELSE
               COMPUTE DER-CURR-BAL = IN-INIT-BAL - IN-AMOUNT
           END-IF.

           MOVE DER-CURR-BAL TO DISPLAY-BAL.
           DISPLAY (17, 20) "Current Balance:".
           DISPLAY (17, 40) DISPLAY-BAL.

           DISPLAY (18, 20) "Branch Code:".
           MOVE 'N' TO VALID-SW.
           PERFORM UNTIL VALID-SW = 'Y'
               ACCEPT (18, 40) IN-BRANCH-CODE
               EVALUATE IN-BRANCH-CODE
                   WHEN "PAR" 
                       MOVE "Paranaque"   TO DER-BRANCH-NAME
                       MOVE 'Y' TO VALID-SW
                   WHEN "PAS" 
                       MOVE "Pasay"       TO DER-BRANCH-NAME
                       MOVE 'Y' TO VALID-SW
                   WHEN "MAN" 
                       MOVE "Mandaluyong" TO DER-BRANCH-NAME
                       MOVE 'Y' TO VALID-SW
                   WHEN "SME" 
                       MOVE "Sta. Mesa"   TO DER-BRANCH-NAME
                       MOVE 'Y' TO VALID-SW
                   WHEN "SJA" 
                       MOVE "San Juan"    TO DER-BRANCH-NAME
                       MOVE 'Y' TO VALID-SW
                   WHEN OTHER 
                       DISPLAY (24, 1) "Invalid Branch Code.  "
                       MOVE 'N' TO VALID-SW
               END-EVALUATE
           END-PERFORM.

           DISPLAY (24, 1) "                      ".
           DISPLAY (19, 20) "Branch Name:".
           DISPLAY (19, 40) DER-BRANCH-NAME.

           MOVE IN-ACCT-NUM       TO FILE-ACCT-NUM.
           MOVE IN-ACCT-NAME      TO FILE-ACCT-NAME.
           MOVE DER-SN      TO FILE-SN.
           MOVE DER-TN      TO FILE-TN.
           MOVE DER-ACCT-TYPE-NAME TO FILE-ACCT-TYPE-NAME.
           MOVE DER-CURR-BAL      TO FILE-BAL.
           MOVE DER-BRANCH-NAME   TO FILE-BRANCH-NAME.

           WRITE OUTREC.
           ADD 1 TO TOTAL-CUST.

           DISPLAY (22, 30) "Input Another Record (Y/N)? ".
           ACCEPT ANS.

       PROCESS-END.

       SUMMARY-RTN.
           DISPLAY (23, 30) "Total no. of Customers: ".
           DISPLAY (23, 55) TOTAL-CUST.
           DISPLAY (25, 20) "Press Enter to Exit".
           ACCEPT ANS.