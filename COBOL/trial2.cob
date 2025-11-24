       IDENTIFICATION DIVISION.  
       PROGRAM-ID.        SALE.  
      *AUTHOR.           ALET.  
      *INSTALLATION.     S-305.  
      *DATE-WRITTEN.     NOVEMBER 19,2008.  
      *DATE-COMPILED.    NOVEMBER 19,2008.  
      *SECURITY.         EXCLUSIVE FOR US.  
      *REMARKS           FIRST PROGRAM.  
       ENVIRONMENT DIVISION.  
       CONFIGURATION SECTION.  
       SOURCE-COMPUTER.    IBM-PC.  
       OBJECT-COMPUTER.    IBM-PC.  
       INPUT-OUTPUT SECTION.  
       FILE-CONTROL.  
           SELECT INFILE ASSIGN TO ‘SALES.txt’.  
           SELECT OUTFILE ASSIGN TO ‘BENTA’.      
       DATA DIVISION.  
       FILE SECTION.  
       FD INFILE  
           LABEL RECORD IS STANDARD  
           RECORD CONTAINS 38 CHARACTERS 
           DATA RECORD IS INREC.  
       01  INREC.  
           02        AC    PIC  X.  
           02        SNO   PIC  9(5).  
           02        SNA.   
                05 FN PIC X(10). 
                05 MN PIC X(5). 
                05 LN PIC X(10). 
 
 
           02        AMT  PIC  9(5)V99.  
       FD  OUTFILE  
           LABEL RECORD IS OMITTED  
           DATA RECORD IS OUTREC.  
       01 OUTREC.  
           02   FILLER PIC X(80).  
       WORKING-STORAGE SECTION.  
       01  SVAC           PIC   X    VALUE SPACES.  
       01  TNS            PIC   9(4)  VALUE ZERO.  
       01  TA             PIC   9(7)V99   VALUE ZERO.  
       01  EOFSW          PIC   9  VALUE ZERO.  
       01  HEAD-1.  
           02  FILLER  PIC X (29)    VALUE  SPACES.  
           02  FILLER  PIC X(22)     VALUE ‘SAN MIGUEL 
           CORPORATION’. 
           02  FILLER  PIC X (29)    VALUE SPACES.  
       01  HEAD-2.  
           02  FILLER  PIC X (31)    VALUE SPACES.  
           02  FILLER  PIC X (17)    VALUE  ‘STA.MESA, 
           MANILA’. 
           02   FILLER PIC X(32)     VALUE SPACES.  
       01  SUB-1.  
           02 FILLER PIC X(34)        VALUE SPACES.  
           02 FILLER PIC X (12)       VALUE ‘SALES REPORT’. 
           02 FILLER PIC X(34)        VALUE SPACES.  
       01  SUB-2.  
           02 FILLER PIC X(7)         VALUE SPACES.  
           02 FILLER PIC X(4)         VALUE ‘AREA’.  
           02 FILLER PIC X(7)         VALUE SPACES.  
           02 FILLER PIC X(8)         VALUE ‘SALESMAN’.  
           02 FILLER PIC X(16)        VALUE SPACES.  
           02 FILLER PIC X(8)         VALUE ‘SALESMAN’.  
           02 FILLER PIC X(15)        VALUE SPACES.  
           02 FILLER PIC X(6)         VALUE ‘AMOUNT’.  
           02 FILLER PIC X(9)         VALUE SPACES.  
       01  SUB-3.  
           02 FILLER PIC X(7)         VALUE SPACES.  
           02 FILLER PIC X(4)         VALUE ‘CODE’.  
           02 FILLER PIC X(8)         VALUE SPACES.  
           02 FILLER PIC X(6)         VALUE ‘NUMBER’.  
           02 FILLER PIC X(18)        VALUE SPACES.  
           02 FILLER PIC X(4)         VALUE ‘NAME’.  
           02 FILLER PIC X(33)        VALUE SPACES.  
       01  DETALYE.  
           02 FILER PIC X(9)          VALUE SPACES.  
           02 P-AC PIC X.  
           02 FILLER PIC X (10)       VALUE SPACES.  
           02 P-SNO PIC 9(5).  
           02 FILLER PIC X (8)        VALUE SPACES.  
           02 P-SNA PIC X (25).  
           02 FILLER PIC X(7)         VALUES SPACES.  
           02 P-AMT PIC 99,999.99.  
           02 FILLER PIC X(6)         VALUE SPACES.  
       01  TOTAL-1.  
           02 FILLER PIC X (7)        VALUE SPACES.  
           02 FILLER PIC X (9)        VALUE ‘TOTAL NO.’.  
           02 FILLER PIC X (12)       VALUE ‘OF SALESMEN:’.  
           02 FILLER PIC X (9)        VALUE SPACES.  
           02 P-TNS PIC 9,999.  
           02 FILLER PIC X (38)       VALUE SPACES.  
       01  TOTAL-2.  
           02 FILLER PIC X (7)        VALUE SPACES.  
           02 FILLER PIC X (6)        VALUE ‘TOTAL ’.  
           02 FILLER PIC X (11)       VALUE ‘ACCUMULATED’. 
           02 FILLER PIC X (8)        VALUE ‘ AMOUNT:’.  
           02 FILLER PIC X(4)         VALUE ‘ PHP’.  
           02 P-TA PIC 9,999,999.99.  
           02 FILLER PIC X (33)        VALUE SPACES.  
       SCREEN SECTION.  
       01 SCRE.  
           02 BLANK SCREEN.  
       PROCEDURE DIVISION.  
       MAIN-RTN.  
           PERFORM INIT-RTN THRU INIT-RTN-END.  
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.  
           PERFORM FINISH-RTN.  
           STOP RUN.  
       INIT-RTN.  
           OPEN INPUT INFILE,  OUTPUT OUTFILE.  
           READ INFILE AT END PERFORM END-RTN  
           GO TO INIT-RTN-END.  
           MOVE AC TO SVAC.  
           PERFORM HEADING-RTN.  
       INIT-RTN-END.
       END-RTN.  
           MOVE 1 TO EOFSW.  
           DISPLAY ‘EMPTY FILE’ LINE 3 COLUMN 20.  
       HEADING-RTN.  
           WRITE OUTREC FROM HEAD-1 AFTER PAGE.  
           WRITE OUTREC FROM HEAD-2 AFTER 1. 
           WRITE OUTREC FROM SUB-1 AFTER 3.  
           WRITE OUTREC FROM SUB-2 AFTER 2.  
           WRITE OUTREC FROM SUB-3 AFTER 1.  
       PROCESS-RTN.  
           DISPLAY SCRE.  
           IF SVAC NOT = AC PERFORM AC-BREAK-RTN.  
           MOVE AC TO P-AC.  
           MOVE SNO TO P-SNO.  
           MOVE SNA TO P-SNA.  
           MOVE AMT TO P-AMT.  
           WRITE OUTREC FROM DETALYE AFTER 1.  
           ADD 1 TO TNS giving tns.  
           ADD AMT TO TA giving ta.  
           READ INFILE AT END MOVE 1 TO EOFSW PERFORM  
           AC-BREAK-RTN. 
       AC-BREAK-RTN.  
           MOVE TNS TO P-TNS.  
           WRITE OUTREC FROM TOTAL-1 AFTER 3.  
           MOVE TA TO P-TA.  
           WRITE OUTREC FROM TOTAL-2 AFTER 1.  
           MOVE 0 TO TNS, TA.  
           MOVE AC TO SVAC.  
       FINISH-RTN.  
           CLOSE INFILE, OUTFILE.  
           DISPLAY ‘TAPOS NA’ LINE 6 COLUMN 20. 