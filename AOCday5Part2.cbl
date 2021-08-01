       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday5Part2.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day5.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  SEATS-I.
           05 ROW-I          PIC X(7).
           05 SEAT-I         PIC X(3).

       WORKING-STORAGE SECTION.
       01  WORK-VARS.
           05  ROW              PIC X(7).
           05  SEAT             PIC X(3).
           05  MAX              PIC 9(4) VALUE 0.
           05  SUMMA            PIC 9(4).
           05  TEMP-ROW         PIC 9(4).
           05  TEMP-SEAT        PIC 9(3).
           05  PRINTROW         PIC X(8).
           05  CCC              PIC 9.
           05  ROWS             PIC 9(3) VALUE 1.
       
       01 SEAT-TABLE.
           05 SEAT-A OCCURS 1024 TIMES INDEXED BY I.
              10 SPOT PIC X VALUE 'O'.

       77  TEMP-CTR          PIC 9(4).
       77  Indeksi           PIC  9(4) VALUE 1.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 300-FIND-YOUR-OWN-SEAT.
           PERFORM 900-WRAP-UP.
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE SEAT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING INDEKSI FROM 1 BY 1
              UNTIL END-OF-FILE
                 MOVE ROW-I TO ROW
                 MOVE SEAT-I TO SEAT                
                 PERFORM 100-FIND-SEAT-ID
                 PERFORM 200-MARK-SEAT
                 READ INPUT-FILE
                   AT END MOVE 'Y' TO  SW-END-OF-FILE
                 END-READ
           END-PERFORM.
           DISPLAY " ".
       100-FIND-SEAT-ID.
           MOVE 0 TO SUMMA.
           MOVE 0 TO TEMP-ROW.
           MOVE 0 TO TEMP-SEAT.
           PERFORM VARYING INDEKSI FROM 1 BY 1 UNTIL INDEKSI > 7
              IF ROW(INDEKSI:1) = 'B' 
                 COMPUTE TEMP-ROW = TEMP-ROW + 2 ** (7 - INDEKSI)
              END-IF
           END-PERFORM.
           PERFORM VARYING INDEKSI FROM 1 BY 1 UNTIL INDEKSI > 3
              IF SEAT(INDEKSI:1) = 'R'
                 COMPUTE TEMP-SEAT = TEMP-SEAT + 2 ** (3 - INDEKSI)
              END-IF
           END-PERFORM.
           COMPUTE SUMMA = TEMP-ROW * 8 + TEMP-SEAT.

       200-MARK-SEAT.
           MOVE 'X' TO SEAT-A(SUMMA).

      * if you use the 400-PRINT-SEATS, you will see that the plane has
      * empty seats up till seat 95 and from 912 onwards, so we will
      * skip those
       300-FIND-YOUR-OWN-SEAT.
           PERFORM VARYING INDEKSI FROM 96 BY 1 UNTIL INDEKSI > 910
              IF SEAT-A(INDEKSI) <> 'X'
                 DISPLAY "Your seat: " INDEKSI
              END-IF
           END-PERFORM.

       400-PRINT-SEATS.
           PERFORM VARYING INDEKSI FROM 96 BY 1 UNTIL INDEKSI > 910           
              MOVE FUNCTION MOD(INDEKSI, 8) TO CCC
              IF CCC = 0
                 MOVE SEAT-A(INDEKSI) TO PRINTROW(8:1)
                 DISPLAY PRINTROW " " ROWS
                 ADD 1 TO ROWS
              ELSE
                 MOVE SEAT-A(INDEKSI) TO PRINTROW(CCC:1)
              END-IF
           END-PERFORM.


       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "-----------------".
