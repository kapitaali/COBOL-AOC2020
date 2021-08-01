       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday5.
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
       
       77  TEMP-CTR             PIC 9(4).
       77  Indeksi           PIC  9(4) VALUE 1.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-FIND-SEAT-ID.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING INDEKSI FROM 1 BY 1
              UNTIL END-OF-FILE
                 MOVE ROW-I TO ROW
                 MOVE SEAT-I TO SEAT                
                 PERFORM 100-FIND-SEAT-ID
                 PERFORM 200-IS-IT-MAX
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

       200-IS-IT-MAX.
           IF SUMMA IS GREATER THAN MAX
              MOVE SUMMA TO MAX
           END-IF.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "Highest seat ID: " MAX.
           DISPLAY "-----------------".
