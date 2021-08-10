       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday6.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day6.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  ANSWERS-I.
           05 ROW-I          PIC X(26).

       WORKING-STORAGE SECTION.
       01  WORK-VARS.
           05  ROW              PIC X(26).
           05  AAKKOSET  PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05  SUMMA            PIC 9(4) VALUE 0.
           05  ONES             PIC 9(26) VALUE ZEROES.
           05  PEOPLE           PIC 9(2) VALUE 1.
           05  GROUPS           PIC 9(2) VALUE 0.

       77  ITER                 PIC 9(2) VALUE 0.
       77  TEMP-CTR             PIC 9(4) VALUE 0.
       77  ITER2                PIC 9(2) VALUE 0.
       77  Indeksi              PIC  9(4) VALUE 1.
       77  SW-END-OF-FILE       PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING INDEKSI FROM 1 BY 1
              UNTIL END-OF-FILE
              PERFORM 200-COUNT-ANSWERS
           END-PERFORM.
           DISPLAY " ".
       
       200-COUNT-ANSWERS.
           READ INPUT-FILE 
            AT END MOVE 'Y' TO  SW-END-OF-FILE
           END-READ.
           MOVE ROW-I TO ROW.
           IF ROW <> '' OR ROW <> ' '
             ADD 1 TO PEOPLE
             PERFORM VARYING ITER FROM 1 BY 1 UNTIL ITER > 26
              PERFORM VARYING ITER2 FROM 1 BY 1 UNTIL ITER2 > 26
                 IF ROW(ITER2:1) = AAKKOSET(ITER:1)
                    MOVE 1 TO ONES(ITER:1)
                 END-IF
              END-PERFORM
             END-PERFORM
           ELSE
              ADD 1 TO GROUPS
              PERFORM VARYING ITER FROM 1 BY 1 UNTIL ITER > 26
                 IF ONES(ITER:1) = 1
                    ADD 1 TO TEMP-CTR
                    ADD 1 TO SUMMA
                 END-IF
              END-PERFORM
              DISPLAY "A group of " PEOPLE " people, "
              DISPLAY "       answering yes to " TEMP-CTR " questions"
              MOVE ZEROES TO ONES
              MOVE 0 TO TEMP-CTR
              MOVE 0 TO PEOPLE
           END-IF.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "Total number of groups: " GROUPS.
           DISPLAY "Total number of questions: " SUMMA. 
           DISPLAY "-----------------".
