       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday3.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day3.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  TREE-MAP.
           05 ROW  PIC X(31).

      * Level-66 items indicate a RENAMES clause is to be expected
      * Level-77 items are atomic, cannot be subdivided, 
      * cannot have an OCCURS clause.
      * Level-88 indicates a condition-name entry

       WORKING-STORAGE SECTION.
       01 VARS-WE-NEED.
        05 TREES          PIC 9(4) VALUE 0.
        05 RIGHT-VAR      PIC 9    VALUE 3.
        05 DOWN-VAR       PIC 9    VALUE 1.

       77 INDEKSI           PIC 9(2) VALUE 1.
       77 COUNTER           PIC 9(4) VALUE 1.
       77 SW-END-OF-FILE    PIC X(01) VALUE SPACES.
           88 END-OF-FILE   VALUE 'Y'.

       01 ROW-TABLE.
        05 ROW-A OCCURS 31 TIMES INDEXED BY I.
         10 SPOT PIC X.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE ROW-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
            AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM UNTIL END-OF-FILE
                ADD 1 TO COUNTER
                ADD RIGHT-VAR TO INDEKSI
                IF INDEKSI > 31
                 SUBTRACT 31 FROM INDEKSI GIVING INDEKSI
                END-IF
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
                MOVE ROW TO ROW-TABLE
                IF ROW-A (INDEKSI) = '#'
                 ADD 1 TO TREES
                 MOVE 'X' TO ROW-A (INDEKSI)
                 DISPLAY ROW-TABLE
      *           DISPLAY "PUU RIVILLÄ " COUNTER " SARAKKEESSA " INDEKSI
                ELSE 
                 MOVE 'O' TO ROW-A (INDEKSI)
                 DISPLAY ROW-TABLE
                END-IF
           END-PERFORM.
           DISPLAY " ".
       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "Encountered treed: " TREES.
