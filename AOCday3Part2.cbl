       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday3Part2.
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
        05 RIGHT1DOWN1    PIC 9(4).
        05 RIGHT3DOWN1    PIC 9(4).
        05 RIGHT5DOWN1    PIC 9(4).
        05 RIGHT7DOWN1    PIC 9(4).
        05 RIGHT1DOWN2    PIC 9(4).
        05 MULTIPL        PIC 9(16).

       77 INDEKSI           PIC 9(2) VALUE 1.
       77 INDEKSI-T         PIC 9(4) VALUE 1.
       77 PROJECT-INDEX     PIC 9(4) VALUE 1.
       77 COUNTER           PIC 9(4) VALUE 1.
       77 SW-END-OF-FILE    PIC X(01) VALUE SPACES.
           88 END-OF-FILE   VALUE 'Y'.

       01 ROW-TABLE.
           05 SOLU OCCURS 1 TO 2000 DEPENDING ON INDEKSI-T.
                10 ROW-A OCCURS 31 TIMES INDEXED BY I.
                 11 SPOT PIC X.

       01 COPYROW-TABLE.
        10 COPYROW OCCURS 31 TIMES INDEXED BY II.
         11 COPYSPOT PIC X.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           MOVE 1 TO RIGHT-VAR.
           PERFORM 100-CHECK-TREES.
           MOVE TREES TO RIGHT1DOWN1.
           MOVE 3 TO RIGHT-VAR.
           PERFORM 100-CHECK-TREES.
           MOVE TREES TO RIGHT3DOWN1.
           MOVE 5 TO RIGHT-VAR.
           PERFORM 100-CHECK-TREES.
           MOVE TREES TO RIGHT5DOWN1.
           MOVE 7 TO RIGHT-VAR.
           PERFORM 100-CHECK-TREES.
           MOVE TREES TO RIGHT7DOWN1.
           MOVE 1 TO RIGHT-VAR.
           MOVE 2 TO DOWN-VAR.
           PERFORM 200-CHECK-RIGHT1DOWN2.
           MOVE TREES TO RIGHT1DOWN2.
           PERFORM 900-WRAP-UP.
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE ROW-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
            AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1 
              UNTIL END-OF-FILE
                MOVE ROW TO SOLU (PROJECT-INDEX)
                ADD 1 TO INDEKSI-T
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
           END-PERFORM.
           DISPLAY " ".
           CLOSE INPUT-FILE.          

       100-CHECK-TREES.
           MOVE 0 TO TREES.
           MOVE 1 TO INDEKSI.
           PERFORM VARYING PROJECT-INDEX FROM 2 BY DOWN-VAR  
            UNTIL PROJECT-INDEX > INDEKSI-T
              MOVE SOLU(PROJECT-INDEX) TO COPYROW-TABLE 
              ADD RIGHT-VAR TO INDEKSI
              IF INDEKSI > 31
               SUBTRACT 31 FROM INDEKSI GIVING INDEKSI
              END-IF
              IF COPYROW (INDEKSI) = '#'
                ADD 1 TO TREES
                MOVE 'X' TO COPYROW (INDEKSI)
      *          DISPLAY COPYROW-TABLE 
      *       DISPLAY "PUU RIVILLÄ " COUNTER " SARAKKEESSA " INDEKSI
              ELSE 
                MOVE 'O' TO COPYROW (INDEKSI)
      *          DISPLAY COPYROW-TABLE 
              END-IF
           END-PERFORM.
           DISPLAY " ".

       200-CHECK-RIGHT1DOWN2.
            MOVE 0 TO TREES.
            MOVE 1 TO INDEKSI.
            PERFORM VARYING PROJECT-INDEX FROM 3 BY 2
             UNTIL PROJECT-INDEX > INDEKSI-T
            MOVE SOLU(PROJECT-INDEX) TO COPYROW-TABLE 
            ADD 1 TO INDEKSI
            IF INDEKSI > 31
             SUBTRACT 31 FROM INDEKSI GIVING INDEKSI
            END-IF
            IF COPYROW (INDEKSI) = '#'
              ADD 1 TO TREES
              MOVE 'X' TO COPYROW (INDEKSI)
      *        DISPLAY SOLU (PROJECT-INDEX - 1)
      *        DISPLAY COPYROW-TABLE 
      *DISPLAY "PUU RIVILLÄ " COUNTER " SARAKKEESSA " INDEKSI
            ELSE 
              MOVE 'O' TO COPYROW (INDEKSI)
      *        DISPLAY SOLU (PROJECT-INDEX - 1)
      *        DISPLAY COPYROW-TABLE 
            END-IF
            END-PERFORM.
            DISPLAY " ".
      *      DISPLAY "trees: " TREES.
      *      DISPLAY " ".

       900-WRAP-UP.
           COMPUTE MULTIPL = RIGHT1DOWN2 * RIGHT1DOWN1 * RIGHT3DOWN1
            * RIGHT5DOWN1 * RIGHT7DOWN1.
           DISPLAY "Encountered trees: ".
           DISPLAY "-------------------".
           DISPLAY "RIGHT 1 DOWN 2: " RIGHT1DOWN2.
           DISPLAY "RIGHT 1 DOWN 1: " RIGHT1DOWN1.
           DISPLAY "RIGHT 3 DOWN 1: " RIGHT3DOWN1.
           DISPLAY "RIGHT 5 DOWN 1: " RIGHT5DOWN1.
           DISPLAY "RIGHT 7 DOWN 1: " RIGHT7DOWN1.
           DISPLAY "-------------------".
           DISPLAY "Their product: " MULTIPL.
           DISPLAY "-------------------".