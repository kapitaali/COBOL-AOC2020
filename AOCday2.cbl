       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday2.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day2.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  PASSWD-TABLE-I.
           05 LOWER-I  PIC 9(2).
           05 FILLER   PIC X VALUE '-'.
           05 UPPER-I  PIC 9(2).
           05 FILLER   PIC X VALUE ' '.
           05 LETTER-I PIC X.
           05 FILLER   PIC X(3) VALUE ':  '.
           05 PASSWD-I PIC X(20).

      * Level-66 items indicate a RENAMES clause is to be expected
      * Level-77 items are atomic, cannot be subdivided, 
      * cannot have an OCCURS clause.
      * Level-88 indicates a condition-name entry

       WORKING-STORAGE SECTION.
       01 VARS-WE-NEED.
        05 OK-PWS         PIC 9(4) VALUE 0.
        05 PW-CHAR        PIC X VALUE SPACE.
        05 COUNTER        PIC 9(2) VALUE 0.

       01 PW-TABLE.
        05 PW-A OCCURS 20 TIMES INDEXED BY I.
         10 PB-B PIC X.

       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE PW-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
            AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM UNTIL END-OF-FILE
                MOVE 0 TO COUNTER
                MOVE LETTER-I TO PW-CHAR
                MOVE PASSWD-I TO PW-TABLE
                PERFORM 100-CHECK-IF-OK
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
      *          DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
           END-PERFORM.
           DISPLAY " ".
       100-CHECK-IF-OK.
             PERFORM VARYING I FROM 1 BY 1 UNTIL I >20
              IF PW-A(I) = PW-CHAR 
                 ADD 1 TO COUNTER
              END-IF
             END-PERFORM.
             IF COUNTER IS GREATER THAN OR EQUAL TO LOWER-I 
              AND UPPER-I IS GREATER THAN OR EQUAL TO COUNTER
              ADD 1 TO OK-PWS
             END-IF.
       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "OK passwords: " OK-PWS.
