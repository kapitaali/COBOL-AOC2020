       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday4.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day4.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  PASSPORT-INPUT.
           05 PP-BYR   PIC X(4) VALUE 'byr:'.
           05 VAL-BYR  PIC 9(4).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-CID   PIC X(4) VALUE 'cid:'.
           05 VAL-CID  PIC 9(3).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-ECL   PIC X(4) VALUE 'ecl:'.
           05 VAL-ECL  PIC X(3).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-EYR   PIC X(4) VALUE 'eyr:'.
           05 VAL-EYR  PIC 9(4).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-HCL   PIC X(4) VALUE 'hcl:'.
           05 VAL-HCL  PIC X(7).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-HGT   PIC X(4) VALUE 'hgt:'.
           05 VAL-HGT  PIC 9(5).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-IYR   PIC X(4) VALUE 'iyr:'.
           05 VAL-IYR  PIC 9(4).
           05 FILLER   PIC X    VALUE SPACE.
           05 PP-PID   PIC X(4) VALUE 'pid:'.
           05 VAL-PID  PIC X(10).

      * Level-66 items indicate a RENAMES clause is to be expected
      * Level-77 items are atomic, cannot be subdivided, 
      * cannot have an OCCURS clause.
      * Level-88 indicates a condition-name entry

       WORKING-STORAGE SECTION.
       01 VARS-WE-NEED.
           05 TOTAL          PIC 9(3) VALUE 0.
           05 MISSING        PIC 9(3) VALUE 0.
           05 OK-PASSPORTS   PIC 9(3) VALUE 0.
           05 SCORE          PIC 9    VALUE 0.
           05 BYR  PIC 9(4).
           05 CID  PIC 9(3).
           05 ECL  PIC X(3).
           05 EYR  PIC 9(4).
           05 HCL  PIC X(7).
           05 HGT  PIC X(5).
           05 IYR  PIC 9(4).
           05 PID  PIC X(10).
       77 SW-END-OF-FILE          PIC X(1) VALUE SPACE.      
           88 END-OF-FILE         VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
            AT END MOVE 'Y' TO SW-END-OF-FILE.
           MOVE VAL-CID TO CID.
           IF CID(1:1) <> ' ' 
              PERFORM 300-CID-OK
           ELSE
              PERFORM 400-CID-NOT-OK
           END-IF.
           
       300-CID-OK.    
           PERFORM UNTIL END-OF-FILE
              MOVE 0 TO SCORE
              MOVE VAL-BYR TO BYR
              IF BYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-ECL TO ECL
              IF ECL(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-EYR TO EYR
              IF EYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-HCL TO HCL
              IF HCL(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-HGT TO HGT
              IF HGT(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-IYR TO IYR
              IF IYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF           
              MOVE VAL-PID TO PID 
              IF PID(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              IF SCORE IS GREATER THAN 6 
                 ADD 1 TO OK-PASSPORTS
              ELSE
                 ADD 1 TO MISSING
              END-IF
              ADD 1 TO TOTAL
              READ INPUT-FILE
                AT END MOVE 'Y' TO  SW-END-OF-FILE
              END-READ
           END-PERFORM.

        400-CID-NOT-OK.   
           PERFORM UNTIL END-OF-FILE
              MOVE 0 TO SCORE
              MOVE VAL-BYR TO BYR
              IF BYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-ECL TO ECL
              IF ECL(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-EYR TO EYR
              IF EYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-HCL TO HCL
              IF HCL(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-HGT TO HGT
              IF HGT(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              MOVE VAL-IYR TO IYR
              IF IYR(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF           
              MOVE VAL-PID TO PID 
              IF PID(1:1) <> ' ' 
                 ADD 1 TO SCORE 
              END-IF
              IF SCORE IS EQUAL TO 7 
                 ADD 1 TO OK-PASSPORTS
              ELSE
                  ADD 1 TO MISSING
              END-IF
              ADD 1 TO TOTAL
              READ INPUT-FILE
                AT END MOVE 'Y' TO  SW-END-OF-FILE
              END-READ
           END-PERFORM.


       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "Number of OK passports: " OK-PASSPORTS.
           DISPLAY "Number of missing: " MISSING.
           DISPLAY "Total number of data: " TOTAL.
           DISPLAY "----------------------".