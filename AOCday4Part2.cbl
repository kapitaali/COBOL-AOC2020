       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday4Part2.
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
           05 INDEKSI        PIC 9    VALUE 0.
           05 HCL-TEST       PIC 9    VALUE 0.
           05 PID-TEST       PIC 9(2) VALUE 0.
           05 PID-I          PIC 9(2) VALUE 0.
      * I just needed some debug output     
           77 DEBUG-ON       PIC 9    VALUE 0.

       01 COPYVARS.
           05 BYR  PIC 9(4).
           05 CID  PIC 9(3).
           05 ECL  PIC X(3).
           05 EYR  PIC 9(4).
           05 HCL  PIC X(7).
           05 HGT  PIC X(5).
           05 IYR  PIC 9(4).
           05 PID  PIC X(10).

       01 OTHER-VARS.
           05 TEST-STRING         PIC X(10).
           05 MATCH-COUNT         PIC 9 VALUE 0.
      *     05 SEARCH-STRING       
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
              MOVE VAL-ECL TO ECL
              MOVE VAL-EYR TO EYR
              MOVE VAL-HGT TO HGT
              MOVE VAL-HCL TO HCL
              MOVE VAL-IYR TO IYR
              MOVE SPACES TO PID
              MOVE VAL-PID TO PID              
              PERFORM 500-CHECK-BYR 
              PERFORM 501-CHECK-ECL
              PERFORM 502-CHECK-EYR
              PERFORM 503-CHECK-HGT
              PERFORM 504-CHECK-HCL
              PERFORM 505-CHECK-IYR 
              PERFORM 506-CHECK-PID
              IF SCORE IS GREATER THAN 6 
                 ADD 1 TO OK-PASSPORTS
                 IF DEBUG-ON = 1
                       DISPLAY "!OK! byr:" BYR " ecl:" ECL 
                       DISPLAY " eyr:" EYR " hcl:" HCL " hgt:" HGT 
                       DISPLAY " iyr:" IYR " pid:" PID " score:" SCORE
                 END-IF
              ELSE
                 ADD 1 TO MISSING
                 IF DEBUG-ON = 1                 
                    DISPLAY "!MISSING! byr:" BYR " cid:" CID " ecl:" ECL 
                    DISPLAY " eyr:" EYR " hcl:" HCL " hgt:" HGT 
                    DISPLAY " iyr:" IYR " pid:" PID " score:" SCORE
                 END-IF
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
              MOVE VAL-ECL TO ECL
              MOVE VAL-EYR TO EYR
              MOVE VAL-HGT TO HGT
              MOVE VAL-HCL TO HCL
              MOVE VAL-IYR TO IYR
              MOVE SPACES TO PID
              MOVE VAL-PID TO PID              
              PERFORM 500-CHECK-BYR 
              PERFORM 501-CHECK-ECL
              PERFORM 502-CHECK-EYR
              PERFORM 503-CHECK-HGT
              PERFORM 504-CHECK-HCL
              PERFORM 505-CHECK-IYR 
              PERFORM 506-CHECK-PID
              IF SCORE IS EQUAL TO 6 
                 ADD 1 TO OK-PASSPORTS
                 DISPLAY "pid:" PID
                 IF DEBUG-ON = 1
                    DISPLAY "!OK! byr:" BYR " ecl:" ECL 
                    DISPLAY " eyr:" EYR " hcl:" HCL " hgt:" HGT 
                    DISPLAY " iyr:" IYR " pid:" PID " score:" SCORE
                    DISPLAY "=-----------------------"
                 END-IF
              ELSE
      *            ADD 1 TO MISSING
      *            DISPLAY " "
      *            DISPLAY "!MISSING! byr:" BYR " cid:" CID " ecl:" ECL 
      *            DISPLAY " eyr:" EYR " hcl:" HCL " hgt:" HGT 
      *            DISPLAY " iyr:" IYR " pid:" PID " score:" SCORE
                   DISPLAY "A-----------------------A"
              END-IF
              ADD 1 TO TOTAL
              READ INPUT-FILE
                AT END MOVE 'Y' TO  SW-END-OF-FILE
              END-READ
           END-PERFORM.
           
       500-CHECK-BYR.
      * must have numeric value between 1920-2002 
        IF BYR(1:1) <> ' ' 
           IF BYR IS GREATER THAN OR EQUAL TO 1920 
              AND BYR IS LESS THAN OR EQUAL TO 2002
               ADD 1 TO SCORE
               IF DEBUG-ON = 1      
                 DISPLAY "+1 BYR OK: " BYR 
               END-IF
           END-IF 
        END-IF.
        
       501-CHECK-ECL.
           EVALUATE ECL
           WHEN 'amb' 
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 ECL amb: " ECL
              END-IF
           WHEN 'blu'
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 ECL blu: " ECL
              END-IF
           WHEN 'brn'
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 ECL brn: " ECL
              END-IF
           WHEN 'gry' 
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 ECL gry: " ECL
              END-IF
           WHEN 'grn'
              ADD 1 TO SCORE
              IF DEBUG-ON = 1              
                 DISPLAY "+1 ECL grn: " ECL
              END-IF
           WHEN 'hzl'
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 ECL hzl: " ECL
              END-IF
           WHEN 'oth'
              ADD 1 TO SCORE
              IF DEBUG-ON = 1              
                 DISPLAY "+1 ECL oth: " ECL
              END-IF
           END-EVALUATE.
           
       502-CHECK-EYR.
      * value must be four digits, 2020-2030
        IF EYR(1:1) <> ' ' 
           IF EYR IS GREATER THAN OR EQUAL TO 2020 
              AND EYR IS LESS THAN OR EQUAL TO 2030
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 EYR OK: " EYR
              END-IF
      *        ELSE 
      *           DISPLAY "BYR PUUTTUU: '" BYR "'" 
           END-IF 
        END-IF.
        
       503-CHECK-HGT.
           MOVE 0 TO MATCH-COUNT
           MOVE HGT TO TEST-STRING.
           INSPECT TEST-STRING TALLYING MATCH-COUNT
              FOR ALL 'cm'.
      * at least 150 and at most 193        
           IF MATCH-COUNT > 0
              PERFORM 601-CM
           ELSE 
              PERFORM 602-IN
           END-IF.

       504-CHECK-HCL.
           MOVE 0 TO HCL-TEST.
           IF HCL(1:1) = '#'                  
                 PERFORM VARYING INDEKSI FROM 2 BY 1 UNTIL INDEKSI > 7 
                    IF HCL(INDEKSI:1) IS GREATER THAN OR EQUAL TO 0 AND 
                       HCL(INDEKSI:1) IS LESS THAN OR EQUAL TO 9
                           ADD 1 TO HCL-TEST 
                    ELSE                    
                       EVALUATE HCL(INDEKSI:1)
                       WHEN 'a'
                          ADD 1 TO HCL-TEST 
                       WHEN 'b'
                          ADD 1 TO HCL-TEST                       
                       WHEN 'c'
                          ADD 1 TO HCL-TEST
                       WHEN 'd'
                          ADD 1 TO HCL-TEST
                       WHEN 'e'
                          ADD 1 TO HCL-TEST
                       WHEN 'f'
                          ADD 1 TO HCL-TEST
                       END-EVALUATE
                    END-IF 
                 END-PERFORM
                 IF HCL-TEST IS EQUAL TO 6
                    ADD 1 TO SCORE
                    IF DEBUG-ON = 1
                       DISPLAY "+1 HCL OK: " HCL
                    END-IF
                 END-IF 
      *        ELSE 
      *           DISPLAY "HCL PUUTTUU: '" HCL "'" 
           END-IF.

       505-CHECK-IYR.
      * value must be four digits, 2010-2020
        IF IYR(1:1) <> ' ' 
           IF IYR IS GREATER THAN OR EQUAL TO 2010 
              AND IYR IS LESS THAN OR EQUAL TO 2020
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 IYR OK: " IYR
              END-IF
      *        ELSE 
      *           DISPLAY "BYR PUUTTUU: '" BYR "'" 
           END-IF
        END-IF.             

       506-CHECK-PID.
           MOVE 0 TO PID-TEST.
           PERFORM VARYING PID-I FROM 1 BY 1 UNTIL PID-I > 10 
            IF PID(PID-I:1) IS GREATER THAN OR EQUAL TO 0 AND 
            PID(PID-I:1) IS LESS THAN OR EQUAL TO 9
             ADD 1 TO PID-TEST
            END-IF
           END-PERFORM.
           IF PID-TEST IS EQUAL TO 9
              ADD 1 TO SCORE
              IF DEBUG-ON = 1
                 DISPLAY "+1 pid:" PID
              END-IF
           END-IF.

       601-CM.
             IF HGT(1:3) IS GREATER THAN OR EQUAL TO 150 
                AND HGT(1:3) IS LESS THAN OR EQUAL TO 193
                ADD 1 TO SCORE
                IF DEBUG-ON = 1
                   DISPLAY "+1 HGT OK: " HGT 
                END-IF
             END-IF.

       602-IN.
           MOVE 0 TO MATCH-COUNT
           MOVE HGT TO TEST-STRING.
           INSPECT TEST-STRING TALLYING MATCH-COUNT
              FOR ALL 'in'.
           IF MATCH-COUNT > 0
              IF HGT(1:2) IS GREATER THAN OR EQUAL TO 59 
                 AND HGT(1:2) IS LESS THAN OR EQUAL TO 76
               ADD 1 TO SCORE 
               IF DEBUG-ON = 1
                  DISPLAY "+1 HGT OK: " HGT(1:2) " " HGT 
               END-IF
              END-IF
           END-IF.   


       900-WRAP-UP.
           CLOSE INPUT-FILE.
           DISPLAY "Number of OK passports: " OK-PASSPORTS.
           DISPLAY "Number of missing: " MISSING.
           DISPLAY "Total number of data: " TOTAL.
           DISPLAY "----------------------".