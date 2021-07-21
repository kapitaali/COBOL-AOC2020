       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday1Part2.
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  
              ASSIGN TO 'day1input.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  LUVUT-TABLE-I.
           05 LUKU-I         PIC X(4).

       WORKING-STORAGE SECTION.
       01 Teibel. 
         02 Taulukko PIC 9(4) OCCURS 1 TO 2000 
            DEPENDING ON Indeksi.        

       01  TEMP-CALC-VALUE.
           05  SUMMA            PIC 9(4)V99 VALUE 1.
           05  SUMMA2           PIC 9(4)V99 VALUE 1.
           05  TEMP-ONE         PIC 9(4) VALUE 1.
           05  TEMP-TWO         PIC 9(4) VALUE 1.
           05  TEMP-THREE       PIC 9(4) VALUE 1.
           05  TULO             PIC 9(16) VALUE 1.
           05  TULO3            PIC 9(16) VALUE 1.
       77  TEMP-CTR             PIC 9(4) COMP.
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  Indeksi           PIC  9(4) VALUE 1.
       77  Ind3              PIC  9(4) VALUE 1.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 200.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE Teibel.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL END-OF-FILE
                MOVE LUKU-I TO
                        Taulukko (PROJECT-INDEX)
                ADD 1 TO Indeksi
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
      *          DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
           END-PERFORM.
           DISPLAY " ".
       100-PROCESS-TABLE-DATA.
               PERFORM 200-CALCULATE VARYING Ind3 FROM 1 BY 1 
                UNTIL Ind3 > Indeksi 
                OR SUMMA = 2020.
      *         MOVE Taulukko (Ind3) TO TEMP-THREE. 
       200-CALCULATE.
      *    DISPLAY "COMPUTING: ".
      *    DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > TABLE-MAX 
               IF Taulukko (PROJECT-INDEX) > 0
                MOVE Taulukko (PROJECT-INDEX) TO TEMP-ONE
               END-IF 
               PERFORM VARYING TEMP-CTR FROM 1 BY 1 
                 UNTIL TEMP-CTR > TABLE-MAX
                  OR SUMMA = 2020
                  IF Taulukko (TEMP-CTR) > 0
                   MOVE Taulukko (TEMP-CTR) TO TEMP-TWO
                  END-IF
                  IF Taulukko (Ind3) > 0
                   MOVE Taulukko (Ind3) TO TEMP-THREE
                  END-IF 
                  ADD TEMP-ONE TO TEMP-TWO GIVING SUMMA2 
                  ADD TEMP-THREE TO SUMMA2 GIVING SUMMA
                  IF SUMMA = 2020
                   MULTIPLY TEMP-ONE BY TEMP-TWO GIVING TULO3 
                   MULTIPLY TULO3 BY TEMP-THREE GIVING TULO
                    DISPLAY "First number: " TEMP-ONE
                    DISPLAY "2nd number: " TEMP-TWO
                    DISPLAY "3rd number: " TEMP-THREE 
                    DISPLAY "Resulting multiplication: " TULO 
                  END-IF
                END-PERFORM
           END-PERFORM.
      *     DISPLAY " ".
       900-WRAP-UP.
           CLOSE INPUT-FILE.
