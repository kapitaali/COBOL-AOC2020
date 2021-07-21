       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCday1.
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
           05  SUMMA            PIC 9(4)V99 VALUE 0.
           05  TEMP-ONE         PIC 9(4).
           05  TEMP-TWO         PIC 9(4).
           05  TULO             PIC 9(16).
       77  TEMP-CTR             PIC 9(4).
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  Indeksi           PIC  9(4) VALUE 1.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 2000.
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
           PERFORM 200-CALCULATE.
       200-CALCULATE.
           DISPLAY "COMPUTING: ".
           DISPLAY "-----".
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
             UNTIL PROJECT-INDEX > Indeksi
               MOVE Taulukko (PROJECT-INDEX) TO TEMP-ONE
               PERFORM VARYING TEMP-CTR FROM 1 BY 1 
                 UNTIL TEMP-CTR > Indeksi 
                  OR SUMMA = 2020
                  MOVE Taulukko (TEMP-CTR) TO TEMP-TWO
                  ADD TEMP-ONE TO TEMP-TWO GIVING SUMMA
                     IF SUMMA = 2020
                       MULTIPLY TEMP-ONE BY TEMP-TWO GIVING TULO
                         DISPLAY "First number: " TEMP-ONE
                         DISPLAY "2nd number: " TEMP-TWO
                         DISPLAY "Resulting multiplication: " TULO 
                     END-IF
                END-PERFORM
           END-PERFORM.
           DISPLAY " ".
       900-WRAP-UP.
           CLOSE INPUT-FILE.
