       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTORS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALCULATING.
           03  NUM  USAGE BINARY-LONG VALUE ZERO.
           03  LIM  USAGE BINARY-LONG VALUE ZERO.
           03  CNT  USAGE BINARY-LONG VALUE ZERO.
           03  DIV  USAGE BINARY-LONG VALUE ZERO.
           03  REM  USAGE BINARY-LONG VALUE ZERO.
           03  ZRS  USAGE BINARY-SHORT VALUE ZERO.

       01  DISPLAYING.
           03  DIS  PIC 9(10) USAGE DISPLAY.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Please enter a number: " WITH NO ADVANCING
           ACCEPT NUM
           DIVIDE NUM BY 2 GIVING LIM.

           DISPLAY "Factors: " WITH NO ADVANCING
           PERFORM VARYING CNT FROM 2 BY 1 UNTIL CNT > LIM
               PERFORM WITH TEST AFTER UNTIL REM <> 0
                   DIVIDE NUM BY CNT GIVING DIV REMAINDER REM
                   IF REM = 0
                       MOVE CNT TO DIS
                       PERFORM SHODIS
                       MOVE DIV TO NUM
                   END-IF
               END-PERFORM
           END-PERFORM
           DISPLAY " "
           STOP RUN.

       SHODIS.
           MOVE ZERO TO ZRS.
           INSPECT DIS TALLYING ZRS FOR LEADING ZERO.
           DISPLAY DIS(ZRS + 1:) WITH NO ADVANCING
           DISPLAY " " WITH NO ADVANCING
           EXIT PARAGRAPH.

       END PROGRAM FACTORS.
