       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLESORT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHANGED-FLAG         PIC x.
           88 HAS-CHANGED      VALUE 'Y'.
           88 HAS-NOT-CHANGED  VALUE 'N'.
       01 ITEM-COUNT           PIC 99.
       01 TEMP-ITEM            PIC 99.
       01 ITEM-ARRAY.
           03 ITEM-ARRAY-COUNT PIC 99.
           03 ITEM             PIC 99 OCCURS 99 TIMES
                               indexed by ITEM-INDEX.

       PROCEDURE DIVISION.
       MAINK.
      * FILL ARRAY WITH VALUES
           MOVE 10 TO ITEM-ARRAY-COUNT
           MOVE 28 TO ITEM (1)
           MOVE 44 TO ITEM (2)
           MOVE 46 TO ITEM (3)
           MOVE 24 TO ITEM (4)
           MOVE 19 TO ITEM (5)
           MOVE  2 TO ITEM (6)
           MOVE 17 TO ITEM (7)
           MOVE 11 TO ITEM (8)
           MOVE 24 TO ITEM (9)
           MOVE  4 TO ITEM (10)
       
           DISPLAY "Unsorted: " WITH NO ADVANCING
           PERFORM VARYING ITEM-INDEX FROM 1 BY 1
              UNTIL ITEM-INDEX > ITEM-ARRAY-COUNT
              DISPLAY ITEM (ITEM-INDEX) ', ' WITH NO ADVANCING
           END-PERFORM

           DISPLAY " "

           MOVE ITEM-ARRAY-COUNT TO ITEM-COUNT
           PERFORM BUBBLE-SORT

           DISPLAY "Sorted:   " WITH NO ADVANCING
           PERFORM VARYING ITEM-INDEX FROM 1 BY 1
              UNTIL ITEM-INDEX > ITEM-ARRAY-COUNT
              DISPLAY ITEM (ITEM-INDEX) ', ' WITH NO ADVANCING
           END-PERFORM

           DISPLAY " "
           STOP RUN.

       BUBBLE-SORT.
           PERFORM WITH TEST AFTER UNTIL HAS-NOT-CHANGED
              SET HAS-NOT-CHANGED TO TRUE
              SUBTRACT 1 FROM ITEM-COUNT
              PERFORM VARYING ITEM-INDEX FROM 1 BY 1
                 UNTIL ITEM-INDEX > ITEM-COUNT
                 IF ITEM (ITEM-INDEX) > ITEM (ITEM-INDEX + 1)
                    MOVE ITEM (ITEM-INDEX) TO TEMP-ITEM
                    MOVE ITEM (ITEM-INDEX + 1) TO ITEM (ITEM-INDEX)
                    MOVE TEMP-ITEM TO ITEM (ITEM-INDEX + 1)
                    SET HAS-CHANGED TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM
           .