DIM arr(10)
RANDOMIZE TIMER
FOR i = 1 TO 10
    arr(i) = INT(RND * 100)
NEXT i
PRINT "Original array:"
FOR i = 1 TO 10
    PRINT arr(i);
NEXT i
FOR i = 1 TO 9
    FOR j = 1 TO 10 - i
        IF arr(j) > arr(j + 1) THEN
            temp = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = temp
        END IF
    NEXT j
NEXT i
PRINT
PRINT "Sorted array:"
FOR i = 1 TO 10
    PRINT arr(i);
NEXT i
PRINT