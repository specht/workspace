PRINT "Enter a number:"
INPUT N
PRINT "Prime factors of " + STR$(N) + ":"
FOR I = 2 TO N
  WHILE N MOD I = 0
    PRINT I
    N = N / I
  WEND
NEXT I
