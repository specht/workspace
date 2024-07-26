<div class='meta'>
image: basic-logo.png
</div>

# BASIC <span style='font-size: 80%;'>(1964)</span>

<p class='abstract'>
BASIC ist eine Programmiersprache, die in den 1960er Jahren entwickelt wurde. Sie wurde ursprünglich für Anfänger entwickelt, um das Programmieren zu erlernen. Der Name BASIC steht für "Beginner's All-purpose Symbolic Instruction Code". BASIC wurde in den 1970er und 1980er Jahren auf vielen Heimcomputern eingesetzt. Es gibt viele verschiedene Versionen von BASIC, darunter Microsoft BASIC, Commodore BASIC und Sinclair BASIC.
</p>

## Hello, World!

```basic
PRINT "Hello, World!"
```

## Primfaktorenzerlegung

```basic
PRINT "Enter a number:"
INPUT N
PRINT "Prime factors of " + STR$(N) + ":"
FOR I = 2 TO N
  WHILE N MOD I = 0
    PRINT I
    N = N / I
  WEND
NEXT I
```

## Bubble Sort

```basic
DIM numbers(10) AS INTEGER

' Populate the array with some random numbers
FOR i = 1 TO 10
    numbers(i) = INT(RND * 100)
NEXT i

' Print the unsorted array
PRINT "Unsorted array:"
FOR i = 1 TO 10
    PRINT numbers(i)
NEXT i

' Bubble sort subroutine
SUB BubbleSort(arr())
    FOR i = 1 TO UBOUND(arr) - 1
        FOR j = 1 TO UBOUND(arr) - i
            IF arr(j) > arr(j + 1) THEN
                ' Swap the elements
                temp = arr(j)
                arr(j) = arr(j + 1)
                arr(j + 1) = temp
            END IF
        NEXT j
    NEXT i
END SUB

' Call the bubble sort subroutine
CALL BubbleSort(numbers)

' Print the sorted array
PRINT "Sorted array:"
FOR i = 1 TO 10
    PRINT numbers(i)
NEXT i
```

<div class='alert alert-warning'>#{stub()}</div>

