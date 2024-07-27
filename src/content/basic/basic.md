<div class='meta'>
image: basic-logo.png
</div>

<div class='floatright mt-5' style='width: 24em;'>
    <img src='kemeny-kurtz.webp'>
    <p>John G. Kemeny &amp; Thomas E. Kurtz</p>
</div>

# BASIC <span style='font-size: 80%;'>(1964)</span>

<p class='abstract'>
BASIC ist eine Programmiersprache, die in den 1960er Jahren von John G. Kemeny und Thomas E. Kurtz am Dartmouth College entwickelt wurde. Sie wurde ursprünglich für Anfänger entwickelt, um das Programmieren zu erlernen. Der Name BASIC steht für "Beginner's All-purpose Symbolic Instruction Code". BASIC wurde in den 1970er und 1980er Jahren auf vielen Heimcomputern eingesetzt. Es gibt viele verschiedene Versionen von BASIC, darunter Microsoft BASIC, Commodore BASIC und Sinclair BASIC.
</p>

## Eigenschaften

- **Einfachheit**: BASIC wurde entwickelt, um einfach zu erlernen und zu verwenden zu sein. Es verwendet eine klare und verständliche Syntax, die es Anfängern erleichtert, das Programmieren zu erlernen.
- **Interpretiert**: BASIC ist eine interpretierte Sprache, was bedeutet, dass der Code zur Laufzeit ausgeführt wird.
- **Strukturiert**: Moderne Versionen von BASIC unterstützen strukturierte Programmierung, die es ermöglicht, den Code in logische Blöcke zu unterteilen.
- **Portabilität**: BASIC-Code ist in der Regel portabel und kann auf verschiedenen Plattformen und Betriebssystemen ausgeführt werden.
- **Grafik und Sound**: BASIC bietet Funktionen zur Erstellung von Grafiken und Soundeffekten, die es ermöglichen, interaktive Programme zu erstellen.
- **Einfache Mathematik**: BASIC bietet eine Vielzahl von mathematischen Funktionen und Operatoren, die es ermöglichen, mathematische Berechnungen durchzuführen.

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

