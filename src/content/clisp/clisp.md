<div class='meta'>
image: clisp-logo.png
</div>

<div class='floatright mt-5' style='width: 12em;'>
    <img src='mccarthy.webp'>
    <p>John McCarthy</p>
</div>

# Lisp <span style='font-size: 80%;'>(1958)</span>

<p class='abstract'>
Die Programmiersprache Lisp wurde 1958 von John McCarthy am MIT entwickelt und ist eine Sprache, die auf der rekursiven Funktionstheorie basiert.
Nachdem sich zunächst eine Vielzahl an Dialkten entwickelt hatte, wurde 1984 Common Lisp als Vereinheitlichung der Sprache veröffentlicht.
Der Hauptaspekt dieser Sprache sind Listen, die als Datenstruktur und Programmiersprache dienen. Common Lisp wird in der KI-Forschung und in der Softwareentwicklung eingesetzt.
</p>

## Eigenschaften

- **Funktional**: Common Lisp ist eine funktionale Programmiersprache, die auf der rekursiven Funktionstheorie basiert.
- **Dynamisch**: Common Lisp ist eine dynamisch typisierte Sprache, was bedeutet, dass Variablen ihren Datentyp zur Laufzeit ändern können.
- **Objektorientierung**: Common Lisp unterstützt die objektorientierte Programmierung, die auf der Verwendung von Objekten und Klassen basiert.
- **Makros**: Common Lisp bietet Makros, die es ermöglichen, den Code zur Kompilierzeit zu transformieren.
- **Hohe Performance**: Common Lisp ist eine der schnellsten Programmiersprachen und wird häufig für rechenintensive Anwendungen eingesetzt.
- **Community**: Common Lisp hat eine aktive und engagierte Community, die eine Vielzahl von Bibliotheken und Frameworks entwickelt hat.

## Hello, World!

```clisp
```

## Primfaktorenzerlegung

```clisp
```

## Bubble Sort

```clisp
(defun bubble-sort (arr)
  (let ((n (length arr)))
    (loop for i from 0 below (- n 1)
          do (loop for j from 0 below (- n i 1)
                   do (when (> (aref arr j) (aref arr (1+ j)))
                        (rotatef (aref arr j) (aref arr (1+ j)))))))
  arr)

(defun main ()
  (let ((arr (make-array 10 :element-type 'integer)))
    (loop for i from 0 below 10
          do (setf (aref arr i) (random 100)))
    (format t "Original array: ~a~%" arr)
    (format t "Sorted array: ~a~%" (bubble-sort arr))))

(main)
```

<div class='alert alert-warning'>#{stub()}</div>
