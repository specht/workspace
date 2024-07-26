<div class='meta'>
image: clisp-logo.png
</div>

# Common Lisp <span style='font-size: 80%;'>(1984)</span>

<p class='abstract'>
Common Lisp wurde im Jahr 1984 als Nachfolger von Lisp entwickelt. Der Hauptaspekt dieser Sprache sind Listen, die als Datenstruktur und Programmiersprache dienen. Common Lisp ist eine der ältesten Programmiersprachen und wird in der KI-Forschung und in der Softwareentwicklung eingesetzt.
</p>

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
