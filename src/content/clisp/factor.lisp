(defun prime-factors (n)
  (let ((factors '())) ; Initialize factors as an empty list
    (loop for i from 2 to (isqrt n)
          do (loop while (zerop (mod n i))
                   do (progn
                        (push i factors)
                        (setf n (/ n i)))))
    (if (> n 1)
        (push n factors))
    (nreverse factors))) ; Reverse the list to maintain the order of factors

(defun read-number ()
  (format *query-io* "Enter a number: ")
  (force-output *query-io*)
  (parse-integer (read-line *query-io*)))

(defun main ()
  (let ((number (read-number)))
    (format t "Prime factors of ~a: ~a~%" number (prime-factors number))))

(main)